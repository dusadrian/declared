#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Boolean.h>

/*
// copied from: https://github.com/wch/r-source/blob/trunk/src/main/envir.c
// as mentioned in https://gist.github.com/wch/3280369#file-unlockenvironment-r
// https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Moving-into-C-API-compliance
// https://rstudio.github.io/r-manuals/r-exts/The-R-API.html#moving-into-c-api-compliance
#define FRAME_LOCK_MASK (1<<14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))

SEXP _unlockEnvironment(SEXP env) {
    UNLOCK_FRAME(env);

    SEXP result = PROTECT( Rf_allocVector(LGLSXP, 1) );
    LOGICAL(result)[0] = FRAME_IS_LOCKED(env) == 0; // maybe check R_EnvironmentIsLocked ?
    UNPROTECT(1);
    return result;
}
*/

typedef union {
    double value;
    char byte[16];
} ieee_double;

SEXP _directDeclared(SEXP x, SEXP na_index, SEXP na_values, SEXP na_range,
                     SEXP labels, SEXP label, SEXP measurement, SEXP decimals,
                     SEXP date, SEXP class_) {
    int nprotect = 1;
    x = PROTECT(x);

    if (MAYBE_SHARED(x)) {
        x = PROTECT(Rf_duplicate(x));
        nprotect++;
    }

    Rf_setAttrib(x, Rf_install("na_index"), na_index);
    Rf_setAttrib(x, Rf_install("na_values"), na_values);
    Rf_setAttrib(x, Rf_install("na_range"), na_range);
    Rf_setAttrib(x, Rf_install("labels"), labels);
    Rf_setAttrib(x, Rf_install("label"), label);
    Rf_setAttrib(x, Rf_install("measurement"), measurement);
    Rf_setAttrib(x, Rf_install("decimals"), decimals);
    Rf_setAttrib(x, Rf_install("date"), date);
    Rf_classgets(x, class_);

    UNPROTECT(nprotect);
    return x;
}



static Rboolean isNAAt(SEXP x, R_xlen_t index) {
    switch (TYPEOF(x)) {
        case LGLSXP:
            return LOGICAL(x)[index] == NA_LOGICAL;
        case INTSXP:
            return INTEGER(x)[index] == NA_INTEGER;
        case REALSXP:
            return ISNAN(REAL(x)[index]);
        case CPLXSXP:
            return ISNAN(COMPLEX(x)[index].r) || ISNAN(COMPLEX(x)[index].i);
        case STRSXP:
            return STRING_ELT(x, index) == NA_STRING;
        default:
            return FALSE;
    }
}



SEXP _allIndexedNA(SEXP x, SEXP na_index) {
    R_xlen_t length_x = XLENGTH(x);
    R_xlen_t length_index = XLENGTH(na_index);

    if (TYPEOF(na_index) == INTSXP) {
        for (R_xlen_t i = 0; i < length_index; ++i) {
            int position = INTEGER(na_index)[i];

            if (
                position == NA_INTEGER || position < 1 ||
                position > length_x || !isNAAt(x, position - 1)
            ) {
                return Rf_ScalarLogical(FALSE);
            }
        }
    }
    else if (TYPEOF(na_index) == REALSXP) {
        for (R_xlen_t i = 0; i < length_index; ++i) {
            double position = REAL(na_index)[i];

            if (
                !R_FINITE(position) || position < 1 || position > length_x
            ) {
                return Rf_ScalarLogical(FALSE);
            }

            R_xlen_t index = (R_xlen_t) position - 1;

            if (
                position != (double) (index + 1) || !isNAAt(x, index)
            ) {
                return Rf_ScalarLogical(FALSE);
            }
        }
    }
    else {
        return Rf_ScalarLogical(FALSE);
    }

    return Rf_ScalarLogical(TRUE);
}



#ifdef WORDS_BIGENDIAN
// First two bytes are sign & exponent
// Last four bytes (that is, 32 bits) are 1954
const int TAG_BYTE = 3;
#else
const int TAG_BYTE = 4;
#endif


SEXP _tag(SEXP x) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

    for (int i = 0; i < n; ++i) {

        int nchars = Rf_length(STRING_ELT(x, i));
        Rboolean firstminus = CHAR(STRING_ELT(x, i))[0] == CHAR(mkChar("-"))[0];

        if (nchars > 2 + firstminus) {
            nchars = 2 + firstminus;
        }

        ieee_double y;
        y.value = NA_REAL;

        if (firstminus) {
            y.value = -1 * NA_REAL;
        }

        int bytepos = TAG_BYTE;

        for (int c = firstminus; c < nchars; c++) {
            y.byte[bytepos] = CHAR(STRING_ELT(x, i))[c];
            if (TAG_BYTE == 3) {
                bytepos -= 1;
            }
            else {
                bytepos += 1;
            }
        }

        REAL(out)[i] = y.value;
    }

    UNPROTECT(1);
    return(out);
}



SEXP _hasTag(SEXP x, SEXP tag_) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

    if (TYPEOF(x) != REALSXP) {
        for (int i = 0; i < n; ++i) {
            LOGICAL(out)[i] = 0;
        }
    }
    else {

        for (int i = 0; i < n; ++i) {
            double xi = REAL(x)[i];

            if (!isnan(xi)) {
                LOGICAL(out)[i] = FALSE;
            } else {
                ieee_double y;
                y.value = xi;
                char tag = y.byte[TAG_BYTE];

                Rboolean test = TRUE;

                if (tag == '\0') {
                    LOGICAL(out)[i] = FALSE;
                }
                else {
                    if (TYPEOF(tag_) != NILSXP) {

                        int nchars = Rf_length(STRING_ELT(tag_, 0));
                        Rboolean firstminus = CHAR(STRING_ELT(tag_, 0))[0] == CHAR(mkChar("-"))[0];

                        if ((firstminus && !signbit(xi)) || (!firstminus && signbit(xi))) {
                            LOGICAL(out)[i] = FALSE;
                        }
                        else {

                            if (nchars > 2 + firstminus) {
                                nchars = 2 + firstminus;
                            }

                            test = test && tag == CHAR(STRING_ELT(tag_, 0))[firstminus];
                            char tag = y.byte[(TAG_BYTE == 4) ? 5 : 2];

                            if (Rf_length(STRING_ELT(tag_, 0)) > 1 && tag != '\0') {
                                test = test && tag == CHAR(STRING_ELT(tag_, 0))[firstminus + 1];
                            }

                            LOGICAL(out)[i] = test;
                        }
                    }
                    else {
                        LOGICAL(out)[i] = TRUE;
                    }
                }
            }
        }
    }

    UNPROTECT(1);
    return out;
}



SEXP _getTag(SEXP x) {

    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(STRSXP, n));

    for (int i = 0; i < n; ++i) {
        double xi = REAL(x)[i];

        if (!isnan(xi)) {
            SET_STRING_ELT(out, i, NA_STRING);
        }
        else {

            ieee_double y;
            y.value = xi;

            Rboolean firstminus = signbit(xi);

            char test[16 + 8 * firstminus];
            if (firstminus) {
                test[0] = CHAR(mkChar("-"))[0];
            }

            test[firstminus] = y.byte[TAG_BYTE];

            if (test[0] == '\0') {
                SET_STRING_ELT(out, i, NA_STRING);
            }
            else {
                char tag2 = y.byte[(TAG_BYTE == 4) ? 5 : 2];
                int nchars = 1 + (strlen(&tag2) > 0) + firstminus;

                test[firstminus + 1] = tag2;
                SET_STRING_ELT(out, i, Rf_mkCharLenCE(test, nchars, CE_UTF8));
            }
        }
    }


    UNPROTECT(1);
    return out;
}



SEXP _anyTagged(SEXP x) {
    int n = Rf_length(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, 1));
    LOGICAL(out)[0] = 0;

    int i = 0;

    while (!LOGICAL(out)[0] && i < n) {
        if (TYPEOF(x) == REALSXP) {
            double xi = REAL(x)[i];
            if (isnan(xi)) {
                ieee_double y;
                y.value = xi;

                Rboolean firstminus = signbit(xi);

                char test[16 + 8 * firstminus];
                if (firstminus) {
                    test[0] = CHAR(mkChar("-"))[0];
                }

                test[firstminus] = y.byte[TAG_BYTE];
                LOGICAL(out)[0] = test[0] != '\0';
            }
        }
        i += 1;
    }

    UNPROTECT(1);
    return out;
}
