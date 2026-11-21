#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Boolean.h>
#include <R_ext/Utils.h>
#include <stdint.h>

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
    unsigned char byte[sizeof(double)];
} ieee_double;

SEXP _directDeclared(
    SEXP x, SEXP na_index, SEXP na_values, SEXP na_range,
    SEXP labels, SEXP label, SEXP measurement, SEXP decimals,
    SEXP date, SEXP class_
) {
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
    Rf_setAttrib(x, Rf_install("class"), class_);

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
    R_xlen_t n = XLENGTH(x);
    SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
    for (R_xlen_t i = 0; i < n; ++i) {
        SEXP value = STRING_ELT(x, i);
        ieee_double y;
        y.value = NA_REAL;
        if (value != NA_STRING) {
            const unsigned char *tag = (const unsigned char *)CHAR(value);
            if (*tag == '-') {
                ++tag;
                y.value = copysign(NA_REAL, -1.0);
            }
            if (*tag) {
                y.byte[TAG_BYTE] = tag[0];
                y.byte[TAG_BYTE == 4 ? 5 : 2] = tag[1];
            }
        }
        REAL(out)[i] = y.value;
    }
    UNPROTECT(1);
    return out;
}


SEXP _hasTag(SEXP x, SEXP tag_) {
    R_xlen_t n = XLENGTH(x);
    SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
    if (TYPEOF(x) != REALSXP) {
        Memzero(LOGICAL(out), n);
        UNPROTECT(1);
        return out;
    }
    if (tag_ != R_NilValue && (TYPEOF(tag_) != STRSXP || XLENGTH(tag_) != 1)) {
        Rf_error("Expected one tag.");
    }
    const unsigned char *tag = tag_ == R_NilValue ? NULL :
        (const unsigned char *)CHAR(STRING_ELT(tag_, 0));
    int negative = tag && *tag == '-';
    if (negative) ++tag;

    for (R_xlen_t i = 0; i < n; ++i) {
        ieee_double y;
        y.value = REAL(x)[i];
        int tagged = ISNAN(y.value) && y.byte[TAG_BYTE] != 0;
        if (tagged && tag) {
            tagged = (!!signbit(y.value) == negative &&
                      y.byte[TAG_BYTE] == tag[0] &&
                      y.byte[TAG_BYTE == 4 ? 5 : 2] == tag[1]);
        }
        LOGICAL(out)[i] = tagged;
    }
    UNPROTECT(1);
    return out;
}


SEXP _getTag(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; ++i) {
        ieee_double y;
        y.value = REAL(x)[i];
        if (!ISNAN(y.value) || !y.byte[TAG_BYTE]) {
            SET_STRING_ELT(out, i, NA_STRING);
        }
        else {
            char tag[3];
            int length = 0;
            if (signbit(y.value)) tag[length++] = '-';
            tag[length++] = y.byte[TAG_BYTE];
            char second = y.byte[TAG_BYTE == 4 ? 5 : 2];
            if (second) tag[length++] = second;
            SET_STRING_ELT(out, i, Rf_mkCharLenCE(tag, length, CE_UTF8));
        }
    }
    UNPROTECT(1);
    return out;
}


SEXP _anyTagged(SEXP x) {
    if (TYPEOF(x) != REALSXP) return Rf_ScalarLogical(FALSE);
    R_xlen_t n = XLENGTH(x);
    for (R_xlen_t i = 0; i < n; ++i) {
        ieee_double y;
        y.value = REAL(x)[i];
        if (ISNAN(y.value) && y.byte[TAG_BYTE]) return Rf_ScalarLogical(TRUE);
    }
    return Rf_ScalarLogical(FALSE);
}


/* Prepared category codes are zero based. R retains factor levels, labels,
   missing-value policy, rounding and the output class. */
SEXP _wtable(SEXP x, SEXP y, SEXP wt, SEXP dims) {
    if (TYPEOF(x) != INTSXP || TYPEOF(wt) != REALSXP ||
        XLENGTH(x) != XLENGTH(wt) || TYPEOF(dims) != INTSXP || LENGTH(dims) != 2 ||
        INTEGER(dims)[0] < 0 || INTEGER(dims)[1] < 0 ||
        (
            y != R_NilValue && (TYPEOF(y) != INTSXP || XLENGTH(y) != XLENGTH(x))
        )
    ) {
        Rf_error("Invalid frequency buffers.");
    }

    size_t nx = (size_t)INTEGER(dims)[0], ny = (size_t)INTEGER(dims)[1];

    if (
        (ny && nx > (size_t)R_XLEN_T_MAX / ny) ||
        (ny && nx > SIZE_MAX / sizeof(long double) / ny) ||
        (y == R_NilValue && ny != 1)
    ) {
        Rf_error("Invalid frequency dimensions.");
    }

    size_t cells = nx * ny;
    SEXP out = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)cells));

    if (cells) {
        /* R-managed storage is reclaimed even if an interrupt is raised. */
        long double *counts = R_allocLD(cells);
        Memzero(counts, cells);
        const int *ix = INTEGER(x), *iy = y == R_NilValue ? NULL : INTEGER(y);
        const double *weights = REAL(wt);
        R_xlen_t n = XLENGTH(x);

        for (R_xlen_t i = 0; i < n; i++) {
            if (i % 65536 == 0) R_CheckUserInterrupt();
            int cx = ix[i], cy = iy ? iy[i] : 0;
            double w = weights[i];
            if (cx >= 0 && cy >= 0 && (size_t)cx < nx &&
                (size_t)cy < ny && !ISNAN(w)) {
                counts[(size_t)cx + nx * (size_t)cy] += w;
            }
        }

        for (size_t i = 0; i < cells; i++) REAL(out)[i] = (double)counts[i];
    }

    R_CheckUserInterrupt();

    UNPROTECT(1);
    return out;
}
