#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP _directDeclared(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _allIndexedNA(SEXP, SEXP);
extern SEXP _tag(SEXP);
extern SEXP _hasTag(SEXP, SEXP);
extern SEXP _getTag(SEXP);
extern SEXP _anyTagged(SEXP);
extern SEXP _wtable(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef callMethods[] = {
    {"_directDeclared", (DL_FUNC) &_directDeclared, 10},
    {"_allIndexedNA", (DL_FUNC) &_allIndexedNA, 2},
    {"_tag", (DL_FUNC) &_tag, 1},
    {"_hasTag", (DL_FUNC) &_hasTag, 2},
    {"_getTag", (DL_FUNC) &_getTag, 1},
    {"_anyTagged", (DL_FUNC) &_anyTagged, 1},
    {"_wtable", (DL_FUNC) &_wtable, 4},
    {NULL, NULL, 0}
};

void R_init_declared(DllInfo* info) {
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}


/*
8.1.3 Registering symbols

An application embedding R needs a different way of registering symbols because it is not a dynamic library loaded by R as would be the case with a package. Therefore R reserves a special DllInfo entry for the embedding application such that it can register symbols to be used with .C, .Call etc. This entry can be obtained by calling getEmbeddingDllInfo, so a typical use is

    DllInfo *info = R_getEmbeddingDllInfo();
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);

The native routines defined by cMethods and callMethods should be present in the embedding application. See Registering native routines for details on registering symbols in general.
*/
