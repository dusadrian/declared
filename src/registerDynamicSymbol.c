#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_declared(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}


/*
8.1.3 Registering symbols

An application embedding R needs a different way of registering symbols because it is not a dynamic library loaded by R as would be the case with a package. Therefore R reserves a special DllInfo entry for the embedding application such that it can register symbols to be used with .C, .Call etc. This entry can be obtained by calling getEmbeddingDllInfo, so a typical use is

    DllInfo *info = R_getEmbeddingDllInfo();
    R_registerRoutines(info, cMethods, callMethods, NULL, NULL);

The native routines defined by cMethods and callMethods should be present in the embedding application. See Registering native routines for details on registering symbols in general.
*/
