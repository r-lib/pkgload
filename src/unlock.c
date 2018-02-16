#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h>  // for NULL

extern SEXP unlock_environment_(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"unlock_environment_", (DL_FUNC) &unlock_environment_, 1},
    {NULL, NULL, 0}
};

void R_init_pkgload(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

/* This is taken from envir.c in the R 2.15.1 source
https://github.com/SurajGupta/r-source/blob/master/src/main/envir.c
*/
#define FRAME_LOCK_MASK (1 << 14)
#define FRAME_IS_LOCKED(e) (ENVFLAGS(e) & FRAME_LOCK_MASK)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~FRAME_LOCK_MASK))

extern SEXP R_TrueValue;
extern SEXP R_FalseValue;

SEXP unlock_environment_(SEXP env) {
  UNLOCK_FRAME(env);
  return FRAME_IS_LOCKED(env) == 0 ? R_TrueValue : R_FalseValue;
}
