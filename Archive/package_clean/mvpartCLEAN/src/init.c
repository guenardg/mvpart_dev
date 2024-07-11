#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void formatgC(int*, double*, char**, char**);
extern void gdistance(double*, int*, int*, double*, int*, int*);
extern void pred_rpart(int*, int*, int*, int*, int*, int*, int*, double*, int*,
                       int*, double*, int*, int*);
extern void rpartexp2(int*, double*, double*, int*);
extern void s_to_rp(int*, int*, int*, int*, double*, double*, int*, int*,
                    double*, double*, int*, int*, char**, double*, int*,
                    double*);
extern void s_to_rp2(int*, int*, int*, int*, int*, int*, int*, int*, double*,
                     double*, int*, int*, double*, int*);
extern void xdists(double*, int*, double*, int*, double*, double*);
extern void s_xpred(int*, int*, int*, int*, double*, double*, int*, int*,
                    double*, double*, int*, double*, int*, double*, char**,
                    double*, int*, double*);

/* .Call calls */
extern SEXP init_rpcallback(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
  {"formatgC",        (DL_FUNC) &formatgC,         4},
  {"gdistance",       (DL_FUNC) &gdistance,        6},
  {"pred_rpart",      (DL_FUNC) &pred_rpart,      13},
  {"rpartexp2",       (DL_FUNC) &rpartexp2,        4},
  {"s_to_rp",         (DL_FUNC) &s_to_rp,         16},
  {"s_to_rp2",        (DL_FUNC) &s_to_rp2,        14},
  {"xdists",          (DL_FUNC) &xdists,           6},
  {"s_xpred",         (DL_FUNC) &s_xpred,         18},
  {NULL,              NULL,                        0}
};

static const R_CallMethodDef CallEntries[] = {
  {"init_rpcallback", (DL_FUNC) &init_rpcallback,  5},
  {NULL,              NULL,                        0}
};

void R_init_mvpartCLEAN(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
