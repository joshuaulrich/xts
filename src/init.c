#include "xts.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"add_xts_class",         (DL_FUNC) &add_xts_class,           1},
  {"do_xtsAttributes",      (DL_FUNC) &do_xtsAttributes,        1},
  {"do_xtsCoreAttributes",  (DL_FUNC) &do_xtsCoreAttributes,    1},
  {"lagXts",                (DL_FUNC) &lagXts,                  3},
  {"do_is_ordered",         (DL_FUNC) &do_is_ordered,           3},
  {"isXts",                 (DL_FUNC) &isXts,                   1},
  {"do_rbind_xts",          (DL_FUNC) &do_rbind_xts,            2},
  {"do_subset_xts",         (DL_FUNC) &do_subset_xts,           3},
  {NULL,                    NULL,                               0}
};

static const
R_ExternalMethodDef externalMethods[] = {
  {"number_of_cols",        (DL_FUNC) &number_of_cols,          1},
  {"mergeXts",              (DL_FUNC) &mergeXts,                1},
  {NULL,                    NULL,                               0}
};

void R_init_xts(DllInfo *info)
{
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     externalMethods);

  R_useDynamicSymbols(info, TRUE);

  /* used by external packages linking to internal xts code from C */
  R_RegisterCCallable("xts",    "do_is_ordered",    (DL_FUNC) &do_is_ordered);
  R_RegisterCCallable("xts",    "isXts",            (DL_FUNC) &isXts        );
}
