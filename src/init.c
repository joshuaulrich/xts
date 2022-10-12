/*
#   xts: eXtensible time-series
#
#   Copyright (C) 2008 - 2013  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich and Dirk Eddelbuettel
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#include "xts.h"
#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"add_class",             (DL_FUNC) &add_class,               2},
  {"coredata_xts",          (DL_FUNC) &coredata_xts,            1},
  {"do_xtsAttributes",      (DL_FUNC) &do_xtsAttributes,        1},
  {"add_xtsCoreAttributes", (DL_FUNC) &add_xtsCoreAttributes,   6},
  {"lag_xts",               (DL_FUNC) &lag_xts,                 3},
  {"lagXts",                (DL_FUNC) &lagXts,                  3},
  {"do_is_ordered",         (DL_FUNC) &do_is_ordered,           3},
  {"isXts",                 (DL_FUNC) &isXts,                   1},
  {"tryXts",                (DL_FUNC) &tryXts,                  1},
  {"na_locf",               (DL_FUNC) &na_locf,                 4},
  {"na_omit_xts",           (DL_FUNC) &na_omit_xts,             1},
  {"do_rbind_xts",          (DL_FUNC) &do_rbind_xts,            3},
  {"_do_subset_xts",        (DL_FUNC) &_do_subset_xts,          4},
  {"do_merge_xts",          (DL_FUNC) &do_merge_xts,           11},
  {"naCheck",               (DL_FUNC) &naCheck,                 2},
  {"make_index_unique",     (DL_FUNC) &make_index_unique,       2},
  {"make_unique",           (DL_FUNC) &make_unique,             2},
  {"any_negative",          (DL_FUNC) &any_negative,            1},
  {"extract_col",           (DL_FUNC) &extract_col,             5},
  {"binsearch",             (DL_FUNC) &binsearch,               3},
  {"fill_window_dups_rev",  (DL_FUNC) &fill_window_dups_rev,    2},
  {"non_duplicates",        (DL_FUNC) &non_duplicates,          2},
  {"roll_min",              (DL_FUNC) &roll_min,                2},
  {"roll_max",              (DL_FUNC) &roll_max,                2},
  {"roll_sum",              (DL_FUNC) &roll_sum,                2},
  {"roll_cov",              (DL_FUNC) &roll_cov,                4},
  {"toPeriod",              (DL_FUNC) &toPeriod,                7},
  {"xts_period_apply",      (DL_FUNC) &xts_period_apply,        4},
  {"xts_period_min",        (DL_FUNC) &xts_period_min,          2},
  {"xts_period_max",        (DL_FUNC) &xts_period_max,          2},
  {"xts_period_sum",        (DL_FUNC) &xts_period_sum,          2},
  {"xts_period_prod",       (DL_FUNC) &xts_period_prod,         2},
  {"endpoints",             (DL_FUNC) &endpoints,               4},
  {"dimnames_zoo",          (DL_FUNC) &dimnames_zoo,            1},
  {"xts_set_dimnames",      (DL_FUNC) &xts_set_dimnames,        2},
  {"do_startofyear",        (DL_FUNC) &do_startofyear,          3},
  {NULL,                    NULL,                               0}
};

static const
R_ExternalMethodDef externalMethods[] = {
  {"number_of_cols",        (DL_FUNC) &number_of_cols,          -1},
  {"mergeXts",              (DL_FUNC) &mergeXts,                -1},
  {"rbindXts",              (DL_FUNC) &rbindXts,                -1},
  {NULL,                    NULL,                               0}
};

/* define globals */
SEXP xts_IndexSymbol;
SEXP xts_ClassSymbol;
SEXP xts_IndexTformatSymbol;
SEXP xts_IndexTclassSymbol;
SEXP xts_IndexTzoneSymbol;

/* define imports from zoo */
SEXP (*zoo_lag)(SEXP,SEXP,SEXP);
SEXP (*zoo_coredata)(SEXP,SEXP);

/*
 * Taken from R/src/main/names.c
 *   "Set up a set of globals so that a symbol table search can be
 *    avoided when matching something like dim or dimnames."
 *
 * This also prevents flags from rchk's maacheck (Multiple-Allocating-
 * Arguments) tool for calls like:
 *   setAttrib(result, xts_IndexSymbol, getAttrib(x, xts_IndexSymbol));
 */
static void SymbolShortcuts(void)
{
  xts_IndexSymbol = install("index");
  xts_ClassSymbol = install(".CLASS");
  xts_IndexTformatSymbol = install("tformat");
  xts_IndexTclassSymbol = install("tclass");
  xts_IndexTzoneSymbol = install("tzone");
}

void R_init_xts(DllInfo *info)
{
  SymbolShortcuts();
  R_registerRoutines(info,
                     NULL,
                     callMethods,
                     NULL,
                     externalMethods);

  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);

  /* used by external packages linking to internal xts code from C */
  R_RegisterCCallable("xts","do_is_ordered",(DL_FUNC) &do_is_ordered);
  R_RegisterCCallable("xts","coredata_xts", (DL_FUNC) &coredata_xts);
  R_RegisterCCallable("xts","isXts",        (DL_FUNC) &isXts);
  R_RegisterCCallable("xts","tryXts",       (DL_FUNC) &tryXts);
  R_RegisterCCallable("xts","do_rbind_xts", (DL_FUNC) &do_rbind_xts);
  R_RegisterCCallable("xts","naCheck",      (DL_FUNC) &naCheck);
  R_RegisterCCallable("xts","lagXts",       (DL_FUNC) &lagXts);

  R_RegisterCCallable("xts","make_index_unique", (DL_FUNC) &make_index_unique);
  R_RegisterCCallable("xts","make_unique",       (DL_FUNC) &make_unique);
  R_RegisterCCallable("xts","endpoints",         (DL_FUNC) &endpoints);
  R_RegisterCCallable("xts","do_merge_xts",      (DL_FUNC) &do_merge_xts);
  R_RegisterCCallable("xts","na_omit_xts",       (DL_FUNC) &na_omit_xts);
  R_RegisterCCallable("xts","na_locf",           (DL_FUNC) &na_locf);

  R_RegisterCCallable("xts","xts_period_min",  (DL_FUNC) &xts_period_min);
  R_RegisterCCallable("xts","xts_period_max",  (DL_FUNC) &xts_period_max);
  R_RegisterCCallable("xts","xts_period_sum",  (DL_FUNC) &xts_period_sum);
  R_RegisterCCallable("xts","xts_period_prod", (DL_FUNC) &xts_period_prod);

  R_RegisterCCallable("xts","xts_set_dimnames", (DL_FUNC) &xts_set_dimnames);

  /* used by xts (functions moved from xts to zoo) */
  zoo_lag      = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("zoo","zoo_lag");
  zoo_coredata = (SEXP(*)(SEXP,SEXP))      R_GetCCallable("zoo","zoo_coredata");
}
