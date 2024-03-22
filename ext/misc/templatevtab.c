/*
** 2018-04-19
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
**
** This file implements a cstruct virtual-table, for C structs.
** Developers can make a copy of this file as a baseline for writing
** new virtual tables and/or table-valued functions.
**
** Steps for writing a new virtual table implementation:
**
**     (1)  Make a copy of this file.  Perhaps call it "mynewvtab.c"
**
**     (2)  Replace this header comment with something appropriate for
**          the new virtual table
**
**     (3)  Change every occurrence of "cstructvtab" to some other string
**          appropriate for the new virtual table.  Ideally, the new string
**          should be the basename of the source file: "mynewvtab".  Also
**          globally change "CSTRUCTVTAB" to "MYNEWVTAB".
**
**     (4)  Run a test compilation to make sure the unmodified virtual
**          table works.
**
**     (5)  Begin making incremental changes, testing as you go, to evolve
**          the new virtual table to do what you want it to do.
**
** This cstruct is minimal, in the sense that it uses only the required
** methods on the sqlite3_module object.  As a result, cstructvtab is
** a read-only and eponymous-only table.  Those limitation can be removed
** by adding new methods.
**
** This cstruct implements an eponymous-only virtual table with a rowid and
** two columns named "a" and "b".  The table as 10 rows with fixed integer
** values. Usage example:
**
**     SELECT rowid, a, b FROM cstructvtab;
*/
#if !defined(SQLITEINT_H)
#include "sqlite3ext.h"
#endif
SQLITE_EXTENSION_INIT1
#include <string.h>
#include <assert.h>

/* cstructvtab_vtab is a subclass of sqlite3_vtab which is
** underlying representation of the virtual table
*/
typedef struct cstructvtab_vtab cstructvtab_vtab;
struct cstructvtab_vtab {
  sqlite3_vtab base;  /* Base class - must be first */
  /* Add new fields here, as necessary */
};

/* cstructvtab_cursor is a subclass of sqlite3_vtab_cursor which will
** serve as the underlying representation of a cursor that scans
** over rows of the result
*/
typedef struct cstructvtab_cursor cstructvtab_cursor;
struct cstructvtab_cursor {
  sqlite3_vtab_cursor base;  /* Base class - must be first */
  /* Insert new fields here.  For this cstructvtab we only keep track
  ** of the rowid */
  sqlite3_int64 iRowid;      /* The rowid */
};

/*
** The structvtabConnect() method is invoked to create a new
** cstruct virtual table.
**
** Think of this routine as the constructor for cstructvtab_vtab objects.
**
** All this routine needs to do is:
**
**    (1) Allocate the cstructvtab_vtab object and initialize all fields.
**
**    (2) Tell SQLite (via the sqlite3_declare_vtab() interface) what the
**        result set of queries against the virtual table will look like.
*/
static int cstructvtabConnect(
  sqlite3 *db,
  void *pAux,
  int argc, const char *const*argv,
  sqlite3_vtab **ppVtab,
  char **pzErr
){
  cstructvtab_vtab *pNew;
  int rc;

  rc = sqlite3_declare_vtab(db,
           "CREATE TABLE x(a,b)"
       );
  /* For convenience, define symbolic names for the index to each column. */
#define CSTRUCTVTAB_A  0
#define CSTRUCTVTAB_B  1
  if( rc==SQLITE_OK ){
    pNew = sqlite3_malloc( sizeof(*pNew) );
    *ppVtab = (sqlite3_vtab*)pNew;
    if( pNew==0 ) return SQLITE_NOMEM;
    memset(pNew, 0, sizeof(*pNew));
  }
  return rc;
}

/*
** This method is the destructor for cstructvtab_vtab objects.
*/
static int cstructvtabDisconnect(sqlite3_vtab *pVtab){
  cstructvtab_vtab *p = (cstructvtab_vtab*)pVtab;
  sqlite3_free(p);
  return SQLITE_OK;
}

/*
** Constructor for a new cstructvtab_cursor object.
*/
static int cstructvtabOpen(sqlite3_vtab *p, sqlite3_vtab_cursor **ppCursor){
  cstructvtab_cursor *pCur;
  pCur = sqlite3_malloc( sizeof(*pCur) );
  if( pCur==0 ) return SQLITE_NOMEM;
  memset(pCur, 0, sizeof(*pCur));
  *ppCursor = &pCur->base;
  return SQLITE_OK;
}

/*
** Destructor for a cstructvtab_cursor.
*/
static int cstructvtabClose(sqlite3_vtab_cursor *cur){
  cstructvtab_cursor *pCur = (cstructvtab_cursor*)cur;
  sqlite3_free(pCur);
  return SQLITE_OK;
}


/*
** Advance a cstructvtab_cursor to its next row of output.
*/
static int cstructvtabNext(sqlite3_vtab_cursor *cur){
  cstructvtab_cursor *pCur = (cstructvtab_cursor*)cur;
  pCur->iRowid++;
  return SQLITE_OK;
}

/*
** Return values of columns for the row at which the cstructvtab_cursor
** is currently pointing.
*/
static int cstructvtabColumn(
  sqlite3_vtab_cursor *cur,   /* The cursor */
  sqlite3_context *ctx,       /* First argument to sqlite3_result_...() */
  int i                       /* Which column to return */
){
  cstructvtab_cursor *pCur = (cstructvtab_cursor*)cur;
  switch( i ){
    case CSTRUCTVTAB_A:
      sqlite3_result_int(ctx, 1000 + pCur->iRowid);
      break;
    default:
      assert( i==CSTRUCTVTAB_B );
      sqlite3_result_int(ctx, 2000 + pCur->iRowid);
      break;
  }
  return SQLITE_OK;
}

/*
** Return the rowid for the current row.  In this implementation, the
** rowid is the same as the output value.
*/
static int cstructvtabRowid(sqlite3_vtab_cursor *cur, sqlite_int64 *pRowid){
  cstructvtab_cursor *pCur = (cstructvtab_cursor*)cur;
  *pRowid = pCur->iRowid;
  return SQLITE_OK;
}

/*
** Return TRUE if the cursor has been moved off of the last
** row of output.
*/
static int cstructvtabEof(sqlite3_vtab_cursor *cur){
  cstructvtab_cursor *pCur = (cstructvtab_cursor*)cur;
  return pCur->iRowid>=10;
}

/*
** This method is called to "rewind" the cstructvtab_cursor object back
** to the first row of output.  This method is always called at least
** once prior to any call to cstructvtabColumn() or cstructvtabRowid() or 
** cstructvtabEof().
*/
static int cstructvtabFilter(
  sqlite3_vtab_cursor *pVtabCursor, 
  int idxNum, const char *idxStr,
  int argc, sqlite3_value **argv
){
  cstructvtab_cursor *pCur = (cstructvtab_cursor *)pVtabCursor;
  pCur->iRowid = 1;
  return SQLITE_OK;
}

/*
** SQLite will invoke this method one or more times while planning a query
** that uses the virtual table.  This routine needs to create
** a query plan for each invocation and compute an estimated cost for that
** plan.
*/
static int cstructvtabBestIndex(
  sqlite3_vtab *tab,
  sqlite3_index_info *pIdxInfo
){
  pIdxInfo->estimatedCost = (double)10;
  pIdxInfo->estimatedRows = 10;
  return SQLITE_OK;
}

/*
** This following structure defines all the methods for the 
** virtual table.
*/
static sqlite3_module cstructvtabModule = {
  /* iVersion    */ 0,
  /* xCreate     */ 0,
  /* xConnect    */ cstructvtabConnect,
  /* xBestIndex  */ cstructvtabBestIndex,
  /* xDisconnect */ cstructvtabDisconnect,
  /* xDestroy    */ 0,
  /* xOpen       */ cstructvtabOpen,
  /* xClose      */ cstructvtabClose,
  /* xFilter     */ cstructvtabFilter,
  /* xNext       */ cstructvtabNext,
  /* xEof        */ cstructvtabEof,
  /* xColumn     */ cstructvtabColumn,
  /* xRowid      */ cstructvtabRowid,
  /* xUpdate     */ 0,
  /* xBegin      */ 0,
  /* xSync       */ 0,
  /* xCommit     */ 0,
  /* xRollback   */ 0,
  /* xFindMethod */ 0,
  /* xRename     */ 0,
  /* xSavepoint  */ 0,
  /* xRelease    */ 0,
  /* xRollbackTo */ 0,
  /* xShadowName */ 0,
  /* xIntegrity  */ 0
};


#ifdef _WIN32
__declspec(dllexport)
#endif
int sqlite3_cstructvtab_init(
  sqlite3 *db, 
  char **pzErrMsg, 
  const sqlite3_api_routines *pApi
){
  int rc = SQLITE_OK;
  SQLITE_EXTENSION_INIT2(pApi);
  rc = sqlite3_create_module(db, "cstructvtab", &cstructvtabModule, 0);
  return rc;
}
