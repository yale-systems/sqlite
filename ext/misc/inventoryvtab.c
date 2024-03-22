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
** Usage:
** CREATE VIRTUAL TABLE my_inventory USING inventoryvtab('/path/to/data.csv');
**
** This file implements a template virtual-table.
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
**     (3)  Change every occurrence of "templatevtab" to some other string
**          appropriate for the new virtual table.  Ideally, the new string
**          should be the basename of the source file: "mynewvtab".  Also
**          globally change "TEMPLATEVTAB" to "MYNEWVTAB".
**
**     (4)  Run a test compilation to make sure the unmodified virtual
**          table works.
**
**     (5)  Begin making incremental changes, testing as you go, to evolve
**          the new virtual table to do what you want it to do.
**
** This template is minimal, in the sense that it uses only the required
** methods on the sqlite3_module object.  As a result, templatevtab is
** a read-only and eponymous-only table.  Those limitation can be removed
** by adding new methods.
**
** This template implements an eponymous-only virtual table with a rowid and
** two columns named "a" and "b".  The table as 10 rows with fixed integer
** values. Usage example:
**
**     SELECT rowid, a, b FROM templatevtab;
*/
#if !defined(SQLITEINT_H)
#include "sqlite3ext.h"
#endif
SQLITE_EXTENSION_INIT1
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>
#include <ctype.h>
#include <stdio.h>

typedef struct {
  int product_id;
  int quantity;
} Inventory;

/* templatevtab_vtab is a subclass of sqlite3_vtab which is
** underlying representation of the virtual table
*/
typedef struct inventoryvtab_vtab inventoryvtab_vtab;
struct inventoryvtab_vtab {
  sqlite3_vtab base;  /* Base class - must be first */
  char *filename; //name of the file from which we read
  Inventory* inventoryData; //dynamic array of inventory objects
  int item_count; //count of items currently in the array
  /* Add new fields here, as necessary */
};

/*
This function dynamically allocates memory (if needed) for a new entry in the dynamic array
*/
int loadInventoryData(inventoryvtab_vtab* pVtab, const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        fprintf(stderr, "Could not open file: %s\n", filename);
        return SQLITE_ERROR;
    }
    
    int capacity = 0;

    int product_id;
    int quantity;

    while (fscanf(file, "%d %d", &product_id, &quantity) == 2) {
        // Resize array if necessary
        if (pVtab->item_count == capacity) {
            capacity = capacity > 0 ? capacity + 5 : 5; // add 5, or initialize to 5
            Inventory* newinventoryData = (Inventory*)realloc(pVtab->inventoryData, capacity * sizeof(Inventory));
            if (!newinventoryData) {
                fprintf(stderr, "Failed to allocate memory\n");
                free(pVtab->inventoryData);
                fclose(file);
                return SQLITE_NOMEM;
            }
            pVtab->inventoryData = newinventoryData;
        }

        // Store the new item
        pVtab->inventoryData[pVtab->item_count].product_id = product_id;
        pVtab->inventoryData[pVtab->item_count].quantity = quantity;
        (pVtab->item_count)++;
    }

    fclose(file);

    return SQLITE_OK;
}

/* templatevtab_cursor is a subclass of sqlite3_vtab_cursor which will
** serve as the underlying representation of a cursor that scans
** over rows of the result
*/
typedef struct inventoryvtab_cursor inventoryvtab_cursor;
struct inventoryvtab_cursor {
  sqlite3_vtab_cursor base;  /* Base class - must be first */
  int pos; // position in the array of inventory instances
};

/* Skip leading whitespace.  Return a pointer to the first non-whitespace
** character, or to the zero terminator if the string has only whitespace */
static const char *inv_skip_whitespace(const char *z){
  while( isspace((unsigned char)z[0]) ) z++;
  return z;
}

/* Remove trailing whitespace from the end of string z[] */
static void inv_trim_whitespace(char *z){
  size_t n = strlen(z);
  while( n>0 && isspace((unsigned char)z[n]) ) n--;
  z[n] = 0;
}

/* Dequote the string */
static void inv_dequote(char *z){
  int j;
  char cQuote = z[0];
  size_t i, n;

  if( cQuote!='\'' && cQuote!='"' ) return;
  n = strlen(z);
  if( n<2 || z[n-1]!=z[0] ) return;
  for(i=1, j=0; i<n-1; i++){
    if( z[i]==cQuote && z[i+1]==cQuote ) i++;
    z[j++] = z[i];
  }
  z[j] = 0;
}

/*
** The templatevtabConnect() method is invoked to create a new
** template virtual table.
**
** Think of this routine as the constructor for templatevtab_vtab objects.
**
** All this routine needs to do is:
**
**    (1) Allocate the templatevtab_vtab object and initialize all fields.
**
**    (2) Tell SQLite (via the sqlite3_declare_vtab() interface) what the
**        result set of queries against the virtual table will look like.
*/
static int inventoryvtabConnect(
  sqlite3 *db,
  void *pAux,
  int argc, const char *const*argv,
  sqlite3_vtab **ppVtab,
  char **pzErr
){
  inventoryvtab_vtab *pNew; //inventory virtual table object to construct
  int rc;
  
  if (argc >= 3) {
    rc = sqlite3_declare_vtab(db,
           "CREATE TABLE x(product_id INT, quantity INT)"
       );
       
    if( rc==SQLITE_OK ){
      pNew = (inventoryvtab_vtab*) sqlite3_malloc(sizeof(*pNew)); //make virtual table
      *ppVtab = (sqlite3_vtab*)pNew; //convert it to virtual table usable by SQLite
      if( pNew==0 ) return SQLITE_NOMEM;
      memset(pNew, 0, sizeof(*pNew));

      pNew->filename = sqlite3_mprintf("%s", argv[2]); //third argument is filename
      loadInventoryData(pNew, pNew->filename);
    }
    else {
      *pzErr = sqlite3_mprintf("Error with filenme\n");
      return SQLITE_ERROR;
    }
  }

  return rc;
}

/*
** This method is the destructor for templatevtab_vtab objects.
*/
static int inventoryvtabDisconnect(sqlite3_vtab *pVtab){
  inventoryvtab_vtab *p = (inventoryvtab_vtab*)pVtab;
  if (p->inventoryData) {
    sqlite3_free(p->inventoryData); //collect garbage pointers
  }
  sqlite3_free(p->filename); //collect garbage pointers
  sqlite3_free(p);
  return SQLITE_OK;
}

/*
** Constructor for a new templatevtab_cursor object.
*/
static int inventoryvtabOpen(sqlite3_vtab *p, sqlite3_vtab_cursor **ppCursor){
  inventoryvtab_cursor *pCur;
  pCur = sqlite3_malloc( sizeof(*pCur) );
  if( pCur==0 ) return SQLITE_NOMEM;
  memset(pCur, 0, sizeof(*pCur));
  *ppCursor = &pCur->base;
  return SQLITE_OK;
}

/*
** Destructor for a templatevtab_cursor.
*/
static int inventoryvtabClose(sqlite3_vtab_cursor *cur){
  inventoryvtab_cursor *pCur = (inventoryvtab_cursor*)cur;
  sqlite3_free(pCur);
  return SQLITE_OK;
}

/*
** Advance a templatevtab_cursor to its next row of output.
*/
static int inventoryvtabNext(sqlite3_vtab_cursor *cur){
  inventoryvtab_cursor *pCur = (inventoryvtab_cursor*)cur;
  pCur->pos++;
  return SQLITE_OK;
}

/*
** Return values of columns for the row at which the templatevtab_cursor
** is currently pointing.
*/
static int inventoryvtabColumn(
  sqlite3_vtab_cursor *cur,   /* The cursor */
  sqlite3_context *ctx,       /* First argument to sqlite3_result_...() */
  int i                       /* Which column to return */
){
  inventoryvtab_cursor *pCur = (inventoryvtab_cursor*)cur;
  inventoryvtab_vtab *pVtab = (inventoryvtab_vtab*)(pCur->base.pVtab);
  Inventory *item = &(pVtab->inventoryData[pCur->pos]);
  switch( i ){
    case 0: //product_id  
      sqlite3_result_int(ctx, item->product_id);
      break;
    case 1: //quantity
      sqlite3_result_int(ctx, item->quantity);
      break;
    default:
      return SQLITE_ERROR; //neither 0 nor 1
  }
  return SQLITE_OK;
}

/*
** Return the rowid for the current row.  In this implementation, the
** rowid is the same as the output value.
*/
static int inventoryvtabRowid(sqlite3_vtab_cursor *cur, sqlite_int64 *pRowid){
  inventoryvtab_cursor *pCur = (inventoryvtab_cursor*)cur;
  *pRowid = pCur->pos;
  return SQLITE_OK;
}

/*
** Return TRUE if the cursor has been moved off of the last
** row of output.
*/
static int inventoryvtabEof(sqlite3_vtab_cursor *cur){
  inventoryvtab_cursor *pCur = (inventoryvtab_cursor*)cur;
  inventoryvtab_vtab *pVtab = (inventoryvtab_vtab*)(pCur->base.pVtab);
  return pCur->pos>= pVtab->item_count;
}

/*
** This method is called to "rewind" the templatevtab_cursor object back
** to the first row of output.  This method is always called at least
** once prior to any call to templatevtabColumn() or templatevtabRowid() or 
** templatevtabEof().
*/
static int inventoryvtabFilter(
  sqlite3_vtab_cursor *pVtabCursor, 
  int idxNum, const char *idxStr,
  int argc, sqlite3_value **argv
){
  inventoryvtab_cursor *pCur = (inventoryvtab_cursor *)pVtabCursor;
  pCur->pos = 0;
  return SQLITE_OK;
}

/*
** SQLite will invoke this method one or more times while planning a query
** that uses the virtual table.  This routine needs to create
** a query plan for each invocation and compute an estimated cost for that
** plan.
*/
static int inventoryvtabBestIndex(
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
static sqlite3_module inventoryvtabModule = {
  /* iVersion    */ 0,
  /* xCreate     */ 0,
  /* xConnect    */ inventoryvtabConnect,
  /* xBestIndex  */ inventoryvtabBestIndex,
  /* xDisconnect */ inventoryvtabDisconnect,
  /* xDestroy    */ 0,
  /* xOpen       */ inventoryvtabOpen,
  /* xClose      */ inventoryvtabClose,
  /* xFilter     */ inventoryvtabFilter,
  /* xNext       */ inventoryvtabNext,
  /* xEof        */ inventoryvtabEof,
  /* xColumn     */ inventoryvtabColumn,
  /* xRowid      */ inventoryvtabRowid,
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
int sqlite3_inventoryvtab_init(
  sqlite3 *db, 
  char **pzErrMsg, 
  const sqlite3_api_routines *pApi
){
  int rc = SQLITE_OK;
  SQLITE_EXTENSION_INIT2(pApi);
  printf("Hello before\n");
  rc = sqlite3_create_module(db, "inventoryvtab", &inventoryvtabModule, 0);
  printf("Hello after, rc = %d\n", rc);
  return rc;
}
