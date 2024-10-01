/*
** 2008 October 07
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
** 2024 May 21 gnn
*************************************************************************
** This file contains the C functions that implement mutexes in the
** FreeBSD kernel.
**
*/
#include "sqliteInt.h"

#ifdef LINUX_KERNEL_BUILD

#include <linux/mutex.h>
DEFINE_MUTEX(sqlite3_mu);
static int sqlite3_mu_initialized = 0;

static int klinuxMutexInit(void) {
  if (!sqlite3_mu_initialized) {
    mutex_init(&sqlite3_mu);
    sqlite3_mu_initialized = 1;
  }

  return SQLITE_OK;
}

static int klinuxMutexEnd(void) {
  sqlite3_mu_initialized = 0;
  return SQLITE_OK;
}

static sqlite3_mutex *klinuxMutexAlloc(int id){ 
  UNUSED_PARAMETER(id);

  return ((sqlite3_mutex *)&sqlite3_mu); 
}

static void klinuxMutexFree(sqlite3_mutex *p) {
  UNUSED_PARAMETER(p);
}

static void klinuxMutexEnter(sqlite3_mutex *p){ 
  UNUSED_PARAMETER(p);
  mutex_lock(&sqlite3_mu);
}

static int klinuxMutexTry(sqlite3_mutex *p){
  UNUSED_PARAMETER(p);
  if (!mutex_trylock(&sqlite3_mu))
    return SQLITE_BUSY;
  return SQLITE_OK;
}

static void klinuxMutexLeave(sqlite3_mutex *p) {
  UNUSED_PARAMETER(p);
  mutex_unlock(&sqlite3_mu);
}


sqlite3_mutex_methods const *sqlite3DefaultMutex(void){
  static const sqlite3_mutex_methods mutex_vtable = {
    klinuxMutexInit,
    klinuxMutexEnd,
    klinuxMutexAlloc,
    klinuxMutexFree,
    klinuxMutexEnter,
    klinuxMutexTry,
    klinuxMutexLeave,
    0,
    0,
  };

  return &mutex_vtable;
}
#endif
