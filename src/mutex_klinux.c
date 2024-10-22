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


struct sqlite3_mutex {
  struct mutex mutex;
  int type;
  volatile struct task_struct *owner;
  volatile int refs;
};

static struct sqlite3_mutex mutexes[12];


static int klinuxMutexInit(void){
  for (int i = 2; i < 14; ++i) {
    mutex_init(&mutexes[i-2].mutex);
    mutexes[i-2].type = i;
    mutexes[i-2].owner = NULL;
    mutexes[i-2].refs = 0;
  }

  return SQLITE_OK;
}

static int klinuxMutexEnd(void){
  return SQLITE_OK;
}

static sqlite3_mutex *klinuxMutexAlloc(int type){
  sqlite3_mutex *p;

  switch(type) {
  case SQLITE_MUTEX_RECURSIVE:
    p = sqlite3Malloc(sizeof(struct sqlite3_mutex));
    if (p) {
      mutex_init(&p->mutex);
      p->type = SQLITE_MUTEX_RECURSIVE;
      p->refs = 0;
      p->owner = NULL;
    }
    break;
  case SQLITE_MUTEX_FAST:
    p = sqlite3Malloc(sizeof(struct sqlite3_mutex));
    if (p) {
      mutex_init(&p->mutex);
      p->type = SQLITE_MUTEX_FAST;
    }
    break;
  default:
    p = &mutexes[type-2];
    break;
  }

  return p;
}

static void klinuxMutexFree(sqlite3_mutex *p){
  sqlite3_free(p);
}

static void klinuxMutexEnter(sqlite3_mutex *p){
  struct task_struct *self = current;
  if (p->refs > 0 && self == p->owner) p->refs++;
  else {
    mutex_lock(&p->mutex);
    p->owner = self;
    p->refs = 1;
  }
}

static int klinuxMutexTry(sqlite3_mutex *p){
  struct task_struct *self = current;
  if (p->refs > 0 && self == p->owner) {
      p->refs++;
      return SQLITE_OK;
  } else if (mutex_trylock(&p->mutex)) {
      p->owner = self;
      p->refs = 1;
      return SQLITE_OK;
  } else {
      return SQLITE_BUSY;
  }
}

static void klinuxMutexLeave(sqlite3_mutex *p) {
  p->refs--;
  if (p->refs == 0) {
      p->owner = 0;
      mutex_unlock(&p->mutex);
  }
}


sqlite3_mutex_methods const *sqlite3DefaultMutex(void){
  static const sqlite3_mutex_methods sMutex = {
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

  return &sMutex;
}
#endif
