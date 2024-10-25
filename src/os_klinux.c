#include "sqliteInt.h"
#ifdef LINUX_KERNEL_BUILD

#include <linux/printk.h>
#include <linux/time64.h>
#include <linux/ktime.h>
#include <linux/timekeeping.h>
#include <linux/time.h>

static int kern_vfs_current_time(sqlite3_vfs *vfs, double *pTime);
static int kern_vfs_current_time_int64(sqlite3_vfs *vfs, sqlite3_int64 *piNow);

static sqlite3_vfs kern_vfs = {
  3,                          // iVersion
  sizeof(sqlite3_file),       // szOsFile
  SQLITE_MAX_PATHLEN,         // mxPathname
  NULL,                       // pNext
  "kern_vfs",                 // zName
  NULL,                       // pAppData
  NULL,                       // xOpen
  NULL,                       // xDelete
  NULL,                       // xAccess
  NULL,                       // xFullPathname
  NULL,                       // xDlOpen
  NULL,                       // xDlError
  NULL,                       // xDlSym
  NULL,                       // xDlClose
  NULL,                       // xRandomness
  NULL,                       // xSleep
  kern_vfs_current_time,      // xCurrentTime
  NULL,                       // xGetLastError
  kern_vfs_current_time_int64, // xCurrentTimeInt64
  NULL,                       /* xSetSystemCall */
  NULL,                       /* xGetSystemCall */
  NULL,                       /* xNextSystemCall */
};

static int kern_vfs_current_time(sqlite3_vfs *vfs, double *pTime) {
  static const sqlite3_int64 unixEpoch = 24405875*(sqlite3_int64)8640000;
  struct timespec64 ts;
  ktime_get_real_ts64(&ts);
  *pTime = unixEpoch + 1000 * (sqlite3_int64)ts.tv_sec + ts.tv_nsec / 1000000;
  // *pTime = ts.tv_sec + ts.tv_nsec * 1e-9;  // to seconds
  return SQLITE_OK;
}

static int kern_vfs_current_time_int64(sqlite3_vfs *vfs, sqlite3_int64 *piNow) {
  static const sqlite3_int64 unixEpoch = 24405875*(sqlite3_int64)8640000;
  struct timespec64 ts;
  ktime_get_real_ts64(&ts);
  *piNow = unixEpoch + 1000 * (sqlite3_int64)ts.tv_sec + ts.tv_nsec / 1000000;
  // *pTime = ts.tv_sec + ts.tv_nsec * 1e-9;  // to seconds
  return SQLITE_OK;
}

/*
** Initialize the operating system interface.
*/
int sqlite3_os_init(void){
  return sqlite3_vfs_register(&kern_vfs, 1);
}

/*
** Deinitialize the operating system interface.
*/
int sqlite3_os_end(void){
  return SQLITE_OK;
}

#endif
