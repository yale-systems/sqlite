#include "sqliteInt.h"

#ifdef LINUX_KERNEL_BUILD

#include "os_klinux.h"

/*
** Initialize the operating system interface.
*/
int sqlite3_os_init(void){
  /* TODO: Implement FreeBSD-specific initialization here. */
  return SQLITE_OK;
}

/*
** Deinitialize the operating system interface.
*/
int sqlite3_os_end(void){
  /* TODO: Implement FreeBSD-specific deinitialization here. */
  return SQLITE_OK;
}

#endif