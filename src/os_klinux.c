#ifdef LINUX_KERNEL_BUILD

#include "os_klinux.h"

/*
** Initialize the operating system interface.
*/
int sqlite3_os_init(void){
  return SQLITE_OK;
}

/*
** Deinitialize the operating system interface.
*/
int sqlite3_os_end(void){
  return SQLITE_OK;
}

#endif
