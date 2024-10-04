#include "sqliteInt.h"
#ifdef LINUX_KERNEL_BUILD

#include <linux/printk.h>
#include "os_klinux.h"

static sqlite3_vfs vfs = { 0 };

/*
** Initialize the operating system interface.
*/
int sqlite3_os_init(void){
  return sqlite3_vfs_register(&vfs, 1);
}

/*
** Deinitialize the operating system interface.
*/
int sqlite3_os_end(void){
  return SQLITE_OK;
}

#endif
