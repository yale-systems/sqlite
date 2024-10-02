#ifdef LINUX_KERNEL_BUILD

#include <linux/delay.h>
#include <linux/fcntl.h>
#include <linux/fs.h>
#include <linux/namei.h>
#include <linux/path.h>
#include <linux/random.h>
#include <linux/string.h>

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

/*
** Specific code for Kernel VFS implementation.
*/
static int kern_vfs_open(sqlite3_vfs *vfs, sqlite3_filename zName, sqlite3_file *file,
                         int flags, int *pOutFlags) {
    struct file *fp;
    int error;
    int oflags = 0;

    // translate SQLite flags to kernel open flags.
    if (flags & SQLITE_OPEN_READWRITE) {
        oflags |= O_RDWR;
    } else if (flags & SQLITE_OPEN_READONLY) {
        oflags |= O_RDONLY;
    }
    if (flags & SQLITE_OPEN_CREATE) {
        oflags |= O_CREAT;
    }
    if (flags & SQLITE_OPEN_EXCLUSIVE) {
        oflags |= O_EXCL;
    }

    // use Linux equivalent to open a file at the kernel.
    error = filp_open(zName, oflags, S_IRUSR | S_IWUSR);
    if (IS_ERR(fp)) {
        return SQLITE_CANTOPEN;
    }

    file->pMethods = (sqlite3_io_methods *)fp;

    if (pOutFlags) {
      *pOutFlags = oflags;
    }

    return SQLITE_OK;
}

// delete file in kernelspace
static int kern_vfs_delete(sqlite3_vfs *vfs, const char *zName, int syncDir) {
    struct path *path;
    struct inode *dir;
    struct dentry *dentry;
    struct user_namespace *user_ns;
    int error;

    error = kern_path(zName, LOOKUP_FOLLOW, path);
    if (error) {
        return SQLITE_IOERR_DELETE;
    }

    dir = path->dentry->d_parent->d_inode;
    dentry = path->dentry;
    user_ns = dir->i_sb->s_user_ns;
    
    error = vfs_unlink(user_ns, dir, dentry, NULL);
    path_put(path);

    if (error) {
      return SQLITE_IOERR_DELETE;
    }

    return SQLITE_OK;
}

static int kern_vfs_access(sqlite3_vfs *vfs, const char *zName, int flags, int *pResOut) {
    struct kstat stat;
    int error;

    error = vfs_stat(zName, &stat);
    if (error) {
        *pResOut = 0;
        return SQLITE_OK;  // not finding the file is a valid response for access check.
    }

    // mapping SQLite access flags to actual permissions
    if ((flags & SQLITE_ACCESS_EXISTS) && S_ISREG(stat.mode)) {
        *pResOut = 1;
    } else if (flags & SQLITE_ACCESS_READWRITE && !(stat.mode & S_IWUSR)) {
        *pResOut = 0;
    } else {
        *pResOut = 1;  // all other checks pass.
    }

    return SQLITE_OK;
}

static int kern_vfs_full_pathname(sqlite3_vfs *vfs, const char *zName, int nOut, char *zOut) {
    strscpy(zOut, zName, nOut);
    return SQLITE_OK;
}

static void *kern_vfs_dl_open(sqlite3_vfs *vfs, const char *zFilename) {
    return NULL;  // not supported
}

static void kern_vfs_dl_error(sqlite3_vfs *vfs, int nByte, char *zErrMsg) {
    strscpy(zErrMsg, "Dynamic loading is not supported", nByte);
}

static void (*kern_vfs_dl_sym(sqlite3_vfs *vfs, void* p, const char *zSymbol))(void) {
    return NULL;  // not supported
}

static void kern_vfs_dl_close(sqlite3_vfs *vfs, void* p) {
    // do nothing
}

static int kern_vfs_randomness(sqlite3_vfs *vfs, int nByte, char *zOut) {
    get_random_bytes(zOut, nByte);
    return nByte; 
}

static int kern_vfs_sleep(sqlite3_vfs *vfs, int microseconds) {
    // Convert microseconds to milliseconds
    unsigned int ms = microseconds / 1000;
    msleep(ms);
    return SQLITE_OK;
}

static int kern_vfs_current_time(sqlite3_vfs *vfs, double *pTime) {
    struct timespec64 ts;
    ktime_get_real_ts64(&ts);
    *pTime = ts.tv_sec; //fp issue if we also include nanoseconds here
    return SQLITE_OK;
}

static int kern_vfs_get_last_error(sqlite3_vfs *vfs, int nBuf, char *zBuf) {
    if (nBuf > 0) {
      strscpy(zBuf, "kernel error occurred", nBuf);
    }
    return 0;
}

int vfs_fstatat(int dfd, const char __user *filename, struct kstat *stat,
		int flags) {
      //pass for compilation
      return 0;
    }

sqlite3_vfs kern_vfs = {
		1,                          // iVersion
		sizeof(sqlite3_file),       // szOsFile
		SQLITE_MAX_PATHLEN,         // mxPathname
		NULL,                       // pNext
		"kern_vfs",                 // zName
		NULL,                       // pAppData
		kern_vfs_open,              // xOpen
		kern_vfs_delete,            // xDelete
		kern_vfs_access,            // xAccess
		kern_vfs_full_pathname,     // xFullPathname
		kern_vfs_dl_open,           // xDlOpen
		kern_vfs_dl_error,          // xDlError
		kern_vfs_dl_sym,            // xDlSym
		kern_vfs_dl_close,          // xDlClose
		kern_vfs_randomness,        // xRandomness
		kern_vfs_sleep,            // xSleep
		kern_vfs_current_time,      // xCurrentTime
		kern_vfs_get_last_error     // xGetLastError
	};


#endif
