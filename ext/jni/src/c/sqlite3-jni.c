/*
** 2023-07-21
**
** The author disclaims copyright to this source code.  In place of
** a legal notice, here is a blessing:
**
**    May you do good and not evil.
**    May you find forgiveness for yourself and forgive others.
**    May you share freely, never taking more than you give.
**
*************************************************************************
** This file implements the JNI bindings declared in
** org.sqlite.jni.SQLiteJni (from which sqlite3-jni.h is generated).
*/

/**
   If you found this comment by searching the code for
   CallStaticObjectMethod then you're the victim of an OpenJDK bug:

   https://bugs.openjdk.org/browse/JDK-8130659

   It's known to happen with OpenJDK v8 but not with v19.

   This code does not use JNI's CallStaticObjectMethod().
*/

/*
** Define any SQLITE_... config defaults we want if they aren't
** overridden by the builder. Please keep these alphabetized.
*/

/**********************************************************************/
/* SQLITE_D... */
#ifndef SQLITE_DEFAULT_CACHE_SIZE
# define SQLITE_DEFAULT_CACHE_SIZE -16384
#endif
#if !defined(SQLITE_DEFAULT_PAGE_SIZE)
# define SQLITE_DEFAULT_PAGE_SIZE 8192
#endif
#ifndef SQLITE_DEFAULT_UNIX_VFS
# define SQLITE_DEFAULT_UNIX_VFS "unix"
#endif
#undef SQLITE_DQS
#define SQLITE_DQS 0

/**********************************************************************/
/* SQLITE_ENABLE_... */
#ifndef SQLITE_ENABLE_BYTECODE_VTAB
#  define SQLITE_ENABLE_BYTECODE_VTAB 1
#endif
#ifndef SQLITE_ENABLE_DBPAGE_VTAB
#  define SQLITE_ENABLE_DBPAGE_VTAB 1
#endif
#ifndef SQLITE_ENABLE_DBSTAT_VTAB
#  define SQLITE_ENABLE_DBSTAT_VTAB 1
#endif
#ifndef SQLITE_ENABLE_EXPLAIN_COMMENTS
#  define SQLITE_ENABLE_EXPLAIN_COMMENTS 1
#endif
#ifdef SQLITE_ENABLE_FTS5
#  ifndef SQLITE_ENABLE_FTS4
#    define SQLITE_ENABLE_FTS4 1
#  endif
#endif
#ifndef SQLITE_ENABLE_MATH_FUNCTIONS
#  define SQLITE_ENABLE_MATH_FUNCTIONS 1
#endif
#ifndef SQLITE_ENABLE_OFFSET_SQL_FUNC
#  define SQLITE_ENABLE_OFFSET_SQL_FUNC 1
#endif
#ifndef SQLITE_ENABLE_PREUPDATE_HOOK
#  define SQLITE_ENABLE_PREUPDATE_HOOK 1 /*required by session extension*/
#endif
#ifndef SQLITE_ENABLE_RTREE
#  define SQLITE_ENABLE_RTREE 1
#endif
#ifndef SQLITE_ENABLE_SESSION
#  define SQLITE_ENABLE_SESSION 1
#endif
#ifndef SQLITE_ENABLE_STMTVTAB
#  define SQLITE_ENABLE_STMTVTAB 1
#endif
#ifndef SQLITE_ENABLE_UNKNOWN_SQL_FUNCTION
#  define SQLITE_ENABLE_UNKNOWN_SQL_FUNCTION
#endif

/**********************************************************************/
/* SQLITE_M... */
#ifndef SQLITE_MAX_ALLOCATION_SIZE
# define SQLITE_MAX_ALLOCATION_SIZE 0x1fffffff
#endif

/**********************************************************************/
/* SQLITE_O... */
#ifndef SQLITE_OMIT_DEPRECATED
# define SQLITE_OMIT_DEPRECATED 1
#endif
#ifndef SQLITE_OMIT_LOAD_EXTENSION
# define SQLITE_OMIT_LOAD_EXTENSION 1
#endif
#ifndef SQLITE_OMIT_SHARED_CACHE
# define SQLITE_OMIT_SHARED_CACHE 1
#endif
#ifdef SQLITE_OMIT_UTF16
/* UTF16 is required for java */
# undef SQLITE_OMIT_UTF16 1
#endif

/**********************************************************************/
/* SQLITE_T... */
#ifndef SQLITE_TEMP_STORE
# define SQLITE_TEMP_STORE 2
#endif
#ifndef SQLITE_THREADSAFE
# define SQLITE_THREADSAFE 0
#endif

/**********************************************************************/
/* SQLITE_USE_... */
#ifndef SQLITE_USE_URI
#  define SQLITE_USE_URI 1
#endif


/*
** Which sqlite3.c we're using needs to be configurable to enable
** building against a custom copy, e.g. the SEE variant. We have to
** include sqlite3.c, as opposed to sqlite3.h, in order to get access
** to SQLITE_MAX_... and friends. This increases the rebuild time
** considerably but we need this in order to keep the exported values
** of SQLITE_MAX_... and SQLITE_LIMIT_... in sync with the C build.
*/
#ifndef SQLITE_C
# define SQLITE_C sqlite3.c
#endif
#define INC__STRINGIFY_(f) #f
#define INC__STRINGIFY(f) INC__STRINGIFY_(f)
#include INC__STRINGIFY(SQLITE_C)
#undef INC__STRINGIFY_
#undef INC__STRINGIFY
#undef SQLITE_C

#include "sqlite3-jni.h"
#include <stdio.h> /* only for testing/debugging */
#include <assert.h>

/* Only for debugging */
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)

/* Creates a verbose JNI function name. */
#define JFuncName(Suffix) \
  Java_org_sqlite_jni_SQLite3Jni_sqlite3_ ## Suffix

/* Prologue for JNI functions. */
#define JDECL(ReturnType,Suffix)                \
  JNIEXPORT ReturnType JNICALL                  \
  JFuncName(Suffix)
/* First 2 parameters to all JNI bindings. */
#define JENV_JSELF JNIEnv * const env, jobject jSelf
/* Helpers to squelch -Xcheck:jni warnings about
   not having checked for exceptions. */
#define IFTHREW if((*env)->ExceptionCheck(env))
#define EXCEPTION_IGNORE (void)((*env)->ExceptionCheck(env))
#define EXCEPTION_CLEAR (*env)->ExceptionClear(env)
#define EXCEPTION_REPORT (*env)->ExceptionDescribe(env)
#define EXCEPTION_WARN_CALLBACK_THREW(STR)             \
  MARKER(("WARNING: " STR " MUST NOT THROW.\n"));  \
  (*env)->ExceptionDescribe(env)
#define IFTHREW_REPORT IFTHREW EXCEPTION_REPORT
#define IFTHREW_CLEAR IFTHREW EXCEPTION_CLEAR

/** To be used for cases where we're _really_ not expecting an
    exception, e.g. looking up well-defined Java class members. */
#define EXCEPTION_IS_FATAL(MSG) IFTHREW {\
    EXCEPTION_REPORT; EXCEPTION_CLEAR; \
    (*env)->FatalError(env, MSG); \
  }

/** Helpers for extracting pointers from jobjects, noting that the
    corresponding Java interfaces have already done the type-checking.
 */
#define PtrGet_sqlite3(OBJ) getNativePointer(env,OBJ,S3ClassNames.sqlite3)
#define PtrGet_sqlite3_stmt(OBJ) getNativePointer(env,OBJ,S3ClassNames.sqlite3_stmt)
#define PtrGet_sqlite3_value(OBJ) getNativePointer(env,OBJ,S3ClassNames.sqlite3_value)
#define PtrGet_sqlite3_context(OBJ) getNativePointer(env,OBJ,S3ClassNames.sqlite3_context)
/* Helpers for Java value reference management. */
#define REF_G(VAR) (*env)->NewGlobalRef(env, VAR)
#define REF_L(VAR) (*env)->NewLocalRef(env, VAR)
#define UNREF_G(VAR) if(VAR) (*env)->DeleteGlobalRef(env, (VAR))
#define UNREF_L(VAR) if(VAR) (*env)->DeleteLocalRef(env, (VAR))

/**
   Constant string class names used as keys for S3Global_nph_cache() and
   friends.
*/
static const struct {
  const char * const sqlite3;
  const char * const sqlite3_stmt;
  const char * const sqlite3_context;
  const char * const sqlite3_value;
  const char * const OutputPointer_Int32;
  const char * const OutputPointer_Int64;
#ifdef SQLITE_ENABLE_FTS5
  const char * const Fts5Context;
  const char * const Fts5ExtensionApi;
  const char * const fts5_api;
#endif
} S3ClassNames = {
  "org/sqlite/jni/sqlite3",
  "org/sqlite/jni/sqlite3_stmt",
  "org/sqlite/jni/sqlite3_context",
  "org/sqlite/jni/sqlite3_value",
  "org/sqlite/jni/OutputPointer$Int32",
  "org/sqlite/jni/OutputPointer$Int64",
#ifdef SQLITE_ENABLE_FTS5
  "org/sqlite/jni/Fts5Context",
  "org/sqlite/jni/Fts5ExtensionApi",
  "org/sqlite/jni/fts5_api"
#endif
};

/** Create a trivial JNI wrapper for (int CName(void)). */
#define WRAP_INT_VOID(JniNameSuffix,CName) \
  JDECL(jint,JniNameSuffix)(JNIEnv *env, jobject jSelf){ \
    return (jint)CName(); \
  }

/** Create a trivial JNI wrapper for (int CName(int)). */
#define WRAP_INT_INT(JniNameSuffix,CName)               \
  JDECL(jint,JniNameSuffix)(JNIEnv *env, jobject jSelf, jint arg){   \
    return (jint)CName((int)arg);                                    \
  }

/** Create a trivial JNI wrapper for (const mutf8_string *
    CName(void)). This is only valid for functions which are known to
    return ASCII or text which is equivalent in UTF-8 and MUTF-8. */
#define WRAP_MUTF8_VOID(JniNameSuffix,CName)                             \
  JDECL(jstring,JniNameSuffix)(JENV_JSELF){                  \
    return (*env)->NewStringUTF( env, CName() );                        \
  }
/** Create a trivial JNI wrapper for (int CName(sqlite3_stmt*)). */
#define WRAP_INT_STMT(JniNameSuffix,CName) \
  JDECL(jint,JniNameSuffix)(JENV_JSELF, jobject jpStmt){   \
    jint const rc = (jint)CName(PtrGet_sqlite3_stmt(jpStmt)); \
    EXCEPTION_IGNORE /* squelch -Xcheck:jni */;        \
    return rc; \
  }
/** Create a trivial JNI wrapper for (int CName(sqlite3_stmt*,int)). */
#define WRAP_INT_STMT_INT(JniNameSuffix,CName) \
  JDECL(jint,JniNameSuffix)(JENV_JSELF, jobject pStmt, jint n){ \
    return (jint)CName(PtrGet_sqlite3_stmt(pStmt), (int)n);                          \
  }
/** Create a trivial JNI wrapper for (jstring CName(sqlite3_stmt*,int)). */
#define WRAP_STR_STMT_INT(JniNameSuffix,CName) \
  JDECL(jstring,JniNameSuffix)(JENV_JSELF, jobject pStmt, jint ndx){ \
    return (*env)->NewStringUTF(env, CName(PtrGet_sqlite3_stmt(pStmt), (int)ndx));  \
  }
/** Create a trivial JNI wrapper for (int CName(sqlite3*)). */
#define WRAP_INT_DB(JniNameSuffix,CName) \
  JDECL(jint,JniNameSuffix)(JENV_JSELF, jobject pDb){   \
    return (jint)CName(PtrGet_sqlite3(pDb)); \
  }
/** Create a trivial JNI wrapper for (int64 CName(sqlite3*)). */
#define WRAP_INT64_DB(JniNameSuffix,CName) \
  JDECL(jlong,JniNameSuffix)(JENV_JSELF, jobject pDb){   \
    return (jlong)CName(PtrGet_sqlite3(pDb)); \
  }
/** Create a trivial JNI wrapper for (int CName(sqlite3_value*)). */
#define WRAP_INT_SVALUE(JniNameSuffix,CName) \
  JDECL(jint,JniNameSuffix)(JENV_JSELF, jobject jpSValue){   \
    return (jint)CName(PtrGet_sqlite3_value(jpSValue)); \
  }

/* Helpers for jstring and jbyteArray. */
#define JSTR_TOC(ARG) (*env)->GetStringUTFChars(env, ARG, NULL)
#define JSTR_RELEASE(ARG,VAR) if(VAR) (*env)->ReleaseStringUTFChars(env, ARG, VAR)
#define JBA_TOC(ARG) (*env)->GetByteArrayElements(env,ARG, NULL)
#define JBA_RELEASE(ARG,VAR) (*env)->ReleaseByteArrayElements(env, ARG, VAR, JNI_ABORT)

/* Marker for code which needs(?) to be made thread-safe. */
#define FIXME_THREADING

enum {
  /**
     Size of the per-JNIEnv cache. We have no way of knowing how many
     distinct JNIEnv's will be used in any given run, but know that it
     will normally be only 1. Perhaps (just speculating) different
     threads use separate JNIEnvs? If that's the case, we don't(?)
     have enough info to evict from the cache when those JNIEnvs
     expire.

     If this ever fills up, we can refactor this to dynamically
     allocate them.
  */
  JNIEnvCache_SIZE = 10,
  /**
    Need enough space for (only) the library's NativePointerHolder
    types, a fixed count known at build-time. If we add more than this
    a fatal error will be triggered with a reminder to increase this.
    This value needs to be, at most, the number of entries in the
    S3ClassNames object, as that value is our upper limit. The
    S3ClassNames entries are the keys for this particular cache.
  */
  NphCache_SIZE = sizeof(S3ClassNames) / sizeof(char const *)
};

/**
   Cache entry for NativePointerHolder lookups.
*/
typedef struct NphCacheLine NphCacheLine;
struct NphCacheLine {
  const char * zClassName /* "full/class/Name". Must be a static string
                             from the S3ClassNames struct. */;
  jclass klazz        /* global ref to concrete NativePointerHolder class */;
  jmethodID midCtor   /* klazz's constructor */;
  jfieldID fidValue   /* NativePointerHolder.nativePointer and OutputPointer.X.value */;
  jfieldID fidSetAgg  /* sqlite3_context::aggregateContext */;
};

/**
   Cache for per-JNIEnv data.
*/
typedef struct JNIEnvCacheLine JNIEnvCacheLine;
struct JNIEnvCacheLine {
  JNIEnv *env            /* env in which this cache entry was created */;
  jclass globalClassObj  /* global ref to java.lang.Object */;
  jclass globalClassLong /* global ref to java.lang.Long */;
  jmethodID ctorLong1    /* the Long(long) constructor */;
  jobject currentStmt    /* Current Java sqlite3_stmt object being
                            prepared, stepped, reset, or
                            finalized. Needed for tracing, the
                            alternative being that we create a new
                            sqlite3_stmt wrapper object for every
                            tracing call which needs a stmt
                            object. This approach is rather invasive,
                            however, requiring code in all stmt
                            operations which can lead through the
                            tracing API. */;
#ifdef SQLITE_ENABLE_FTS5
  jobject jFtsExt       /* Global ref to Java singleton for the
                           Fts5ExtensionApi instance. */;
#endif
#if 0
  /* TODO: refactor this cache as a linked list with malloc()'d entries,
     rather than a fixed-size array in S3Global.envCache */
  JNIEnvCacheLine * pPrev /* Previous entry in the linked list */;
  JNIEnvCacheLine * pNext /* Next entry in the linked list */;
#endif
  struct NphCacheLine nph[NphCache_SIZE];
};
typedef struct JNIEnvCache JNIEnvCache;
struct JNIEnvCache {
  struct JNIEnvCacheLine lines[JNIEnvCache_SIZE];
  unsigned int used;
};

static void NphCacheLine_clear(JNIEnv * const env, NphCacheLine * const p){
  UNREF_G(p->klazz);
  memset(p, 0, sizeof(NphCacheLine));
}

static void JNIEnvCacheLine_clear(JNIEnvCacheLine * const p){
  JNIEnv * const env = p->env;
  int i;
  if(env){
    UNREF_G(p->globalClassObj);
    UNREF_G(p->globalClassLong);
#ifdef SQLITE_ENABLE_FTS5
    UNREF_G(p->jFtsExt);
#endif
    i = 0;
    for( ; i < NphCache_SIZE; ++i){
      NphCacheLine_clear(env, &p->nph[i]);
    }
  }
}

static void JNIEnvCache_clear(JNIEnvCache * const p){
  unsigned int i = 0;
  for( ; i < p->used; ++i ){
    JNIEnvCacheLine_clear( &p->lines[i] );
  }
  memset(p, 0, sizeof(JNIEnvCache));
}

/** State for various hook callbacks. */
typedef struct JniHookState JniHookState;
struct JniHookState{
  jobject jObj            /* global ref to Java instance */;
  jmethodID midCallback   /* callback method. Signature depends on
                             jObj's type */;
  jclass klazz            /* global ref to jObj's class. Only needed
                             by hooks which have an xDestroy() method,
                             as lookup of that method is deferred
                             until the object requires cleanup. */;
};

/**
   Per-(sqlite3*) state for various JNI bindings.  This state is
   allocated as needed, cleaned up in sqlite3_close(_v2)(), and
   recycled when possible. It is freed during sqlite3_shutdown().
*/
typedef struct PerDbStateJni PerDbStateJni;
struct PerDbStateJni {
  JNIEnv *env   /* The associated JNIEnv handle */;
  sqlite3 *pDb  /* The associated db handle */;
  jobject jDb   /* A global ref of the object which was passed to
                   sqlite3_open(_v2)(). We need this in order to have
                   an object to pass to sqlite3_collation_needed()'s
                   callback, or else we have to dynamically create one
                   for that purpose, which would be fine except that
                   it would be a different instance (and maybe even a
                   different class) than the one the user may expect
                   to receive. */;
#ifdef SQLITE_ENABLE_FTS5
  jobject jFtsApi /* global ref to fts5_api object for the db. */;
#endif
  JniHookState busyHandler;
  JniHookState collation;
  JniHookState collationNeeded;
  JniHookState commitHook;
  JniHookState progress;
  JniHookState rollbackHook;
  JniHookState trace;
  JniHookState updateHook;
  PerDbStateJni * pNext /* Next entry in the available/free list */;
  PerDbStateJni * pPrev /* Previous entry in the available/free list */;
};

static struct {
  /**
     According to: https://developer.ibm.com/articles/j-jni/

     > A thread can get a JNIEnv by calling GetEnv() using the JNI
       invocation interface through a JavaVM object. The JavaVM object
       itself can be obtained by calling the JNI GetJavaVM() method
       using a JNIEnv object and can be cached and shared across
       threads. Caching a copy of the JavaVM object enables any thread
       with access to the cached object to get access to its own
       JNIEnv when necessary.
  */
  JavaVM * jvm;
  struct JNIEnvCache envCache;
  struct {
    PerDbStateJni * aUsed  /* Linked list of in-use instances */;
    PerDbStateJni * aFree  /* Linked list of free instances */;
  } perDb;
  struct {
    unsigned nphCacheHits;
    unsigned nphCacheMisses;
    unsigned envCacheHits;
    unsigned envCacheMisses;
    unsigned nDestroy        /* xDestroy() calls across all types */;
    struct {
      /* Number of calls for each type of UDF callback. */
      unsigned nFunc;
      unsigned nStep;
      unsigned nFinal;
      unsigned nValue;
      unsigned nInverse;
    } udf;
  } metrics;
} S3Global;

/**
   sqlite3_malloc() proxy which fails fatally on OOM.  This should
   only be used for routines which manage global state and have no
   recovery strategy for OOM. For sqlite3 API which can reasonably
   return SQLITE_NOMEM, sqlite3_malloc() should be used instead.
*/
static void * s3jni_malloc(JNIEnv * const env, size_t n){
  void * const rv = sqlite3_malloc(n);
  if(n && !rv){
    (*env)->FatalError(env, "Out of memory.") /* does not return */;
  }
  return rv;
}

static void s3jni_free(void * const p){
  if(p) sqlite3_free(p);
}


/*
** This function is NOT part of the sqlite3 public API. It is strictly
** for use by the sqlite project's own Java/JNI bindings.
**
** For purposes of certain hand-crafted JNI function bindings, we
** need a way of reporting errors which is consistent with the rest of
** the C API, as opposed to throwing JS exceptions. To that end, this
** internal-use-only function is a thin proxy around
** sqlite3ErrorWithMessage(). The intent is that it only be used from
** JNI bindings such as sqlite3_prepare_v2/v3(), and definitely not
** from client code.
**
** Returns err_code.
*/
static int s3jni_db_error(sqlite3* const db, int err_code, const char * const zMsg){
  if( db!=0 ){
    if( 0==zMsg ){
      sqlite3Error(db, err_code);
    }else{
      const int nMsg = sqlite3Strlen30(zMsg);
      sqlite3ErrorWithMsg(db, err_code, "%.*s", nMsg, zMsg);
    }
  }
  return err_code;
}

/**
   Extracts the (void xDestroy()) method from the given jclass and
   applies it to jobj. If jObj is NULL, this is a no-op. If klazz is
   NULL then it's derived from jobj. The lack of an xDestroy() method
   is silently ignored and any exceptions thrown by the method trigger
   a warning to stdout or stderr and then the exception is suppressed.
*/
static void s3jni_call_xDestroy(JNIEnv * const env, jobject jObj, jclass klazz){
  if(jObj){
    jmethodID method;
    if(!klazz){
      klazz = (*env)->GetObjectClass(env, jObj);
      assert(klazz);
    }
    method = (*env)->GetMethodID(env, klazz, "xDestroy", "()V");
    //MARKER(("jObj=%p, klazz=%p, method=%p\n", jObj, klazz, method));
    if(method){
      ++S3Global.metrics.nDestroy;
      (*env)->CallVoidMethod(env, jObj, method);
      IFTHREW{
        EXCEPTION_WARN_CALLBACK_THREW("xDestroy() callback");
        EXCEPTION_CLEAR;
      }
    }else{
      EXCEPTION_CLEAR;
    }
  }
}

/**
   Fetches the S3Global.envCache row for the given env, allocing
   a row if needed. When a row is allocated, its state is initialized
   insofar as possible. Calls (*env)->FatalError() if the cache
   fills up. That's hypothetically possible but "shouldn't happen."
   If it does, we can dynamically allocate these instead.
*/
FIXME_THREADING
static struct JNIEnvCacheLine * S3Global_env_cache(JNIEnv * const env){
  struct JNIEnvCacheLine * row = 0;
  int i = 0;
  for( ; i < JNIEnvCache_SIZE; ++i ){
    row = &S3Global.envCache.lines[i];
    if(row->env == env){
      ++S3Global.metrics.envCacheHits;
      return row;
    }
    else if(!row->env) break;
  }
  ++S3Global.metrics.envCacheMisses;
  if(i == JNIEnvCache_SIZE){
    (*env)->FatalError(env, "Maintenance required: JNIEnvCache is full.");
    return NULL;
  }
  memset(row, 0, sizeof(JNIEnvCacheLine));
  row->env = env;
  row->globalClassObj = REF_G((*env)->FindClass(env,"java/lang/Object"));
  row->globalClassLong = REF_G((*env)->FindClass(env,"java/lang/Long"));
  row->ctorLong1 = (*env)->GetMethodID(env, row->globalClassLong,
                                       "<init>", "(J)V");
  ++S3Global.envCache.used;
  //MARKER(("Added S3Global.envCache entry #%d.\n", S3Global.envCache.used));
  return row;
}

/**
   Searches the NativePointerHolder cache for the given combination.
   If it finds one, it returns it as-is. If it doesn't AND the cache
   has a free slot, it populates that slot with (env, zClassName,
   klazz) and returns it. If the cache is full with no match it
   returns NULL.

   It is up to the caller to populate the other members of the returned
   object if needed.

   zClassName must be a static string so we can use its address as a
   cache key.

   This simple cache catches >99% of searches in the current
   (2023-07-31) tests.
*/
FIXME_THREADING
static struct NphCacheLine * S3Global_nph_cache(JNIEnv * const env, const char *zClassName){
  /**
     According to:

     https://developer.ibm.com/articles/j-jni/

     > ... the IDs returned for a given class don't change for the
     lifetime of the JVM process. But the call to get the field or
     method can require significant work in the JVM, because
     fields and methods might have been inherited from
     superclasses, making the JVM walk up the class hierarchy to
     find them. Because the IDs are the same for a given class,
     you should look them up once and then reuse them. Similarly,
     looking up class objects can be expensive, so they should be
     cached as well.
  */
  struct JNIEnvCacheLine * const envRow = S3Global_env_cache(env);
  struct NphCacheLine * freeSlot = 0;
  struct NphCacheLine * cacheLine = 0;
  int i;
  assert(envRow);
  for( i = 0; i < NphCache_SIZE; ++i ){
    cacheLine = &envRow->nph[i];
    if(zClassName == cacheLine->zClassName){
      ++S3Global.metrics.nphCacheHits;
#define DUMP_NPH_CACHES 0
#if DUMP_NPH_CACHES
      MARKER(("Cache hit #%u %s klazz@%p nativePointer field@%p, ctor@%p\n",
              S3Global.metrics.nphCacheHits, zClassName, cacheLine->klazz, cacheLine->fidValue,
              cacheLine->midCtor));
#endif
      assert(cacheLine->klazz);
      return cacheLine;
    }else if(!freeSlot && !cacheLine->zClassName){
      freeSlot = cacheLine;
    }
  }
  if(freeSlot){
    freeSlot->zClassName = zClassName;
    freeSlot->klazz = (*env)->FindClass(env, zClassName);
    EXCEPTION_IS_FATAL("FindClass() unexpectedly threw");
    freeSlot->klazz = REF_G(freeSlot->klazz);
    ++S3Global.metrics.nphCacheMisses;
#if DUMP_NPH_CACHES
    static unsigned int cacheMisses = 0;
    MARKER(("Cache miss #%u %s klazz@%p nativePointer field@%p, ctor@%p\n",
            S3Global.metrics.nphCacheMisses, zClassName, freeSlot->klazz,
            freeSlot->fidValue, freeSlot->midCtor));
#endif
#undef DUMP_NPH_CACHES
  }else{
    (*env)->FatalError(env, "MAINTENANCE REQUIRED: NphCache_SIZE is too low.");
  }
  return freeSlot;
}

static jfieldID getNativePointerField(JNIEnv * const env, jclass klazz){
  jfieldID rv = (*env)->GetFieldID(env, klazz, "nativePointer", "J");
  EXCEPTION_IS_FATAL("Code maintenance required: missing nativePointer field.");
  return rv;
}

/**
   Sets a native ptr value in NativePointerHolder object ppOut.
   zClassName must be a static string so we can use its address
   as a cache key.
*/
static void setNativePointer(JNIEnv * env, jobject ppOut, const void * p,
                             const char *zClassName){
  jfieldID setter = 0;
  struct NphCacheLine * const cacheLine = S3Global_nph_cache(env, zClassName);
  if(cacheLine && cacheLine->klazz && cacheLine->fidValue){
    assert(zClassName == cacheLine->zClassName);
    setter = cacheLine->fidValue;
    assert(setter);
  }else{
    jclass const klazz =
      cacheLine ? cacheLine->klazz : (*env)->GetObjectClass(env, ppOut);
    setter = getNativePointerField(env, klazz);
    if(cacheLine){
      assert(cacheLine->klazz);
      assert(!cacheLine->fidValue);
      assert(zClassName == cacheLine->zClassName);
      cacheLine->fidValue = setter;
    }
  }
  (*env)->SetLongField(env, ppOut, setter, (jlong)p);
  EXCEPTION_IS_FATAL("Could not set NativePointerHolder.nativePointer.");
}

/**
   Fetches a native ptr value from NativePointerHolder object ppOut.
   zClassName must be a static string so we can use its address as a
   cache key.
*/
static void * getNativePointer(JNIEnv * env, jobject pObj, const char *zClassName){
  if( 0==pObj ) return 0;
  else{
    jfieldID getter = 0;
    void * rv = 0;
    struct NphCacheLine * const cacheLine = S3Global_nph_cache(env, zClassName);
    if(cacheLine && cacheLine->fidValue){
      getter = cacheLine->fidValue;
    }else{
      jclass const klazz =
        cacheLine ? cacheLine->klazz : (*env)->GetObjectClass(env, pObj);
      getter = getNativePointerField(env, klazz);
      if(cacheLine){
        assert(cacheLine->klazz);
        assert(zClassName == cacheLine->zClassName);
        cacheLine->fidValue = getter;
      }
    }
    rv = (void*)(*env)->GetLongField(env, pObj, getter);
    IFTHREW_REPORT;
    return rv;
  }
}

/**
   Extracts the new PerDbStateJni instance from the free-list, or
   allocates one if needed, associats it with pDb, and returns.
   Returns NULL on OOM. pDb MUST be associated with jDb via
   setNativePointer().
*/
static PerDbStateJni * PerDbStateJni_alloc(JNIEnv * const env, sqlite3 *pDb, jobject jDb){
  PerDbStateJni * rv;
  assert( pDb );
  if(S3Global.perDb.aFree){
    rv = S3Global.perDb.aFree;
    //MARKER(("state@%p for db allocating for db@%p from free-list\n", rv, pDb));
    //MARKER(("%p->pPrev@%p, pNext@%p\n", rv, rv->pPrev, rv->pNext));
    S3Global.perDb.aFree = rv->pNext;
    assert(rv->pNext != rv);
    assert(rv->pPrev != rv);
    assert(rv->pPrev ? (rv->pPrev!=rv->pNext) : 1);
    if(rv->pNext){
      assert(rv->pNext->pPrev == rv);
      assert(rv->pPrev ? (rv->pNext == rv->pPrev->pNext) : 1);
      rv->pNext->pPrev = 0;
      rv->pNext = 0;
    }
  }else{
    rv = s3jni_malloc(env, sizeof(PerDbStateJni));
    //MARKER(("state@%p for db allocating for db@%p from heap\n", rv, pDb));
    if(rv){
      memset(rv, 0, sizeof(PerDbStateJni));
    }
  }
  if(rv){
    rv->pNext = S3Global.perDb.aUsed;
    S3Global.perDb.aUsed = rv;
    if(rv->pNext){
      assert(!rv->pNext->pPrev);
      rv->pNext->pPrev = rv;
    }
    rv->jDb = REF_G(jDb);
    rv->pDb = pDb;
    rv->env = env;
  }
  return rv;
}

/**
   Removes any Java references from s and clears its state. If
   doXDestroy is true and s->klazz and s->jObj are not NULL, s->jObj's
   s is passed to s3jni_call_xDestroy() before any references are
   cleared. It is legal to call this when the object has no Java
   references.
*/
static void JniHookState_unref(JNIEnv * const env, JniHookState * const s, int doXDestroy){
  if(doXDestroy && s->klazz && s->jObj){
    s3jni_call_xDestroy(env, s->jObj, s->klazz);
  }
  UNREF_G(s->jObj);
  UNREF_G(s->klazz);
  s->jObj = 0;
  s->klazz = 0;
  s->midCallback = 0;
}

/**
   Clears s's state and moves it to the free-list.
*/
FIXME_THREADING
static void PerDbStateJni_set_aside(PerDbStateJni * const s){
  if(s){
    JNIEnv * const env = s->env;
    assert(s->pDb && "Else this object is already in the free-list.");
    //MARKER(("state@%p for db@%p setting aside\n", s, s->pDb));
    assert(s->pPrev != s);
    assert(s->pNext != s);
    assert(s->pPrev ? (s->pPrev!=s->pNext) : 1);
    if(s->pNext) s->pNext->pPrev = s->pPrev;
    if(s->pPrev) s->pPrev->pNext = s->pNext;
    else if(S3Global.perDb.aUsed == s){
      assert(!s->pPrev);
      S3Global.perDb.aUsed = s->pNext;
    }
#define UNHOOK(MEMBER,XDESTROY) JniHookState_unref(env, &s->MEMBER, XDESTROY)
    UNHOOK(trace, 0);
    UNHOOK(progress, 0);
    UNHOOK(commitHook, 0);
    UNHOOK(rollbackHook, 0);
    UNHOOK(updateHook, 0);
    UNHOOK(collation, 1);
    UNHOOK(collationNeeded, 1);
    UNHOOK(busyHandler, 1);
#undef UNHOOK
    UNREF_G(s->jDb);
#ifdef SQLITE_ENABLE_FTS5
    UNREF_G(s->jFtsApi);
#endif
    memset(s, 0, sizeof(PerDbStateJni));
    s->pNext = S3Global.perDb.aFree;
    if(s->pNext) s->pNext->pPrev = s;
    S3Global.perDb.aFree = s;
    //MARKER(("%p->pPrev@%p, pNext@%p\n", s, s->pPrev, s->pNext));
    //if(s->pNext) MARKER(("next: %p->pPrev@%p\n", s->pNext, s->pNext->pPrev));
  }
}

static void PerDbStateJni_dump(PerDbStateJni *s){
  MARKER(("PerDbStateJni->env @ %p\n", s->env));
  MARKER(("PerDbStateJni->pDb @ %p\n", s->pDb));
  MARKER(("PerDbStateJni->trace.jObj @ %p\n", s->trace.jObj));
  MARKER(("PerDbStateJni->progress.jObj @ %p\n", s->progress.jObj));
  MARKER(("PerDbStateJni->commitHook.jObj @ %p\n", s->commitHook.jObj));
  MARKER(("PerDbStateJni->rollbackHook.jObj @ %p\n", s->rollbackHook.jObj));
  MARKER(("PerDbStateJni->busyHandler.jObj @ %p\n", s->busyHandler.jObj));
  MARKER(("PerDbStateJni->env @ %p\n", s->env));
}

/**
   Returns the PerDbStateJni object for the given db. If allocIfNeeded is
   true then a new instance will be allocated if no mapping currently
   exists, else NULL is returned if no mapping is found.

   The 3rd and 4th args should normally only be non-0 for
   sqlite3_open(_v2)(). For most other cases, they must be 0, in which
   case the db handle will be fished out of the jDb object and NULL is
   returned if jDb does not have any associated PerDbStateJni.

   If called with a NULL jDb and non-NULL pDb then allocIfNeeded MUST
   be false and it will look for a matching db object. That usage is
   required for functions, like sqlite3_context_db_handle(), which
   return a (sqlite3*) but do not take one as an argument.
*/
FIXME_THREADING
static PerDbStateJni * PerDbStateJni_for_db(JNIEnv * const env, jobject jDb, sqlite3 *pDb, int allocIfNeeded){
  PerDbStateJni * s = S3Global.perDb.aUsed;
  if(!jDb){
    if(pDb){
      assert(!allocIfNeeded);
    }else{
      return 0;
    }
  }
  assert(allocIfNeeded ? !!pDb : 1);
  if(!allocIfNeeded && !pDb){
    pDb = PtrGet_sqlite3(jDb);
  }
  for( ; pDb && s; s = s->pNext){
    if(s->pDb == pDb) return s;
  }
  if(allocIfNeeded){
    s = PerDbStateJni_alloc(env, pDb, jDb);
  }
  return s;
}

/**
   Cleans up and frees all state in S3Global.perDb.
*/
FIXME_THREADING
static void PerDbStateJni_free_all(void){
  PerDbStateJni * ps = S3Global.perDb.aUsed;
  PerDbStateJni * pSNext = 0;
  for( ; ps; ps = pSNext ){
    pSNext = ps->pNext;
    PerDbStateJni_set_aside(ps);
    assert(pSNext ? !pSNext->pPrev : 1);
  }
  assert( 0==S3Global.perDb.aUsed );
  ps = S3Global.perDb.aFree;
  S3Global.perDb.aFree = 0;
  pSNext = 0;
  for( ; ps; ps = pSNext ){
    pSNext = ps->pNext;
    s3jni_free(pSNext);
  }
}


/**
   Requires that jCx be a Java-side sqlite3_context wrapper for pCx.
   This function calls sqlite3_aggregate_context() to allocate a tiny
   sliver of memory, the address of which is set in
   jCx->aggregateContext.  The memory is only used as a key for
   mapping client-side results of aggregate result sets across
   calls to the UDF's callbacks.

   isFinal must be 1 for xFinal() calls and 0 for all others, the
   difference being that the xFinal() invocation will not allocate
   new memory if it was not already, resulting in a value of 0
   for jCx->aggregateContext.

   Returns 0 on success. Returns SQLITE_NOMEM on allocation error,
   noting that it will not allocate when isFinal is true. It returns
   SQLITE_ERROR if there's a serious internal error in dealing with
   the JNI state.
*/
static int udf_setAggregateContext(JNIEnv * env, jobject jCx,
                                   sqlite3_context * pCx,
                                   int isFinal){
  jfieldID member;
  void * pAgg;
  int rc = 0;
  struct NphCacheLine * const cacheLine =
    S3Global_nph_cache(env, S3ClassNames.sqlite3_context);
  if(cacheLine && cacheLine->klazz && cacheLine->fidSetAgg){
    member = cacheLine->fidSetAgg;
    assert(member);
  }else{
    jclass const klazz =
      cacheLine ? cacheLine->klazz : (*env)->GetObjectClass(env, jCx);
    member = (*env)->GetFieldID(env, klazz, "aggregateContext", "J");
    if( !member ){
      IFTHREW{ EXCEPTION_REPORT; EXCEPTION_CLEAR; }
      return s3jni_db_error(sqlite3_context_db_handle(pCx),
                            SQLITE_ERROR,
                            "Internal error: cannot find "
                            "sqlite3_context::aggregateContext field.");
    }
    if(cacheLine){
      assert(cacheLine->klazz);
      assert(!cacheLine->fidSetAgg);
      cacheLine->fidSetAgg = member;
    }
  }
  pAgg = sqlite3_aggregate_context(pCx, isFinal ? 0 : 4);
  if( pAgg || isFinal ){
    (*env)->SetLongField(env, jCx, member, (jlong)pAgg);
  }else{
    assert(!pAgg);
    rc = SQLITE_NOMEM;
  }
  return rc;
}

/* Sets a native int32 value in OutputPointer.Int32 object jOut. */
static void setOutputInt32(JNIEnv * env, jobject jOut, int v){
  jfieldID setter = 0;
  struct NphCacheLine * const cacheLine =
    S3Global_nph_cache(env, S3ClassNames.OutputPointer_Int32);
  if(cacheLine && cacheLine->klazz && cacheLine->fidValue){
    setter = cacheLine->fidValue;
  }else{
    const jclass klazz = (*env)->GetObjectClass(env, jOut);
    setter = (*env)->GetFieldID(env, klazz, "value", "I");
    if(cacheLine){
      assert(!cacheLine->fidValue);
      cacheLine->fidValue = setter;
    }
  }
  (*env)->SetIntField(env, jOut, setter, (jint)v);
  EXCEPTION_IS_FATAL("Cannot set OutputPointer.Int32.value");
}

#ifdef SQLITE_ENABLE_FTS5
/* Sets a native int64 value in OutputPointer.Int64 object jOut. */
static void setOutputInt64(JNIEnv * env, jobject jOut, jlong v){
  jfieldID setter = 0;
  struct NphCacheLine * const cacheLine =
    S3Global_nph_cache(env, S3ClassNames.OutputPointer_Int64);
  if(cacheLine && cacheLine->klazz && cacheLine->fidValue){
    setter = cacheLine->fidValue;
  }else{
    const jclass klazz = (*env)->GetObjectClass(env, jOut);
    setter = (*env)->GetFieldID(env, klazz, "value", "I");
    if(cacheLine){
      assert(!cacheLine->fidValue);
      cacheLine->fidValue = setter;
    }
  }
  (*env)->SetIntField(env, jOut, setter, (jint)v);
  EXCEPTION_IS_FATAL("Cannot set OutputPointer.Int64.value");
}
#endif /* SQLITE_ENABLE_FTS5 */

static int encodingTypeIsValid(int eTextRep){
  switch(eTextRep){
    case SQLITE_UTF8: case SQLITE_UTF16:
    case SQLITE_UTF16LE: case SQLITE_UTF16BE:
      return 1;
    default:
      return 0;
  }
}

static int CollationState_xCompare(void *pArg, int nLhs, const void *lhs,
                                   int nRhs, const void *rhs){
  PerDbStateJni * const ps = pArg;
  JNIEnv * env = ps->env;
  jint rc = 0;
  jbyteArray jbaLhs = (*env)->NewByteArray(env, (jint)nLhs);
  jbyteArray jbaRhs = jbaLhs ? (*env)->NewByteArray(env, (jint)nRhs) : NULL;
  //MARKER(("native xCompare nLhs=%d nRhs=%d\n", nLhs, nRhs));
  if(!jbaRhs){
    (*env)->FatalError(env, "Out of memory. Cannot allocate arrays for collation.");
  }
  (*env)->SetByteArrayRegion(env, jbaLhs, 0, (jint)nLhs, (const jbyte*)lhs);
  (*env)->SetByteArrayRegion(env, jbaRhs, 0, (jint)nRhs, (const jbyte*)rhs);
  rc = (*env)->CallIntMethod(env, ps->collation.jObj, ps->collation.midCallback,
                             jbaLhs, jbaRhs);
  EXCEPTION_IGNORE;
  UNREF_L(jbaLhs);
  UNREF_L(jbaRhs);
  return (int)rc;
}

/* Collation finalizer for use by the sqlite3 internals. */
static void CollationState_xDestroy(void *pArg){
  PerDbStateJni * const ps = pArg;
  JniHookState_unref( ps->env, &ps->collation, 1 );
}

/* State for sqlite3_result_java_object() and
   sqlite3_value_java_object(). */
typedef struct {
  /* The JNI docs say:

     https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/design.html

     > The VM is guaranteed to pass the same interface pointer to a
       native method when it makes multiple calls to the native method
       from the same Java thread.

     Per the accompanying diagram, the "interface pointer" is the
     pointer-to-pointer which is passed to all JNI calls
     (`JNIEnv *env`), implying that we need to be caching that. The
     verbiage "interface pointer" implies, however, that we should be
     storing the dereferenced `(*env)` pointer.

     This posts claims it's unsafe to cache JNIEnv at all, even when
     it's always used in the same thread:

     https://stackoverflow.com/questions/12420463

     And this one seems to contradict that:

     https://stackoverflow.com/questions/13964608

     For later reference:

     https://docs.oracle.com/javase/1.5.0/docs/guide/jni/spec/design.html#wp1242

     https://developer.android.com/training/articles/perf-jni

     The later has the following say about caching:

     > If performance is important, it's useful to look the
       [class/method ID] values up once and cache the results in your
       native code. Because there is a limit of one JavaVM per
       process, it's reasonable to store this data in a static local
       structure. ... The class references, field IDs, and method IDs
       are guaranteed valid until the class is unloaded. Classes are
       only unloaded if all classes associated with a ClassLoader can
       be garbage collected, which is rare but will not be impossible
       in Android. Note however that the jclass is a class reference
       and must be protected with a call to NewGlobalRef (see the next
       section).
  */
  JNIEnv * env;
  jobject jObj;
} ResultJavaVal;

/* For use with sqlite3_result/value_pointer() */
#define RESULT_JAVA_VAL_STRING "ResultJavaVal"

static ResultJavaVal * ResultJavaVal_alloc(JNIEnv * const env, jobject jObj){
  ResultJavaVal * rv = sqlite3_malloc(sizeof(ResultJavaVal));
  if(rv){
    rv->env = env;
    rv->jObj = jObj ? REF_G(jObj) : 0;
  }
  return rv;
}

static void ResultJavaVal_finalizer(void *v){
  if(v){
    ResultJavaVal * const rv = (ResultJavaVal*)v;
    if(rv->jObj) (*(rv->env))->DeleteGlobalRef(rv->env, rv->jObj);
    sqlite3_free(rv);
  }
}



/**
   Returns a new Java instance of the class named by zClassName, which
   MUST be interface-compatible with NativePointerHolder and MUST have
   a no-arg constructor. Its setNativePointer() method is passed
   pNative. Hypothetically returns NULL if Java fails to allocate, but
   the JNI docs are not entirely clear on that detail.

   Always use a string literal for the 2nd argument so that we can use
   its address as a cache key.
*/
static jobject new_NativePointerHolder_object(JNIEnv * const env, const char *zClassName,
                                              const void * pNative){
  jobject rv = 0;
  jclass klazz = 0;
  jmethodID ctor = 0;
  struct NphCacheLine * const cacheLine =
    S3Global_nph_cache(env, zClassName);
  if(cacheLine && cacheLine->midCtor){
    assert( cacheLine->klazz );
    klazz = cacheLine->klazz;
    ctor = cacheLine->midCtor;
  }else{
    klazz = cacheLine
      ? cacheLine->klazz
      : (*env)->FindClass(env, zClassName);
    ctor = klazz ? (*env)->GetMethodID(env, klazz, "<init>", "()V") : 0;
    EXCEPTION_IS_FATAL("Cannot find constructor for class.");
    if(cacheLine){
      assert(zClassName == cacheLine->zClassName);
      assert(cacheLine->klazz);
      assert(!cacheLine->midCtor);
      cacheLine->midCtor = ctor;
    }
  }
  assert(klazz);
  assert(ctor);
  rv = (*env)->NewObject(env, klazz, ctor);
  EXCEPTION_IS_FATAL("No-arg constructor threw.");
  if(rv) setNativePointer(env, rv, pNative, zClassName);
  return rv;
}

static jobject new_sqlite3_value_wrapper(JNIEnv * const env, sqlite3_value *sv){
  return new_NativePointerHolder_object(env, S3ClassNames.sqlite3_value, sv);
}

static jobject new_sqlite3_context_wrapper(JNIEnv * const env, sqlite3_context *sv){
  return new_NativePointerHolder_object(env, S3ClassNames.sqlite3_context, sv);
}

static jobject new_sqlite3_stmt_wrapper(JNIEnv * const env, sqlite3_stmt *sv){
  return new_NativePointerHolder_object(env, S3ClassNames.sqlite3_stmt, sv);
}

enum UDFType {
  UDF_SCALAR = 1,
  UDF_AGGREGATE,
  UDF_WINDOW,
  UDF_UNKNOWN_TYPE/*for error propagation*/
};

typedef void (*udf_xFunc_f)(sqlite3_context*,int,sqlite3_value**);
typedef void (*udf_xStep_f)(sqlite3_context*,int,sqlite3_value**);
typedef void (*udf_xFinal_f)(sqlite3_context*);
/*typedef void (*udf_xValue_f)(sqlite3_context*);*/
/*typedef void (*udf_xInverse_f)(sqlite3_context*,int,sqlite3_value**);*/

/**
   State for binding Java-side UDFs.
*/
typedef struct {
  JNIEnv * env;         /* env registered from */;
  jobject jObj          /* SQLFunction instance */;
  jclass klazz          /* jObj's class */;
  char * zFuncName      /* Only for error reporting and debug logging */;
  enum UDFType type;
  /** Method IDs for the various UDF methods. */
  jmethodID jmidxFunc;
  jmethodID jmidxStep;
  jmethodID jmidxFinal;
  jmethodID jmidxValue;
  jmethodID jmidxInverse;
} UDFState;

static UDFState * UDFState_alloc(JNIEnv * const env, jobject jObj){
  UDFState * const s = sqlite3_malloc(sizeof(UDFState));
  if(s){
    const char * zFSI = /* signature for xFunc, xStep, xInverse */
      "(Lorg/sqlite/jni/sqlite3_context;[Lorg/sqlite/jni/sqlite3_value;)V";
    const char * zFV = /* signature for xFinal, xValue */
      "(Lorg/sqlite/jni/sqlite3_context;)V";
    memset(s, 0, sizeof(UDFState));
    s->env = env;
    s->jObj = REF_G(jObj);
    s->klazz = REF_G((*env)->GetObjectClass(env, jObj));
#define FGET(FuncName,FuncType,Field) \
    s->Field = (*env)->GetMethodID(env, s->klazz, FuncName, FuncType); \
    if(!s->Field) (*env)->ExceptionClear(env)
    FGET("xFunc",    zFSI, jmidxFunc);
    FGET("xStep",    zFSI, jmidxStep);
    FGET("xFinal",   zFV,  jmidxFinal);
    FGET("xValue",   zFV,  jmidxValue);
    FGET("xInverse", zFSI, jmidxInverse);
#undef FGET
    if(s->jmidxFunc) s->type = UDF_SCALAR;
    else if(s->jmidxStep && s->jmidxFinal){
      s->type = s->jmidxValue ? UDF_WINDOW : UDF_AGGREGATE;
    }else{
      s->type = UDF_UNKNOWN_TYPE;
    }
  }
  return s;
}

static void UDFState_free(UDFState * s){
  JNIEnv * const env = s->env;
  if(env){
    //MARKER(("UDF cleanup: %s\n", s->zFuncName));
    s3jni_call_xDestroy(env, s->jObj, s->klazz);
    UNREF_G(s->jObj);
    UNREF_G(s->klazz);
  }
  sqlite3_free(s->zFuncName);
  sqlite3_free(s);
}

static void UDFState_finalizer(void * s){
  //MARKER(("UDF finalizer @ %p\n", s));
  if(s) UDFState_free((UDFState*)s);
}

/**
   Helper for processing args to UDF handlers
   with signature (sqlite3_context*,int,sqlite3_value**).
*/
typedef struct {
  jobject jcx;
  jobjectArray jargv;
} udf_jargs;

/**
   Converts the given (cx, argc, argv) into arguments for the given
   UDF, placing the result in the final argument. Returns 0 on
   success, SQLITE_NOMEM on allocation error.
*/
static int udf_args(sqlite3_context * const cx,
                    int argc, sqlite3_value**argv,
                    UDFState * const s,
                    udf_jargs * const args){
  jobjectArray ja = 0;
  JNIEnv * const env = s->env;
  jobject jcx = new_sqlite3_context_wrapper(s->env, cx);
  jint i;
  args->jcx = 0;
  args->jargv = 0;
  if(!jcx) goto error_oom;
  ja = (*(s->env))->NewObjectArray(s->env, argc,
                                   S3Global_env_cache(env)->globalClassObj,
                                   NULL);
  if(!ja) goto error_oom;
  for(i = 0; i < argc; ++i){
    jobject jsv = new_sqlite3_value_wrapper(s->env, argv[i]);
    if(!jsv) goto error_oom;
    (*env)->SetObjectArrayElement(env, ja, i, jsv);
    UNREF_L(jsv)/*array has a ref*/;
  }
  args->jcx = jcx;
  args->jargv = ja;
  return 0;
error_oom:
  sqlite3_result_error_nomem(cx);
  UNREF_L(jcx);
  UNREF_L(ja);
  return 1;
}

static int udf_report_exception(sqlite3_context * cx, UDFState *s,
                                const char *zFuncType){
  int rc;
  char * z =
    sqlite3_mprintf("UDF %s.%s() threw. It should not do that.",
                    s->zFuncName, zFuncType);
  if(z){
    sqlite3_result_error(cx, z, -1);
    sqlite3_free(z);
    rc = SQLITE_ERROR;
  }else{
    rc = SQLITE_NOMEM;
  }
  return rc;
}

/**
   Sets up the state for calling a Java-side xFunc/xStep/xInverse()
   UDF, calls it, and returns 0 on success.
*/
static int udf_xFSI(sqlite3_context* pCx, int argc,
                    sqlite3_value** argv,
                    UDFState * s,
                    jmethodID xMethodID,
                    const char * zFuncType){
  udf_jargs args;
  JNIEnv * const env = s->env;
  int rc = udf_args(pCx, argc, argv, s, &args);
  //MARKER(("%s.%s() pCx = %p\n", s->zFuncName, zFuncType, pCx));
  if(rc) return rc;
  //MARKER(("UDF::%s.%s()\n", s->zFuncName, zFuncType));
  if( UDF_SCALAR != s->type ){
    rc = udf_setAggregateContext(env, args.jcx, pCx, 0);
  }
  if( 0 == rc ){
    (*env)->CallVoidMethod(env, s->jObj, xMethodID, args.jcx, args.jargv);
    IFTHREW{
      rc = udf_report_exception(pCx,s, zFuncType);
    }
  }
  UNREF_L(args.jcx);
  UNREF_L(args.jargv);
  return rc;
}

/**
   Sets up the state for calling a Java-side xFinal/xValue() UDF,
   calls it, and returns 0 on success.
*/
static int udf_xFV(sqlite3_context* cx, UDFState * s,
                   jmethodID xMethodID,
                   const char *zFuncType){
  JNIEnv * const env = s->env;
  jobject jcx = new_sqlite3_context_wrapper(s->env, cx);
  int rc = 0;
  //MARKER(("%s.%s() cx = %p\n", s->zFuncName, zFuncType, cx));
  if(!jcx){
    sqlite3_result_error_nomem(cx);
    return SQLITE_NOMEM;
  }
  //MARKER(("UDF::%s.%s()\n", s->zFuncName, zFuncType));
  if( UDF_SCALAR != s->type ){
    rc = udf_setAggregateContext(env, jcx, cx, 1);
  }
  if( 0 == rc ){
    (*env)->CallVoidMethod(env, s->jObj, xMethodID, jcx);
    IFTHREW{
      rc = udf_report_exception(cx,s, zFuncType);
    }
  }
  UNREF_L(jcx);
  return rc;
}

static void udf_xFunc(sqlite3_context* cx, int argc,
                      sqlite3_value** argv){
  UDFState * const s = (UDFState*)sqlite3_user_data(cx);
  ++S3Global.metrics.udf.nFunc;
  udf_xFSI(cx, argc, argv, s, s->jmidxFunc, "xFunc");
}
static void udf_xStep(sqlite3_context* cx, int argc,
                      sqlite3_value** argv){
  UDFState * const s = (UDFState*)sqlite3_user_data(cx);
  ++S3Global.metrics.udf.nStep;
  udf_xFSI(cx, argc, argv, s, s->jmidxStep, "xStep");
}
static void udf_xFinal(sqlite3_context* cx){
  UDFState * const s = (UDFState*)sqlite3_user_data(cx);
  ++S3Global.metrics.udf.nFinal;
  udf_xFV(cx, s, s->jmidxFinal, "xFinal");
}
static void udf_xValue(sqlite3_context* cx){
  UDFState * const s = (UDFState*)sqlite3_user_data(cx);
  ++S3Global.metrics.udf.nValue;
  udf_xFV(cx, s, s->jmidxValue, "xValue");
}
static void udf_xInverse(sqlite3_context* cx, int argc,
                         sqlite3_value** argv){
  UDFState * const s = (UDFState*)sqlite3_user_data(cx);
  ++S3Global.metrics.udf.nInverse;
  udf_xFSI(cx, argc, argv, s, s->jmidxInverse, "xInverse");
}


////////////////////////////////////////////////////////////////////////
// What follows is the JNI/C bindings. They are in alphabetical order
// except for this macro-generated subset which are kept together here
// at the front...
////////////////////////////////////////////////////////////////////////
WRAP_INT_DB(1errcode,                  sqlite3_errcode)
WRAP_INT_DB(1error_1offset,            sqlite3_error_offset)
WRAP_INT_DB(1extended_1errcode,        sqlite3_extended_errcode)
WRAP_INT_STMT(1bind_1parameter_1count, sqlite3_bind_parameter_count)
WRAP_INT_DB(1changes,                  sqlite3_changes)
WRAP_INT64_DB(1changes64,              sqlite3_changes64)
WRAP_INT_STMT(1clear_1bindings,        sqlite3_clear_bindings)
WRAP_INT_STMT_INT(1column_1bytes,      sqlite3_column_bytes)
WRAP_INT_STMT_INT(1column_1bytes16,    sqlite3_column_bytes16)
WRAP_INT_STMT(1column_1count,          sqlite3_column_count)
WRAP_STR_STMT_INT(1column_1decltype,   sqlite3_column_decltype)
WRAP_STR_STMT_INT(1column_1name,       sqlite3_column_name)
WRAP_STR_STMT_INT(1column_1database_1name,  sqlite3_column_database_name)
WRAP_STR_STMT_INT(1column_1origin_1name,    sqlite3_column_origin_name)
WRAP_STR_STMT_INT(1column_1table_1name,     sqlite3_column_table_name)
WRAP_INT_STMT_INT(1column_1type,       sqlite3_column_type)
WRAP_INT_STMT(1data_1count,            sqlite3_data_count)
WRAP_MUTF8_VOID(1libversion,           sqlite3_libversion)
WRAP_INT_VOID(1libversion_1number,     sqlite3_libversion_number)
WRAP_INT_INT(1sleep,                   sqlite3_sleep)
WRAP_MUTF8_VOID(1sourceid,             sqlite3_sourceid)
WRAP_INT_VOID(1threadsafe,             sqlite3_threadsafe)
WRAP_INT_DB(1total_1changes,           sqlite3_total_changes)
WRAP_INT64_DB(1total_1changes64,       sqlite3_total_changes64)
WRAP_INT_SVALUE(1value_1bytes,         sqlite3_value_bytes)
WRAP_INT_SVALUE(1value_1bytes16,       sqlite3_value_bytes16)
WRAP_INT_SVALUE(1value_1encoding,      sqlite3_value_encoding)
WRAP_INT_SVALUE(1value_1frombind,      sqlite3_value_frombind)
WRAP_INT_SVALUE(1value_1nochange,      sqlite3_value_nochange)
WRAP_INT_SVALUE(1value_1numeric_1type, sqlite3_value_numeric_type)
WRAP_INT_SVALUE(1value_1subtype,       sqlite3_value_subtype)
WRAP_INT_SVALUE(1value_1type,          sqlite3_value_type)

JDECL(jint,1bind_1blob)(JENV_JSELF, jobject jpStmt,
                        jint ndx, jbyteArray baData, jint nMax){
  int rc;
  if(!baData){
    rc = sqlite3_bind_null(PtrGet_sqlite3_stmt(jpStmt), ndx);
  }else{
    jbyte * const pBuf = JBA_TOC(baData);
    rc = sqlite3_bind_blob(PtrGet_sqlite3_stmt(jpStmt), (int)ndx, pBuf, (int)nMax,
                           SQLITE_TRANSIENT);
    JBA_RELEASE(baData,pBuf);
  }
  return (jint)rc;
}

JDECL(jint,1bind_1double)(JENV_JSELF, jobject jpStmt,
                         jint ndx, jdouble val){
  return (jint)sqlite3_bind_double(PtrGet_sqlite3_stmt(jpStmt), (int)ndx, (double)val);
}

JDECL(jint,1bind_1int)(JENV_JSELF, jobject jpStmt,
                      jint ndx, jint val){
  return (jint)sqlite3_bind_int(PtrGet_sqlite3_stmt(jpStmt), (int)ndx, (int)val);
}

JDECL(jint,1bind_1int64)(JENV_JSELF, jobject jpStmt,
                        jint ndx, jlong val){
  return (jint)sqlite3_bind_int64(PtrGet_sqlite3_stmt(jpStmt), (int)ndx, (sqlite3_int64)val);
}

JDECL(jint,1bind_1null)(JENV_JSELF, jobject jpStmt,
                       jint ndx){
  return (jint)sqlite3_bind_null(PtrGet_sqlite3_stmt(jpStmt), (int)ndx);
}

JDECL(jint,1bind_1parameter_1index)(JENV_JSELF, jobject jpStmt, jbyteArray jName){
  int rc = 0;
  jbyte * const pBuf = JBA_TOC(jName);
  if(pBuf){
    rc = sqlite3_bind_parameter_index(PtrGet_sqlite3_stmt(jpStmt),
                                      (const char *)pBuf);
    JBA_RELEASE(jName, pBuf);
  }
  return rc;
}

JDECL(jint,1bind_1text)(JENV_JSELF, jobject jpStmt,
                       jint ndx, jbyteArray baData, jint nMax){
  if(baData){
    jbyte * const pBuf = JBA_TOC(baData);
    int rc = sqlite3_bind_text(PtrGet_sqlite3_stmt(jpStmt), (int)ndx, (const char *)pBuf,
                               (int)nMax, SQLITE_TRANSIENT);
    JBA_RELEASE(baData, pBuf);
    return (jint)rc;
  }else{
    return sqlite3_bind_null(PtrGet_sqlite3_stmt(jpStmt), (int)ndx);
  }
}

JDECL(jint,1bind_1zeroblob)(JENV_JSELF, jobject jpStmt,
                           jint ndx, jint n){
  return (jint)sqlite3_bind_zeroblob(PtrGet_sqlite3_stmt(jpStmt), (int)ndx, (int)n);
}

JDECL(jint,1bind_1zeroblob64)(JENV_JSELF, jobject jpStmt,
                           jint ndx, jlong n){
  return (jint)sqlite3_bind_zeroblob(PtrGet_sqlite3_stmt(jpStmt), (int)ndx, (sqlite3_uint64)n);
}

static int s3jni_busy_handler(void* pState, int n){
  PerDbStateJni * const ps = (PerDbStateJni *)pState;
  int rc = 0;
  if( ps->busyHandler.jObj ){
    JNIEnv * const env = ps->env;
    rc = (*env)->CallIntMethod(env, ps->busyHandler.jObj,
                               ps->busyHandler.midCallback, (jint)n);
    IFTHREW{
      EXCEPTION_WARN_CALLBACK_THREW("busy-handler callback");
      EXCEPTION_CLEAR;
      rc = s3jni_db_error(ps->pDb, SQLITE_ERROR, "busy-handle callback threw.");
    }
  }
  return rc;
}

JDECL(jint,1busy_1handler)(JENV_JSELF, jobject jDb, jobject jBusy){
  PerDbStateJni * const ps = PerDbStateJni_for_db(env, jDb, 0, 0);
  int rc = 0;
  if(!ps) return (jint)SQLITE_NOMEM;
  if(jBusy){
    JniHookState * const pHook = &ps->busyHandler;
    if(pHook->jObj && (*env)->IsSameObject(env, pHook->jObj, jBusy)){
      /* Same object - this is a no-op. */
      return 0;
    }
    JniHookState_unref(env, pHook, 1);
    pHook->jObj = REF_G(jBusy);
    pHook->klazz = REF_G((*env)->GetObjectClass(env, jBusy));
    pHook->midCallback = (*env)->GetMethodID(env, pHook->klazz, "xCallback", "(I)I");
    IFTHREW {
      JniHookState_unref(env, pHook, 0);
      rc = SQLITE_ERROR;
    }
    if(rc){
      return rc;
    }
  }else{
    JniHookState_unref(env, &ps->busyHandler, 1);
  }
  return jBusy
    ? sqlite3_busy_handler(ps->pDb, s3jni_busy_handler, ps)
    : sqlite3_busy_handler(ps->pDb, 0, 0);
}

JDECL(jint,1busy_1timeout)(JENV_JSELF, jobject jDb, jint ms){
  PerDbStateJni * const ps = PerDbStateJni_for_db(env, jDb, 0, 0);
  if( ps ){
    JniHookState_unref(env, &ps->busyHandler, 1);
    return sqlite3_busy_timeout(ps->pDb, (int)ms);
  }
  return SQLITE_MISUSE;
}

/**
   Wrapper for sqlite3_close(_v2)().
*/
static jint s3jni_close_db(JNIEnv * const env, jobject jDb, int version){
  int rc = 0;
  PerDbStateJni * ps = 0;
  assert(version == 1 || version == 2);
  if(0){
    PerDbStateJni * s = S3Global.perDb.aUsed;
    for( ; s; s = s->pNext){
      PerDbStateJni_dump(s);
    }
  }
  ps = PerDbStateJni_for_db(env, jDb, 0, 0);
  if(!ps) return rc;
  rc = 1==version ? (jint)sqlite3_close(ps->pDb) : (jint)sqlite3_close_v2(ps->pDb);
  if(ps) PerDbStateJni_set_aside(ps)
           /* MUST come after close() because of ps->trace. */;
  setNativePointer(env, jDb, 0, S3ClassNames.sqlite3);
  return (jint)rc;
}

JDECL(jint,1close_1v2)(JENV_JSELF, jobject pDb){
  return s3jni_close_db(env, pDb, 2);
}

JDECL(jint,1close)(JENV_JSELF, jobject pDb){
  return s3jni_close_db(env, pDb, 1);
}

/**
   Assumes z is an array of unsigned short and returns the index in
   that array of the first element with the value 0.
*/
static unsigned int s3jni_utf16_strlen(void const * z){
  unsigned int i = 0;
  const unsigned short * p = z;
  while( p[i] ) ++i;
  return i;
}

static void s3jni_collation_needed_impl16(void *pState, sqlite3 *pDb,
                                          int eTextRep, const void * z16Name){
  PerDbStateJni * const ps = pState;
  JNIEnv * const env = ps->env;
  unsigned int const nName = s3jni_utf16_strlen(z16Name);
  jstring jName;
  jName  = (*env)->NewString(env, (jchar const *)z16Name, nName);
  IFTHREW {
    s3jni_db_error(ps->pDb, SQLITE_NOMEM, 0);
  }else{
    (*env)->CallVoidMethod(env, ps->collationNeeded.jObj,
                           ps->collationNeeded.midCallback,
                           ps->jDb, (jint)eTextRep, jName);
    IFTHREW{
      EXCEPTION_WARN_CALLBACK_THREW("collation-needed callback");
      EXCEPTION_CLEAR;
      s3jni_db_error(ps->pDb, SQLITE_ERROR, "collation-needed hook threw.");
    }
  }
  UNREF_L(jName);
}

JDECL(jint,1collation_1needed)(JENV_JSELF, jobject jDb, jobject jHook){
  PerDbStateJni * const ps = PerDbStateJni_for_db(env, jDb, 0, 0);
  jclass klazz;
  jobject pOld = 0;
  jmethodID xCallback;
  JniHookState * const pHook = &ps->collationNeeded;
  int rc;
  if(!ps) return SQLITE_MISUSE;
  pOld = pHook->jObj;
  if(pOld && jHook &&
     (*env)->IsSameObject(env, pOld, jHook)){
    return 0;
  }
  if( !jHook ){
    UNREF_G(pOld);
    memset(pHook, 0, sizeof(JniHookState));
    sqlite3_collation_needed(ps->pDb, 0, 0);
    return 0;
  }
  klazz = (*env)->GetObjectClass(env, jHook);
  xCallback = (*env)->GetMethodID(env, klazz, "xCollationNeeded",
                                  "(Lorg/sqlite/jni/sqlite3;ILjava/lang/String;)I");
  IFTHREW {
    EXCEPTION_CLEAR;
    rc = s3jni_db_error(ps->pDb, SQLITE_MISUSE,
                        "Cannot not find matching callback on "
                        "collation-needed hook object.");
  }else{
    pHook->midCallback = xCallback;
    pHook->jObj = REF_G(jHook);
    UNREF_G(pOld);
    rc = sqlite3_collation_needed16(ps->pDb, ps, s3jni_collation_needed_impl16);
  }
  return rc;
}

JDECL(jbyteArray,1column_1blob)(JENV_JSELF, jobject jpStmt,
                                jint ndx){
  sqlite3_stmt * const pStmt = PtrGet_sqlite3_stmt(jpStmt);
  void const * const p = sqlite3_column_blob(pStmt, (int)ndx);
  int const n = p ? sqlite3_column_bytes(pStmt, (int)ndx) : 0;
  if( 0==p ) return NULL;
  else{
    jbyteArray const jba = (*env)->NewByteArray(env, n);
    (*env)->SetByteArrayRegion(env, jba, 0, n, (const jbyte *)p);
    return jba;
  }
}

JDECL(jdouble,1column_1double)(JENV_JSELF, jobject jpStmt,
                               jint ndx){
  return (jdouble)sqlite3_column_double(PtrGet_sqlite3_stmt(jpStmt), (int)ndx);
}

JDECL(jint,1column_1int)(JENV_JSELF, jobject jpStmt,
                            jint ndx){
  return (jint)sqlite3_column_int(PtrGet_sqlite3_stmt(jpStmt), (int)ndx);
}

JDECL(jlong,1column_1int64)(JENV_JSELF, jobject jpStmt,
                            jint ndx){
  return (jlong)sqlite3_column_int64(PtrGet_sqlite3_stmt(jpStmt), (int)ndx);
}

/**
   Expects to be passed a pointer from sqlite3_column_text16() or
   sqlite3_value_text16() and a length value from
   sqlite3_column_bytes16() or sqlite3_value_bytes16(). It creates a
   Java String of exactly half that length, returning NULL if !p or
   (*env)->NewString() fails.
*/
static jstring s3jni_text_to_jstring(JNIEnv * const env, const void * const p, int nP){
  return p
    ? (*env)->NewString(env, (const jchar *)p, (jsize)(nP/2))
    : NULL;
}

/**
   Creates a new jByteArray of length nP, copies p's contents into it, and
   returns that byte array.
 */
static jbyteArray s3jni_new_jbyteArray(JNIEnv * const env, const unsigned char * const p, int nP){
  jbyteArray jba = (*env)->NewByteArray(env, (jint)nP);
  if(jba){
    (*env)->SetByteArrayRegion(env, jba, 0, (jint)nP, (const jbyte*)p);
  }
  return jba;
}

JDECL(jstring,1column_1text)(JENV_JSELF, jobject jpStmt,
                             jint ndx){
  sqlite3_stmt * const stmt = PtrGet_sqlite3_stmt(jpStmt);
  const int n = sqlite3_column_bytes16(stmt, (int)ndx);
  const void * const p = sqlite3_column_text16(stmt, (int)ndx);
  return s3jni_text_to_jstring(env, p, n);
}

JDECL(jbyteArray,1column_1text_1utf8)(JENV_JSELF, jobject jpStmt,
                                      jint ndx){
  sqlite3_stmt * const stmt = PtrGet_sqlite3_stmt(jpStmt);
  const int n = sqlite3_column_bytes(stmt, (int)ndx);
  const unsigned char * const p = sqlite3_column_text(stmt, (int)ndx);
  return s3jni_new_jbyteArray(env, p, n);
}

JDECL(jobject,1column_1value)(JENV_JSELF, jobject jpStmt,
                              jint ndx){
  sqlite3_value * const sv = sqlite3_column_value(PtrGet_sqlite3_stmt(jpStmt), (int)ndx);
  return new_sqlite3_value_wrapper(env, sv);
}


static int s3jni_commit_rollback_hook_impl(int isCommit, PerDbStateJni * const ps){
  JNIEnv * const env = ps->env;
  int rc = isCommit
    ? (int)(*env)->CallIntMethod(env, ps->commitHook.jObj,
                                 ps->commitHook.midCallback)
    : (int)((*env)->CallVoidMethod(env, ps->rollbackHook.jObj,
                                   ps->rollbackHook.midCallback), 0);
  IFTHREW{
    EXCEPTION_CLEAR;
    rc = s3jni_db_error(ps->pDb, SQLITE_ERROR, "hook callback threw.");
  }
  return rc;
}

static int s3jni_commit_hook_impl(void *pP){
  return s3jni_commit_rollback_hook_impl(1, pP);
}

static void s3jni_rollback_hook_impl(void *pP){
  (void)s3jni_commit_rollback_hook_impl(0, pP);
}

static jobject s3jni_commit_rollback_hook(int isCommit, JNIEnv * const env,jobject jDb,
                                          jobject jHook){
  PerDbStateJni * const ps = PerDbStateJni_for_db(env, jDb, 0, 0);
  jclass klazz;
  jobject pOld = 0;
  jmethodID xCallback;
  JniHookState * const pHook = isCommit ? &ps->commitHook : &ps->rollbackHook;
  if(!ps){
    s3jni_db_error(ps->pDb, SQLITE_NOMEM, 0);
    return 0;
  }
  pOld = pHook->jObj;
  if(pOld && jHook &&
     (*env)->IsSameObject(env, pOld, jHook)){
    return pOld;
  }
  if( !jHook ){
    if(pOld){
      jobject tmp = REF_L(pOld);
      UNREF_G(pOld);
      pOld = tmp;
    }
    memset(pHook, 0, sizeof(JniHookState));
    if( isCommit ) sqlite3_commit_hook(ps->pDb, 0, 0);
    else sqlite3_rollback_hook(ps->pDb, 0, 0);
    return pOld;
  }
  klazz = (*env)->GetObjectClass(env, jHook);
  xCallback = (*env)->GetMethodID(env, klazz,
                                  isCommit ? "xCommitHook" : "xRollbackHook",
                                  isCommit ? "()I" : "()V");
  IFTHREW {
    EXCEPTION_REPORT;
    EXCEPTION_CLEAR;
    s3jni_db_error(ps->pDb, SQLITE_ERROR,
                   "Cannot not find matching callback on "
                   "hook object.");
  }else{
    pHook->midCallback = xCallback;
    pHook->jObj = REF_G(jHook);
    if( isCommit ) sqlite3_commit_hook(ps->pDb, s3jni_commit_hook_impl, ps);
    else sqlite3_rollback_hook(ps->pDb, s3jni_rollback_hook_impl, ps);
    if(pOld){
      jobject tmp = REF_L(pOld);
      UNREF_G(pOld);
      pOld = tmp;
    }
  }
  return pOld;
}

JDECL(jobject,1commit_1hook)(JENV_JSELF,jobject jDb, jobject jHook){
  return s3jni_commit_rollback_hook(1, env, jDb, jHook);
}


JDECL(jstring,1compileoption_1get)(JENV_JSELF, jint n){
  return (*env)->NewStringUTF( env, sqlite3_compileoption_get(n) );
}

JDECL(jboolean,1compileoption_1used)(JENV_JSELF, jstring name){
  const char *zUtf8 = (*env)->GetStringUTFChars(env, name, NULL);
  const jboolean rc =
    0==sqlite3_compileoption_used(zUtf8) ? JNI_FALSE : JNI_TRUE;
  (*env)->ReleaseStringUTFChars(env, name, zUtf8);
  return rc;
}

JDECL(jobject,1context_1db_1handle)(JENV_JSELF, jobject jpCx){
  sqlite3 * const pDb = sqlite3_context_db_handle(PtrGet_sqlite3_context(jpCx));
  PerDbStateJni * const ps = pDb ? PerDbStateJni_for_db(env, 0, pDb, 0) : 0;
  return ps ? ps->jDb : 0;
}

JDECL(jint,1create_1collation)(JENV_JSELF, jobject jDb,
                               jstring name, jint eTextRep,
                               jobject oCollation){
  PerDbStateJni * const ps = PerDbStateJni_for_db(env, jDb, 0, 0);
  jclass klazz;
  int rc;
  const char *zName;
  JniHookState * pHook;
  if(!ps) return (jint)SQLITE_NOMEM;
  pHook = &ps->collation;
  klazz = (*env)->GetObjectClass(env, oCollation);
  pHook->midCallback = (*env)->GetMethodID(env, klazz, "xCompare",
                                           "([B[B)I");
  IFTHREW{
    EXCEPTION_REPORT;
    return s3jni_db_error(ps->pDb, SQLITE_ERROR,
                          "Could not get xCompare() method for object.");
  }
  zName = JSTR_TOC(name);
  rc = sqlite3_create_collation_v2(ps->pDb, zName, (int)eTextRep,
                                   ps, CollationState_xCompare,
                                   CollationState_xDestroy);
  JSTR_RELEASE(name, zName);
  if( 0==rc ){
    pHook->jObj = REF_G(oCollation);
    pHook->klazz = REF_G(klazz);
  }else{
    JniHookState_unref(env, pHook, 1);
  }
  return (jint)rc;
}

static jint create_function(JNIEnv * env, jobject jDb, jstring jFuncName,
                            jint nArg, jint eTextRep, jobject jFunctor){
  UDFState * s = 0;
  int rc;
  sqlite3 * const pDb = PtrGet_sqlite3(jDb);
  const char * zFuncName = 0;

  if( !encodingTypeIsValid(eTextRep) ){
    return s3jni_db_error(pDb, SQLITE_FORMAT,
                                "Invalid function encoding option.");
  }
  s = UDFState_alloc(env, jFunctor);
  if( !s ) return SQLITE_NOMEM;
  else if( UDF_UNKNOWN_TYPE==s->type ){
    rc = s3jni_db_error(pDb, SQLITE_MISUSE,
                        "Cannot unambiguously determine function type.");
    goto error_cleanup;
  }
  zFuncName = JSTR_TOC(jFuncName);
  if(!zFuncName){
    rc = SQLITE_NOMEM;
    goto error_cleanup;
  }
  if( UDF_WINDOW == s->type ){
    rc = sqlite3_create_window_function(pDb, zFuncName, nArg, eTextRep, s,
                                        udf_xStep, udf_xFinal, udf_xValue,
                                        udf_xInverse, UDFState_finalizer);
  }else{
    udf_xFunc_f xFunc = 0;
    udf_xStep_f xStep = 0;
    udf_xFinal_f xFinal = 0;
    if( UDF_SCALAR == s->type ){
      xFunc = udf_xFunc;
    }else{
      assert( UDF_AGGREGATE == s->type );
      xStep = udf_xStep;
      xFinal = udf_xFinal;
    }
    rc = sqlite3_create_function_v2(pDb, zFuncName, nArg, eTextRep, s,
                                    xFunc, xStep, xFinal, UDFState_finalizer);
  }
  if( 0==rc ){
    s->zFuncName = sqlite3_mprintf("%s", zFuncName);
    if( !s->zFuncName ){
      rc = SQLITE_NOMEM;
    }
  }
  if( 0!=rc ){
    UDFState_free(s);
  }
error_cleanup:
  JSTR_RELEASE(jFuncName, zFuncName);
  /* on create_function() error, s will be destroyed via create_function() */
  return (jint)rc;
}

JDECL(jint,1create_1function)(JENV_JSELF, jobject jDb, jstring jFuncName,
                              jint nArg, jint eTextRep, jobject jFunctor){
  return create_function(env, jDb, jFuncName, nArg, eTextRep, jFunctor);
}

/*
JDECL(jint,1create_1window_1function)(JENV_JSELF, jstring jFuncName, jint nArg,
                                      jint eTextRep, jobject jFunctor){
  return create_function_mega(env, jFuncName, nArg, eTextRep, jFunctor);
}
*/

JDECL(jstring,1errmsg)(JENV_JSELF, jobject jpDb){
  return (*env)->NewStringUTF(env, sqlite3_errmsg(PtrGet_sqlite3(jpDb)));
}

JDECL(jstring,1errstr)(JENV_JSELF, jint rcCode){
  return (*env)->NewStringUTF(env, sqlite3_errstr((int)rcCode));
}

JDECL(jboolean,1extended_1result_1codes)(JENV_JSELF, jobject jpDb,
                                         jboolean onoff){
  int const rc = sqlite3_extended_result_codes(PtrGet_sqlite3(jpDb), onoff ? 1 : 0);
  return rc ? JNI_TRUE : JNI_FALSE;
}

JDECL(jint,1initialize)(JENV_JSELF){
  return sqlite3_initialize();
}

/**
   Sets jc->currentStmt to the 2nd arugment and returns the previous
   value of jc->currentStmt. This must always be called in pairs: once
   to replace the current statement with the call-local one, and once
   to restore it. It must be used in any stmt-handling routines which
   may lead to the tracing callback being called, as the current stmt
   is needed for certain tracing flags. At a minumum those ops are:
   step, reset, finalize, prepare.
*/
static jobject stmt_set_current(JNIEnvCacheLine * const jc, jobject jStmt){
  jobject const old = jc->currentStmt;
  jc->currentStmt = jStmt;
  return old;
}

JDECL(jint,1finalize)(JENV_JSELF, jobject jpStmt){
  int rc = 0;
  sqlite3_stmt * const pStmt = PtrGet_sqlite3_stmt(jpStmt);
  if( pStmt ){
    JNIEnvCacheLine * const jc = S3Global_env_cache(env);
    jobject const pPrev = stmt_set_current(jc, jpStmt);
    rc = sqlite3_finalize(pStmt);
    setNativePointer(env, jpStmt, 0, S3ClassNames.sqlite3_stmt);
    (void)stmt_set_current(jc, pPrev);
  }
  return rc;
}


JDECL(jlong,1last_1insert_1rowid)(JENV_JSELF, jobject jpDb){
  return (jlong)sqlite3_last_insert_rowid(PtrGet_sqlite3(jpDb));
}

/**
   Code common to both the sqlite3_open() and sqlite3_open_v2()
   bindings. Allocates the PerDbStateJni for *ppDb if *ppDb is not
   NULL. jDb must be the org.sqlite.jni.sqlite3 object which will hold
   the db's native pointer. theRc must be the result code of the open
   op(). If allocation of the PerDbStateJni object fails then *ppDb is
   passed to sqlite3_close(), *ppDb is assigned to 0, and SQLITE_NOMEM
   is returned, else theRc is returned. In in case, *ppDb is stored in
   jDb's native pointer property (even if it's NULL).
*/
static int s3jni_open_post(JNIEnv * const env, sqlite3 **ppDb, jobject jDb, int theRc){
  if(*ppDb){
    PerDbStateJni * const s = PerDbStateJni_for_db(env, jDb, *ppDb, 1);
    if(!s && 0==theRc){
      sqlite3_close(*ppDb);
      *ppDb = 0;
      theRc = SQLITE_NOMEM;
    }
  }
  setNativePointer(env, jDb, *ppDb, S3ClassNames.sqlite3);
  return theRc;
}

JDECL(jint,1open)(JENV_JSELF, jstring strName, jobject jOut){
  sqlite3 * pOut = 0;
  const char *zName = strName ? JSTR_TOC(strName) : 0;
  int nrc = sqlite3_open(zName, &pOut);
  //MARKER(("env=%p, *env=%p\n", env, *env));
  nrc = s3jni_open_post(env, &pOut, jOut, nrc);
  assert(nrc==0 ? pOut!=0 : 1);
  JSTR_RELEASE(strName, zName);
  return (jint)nrc;
}

JDECL(jint,1open_1v2)(JENV_JSELF, jstring strName,
                      jobject jOut, jint flags, jstring strVfs){
  sqlite3 * pOut = 0;
  const char *zName = strName ? JSTR_TOC(strName) : 0;
  const char *zVfs = strVfs ? JSTR_TOC(strVfs) : 0;
  int nrc = sqlite3_open_v2(zName, &pOut, (int)flags, zVfs);
  /*MARKER(("zName=%s, zVfs=%s, pOut=%p, flags=%d, nrc=%d\n",
    zName, zVfs, pOut, (int)flags, nrc));*/
  nrc = s3jni_open_post(env, &pOut, jOut, nrc);
  assert(nrc==0 ? pOut!=0 : 1);
  JSTR_RELEASE(strName, zName);
  JSTR_RELEASE(strVfs, zVfs);
  return (jint)nrc;
}

/* Proxy for the sqlite3_prepare[_v2/3]() family. */
static jint sqlite3_jni_prepare_v123(int prepVersion, JNIEnv * const env, jclass self,
                                     jobject jpDb, jbyteArray baSql,
                                     jint nMax, jint prepFlags,
                                     jobject jOutStmt, jobject outTail){
  sqlite3_stmt * pStmt = 0;
  const char * zTail = 0;
  jbyte * const pBuf = JBA_TOC(baSql);
  JNIEnvCacheLine * const jc = S3Global_env_cache(env);
  jobject const pOldStmt = stmt_set_current(jc, jOutStmt);
  int rc = SQLITE_ERROR;
  assert(prepVersion==1 || prepVersion==2 || prepVersion==3);
  switch( prepVersion ){
    case 1: rc = sqlite3_prepare(PtrGet_sqlite3(jpDb), (const char *)pBuf,
                                 (int)nMax, &pStmt, &zTail);
      break;
    case 2: rc = sqlite3_prepare_v2(PtrGet_sqlite3(jpDb), (const char *)pBuf,
                                    (int)nMax, &pStmt, &zTail);
      break;
    case 3: rc = sqlite3_prepare_v3(PtrGet_sqlite3(jpDb), (const char *)pBuf,
                                    (int)nMax, (unsigned int)prepFlags,
                                    &pStmt, &zTail);
      break;
    default:
      assert(0 && "Invalid prepare() version");
  }
  JBA_RELEASE(baSql,pBuf);
  if( 0!=outTail ){
    assert(zTail ? ((void*)zTail>=(void*)pBuf) : 1);
    assert(zTail ? (((int)((void*)zTail - (void*)pBuf)) >= 0) : 1);
    setOutputInt32(env, outTail, (int)(zTail ? (zTail - (const char *)pBuf) : 0));
  }
  setNativePointer(env, jOutStmt, pStmt, S3ClassNames.sqlite3_stmt);
  (void)stmt_set_current(jc, pOldStmt);
  return (jint)rc;
}
JDECL(jint,1prepare)(JNIEnv * const env, jclass self, jobject jpDb, jbyteArray baSql,
                     jint nMax, jobject jOutStmt, jobject outTail){
  return sqlite3_jni_prepare_v123(1, env, self, jpDb, baSql, nMax, 0,
                                  jOutStmt, outTail);
}
JDECL(jint,1prepare_1v2)(JNIEnv * const env, jclass self, jobject jpDb, jbyteArray baSql,
                         jint nMax, jobject jOutStmt, jobject outTail){
  return sqlite3_jni_prepare_v123(2, env, self, jpDb, baSql, nMax, 0,
                                  jOutStmt, outTail);
}
JDECL(jint,1prepare_1v3)(JNIEnv * const env, jclass self, jobject jpDb, jbyteArray baSql,
                         jint nMax, jint prepFlags, jobject jOutStmt, jobject outTail){
  return sqlite3_jni_prepare_v123(3, env, self, jpDb, baSql, nMax,
                                  prepFlags, jOutStmt, outTail);
}


static int s3jni_progress_handler_impl(void *pP){
  PerDbStateJni * const ps = (PerDbStateJni *)pP;
  JNIEnv * const env = ps->env;
  int rc = (int)(*env)->CallIntMethod(env, ps->progress.jObj,
                                      ps->progress.midCallback);
  IFTHREW{
    EXCEPTION_WARN_CALLBACK_THREW("sqlite3_progress_handler() callback");
    EXCEPTION_CLEAR;
    rc = s3jni_db_error(ps->pDb, SQLITE_ERROR,
                        "sqlite3_progress_handler() callback threw.");
  }
  return rc;
}

JDECL(void,1progress_1handler)(JENV_JSELF,jobject jDb, jint n, jobject jProgress){
  PerDbStateJni * ps = PerDbStateJni_for_db(env, jDb, 0, 0);
  jclass klazz;
  jmethodID xCallback;
  if( n<1 || !jProgress ){
    if(ps){
      UNREF_G(ps->progress.jObj);
      memset(&ps->progress, 0, sizeof(ps->progress));
    }
    sqlite3_progress_handler(ps->pDb, 0, 0, 0);
    return;
  }
  if(!ps){
    s3jni_db_error(ps->pDb, SQLITE_NOMEM, 0);
    return;
  }
  klazz = (*env)->GetObjectClass(env, jProgress);
  xCallback = (*env)->GetMethodID(env, klazz, "xCallback", "()I");
  IFTHREW {
    EXCEPTION_CLEAR;
    s3jni_db_error(ps->pDb, SQLITE_ERROR,
                   "Cannot not find matching xCallback() on "
                   "ProgressHandler object.");
  }else{
    UNREF_G(ps->progress.jObj);
    ps->progress.midCallback = xCallback;
    ps->progress.jObj = REF_G(jProgress);
    sqlite3_progress_handler(ps->pDb, (int)n, s3jni_progress_handler_impl, ps);
  }
}


JDECL(jint,1reset)(JENV_JSELF, jobject jpStmt){
  int rc = 0;
  sqlite3_stmt * const pStmt = PtrGet_sqlite3_stmt(jpStmt);
  if( pStmt ){
    JNIEnvCacheLine * const jc = S3Global_env_cache(env);
    jobject const pPrev = stmt_set_current(jc, jpStmt);
    rc = sqlite3_reset(pStmt);
    (void)stmt_set_current(jc, pPrev);
  }
  return rc;
}

/* sqlite3_result_text/blob() and friends. */
static void result_blob_text(int asBlob, int as64,
                             int eTextRep/*only for (asBlob=0)*/,
                             JNIEnv * const env, sqlite3_context *pCx,
                             jbyteArray jBa, jlong nMax){
  if(jBa){
    jbyte * const pBuf = JBA_TOC(jBa);
    jsize nBa = (*env)->GetArrayLength(env, jBa);
    if( nMax>=0 && nBa>(jsize)nMax ){
      nBa = (jsize)nMax;
      /**
         From the sqlite docs:

         > If the 3rd parameter to any of the sqlite3_result_text*
           interfaces other than sqlite3_result_text64() is negative,
           then SQLite computes the string length itself by searching
           the 2nd parameter for the first zero character.

         Note that the text64() interfaces take an unsigned value for
         the length, which Java does not support. This binding takes
         the approach of passing on negative values to the C API,
         which will, in turn fail with SQLITE_TOOBIG at some later
         point (recall that the sqlite3_result_xyz() family do not
         have result values).
      */
    }
    if(as64){ /* 64-bit... */
      static const jsize nLimit64 =
        SQLITE_MAX_ALLOCATION_SIZE/*only _kinda_ arbitrary!*/
        /* jsize is int32, not int64! */;
      if(nBa > nLimit64){
        sqlite3_result_error_toobig(pCx);
      }else if(asBlob){
        sqlite3_result_blob64(pCx, pBuf, (sqlite3_uint64)nBa,
                              SQLITE_TRANSIENT);
      }else{ /* text64... */
        if(encodingTypeIsValid(eTextRep)){
          sqlite3_result_text64(pCx, (const char *)pBuf,
                                (sqlite3_uint64)nBa,
                                SQLITE_TRANSIENT, eTextRep);
        }else{
          sqlite3_result_error_code(pCx, SQLITE_FORMAT);
        }
      }
    }else{ /* 32-bit... */
      static const jsize nLimit = SQLITE_MAX_ALLOCATION_SIZE;
      if(nBa > nLimit){
        sqlite3_result_error_toobig(pCx);
      }else if(asBlob){
        sqlite3_result_blob(pCx, pBuf, (int)nBa,
                            SQLITE_TRANSIENT);
      }else{
        switch(eTextRep){
          case SQLITE_UTF8:
            sqlite3_result_text(pCx, (const char *)pBuf, (int)nBa,
                                SQLITE_TRANSIENT);
            break;
          case SQLITE_UTF16:
            sqlite3_result_text16(pCx, (const char *)pBuf, (int)nBa,
                                  SQLITE_TRANSIENT);
            break;
          case SQLITE_UTF16LE:
            sqlite3_result_text16le(pCx, (const char *)pBuf, (int)nBa,
                                    SQLITE_TRANSIENT);
            break;
          case SQLITE_UTF16BE:
            sqlite3_result_text16be(pCx, (const char *)pBuf, (int)nBa,
                                    SQLITE_TRANSIENT);
            break;
        }
      }
      JBA_RELEASE(jBa, pBuf);
    }
  }else{
    sqlite3_result_null(pCx);
  }
}

JDECL(void,1result_1blob)(JENV_JSELF, jobject jpCx, jbyteArray jBa, jint nMax){
  return result_blob_text(1, 0, 0, env, PtrGet_sqlite3_context(jpCx), jBa, nMax);
}

JDECL(void,1result_1blob64)(JENV_JSELF, jobject jpCx, jbyteArray jBa, jlong nMax){
  return result_blob_text(1, 1, 0, env, PtrGet_sqlite3_context(jpCx), jBa, nMax);
}

JDECL(void,1result_1double)(JENV_JSELF, jobject jpCx, jdouble v){
  sqlite3_result_double(PtrGet_sqlite3_context(jpCx), v);
}

JDECL(void,1result_1error)(JENV_JSELF, jobject jpCx, jbyteArray baMsg,
                           int eTextRep){
  const char * zUnspecified = "Unspecified error.";
  jsize const baLen = (*env)->GetArrayLength(env, baMsg);
  jbyte * const pjBuf = baMsg ? JBA_TOC(baMsg) : NULL;
  switch(pjBuf ? eTextRep : SQLITE_UTF8){
    case SQLITE_UTF8: {
      const char *zMsg = pjBuf ? (const char *)pjBuf : zUnspecified;
      sqlite3_result_error(PtrGet_sqlite3_context(jpCx), zMsg, baLen);
      break;
    }
    case SQLITE_UTF16: {
      const void *zMsg = pjBuf
        ? (const void *)pjBuf : (const void *)zUnspecified;
      sqlite3_result_error16(PtrGet_sqlite3_context(jpCx), zMsg, baLen);
      break;
    }
    default:
      sqlite3_result_error(PtrGet_sqlite3_context(jpCx),
                           "Invalid encoding argument passed "
                           "to sqlite3_result_error().", -1);
      break;
  }
  JBA_RELEASE(baMsg,pjBuf);
}

JDECL(void,1result_1error_1code)(JENV_JSELF, jobject jpCx, jint v){
  sqlite3_result_error_code(PtrGet_sqlite3_context(jpCx), v ? (int)v : SQLITE_ERROR);
}

JDECL(void,1result_1error_1nomem)(JENV_JSELF, jobject jpCx){
  sqlite3_result_error_nomem(PtrGet_sqlite3_context(jpCx));
}

JDECL(void,1result_1error_1toobig)(JENV_JSELF, jobject jpCx){
  sqlite3_result_error_toobig(PtrGet_sqlite3_context(jpCx));
}

JDECL(void,1result_1int)(JENV_JSELF, jobject jpCx, jint v){
  sqlite3_result_int(PtrGet_sqlite3_context(jpCx), (int)v);
}

JDECL(void,1result_1int64)(JENV_JSELF, jobject jpCx, jlong v){
  sqlite3_result_int64(PtrGet_sqlite3_context(jpCx), (sqlite3_int64)v);
}

JDECL(void,1result_1java_1object)(JENV_JSELF, jobject jpCx, jobject v){
  if(v){
    ResultJavaVal * const rjv = ResultJavaVal_alloc(env, v);
    if(rjv){
      sqlite3_result_pointer(PtrGet_sqlite3_context(jpCx), rjv, RESULT_JAVA_VAL_STRING,
                             ResultJavaVal_finalizer);
    }else{
      sqlite3_result_error_nomem(PtrGet_sqlite3_context(jpCx));
    }
  }else{
    sqlite3_result_null(PtrGet_sqlite3_context(jpCx));
  }
}

JDECL(void,1result_1null)(JENV_JSELF, jobject jpCx){
  sqlite3_result_null(PtrGet_sqlite3_context(jpCx));
}

JDECL(void,1result_1text)(JENV_JSELF, jobject jpCx, jbyteArray jBa, jint nMax){
  return result_blob_text(0, 0, SQLITE_UTF8, env, PtrGet_sqlite3_context(jpCx), jBa, nMax);
}

JDECL(void,1result_1text64)(JENV_JSELF, jobject jpCx, jbyteArray jBa, jlong nMax,
                            jint eTextRep){
  return result_blob_text(0, 1, eTextRep, env, PtrGet_sqlite3_context(jpCx), jBa, nMax);
}

JDECL(void,1result_1value)(JENV_JSELF, jobject jpCx, jobject jpSVal){
  sqlite3_result_value(PtrGet_sqlite3_context(jpCx), PtrGet_sqlite3_value(jpSVal));
}

JDECL(void,1result_1zeroblob)(JENV_JSELF, jobject jpCx, jint v){
  sqlite3_result_zeroblob(PtrGet_sqlite3_context(jpCx), (int)v);
}

JDECL(jint,1result_1zeroblob64)(JENV_JSELF, jobject jpCx, jlong v){
  return (jint)sqlite3_result_zeroblob64(PtrGet_sqlite3_context(jpCx), (sqlite3_int64)v);
}

JDECL(jobject,1rollback_1hook)(JENV_JSELF,jobject jDb, jobject jHook){
  return s3jni_commit_rollback_hook(0, env, jDb, jHook);
}

JDECL(void,1set_1last_1insert_1rowid)(JENV_JSELF, jobject jpDb, jlong rowId){
  sqlite3_set_last_insert_rowid(PtrGet_sqlite3_context(jpDb),
                                (sqlite3_int64)rowId);
}

JDECL(jint,1shutdown)(JENV_JSELF){
  PerDbStateJni_free_all();
  JNIEnvCache_clear(&S3Global.envCache);
  /* Do not clear S3Global.jvm or the global refs to specific classes:
     it's legal to call sqlite3_initialize() again to restart the
     lib. */
  return sqlite3_shutdown();
}

JDECL(jint,1step)(JENV_JSELF,jobject jStmt){
  int rc = SQLITE_MISUSE;
  sqlite3_stmt * const pStmt = PtrGet_sqlite3_stmt(jStmt);
  if(pStmt){
    JNIEnvCacheLine * const jc = S3Global_env_cache(env);
    jobject const jPrevStmt = stmt_set_current(jc, jStmt);
    rc = sqlite3_step(pStmt);
    (void)stmt_set_current(jc, jPrevStmt);
  }
  return rc;
}

static int s3jni_trace_impl(unsigned traceflag, void *pC, void *pP, void *pX){
  PerDbStateJni * const ps = (PerDbStateJni *)pC;
  JNIEnv * const env = ps->env;
  jobject jX = NULL  /* the tracer's X arg */;
  jobject jP = NULL  /* the tracer's P arg */;
  jobject jPUnref = NULL /* potentially a local ref to jP */;
  JNIEnvCacheLine * const jc = S3Global_env_cache(env);
  int rc;
  switch(traceflag){
    case SQLITE_TRACE_STMT:
      /* This is not _quite_ right: we're converting to MUTF-8.  It
         should(?) suffice for purposes of tracing, though. */
      jX = (*env)->NewStringUTF(env, (const char *)pX);
      if(!jX) return SQLITE_NOMEM;
      jP = jc->currentStmt;
      break;
    case SQLITE_TRACE_PROFILE:
      jX = (*env)->NewObject(env, jc->globalClassLong, jc->ctorLong1,
                             (jlong)*((sqlite3_int64*)pX));
      // hmm. It really is zero.
      // MARKER(("profile time = %llu\n", *((sqlite3_int64*)pX)));
      jP = jc->currentStmt;
      if(!jP){
        jP = jPUnref = new_sqlite3_stmt_wrapper(env, pP);
        if(!jP){
          UNREF_L(jX);
          return SQLITE_NOMEM;
        }
        MARKER(("WARNING: created new sqlite3_stmt wrapper for TRACE_PROFILE. stmt@%p\n"
                "This means we have missed a route into the tracing API and it "
                "needs the stmt_set_current() treatment which is littered around "
                "a handful of other functions in this file.\n", pP));
      }
      break;
    case SQLITE_TRACE_ROW:
      jP = jc->currentStmt;
      break;
    case SQLITE_TRACE_CLOSE:
      jP = ps->jDb;
      break;
    default:
      assert(!"cannot happen - unkown trace flag");
      return SQLITE_ERROR;
  }
  assert(jP);
  rc = (int)(*env)->CallIntMethod(env, ps->trace.jObj,
                                  ps->trace.midCallback,
                                  (jint)traceflag, jP, jX);
  IFTHREW{
    EXCEPTION_WARN_CALLBACK_THREW("sqlite3_trace_v2() callback");
    EXCEPTION_CLEAR;
    rc = SQLITE_ERROR;
  }
  UNREF_L(jPUnref);
  UNREF_L(jX);
  return rc;
}

JDECL(jint,1trace_1v2)(JENV_JSELF,jobject jDb, jint traceMask, jobject jTracer){
  PerDbStateJni * const ps = PerDbStateJni_for_db(env, jDb, 0, 0);
  jclass klazz;
  if( !traceMask || !jTracer ){
    if(ps){
      UNREF_G(ps->trace.jObj);
      memset(&ps->trace, 0, sizeof(ps->trace));
    }
    return (jint)sqlite3_trace_v2(ps->pDb, 0, 0, 0);
  }
  if(!ps) return SQLITE_NOMEM;
  klazz = (*env)->GetObjectClass(env, jTracer);
  ps->trace.midCallback = (*env)->GetMethodID(env, klazz, "xCallback",
                                              "(ILjava/lang/Object;Ljava/lang/Object;)I");
  IFTHREW {
    EXCEPTION_CLEAR;
    return s3jni_db_error(ps->pDb, SQLITE_ERROR,
                          "Cannot not find matching xCallback() on Tracer object.");
  }
  ps->trace.jObj = REF_G(jTracer);
  return sqlite3_trace_v2(ps->pDb, (unsigned)traceMask, s3jni_trace_impl, ps);
}

static void s3jni_update_hook_impl(void * pState, int opId, const char *zDb,
                                   const char *zTable, sqlite3_int64 nRowid){
  PerDbStateJni * const ps = pState;
  JNIEnv * const env = ps->env;
  /* ACHTUNG: this will break if zDb or zTable contain chars which are
     different in MUTF-8 than UTF-8. That seems like a low risk,
     but it's possible. */
  jstring jDbName;
  jstring jTable;
  jDbName  = (*env)->NewStringUTF(env, zDb);
  jTable = jDbName ? (*env)->NewStringUTF(env, zTable) : 0;
  IFTHREW {
    s3jni_db_error(ps->pDb, SQLITE_NOMEM, 0);
  }else{
    (*env)->CallVoidMethod(env, ps->updateHook.jObj,
                           ps->updateHook.midCallback,
                           (jint)opId, jDbName, jTable, (jlong)nRowid);
    IFTHREW{
      EXCEPTION_WARN_CALLBACK_THREW("update hook");
      EXCEPTION_CLEAR;
      s3jni_db_error(ps->pDb, SQLITE_ERROR, "update hook callback threw.");
    }
  }
  UNREF_L(jDbName);
  UNREF_L(jTable);
}


JDECL(jobject,1update_1hook)(JENV_JSELF, jobject jDb, jobject jHook){
  PerDbStateJni * const ps = PerDbStateJni_for_db(env, jDb, 0, 0);
  jclass klazz;
  jobject pOld = 0;
  jmethodID xCallback;
  JniHookState * const pHook = &ps->updateHook;
  if(!ps){
    s3jni_db_error(ps->pDb, SQLITE_MISUSE, 0);
    return 0;
  }
  pOld = pHook->jObj;
  if(pOld && jHook &&
     (*env)->IsSameObject(env, pOld, jHook)){
    return pOld;
  }
  if( !jHook ){
    if(pOld){
      jobject tmp = REF_L(pOld);
      UNREF_G(pOld);
      pOld = tmp;
    }
    memset(pHook, 0, sizeof(JniHookState));
    sqlite3_update_hook(ps->pDb, 0, 0);
    return pOld;
  }
  klazz = (*env)->GetObjectClass(env, jHook);
  xCallback = (*env)->GetMethodID(env, klazz, "xUpdateHook",
                                  "(ILjava/lang/String;Ljava/lang/String;J)V");
  IFTHREW {
    EXCEPTION_CLEAR;
    s3jni_db_error(ps->pDb, SQLITE_ERROR,
                   "Cannot not find matching callback on "
                   "update hook object.");
  }else{
    pHook->midCallback = xCallback;
    pHook->jObj = REF_G(jHook);
    sqlite3_update_hook(ps->pDb, s3jni_update_hook_impl, ps);
    if(pOld){
      jobject tmp = REF_L(pOld);
      UNREF_G(pOld);
      pOld = tmp;
    }
  }
  return pOld;
}


JDECL(jbyteArray,1value_1blob)(JENV_JSELF, jobject jpSVal){
  sqlite3_value * const sv = PtrGet_sqlite3_value(jpSVal);
  int const nLen = sqlite3_value_bytes(sv);
  const jbyte * pBytes = sqlite3_value_blob(sv);
  jbyteArray const jba = pBytes
    ? (*env)->NewByteArray(env, (jsize)nLen)
    : NULL;
  if(jba){
    (*env)->SetByteArrayRegion(env, jba, 0, nLen, pBytes);
  }
  return jba;
}


JDECL(jdouble,1value_1double)(JENV_JSELF, jobject jpSVal){
  return (jdouble) sqlite3_value_double(PtrGet_sqlite3_value(jpSVal));
}


JDECL(jobject,1value_1dup)(JENV_JSELF, jobject jpSVal){
  sqlite3_value * const sv = sqlite3_value_dup(PtrGet_sqlite3_value(jpSVal));
  return sv ? new_sqlite3_value_wrapper(env, sv) : 0;
}

JDECL(void,1value_1free)(JENV_JSELF, jobject jpSVal){
  sqlite3_value_free(PtrGet_sqlite3_value(jpSVal));
}

JDECL(jint,1value_1int)(JENV_JSELF, jobject jpSVal){
  return (jint) sqlite3_value_int(PtrGet_sqlite3_value(jpSVal));
}

JDECL(jlong,1value_1int64)(JENV_JSELF, jobject jpSVal){
  return (jlong) sqlite3_value_int64(PtrGet_sqlite3_value(jpSVal));
}

JDECL(jobject,1value_1java_1object)(JENV_JSELF, jobject jpSVal){
  ResultJavaVal * const rv = sqlite3_value_pointer(PtrGet_sqlite3_value(jpSVal), RESULT_JAVA_VAL_STRING);
  return rv ? rv->jObj : NULL;
}

JDECL(jstring,1value_1text)(JENV_JSELF, jobject jpSVal){
  sqlite3_value * const sv = PtrGet_sqlite3_value(jpSVal);
  int const n = sqlite3_value_bytes16(sv);
  const void * const p = sqlite3_value_text16(sv);
  return s3jni_text_to_jstring(env, p, n);
}

JDECL(jbyteArray,1value_1text_1utf8)(JENV_JSELF, jobject jpSVal){
  sqlite3_value * const sv = PtrGet_sqlite3_value(jpSVal);
  int const n = sqlite3_value_bytes(sv);
  const unsigned char * const p = sqlite3_value_text(sv);
  return s3jni_new_jbyteArray(env, p, n);
}

static jbyteArray value_text16(int mode, JNIEnv * const env, jobject jpSVal){
  int const nLen = sqlite3_value_bytes16(PtrGet_sqlite3_value(jpSVal));
  jbyteArray jba;
  const jbyte * pBytes;
  switch(mode){
    case SQLITE_UTF16:
      pBytes = sqlite3_value_text16(PtrGet_sqlite3_value(jpSVal));
      break;
    case SQLITE_UTF16LE:
      pBytes = sqlite3_value_text16le(PtrGet_sqlite3_value(jpSVal));
      break;
    case SQLITE_UTF16BE:
      pBytes = sqlite3_value_text16be(PtrGet_sqlite3_value(jpSVal));
      break;
    default:
      assert(!"not possible");
      return NULL;
  }
  jba = pBytes
    ? (*env)->NewByteArray(env, (jsize)nLen)
    : NULL;
  if(jba){
    (*env)->SetByteArrayRegion(env, jba, 0, nLen, pBytes);
  }
  return jba;
}

JDECL(jbyteArray,1value_1text16)(JENV_JSELF, jobject jpSVal){
  return value_text16(SQLITE_UTF16, env, jpSVal);
}

JDECL(jbyteArray,1value_1text16le)(JENV_JSELF, jobject jpSVal){
  return value_text16(SQLITE_UTF16LE, env, jpSVal);
}

JDECL(jbyteArray,1value_1text16be)(JENV_JSELF, jobject jpSVal){
  return value_text16(SQLITE_UTF16BE, env, jpSVal);
}

JDECL(void,1do_1something_1for_1developer)(JENV_JSELF){
  MARKER(("\nVarious bits of internal info:\n"));
  puts("sizeofs:");
#define SO(T) printf("\tsizeof(" #T ") = %u\n", (unsigned)sizeof(T))
  SO(void*);
  SO(JniHookState);
  SO(PerDbStateJni);
  SO(S3Global);
  SO(JNIEnvCache);
  printf("Cache info:\n");
  printf("\tNativePointerHolder cache: %u misses, %u hits\n",
         S3Global.metrics.nphCacheMisses,
         S3Global.metrics.nphCacheHits);
  printf("\tJNIEnv cache               %u misses, %u hits\n",
         S3Global.metrics.envCacheMisses,
         S3Global.metrics.envCacheHits);
  puts("UDF calls:");
#define UDF(T) printf("\t%-8s = %u\n", "x" #T, S3Global.metrics.udf.n##T)
  UDF(Func); UDF(Step); UDF(Final); UDF(Value); UDF(Inverse);
#undef UDF
  printf("xDestroy calls across all callback types: %u\n",
         S3Global.metrics.nDestroy);
#undef SO
}

////////////////////////////////////////////////////////////////////////
// End of the sqlite3_... API bindings. Next up, FTS5...
////////////////////////////////////////////////////////////////////////
#ifdef SQLITE_ENABLE_FTS5

/* Creates a verbose JNI Fts5 function name. */
#define JFuncNameFts5Ext(Suffix)                  \
  Java_org_sqlite_jni_Fts5ExtensionApi_ ## Suffix

#define JDECLFts(ReturnType,Suffix)             \
  JNIEXPORT ReturnType JNICALL                  \
  JFuncNameFts5Ext(Suffix)

#define PtrGet_Fts5Context(OBJ) getNativePointer(env,OBJ,S3ClassNames.Fts5Context)
static inline Fts5ExtensionApi const * s3jni_ftsext(void){
  return &sFts5Api/*singleton from sqlite3.c*/;
}
#define Fts5ExtDecl Fts5ExtensionApi const * const fext = s3jni_ftsext()

#if 0
static jobject new_Fts5Context_wrapper(JNIEnv * const env, Fts5Context *sv){
  return new_NativePointerHolder_object(env, S3ClassNames.Fts5Context, sv);
}
#endif

static jobject s3jni_getFts5ExensionApi(JNIEnv * const env){
  JNIEnvCacheLine * const row = S3Global_env_cache(env);
  if( !row->jFtsExt ){
    row->jFtsExt = new_NativePointerHolder_object(env, S3ClassNames.Fts5ExtensionApi,
                                                  s3jni_ftsext());
    if(row->jFtsExt) row->jFtsExt = REF_G(row->jFtsExt);
  }
  return row->jFtsExt;
}

JDECLFts(jobject,getInstance)(JENV_JSELF){
  return s3jni_getFts5ExensionApi(env);
}

JDECLFts(jint,xColumnCount)(JENV_JSELF,jobject jCtx){
  Fts5ExtDecl;
  return (jint)fext->xColumnCount(PtrGet_Fts5Context(jCtx));
}

JDECLFts(jint,xColumnTotalSize)(JENV_JSELF,jobject jCtx, jint iCol, jobject jOut64){
  Fts5ExtDecl;
  sqlite3_int64 nOut = 0;
  int const rc = fext->xColumnTotalSize(PtrGet_Fts5Context(jCtx), (int)iCol, &nOut);
  if( 0==rc && jOut64 ) setOutputInt64(env, jOut64, (jlong)nOut);
  return (jint)rc;
}

JDECLFts(jint,xPhraseCount)(JENV_JSELF,jobject jCtx){
  Fts5ExtDecl;
  return (jint)fext->xPhraseCount(PtrGet_Fts5Context(jCtx));
}

JDECLFts(jint,xPhraseSize)(JENV_JSELF,jobject jCtx, jint iPhrase){
  Fts5ExtDecl;
  return (jint)fext->xPhraseSize(PtrGet_Fts5Context(jCtx), (int)iPhrase);
}

JDECLFts(jint,xRowCount)(JENV_JSELF,jobject jCtx, jobject jOut64){
  Fts5ExtDecl;
  sqlite3_int64 nOut = 0;
  int const rc = fext->xRowCount(PtrGet_Fts5Context(jCtx), &nOut);
  if( 0==rc && jOut64 ) setOutputInt64(env, jOut64, (jlong)nOut);
  return (jint)rc;
}


#endif /* SQLITE_ENABLE_FTS5 */
////////////////////////////////////////////////////////////////////////
// End of the main API bindings. What follows are internal utilities.
////////////////////////////////////////////////////////////////////////


/**
   Called during static init of the SQLite3Jni class to sync certain
   compile-time constants to Java-space.

   This routine is why we have to #include sqlite3.c instead of
   sqlite3.h.
*/
JNIEXPORT void JNICALL
Java_org_sqlite_jni_SQLite3Jni_init(JNIEnv * const env, jclass self, jobject sJni){
  typedef struct {
    const char *zName;
    int value;
  } LimitEntry;
  const LimitEntry aLimits[] = {
    {"SQLITE_MAX_ALLOCATION_SIZE", SQLITE_MAX_ALLOCATION_SIZE},
    {"SQLITE_LIMIT_LENGTH", SQLITE_LIMIT_LENGTH},
    {"SQLITE_MAX_LENGTH", SQLITE_MAX_LENGTH},
    {"SQLITE_LIMIT_SQL_LENGTH", SQLITE_LIMIT_SQL_LENGTH},
    {"SQLITE_MAX_SQL_LENGTH", SQLITE_MAX_SQL_LENGTH},
    {"SQLITE_LIMIT_COLUMN", SQLITE_LIMIT_COLUMN},
    {"SQLITE_MAX_COLUMN", SQLITE_MAX_COLUMN},
    {"SQLITE_LIMIT_EXPR_DEPTH", SQLITE_LIMIT_EXPR_DEPTH},
    {"SQLITE_MAX_EXPR_DEPTH", SQLITE_MAX_EXPR_DEPTH},
    {"SQLITE_LIMIT_COMPOUND_SELECT", SQLITE_LIMIT_COMPOUND_SELECT},
    {"SQLITE_MAX_COMPOUND_SELECT", SQLITE_MAX_COMPOUND_SELECT},
    {"SQLITE_LIMIT_VDBE_OP", SQLITE_LIMIT_VDBE_OP},
    {"SQLITE_MAX_VDBE_OP", SQLITE_MAX_VDBE_OP},
    {"SQLITE_LIMIT_FUNCTION_ARG", SQLITE_LIMIT_FUNCTION_ARG},
    {"SQLITE_MAX_FUNCTION_ARG", SQLITE_MAX_FUNCTION_ARG},
    {"SQLITE_LIMIT_ATTACHED", SQLITE_LIMIT_ATTACHED},
    {"SQLITE_MAX_ATTACHED", SQLITE_MAX_ATTACHED},
    {"SQLITE_LIMIT_LIKE_PATTERN_LENGTH", SQLITE_LIMIT_LIKE_PATTERN_LENGTH},
    {"SQLITE_MAX_LIKE_PATTERN_LENGTH", SQLITE_MAX_LIKE_PATTERN_LENGTH},
    {"SQLITE_LIMIT_VARIABLE_NUMBER", SQLITE_LIMIT_VARIABLE_NUMBER},
    {"SQLITE_MAX_VARIABLE_NUMBER", SQLITE_MAX_VARIABLE_NUMBER},
    {"SQLITE_LIMIT_TRIGGER_DEPTH", SQLITE_LIMIT_TRIGGER_DEPTH},
    {"SQLITE_MAX_TRIGGER_DEPTH", SQLITE_MAX_TRIGGER_DEPTH},
    {"SQLITE_LIMIT_WORKER_THREADS", SQLITE_LIMIT_WORKER_THREADS},
    {"SQLITE_MAX_WORKER_THREADS", SQLITE_MAX_WORKER_THREADS},
    {0,0}
  };
  jfieldID fieldId;
  jclass const klazz = (*env)->GetObjectClass(env, sJni);
  const LimitEntry * pLimit;
  memset(&S3Global, 0, sizeof(S3Global));
  (void)S3Global_env_cache(env);
  assert( 1 == S3Global.envCache.used );
  assert( env == S3Global.envCache.lines[0].env );
  assert( 0 != S3Global.envCache.lines[0].globalClassObj );
  if( (*env)->GetJavaVM(env, &S3Global.jvm) ){
    (*env)->FatalError(env, "GetJavaVM() failure shouldn't be possible.");
  }

  for( pLimit = &aLimits[0]; pLimit->zName; ++pLimit ){
    fieldId = (*env)->GetStaticFieldID(env, klazz, pLimit->zName, "I");
    EXCEPTION_IS_FATAL("Missing an expected static member of the SQLite3Jni class.");
    //MARKER(("Setting %s (field=%p) = %d\n", pLimit->zName, fieldId, pLimit->value));
    assert(fieldId);
    (*env)->SetStaticIntField(env, klazz, fieldId, (jint)pLimit->value);
    EXCEPTION_IS_FATAL("Seting a static member of the SQLite3Jni class failed.");
  }
}