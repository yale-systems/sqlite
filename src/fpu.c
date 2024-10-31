#include "fpu.h"


#if !defined(SQLITE_OMIT_FLOATING_POINT) && defined(LINUX_KERNEL_BUILD)
# if defined(CONFIG_X86_64)
#  include <asm/fpu/api.h>
# elif defined(CONFIG_ARM) || defined(CONFIG_ARM64)
#  include <asm/neon.h>
#  define kernel_fpu_available() cpu_has_neon()
#  define kernel_fpu_begin() kernel_neon_begin()
#  define kernel_fpu_end() kernel_neon_end()
# else
#  error "Not a supported architecture"
# endif

static int usingFPU = 0;

void enterFPURegion(void){
  if (!usingFPU) {
    kernel_fpu_begin();
    usingFPU = 1;
  }
}

void exitFPURegion(void){
  if (usingFPU) {
    kernel_fpu_end();
    usingFPU = 0;
  }
}


double __floatditf(long a) {
  // TODO replace the dummy implementation with a real one
  return 0.0;
}

double __multf3(double a, double b) {
  // TODO replace the dummy implementation with a real one
  return 0.0;
}

int __fixtfdi(double a) {
  // TODO replace the dummy implementation with a real one
  return 0;
}

int __getf2(double a, double b) {
  // TODO replace the dummy implementation with a real one
  return 0;
}

double __addtf3(double a, double b) {
  // TODO replace the dummy implementation with a real one
  return 0.0;
}

int __fixunstfdi(double a) {
  // TODO replace the dummy implementation with a real one
  return 0;
}

int __lttf2(double a, double b) {
  // TODO replace the dummy implementation with a real one
  return 0;
}

int __netf2(double a, double b) {
  // TODO replace the dummy implementation with a real one
  return 0;
}

double __extenddftf2(double a) {
  // TODO replace the dummy implementation with a real one
  return 0.0;
}

int __gttf2(double a, double b) {
  // TODO replace the dummy implementation with a real one
  return 0;
}

double __floatunditf(unsigned long a) {
  // TODO replace the dummy implementation with a real one
  return 0.0;
}

double __trunctfdf2(double a) {
  // TODO replace the dummy implementation with a real one
  return 0.0;
}
  
#endif
