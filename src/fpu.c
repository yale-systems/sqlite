#include "fpu.h"



#if !defined(SQLITE_OMIT_FLOATING_POINT) && defined(LINUX_KERNEL_BUILD)
# include <linux/asm/fpu.h>

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


double __floatditf(long a){
  return (double)a;
}

double __multf3(double a, double b){
  return a*b;
}

int __fixtfdi(double a){
  return (int)a;
}
  
#endif

