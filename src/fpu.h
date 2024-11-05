#ifndef SQLITE_FPU_H
#define SQLITE_FPU_H

#if !defined(SQLITE_OMIT_FLOATING_POINT) && defined(LINUX_KERNEL_BUILD)

void enterFPURegion(void);
void exitFPURegion(void);

double __floatditf(long a);
double __multf3(double a, double b);
int __fixtfdi(double a);
int __getf2(double a, double b);
double __addtf3(double a, double b);
int __fixunstfdi(double a);
int __lttf2(double a, double b);
int __netf2(double a, double b);
double __extenddftf2(double a);
int __gttf2(double a, double b);
double __floatunditf(unsigned long a);
double __trunctfdf2(double a);

#else
# define enterFPURegion() (void)0
# define exitFPURegion() (void)0
#endif

#endif
