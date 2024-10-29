#ifndef SQLITE_FPU_H
#define SQLITE_FPU_H

double __floatditf(long a) {
    //TODO
    return 0.0;
}

double __multf3(double a, double b) {
    //TODO
    return 0.0;
}

int __fixtfdi(double a) {
    //TODO
    return 0;
}

#if !defined(SQLITE_OMIT_FLOATING_POINT) && defined(LINUX_KERNEL_BUILD)

void enterFPURegion(void);
void exitFPURegion(void);


#else
# define enterFPURegion() (void)0
# define exitFPURegion() (void)0
#endif

#endif
