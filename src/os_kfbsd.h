#ifndef FREEBSD_KERNEL_H
#define FREEBSD_KERNEL_H

// XXX gnn@ - these remain in the overall module but as yet are uncalled.

double __floatditf(long a) {
    //todo: STELIOS
    printf("Warning: Symbol __floatditf is undefined - this function should not be called!\n");
    return 0.0;
}

double __multf3(double a, double b) {
    //todo: STELIOS
    printf("Warning: Symbol __multf3 is undefined - this function should not be called!\n");
    return 0.0;
}

int __fixtfdi(double a) {
    //todo: STELIOS
    printf("Warning: Symbol __fixtfdi is undefined - this function should not be called!\n");
    return 0;
}

#endif
