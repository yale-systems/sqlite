#ifndef FREEBSD_KERNEL_H
#define FREEBSD_KERNEL_H

#include <sys/systm.h>
#include <sys/types.h>
#include <sys/libkern.h>

double __floatdidf(long long a) {
    printf("%lld\n",a);
    if (a == 0) return 0.0;

    int sign = a < 0 ? -1 : 1;

    //use positive integer value for ease
    long long u = sign > 0 ? a : -a;

    //find highest integer bit set
    int shift = 63;
    while ((u & (1ULL << shift)) == 0 && shift > 0) shift--;

    long long mantissa;
    int exp = 1023 + shift;
    if (shift > 52) {
        mantissa = (u >> (shift - 52)) & 0xFFFFFFFFFFFFF;
    } else {
        mantissa = (u << (52 - shift)) & 0xFFFFFFFFFFFFF;
    }

    long long double_bits = ((long long)(sign == -1) << 63) | ((long long)exp << 52) | mantissa;
    double res;
    bcopy(&double_bits, &res, sizeof(res));
    printf("__floatdidf was called\n");

    return(res);
}

double __floatditf(long long a) {
    if (a == 0) return 0.0;

    int sign = a < 0 ? -1 : 1;

    //use positive integer value for ease
    long long u = sign > 0 ? a : -a;

    //find highest integer bit set
    int shift = 63;
    while ((u & (1ULL << shift)) == 0 && shift > 0) shift--;

    long long mantissa;
    int exp = 1023 + shift;
    if (shift > 52) {
        mantissa = (u >> (shift - 52)) & 0xFFFFFFFFFFFFF;
    } else {
        mantissa = (u << (52 - shift)) & 0xFFFFFFFFFFFFF;
    }

    long long double_bits = ((long long)(sign == -1) << 63) | ((long long)exp << 52) | mantissa;
    double res;
    bcopy(&double_bits, &res, sizeof(res));
    printf("__floatditf was called\n");

    return(res);
}

int __gtdf2(double a, double b) {
    int res;
    if (a > b) res = 1; 
    else if (a < b) res = -1;
    else res = 0;
    printf("__gtdf2 was called with a = %lld and b = %lld and res = %d\n", a, b, res);
    return res;
}

int __ltdf2(double a, double b) {
    printf("__ltdf2 was called\n");
    if (a < b) return 1;
    else if (a > b) return -1;
    else return 0;
}

int __gedf2(double a, double b) {
    printf("__gedf2 was called\n");
    if (a > b) return 1;
    else if (a == b) return 0;
    else return -1;
}

double __multf3(double a, double b) {
    printf("__multf3 was called\n");
    return a*b;
}

double __muldf3(double a, double b) {
    printf("__muldf3 was called\n");
    return a*b;
}

int __fixdfdi(double a) {
    //todo: STELIOS
    printf("Warning: Symbol __fixdfdi is undefined - this function should not be called!\n");
    return 0;
}

int __fixtfdi(double a) {
    //todo: STELIOS
    printf("Warning: Symbol __fixtfdi is undefined - this function should not be called!\n");
    return 0;
}

int __fixdfsi(double a) {
    //todo: STELIOS
    printf("Warning: Symbol __fixdfsi is undefined - this function should not be called!\n");
    return 0;
}

double fma(double a, double b, double c) {
    //todo: STELIOS
    printf("Warning: Symbol fma is undefined - this function should not be called!\n");
    return 0.0;
}

#endif