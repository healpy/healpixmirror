#if (!defined(__FMA__)) && defined(__GNUC__) && defined (__x86_64__) && (__GNUC__>=6)

#define XCONCATX(a,b) a##_##b
#define XCONCATX2(a,b) XCONCATX(a,b)
#define XARCH(a) XCONCATX2(a,ARCH)

#define ARCH fma
#pragma GCC target("fma")
#include "sharp_core_inc.c"

#endif
