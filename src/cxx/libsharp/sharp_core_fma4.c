#if (!defined(__FMA4__)) && defined(__GNUC__) && defined (__x86_64__) && (__GNUC__>=6)

#define XCONCATX(a,b) a##_##b
#define XCONCATX2(a,b) XCONCATX(a,b)
#define XARCH(a) XCONCATX2(a,ARCH)

#define ARCH fma4
#pragma GCC target("fma4")
#include "sharp_core_inc.c"

#endif
