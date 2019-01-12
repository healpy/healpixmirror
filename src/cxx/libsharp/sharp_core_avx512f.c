#if (!defined(__AVX512F__)) && defined(__GNUC__) && defined (__x86_64__) && (__GNUC__>=6)

#define XCONCATX(a,b) a##_##b
#define XCONCATX2(a,b) XCONCATX(a,b)
#define XARCH(a) XCONCATX2(a,ARCH)

#define ARCH avx512f
#pragma GCC target("avx512f")
#include "sharp_core_inc.c"

#endif
