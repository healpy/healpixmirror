#if (!defined(__AVX__)) && defined(__GNUC__) && defined (__x86_64__) && (__GNUC__>=5)
// if we arrive here, we can benefit from an additional AVX version
// #warning entering gcc and x86_64 specific code branch

#define ARCH _avx
//#define __AVX__
#pragma GCC push_options
#pragma GCC target("avx")
#include "sharp_core_inc0.c"
#pragma GCC pop_options
//#undef __AVX__
#undef ARCH

#endif
