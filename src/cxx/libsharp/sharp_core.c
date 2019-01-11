#define XCONCATX(a,b) a##_##b
#define XCONCATX2(a,b) XCONCATX(a,b)
#define XARCH(a) XCONCATX2(a,ARCH)

#define ARCH default
#include "sharp_core_inc.c"
#undef ARCH

typedef void (*t_inner_loop) (sharp_job *job, const int *ispair,
  const double *cth_, const double *sth_, int llim, int ulim,
  sharp_Ylmgen_C *gen, int mi, const int *mlim);
typedef int (*t_veclen) (void);
typedef int (*t_max_nvec) (int spin);
typedef const char *(*t_architecture) (void);

static t_inner_loop inner_loop_ = NULL;
static t_veclen veclen_ = NULL;
static t_max_nvec max_nvec_ = NULL;
static t_architecture architecture_ = NULL;

#if defined(__GNUC__) && defined (__x86_64__) && (__GNUC__>=6)

#define DECL(arch) \
static int XCONCATX2(have,arch)(void) \
  { \
  static int res=-1; \
  if (res<0) \
    { \
    __builtin_cpu_init(); \
    res = __builtin_cpu_supports(#arch); \
    } \
  return res; \
  } \
\
void XCONCATX2(inner_loop,arch) (sharp_job *job, const int *ispair, \
  const double *cth_, const double *sth_, int llim, int ulim, \
  sharp_Ylmgen_C *gen, int mi, const int *mlim); \
int XCONCATX2(sharp_veclen,arch) (void); \
int XCONCATX2(sharp_max_nvec,arch) (int spin); \
const char *XCONCATX2(sharp_architecture,arch) (void);

#if (!defined(__AVX512F__))
DECL(avx512f)
#endif
#if (!defined(__FMA4__))
DECL(fma4)
#endif
#if (!defined(__FMA__))
DECL(fma)
#endif
#if (!defined(__AVX2__))
DECL(avx2)
#endif
#if (!defined(__AVX__))
DECL(avx)
#endif

#endif

static void assign_funcs(void)
  {
#if defined(__GNUC__) && defined (__x86_64__) && (__GNUC__>=6)
#define DECL2(arch) \
  if (XCONCATX2(have,arch)()) \
    { \
    inner_loop_ = XCONCATX2(inner_loop,arch); \
    veclen_ = XCONCATX2(sharp_veclen,arch); \
    max_nvec_ = XCONCATX2(sharp_max_nvec,arch); \
    architecture_ = XCONCATX2(sharp_architecture,arch); \
    return; \
    }
#if (!defined(__AVX512F__))
DECL2(avx512f)
#endif
#if (!defined(__FMA4__))
DECL2(fma4)
#endif
#if (!defined(__FMA__))
DECL2(fma)
#endif
#if (!defined(__AVX2__))
DECL2(avx2)
#endif
#if (!defined(__AVX__))
DECL2(avx)
#endif
#endif
  inner_loop_ = inner_loop_default;
  veclen_ = sharp_veclen_default;
  max_nvec_ = sharp_max_nvec_default;
  architecture_ = sharp_architecture_default;
  }


void inner_loop (sharp_job *job, const int *ispair,const double *cth,
  const double *sth, int llim, int ulim, sharp_Ylmgen_C *gen, int mi,
  const int *mlim)
  {
  if (!inner_loop_) assign_funcs();
  inner_loop_(job, ispair, cth, sth, llim, ulim, gen, mi, mlim);
  }
int sharp_veclen(void)
  {
  if (!veclen_) assign_funcs();
  return veclen_();
  }
int sharp_max_nvec(int spin)
  {
  if (!max_nvec_) assign_funcs();
  return max_nvec_(spin);
  }
const char *sharp_architecture(void)
  {
  if (!architecture_) assign_funcs();
  return architecture_();
  }
