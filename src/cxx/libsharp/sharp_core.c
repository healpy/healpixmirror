#define ARCH default
#define GENERIC_ARCH
#include "sharp_core_inc.c"
#undef GENERIC_ARCH
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

#ifdef MULTIARCH

#if (defined(___AVX512F__) || defined(__FMA4__) || defined(__FMA__) || \
     defined(__AVX2__) || defined(__AVX__))
#error MULTIARCH specified but platform-specific flags detected
#endif

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

DECL(avx512f)
DECL(fma4)
DECL(fma)
DECL(avx2)
DECL(avx)

#endif

static void assign_funcs(void)
  {
#ifdef MULTIARCH
#define DECL2(arch) \
  if (XCONCATX2(have,arch)()) \
    { \
    inner_loop_ = XCONCATX2(inner_loop,arch); \
    veclen_ = XCONCATX2(sharp_veclen,arch); \
    max_nvec_ = XCONCATX2(sharp_max_nvec,arch); \
    architecture_ = XCONCATX2(sharp_architecture,arch); \
    return; \
    }
DECL2(avx512f)
DECL2(fma4)
DECL2(fma)
DECL2(avx2)
DECL2(avx)
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
