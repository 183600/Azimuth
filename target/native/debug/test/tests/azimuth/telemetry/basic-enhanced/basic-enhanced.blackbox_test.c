#ifdef __cplusplus
extern "C" {
#endif

#include "moonbit.h"

#ifdef _MSC_VER
#define _Noreturn __declspec(noreturn)
#endif

#if defined(__clang__)
#pragma clang diagnostic ignored "-Wshift-op-parentheses"
#pragma clang diagnostic ignored "-Wtautological-compare"
#endif

MOONBIT_EXPORT _Noreturn void moonbit_panic(void);
MOONBIT_EXPORT void *moonbit_malloc_array(enum moonbit_block_kind kind,
                                          int elem_size_shift, int32_t len);
MOONBIT_EXPORT int moonbit_val_array_equal(const void *lhs, const void *rhs);
MOONBIT_EXPORT moonbit_string_t moonbit_add_string(moonbit_string_t s1,
                                                   moonbit_string_t s2);
MOONBIT_EXPORT void moonbit_unsafe_bytes_blit(moonbit_bytes_t dst,
                                              int32_t dst_start,
                                              moonbit_bytes_t src,
                                              int32_t src_offset, int32_t len);
MOONBIT_EXPORT moonbit_string_t moonbit_unsafe_bytes_sub_string(
    moonbit_bytes_t bytes, int32_t start, int32_t len);
MOONBIT_EXPORT void moonbit_println(moonbit_string_t str);
MOONBIT_EXPORT moonbit_bytes_t *moonbit_get_cli_args(void);
MOONBIT_EXPORT void moonbit_runtime_init(int argc, char **argv);
MOONBIT_EXPORT void moonbit_drop_object(void *);

#define Moonbit_make_regular_object_header(ptr_field_offset, ptr_field_count,  \
                                           tag)                                \
  (((uint32_t)moonbit_BLOCK_KIND_REGULAR << 30) |                              \
   (((uint32_t)(ptr_field_offset) & (((uint32_t)1 << 11) - 1)) << 19) |        \
   (((uint32_t)(ptr_field_count) & (((uint32_t)1 << 11) - 1)) << 8) |          \
   ((tag) & 0xFF))

// header manipulation macros
#define Moonbit_object_ptr_field_offset(obj)                                   \
  ((Moonbit_object_header(obj)->meta >> 19) & (((uint32_t)1 << 11) - 1))

#define Moonbit_object_ptr_field_count(obj)                                    \
  ((Moonbit_object_header(obj)->meta >> 8) & (((uint32_t)1 << 11) - 1))

#if !defined(_WIN64) && !defined(_WIN32)
void *malloc(size_t size);
void free(void *ptr);
#define libc_malloc malloc
#define libc_free free
#endif

// several important runtime functions are inlined
static void *moonbit_malloc_inlined(size_t size) {
  struct moonbit_object *ptr = (struct moonbit_object *)libc_malloc(
      sizeof(struct moonbit_object) + size);
  ptr->rc = 1;
  return ptr + 1;
}

#define moonbit_malloc(obj) moonbit_malloc_inlined(obj)
#define moonbit_free(obj) libc_free(Moonbit_object_header(obj))

static void moonbit_incref_inlined(void *ptr) {
  struct moonbit_object *header = Moonbit_object_header(ptr);
  int32_t const count = header->rc;
  if (count > 0) {
    header->rc = count + 1;
  }
}

#define moonbit_incref moonbit_incref_inlined

static void moonbit_decref_inlined(void *ptr) {
  struct moonbit_object *header = Moonbit_object_header(ptr);
  int32_t const count = header->rc;
  if (count > 1) {
    header->rc = count - 1;
  } else if (count == 1) {
    moonbit_drop_object(ptr);
  }
}

#define moonbit_decref moonbit_decref_inlined

#define moonbit_unsafe_make_string moonbit_make_string

// detect whether compiler builtins exist for advanced bitwise operations
#ifdef __has_builtin

#if __has_builtin(__builtin_clz)
#define HAS_BUILTIN_CLZ
#endif

#if __has_builtin(__builtin_ctz)
#define HAS_BUILTIN_CTZ
#endif

#if __has_builtin(__builtin_popcount)
#define HAS_BUILTIN_POPCNT
#endif

#if __has_builtin(__builtin_sqrt)
#define HAS_BUILTIN_SQRT
#endif

#if __has_builtin(__builtin_sqrtf)
#define HAS_BUILTIN_SQRTF
#endif

#if __has_builtin(__builtin_fabs)
#define HAS_BUILTIN_FABS
#endif

#if __has_builtin(__builtin_fabsf)
#define HAS_BUILTIN_FABSF
#endif

#endif

// if there is no builtin operators, use software implementation
#ifdef HAS_BUILTIN_CLZ
static inline int32_t moonbit_clz32(int32_t x) {
  return x == 0 ? 32 : __builtin_clz(x);
}

static inline int32_t moonbit_clz64(int64_t x) {
  return x == 0 ? 64 : __builtin_clzll(x);
}

#undef HAS_BUILTIN_CLZ
#else
// table for [clz] value of 4bit integer.
static const uint8_t moonbit_clz4[] = {4, 3, 2, 2, 1, 1, 1, 1,
                                       0, 0, 0, 0, 0, 0, 0, 0};

int32_t moonbit_clz32(uint32_t x) {
  /* The ideas is to:

     1. narrow down the 4bit block where the most signficant "1" bit lies,
        using binary search
     2. find the number of leading zeros in that 4bit block via table lookup

     Different time/space tradeoff can be made here by enlarging the table
     and do less binary search.
     One benefit of the 4bit lookup table is that it can fit into a single cache
     line.
  */
  int32_t result = 0;
  if (x > 0xffff) {
    x >>= 16;
  } else {
    result += 16;
  }
  if (x > 0xff) {
    x >>= 8;
  } else {
    result += 8;
  }
  if (x > 0xf) {
    x >>= 4;
  } else {
    result += 4;
  }
  return result + moonbit_clz4[x];
}

int32_t moonbit_clz64(uint64_t x) {
  int32_t result = 0;
  if (x > 0xffffffff) {
    x >>= 32;
  } else {
    result += 32;
  }
  return result + moonbit_clz32((uint32_t)x);
}
#endif

#ifdef HAS_BUILTIN_CTZ
static inline int32_t moonbit_ctz32(int32_t x) {
  return x == 0 ? 32 : __builtin_ctz(x);
}

static inline int32_t moonbit_ctz64(int64_t x) {
  return x == 0 ? 64 : __builtin_ctzll(x);
}

#undef HAS_BUILTIN_CTZ
#else
int32_t moonbit_ctz32(int32_t x) {
  /* The algorithm comes from:

       Leiserson, Charles E. et al. “Using de Bruijn Sequences to Index a 1 in a
     Computer Word.” (1998).

     The ideas is:

     1. leave only the least significant "1" bit in the input,
        set all other bits to "0". This is achieved via [x & -x]
     2. now we have [x * n == n << ctz(x)], if [n] is a de bruijn sequence
        (every 5bit pattern occurn exactly once when you cycle through the bit
     string), we can find [ctz(x)] from the most significant 5 bits of [x * n]
 */
  static const uint32_t de_bruijn_32 = 0x077CB531;
  static const uint8_t index32[] = {0,  1,  28, 2,  29, 14, 24, 3,  30, 22, 20,
                                    15, 25, 17, 4,  8,  31, 27, 13, 23, 21, 19,
                                    16, 7,  26, 12, 18, 6,  11, 5,  10, 9};
  return (x == 0) * 32 + index32[(de_bruijn_32 * (x & -x)) >> 27];
}

int32_t moonbit_ctz64(int64_t x) {
  static const uint64_t de_bruijn_64 = 0x0218A392CD3D5DBF;
  static const uint8_t index64[] = {
      0,  1,  2,  7,  3,  13, 8,  19, 4,  25, 14, 28, 9,  34, 20, 40,
      5,  17, 26, 38, 15, 46, 29, 48, 10, 31, 35, 54, 21, 50, 41, 57,
      63, 6,  12, 18, 24, 27, 33, 39, 16, 37, 45, 47, 30, 53, 49, 56,
      62, 11, 23, 32, 36, 44, 52, 55, 61, 22, 43, 51, 60, 42, 59, 58};
  return (x == 0) * 64 + index64[(de_bruijn_64 * (x & -x)) >> 58];
}
#endif

#ifdef HAS_BUILTIN_POPCNT

#define moonbit_popcnt32 __builtin_popcount
#define moonbit_popcnt64 __builtin_popcountll
#undef HAS_BUILTIN_POPCNT

#else
int32_t moonbit_popcnt32(uint32_t x) {
  /* The classic SIMD Within A Register algorithm.
     ref: [https://nimrod.blog/posts/algorithms-behind-popcount/]
 */
  x = x - ((x >> 1) & 0x55555555);
  x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  x = (x + (x >> 4)) & 0x0F0F0F0F;
  return (x * 0x01010101) >> 24;
}

int32_t moonbit_popcnt64(uint64_t x) {
  x = x - ((x >> 1) & 0x5555555555555555);
  x = (x & 0x3333333333333333) + ((x >> 2) & 0x3333333333333333);
  x = (x + (x >> 4)) & 0x0F0F0F0F0F0F0F0F;
  return (x * 0x0101010101010101) >> 56;
}
#endif

/* The following sqrt implementation comes from
   [musl](https://git.musl-libc.org/cgit/musl),
   with some helpers inlined to make it zero dependency.
 */
#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
const uint16_t __rsqrt_tab[128] = {
    0xb451, 0xb2f0, 0xb196, 0xb044, 0xaef9, 0xadb6, 0xac79, 0xab43, 0xaa14,
    0xa8eb, 0xa7c8, 0xa6aa, 0xa592, 0xa480, 0xa373, 0xa26b, 0xa168, 0xa06a,
    0x9f70, 0x9e7b, 0x9d8a, 0x9c9d, 0x9bb5, 0x9ad1, 0x99f0, 0x9913, 0x983a,
    0x9765, 0x9693, 0x95c4, 0x94f8, 0x9430, 0x936b, 0x92a9, 0x91ea, 0x912e,
    0x9075, 0x8fbe, 0x8f0a, 0x8e59, 0x8daa, 0x8cfe, 0x8c54, 0x8bac, 0x8b07,
    0x8a64, 0x89c4, 0x8925, 0x8889, 0x87ee, 0x8756, 0x86c0, 0x862b, 0x8599,
    0x8508, 0x8479, 0x83ec, 0x8361, 0x82d8, 0x8250, 0x81c9, 0x8145, 0x80c2,
    0x8040, 0xff02, 0xfd0e, 0xfb25, 0xf947, 0xf773, 0xf5aa, 0xf3ea, 0xf234,
    0xf087, 0xeee3, 0xed47, 0xebb3, 0xea27, 0xe8a3, 0xe727, 0xe5b2, 0xe443,
    0xe2dc, 0xe17a, 0xe020, 0xdecb, 0xdd7d, 0xdc34, 0xdaf1, 0xd9b3, 0xd87b,
    0xd748, 0xd61a, 0xd4f1, 0xd3cd, 0xd2ad, 0xd192, 0xd07b, 0xcf69, 0xce5b,
    0xcd51, 0xcc4a, 0xcb48, 0xca4a, 0xc94f, 0xc858, 0xc764, 0xc674, 0xc587,
    0xc49d, 0xc3b7, 0xc2d4, 0xc1f4, 0xc116, 0xc03c, 0xbf65, 0xbe90, 0xbdbe,
    0xbcef, 0xbc23, 0xbb59, 0xba91, 0xb9cc, 0xb90a, 0xb84a, 0xb78c, 0xb6d0,
    0xb617, 0xb560,
};

/* returns a*b*2^-32 - e, with error 0 <= e < 1.  */
static inline uint32_t mul32(uint32_t a, uint32_t b) {
  return (uint64_t)a * b >> 32;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
float sqrtf(float x) {
  uint32_t ix, m, m1, m0, even, ey;

  ix = *(uint32_t *)&x;
  if (ix - 0x00800000 >= 0x7f800000 - 0x00800000) {
    /* x < 0x1p-126 or inf or nan.  */
    if (ix * 2 == 0)
      return x;
    if (ix == 0x7f800000)
      return x;
    if (ix > 0x7f800000)
      return (x - x) / (x - x);
    /* x is subnormal, normalize it.  */
    x *= 0x1p23f;
    ix = *(uint32_t *)&x;
    ix -= 23 << 23;
  }

  /* x = 4^e m; with int e and m in [1, 4).  */
  even = ix & 0x00800000;
  m1 = (ix << 8) | 0x80000000;
  m0 = (ix << 7) & 0x7fffffff;
  m = even ? m0 : m1;

  /* 2^e is the exponent part of the return value.  */
  ey = ix >> 1;
  ey += 0x3f800000 >> 1;
  ey &= 0x7f800000;

  /* compute r ~ 1/sqrt(m), s ~ sqrt(m) with 2 goldschmidt iterations.  */
  static const uint32_t three = 0xc0000000;
  uint32_t r, s, d, u, i;
  i = (ix >> 17) % 128;
  r = (uint32_t)__rsqrt_tab[i] << 16;
  /* |r*sqrt(m) - 1| < 0x1p-8 */
  s = mul32(m, r);
  /* |s/sqrt(m) - 1| < 0x1p-8 */
  d = mul32(s, r);
  u = three - d;
  r = mul32(r, u) << 1;
  /* |r*sqrt(m) - 1| < 0x1.7bp-16 */
  s = mul32(s, u) << 1;
  /* |s/sqrt(m) - 1| < 0x1.7bp-16 */
  d = mul32(s, r);
  u = three - d;
  s = mul32(s, u);
  /* -0x1.03p-28 < s/sqrt(m) - 1 < 0x1.fp-31 */
  s = (s - 1) >> 6;
  /* s < sqrt(m) < s + 0x1.08p-23 */

  /* compute nearest rounded result.  */
  uint32_t d0, d1, d2;
  float y, t;
  d0 = (m << 16) - s * s;
  d1 = s - d0;
  d2 = d1 + s + 1;
  s += d1 >> 31;
  s &= 0x007fffff;
  s |= ey;
  y = *(float *)&s;
  /* handle rounding and inexact exception. */
  uint32_t tiny = d2 == 0 ? 0 : 0x01000000;
  tiny |= (d1 ^ d2) & 0x80000000;
  t = *(float *)&tiny;
  y = y + t;
  return y;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
/* returns a*b*2^-64 - e, with error 0 <= e < 3.  */
static inline uint64_t mul64(uint64_t a, uint64_t b) {
  uint64_t ahi = a >> 32;
  uint64_t alo = a & 0xffffffff;
  uint64_t bhi = b >> 32;
  uint64_t blo = b & 0xffffffff;
  return ahi * bhi + (ahi * blo >> 32) + (alo * bhi >> 32);
}

double sqrt(double x) {
  uint64_t ix, top, m;

  /* special case handling.  */
  ix = *(uint64_t *)&x;
  top = ix >> 52;
  if (top - 0x001 >= 0x7ff - 0x001) {
    /* x < 0x1p-1022 or inf or nan.  */
    if (ix * 2 == 0)
      return x;
    if (ix == 0x7ff0000000000000)
      return x;
    if (ix > 0x7ff0000000000000)
      return (x - x) / (x - x);
    /* x is subnormal, normalize it.  */
    x *= 0x1p52;
    ix = *(uint64_t *)&x;
    top = ix >> 52;
    top -= 52;
  }

  /* argument reduction:
     x = 4^e m; with integer e, and m in [1, 4)
     m: fixed point representation [2.62]
     2^e is the exponent part of the result.  */
  int even = top & 1;
  m = (ix << 11) | 0x8000000000000000;
  if (even)
    m >>= 1;
  top = (top + 0x3ff) >> 1;

  /* approximate r ~ 1/sqrt(m) and s ~ sqrt(m) when m in [1,4)

     initial estimate:
     7bit table lookup (1bit exponent and 6bit significand).

     iterative approximation:
     using 2 goldschmidt iterations with 32bit int arithmetics
     and a final iteration with 64bit int arithmetics.

     details:

     the relative error (e = r0 sqrt(m)-1) of a linear estimate
     (r0 = a m + b) is |e| < 0.085955 ~ 0x1.6p-4 at best,
     a table lookup is faster and needs one less iteration
     6 bit lookup table (128b) gives |e| < 0x1.f9p-8
     7 bit lookup table (256b) gives |e| < 0x1.fdp-9
     for single and double prec 6bit is enough but for quad
     prec 7bit is needed (or modified iterations). to avoid
     one more iteration >=13bit table would be needed (16k).

     a newton-raphson iteration for r is
       w = r*r
       u = 3 - m*w
       r = r*u/2
     can use a goldschmidt iteration for s at the end or
       s = m*r

     first goldschmidt iteration is
       s = m*r
       u = 3 - s*r
       r = r*u/2
       s = s*u/2
     next goldschmidt iteration is
       u = 3 - s*r
       r = r*u/2
       s = s*u/2
     and at the end r is not computed only s.

     they use the same amount of operations and converge at the
     same quadratic rate, i.e. if
       r1 sqrt(m) - 1 = e, then
       r2 sqrt(m) - 1 = -3/2 e^2 - 1/2 e^3
     the advantage of goldschmidt is that the mul for s and r
     are independent (computed in parallel), however it is not
     "self synchronizing": it only uses the input m in the
     first iteration so rounding errors accumulate. at the end
     or when switching to larger precision arithmetics rounding
     errors dominate so the first iteration should be used.

     the fixed point representations are
       m: 2.30 r: 0.32, s: 2.30, d: 2.30, u: 2.30, three: 2.30
     and after switching to 64 bit
       m: 2.62 r: 0.64, s: 2.62, d: 2.62, u: 2.62, three: 2.62  */

  static const uint64_t three = 0xc0000000;
  uint64_t r, s, d, u, i;

  i = (ix >> 46) % 128;
  r = (uint32_t)__rsqrt_tab[i] << 16;
  /* |r sqrt(m) - 1| < 0x1.fdp-9 */
  s = mul32(m >> 32, r);
  /* |s/sqrt(m) - 1| < 0x1.fdp-9 */
  d = mul32(s, r);
  u = three - d;
  r = mul32(r, u) << 1;
  /* |r sqrt(m) - 1| < 0x1.7bp-16 */
  s = mul32(s, u) << 1;
  /* |s/sqrt(m) - 1| < 0x1.7bp-16 */
  d = mul32(s, r);
  u = three - d;
  r = mul32(r, u) << 1;
  /* |r sqrt(m) - 1| < 0x1.3704p-29 (measured worst-case) */
  r = r << 32;
  s = mul64(m, r);
  d = mul64(s, r);
  u = (three << 32) - d;
  s = mul64(s, u); /* repr: 3.61 */
  /* -0x1p-57 < s - sqrt(m) < 0x1.8001p-61 */
  s = (s - 2) >> 9; /* repr: 12.52 */
  /* -0x1.09p-52 < s - sqrt(m) < -0x1.fffcp-63 */

  /* s < sqrt(m) < s + 0x1.09p-52,
     compute nearest rounded result:
     the nearest result to 52 bits is either s or s+0x1p-52,
     we can decide by comparing (2^52 s + 0.5)^2 to 2^104 m.  */
  uint64_t d0, d1, d2;
  double y, t;
  d0 = (m << 42) - s * s;
  d1 = s - d0;
  d2 = d1 + s + 1;
  s += d1 >> 63;
  s &= 0x000fffffffffffff;
  s |= top << 52;
  y = *(double *)&s;
  return y;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
double fabs(double x) {
  union {
    double f;
    uint64_t i;
  } u = {x};
  u.i &= 0x7fffffffffffffffULL;
  return u.f;
}
#endif

#ifdef MOONBIT_NATIVE_NO_SYS_HEADER
float fabsf(float x) {
  union {
    float f;
    uint32_t i;
  } u = {x};
  u.i &= 0x7fffffff;
  return u.f;
}
#endif

#ifdef _MSC_VER
/* MSVC treats syntactic division by zero as fatal error,
   even for float point numbers,
   so we have to use a constant variable to work around this */
static const int MOONBIT_ZERO = 0;
#else
#define MOONBIT_ZERO 0
#endif

#ifdef __cplusplus
}
#endif
struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16$2d$cap;

struct $$3c$String$2a$Int$3e$;

struct $$moonbitlang$core$builtin$Array$3c$String$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$3c$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $Ref$3c$Int$3e$;

struct $StringView;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$double$internal$ryu$Umul128;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17$2d$cap;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19$2d$cap;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap;

struct $$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__TestCase;

struct $$3c$$3e$$3d$$3e$Unit;

struct $$3c$Int$2a$Int$3e$;

struct $$3c$String$3e$$3d$$3e$Int;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $$3c$String$2a$Int$2a$String$3e$;

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap;

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$3c$StringView$2a$StringView$3e$;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $Moonbit_Test_Driver_Internal__F$F2;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18$2d$cap;

struct $$moonbitlang$core$builtin$Hasher;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15$2d$cap;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12$2d$cap;

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair;

struct $Option$3c$StringView$3e$$Some;

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Ok;

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10$2d$cap;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$;

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result;

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Err {
  void* $0;
  
};

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $5;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
};

struct $$3c$String$2a$Int$3e$ {
  int32_t $1;
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$String$3e$ {
  int32_t $1;
  moonbit_string_t* $0;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  
};

struct $$3c$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  
};

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  
};

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap {
  int32_t(* code)(
    struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*,
    struct $$3c$String$3e$$3d$$3e$Int*
  );
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $0;
  
};

struct $Ref$3c$Int$3e$ {
  int32_t $0;
  
};

struct $StringView {
  int32_t $1;
  int32_t $2;
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $5;
  
};

struct $$moonbitlang$core$double$internal$ryu$Umul128 {
  uint64_t $0;
  uint64_t $1;
  
};

struct $Moonbit_Test_Driver_Internal_Meta {
  int32_t $1;
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $2;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ {
  int32_t $1;
  int32_t $2;
  moonbit_string_t* $0;
  
};

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  
};

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $Result$3c$Unit$2a$Error$3e$$Ok {
  int32_t $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  void* $1;
  
};

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    int32_t,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  
};

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $1;
  
};

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $5;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap {
  moonbit_string_t(* code)(struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*);
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t* $0_0;
  struct $Ref$3c$Int$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$ {
  int32_t $1;
  struct $$3c$String$2a$Int$3e$** $0;
  
};

struct $Moonbit_Test_Driver_Internal__TestCase {
  void* $0;
  struct $Moonbit_Test_Driver_Internal_Meta* $1;
  
};

struct $$3c$$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  
};

struct $$3c$Int$2a$Int$3e$ {
  int32_t $0;
  int32_t $1;
  
};

struct $$3c$String$3e$$3d$$3e$Int {
  int32_t(* code)(struct $$3c$String$3e$$3d$$3e$Int*, moonbit_string_t);
  
};

struct $Error$moonbitlang$core$builtin$Failure$Failure {
  moonbit_string_t $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
};

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
};

struct $$3c$String$2a$Int$2a$String$3e$ {
  int32_t $1;
  moonbit_string_t $0;
  moonbit_string_t $2;
  
};

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap {
  int32_t(* code)(struct $$3c$String$3e$$3d$$3e$Int*, moonbit_string_t);
  struct $$3c$String$3e$$3d$$3e$Int* $0;
  
};

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
  moonbit_string_t $0;
  
};

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  
};

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64 {
  uint64_t $0;
  int32_t $1;
  
};

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$ {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0_0;
  void* $0_1;
  moonbit_string_t $1;
  
};

struct $$3c$StringView$2a$StringView$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  int32_t $1_1;
  int32_t $1_2;
  moonbit_string_t $0_0;
  moonbit_string_t $1_0;
  
};

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err {
  void* $0;
  
};

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$3c$Error$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  
};

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$ {
  moonbit_string_t(* code)(struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*);
  
};

struct $Error$moonbitlang$core$builtin$InspectError$InspectError {
  moonbit_string_t $0;
  
};

struct $Moonbit_Test_Driver_Internal__F$F2 {
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  moonbit_string_t $1;
  
};

struct $$moonbitlang$core$builtin$Hasher {
  uint32_t $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$ {
  int32_t $1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $0;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError {
  moonbit_string_t $0;
  
};

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int {
  int32_t(* code)(
    struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*,
    struct $$3c$String$3e$$3d$$3e$Int*
  );
  
};

struct $Moonbit_Test_Driver_Internal__F$F0 {
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207 {
  void* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
};

struct $Result$3c$Unit$2a$Error$3e$$Err {
  void* $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$ {
  int32_t $1;
  int32_t* $0;
  
};

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $Moonbit_Test_Driver_Internal__F$F1 {
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  
};

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $$moonbitlang$core$builtin$SourceLocRepr {
  int32_t $0_1;
  int32_t $0_2;
  int32_t $1_1;
  int32_t $1_2;
  int32_t $2_1;
  int32_t $2_2;
  int32_t $3_1;
  int32_t $3_2;
  int32_t $4_1;
  int32_t $4_2;
  int32_t $5_1;
  int32_t $5_2;
  moonbit_string_t $0_0;
  moonbit_string_t $1_0;
  moonbit_string_t $2_0;
  moonbit_string_t $3_0;
  moonbit_string_t $4_0;
  moonbit_string_t $5_0;
  
};

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair {
  uint64_t $0;
  uint64_t $1;
  
};

struct $Option$3c$StringView$3e$$Some {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Ok {
  int32_t $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
};

struct $$moonbitlang$core$builtin$Logger$static_method_table {
  int32_t(* $method_0)(void*, moonbit_string_t);
  int32_t(* $method_1)(void*, moonbit_string_t, int32_t, int32_t);
  int32_t(* $method_2)(void*, struct $StringView);
  int32_t(* $method_3)(void*, int32_t);
  
};

struct $$moonbitlang$core$builtin$StringBuilder {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$ {
  int32_t $1;
  int32_t* $0;
  
};

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result {
  uint64_t $0;
  uint64_t $1;
  uint64_t $2;
  
};

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ {
  struct moonbit_result_0(* code)(
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*,
    int32_t
  );
  
};

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  
};

struct moonbit_result_0 {
  int tag;
  union { int32_t ok; void* err;  } data;
  
};

struct moonbit_result_1 {
  int tag;
  union { struct $StringView ok; void* err;  } data;
  
};

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3458
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3457
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3456
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3455
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3454
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3453
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3452
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3451
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1496
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$23(
  int32_t _env$3425,
  moonbit_string_t s$1474,
  int32_t sep$1475
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$22(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1461
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$21(
  int32_t _env$3334,
  moonbit_bytes_t bytes$1462
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$20(
  int32_t _env$3327,
  moonbit_string_t s$1456
);

#define $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1419,
  moonbit_string_t filename$1380,
  int32_t index$1381
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3320
);

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3316
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3314
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$3298,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1420,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1421
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$3309,
  int32_t _cont_param$1440
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3306,
  void* _cont_param$1441
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$14(
  int32_t _env$3300,
  void* _state$1423
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3295,
  void* err$1403
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$String$3e$$3d$$3e$Int* _env$3281,
  moonbit_string_t attr$1396
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$3265,
  moonbit_string_t test_name$1383,
  moonbit_string_t file_name$1384,
  moonbit_string_t message$1385,
  int32_t skipped$1386
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1378
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1376,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1377,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1374
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1337,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1350,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1363,
  moonbit_string_t file_filter$1334,
  int32_t index_filter$1335
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7(
  
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$describe_number$fn$9(
  int32_t _env$3236,
  int32_t n$1325
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6(
  
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$increment$fn$8(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$3214,
  int32_t n$1319
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$apply_twice$fn$7(
  int32_t _env$3212,
  struct $$3c$Unit$3e$$3d$$3e$Unit* f$1316,
  int32_t x$1317
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$multiply$fn$6(
  int32_t _env$3195,
  int32_t a$1313,
  int32_t b$1314
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$add$fn$5(
  int32_t _env$3194,
  int32_t a$1310,
  int32_t b$1311
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4(
  
);

int64_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$divide$fn$4(
  int32_t _env$3148,
  int32_t a$1298,
  int32_t b$1299
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0(
  
);

int32_t $$moonbitlang$core$builtin$Show$$Double$$output(
  double self$1273,
  struct $$moonbitlang$core$builtin$Logger logger$1272
);

moonbit_string_t $Double$$to_string(double self$1271);

moonbit_string_t $moonbitlang$core$double$internal$ryu$ryu_to_string(
  double val$1258
);

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d_small_int(
  uint64_t ieeeMantissa$1252,
  int32_t ieeeExponent$1254
);

moonbit_string_t $moonbitlang$core$double$internal$ryu$to_chars(
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* v$1225,
  int32_t sign$1223
);

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d(
  uint64_t ieeeMantissa$1167,
  uint32_t ieeeExponent$1166
);

int32_t $moonbitlang$core$double$internal$ryu$decimal_length17(
  uint64_t v$1163
);

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computeInvPow5(
  int32_t i$1146
);

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computePow5(
  int32_t i$1128
);

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result $moonbitlang$core$double$internal$ryu$mulShiftAll64(
  uint64_t m$1101,
  struct $$moonbitlang$core$double$internal$ryu$Pow5Pair mul$1098,
  int32_t j$1114,
  int32_t mmShift$1116
);

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf2(
  uint64_t value$1095,
  int32_t p$1096
);

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
  uint64_t value$1093,
  int32_t p$1094
);

int32_t $moonbitlang$core$double$internal$ryu$pow5Factor(uint64_t value$1089);

uint64_t $moonbitlang$core$double$internal$ryu$shiftright128(
  uint64_t lo$1088,
  uint64_t hi$1086,
  int32_t dist$1087
);

struct $$moonbitlang$core$double$internal$ryu$Umul128 $moonbitlang$core$double$internal$ryu$umul128(
  uint64_t a$1076,
  uint64_t b$1079
);

moonbit_string_t $moonbitlang$core$double$internal$ryu$string_from_bytes(
  moonbit_bytes_t bytes$1070,
  int32_t from$1074,
  int32_t to$1072
);

int32_t $moonbitlang$core$double$internal$ryu$log10Pow2(int32_t e$1068);

int32_t $moonbitlang$core$double$internal$ryu$log10Pow5(int32_t e$1067);

moonbit_string_t $moonbitlang$core$double$internal$ryu$copy_special_str(
  int32_t sign$1065,
  int32_t exponent$1066,
  int32_t mantissa$1063
);

int32_t $moonbitlang$core$double$internal$ryu$pow5bits(int32_t e$1062);

int32_t $$moonbitlang$core$builtin$Array$$contains$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1057,
  moonbit_string_t value$1060
);

int32_t $$moonbitlang$core$builtin$Array$$contains$0(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$1051,
  int32_t value$1054
);

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1049
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$1047,
  struct $$moonbitlang$core$builtin$Logger logger$1048
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$1033,
  struct $$moonbitlang$core$builtin$Logger logger$1046
);

uint64_t $Bool$$to_uint64(int32_t self$1031);

int64_t $Bool$$to_int64(int32_t self$1030);

int32_t $Bool$$to_int(int32_t self$1029);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$1028);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$1023,
  moonbit_string_t msg$1025,
  moonbit_string_t loc$1027
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$1018,
  moonbit_string_t msg$1020,
  moonbit_string_t loc$1022
);

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$1017,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1016
);

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$1015,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1014
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$1012,
  moonbit_string_t value$1010
);

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$1006,
  struct $$3c$String$3e$$3d$$3e$Int* f$1008
);

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2593,
  moonbit_string_t k$1007
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1004,
  int32_t idx$1005
);

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$1002,
  int32_t idx$1003
);

uint64_t $UInt$$to_uint64(uint32_t self$1001);

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$997,
  int32_t key$993
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$988,
  moonbit_string_t key$984
);

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$979,
  int32_t key$975
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$970,
  moonbit_string_t key$966
);

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$961,
  int32_t key$957
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$952,
  moonbit_string_t key$948
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$940
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$932
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$924
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$916
);

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$912,
  moonbit_string_t key$913,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$914
);

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$909,
  moonbit_string_t key$910,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$911
);

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$906,
  int32_t key$907,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$908
);

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$903,
  moonbit_string_t key$904,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$905
);

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$893
);

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$882
);

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$871
);

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$860
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$843,
  moonbit_string_t key$852,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$853,
  int32_t hash$851
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$827,
  moonbit_string_t key$836,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$837,
  int32_t hash$835
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$811,
  int32_t key$820,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$821,
  int32_t hash$819
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$795,
  moonbit_string_t key$804,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$805,
  int32_t hash$803
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$789,
  int32_t idx$794,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$793
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$779,
  int32_t idx$784,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$783
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$769,
  int32_t idx$774,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$773
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$759,
  int32_t idx$764,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$763
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$749,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$751,
  int32_t new_idx$750
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$743,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$745,
  int32_t new_idx$744
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$737,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$739,
  int32_t new_idx$738
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$731,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$733,
  int32_t new_idx$732
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$728,
  int32_t idx$730,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$729
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$724,
  int32_t idx$726,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$725
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$720,
  int32_t idx$722,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$721
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$716,
  int32_t idx$718,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$717
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$710
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$704
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$698
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$692
);

int32_t $Int$$next_power_of_two(int32_t self$690);

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$689);

int32_t $Option$$is_none$1(int32_t self$688);

int32_t $Option$$is_none$0(int64_t self$687);

int32_t $Option$$is_some$1(int32_t self$686);

int32_t $Option$$is_some$0(int64_t self$685);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$683
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$681
);

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$679
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$677
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$2(
  moonbit_string_t self$671,
  moonbit_string_t other$672
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$1(
  int32_t self$665,
  int32_t other$666
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  int64_t self$659,
  int64_t other$660
);

uint32_t $ReadOnlyArray$$at$1(uint32_t* self$657, int32_t index$658);

uint64_t $ReadOnlyArray$$at$0(uint64_t* self$655, int32_t index$656);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$654
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$653
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$651
);

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$2242
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$2(
  moonbit_string_t self$646,
  struct $$moonbitlang$core$builtin$Logger logger$647
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$1(
  int32_t self$642,
  struct $$moonbitlang$core$builtin$Logger logger$643
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  int64_t self$638,
  struct $$moonbitlang$core$builtin$Logger logger$639
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$637
);

int32_t $$moonbitlang$core$builtin$Show$$UInt64$$output(
  uint64_t self$636,
  struct $$moonbitlang$core$builtin$Logger logger$635
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$634,
  struct $$moonbitlang$core$builtin$Logger logger$633
);

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$631,
  struct $$moonbitlang$core$builtin$Logger logger$632
);

int32_t $$moonbitlang$core$builtin$Show$$Unit$$output(
  int32_t _self$630,
  struct $$moonbitlang$core$builtin$Logger logger$629
);

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$627,
  struct $$3c$String$3e$$3d$$3e$Int* f$628
);

int32_t $String$$contains(
  moonbit_string_t self$624,
  struct $StringView str$625
);

int32_t $StringView$$contains(
  struct $StringView self$622,
  struct $StringView str$623
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$618,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$620
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$615,
  struct $$3c$String$2a$Int$3e$* value$617
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$612,
  moonbit_string_t value$614
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$610
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$607
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$604
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$600,
  int32_t new_capacity$598
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$594,
  int32_t new_capacity$592
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$588,
  int32_t new_capacity$586
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$584
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$582,
  struct $StringView str$583
);

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$579,
  int32_t i$580,
  int32_t start_offset$581,
  int64_t end_offset$577
);

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$574,
  int32_t n$572,
  int32_t start_offset$568,
  int32_t end_offset$569
);

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$566,
  int32_t n$564,
  int32_t start_offset$563,
  int32_t end_offset$562
);

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$552,
  int32_t len$555,
  int32_t start_offset$559,
  int64_t end_offset$550
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$548
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$547
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$546
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$545
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$544
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$539
);

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$2164,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$537
);

moonbit_string_t $$moonbitlang$core$builtin$Iterator$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$536
);

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$527,
  struct $$moonbitlang$core$builtin$Logger logger$525
);

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$521,
  int32_t seg$524,
  int32_t i$523
);

moonbit_string_t $Byte$$to_hex(int32_t b$519);

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$517);

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$515,
  int32_t that$516
);

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$513,
  int32_t that$514
);

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$511,
  int32_t that$512
);

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$509,
  int32_t that$510
);

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$506,
  int32_t start$504,
  int32_t end$505
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$503
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$6(
  int32_t a$497,
  int32_t b$498,
  moonbit_string_t msg$500,
  moonbit_string_t loc$502
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$5(
  moonbit_string_t a$491,
  moonbit_string_t b$492,
  moonbit_string_t msg$494,
  moonbit_string_t loc$496
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  double a$485,
  double b$486,
  moonbit_string_t msg$488,
  moonbit_string_t loc$490
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  int32_t a$479,
  int32_t b$480,
  moonbit_string_t msg$482,
  moonbit_string_t loc$484
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  int64_t a$473,
  int64_t b$474,
  moonbit_string_t msg$476,
  moonbit_string_t loc$478
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  moonbit_string_t a$467,
  moonbit_string_t b$468,
  moonbit_string_t msg$470,
  moonbit_string_t loc$472
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  int32_t a$461,
  int32_t b$462,
  moonbit_string_t msg$464,
  moonbit_string_t loc$466
);

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$460,
  moonbit_string_t loc$459
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$6(int32_t t$458);

moonbit_string_t $moonbitlang$core$builtin$debug_string$5(
  moonbit_string_t t$456
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(double t$454);

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(int32_t t$452);

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(int64_t t$450);

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(
  moonbit_string_t t$448
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(int32_t t$446);

moonbit_string_t $UInt64$$to_string$inner(
  uint64_t self$437,
  int32_t radix$436
);

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$426,
  uint64_t num$414,
  int32_t digit_start$417,
  int32_t total_len$416
);

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$408,
  uint64_t num$402,
  int32_t digit_start$400,
  int32_t total_len$399,
  int32_t radix$404
);

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$395,
  uint64_t num$391,
  int32_t digit_start$389,
  int32_t total_len$388
);

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$381,
  int32_t radix$384
);

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$379);

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$378);

moonbit_string_t $Int$$to_string$inner(int32_t self$362, int32_t radix$361);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$355,
  int32_t radix$358
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$353);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$352);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$342,
  uint32_t num$330,
  int32_t digit_start$333,
  int32_t total_len$332
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$324,
  uint32_t num$318,
  int32_t digit_start$316,
  int32_t total_len$315,
  int32_t radix$320
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$311,
  uint32_t num$307,
  int32_t digit_start$305,
  int32_t total_len$304
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
  void* self$302
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$300
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  uint64_t self$298
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$296
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$294
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$292
);

int32_t $StringView$$start_offset(struct $StringView self$290);

moonbit_string_t $StringView$$data(struct $StringView self$289);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$283,
  moonbit_string_t value$286,
  int32_t start$287,
  int32_t len$288
);

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$276,
  int32_t start$282,
  int64_t end$278
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$274
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$272
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$269
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$267
);

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$266
);

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$265
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$263,
  int32_t value$262
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$261,
  moonbit_string_t value$260
);

uint64_t $Int$$to_uint64(int32_t self$259);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$257,
  int32_t value$258
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$255,
  uint32_t value$256
);

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$253,
  uint32_t input$254
);

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$251, int32_t r$252);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$249,
  moonbit_string_t str$250
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$241,
  int32_t bytes_offset$236,
  moonbit_string_t str$243,
  int32_t str_offset$239,
  int32_t length$237
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$203
);

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$199
);

struct $StringView $StringView$$view$inner(
  struct $StringView self$196,
  int32_t start_offset$197,
  int64_t end_offset$194
);

int64_t $StringView$$rev_find(
  struct $StringView self$192,
  struct $StringView str$191
);

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$182,
  struct $StringView needle$184
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$171,
  struct $StringView needle$173
);

int64_t $StringView$$find(
  struct $StringView self$169,
  struct $StringView str$168
);

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$158,
  struct $StringView needle$160
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$144,
  struct $StringView needle$146
);

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$140,
  int32_t index$141
);

int32_t $StringView$$length(struct $StringView self$139);

int32_t $$moonbitlang$core$builtin$Array$$at$1(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$137,
  int32_t index$138
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$134,
  int32_t index$135
);

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$132
);

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$* self$131
);

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$130
);

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$129
);

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$128
);

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$127
);

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$126
);

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$125
);

moonbit_string_t $String$$escape(moonbit_string_t self$124);

int32_t $moonbitlang$core$builtin$op_notequal$5(int32_t x$121, int32_t y$122);

int32_t $moonbitlang$core$builtin$op_notequal$4(
  moonbit_string_t x$119,
  moonbit_string_t y$120
);

int32_t $moonbitlang$core$builtin$op_notequal$3(int32_t x$117, int32_t y$118);

int32_t $moonbitlang$core$builtin$op_notequal$2(int64_t x$115, int64_t y$116);

int32_t $moonbitlang$core$builtin$op_notequal$1(
  moonbit_string_t x$113,
  moonbit_string_t y$114
);

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$111, int32_t y$112);

int32_t $Int$$is_trailing_surrogate(int32_t self$110);

int32_t $Int$$is_leading_surrogate(int32_t self$109);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$106,
  int32_t ch$108
);

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$101,
  int32_t required$102
);

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default();

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$95,
  int32_t offset$96,
  int32_t value$94
);

int32_t $UInt$$to_byte(uint32_t self$92);

uint32_t $Char$$to_uint(int32_t self$91);

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$90
);

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$85,
  int32_t offset$89,
  int64_t length$87
);

#define $moonbitlang$core$builtin$unsafe_sub_string moonbit_unsafe_bytes_sub_string

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$82
);

int32_t $Byte$$to_char(int32_t self$80);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$75,
  int32_t dst_offset$76,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$77,
  int32_t src_offset$78,
  int32_t len$79
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$70,
  int32_t dst_offset$71,
  struct $$3c$String$2a$Int$3e$** src$72,
  int32_t src_offset$73,
  int32_t len$74
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$65,
  int32_t dst_offset$66,
  moonbit_string_t* src$67,
  int32_t src_offset$68,
  int32_t len$69
);

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$56,
  int32_t dst_offset$58,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$57,
  int32_t src_offset$59,
  int32_t len$61
);

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$47,
  int32_t dst_offset$49,
  struct $$3c$String$2a$Int$3e$** src$48,
  int32_t src_offset$50,
  int32_t len$52
);

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$38,
  int32_t dst_offset$40,
  moonbit_string_t* src$39,
  int32_t src_offset$41,
  int32_t len$43
);

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$29,
  int32_t dst_offset$31,
  moonbit_bytes_t src$30,
  int32_t src_offset$32,
  int32_t len$34
);

int64_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$27,
  moonbit_string_t loc$28
);

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$25,
  moonbit_string_t loc$26
);

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$23,
  moonbit_string_t loc$24
);

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$21,
  moonbit_string_t loc$22
);

int32_t $$moonbitlang$core$builtin$Eq$$Unit$$equal(
  int32_t _discard_$19,
  int32_t _discard_$20
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$15,
  struct $$moonbitlang$core$builtin$Logger _x_5272$18
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
  void* _x_5285$13,
  struct $$moonbitlang$core$builtin$Logger _x_5286$14
);

int32_t $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
  int32_t _x_5289$11,
  int32_t _x_5290$12
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$2(
  struct $$moonbitlang$core$builtin$Logger self$10,
  moonbit_string_t obj$9
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$1(
  struct $$moonbitlang$core$builtin$Logger self$8,
  int32_t obj$7
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$0(
  struct $$moonbitlang$core$builtin$Logger self$6,
  int32_t obj$5
);

int64_t $moonbitlang$core$abort$abort$3(moonbit_string_t msg$4);

struct $StringView $moonbitlang$core$abort$abort$2(moonbit_string_t msg$3);

int32_t $moonbitlang$core$abort$abort$1(moonbit_string_t msg$2);

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1);

moonbit_string_t $Error$to_string(void* _e$1503);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1521,
  int32_t _param$1520
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1518,
  struct $StringView _param$1517
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1515,
  moonbit_string_t _param$1512,
  int32_t _param$1513,
  int32_t _param$1514
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1510,
  moonbit_string_t _param$1509
);

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_86 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 50, 58, 51, 45, 50, 
    50, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_49 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    104, 101, 108, 108, 111, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_25 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    122, 101, 114, 111, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[1]; 
} const moonbit_string_literal_3 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 0), 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_98 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 58, 51, 45, 57, 58, 
    51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_60 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 97, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_52 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 51, 58, 51, 45, 57, 
    51, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_53 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 52, 58, 51, 45, 57, 
    52, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_6 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[19]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 18), 
    121, 32, 103, 114, 101, 97, 116, 101, 114, 32, 111, 114, 32, 101, 
    113, 117, 97, 108, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_140 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_69 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 57, 58, 51, 45, 55, 
    57, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_120 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_105 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    73, 110, 102, 105, 110, 105, 116, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_104 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    78, 97, 78, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_94 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 51, 58, 51, 45, 51, 
    51, 58, 52, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 55, 49, 58, 51, 45, 
    49, 55, 49, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_110 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    78, 111, 110, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 51, 51, 58, 51, 45, 
    49, 51, 51, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_101 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_78 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 51, 58, 51, 45, 53, 
    51, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_100 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 49, 58, 51, 45, 49, 
    49, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_84 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    55357, 56960, 32, 55356, 57263, 32, 55357, 56522, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_50 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 49, 58, 51, 45, 57, 
    49, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_73 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 54, 58, 51, 45, 52, 
    54, 58, 52, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_63 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 57, 58, 51, 45, 54, 
    57, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[54]; 
} const moonbit_string_literal_132 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 53), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 118, 105, 101, 119, 46, 109, 98, 116, 58, 49, 
    49, 49, 58, 53, 45, 49, 49, 49, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_9 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    44, 32, 34, 105, 110, 100, 101, 120, 34, 58, 32, 34, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_130 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_112 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 41, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_55 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 56, 58, 51, 45, 57, 
    56, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_45 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    110, 111, 116, 32, 101, 113, 117, 97, 108, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_89 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 32, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_77 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 50, 58, 51, 45, 53, 
    50, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_111 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    83, 111, 109, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_82 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_71 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 56, 49, 58, 51, 45, 56, 
    49, 58, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_0 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 58, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_128 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 54, 54, 51, 
    58, 53, 45, 54, 54, 51, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_117 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 51, 54, 54, 58, 53, 45, 
    51, 54, 54, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_2 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 47, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[26]; 
} const moonbit_string_literal_102 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 25), 
    73, 108, 108, 101, 103, 97, 108, 65, 114, 103, 117, 109, 101, 110, 
    116, 69, 120, 99, 101, 112, 116, 105, 111, 110, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_22 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 48, 50, 58, 51, 45, 
    50, 48, 50, 58, 52, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_48 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 52, 54, 58, 51, 45, 
    49, 52, 54, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_65 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 49, 58, 51, 45, 55, 
    49, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_35 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 56, 58, 51, 45, 
    49, 50, 56, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_109 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 102, 97, 108, 115, 101, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_123 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_121 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_108 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_36 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 57, 58, 51, 45, 
    49, 50, 57, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_15 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    69, 110, 103, 105, 110, 101, 101, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_1 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 45, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_61 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 98, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 56, 48, 58, 51, 45, 
    49, 56, 48, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_83 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    20013, 25991, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_96 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 58, 51, 45, 55, 58, 
    50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_58 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 49, 52, 58, 51, 45, 
    49, 49, 52, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_87 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 51, 58, 51, 45, 50, 
    51, 58, 52, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_70 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 56, 48, 58, 51, 45, 56, 
    48, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_34 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 54, 58, 51, 45, 
    49, 50, 54, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_75 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 56, 58, 51, 45, 52, 
    56, 58, 55, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_28 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 54, 48, 58, 51, 45, 
    49, 54, 48, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    112, 111, 115, 105, 116, 105, 118, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_116 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    73, 110, 118, 97, 108, 105, 100, 32, 115, 116, 97, 114, 116, 32, 
    105, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_27 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 53, 57, 58, 51, 45, 
    49, 53, 57, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[59]; 
} const moonbit_string_literal_103 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 58), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 100, 111, 117, 98, 108, 101, 47, 105, 110, 116, 
    101, 114, 110, 97, 108, 47, 114, 121, 117, 58, 114, 121, 117, 46, 
    109, 98, 116, 58, 49, 49, 54, 58, 51, 45, 49, 49, 54, 58, 52, 53, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_42 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    120, 32, 115, 109, 97, 108, 108, 101, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_148 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    24067, 23572, 36923, 36753, 21644, 26465, 20214, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_125 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    32, 33, 61, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_95 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 58, 51, 45, 54, 58, 
    50, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 54, 49, 58, 51, 45, 
    49, 54, 49, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_4 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_119 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 52, 57, 49, 58, 57, 45, 
    52, 57, 49, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_57 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 48, 48, 58, 51, 45, 
    49, 48, 48, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_66 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 52, 58, 51, 45, 55, 
    52, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_118 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 48, 52, 58, 51, 45, 
    50, 48, 52, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_150 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11),
    25968, 25454, 32467, 26500, 21644, 27169, 24335, 21305, 37197, 27979,
    35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_135 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_127 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 48, 51, 58, 51, 45, 
    50, 48, 51, 58, 52, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[24]; 
} const moonbit_string_literal_151 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 23), 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_126 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    32, 70, 65, 73, 76, 69, 68, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    110, 101, 103, 97, 116, 105, 118, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_146 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    25968, 32452, 25805, 20316, 21644, 36793, 30028, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[19]; 
} const moonbit_string_literal_43 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 18), 
    121, 32, 115, 109, 97, 108, 108, 101, 114, 32, 111, 114, 32, 101, 
    113, 117, 97, 108, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_114 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    102, 97, 108, 115, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_68 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 54, 58, 51, 45, 55, 
    54, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_79 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 52, 58, 51, 45, 53, 
    52, 58, 52, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_44 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    101, 113, 117, 97, 108, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_144 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10),
    23383, 31526, 20018, 25805, 20316, 21644, 36793, 30028, 27979, 35797, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_138 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    73, 110, 100, 101, 120, 79, 117, 116, 79, 102, 66, 111, 117, 110, 
    100, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[85]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 84), 
    123, 34, 112, 97, 99, 107, 97, 103, 101, 34, 58, 32, 34, 97, 122, 
    105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 
    121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 109, 117, 116, 
    104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 47, 98, 97, 
    115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 34, 44, 32, 
    34, 102, 105, 108, 101, 110, 97, 109, 101, 34, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_5 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 32, 98, 
    108, 111, 99, 107, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_32 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 52, 58, 51, 45, 
    49, 50, 52, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_76 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 49, 58, 51, 45, 53, 
    49, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_54 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 55, 58, 51, 45, 57, 
    55, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_149 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    20989, 25968, 21644, 20316, 29992, 22495, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_129 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_115 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    40, 41, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_91 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    20013, 25991, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_142 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 73, 110, 115, 
    112, 101, 99, 116, 69, 114, 114, 111, 114, 46, 73, 110, 115, 112, 
    101, 99, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_136 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 10, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_67 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 53, 58, 51, 45, 55, 
    53, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_64 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 48, 58, 51, 45, 55, 
    48, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    120, 32, 103, 114, 101, 97, 116, 101, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_56 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 57, 58, 51, 45, 57, 
    57, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_13 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    45, 45, 45, 45, 45, 32, 69, 78, 68, 32, 77, 79, 79, 78, 32, 84, 69, 
    83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[27]; 
} const moonbit_string_literal_93 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 26),
    36825, 26159, 19968, 20010, 38750, 24120, 38271, 30340, 23383, 31526,
    20018, 65292, 29992, 20110, 27979, 35797, 36793, 30028, 24773, 20917,
    21644, 22788, 29702, 33021, 21147, 12290, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    65, 108, 105, 99, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_137 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    70, 97, 105, 108, 117, 114, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_72 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 53, 58, 51, 45, 52, 
    53, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_10 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    34, 44, 32, 34, 116, 101, 115, 116, 95, 110, 97, 109, 101, 34, 58, 
    32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_62 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 99, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_12 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 125, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_11 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    44, 32, 34, 109, 101, 115, 115, 97, 103, 101, 34, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_59 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 49, 53, 58, 51, 45, 
    49, 49, 53, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 55, 57, 58, 51, 45, 
    49, 55, 57, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    45, 45, 45, 45, 45, 32, 66, 69, 71, 73, 78, 32, 77, 79, 79, 78, 32, 
    84, 69, 83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_145 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    25968, 20540, 25805, 20316, 21644, 36793, 30028, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_143 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    22522, 26412, 26029, 35328, 21644, 36923, 36753, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_97 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 56, 58, 51, 45, 56, 58, 
    49, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_20 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 56, 57, 58, 51, 45, 
    49, 56, 57, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 56, 49, 58, 51, 45, 
    49, 56, 49, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_147 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    79, 112, 116, 105, 111, 110, 31867, 22411, 21644, 38169, 35823, 22788,
    29702, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_134 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 48, 58, 53, 45, 50, 
    57, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_131 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    73, 110, 118, 97, 108, 105, 100, 32, 105, 110, 100, 101, 120, 32, 
    102, 111, 114, 32, 86, 105, 101, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_88 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 52, 58, 51, 45, 50, 
    52, 58, 52, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_92 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 57, 58, 51, 45, 50, 
    57, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_46 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 52, 52, 58, 51, 45, 
    49, 52, 52, 58, 52, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 51, 52, 58, 51, 45, 
    49, 51, 52, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_107 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 96, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_133 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_47 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 52, 53, 58, 51, 45, 
    49, 52, 53, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 53, 58, 51, 45, 
    49, 50, 53, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_30 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 54, 50, 58, 51, 45, 
    49, 54, 50, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_124 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    92, 117, 123, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_74 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 55, 58, 51, 45, 52, 
    55, 58, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_37 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 51, 48, 58, 51, 45, 
    49, 51, 48, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[109]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 108), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 56, 53, 58, 51, 45, 
    49, 56, 53, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[137]; 
} const moonbit_string_literal_141 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 136), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 109, 
    117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 47, 
    98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 46, 77, 
    111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 68, 114, 105, 118, 
    101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 74, 115, 69, 114, 
    114, 111, 114, 46, 77, 111, 111, 110, 66, 105, 116, 84, 101, 115, 
    116, 68, 114, 105, 118, 101, 114, 73, 110, 116, 101, 114, 110, 97, 
    108, 74, 115, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_81 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 48, 58, 51, 45, 54, 
    48, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_113 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_122 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_99 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 48, 58, 51, 45, 49, 
    48, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_90 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 56, 58, 51, 45, 50, 
    56, 58, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_85 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 49, 58, 51, 45, 50, 
    49, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_51 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 50, 58, 51, 45, 57, 
    50, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_80 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 98, 97, 115, 105, 99, 45, 101, 110, 104, 97, 110, 99, 101, 100, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    98, 97, 115, 105, 99, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 57, 58, 51, 45, 53, 
    57, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_139 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    73, 110, 118, 97, 108, 105, 100, 73, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_106 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    48, 46, 48, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$dyncall
  };

struct { int32_t rc; uint32_t meta; struct $$3c$Unit$3e$$3d$$3e$Unit data; 
} const $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$increment$fn$8$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$increment$fn$8
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3$dyncall
  };

struct { int32_t rc; uint32_t meta; struct $$3c$String$3e$$3d$$3e$Int data; 
} const $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$moonbitlang$core$builtin$Logger$static_method_table data;
  
} $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id$object =
  {
    -1,
    Moonbit_make_regular_object_header(
      sizeof(
        struct $$moonbitlang$core$builtin$Logger$static_method_table
      )
      >> 2,
        0,
        0
    ),
    {
      .$method_0 = $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger,
        .$method_1 = $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0,
        .$method_2 = $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger,
        .$method_3 = $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger
    }
  };

struct $$moonbitlang$core$builtin$Logger$static_method_table* $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id =
  &$$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id$object.data;

moonbit_string_t $moonbitlang$core$builtin$parse$$2a$bind$7c$5404 =
  (moonbit_string_t)moonbit_string_literal_0.data;

moonbit_string_t $moonbitlang$core$builtin$parse$$2a$bind$7c$5443 =
  (moonbit_string_t)moonbit_string_literal_0.data;

moonbit_string_t $moonbitlang$core$builtin$parse$$2a$bind$7c$5437 =
  (moonbit_string_t)moonbit_string_literal_1.data;

moonbit_string_t $moonbitlang$core$builtin$parse$$2a$bind$7c$5424 =
  (moonbit_string_t)moonbit_string_literal_0.data;

moonbit_string_t $moonbitlang$core$builtin$parse$$2a$bind$7c$5418 =
  (moonbit_string_t)moonbit_string_literal_0.data;

moonbit_string_t $moonbitlang$core$builtin$output$$2a$bind$7c$8193 =
  (moonbit_string_t)moonbit_string_literal_2.data;

moonbit_string_t $moonbitlang$core$builtin$output$$2a$bind$7c$8187 =
  (moonbit_string_t)moonbit_string_literal_2.data;

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$142;

int64_t $moonbitlang$core$builtin$brute_force_find$constr$156;

struct { int32_t rc; uint32_t meta; uint64_t data[30]; 
} $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2$object =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 0, 30), 
    1ull, 2305843009213693952ull, 5955668970331000884ull,
    1784059615882449851ull, 8982663654677661702ull, 1380349269358112757ull,
    7286864317269821294ull, 2135987035920910082ull, 7005857020398200553ull,
    1652639921975621497ull, 17965325103354776697ull, 1278668206209430417ull,
    8928596168509315048ull, 1978643211784836272ull, 10075671573058298858ull,
    1530901034580419511ull, 597001226353042382ull, 1184477304306571148ull,
    1527430471115325346ull, 1832889850782397517ull, 12533209867169019542ull,
    1418129833677084982ull, 5577825024675947042ull, 2194449627517475473ull,
    11006974540203867551ull, 1697873161311732311ull, 10313493231639821582ull,
    1313665730009899186ull, 12701016819766672773ull, 2032799256770390445ull
  };

uint64_t* $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2 =
  $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2$object.data;

struct { int32_t rc; uint32_t meta; uint32_t data[19]; 
} $moonbitlang$core$double$internal$ryu$gPOW5_INV_OFFSETS$object =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 0, 19),
    1414808916u, 67458373u, 268701696u, 4195348u, 1073807360u, 1091917141u,
    1108u, 65604u, 1073741824u, 1140850753u, 1346716752u, 1431634004u,
    1365595476u, 1073758208u, 16777217u, 66816u, 1364284433u, 89478484u, 
    0u
  };

uint32_t* $moonbitlang$core$double$internal$ryu$gPOW5_INV_OFFSETS =
  $moonbitlang$core$double$internal$ryu$gPOW5_INV_OFFSETS$object.data;

struct { int32_t rc; uint32_t meta; uint64_t data[26]; 
} $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2$object =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 0, 26), 
    0ull, 1152921504606846976ull, 0ull, 1490116119384765625ull,
    1032610780636961552ull, 1925929944387235853ull, 7910200175544436838ull,
    1244603055572228341ull, 16941905809032713930ull, 1608611746708759036ull,
    13024893955298202172ull, 2079081953128979843ull, 6607496772837067824ull,
    1343575221513417750ull, 17332926989895652603ull, 1736530273035216783ull,
    13037379183483547984ull, 2244412773384604712ull, 1605989338741628675ull,
    1450417759929778918ull, 9630225068416591280ull, 1874621017369538693ull,
    665883850346957067ull, 1211445438634777304ull, 14931890668723713708ull,
    1565756531257009982ull
  };

uint64_t* $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2 =
  $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2$object.data;

struct { int32_t rc; uint32_t meta; uint32_t data[21]; 
} $moonbitlang$core$double$internal$ryu$gPOW5_OFFSETS$object =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 0, 21), 
    0u, 0u, 0u, 0u, 1073741824u, 1500076437u, 1431590229u, 1448432917u,
    1091896580u, 1079333904u, 1146442053u, 1146111296u, 1163220304u,
    1073758208u, 2521039936u, 1431721317u, 1413824581u, 1075134801u,
    1431671125u, 1363170645u, 261u
  };

uint32_t* $moonbitlang$core$double$internal$ryu$gPOW5_OFFSETS =
  $moonbitlang$core$double$internal$ryu$gPOW5_OFFSETS$object.data;

struct { int32_t rc; uint32_t meta; uint64_t data[26]; 
} $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE$object =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 0, 26), 
    1ull, 5ull, 25ull, 125ull, 625ull, 3125ull, 15625ull, 78125ull,
    390625ull, 1953125ull, 9765625ull, 48828125ull, 244140625ull,
    1220703125ull, 6103515625ull, 30517578125ull, 152587890625ull,
    762939453125ull, 3814697265625ull, 19073486328125ull, 95367431640625ull,
    476837158203125ull, 2384185791015625ull, 11920928955078125ull,
    59604644775390625ull, 298023223876953125ull
  };

uint64_t* $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE =
  $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE$object.data;

struct {
  int32_t rc;
  uint32_t meta;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64 data;
  
} $moonbitlang$core$double$internal$ryu$ryu_to_string$record$1257$object =
  {
    -1,
    Moonbit_make_regular_object_header(
      sizeof(
        struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
      )
      >> 2,
        0,
        0
    ), {.$0 = 0ull, .$1 = 0}
  };

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$ryu_to_string$record$1257 =
  &$moonbitlang$core$double$internal$ryu$ryu_to_string$record$1257$object.data;

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_no_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_async_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_with_args_tests;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0$clo;

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3458
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3457
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3456
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3455
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3454
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3453
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3452
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3451
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1();
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1496
) {
  moonbit_decref(_tests$1496);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$1455 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1461 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$1468 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1461;
  int32_t moonbit_test_driver_internal_split_mbt_string$1473 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$3450 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$1480 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$1481;
  moonbit_string_t _tmp$3449;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$1482;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$1483;
  int32_t _len$1484;
  int32_t _i$1485;
  Moonbit_object_header(file_and_index$1480)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$1480->$0 = _tmp$3450;
  file_and_index$1480->$1 = 0;
  cli_args$1481
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$22(
    moonbit_test_driver_internal_get_cli_args_internal$1468
  );
  _tmp$3449 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$1481, 1);
  test_args$1482
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$23(
    moonbit_test_driver_internal_split_mbt_string$1473, _tmp$3449, 47
  );
  _arr$1483 = test_args$1482;
  moonbit_incref(_arr$1483);
  _len$1484 = $$moonbitlang$core$builtin$Array$$length$1(_arr$1483);
  _i$1485 = 0;
  while (1) {
    if (_i$1485 < _len$1484) {
      moonbit_string_t arg$1486;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$1487;
      moonbit_string_t file$1488;
      moonbit_string_t range$1489;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1490;
      moonbit_string_t _tmp$3447;
      int32_t start$1491;
      moonbit_string_t _tmp$3446;
      int32_t end$1492;
      int32_t i$1493;
      int32_t _tmp$3448;
      moonbit_incref(_arr$1483);
      arg$1486
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$1483, _i$1485
      );
      file_and_range$1487
      = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$23(
        moonbit_test_driver_internal_split_mbt_string$1473, arg$1486, 58
      );
      moonbit_incref(file_and_range$1487);
      file$1488
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1487, 0
      );
      range$1489
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1487, 1
      );
      start_and_end$1490
      = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$23(
        moonbit_test_driver_internal_split_mbt_string$1473, range$1489, 45
      );
      moonbit_incref(start_and_end$1490);
      _tmp$3447
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1490, 0
      );
      start$1491
      = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$20(
        moonbit_test_driver_internal_parse_int_$1455, _tmp$3447
      );
      _tmp$3446
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1490, 1
      );
      end$1492
      = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$20(
        moonbit_test_driver_internal_parse_int_$1455, _tmp$3446
      );
      i$1493 = start$1491;
      while (1) {
        if (i$1493 < end$1492) {
          struct $$3c$String$2a$Int$3e$* _tuple$3444;
          int32_t _tmp$3445;
          moonbit_incref(file$1488);
          _tuple$3444
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$3444)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$3444->$0 = file$1488;
          _tuple$3444->$1 = i$1493;
          moonbit_incref(file_and_index$1480);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$1480, _tuple$3444
          );
          _tmp$3445 = i$1493 + 1;
          i$1493 = _tmp$3445;
          continue;
        } else {
          moonbit_decref(file$1488);
        }
        break;
      }
      _tmp$3448 = _i$1485 + 1;
      _i$1485 = _tmp$3448;
      continue;
    } else {
      moonbit_decref(_arr$1483);
    }
    break;
  }
  return file_and_index$1480;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$23(
  int32_t _env$3425,
  moonbit_string_t s$1474,
  int32_t sep$1475
) {
  moonbit_string_t* _tmp$3443 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1476 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$1477;
  struct $Ref$3c$Int$3e$* start$1478;
  int32_t val$3438;
  int32_t _tmp$3439;
  Moonbit_object_header(res$1476)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$1476->$0 = _tmp$3443;
  res$1476->$1 = 0;
  i$1477
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1477)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1477->$0 = 0;
  start$1478
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$1478)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$1478->$0 = 0;
  while (1) {
    int32_t val$3426 = i$1477->$0;
    int32_t _tmp$3427 = Moonbit_array_length(s$1474);
    if (val$3426 < _tmp$3427) {
      int32_t val$3430 = i$1477->$0;
      int32_t _tmp$3429;
      int32_t _tmp$3428;
      int32_t val$3437;
      int32_t _tmp$3436;
      if (val$3430 < 0 || val$3430 >= Moonbit_array_length(s$1474)) {
        moonbit_panic();
      }
      _tmp$3429 = s$1474[val$3430];
      _tmp$3428 = _tmp$3429;
      if (_tmp$3428 == sep$1475) {
        int32_t val$3432 = start$1478->$0;
        int32_t val$3433 = i$1477->$0;
        moonbit_string_t _tmp$3431;
        int32_t val$3435;
        int32_t _tmp$3434;
        moonbit_incref(s$1474);
        _tmp$3431 = $String$$unsafe_substring(s$1474, val$3432, val$3433);
        moonbit_incref(res$1476);
        $$moonbitlang$core$builtin$Array$$push$0(res$1476, _tmp$3431);
        val$3435 = i$1477->$0;
        _tmp$3434 = val$3435 + 1;
        start$1478->$0 = _tmp$3434;
      }
      val$3437 = i$1477->$0;
      _tmp$3436 = val$3437 + 1;
      i$1477->$0 = _tmp$3436;
      continue;
    } else {
      moonbit_decref(i$1477);
    }
    break;
  }
  val$3438 = start$1478->$0;
  _tmp$3439 = Moonbit_array_length(s$1474);
  if (val$3438 < _tmp$3439) {
    int32_t _field$3459 = start$1478->$0;
    int32_t val$3441;
    int32_t _tmp$3442;
    moonbit_string_t _tmp$3440;
    moonbit_decref(start$1478);
    val$3441 = _field$3459;
    _tmp$3442 = Moonbit_array_length(s$1474);
    _tmp$3440 = $String$$unsafe_substring(s$1474, val$3441, _tmp$3442);
    moonbit_incref(res$1476);
    $$moonbitlang$core$builtin$Array$$push$0(res$1476, _tmp$3440);
  } else {
    moonbit_decref(start$1478);
    moonbit_decref(s$1474);
  }
  return res$1476;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$22(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1461
) {
  moonbit_bytes_t* tmp$1469 =
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$3424 = Moonbit_array_length(tmp$1469);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1470 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$3424);
  int32_t i$1471 = 0;
  while (1) {
    int32_t _tmp$3420 = Moonbit_array_length(tmp$1469);
    if (i$1471 < _tmp$3420) {
      moonbit_bytes_t _tmp$3460;
      moonbit_bytes_t _tmp$3422;
      moonbit_string_t _tmp$3421;
      int32_t _tmp$3423;
      if (i$1471 < 0 || i$1471 >= Moonbit_array_length(tmp$1469)) {
        moonbit_panic();
      }
      _tmp$3460 = (moonbit_bytes_t)tmp$1469[i$1471];
      _tmp$3422 = _tmp$3460;
      moonbit_incref(_tmp$3422);
      _tmp$3421
      = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$21(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1461, _tmp$3422
      );
      moonbit_incref(res$1470);
      $$moonbitlang$core$builtin$Array$$push$0(res$1470, _tmp$3421);
      _tmp$3423 = i$1471 + 1;
      i$1471 = _tmp$3423;
      continue;
    } else {
      moonbit_decref(tmp$1469);
    }
    break;
  }
  return res$1470;
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$21(
  int32_t _env$3334,
  moonbit_bytes_t bytes$1462
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$1463 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$1464 = Moonbit_array_length(bytes$1462);
  struct $Ref$3c$Int$3e$* i$1465 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1465)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1465->$0 = 0;
  while (1) {
    int32_t val$3335 = i$1465->$0;
    if (val$3335 < len$1464) {
      int32_t val$3419 = i$1465->$0;
      int32_t _tmp$3418;
      int32_t _tmp$3417;
      struct $Ref$3c$Int$3e$* c$1466;
      int32_t val$3336;
      if (val$3419 < 0 || val$3419 >= Moonbit_array_length(bytes$1462)) {
        moonbit_panic();
      }
      _tmp$3418 = bytes$1462[val$3419];
      _tmp$3417 = (int32_t)_tmp$3418;
      c$1466
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$1466)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$1466->$0 = _tmp$3417;
      val$3336 = c$1466->$0;
      if (val$3336 < 128) {
        int32_t _field$3461 = c$1466->$0;
        int32_t val$3338;
        int32_t _tmp$3337;
        int32_t val$3340;
        int32_t _tmp$3339;
        moonbit_decref(c$1466);
        val$3338 = _field$3461;
        _tmp$3337 = val$3338;
        moonbit_incref(res$1463);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$1463, _tmp$3337
        );
        val$3340 = i$1465->$0;
        _tmp$3339 = val$3340 + 1;
        i$1465->$0 = _tmp$3339;
      } else {
        int32_t val$3341 = c$1466->$0;
        if (val$3341 < 224) {
          int32_t val$3343 = i$1465->$0;
          int32_t _tmp$3342 = val$3343 + 1;
          int32_t val$3352;
          int32_t _tmp$3351;
          int32_t _tmp$3345;
          int32_t val$3350;
          int32_t _tmp$3349;
          int32_t _tmp$3348;
          int32_t _tmp$3347;
          int32_t _tmp$3346;
          int32_t _tmp$3344;
          int32_t _field$3462;
          int32_t val$3354;
          int32_t _tmp$3353;
          int32_t val$3356;
          int32_t _tmp$3355;
          if (_tmp$3342 >= len$1464) {
            moonbit_decref(c$1466);
            moonbit_decref(i$1465);
            moonbit_decref(bytes$1462);
            break;
          }
          val$3352 = c$1466->$0;
          _tmp$3351 = val$3352 & 31;
          _tmp$3345 = _tmp$3351 << 6;
          val$3350 = i$1465->$0;
          _tmp$3349 = val$3350 + 1;
          if (_tmp$3349 < 0 || _tmp$3349 >= Moonbit_array_length(bytes$1462)) {
            moonbit_panic();
          }
          _tmp$3348 = bytes$1462[_tmp$3349];
          _tmp$3347 = (int32_t)_tmp$3348;
          _tmp$3346 = _tmp$3347 & 63;
          _tmp$3344 = _tmp$3345 | _tmp$3346;
          c$1466->$0 = _tmp$3344;
          _field$3462 = c$1466->$0;
          moonbit_decref(c$1466);
          val$3354 = _field$3462;
          _tmp$3353 = val$3354;
          moonbit_incref(res$1463);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$1463, _tmp$3353
          );
          val$3356 = i$1465->$0;
          _tmp$3355 = val$3356 + 2;
          i$1465->$0 = _tmp$3355;
        } else {
          int32_t val$3357 = c$1466->$0;
          if (val$3357 < 240) {
            int32_t val$3359 = i$1465->$0;
            int32_t _tmp$3358 = val$3359 + 2;
            int32_t val$3375;
            int32_t _tmp$3374;
            int32_t _tmp$3367;
            int32_t val$3373;
            int32_t _tmp$3372;
            int32_t _tmp$3371;
            int32_t _tmp$3370;
            int32_t _tmp$3369;
            int32_t _tmp$3368;
            int32_t _tmp$3361;
            int32_t val$3366;
            int32_t _tmp$3365;
            int32_t _tmp$3364;
            int32_t _tmp$3363;
            int32_t _tmp$3362;
            int32_t _tmp$3360;
            int32_t _field$3463;
            int32_t val$3377;
            int32_t _tmp$3376;
            int32_t val$3379;
            int32_t _tmp$3378;
            if (_tmp$3358 >= len$1464) {
              moonbit_decref(c$1466);
              moonbit_decref(i$1465);
              moonbit_decref(bytes$1462);
              break;
            }
            val$3375 = c$1466->$0;
            _tmp$3374 = val$3375 & 15;
            _tmp$3367 = _tmp$3374 << 12;
            val$3373 = i$1465->$0;
            _tmp$3372 = val$3373 + 1;
            if (
              _tmp$3372 < 0 || _tmp$3372 >= Moonbit_array_length(bytes$1462)
            ) {
              moonbit_panic();
            }
            _tmp$3371 = bytes$1462[_tmp$3372];
            _tmp$3370 = (int32_t)_tmp$3371;
            _tmp$3369 = _tmp$3370 & 63;
            _tmp$3368 = _tmp$3369 << 6;
            _tmp$3361 = _tmp$3367 | _tmp$3368;
            val$3366 = i$1465->$0;
            _tmp$3365 = val$3366 + 2;
            if (
              _tmp$3365 < 0 || _tmp$3365 >= Moonbit_array_length(bytes$1462)
            ) {
              moonbit_panic();
            }
            _tmp$3364 = bytes$1462[_tmp$3365];
            _tmp$3363 = (int32_t)_tmp$3364;
            _tmp$3362 = _tmp$3363 & 63;
            _tmp$3360 = _tmp$3361 | _tmp$3362;
            c$1466->$0 = _tmp$3360;
            _field$3463 = c$1466->$0;
            moonbit_decref(c$1466);
            val$3377 = _field$3463;
            _tmp$3376 = val$3377;
            moonbit_incref(res$1463);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1463, _tmp$3376
            );
            val$3379 = i$1465->$0;
            _tmp$3378 = val$3379 + 3;
            i$1465->$0 = _tmp$3378;
          } else {
            int32_t val$3381 = i$1465->$0;
            int32_t _tmp$3380 = val$3381 + 3;
            int32_t val$3404;
            int32_t _tmp$3403;
            int32_t _tmp$3396;
            int32_t val$3402;
            int32_t _tmp$3401;
            int32_t _tmp$3400;
            int32_t _tmp$3399;
            int32_t _tmp$3398;
            int32_t _tmp$3397;
            int32_t _tmp$3389;
            int32_t val$3395;
            int32_t _tmp$3394;
            int32_t _tmp$3393;
            int32_t _tmp$3392;
            int32_t _tmp$3391;
            int32_t _tmp$3390;
            int32_t _tmp$3383;
            int32_t val$3388;
            int32_t _tmp$3387;
            int32_t _tmp$3386;
            int32_t _tmp$3385;
            int32_t _tmp$3384;
            int32_t _tmp$3382;
            int32_t val$3406;
            int32_t _tmp$3405;
            int32_t val$3410;
            int32_t _tmp$3409;
            int32_t _tmp$3408;
            int32_t _tmp$3407;
            int32_t _field$3464;
            int32_t val$3414;
            int32_t _tmp$3413;
            int32_t _tmp$3412;
            int32_t _tmp$3411;
            int32_t val$3416;
            int32_t _tmp$3415;
            if (_tmp$3380 >= len$1464) {
              moonbit_decref(c$1466);
              moonbit_decref(i$1465);
              moonbit_decref(bytes$1462);
              break;
            }
            val$3404 = c$1466->$0;
            _tmp$3403 = val$3404 & 7;
            _tmp$3396 = _tmp$3403 << 18;
            val$3402 = i$1465->$0;
            _tmp$3401 = val$3402 + 1;
            if (
              _tmp$3401 < 0 || _tmp$3401 >= Moonbit_array_length(bytes$1462)
            ) {
              moonbit_panic();
            }
            _tmp$3400 = bytes$1462[_tmp$3401];
            _tmp$3399 = (int32_t)_tmp$3400;
            _tmp$3398 = _tmp$3399 & 63;
            _tmp$3397 = _tmp$3398 << 12;
            _tmp$3389 = _tmp$3396 | _tmp$3397;
            val$3395 = i$1465->$0;
            _tmp$3394 = val$3395 + 2;
            if (
              _tmp$3394 < 0 || _tmp$3394 >= Moonbit_array_length(bytes$1462)
            ) {
              moonbit_panic();
            }
            _tmp$3393 = bytes$1462[_tmp$3394];
            _tmp$3392 = (int32_t)_tmp$3393;
            _tmp$3391 = _tmp$3392 & 63;
            _tmp$3390 = _tmp$3391 << 6;
            _tmp$3383 = _tmp$3389 | _tmp$3390;
            val$3388 = i$1465->$0;
            _tmp$3387 = val$3388 + 3;
            if (
              _tmp$3387 < 0 || _tmp$3387 >= Moonbit_array_length(bytes$1462)
            ) {
              moonbit_panic();
            }
            _tmp$3386 = bytes$1462[_tmp$3387];
            _tmp$3385 = (int32_t)_tmp$3386;
            _tmp$3384 = _tmp$3385 & 63;
            _tmp$3382 = _tmp$3383 | _tmp$3384;
            c$1466->$0 = _tmp$3382;
            val$3406 = c$1466->$0;
            _tmp$3405 = val$3406 - 65536;
            c$1466->$0 = _tmp$3405;
            val$3410 = c$1466->$0;
            _tmp$3409 = val$3410 >> 10;
            _tmp$3408 = _tmp$3409 + 55296;
            _tmp$3407 = _tmp$3408;
            moonbit_incref(res$1463);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1463, _tmp$3407
            );
            _field$3464 = c$1466->$0;
            moonbit_decref(c$1466);
            val$3414 = _field$3464;
            _tmp$3413 = val$3414 & 1023;
            _tmp$3412 = _tmp$3413 + 56320;
            _tmp$3411 = _tmp$3412;
            moonbit_incref(res$1463);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1463, _tmp$3411
            );
            val$3416 = i$1465->$0;
            _tmp$3415 = val$3416 + 4;
            i$1465->$0 = _tmp$3415;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$1465);
      moonbit_decref(bytes$1462);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$1463);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$20(
  int32_t _env$3327,
  moonbit_string_t s$1456
) {
  struct $Ref$3c$Int$3e$* res$1457 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$1458;
  int32_t i$1459;
  int32_t _field$3465;
  Moonbit_object_header(res$1457)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$1457->$0 = 0;
  len$1458 = Moonbit_array_length(s$1456);
  i$1459 = 0;
  while (1) {
    if (i$1459 < len$1458) {
      int32_t val$3332 = res$1457->$0;
      int32_t _tmp$3329 = val$3332 * 10;
      int32_t _tmp$3331;
      int32_t _tmp$3330;
      int32_t _tmp$3328;
      int32_t _tmp$3333;
      if (i$1459 < 0 || i$1459 >= Moonbit_array_length(s$1456)) {
        moonbit_panic();
      }
      _tmp$3331 = s$1456[i$1459];
      _tmp$3330 = _tmp$3331 - 48;
      _tmp$3328 = _tmp$3329 + _tmp$3330;
      res$1457->$0 = _tmp$3328;
      _tmp$3333 = i$1459 + 1;
      i$1459 = _tmp$3333;
      continue;
    } else {
      moonbit_decref(s$1456);
    }
    break;
  }
  _field$3465 = res$1457->$0;
  moonbit_decref(res$1457);
  return _field$3465;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1419,
  moonbit_string_t filename$1380,
  int32_t index$1381
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$1379;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10$2d$cap* _closure$3905;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1382;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$1391;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$3475;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$3326;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3474;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$1392;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$3473;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$3325;
  moonbit_string_t _field$3472;
  moonbit_string_t file_name$1393;
  moonbit_string_t name$1394;
  int32_t _tmp$3322;
  moonbit_string_t name$1395;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$3279;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$3280;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12$2d$cap* _closure$3907;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1402;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1418;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1443;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1445;
  void* _field$3469;
  int32_t _cnt$3772;
  void* _bind$1446;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19$2d$cap* _closure$3911;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$3319;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18$2d$cap* _closure$3912;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$3312;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17$2d$cap* _closure$3913;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$3313;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$3914;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3297;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_async_tests
  );
  filtered_test$1379
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_async_tests,
      filename$1380,
      index$1381
  );
  _closure$3905
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$3905)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$3905->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10;
  _closure$3905->$0 = index$1381;
  handle_result$1382
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$3905;
  if (filtered_test$1379 == 0) {
    moonbit_decref(async_tests$1419);
    if (filtered_test$1379) {
      moonbit_decref(filtered_test$1379);
    }
    $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10(
      handle_result$1382,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$1453 =
      filtered_test$1379;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$1454 = _Some$1453;
    item$1391 = _item$1454;
    goto $join$1390;
  }
  goto $joinlet$3906;
  $join$1390:;
  _field$3475 = item$1391->$1;
  meta$3326 = _field$3475;
  _field$3474 = meta$3326->$2;
  attrs$1392 = _field$3474;
  _field$3473 = item$1391->$1;
  meta$3325 = _field$3473;
  _field$3472 = meta$3325->$0;
  file_name$1393 = _field$3472;
  moonbit_incref(attrs$1392);
  moonbit_incref(file_name$1393);
  moonbit_incref(attrs$1392);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$1392)) {
    name$1394 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$1392);
    name$1394 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1392, 0);
  }
  _tmp$3322 = Moonbit_array_length(name$1394);
  if (_tmp$3322 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$3471;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$3324;
    int32_t _field$3470;
    int32_t index$3323;
    moonbit_decref(name$1394);
    _field$3471 = item$1391->$1;
    meta$3324 = _field$3471;
    _field$3470 = meta$3324->$1;
    index$3323 = _field$3470;
    name$1395 = $Int$$to_string$inner(index$3323, 10);
  } else {
    name$1395 = name$1394;
  }
  moonbit_incref(attrs$1392);
  _tmp$3279 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$1392);
  _tmp$3280
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$3279, _tmp$3280)) {
    moonbit_string_t _tmp$3294;
    moonbit_string_t _tmp$3293;
    moonbit_string_t _tmp$3290;
    moonbit_string_t _tmp$3292;
    moonbit_string_t _tmp$3291;
    moonbit_string_t _tmp$3289;
    moonbit_decref(async_tests$1419);
    moonbit_decref(item$1391);
    moonbit_incref(file_name$1393);
    _tmp$3294
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$1393
    );
    _tmp$3293
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$3294
    );
    _tmp$3290
    = moonbit_add_string(
      _tmp$3293, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$3292 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1392, 0);
    _tmp$3291 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$3292);
    _tmp$3289 = moonbit_add_string(_tmp$3290, _tmp$3291);
    $moonbitlang$core$builtin$println$0(_tmp$3289);
    $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10(
      handle_result$1382,
        name$1395,
        file_name$1393,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$1392);
  }
  moonbit_incref(name$1395);
  moonbit_incref(file_name$1393);
  moonbit_incref(handle_result$1382);
  _closure$3907
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$3907)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$3907->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12;
  _closure$3907->$0 = name$1395;
  _closure$3907->$1 = file_name$1393;
  _closure$3907->$2 = handle_result$1382;
  on_err$1402 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$3907;
  _field$3469 = item$1391->$0;
  _cnt$3772 = Moonbit_object_header(item$1391)->rc;
  if (_cnt$3772 > 1) {
    int32_t _new_cnt$3774;
    moonbit_incref(_field$3469);
    _new_cnt$3774 = _cnt$3772 - 1;
    Moonbit_object_header(item$1391)->rc = _new_cnt$3774;
  } else if (_cnt$3772 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$3773 = item$1391->$1;
    moonbit_decref(_field$3773);
    moonbit_free(item$1391);
  }
  _bind$1446 = _field$3469;
  switch (Moonbit_object_tag(_bind$1446)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$1447;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3466;
      int32_t _cnt$3775;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1448;
      moonbit_decref(async_tests$1419);
      _F0$1447 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$1446;
      _field$3466 = _F0$1447->$0;
      _cnt$3775 = Moonbit_object_header(_F0$1447)->rc;
      if (_cnt$3775 > 1) {
        int32_t _new_cnt$3776;
        moonbit_incref(_field$3466);
        _new_cnt$3776 = _cnt$3775 - 1;
        Moonbit_object_header(_F0$1447)->rc = _new_cnt$3776;
      } else if (_cnt$3775 == 1) {
        moonbit_free(_F0$1447);
      }
      _f$1448 = _field$3466;
      f$1445 = _f$1448;
      goto $join$1444;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$1449;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3467;
      int32_t _cnt$3777;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1450;
      moonbit_decref(async_tests$1419);
      _F1$1449 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$1446;
      _field$3467 = _F1$1449->$0;
      _cnt$3777 = Moonbit_object_header(_F1$1449)->rc;
      if (_cnt$3777 > 1) {
        int32_t _new_cnt$3778;
        moonbit_incref(_field$3467);
        _new_cnt$3778 = _cnt$3777 - 1;
        Moonbit_object_header(_F1$1449)->rc = _new_cnt$3778;
      } else if (_cnt$3777 == 1) {
        moonbit_free(_F1$1449);
      }
      _f$1450 = _field$3467;
      f$1443 = _f$1450;
      goto $join$1442;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$1451 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$1446;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3468 =
        _F2$1451->$0;
      int32_t _cnt$3779 = Moonbit_object_header(_F2$1451)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$1452;
      if (_cnt$3779 > 1) {
        int32_t _new_cnt$3780;
        moonbit_incref(_field$3468);
        _new_cnt$3780 = _cnt$3779 - 1;
        Moonbit_object_header(_F2$1451)->rc = _new_cnt$3780;
      } else if (_cnt$3779 == 1) {
        moonbit_free(_F2$1451);
      }
      _f$1452 = _field$3468;
      f$1418 = _f$1452;
      goto $join$1417;
      break;
    }
  }
  goto $joinlet$3910;
  $join$1444:;
  _closure$3911
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19$2d$cap
      )
    );
  Moonbit_object_header(_closure$3911)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$3911->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19;
  _closure$3911->$0 = name$1395;
  _closure$3911->$1 = file_name$1393;
  _closure$3911->$2 = handle_result$1382;
  _tmp$3319 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$3911;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_catch_error(
    f$1445, _tmp$3319, on_err$1402
  );
  $joinlet$3910:;
  goto $joinlet$3909;
  $join$1442:;
  moonbit_incref(name$1395);
  _closure$3912
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18$2d$cap
      )
    );
  Moonbit_object_header(_closure$3912)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$3912->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18;
  _closure$3912->$0 = f$1443;
  _closure$3912->$1 = name$1395;
  _tmp$3312
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$3912;
  _closure$3913
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17$2d$cap
      )
    );
  Moonbit_object_header(_closure$3913)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$3913->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17;
  _closure$3913->$0 = name$1395;
  _closure$3913->$1 = file_name$1393;
  _closure$3913->$2 = handle_result$1382;
  _tmp$3313 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$3913;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_catch_error(
    _tmp$3312, _tmp$3313, on_err$1402
  );
  $joinlet$3909:;
  goto $joinlet$3908;
  $join$1417:;
  _closure$3914
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$3914)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$3914->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$3914->$0 = f$1418;
  _closure$3914->$1 = on_err$1402;
  _closure$3914->$2 = name$1395;
  _closure$3914->$3 = file_name$1393;
  _closure$3914->$4 = handle_result$1382;
  _tmp$3297
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$3914;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$1419, _tmp$3297);
  $joinlet$3908:;
  $joinlet$3906:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3320
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19$2d$cap* _casted_env$3321 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$19$2d$cap*)_env$3320;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3478 =
    _casted_env$3321->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1382 =
    _field$3478;
  moonbit_string_t _field$3477 = _casted_env$3321->$1;
  moonbit_string_t file_name$1393 = _field$3477;
  moonbit_string_t _field$3476 = _casted_env$3321->$0;
  int32_t _cnt$3781 = Moonbit_object_header(_casted_env$3321)->rc;
  moonbit_string_t name$1395;
  if (_cnt$3781 > 1) {
    int32_t _new_cnt$3782;
    moonbit_incref(handle_result$1382);
    moonbit_incref(file_name$1393);
    moonbit_incref(_field$3476);
    _new_cnt$3782 = _cnt$3781 - 1;
    Moonbit_object_header(_casted_env$3321)->rc = _new_cnt$3782;
  } else if (_cnt$3781 == 1) {
    moonbit_free(_casted_env$3321);
  }
  name$1395 = _field$3476;
  $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10(
    handle_result$1382,
      name$1395,
      file_name$1393,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3316
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18$2d$cap* _casted_env$3317 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$18$2d$cap*)_env$3316;
  moonbit_string_t _field$3480 = _casted_env$3317->$1;
  moonbit_string_t name$1395 = _field$3480;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3479 =
    _casted_env$3317->$0;
  int32_t _cnt$3783 = Moonbit_object_header(_casted_env$3317)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1443;
  int32_t _tmp$3318;
  if (_cnt$3783 > 1) {
    int32_t _new_cnt$3784;
    moonbit_incref(name$1395);
    moonbit_incref(_field$3479);
    _new_cnt$3784 = _cnt$3783 - 1;
    Moonbit_object_header(_casted_env$3317)->rc = _new_cnt$3784;
  } else if (_cnt$3783 == 1) {
    moonbit_free(_casted_env$3317);
  }
  f$1443 = _field$3479;
  _tmp$3318
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$1395
  );
  return f$1443->code(f$1443, _tmp$3318);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3314
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17$2d$cap* _casted_env$3315 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$17$2d$cap*)_env$3314;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3483 =
    _casted_env$3315->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1382 =
    _field$3483;
  moonbit_string_t _field$3482 = _casted_env$3315->$1;
  moonbit_string_t file_name$1393 = _field$3482;
  moonbit_string_t _field$3481 = _casted_env$3315->$0;
  int32_t _cnt$3785 = Moonbit_object_header(_casted_env$3315)->rc;
  moonbit_string_t name$1395;
  if (_cnt$3785 > 1) {
    int32_t _new_cnt$3786;
    moonbit_incref(handle_result$1382);
    moonbit_incref(file_name$1393);
    moonbit_incref(_field$3481);
    _new_cnt$3786 = _cnt$3785 - 1;
    Moonbit_object_header(_casted_env$3315)->rc = _new_cnt$3786;
  } else if (_cnt$3785 == 1) {
    moonbit_free(_casted_env$3315);
  }
  name$1395 = _field$3481;
  $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10(
    handle_result$1382,
      name$1395,
      file_name$1393,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$3298,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1420,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1421
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$3299 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$3298;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3488 =
    _casted_env$3299->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1382 =
    _field$3488;
  moonbit_string_t _field$3487 = _casted_env$3299->$3;
  moonbit_string_t file_name$1393 = _field$3487;
  moonbit_string_t _field$3486 = _casted_env$3299->$2;
  moonbit_string_t name$1395 = _field$3486;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$3485 = _casted_env$3299->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1402 = _field$3485;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3484 =
    _casted_env$3299->$0;
  int32_t _cnt$3787 = Moonbit_object_header(_casted_env$3299)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1418;
  int32_t _async_driver$1422;
  int32_t _tmp$3303;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16$2d$cap* _closure$3915;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$3304;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15$2d$cap* _closure$3916;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$3305;
  if (_cnt$3787 > 1) {
    int32_t _new_cnt$3788;
    moonbit_incref(handle_result$1382);
    moonbit_incref(file_name$1393);
    moonbit_incref(name$1395);
    moonbit_incref(on_err$1402);
    moonbit_incref(_field$3484);
    _new_cnt$3788 = _cnt$3787 - 1;
    Moonbit_object_header(_casted_env$3299)->rc = _new_cnt$3788;
  } else if (_cnt$3787 == 1) {
    moonbit_free(_casted_env$3299);
  }
  f$1418 = _field$3484;
  _async_driver$1422 = 0;
  moonbit_incref(name$1395);
  _tmp$3303
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$1395
  );
  moonbit_incref(_cont$1420);
  _closure$3915
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16$2d$cap
      )
    );
  Moonbit_object_header(_closure$3915)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$3915->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16;
  _closure$3915->$0 = _async_driver$1422;
  _closure$3915->$1 = _cont$1420;
  _closure$3915->$2 = name$1395;
  _closure$3915->$3 = file_name$1393;
  _closure$3915->$4 = handle_result$1382;
  _tmp$3304 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$3915;
  _closure$3916
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15$2d$cap
      )
    );
  Moonbit_object_header(_closure$3916)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$3916->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15;
  _closure$3916->$0 = _async_driver$1422;
  _closure$3916->$1 = _err_cont$1421;
  _closure$3916->$2 = _cont$1420;
  _closure$3916->$3 = on_err$1402;
  _tmp$3305 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$3916;
  f$1418->code(f$1418, _tmp$3303, _tmp$3304, _tmp$3305);
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$3309,
  int32_t _cont_param$1440
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16$2d$cap* _casted_env$3310 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$16$2d$cap*)_env$3309;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3493 =
    _casted_env$3310->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1382 =
    _field$3493;
  moonbit_string_t _field$3492 = _casted_env$3310->$3;
  moonbit_string_t file_name$1393 = _field$3492;
  moonbit_string_t _field$3491 = _casted_env$3310->$2;
  moonbit_string_t name$1395 = _field$3491;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3490 = _casted_env$3310->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1420 = _field$3490;
  int32_t _field$3489 = _casted_env$3310->$0;
  int32_t _cnt$3789 = Moonbit_object_header(_casted_env$3310)->rc;
  int32_t _async_driver$1422;
  void* State_1$3311;
  if (_cnt$3789 > 1) {
    int32_t _new_cnt$3790;
    moonbit_incref(handle_result$1382);
    moonbit_incref(file_name$1393);
    moonbit_incref(name$1395);
    moonbit_incref(_cont$1420);
    _new_cnt$3790 = _cnt$3789 - 1;
    Moonbit_object_header(_casted_env$3310)->rc = _new_cnt$3790;
  } else if (_cnt$3789 == 1) {
    moonbit_free(_casted_env$3310);
  }
  _async_driver$1422 = _field$3489;
  State_1$3311
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1
      )
    );
  Moonbit_object_header(State_1$3311)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1*)State_1$3311)->$0
  = _cont_param$1440;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1*)State_1$3311)->$1
  = handle_result$1382;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1*)State_1$3311)->$2
  = file_name$1393;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1*)State_1$3311)->$3
  = name$1395;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1*)State_1$3311)->$4
  = _cont$1420;
  $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$14(
    _async_driver$1422, State_1$3311
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3306,
  void* _cont_param$1441
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15$2d$cap* _casted_env$3307 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$15$2d$cap*)_env$3306;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$3497 = _casted_env$3307->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1402 = _field$3497;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3496 = _casted_env$3307->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1420 = _field$3496;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$3495 = _casted_env$3307->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1421 = _field$3495;
  int32_t _field$3494 = _casted_env$3307->$0;
  int32_t _cnt$3791 = Moonbit_object_header(_casted_env$3307)->rc;
  int32_t _async_driver$1422;
  void* _try$207$3308;
  if (_cnt$3791 > 1) {
    int32_t _new_cnt$3792;
    moonbit_incref(on_err$1402);
    moonbit_incref(_cont$1420);
    moonbit_incref(_err_cont$1421);
    _new_cnt$3792 = _cnt$3791 - 1;
    Moonbit_object_header(_casted_env$3307)->rc = _new_cnt$3792;
  } else if (_cnt$3791 == 1) {
    moonbit_free(_casted_env$3307);
  }
  _async_driver$1422 = _field$3494;
  _try$207$3308
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207
      )
    );
  Moonbit_object_header(_try$207$3308)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207*)_try$207$3308)->$0
  = _cont_param$1441;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207*)_try$207$3308)->$1
  = on_err$1402;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207*)_try$207$3308)->$2
  = _cont$1420;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207*)_try$207$3308)->$3
  = _err_cont$1421;
  $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$14(
    _async_driver$1422, _try$207$3308
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$14(
  int32_t _env$3300,
  void* _state$1423
) {
  switch (Moonbit_object_tag(_state$1423)) {
    case 0: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207* _$2a$try$207$1424 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$$2a$try$207*)_state$1423;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$3501 = _$2a$try$207$1424->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1425 = _field$3501;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3500 = _$2a$try$207$1424->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1426 = _field$3500;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$3499 = _$2a$try$207$1424->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1427 = _field$3499;
      void* _field$3498 = _$2a$try$207$1424->$0;
      int32_t _cnt$3793 = Moonbit_object_header(_$2a$try$207$1424)->rc;
      void* _try_err$1428;
      void* err$1430;
      void* err$1432;
      int32_t _tmp$3302;
      if (_cnt$3793 > 1) {
        int32_t _new_cnt$3794;
        moonbit_incref(_err_cont$1425);
        moonbit_incref(_cont$1426);
        moonbit_incref(on_err$1427);
        moonbit_incref(_field$3498);
        _new_cnt$3794 = _cnt$3793 - 1;
        Moonbit_object_header(_$2a$try$207$1424)->rc = _new_cnt$3794;
      } else if (_cnt$3793 == 1) {
        moonbit_free(_$2a$try$207$1424);
      }
      _try_err$1428 = _field$3498;
      if (
        $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$1427);
        moonbit_decref(_cont$1426);
        err$1432 = _try_err$1428;
        goto $join$1431;
      } else {
        moonbit_decref(_err_cont$1425);
        err$1430 = _try_err$1428;
        goto $join$1429;
      }
      goto $joinlet$3918;
      $join$1431:;
      return _err_cont$1425->code(_err_cont$1425, err$1432);
      $joinlet$3918:;
      goto $joinlet$3917;
      $join$1429:;
      _tmp$3302 = on_err$1427->code(on_err$1427, err$1430);
      _cont$1426->code(_cont$1426, _tmp$3302);
      $joinlet$3917:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1* _State_1$1433 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$223$on_err$68$$2a$arm$215$lambda$241$State$State_1*)_state$1423;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3505 = _State_1$1433->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1434 = _field$3505;
      moonbit_string_t _field$3504 = _State_1$1433->$3;
      moonbit_string_t name$1435 = _field$3504;
      moonbit_string_t _field$3503 = _State_1$1433->$2;
      moonbit_string_t file_name$1436 = _field$3503;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3502 =
        _State_1$1433->$1;
      int32_t _cnt$3795 = Moonbit_object_header(_State_1$1433)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1437;
      int32_t _tmp$3301;
      if (_cnt$3795 > 1) {
        int32_t _new_cnt$3796;
        moonbit_incref(_cont$1434);
        moonbit_incref(name$1435);
        moonbit_incref(file_name$1436);
        moonbit_incref(_field$3502);
        _new_cnt$3796 = _cnt$3795 - 1;
        Moonbit_object_header(_State_1$1433)->rc = _new_cnt$3796;
      } else if (_cnt$3795 == 1) {
        moonbit_free(_State_1$1433);
      }
      handle_result$1437 = _field$3502;
      _tmp$3301
      = handle_result$1437->code(
        handle_result$1437,
          name$1435,
          file_name$1436,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$1434->code(_cont$1434, _tmp$3301);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3295,
  void* err$1403
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12$2d$cap* _casted_env$3296 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$12$2d$cap*)_env$3295;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3512 =
    _casted_env$3296->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1382 =
    _field$3512;
  moonbit_string_t _field$3511 = _casted_env$3296->$1;
  moonbit_string_t file_name$1393 = _field$3511;
  moonbit_string_t _field$3510 = _casted_env$3296->$0;
  int32_t _cnt$3797 = Moonbit_object_header(_casted_env$3296)->rc;
  moonbit_string_t name$1395;
  void* e$1405;
  moonbit_string_t e$1408;
  moonbit_string_t message$1406;
  if (_cnt$3797 > 1) {
    int32_t _new_cnt$3798;
    moonbit_incref(handle_result$1382);
    moonbit_incref(file_name$1393);
    moonbit_incref(_field$3510);
    _new_cnt$3798 = _cnt$3797 - 1;
    Moonbit_object_header(_casted_env$3296)->rc = _new_cnt$3798;
  } else if (_cnt$3797 == 1) {
    moonbit_free(_casted_env$3296);
  }
  name$1395 = _field$3510;
  switch (Moonbit_object_tag(err$1403)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$1409 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$1403;
      moonbit_string_t _field$3506 = _Failure$1409->$0;
      int32_t _cnt$3799 = Moonbit_object_header(_Failure$1409)->rc;
      moonbit_string_t _e$1410;
      if (_cnt$3799 > 1) {
        int32_t _new_cnt$3800;
        moonbit_incref(_field$3506);
        _new_cnt$3800 = _cnt$3799 - 1;
        Moonbit_object_header(_Failure$1409)->rc = _new_cnt$3800;
      } else if (_cnt$3799 == 1) {
        moonbit_free(_Failure$1409);
      }
      _e$1410 = _field$3506;
      e$1408 = _e$1410;
      goto $join$1407;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$1411 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$1403;
      moonbit_string_t _field$3507 = _InspectError$1411->$0;
      int32_t _cnt$3801 = Moonbit_object_header(_InspectError$1411)->rc;
      moonbit_string_t _e$1412;
      if (_cnt$3801 > 1) {
        int32_t _new_cnt$3802;
        moonbit_incref(_field$3507);
        _new_cnt$3802 = _cnt$3801 - 1;
        Moonbit_object_header(_InspectError$1411)->rc = _new_cnt$3802;
      } else if (_cnt$3801 == 1) {
        moonbit_free(_InspectError$1411);
      }
      _e$1412 = _field$3507;
      e$1408 = _e$1412;
      goto $join$1407;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$1413 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$1403;
      moonbit_string_t _field$3508 = _SnapshotError$1413->$0;
      int32_t _cnt$3803 = Moonbit_object_header(_SnapshotError$1413)->rc;
      moonbit_string_t _e$1414;
      if (_cnt$3803 > 1) {
        int32_t _new_cnt$3804;
        moonbit_incref(_field$3508);
        _new_cnt$3804 = _cnt$3803 - 1;
        Moonbit_object_header(_SnapshotError$1413)->rc = _new_cnt$3804;
      } else if (_cnt$3803 == 1) {
        moonbit_free(_SnapshotError$1413);
      }
      _e$1414 = _field$3508;
      e$1408 = _e$1414;
      goto $join$1407;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$1415 =
        (struct $Error$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$1403;
      moonbit_string_t _field$3509 =
        _MoonBitTestDriverInternalJsError$1415->$0;
      int32_t _cnt$3805 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1415)->rc;
      moonbit_string_t _e$1416;
      if (_cnt$3805 > 1) {
        int32_t _new_cnt$3806;
        moonbit_incref(_field$3509);
        _new_cnt$3806 = _cnt$3805 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1415)->rc
        = _new_cnt$3806;
      } else if (_cnt$3805 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$1415);
      }
      _e$1416 = _field$3509;
      e$1408 = _e$1416;
      goto $join$1407;
      break;
    }
    default: {
      e$1405 = err$1403;
      goto $join$1404;
      break;
    }
  }
  goto $joinlet$3920;
  $join$1407:;
  $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10(
    handle_result$1382, name$1395, file_name$1393, e$1408, 0
  );
  $joinlet$3920:;
  goto $joinlet$3919;
  $join$1404:;
  message$1406 = $Error$to_string(e$1405);
  $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10(
    handle_result$1382, name$1395, file_name$1393, message$1406, 0
  );
  $joinlet$3919:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$String$3e$$3d$$3e$Int* _env$3281,
  moonbit_string_t attr$1396
) {
  int32_t _tmp$3283;
  int64_t _tmp$3282;
  moonbit_decref(_env$3281);
  _tmp$3283 = Moonbit_array_length(attr$1396);
  _tmp$3282 = (int64_t)_tmp$3283;
  moonbit_incref(attr$1396);
  if ($String$$char_length_ge$inner(attr$1396, 5, 0, _tmp$3282)) {
    int32_t _tmp$3288 = attr$1396[0];
    int32_t _x$1397 = _tmp$3288;
    if (_x$1397 == 112) {
      int32_t _tmp$3287 = attr$1396[1];
      int32_t _x$1398 = _tmp$3287;
      if (_x$1398 == 97) {
        int32_t _tmp$3286 = attr$1396[2];
        int32_t _x$1399 = _tmp$3286;
        if (_x$1399 == 110) {
          int32_t _tmp$3285 = attr$1396[3];
          int32_t _x$1400 = _tmp$3285;
          if (_x$1400 == 105) {
            int32_t _tmp$3513 = attr$1396[4];
            int32_t _tmp$3284;
            int32_t _x$1401;
            moonbit_decref(attr$1396);
            _tmp$3284 = _tmp$3513;
            _x$1401 = _tmp$3284;
            return _x$1401 == 99 || 0;
          } else {
            moonbit_decref(attr$1396);
            return 0;
          }
        } else {
          moonbit_decref(attr$1396);
          return 0;
        }
      } else {
        moonbit_decref(attr$1396);
        return 0;
      }
    } else {
      moonbit_decref(attr$1396);
      return 0;
    }
  } else {
    moonbit_decref(attr$1396);
    return 0;
  }
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$3265,
  moonbit_string_t test_name$1383,
  moonbit_string_t file_name$1384,
  moonbit_string_t message$1385,
  int32_t skipped$1386
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10$2d$cap* _casted_env$3266 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$10$2d$cap*)_env$3265;
  int32_t _field$3514 = _casted_env$3266->$0;
  int32_t index$1381;
  int32_t _if_result$3921;
  moonbit_string_t file_name$1387;
  moonbit_string_t test_name$1388;
  moonbit_string_t message$1389;
  moonbit_string_t _tmp$3278;
  moonbit_string_t _tmp$3277;
  moonbit_string_t _tmp$3275;
  moonbit_string_t _tmp$3276;
  moonbit_string_t _tmp$3274;
  moonbit_string_t _tmp$3272;
  moonbit_string_t _tmp$3273;
  moonbit_string_t _tmp$3271;
  moonbit_string_t _tmp$3269;
  moonbit_string_t _tmp$3270;
  moonbit_string_t _tmp$3268;
  moonbit_string_t _tmp$3267;
  moonbit_decref(_casted_env$3266);
  index$1381 = _field$3514;
  if (!skipped$1386) {
    _if_result$3921 = 1;
  } else {
    _if_result$3921 = 0;
  }
  if (_if_result$3921) {
    
  }
  file_name$1387 = $String$$escape(file_name$1384);
  test_name$1388 = $String$$escape(test_name$1383);
  message$1389 = $String$$escape(message$1385);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$3278
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$1387
  );
  _tmp$3277
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$3278
  );
  _tmp$3275
  = moonbit_add_string(
    _tmp$3277, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$3276
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$1381
  );
  _tmp$3274 = moonbit_add_string(_tmp$3275, _tmp$3276);
  _tmp$3272
  = moonbit_add_string(
    _tmp$3274, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$3273
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$1388
  );
  _tmp$3271 = moonbit_add_string(_tmp$3272, _tmp$3273);
  _tmp$3269
  = moonbit_add_string(
    _tmp$3271, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$3270
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    message$1389
  );
  _tmp$3268 = moonbit_add_string(_tmp$3269, _tmp$3270);
  _tmp$3267
  = moonbit_add_string(
    _tmp$3268, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$3267);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1378
) {
  moonbit_decref(_discard_$1378);
  return 42;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1376,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1377,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1374
) {
  void* _try_err$1372;
  struct moonbit_result_0 _tmp$3923 = f$1376->code(f$1376);
  void* err$1373;
  if (_tmp$3923.tag) {
    int32_t const _ok$3263 = _tmp$3923.data.ok;
    moonbit_decref(on_err$1374);
  } else {
    void* const _err$3264 = _tmp$3923.data.err;
    moonbit_decref(on_ok$1377);
    _try_err$1372 = _err$3264;
    goto $join$1371;
  }
  on_ok$1377->code(on_ok$1377);
  goto $joinlet$3922;
  $join$1371:;
  err$1373 = _try_err$1372;
  on_err$1374->code(on_err$1374, err$1373);
  $joinlet$3922:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1337,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1350,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1363,
  moonbit_string_t file_filter$1334,
  int32_t index_filter$1335
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1331;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1332;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1336;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3520;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$3254;
  void* F0$3251;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3519;
  int32_t _cnt$3807;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$3253;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$3252;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1333;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1346;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1347;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1349;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3518;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$3258;
  void* F1$3255;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3517;
  int32_t _cnt$3810;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$3257;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$3256;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1348;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1359;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1360;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1362;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3516;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3262;
  void* F2$3259;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3515;
  int32_t _cnt$3813;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$3261;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$3260;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1361;
  moonbit_incref(file_filter$1334);
  _bind$1336
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$1337, file_filter$1334
  );
  if (_bind$1336 == 0) {
    if (_bind$1336) {
      moonbit_decref(_bind$1336);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1338 =
      _bind$1336;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1339 =
      _Some$1338;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1341;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1342;
    moonbit_incref(_index_func_map$1339);
    _bind$1342
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$1339, index_filter$1335
    );
    if (_bind$1342 == 0) {
      if (_bind$1342) {
        moonbit_decref(_bind$1342);
      }
      moonbit_decref(_index_func_map$1339);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1343;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1344;
      moonbit_decref(async_tests$1363);
      moonbit_decref(with_args_tests$1350);
      _Some$1343 = _bind$1342;
      _func_attrs_tuple$1344 = _Some$1343;
      func_attrs_tuple$1341 = _func_attrs_tuple$1344;
      goto $join$1340;
    }
    goto $joinlet$3925;
    $join$1340:;
    index_func_map$1331 = _index_func_map$1339;
    func_attrs_tuple$1332 = func_attrs_tuple$1341;
    goto $join$1330;
    $joinlet$3925:;
  }
  goto $joinlet$3924;
  $join$1330:;
  moonbit_decref(index_func_map$1331);
  _field$3520 = func_attrs_tuple$1332->$0;
  _tmp$3254 = _field$3520;
  moonbit_incref(_tmp$3254);
  F0$3251
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$3251)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$3251)->$0 = _tmp$3254;
  _field$3519 = func_attrs_tuple$1332->$1;
  _cnt$3807 = Moonbit_object_header(func_attrs_tuple$1332)->rc;
  if (_cnt$3807 > 1) {
    int32_t _new_cnt$3809;
    moonbit_incref(_field$3519);
    _new_cnt$3809 = _cnt$3807 - 1;
    Moonbit_object_header(func_attrs_tuple$1332)->rc = _new_cnt$3809;
  } else if (_cnt$3807 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3808 =
      func_attrs_tuple$1332->$0;
    moonbit_decref(_field$3808);
    moonbit_free(func_attrs_tuple$1332);
  }
  _tmp$3253 = _field$3519;
  _tmp$3252
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$3252)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$3252->$0 = file_filter$1334;
  _tmp$3252->$1 = index_filter$1335;
  _tmp$3252->$2 = _tmp$3253;
  k$1333
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1333)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1333->$0 = F0$3251;
  k$1333->$1 = _tmp$3252;
  return k$1333;
  $joinlet$3924:;
  moonbit_incref(file_filter$1334);
  _bind$1349
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$1350, file_filter$1334
  );
  if (_bind$1349 == 0) {
    if (_bind$1349) {
      moonbit_decref(_bind$1349);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1351 =
      _bind$1349;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1352 =
      _Some$1351;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1354;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1355;
    moonbit_incref(_index_func_map$1352);
    _bind$1355
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$1352, index_filter$1335
    );
    if (_bind$1355 == 0) {
      if (_bind$1355) {
        moonbit_decref(_bind$1355);
      }
      moonbit_decref(_index_func_map$1352);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1356;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1357;
      moonbit_decref(async_tests$1363);
      _Some$1356 = _bind$1355;
      _func_attrs_tuple$1357 = _Some$1356;
      func_attrs_tuple$1354 = _func_attrs_tuple$1357;
      goto $join$1353;
    }
    goto $joinlet$3927;
    $join$1353:;
    index_func_map$1346 = _index_func_map$1352;
    func_attrs_tuple$1347 = func_attrs_tuple$1354;
    goto $join$1345;
    $joinlet$3927:;
  }
  goto $joinlet$3926;
  $join$1345:;
  moonbit_decref(index_func_map$1346);
  _field$3518 = func_attrs_tuple$1347->$0;
  _tmp$3258 = _field$3518;
  moonbit_incref(_tmp$3258);
  F1$3255
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$3255)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$3255)->$0 = _tmp$3258;
  _field$3517 = func_attrs_tuple$1347->$1;
  _cnt$3810 = Moonbit_object_header(func_attrs_tuple$1347)->rc;
  if (_cnt$3810 > 1) {
    int32_t _new_cnt$3812;
    moonbit_incref(_field$3517);
    _new_cnt$3812 = _cnt$3810 - 1;
    Moonbit_object_header(func_attrs_tuple$1347)->rc = _new_cnt$3812;
  } else if (_cnt$3810 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3811 =
      func_attrs_tuple$1347->$0;
    moonbit_decref(_field$3811);
    moonbit_free(func_attrs_tuple$1347);
  }
  _tmp$3257 = _field$3517;
  _tmp$3256
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$3256)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$3256->$0 = file_filter$1334;
  _tmp$3256->$1 = index_filter$1335;
  _tmp$3256->$2 = _tmp$3257;
  k$1348
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1348)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1348->$0 = F1$3255;
  k$1348->$1 = _tmp$3256;
  return k$1348;
  $joinlet$3926:;
  moonbit_incref(file_filter$1334);
  _bind$1362
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$1363, file_filter$1334
  );
  if (_bind$1362 == 0) {
    if (_bind$1362) {
      moonbit_decref(_bind$1362);
    }
    moonbit_decref(file_filter$1334);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1364 =
      _bind$1362;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1365 =
      _Some$1364;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1367;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1368;
    moonbit_incref(_index_func_map$1365);
    _bind$1368
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$1365, index_filter$1335
    );
    if (_bind$1368 == 0) {
      if (_bind$1368) {
        moonbit_decref(_bind$1368);
      }
      moonbit_decref(_index_func_map$1365);
      moonbit_decref(file_filter$1334);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1369 =
        _bind$1368;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1370 =
        _Some$1369;
      func_attrs_tuple$1367 = _func_attrs_tuple$1370;
      goto $join$1366;
    }
    goto $joinlet$3929;
    $join$1366:;
    index_func_map$1359 = _index_func_map$1365;
    func_attrs_tuple$1360 = func_attrs_tuple$1367;
    goto $join$1358;
    $joinlet$3929:;
  }
  goto $joinlet$3928;
  $join$1358:;
  moonbit_decref(index_func_map$1359);
  _field$3516 = func_attrs_tuple$1360->$0;
  _tmp$3262 = _field$3516;
  moonbit_incref(_tmp$3262);
  F2$3259
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$3259)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$3259)->$0 = _tmp$3262;
  _field$3515 = func_attrs_tuple$1360->$1;
  _cnt$3813 = Moonbit_object_header(func_attrs_tuple$1360)->rc;
  if (_cnt$3813 > 1) {
    int32_t _new_cnt$3815;
    moonbit_incref(_field$3515);
    _new_cnt$3815 = _cnt$3813 - 1;
    Moonbit_object_header(func_attrs_tuple$1360)->rc = _new_cnt$3815;
  } else if (_cnt$3813 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3814 =
      func_attrs_tuple$1360->$0;
    moonbit_decref(_field$3814);
    moonbit_free(func_attrs_tuple$1360);
  }
  _tmp$3261 = _field$3515;
  _tmp$3260
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$3260)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$3260->$0 = file_filter$1334;
  _tmp$3260->$1 = index_filter$1335;
  _tmp$3260->$2 = _tmp$3261;
  k$1361
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1361)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1361->$0 = F2$3259;
  k$1361->$1 = _tmp$3260;
  return k$1361;
  $joinlet$3928:;
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7(
  
) {
  struct $$3c$String$2a$Int$2a$String$3e$* person$1321 =
    (struct $$3c$String$2a$Int$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$Int$2a$String$3e$)
    );
  moonbit_string_t _field$3525;
  moonbit_string_t _tmp$3216;
  moonbit_string_t _tmp$3217;
  struct moonbit_result_0 _tmp$3930;
  int32_t _tmp$3220;
  moonbit_string_t _tmp$3221;
  struct moonbit_result_0 _tmp$3932;
  moonbit_string_t _field$3524;
  moonbit_string_t _tmp$3224;
  moonbit_string_t _tmp$3225;
  struct moonbit_result_0 _tmp$3934;
  moonbit_string_t _field$3523;
  moonbit_string_t _tmp$3247;
  int32_t _tmp$3250;
  int32_t _tmp$3248;
  moonbit_string_t _field$3522;
  int32_t _cnt$3816;
  moonbit_string_t _tmp$3249;
  struct $$3c$String$2a$Int$2a$String$3e$* updated_person$1322;
  int32_t _field$3521;
  int32_t _tmp$3228;
  moonbit_string_t _tmp$3229;
  struct moonbit_result_0 _tmp$3936;
  int32_t empty_tuple$1323;
  int32_t _tmp$3232;
  moonbit_string_t _tmp$3233;
  struct moonbit_result_0 _tmp$3938;
  int32_t describe_number$1324;
  moonbit_string_t _tmp$3237;
  moonbit_string_t _tmp$3238;
  struct moonbit_result_0 _tmp$3940;
  moonbit_string_t _tmp$3241;
  moonbit_string_t _tmp$3242;
  struct moonbit_result_0 _tmp$3942;
  moonbit_string_t _tmp$3245;
  moonbit_string_t _tmp$3246;
  Moonbit_object_header(person$1321)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$Int$2a$String$3e$, $0) >> 2, 2, 0
  );
  person$1321->$0 = (moonbit_string_t)moonbit_string_literal_14.data;
  person$1321->$1 = 30;
  person$1321->$2 = (moonbit_string_t)moonbit_string_literal_15.data;
  _field$3525 = person$1321->$0;
  _tmp$3216 = _field$3525;
  _tmp$3217 = 0;
  moonbit_incref(_tmp$3216);
  _tmp$3930
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3216,
      (moonbit_string_t)moonbit_string_literal_14.data,
      _tmp$3217,
      (moonbit_string_t)moonbit_string_literal_16.data
  );
  if (_tmp$3930.tag) {
    int32_t const _ok$3218 = _tmp$3930.data.ok;
  } else {
    void* const _err$3219 = _tmp$3930.data.err;
    struct moonbit_result_0 _result$3931;
    moonbit_decref(person$1321);
    _result$3931.tag = 0;
    _result$3931.data.err = _err$3219;
    return _result$3931;
  }
  _tmp$3220 = person$1321->$1;
  _tmp$3221 = 0;
  _tmp$3932
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3220,
      30,
      _tmp$3221,
      (moonbit_string_t)moonbit_string_literal_17.data
  );
  if (_tmp$3932.tag) {
    int32_t const _ok$3222 = _tmp$3932.data.ok;
  } else {
    void* const _err$3223 = _tmp$3932.data.err;
    struct moonbit_result_0 _result$3933;
    moonbit_decref(person$1321);
    _result$3933.tag = 0;
    _result$3933.data.err = _err$3223;
    return _result$3933;
  }
  _field$3524 = person$1321->$2;
  _tmp$3224 = _field$3524;
  _tmp$3225 = 0;
  moonbit_incref(_tmp$3224);
  _tmp$3934
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3224,
      (moonbit_string_t)moonbit_string_literal_15.data,
      _tmp$3225,
      (moonbit_string_t)moonbit_string_literal_18.data
  );
  if (_tmp$3934.tag) {
    int32_t const _ok$3226 = _tmp$3934.data.ok;
  } else {
    void* const _err$3227 = _tmp$3934.data.err;
    struct moonbit_result_0 _result$3935;
    moonbit_decref(person$1321);
    _result$3935.tag = 0;
    _result$3935.data.err = _err$3227;
    return _result$3935;
  }
  _field$3523 = person$1321->$0;
  _tmp$3247 = _field$3523;
  _tmp$3250 = person$1321->$1;
  _tmp$3248 = _tmp$3250 + 1;
  _field$3522 = person$1321->$2;
  _cnt$3816 = Moonbit_object_header(person$1321)->rc;
  if (_cnt$3816 > 1) {
    int32_t _new_cnt$3817;
    moonbit_incref(_field$3522);
    moonbit_incref(_tmp$3247);
    _new_cnt$3817 = _cnt$3816 - 1;
    Moonbit_object_header(person$1321)->rc = _new_cnt$3817;
  } else if (_cnt$3816 == 1) {
    moonbit_free(person$1321);
  }
  _tmp$3249 = _field$3522;
  updated_person$1322
  = (struct $$3c$String$2a$Int$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$Int$2a$String$3e$)
    );
  Moonbit_object_header(updated_person$1322)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$Int$2a$String$3e$, $0) >> 2, 2, 0
  );
  updated_person$1322->$0 = _tmp$3247;
  updated_person$1322->$1 = _tmp$3248;
  updated_person$1322->$2 = _tmp$3249;
  _field$3521 = updated_person$1322->$1;
  moonbit_decref(updated_person$1322);
  _tmp$3228 = _field$3521;
  _tmp$3229 = 0;
  _tmp$3936
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3228,
      31,
      _tmp$3229,
      (moonbit_string_t)moonbit_string_literal_19.data
  );
  if (_tmp$3936.tag) {
    int32_t const _ok$3230 = _tmp$3936.data.ok;
  } else {
    void* const _err$3231 = _tmp$3936.data.err;
    struct moonbit_result_0 _result$3937;
    _result$3937.tag = 0;
    _result$3937.data.err = _err$3231;
    return _result$3937;
  }
  empty_tuple$1323 = 0;
  _tmp$3232 = 0;
  _tmp$3233 = 0;
  _tmp$3938
  = $moonbitlang$core$builtin$assert_eq$6(
    empty_tuple$1323,
      _tmp$3232,
      _tmp$3233,
      (moonbit_string_t)moonbit_string_literal_20.data
  );
  if (_tmp$3938.tag) {
    int32_t const _ok$3234 = _tmp$3938.data.ok;
  } else {
    void* const _err$3235 = _tmp$3938.data.err;
    struct moonbit_result_0 _result$3939;
    _result$3939.tag = 0;
    _result$3939.data.err = _err$3235;
    return _result$3939;
  }
  describe_number$1324 = 0;
  _tmp$3237
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$describe_number$fn$9(
    describe_number$1324, 10
  );
  _tmp$3238 = 0;
  _tmp$3940
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3237,
      (moonbit_string_t)moonbit_string_literal_21.data,
      _tmp$3238,
      (moonbit_string_t)moonbit_string_literal_22.data
  );
  if (_tmp$3940.tag) {
    int32_t const _ok$3239 = _tmp$3940.data.ok;
  } else {
    void* const _err$3240 = _tmp$3940.data.err;
    struct moonbit_result_0 _result$3941;
    _result$3941.tag = 0;
    _result$3941.data.err = _err$3240;
    return _result$3941;
  }
  _tmp$3241
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$describe_number$fn$9(
    describe_number$1324, -5
  );
  _tmp$3242 = 0;
  _tmp$3942
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3241,
      (moonbit_string_t)moonbit_string_literal_23.data,
      _tmp$3242,
      (moonbit_string_t)moonbit_string_literal_24.data
  );
  if (_tmp$3942.tag) {
    int32_t const _ok$3243 = _tmp$3942.data.ok;
  } else {
    void* const _err$3244 = _tmp$3942.data.err;
    struct moonbit_result_0 _result$3943;
    _result$3943.tag = 0;
    _result$3943.data.err = _err$3244;
    return _result$3943;
  }
  _tmp$3245
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$describe_number$fn$9(
    describe_number$1324, 0
  );
  _tmp$3246 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           _tmp$3245,
             (moonbit_string_t)moonbit_string_literal_25.data,
             _tmp$3246,
             (moonbit_string_t)moonbit_string_literal_26.data
         );
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$describe_number$fn$9(
  int32_t _env$3236,
  int32_t n$1325
) {
  if (n$1325 > 0) {
    return (moonbit_string_t)moonbit_string_literal_21.data;
  } else if (n$1325 < 0) {
    return (moonbit_string_t)moonbit_string_literal_23.data;
  } else {
    return (moonbit_string_t)moonbit_string_literal_25.data;
  }
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6(
  
) {
  int32_t add$1309 = 0;
  int32_t multiply$1312 = 0;
  int32_t _tmp$3196 =
    $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$add$fn$5(
      add$1309, 5, 3
    );
  moonbit_string_t _tmp$3197 = 0;
  struct moonbit_result_0 _tmp$3944 =
    $moonbitlang$core$builtin$assert_eq$0(
      _tmp$3196,
        8,
        _tmp$3197,
        (moonbit_string_t)moonbit_string_literal_27.data
    );
  int32_t _tmp$3200;
  moonbit_string_t _tmp$3201;
  struct moonbit_result_0 _tmp$3946;
  int32_t _tmp$3204;
  moonbit_string_t _tmp$3205;
  struct moonbit_result_0 _tmp$3948;
  int32_t _tmp$3208;
  moonbit_string_t _tmp$3209;
  struct moonbit_result_0 _tmp$3950;
  int32_t apply_twice$1315;
  struct $$3c$Unit$3e$$3d$$3e$Unit* increment$1318;
  int32_t result$1320;
  moonbit_string_t _tmp$3215;
  if (_tmp$3944.tag) {
    int32_t const _ok$3198 = _tmp$3944.data.ok;
  } else {
    void* const _err$3199 = _tmp$3944.data.err;
    struct moonbit_result_0 _result$3945;
    _result$3945.tag = 0;
    _result$3945.data.err = _err$3199;
    return _result$3945;
  }
  _tmp$3200
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$multiply$fn$6(
    multiply$1312, 4, 6
  );
  _tmp$3201 = 0;
  _tmp$3946
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3200,
      24,
      _tmp$3201,
      (moonbit_string_t)moonbit_string_literal_28.data
  );
  if (_tmp$3946.tag) {
    int32_t const _ok$3202 = _tmp$3946.data.ok;
  } else {
    void* const _err$3203 = _tmp$3946.data.err;
    struct moonbit_result_0 _result$3947;
    _result$3947.tag = 0;
    _result$3947.data.err = _err$3203;
    return _result$3947;
  }
  _tmp$3204
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$add$fn$5(
    add$1309, 0, 0
  );
  _tmp$3205 = 0;
  _tmp$3948
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3204, 0, _tmp$3205, (moonbit_string_t)moonbit_string_literal_29.data
  );
  if (_tmp$3948.tag) {
    int32_t const _ok$3206 = _tmp$3948.data.ok;
  } else {
    void* const _err$3207 = _tmp$3948.data.err;
    struct moonbit_result_0 _result$3949;
    _result$3949.tag = 0;
    _result$3949.data.err = _err$3207;
    return _result$3949;
  }
  _tmp$3208
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$multiply$fn$6(
    multiply$1312, 0, 5
  );
  _tmp$3209 = 0;
  _tmp$3950
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3208, 0, _tmp$3209, (moonbit_string_t)moonbit_string_literal_30.data
  );
  if (_tmp$3950.tag) {
    int32_t const _ok$3210 = _tmp$3950.data.ok;
  } else {
    void* const _err$3211 = _tmp$3950.data.err;
    struct moonbit_result_0 _result$3951;
    _result$3951.tag = 0;
    _result$3951.data.err = _err$3211;
    return _result$3951;
  }
  apply_twice$1315 = 0;
  increment$1318
  = (struct $$3c$Unit$3e$$3d$$3e$Unit*)&$$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$increment$fn$8$closure.data;
  result$1320
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$apply_twice$fn$7(
    apply_twice$1315, increment$1318, 5
  );
  _tmp$3215 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           result$1320,
             7,
             _tmp$3215,
             (moonbit_string_t)moonbit_string_literal_31.data
         );
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$increment$fn$8(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$3214,
  int32_t n$1319
) {
  moonbit_decref(_env$3214);
  return n$1319 + 1;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$apply_twice$fn$7(
  int32_t _env$3212,
  struct $$3c$Unit$3e$$3d$$3e$Unit* f$1316,
  int32_t x$1317
) {
  int32_t _tmp$3213;
  moonbit_incref(f$1316);
  _tmp$3213 = f$1316->code(f$1316, x$1317);
  return f$1316->code(f$1316, _tmp$3213);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$multiply$fn$6(
  int32_t _env$3195,
  int32_t a$1313,
  int32_t b$1314
) {
  return a$1313 * b$1314;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$add$fn$5(
  int32_t _env$3194,
  int32_t a$1310,
  int32_t b$1311
) {
  return a$1310 + b$1311;
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5(
  
) {
  int32_t true_val$1302 = 1;
  int32_t false_val$1303 = 0;
  int32_t _tmp$3155 = true_val$1302 && true_val$1302;
  moonbit_string_t _tmp$3156 = 0;
  struct moonbit_result_0 _tmp$3952 =
    $moonbitlang$core$builtin$assert_true(
      _tmp$3155, _tmp$3156, (moonbit_string_t)moonbit_string_literal_32.data
    );
  int32_t _tmp$3159;
  moonbit_string_t _tmp$3160;
  struct moonbit_result_0 _tmp$3954;
  int32_t _tmp$3163;
  moonbit_string_t _tmp$3164;
  struct moonbit_result_0 _tmp$3956;
  int32_t _tmp$3167;
  moonbit_string_t _tmp$3168;
  struct moonbit_result_0 _tmp$3958;
  int32_t _tmp$3171;
  moonbit_string_t _tmp$3172;
  struct moonbit_result_0 _tmp$3960;
  int32_t _tmp$3175;
  moonbit_string_t _tmp$3176;
  struct moonbit_result_0 _tmp$3962;
  int32_t _tmp$3179;
  moonbit_string_t _tmp$3180;
  struct moonbit_result_0 _tmp$3964;
  int32_t _tmp$3183;
  moonbit_string_t _tmp$3184;
  struct moonbit_result_0 _tmp$3966;
  int32_t x$1304;
  int32_t y$1305;
  moonbit_string_t result1$1306;
  moonbit_string_t result2$1307;
  moonbit_string_t result3$1308;
  moonbit_string_t _tmp$3187;
  struct moonbit_result_0 _tmp$3968;
  moonbit_string_t _tmp$3190;
  struct moonbit_result_0 _tmp$3970;
  moonbit_string_t _tmp$3193;
  if (_tmp$3952.tag) {
    int32_t const _ok$3157 = _tmp$3952.data.ok;
  } else {
    void* const _err$3158 = _tmp$3952.data.err;
    struct moonbit_result_0 _result$3953;
    _result$3953.tag = 0;
    _result$3953.data.err = _err$3158;
    return _result$3953;
  }
  _tmp$3159 = true_val$1302 && false_val$1303;
  _tmp$3160 = 0;
  _tmp$3954
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3159, _tmp$3160, (moonbit_string_t)moonbit_string_literal_33.data
  );
  if (_tmp$3954.tag) {
    int32_t const _ok$3161 = _tmp$3954.data.ok;
  } else {
    void* const _err$3162 = _tmp$3954.data.err;
    struct moonbit_result_0 _result$3955;
    _result$3955.tag = 0;
    _result$3955.data.err = _err$3162;
    return _result$3955;
  }
  _tmp$3163 = false_val$1303 && false_val$1303;
  _tmp$3164 = 0;
  _tmp$3956
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3163, _tmp$3164, (moonbit_string_t)moonbit_string_literal_34.data
  );
  if (_tmp$3956.tag) {
    int32_t const _ok$3165 = _tmp$3956.data.ok;
  } else {
    void* const _err$3166 = _tmp$3956.data.err;
    struct moonbit_result_0 _result$3957;
    _result$3957.tag = 0;
    _result$3957.data.err = _err$3166;
    return _result$3957;
  }
  _tmp$3167 = true_val$1302 || true_val$1302;
  _tmp$3168 = 0;
  _tmp$3958
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3167, _tmp$3168, (moonbit_string_t)moonbit_string_literal_35.data
  );
  if (_tmp$3958.tag) {
    int32_t const _ok$3169 = _tmp$3958.data.ok;
  } else {
    void* const _err$3170 = _tmp$3958.data.err;
    struct moonbit_result_0 _result$3959;
    _result$3959.tag = 0;
    _result$3959.data.err = _err$3170;
    return _result$3959;
  }
  _tmp$3171 = true_val$1302 || false_val$1303;
  _tmp$3172 = 0;
  _tmp$3960
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3171, _tmp$3172, (moonbit_string_t)moonbit_string_literal_36.data
  );
  if (_tmp$3960.tag) {
    int32_t const _ok$3173 = _tmp$3960.data.ok;
  } else {
    void* const _err$3174 = _tmp$3960.data.err;
    struct moonbit_result_0 _result$3961;
    _result$3961.tag = 0;
    _result$3961.data.err = _err$3174;
    return _result$3961;
  }
  _tmp$3175 = false_val$1303 || false_val$1303;
  _tmp$3176 = 0;
  _tmp$3962
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3175, _tmp$3176, (moonbit_string_t)moonbit_string_literal_37.data
  );
  if (_tmp$3962.tag) {
    int32_t const _ok$3177 = _tmp$3962.data.ok;
  } else {
    void* const _err$3178 = _tmp$3962.data.err;
    struct moonbit_result_0 _result$3963;
    _result$3963.tag = 0;
    _result$3963.data.err = _err$3178;
    return _result$3963;
  }
  _tmp$3179 = !true_val$1302;
  _tmp$3180 = 0;
  _tmp$3964
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3179, _tmp$3180, (moonbit_string_t)moonbit_string_literal_38.data
  );
  if (_tmp$3964.tag) {
    int32_t const _ok$3181 = _tmp$3964.data.ok;
  } else {
    void* const _err$3182 = _tmp$3964.data.err;
    struct moonbit_result_0 _result$3965;
    _result$3965.tag = 0;
    _result$3965.data.err = _err$3182;
    return _result$3965;
  }
  _tmp$3183 = !false_val$1303;
  _tmp$3184 = 0;
  _tmp$3966
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3183, _tmp$3184, (moonbit_string_t)moonbit_string_literal_39.data
  );
  if (_tmp$3966.tag) {
    int32_t const _ok$3185 = _tmp$3966.data.ok;
  } else {
    void* const _err$3186 = _tmp$3966.data.err;
    struct moonbit_result_0 _result$3967;
    _result$3967.tag = 0;
    _result$3967.data.err = _err$3186;
    return _result$3967;
  }
  x$1304 = 10;
  y$1305 = 20;
  if (x$1304 > y$1305) {
    result1$1306 = (moonbit_string_t)moonbit_string_literal_40.data;
  } else {
    result1$1306 = (moonbit_string_t)moonbit_string_literal_41.data;
  }
  if (x$1304 < y$1305) {
    result2$1307 = (moonbit_string_t)moonbit_string_literal_42.data;
  } else {
    result2$1307 = (moonbit_string_t)moonbit_string_literal_43.data;
  }
  if (x$1304 == y$1305) {
    result3$1308 = (moonbit_string_t)moonbit_string_literal_44.data;
  } else {
    result3$1308 = (moonbit_string_t)moonbit_string_literal_45.data;
  }
  _tmp$3187 = 0;
  _tmp$3968
  = $moonbitlang$core$builtin$assert_eq$1(
    result1$1306,
      (moonbit_string_t)moonbit_string_literal_41.data,
      _tmp$3187,
      (moonbit_string_t)moonbit_string_literal_46.data
  );
  if (_tmp$3968.tag) {
    int32_t const _ok$3188 = _tmp$3968.data.ok;
  } else {
    void* const _err$3189 = _tmp$3968.data.err;
    struct moonbit_result_0 _result$3969;
    moonbit_decref(result3$1308);
    moonbit_decref(result2$1307);
    _result$3969.tag = 0;
    _result$3969.data.err = _err$3189;
    return _result$3969;
  }
  _tmp$3190 = 0;
  _tmp$3970
  = $moonbitlang$core$builtin$assert_eq$1(
    result2$1307,
      (moonbit_string_t)moonbit_string_literal_42.data,
      _tmp$3190,
      (moonbit_string_t)moonbit_string_literal_47.data
  );
  if (_tmp$3970.tag) {
    int32_t const _ok$3191 = _tmp$3970.data.ok;
  } else {
    void* const _err$3192 = _tmp$3970.data.err;
    struct moonbit_result_0 _result$3971;
    moonbit_decref(result3$1308);
    _result$3971.tag = 0;
    _result$3971.data.err = _err$3192;
    return _result$3971;
  }
  _tmp$3193 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           result3$1308,
             (moonbit_string_t)moonbit_string_literal_45.data,
             _tmp$3193,
             (moonbit_string_t)moonbit_string_literal_48.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4(
  
) {
  int64_t some_value$1293 = (int64_t)42;
  int32_t none_value$1294 = -1;
  moonbit_string_t some_string$1295 =
    (moonbit_string_t)moonbit_string_literal_49.data;
  int32_t none_string$1296 = -1;
  int64_t _tmp$3118 = (int64_t)42;
  moonbit_string_t _tmp$3119 = 0;
  struct moonbit_result_0 _tmp$3972 =
    $moonbitlang$core$builtin$assert_eq$2(
      some_value$1293,
        _tmp$3118,
        _tmp$3119,
        (moonbit_string_t)moonbit_string_literal_50.data
    );
  moonbit_string_t _tmp$3122;
  struct moonbit_result_0 _tmp$3974;
  moonbit_string_t _tmp$3125;
  moonbit_string_t _tmp$3126;
  struct moonbit_result_0 _tmp$3976;
  moonbit_string_t _tmp$3129;
  struct moonbit_result_0 _tmp$3978;
  int32_t _tmp$3132;
  moonbit_string_t _tmp$3133;
  struct moonbit_result_0 _tmp$3980;
  int32_t _tmp$3136;
  moonbit_string_t _tmp$3137;
  struct moonbit_result_0 _tmp$3982;
  int32_t _tmp$3140;
  moonbit_string_t _tmp$3141;
  struct moonbit_result_0 _tmp$3984;
  int32_t _tmp$3144;
  moonbit_string_t _tmp$3145;
  struct moonbit_result_0 _tmp$3986;
  int32_t divide$1297;
  int64_t result1$1300;
  int64_t result2$1301;
  int64_t _tmp$3150;
  moonbit_string_t _tmp$3151;
  struct moonbit_result_0 _tmp$3988;
  moonbit_string_t _tmp$3154;
  if (_tmp$3972.tag) {
    int32_t const _ok$3120 = _tmp$3972.data.ok;
  } else {
    void* const _err$3121 = _tmp$3972.data.err;
    struct moonbit_result_0 _result$3973;
    if (some_string$1295) {
      moonbit_decref(some_string$1295);
    }
    _result$3973.tag = 0;
    _result$3973.data.err = _err$3121;
    return _result$3973;
  }
  _tmp$3122 = 0;
  _tmp$3974
  = $moonbitlang$core$builtin$assert_eq$3(
    none_value$1294,
      -1,
      _tmp$3122,
      (moonbit_string_t)moonbit_string_literal_51.data
  );
  if (_tmp$3974.tag) {
    int32_t const _ok$3123 = _tmp$3974.data.ok;
  } else {
    void* const _err$3124 = _tmp$3974.data.err;
    struct moonbit_result_0 _result$3975;
    if (some_string$1295) {
      moonbit_decref(some_string$1295);
    }
    _result$3975.tag = 0;
    _result$3975.data.err = _err$3124;
    return _result$3975;
  }
  _tmp$3125 = (moonbit_string_t)moonbit_string_literal_49.data;
  _tmp$3126 = 0;
  _tmp$3976
  = $moonbitlang$core$builtin$assert_eq$5(
    some_string$1295,
      _tmp$3125,
      _tmp$3126,
      (moonbit_string_t)moonbit_string_literal_52.data
  );
  if (_tmp$3976.tag) {
    int32_t const _ok$3127 = _tmp$3976.data.ok;
  } else {
    void* const _err$3128 = _tmp$3976.data.err;
    struct moonbit_result_0 _result$3977;
    _result$3977.tag = 0;
    _result$3977.data.err = _err$3128;
    return _result$3977;
  }
  _tmp$3129 = 0;
  _tmp$3978
  = $moonbitlang$core$builtin$assert_eq$3(
    none_string$1296,
      -1,
      _tmp$3129,
      (moonbit_string_t)moonbit_string_literal_53.data
  );
  if (_tmp$3978.tag) {
    int32_t const _ok$3130 = _tmp$3978.data.ok;
  } else {
    void* const _err$3131 = _tmp$3978.data.err;
    struct moonbit_result_0 _result$3979;
    _result$3979.tag = 0;
    _result$3979.data.err = _err$3131;
    return _result$3979;
  }
  _tmp$3132 = $Option$$is_some$0(some_value$1293);
  _tmp$3133 = 0;
  _tmp$3980
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3132, _tmp$3133, (moonbit_string_t)moonbit_string_literal_54.data
  );
  if (_tmp$3980.tag) {
    int32_t const _ok$3134 = _tmp$3980.data.ok;
  } else {
    void* const _err$3135 = _tmp$3980.data.err;
    struct moonbit_result_0 _result$3981;
    _result$3981.tag = 0;
    _result$3981.data.err = _err$3135;
    return _result$3981;
  }
  _tmp$3136 = $Option$$is_none$0(some_value$1293);
  _tmp$3137 = 0;
  _tmp$3982
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3136, _tmp$3137, (moonbit_string_t)moonbit_string_literal_55.data
  );
  if (_tmp$3982.tag) {
    int32_t const _ok$3138 = _tmp$3982.data.ok;
  } else {
    void* const _err$3139 = _tmp$3982.data.err;
    struct moonbit_result_0 _result$3983;
    _result$3983.tag = 0;
    _result$3983.data.err = _err$3139;
    return _result$3983;
  }
  _tmp$3140 = $Option$$is_some$1(none_value$1294);
  _tmp$3141 = 0;
  _tmp$3984
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3140, _tmp$3141, (moonbit_string_t)moonbit_string_literal_56.data
  );
  if (_tmp$3984.tag) {
    int32_t const _ok$3142 = _tmp$3984.data.ok;
  } else {
    void* const _err$3143 = _tmp$3984.data.err;
    struct moonbit_result_0 _result$3985;
    _result$3985.tag = 0;
    _result$3985.data.err = _err$3143;
    return _result$3985;
  }
  _tmp$3144 = $Option$$is_none$1(none_value$1294);
  _tmp$3145 = 0;
  _tmp$3986
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3144, _tmp$3145, (moonbit_string_t)moonbit_string_literal_57.data
  );
  if (_tmp$3986.tag) {
    int32_t const _ok$3146 = _tmp$3986.data.ok;
  } else {
    void* const _err$3147 = _tmp$3986.data.err;
    struct moonbit_result_0 _result$3987;
    _result$3987.tag = 0;
    _result$3987.data.err = _err$3147;
    return _result$3987;
  }
  divide$1297 = 0;
  result1$1300
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$divide$fn$4(
    divide$1297, 10, 2
  );
  result2$1301
  = $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$divide$fn$4(
    divide$1297, 10, 0
  );
  _tmp$3150 = (int64_t)5;
  _tmp$3151 = 0;
  _tmp$3988
  = $moonbitlang$core$builtin$assert_eq$2(
    result1$1300,
      _tmp$3150,
      _tmp$3151,
      (moonbit_string_t)moonbit_string_literal_58.data
  );
  if (_tmp$3988.tag) {
    int32_t const _ok$3152 = _tmp$3988.data.ok;
  } else {
    void* const _err$3153 = _tmp$3988.data.err;
    struct moonbit_result_0 _result$3989;
    _result$3989.tag = 0;
    _result$3989.data.err = _err$3153;
    return _result$3989;
  }
  _tmp$3154 = 0;
  return $moonbitlang$core$builtin$assert_eq$2(
           result2$1301,
             4294967296ll,
             _tmp$3154,
             (moonbit_string_t)moonbit_string_literal_59.data
         );
}

int64_t $$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$divide$fn$4(
  int32_t _env$3148,
  int32_t a$1298,
  int32_t b$1299
) {
  if (b$1299 == 0) {
    return 4294967296ll;
  } else {
    int32_t _tmp$3149 = a$1298 / b$1299;
    return (int64_t)_tmp$3149;
  }
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3(
  
) {
  int32_t* _tmp$3117 = (int32_t*)moonbit_empty_int32_array;
  struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$* empty_array$1290 =
    (struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$)
    );
  int32_t* _tmp$3116;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* int_array$1291;
  moonbit_string_t* _tmp$3115;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* string_array$1292;
  int32_t _tmp$3081;
  moonbit_string_t _tmp$3082;
  struct moonbit_result_0 _tmp$3990;
  int32_t _tmp$3085;
  moonbit_string_t _tmp$3086;
  struct moonbit_result_0 _tmp$3992;
  int32_t _tmp$3089;
  moonbit_string_t _tmp$3090;
  struct moonbit_result_0 _tmp$3994;
  int32_t _tmp$3093;
  moonbit_string_t _tmp$3094;
  struct moonbit_result_0 _tmp$3996;
  int32_t _tmp$3097;
  moonbit_string_t _tmp$3098;
  struct moonbit_result_0 _tmp$3998;
  moonbit_string_t _tmp$3101;
  moonbit_string_t _tmp$3102;
  struct moonbit_result_0 _tmp$4000;
  int32_t _tmp$3105;
  moonbit_string_t _tmp$3106;
  struct moonbit_result_0 _tmp$4002;
  int32_t _tmp$3109;
  moonbit_string_t _tmp$3110;
  struct moonbit_result_0 _tmp$4004;
  int32_t _tmp$3113;
  moonbit_string_t _tmp$3114;
  Moonbit_object_header(empty_array$1290)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$, $0) >> 2,
      1,
      0
  );
  empty_array$1290->$0 = _tmp$3117;
  empty_array$1290->$1 = 0;
  _tmp$3116 = (int32_t*)moonbit_make_int32_array_raw(5);
  _tmp$3116[0] = 1;
  _tmp$3116[1] = 2;
  _tmp$3116[2] = 3;
  _tmp$3116[3] = 4;
  _tmp$3116[4] = 5;
  int_array$1291
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(int_array$1291)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  int_array$1291->$0 = _tmp$3116;
  int_array$1291->$1 = 5;
  _tmp$3115 = (moonbit_string_t*)moonbit_make_ref_array_raw(3);
  _tmp$3115[0] = (moonbit_string_t)moonbit_string_literal_60.data;
  _tmp$3115[1] = (moonbit_string_t)moonbit_string_literal_61.data;
  _tmp$3115[2] = (moonbit_string_t)moonbit_string_literal_62.data;
  string_array$1292
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(string_array$1292)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  string_array$1292->$0 = _tmp$3115;
  string_array$1292->$1 = 3;
  _tmp$3081 = $$moonbitlang$core$builtin$Array$$length$2(empty_array$1290);
  _tmp$3082 = 0;
  _tmp$3990
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3081, 0, _tmp$3082, (moonbit_string_t)moonbit_string_literal_63.data
  );
  if (_tmp$3990.tag) {
    int32_t const _ok$3083 = _tmp$3990.data.ok;
  } else {
    void* const _err$3084 = _tmp$3990.data.err;
    struct moonbit_result_0 _result$3991;
    moonbit_decref(string_array$1292);
    moonbit_decref(int_array$1291);
    _result$3991.tag = 0;
    _result$3991.data.err = _err$3084;
    return _result$3991;
  }
  moonbit_incref(int_array$1291);
  _tmp$3085 = $$moonbitlang$core$builtin$Array$$length$3(int_array$1291);
  _tmp$3086 = 0;
  _tmp$3992
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3085, 5, _tmp$3086, (moonbit_string_t)moonbit_string_literal_64.data
  );
  if (_tmp$3992.tag) {
    int32_t const _ok$3087 = _tmp$3992.data.ok;
  } else {
    void* const _err$3088 = _tmp$3992.data.err;
    struct moonbit_result_0 _result$3993;
    moonbit_decref(string_array$1292);
    moonbit_decref(int_array$1291);
    _result$3993.tag = 0;
    _result$3993.data.err = _err$3088;
    return _result$3993;
  }
  moonbit_incref(string_array$1292);
  _tmp$3089 = $$moonbitlang$core$builtin$Array$$length$1(string_array$1292);
  _tmp$3090 = 0;
  _tmp$3994
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3089, 3, _tmp$3090, (moonbit_string_t)moonbit_string_literal_65.data
  );
  if (_tmp$3994.tag) {
    int32_t const _ok$3091 = _tmp$3994.data.ok;
  } else {
    void* const _err$3092 = _tmp$3994.data.err;
    struct moonbit_result_0 _result$3995;
    moonbit_decref(string_array$1292);
    moonbit_decref(int_array$1291);
    _result$3995.tag = 0;
    _result$3995.data.err = _err$3092;
    return _result$3995;
  }
  moonbit_incref(int_array$1291);
  _tmp$3093 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1291, 0);
  _tmp$3094 = 0;
  _tmp$3996
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3093, 1, _tmp$3094, (moonbit_string_t)moonbit_string_literal_66.data
  );
  if (_tmp$3996.tag) {
    int32_t const _ok$3095 = _tmp$3996.data.ok;
  } else {
    void* const _err$3096 = _tmp$3996.data.err;
    struct moonbit_result_0 _result$3997;
    moonbit_decref(string_array$1292);
    moonbit_decref(int_array$1291);
    _result$3997.tag = 0;
    _result$3997.data.err = _err$3096;
    return _result$3997;
  }
  moonbit_incref(int_array$1291);
  _tmp$3097 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1291, 4);
  _tmp$3098 = 0;
  _tmp$3998
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3097, 5, _tmp$3098, (moonbit_string_t)moonbit_string_literal_67.data
  );
  if (_tmp$3998.tag) {
    int32_t const _ok$3099 = _tmp$3998.data.ok;
  } else {
    void* const _err$3100 = _tmp$3998.data.err;
    struct moonbit_result_0 _result$3999;
    moonbit_decref(string_array$1292);
    moonbit_decref(int_array$1291);
    _result$3999.tag = 0;
    _result$3999.data.err = _err$3100;
    return _result$3999;
  }
  moonbit_incref(string_array$1292);
  _tmp$3101 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1292, 1);
  _tmp$3102 = 0;
  _tmp$4000
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3101,
      (moonbit_string_t)moonbit_string_literal_61.data,
      _tmp$3102,
      (moonbit_string_t)moonbit_string_literal_68.data
  );
  if (_tmp$4000.tag) {
    int32_t const _ok$3103 = _tmp$4000.data.ok;
  } else {
    void* const _err$3104 = _tmp$4000.data.err;
    struct moonbit_result_0 _result$4001;
    moonbit_decref(string_array$1292);
    moonbit_decref(int_array$1291);
    _result$4001.tag = 0;
    _result$4001.data.err = _err$3104;
    return _result$4001;
  }
  moonbit_incref(int_array$1291);
  _tmp$3105 = $$moonbitlang$core$builtin$Array$$contains$0(int_array$1291, 3);
  _tmp$3106 = 0;
  _tmp$4002
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3105, _tmp$3106, (moonbit_string_t)moonbit_string_literal_69.data
  );
  if (_tmp$4002.tag) {
    int32_t const _ok$3107 = _tmp$4002.data.ok;
  } else {
    void* const _err$3108 = _tmp$4002.data.err;
    struct moonbit_result_0 _result$4003;
    moonbit_decref(string_array$1292);
    moonbit_decref(int_array$1291);
    _result$4003.tag = 0;
    _result$4003.data.err = _err$3108;
    return _result$4003;
  }
  _tmp$3109
  = $$moonbitlang$core$builtin$Array$$contains$0(
    int_array$1291, 10
  );
  _tmp$3110 = 0;
  _tmp$4004
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3109, _tmp$3110, (moonbit_string_t)moonbit_string_literal_70.data
  );
  if (_tmp$4004.tag) {
    int32_t const _ok$3111 = _tmp$4004.data.ok;
  } else {
    void* const _err$3112 = _tmp$4004.data.err;
    struct moonbit_result_0 _result$4005;
    moonbit_decref(string_array$1292);
    _result$4005.tag = 0;
    _result$4005.data.err = _err$3112;
    return _result$4005;
  }
  _tmp$3113
  = $$moonbitlang$core$builtin$Array$$contains$1(
    string_array$1292, (moonbit_string_t)moonbit_string_literal_62.data
  );
  _tmp$3114 = 0;
  return $moonbitlang$core$builtin$assert_true(
           _tmp$3113,
             _tmp$3114,
             (moonbit_string_t)moonbit_string_literal_71.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2(
  
) {
  int32_t int_value$1282 = 42;
  double float_value$1283 = 0x1.921f9f01b866ep+1;
  int32_t negative_int$1284 = -10;
  double negative_float$1285 = -0x1.4p+1;
  int32_t zero_int$1286 = 0;
  double zero_float$1287 = 0x0p+0;
  int32_t _tmp$3040 = int_value$1282 + zero_int$1286;
  moonbit_string_t _tmp$3041 = 0;
  struct moonbit_result_0 _tmp$4006 =
    $moonbitlang$core$builtin$assert_eq$0(
      _tmp$3040,
        42,
        _tmp$3041,
        (moonbit_string_t)moonbit_string_literal_72.data
    );
  double _tmp$3044;
  moonbit_string_t _tmp$3045;
  struct moonbit_result_0 _tmp$4008;
  int32_t _tmp$3048;
  moonbit_string_t _tmp$3049;
  struct moonbit_result_0 _tmp$4010;
  double _tmp$3056;
  double _tmp$3055;
  double _tmp$3054;
  int32_t _tmp$3052;
  moonbit_string_t _tmp$3053;
  struct moonbit_result_0 _tmp$4012;
  int32_t _tmp$3059;
  moonbit_string_t _tmp$3060;
  struct moonbit_result_0 _tmp$4014;
  int32_t _tmp$3063;
  moonbit_string_t _tmp$3064;
  struct moonbit_result_0 _tmp$4016;
  int32_t _tmp$3067;
  moonbit_string_t _tmp$3068;
  struct moonbit_result_0 _tmp$4018;
  int32_t _tmp$3071;
  moonbit_string_t _tmp$3072;
  struct moonbit_result_0 _tmp$4020;
  int32_t max_int$1288;
  int32_t min_int$1289;
  int32_t _tmp$3075;
  moonbit_string_t _tmp$3076;
  struct moonbit_result_0 _tmp$4022;
  int32_t _tmp$3079;
  moonbit_string_t _tmp$3080;
  if (_tmp$4006.tag) {
    int32_t const _ok$3042 = _tmp$4006.data.ok;
  } else {
    void* const _err$3043 = _tmp$4006.data.err;
    struct moonbit_result_0 _result$4007;
    _result$4007.tag = 0;
    _result$4007.data.err = _err$3043;
    return _result$4007;
  }
  _tmp$3044 = float_value$1283 + zero_float$1287;
  _tmp$3045 = 0;
  _tmp$4008
  = $moonbitlang$core$builtin$assert_eq$4(
    _tmp$3044,
      0x1.921f9f01b866ep+1,
      _tmp$3045,
      (moonbit_string_t)moonbit_string_literal_73.data
  );
  if (_tmp$4008.tag) {
    int32_t const _ok$3046 = _tmp$4008.data.ok;
  } else {
    void* const _err$3047 = _tmp$4008.data.err;
    struct moonbit_result_0 _result$4009;
    _result$4009.tag = 0;
    _result$4009.data.err = _err$3047;
    return _result$4009;
  }
  _tmp$3048 = negative_int$1284 + int_value$1282;
  _tmp$3049 = 0;
  _tmp$4010
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3048,
      32,
      _tmp$3049,
      (moonbit_string_t)moonbit_string_literal_74.data
  );
  if (_tmp$4010.tag) {
    int32_t const _ok$3050 = _tmp$4010.data.ok;
  } else {
    void* const _err$3051 = _tmp$4010.data.err;
    struct moonbit_result_0 _result$4011;
    _result$4011.tag = 0;
    _result$4011.data.err = _err$3051;
    return _result$4011;
  }
  _tmp$3056 = negative_float$1285 + float_value$1283;
  _tmp$3055 = _tmp$3056 - 0x1.487e7c06e19b9p-1;
  _tmp$3054 = fabs(_tmp$3055);
  _tmp$3052 = _tmp$3054 < 0x1.4f8b588e368f1p-17;
  _tmp$3053 = 0;
  _tmp$4012
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3052, _tmp$3053, (moonbit_string_t)moonbit_string_literal_75.data
  );
  if (_tmp$4012.tag) {
    int32_t const _ok$3057 = _tmp$4012.data.ok;
  } else {
    void* const _err$3058 = _tmp$4012.data.err;
    struct moonbit_result_0 _result$4013;
    _result$4013.tag = 0;
    _result$4013.data.err = _err$3058;
    return _result$4013;
  }
  _tmp$3059 = int_value$1282 > zero_int$1286;
  _tmp$3060 = 0;
  _tmp$4014
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3059, _tmp$3060, (moonbit_string_t)moonbit_string_literal_76.data
  );
  if (_tmp$4014.tag) {
    int32_t const _ok$3061 = _tmp$4014.data.ok;
  } else {
    void* const _err$3062 = _tmp$4014.data.err;
    struct moonbit_result_0 _result$4015;
    _result$4015.tag = 0;
    _result$4015.data.err = _err$3062;
    return _result$4015;
  }
  _tmp$3063 = negative_int$1284 < zero_int$1286;
  _tmp$3064 = 0;
  _tmp$4016
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3063, _tmp$3064, (moonbit_string_t)moonbit_string_literal_77.data
  );
  if (_tmp$4016.tag) {
    int32_t const _ok$3065 = _tmp$4016.data.ok;
  } else {
    void* const _err$3066 = _tmp$4016.data.err;
    struct moonbit_result_0 _result$4017;
    _result$4017.tag = 0;
    _result$4017.data.err = _err$3066;
    return _result$4017;
  }
  _tmp$3067 = float_value$1283 > zero_float$1287;
  _tmp$3068 = 0;
  _tmp$4018
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3067, _tmp$3068, (moonbit_string_t)moonbit_string_literal_78.data
  );
  if (_tmp$4018.tag) {
    int32_t const _ok$3069 = _tmp$4018.data.ok;
  } else {
    void* const _err$3070 = _tmp$4018.data.err;
    struct moonbit_result_0 _result$4019;
    _result$4019.tag = 0;
    _result$4019.data.err = _err$3070;
    return _result$4019;
  }
  _tmp$3071 = negative_float$1285 < zero_float$1287;
  _tmp$3072 = 0;
  _tmp$4020
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3071, _tmp$3072, (moonbit_string_t)moonbit_string_literal_79.data
  );
  if (_tmp$4020.tag) {
    int32_t const _ok$3073 = _tmp$4020.data.ok;
  } else {
    void* const _err$3074 = _tmp$4020.data.err;
    struct moonbit_result_0 _result$4021;
    _result$4021.tag = 0;
    _result$4021.data.err = _err$3074;
    return _result$4021;
  }
  max_int$1288 = 2147483647;
  min_int$1289 = (int32_t)0x80000000;
  _tmp$3075 = max_int$1288 > 0;
  _tmp$3076 = 0;
  _tmp$4022
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3075, _tmp$3076, (moonbit_string_t)moonbit_string_literal_80.data
  );
  if (_tmp$4022.tag) {
    int32_t const _ok$3077 = _tmp$4022.data.ok;
  } else {
    void* const _err$3078 = _tmp$4022.data.err;
    struct moonbit_result_0 _result$4023;
    _result$4023.tag = 0;
    _result$4023.data.err = _err$3078;
    return _result$4023;
  }
  _tmp$3079 = min_int$1289 < 0;
  _tmp$3080 = 0;
  return $moonbitlang$core$builtin$assert_true(
           _tmp$3079,
             _tmp$3080,
             (moonbit_string_t)moonbit_string_literal_81.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1(
  
) {
  moonbit_string_t empty_string$1274 =
    (moonbit_string_t)moonbit_string_literal_3.data;
  moonbit_string_t normal_string$1275 =
    (moonbit_string_t)moonbit_string_literal_82.data;
  moonbit_string_t unicode_string$1276 =
    (moonbit_string_t)moonbit_string_literal_83.data;
  moonbit_string_t emoji_string$1277 =
    (moonbit_string_t)moonbit_string_literal_84.data;
  int32_t _tmp$3528 = Moonbit_array_length(empty_string$1274);
  int32_t _tmp$3006;
  moonbit_string_t _tmp$3007;
  struct moonbit_result_0 _tmp$4024;
  int32_t _tmp$3010;
  moonbit_string_t _tmp$3011;
  struct moonbit_result_0 _tmp$4026;
  int32_t _tmp$3016;
  int32_t _tmp$3014;
  moonbit_string_t _tmp$3015;
  struct moonbit_result_0 _tmp$4028;
  int32_t _tmp$3527;
  int32_t _tmp$3021;
  int32_t _tmp$3019;
  moonbit_string_t _tmp$3020;
  struct moonbit_result_0 _tmp$4030;
  moonbit_string_t _tmp$3039;
  moonbit_string_t combined$1278;
  moonbit_string_t _bind$1279;
  int32_t _tmp$3027;
  struct $StringView _tmp$3026;
  int32_t _tmp$3024;
  moonbit_string_t _tmp$3025;
  struct moonbit_result_0 _tmp$4032;
  moonbit_string_t _bind$1280;
  int32_t _tmp$3033;
  struct $StringView _tmp$3032;
  int32_t _tmp$3030;
  moonbit_string_t _tmp$3031;
  struct moonbit_result_0 _tmp$4034;
  moonbit_string_t very_long_string$1281;
  int32_t _tmp$3526;
  int32_t _tmp$3038;
  int32_t _tmp$3036;
  moonbit_string_t _tmp$3037;
  moonbit_decref(empty_string$1274);
  _tmp$3006 = _tmp$3528;
  _tmp$3007 = 0;
  _tmp$4024
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3006, 0, _tmp$3007, (moonbit_string_t)moonbit_string_literal_85.data
  );
  if (_tmp$4024.tag) {
    int32_t const _ok$3008 = _tmp$4024.data.ok;
  } else {
    void* const _err$3009 = _tmp$4024.data.err;
    struct moonbit_result_0 _result$4025;
    moonbit_decref(emoji_string$1277);
    moonbit_decref(unicode_string$1276);
    moonbit_decref(normal_string$1275);
    _result$4025.tag = 0;
    _result$4025.data.err = _err$3009;
    return _result$4025;
  }
  _tmp$3010 = Moonbit_array_length(normal_string$1275);
  _tmp$3011 = 0;
  _tmp$4026
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3010,
      11,
      _tmp$3011,
      (moonbit_string_t)moonbit_string_literal_86.data
  );
  if (_tmp$4026.tag) {
    int32_t const _ok$3012 = _tmp$4026.data.ok;
  } else {
    void* const _err$3013 = _tmp$4026.data.err;
    struct moonbit_result_0 _result$4027;
    moonbit_decref(emoji_string$1277);
    moonbit_decref(unicode_string$1276);
    moonbit_decref(normal_string$1275);
    _result$4027.tag = 0;
    _result$4027.data.err = _err$3013;
    return _result$4027;
  }
  _tmp$3016 = Moonbit_array_length(unicode_string$1276);
  _tmp$3014 = _tmp$3016 > 0;
  _tmp$3015 = 0;
  _tmp$4028
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3014, _tmp$3015, (moonbit_string_t)moonbit_string_literal_87.data
  );
  if (_tmp$4028.tag) {
    int32_t const _ok$3017 = _tmp$4028.data.ok;
  } else {
    void* const _err$3018 = _tmp$4028.data.err;
    struct moonbit_result_0 _result$4029;
    moonbit_decref(emoji_string$1277);
    moonbit_decref(unicode_string$1276);
    moonbit_decref(normal_string$1275);
    _result$4029.tag = 0;
    _result$4029.data.err = _err$3018;
    return _result$4029;
  }
  _tmp$3527 = Moonbit_array_length(emoji_string$1277);
  moonbit_decref(emoji_string$1277);
  _tmp$3021 = _tmp$3527;
  _tmp$3019 = _tmp$3021 > 0;
  _tmp$3020 = 0;
  _tmp$4030
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3019, _tmp$3020, (moonbit_string_t)moonbit_string_literal_88.data
  );
  if (_tmp$4030.tag) {
    int32_t const _ok$3022 = _tmp$4030.data.ok;
  } else {
    void* const _err$3023 = _tmp$4030.data.err;
    struct moonbit_result_0 _result$4031;
    moonbit_decref(unicode_string$1276);
    moonbit_decref(normal_string$1275);
    _result$4031.tag = 0;
    _result$4031.data.err = _err$3023;
    return _result$4031;
  }
  _tmp$3039
  = moonbit_add_string(
    normal_string$1275, (moonbit_string_t)moonbit_string_literal_89.data
  );
  combined$1278 = moonbit_add_string(_tmp$3039, unicode_string$1276);
  _bind$1279 = (moonbit_string_t)moonbit_string_literal_49.data;
  _tmp$3027 = Moonbit_array_length(_bind$1279);
  _tmp$3026 = (struct $StringView){0, _tmp$3027, _bind$1279};
  moonbit_incref(combined$1278);
  _tmp$3024 = $String$$contains(combined$1278, _tmp$3026);
  _tmp$3025 = 0;
  _tmp$4032
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3024, _tmp$3025, (moonbit_string_t)moonbit_string_literal_90.data
  );
  if (_tmp$4032.tag) {
    int32_t const _ok$3028 = _tmp$4032.data.ok;
  } else {
    void* const _err$3029 = _tmp$4032.data.err;
    struct moonbit_result_0 _result$4033;
    moonbit_decref(combined$1278);
    _result$4033.tag = 0;
    _result$4033.data.err = _err$3029;
    return _result$4033;
  }
  _bind$1280 = (moonbit_string_t)moonbit_string_literal_91.data;
  _tmp$3033 = Moonbit_array_length(_bind$1280);
  _tmp$3032 = (struct $StringView){0, _tmp$3033, _bind$1280};
  _tmp$3030 = $String$$contains(combined$1278, _tmp$3032);
  _tmp$3031 = 0;
  _tmp$4034
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3030, _tmp$3031, (moonbit_string_t)moonbit_string_literal_92.data
  );
  if (_tmp$4034.tag) {
    int32_t const _ok$3034 = _tmp$4034.data.ok;
  } else {
    void* const _err$3035 = _tmp$4034.data.err;
    struct moonbit_result_0 _result$4035;
    _result$4035.tag = 0;
    _result$4035.data.err = _err$3035;
    return _result$4035;
  }
  very_long_string$1281 = (moonbit_string_t)moonbit_string_literal_93.data;
  _tmp$3526 = Moonbit_array_length(very_long_string$1281);
  moonbit_decref(very_long_string$1281);
  _tmp$3038 = _tmp$3526;
  _tmp$3036 = _tmp$3038 > 20;
  _tmp$3037 = 0;
  return $moonbitlang$core$builtin$assert_true(
           _tmp$3036,
             _tmp$3037,
             (moonbit_string_t)moonbit_string_literal_94.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0(
  
) {
  moonbit_string_t _tmp$2988 = 0;
  struct moonbit_result_0 _tmp$4036 =
    $moonbitlang$core$builtin$assert_true(
      1, _tmp$2988, (moonbit_string_t)moonbit_string_literal_95.data
    );
  moonbit_string_t _tmp$2991;
  struct moonbit_result_0 _tmp$4038;
  moonbit_string_t _tmp$2994;
  struct moonbit_result_0 _tmp$4040;
  moonbit_string_t _tmp$2997;
  struct moonbit_result_0 _tmp$4042;
  int64_t _tmp$3000;
  int64_t _tmp$3001;
  moonbit_string_t _tmp$3002;
  struct moonbit_result_0 _tmp$4044;
  moonbit_string_t _tmp$3005;
  if (_tmp$4036.tag) {
    int32_t const _ok$2989 = _tmp$4036.data.ok;
  } else {
    void* const _err$2990 = _tmp$4036.data.err;
    struct moonbit_result_0 _result$4037;
    _result$4037.tag = 0;
    _result$4037.data.err = _err$2990;
    return _result$4037;
  }
  _tmp$2991 = 0;
  _tmp$4038
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$2991, (moonbit_string_t)moonbit_string_literal_96.data
  );
  if (_tmp$4038.tag) {
    int32_t const _ok$2992 = _tmp$4038.data.ok;
  } else {
    void* const _err$2993 = _tmp$4038.data.err;
    struct moonbit_result_0 _result$4039;
    _result$4039.tag = 0;
    _result$4039.data.err = _err$2993;
    return _result$4039;
  }
  _tmp$2994 = 0;
  _tmp$4040
  = $moonbitlang$core$builtin$assert_eq$0(
    1, 1, _tmp$2994, (moonbit_string_t)moonbit_string_literal_97.data
  );
  if (_tmp$4040.tag) {
    int32_t const _ok$2995 = _tmp$4040.data.ok;
  } else {
    void* const _err$2996 = _tmp$4040.data.err;
    struct moonbit_result_0 _result$4041;
    _result$4041.tag = 0;
    _result$4041.data.err = _err$2996;
    return _result$4041;
  }
  _tmp$2997 = 0;
  _tmp$4042
  = $moonbitlang$core$builtin$assert_eq$1(
    (moonbit_string_t)moonbit_string_literal_49.data,
      (moonbit_string_t)moonbit_string_literal_49.data,
      _tmp$2997,
      (moonbit_string_t)moonbit_string_literal_98.data
  );
  if (_tmp$4042.tag) {
    int32_t const _ok$2998 = _tmp$4042.data.ok;
  } else {
    void* const _err$2999 = _tmp$4042.data.err;
    struct moonbit_result_0 _result$4043;
    _result$4043.tag = 0;
    _result$4043.data.err = _err$2999;
    return _result$4043;
  }
  _tmp$3000 = (int64_t)42;
  _tmp$3001 = (int64_t)42;
  _tmp$3002 = 0;
  _tmp$4044
  = $moonbitlang$core$builtin$assert_eq$2(
    _tmp$3000,
      _tmp$3001,
      _tmp$3002,
      (moonbit_string_t)moonbit_string_literal_99.data
  );
  if (_tmp$4044.tag) {
    int32_t const _ok$3003 = _tmp$4044.data.ok;
  } else {
    void* const _err$3004 = _tmp$4044.data.err;
    struct moonbit_result_0 _result$4045;
    _result$4045.tag = 0;
    _result$4045.data.err = _err$3004;
    return _result$4045;
  }
  _tmp$3005 = 0;
  return $moonbitlang$core$builtin$assert_eq$3(
           -1,
             -1,
             _tmp$3005,
             (moonbit_string_t)moonbit_string_literal_100.data
         );
}

int32_t $$moonbitlang$core$builtin$Show$$Double$$output(
  double self$1273,
  struct $$moonbitlang$core$builtin$Logger logger$1272
) {
  moonbit_string_t _tmp$2987 = $Double$$to_string(self$1273);
  logger$1272.$0->$method_0(logger$1272.$1, _tmp$2987);
  return 0;
}

moonbit_string_t $Double$$to_string(double self$1271) {
  return $moonbitlang$core$double$internal$ryu$ryu_to_string(self$1271);
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$ryu_to_string(
  double val$1258
) {
  uint64_t bits$1259;
  uint64_t _tmp$2986;
  uint64_t _tmp$2985;
  int32_t ieeeSign$1260;
  uint64_t ieeeMantissa$1261;
  uint64_t _tmp$2984;
  uint64_t _tmp$2983;
  int32_t ieeeExponent$1262;
  int32_t _if_result$4046;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* v$1263;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* small$1264;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$2982;
  if (val$1258 == 0x0p+0) {
    return (moonbit_string_t)moonbit_string_literal_101.data;
  }
  bits$1259 = *(int64_t*)&val$1258;
  _tmp$2986 = bits$1259 >> 63;
  _tmp$2985 = _tmp$2986 & 1ull;
  ieeeSign$1260 = _tmp$2985 != 0ull;
  ieeeMantissa$1261 = bits$1259 & 4503599627370495ull;
  _tmp$2984 = bits$1259 >> 52;
  _tmp$2983 = _tmp$2984 & 2047ull;
  ieeeExponent$1262 = (int32_t)_tmp$2983;
  if (ieeeExponent$1262 == 2047) {
    _if_result$4046 = 1;
  } else if (ieeeExponent$1262 == 0) {
    _if_result$4046 = ieeeMantissa$1261 == 0ull;
  } else {
    _if_result$4046 = 0;
  }
  if (_if_result$4046) {
    int32_t _tmp$2971 = ieeeExponent$1262 != 0;
    int32_t _tmp$2972 = ieeeMantissa$1261 != 0ull;
    return $moonbitlang$core$double$internal$ryu$copy_special_str(
             ieeeSign$1260, _tmp$2971, _tmp$2972
           );
  }
  v$1263 = $moonbitlang$core$double$internal$ryu$ryu_to_string$record$1257;
  small$1264
  = $moonbitlang$core$double$internal$ryu$d2d_small_int(
    ieeeMantissa$1261, ieeeExponent$1262
  );
  if (small$1264 == 0) {
    uint32_t _tmp$2973;
    if (small$1264) {
      moonbit_decref(small$1264);
    }
    _tmp$2973 = *(uint32_t*)&ieeeExponent$1262;
    v$1263
    = $moonbitlang$core$double$internal$ryu$d2d(
      ieeeMantissa$1261, _tmp$2973
    );
  } else {
    struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _Some$1265 =
      small$1264;
    struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _f$1266 =
      _Some$1265;
    struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* x$1267 =
      _f$1266;
    while (1) {
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$2981 =
        x$1267;
      uint64_t _field$3531 = _tmp$2981->$0;
      uint64_t mantissa$2980 = _field$3531;
      uint64_t q$1268 = mantissa$2980 / 10ull;
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$2979 =
        x$1267;
      uint64_t _field$3530 = _tmp$2979->$0;
      uint64_t mantissa$2977 = _field$3530;
      uint64_t _tmp$2978 = 10ull * q$1268;
      uint64_t r$1269 = mantissa$2977 - _tmp$2978;
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$2976;
      int32_t _field$3529;
      int32_t exponent$2975;
      int32_t _tmp$2974;
      if (r$1269 != 0ull) {
        break;
      }
      _tmp$2976 = x$1267;
      _field$3529 = _tmp$2976->$1;
      moonbit_decref(_tmp$2976);
      exponent$2975 = _field$3529;
      _tmp$2974 = exponent$2975 + 1;
      x$1267
      = (struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64*)moonbit_malloc(
          sizeof(
            struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
          )
        );
      Moonbit_object_header(x$1267)->meta
      = Moonbit_make_regular_object_header(
        sizeof(
          struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
        )
        >> 2,
          0,
          0
      );
      x$1267->$0 = q$1268;
      x$1267->$1 = _tmp$2974;
      continue;
      break;
    }
    v$1263 = x$1267;
  }
  _tmp$2982 = v$1263;
  return $moonbitlang$core$double$internal$ryu$to_chars(
           _tmp$2982, ieeeSign$1260
         );
}

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d_small_int(
  uint64_t ieeeMantissa$1252,
  int32_t ieeeExponent$1254
) {
  uint64_t m2$1251 = 4503599627370496ull | ieeeMantissa$1252;
  int32_t _tmp$2970 = ieeeExponent$1254 - 1023;
  int32_t e2$1253 = _tmp$2970 - 52;
  int32_t _tmp$2969;
  uint64_t _tmp$2968;
  uint64_t mask$1255;
  uint64_t fraction$1256;
  int32_t _tmp$2967;
  uint64_t _tmp$2966;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$2965;
  if (e2$1253 > 0) {
    return 0;
  }
  if (e2$1253 < -52) {
    return 0;
  }
  _tmp$2969 = -e2$1253;
  _tmp$2968 = 1ull << (_tmp$2969 & 63);
  mask$1255 = _tmp$2968 - 1ull;
  fraction$1256 = m2$1251 & mask$1255;
  if (fraction$1256 != 0ull) {
    return 0;
  }
  _tmp$2967 = -e2$1253;
  _tmp$2966 = m2$1251 >> (_tmp$2967 & 63);
  _tmp$2965
  = (struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64)
    );
  Moonbit_object_header(_tmp$2965)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
    )
    >> 2,
      0,
      0
  );
  _tmp$2965->$0 = _tmp$2966;
  _tmp$2965->$1 = 0;
  return _tmp$2965;
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$to_chars(
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* v$1225,
  int32_t sign$1223
) {
  int32_t _tmp$2964 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  moonbit_bytes_t result$1221 =
    (moonbit_bytes_t)moonbit_make_bytes(25, _tmp$2964);
  int32_t index$1222 = 0;
  uint64_t output$1224;
  uint64_t _tmp$2963;
  int32_t olength$1226;
  int32_t _field$3532;
  int32_t exponent$2962;
  int32_t _tmp$2961;
  int32_t exp$1227;
  int32_t _tmp$2960;
  int32_t _tmp$2958;
  int32_t scientificNotation$1228;
  if (sign$1223) {
    int32_t _tmp$2833 = index$1222;
    int32_t _tmp$2834;
    if (_tmp$2833 < 0 || _tmp$2833 >= Moonbit_array_length(result$1221)) {
      moonbit_panic();
    }
    result$1221[_tmp$2833] = 45;
    _tmp$2834 = index$1222;
    index$1222 = _tmp$2834 + 1;
  }
  output$1224 = v$1225->$0;
  _tmp$2963 = output$1224;
  olength$1226
  = $moonbitlang$core$double$internal$ryu$decimal_length17(
    _tmp$2963
  );
  _field$3532 = v$1225->$1;
  moonbit_decref(v$1225);
  exponent$2962 = _field$3532;
  _tmp$2961 = exponent$2962 + olength$1226;
  exp$1227 = _tmp$2961 - 1;
  _tmp$2960 = exp$1227;
  if (_tmp$2960 >= -6) {
    int32_t _tmp$2959 = exp$1227;
    _tmp$2958 = _tmp$2959 < 21;
  } else {
    _tmp$2958 = 0;
  }
  scientificNotation$1228 = !_tmp$2958;
  if (scientificNotation$1228) {
    int32_t _end41$1229 = olength$1226 - 1;
    int32_t i$1230 = 0;
    int32_t _tmp$2844;
    uint64_t _tmp$2849;
    int32_t _tmp$2848;
    int32_t _tmp$2847;
    int32_t _tmp$2846;
    int32_t _tmp$2845;
    int32_t _tmp$2853;
    int32_t _tmp$2854;
    int32_t _tmp$2855;
    int32_t _tmp$2856;
    int32_t _tmp$2857;
    int32_t _tmp$2863;
    int32_t _tmp$2896;
    while (1) {
      if (i$1230 < _end41$1229) {
        uint64_t _tmp$2842 = output$1224;
        uint64_t c$1231 = _tmp$2842 % 10ull;
        uint64_t _tmp$2835 = output$1224;
        int32_t _tmp$2841;
        int32_t _tmp$2840;
        int32_t _tmp$2836;
        int32_t _tmp$2839;
        int32_t _tmp$2838;
        int32_t _tmp$2837;
        int32_t _tmp$2843;
        output$1224 = _tmp$2835 / 10ull;
        _tmp$2841 = index$1222;
        _tmp$2840 = _tmp$2841 + olength$1226;
        _tmp$2836 = _tmp$2840 - i$1230;
        _tmp$2839 = (int32_t)c$1231;
        _tmp$2838 = 48 + _tmp$2839;
        _tmp$2837 = _tmp$2838 & 0xff;
        if (_tmp$2836 < 0 || _tmp$2836 >= Moonbit_array_length(result$1221)) {
          moonbit_panic();
        }
        result$1221[_tmp$2836] = _tmp$2837;
        _tmp$2843 = i$1230 + 1;
        i$1230 = _tmp$2843;
        continue;
      }
      break;
    }
    _tmp$2844 = index$1222;
    _tmp$2849 = output$1224;
    _tmp$2848 = (int32_t)_tmp$2849;
    _tmp$2847 = _tmp$2848 % 10;
    _tmp$2846 = 48 + _tmp$2847;
    _tmp$2845 = _tmp$2846 & 0xff;
    if (_tmp$2844 < 0 || _tmp$2844 >= Moonbit_array_length(result$1221)) {
      moonbit_panic();
    }
    result$1221[_tmp$2844] = _tmp$2845;
    if (olength$1226 > 1) {
      int32_t _tmp$2851 = index$1222;
      int32_t _tmp$2850 = _tmp$2851 + 1;
      if (_tmp$2850 < 0 || _tmp$2850 >= Moonbit_array_length(result$1221)) {
        moonbit_panic();
      }
      result$1221[_tmp$2850] = 46;
    } else {
      int32_t _tmp$2852 = index$1222;
      index$1222 = _tmp$2852 - 1;
    }
    _tmp$2853 = index$1222;
    _tmp$2854 = olength$1226 + 1;
    index$1222 = _tmp$2853 + _tmp$2854;
    _tmp$2855 = index$1222;
    if (_tmp$2855 < 0 || _tmp$2855 >= Moonbit_array_length(result$1221)) {
      moonbit_panic();
    }
    result$1221[_tmp$2855] = 101;
    _tmp$2856 = index$1222;
    index$1222 = _tmp$2856 + 1;
    _tmp$2857 = exp$1227;
    if (_tmp$2857 < 0) {
      int32_t _tmp$2858 = index$1222;
      int32_t _tmp$2859;
      int32_t _tmp$2860;
      if (_tmp$2858 < 0 || _tmp$2858 >= Moonbit_array_length(result$1221)) {
        moonbit_panic();
      }
      result$1221[_tmp$2858] = 45;
      _tmp$2859 = index$1222;
      index$1222 = _tmp$2859 + 1;
      _tmp$2860 = exp$1227;
      exp$1227 = -_tmp$2860;
    } else {
      int32_t _tmp$2861 = index$1222;
      int32_t _tmp$2862;
      if (_tmp$2861 < 0 || _tmp$2861 >= Moonbit_array_length(result$1221)) {
        moonbit_panic();
      }
      result$1221[_tmp$2861] = 43;
      _tmp$2862 = index$1222;
      index$1222 = _tmp$2862 + 1;
    }
    _tmp$2863 = exp$1227;
    if (_tmp$2863 >= 100) {
      int32_t _tmp$2879 = exp$1227;
      int32_t a$1233 = _tmp$2879 / 100;
      int32_t _tmp$2878 = exp$1227;
      int32_t _tmp$2877 = _tmp$2878 / 10;
      int32_t b$1234 = _tmp$2877 % 10;
      int32_t _tmp$2876 = exp$1227;
      int32_t c$1235 = _tmp$2876 % 10;
      int32_t _tmp$2864 = index$1222;
      int32_t _tmp$2866 = 48 + a$1233;
      int32_t _tmp$2865 = _tmp$2866 & 0xff;
      int32_t _tmp$2870;
      int32_t _tmp$2867;
      int32_t _tmp$2869;
      int32_t _tmp$2868;
      int32_t _tmp$2874;
      int32_t _tmp$2871;
      int32_t _tmp$2873;
      int32_t _tmp$2872;
      int32_t _tmp$2875;
      if (_tmp$2864 < 0 || _tmp$2864 >= Moonbit_array_length(result$1221)) {
        moonbit_panic();
      }
      result$1221[_tmp$2864] = _tmp$2865;
      _tmp$2870 = index$1222;
      _tmp$2867 = _tmp$2870 + 1;
      _tmp$2869 = 48 + b$1234;
      _tmp$2868 = _tmp$2869 & 0xff;
      if (_tmp$2867 < 0 || _tmp$2867 >= Moonbit_array_length(result$1221)) {
        moonbit_panic();
      }
      result$1221[_tmp$2867] = _tmp$2868;
      _tmp$2874 = index$1222;
      _tmp$2871 = _tmp$2874 + 2;
      _tmp$2873 = 48 + c$1235;
      _tmp$2872 = _tmp$2873 & 0xff;
      if (_tmp$2871 < 0 || _tmp$2871 >= Moonbit_array_length(result$1221)) {
        moonbit_panic();
      }
      result$1221[_tmp$2871] = _tmp$2872;
      _tmp$2875 = index$1222;
      index$1222 = _tmp$2875 + 3;
    } else {
      int32_t _tmp$2880 = exp$1227;
      if (_tmp$2880 >= 10) {
        int32_t _tmp$2890 = exp$1227;
        int32_t a$1236 = _tmp$2890 / 10;
        int32_t _tmp$2889 = exp$1227;
        int32_t b$1237 = _tmp$2889 % 10;
        int32_t _tmp$2881 = index$1222;
        int32_t _tmp$2883 = 48 + a$1236;
        int32_t _tmp$2882 = _tmp$2883 & 0xff;
        int32_t _tmp$2887;
        int32_t _tmp$2884;
        int32_t _tmp$2886;
        int32_t _tmp$2885;
        int32_t _tmp$2888;
        if (_tmp$2881 < 0 || _tmp$2881 >= Moonbit_array_length(result$1221)) {
          moonbit_panic();
        }
        result$1221[_tmp$2881] = _tmp$2882;
        _tmp$2887 = index$1222;
        _tmp$2884 = _tmp$2887 + 1;
        _tmp$2886 = 48 + b$1237;
        _tmp$2885 = _tmp$2886 & 0xff;
        if (_tmp$2884 < 0 || _tmp$2884 >= Moonbit_array_length(result$1221)) {
          moonbit_panic();
        }
        result$1221[_tmp$2884] = _tmp$2885;
        _tmp$2888 = index$1222;
        index$1222 = _tmp$2888 + 2;
      } else {
        int32_t _tmp$2891 = index$1222;
        int32_t _tmp$2894 = exp$1227;
        int32_t _tmp$2893 = 48 + _tmp$2894;
        int32_t _tmp$2892 = _tmp$2893 & 0xff;
        int32_t _tmp$2895;
        if (_tmp$2891 < 0 || _tmp$2891 >= Moonbit_array_length(result$1221)) {
          moonbit_panic();
        }
        result$1221[_tmp$2891] = _tmp$2892;
        _tmp$2895 = index$1222;
        index$1222 = _tmp$2895 + 1;
      }
    }
    _tmp$2896 = index$1222;
    return $moonbitlang$core$double$internal$ryu$string_from_bytes(
             result$1221, 0, _tmp$2896
           );
  } else {
    int32_t _tmp$2897 = exp$1227;
    int32_t _tmp$2957;
    if (_tmp$2897 < 0) {
      int32_t _tmp$2898 = index$1222;
      int32_t _tmp$2899;
      int32_t _tmp$2900;
      int32_t _tmp$2901;
      int32_t i$1238;
      int32_t current$1240;
      int32_t i$1241;
      if (_tmp$2898 < 0 || _tmp$2898 >= Moonbit_array_length(result$1221)) {
        moonbit_panic();
      }
      result$1221[_tmp$2898] = 48;
      _tmp$2899 = index$1222;
      index$1222 = _tmp$2899 + 1;
      _tmp$2900 = index$1222;
      if (_tmp$2900 < 0 || _tmp$2900 >= Moonbit_array_length(result$1221)) {
        moonbit_panic();
      }
      result$1221[_tmp$2900] = 46;
      _tmp$2901 = index$1222;
      index$1222 = _tmp$2901 + 1;
      i$1238 = -1;
      while (1) {
        int32_t _tmp$2902 = exp$1227;
        if (i$1238 > _tmp$2902) {
          int32_t _tmp$2903 = index$1222;
          int32_t _tmp$2904;
          int32_t _tmp$2905;
          if (
            _tmp$2903 < 0 || _tmp$2903 >= Moonbit_array_length(result$1221)
          ) {
            moonbit_panic();
          }
          result$1221[_tmp$2903] = 48;
          _tmp$2904 = index$1222;
          index$1222 = _tmp$2904 + 1;
          _tmp$2905 = i$1238 - 1;
          i$1238 = _tmp$2905;
          continue;
        }
        break;
      }
      current$1240 = index$1222;
      i$1241 = 0;
      while (1) {
        if (i$1241 < olength$1226) {
          int32_t _tmp$2913 = current$1240 + olength$1226;
          int32_t _tmp$2912 = _tmp$2913 - i$1241;
          int32_t _tmp$2906 = _tmp$2912 - 1;
          uint64_t _tmp$2911 = output$1224;
          uint64_t _tmp$2910 = _tmp$2911 % 10ull;
          int32_t _tmp$2909 = (int32_t)_tmp$2910;
          int32_t _tmp$2908 = 48 + _tmp$2909;
          int32_t _tmp$2907 = _tmp$2908 & 0xff;
          uint64_t _tmp$2914;
          int32_t _tmp$2915;
          int32_t _tmp$2916;
          if (
            _tmp$2906 < 0 || _tmp$2906 >= Moonbit_array_length(result$1221)
          ) {
            moonbit_panic();
          }
          result$1221[_tmp$2906] = _tmp$2907;
          _tmp$2914 = output$1224;
          output$1224 = _tmp$2914 / 10ull;
          _tmp$2915 = index$1222;
          index$1222 = _tmp$2915 + 1;
          _tmp$2916 = i$1241 + 1;
          i$1241 = _tmp$2916;
          continue;
        }
        break;
      }
    } else {
      int32_t _tmp$2918 = exp$1227;
      int32_t _tmp$2917 = _tmp$2918 + 1;
      if (_tmp$2917 >= olength$1226) {
        int32_t i$1243 = 0;
        int32_t _tmp$2930;
        int32_t _tmp$2934;
        int32_t _end64$1245;
        int32_t i$1246;
        while (1) {
          if (i$1243 < olength$1226) {
            int32_t _tmp$2927 = index$1222;
            int32_t _tmp$2926 = _tmp$2927 + olength$1226;
            int32_t _tmp$2925 = _tmp$2926 - i$1243;
            int32_t _tmp$2919 = _tmp$2925 - 1;
            uint64_t _tmp$2924 = output$1224;
            uint64_t _tmp$2923 = _tmp$2924 % 10ull;
            int32_t _tmp$2922 = (int32_t)_tmp$2923;
            int32_t _tmp$2921 = 48 + _tmp$2922;
            int32_t _tmp$2920 = _tmp$2921 & 0xff;
            uint64_t _tmp$2928;
            int32_t _tmp$2929;
            if (
              _tmp$2919 < 0 || _tmp$2919 >= Moonbit_array_length(result$1221)
            ) {
              moonbit_panic();
            }
            result$1221[_tmp$2919] = _tmp$2920;
            _tmp$2928 = output$1224;
            output$1224 = _tmp$2928 / 10ull;
            _tmp$2929 = i$1243 + 1;
            i$1243 = _tmp$2929;
            continue;
          }
          break;
        }
        _tmp$2930 = index$1222;
        index$1222 = _tmp$2930 + olength$1226;
        _tmp$2934 = exp$1227;
        _end64$1245 = _tmp$2934 + 1;
        i$1246 = olength$1226;
        while (1) {
          if (i$1246 < _end64$1245) {
            int32_t _tmp$2931 = index$1222;
            int32_t _tmp$2932;
            int32_t _tmp$2933;
            if (
              _tmp$2931 < 0 || _tmp$2931 >= Moonbit_array_length(result$1221)
            ) {
              moonbit_panic();
            }
            result$1221[_tmp$2931] = 48;
            _tmp$2932 = index$1222;
            index$1222 = _tmp$2932 + 1;
            _tmp$2933 = i$1246 + 1;
            i$1246 = _tmp$2933;
            continue;
          }
          break;
        }
      } else {
        int32_t _tmp$2956 = index$1222;
        int32_t current$1248 = _tmp$2956 + 1;
        int32_t i$1249 = 0;
        int32_t _tmp$2954;
        int32_t _tmp$2955;
        while (1) {
          if (i$1249 < olength$1226) {
            int32_t _tmp$2937 = olength$1226 - i$1249;
            int32_t _tmp$2935 = _tmp$2937 - 1;
            int32_t _tmp$2936 = exp$1227;
            int32_t _tmp$2951;
            int32_t _tmp$2950;
            int32_t _tmp$2949;
            int32_t _tmp$2943;
            uint64_t _tmp$2948;
            uint64_t _tmp$2947;
            int32_t _tmp$2946;
            int32_t _tmp$2945;
            int32_t _tmp$2944;
            uint64_t _tmp$2952;
            int32_t _tmp$2953;
            if (_tmp$2935 == _tmp$2936) {
              int32_t _tmp$2941 = current$1248;
              int32_t _tmp$2940 = _tmp$2941 + olength$1226;
              int32_t _tmp$2939 = _tmp$2940 - i$1249;
              int32_t _tmp$2938 = _tmp$2939 - 1;
              int32_t _tmp$2942;
              if (
                _tmp$2938 < 0
                || _tmp$2938 >= Moonbit_array_length(result$1221)
              ) {
                moonbit_panic();
              }
              result$1221[_tmp$2938] = 46;
              _tmp$2942 = current$1248;
              current$1248 = _tmp$2942 - 1;
            }
            _tmp$2951 = current$1248;
            _tmp$2950 = _tmp$2951 + olength$1226;
            _tmp$2949 = _tmp$2950 - i$1249;
            _tmp$2943 = _tmp$2949 - 1;
            _tmp$2948 = output$1224;
            _tmp$2947 = _tmp$2948 % 10ull;
            _tmp$2946 = (int32_t)_tmp$2947;
            _tmp$2945 = 48 + _tmp$2946;
            _tmp$2944 = _tmp$2945 & 0xff;
            if (
              _tmp$2943 < 0 || _tmp$2943 >= Moonbit_array_length(result$1221)
            ) {
              moonbit_panic();
            }
            result$1221[_tmp$2943] = _tmp$2944;
            _tmp$2952 = output$1224;
            output$1224 = _tmp$2952 / 10ull;
            _tmp$2953 = i$1249 + 1;
            i$1249 = _tmp$2953;
            continue;
          }
          break;
        }
        _tmp$2954 = index$1222;
        _tmp$2955 = olength$1226 + 1;
        index$1222 = _tmp$2954 + _tmp$2955;
      }
    }
    _tmp$2957 = index$1222;
    return $moonbitlang$core$double$internal$ryu$string_from_bytes(
             result$1221, 0, _tmp$2957
           );
  }
}

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d(
  uint64_t ieeeMantissa$1167,
  uint32_t ieeeExponent$1166
) {
  int32_t e2$1164 = 0;
  uint64_t m2$1165 = 0ull;
  uint64_t _tmp$2832;
  uint64_t _tmp$2831;
  int32_t even$1168;
  uint64_t _tmp$2830;
  uint64_t mv$1169;
  int32_t mmShift$1170;
  uint64_t vr$1171;
  uint64_t vp$1172;
  uint64_t vm$1173;
  int32_t e10$1174;
  int32_t vmIsTrailingZeros$1175;
  int32_t vrIsTrailingZeros$1176;
  int32_t _tmp$2732;
  int32_t removed$1195;
  int32_t lastRemovedDigit$1196;
  uint64_t output$1197;
  int32_t _tmp$2828;
  int32_t _tmp$2829;
  int32_t exp$1220;
  uint64_t _tmp$2827;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _block$4059;
  if (ieeeExponent$1166 == 0u) {
    e2$1164 = -1076;
    m2$1165 = ieeeMantissa$1167;
  } else {
    int32_t _tmp$2731 = *(int32_t*)&ieeeExponent$1166;
    int32_t _tmp$2730 = _tmp$2731 - 1023;
    int32_t _tmp$2729 = _tmp$2730 - 52;
    e2$1164 = _tmp$2729 - 2;
    m2$1165 = 4503599627370496ull | ieeeMantissa$1167;
  }
  _tmp$2832 = m2$1165;
  _tmp$2831 = _tmp$2832 & 1ull;
  even$1168 = _tmp$2831 == 0ull;
  _tmp$2830 = m2$1165;
  mv$1169 = 4ull * _tmp$2830;
  if (ieeeMantissa$1167 != 0ull) {
    mmShift$1170 = 1;
  } else {
    mmShift$1170 = ieeeExponent$1166 <= 1u;
  }
  vr$1171 = 0ull;
  vp$1172 = 0ull;
  vm$1173 = 0ull;
  e10$1174 = 0;
  vmIsTrailingZeros$1175 = 0;
  vrIsTrailingZeros$1176 = 0;
  _tmp$2732 = e2$1164;
  if (_tmp$2732 >= 0) {
    int32_t _tmp$2754 = e2$1164;
    int32_t _tmp$2750 =
      $moonbitlang$core$double$internal$ryu$log10Pow2(_tmp$2754);
    int32_t _tmp$2753 = e2$1164;
    int32_t _tmp$2752 = _tmp$2753 > 3;
    int32_t _tmp$2751 = $Bool$$to_int(_tmp$2752);
    int32_t q$1177 = _tmp$2750 - _tmp$2751;
    int32_t _tmp$2749;
    int32_t _tmp$2748;
    int32_t k$1178;
    int32_t _tmp$2747;
    int32_t _tmp$2746;
    int32_t _tmp$2745;
    int32_t i$1179;
    struct $$moonbitlang$core$double$internal$ryu$Pow5Pair pow5$1180;
    uint64_t _tmp$2744;
    struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result _bind$1181;
    uint64_t _vrOut$1182;
    uint64_t _vpOut$1183;
    uint64_t _vmOut$1184;
    e10$1174 = q$1177;
    _tmp$2749 = $moonbitlang$core$double$internal$ryu$pow5bits(q$1177);
    _tmp$2748 = 125 + _tmp$2749;
    k$1178 = _tmp$2748 - 1;
    _tmp$2747 = e2$1164;
    _tmp$2746 = -_tmp$2747;
    _tmp$2745 = _tmp$2746 + q$1177;
    i$1179 = _tmp$2745 + k$1178;
    pow5$1180
    = $moonbitlang$core$double$internal$ryu$double_computeInvPow5(
      q$1177
    );
    _tmp$2744 = m2$1165;
    _bind$1181
    = $moonbitlang$core$double$internal$ryu$mulShiftAll64(
      _tmp$2744, pow5$1180, i$1179, mmShift$1170
    );
    _vrOut$1182 = _bind$1181.$0;
    _vpOut$1183 = _bind$1181.$1;
    _vmOut$1184 = _bind$1181.$2;
    vr$1171 = _vrOut$1182;
    vp$1172 = _vpOut$1183;
    vm$1173 = _vmOut$1184;
    if (q$1177 <= 21) {
      int32_t _tmp$2740 = (int32_t)mv$1169;
      uint64_t _tmp$2743 = mv$1169 / 5ull;
      int32_t _tmp$2742 = (int32_t)_tmp$2743;
      int32_t _tmp$2741 = 5 * _tmp$2742;
      int32_t mvMod5$1185 = _tmp$2740 - _tmp$2741;
      if (mvMod5$1185 == 0) {
        vrIsTrailingZeros$1176
        = $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
          mv$1169, q$1177
        );
      } else if (even$1168) {
        uint64_t _tmp$2734 = mv$1169 - 1ull;
        uint64_t _tmp$2735 = $Bool$$to_uint64(mmShift$1170);
        uint64_t _tmp$2733 = _tmp$2734 - _tmp$2735;
        vmIsTrailingZeros$1175
        = $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
          _tmp$2733, q$1177
        );
      } else {
        uint64_t _tmp$2736 = vp$1172;
        uint64_t _tmp$2739 = mv$1169 + 2ull;
        int32_t _tmp$2738 =
          $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
            _tmp$2739, q$1177
          );
        uint64_t _tmp$2737 = $Bool$$to_uint64(_tmp$2738);
        vp$1172 = _tmp$2736 - _tmp$2737;
      }
    }
  } else {
    int32_t _tmp$2768 = e2$1164;
    int32_t _tmp$2767 = -_tmp$2768;
    int32_t _tmp$2762 =
      $moonbitlang$core$double$internal$ryu$log10Pow5(_tmp$2767);
    int32_t _tmp$2766 = e2$1164;
    int32_t _tmp$2765 = -_tmp$2766;
    int32_t _tmp$2764 = _tmp$2765 > 1;
    int32_t _tmp$2763 = $Bool$$to_int(_tmp$2764);
    int32_t q$1186 = _tmp$2762 - _tmp$2763;
    int32_t _tmp$2755 = e2$1164;
    int32_t _tmp$2761;
    int32_t _tmp$2760;
    int32_t i$1187;
    int32_t _tmp$2759;
    int32_t k$1188;
    int32_t j$1189;
    struct $$moonbitlang$core$double$internal$ryu$Pow5Pair pow5$1190;
    uint64_t _tmp$2758;
    struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result _bind$1191;
    uint64_t _vrOut$1192;
    uint64_t _vpOut$1193;
    uint64_t _vmOut$1194;
    e10$1174 = q$1186 + _tmp$2755;
    _tmp$2761 = e2$1164;
    _tmp$2760 = -_tmp$2761;
    i$1187 = _tmp$2760 - q$1186;
    _tmp$2759 = $moonbitlang$core$double$internal$ryu$pow5bits(i$1187);
    k$1188 = _tmp$2759 - 125;
    j$1189 = q$1186 - k$1188;
    pow5$1190
    = $moonbitlang$core$double$internal$ryu$double_computePow5(
      i$1187
    );
    _tmp$2758 = m2$1165;
    _bind$1191
    = $moonbitlang$core$double$internal$ryu$mulShiftAll64(
      _tmp$2758, pow5$1190, j$1189, mmShift$1170
    );
    _vrOut$1192 = _bind$1191.$0;
    _vpOut$1193 = _bind$1191.$1;
    _vmOut$1194 = _bind$1191.$2;
    vr$1171 = _vrOut$1192;
    vp$1172 = _vpOut$1193;
    vm$1173 = _vmOut$1194;
    if (q$1186 <= 1) {
      vrIsTrailingZeros$1176 = 1;
      if (even$1168) {
        int32_t _tmp$2756 = $Bool$$to_int(mmShift$1170);
        vmIsTrailingZeros$1175 = _tmp$2756 == 1;
      } else {
        uint64_t _tmp$2757 = vp$1172;
        vp$1172 = _tmp$2757 - 1ull;
      }
    } else if (q$1186 < 63) {
      vrIsTrailingZeros$1176
      = $moonbitlang$core$double$internal$ryu$multipleOfPowerOf2(
        mv$1169, q$1186
      );
    }
  }
  removed$1195 = 0;
  lastRemovedDigit$1196 = 0;
  output$1197 = 0ull;
  if (vmIsTrailingZeros$1175 || vrIsTrailingZeros$1176) {
    int32_t _if_result$4056;
    uint64_t _tmp$2798;
    uint64_t _tmp$2804;
    uint64_t _tmp$2805;
    int32_t _if_result$4057;
    int32_t _tmp$2801;
    int64_t _tmp$2800;
    uint64_t _tmp$2799;
    while (1) {
      uint64_t _tmp$2781 = vp$1172;
      uint64_t vpDiv10$1198 = _tmp$2781 / 10ull;
      uint64_t _tmp$2780 = vm$1173;
      uint64_t vmDiv10$1199 = _tmp$2780 / 10ull;
      uint64_t _tmp$2779;
      int32_t _tmp$2776;
      int32_t _tmp$2778;
      int32_t _tmp$2777;
      int32_t vmMod10$1201;
      uint64_t _tmp$2775;
      uint64_t vrDiv10$1202;
      uint64_t _tmp$2774;
      int32_t _tmp$2771;
      int32_t _tmp$2773;
      int32_t _tmp$2772;
      int32_t vrMod10$1203;
      int32_t _tmp$2770;
      if (vpDiv10$1198 <= vmDiv10$1199) {
        break;
      }
      _tmp$2779 = vm$1173;
      _tmp$2776 = (int32_t)_tmp$2779;
      _tmp$2778 = (int32_t)vmDiv10$1199;
      _tmp$2777 = 10 * _tmp$2778;
      vmMod10$1201 = _tmp$2776 - _tmp$2777;
      _tmp$2775 = vr$1171;
      vrDiv10$1202 = _tmp$2775 / 10ull;
      _tmp$2774 = vr$1171;
      _tmp$2771 = (int32_t)_tmp$2774;
      _tmp$2773 = (int32_t)vrDiv10$1202;
      _tmp$2772 = 10 * _tmp$2773;
      vrMod10$1203 = _tmp$2771 - _tmp$2772;
      if (vmIsTrailingZeros$1175) {
        vmIsTrailingZeros$1175 = vmMod10$1201 == 0;
      } else {
        vmIsTrailingZeros$1175 = 0;
      }
      if (vrIsTrailingZeros$1176) {
        int32_t _tmp$2769 = lastRemovedDigit$1196;
        vrIsTrailingZeros$1176 = _tmp$2769 == 0;
      } else {
        vrIsTrailingZeros$1176 = 0;
      }
      lastRemovedDigit$1196 = vrMod10$1203;
      vr$1171 = vrDiv10$1202;
      vp$1172 = vpDiv10$1198;
      vm$1173 = vmDiv10$1199;
      _tmp$2770 = removed$1195;
      removed$1195 = _tmp$2770 + 1;
      continue;
      break;
    }
    if (vmIsTrailingZeros$1175) {
      while (1) {
        uint64_t _tmp$2794 = vm$1173;
        uint64_t vmDiv10$1204 = _tmp$2794 / 10ull;
        uint64_t _tmp$2793 = vm$1173;
        int32_t _tmp$2790 = (int32_t)_tmp$2793;
        int32_t _tmp$2792 = (int32_t)vmDiv10$1204;
        int32_t _tmp$2791 = 10 * _tmp$2792;
        int32_t vmMod10$1205 = _tmp$2790 - _tmp$2791;
        uint64_t _tmp$2789;
        uint64_t vpDiv10$1207;
        uint64_t _tmp$2788;
        uint64_t vrDiv10$1208;
        uint64_t _tmp$2787;
        int32_t _tmp$2784;
        int32_t _tmp$2786;
        int32_t _tmp$2785;
        int32_t vrMod10$1209;
        int32_t _tmp$2783;
        if (vmMod10$1205 != 0) {
          break;
        }
        _tmp$2789 = vp$1172;
        vpDiv10$1207 = _tmp$2789 / 10ull;
        _tmp$2788 = vr$1171;
        vrDiv10$1208 = _tmp$2788 / 10ull;
        _tmp$2787 = vr$1171;
        _tmp$2784 = (int32_t)_tmp$2787;
        _tmp$2786 = (int32_t)vrDiv10$1208;
        _tmp$2785 = 10 * _tmp$2786;
        vrMod10$1209 = _tmp$2784 - _tmp$2785;
        if (vrIsTrailingZeros$1176) {
          int32_t _tmp$2782 = lastRemovedDigit$1196;
          vrIsTrailingZeros$1176 = _tmp$2782 == 0;
        } else {
          vrIsTrailingZeros$1176 = 0;
        }
        lastRemovedDigit$1196 = vrMod10$1209;
        vr$1171 = vrDiv10$1208;
        vp$1172 = vpDiv10$1207;
        vm$1173 = vmDiv10$1204;
        _tmp$2783 = removed$1195;
        removed$1195 = _tmp$2783 + 1;
        continue;
        break;
      }
    }
    if (vrIsTrailingZeros$1176) {
      int32_t _tmp$2797 = lastRemovedDigit$1196;
      if (_tmp$2797 == 5) {
        uint64_t _tmp$2796 = vr$1171;
        uint64_t _tmp$2795 = _tmp$2796 % 2ull;
        _if_result$4056 = _tmp$2795 == 0ull;
      } else {
        _if_result$4056 = 0;
      }
    } else {
      _if_result$4056 = 0;
    }
    if (_if_result$4056) {
      lastRemovedDigit$1196 = 4;
    }
    _tmp$2798 = vr$1171;
    _tmp$2804 = vr$1171;
    _tmp$2805 = vm$1173;
    if (_tmp$2804 == _tmp$2805) {
      if (!even$1168) {
        _if_result$4057 = 1;
      } else {
        int32_t _tmp$2803 = vmIsTrailingZeros$1175;
        _if_result$4057 = !_tmp$2803;
      }
    } else {
      _if_result$4057 = 0;
    }
    if (_if_result$4057) {
      _tmp$2801 = 1;
    } else {
      int32_t _tmp$2802 = lastRemovedDigit$1196;
      _tmp$2801 = _tmp$2802 >= 5;
    }
    _tmp$2800 = $Bool$$to_int64(_tmp$2801);
    _tmp$2799 = *(uint64_t*)&_tmp$2800;
    output$1197 = _tmp$2798 + _tmp$2799;
  } else {
    int32_t roundUp$1210 = 0;
    uint64_t _tmp$2826 = vp$1172;
    uint64_t vpDiv100$1211 = _tmp$2826 / 100ull;
    uint64_t _tmp$2825 = vm$1173;
    uint64_t vmDiv100$1212 = _tmp$2825 / 100ull;
    uint64_t _tmp$2820;
    uint64_t _tmp$2823;
    uint64_t _tmp$2824;
    int32_t _tmp$2822;
    uint64_t _tmp$2821;
    if (vpDiv100$1211 > vmDiv100$1212) {
      uint64_t _tmp$2811 = vr$1171;
      uint64_t vrDiv100$1213 = _tmp$2811 / 100ull;
      uint64_t _tmp$2810 = vr$1171;
      int32_t _tmp$2807 = (int32_t)_tmp$2810;
      int32_t _tmp$2809 = (int32_t)vrDiv100$1213;
      int32_t _tmp$2808 = 100 * _tmp$2809;
      int32_t vrMod100$1214 = _tmp$2807 - _tmp$2808;
      int32_t _tmp$2806;
      roundUp$1210 = vrMod100$1214 >= 50;
      vr$1171 = vrDiv100$1213;
      vp$1172 = vpDiv100$1211;
      vm$1173 = vmDiv100$1212;
      _tmp$2806 = removed$1195;
      removed$1195 = _tmp$2806 + 2;
    }
    while (1) {
      uint64_t _tmp$2819 = vp$1172;
      uint64_t vpDiv10$1215 = _tmp$2819 / 10ull;
      uint64_t _tmp$2818 = vm$1173;
      uint64_t vmDiv10$1216 = _tmp$2818 / 10ull;
      uint64_t _tmp$2817;
      uint64_t vrDiv10$1218;
      uint64_t _tmp$2816;
      int32_t _tmp$2813;
      int32_t _tmp$2815;
      int32_t _tmp$2814;
      int32_t vrMod10$1219;
      int32_t _tmp$2812;
      if (vpDiv10$1215 <= vmDiv10$1216) {
        break;
      }
      _tmp$2817 = vr$1171;
      vrDiv10$1218 = _tmp$2817 / 10ull;
      _tmp$2816 = vr$1171;
      _tmp$2813 = (int32_t)_tmp$2816;
      _tmp$2815 = (int32_t)vrDiv10$1218;
      _tmp$2814 = 10 * _tmp$2815;
      vrMod10$1219 = _tmp$2813 - _tmp$2814;
      roundUp$1210 = vrMod10$1219 >= 5;
      vr$1171 = vrDiv10$1218;
      vp$1172 = vpDiv10$1215;
      vm$1173 = vmDiv10$1216;
      _tmp$2812 = removed$1195;
      removed$1195 = _tmp$2812 + 1;
      continue;
      break;
    }
    _tmp$2820 = vr$1171;
    _tmp$2823 = vr$1171;
    _tmp$2824 = vm$1173;
    _tmp$2822 = _tmp$2823 == _tmp$2824 || roundUp$1210;
    _tmp$2821 = $Bool$$to_uint64(_tmp$2822);
    output$1197 = _tmp$2820 + _tmp$2821;
  }
  _tmp$2828 = e10$1174;
  _tmp$2829 = removed$1195;
  exp$1220 = _tmp$2828 + _tmp$2829;
  _tmp$2827 = output$1197;
  _block$4059
  = (struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64)
    );
  Moonbit_object_header(_block$4059)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
    )
    >> 2,
      0,
      0
  );
  _block$4059->$0 = _tmp$2827;
  _block$4059->$1 = exp$1220;
  return _block$4059;
}

int32_t $moonbitlang$core$double$internal$ryu$decimal_length17(
  uint64_t v$1163
) {
  if (v$1163 >= 10000000000000000ull) {
    return 17;
  }
  if (v$1163 >= 1000000000000000ull) {
    return 16;
  }
  if (v$1163 >= 100000000000000ull) {
    return 15;
  }
  if (v$1163 >= 10000000000000ull) {
    return 14;
  }
  if (v$1163 >= 1000000000000ull) {
    return 13;
  }
  if (v$1163 >= 100000000000ull) {
    return 12;
  }
  if (v$1163 >= 10000000000ull) {
    return 11;
  }
  if (v$1163 >= 1000000000ull) {
    return 10;
  }
  if (v$1163 >= 100000000ull) {
    return 9;
  }
  if (v$1163 >= 10000000ull) {
    return 8;
  }
  if (v$1163 >= 1000000ull) {
    return 7;
  }
  if (v$1163 >= 100000ull) {
    return 6;
  }
  if (v$1163 >= 10000ull) {
    return 5;
  }
  if (v$1163 >= 1000ull) {
    return 4;
  }
  if (v$1163 >= 100ull) {
    return 3;
  }
  if (v$1163 >= 10ull) {
    return 2;
  }
  return 1;
}

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computeInvPow5(
  int32_t i$1146
) {
  int32_t _tmp$2728 = i$1146 + 26;
  int32_t _tmp$2727 = _tmp$2728 - 1;
  int32_t base$1145 = _tmp$2727 / 26;
  int32_t base2$1147 = base$1145 * 26;
  int32_t offset$1148 = base2$1147 - i$1146;
  int32_t _tmp$2726 = base$1145 * 2;
  uint64_t mul0$1149;
  int32_t _tmp$2725;
  int32_t _tmp$2724;
  uint64_t mul1$1150;
  uint64_t m$1151;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1152;
  uint64_t _low1$1153;
  uint64_t _high1$1154;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1155;
  uint64_t _low0$1156;
  uint64_t _high0$1157;
  uint64_t sum$1158;
  uint64_t high1$1159;
  int32_t _tmp$2722;
  int32_t _tmp$2723;
  int32_t delta$1160;
  uint64_t _tmp$2721;
  uint64_t _tmp$2713;
  int32_t _tmp$2720;
  uint32_t _tmp$2717;
  int32_t _tmp$2719;
  int32_t _tmp$2718;
  uint32_t _tmp$2716;
  uint32_t _tmp$2715;
  uint64_t _tmp$2714;
  uint64_t a$1161;
  uint64_t _tmp$2712;
  uint64_t b$1162;
  moonbit_incref(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2
  );
  mul0$1149
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2, _tmp$2726
  );
  _tmp$2725 = base$1145 * 2;
  _tmp$2724 = _tmp$2725 + 1;
  moonbit_incref(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2
  );
  mul1$1150
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2, _tmp$2724
  );
  if (offset$1148 == 0) {
    return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
             mul0$1149, mul1$1150
           };
  }
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE);
  m$1151
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE, offset$1148
  );
  _bind$1152
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1151, mul1$1150
  );
  _low1$1153 = _bind$1152.$0;
  _high1$1154 = _bind$1152.$1;
  _bind$1155
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1151, mul0$1149
  );
  _low0$1156 = _bind$1155.$0;
  _high0$1157 = _bind$1155.$1;
  sum$1158 = _high0$1157 + _low1$1153;
  high1$1159 = _high1$1154;
  if (sum$1158 < _high0$1157) {
    uint64_t _tmp$2711 = high1$1159;
    high1$1159 = _tmp$2711 + 1ull;
  }
  _tmp$2722 = $moonbitlang$core$double$internal$ryu$pow5bits(base2$1147);
  _tmp$2723 = $moonbitlang$core$double$internal$ryu$pow5bits(i$1146);
  delta$1160 = _tmp$2722 - _tmp$2723;
  _tmp$2721
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    _low0$1156, sum$1158, delta$1160
  );
  _tmp$2713 = _tmp$2721 + 1ull;
  _tmp$2720 = i$1146 / 16;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gPOW5_INV_OFFSETS);
  _tmp$2717
  = $ReadOnlyArray$$at$1(
    $moonbitlang$core$double$internal$ryu$gPOW5_INV_OFFSETS, _tmp$2720
  );
  _tmp$2719 = i$1146 % 16;
  _tmp$2718 = _tmp$2719 << 1;
  _tmp$2716 = _tmp$2717 >> (_tmp$2718 & 31);
  _tmp$2715 = _tmp$2716 & 3u;
  _tmp$2714 = $UInt$$to_uint64(_tmp$2715);
  a$1161 = _tmp$2713 + _tmp$2714;
  _tmp$2712 = high1$1159;
  b$1162
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    sum$1158, _tmp$2712, delta$1160
  );
  return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
           a$1161, b$1162
         };
}

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computePow5(
  int32_t i$1128
) {
  int32_t base$1127 = i$1128 / 26;
  int32_t base2$1129 = base$1127 * 26;
  int32_t offset$1130 = i$1128 - base2$1129;
  int32_t _tmp$2710 = base$1127 * 2;
  uint64_t mul0$1131;
  int32_t _tmp$2709;
  int32_t _tmp$2708;
  uint64_t mul1$1132;
  uint64_t m$1133;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1134;
  uint64_t _low1$1135;
  uint64_t _high1$1136;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1137;
  uint64_t _low0$1138;
  uint64_t _high0$1139;
  uint64_t sum$1140;
  uint64_t high1$1141;
  int32_t _tmp$2706;
  int32_t _tmp$2707;
  int32_t delta$1142;
  uint64_t _tmp$2698;
  int32_t _tmp$2705;
  uint32_t _tmp$2702;
  int32_t _tmp$2704;
  int32_t _tmp$2703;
  uint32_t _tmp$2701;
  uint32_t _tmp$2700;
  uint64_t _tmp$2699;
  uint64_t a$1143;
  uint64_t _tmp$2697;
  uint64_t b$1144;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2);
  mul0$1131
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2, _tmp$2710
  );
  _tmp$2709 = base$1127 * 2;
  _tmp$2708 = _tmp$2709 + 1;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2);
  mul1$1132
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2, _tmp$2708
  );
  if (offset$1130 == 0) {
    return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
             mul0$1131, mul1$1132
           };
  }
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE);
  m$1133
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE, offset$1130
  );
  _bind$1134
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1133, mul1$1132
  );
  _low1$1135 = _bind$1134.$0;
  _high1$1136 = _bind$1134.$1;
  _bind$1137
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1133, mul0$1131
  );
  _low0$1138 = _bind$1137.$0;
  _high0$1139 = _bind$1137.$1;
  sum$1140 = _high0$1139 + _low1$1135;
  high1$1141 = _high1$1136;
  if (sum$1140 < _high0$1139) {
    uint64_t _tmp$2696 = high1$1141;
    high1$1141 = _tmp$2696 + 1ull;
  }
  _tmp$2706 = $moonbitlang$core$double$internal$ryu$pow5bits(i$1128);
  _tmp$2707 = $moonbitlang$core$double$internal$ryu$pow5bits(base2$1129);
  delta$1142 = _tmp$2706 - _tmp$2707;
  _tmp$2698
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    _low0$1138, sum$1140, delta$1142
  );
  _tmp$2705 = i$1128 / 16;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gPOW5_OFFSETS);
  _tmp$2702
  = $ReadOnlyArray$$at$1(
    $moonbitlang$core$double$internal$ryu$gPOW5_OFFSETS, _tmp$2705
  );
  _tmp$2704 = i$1128 % 16;
  _tmp$2703 = _tmp$2704 << 1;
  _tmp$2701 = _tmp$2702 >> (_tmp$2703 & 31);
  _tmp$2700 = _tmp$2701 & 3u;
  _tmp$2699 = $UInt$$to_uint64(_tmp$2700);
  a$1143 = _tmp$2698 + _tmp$2699;
  _tmp$2697 = high1$1141;
  b$1144
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    sum$1140, _tmp$2697, delta$1142
  );
  return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
           a$1143, b$1144
         };
}

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result $moonbitlang$core$double$internal$ryu$mulShiftAll64(
  uint64_t m$1101,
  struct $$moonbitlang$core$double$internal$ryu$Pow5Pair mul$1098,
  int32_t j$1114,
  int32_t mmShift$1116
) {
  uint64_t _mul0$1097 = mul$1098.$0;
  uint64_t _mul1$1099 = mul$1098.$1;
  uint64_t m$1100 = m$1101 << 1;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1102 =
    $moonbitlang$core$double$internal$ryu$umul128(m$1100, _mul0$1097);
  uint64_t _lo$1103 = _bind$1102.$0;
  uint64_t _tmp$1104 = _bind$1102.$1;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1105 =
    $moonbitlang$core$double$internal$ryu$umul128(m$1100, _mul1$1099);
  uint64_t _lo2$1106 = _bind$1105.$0;
  uint64_t _hi2$1107 = _bind$1105.$1;
  uint64_t mid$1108 = _tmp$1104 + _lo2$1106;
  uint64_t _tmp$2695;
  uint64_t hi$1109;
  uint64_t lo2$1110;
  uint64_t _tmp$2693;
  uint64_t _tmp$2694;
  uint64_t mid2$1111;
  uint64_t _tmp$2692;
  uint64_t hi2$1112;
  int32_t _tmp$2691;
  int32_t _tmp$2690;
  uint64_t vp$1113;
  uint64_t vm$1115;
  int32_t _tmp$2689;
  int32_t _tmp$2688;
  uint64_t vr$1126;
  uint64_t _tmp$2687;
  if (mid$1108 < _tmp$1104) {
    _tmp$2695 = 1ull;
  } else {
    _tmp$2695 = 0ull;
  }
  hi$1109 = _hi2$1107 + _tmp$2695;
  lo2$1110 = _lo$1103 + _mul0$1097;
  _tmp$2693 = mid$1108 + _mul1$1099;
  if (lo2$1110 < _lo$1103) {
    _tmp$2694 = 1ull;
  } else {
    _tmp$2694 = 0ull;
  }
  mid2$1111 = _tmp$2693 + _tmp$2694;
  if (mid2$1111 < mid$1108) {
    _tmp$2692 = 1ull;
  } else {
    _tmp$2692 = 0ull;
  }
  hi2$1112 = hi$1109 + _tmp$2692;
  _tmp$2691 = j$1114 - 64;
  _tmp$2690 = _tmp$2691 - 1;
  vp$1113
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    mid2$1111, hi2$1112, _tmp$2690
  );
  vm$1115 = 0ull;
  if (mmShift$1116) {
    uint64_t lo3$1117 = _lo$1103 - _mul0$1097;
    uint64_t _tmp$2677 = mid$1108 - _mul1$1099;
    uint64_t _tmp$2678;
    uint64_t mid3$1118;
    uint64_t _tmp$2676;
    uint64_t hi3$1119;
    int32_t _tmp$2675;
    int32_t _tmp$2674;
    if (_lo$1103 < lo3$1117) {
      _tmp$2678 = 1ull;
    } else {
      _tmp$2678 = 0ull;
    }
    mid3$1118 = _tmp$2677 - _tmp$2678;
    if (mid$1108 < mid3$1118) {
      _tmp$2676 = 1ull;
    } else {
      _tmp$2676 = 0ull;
    }
    hi3$1119 = hi$1109 - _tmp$2676;
    _tmp$2675 = j$1114 - 64;
    _tmp$2674 = _tmp$2675 - 1;
    vm$1115
    = $moonbitlang$core$double$internal$ryu$shiftright128(
      mid3$1118, hi3$1119, _tmp$2674
    );
  } else {
    uint64_t lo3$1120 = _lo$1103 + _lo$1103;
    uint64_t _tmp$2685 = mid$1108 + mid$1108;
    uint64_t _tmp$2686;
    uint64_t mid3$1121;
    uint64_t _tmp$2683;
    uint64_t _tmp$2684;
    uint64_t hi3$1122;
    uint64_t lo4$1123;
    uint64_t _tmp$2681;
    uint64_t _tmp$2682;
    uint64_t mid4$1124;
    uint64_t _tmp$2680;
    uint64_t hi4$1125;
    int32_t _tmp$2679;
    if (lo3$1120 < _lo$1103) {
      _tmp$2686 = 1ull;
    } else {
      _tmp$2686 = 0ull;
    }
    mid3$1121 = _tmp$2685 + _tmp$2686;
    _tmp$2683 = hi$1109 + hi$1109;
    if (mid3$1121 < mid$1108) {
      _tmp$2684 = 1ull;
    } else {
      _tmp$2684 = 0ull;
    }
    hi3$1122 = _tmp$2683 + _tmp$2684;
    lo4$1123 = lo3$1120 - _mul0$1097;
    _tmp$2681 = mid3$1121 - _mul1$1099;
    if (lo3$1120 < lo4$1123) {
      _tmp$2682 = 1ull;
    } else {
      _tmp$2682 = 0ull;
    }
    mid4$1124 = _tmp$2681 - _tmp$2682;
    if (mid3$1121 < mid4$1124) {
      _tmp$2680 = 1ull;
    } else {
      _tmp$2680 = 0ull;
    }
    hi4$1125 = hi3$1122 - _tmp$2680;
    _tmp$2679 = j$1114 - 64;
    vm$1115
    = $moonbitlang$core$double$internal$ryu$shiftright128(
      mid4$1124, hi4$1125, _tmp$2679
    );
  }
  _tmp$2689 = j$1114 - 64;
  _tmp$2688 = _tmp$2689 - 1;
  vr$1126
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    mid$1108, hi$1109, _tmp$2688
  );
  _tmp$2687 = vm$1115;
  return (struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result){
           vr$1126, vp$1113, _tmp$2687
         };
}

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf2(
  uint64_t value$1095,
  int32_t p$1096
) {
  uint64_t _tmp$2673 = 1ull << (p$1096 & 63);
  uint64_t _tmp$2672 = _tmp$2673 - 1ull;
  uint64_t _tmp$2671 = value$1095 & _tmp$2672;
  return _tmp$2671 == 0ull;
}

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
  uint64_t value$1093,
  int32_t p$1094
) {
  int32_t _tmp$2670 =
    $moonbitlang$core$double$internal$ryu$pow5Factor(value$1093);
  return _tmp$2670 >= p$1094;
}

int32_t $moonbitlang$core$double$internal$ryu$pow5Factor(uint64_t value$1089) {
  uint64_t _tmp$2658 = value$1089 % 5ull;
  uint64_t _tmp$2659;
  uint64_t _tmp$2660;
  uint64_t _tmp$2661;
  int32_t count$1090;
  uint64_t value$1091;
  uint64_t _tmp$2669;
  moonbit_string_t _tmp$2668;
  moonbit_string_t _tmp$2667;
  if (_tmp$2658 != 0ull) {
    return 0;
  }
  _tmp$2659 = value$1089 % 25ull;
  if (_tmp$2659 != 0ull) {
    return 1;
  }
  _tmp$2660 = value$1089 % 125ull;
  if (_tmp$2660 != 0ull) {
    return 2;
  }
  _tmp$2661 = value$1089 % 625ull;
  if (_tmp$2661 != 0ull) {
    return 3;
  }
  count$1090 = 4;
  value$1091 = value$1089 / 625ull;
  while (1) {
    uint64_t _tmp$2662 = value$1091;
    if (_tmp$2662 > 0ull) {
      uint64_t _tmp$2664 = value$1091;
      uint64_t _tmp$2663 = _tmp$2664 % 5ull;
      uint64_t _tmp$2665;
      int32_t _tmp$2666;
      if (_tmp$2663 != 0ull) {
        return count$1090;
      }
      _tmp$2665 = value$1091;
      value$1091 = _tmp$2665 / 5ull;
      _tmp$2666 = count$1090;
      count$1090 = _tmp$2666 + 1;
      continue;
    }
    break;
  }
  _tmp$2669 = value$1091;
  _tmp$2668
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
    _tmp$2669
  );
  _tmp$2667
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_102.data, _tmp$2668
  );
  return $moonbitlang$core$builtin$abort$1(
           _tmp$2667, (moonbit_string_t)moonbit_string_literal_103.data
         );
}

uint64_t $moonbitlang$core$double$internal$ryu$shiftright128(
  uint64_t lo$1088,
  uint64_t hi$1086,
  int32_t dist$1087
) {
  int32_t _tmp$2657 = 64 - dist$1087;
  uint64_t _tmp$2655 = hi$1086 << (_tmp$2657 & 63);
  uint64_t _tmp$2656 = lo$1088 >> (dist$1087 & 63);
  return _tmp$2655 | _tmp$2656;
}

struct $$moonbitlang$core$double$internal$ryu$Umul128 $moonbitlang$core$double$internal$ryu$umul128(
  uint64_t a$1076,
  uint64_t b$1079
) {
  uint64_t aLo$1075 = a$1076 & 4294967295ull;
  uint64_t aHi$1077 = a$1076 >> 32;
  uint64_t bLo$1078 = b$1079 & 4294967295ull;
  uint64_t bHi$1080 = b$1079 >> 32;
  uint64_t x$1081 = aLo$1075 * bLo$1078;
  uint64_t _tmp$2653 = aHi$1077 * bLo$1078;
  uint64_t _tmp$2654 = x$1081 >> 32;
  uint64_t y$1082 = _tmp$2653 + _tmp$2654;
  uint64_t _tmp$2651 = aLo$1075 * bHi$1080;
  uint64_t _tmp$2652 = y$1082 & 4294967295ull;
  uint64_t z$1083 = _tmp$2651 + _tmp$2652;
  uint64_t _tmp$2649 = aHi$1077 * bHi$1080;
  uint64_t _tmp$2650 = y$1082 >> 32;
  uint64_t _tmp$2647 = _tmp$2649 + _tmp$2650;
  uint64_t _tmp$2648 = z$1083 >> 32;
  uint64_t w$1084 = _tmp$2647 + _tmp$2648;
  uint64_t lo$1085 = a$1076 * b$1079;
  return (struct $$moonbitlang$core$double$internal$ryu$Umul128){
           lo$1085, w$1084
         };
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$string_from_bytes(
  moonbit_bytes_t bytes$1070,
  int32_t from$1074,
  int32_t to$1072
) {
  int32_t _tmp$2646 = Moonbit_array_length(bytes$1070);
  struct $$moonbitlang$core$builtin$StringBuilder* buf$1069 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(_tmp$2646);
  int32_t i$1071 = from$1074;
  while (1) {
    if (i$1071 < to$1072) {
      int32_t _tmp$2644;
      int32_t _tmp$2643;
      int32_t _tmp$2645;
      if (i$1071 < 0 || i$1071 >= Moonbit_array_length(bytes$1070)) {
        moonbit_panic();
      }
      _tmp$2644 = (int32_t)bytes$1070[i$1071];
      _tmp$2643 = $Byte$$to_char(_tmp$2644);
      moonbit_incref(buf$1069);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
        buf$1069, _tmp$2643
      );
      _tmp$2645 = i$1071 + 1;
      i$1071 = _tmp$2645;
      continue;
    } else {
      moonbit_decref(bytes$1070);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$1069);
}

int32_t $moonbitlang$core$double$internal$ryu$log10Pow2(int32_t e$1068) {
  int32_t _tmp$2642 = e$1068 * 78913;
  uint32_t _tmp$2641 = *(uint32_t*)&_tmp$2642;
  uint32_t _tmp$2640 = _tmp$2641 >> 18;
  return *(int32_t*)&_tmp$2640;
}

int32_t $moonbitlang$core$double$internal$ryu$log10Pow5(int32_t e$1067) {
  int32_t _tmp$2639 = e$1067 * 732923;
  uint32_t _tmp$2638 = *(uint32_t*)&_tmp$2639;
  uint32_t _tmp$2637 = _tmp$2638 >> 20;
  return *(int32_t*)&_tmp$2637;
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$copy_special_str(
  int32_t sign$1065,
  int32_t exponent$1066,
  int32_t mantissa$1063
) {
  moonbit_string_t s$1064;
  if (mantissa$1063) {
    return (moonbit_string_t)moonbit_string_literal_104.data;
  }
  if (sign$1065) {
    s$1064 = (moonbit_string_t)moonbit_string_literal_1.data;
  } else {
    s$1064 = (moonbit_string_t)moonbit_string_literal_3.data;
  }
  if (exponent$1066) {
    return moonbit_add_string(
             s$1064, (moonbit_string_t)moonbit_string_literal_105.data
           );
  }
  return moonbit_add_string(
           s$1064, (moonbit_string_t)moonbit_string_literal_106.data
         );
}

int32_t $moonbitlang$core$double$internal$ryu$pow5bits(int32_t e$1062) {
  int32_t _tmp$2636 = e$1062 * 1217359;
  uint32_t _tmp$2635 = *(uint32_t*)&_tmp$2636;
  uint32_t _tmp$2634 = _tmp$2635 >> 19;
  int32_t _tmp$2633 = *(int32_t*)&_tmp$2634;
  return _tmp$2633 + 1;
}

int32_t $$moonbitlang$core$builtin$Array$$contains$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1057,
  moonbit_string_t value$1060
) {
  int32_t _len$1056 = self$1057->$1;
  int32_t _i$1058 = 0;
  while (1) {
    if (_i$1058 < _len$1056) {
      moonbit_string_t* _field$3535 = self$1057->$0;
      moonbit_string_t* buf$2631 = _field$3535;
      moonbit_string_t _tmp$3534 = (moonbit_string_t)buf$2631[_i$1058];
      moonbit_string_t v$1059 = _tmp$3534;
      int32_t _tmp$3533 = moonbit_val_array_equal(v$1059, value$1060);
      int32_t _tmp$2632;
      if (_tmp$3533) {
        moonbit_decref(value$1060);
        moonbit_decref(self$1057);
        return 1;
      }
      _tmp$2632 = _i$1058 + 1;
      _i$1058 = _tmp$2632;
      continue;
    } else {
      moonbit_decref(value$1060);
      moonbit_decref(self$1057);
      return 0;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$Array$$contains$0(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$1051,
  int32_t value$1054
) {
  int32_t _len$1050 = self$1051->$1;
  int32_t _i$1052 = 0;
  while (1) {
    if (_i$1052 < _len$1050) {
      int32_t* _field$3537 = self$1051->$0;
      int32_t* buf$2629 = _field$3537;
      int32_t _tmp$3536 = (int32_t)buf$2629[_i$1052];
      int32_t v$1053 = _tmp$3536;
      int32_t _tmp$2630;
      if (v$1053 == value$1054) {
        moonbit_decref(self$1051);
        return 1;
      }
      _tmp$2630 = _i$1052 + 1;
      _i$1052 = _tmp$2630;
      continue;
    } else {
      moonbit_decref(self$1051);
      return 0;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1049
) {
  int32_t _field$3538 = self$1049->$1;
  int32_t len$2628;
  moonbit_decref(self$1049);
  len$2628 = _field$3538;
  return len$2628 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$1047,
  struct $$moonbitlang$core$builtin$Logger logger$1048
) {
  moonbit_string_t _tmp$2627 = self$1047;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$2626 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$2627);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$2626, logger$1048
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$1033,
  struct $$moonbitlang$core$builtin$Logger logger$1046
) {
  struct $StringView _field$3547 =
    (struct $StringView){self$1033->$0_1, self$1033->$0_2, self$1033->$0_0};
  struct $StringView pkg$1032 = _field$3547;
  int32_t _tmp$2625 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$2624;
  int64_t _bind$1034;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$1035;
  struct $StringView _field$3546;
  struct $StringView _module_name$1042;
  void* _field$3545;
  int32_t _cnt$3818;
  void* _package_name$1043;
  struct $StringView _field$3543;
  struct $StringView filename$2607;
  struct $StringView _field$3542;
  struct $StringView start_line$2608;
  struct $StringView _field$3541;
  struct $StringView start_column$2609;
  struct $StringView _field$3540;
  struct $StringView end_line$2610;
  struct $StringView _field$3539;
  int32_t _cnt$3822;
  struct $StringView end_column$2611;
  struct $$moonbitlang$core$builtin$Logger _bind$2606;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$2624
  = (struct $StringView){
    0, _tmp$2625, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$1032.$0);
  moonbit_incref(pkg$1032.$0);
  _bind$1034 = $StringView$$find(pkg$1032, _tmp$2624);
  if (_bind$1034 == 4294967296ll) {
    void* None$2612 =
      (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
    _bind$1035
    = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
        sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
      );
    Moonbit_object_header(_bind$1035)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
      )
      >> 2,
        2,
        0
    );
    _bind$1035->$0_0 = pkg$1032.$0;
    _bind$1035->$0_1 = pkg$1032.$1;
    _bind$1035->$0_2 = pkg$1032.$2;
    _bind$1035->$1 = None$2612;
  } else {
    int64_t _Some$1036 = _bind$1034;
    int32_t _first_slash$1037 = (int32_t)_Some$1036;
    int32_t _tmp$2623 = _first_slash$1037 + 1;
    struct $StringView _tmp$2620;
    int32_t _tmp$2622;
    struct $StringView _tmp$2621;
    int64_t _bind$1038;
    moonbit_incref(pkg$1032.$0);
    _tmp$2620 = $StringView$$view$inner(pkg$1032, _tmp$2623, 4294967296ll);
    _tmp$2622
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$2621
    = (struct $StringView){
      0, _tmp$2622, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$1038 = $StringView$$find(_tmp$2620, _tmp$2621);
    if (_bind$1038 == 4294967296ll) {
      void* None$2613 =
        (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$1035
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$1035)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$1035->$0_0 = pkg$1032.$0;
      _bind$1035->$0_1 = pkg$1032.$1;
      _bind$1035->$0_2 = pkg$1032.$2;
      _bind$1035->$1 = None$2613;
    } else {
      int64_t _Some$1039 = _bind$1038;
      int32_t _second_slash$1040 = (int32_t)_Some$1039;
      int32_t _tmp$2619 = _first_slash$1037 + 1;
      int32_t module_name_end$1041 = _tmp$2619 + _second_slash$1040;
      int64_t _tmp$2618 = (int64_t)module_name_end$1041;
      struct $StringView _tmp$2614;
      int32_t _tmp$2617;
      struct $StringView _tmp$2616;
      void* Some$2615;
      moonbit_incref(pkg$1032.$0);
      _tmp$2614 = $StringView$$view$inner(pkg$1032, 0, _tmp$2618);
      _tmp$2617 = module_name_end$1041 + 1;
      _tmp$2616 = $StringView$$view$inner(pkg$1032, _tmp$2617, 4294967296ll);
      Some$2615
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$2615)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$2615)->$0_0
      = _tmp$2616.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2615)->$0_1
      = _tmp$2616.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2615)->$0_2
      = _tmp$2616.$2;
      _bind$1035
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$1035)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$1035->$0_0 = _tmp$2614.$0;
      _bind$1035->$0_1 = _tmp$2614.$1;
      _bind$1035->$0_2 = _tmp$2614.$2;
      _bind$1035->$1 = Some$2615;
    }
  }
  _field$3546
  = (struct $StringView){
    _bind$1035->$0_1, _bind$1035->$0_2, _bind$1035->$0_0
  };
  _module_name$1042 = _field$3546;
  _field$3545 = _bind$1035->$1;
  _cnt$3818 = Moonbit_object_header(_bind$1035)->rc;
  if (_cnt$3818 > 1) {
    int32_t _new_cnt$3819;
    moonbit_incref(_field$3545);
    moonbit_incref(_module_name$1042.$0);
    _new_cnt$3819 = _cnt$3818 - 1;
    Moonbit_object_header(_bind$1035)->rc = _new_cnt$3819;
  } else if (_cnt$3818 == 1) {
    moonbit_free(_bind$1035);
  }
  _package_name$1043 = _field$3545;
  switch (Moonbit_object_tag(_package_name$1043)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$1044 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$1043;
      struct $StringView _field$3544 =
        (struct $StringView){
          _Some$1044->$0_1, _Some$1044->$0_2, _Some$1044->$0_0
        };
      int32_t _cnt$3820 = Moonbit_object_header(_Some$1044)->rc;
      struct $StringView _pkg_name$1045;
      struct $$moonbitlang$core$builtin$Logger _bind$2605;
      if (_cnt$3820 > 1) {
        int32_t _new_cnt$3821;
        moonbit_incref(_field$3544.$0);
        _new_cnt$3821 = _cnt$3820 - 1;
        Moonbit_object_header(_Some$1044)->rc = _new_cnt$3821;
      } else if (_cnt$3820 == 1) {
        moonbit_free(_Some$1044);
      }
      _pkg_name$1045 = _field$3544;
      if (logger$1046.$1) {
        moonbit_incref(logger$1046.$1);
      }
      logger$1046.$0->$method_2(logger$1046.$1, _pkg_name$1045);
      _bind$2605 = logger$1046;
      if (_bind$2605.$1) {
        moonbit_incref(_bind$2605.$1);
      }
      _bind$2605.$0->$method_3(_bind$2605.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$1043);
      break;
    }
  }
  _field$3543
  = (struct $StringView){
    self$1033->$1_1, self$1033->$1_2, self$1033->$1_0
  };
  filename$2607 = _field$3543;
  moonbit_incref(filename$2607.$0);
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_2(logger$1046.$1, filename$2607);
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_3(logger$1046.$1, 58);
  _field$3542
  = (struct $StringView){
    self$1033->$2_1, self$1033->$2_2, self$1033->$2_0
  };
  start_line$2608 = _field$3542;
  moonbit_incref(start_line$2608.$0);
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_2(logger$1046.$1, start_line$2608);
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_3(logger$1046.$1, 58);
  _field$3541
  = (struct $StringView){
    self$1033->$3_1, self$1033->$3_2, self$1033->$3_0
  };
  start_column$2609 = _field$3541;
  moonbit_incref(start_column$2609.$0);
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_2(logger$1046.$1, start_column$2609);
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_3(logger$1046.$1, 45);
  _field$3540
  = (struct $StringView){
    self$1033->$4_1, self$1033->$4_2, self$1033->$4_0
  };
  end_line$2610 = _field$3540;
  moonbit_incref(end_line$2610.$0);
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_2(logger$1046.$1, end_line$2610);
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_3(logger$1046.$1, 58);
  _field$3539
  = (struct $StringView){
    self$1033->$5_1, self$1033->$5_2, self$1033->$5_0
  };
  _cnt$3822 = Moonbit_object_header(self$1033)->rc;
  if (_cnt$3822 > 1) {
    int32_t _new_cnt$3828;
    moonbit_incref(_field$3539.$0);
    _new_cnt$3828 = _cnt$3822 - 1;
    Moonbit_object_header(self$1033)->rc = _new_cnt$3828;
  } else if (_cnt$3822 == 1) {
    struct $StringView _field$3827 =
      (struct $StringView){self$1033->$4_1, self$1033->$4_2, self$1033->$4_0};
    struct $StringView _field$3826;
    struct $StringView _field$3825;
    struct $StringView _field$3824;
    struct $StringView _field$3823;
    moonbit_decref(_field$3827.$0);
    _field$3826
    = (struct $StringView){
      self$1033->$3_1, self$1033->$3_2, self$1033->$3_0
    };
    moonbit_decref(_field$3826.$0);
    _field$3825
    = (struct $StringView){
      self$1033->$2_1, self$1033->$2_2, self$1033->$2_0
    };
    moonbit_decref(_field$3825.$0);
    _field$3824
    = (struct $StringView){
      self$1033->$1_1, self$1033->$1_2, self$1033->$1_0
    };
    moonbit_decref(_field$3824.$0);
    _field$3823
    = (struct $StringView){
      self$1033->$0_1, self$1033->$0_2, self$1033->$0_0
    };
    moonbit_decref(_field$3823.$0);
    moonbit_free(self$1033);
  }
  end_column$2611 = _field$3539;
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_2(logger$1046.$1, end_column$2611);
  if (logger$1046.$1) {
    moonbit_incref(logger$1046.$1);
  }
  logger$1046.$0->$method_3(logger$1046.$1, 64);
  _bind$2606 = logger$1046;
  _bind$2606.$0->$method_2(_bind$2606.$1, _module_name$1042);
  return 0;
}

uint64_t $Bool$$to_uint64(int32_t self$1031) {
  if (self$1031) {
    return 1ull;
  } else {
    return 0ull;
  }
}

int64_t $Bool$$to_int64(int32_t self$1030) {
  if (self$1030) {
    return 1ll;
  } else {
    return 0ll;
  }
}

int32_t $Bool$$to_int(int32_t self$1029) {
  if (self$1029) {
    return 1;
  } else {
    return 0;
  }
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$1028) {
  moonbit_string_t _tmp$2604 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$1028);
  moonbit_println(_tmp$2604);
  moonbit_decref(_tmp$2604);
  return 0;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$1023,
  moonbit_string_t msg$1025,
  moonbit_string_t loc$1027
) {
  if (!x$1023) {
    moonbit_string_t fail_msg$1024;
    if (msg$1025 == 0) {
      moonbit_string_t _tmp$2602;
      moonbit_string_t _tmp$2601;
      if (msg$1025) {
        moonbit_decref(msg$1025);
      }
      _tmp$2602
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$1023
      );
      _tmp$2601
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_107.data, _tmp$2602
      );
      fail_msg$1024
      = moonbit_add_string(
        _tmp$2601, (moonbit_string_t)moonbit_string_literal_108.data
      );
    } else {
      moonbit_string_t _Some$1026 = msg$1025;
      fail_msg$1024 = _Some$1026;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$1024, loc$1027);
  } else {
    int32_t _tmp$2603;
    struct moonbit_result_0 _result$4064;
    moonbit_decref(loc$1027);
    if (msg$1025) {
      moonbit_decref(msg$1025);
    }
    _tmp$2603 = 0;
    _result$4064.tag = 1;
    _result$4064.data.ok = _tmp$2603;
    return _result$4064;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$1018,
  moonbit_string_t msg$1020,
  moonbit_string_t loc$1022
) {
  if (x$1018) {
    moonbit_string_t fail_msg$1019;
    if (msg$1020 == 0) {
      moonbit_string_t _tmp$2599;
      moonbit_string_t _tmp$2598;
      if (msg$1020) {
        moonbit_decref(msg$1020);
      }
      _tmp$2599
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$1018
      );
      _tmp$2598
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_107.data, _tmp$2599
      );
      fail_msg$1019
      = moonbit_add_string(
        _tmp$2598, (moonbit_string_t)moonbit_string_literal_109.data
      );
    } else {
      moonbit_string_t _Some$1021 = msg$1020;
      fail_msg$1019 = _Some$1021;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$1019, loc$1022);
  } else {
    int32_t _tmp$2600;
    struct moonbit_result_0 _result$4065;
    moonbit_decref(loc$1022);
    if (msg$1020) {
      moonbit_decref(msg$1020);
    }
    _tmp$2600 = 0;
    _result$4065.tag = 1;
    _result$4065.data.ok = _tmp$2600;
    return _result$4065;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$1017,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1016
) {
  $$moonbitlang$core$builtin$Hasher$$combine_int(hasher$1016, self$1017);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$1015,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1014
) {
  $$moonbitlang$core$builtin$Hasher$$combine_string(hasher$1014, self$1015);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$1012,
  moonbit_string_t value$1010
) {
  int32_t _end2448$1009 = Moonbit_array_length(value$1010);
  int32_t i$1011 = 0;
  while (1) {
    if (i$1011 < _end2448$1009) {
      int32_t _tmp$2596 = value$1010[i$1011];
      uint32_t _tmp$2595 = *(uint32_t*)&_tmp$2596;
      int32_t _tmp$2597;
      moonbit_incref(self$1012);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$1012, _tmp$2595);
      _tmp$2597 = i$1011 + 1;
      i$1011 = _tmp$2597;
      continue;
    } else {
      moonbit_decref(self$1012);
      moonbit_decref(value$1010);
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$1006,
  struct $$3c$String$3e$$3d$$3e$Int* f$1008
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$4067 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$2592;
  int32_t _tmp$2591;
  Moonbit_object_header(_closure$4067)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$4067->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$4067->$0 = f$1008;
  _tmp$2592 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$4067;
  _tmp$2591 = $$moonbitlang$core$builtin$Iter$$run$0(self$1006, _tmp$2592);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$2591, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2593,
  moonbit_string_t k$1007
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$2594 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$2593;
  struct $$3c$String$3e$$3d$$3e$Int* _field$3548 = _casted_env$2594->$0;
  int32_t _cnt$3829 = Moonbit_object_header(_casted_env$2594)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$1008;
  if (_cnt$3829 > 1) {
    int32_t _new_cnt$3830;
    moonbit_incref(_field$3548);
    _new_cnt$3830 = _cnt$3829 - 1;
    Moonbit_object_header(_casted_env$2594)->rc = _new_cnt$3830;
  } else if (_cnt$3829 == 1) {
    moonbit_free(_casted_env$2594);
  }
  f$1008 = _field$3548;
  if (f$1008->code(f$1008, k$1007)) {
    return 0;
  } else {
    return 1;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1004,
  int32_t idx$1005
) {
  moonbit_string_t* _tmp$2590 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$1004);
  moonbit_string_t _tmp$3549;
  if (idx$1005 < 0 || idx$1005 >= Moonbit_array_length(_tmp$2590)) {
    moonbit_panic();
  }
  _tmp$3549 = (moonbit_string_t)_tmp$2590[idx$1005];
  moonbit_incref(_tmp$3549);
  moonbit_decref(_tmp$2590);
  return _tmp$3549;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$1002,
  int32_t idx$1003
) {
  struct $$3c$String$2a$Int$3e$** _tmp$2589 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$1002);
  struct $$3c$String$2a$Int$3e$* _tmp$3550;
  if (idx$1003 < 0 || idx$1003 >= Moonbit_array_length(_tmp$2589)) {
    moonbit_panic();
  }
  _tmp$3550 = (struct $$3c$String$2a$Int$3e$*)_tmp$2589[idx$1003];
  if (_tmp$3550) {
    moonbit_incref(_tmp$3550);
  }
  moonbit_decref(_tmp$2589);
  return _tmp$3550;
}

uint64_t $UInt$$to_uint64(uint32_t self$1001) {
  return (uint64_t)self$1001;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$997,
  int32_t key$993
) {
  int32_t hash$992 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$993);
  int32_t capacity_mask$2588 = self$997->$3;
  int32_t _tmp$2587 = hash$992 & capacity_mask$2588;
  int32_t i$994 = 0;
  int32_t idx$995 = _tmp$2587;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3554 =
      self$997->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2586 =
      _field$3554;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3553;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$996;
    if (idx$995 < 0 || idx$995 >= Moonbit_array_length(entries$2586)) {
      moonbit_panic();
    }
    _tmp$3553
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2586[
        idx$995
      ];
    _bind$996 = _tmp$3553;
    if (_bind$996 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2575;
      if (_bind$996) {
        moonbit_incref(_bind$996);
      }
      moonbit_decref(self$997);
      if (_bind$996) {
        moonbit_decref(_bind$996);
      }
      _tmp$2575 = 0;
      return _tmp$2575;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$998 =
        _bind$996;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$999 =
        _Some$998;
      int32_t hash$2577 = _entry$999->$3;
      int32_t _if_result$4069;
      int32_t _field$3551;
      int32_t psl$2580;
      int32_t _tmp$2582;
      int32_t _tmp$2584;
      int32_t capacity_mask$2585;
      int32_t _tmp$2583;
      if (hash$2577 == hash$992) {
        int32_t key$2576 = _entry$999->$4;
        _if_result$4069 = key$2576 == key$993;
      } else {
        _if_result$4069 = 0;
      }
      if (_if_result$4069) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3552;
        int32_t _cnt$3831;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2579;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2578;
        moonbit_incref(_entry$999);
        moonbit_decref(self$997);
        _field$3552 = _entry$999->$5;
        _cnt$3831 = Moonbit_object_header(_entry$999)->rc;
        if (_cnt$3831 > 1) {
          int32_t _new_cnt$3833;
          moonbit_incref(_field$3552);
          _new_cnt$3833 = _cnt$3831 - 1;
          Moonbit_object_header(_entry$999)->rc = _new_cnt$3833;
        } else if (_cnt$3831 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3832 =
            _entry$999->$1;
          if (_field$3832) {
            moonbit_decref(_field$3832);
          }
          moonbit_free(_entry$999);
        }
        value$2579 = _field$3552;
        _tmp$2578 = value$2579;
        return _tmp$2578;
      } else {
        moonbit_incref(_entry$999);
      }
      _field$3551 = _entry$999->$2;
      moonbit_decref(_entry$999);
      psl$2580 = _field$3551;
      if (i$994 > psl$2580) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2581;
        moonbit_decref(self$997);
        _tmp$2581 = 0;
        return _tmp$2581;
      }
      _tmp$2582 = i$994 + 1;
      _tmp$2584 = idx$995 + 1;
      capacity_mask$2585 = self$997->$3;
      _tmp$2583 = _tmp$2584 & capacity_mask$2585;
      i$994 = _tmp$2582;
      idx$995 = _tmp$2583;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$988,
  moonbit_string_t key$984
) {
  int32_t hash$983;
  int32_t capacity_mask$2574;
  int32_t _tmp$2573;
  int32_t i$985;
  int32_t idx$986;
  moonbit_incref(key$984);
  hash$983 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$984);
  capacity_mask$2574 = self$988->$3;
  _tmp$2573 = hash$983 & capacity_mask$2574;
  i$985 = 0;
  idx$986 = _tmp$2573;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3560 =
      self$988->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2572 =
      _field$3560;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3559;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$987;
    if (idx$986 < 0 || idx$986 >= Moonbit_array_length(entries$2572)) {
      moonbit_panic();
    }
    _tmp$3559
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2572[
        idx$986
      ];
    _bind$987 = _tmp$3559;
    if (_bind$987 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2561;
      if (_bind$987) {
        moonbit_incref(_bind$987);
      }
      moonbit_decref(self$988);
      if (_bind$987) {
        moonbit_decref(_bind$987);
      }
      moonbit_decref(key$984);
      _tmp$2561 = 0;
      return _tmp$2561;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$989 =
        _bind$987;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$990 =
        _Some$989;
      int32_t hash$2563 = _entry$990->$3;
      int32_t _if_result$4071;
      int32_t _field$3555;
      int32_t psl$2566;
      int32_t _tmp$2568;
      int32_t _tmp$2570;
      int32_t capacity_mask$2571;
      int32_t _tmp$2569;
      if (hash$2563 == hash$983) {
        moonbit_string_t _field$3558 = _entry$990->$4;
        moonbit_string_t key$2562 = _field$3558;
        int32_t _tmp$3557 = moonbit_val_array_equal(key$2562, key$984);
        _if_result$4071 = _tmp$3557;
      } else {
        _if_result$4071 = 0;
      }
      if (_if_result$4071) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3556;
        int32_t _cnt$3834;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2565;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2564;
        moonbit_incref(_entry$990);
        moonbit_decref(self$988);
        moonbit_decref(key$984);
        _field$3556 = _entry$990->$5;
        _cnt$3834 = Moonbit_object_header(_entry$990)->rc;
        if (_cnt$3834 > 1) {
          int32_t _new_cnt$3837;
          moonbit_incref(_field$3556);
          _new_cnt$3837 = _cnt$3834 - 1;
          Moonbit_object_header(_entry$990)->rc = _new_cnt$3837;
        } else if (_cnt$3834 == 1) {
          moonbit_string_t _field$3836 = _entry$990->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3835;
          moonbit_decref(_field$3836);
          _field$3835 = _entry$990->$1;
          if (_field$3835) {
            moonbit_decref(_field$3835);
          }
          moonbit_free(_entry$990);
        }
        value$2565 = _field$3556;
        _tmp$2564 = value$2565;
        return _tmp$2564;
      } else {
        moonbit_incref(_entry$990);
      }
      _field$3555 = _entry$990->$2;
      moonbit_decref(_entry$990);
      psl$2566 = _field$3555;
      if (i$985 > psl$2566) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2567;
        moonbit_decref(self$988);
        moonbit_decref(key$984);
        _tmp$2567 = 0;
        return _tmp$2567;
      }
      _tmp$2568 = i$985 + 1;
      _tmp$2570 = idx$986 + 1;
      capacity_mask$2571 = self$988->$3;
      _tmp$2569 = _tmp$2570 & capacity_mask$2571;
      i$985 = _tmp$2568;
      idx$986 = _tmp$2569;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$979,
  int32_t key$975
) {
  int32_t hash$974 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$975);
  int32_t capacity_mask$2560 = self$979->$3;
  int32_t _tmp$2559 = hash$974 & capacity_mask$2560;
  int32_t i$976 = 0;
  int32_t idx$977 = _tmp$2559;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3564 =
      self$979->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2558 =
      _field$3564;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3563;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$978;
    if (idx$977 < 0 || idx$977 >= Moonbit_array_length(entries$2558)) {
      moonbit_panic();
    }
    _tmp$3563
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2558[
        idx$977
      ];
    _bind$978 = _tmp$3563;
    if (_bind$978 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2547;
      if (_bind$978) {
        moonbit_incref(_bind$978);
      }
      moonbit_decref(self$979);
      if (_bind$978) {
        moonbit_decref(_bind$978);
      }
      _tmp$2547 = 0;
      return _tmp$2547;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$980 =
        _bind$978;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$981 =
        _Some$980;
      int32_t hash$2549 = _entry$981->$3;
      int32_t _if_result$4073;
      int32_t _field$3561;
      int32_t psl$2552;
      int32_t _tmp$2554;
      int32_t _tmp$2556;
      int32_t capacity_mask$2557;
      int32_t _tmp$2555;
      if (hash$2549 == hash$974) {
        int32_t key$2548 = _entry$981->$4;
        _if_result$4073 = key$2548 == key$975;
      } else {
        _if_result$4073 = 0;
      }
      if (_if_result$4073) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3562;
        int32_t _cnt$3838;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2551;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2550;
        moonbit_incref(_entry$981);
        moonbit_decref(self$979);
        _field$3562 = _entry$981->$5;
        _cnt$3838 = Moonbit_object_header(_entry$981)->rc;
        if (_cnt$3838 > 1) {
          int32_t _new_cnt$3840;
          moonbit_incref(_field$3562);
          _new_cnt$3840 = _cnt$3838 - 1;
          Moonbit_object_header(_entry$981)->rc = _new_cnt$3840;
        } else if (_cnt$3838 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3839 =
            _entry$981->$1;
          if (_field$3839) {
            moonbit_decref(_field$3839);
          }
          moonbit_free(_entry$981);
        }
        value$2551 = _field$3562;
        _tmp$2550 = value$2551;
        return _tmp$2550;
      } else {
        moonbit_incref(_entry$981);
      }
      _field$3561 = _entry$981->$2;
      moonbit_decref(_entry$981);
      psl$2552 = _field$3561;
      if (i$976 > psl$2552) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2553;
        moonbit_decref(self$979);
        _tmp$2553 = 0;
        return _tmp$2553;
      }
      _tmp$2554 = i$976 + 1;
      _tmp$2556 = idx$977 + 1;
      capacity_mask$2557 = self$979->$3;
      _tmp$2555 = _tmp$2556 & capacity_mask$2557;
      i$976 = _tmp$2554;
      idx$977 = _tmp$2555;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$970,
  moonbit_string_t key$966
) {
  int32_t hash$965;
  int32_t capacity_mask$2546;
  int32_t _tmp$2545;
  int32_t i$967;
  int32_t idx$968;
  moonbit_incref(key$966);
  hash$965 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$966);
  capacity_mask$2546 = self$970->$3;
  _tmp$2545 = hash$965 & capacity_mask$2546;
  i$967 = 0;
  idx$968 = _tmp$2545;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3570 =
      self$970->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2544 =
      _field$3570;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3569;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$969;
    if (idx$968 < 0 || idx$968 >= Moonbit_array_length(entries$2544)) {
      moonbit_panic();
    }
    _tmp$3569
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2544[
        idx$968
      ];
    _bind$969 = _tmp$3569;
    if (_bind$969 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2533;
      if (_bind$969) {
        moonbit_incref(_bind$969);
      }
      moonbit_decref(self$970);
      if (_bind$969) {
        moonbit_decref(_bind$969);
      }
      moonbit_decref(key$966);
      _tmp$2533 = 0;
      return _tmp$2533;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$971 =
        _bind$969;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$972 =
        _Some$971;
      int32_t hash$2535 = _entry$972->$3;
      int32_t _if_result$4075;
      int32_t _field$3565;
      int32_t psl$2538;
      int32_t _tmp$2540;
      int32_t _tmp$2542;
      int32_t capacity_mask$2543;
      int32_t _tmp$2541;
      if (hash$2535 == hash$965) {
        moonbit_string_t _field$3568 = _entry$972->$4;
        moonbit_string_t key$2534 = _field$3568;
        int32_t _tmp$3567 = moonbit_val_array_equal(key$2534, key$966);
        _if_result$4075 = _tmp$3567;
      } else {
        _if_result$4075 = 0;
      }
      if (_if_result$4075) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3566;
        int32_t _cnt$3841;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2537;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2536;
        moonbit_incref(_entry$972);
        moonbit_decref(self$970);
        moonbit_decref(key$966);
        _field$3566 = _entry$972->$5;
        _cnt$3841 = Moonbit_object_header(_entry$972)->rc;
        if (_cnt$3841 > 1) {
          int32_t _new_cnt$3844;
          moonbit_incref(_field$3566);
          _new_cnt$3844 = _cnt$3841 - 1;
          Moonbit_object_header(_entry$972)->rc = _new_cnt$3844;
        } else if (_cnt$3841 == 1) {
          moonbit_string_t _field$3843 = _entry$972->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3842;
          moonbit_decref(_field$3843);
          _field$3842 = _entry$972->$1;
          if (_field$3842) {
            moonbit_decref(_field$3842);
          }
          moonbit_free(_entry$972);
        }
        value$2537 = _field$3566;
        _tmp$2536 = value$2537;
        return _tmp$2536;
      } else {
        moonbit_incref(_entry$972);
      }
      _field$3565 = _entry$972->$2;
      moonbit_decref(_entry$972);
      psl$2538 = _field$3565;
      if (i$967 > psl$2538) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2539;
        moonbit_decref(self$970);
        moonbit_decref(key$966);
        _tmp$2539 = 0;
        return _tmp$2539;
      }
      _tmp$2540 = i$967 + 1;
      _tmp$2542 = idx$968 + 1;
      capacity_mask$2543 = self$970->$3;
      _tmp$2541 = _tmp$2542 & capacity_mask$2543;
      i$967 = _tmp$2540;
      idx$968 = _tmp$2541;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$961,
  int32_t key$957
) {
  int32_t hash$956 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$957);
  int32_t capacity_mask$2532 = self$961->$3;
  int32_t _tmp$2531 = hash$956 & capacity_mask$2532;
  int32_t i$958 = 0;
  int32_t idx$959 = _tmp$2531;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3574 =
      self$961->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2530 =
      _field$3574;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3573;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$960;
    if (idx$959 < 0 || idx$959 >= Moonbit_array_length(entries$2530)) {
      moonbit_panic();
    }
    _tmp$3573
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2530[
        idx$959
      ];
    _bind$960 = _tmp$3573;
    if (_bind$960 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2519;
      if (_bind$960) {
        moonbit_incref(_bind$960);
      }
      moonbit_decref(self$961);
      if (_bind$960) {
        moonbit_decref(_bind$960);
      }
      _tmp$2519 = 0;
      return _tmp$2519;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$962 =
        _bind$960;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$963 =
        _Some$962;
      int32_t hash$2521 = _entry$963->$3;
      int32_t _if_result$4077;
      int32_t _field$3571;
      int32_t psl$2524;
      int32_t _tmp$2526;
      int32_t _tmp$2528;
      int32_t capacity_mask$2529;
      int32_t _tmp$2527;
      if (hash$2521 == hash$956) {
        int32_t key$2520 = _entry$963->$4;
        _if_result$4077 = key$2520 == key$957;
      } else {
        _if_result$4077 = 0;
      }
      if (_if_result$4077) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3572;
        int32_t _cnt$3845;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2523;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2522;
        moonbit_incref(_entry$963);
        moonbit_decref(self$961);
        _field$3572 = _entry$963->$5;
        _cnt$3845 = Moonbit_object_header(_entry$963)->rc;
        if (_cnt$3845 > 1) {
          int32_t _new_cnt$3847;
          moonbit_incref(_field$3572);
          _new_cnt$3847 = _cnt$3845 - 1;
          Moonbit_object_header(_entry$963)->rc = _new_cnt$3847;
        } else if (_cnt$3845 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3846 =
            _entry$963->$1;
          if (_field$3846) {
            moonbit_decref(_field$3846);
          }
          moonbit_free(_entry$963);
        }
        value$2523 = _field$3572;
        _tmp$2522 = value$2523;
        return _tmp$2522;
      } else {
        moonbit_incref(_entry$963);
      }
      _field$3571 = _entry$963->$2;
      moonbit_decref(_entry$963);
      psl$2524 = _field$3571;
      if (i$958 > psl$2524) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2525;
        moonbit_decref(self$961);
        _tmp$2525 = 0;
        return _tmp$2525;
      }
      _tmp$2526 = i$958 + 1;
      _tmp$2528 = idx$959 + 1;
      capacity_mask$2529 = self$961->$3;
      _tmp$2527 = _tmp$2528 & capacity_mask$2529;
      i$958 = _tmp$2526;
      idx$959 = _tmp$2527;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$952,
  moonbit_string_t key$948
) {
  int32_t hash$947;
  int32_t capacity_mask$2518;
  int32_t _tmp$2517;
  int32_t i$949;
  int32_t idx$950;
  moonbit_incref(key$948);
  hash$947 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$948);
  capacity_mask$2518 = self$952->$3;
  _tmp$2517 = hash$947 & capacity_mask$2518;
  i$949 = 0;
  idx$950 = _tmp$2517;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3580 =
      self$952->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2516 =
      _field$3580;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3579;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$951;
    if (idx$950 < 0 || idx$950 >= Moonbit_array_length(entries$2516)) {
      moonbit_panic();
    }
    _tmp$3579
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2516[
        idx$950
      ];
    _bind$951 = _tmp$3579;
    if (_bind$951 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2505;
      if (_bind$951) {
        moonbit_incref(_bind$951);
      }
      moonbit_decref(self$952);
      if (_bind$951) {
        moonbit_decref(_bind$951);
      }
      moonbit_decref(key$948);
      _tmp$2505 = 0;
      return _tmp$2505;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$953 =
        _bind$951;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$954 =
        _Some$953;
      int32_t hash$2507 = _entry$954->$3;
      int32_t _if_result$4079;
      int32_t _field$3575;
      int32_t psl$2510;
      int32_t _tmp$2512;
      int32_t _tmp$2514;
      int32_t capacity_mask$2515;
      int32_t _tmp$2513;
      if (hash$2507 == hash$947) {
        moonbit_string_t _field$3578 = _entry$954->$4;
        moonbit_string_t key$2506 = _field$3578;
        int32_t _tmp$3577 = moonbit_val_array_equal(key$2506, key$948);
        _if_result$4079 = _tmp$3577;
      } else {
        _if_result$4079 = 0;
      }
      if (_if_result$4079) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3576;
        int32_t _cnt$3848;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2509;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2508;
        moonbit_incref(_entry$954);
        moonbit_decref(self$952);
        moonbit_decref(key$948);
        _field$3576 = _entry$954->$5;
        _cnt$3848 = Moonbit_object_header(_entry$954)->rc;
        if (_cnt$3848 > 1) {
          int32_t _new_cnt$3851;
          moonbit_incref(_field$3576);
          _new_cnt$3851 = _cnt$3848 - 1;
          Moonbit_object_header(_entry$954)->rc = _new_cnt$3851;
        } else if (_cnt$3848 == 1) {
          moonbit_string_t _field$3850 = _entry$954->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3849;
          moonbit_decref(_field$3850);
          _field$3849 = _entry$954->$1;
          if (_field$3849) {
            moonbit_decref(_field$3849);
          }
          moonbit_free(_entry$954);
        }
        value$2509 = _field$3576;
        _tmp$2508 = value$2509;
        return _tmp$2508;
      } else {
        moonbit_incref(_entry$954);
      }
      _field$3575 = _entry$954->$2;
      moonbit_decref(_entry$954);
      psl$2510 = _field$3575;
      if (i$949 > psl$2510) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2511;
        moonbit_decref(self$952);
        moonbit_decref(key$948);
        _tmp$2511 = 0;
        return _tmp$2511;
      }
      _tmp$2512 = i$949 + 1;
      _tmp$2514 = idx$950 + 1;
      capacity_mask$2515 = self$952->$3;
      _tmp$2513 = _tmp$2514 & capacity_mask$2515;
      i$949 = _tmp$2512;
      idx$950 = _tmp$2513;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$940
) {
  int32_t length$939;
  int32_t capacity$941;
  int32_t _tmp$2496;
  int32_t _tmp$2495;
  int32_t _tmp$2504;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$942;
  int32_t _len$943;
  int32_t _i$944;
  moonbit_incref(arr$940.$0);
  length$939 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$940);
  capacity$941 = $Int$$next_power_of_two(length$939);
  _tmp$2496 = capacity$941;
  _tmp$2495 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2496);
  if (length$939 > _tmp$2495) {
    int32_t _tmp$2497 = capacity$941;
    capacity$941 = _tmp$2497 * 2;
  }
  _tmp$2504 = capacity$941;
  m$942 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$2504);
  moonbit_incref(arr$940.$0);
  _len$943 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$940);
  _i$944 = 0;
  while (1) {
    if (_i$944 < _len$943) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3584 =
        arr$940.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2500 =
        _field$3584;
      int32_t start$2502 = arr$940.$1;
      int32_t _tmp$2501 = start$2502 + _i$944;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3583 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2500[
          _tmp$2501
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$945 =
        _tmp$3583;
      moonbit_string_t _field$3582 = e$945->$0;
      moonbit_string_t _tmp$2498 = _field$3582;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3581 =
        e$945->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2499 =
        _field$3581;
      int32_t _tmp$2503;
      moonbit_incref(_tmp$2499);
      moonbit_incref(_tmp$2498);
      moonbit_incref(m$942);
      $$moonbitlang$core$builtin$Map$$set$3(m$942, _tmp$2498, _tmp$2499);
      _tmp$2503 = _i$944 + 1;
      _i$944 = _tmp$2503;
      continue;
    } else {
      moonbit_decref(arr$940.$0);
    }
    break;
  }
  return m$942;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$932
) {
  int32_t length$931;
  int32_t capacity$933;
  int32_t _tmp$2486;
  int32_t _tmp$2485;
  int32_t _tmp$2494;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$934;
  int32_t _len$935;
  int32_t _i$936;
  moonbit_incref(arr$932.$0);
  length$931 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$932);
  capacity$933 = $Int$$next_power_of_two(length$931);
  _tmp$2486 = capacity$933;
  _tmp$2485 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2486);
  if (length$931 > _tmp$2485) {
    int32_t _tmp$2487 = capacity$933;
    capacity$933 = _tmp$2487 * 2;
  }
  _tmp$2494 = capacity$933;
  m$934 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$2494);
  moonbit_incref(arr$932.$0);
  _len$935 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$932);
  _i$936 = 0;
  while (1) {
    if (_i$936 < _len$935) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3588 =
        arr$932.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2490 =
        _field$3588;
      int32_t start$2492 = arr$932.$1;
      int32_t _tmp$2491 = start$2492 + _i$936;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3587 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2490[
          _tmp$2491
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$937 =
        _tmp$3587;
      moonbit_string_t _field$3586 = e$937->$0;
      moonbit_string_t _tmp$2488 = _field$3586;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3585 =
        e$937->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2489 =
        _field$3585;
      int32_t _tmp$2493;
      moonbit_incref(_tmp$2489);
      moonbit_incref(_tmp$2488);
      moonbit_incref(m$934);
      $$moonbitlang$core$builtin$Map$$set$2(m$934, _tmp$2488, _tmp$2489);
      _tmp$2493 = _i$936 + 1;
      _i$936 = _tmp$2493;
      continue;
    } else {
      moonbit_decref(arr$932.$0);
    }
    break;
  }
  return m$934;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$924
) {
  int32_t length$923;
  int32_t capacity$925;
  int32_t _tmp$2476;
  int32_t _tmp$2475;
  int32_t _tmp$2484;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$926;
  int32_t _len$927;
  int32_t _i$928;
  moonbit_incref(arr$924.$0);
  length$923 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$924);
  capacity$925 = $Int$$next_power_of_two(length$923);
  _tmp$2476 = capacity$925;
  _tmp$2475 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2476);
  if (length$923 > _tmp$2475) {
    int32_t _tmp$2477 = capacity$925;
    capacity$925 = _tmp$2477 * 2;
  }
  _tmp$2484 = capacity$925;
  m$926 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$2484);
  moonbit_incref(arr$924.$0);
  _len$927 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$924);
  _i$928 = 0;
  while (1) {
    if (_i$928 < _len$927) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3591 =
        arr$924.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$2480 =
        _field$3591;
      int32_t start$2482 = arr$924.$1;
      int32_t _tmp$2481 = start$2482 + _i$928;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3590 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$2480[
          _tmp$2481
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$929 =
        _tmp$3590;
      int32_t _tmp$2478 = e$929->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3589 =
        e$929->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2479 =
        _field$3589;
      int32_t _tmp$2483;
      moonbit_incref(_tmp$2479);
      moonbit_incref(m$926);
      $$moonbitlang$core$builtin$Map$$set$1(m$926, _tmp$2478, _tmp$2479);
      _tmp$2483 = _i$928 + 1;
      _i$928 = _tmp$2483;
      continue;
    } else {
      moonbit_decref(arr$924.$0);
    }
    break;
  }
  return m$926;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$916
) {
  int32_t length$915;
  int32_t capacity$917;
  int32_t _tmp$2466;
  int32_t _tmp$2465;
  int32_t _tmp$2474;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$918;
  int32_t _len$919;
  int32_t _i$920;
  moonbit_incref(arr$916.$0);
  length$915 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$916);
  capacity$917 = $Int$$next_power_of_two(length$915);
  _tmp$2466 = capacity$917;
  _tmp$2465 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2466);
  if (length$915 > _tmp$2465) {
    int32_t _tmp$2467 = capacity$917;
    capacity$917 = _tmp$2467 * 2;
  }
  _tmp$2474 = capacity$917;
  m$918 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$2474);
  moonbit_incref(arr$916.$0);
  _len$919 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$916);
  _i$920 = 0;
  while (1) {
    if (_i$920 < _len$919) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3595 =
        arr$916.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2470 =
        _field$3595;
      int32_t start$2472 = arr$916.$1;
      int32_t _tmp$2471 = start$2472 + _i$920;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3594 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2470[
          _tmp$2471
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$921 =
        _tmp$3594;
      moonbit_string_t _field$3593 = e$921->$0;
      moonbit_string_t _tmp$2468 = _field$3593;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3592 =
        e$921->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2469 =
        _field$3592;
      int32_t _tmp$2473;
      moonbit_incref(_tmp$2469);
      moonbit_incref(_tmp$2468);
      moonbit_incref(m$918);
      $$moonbitlang$core$builtin$Map$$set$0(m$918, _tmp$2468, _tmp$2469);
      _tmp$2473 = _i$920 + 1;
      _i$920 = _tmp$2473;
      continue;
    } else {
      moonbit_decref(arr$916.$0);
    }
    break;
  }
  return m$918;
}

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$912,
  moonbit_string_t key$913,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$914
) {
  int32_t _tmp$2464;
  moonbit_incref(key$913);
  _tmp$2464 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$913);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$912, key$913, value$914, _tmp$2464
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$909,
  moonbit_string_t key$910,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$911
) {
  int32_t _tmp$2463;
  moonbit_incref(key$910);
  _tmp$2463 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$910);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$909, key$910, value$911, _tmp$2463
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$906,
  int32_t key$907,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$908
) {
  int32_t _tmp$2462 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$907);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$906, key$907, value$908, _tmp$2462
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$903,
  moonbit_string_t key$904,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$905
) {
  int32_t _tmp$2461;
  moonbit_incref(key$904);
  _tmp$2461 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$904);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$903, key$904, value$905, _tmp$2461
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$893
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3602 =
    self$893->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$892 =
    _field$3602;
  int32_t capacity$2460 = self$893->$2;
  int32_t new_capacity$894 = capacity$2460 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2455 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2454 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$894, _tmp$2455
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$3601 =
    self$893->$0;
  int32_t _tmp$2456;
  int32_t capacity$2458;
  int32_t _tmp$2457;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2459;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3600;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$895;
  if (old_head$892) {
    moonbit_incref(old_head$892);
  }
  moonbit_decref(_old$3601);
  self$893->$0 = _tmp$2454;
  self$893->$2 = new_capacity$894;
  _tmp$2456 = new_capacity$894 - 1;
  self$893->$3 = _tmp$2456;
  capacity$2458 = self$893->$2;
  _tmp$2457 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2458);
  self$893->$4 = _tmp$2457;
  self$893->$1 = 0;
  _tmp$2459 = 0;
  _old$3600 = self$893->$5;
  if (_old$3600) {
    moonbit_decref(_old$3600);
  }
  self$893->$5 = _tmp$2459;
  self$893->$6 = -1;
  _param$895 = old_head$892;
  while (1) {
    if (_param$895 == 0) {
      if (_param$895) {
        moonbit_decref(_param$895);
      }
      moonbit_decref(self$893);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$896 =
        _param$895;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$897 =
        _Some$896;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3599 =
        _x$897->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$898 =
        _field$3599;
      moonbit_string_t _field$3598 = _x$897->$4;
      moonbit_string_t _key$899 = _field$3598;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3597 =
        _x$897->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$900 =
        _field$3597;
      int32_t _field$3596 = _x$897->$3;
      int32_t _cnt$3852 = Moonbit_object_header(_x$897)->rc;
      int32_t _hash$901;
      if (_cnt$3852 > 1) {
        int32_t _new_cnt$3853;
        moonbit_incref(_value$900);
        moonbit_incref(_key$899);
        if (_next$898) {
          moonbit_incref(_next$898);
        }
        _new_cnt$3853 = _cnt$3852 - 1;
        Moonbit_object_header(_x$897)->rc = _new_cnt$3853;
      } else if (_cnt$3852 == 1) {
        moonbit_free(_x$897);
      }
      _hash$901 = _field$3596;
      moonbit_incref(self$893);
      $$moonbitlang$core$builtin$Map$$set_with_hash$3(
        self$893, _key$899, _value$900, _hash$901
      );
      _param$895 = _next$898;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$882
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3609 =
    self$882->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$881 =
    _field$3609;
  int32_t capacity$2453 = self$882->$2;
  int32_t new_capacity$883 = capacity$2453 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2448 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2447 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$883, _tmp$2448
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$3608 =
    self$882->$0;
  int32_t _tmp$2449;
  int32_t capacity$2451;
  int32_t _tmp$2450;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2452;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3607;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$884;
  if (old_head$881) {
    moonbit_incref(old_head$881);
  }
  moonbit_decref(_old$3608);
  self$882->$0 = _tmp$2447;
  self$882->$2 = new_capacity$883;
  _tmp$2449 = new_capacity$883 - 1;
  self$882->$3 = _tmp$2449;
  capacity$2451 = self$882->$2;
  _tmp$2450 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2451);
  self$882->$4 = _tmp$2450;
  self$882->$1 = 0;
  _tmp$2452 = 0;
  _old$3607 = self$882->$5;
  if (_old$3607) {
    moonbit_decref(_old$3607);
  }
  self$882->$5 = _tmp$2452;
  self$882->$6 = -1;
  _param$884 = old_head$881;
  while (1) {
    if (_param$884 == 0) {
      if (_param$884) {
        moonbit_decref(_param$884);
      }
      moonbit_decref(self$882);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$885 =
        _param$884;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$886 =
        _Some$885;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3606 =
        _x$886->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$887 =
        _field$3606;
      moonbit_string_t _field$3605 = _x$886->$4;
      moonbit_string_t _key$888 = _field$3605;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3604 =
        _x$886->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$889 =
        _field$3604;
      int32_t _field$3603 = _x$886->$3;
      int32_t _cnt$3854 = Moonbit_object_header(_x$886)->rc;
      int32_t _hash$890;
      if (_cnt$3854 > 1) {
        int32_t _new_cnt$3855;
        moonbit_incref(_value$889);
        moonbit_incref(_key$888);
        if (_next$887) {
          moonbit_incref(_next$887);
        }
        _new_cnt$3855 = _cnt$3854 - 1;
        Moonbit_object_header(_x$886)->rc = _new_cnt$3855;
      } else if (_cnt$3854 == 1) {
        moonbit_free(_x$886);
      }
      _hash$890 = _field$3603;
      moonbit_incref(self$882);
      $$moonbitlang$core$builtin$Map$$set_with_hash$2(
        self$882, _key$888, _value$889, _hash$890
      );
      _param$884 = _next$887;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$871
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3615 =
    self$871->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$870 =
    _field$3615;
  int32_t capacity$2446 = self$871->$2;
  int32_t new_capacity$872 = capacity$2446 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2441 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$2440 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$872, _tmp$2441
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$3614 =
    self$871->$0;
  int32_t _tmp$2442;
  int32_t capacity$2444;
  int32_t _tmp$2443;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2445;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3613;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$873;
  if (old_head$870) {
    moonbit_incref(old_head$870);
  }
  moonbit_decref(_old$3614);
  self$871->$0 = _tmp$2440;
  self$871->$2 = new_capacity$872;
  _tmp$2442 = new_capacity$872 - 1;
  self$871->$3 = _tmp$2442;
  capacity$2444 = self$871->$2;
  _tmp$2443 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2444);
  self$871->$4 = _tmp$2443;
  self$871->$1 = 0;
  _tmp$2445 = 0;
  _old$3613 = self$871->$5;
  if (_old$3613) {
    moonbit_decref(_old$3613);
  }
  self$871->$5 = _tmp$2445;
  self$871->$6 = -1;
  _param$873 = old_head$870;
  while (1) {
    if (_param$873 == 0) {
      if (_param$873) {
        moonbit_decref(_param$873);
      }
      moonbit_decref(self$871);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$874 =
        _param$873;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _x$875 =
        _Some$874;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3612 =
        _x$875->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$876 =
        _field$3612;
      int32_t _key$877 = _x$875->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3611 =
        _x$875->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$878 =
        _field$3611;
      int32_t _field$3610 = _x$875->$3;
      int32_t _cnt$3856 = Moonbit_object_header(_x$875)->rc;
      int32_t _hash$879;
      if (_cnt$3856 > 1) {
        int32_t _new_cnt$3857;
        moonbit_incref(_value$878);
        if (_next$876) {
          moonbit_incref(_next$876);
        }
        _new_cnt$3857 = _cnt$3856 - 1;
        Moonbit_object_header(_x$875)->rc = _new_cnt$3857;
      } else if (_cnt$3856 == 1) {
        moonbit_free(_x$875);
      }
      _hash$879 = _field$3610;
      moonbit_incref(self$871);
      $$moonbitlang$core$builtin$Map$$set_with_hash$1(
        self$871, _key$877, _value$878, _hash$879
      );
      _param$873 = _next$876;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$860
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3622 =
    self$860->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$859 =
    _field$3622;
  int32_t capacity$2439 = self$860->$2;
  int32_t new_capacity$861 = capacity$2439 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2434 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2433 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$861, _tmp$2434
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$3621 =
    self$860->$0;
  int32_t _tmp$2435;
  int32_t capacity$2437;
  int32_t _tmp$2436;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2438;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3620;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$862;
  if (old_head$859) {
    moonbit_incref(old_head$859);
  }
  moonbit_decref(_old$3621);
  self$860->$0 = _tmp$2433;
  self$860->$2 = new_capacity$861;
  _tmp$2435 = new_capacity$861 - 1;
  self$860->$3 = _tmp$2435;
  capacity$2437 = self$860->$2;
  _tmp$2436 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2437);
  self$860->$4 = _tmp$2436;
  self$860->$1 = 0;
  _tmp$2438 = 0;
  _old$3620 = self$860->$5;
  if (_old$3620) {
    moonbit_decref(_old$3620);
  }
  self$860->$5 = _tmp$2438;
  self$860->$6 = -1;
  _param$862 = old_head$859;
  while (1) {
    if (_param$862 == 0) {
      if (_param$862) {
        moonbit_decref(_param$862);
      }
      moonbit_decref(self$860);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$863 =
        _param$862;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$864 =
        _Some$863;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3619 =
        _x$864->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$865 =
        _field$3619;
      moonbit_string_t _field$3618 = _x$864->$4;
      moonbit_string_t _key$866 = _field$3618;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3617 =
        _x$864->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$867 =
        _field$3617;
      int32_t _field$3616 = _x$864->$3;
      int32_t _cnt$3858 = Moonbit_object_header(_x$864)->rc;
      int32_t _hash$868;
      if (_cnt$3858 > 1) {
        int32_t _new_cnt$3859;
        moonbit_incref(_value$867);
        moonbit_incref(_key$866);
        if (_next$865) {
          moonbit_incref(_next$865);
        }
        _new_cnt$3859 = _cnt$3858 - 1;
        Moonbit_object_header(_x$864)->rc = _new_cnt$3859;
      } else if (_cnt$3858 == 1) {
        moonbit_free(_x$864);
      }
      _hash$868 = _field$3616;
      moonbit_incref(self$860);
      $$moonbitlang$core$builtin$Map$$set_with_hash$0(
        self$860, _key$866, _value$867, _hash$868
      );
      _param$862 = _next$865;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$843,
  moonbit_string_t key$852,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$853,
  int32_t hash$851
) {
  int32_t size$2419 = self$843->$1;
  int32_t grow_at$2420 = self$843->$4;
  int32_t capacity_mask$2432;
  int32_t _tmp$2431;
  struct $$3c$Int$2a$Int$3e$* _bind$844;
  int32_t psl$845;
  int32_t idx$846;
  int32_t _idx$854;
  int32_t _field$3623;
  int32_t _psl$855;
  int32_t _bind$856;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$857;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$858;
  if (size$2419 >= grow_at$2420) {
    moonbit_incref(self$843);
    $$moonbitlang$core$builtin$Map$$grow$3(self$843);
  }
  capacity_mask$2432 = self$843->$3;
  _tmp$2431 = hash$851 & capacity_mask$2432;
  psl$845 = 0;
  idx$846 = _tmp$2431;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3628 =
      self$843->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2430 =
      _field$3628;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3627;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$847;
    if (idx$846 < 0 || idx$846 >= Moonbit_array_length(entries$2430)) {
      moonbit_panic();
    }
    _tmp$3627
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2430[
        idx$846
      ];
    _bind$847 = _tmp$3627;
    if (_bind$847 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2421 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2421)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2421->$0 = idx$846;
      _tuple$2421->$1 = psl$845;
      _bind$844 = _tuple$2421;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$849 =
        _bind$847;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$850 =
        _Some$849;
      int32_t hash$2423 = _curr_entry$850->$3;
      int32_t _if_result$4089;
      int32_t psl$2424;
      int32_t _tmp$2426;
      int32_t _tmp$2428;
      int32_t capacity_mask$2429;
      int32_t _tmp$2427;
      if (hash$2423 == hash$851) {
        moonbit_string_t _field$3626 = _curr_entry$850->$4;
        moonbit_string_t key$2422 = _field$3626;
        int32_t _tmp$3625 = moonbit_val_array_equal(key$2422, key$852);
        _if_result$4089 = _tmp$3625;
      } else {
        _if_result$4089 = 0;
      }
      if (_if_result$4089) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3624;
        moonbit_incref(_curr_entry$850);
        moonbit_decref(key$852);
        moonbit_decref(self$843);
        _old$3624 = _curr_entry$850->$5;
        moonbit_decref(_old$3624);
        _curr_entry$850->$5 = value$853;
        moonbit_decref(_curr_entry$850);
        return 0;
      } else {
        moonbit_incref(_curr_entry$850);
      }
      psl$2424 = _curr_entry$850->$2;
      if (psl$845 > psl$2424) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2425;
        moonbit_incref(self$843);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$843, idx$846, _curr_entry$850
        );
        _tuple$2425
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2425)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2425->$0 = idx$846;
        _tuple$2425->$1 = psl$845;
        _bind$844 = _tuple$2425;
        break;
      } else {
        moonbit_decref(_curr_entry$850);
      }
      _tmp$2426 = psl$845 + 1;
      _tmp$2428 = idx$846 + 1;
      capacity_mask$2429 = self$843->$3;
      _tmp$2427 = _tmp$2428 & capacity_mask$2429;
      psl$845 = _tmp$2426;
      idx$846 = _tmp$2427;
      continue;
    }
    break;
  }
  _idx$854 = _bind$844->$0;
  _field$3623 = _bind$844->$1;
  moonbit_decref(_bind$844);
  _psl$855 = _field$3623;
  _bind$856 = self$843->$6;
  _bind$857 = 0;
  entry$858
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$858)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$858->$0 = _bind$856;
  entry$858->$1 = _bind$857;
  entry$858->$2 = _psl$855;
  entry$858->$3 = hash$851;
  entry$858->$4 = key$852;
  entry$858->$5 = value$853;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
    self$843, _idx$854, entry$858
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$827,
  moonbit_string_t key$836,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$837,
  int32_t hash$835
) {
  int32_t size$2405 = self$827->$1;
  int32_t grow_at$2406 = self$827->$4;
  int32_t capacity_mask$2418;
  int32_t _tmp$2417;
  struct $$3c$Int$2a$Int$3e$* _bind$828;
  int32_t psl$829;
  int32_t idx$830;
  int32_t _idx$838;
  int32_t _field$3629;
  int32_t _psl$839;
  int32_t _bind$840;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$841;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$842;
  if (size$2405 >= grow_at$2406) {
    moonbit_incref(self$827);
    $$moonbitlang$core$builtin$Map$$grow$2(self$827);
  }
  capacity_mask$2418 = self$827->$3;
  _tmp$2417 = hash$835 & capacity_mask$2418;
  psl$829 = 0;
  idx$830 = _tmp$2417;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3634 =
      self$827->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2416 =
      _field$3634;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3633;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$831;
    if (idx$830 < 0 || idx$830 >= Moonbit_array_length(entries$2416)) {
      moonbit_panic();
    }
    _tmp$3633
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2416[
        idx$830
      ];
    _bind$831 = _tmp$3633;
    if (_bind$831 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2407 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2407)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2407->$0 = idx$830;
      _tuple$2407->$1 = psl$829;
      _bind$828 = _tuple$2407;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$833 =
        _bind$831;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$834 =
        _Some$833;
      int32_t hash$2409 = _curr_entry$834->$3;
      int32_t _if_result$4091;
      int32_t psl$2410;
      int32_t _tmp$2412;
      int32_t _tmp$2414;
      int32_t capacity_mask$2415;
      int32_t _tmp$2413;
      if (hash$2409 == hash$835) {
        moonbit_string_t _field$3632 = _curr_entry$834->$4;
        moonbit_string_t key$2408 = _field$3632;
        int32_t _tmp$3631 = moonbit_val_array_equal(key$2408, key$836);
        _if_result$4091 = _tmp$3631;
      } else {
        _if_result$4091 = 0;
      }
      if (_if_result$4091) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3630;
        moonbit_incref(_curr_entry$834);
        moonbit_decref(key$836);
        moonbit_decref(self$827);
        _old$3630 = _curr_entry$834->$5;
        moonbit_decref(_old$3630);
        _curr_entry$834->$5 = value$837;
        moonbit_decref(_curr_entry$834);
        return 0;
      } else {
        moonbit_incref(_curr_entry$834);
      }
      psl$2410 = _curr_entry$834->$2;
      if (psl$829 > psl$2410) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2411;
        moonbit_incref(self$827);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$827, idx$830, _curr_entry$834
        );
        _tuple$2411
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2411)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2411->$0 = idx$830;
        _tuple$2411->$1 = psl$829;
        _bind$828 = _tuple$2411;
        break;
      } else {
        moonbit_decref(_curr_entry$834);
      }
      _tmp$2412 = psl$829 + 1;
      _tmp$2414 = idx$830 + 1;
      capacity_mask$2415 = self$827->$3;
      _tmp$2413 = _tmp$2414 & capacity_mask$2415;
      psl$829 = _tmp$2412;
      idx$830 = _tmp$2413;
      continue;
    }
    break;
  }
  _idx$838 = _bind$828->$0;
  _field$3629 = _bind$828->$1;
  moonbit_decref(_bind$828);
  _psl$839 = _field$3629;
  _bind$840 = self$827->$6;
  _bind$841 = 0;
  entry$842
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$842)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$842->$0 = _bind$840;
  entry$842->$1 = _bind$841;
  entry$842->$2 = _psl$839;
  entry$842->$3 = hash$835;
  entry$842->$4 = key$836;
  entry$842->$5 = value$837;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
    self$827, _idx$838, entry$842
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$811,
  int32_t key$820,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$821,
  int32_t hash$819
) {
  int32_t size$2391 = self$811->$1;
  int32_t grow_at$2392 = self$811->$4;
  int32_t capacity_mask$2404;
  int32_t _tmp$2403;
  struct $$3c$Int$2a$Int$3e$* _bind$812;
  int32_t psl$813;
  int32_t idx$814;
  int32_t _idx$822;
  int32_t _field$3635;
  int32_t _psl$823;
  int32_t _bind$824;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$825;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$826;
  if (size$2391 >= grow_at$2392) {
    moonbit_incref(self$811);
    $$moonbitlang$core$builtin$Map$$grow$1(self$811);
  }
  capacity_mask$2404 = self$811->$3;
  _tmp$2403 = hash$819 & capacity_mask$2404;
  psl$813 = 0;
  idx$814 = _tmp$2403;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3638 =
      self$811->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2402 =
      _field$3638;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3637;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$815;
    if (idx$814 < 0 || idx$814 >= Moonbit_array_length(entries$2402)) {
      moonbit_panic();
    }
    _tmp$3637
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2402[
        idx$814
      ];
    _bind$815 = _tmp$3637;
    if (_bind$815 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2393 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2393)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2393->$0 = idx$814;
      _tuple$2393->$1 = psl$813;
      _bind$812 = _tuple$2393;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$817 =
        _bind$815;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$818 =
        _Some$817;
      int32_t hash$2395 = _curr_entry$818->$3;
      int32_t _if_result$4093;
      int32_t psl$2396;
      int32_t _tmp$2398;
      int32_t _tmp$2400;
      int32_t capacity_mask$2401;
      int32_t _tmp$2399;
      if (hash$2395 == hash$819) {
        int32_t key$2394 = _curr_entry$818->$4;
        _if_result$4093 = key$2394 == key$820;
      } else {
        _if_result$4093 = 0;
      }
      if (_if_result$4093) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$3636;
        moonbit_incref(_curr_entry$818);
        moonbit_decref(self$811);
        _old$3636 = _curr_entry$818->$5;
        moonbit_decref(_old$3636);
        _curr_entry$818->$5 = value$821;
        moonbit_decref(_curr_entry$818);
        return 0;
      } else {
        moonbit_incref(_curr_entry$818);
      }
      psl$2396 = _curr_entry$818->$2;
      if (psl$813 > psl$2396) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2397;
        moonbit_incref(self$811);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$811, idx$814, _curr_entry$818
        );
        _tuple$2397
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2397)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2397->$0 = idx$814;
        _tuple$2397->$1 = psl$813;
        _bind$812 = _tuple$2397;
        break;
      } else {
        moonbit_decref(_curr_entry$818);
      }
      _tmp$2398 = psl$813 + 1;
      _tmp$2400 = idx$814 + 1;
      capacity_mask$2401 = self$811->$3;
      _tmp$2399 = _tmp$2400 & capacity_mask$2401;
      psl$813 = _tmp$2398;
      idx$814 = _tmp$2399;
      continue;
    }
    break;
  }
  _idx$822 = _bind$812->$0;
  _field$3635 = _bind$812->$1;
  moonbit_decref(_bind$812);
  _psl$823 = _field$3635;
  _bind$824 = self$811->$6;
  _bind$825 = 0;
  entry$826
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$826)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      2,
      0
  );
  entry$826->$0 = _bind$824;
  entry$826->$1 = _bind$825;
  entry$826->$2 = _psl$823;
  entry$826->$3 = hash$819;
  entry$826->$4 = key$820;
  entry$826->$5 = value$821;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
    self$811, _idx$822, entry$826
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$795,
  moonbit_string_t key$804,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$805,
  int32_t hash$803
) {
  int32_t size$2377 = self$795->$1;
  int32_t grow_at$2378 = self$795->$4;
  int32_t capacity_mask$2390;
  int32_t _tmp$2389;
  struct $$3c$Int$2a$Int$3e$* _bind$796;
  int32_t psl$797;
  int32_t idx$798;
  int32_t _idx$806;
  int32_t _field$3639;
  int32_t _psl$807;
  int32_t _bind$808;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$809;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$810;
  if (size$2377 >= grow_at$2378) {
    moonbit_incref(self$795);
    $$moonbitlang$core$builtin$Map$$grow$0(self$795);
  }
  capacity_mask$2390 = self$795->$3;
  _tmp$2389 = hash$803 & capacity_mask$2390;
  psl$797 = 0;
  idx$798 = _tmp$2389;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3644 =
      self$795->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2388 =
      _field$3644;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3643;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$799;
    if (idx$798 < 0 || idx$798 >= Moonbit_array_length(entries$2388)) {
      moonbit_panic();
    }
    _tmp$3643
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2388[
        idx$798
      ];
    _bind$799 = _tmp$3643;
    if (_bind$799 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2379 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2379)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2379->$0 = idx$798;
      _tuple$2379->$1 = psl$797;
      _bind$796 = _tuple$2379;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$801 =
        _bind$799;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$802 =
        _Some$801;
      int32_t hash$2381 = _curr_entry$802->$3;
      int32_t _if_result$4095;
      int32_t psl$2382;
      int32_t _tmp$2384;
      int32_t _tmp$2386;
      int32_t capacity_mask$2387;
      int32_t _tmp$2385;
      if (hash$2381 == hash$803) {
        moonbit_string_t _field$3642 = _curr_entry$802->$4;
        moonbit_string_t key$2380 = _field$3642;
        int32_t _tmp$3641 = moonbit_val_array_equal(key$2380, key$804);
        _if_result$4095 = _tmp$3641;
      } else {
        _if_result$4095 = 0;
      }
      if (_if_result$4095) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3640;
        moonbit_incref(_curr_entry$802);
        moonbit_decref(key$804);
        moonbit_decref(self$795);
        _old$3640 = _curr_entry$802->$5;
        moonbit_decref(_old$3640);
        _curr_entry$802->$5 = value$805;
        moonbit_decref(_curr_entry$802);
        return 0;
      } else {
        moonbit_incref(_curr_entry$802);
      }
      psl$2382 = _curr_entry$802->$2;
      if (psl$797 > psl$2382) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2383;
        moonbit_incref(self$795);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$795, idx$798, _curr_entry$802
        );
        _tuple$2383
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2383)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2383->$0 = idx$798;
        _tuple$2383->$1 = psl$797;
        _bind$796 = _tuple$2383;
        break;
      } else {
        moonbit_decref(_curr_entry$802);
      }
      _tmp$2384 = psl$797 + 1;
      _tmp$2386 = idx$798 + 1;
      capacity_mask$2387 = self$795->$3;
      _tmp$2385 = _tmp$2386 & capacity_mask$2387;
      psl$797 = _tmp$2384;
      idx$798 = _tmp$2385;
      continue;
    }
    break;
  }
  _idx$806 = _bind$796->$0;
  _field$3639 = _bind$796->$1;
  moonbit_decref(_bind$796);
  _psl$807 = _field$3639;
  _bind$808 = self$795->$6;
  _bind$809 = 0;
  entry$810
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$810)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$810->$0 = _bind$808;
  entry$810->$1 = _bind$809;
  entry$810->$2 = _psl$807;
  entry$810->$3 = hash$803;
  entry$810->$4 = key$804;
  entry$810->$5 = value$805;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
    self$795, _idx$806, entry$810
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$789,
  int32_t idx$794,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$793
) {
  int32_t psl$2376 = entry$793->$2;
  int32_t _tmp$2372 = psl$2376 + 1;
  int32_t _tmp$2374 = idx$794 + 1;
  int32_t capacity_mask$2375 = self$789->$3;
  int32_t _tmp$2373 = _tmp$2374 & capacity_mask$2375;
  int32_t psl$785 = _tmp$2372;
  int32_t idx$786 = _tmp$2373;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$787 =
    entry$793;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3646 =
      self$789->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2371 =
      _field$3646;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3645;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$788;
    if (idx$786 < 0 || idx$786 >= Moonbit_array_length(entries$2371)) {
      moonbit_panic();
    }
    _tmp$3645
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2371[
        idx$786
      ];
    _bind$788 = _tmp$3645;
    if (_bind$788 == 0) {
      entry$787->$2 = psl$785;
      $$moonbitlang$core$builtin$Map$$set_entry$3(
        self$789, entry$787, idx$786
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$791 =
        _bind$788;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$792 =
        _Some$791;
      int32_t psl$2361 = _curr_entry$792->$2;
      if (psl$785 > psl$2361) {
        int32_t psl$2366;
        int32_t _tmp$2362;
        int32_t _tmp$2364;
        int32_t capacity_mask$2365;
        int32_t _tmp$2363;
        entry$787->$2 = psl$785;
        moonbit_incref(_curr_entry$792);
        moonbit_incref(self$789);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$789, entry$787, idx$786
        );
        psl$2366 = _curr_entry$792->$2;
        _tmp$2362 = psl$2366 + 1;
        _tmp$2364 = idx$786 + 1;
        capacity_mask$2365 = self$789->$3;
        _tmp$2363 = _tmp$2364 & capacity_mask$2365;
        psl$785 = _tmp$2362;
        idx$786 = _tmp$2363;
        entry$787 = _curr_entry$792;
        continue;
      } else {
        int32_t _tmp$2367 = psl$785 + 1;
        int32_t _tmp$2369 = idx$786 + 1;
        int32_t capacity_mask$2370 = self$789->$3;
        int32_t _tmp$2368 = _tmp$2369 & capacity_mask$2370;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4097 =
          entry$787;
        psl$785 = _tmp$2367;
        idx$786 = _tmp$2368;
        entry$787 = _tmp$4097;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$779,
  int32_t idx$784,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$783
) {
  int32_t psl$2360 = entry$783->$2;
  int32_t _tmp$2356 = psl$2360 + 1;
  int32_t _tmp$2358 = idx$784 + 1;
  int32_t capacity_mask$2359 = self$779->$3;
  int32_t _tmp$2357 = _tmp$2358 & capacity_mask$2359;
  int32_t psl$775 = _tmp$2356;
  int32_t idx$776 = _tmp$2357;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$777 =
    entry$783;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3648 =
      self$779->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2355 =
      _field$3648;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3647;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$778;
    if (idx$776 < 0 || idx$776 >= Moonbit_array_length(entries$2355)) {
      moonbit_panic();
    }
    _tmp$3647
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2355[
        idx$776
      ];
    _bind$778 = _tmp$3647;
    if (_bind$778 == 0) {
      entry$777->$2 = psl$775;
      $$moonbitlang$core$builtin$Map$$set_entry$2(
        self$779, entry$777, idx$776
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$781 =
        _bind$778;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$782 =
        _Some$781;
      int32_t psl$2345 = _curr_entry$782->$2;
      if (psl$775 > psl$2345) {
        int32_t psl$2350;
        int32_t _tmp$2346;
        int32_t _tmp$2348;
        int32_t capacity_mask$2349;
        int32_t _tmp$2347;
        entry$777->$2 = psl$775;
        moonbit_incref(_curr_entry$782);
        moonbit_incref(self$779);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$779, entry$777, idx$776
        );
        psl$2350 = _curr_entry$782->$2;
        _tmp$2346 = psl$2350 + 1;
        _tmp$2348 = idx$776 + 1;
        capacity_mask$2349 = self$779->$3;
        _tmp$2347 = _tmp$2348 & capacity_mask$2349;
        psl$775 = _tmp$2346;
        idx$776 = _tmp$2347;
        entry$777 = _curr_entry$782;
        continue;
      } else {
        int32_t _tmp$2351 = psl$775 + 1;
        int32_t _tmp$2353 = idx$776 + 1;
        int32_t capacity_mask$2354 = self$779->$3;
        int32_t _tmp$2352 = _tmp$2353 & capacity_mask$2354;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4099 =
          entry$777;
        psl$775 = _tmp$2351;
        idx$776 = _tmp$2352;
        entry$777 = _tmp$4099;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$769,
  int32_t idx$774,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$773
) {
  int32_t psl$2344 = entry$773->$2;
  int32_t _tmp$2340 = psl$2344 + 1;
  int32_t _tmp$2342 = idx$774 + 1;
  int32_t capacity_mask$2343 = self$769->$3;
  int32_t _tmp$2341 = _tmp$2342 & capacity_mask$2343;
  int32_t psl$765 = _tmp$2340;
  int32_t idx$766 = _tmp$2341;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$767 =
    entry$773;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3650 =
      self$769->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2339 =
      _field$3650;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3649;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$768;
    if (idx$766 < 0 || idx$766 >= Moonbit_array_length(entries$2339)) {
      moonbit_panic();
    }
    _tmp$3649
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2339[
        idx$766
      ];
    _bind$768 = _tmp$3649;
    if (_bind$768 == 0) {
      entry$767->$2 = psl$765;
      $$moonbitlang$core$builtin$Map$$set_entry$1(
        self$769, entry$767, idx$766
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$771 =
        _bind$768;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$772 =
        _Some$771;
      int32_t psl$2329 = _curr_entry$772->$2;
      if (psl$765 > psl$2329) {
        int32_t psl$2334;
        int32_t _tmp$2330;
        int32_t _tmp$2332;
        int32_t capacity_mask$2333;
        int32_t _tmp$2331;
        entry$767->$2 = psl$765;
        moonbit_incref(_curr_entry$772);
        moonbit_incref(self$769);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$769, entry$767, idx$766
        );
        psl$2334 = _curr_entry$772->$2;
        _tmp$2330 = psl$2334 + 1;
        _tmp$2332 = idx$766 + 1;
        capacity_mask$2333 = self$769->$3;
        _tmp$2331 = _tmp$2332 & capacity_mask$2333;
        psl$765 = _tmp$2330;
        idx$766 = _tmp$2331;
        entry$767 = _curr_entry$772;
        continue;
      } else {
        int32_t _tmp$2335 = psl$765 + 1;
        int32_t _tmp$2337 = idx$766 + 1;
        int32_t capacity_mask$2338 = self$769->$3;
        int32_t _tmp$2336 = _tmp$2337 & capacity_mask$2338;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4101 =
          entry$767;
        psl$765 = _tmp$2335;
        idx$766 = _tmp$2336;
        entry$767 = _tmp$4101;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$759,
  int32_t idx$764,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$763
) {
  int32_t psl$2328 = entry$763->$2;
  int32_t _tmp$2324 = psl$2328 + 1;
  int32_t _tmp$2326 = idx$764 + 1;
  int32_t capacity_mask$2327 = self$759->$3;
  int32_t _tmp$2325 = _tmp$2326 & capacity_mask$2327;
  int32_t psl$755 = _tmp$2324;
  int32_t idx$756 = _tmp$2325;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$757 =
    entry$763;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3652 =
      self$759->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2323 =
      _field$3652;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3651;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$758;
    if (idx$756 < 0 || idx$756 >= Moonbit_array_length(entries$2323)) {
      moonbit_panic();
    }
    _tmp$3651
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2323[
        idx$756
      ];
    _bind$758 = _tmp$3651;
    if (_bind$758 == 0) {
      entry$757->$2 = psl$755;
      $$moonbitlang$core$builtin$Map$$set_entry$0(
        self$759, entry$757, idx$756
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$761 =
        _bind$758;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$762 =
        _Some$761;
      int32_t psl$2313 = _curr_entry$762->$2;
      if (psl$755 > psl$2313) {
        int32_t psl$2318;
        int32_t _tmp$2314;
        int32_t _tmp$2316;
        int32_t capacity_mask$2317;
        int32_t _tmp$2315;
        entry$757->$2 = psl$755;
        moonbit_incref(_curr_entry$762);
        moonbit_incref(self$759);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$759, entry$757, idx$756
        );
        psl$2318 = _curr_entry$762->$2;
        _tmp$2314 = psl$2318 + 1;
        _tmp$2316 = idx$756 + 1;
        capacity_mask$2317 = self$759->$3;
        _tmp$2315 = _tmp$2316 & capacity_mask$2317;
        psl$755 = _tmp$2314;
        idx$756 = _tmp$2315;
        entry$757 = _curr_entry$762;
        continue;
      } else {
        int32_t _tmp$2319 = psl$755 + 1;
        int32_t _tmp$2321 = idx$756 + 1;
        int32_t capacity_mask$2322 = self$759->$3;
        int32_t _tmp$2320 = _tmp$2321 & capacity_mask$2322;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4103 =
          entry$757;
        psl$755 = _tmp$2319;
        idx$756 = _tmp$2320;
        entry$757 = _tmp$4103;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$749,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$751,
  int32_t new_idx$750
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3655 =
    self$749->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2311 =
    _field$3655;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2312;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3654;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3653;
  int32_t _cnt$3860;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$752;
  moonbit_incref(entry$751);
  _tmp$2312 = entry$751;
  if (new_idx$750 < 0 || new_idx$750 >= Moonbit_array_length(entries$2311)) {
    moonbit_panic();
  }
  _old$3654
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2311[
      new_idx$750
    ];
  if (_old$3654) {
    moonbit_decref(_old$3654);
  }
  entries$2311[new_idx$750] = _tmp$2312;
  _field$3653 = entry$751->$1;
  _cnt$3860 = Moonbit_object_header(entry$751)->rc;
  if (_cnt$3860 > 1) {
    int32_t _new_cnt$3863;
    if (_field$3653) {
      moonbit_incref(_field$3653);
    }
    _new_cnt$3863 = _cnt$3860 - 1;
    Moonbit_object_header(entry$751)->rc = _new_cnt$3863;
  } else if (_cnt$3860 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3862 =
      entry$751->$5;
    moonbit_string_t _field$3861;
    moonbit_decref(_field$3862);
    _field$3861 = entry$751->$4;
    moonbit_decref(_field$3861);
    moonbit_free(entry$751);
  }
  _bind$752 = _field$3653;
  if (_bind$752 == 0) {
    if (_bind$752) {
      moonbit_decref(_bind$752);
    }
    self$749->$6 = new_idx$750;
    moonbit_decref(self$749);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$753;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$754;
    moonbit_decref(self$749);
    _Some$753 = _bind$752;
    _next$754 = _Some$753;
    _next$754->$0 = new_idx$750;
    moonbit_decref(_next$754);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$743,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$745,
  int32_t new_idx$744
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3658 =
    self$743->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2309 =
    _field$3658;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2310;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3657;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3656;
  int32_t _cnt$3864;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$746;
  moonbit_incref(entry$745);
  _tmp$2310 = entry$745;
  if (new_idx$744 < 0 || new_idx$744 >= Moonbit_array_length(entries$2309)) {
    moonbit_panic();
  }
  _old$3657
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2309[
      new_idx$744
    ];
  if (_old$3657) {
    moonbit_decref(_old$3657);
  }
  entries$2309[new_idx$744] = _tmp$2310;
  _field$3656 = entry$745->$1;
  _cnt$3864 = Moonbit_object_header(entry$745)->rc;
  if (_cnt$3864 > 1) {
    int32_t _new_cnt$3867;
    if (_field$3656) {
      moonbit_incref(_field$3656);
    }
    _new_cnt$3867 = _cnt$3864 - 1;
    Moonbit_object_header(entry$745)->rc = _new_cnt$3867;
  } else if (_cnt$3864 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3866 =
      entry$745->$5;
    moonbit_string_t _field$3865;
    moonbit_decref(_field$3866);
    _field$3865 = entry$745->$4;
    moonbit_decref(_field$3865);
    moonbit_free(entry$745);
  }
  _bind$746 = _field$3656;
  if (_bind$746 == 0) {
    if (_bind$746) {
      moonbit_decref(_bind$746);
    }
    self$743->$6 = new_idx$744;
    moonbit_decref(self$743);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$747;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$748;
    moonbit_decref(self$743);
    _Some$747 = _bind$746;
    _next$748 = _Some$747;
    _next$748->$0 = new_idx$744;
    moonbit_decref(_next$748);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$737,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$739,
  int32_t new_idx$738
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3661 =
    self$737->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2307 =
    _field$3661;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2308;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3660;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3659;
  int32_t _cnt$3868;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$740;
  moonbit_incref(entry$739);
  _tmp$2308 = entry$739;
  if (new_idx$738 < 0 || new_idx$738 >= Moonbit_array_length(entries$2307)) {
    moonbit_panic();
  }
  _old$3660
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2307[
      new_idx$738
    ];
  if (_old$3660) {
    moonbit_decref(_old$3660);
  }
  entries$2307[new_idx$738] = _tmp$2308;
  _field$3659 = entry$739->$1;
  _cnt$3868 = Moonbit_object_header(entry$739)->rc;
  if (_cnt$3868 > 1) {
    int32_t _new_cnt$3870;
    if (_field$3659) {
      moonbit_incref(_field$3659);
    }
    _new_cnt$3870 = _cnt$3868 - 1;
    Moonbit_object_header(entry$739)->rc = _new_cnt$3870;
  } else if (_cnt$3868 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3869 =
      entry$739->$5;
    moonbit_decref(_field$3869);
    moonbit_free(entry$739);
  }
  _bind$740 = _field$3659;
  if (_bind$740 == 0) {
    if (_bind$740) {
      moonbit_decref(_bind$740);
    }
    self$737->$6 = new_idx$738;
    moonbit_decref(self$737);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$741;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$742;
    moonbit_decref(self$737);
    _Some$741 = _bind$740;
    _next$742 = _Some$741;
    _next$742->$0 = new_idx$738;
    moonbit_decref(_next$742);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$731,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$733,
  int32_t new_idx$732
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3664 =
    self$731->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2305 =
    _field$3664;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2306;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3663;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3662;
  int32_t _cnt$3871;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$734;
  moonbit_incref(entry$733);
  _tmp$2306 = entry$733;
  if (new_idx$732 < 0 || new_idx$732 >= Moonbit_array_length(entries$2305)) {
    moonbit_panic();
  }
  _old$3663
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2305[
      new_idx$732
    ];
  if (_old$3663) {
    moonbit_decref(_old$3663);
  }
  entries$2305[new_idx$732] = _tmp$2306;
  _field$3662 = entry$733->$1;
  _cnt$3871 = Moonbit_object_header(entry$733)->rc;
  if (_cnt$3871 > 1) {
    int32_t _new_cnt$3874;
    if (_field$3662) {
      moonbit_incref(_field$3662);
    }
    _new_cnt$3874 = _cnt$3871 - 1;
    Moonbit_object_header(entry$733)->rc = _new_cnt$3874;
  } else if (_cnt$3871 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3873 =
      entry$733->$5;
    moonbit_string_t _field$3872;
    moonbit_decref(_field$3873);
    _field$3872 = entry$733->$4;
    moonbit_decref(_field$3872);
    moonbit_free(entry$733);
  }
  _bind$734 = _field$3662;
  if (_bind$734 == 0) {
    if (_bind$734) {
      moonbit_decref(_bind$734);
    }
    self$731->$6 = new_idx$732;
    moonbit_decref(self$731);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$735;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$736;
    moonbit_decref(self$731);
    _Some$735 = _bind$734;
    _next$736 = _Some$735;
    _next$736->$0 = new_idx$732;
    moonbit_decref(_next$736);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$728,
  int32_t idx$730,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$729
) {
  int32_t _bind$727 = self$728->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3666;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2301;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2302;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3665;
  int32_t size$2304;
  int32_t _tmp$2303;
  switch (_bind$727) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2296;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3667;
      moonbit_incref(entry$729);
      _tmp$2296 = entry$729;
      _old$3667 = self$728->$5;
      if (_old$3667) {
        moonbit_decref(_old$3667);
      }
      self$728->$5 = _tmp$2296;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3670 =
        self$728->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2300 =
        _field$3670;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3669;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2299;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2297;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2298;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3668;
      if (_bind$727 < 0 || _bind$727 >= Moonbit_array_length(entries$2300)) {
        moonbit_panic();
      }
      _tmp$3669
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2300[
          _bind$727
        ];
      _tmp$2299 = _tmp$3669;
      if (_tmp$2299) {
        moonbit_incref(_tmp$2299);
      }
      _tmp$2297 = $Option$$unwrap$3(_tmp$2299);
      moonbit_incref(entry$729);
      _tmp$2298 = entry$729;
      _old$3668 = _tmp$2297->$1;
      if (_old$3668) {
        moonbit_decref(_old$3668);
      }
      _tmp$2297->$1 = _tmp$2298;
      moonbit_decref(_tmp$2297);
      break;
    }
  }
  self$728->$6 = idx$730;
  _field$3666 = self$728->$0;
  entries$2301 = _field$3666;
  _tmp$2302 = entry$729;
  if (idx$730 < 0 || idx$730 >= Moonbit_array_length(entries$2301)) {
    moonbit_panic();
  }
  _old$3665
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2301[
      idx$730
    ];
  if (_old$3665) {
    moonbit_decref(_old$3665);
  }
  entries$2301[idx$730] = _tmp$2302;
  size$2304 = self$728->$1;
  _tmp$2303 = size$2304 + 1;
  self$728->$1 = _tmp$2303;
  moonbit_decref(self$728);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$724,
  int32_t idx$726,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$725
) {
  int32_t _bind$723 = self$724->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3672;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2292;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2293;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3671;
  int32_t size$2295;
  int32_t _tmp$2294;
  switch (_bind$723) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2287;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3673;
      moonbit_incref(entry$725);
      _tmp$2287 = entry$725;
      _old$3673 = self$724->$5;
      if (_old$3673) {
        moonbit_decref(_old$3673);
      }
      self$724->$5 = _tmp$2287;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3676 =
        self$724->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2291 =
        _field$3676;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3675;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2290;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2288;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2289;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3674;
      if (_bind$723 < 0 || _bind$723 >= Moonbit_array_length(entries$2291)) {
        moonbit_panic();
      }
      _tmp$3675
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2291[
          _bind$723
        ];
      _tmp$2290 = _tmp$3675;
      if (_tmp$2290) {
        moonbit_incref(_tmp$2290);
      }
      _tmp$2288 = $Option$$unwrap$2(_tmp$2290);
      moonbit_incref(entry$725);
      _tmp$2289 = entry$725;
      _old$3674 = _tmp$2288->$1;
      if (_old$3674) {
        moonbit_decref(_old$3674);
      }
      _tmp$2288->$1 = _tmp$2289;
      moonbit_decref(_tmp$2288);
      break;
    }
  }
  self$724->$6 = idx$726;
  _field$3672 = self$724->$0;
  entries$2292 = _field$3672;
  _tmp$2293 = entry$725;
  if (idx$726 < 0 || idx$726 >= Moonbit_array_length(entries$2292)) {
    moonbit_panic();
  }
  _old$3671
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2292[
      idx$726
    ];
  if (_old$3671) {
    moonbit_decref(_old$3671);
  }
  entries$2292[idx$726] = _tmp$2293;
  size$2295 = self$724->$1;
  _tmp$2294 = size$2295 + 1;
  self$724->$1 = _tmp$2294;
  moonbit_decref(self$724);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$720,
  int32_t idx$722,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$721
) {
  int32_t _bind$719 = self$720->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3678;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2283;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2284;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3677;
  int32_t size$2286;
  int32_t _tmp$2285;
  switch (_bind$719) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2278;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3679;
      moonbit_incref(entry$721);
      _tmp$2278 = entry$721;
      _old$3679 = self$720->$5;
      if (_old$3679) {
        moonbit_decref(_old$3679);
      }
      self$720->$5 = _tmp$2278;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3682 =
        self$720->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2282 =
        _field$3682;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3681;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2281;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2279;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2280;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3680;
      if (_bind$719 < 0 || _bind$719 >= Moonbit_array_length(entries$2282)) {
        moonbit_panic();
      }
      _tmp$3681
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2282[
          _bind$719
        ];
      _tmp$2281 = _tmp$3681;
      if (_tmp$2281) {
        moonbit_incref(_tmp$2281);
      }
      _tmp$2279 = $Option$$unwrap$1(_tmp$2281);
      moonbit_incref(entry$721);
      _tmp$2280 = entry$721;
      _old$3680 = _tmp$2279->$1;
      if (_old$3680) {
        moonbit_decref(_old$3680);
      }
      _tmp$2279->$1 = _tmp$2280;
      moonbit_decref(_tmp$2279);
      break;
    }
  }
  self$720->$6 = idx$722;
  _field$3678 = self$720->$0;
  entries$2283 = _field$3678;
  _tmp$2284 = entry$721;
  if (idx$722 < 0 || idx$722 >= Moonbit_array_length(entries$2283)) {
    moonbit_panic();
  }
  _old$3677
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2283[
      idx$722
    ];
  if (_old$3677) {
    moonbit_decref(_old$3677);
  }
  entries$2283[idx$722] = _tmp$2284;
  size$2286 = self$720->$1;
  _tmp$2285 = size$2286 + 1;
  self$720->$1 = _tmp$2285;
  moonbit_decref(self$720);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$716,
  int32_t idx$718,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$717
) {
  int32_t _bind$715 = self$716->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3684;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2274;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2275;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3683;
  int32_t size$2277;
  int32_t _tmp$2276;
  switch (_bind$715) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2269;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3685;
      moonbit_incref(entry$717);
      _tmp$2269 = entry$717;
      _old$3685 = self$716->$5;
      if (_old$3685) {
        moonbit_decref(_old$3685);
      }
      self$716->$5 = _tmp$2269;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3688 =
        self$716->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2273 =
        _field$3688;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3687;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2272;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2270;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2271;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3686;
      if (_bind$715 < 0 || _bind$715 >= Moonbit_array_length(entries$2273)) {
        moonbit_panic();
      }
      _tmp$3687
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2273[
          _bind$715
        ];
      _tmp$2272 = _tmp$3687;
      if (_tmp$2272) {
        moonbit_incref(_tmp$2272);
      }
      _tmp$2270 = $Option$$unwrap$0(_tmp$2272);
      moonbit_incref(entry$717);
      _tmp$2271 = entry$717;
      _old$3686 = _tmp$2270->$1;
      if (_old$3686) {
        moonbit_decref(_old$3686);
      }
      _tmp$2270->$1 = _tmp$2271;
      moonbit_decref(_tmp$2270);
      break;
    }
  }
  self$716->$6 = idx$718;
  _field$3684 = self$716->$0;
  entries$2274 = _field$3684;
  _tmp$2275 = entry$717;
  if (idx$718 < 0 || idx$718 >= Moonbit_array_length(entries$2274)) {
    moonbit_panic();
  }
  _old$3683
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2274[
      idx$718
    ];
  if (_old$3683) {
    moonbit_decref(_old$3683);
  }
  entries$2274[idx$718] = _tmp$2275;
  size$2277 = self$716->$1;
  _tmp$2276 = size$2277 + 1;
  self$716->$1 = _tmp$2276;
  moonbit_decref(self$716);
  return 0;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$710
) {
  int32_t capacity$709 = $Int$$next_power_of_two(capacity$710);
  int32_t _bind$711 = capacity$709 - 1;
  int32_t _bind$712 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$709);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2268 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$713 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$709, _tmp$2268
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$714 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$4104 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4104)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4104->$0 = _bind$713;
  _block$4104->$1 = 0;
  _block$4104->$2 = capacity$709;
  _block$4104->$3 = _bind$711;
  _block$4104->$4 = _bind$712;
  _block$4104->$5 = _bind$714;
  _block$4104->$6 = -1;
  return _block$4104;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$704
) {
  int32_t capacity$703 = $Int$$next_power_of_two(capacity$704);
  int32_t _bind$705 = capacity$703 - 1;
  int32_t _bind$706 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$703);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2267 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$707 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$703, _tmp$2267
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$708 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$4105 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4105)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4105->$0 = _bind$707;
  _block$4105->$1 = 0;
  _block$4105->$2 = capacity$703;
  _block$4105->$3 = _bind$705;
  _block$4105->$4 = _bind$706;
  _block$4105->$5 = _bind$708;
  _block$4105->$6 = -1;
  return _block$4105;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$698
) {
  int32_t capacity$697 = $Int$$next_power_of_two(capacity$698);
  int32_t _bind$699 = capacity$697 - 1;
  int32_t _bind$700 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$697);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2266 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$701 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$697, _tmp$2266
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$702 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$4106 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4106)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4106->$0 = _bind$701;
  _block$4106->$1 = 0;
  _block$4106->$2 = capacity$697;
  _block$4106->$3 = _bind$699;
  _block$4106->$4 = _bind$700;
  _block$4106->$5 = _bind$702;
  _block$4106->$6 = -1;
  return _block$4106;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$692
) {
  int32_t capacity$691 = $Int$$next_power_of_two(capacity$692);
  int32_t _bind$693 = capacity$691 - 1;
  int32_t _bind$694 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$691);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2265 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$695 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$691, _tmp$2265
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$696 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$4107 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4107)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4107->$0 = _bind$695;
  _block$4107->$1 = 0;
  _block$4107->$2 = capacity$691;
  _block$4107->$3 = _bind$693;
  _block$4107->$4 = _bind$694;
  _block$4107->$5 = _bind$696;
  _block$4107->$6 = -1;
  return _block$4107;
}

int32_t $Int$$next_power_of_two(int32_t self$690) {
  if (self$690 >= 0) {
    int32_t _tmp$2264;
    int32_t _tmp$2263;
    int32_t _tmp$2262;
    int32_t _tmp$2261;
    if (self$690 <= 1) {
      return 1;
    }
    if (self$690 > 1073741824) {
      return 1073741824;
    }
    _tmp$2264 = self$690 - 1;
    _tmp$2263 = moonbit_clz32(_tmp$2264);
    _tmp$2262 = _tmp$2263 - 1;
    _tmp$2261 = 2147483647 >> (_tmp$2262 & 31);
    return _tmp$2261 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$689) {
  int32_t _tmp$2260 = capacity$689 * 13;
  return _tmp$2260 / 16;
}

int32_t $Option$$is_none$1(int32_t self$688) {
  return self$688 == -1;
}

int32_t $Option$$is_none$0(int64_t self$687) {
  return self$687 == 4294967296ll;
}

int32_t $Option$$is_some$1(int32_t self$686) {
  int32_t _tmp$2259 = self$686 == -1;
  return !_tmp$2259;
}

int32_t $Option$$is_some$0(int64_t self$685) {
  int32_t _tmp$2258 = self$685 == 4294967296ll;
  return !_tmp$2258;
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$683
) {
  if (self$683 == 0) {
    if (self$683) {
      moonbit_decref(self$683);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$684 =
      self$683;
    return _Some$684;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$681
) {
  if (self$681 == 0) {
    if (self$681) {
      moonbit_decref(self$681);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$682 =
      self$681;
    return _Some$682;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$679
) {
  if (self$679 == 0) {
    if (self$679) {
      moonbit_decref(self$679);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$680 =
      self$679;
    return _Some$680;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$677
) {
  if (self$677 == 0) {
    if (self$677) {
      moonbit_decref(self$677);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$678 =
      self$677;
    return _Some$678;
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$2(
  moonbit_string_t self$671,
  moonbit_string_t other$672
) {
  if (self$671 == 0) {
    int32_t _tmp$3689;
    if (self$671) {
      moonbit_decref(self$671);
    }
    _tmp$3689 = other$672 == 0;
    if (other$672) {
      moonbit_decref(other$672);
    }
    return _tmp$3689;
  } else {
    moonbit_string_t _Some$673 = self$671;
    moonbit_string_t _x$674 = _Some$673;
    if (other$672 == 0) {
      moonbit_decref(_x$674);
      if (other$672) {
        moonbit_decref(other$672);
      }
      return 0;
    } else {
      moonbit_string_t _Some$675 = other$672;
      moonbit_string_t _y$676 = _Some$675;
      int32_t _tmp$3690 = moonbit_val_array_equal(_x$674, _y$676);
      moonbit_decref(_x$674);
      moonbit_decref(_y$676);
      return _tmp$3690;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$1(
  int32_t self$665,
  int32_t other$666
) {
  if (self$665 == -1) {
    return other$666 == -1;
  } else {
    int32_t _Some$667 = self$665;
    int32_t _x$668 = _Some$667;
    if (other$666 == -1) {
      return 0;
    } else {
      int32_t _Some$669 = other$666;
      int32_t _y$670 = _Some$669;
      return $$moonbitlang$core$builtin$Eq$$Unit$$equal(_x$668, _y$670);
    }
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  int64_t self$659,
  int64_t other$660
) {
  if (self$659 == 4294967296ll) {
    return other$660 == 4294967296ll;
  } else {
    int64_t _Some$661 = self$659;
    int32_t _x$662 = (int32_t)_Some$661;
    if (other$660 == 4294967296ll) {
      return 0;
    } else {
      int64_t _Some$663 = other$660;
      int32_t _y$664 = (int32_t)_Some$663;
      return _x$662 == _y$664;
    }
  }
}

uint32_t $ReadOnlyArray$$at$1(uint32_t* self$657, int32_t index$658) {
  uint32_t* _tmp$2257 = self$657;
  uint32_t _tmp$3691;
  if (index$658 < 0 || index$658 >= Moonbit_array_length(_tmp$2257)) {
    moonbit_panic();
  }
  _tmp$3691 = (uint32_t)_tmp$2257[index$658];
  moonbit_decref(_tmp$2257);
  return _tmp$3691;
}

uint64_t $ReadOnlyArray$$at$0(uint64_t* self$655, int32_t index$656) {
  uint64_t* _tmp$2256 = self$655;
  uint64_t _tmp$3692;
  if (index$656 < 0 || index$656 >= Moonbit_array_length(_tmp$2256)) {
    moonbit_panic();
  }
  _tmp$3692 = (uint64_t)_tmp$2256[index$656];
  moonbit_decref(_tmp$2256);
  return _tmp$3692;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$654
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$2255 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$654);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$2255);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$653
) {
  moonbit_string_t* _field$3694 = self$653->$0;
  moonbit_string_t* buf$2253 = _field$3694;
  int32_t _field$3693 = self$653->$1;
  int32_t _cnt$3875 = Moonbit_object_header(self$653)->rc;
  int32_t len$2254;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$2252;
  if (_cnt$3875 > 1) {
    int32_t _new_cnt$3876;
    moonbit_incref(buf$2253);
    _new_cnt$3876 = _cnt$3875 - 1;
    Moonbit_object_header(self$653)->rc = _new_cnt$3876;
  } else if (_cnt$3875 == 1) {
    moonbit_free(self$653);
  }
  len$2254 = _field$3693;
  _tmp$2252
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$2254, buf$2253
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$2252);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$651
) {
  struct $Ref$3c$Int$3e$* i$650 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$4108;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$2241;
  Moonbit_object_header(i$650)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$650->$0 = 0;
  _closure$4108
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$4108)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$4108->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$4108->$0_0 = self$651.$0;
  _closure$4108->$0_1 = self$651.$1;
  _closure$4108->$0_2 = self$651.$2;
  _closure$4108->$1 = i$650;
  _tmp$2241 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$4108;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$2241);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$2242
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$2243 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$2242;
  struct $Ref$3c$Int$3e$* _field$3699 = _casted_env$2243->$1;
  struct $Ref$3c$Int$3e$* i$650 = _field$3699;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$3698 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$2243->$0_1, _casted_env$2243->$0_2, _casted_env$2243->$0_0
    };
  int32_t _cnt$3877 = Moonbit_object_header(_casted_env$2243)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$651;
  int32_t val$2244;
  int32_t _tmp$2245;
  if (_cnt$3877 > 1) {
    int32_t _new_cnt$3878;
    moonbit_incref(i$650);
    moonbit_incref(_field$3698.$0);
    _new_cnt$3878 = _cnt$3877 - 1;
    Moonbit_object_header(_casted_env$2243)->rc = _new_cnt$3878;
  } else if (_cnt$3877 == 1) {
    moonbit_free(_casted_env$2243);
  }
  self$651 = _field$3698;
  val$2244 = i$650->$0;
  moonbit_incref(self$651.$0);
  _tmp$2245 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$651);
  if (val$2244 < _tmp$2245) {
    moonbit_string_t* _field$3697 = self$651.$0;
    moonbit_string_t* buf$2248 = _field$3697;
    int32_t _field$3696 = self$651.$1;
    int32_t start$2250 = _field$3696;
    int32_t val$2251 = i$650->$0;
    int32_t _tmp$2249 = start$2250 + val$2251;
    moonbit_string_t _tmp$3695 = (moonbit_string_t)buf$2248[_tmp$2249];
    moonbit_string_t elem$652;
    int32_t val$2247;
    int32_t _tmp$2246;
    moonbit_incref(_tmp$3695);
    moonbit_decref(buf$2248);
    elem$652 = _tmp$3695;
    val$2247 = i$650->$0;
    _tmp$2246 = val$2247 + 1;
    i$650->$0 = _tmp$2246;
    moonbit_decref(i$650);
    return elem$652;
  } else {
    moonbit_decref(self$651.$0);
    moonbit_decref(i$650);
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$2(
  moonbit_string_t self$646,
  struct $$moonbitlang$core$builtin$Logger logger$647
) {
  if (self$646 == 0) {
    if (self$646) {
      moonbit_decref(self$646);
    }
    logger$647.$0->$method_0(
      logger$647.$1, (moonbit_string_t)moonbit_string_literal_110.data
    );
  } else {
    moonbit_string_t _Some$648 = self$646;
    moonbit_string_t _arg$649 = _Some$648;
    struct $$moonbitlang$core$builtin$Logger _bind$2240;
    if (logger$647.$1) {
      moonbit_incref(logger$647.$1);
    }
    logger$647.$0->$method_0(
      logger$647.$1, (moonbit_string_t)moonbit_string_literal_111.data
    );
    if (logger$647.$1) {
      moonbit_incref(logger$647.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$2(logger$647, _arg$649);
    _bind$2240 = logger$647;
    _bind$2240.$0->$method_0(
      _bind$2240.$1, (moonbit_string_t)moonbit_string_literal_112.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$1(
  int32_t self$642,
  struct $$moonbitlang$core$builtin$Logger logger$643
) {
  if (self$642 == -1) {
    logger$643.$0->$method_0(
      logger$643.$1, (moonbit_string_t)moonbit_string_literal_110.data
    );
  } else {
    int32_t _Some$644 = self$642;
    int32_t _arg$645 = _Some$644;
    struct $$moonbitlang$core$builtin$Logger _bind$2239;
    if (logger$643.$1) {
      moonbit_incref(logger$643.$1);
    }
    logger$643.$0->$method_0(
      logger$643.$1, (moonbit_string_t)moonbit_string_literal_111.data
    );
    if (logger$643.$1) {
      moonbit_incref(logger$643.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$1(logger$643, _arg$645);
    _bind$2239 = logger$643;
    _bind$2239.$0->$method_0(
      _bind$2239.$1, (moonbit_string_t)moonbit_string_literal_112.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  int64_t self$638,
  struct $$moonbitlang$core$builtin$Logger logger$639
) {
  if (self$638 == 4294967296ll) {
    logger$639.$0->$method_0(
      logger$639.$1, (moonbit_string_t)moonbit_string_literal_110.data
    );
  } else {
    int64_t _Some$640 = self$638;
    int32_t _arg$641 = (int32_t)_Some$640;
    struct $$moonbitlang$core$builtin$Logger _bind$2238;
    if (logger$639.$1) {
      moonbit_incref(logger$639.$1);
    }
    logger$639.$0->$method_0(
      logger$639.$1, (moonbit_string_t)moonbit_string_literal_111.data
    );
    if (logger$639.$1) {
      moonbit_incref(logger$639.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$0(logger$639, _arg$641);
    _bind$2238 = logger$639;
    _bind$2238.$0->$method_0(
      _bind$2238.$1, (moonbit_string_t)moonbit_string_literal_112.data
    );
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$637
) {
  return self$637;
}

int32_t $$moonbitlang$core$builtin$Show$$UInt64$$output(
  uint64_t self$636,
  struct $$moonbitlang$core$builtin$Logger logger$635
) {
  moonbit_string_t _tmp$2237 = $UInt64$$to_string$inner(self$636, 10);
  logger$635.$0->$method_0(logger$635.$1, _tmp$2237);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$634,
  struct $$moonbitlang$core$builtin$Logger logger$633
) {
  moonbit_string_t _tmp$2236 = $Int$$to_string$inner(self$634, 10);
  logger$633.$0->$method_0(logger$633.$1, _tmp$2236);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$631,
  struct $$moonbitlang$core$builtin$Logger logger$632
) {
  if (self$631) {
    logger$632.$0->$method_0(
      logger$632.$1, (moonbit_string_t)moonbit_string_literal_113.data
    );
  } else {
    logger$632.$0->$method_0(
      logger$632.$1, (moonbit_string_t)moonbit_string_literal_114.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Unit$$output(
  int32_t _self$630,
  struct $$moonbitlang$core$builtin$Logger logger$629
) {
  logger$629.$0->$method_0(
    logger$629.$1, (moonbit_string_t)moonbit_string_literal_115.data
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$627,
  struct $$3c$String$3e$$3d$$3e$Int* f$628
) {
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _func$626 = self$627;
  return _func$626->code(_func$626, f$628);
}

int32_t $String$$contains(
  moonbit_string_t self$624,
  struct $StringView str$625
) {
  int32_t _tmp$2235 = Moonbit_array_length(self$624);
  struct $StringView _tmp$2234 = (struct $StringView){0, _tmp$2235, self$624};
  return $StringView$$contains(_tmp$2234, str$625);
}

int32_t $StringView$$contains(
  struct $StringView self$622,
  struct $StringView str$623
) {
  int64_t _bind$621 = $StringView$$find(self$622, str$623);
  int32_t _tmp$2233 = _bind$621 == 4294967296ll;
  return !_tmp$2233;
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$618,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$620
) {
  int32_t len$2228 = self$618->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$2230;
  int32_t _tmp$3702;
  int32_t _tmp$2229;
  int32_t length$619;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$3701;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$2231;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$3700;
  int32_t _tmp$2232;
  moonbit_incref(self$618);
  _tmp$2230 = $$moonbitlang$core$builtin$Array$$buffer$2(self$618);
  _tmp$3702 = Moonbit_array_length(_tmp$2230);
  moonbit_decref(_tmp$2230);
  _tmp$2229 = _tmp$3702;
  if (len$2228 == _tmp$2229) {
    moonbit_incref(self$618);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$618);
  }
  length$619 = self$618->$1;
  _field$3701 = self$618->$0;
  buf$2231 = _field$3701;
  _old$3700
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$2231[
      length$619
    ];
  if (_old$3700) {
    moonbit_decref(_old$3700);
  }
  buf$2231[length$619] = value$620;
  _tmp$2232 = length$619 + 1;
  self$618->$1 = _tmp$2232;
  moonbit_decref(self$618);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$615,
  struct $$3c$String$2a$Int$3e$* value$617
) {
  int32_t len$2223 = self$615->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$2225;
  int32_t _tmp$3705;
  int32_t _tmp$2224;
  int32_t length$616;
  struct $$3c$String$2a$Int$3e$** _field$3704;
  struct $$3c$String$2a$Int$3e$** buf$2226;
  struct $$3c$String$2a$Int$3e$* _old$3703;
  int32_t _tmp$2227;
  moonbit_incref(self$615);
  _tmp$2225 = $$moonbitlang$core$builtin$Array$$buffer$0(self$615);
  _tmp$3705 = Moonbit_array_length(_tmp$2225);
  moonbit_decref(_tmp$2225);
  _tmp$2224 = _tmp$3705;
  if (len$2223 == _tmp$2224) {
    moonbit_incref(self$615);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$615);
  }
  length$616 = self$615->$1;
  _field$3704 = self$615->$0;
  buf$2226 = _field$3704;
  _old$3703 = (struct $$3c$String$2a$Int$3e$*)buf$2226[length$616];
  if (_old$3703) {
    moonbit_decref(_old$3703);
  }
  buf$2226[length$616] = value$617;
  _tmp$2227 = length$616 + 1;
  self$615->$1 = _tmp$2227;
  moonbit_decref(self$615);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$612,
  moonbit_string_t value$614
) {
  int32_t len$2218 = self$612->$1;
  moonbit_string_t* _tmp$2220;
  int32_t _tmp$3708;
  int32_t _tmp$2219;
  int32_t length$613;
  moonbit_string_t* _field$3707;
  moonbit_string_t* buf$2221;
  moonbit_string_t _old$3706;
  int32_t _tmp$2222;
  moonbit_incref(self$612);
  _tmp$2220 = $$moonbitlang$core$builtin$Array$$buffer$1(self$612);
  _tmp$3708 = Moonbit_array_length(_tmp$2220);
  moonbit_decref(_tmp$2220);
  _tmp$2219 = _tmp$3708;
  if (len$2218 == _tmp$2219) {
    moonbit_incref(self$612);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$612);
  }
  length$613 = self$612->$1;
  _field$3707 = self$612->$0;
  buf$2221 = _field$3707;
  _old$3706 = (moonbit_string_t)buf$2221[length$613];
  moonbit_decref(_old$3706);
  buf$2221[length$613] = value$614;
  _tmp$2222 = length$613 + 1;
  self$612->$1 = _tmp$2222;
  moonbit_decref(self$612);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$610
) {
  int32_t old_cap$609 = self$610->$1;
  int32_t new_cap$611;
  if (old_cap$609 == 0) {
    new_cap$611 = 8;
  } else {
    new_cap$611 = old_cap$609 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$610, new_cap$611);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$607
) {
  int32_t old_cap$606 = self$607->$1;
  int32_t new_cap$608;
  if (old_cap$606 == 0) {
    new_cap$608 = 8;
  } else {
    new_cap$608 = old_cap$606 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$607, new_cap$608);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$604
) {
  int32_t old_cap$603 = self$604->$1;
  int32_t new_cap$605;
  if (old_cap$603 == 0) {
    new_cap$605 = 8;
  } else {
    new_cap$605 = old_cap$603 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$604, new_cap$605);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$600,
  int32_t new_capacity$598
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** new_buf$597 =
    (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_make_ref_array(
      new_capacity$598, 0
    );
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$3710 =
    self$600->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$599 =
    _field$3710;
  int32_t old_cap$601 = Moonbit_array_length(old_buf$599);
  int32_t copy_len$602;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$3709;
  if (old_cap$601 < new_capacity$598) {
    copy_len$602 = old_cap$601;
  } else {
    copy_len$602 = new_capacity$598;
  }
  moonbit_incref(old_buf$599);
  moonbit_incref(new_buf$597);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$597, 0, old_buf$599, 0, copy_len$602
  );
  _old$3709 = self$600->$0;
  moonbit_decref(_old$3709);
  self$600->$0 = new_buf$597;
  moonbit_decref(self$600);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$594,
  int32_t new_capacity$592
) {
  struct $$3c$String$2a$Int$3e$** new_buf$591 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_make_ref_array(
      new_capacity$592, 0
    );
  struct $$3c$String$2a$Int$3e$** _field$3712 = self$594->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$593 = _field$3712;
  int32_t old_cap$595 = Moonbit_array_length(old_buf$593);
  int32_t copy_len$596;
  struct $$3c$String$2a$Int$3e$** _old$3711;
  if (old_cap$595 < new_capacity$592) {
    copy_len$596 = old_cap$595;
  } else {
    copy_len$596 = new_capacity$592;
  }
  moonbit_incref(old_buf$593);
  moonbit_incref(new_buf$591);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$591, 0, old_buf$593, 0, copy_len$596
  );
  _old$3711 = self$594->$0;
  moonbit_decref(_old$3711);
  self$594->$0 = new_buf$591;
  moonbit_decref(self$594);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$588,
  int32_t new_capacity$586
) {
  moonbit_string_t* new_buf$585 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$586, (moonbit_string_t)moonbit_string_literal_3.data
    );
  moonbit_string_t* _field$3714 = self$588->$0;
  moonbit_string_t* old_buf$587 = _field$3714;
  int32_t old_cap$589 = Moonbit_array_length(old_buf$587);
  int32_t copy_len$590;
  moonbit_string_t* _old$3713;
  if (old_cap$589 < new_capacity$586) {
    copy_len$590 = old_cap$589;
  } else {
    copy_len$590 = new_capacity$586;
  }
  moonbit_incref(old_buf$587);
  moonbit_incref(new_buf$585);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$585, 0, old_buf$587, 0, copy_len$590
  );
  _old$3713 = self$588->$0;
  moonbit_decref(_old$3713);
  self$588->$0 = new_buf$585;
  moonbit_decref(self$588);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$584
) {
  if (capacity$584 == 0) {
    moonbit_string_t* _tmp$2216 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$4109 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$4109)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$4109->$0 = _tmp$2216;
    _block$4109->$1 = 0;
    return _block$4109;
  } else {
    moonbit_string_t* _tmp$2217 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$584, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$4110 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$4110)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$4110->$0 = _tmp$2217;
    _block$4110->$1 = 0;
    return _block$4110;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$582,
  struct $StringView str$583
) {
  int32_t len$2204 = self$582->$1;
  int32_t _tmp$2206;
  int32_t _tmp$2205;
  int32_t _tmp$2203;
  moonbit_bytes_t _field$3715;
  moonbit_bytes_t data$2207;
  int32_t len$2208;
  moonbit_string_t _tmp$2209;
  int32_t _tmp$2210;
  int32_t _tmp$2211;
  int32_t len$2213;
  int32_t _tmp$2215;
  int32_t _tmp$2214;
  int32_t _tmp$2212;
  moonbit_incref(str$583.$0);
  _tmp$2206 = $StringView$$length(str$583);
  _tmp$2205 = _tmp$2206 * 2;
  _tmp$2203 = len$2204 + _tmp$2205;
  moonbit_incref(self$582);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$582, _tmp$2203
  );
  _field$3715 = self$582->$0;
  data$2207 = _field$3715;
  len$2208 = self$582->$1;
  moonbit_incref(data$2207);
  moonbit_incref(str$583.$0);
  _tmp$2209 = $StringView$$data(str$583);
  moonbit_incref(str$583.$0);
  _tmp$2210 = $StringView$$start_offset(str$583);
  moonbit_incref(str$583.$0);
  _tmp$2211 = $StringView$$length(str$583);
  $FixedArray$$blit_from_string(
    data$2207, len$2208, _tmp$2209, _tmp$2210, _tmp$2211
  );
  len$2213 = self$582->$1;
  _tmp$2215 = $StringView$$length(str$583);
  _tmp$2214 = _tmp$2215 * 2;
  _tmp$2212 = len$2213 + _tmp$2214;
  self$582->$1 = _tmp$2212;
  moonbit_decref(self$582);
  return 0;
}

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$579,
  int32_t i$580,
  int32_t start_offset$581,
  int64_t end_offset$577
) {
  int32_t end_offset$576;
  if (end_offset$577 == 4294967296ll) {
    end_offset$576 = Moonbit_array_length(self$579);
  } else {
    int64_t _Some$578 = end_offset$577;
    end_offset$576 = (int32_t)_Some$578;
  }
  if (i$580 >= 0) {
    return $String$$offset_of_nth_char_forward(
             self$579, i$580, start_offset$581, end_offset$576
           );
  } else {
    int32_t _tmp$2202 = -i$580;
    return $String$$offset_of_nth_char_backward(
             self$579, _tmp$2202, start_offset$581, end_offset$576
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$574,
  int32_t n$572,
  int32_t start_offset$568,
  int32_t end_offset$569
) {
  int32_t _if_result$4111;
  if (start_offset$568 >= 0) {
    _if_result$4111 = start_offset$568 <= end_offset$569;
  } else {
    _if_result$4111 = 0;
  }
  if (_if_result$4111) {
    int32_t utf16_offset$570 = start_offset$568;
    int32_t char_count$571 = 0;
    int32_t _tmp$2200;
    int32_t _if_result$4114;
    while (1) {
      int32_t _tmp$2194 = utf16_offset$570;
      int32_t _if_result$4113;
      if (_tmp$2194 < end_offset$569) {
        int32_t _tmp$2193 = char_count$571;
        _if_result$4113 = _tmp$2193 < n$572;
      } else {
        _if_result$4113 = 0;
      }
      if (_if_result$4113) {
        int32_t _tmp$2198 = utf16_offset$570;
        int32_t c$573 = self$574[_tmp$2198];
        int32_t _tmp$2197;
        if ($Int$$is_leading_surrogate(c$573)) {
          int32_t _tmp$2195 = utf16_offset$570;
          utf16_offset$570 = _tmp$2195 + 2;
        } else {
          int32_t _tmp$2196 = utf16_offset$570;
          utf16_offset$570 = _tmp$2196 + 1;
        }
        _tmp$2197 = char_count$571;
        char_count$571 = _tmp$2197 + 1;
        continue;
      } else {
        moonbit_decref(self$574);
      }
      break;
    }
    _tmp$2200 = char_count$571;
    if (_tmp$2200 < n$572) {
      _if_result$4114 = 1;
    } else {
      int32_t _tmp$2199 = utf16_offset$570;
      _if_result$4114 = _tmp$2199 >= end_offset$569;
    }
    if (_if_result$4114) {
      return 4294967296ll;
    } else {
      int32_t _tmp$2201 = utf16_offset$570;
      return (int64_t)_tmp$2201;
    }
  } else {
    moonbit_decref(self$574);
    return $moonbitlang$core$builtin$abort$3(
             (moonbit_string_t)moonbit_string_literal_116.data,
               (moonbit_string_t)moonbit_string_literal_117.data
           );
  }
}

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$566,
  int32_t n$564,
  int32_t start_offset$563,
  int32_t end_offset$562
) {
  int32_t char_count$560 = 0;
  int32_t utf16_offset$561 = end_offset$562;
  int32_t _tmp$2191;
  int32_t _if_result$4117;
  while (1) {
    int32_t _tmp$2184 = utf16_offset$561;
    int32_t _tmp$2183 = _tmp$2184 - 1;
    int32_t _if_result$4116;
    if (_tmp$2183 >= start_offset$563) {
      int32_t _tmp$2182 = char_count$560;
      _if_result$4116 = _tmp$2182 < n$564;
    } else {
      _if_result$4116 = 0;
    }
    if (_if_result$4116) {
      int32_t _tmp$2189 = utf16_offset$561;
      int32_t _tmp$2188 = _tmp$2189 - 1;
      int32_t c$565 = self$566[_tmp$2188];
      int32_t _tmp$2187;
      if ($Int$$is_trailing_surrogate(c$565)) {
        int32_t _tmp$2185 = utf16_offset$561;
        utf16_offset$561 = _tmp$2185 - 2;
      } else {
        int32_t _tmp$2186 = utf16_offset$561;
        utf16_offset$561 = _tmp$2186 - 1;
      }
      _tmp$2187 = char_count$560;
      char_count$560 = _tmp$2187 + 1;
      continue;
    } else {
      moonbit_decref(self$566);
    }
    break;
  }
  _tmp$2191 = char_count$560;
  if (_tmp$2191 < n$564) {
    _if_result$4117 = 1;
  } else {
    int32_t _tmp$2190 = utf16_offset$561;
    _if_result$4117 = _tmp$2190 < start_offset$563;
  }
  if (_if_result$4117) {
    return 4294967296ll;
  } else {
    int32_t _tmp$2192 = utf16_offset$561;
    return (int64_t)_tmp$2192;
  }
}

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$552,
  int32_t len$555,
  int32_t start_offset$559,
  int64_t end_offset$550
) {
  int32_t end_offset$549;
  int32_t index$553;
  int32_t count$554;
  if (end_offset$550 == 4294967296ll) {
    end_offset$549 = Moonbit_array_length(self$552);
  } else {
    int64_t _Some$551 = end_offset$550;
    end_offset$549 = (int32_t)_Some$551;
  }
  index$553 = start_offset$559;
  count$554 = 0;
  while (1) {
    int32_t _if_result$4119;
    if (index$553 < end_offset$549) {
      _if_result$4119 = count$554 < len$555;
    } else {
      _if_result$4119 = 0;
    }
    if (_if_result$4119) {
      int32_t c1$556 = self$552[index$553];
      int32_t _if_result$4120;
      int32_t _tmp$2180;
      int32_t _tmp$2181;
      if ($Int$$is_leading_surrogate(c1$556)) {
        int32_t _tmp$2176 = index$553 + 1;
        _if_result$4120 = _tmp$2176 < end_offset$549;
      } else {
        _if_result$4120 = 0;
      }
      if (_if_result$4120) {
        int32_t _tmp$2179 = index$553 + 1;
        int32_t c2$557 = self$552[_tmp$2179];
        if ($Int$$is_trailing_surrogate(c2$557)) {
          int32_t _tmp$2177 = index$553 + 2;
          int32_t _tmp$2178 = count$554 + 1;
          index$553 = _tmp$2177;
          count$554 = _tmp$2178;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_118.data,
              (moonbit_string_t)moonbit_string_literal_119.data
          );
        }
      }
      _tmp$2180 = index$553 + 1;
      _tmp$2181 = count$554 + 1;
      index$553 = _tmp$2180;
      count$554 = _tmp$2181;
      continue;
    } else {
      moonbit_decref(self$552);
      return count$554 >= len$555;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$548
) {
  int32_t end$2174 = self$548.$2;
  int32_t _field$3716 = self$548.$1;
  int32_t start$2175;
  moonbit_decref(self$548.$0);
  start$2175 = _field$3716;
  return end$2174 - start$2175;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$547
) {
  int32_t end$2172 = self$547.$2;
  int32_t _field$3717 = self$547.$1;
  int32_t start$2173;
  moonbit_decref(self$547.$0);
  start$2173 = _field$3717;
  return end$2172 - start$2173;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$546
) {
  int32_t end$2170 = self$546.$2;
  int32_t _field$3718 = self$546.$1;
  int32_t start$2171;
  moonbit_decref(self$546.$0);
  start$2171 = _field$3718;
  return end$2170 - start$2171;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$545
) {
  int32_t end$2168 = self$545.$2;
  int32_t _field$3719 = self$545.$1;
  int32_t start$2169;
  moonbit_decref(self$545.$0);
  start$2169 = _field$3719;
  return end$2168 - start$2169;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$544
) {
  int32_t end$2166 = self$544.$2;
  int32_t _field$3720 = self$544.$1;
  int32_t start$2167;
  moonbit_decref(self$544.$0);
  start$2167 = _field$3720;
  return end$2166 - start$2167;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$539
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$4121 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$4121)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$4121->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$4121->$0 = self$539;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$4121;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$2164,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$537
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$2165 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$2164;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$3721 =
    _casted_env$2165->$0;
  int32_t _cnt$3879 = Moonbit_object_header(_casted_env$2165)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$539;
  if (_cnt$3879 > 1) {
    int32_t _new_cnt$3880;
    moonbit_incref(_field$3721);
    _new_cnt$3880 = _cnt$3879 - 1;
    Moonbit_object_header(_casted_env$2165)->rc = _new_cnt$3880;
  } else if (_cnt$3879 == 1) {
    moonbit_free(_casted_env$2165);
  }
  self$539 = _field$3721;
  while (1) {
    moonbit_string_t _bind$538;
    moonbit_incref(self$539);
    _bind$538 = $$moonbitlang$core$builtin$Iterator$$next$0(self$539);
    if (_bind$538 == 0) {
      moonbit_decref(self$539);
      if (_bind$538) {
        moonbit_decref(_bind$538);
      }
      moonbit_decref(yield_$537);
      return 1;
    } else {
      moonbit_string_t _Some$540 = _bind$538;
      moonbit_string_t _x$541 = _Some$540;
      int32_t _bind$542;
      moonbit_incref(yield_$537);
      _bind$542 = yield_$537->code(yield_$537, _x$541);
      switch (_bind$542) {
        case 1:
          break;
        default: {
          moonbit_decref(self$539);
          moonbit_decref(yield_$537);
          return 0;
          break;
        }
      }
      continue;
    }
    break;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Iterator$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$536
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _func$535 = self$536;
  return _func$535->code(_func$535);
}

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$527,
  struct $$moonbitlang$core$builtin$Logger logger$525
) {
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$526;
  int32_t len$528;
  int32_t i$529;
  int32_t seg$530;
  if (logger$525.$1) {
    moonbit_incref(logger$525.$1);
  }
  logger$525.$0->$method_3(logger$525.$1, 34);
  if (logger$525.$1) {
    moonbit_incref(logger$525.$1);
  }
  moonbit_incref(self$527);
  _env$526
  = (struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$)
    );
  Moonbit_object_header(_env$526)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$, $0_0
    )
    >> 2,
      3,
      0
  );
  _env$526->$0_0 = logger$525.$0;
  _env$526->$0_1 = logger$525.$1;
  _env$526->$1 = self$527;
  len$528 = Moonbit_array_length(self$527);
  i$529 = 0;
  seg$530 = 0;
  $$2a$for$531:;
  while (1) {
    int32_t code$532;
    int32_t c$534;
    struct $$moonbitlang$core$builtin$Logger _bind$2146;
    int32_t _tmp$2147;
    int32_t _tmp$2148;
    int32_t _tmp$2149;
    int32_t _tmp$4126;
    int32_t _tmp$4127;
    if (i$529 >= len$528) {
      moonbit_decref(self$527);
      $moonbitlang$core$builtin$output$flush_segment$7c$3831(
        _env$526, seg$530, i$529
      );
      break;
    }
    code$532 = self$527[i$529];
    switch (code$532) {
      case 34: {
        c$534 = code$532;
        goto $join$533;
        break;
      }
      
      case 92: {
        c$534 = code$532;
        goto $join$533;
        break;
      }
      
      case 10: {
        int32_t _tmp$2150;
        int32_t _tmp$2151;
        moonbit_incref(_env$526);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$526, seg$530, i$529
        );
        if (logger$525.$1) {
          moonbit_incref(logger$525.$1);
        }
        logger$525.$0->$method_0(
          logger$525.$1, (moonbit_string_t)moonbit_string_literal_120.data
        );
        _tmp$2150 = i$529 + 1;
        _tmp$2151 = i$529 + 1;
        i$529 = _tmp$2150;
        seg$530 = _tmp$2151;
        goto $$2a$for$531;
        break;
      }
      
      case 13: {
        int32_t _tmp$2152;
        int32_t _tmp$2153;
        moonbit_incref(_env$526);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$526, seg$530, i$529
        );
        if (logger$525.$1) {
          moonbit_incref(logger$525.$1);
        }
        logger$525.$0->$method_0(
          logger$525.$1, (moonbit_string_t)moonbit_string_literal_121.data
        );
        _tmp$2152 = i$529 + 1;
        _tmp$2153 = i$529 + 1;
        i$529 = _tmp$2152;
        seg$530 = _tmp$2153;
        goto $$2a$for$531;
        break;
      }
      
      case 8: {
        int32_t _tmp$2154;
        int32_t _tmp$2155;
        moonbit_incref(_env$526);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$526, seg$530, i$529
        );
        if (logger$525.$1) {
          moonbit_incref(logger$525.$1);
        }
        logger$525.$0->$method_0(
          logger$525.$1, (moonbit_string_t)moonbit_string_literal_122.data
        );
        _tmp$2154 = i$529 + 1;
        _tmp$2155 = i$529 + 1;
        i$529 = _tmp$2154;
        seg$530 = _tmp$2155;
        goto $$2a$for$531;
        break;
      }
      
      case 9: {
        int32_t _tmp$2156;
        int32_t _tmp$2157;
        moonbit_incref(_env$526);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$526, seg$530, i$529
        );
        if (logger$525.$1) {
          moonbit_incref(logger$525.$1);
        }
        logger$525.$0->$method_0(
          logger$525.$1, (moonbit_string_t)moonbit_string_literal_123.data
        );
        _tmp$2156 = i$529 + 1;
        _tmp$2157 = i$529 + 1;
        i$529 = _tmp$2156;
        seg$530 = _tmp$2157;
        goto $$2a$for$531;
        break;
      }
      default: {
        if (code$532 < 32) {
          int32_t _tmp$2160;
          moonbit_string_t _tmp$2159;
          struct $$moonbitlang$core$builtin$Logger _bind$2158;
          int32_t _tmp$2161;
          int32_t _tmp$2162;
          moonbit_incref(_env$526);
          $moonbitlang$core$builtin$output$flush_segment$7c$3831(
            _env$526, seg$530, i$529
          );
          if (logger$525.$1) {
            moonbit_incref(logger$525.$1);
          }
          logger$525.$0->$method_0(
            logger$525.$1, (moonbit_string_t)moonbit_string_literal_124.data
          );
          _tmp$2160 = code$532 & 0xff;
          _tmp$2159 = $Byte$$to_hex(_tmp$2160);
          if (logger$525.$1) {
            moonbit_incref(logger$525.$1);
          }
          logger$525.$0->$method_0(logger$525.$1, _tmp$2159);
          _bind$2158 = logger$525;
          if (_bind$2158.$1) {
            moonbit_incref(_bind$2158.$1);
          }
          _bind$2158.$0->$method_3(_bind$2158.$1, 125);
          _tmp$2161 = i$529 + 1;
          _tmp$2162 = i$529 + 1;
          i$529 = _tmp$2161;
          seg$530 = _tmp$2162;
          goto $$2a$for$531;
        } else {
          int32_t _tmp$2163 = i$529 + 1;
          int32_t _tmp$4125 = seg$530;
          i$529 = _tmp$2163;
          seg$530 = _tmp$4125;
          goto $$2a$for$531;
        }
        break;
      }
    }
    goto $joinlet$4124;
    $join$533:;
    moonbit_incref(_env$526);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$526, seg$530, i$529
    );
    if (logger$525.$1) {
      moonbit_incref(logger$525.$1);
    }
    logger$525.$0->$method_3(logger$525.$1, 92);
    _bind$2146 = logger$525;
    _tmp$2147 = c$534;
    if (_bind$2146.$1) {
      moonbit_incref(_bind$2146.$1);
    }
    _bind$2146.$0->$method_3(_bind$2146.$1, _tmp$2147);
    _tmp$2148 = i$529 + 1;
    _tmp$2149 = i$529 + 1;
    i$529 = _tmp$2148;
    seg$530 = _tmp$2149;
    continue;
    $joinlet$4124:;
    _tmp$4126 = i$529;
    _tmp$4127 = seg$530;
    i$529 = _tmp$4126;
    seg$530 = _tmp$4127;
    continue;
    break;
  }
  logger$525.$0->$method_3(logger$525.$1, 34);
  return 0;
}

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$521,
  int32_t seg$524,
  int32_t i$523
) {
  moonbit_string_t _field$3723 = _env$521->$1;
  moonbit_string_t self$520 = _field$3723;
  struct $$moonbitlang$core$builtin$Logger _field$3722 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$521->$0_0, _env$521->$0_1
    };
  int32_t _cnt$3881 = Moonbit_object_header(_env$521)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$522;
  if (_cnt$3881 > 1) {
    int32_t _new_cnt$3882;
    moonbit_incref(self$520);
    if (_field$3722.$1) {
      moonbit_incref(_field$3722.$1);
    }
    _new_cnt$3882 = _cnt$3881 - 1;
    Moonbit_object_header(_env$521)->rc = _new_cnt$3882;
  } else if (_cnt$3881 == 1) {
    moonbit_free(_env$521);
  }
  logger$522 = _field$3722;
  if (i$523 > seg$524) {
    int32_t _tmp$2145 = i$523 - seg$524;
    logger$522.$0->$method_1(logger$522.$1, self$520, seg$524, _tmp$2145);
  } else {
    if (logger$522.$1) {
      moonbit_decref(logger$522.$1);
    }
    moonbit_decref(self$520);
  }
  return 0;
}

moonbit_string_t $Byte$$to_hex(int32_t b$519) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$518 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _tmp$2142 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$519, 16);
  int32_t _tmp$2141 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$2142);
  int32_t _tmp$2144;
  int32_t _tmp$2143;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$2140;
  moonbit_incref(_self$518);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$518, _tmp$2141
  );
  _tmp$2144 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$519, 16);
  _tmp$2143
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$2144
  );
  moonbit_incref(_self$518);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$518, _tmp$2143
  );
  _tmp$2140 = _self$518;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$2140);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$517) {
  if (i$517 < 10) {
    int32_t _tmp$2137 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$517, 48);
    return $Byte$$to_char(_tmp$2137);
  } else {
    int32_t _tmp$2139 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$517, 97);
    int32_t _tmp$2138 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$2139, 10);
    return $Byte$$to_char(_tmp$2138);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$515,
  int32_t that$516
) {
  int32_t _tmp$2135 = (int32_t)self$515;
  int32_t _tmp$2136 = (int32_t)that$516;
  int32_t _tmp$2134 = _tmp$2135 - _tmp$2136;
  return _tmp$2134 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$513,
  int32_t that$514
) {
  int32_t _tmp$2132 = (int32_t)self$513;
  int32_t _tmp$2133 = (int32_t)that$514;
  int32_t _tmp$2131 = _tmp$2132 % _tmp$2133;
  return _tmp$2131 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$511,
  int32_t that$512
) {
  int32_t _tmp$2129 = (int32_t)self$511;
  int32_t _tmp$2130 = (int32_t)that$512;
  int32_t _tmp$2128 = _tmp$2129 / _tmp$2130;
  return _tmp$2128 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$509,
  int32_t that$510
) {
  int32_t _tmp$2126 = (int32_t)self$509;
  int32_t _tmp$2127 = (int32_t)that$510;
  int32_t _tmp$2125 = _tmp$2126 + _tmp$2127;
  return _tmp$2125 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$506,
  int32_t start$504,
  int32_t end$505
) {
  int32_t _if_result$4128;
  int32_t len$507;
  int32_t _tmp$2123;
  int32_t _tmp$2124;
  moonbit_bytes_t bytes$508;
  moonbit_bytes_t _tmp$2122;
  if (start$504 == 0) {
    int32_t _tmp$2121 = Moonbit_array_length(str$506);
    _if_result$4128 = end$505 == _tmp$2121;
  } else {
    _if_result$4128 = 0;
  }
  if (_if_result$4128) {
    return str$506;
  }
  len$507 = end$505 - start$504;
  _tmp$2123 = len$507 * 2;
  _tmp$2124 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$508 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$2123, _tmp$2124);
  moonbit_incref(bytes$508);
  $FixedArray$$blit_from_string(bytes$508, 0, str$506, start$504, len$507);
  _tmp$2122 = bytes$508;
  return $Bytes$$to_unchecked_string$inner(_tmp$2122, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$503
) {
  return f$503;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$6(
  int32_t a$497,
  int32_t b$498,
  moonbit_string_t msg$500,
  moonbit_string_t loc$502
) {
  if ($moonbitlang$core$builtin$op_notequal$5(a$497, b$498)) {
    moonbit_string_t fail_msg$499;
    if (msg$500 == 0) {
      moonbit_string_t _tmp$2119;
      moonbit_string_t _tmp$2118;
      moonbit_string_t _tmp$2117;
      moonbit_string_t _tmp$2114;
      moonbit_string_t _tmp$2116;
      moonbit_string_t _tmp$2115;
      moonbit_string_t _tmp$2113;
      if (msg$500) {
        moonbit_decref(msg$500);
      }
      _tmp$2119 = $moonbitlang$core$builtin$debug_string$6(a$497);
      _tmp$2118
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2119
      );
      _tmp$2117
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_107.data, _tmp$2118
      );
      _tmp$2114
      = moonbit_add_string(
        _tmp$2117, (moonbit_string_t)moonbit_string_literal_125.data
      );
      _tmp$2116 = $moonbitlang$core$builtin$debug_string$6(b$498);
      _tmp$2115
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2116
      );
      _tmp$2113 = moonbit_add_string(_tmp$2114, _tmp$2115);
      fail_msg$499
      = moonbit_add_string(
        _tmp$2113, (moonbit_string_t)moonbit_string_literal_107.data
      );
    } else {
      moonbit_string_t _Some$501 = msg$500;
      fail_msg$499 = _Some$501;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$499, loc$502);
  } else {
    int32_t _tmp$2120;
    struct moonbit_result_0 _result$4129;
    moonbit_decref(loc$502);
    if (msg$500) {
      moonbit_decref(msg$500);
    }
    _tmp$2120 = 0;
    _result$4129.tag = 1;
    _result$4129.data.ok = _tmp$2120;
    return _result$4129;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$5(
  moonbit_string_t a$491,
  moonbit_string_t b$492,
  moonbit_string_t msg$494,
  moonbit_string_t loc$496
) {
  if (b$492) {
    moonbit_incref(b$492);
  }
  if (a$491) {
    moonbit_incref(a$491);
  }
  if ($moonbitlang$core$builtin$op_notequal$4(a$491, b$492)) {
    moonbit_string_t fail_msg$493;
    if (msg$494 == 0) {
      moonbit_string_t _tmp$2111;
      moonbit_string_t _tmp$2110;
      moonbit_string_t _tmp$2109;
      moonbit_string_t _tmp$2106;
      moonbit_string_t _tmp$2108;
      moonbit_string_t _tmp$2107;
      moonbit_string_t _tmp$2105;
      if (msg$494) {
        moonbit_decref(msg$494);
      }
      _tmp$2111 = $moonbitlang$core$builtin$debug_string$5(a$491);
      _tmp$2110
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2111
      );
      _tmp$2109
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_107.data, _tmp$2110
      );
      _tmp$2106
      = moonbit_add_string(
        _tmp$2109, (moonbit_string_t)moonbit_string_literal_125.data
      );
      _tmp$2108 = $moonbitlang$core$builtin$debug_string$5(b$492);
      _tmp$2107
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2108
      );
      _tmp$2105 = moonbit_add_string(_tmp$2106, _tmp$2107);
      fail_msg$493
      = moonbit_add_string(
        _tmp$2105, (moonbit_string_t)moonbit_string_literal_107.data
      );
    } else {
      moonbit_string_t _Some$495;
      if (b$492) {
        moonbit_decref(b$492);
      }
      if (a$491) {
        moonbit_decref(a$491);
      }
      _Some$495 = msg$494;
      fail_msg$493 = _Some$495;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$493, loc$496);
  } else {
    int32_t _tmp$2112;
    struct moonbit_result_0 _result$4130;
    moonbit_decref(loc$496);
    if (msg$494) {
      moonbit_decref(msg$494);
    }
    if (b$492) {
      moonbit_decref(b$492);
    }
    if (a$491) {
      moonbit_decref(a$491);
    }
    _tmp$2112 = 0;
    _result$4130.tag = 1;
    _result$4130.data.ok = _tmp$2112;
    return _result$4130;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  double a$485,
  double b$486,
  moonbit_string_t msg$488,
  moonbit_string_t loc$490
) {
  if (a$485 != b$486) {
    moonbit_string_t fail_msg$487;
    if (msg$488 == 0) {
      moonbit_string_t _tmp$2103;
      moonbit_string_t _tmp$2102;
      moonbit_string_t _tmp$2101;
      moonbit_string_t _tmp$2098;
      moonbit_string_t _tmp$2100;
      moonbit_string_t _tmp$2099;
      moonbit_string_t _tmp$2097;
      if (msg$488) {
        moonbit_decref(msg$488);
      }
      _tmp$2103 = $moonbitlang$core$builtin$debug_string$4(a$485);
      _tmp$2102
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2103
      );
      _tmp$2101
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_107.data, _tmp$2102
      );
      _tmp$2098
      = moonbit_add_string(
        _tmp$2101, (moonbit_string_t)moonbit_string_literal_125.data
      );
      _tmp$2100 = $moonbitlang$core$builtin$debug_string$4(b$486);
      _tmp$2099
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2100
      );
      _tmp$2097 = moonbit_add_string(_tmp$2098, _tmp$2099);
      fail_msg$487
      = moonbit_add_string(
        _tmp$2097, (moonbit_string_t)moonbit_string_literal_107.data
      );
    } else {
      moonbit_string_t _Some$489 = msg$488;
      fail_msg$487 = _Some$489;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$487, loc$490);
  } else {
    int32_t _tmp$2104;
    struct moonbit_result_0 _result$4131;
    moonbit_decref(loc$490);
    if (msg$488) {
      moonbit_decref(msg$488);
    }
    _tmp$2104 = 0;
    _result$4131.tag = 1;
    _result$4131.data.ok = _tmp$2104;
    return _result$4131;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  int32_t a$479,
  int32_t b$480,
  moonbit_string_t msg$482,
  moonbit_string_t loc$484
) {
  if ($moonbitlang$core$builtin$op_notequal$3(a$479, b$480)) {
    moonbit_string_t fail_msg$481;
    if (msg$482 == 0) {
      moonbit_string_t _tmp$2095;
      moonbit_string_t _tmp$2094;
      moonbit_string_t _tmp$2093;
      moonbit_string_t _tmp$2090;
      moonbit_string_t _tmp$2092;
      moonbit_string_t _tmp$2091;
      moonbit_string_t _tmp$2089;
      if (msg$482) {
        moonbit_decref(msg$482);
      }
      _tmp$2095 = $moonbitlang$core$builtin$debug_string$3(a$479);
      _tmp$2094
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2095
      );
      _tmp$2093
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_107.data, _tmp$2094
      );
      _tmp$2090
      = moonbit_add_string(
        _tmp$2093, (moonbit_string_t)moonbit_string_literal_125.data
      );
      _tmp$2092 = $moonbitlang$core$builtin$debug_string$3(b$480);
      _tmp$2091
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2092
      );
      _tmp$2089 = moonbit_add_string(_tmp$2090, _tmp$2091);
      fail_msg$481
      = moonbit_add_string(
        _tmp$2089, (moonbit_string_t)moonbit_string_literal_107.data
      );
    } else {
      moonbit_string_t _Some$483 = msg$482;
      fail_msg$481 = _Some$483;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$481, loc$484);
  } else {
    int32_t _tmp$2096;
    struct moonbit_result_0 _result$4132;
    moonbit_decref(loc$484);
    if (msg$482) {
      moonbit_decref(msg$482);
    }
    _tmp$2096 = 0;
    _result$4132.tag = 1;
    _result$4132.data.ok = _tmp$2096;
    return _result$4132;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  int64_t a$473,
  int64_t b$474,
  moonbit_string_t msg$476,
  moonbit_string_t loc$478
) {
  if ($moonbitlang$core$builtin$op_notequal$2(a$473, b$474)) {
    moonbit_string_t fail_msg$475;
    if (msg$476 == 0) {
      moonbit_string_t _tmp$2087;
      moonbit_string_t _tmp$2086;
      moonbit_string_t _tmp$2085;
      moonbit_string_t _tmp$2082;
      moonbit_string_t _tmp$2084;
      moonbit_string_t _tmp$2083;
      moonbit_string_t _tmp$2081;
      if (msg$476) {
        moonbit_decref(msg$476);
      }
      _tmp$2087 = $moonbitlang$core$builtin$debug_string$2(a$473);
      _tmp$2086
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2087
      );
      _tmp$2085
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_107.data, _tmp$2086
      );
      _tmp$2082
      = moonbit_add_string(
        _tmp$2085, (moonbit_string_t)moonbit_string_literal_125.data
      );
      _tmp$2084 = $moonbitlang$core$builtin$debug_string$2(b$474);
      _tmp$2083
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2084
      );
      _tmp$2081 = moonbit_add_string(_tmp$2082, _tmp$2083);
      fail_msg$475
      = moonbit_add_string(
        _tmp$2081, (moonbit_string_t)moonbit_string_literal_107.data
      );
    } else {
      moonbit_string_t _Some$477 = msg$476;
      fail_msg$475 = _Some$477;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$475, loc$478);
  } else {
    int32_t _tmp$2088;
    struct moonbit_result_0 _result$4133;
    moonbit_decref(loc$478);
    if (msg$476) {
      moonbit_decref(msg$476);
    }
    _tmp$2088 = 0;
    _result$4133.tag = 1;
    _result$4133.data.ok = _tmp$2088;
    return _result$4133;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  moonbit_string_t a$467,
  moonbit_string_t b$468,
  moonbit_string_t msg$470,
  moonbit_string_t loc$472
) {
  moonbit_incref(b$468);
  moonbit_incref(a$467);
  if ($moonbitlang$core$builtin$op_notequal$1(a$467, b$468)) {
    moonbit_string_t fail_msg$469;
    if (msg$470 == 0) {
      moonbit_string_t _tmp$2079;
      moonbit_string_t _tmp$2078;
      moonbit_string_t _tmp$2077;
      moonbit_string_t _tmp$2074;
      moonbit_string_t _tmp$2076;
      moonbit_string_t _tmp$2075;
      moonbit_string_t _tmp$2073;
      if (msg$470) {
        moonbit_decref(msg$470);
      }
      _tmp$2079 = $moonbitlang$core$builtin$debug_string$1(a$467);
      _tmp$2078
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2079
      );
      _tmp$2077
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_107.data, _tmp$2078
      );
      _tmp$2074
      = moonbit_add_string(
        _tmp$2077, (moonbit_string_t)moonbit_string_literal_125.data
      );
      _tmp$2076 = $moonbitlang$core$builtin$debug_string$1(b$468);
      _tmp$2075
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2076
      );
      _tmp$2073 = moonbit_add_string(_tmp$2074, _tmp$2075);
      fail_msg$469
      = moonbit_add_string(
        _tmp$2073, (moonbit_string_t)moonbit_string_literal_107.data
      );
    } else {
      moonbit_string_t _Some$471;
      moonbit_decref(b$468);
      moonbit_decref(a$467);
      _Some$471 = msg$470;
      fail_msg$469 = _Some$471;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$469, loc$472);
  } else {
    int32_t _tmp$2080;
    struct moonbit_result_0 _result$4134;
    moonbit_decref(loc$472);
    if (msg$470) {
      moonbit_decref(msg$470);
    }
    moonbit_decref(b$468);
    moonbit_decref(a$467);
    _tmp$2080 = 0;
    _result$4134.tag = 1;
    _result$4134.data.ok = _tmp$2080;
    return _result$4134;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  int32_t a$461,
  int32_t b$462,
  moonbit_string_t msg$464,
  moonbit_string_t loc$466
) {
  if (a$461 != b$462) {
    moonbit_string_t fail_msg$463;
    if (msg$464 == 0) {
      moonbit_string_t _tmp$2071;
      moonbit_string_t _tmp$2070;
      moonbit_string_t _tmp$2069;
      moonbit_string_t _tmp$2066;
      moonbit_string_t _tmp$2068;
      moonbit_string_t _tmp$2067;
      moonbit_string_t _tmp$2065;
      if (msg$464) {
        moonbit_decref(msg$464);
      }
      _tmp$2071 = $moonbitlang$core$builtin$debug_string$0(a$461);
      _tmp$2070
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2071
      );
      _tmp$2069
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_107.data, _tmp$2070
      );
      _tmp$2066
      = moonbit_add_string(
        _tmp$2069, (moonbit_string_t)moonbit_string_literal_125.data
      );
      _tmp$2068 = $moonbitlang$core$builtin$debug_string$0(b$462);
      _tmp$2067
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2068
      );
      _tmp$2065 = moonbit_add_string(_tmp$2066, _tmp$2067);
      fail_msg$463
      = moonbit_add_string(
        _tmp$2065, (moonbit_string_t)moonbit_string_literal_107.data
      );
    } else {
      moonbit_string_t _Some$465 = msg$464;
      fail_msg$463 = _Some$465;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$463, loc$466);
  } else {
    int32_t _tmp$2072;
    struct moonbit_result_0 _result$4135;
    moonbit_decref(loc$466);
    if (msg$464) {
      moonbit_decref(msg$464);
    }
    _tmp$2072 = 0;
    _result$4135.tag = 1;
    _result$4135.data.ok = _tmp$2072;
    return _result$4135;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$460,
  moonbit_string_t loc$459
) {
  moonbit_string_t _tmp$2064 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$459);
  moonbit_string_t _tmp$2062 =
    moonbit_add_string(
      _tmp$2064, (moonbit_string_t)moonbit_string_literal_126.data
    );
  moonbit_string_t _tmp$2063 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(msg$460);
  moonbit_string_t _tmp$2061 = moonbit_add_string(_tmp$2062, _tmp$2063);
  void* moonbitlang$core$builtin$Failure$Failure$2060 =
    (void*)moonbit_malloc(
      sizeof(struct $Error$moonbitlang$core$builtin$Failure$Failure)
    );
  struct moonbit_result_0 _result$4136;
  Moonbit_object_header(moonbitlang$core$builtin$Failure$Failure$2060)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Error$moonbitlang$core$builtin$Failure$Failure, $0) >> 2,
      1,
      2
  );
  ((struct $Error$moonbitlang$core$builtin$Failure$Failure*)moonbitlang$core$builtin$Failure$Failure$2060)->$0
  = _tmp$2061;
  _result$4136.tag = 0;
  _result$4136.data.err = moonbitlang$core$builtin$Failure$Failure$2060;
  return _result$4136;
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$6(int32_t t$458) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$457 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2059;
  moonbit_incref(buf$457);
  _tmp$2059
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$457
  };
  $$moonbitlang$core$builtin$Show$$Unit$$output(t$458, _tmp$2059);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$457);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$5(
  moonbit_string_t t$456
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$455 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2058;
  moonbit_incref(buf$455);
  _tmp$2058
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$455
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$2(t$456, _tmp$2058);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$455);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(double t$454) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$453 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2057;
  moonbit_incref(buf$453);
  _tmp$2057
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$453
  };
  $$moonbitlang$core$builtin$Show$$Double$$output(t$454, _tmp$2057);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$453);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(int32_t t$452) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$451 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2056;
  moonbit_incref(buf$451);
  _tmp$2056
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$451
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$1(t$452, _tmp$2056);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$451);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(int64_t t$450) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$449 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2055;
  moonbit_incref(buf$449);
  _tmp$2055
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$449
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$0(t$450, _tmp$2055);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$449);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(
  moonbit_string_t t$448
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$447 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2054;
  moonbit_incref(buf$447);
  _tmp$2054
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$447
  };
  $$moonbitlang$core$builtin$Show$$String$$output(t$448, _tmp$2054);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$447);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(int32_t t$446) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$445 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2053;
  moonbit_incref(buf$445);
  _tmp$2053
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$445
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(t$446, _tmp$2053);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$445);
}

moonbit_string_t $UInt64$$to_string$inner(
  uint64_t self$437,
  int32_t radix$436
) {
  int32_t _if_result$4137;
  uint16_t* buffer$438;
  if (radix$436 < 2) {
    _if_result$4137 = 1;
  } else {
    _if_result$4137 = radix$436 > 36;
  }
  if (_if_result$4137) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_127.data,
        (moonbit_string_t)moonbit_string_literal_128.data
    );
  }
  if (self$437 == 0ull) {
    return (moonbit_string_t)moonbit_string_literal_101.data;
  }
  switch (radix$436) {
    case 10: {
      int32_t len$439 = $moonbitlang$core$builtin$dec_count64(self$437);
      uint16_t* buffer$440 = (uint16_t*)moonbit_make_string(len$439, 0);
      moonbit_incref(buffer$440);
      $moonbitlang$core$builtin$int64_to_string_dec(
        buffer$440, self$437, 0, len$439
      );
      buffer$438 = buffer$440;
      break;
    }
    
    case 16: {
      int32_t len$441 = $moonbitlang$core$builtin$hex_count64(self$437);
      uint16_t* buffer$442 = (uint16_t*)moonbit_make_string(len$441, 0);
      moonbit_incref(buffer$442);
      $moonbitlang$core$builtin$int64_to_string_hex(
        buffer$442, self$437, 0, len$441
      );
      buffer$438 = buffer$442;
      break;
    }
    default: {
      int32_t len$443 =
        $moonbitlang$core$builtin$radix_count64(self$437, radix$436);
      uint16_t* buffer$444 = (uint16_t*)moonbit_make_string(len$443, 0);
      moonbit_incref(buffer$444);
      $moonbitlang$core$builtin$int64_to_string_generic(
        buffer$444, self$437, 0, len$443, radix$436
      );
      buffer$438 = buffer$444;
      break;
    }
  }
  return buffer$438;
}

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$426,
  uint64_t num$414,
  int32_t digit_start$417,
  int32_t total_len$416
) {
  uint64_t num$413 = num$414;
  int32_t offset$415 = total_len$416 - digit_start$417;
  uint64_t _tmp$2052;
  int32_t remaining$428;
  int32_t _tmp$2033;
  while (1) {
    uint64_t _tmp$1996 = num$413;
    if (_tmp$1996 >= 10000ull) {
      uint64_t _tmp$2019 = num$413;
      uint64_t t$418 = _tmp$2019 / 10000ull;
      uint64_t _tmp$2018 = num$413;
      uint64_t _tmp$2017 = _tmp$2018 % 10000ull;
      int32_t r$419 = (int32_t)_tmp$2017;
      int32_t d1$420;
      int32_t d2$421;
      int32_t _tmp$1997;
      int32_t _tmp$2016;
      int32_t _tmp$2015;
      int32_t d1_hi$422;
      int32_t _tmp$2014;
      int32_t _tmp$2013;
      int32_t d1_lo$423;
      int32_t _tmp$2012;
      int32_t _tmp$2011;
      int32_t d2_hi$424;
      int32_t _tmp$2010;
      int32_t _tmp$2009;
      int32_t d2_lo$425;
      int32_t _tmp$1999;
      int32_t _tmp$1998;
      int32_t _tmp$2002;
      int32_t _tmp$2001;
      int32_t _tmp$2000;
      int32_t _tmp$2005;
      int32_t _tmp$2004;
      int32_t _tmp$2003;
      int32_t _tmp$2008;
      int32_t _tmp$2007;
      int32_t _tmp$2006;
      num$413 = t$418;
      d1$420 = r$419 / 100;
      d2$421 = r$419 % 100;
      _tmp$1997 = offset$415;
      offset$415 = _tmp$1997 - 4;
      _tmp$2016 = d1$420 / 10;
      _tmp$2015 = 48 + _tmp$2016;
      d1_hi$422 = (uint16_t)_tmp$2015;
      _tmp$2014 = d1$420 % 10;
      _tmp$2013 = 48 + _tmp$2014;
      d1_lo$423 = (uint16_t)_tmp$2013;
      _tmp$2012 = d2$421 / 10;
      _tmp$2011 = 48 + _tmp$2012;
      d2_hi$424 = (uint16_t)_tmp$2011;
      _tmp$2010 = d2$421 % 10;
      _tmp$2009 = 48 + _tmp$2010;
      d2_lo$425 = (uint16_t)_tmp$2009;
      _tmp$1999 = offset$415;
      _tmp$1998 = digit_start$417 + _tmp$1999;
      buffer$426[_tmp$1998] = d1_hi$422;
      _tmp$2002 = offset$415;
      _tmp$2001 = digit_start$417 + _tmp$2002;
      _tmp$2000 = _tmp$2001 + 1;
      buffer$426[_tmp$2000] = d1_lo$423;
      _tmp$2005 = offset$415;
      _tmp$2004 = digit_start$417 + _tmp$2005;
      _tmp$2003 = _tmp$2004 + 2;
      buffer$426[_tmp$2003] = d2_hi$424;
      _tmp$2008 = offset$415;
      _tmp$2007 = digit_start$417 + _tmp$2008;
      _tmp$2006 = _tmp$2007 + 3;
      buffer$426[_tmp$2006] = d2_lo$425;
      continue;
    }
    break;
  }
  _tmp$2052 = num$413;
  remaining$428 = (int32_t)_tmp$2052;
  while (1) {
    int32_t _tmp$2020 = remaining$428;
    if (_tmp$2020 >= 100) {
      int32_t _tmp$2032 = remaining$428;
      int32_t t$429 = _tmp$2032 / 100;
      int32_t _tmp$2031 = remaining$428;
      int32_t d$430 = _tmp$2031 % 100;
      int32_t _tmp$2021;
      int32_t _tmp$2030;
      int32_t _tmp$2029;
      int32_t d_hi$431;
      int32_t _tmp$2028;
      int32_t _tmp$2027;
      int32_t d_lo$432;
      int32_t _tmp$2023;
      int32_t _tmp$2022;
      int32_t _tmp$2026;
      int32_t _tmp$2025;
      int32_t _tmp$2024;
      remaining$428 = t$429;
      _tmp$2021 = offset$415;
      offset$415 = _tmp$2021 - 2;
      _tmp$2030 = d$430 / 10;
      _tmp$2029 = 48 + _tmp$2030;
      d_hi$431 = (uint16_t)_tmp$2029;
      _tmp$2028 = d$430 % 10;
      _tmp$2027 = 48 + _tmp$2028;
      d_lo$432 = (uint16_t)_tmp$2027;
      _tmp$2023 = offset$415;
      _tmp$2022 = digit_start$417 + _tmp$2023;
      buffer$426[_tmp$2022] = d_hi$431;
      _tmp$2026 = offset$415;
      _tmp$2025 = digit_start$417 + _tmp$2026;
      _tmp$2024 = _tmp$2025 + 1;
      buffer$426[_tmp$2024] = d_lo$432;
      continue;
    }
    break;
  }
  _tmp$2033 = remaining$428;
  if (_tmp$2033 >= 10) {
    int32_t _tmp$2034 = offset$415;
    int32_t _tmp$2045;
    int32_t _tmp$2044;
    int32_t _tmp$2043;
    int32_t d_hi$434;
    int32_t _tmp$2042;
    int32_t _tmp$2041;
    int32_t _tmp$2040;
    int32_t d_lo$435;
    int32_t _tmp$2036;
    int32_t _tmp$2035;
    int32_t _tmp$2039;
    int32_t _tmp$2038;
    int32_t _tmp$2037;
    offset$415 = _tmp$2034 - 2;
    _tmp$2045 = remaining$428;
    _tmp$2044 = _tmp$2045 / 10;
    _tmp$2043 = 48 + _tmp$2044;
    d_hi$434 = (uint16_t)_tmp$2043;
    _tmp$2042 = remaining$428;
    _tmp$2041 = _tmp$2042 % 10;
    _tmp$2040 = 48 + _tmp$2041;
    d_lo$435 = (uint16_t)_tmp$2040;
    _tmp$2036 = offset$415;
    _tmp$2035 = digit_start$417 + _tmp$2036;
    buffer$426[_tmp$2035] = d_hi$434;
    _tmp$2039 = offset$415;
    _tmp$2038 = digit_start$417 + _tmp$2039;
    _tmp$2037 = _tmp$2038 + 1;
    buffer$426[_tmp$2037] = d_lo$435;
    moonbit_decref(buffer$426);
  } else {
    int32_t _tmp$2046 = offset$415;
    int32_t _tmp$2051;
    int32_t _tmp$2047;
    int32_t _tmp$2050;
    int32_t _tmp$2049;
    int32_t _tmp$2048;
    offset$415 = _tmp$2046 - 1;
    _tmp$2051 = offset$415;
    _tmp$2047 = digit_start$417 + _tmp$2051;
    _tmp$2050 = remaining$428;
    _tmp$2049 = 48 + _tmp$2050;
    _tmp$2048 = (uint16_t)_tmp$2049;
    buffer$426[_tmp$2047] = _tmp$2048;
    moonbit_decref(buffer$426);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$408,
  uint64_t num$402,
  int32_t digit_start$400,
  int32_t total_len$399,
  int32_t radix$404
) {
  int32_t offset$398 = total_len$399 - digit_start$400;
  uint64_t n$401 = num$402;
  uint64_t base$403 = $Int$$to_uint64(radix$404);
  int32_t _tmp$1976 = radix$404 - 1;
  int32_t _tmp$1975 = radix$404 & _tmp$1976;
  if (_tmp$1975 == 0) {
    int32_t shift$405 = moonbit_ctz32(radix$404);
    uint64_t mask$406 = base$403 - 1ull;
    while (1) {
      uint64_t _tmp$1977 = n$401;
      if (_tmp$1977 > 0ull) {
        int32_t _tmp$1978 = offset$398;
        uint64_t _tmp$1985;
        uint64_t _tmp$1984;
        int32_t digit$407;
        int32_t _tmp$1982;
        int32_t _tmp$1979;
        int32_t _tmp$1981;
        int32_t _tmp$1980;
        uint64_t _tmp$1983;
        offset$398 = _tmp$1978 - 1;
        _tmp$1985 = n$401;
        _tmp$1984 = _tmp$1985 & mask$406;
        digit$407 = (int32_t)_tmp$1984;
        _tmp$1982 = offset$398;
        _tmp$1979 = digit_start$400 + _tmp$1982;
        _tmp$1981
        = ((moonbit_string_t)moonbit_string_literal_129.data)[
          digit$407
        ];
        _tmp$1980 = (uint16_t)_tmp$1981;
        buffer$408[_tmp$1979] = _tmp$1980;
        _tmp$1983 = n$401;
        n$401 = _tmp$1983 >> (shift$405 & 63);
        continue;
      } else {
        moonbit_decref(buffer$408);
      }
      break;
    }
  } else {
    while (1) {
      uint64_t _tmp$1986 = n$401;
      if (_tmp$1986 > 0ull) {
        int32_t _tmp$1987 = offset$398;
        uint64_t _tmp$1995;
        uint64_t q$410;
        uint64_t _tmp$1993;
        uint64_t _tmp$1994;
        uint64_t _tmp$1992;
        int32_t digit$411;
        int32_t _tmp$1991;
        int32_t _tmp$1988;
        int32_t _tmp$1990;
        int32_t _tmp$1989;
        offset$398 = _tmp$1987 - 1;
        _tmp$1995 = n$401;
        q$410 = _tmp$1995 / base$403;
        _tmp$1993 = n$401;
        _tmp$1994 = q$410 * base$403;
        _tmp$1992 = _tmp$1993 - _tmp$1994;
        digit$411 = (int32_t)_tmp$1992;
        _tmp$1991 = offset$398;
        _tmp$1988 = digit_start$400 + _tmp$1991;
        _tmp$1990
        = ((moonbit_string_t)moonbit_string_literal_129.data)[
          digit$411
        ];
        _tmp$1989 = (uint16_t)_tmp$1990;
        buffer$408[_tmp$1988] = _tmp$1989;
        n$401 = q$410;
        continue;
      } else {
        moonbit_decref(buffer$408);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$395,
  uint64_t num$391,
  int32_t digit_start$389,
  int32_t total_len$388
) {
  int32_t offset$387 = total_len$388 - digit_start$389;
  uint64_t n$390 = num$391;
  int32_t _tmp$1970;
  while (1) {
    int32_t _tmp$1956 = offset$387;
    if (_tmp$1956 >= 2) {
      int32_t _tmp$1957 = offset$387;
      uint64_t _tmp$1969;
      uint64_t _tmp$1968;
      int32_t byte_val$392;
      int32_t hi$393;
      int32_t lo$394;
      int32_t _tmp$1961;
      int32_t _tmp$1958;
      int32_t _tmp$1960;
      int32_t _tmp$1959;
      int32_t _tmp$1966;
      int32_t _tmp$1965;
      int32_t _tmp$1962;
      int32_t _tmp$1964;
      int32_t _tmp$1963;
      uint64_t _tmp$1967;
      offset$387 = _tmp$1957 - 2;
      _tmp$1969 = n$390;
      _tmp$1968 = _tmp$1969 & 255ull;
      byte_val$392 = (int32_t)_tmp$1968;
      hi$393 = byte_val$392 / 16;
      lo$394 = byte_val$392 % 16;
      _tmp$1961 = offset$387;
      _tmp$1958 = digit_start$389 + _tmp$1961;
      _tmp$1960 = ((moonbit_string_t)moonbit_string_literal_129.data)[hi$393];
      _tmp$1959 = (uint16_t)_tmp$1960;
      buffer$395[_tmp$1958] = _tmp$1959;
      _tmp$1966 = offset$387;
      _tmp$1965 = digit_start$389 + _tmp$1966;
      _tmp$1962 = _tmp$1965 + 1;
      _tmp$1964 = ((moonbit_string_t)moonbit_string_literal_129.data)[lo$394];
      _tmp$1963 = (uint16_t)_tmp$1964;
      buffer$395[_tmp$1962] = _tmp$1963;
      _tmp$1967 = n$390;
      n$390 = _tmp$1967 >> 8;
      continue;
    }
    break;
  }
  _tmp$1970 = offset$387;
  if (_tmp$1970 == 1) {
    uint64_t _tmp$1974 = n$390;
    uint64_t _tmp$1973 = _tmp$1974 & 15ull;
    int32_t nibble$397 = (int32_t)_tmp$1973;
    int32_t _tmp$1972 =
      ((moonbit_string_t)moonbit_string_literal_129.data)[nibble$397];
    int32_t _tmp$1971 = (uint16_t)_tmp$1972;
    buffer$395[digit_start$389] = _tmp$1971;
    moonbit_decref(buffer$395);
  } else {
    moonbit_decref(buffer$395);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$381,
  int32_t radix$384
) {
  uint64_t num$382;
  uint64_t base$383;
  int32_t count$385;
  if (value$381 == 0ull) {
    return 1;
  }
  num$382 = value$381;
  base$383 = $Int$$to_uint64(radix$384);
  count$385 = 0;
  while (1) {
    uint64_t _tmp$1953 = num$382;
    if (_tmp$1953 > 0ull) {
      int32_t _tmp$1954 = count$385;
      uint64_t _tmp$1955;
      count$385 = _tmp$1954 + 1;
      _tmp$1955 = num$382;
      num$382 = _tmp$1955 / base$383;
      continue;
    }
    break;
  }
  return count$385;
}

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$379) {
  if (value$379 == 0ull) {
    return 1;
  } else {
    int32_t leading_zeros$380 = moonbit_clz64(value$379);
    int32_t _tmp$1952 = 63 - leading_zeros$380;
    int32_t _tmp$1951 = _tmp$1952 / 4;
    return _tmp$1951 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$378) {
  if (value$378 >= 10000000000ull) {
    if (value$378 >= 100000000000000ull) {
      if (value$378 >= 10000000000000000ull) {
        if (value$378 >= 1000000000000000000ull) {
          if (value$378 >= 10000000000000000000ull) {
            return 20;
          } else {
            return 19;
          }
        } else if (value$378 >= 100000000000000000ull) {
          return 18;
        } else {
          return 17;
        }
      } else if (value$378 >= 1000000000000000ull) {
        return 16;
      } else {
        return 15;
      }
    } else if (value$378 >= 1000000000000ull) {
      if (value$378 >= 10000000000000ull) {
        return 14;
      } else {
        return 13;
      }
    } else if (value$378 >= 100000000000ull) {
      return 12;
    } else {
      return 11;
    }
  } else if (value$378 >= 100000ull) {
    if (value$378 >= 10000000ull) {
      if (value$378 >= 1000000000ull) {
        return 10;
      } else if (value$378 >= 100000000ull) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$378 >= 1000000ull) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$378 >= 1000ull) {
    if (value$378 >= 10000ull) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$378 >= 100ull) {
    return 3;
  } else if (value$378 >= 10ull) {
    return 2;
  } else {
    return 1;
  }
}

moonbit_string_t $Int$$to_string$inner(int32_t self$362, int32_t radix$361) {
  int32_t _if_result$4144;
  int32_t is_negative$363;
  uint32_t num$364;
  uint16_t* buffer$365;
  if (radix$361 < 2) {
    _if_result$4144 = 1;
  } else {
    _if_result$4144 = radix$361 > 36;
  }
  if (_if_result$4144) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_127.data,
        (moonbit_string_t)moonbit_string_literal_130.data
    );
  }
  if (self$362 == 0) {
    return (moonbit_string_t)moonbit_string_literal_101.data;
  }
  is_negative$363 = self$362 < 0;
  if (is_negative$363) {
    int32_t _tmp$1950 = -self$362;
    num$364 = *(uint32_t*)&_tmp$1950;
  } else {
    num$364 = *(uint32_t*)&self$362;
  }
  switch (radix$361) {
    case 10: {
      int32_t digit_len$366 = $moonbitlang$core$builtin$dec_count32(num$364);
      int32_t _tmp$1947;
      int32_t total_len$367;
      uint16_t* buffer$368;
      int32_t digit_start$369;
      if (is_negative$363) {
        _tmp$1947 = 1;
      } else {
        _tmp$1947 = 0;
      }
      total_len$367 = digit_len$366 + _tmp$1947;
      buffer$368 = (uint16_t*)moonbit_make_string(total_len$367, 0);
      if (is_negative$363) {
        digit_start$369 = 1;
      } else {
        digit_start$369 = 0;
      }
      moonbit_incref(buffer$368);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$368, num$364, digit_start$369, total_len$367
      );
      buffer$365 = buffer$368;
      break;
    }
    
    case 16: {
      int32_t digit_len$370 = $moonbitlang$core$builtin$hex_count32(num$364);
      int32_t _tmp$1948;
      int32_t total_len$371;
      uint16_t* buffer$372;
      int32_t digit_start$373;
      if (is_negative$363) {
        _tmp$1948 = 1;
      } else {
        _tmp$1948 = 0;
      }
      total_len$371 = digit_len$370 + _tmp$1948;
      buffer$372 = (uint16_t*)moonbit_make_string(total_len$371, 0);
      if (is_negative$363) {
        digit_start$373 = 1;
      } else {
        digit_start$373 = 0;
      }
      moonbit_incref(buffer$372);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$372, num$364, digit_start$373, total_len$371
      );
      buffer$365 = buffer$372;
      break;
    }
    default: {
      int32_t digit_len$374 =
        $moonbitlang$core$builtin$radix_count32(num$364, radix$361);
      int32_t _tmp$1949;
      int32_t total_len$375;
      uint16_t* buffer$376;
      int32_t digit_start$377;
      if (is_negative$363) {
        _tmp$1949 = 1;
      } else {
        _tmp$1949 = 0;
      }
      total_len$375 = digit_len$374 + _tmp$1949;
      buffer$376 = (uint16_t*)moonbit_make_string(total_len$375, 0);
      if (is_negative$363) {
        digit_start$377 = 1;
      } else {
        digit_start$377 = 0;
      }
      moonbit_incref(buffer$376);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$376, num$364, digit_start$377, total_len$375, radix$361
      );
      buffer$365 = buffer$376;
      break;
    }
  }
  if (is_negative$363) {
    buffer$365[0] = 45;
  }
  return buffer$365;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$355,
  int32_t radix$358
) {
  uint32_t num$356;
  uint32_t base$357;
  int32_t count$359;
  if (value$355 == 0u) {
    return 1;
  }
  num$356 = value$355;
  base$357 = *(uint32_t*)&radix$358;
  count$359 = 0;
  while (1) {
    uint32_t _tmp$1944 = num$356;
    if (_tmp$1944 > 0u) {
      int32_t _tmp$1945 = count$359;
      uint32_t _tmp$1946;
      count$359 = _tmp$1945 + 1;
      _tmp$1946 = num$356;
      num$356 = _tmp$1946 / base$357;
      continue;
    }
    break;
  }
  return count$359;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$353) {
  if (value$353 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$354 = moonbit_clz32(value$353);
    int32_t _tmp$1943 = 31 - leading_zeros$354;
    int32_t _tmp$1942 = _tmp$1943 / 4;
    return _tmp$1942 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$352) {
  if (value$352 >= 100000u) {
    if (value$352 >= 10000000u) {
      if (value$352 >= 1000000000u) {
        return 10;
      } else if (value$352 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$352 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$352 >= 1000u) {
    if (value$352 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$352 >= 100u) {
    return 3;
  } else if (value$352 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$342,
  uint32_t num$330,
  int32_t digit_start$333,
  int32_t total_len$332
) {
  uint32_t num$329 = num$330;
  int32_t offset$331 = total_len$332 - digit_start$333;
  uint32_t _tmp$1941;
  int32_t remaining$344;
  int32_t _tmp$1922;
  while (1) {
    uint32_t _tmp$1885 = num$329;
    if (_tmp$1885 >= 10000u) {
      uint32_t _tmp$1908 = num$329;
      uint32_t t$334 = _tmp$1908 / 10000u;
      uint32_t _tmp$1907 = num$329;
      uint32_t _tmp$1906 = _tmp$1907 % 10000u;
      int32_t r$335 = *(int32_t*)&_tmp$1906;
      int32_t d1$336;
      int32_t d2$337;
      int32_t _tmp$1886;
      int32_t _tmp$1905;
      int32_t _tmp$1904;
      int32_t d1_hi$338;
      int32_t _tmp$1903;
      int32_t _tmp$1902;
      int32_t d1_lo$339;
      int32_t _tmp$1901;
      int32_t _tmp$1900;
      int32_t d2_hi$340;
      int32_t _tmp$1899;
      int32_t _tmp$1898;
      int32_t d2_lo$341;
      int32_t _tmp$1888;
      int32_t _tmp$1887;
      int32_t _tmp$1891;
      int32_t _tmp$1890;
      int32_t _tmp$1889;
      int32_t _tmp$1894;
      int32_t _tmp$1893;
      int32_t _tmp$1892;
      int32_t _tmp$1897;
      int32_t _tmp$1896;
      int32_t _tmp$1895;
      num$329 = t$334;
      d1$336 = r$335 / 100;
      d2$337 = r$335 % 100;
      _tmp$1886 = offset$331;
      offset$331 = _tmp$1886 - 4;
      _tmp$1905 = d1$336 / 10;
      _tmp$1904 = 48 + _tmp$1905;
      d1_hi$338 = (uint16_t)_tmp$1904;
      _tmp$1903 = d1$336 % 10;
      _tmp$1902 = 48 + _tmp$1903;
      d1_lo$339 = (uint16_t)_tmp$1902;
      _tmp$1901 = d2$337 / 10;
      _tmp$1900 = 48 + _tmp$1901;
      d2_hi$340 = (uint16_t)_tmp$1900;
      _tmp$1899 = d2$337 % 10;
      _tmp$1898 = 48 + _tmp$1899;
      d2_lo$341 = (uint16_t)_tmp$1898;
      _tmp$1888 = offset$331;
      _tmp$1887 = digit_start$333 + _tmp$1888;
      buffer$342[_tmp$1887] = d1_hi$338;
      _tmp$1891 = offset$331;
      _tmp$1890 = digit_start$333 + _tmp$1891;
      _tmp$1889 = _tmp$1890 + 1;
      buffer$342[_tmp$1889] = d1_lo$339;
      _tmp$1894 = offset$331;
      _tmp$1893 = digit_start$333 + _tmp$1894;
      _tmp$1892 = _tmp$1893 + 2;
      buffer$342[_tmp$1892] = d2_hi$340;
      _tmp$1897 = offset$331;
      _tmp$1896 = digit_start$333 + _tmp$1897;
      _tmp$1895 = _tmp$1896 + 3;
      buffer$342[_tmp$1895] = d2_lo$341;
      continue;
    }
    break;
  }
  _tmp$1941 = num$329;
  remaining$344 = *(int32_t*)&_tmp$1941;
  while (1) {
    int32_t _tmp$1909 = remaining$344;
    if (_tmp$1909 >= 100) {
      int32_t _tmp$1921 = remaining$344;
      int32_t t$345 = _tmp$1921 / 100;
      int32_t _tmp$1920 = remaining$344;
      int32_t d$346 = _tmp$1920 % 100;
      int32_t _tmp$1910;
      int32_t _tmp$1919;
      int32_t _tmp$1918;
      int32_t d_hi$347;
      int32_t _tmp$1917;
      int32_t _tmp$1916;
      int32_t d_lo$348;
      int32_t _tmp$1912;
      int32_t _tmp$1911;
      int32_t _tmp$1915;
      int32_t _tmp$1914;
      int32_t _tmp$1913;
      remaining$344 = t$345;
      _tmp$1910 = offset$331;
      offset$331 = _tmp$1910 - 2;
      _tmp$1919 = d$346 / 10;
      _tmp$1918 = 48 + _tmp$1919;
      d_hi$347 = (uint16_t)_tmp$1918;
      _tmp$1917 = d$346 % 10;
      _tmp$1916 = 48 + _tmp$1917;
      d_lo$348 = (uint16_t)_tmp$1916;
      _tmp$1912 = offset$331;
      _tmp$1911 = digit_start$333 + _tmp$1912;
      buffer$342[_tmp$1911] = d_hi$347;
      _tmp$1915 = offset$331;
      _tmp$1914 = digit_start$333 + _tmp$1915;
      _tmp$1913 = _tmp$1914 + 1;
      buffer$342[_tmp$1913] = d_lo$348;
      continue;
    }
    break;
  }
  _tmp$1922 = remaining$344;
  if (_tmp$1922 >= 10) {
    int32_t _tmp$1923 = offset$331;
    int32_t _tmp$1934;
    int32_t _tmp$1933;
    int32_t _tmp$1932;
    int32_t d_hi$350;
    int32_t _tmp$1931;
    int32_t _tmp$1930;
    int32_t _tmp$1929;
    int32_t d_lo$351;
    int32_t _tmp$1925;
    int32_t _tmp$1924;
    int32_t _tmp$1928;
    int32_t _tmp$1927;
    int32_t _tmp$1926;
    offset$331 = _tmp$1923 - 2;
    _tmp$1934 = remaining$344;
    _tmp$1933 = _tmp$1934 / 10;
    _tmp$1932 = 48 + _tmp$1933;
    d_hi$350 = (uint16_t)_tmp$1932;
    _tmp$1931 = remaining$344;
    _tmp$1930 = _tmp$1931 % 10;
    _tmp$1929 = 48 + _tmp$1930;
    d_lo$351 = (uint16_t)_tmp$1929;
    _tmp$1925 = offset$331;
    _tmp$1924 = digit_start$333 + _tmp$1925;
    buffer$342[_tmp$1924] = d_hi$350;
    _tmp$1928 = offset$331;
    _tmp$1927 = digit_start$333 + _tmp$1928;
    _tmp$1926 = _tmp$1927 + 1;
    buffer$342[_tmp$1926] = d_lo$351;
    moonbit_decref(buffer$342);
  } else {
    int32_t _tmp$1935 = offset$331;
    int32_t _tmp$1940;
    int32_t _tmp$1936;
    int32_t _tmp$1939;
    int32_t _tmp$1938;
    int32_t _tmp$1937;
    offset$331 = _tmp$1935 - 1;
    _tmp$1940 = offset$331;
    _tmp$1936 = digit_start$333 + _tmp$1940;
    _tmp$1939 = remaining$344;
    _tmp$1938 = 48 + _tmp$1939;
    _tmp$1937 = (uint16_t)_tmp$1938;
    buffer$342[_tmp$1936] = _tmp$1937;
    moonbit_decref(buffer$342);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$324,
  uint32_t num$318,
  int32_t digit_start$316,
  int32_t total_len$315,
  int32_t radix$320
) {
  int32_t offset$314 = total_len$315 - digit_start$316;
  uint32_t n$317 = num$318;
  uint32_t base$319 = *(uint32_t*)&radix$320;
  int32_t _tmp$1865 = radix$320 - 1;
  int32_t _tmp$1864 = radix$320 & _tmp$1865;
  if (_tmp$1864 == 0) {
    int32_t shift$321 = moonbit_ctz32(radix$320);
    uint32_t mask$322 = base$319 - 1u;
    while (1) {
      uint32_t _tmp$1866 = n$317;
      if (_tmp$1866 > 0u) {
        int32_t _tmp$1867 = offset$314;
        uint32_t _tmp$1874;
        uint32_t _tmp$1873;
        int32_t digit$323;
        int32_t _tmp$1871;
        int32_t _tmp$1868;
        int32_t _tmp$1870;
        int32_t _tmp$1869;
        uint32_t _tmp$1872;
        offset$314 = _tmp$1867 - 1;
        _tmp$1874 = n$317;
        _tmp$1873 = _tmp$1874 & mask$322;
        digit$323 = *(int32_t*)&_tmp$1873;
        _tmp$1871 = offset$314;
        _tmp$1868 = digit_start$316 + _tmp$1871;
        _tmp$1870
        = ((moonbit_string_t)moonbit_string_literal_129.data)[
          digit$323
        ];
        _tmp$1869 = (uint16_t)_tmp$1870;
        buffer$324[_tmp$1868] = _tmp$1869;
        _tmp$1872 = n$317;
        n$317 = _tmp$1872 >> (shift$321 & 31);
        continue;
      } else {
        moonbit_decref(buffer$324);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1875 = n$317;
      if (_tmp$1875 > 0u) {
        int32_t _tmp$1876 = offset$314;
        uint32_t _tmp$1884;
        uint32_t q$326;
        uint32_t _tmp$1882;
        uint32_t _tmp$1883;
        uint32_t _tmp$1881;
        int32_t digit$327;
        int32_t _tmp$1880;
        int32_t _tmp$1877;
        int32_t _tmp$1879;
        int32_t _tmp$1878;
        offset$314 = _tmp$1876 - 1;
        _tmp$1884 = n$317;
        q$326 = _tmp$1884 / base$319;
        _tmp$1882 = n$317;
        _tmp$1883 = q$326 * base$319;
        _tmp$1881 = _tmp$1882 - _tmp$1883;
        digit$327 = *(int32_t*)&_tmp$1881;
        _tmp$1880 = offset$314;
        _tmp$1877 = digit_start$316 + _tmp$1880;
        _tmp$1879
        = ((moonbit_string_t)moonbit_string_literal_129.data)[
          digit$327
        ];
        _tmp$1878 = (uint16_t)_tmp$1879;
        buffer$324[_tmp$1877] = _tmp$1878;
        n$317 = q$326;
        continue;
      } else {
        moonbit_decref(buffer$324);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$311,
  uint32_t num$307,
  int32_t digit_start$305,
  int32_t total_len$304
) {
  int32_t offset$303 = total_len$304 - digit_start$305;
  uint32_t n$306 = num$307;
  int32_t _tmp$1859;
  while (1) {
    int32_t _tmp$1845 = offset$303;
    if (_tmp$1845 >= 2) {
      int32_t _tmp$1846 = offset$303;
      uint32_t _tmp$1858;
      uint32_t _tmp$1857;
      int32_t byte_val$308;
      int32_t hi$309;
      int32_t lo$310;
      int32_t _tmp$1850;
      int32_t _tmp$1847;
      int32_t _tmp$1849;
      int32_t _tmp$1848;
      int32_t _tmp$1855;
      int32_t _tmp$1854;
      int32_t _tmp$1851;
      int32_t _tmp$1853;
      int32_t _tmp$1852;
      uint32_t _tmp$1856;
      offset$303 = _tmp$1846 - 2;
      _tmp$1858 = n$306;
      _tmp$1857 = _tmp$1858 & 255u;
      byte_val$308 = *(int32_t*)&_tmp$1857;
      hi$309 = byte_val$308 / 16;
      lo$310 = byte_val$308 % 16;
      _tmp$1850 = offset$303;
      _tmp$1847 = digit_start$305 + _tmp$1850;
      _tmp$1849 = ((moonbit_string_t)moonbit_string_literal_129.data)[hi$309];
      _tmp$1848 = (uint16_t)_tmp$1849;
      buffer$311[_tmp$1847] = _tmp$1848;
      _tmp$1855 = offset$303;
      _tmp$1854 = digit_start$305 + _tmp$1855;
      _tmp$1851 = _tmp$1854 + 1;
      _tmp$1853 = ((moonbit_string_t)moonbit_string_literal_129.data)[lo$310];
      _tmp$1852 = (uint16_t)_tmp$1853;
      buffer$311[_tmp$1851] = _tmp$1852;
      _tmp$1856 = n$306;
      n$306 = _tmp$1856 >> 8;
      continue;
    }
    break;
  }
  _tmp$1859 = offset$303;
  if (_tmp$1859 == 1) {
    uint32_t _tmp$1863 = n$306;
    uint32_t _tmp$1862 = _tmp$1863 & 15u;
    int32_t nibble$313 = *(int32_t*)&_tmp$1862;
    int32_t _tmp$1861 =
      ((moonbit_string_t)moonbit_string_literal_129.data)[nibble$313];
    int32_t _tmp$1860 = (uint16_t)_tmp$1861;
    buffer$311[digit_start$305] = _tmp$1860;
    moonbit_decref(buffer$311);
  } else {
    moonbit_decref(buffer$311);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
  void* self$302
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$301 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1844;
  moonbit_incref(logger$301);
  _tmp$1844
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$301
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$302, _tmp$1844
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$301);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$300
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$299 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1843;
  moonbit_incref(logger$299);
  _tmp$1843
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$299
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$300, _tmp$1843
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$299);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  uint64_t self$298
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$297 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1842;
  moonbit_incref(logger$297);
  _tmp$1842
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$297
  };
  $$moonbitlang$core$builtin$Show$$UInt64$$output(self$298, _tmp$1842);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$297);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$296
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$295 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1841;
  moonbit_incref(logger$295);
  _tmp$1841
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$295
  };
  $$moonbitlang$core$builtin$Show$$Bool$$output(self$296, _tmp$1841);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$295);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$294
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$293 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1840;
  moonbit_incref(logger$293);
  _tmp$1840
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$293
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$294, _tmp$1840
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$293);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$292
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$291 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1839;
  moonbit_incref(logger$291);
  _tmp$1839
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$291
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$292, _tmp$1839);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$291);
}

int32_t $StringView$$start_offset(struct $StringView self$290) {
  int32_t _field$3724 = self$290.$1;
  moonbit_decref(self$290.$0);
  return _field$3724;
}

moonbit_string_t $StringView$$data(struct $StringView self$289) {
  moonbit_string_t _field$3725 = self$289.$0;
  return _field$3725;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$283,
  moonbit_string_t value$286,
  int32_t start$287,
  int32_t len$288
) {
  void* _try_err$285;
  struct $StringView _tmp$1834;
  int32_t _tmp$1836 = start$287 + len$288;
  int64_t _tmp$1835 = (int64_t)_tmp$1836;
  struct moonbit_result_1 _tmp$4152 =
    $String$$sub$inner(value$286, start$287, _tmp$1835);
  if (_tmp$4152.tag) {
    struct $StringView const _ok$1837 = _tmp$4152.data.ok;
    _tmp$1834 = _ok$1837;
  } else {
    void* const _err$1838 = _tmp$4152.data.err;
    _try_err$285 = _err$1838;
    goto $join$284;
  }
  goto $joinlet$4151;
  $join$284:;
  moonbit_decref(_try_err$285);
  moonbit_panic();
  $joinlet$4151:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$283, _tmp$1834
  );
  return 0;
}

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$276,
  int32_t start$282,
  int64_t end$278
) {
  int32_t len$275 = Moonbit_array_length(self$276);
  int32_t end$277;
  int32_t start$281;
  int32_t _if_result$4153;
  if (end$278 == 4294967296ll) {
    end$277 = len$275;
  } else {
    int64_t _Some$279 = end$278;
    int32_t _end$280 = (int32_t)_Some$279;
    if (_end$280 < 0) {
      end$277 = len$275 + _end$280;
    } else {
      end$277 = _end$280;
    }
  }
  if (start$282 < 0) {
    start$281 = len$275 + start$282;
  } else {
    start$281 = start$282;
  }
  if (start$281 >= 0) {
    if (start$281 <= end$277) {
      _if_result$4153 = end$277 <= len$275;
    } else {
      _if_result$4153 = 0;
    }
  } else {
    _if_result$4153 = 0;
  }
  if (_if_result$4153) {
    int32_t _if_result$4154;
    int32_t _if_result$4156;
    struct $StringView _tmp$1832;
    struct moonbit_result_1 _result$4158;
    if (start$281 < len$275) {
      int32_t _tmp$1828 = self$276[start$281];
      _if_result$4154 = $Int$$is_trailing_surrogate(_tmp$1828);
    } else {
      _if_result$4154 = 0;
    }
    if (_if_result$4154) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1829;
      struct moonbit_result_1 _result$4155;
      moonbit_decref(self$276);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1829
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$4155.tag = 0;
      _result$4155.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1829;
      return _result$4155;
    }
    if (end$277 < len$275) {
      int32_t _tmp$1830 = self$276[end$277];
      _if_result$4156 = $Int$$is_trailing_surrogate(_tmp$1830);
    } else {
      _if_result$4156 = 0;
    }
    if (_if_result$4156) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1831;
      struct moonbit_result_1 _result$4157;
      moonbit_decref(self$276);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1831
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$4157.tag = 0;
      _result$4157.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1831;
      return _result$4157;
    }
    _tmp$1832 = (struct $StringView){start$281, end$277, self$276};
    _result$4158.tag = 1;
    _result$4158.data.ok = _tmp$1832;
    return _result$4158;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1833;
    struct moonbit_result_1 _result$4159;
    moonbit_decref(self$276);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1833
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$4159.tag = 0;
    _result$4159.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1833;
    return _result$4159;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$274
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$273 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1827;
  moonbit_incref(_self$273);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$273, self$274);
  _tmp$1827 = _self$273;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1827);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$272
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$271 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1826;
  moonbit_incref(_self$271);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$271, self$272);
  _tmp$1826 = _self$271;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1826);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$269
) {
  int32_t seed$268;
  if (seed$opt$269 == 4294967296ll) {
    seed$268 = 0;
  } else {
    int64_t _Some$270 = seed$opt$269;
    seed$268 = (int32_t)_Some$270;
  }
  return $$moonbitlang$core$builtin$Hasher$$new$inner(seed$268);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$267
) {
  uint32_t _tmp$1825 = *(uint32_t*)&seed$267;
  uint32_t _tmp$1824 = _tmp$1825 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$4160 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$4160)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$4160->$0 = _tmp$1824;
  return _block$4160;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$266
) {
  uint32_t _tmp$1823 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$266);
  return *(int32_t*)&_tmp$1823;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$265
) {
  uint32_t _field$3726 = self$265->$0;
  uint32_t acc$264;
  uint32_t _tmp$1812;
  uint32_t _tmp$1814;
  uint32_t _tmp$1813;
  uint32_t _tmp$1815;
  uint32_t _tmp$1816;
  uint32_t _tmp$1818;
  uint32_t _tmp$1817;
  uint32_t _tmp$1819;
  uint32_t _tmp$1820;
  uint32_t _tmp$1822;
  uint32_t _tmp$1821;
  moonbit_decref(self$265);
  acc$264 = _field$3726;
  _tmp$1812 = acc$264;
  _tmp$1814 = acc$264;
  _tmp$1813 = _tmp$1814 >> 15;
  acc$264 = _tmp$1812 ^ _tmp$1813;
  _tmp$1815 = acc$264;
  acc$264 = _tmp$1815 * 2246822519u;
  _tmp$1816 = acc$264;
  _tmp$1818 = acc$264;
  _tmp$1817 = _tmp$1818 >> 13;
  acc$264 = _tmp$1816 ^ _tmp$1817;
  _tmp$1819 = acc$264;
  acc$264 = _tmp$1819 * 3266489917u;
  _tmp$1820 = acc$264;
  _tmp$1822 = acc$264;
  _tmp$1821 = _tmp$1822 >> 16;
  acc$264 = _tmp$1820 ^ _tmp$1821;
  return acc$264;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$263,
  int32_t value$262
) {
  $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(value$262, self$263);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$261,
  moonbit_string_t value$260
) {
  $$moonbitlang$core$builtin$Hash$$String$$hash_combine(value$260, self$261);
  return 0;
}

uint64_t $Int$$to_uint64(int32_t self$259) {
  int64_t _tmp$1811 = (int64_t)self$259;
  return *(uint64_t*)&_tmp$1811;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$257,
  int32_t value$258
) {
  uint32_t _tmp$1810 = *(uint32_t*)&value$258;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$257, _tmp$1810);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$255,
  uint32_t value$256
) {
  uint32_t acc$1809 = self$255->$0;
  uint32_t _tmp$1808 = acc$1809 + 4u;
  self$255->$0 = _tmp$1808;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$255, value$256);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$253,
  uint32_t input$254
) {
  uint32_t acc$1806 = self$253->$0;
  uint32_t _tmp$1807 = input$254 * 3266489917u;
  uint32_t _tmp$1805 = acc$1806 + _tmp$1807;
  uint32_t _tmp$1804 = $moonbitlang$core$builtin$rotl(_tmp$1805, 17);
  uint32_t _tmp$1803 = _tmp$1804 * 668265263u;
  self$253->$0 = _tmp$1803;
  moonbit_decref(self$253);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$251, int32_t r$252) {
  uint32_t _tmp$1800 = x$251 << (r$252 & 31);
  int32_t _tmp$1802 = 32 - r$252;
  uint32_t _tmp$1801 = x$251 >> (_tmp$1802 & 31);
  return _tmp$1800 | _tmp$1801;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$249,
  moonbit_string_t str$250
) {
  int32_t len$1790 = self$249->$1;
  int32_t _tmp$1792 = Moonbit_array_length(str$250);
  int32_t _tmp$1791 = _tmp$1792 * 2;
  int32_t _tmp$1789 = len$1790 + _tmp$1791;
  moonbit_bytes_t _field$3728;
  moonbit_bytes_t data$1793;
  int32_t len$1794;
  int32_t _tmp$1795;
  int32_t len$1797;
  int32_t _tmp$3727;
  int32_t _tmp$1799;
  int32_t _tmp$1798;
  int32_t _tmp$1796;
  moonbit_incref(self$249);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$249, _tmp$1789
  );
  _field$3728 = self$249->$0;
  data$1793 = _field$3728;
  len$1794 = self$249->$1;
  _tmp$1795 = Moonbit_array_length(str$250);
  moonbit_incref(data$1793);
  moonbit_incref(str$250);
  $FixedArray$$blit_from_string(data$1793, len$1794, str$250, 0, _tmp$1795);
  len$1797 = self$249->$1;
  _tmp$3727 = Moonbit_array_length(str$250);
  moonbit_decref(str$250);
  _tmp$1799 = _tmp$3727;
  _tmp$1798 = _tmp$1799 * 2;
  _tmp$1796 = len$1797 + _tmp$1798;
  self$249->$1 = _tmp$1796;
  moonbit_decref(self$249);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$241,
  int32_t bytes_offset$236,
  moonbit_string_t str$243,
  int32_t str_offset$239,
  int32_t length$237
) {
  int32_t _tmp$1788 = length$237 * 2;
  int32_t _tmp$1787 = bytes_offset$236 + _tmp$1788;
  int32_t e1$235 = _tmp$1787 - 1;
  int32_t _tmp$1786 = str_offset$239 + length$237;
  int32_t e2$238 = _tmp$1786 - 1;
  int32_t len1$240 = Moonbit_array_length(self$241);
  int32_t len2$242 = Moonbit_array_length(str$243);
  int32_t _if_result$4161;
  if (length$237 >= 0) {
    if (bytes_offset$236 >= 0) {
      if (e1$235 < len1$240) {
        if (str_offset$239 >= 0) {
          _if_result$4161 = e2$238 < len2$242;
        } else {
          _if_result$4161 = 0;
        }
      } else {
        _if_result$4161 = 0;
      }
    } else {
      _if_result$4161 = 0;
    }
  } else {
    _if_result$4161 = 0;
  }
  if (_if_result$4161) {
    int32_t end_str_offset$244 = str_offset$239 + length$237;
    int32_t i$245 = str_offset$239;
    int32_t j$246 = bytes_offset$236;
    while (1) {
      if (i$245 < end_str_offset$244) {
        int32_t _tmp$1783 = str$243[i$245];
        uint32_t c$247 = *(uint32_t*)&_tmp$1783;
        uint32_t _tmp$1779 = c$247 & 255u;
        int32_t _tmp$1778 = $UInt$$to_byte(_tmp$1779);
        int32_t _tmp$1780;
        uint32_t _tmp$1782;
        int32_t _tmp$1781;
        int32_t _tmp$1784;
        int32_t _tmp$1785;
        if (j$246 < 0 || j$246 >= Moonbit_array_length(self$241)) {
          moonbit_panic();
        }
        self$241[j$246] = _tmp$1778;
        _tmp$1780 = j$246 + 1;
        _tmp$1782 = c$247 >> 8;
        _tmp$1781 = $UInt$$to_byte(_tmp$1782);
        if (_tmp$1780 < 0 || _tmp$1780 >= Moonbit_array_length(self$241)) {
          moonbit_panic();
        }
        self$241[_tmp$1780] = _tmp$1781;
        _tmp$1784 = i$245 + 1;
        _tmp$1785 = j$246 + 2;
        i$245 = _tmp$1784;
        j$246 = _tmp$1785;
        continue;
      } else {
        moonbit_decref(str$243);
        moonbit_decref(self$241);
      }
      break;
    }
  } else {
    moonbit_decref(str$243);
    moonbit_decref(self$241);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$203
) {
  int32_t _tmp$1751 = Moonbit_array_length(repr$203);
  int64_t _tmp$1750 = (int64_t)_tmp$1751;
  moonbit_incref(repr$203);
  if ($String$$char_length_ge$inner(repr$203, 1, 0, _tmp$1750)) {
    int32_t _tmp$1777 = repr$203[0];
    int32_t _x$204 = _tmp$1777;
    if (_x$204 == 64) {
      int32_t _tmp$1776 = Moonbit_array_length(repr$203);
      int64_t _tmp$1775 = (int64_t)_tmp$1776;
      int64_t _bind$1504;
      int32_t _tmp$1773;
      int32_t _tmp$1774;
      struct $StringView _x$205;
      int32_t _tmp$1772;
      struct $StringView _tmp$1771;
      int64_t _bind$207;
      moonbit_incref(repr$203);
      _bind$1504
      = $String$$offset_of_nth_char$inner(
        repr$203, 1, 0, _tmp$1775
      );
      if (_bind$1504 == 4294967296ll) {
        _tmp$1773 = Moonbit_array_length(repr$203);
      } else {
        int64_t _Some$206 = _bind$1504;
        _tmp$1773 = (int32_t)_Some$206;
      }
      _tmp$1774 = Moonbit_array_length(repr$203);
      _x$205 = (struct $StringView){_tmp$1773, _tmp$1774, repr$203};
      _tmp$1772
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1771
      = (struct $StringView){
        0, _tmp$1772, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$205.$0);
      _bind$207 = $StringView$$find(_x$205, _tmp$1771);
      if (_bind$207 == 4294967296ll) {
        moonbit_decref(_x$205.$0);
        moonbit_panic();
      } else {
        int64_t _Some$208 = _bind$207;
        int32_t _pkg_end$209 = (int32_t)_Some$208;
        int64_t _tmp$1770 = (int64_t)_pkg_end$209;
        struct $StringView pkg$210;
        int32_t _tmp$1769;
        struct $StringView _tmp$1768;
        int64_t _bind$211;
        moonbit_incref(_x$205.$0);
        pkg$210 = $StringView$$view$inner(_x$205, 0, _tmp$1770);
        _tmp$1769
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1768
        = (struct $StringView){
          0, _tmp$1769, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$205.$0);
        _bind$211 = $StringView$$rev_find(_x$205, _tmp$1768);
        if (_bind$211 == 4294967296ll) {
          moonbit_decref(pkg$210.$0);
          moonbit_decref(_x$205.$0);
          moonbit_panic();
        } else {
          int64_t _Some$212 = _bind$211;
          int32_t _start_loc_end$213 = (int32_t)_Some$212;
          int32_t _tmp$1752 = _start_loc_end$213 + 1;
          int32_t _tmp$1753;
          moonbit_incref(_x$205.$0);
          _tmp$1753 = $StringView$$length(_x$205);
          if (_tmp$1752 < _tmp$1753) {
            int32_t _tmp$1767 = _start_loc_end$213 + 1;
            struct $StringView end_loc$214;
            struct $$3c$StringView$2a$StringView$3e$* _bind$215;
            moonbit_incref(_x$205.$0);
            end_loc$214
            = $StringView$$view$inner(
              _x$205, _tmp$1767, 4294967296ll
            );
            _bind$215
            = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
              end_loc$214
            );
            if (_bind$215 == 0) {
              if (_bind$215) {
                moonbit_decref(_bind$215);
              }
              moonbit_decref(pkg$210.$0);
              moonbit_decref(_x$205.$0);
              moonbit_panic();
            } else {
              struct $$3c$StringView$2a$StringView$3e$* _Some$216 = _bind$215;
              struct $$3c$StringView$2a$StringView$3e$* _x$217 = _Some$216;
              struct $StringView _field$3732 =
                (struct $StringView){
                  _x$217->$0_1, _x$217->$0_2, _x$217->$0_0
                };
              struct $StringView _end_line$218 = _field$3732;
              struct $StringView _field$3731 =
                (struct $StringView){
                  _x$217->$1_1, _x$217->$1_2, _x$217->$1_0
                };
              int32_t _cnt$3883 = Moonbit_object_header(_x$217)->rc;
              struct $StringView _end_column$219;
              int64_t _tmp$1766;
              struct $StringView rest$220;
              int32_t _tmp$1765;
              struct $StringView _tmp$1764;
              int64_t _bind$222;
              if (_cnt$3883 > 1) {
                int32_t _new_cnt$3884;
                moonbit_incref(_field$3731.$0);
                moonbit_incref(_end_line$218.$0);
                _new_cnt$3884 = _cnt$3883 - 1;
                Moonbit_object_header(_x$217)->rc = _new_cnt$3884;
              } else if (_cnt$3883 == 1) {
                moonbit_free(_x$217);
              }
              _end_column$219 = _field$3731;
              _tmp$1766 = (int64_t)_start_loc_end$213;
              rest$220 = $StringView$$view$inner(_x$205, 0, _tmp$1766);
              _tmp$1765
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1764
              = (struct $StringView){
                0,
                  _tmp$1765,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$220.$0);
              _bind$222 = $StringView$$rev_find(rest$220, _tmp$1764);
              if (_bind$222 == 4294967296ll) {
                moonbit_decref(rest$220.$0);
                moonbit_decref(_end_column$219.$0);
                moonbit_decref(_end_line$218.$0);
                moonbit_decref(pkg$210.$0);
                goto $join$221;
              } else {
                int64_t _Some$223 = _bind$222;
                int32_t _start_line_end$224 = (int32_t)_Some$223;
                int64_t _tmp$1763 = (int64_t)_start_line_end$224;
                struct $StringView _tmp$1760;
                int32_t _tmp$1762;
                struct $StringView _tmp$1761;
                int64_t _bind$225;
                moonbit_incref(rest$220.$0);
                _tmp$1760 = $StringView$$view$inner(rest$220, 0, _tmp$1763);
                _tmp$1762
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1761
                = (struct $StringView){
                  0,
                    _tmp$1762,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$225 = $StringView$$rev_find(_tmp$1760, _tmp$1761);
                if (_bind$225 == 4294967296ll) {
                  moonbit_decref(rest$220.$0);
                  moonbit_decref(_end_column$219.$0);
                  moonbit_decref(_end_line$218.$0);
                  moonbit_decref(pkg$210.$0);
                  goto $join$221;
                } else {
                  int64_t _Some$226 = _bind$225;
                  int32_t _filename_end$227 = (int32_t)_Some$226;
                  int32_t _tmp$1754 = _filename_end$227 + 1;
                  int32_t _tmp$1755;
                  moonbit_incref(rest$220.$0);
                  _tmp$1755 = $StringView$$length(rest$220);
                  if (_tmp$1754 < _tmp$1755) {
                    int32_t _tmp$1759 = _filename_end$227 + 1;
                    struct $StringView start_loc$228;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$229;
                    moonbit_incref(rest$220.$0);
                    start_loc$228
                    = $StringView$$view$inner(
                      rest$220, _tmp$1759, 4294967296ll
                    );
                    _bind$229
                    = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
                      start_loc$228
                    );
                    if (_bind$229 == 0) {
                      if (_bind$229) {
                        moonbit_decref(_bind$229);
                      }
                      moonbit_decref(rest$220.$0);
                      moonbit_decref(_end_column$219.$0);
                      moonbit_decref(_end_line$218.$0);
                      moonbit_decref(pkg$210.$0);
                      moonbit_panic();
                    } else {
                      struct $$3c$StringView$2a$StringView$3e$* _Some$230 =
                        _bind$229;
                      struct $$3c$StringView$2a$StringView$3e$* _x$231 =
                        _Some$230;
                      struct $StringView _field$3730 =
                        (struct $StringView){
                          _x$231->$0_1, _x$231->$0_2, _x$231->$0_0
                        };
                      struct $StringView _start_line$232 = _field$3730;
                      struct $StringView _field$3729 =
                        (struct $StringView){
                          _x$231->$1_1, _x$231->$1_2, _x$231->$1_0
                        };
                      int32_t _cnt$3885 = Moonbit_object_header(_x$231)->rc;
                      struct $StringView _start_column$233;
                      int32_t _tmp$1756;
                      if (_cnt$3885 > 1) {
                        int32_t _new_cnt$3886;
                        moonbit_incref(_field$3729.$0);
                        moonbit_incref(_start_line$232.$0);
                        _new_cnt$3886 = _cnt$3885 - 1;
                        Moonbit_object_header(_x$231)->rc = _new_cnt$3886;
                      } else if (_cnt$3885 == 1) {
                        moonbit_free(_x$231);
                      }
                      _start_column$233 = _field$3729;
                      _tmp$1756 = _pkg_end$209 + 1;
                      if (_filename_end$227 > _tmp$1756) {
                        int32_t _tmp$1757 = _pkg_end$209 + 1;
                        int64_t _tmp$1758 = (int64_t)_filename_end$227;
                        struct $StringView filename$234 =
                          $StringView$$view$inner(
                            rest$220, _tmp$1757, _tmp$1758
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$4165 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$4165)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$4165->$0_0 = pkg$210.$0;
                        _block$4165->$0_1 = pkg$210.$1;
                        _block$4165->$0_2 = pkg$210.$2;
                        _block$4165->$1_0 = filename$234.$0;
                        _block$4165->$1_1 = filename$234.$1;
                        _block$4165->$1_2 = filename$234.$2;
                        _block$4165->$2_0 = _start_line$232.$0;
                        _block$4165->$2_1 = _start_line$232.$1;
                        _block$4165->$2_2 = _start_line$232.$2;
                        _block$4165->$3_0 = _start_column$233.$0;
                        _block$4165->$3_1 = _start_column$233.$1;
                        _block$4165->$3_2 = _start_column$233.$2;
                        _block$4165->$4_0 = _end_line$218.$0;
                        _block$4165->$4_1 = _end_line$218.$1;
                        _block$4165->$4_2 = _end_line$218.$2;
                        _block$4165->$5_0 = _end_column$219.$0;
                        _block$4165->$5_1 = _end_column$219.$1;
                        _block$4165->$5_2 = _end_column$219.$2;
                        return _block$4165;
                      } else {
                        moonbit_decref(_start_column$233.$0);
                        moonbit_decref(_start_line$232.$0);
                        moonbit_decref(rest$220.$0);
                        moonbit_decref(_end_column$219.$0);
                        moonbit_decref(_end_line$218.$0);
                        moonbit_decref(pkg$210.$0);
                        moonbit_panic();
                      }
                    }
                  } else {
                    moonbit_decref(rest$220.$0);
                    moonbit_decref(_end_column$219.$0);
                    moonbit_decref(_end_line$218.$0);
                    moonbit_decref(pkg$210.$0);
                    moonbit_panic();
                  }
                }
              }
              $join$221:;
              moonbit_panic();
            }
          } else {
            moonbit_decref(pkg$210.$0);
            moonbit_decref(_x$205.$0);
            moonbit_panic();
          }
        }
      }
    } else {
      moonbit_decref(repr$203);
      goto $join$202;
    }
  } else {
    moonbit_decref(repr$203);
    goto $join$202;
  }
  $join$202:;
  moonbit_panic();
}

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$199
) {
  int32_t _tmp$1749 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1748;
  int64_t _bind$198;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1748
  = (struct $StringView){
    0, _tmp$1749, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$199.$0);
  _bind$198 = $StringView$$find(view$199, _tmp$1748);
  if (_bind$198 == 4294967296ll) {
    moonbit_decref(view$199.$0);
    return 0;
  } else {
    int64_t _Some$200 = _bind$198;
    int32_t _i$201 = (int32_t)_Some$200;
    int32_t _if_result$4166;
    if (_i$201 > 0) {
      int32_t _tmp$1741 = _i$201 + 1;
      int32_t _tmp$1742;
      moonbit_incref(view$199.$0);
      _tmp$1742 = $StringView$$length(view$199);
      _if_result$4166 = _tmp$1741 < _tmp$1742;
    } else {
      _if_result$4166 = 0;
    }
    if (_if_result$4166) {
      int64_t _tmp$1747 = (int64_t)_i$201;
      struct $StringView _tmp$1744;
      int32_t _tmp$1746;
      struct $StringView _tmp$1745;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1743;
      moonbit_incref(view$199.$0);
      _tmp$1744 = $StringView$$view$inner(view$199, 0, _tmp$1747);
      _tmp$1746 = _i$201 + 1;
      _tmp$1745 = $StringView$$view$inner(view$199, _tmp$1746, 4294967296ll);
      _tuple$1743
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1743)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1743->$0_0 = _tmp$1744.$0;
      _tuple$1743->$0_1 = _tmp$1744.$1;
      _tuple$1743->$0_2 = _tmp$1744.$2;
      _tuple$1743->$1_0 = _tmp$1745.$0;
      _tuple$1743->$1_1 = _tmp$1745.$1;
      _tuple$1743->$1_2 = _tmp$1745.$2;
      return _tuple$1743;
    } else {
      moonbit_decref(view$199.$0);
      return 0;
    }
  }
}

struct $StringView $StringView$$view$inner(
  struct $StringView self$196,
  int32_t start_offset$197,
  int64_t end_offset$194
) {
  int32_t end_offset$193;
  int32_t _if_result$4167;
  if (end_offset$194 == 4294967296ll) {
    moonbit_incref(self$196.$0);
    end_offset$193 = $StringView$$length(self$196);
  } else {
    int64_t _Some$195 = end_offset$194;
    end_offset$193 = (int32_t)_Some$195;
  }
  if (start_offset$197 >= 0) {
    if (start_offset$197 <= end_offset$193) {
      int32_t _tmp$1735;
      moonbit_incref(self$196.$0);
      _tmp$1735 = $StringView$$length(self$196);
      _if_result$4167 = end_offset$193 <= _tmp$1735;
    } else {
      _if_result$4167 = 0;
    }
  } else {
    _if_result$4167 = 0;
  }
  if (_if_result$4167) {
    moonbit_string_t _field$3734 = self$196.$0;
    moonbit_string_t str$1736 = _field$3734;
    int32_t start$1740 = self$196.$1;
    int32_t _tmp$1737 = start$1740 + start_offset$197;
    int32_t _field$3733 = self$196.$1;
    int32_t start$1739 = _field$3733;
    int32_t _tmp$1738 = start$1739 + end_offset$193;
    return (struct $StringView){_tmp$1737, _tmp$1738, str$1736};
  } else {
    moonbit_decref(self$196.$0);
    return $moonbitlang$core$builtin$abort$2(
             (moonbit_string_t)moonbit_string_literal_131.data,
               (moonbit_string_t)moonbit_string_literal_132.data
           );
  }
}

int64_t $StringView$$rev_find(
  struct $StringView self$192,
  struct $StringView str$191
) {
  int32_t _tmp$1734;
  moonbit_incref(str$191.$0);
  _tmp$1734 = $StringView$$length(str$191);
  if (_tmp$1734 <= 4) {
    return $moonbitlang$core$builtin$brute_force_rev_find(self$192, str$191);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
             self$192, str$191
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$182,
  struct $StringView needle$184
) {
  int32_t haystack_len$181;
  int32_t needle_len$183;
  moonbit_incref(haystack$182.$0);
  haystack_len$181 = $StringView$$length(haystack$182);
  moonbit_incref(needle$184.$0);
  needle_len$183 = $StringView$$length(needle$184);
  if (needle_len$183 > 0) {
    if (haystack_len$181 >= needle_len$183) {
      int32_t needle_first$185;
      int32_t i$186;
      moonbit_incref(needle$184.$0);
      needle_first$185 = $StringView$$unsafe_charcode_at(needle$184, 0);
      i$186 = haystack_len$181 - needle_len$183;
      while (1) {
        int32_t _tmp$1721 = i$186;
        if (_tmp$1721 >= 0) {
          int32_t _tmp$1726;
          while (1) {
            int32_t _tmp$1724 = i$186;
            int32_t _if_result$4170;
            if (_tmp$1724 >= 0) {
              int32_t _tmp$1723 = i$186;
              int32_t _tmp$1722;
              moonbit_incref(haystack$182.$0);
              _tmp$1722
              = $StringView$$unsafe_charcode_at(
                haystack$182, _tmp$1723
              );
              _if_result$4170 = _tmp$1722 != needle_first$185;
            } else {
              _if_result$4170 = 0;
            }
            if (_if_result$4170) {
              int32_t _tmp$1725 = i$186;
              i$186 = _tmp$1725 - 1;
              continue;
            }
            break;
          }
          _tmp$1726 = i$186;
          if (_tmp$1726 >= 0) {
            int32_t j$188 = 1;
            int32_t _tmp$1733;
            while (1) {
              if (j$188 < needle_len$183) {
                int32_t _tmp$1730 = i$186;
                int32_t _tmp$1729 = _tmp$1730 + j$188;
                int32_t _tmp$1727;
                int32_t _tmp$1728;
                int32_t _tmp$1731;
                moonbit_incref(haystack$182.$0);
                _tmp$1727
                = $StringView$$unsafe_charcode_at(
                  haystack$182, _tmp$1729
                );
                moonbit_incref(needle$184.$0);
                _tmp$1728
                = $StringView$$unsafe_charcode_at(
                  needle$184, j$188
                );
                if (_tmp$1727 != _tmp$1728) {
                  break;
                }
                _tmp$1731 = j$188 + 1;
                j$188 = _tmp$1731;
                continue;
              } else {
                int32_t _tmp$1732;
                moonbit_decref(needle$184.$0);
                moonbit_decref(haystack$182.$0);
                _tmp$1732 = i$186;
                return (int64_t)_tmp$1732;
              }
              break;
            }
            _tmp$1733 = i$186;
            i$186 = _tmp$1733 - 1;
          }
          continue;
        } else {
          moonbit_decref(needle$184.$0);
          moonbit_decref(haystack$182.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$184.$0);
      moonbit_decref(haystack$182.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$184.$0);
    moonbit_decref(haystack$182.$0);
    return (int64_t)haystack_len$181;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$171,
  struct $StringView needle$173
) {
  int32_t haystack_len$170;
  int32_t needle_len$172;
  moonbit_incref(haystack$171.$0);
  haystack_len$170 = $StringView$$length(haystack$171);
  moonbit_incref(needle$173.$0);
  needle_len$172 = $StringView$$length(needle$173);
  if (needle_len$172 > 0) {
    if (haystack_len$170 >= needle_len$172) {
      int32_t* skip_table$174 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$172);
      int32_t _tmp$1711 = needle_len$172 - 1;
      int32_t i$175 = _tmp$1711;
      int32_t _tmp$1720;
      int32_t i$177;
      while (1) {
        if (i$175 > 0) {
          int32_t _tmp$1709;
          int32_t _tmp$1708;
          int32_t _tmp$1710;
          moonbit_incref(needle$173.$0);
          _tmp$1709 = $StringView$$unsafe_charcode_at(needle$173, i$175);
          _tmp$1708 = _tmp$1709 & 255;
          if (
            _tmp$1708 < 0
            || _tmp$1708 >= Moonbit_array_length(skip_table$174)
          ) {
            moonbit_panic();
          }
          skip_table$174[_tmp$1708] = i$175;
          _tmp$1710 = i$175 - 1;
          i$175 = _tmp$1710;
          continue;
        }
        break;
      }
      _tmp$1720 = haystack_len$170 - needle_len$172;
      i$177 = _tmp$1720;
      while (1) {
        if (i$177 >= 0) {
          int32_t j$178 = 0;
          int32_t _tmp$1719;
          int32_t _tmp$1718;
          int32_t _tmp$1717;
          int32_t _tmp$1716;
          while (1) {
            if (j$178 < needle_len$172) {
              int32_t _tmp$1714 = i$177 + j$178;
              int32_t _tmp$1712;
              int32_t _tmp$1713;
              int32_t _tmp$1715;
              moonbit_incref(haystack$171.$0);
              _tmp$1712
              = $StringView$$unsafe_charcode_at(
                haystack$171, _tmp$1714
              );
              moonbit_incref(needle$173.$0);
              _tmp$1713 = $StringView$$unsafe_charcode_at(needle$173, j$178);
              if (_tmp$1712 != _tmp$1713) {
                break;
              }
              _tmp$1715 = j$178 + 1;
              j$178 = _tmp$1715;
              continue;
            } else {
              moonbit_decref(skip_table$174);
              moonbit_decref(needle$173.$0);
              moonbit_decref(haystack$171.$0);
              return (int64_t)i$177;
            }
            break;
          }
          moonbit_incref(haystack$171.$0);
          _tmp$1719 = $StringView$$unsafe_charcode_at(haystack$171, i$177);
          _tmp$1718 = _tmp$1719 & 255;
          if (
            _tmp$1718 < 0
            || _tmp$1718 >= Moonbit_array_length(skip_table$174)
          ) {
            moonbit_panic();
          }
          _tmp$1717 = (int32_t)skip_table$174[_tmp$1718];
          _tmp$1716 = i$177 - _tmp$1717;
          i$177 = _tmp$1716;
          continue;
        } else {
          moonbit_decref(skip_table$174);
          moonbit_decref(needle$173.$0);
          moonbit_decref(haystack$171.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$173.$0);
      moonbit_decref(haystack$171.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$173.$0);
    moonbit_decref(haystack$171.$0);
    return (int64_t)haystack_len$170;
  }
}

int64_t $StringView$$find(
  struct $StringView self$169,
  struct $StringView str$168
) {
  int32_t _tmp$1707;
  moonbit_incref(str$168.$0);
  _tmp$1707 = $StringView$$length(str$168);
  if (_tmp$1707 <= 4) {
    return $moonbitlang$core$builtin$brute_force_find(self$169, str$168);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_find(
             self$169, str$168
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$158,
  struct $StringView needle$160
) {
  int32_t haystack_len$157;
  int32_t needle_len$159;
  moonbit_incref(haystack$158.$0);
  haystack_len$157 = $StringView$$length(haystack$158);
  moonbit_incref(needle$160.$0);
  needle_len$159 = $StringView$$length(needle$160);
  if (needle_len$159 > 0) {
    if (haystack_len$157 >= needle_len$159) {
      int32_t needle_first$161;
      int32_t forward_len$162;
      int32_t i$163;
      moonbit_incref(needle$160.$0);
      needle_first$161 = $StringView$$unsafe_charcode_at(needle$160, 0);
      forward_len$162 = haystack_len$157 - needle_len$159;
      i$163 = 0;
      while (1) {
        int32_t _tmp$1694 = i$163;
        if (_tmp$1694 <= forward_len$162) {
          int32_t _tmp$1699;
          while (1) {
            int32_t _tmp$1697 = i$163;
            int32_t _if_result$4177;
            if (_tmp$1697 <= forward_len$162) {
              int32_t _tmp$1696 = i$163;
              int32_t _tmp$1695;
              moonbit_incref(haystack$158.$0);
              _tmp$1695
              = $StringView$$unsafe_charcode_at(
                haystack$158, _tmp$1696
              );
              _if_result$4177 = _tmp$1695 != needle_first$161;
            } else {
              _if_result$4177 = 0;
            }
            if (_if_result$4177) {
              int32_t _tmp$1698 = i$163;
              i$163 = _tmp$1698 + 1;
              continue;
            }
            break;
          }
          _tmp$1699 = i$163;
          if (_tmp$1699 <= forward_len$162) {
            int32_t j$165 = 1;
            int32_t _tmp$1706;
            while (1) {
              if (j$165 < needle_len$159) {
                int32_t _tmp$1703 = i$163;
                int32_t _tmp$1702 = _tmp$1703 + j$165;
                int32_t _tmp$1700;
                int32_t _tmp$1701;
                int32_t _tmp$1704;
                moonbit_incref(haystack$158.$0);
                _tmp$1700
                = $StringView$$unsafe_charcode_at(
                  haystack$158, _tmp$1702
                );
                moonbit_incref(needle$160.$0);
                _tmp$1701
                = $StringView$$unsafe_charcode_at(
                  needle$160, j$165
                );
                if (_tmp$1700 != _tmp$1701) {
                  break;
                }
                _tmp$1704 = j$165 + 1;
                j$165 = _tmp$1704;
                continue;
              } else {
                int32_t _tmp$1705;
                moonbit_decref(needle$160.$0);
                moonbit_decref(haystack$158.$0);
                _tmp$1705 = i$163;
                return (int64_t)_tmp$1705;
              }
              break;
            }
            _tmp$1706 = i$163;
            i$163 = _tmp$1706 + 1;
          }
          continue;
        } else {
          moonbit_decref(needle$160.$0);
          moonbit_decref(haystack$158.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$160.$0);
      moonbit_decref(haystack$158.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$160.$0);
    moonbit_decref(haystack$158.$0);
    return $moonbitlang$core$builtin$brute_force_find$constr$156;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$144,
  struct $StringView needle$146
) {
  int32_t haystack_len$143;
  int32_t needle_len$145;
  moonbit_incref(haystack$144.$0);
  haystack_len$143 = $StringView$$length(haystack$144);
  moonbit_incref(needle$146.$0);
  needle_len$145 = $StringView$$length(needle$146);
  if (needle_len$145 > 0) {
    if (haystack_len$143 >= needle_len$145) {
      int32_t* skip_table$147 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$145);
      int32_t _end4301$148 = needle_len$145 - 1;
      int32_t i$149 = 0;
      int32_t i$151;
      while (1) {
        if (i$149 < _end4301$148) {
          int32_t _tmp$1681;
          int32_t _tmp$1678;
          int32_t _tmp$1680;
          int32_t _tmp$1679;
          int32_t _tmp$1682;
          moonbit_incref(needle$146.$0);
          _tmp$1681 = $StringView$$unsafe_charcode_at(needle$146, i$149);
          _tmp$1678 = _tmp$1681 & 255;
          _tmp$1680 = needle_len$145 - 1;
          _tmp$1679 = _tmp$1680 - i$149;
          if (
            _tmp$1678 < 0
            || _tmp$1678 >= Moonbit_array_length(skip_table$147)
          ) {
            moonbit_panic();
          }
          skip_table$147[_tmp$1678] = _tmp$1679;
          _tmp$1682 = i$149 + 1;
          i$149 = _tmp$1682;
          continue;
        }
        break;
      }
      i$151 = 0;
      while (1) {
        int32_t _tmp$1683 = haystack_len$143 - needle_len$145;
        if (i$151 <= _tmp$1683) {
          int32_t _end4307$152 = needle_len$145 - 1;
          int32_t j$153 = 0;
          int32_t _tmp$1693;
          int32_t _tmp$1692;
          int32_t _tmp$1691;
          int32_t _tmp$1690;
          int32_t _tmp$1689;
          int32_t _tmp$1688;
          while (1) {
            if (j$153 <= _end4307$152) {
              int32_t _tmp$1686 = i$151 + j$153;
              int32_t _tmp$1684;
              int32_t _tmp$1685;
              int32_t _tmp$1687;
              moonbit_incref(haystack$144.$0);
              _tmp$1684
              = $StringView$$unsafe_charcode_at(
                haystack$144, _tmp$1686
              );
              moonbit_incref(needle$146.$0);
              _tmp$1685 = $StringView$$unsafe_charcode_at(needle$146, j$153);
              if (_tmp$1684 != _tmp$1685) {
                break;
              }
              _tmp$1687 = j$153 + 1;
              j$153 = _tmp$1687;
              continue;
            } else {
              moonbit_decref(skip_table$147);
              moonbit_decref(needle$146.$0);
              moonbit_decref(haystack$144.$0);
              return (int64_t)i$151;
            }
            break;
          }
          _tmp$1693 = i$151 + needle_len$145;
          _tmp$1692 = _tmp$1693 - 1;
          moonbit_incref(haystack$144.$0);
          _tmp$1691
          = $StringView$$unsafe_charcode_at(
            haystack$144, _tmp$1692
          );
          _tmp$1690 = _tmp$1691 & 255;
          if (
            _tmp$1690 < 0
            || _tmp$1690 >= Moonbit_array_length(skip_table$147)
          ) {
            moonbit_panic();
          }
          _tmp$1689 = (int32_t)skip_table$147[_tmp$1690];
          _tmp$1688 = i$151 + _tmp$1689;
          i$151 = _tmp$1688;
          continue;
        } else {
          moonbit_decref(skip_table$147);
          moonbit_decref(needle$146.$0);
          moonbit_decref(haystack$144.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$146.$0);
      moonbit_decref(haystack$144.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$146.$0);
    moonbit_decref(haystack$144.$0);
    return $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$142;
  }
}

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$140,
  int32_t index$141
) {
  moonbit_string_t _field$3737 = self$140.$0;
  moonbit_string_t str$1675 = _field$3737;
  int32_t _field$3736 = self$140.$1;
  int32_t start$1677 = _field$3736;
  int32_t _tmp$1676 = start$1677 + index$141;
  int32_t _tmp$3735 = str$1675[_tmp$1676];
  moonbit_decref(str$1675);
  return _tmp$3735;
}

int32_t $StringView$$length(struct $StringView self$139) {
  int32_t end$1673 = self$139.$2;
  int32_t _field$3738 = self$139.$1;
  int32_t start$1674;
  moonbit_decref(self$139.$0);
  start$1674 = _field$3738;
  return end$1673 - start$1674;
}

int32_t $$moonbitlang$core$builtin$Array$$at$1(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$137,
  int32_t index$138
) {
  int32_t len$136 = self$137->$1;
  int32_t _if_result$4182;
  if (index$138 >= 0) {
    _if_result$4182 = index$138 < len$136;
  } else {
    _if_result$4182 = 0;
  }
  if (_if_result$4182) {
    int32_t* _tmp$1672 = $$moonbitlang$core$builtin$Array$$buffer$3(self$137);
    int32_t _tmp$3739;
    if (index$138 < 0 || index$138 >= Moonbit_array_length(_tmp$1672)) {
      moonbit_panic();
    }
    _tmp$3739 = (int32_t)_tmp$1672[index$138];
    moonbit_decref(_tmp$1672);
    return _tmp$3739;
  } else {
    moonbit_decref(self$137);
    moonbit_panic();
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$134,
  int32_t index$135
) {
  int32_t len$133 = self$134->$1;
  int32_t _if_result$4183;
  if (index$135 >= 0) {
    _if_result$4183 = index$135 < len$133;
  } else {
    _if_result$4183 = 0;
  }
  if (_if_result$4183) {
    moonbit_string_t* _tmp$1671 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$134);
    moonbit_string_t _tmp$3740;
    if (index$135 < 0 || index$135 >= Moonbit_array_length(_tmp$1671)) {
      moonbit_panic();
    }
    _tmp$3740 = (moonbit_string_t)_tmp$1671[index$135];
    moonbit_incref(_tmp$3740);
    moonbit_decref(_tmp$1671);
    return _tmp$3740;
  } else {
    moonbit_decref(self$134);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$132
) {
  int32_t _field$3741 = self$132->$1;
  moonbit_decref(self$132);
  return _field$3741;
}

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$* self$131
) {
  int32_t _field$3742 = self$131->$1;
  moonbit_decref(self$131);
  return _field$3742;
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$130
) {
  int32_t _field$3743 = self$130->$1;
  moonbit_decref(self$130);
  return _field$3743;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$129
) {
  int32_t _field$3744 = self$129->$1;
  moonbit_decref(self$129);
  return _field$3744;
}

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$128
) {
  int32_t* _field$3745 = self$128->$0;
  int32_t _cnt$3887 = Moonbit_object_header(self$128)->rc;
  if (_cnt$3887 > 1) {
    int32_t _new_cnt$3888;
    moonbit_incref(_field$3745);
    _new_cnt$3888 = _cnt$3887 - 1;
    Moonbit_object_header(self$128)->rc = _new_cnt$3888;
  } else if (_cnt$3887 == 1) {
    moonbit_free(self$128);
  }
  return _field$3745;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$127
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$3746 =
    self$127->$0;
  int32_t _cnt$3889 = Moonbit_object_header(self$127)->rc;
  if (_cnt$3889 > 1) {
    int32_t _new_cnt$3890;
    moonbit_incref(_field$3746);
    _new_cnt$3890 = _cnt$3889 - 1;
    Moonbit_object_header(self$127)->rc = _new_cnt$3890;
  } else if (_cnt$3889 == 1) {
    moonbit_free(self$127);
  }
  return _field$3746;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$126
) {
  moonbit_string_t* _field$3747 = self$126->$0;
  int32_t _cnt$3891 = Moonbit_object_header(self$126)->rc;
  if (_cnt$3891 > 1) {
    int32_t _new_cnt$3892;
    moonbit_incref(_field$3747);
    _new_cnt$3892 = _cnt$3891 - 1;
    Moonbit_object_header(self$126)->rc = _new_cnt$3892;
  } else if (_cnt$3891 == 1) {
    moonbit_free(self$126);
  }
  return _field$3747;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$125
) {
  struct $$3c$String$2a$Int$3e$** _field$3748 = self$125->$0;
  int32_t _cnt$3893 = Moonbit_object_header(self$125)->rc;
  if (_cnt$3893 > 1) {
    int32_t _new_cnt$3894;
    moonbit_incref(_field$3748);
    _new_cnt$3894 = _cnt$3893 - 1;
    Moonbit_object_header(self$125)->rc = _new_cnt$3894;
  } else if (_cnt$3893 == 1) {
    moonbit_free(self$125);
  }
  return _field$3748;
}

moonbit_string_t $String$$escape(moonbit_string_t self$124) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$123 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1670;
  moonbit_incref(buf$123);
  _tmp$1670
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$123
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$124, _tmp$1670);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$123);
}

int32_t $moonbitlang$core$builtin$op_notequal$5(int32_t x$121, int32_t y$122) {
  int32_t _tmp$1669 =
    $$moonbitlang$core$builtin$Eq$$Unit$$equal(x$121, y$122);
  return !_tmp$1669;
}

int32_t $moonbitlang$core$builtin$op_notequal$4(
  moonbit_string_t x$119,
  moonbit_string_t y$120
) {
  int32_t _tmp$1668 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$2(x$119, y$120);
  return !_tmp$1668;
}

int32_t $moonbitlang$core$builtin$op_notequal$3(int32_t x$117, int32_t y$118) {
  int32_t _tmp$1667 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$1(x$117, y$118);
  return !_tmp$1667;
}

int32_t $moonbitlang$core$builtin$op_notequal$2(int64_t x$115, int64_t y$116) {
  int32_t _tmp$1666 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$0(x$115, y$116);
  return !_tmp$1666;
}

int32_t $moonbitlang$core$builtin$op_notequal$1(
  moonbit_string_t x$113,
  moonbit_string_t y$114
) {
  int32_t _tmp$3749 = moonbit_val_array_equal(x$113, y$114);
  int32_t _tmp$1665;
  moonbit_decref(x$113);
  moonbit_decref(y$114);
  _tmp$1665 = _tmp$3749;
  return !_tmp$1665;
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$111, int32_t y$112) {
  int32_t _tmp$1664 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$111, y$112
    );
  return !_tmp$1664;
}

int32_t $Int$$is_trailing_surrogate(int32_t self$110) {
  if (56320 <= self$110) {
    return self$110 <= 57343;
  } else {
    return 0;
  }
}

int32_t $Int$$is_leading_surrogate(int32_t self$109) {
  if (55296 <= self$109) {
    return self$109 <= 56319;
  } else {
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$106,
  int32_t ch$108
) {
  int32_t len$1659 = self$106->$1;
  int32_t _tmp$1658 = len$1659 + 4;
  moonbit_bytes_t _field$3750;
  moonbit_bytes_t data$1662;
  int32_t len$1663;
  int32_t inc$107;
  int32_t len$1661;
  int32_t _tmp$1660;
  moonbit_incref(self$106);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$106, _tmp$1658
  );
  _field$3750 = self$106->$0;
  data$1662 = _field$3750;
  len$1663 = self$106->$1;
  moonbit_incref(data$1662);
  inc$107 = $FixedArray$$set_utf16le_char(data$1662, len$1663, ch$108);
  len$1661 = self$106->$1;
  _tmp$1660 = len$1661 + inc$107;
  self$106->$1 = _tmp$1660;
  moonbit_decref(self$106);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$101,
  int32_t required$102
) {
  moonbit_bytes_t _field$3754 = self$101->$0;
  moonbit_bytes_t data$1657 = _field$3754;
  int32_t _tmp$3753 = Moonbit_array_length(data$1657);
  int32_t current_len$100 = _tmp$3753;
  int32_t enough_space$103;
  int32_t _tmp$1655;
  int32_t _tmp$1656;
  moonbit_bytes_t new_data$105;
  moonbit_bytes_t _field$3752;
  moonbit_bytes_t data$1653;
  int32_t len$1654;
  moonbit_bytes_t _old$3751;
  if (required$102 <= current_len$100) {
    moonbit_decref(self$101);
    return 0;
  }
  enough_space$103 = current_len$100;
  while (1) {
    int32_t _tmp$1651 = enough_space$103;
    if (_tmp$1651 < required$102) {
      int32_t _tmp$1652 = enough_space$103;
      enough_space$103 = _tmp$1652 * 2;
      continue;
    }
    break;
  }
  _tmp$1655 = enough_space$103;
  _tmp$1656 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$105 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1655, _tmp$1656);
  _field$3752 = self$101->$0;
  data$1653 = _field$3752;
  len$1654 = self$101->$1;
  moonbit_incref(data$1653);
  moonbit_incref(new_data$105);
  $FixedArray$$unsafe_blit$0(new_data$105, 0, data$1653, 0, len$1654);
  _old$3751 = self$101->$0;
  moonbit_decref(_old$3751);
  self$101->$0 = new_data$105;
  moonbit_decref(self$101);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default() {
  return 0;
}

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$95,
  int32_t offset$96,
  int32_t value$94
) {
  uint32_t code$93 = $Char$$to_uint(value$94);
  if (code$93 < 65536u) {
    uint32_t _tmp$1634 = code$93 & 255u;
    int32_t _tmp$1633 = $UInt$$to_byte(_tmp$1634);
    int32_t _tmp$1635;
    uint32_t _tmp$1637;
    int32_t _tmp$1636;
    if (offset$96 < 0 || offset$96 >= Moonbit_array_length(self$95)) {
      moonbit_panic();
    }
    self$95[offset$96] = _tmp$1633;
    _tmp$1635 = offset$96 + 1;
    _tmp$1637 = code$93 >> 8;
    _tmp$1636 = $UInt$$to_byte(_tmp$1637);
    if (_tmp$1635 < 0 || _tmp$1635 >= Moonbit_array_length(self$95)) {
      moonbit_panic();
    }
    self$95[_tmp$1635] = _tmp$1636;
    moonbit_decref(self$95);
    return 2;
  } else if (code$93 < 1114112u) {
    uint32_t hi$97 = code$93 - 65536u;
    uint32_t _tmp$1650 = hi$97 >> 10;
    uint32_t lo$98 = _tmp$1650 | 55296u;
    uint32_t _tmp$1649 = hi$97 & 1023u;
    uint32_t hi$99 = _tmp$1649 | 56320u;
    uint32_t _tmp$1639 = lo$98 & 255u;
    int32_t _tmp$1638 = $UInt$$to_byte(_tmp$1639);
    int32_t _tmp$1640;
    uint32_t _tmp$1642;
    int32_t _tmp$1641;
    int32_t _tmp$1643;
    uint32_t _tmp$1645;
    int32_t _tmp$1644;
    int32_t _tmp$1646;
    uint32_t _tmp$1648;
    int32_t _tmp$1647;
    if (offset$96 < 0 || offset$96 >= Moonbit_array_length(self$95)) {
      moonbit_panic();
    }
    self$95[offset$96] = _tmp$1638;
    _tmp$1640 = offset$96 + 1;
    _tmp$1642 = lo$98 >> 8;
    _tmp$1641 = $UInt$$to_byte(_tmp$1642);
    if (_tmp$1640 < 0 || _tmp$1640 >= Moonbit_array_length(self$95)) {
      moonbit_panic();
    }
    self$95[_tmp$1640] = _tmp$1641;
    _tmp$1643 = offset$96 + 2;
    _tmp$1645 = hi$99 & 255u;
    _tmp$1644 = $UInt$$to_byte(_tmp$1645);
    if (_tmp$1643 < 0 || _tmp$1643 >= Moonbit_array_length(self$95)) {
      moonbit_panic();
    }
    self$95[_tmp$1643] = _tmp$1644;
    _tmp$1646 = offset$96 + 3;
    _tmp$1648 = hi$99 >> 8;
    _tmp$1647 = $UInt$$to_byte(_tmp$1648);
    if (_tmp$1646 < 0 || _tmp$1646 >= Moonbit_array_length(self$95)) {
      moonbit_panic();
    }
    self$95[_tmp$1646] = _tmp$1647;
    moonbit_decref(self$95);
    return 4;
  } else {
    moonbit_decref(self$95);
    return $moonbitlang$core$builtin$abort$1(
             (moonbit_string_t)moonbit_string_literal_133.data,
               (moonbit_string_t)moonbit_string_literal_134.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$92) {
  int32_t _tmp$1632 = *(int32_t*)&self$92;
  return _tmp$1632 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$91) {
  int32_t _tmp$1631 = self$91;
  return *(uint32_t*)&_tmp$1631;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$90
) {
  moonbit_bytes_t _field$3756 = self$90->$0;
  moonbit_bytes_t data$1630 = _field$3756;
  moonbit_bytes_t _tmp$1627;
  int32_t _field$3755;
  int32_t len$1629;
  int64_t _tmp$1628;
  moonbit_incref(data$1630);
  _tmp$1627 = data$1630;
  _field$3755 = self$90->$1;
  moonbit_decref(self$90);
  len$1629 = _field$3755;
  _tmp$1628 = (int64_t)len$1629;
  return $Bytes$$to_unchecked_string$inner(_tmp$1627, 0, _tmp$1628);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$85,
  int32_t offset$89,
  int64_t length$87
) {
  int32_t len$84 = Moonbit_array_length(self$85);
  int32_t length$86;
  int32_t _if_result$4185;
  if (length$87 == 4294967296ll) {
    length$86 = len$84 - offset$89;
  } else {
    int64_t _Some$88 = length$87;
    length$86 = (int32_t)_Some$88;
  }
  if (offset$89 >= 0) {
    if (length$86 >= 0) {
      int32_t _tmp$1626 = offset$89 + length$86;
      _if_result$4185 = _tmp$1626 <= len$84;
    } else {
      _if_result$4185 = 0;
    }
  } else {
    _if_result$4185 = 0;
  }
  if (_if_result$4185) {
    return $moonbitlang$core$builtin$unsafe_sub_string(
             self$85, offset$89, length$86
           );
  } else {
    moonbit_decref(self$85);
    moonbit_panic();
  }
}

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$82
) {
  int32_t initial$81;
  moonbit_bytes_t data$83;
  struct $$moonbitlang$core$builtin$StringBuilder* _block$4186;
  if (size_hint$82 < 1) {
    initial$81 = 1;
  } else {
    initial$81 = size_hint$82;
  }
  data$83 = (moonbit_bytes_t)moonbit_make_bytes(initial$81, 0);
  _block$4186
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$4186)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$4186->$0 = data$83;
  _block$4186->$1 = 0;
  return _block$4186;
}

int32_t $Byte$$to_char(int32_t self$80) {
  int32_t _tmp$1625 = (int32_t)self$80;
  return _tmp$1625;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$75,
  int32_t dst_offset$76,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$77,
  int32_t src_offset$78,
  int32_t len$79
) {
  $FixedArray$$unsafe_blit$3(
    dst$75, dst_offset$76, src$77, src_offset$78, len$79
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$70,
  int32_t dst_offset$71,
  struct $$3c$String$2a$Int$3e$** src$72,
  int32_t src_offset$73,
  int32_t len$74
) {
  $FixedArray$$unsafe_blit$2(
    dst$70, dst_offset$71, src$72, src_offset$73, len$74
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$65,
  int32_t dst_offset$66,
  moonbit_string_t* src$67,
  int32_t src_offset$68,
  int32_t len$69
) {
  $FixedArray$$unsafe_blit$1(
    dst$65, dst_offset$66, src$67, src_offset$68, len$69
  );
  return 0;
}

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$56,
  int32_t dst_offset$58,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$57,
  int32_t src_offset$59,
  int32_t len$61
) {
  int32_t _if_result$4187;
  if (dst$56 == src$57) {
    _if_result$4187 = dst_offset$58 < src_offset$59;
  } else {
    _if_result$4187 = 0;
  }
  if (_if_result$4187) {
    int32_t i$60 = 0;
    while (1) {
      if (i$60 < len$61) {
        int32_t _tmp$1616 = dst_offset$58 + i$60;
        int32_t _tmp$1618 = src_offset$59 + i$60;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3758;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1617;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$3757;
        int32_t _tmp$1619;
        if (_tmp$1618 < 0 || _tmp$1618 >= Moonbit_array_length(src$57)) {
          moonbit_panic();
        }
        _tmp$3758
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$57[
            _tmp$1618
          ];
        _tmp$1617 = _tmp$3758;
        if (_tmp$1616 < 0 || _tmp$1616 >= Moonbit_array_length(dst$56)) {
          moonbit_panic();
        }
        _old$3757
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$56[
            _tmp$1616
          ];
        if (_tmp$1617) {
          moonbit_incref(_tmp$1617);
        }
        if (_old$3757) {
          moonbit_decref(_old$3757);
        }
        dst$56[_tmp$1616] = _tmp$1617;
        _tmp$1619 = i$60 + 1;
        i$60 = _tmp$1619;
        continue;
      } else {
        moonbit_decref(src$57);
        moonbit_decref(dst$56);
      }
      break;
    }
  } else {
    int32_t _tmp$1624 = len$61 - 1;
    int32_t i$63 = _tmp$1624;
    while (1) {
      if (i$63 >= 0) {
        int32_t _tmp$1620 = dst_offset$58 + i$63;
        int32_t _tmp$1622 = src_offset$59 + i$63;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3760;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1621;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$3759;
        int32_t _tmp$1623;
        if (_tmp$1622 < 0 || _tmp$1622 >= Moonbit_array_length(src$57)) {
          moonbit_panic();
        }
        _tmp$3760
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$57[
            _tmp$1622
          ];
        _tmp$1621 = _tmp$3760;
        if (_tmp$1620 < 0 || _tmp$1620 >= Moonbit_array_length(dst$56)) {
          moonbit_panic();
        }
        _old$3759
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$56[
            _tmp$1620
          ];
        if (_tmp$1621) {
          moonbit_incref(_tmp$1621);
        }
        if (_old$3759) {
          moonbit_decref(_old$3759);
        }
        dst$56[_tmp$1620] = _tmp$1621;
        _tmp$1623 = i$63 - 1;
        i$63 = _tmp$1623;
        continue;
      } else {
        moonbit_decref(src$57);
        moonbit_decref(dst$56);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$47,
  int32_t dst_offset$49,
  struct $$3c$String$2a$Int$3e$** src$48,
  int32_t src_offset$50,
  int32_t len$52
) {
  int32_t _if_result$4190;
  if (dst$47 == src$48) {
    _if_result$4190 = dst_offset$49 < src_offset$50;
  } else {
    _if_result$4190 = 0;
  }
  if (_if_result$4190) {
    int32_t i$51 = 0;
    while (1) {
      if (i$51 < len$52) {
        int32_t _tmp$1607 = dst_offset$49 + i$51;
        int32_t _tmp$1609 = src_offset$50 + i$51;
        struct $$3c$String$2a$Int$3e$* _tmp$3762;
        struct $$3c$String$2a$Int$3e$* _tmp$1608;
        struct $$3c$String$2a$Int$3e$* _old$3761;
        int32_t _tmp$1610;
        if (_tmp$1609 < 0 || _tmp$1609 >= Moonbit_array_length(src$48)) {
          moonbit_panic();
        }
        _tmp$3762 = (struct $$3c$String$2a$Int$3e$*)src$48[_tmp$1609];
        _tmp$1608 = _tmp$3762;
        if (_tmp$1607 < 0 || _tmp$1607 >= Moonbit_array_length(dst$47)) {
          moonbit_panic();
        }
        _old$3761 = (struct $$3c$String$2a$Int$3e$*)dst$47[_tmp$1607];
        if (_tmp$1608) {
          moonbit_incref(_tmp$1608);
        }
        if (_old$3761) {
          moonbit_decref(_old$3761);
        }
        dst$47[_tmp$1607] = _tmp$1608;
        _tmp$1610 = i$51 + 1;
        i$51 = _tmp$1610;
        continue;
      } else {
        moonbit_decref(src$48);
        moonbit_decref(dst$47);
      }
      break;
    }
  } else {
    int32_t _tmp$1615 = len$52 - 1;
    int32_t i$54 = _tmp$1615;
    while (1) {
      if (i$54 >= 0) {
        int32_t _tmp$1611 = dst_offset$49 + i$54;
        int32_t _tmp$1613 = src_offset$50 + i$54;
        struct $$3c$String$2a$Int$3e$* _tmp$3764;
        struct $$3c$String$2a$Int$3e$* _tmp$1612;
        struct $$3c$String$2a$Int$3e$* _old$3763;
        int32_t _tmp$1614;
        if (_tmp$1613 < 0 || _tmp$1613 >= Moonbit_array_length(src$48)) {
          moonbit_panic();
        }
        _tmp$3764 = (struct $$3c$String$2a$Int$3e$*)src$48[_tmp$1613];
        _tmp$1612 = _tmp$3764;
        if (_tmp$1611 < 0 || _tmp$1611 >= Moonbit_array_length(dst$47)) {
          moonbit_panic();
        }
        _old$3763 = (struct $$3c$String$2a$Int$3e$*)dst$47[_tmp$1611];
        if (_tmp$1612) {
          moonbit_incref(_tmp$1612);
        }
        if (_old$3763) {
          moonbit_decref(_old$3763);
        }
        dst$47[_tmp$1611] = _tmp$1612;
        _tmp$1614 = i$54 - 1;
        i$54 = _tmp$1614;
        continue;
      } else {
        moonbit_decref(src$48);
        moonbit_decref(dst$47);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$38,
  int32_t dst_offset$40,
  moonbit_string_t* src$39,
  int32_t src_offset$41,
  int32_t len$43
) {
  int32_t _if_result$4193;
  if (dst$38 == src$39) {
    _if_result$4193 = dst_offset$40 < src_offset$41;
  } else {
    _if_result$4193 = 0;
  }
  if (_if_result$4193) {
    int32_t i$42 = 0;
    while (1) {
      if (i$42 < len$43) {
        int32_t _tmp$1598 = dst_offset$40 + i$42;
        int32_t _tmp$1600 = src_offset$41 + i$42;
        moonbit_string_t _tmp$3766;
        moonbit_string_t _tmp$1599;
        moonbit_string_t _old$3765;
        int32_t _tmp$1601;
        if (_tmp$1600 < 0 || _tmp$1600 >= Moonbit_array_length(src$39)) {
          moonbit_panic();
        }
        _tmp$3766 = (moonbit_string_t)src$39[_tmp$1600];
        _tmp$1599 = _tmp$3766;
        if (_tmp$1598 < 0 || _tmp$1598 >= Moonbit_array_length(dst$38)) {
          moonbit_panic();
        }
        _old$3765 = (moonbit_string_t)dst$38[_tmp$1598];
        moonbit_incref(_tmp$1599);
        moonbit_decref(_old$3765);
        dst$38[_tmp$1598] = _tmp$1599;
        _tmp$1601 = i$42 + 1;
        i$42 = _tmp$1601;
        continue;
      } else {
        moonbit_decref(src$39);
        moonbit_decref(dst$38);
      }
      break;
    }
  } else {
    int32_t _tmp$1606 = len$43 - 1;
    int32_t i$45 = _tmp$1606;
    while (1) {
      if (i$45 >= 0) {
        int32_t _tmp$1602 = dst_offset$40 + i$45;
        int32_t _tmp$1604 = src_offset$41 + i$45;
        moonbit_string_t _tmp$3768;
        moonbit_string_t _tmp$1603;
        moonbit_string_t _old$3767;
        int32_t _tmp$1605;
        if (_tmp$1604 < 0 || _tmp$1604 >= Moonbit_array_length(src$39)) {
          moonbit_panic();
        }
        _tmp$3768 = (moonbit_string_t)src$39[_tmp$1604];
        _tmp$1603 = _tmp$3768;
        if (_tmp$1602 < 0 || _tmp$1602 >= Moonbit_array_length(dst$38)) {
          moonbit_panic();
        }
        _old$3767 = (moonbit_string_t)dst$38[_tmp$1602];
        moonbit_incref(_tmp$1603);
        moonbit_decref(_old$3767);
        dst$38[_tmp$1602] = _tmp$1603;
        _tmp$1605 = i$45 - 1;
        i$45 = _tmp$1605;
        continue;
      } else {
        moonbit_decref(src$39);
        moonbit_decref(dst$38);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$29,
  int32_t dst_offset$31,
  moonbit_bytes_t src$30,
  int32_t src_offset$32,
  int32_t len$34
) {
  int32_t _if_result$4196;
  if (dst$29 == src$30) {
    _if_result$4196 = dst_offset$31 < src_offset$32;
  } else {
    _if_result$4196 = 0;
  }
  if (_if_result$4196) {
    int32_t i$33 = 0;
    while (1) {
      if (i$33 < len$34) {
        int32_t _tmp$1589 = dst_offset$31 + i$33;
        int32_t _tmp$1591 = src_offset$32 + i$33;
        int32_t _tmp$1590;
        int32_t _tmp$1592;
        if (_tmp$1591 < 0 || _tmp$1591 >= Moonbit_array_length(src$30)) {
          moonbit_panic();
        }
        _tmp$1590 = (int32_t)src$30[_tmp$1591];
        if (_tmp$1589 < 0 || _tmp$1589 >= Moonbit_array_length(dst$29)) {
          moonbit_panic();
        }
        dst$29[_tmp$1589] = _tmp$1590;
        _tmp$1592 = i$33 + 1;
        i$33 = _tmp$1592;
        continue;
      } else {
        moonbit_decref(src$30);
        moonbit_decref(dst$29);
      }
      break;
    }
  } else {
    int32_t _tmp$1597 = len$34 - 1;
    int32_t i$36 = _tmp$1597;
    while (1) {
      if (i$36 >= 0) {
        int32_t _tmp$1593 = dst_offset$31 + i$36;
        int32_t _tmp$1595 = src_offset$32 + i$36;
        int32_t _tmp$1594;
        int32_t _tmp$1596;
        if (_tmp$1595 < 0 || _tmp$1595 >= Moonbit_array_length(src$30)) {
          moonbit_panic();
        }
        _tmp$1594 = (int32_t)src$30[_tmp$1595];
        if (_tmp$1593 < 0 || _tmp$1593 >= Moonbit_array_length(dst$29)) {
          moonbit_panic();
        }
        dst$29[_tmp$1593] = _tmp$1594;
        _tmp$1596 = i$36 - 1;
        i$36 = _tmp$1596;
        continue;
      } else {
        moonbit_decref(src$30);
        moonbit_decref(dst$29);
      }
      break;
    }
  }
  return 0;
}

int64_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$27,
  moonbit_string_t loc$28
) {
  moonbit_string_t _tmp$1588 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$27);
  moonbit_string_t _tmp$1586 =
    moonbit_add_string(
      _tmp$1588, (moonbit_string_t)moonbit_string_literal_135.data
    );
  moonbit_string_t _tmp$1587 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$28);
  moonbit_string_t _tmp$1585 = moonbit_add_string(_tmp$1586, _tmp$1587);
  moonbit_string_t _tmp$1584 =
    moonbit_add_string(
      _tmp$1585, (moonbit_string_t)moonbit_string_literal_136.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1584);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$25,
  moonbit_string_t loc$26
) {
  moonbit_string_t _tmp$1583 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$25);
  moonbit_string_t _tmp$1581 =
    moonbit_add_string(
      _tmp$1583, (moonbit_string_t)moonbit_string_literal_135.data
    );
  moonbit_string_t _tmp$1582 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$26);
  moonbit_string_t _tmp$1580 = moonbit_add_string(_tmp$1581, _tmp$1582);
  moonbit_string_t _tmp$1579 =
    moonbit_add_string(
      _tmp$1580, (moonbit_string_t)moonbit_string_literal_136.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1579);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$23,
  moonbit_string_t loc$24
) {
  moonbit_string_t _tmp$1578 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$23);
  moonbit_string_t _tmp$1576 =
    moonbit_add_string(
      _tmp$1578, (moonbit_string_t)moonbit_string_literal_135.data
    );
  moonbit_string_t _tmp$1577 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$24);
  moonbit_string_t _tmp$1575 = moonbit_add_string(_tmp$1576, _tmp$1577);
  moonbit_string_t _tmp$1574 =
    moonbit_add_string(
      _tmp$1575, (moonbit_string_t)moonbit_string_literal_136.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1574);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$21,
  moonbit_string_t loc$22
) {
  moonbit_string_t _tmp$1573 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$1571 =
    moonbit_add_string(
      _tmp$1573, (moonbit_string_t)moonbit_string_literal_135.data
    );
  moonbit_string_t _tmp$1572 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$1570 = moonbit_add_string(_tmp$1571, _tmp$1572);
  moonbit_string_t _tmp$1569 =
    moonbit_add_string(
      _tmp$1570, (moonbit_string_t)moonbit_string_literal_136.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1569);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Eq$$Unit$$equal(
  int32_t _discard_$19,
  int32_t _discard_$20
) {
  return 1;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$15,
  struct $$moonbitlang$core$builtin$Logger _x_5272$18
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$16 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$15;
  moonbit_string_t _field$3769 = _Failure$16->$0;
  int32_t _cnt$3895 = Moonbit_object_header(_Failure$16)->rc;
  moonbit_string_t _$2a$err_payload_5273$17;
  struct $$moonbitlang$core$builtin$Logger _bind$1568;
  if (_cnt$3895 > 1) {
    int32_t _new_cnt$3896;
    moonbit_incref(_field$3769);
    _new_cnt$3896 = _cnt$3895 - 1;
    Moonbit_object_header(_Failure$16)->rc = _new_cnt$3896;
  } else if (_cnt$3895 == 1) {
    moonbit_free(_Failure$16);
  }
  _$2a$err_payload_5273$17 = _field$3769;
  if (_x_5272$18.$1) {
    moonbit_incref(_x_5272$18.$1);
  }
  _x_5272$18.$0->$method_0(
    _x_5272$18.$1, (moonbit_string_t)moonbit_string_literal_137.data
  );
  if (_x_5272$18.$1) {
    moonbit_incref(_x_5272$18.$1);
  }
  $$moonbitlang$core$builtin$Logger$$write_object$2(
    _x_5272$18, _$2a$err_payload_5273$17
  );
  _bind$1568 = _x_5272$18;
  _bind$1568.$0->$method_0(
    _bind$1568.$1, (moonbit_string_t)moonbit_string_literal_112.data
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
  void* _x_5285$13,
  struct $$moonbitlang$core$builtin$Logger _x_5286$14
) {
  switch (Moonbit_object_tag(_x_5285$13)) {
    case 1: {
      _x_5286$14.$0->$method_0(
        _x_5286$14.$1, (moonbit_string_t)moonbit_string_literal_138.data
      );
      break;
    }
    default: {
      moonbit_decref(_x_5285$13);
      _x_5286$14.$0->$method_0(
        _x_5286$14.$1, (moonbit_string_t)moonbit_string_literal_139.data
      );
      break;
    }
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
  int32_t _x_5289$11,
  int32_t _x_5290$12
) {
  switch (_x_5289$11) {
    case 0: {
      switch (_x_5290$12) {
        case 0: {
          return 1;
          break;
        }
        default: {
          return 0;
          break;
        }
      }
      break;
    }
    default: {
      switch (_x_5290$12) {
        case 1: {
          return 1;
          break;
        }
        default: {
          return 0;
          break;
        }
      }
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$write_object$2(
  struct $$moonbitlang$core$builtin$Logger self$10,
  moonbit_string_t obj$9
) {
  $$moonbitlang$core$builtin$Show$$String$$output(obj$9, self$10);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$write_object$1(
  struct $$moonbitlang$core$builtin$Logger self$8,
  int32_t obj$7
) {
  $$moonbitlang$core$builtin$Show$$Unit$$output(obj$7, self$8);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$write_object$0(
  struct $$moonbitlang$core$builtin$Logger self$6,
  int32_t obj$5
) {
  $$moonbitlang$core$builtin$Show$$Int$$output(obj$5, self$6);
  return 0;
}

int64_t $moonbitlang$core$abort$abort$3(moonbit_string_t msg$4) {
  moonbit_println(msg$4);
  moonbit_decref(msg$4);
  moonbit_panic();
}

struct $StringView $moonbitlang$core$abort$abort$2(moonbit_string_t msg$3) {
  moonbit_println(msg$3);
  moonbit_decref(msg$3);
  moonbit_panic();
}

int32_t $moonbitlang$core$abort$abort$1(moonbit_string_t msg$2) {
  moonbit_println(msg$2);
  moonbit_decref(msg$2);
  moonbit_panic();
}

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1) {
  moonbit_println(msg$1);
  moonbit_decref(msg$1);
  moonbit_panic();
  return 0;
}

moonbit_string_t $Error$to_string(void* _e$1503) {
  switch (Moonbit_object_tag(_e$1503)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
               _e$1503
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
               _e$1503
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1503);
      return (moonbit_string_t)moonbit_string_literal_140.data;
      break;
    }
    
    case 5: {
      moonbit_decref(_e$1503);
      return (moonbit_string_t)moonbit_string_literal_141.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
               _e$1503
             );
      break;
    }
    default: {
      moonbit_decref(_e$1503);
      return (moonbit_string_t)moonbit_string_literal_142.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1521,
  int32_t _param$1520
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1519 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1521;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1519, _param$1520
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1518,
  struct $StringView _param$1517
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1516 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1518;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1516, _param$1517
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1515,
  moonbit_string_t _param$1512,
  int32_t _param$1513,
  int32_t _param$1514
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1511 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1515;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1511, _param$1512, _param$1513, _param$1514
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1510,
  moonbit_string_t _param$1509
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1508 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1510;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1508, _param$1509
  );
  return 0;
}

void moonbit_init() {
  moonbit_string_t* _tmp$1563;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1562;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1561;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1532;
  moonbit_string_t* _tmp$1560;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1559;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1558;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1533;
  moonbit_string_t* _tmp$1557;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1556;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1555;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1534;
  moonbit_string_t* _tmp$1554;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1553;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1552;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1535;
  moonbit_string_t* _tmp$1551;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1550;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1549;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1536;
  moonbit_string_t* _tmp$1548;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1547;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1546;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1537;
  moonbit_string_t* _tmp$1545;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1544;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1543;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1538;
  moonbit_string_t* _tmp$1542;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1541;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1540;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1539;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$1327;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1531;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1530;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1529;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1528;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1326;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1527;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1526;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1329;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1565;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1564;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1328;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1567;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1566;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0$dyncall$closure.data;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$142 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$156 = (int64_t)0;
  _tmp$1563 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1563[0] = (moonbit_string_t)moonbit_string_literal_143.data;
  _tmp$1562
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1562)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1562->$0 = _tmp$1563;
  _tmp$1562->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0$clo
  );
  _tuple$1561
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1561)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1561->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_0$clo;
  _tuple$1561->$1 = _tmp$1562;
  _tuple$1532
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1532)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1532->$0 = 0;
  _tuple$1532->$1 = _tuple$1561;
  _tmp$1560 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1560[0] = (moonbit_string_t)moonbit_string_literal_144.data;
  _tmp$1559
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1559)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1559->$0 = _tmp$1560;
  _tmp$1559->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1$clo
  );
  _tuple$1558
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1558)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1558->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_1$clo;
  _tuple$1558->$1 = _tmp$1559;
  _tuple$1533
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1533)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1533->$0 = 1;
  _tuple$1533->$1 = _tuple$1558;
  _tmp$1557 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1557[0] = (moonbit_string_t)moonbit_string_literal_145.data;
  _tmp$1556
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1556)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1556->$0 = _tmp$1557;
  _tmp$1556->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2$clo
  );
  _tuple$1555
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1555)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1555->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_2$clo;
  _tuple$1555->$1 = _tmp$1556;
  _tuple$1534
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1534)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1534->$0 = 2;
  _tuple$1534->$1 = _tuple$1555;
  _tmp$1554 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1554[0] = (moonbit_string_t)moonbit_string_literal_146.data;
  _tmp$1553
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1553)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1553->$0 = _tmp$1554;
  _tmp$1553->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3$clo
  );
  _tuple$1552
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1552)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1552->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_3$clo;
  _tuple$1552->$1 = _tmp$1553;
  _tuple$1535
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1535)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1535->$0 = 3;
  _tuple$1535->$1 = _tuple$1552;
  _tmp$1551 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1551[0] = (moonbit_string_t)moonbit_string_literal_147.data;
  _tmp$1550
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1550)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1550->$0 = _tmp$1551;
  _tmp$1550->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$clo
  );
  _tuple$1549
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1549)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1549->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_4$clo;
  _tuple$1549->$1 = _tmp$1550;
  _tuple$1536
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1536)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1536->$0 = 4;
  _tuple$1536->$1 = _tuple$1549;
  _tmp$1548 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1548[0] = (moonbit_string_t)moonbit_string_literal_148.data;
  _tmp$1547
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1547)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1547->$0 = _tmp$1548;
  _tmp$1547->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5$clo
  );
  _tuple$1546
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1546)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1546->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_5$clo;
  _tuple$1546->$1 = _tmp$1547;
  _tuple$1537
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1537)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1537->$0 = 5;
  _tuple$1537->$1 = _tuple$1546;
  _tmp$1545 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1545[0] = (moonbit_string_t)moonbit_string_literal_149.data;
  _tmp$1544
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1544)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1544->$0 = _tmp$1545;
  _tmp$1544->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$clo
  );
  _tuple$1543
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1543)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1543->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_6$clo;
  _tuple$1543->$1 = _tmp$1544;
  _tuple$1538
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1538)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1538->$0 = 6;
  _tuple$1538->$1 = _tuple$1543;
  _tmp$1542 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1542[0] = (moonbit_string_t)moonbit_string_literal_150.data;
  _tmp$1541
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1541)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1541->$0 = _tmp$1542;
  _tmp$1541->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$clo
  );
  _tuple$1540
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1540)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1540->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$__test_62617369635f656e68616e6365645f746573742e6d6274_7$clo;
  _tuple$1540->$1 = _tmp$1541;
  _tuple$1539
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1539)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1539->$0 = 7;
  _tuple$1539->$1 = _tuple$1540;
  _bind$1327
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      8
    );
  _bind$1327[0] = _tuple$1532;
  _bind$1327[1] = _tuple$1533;
  _bind$1327[2] = _tuple$1534;
  _bind$1327[3] = _tuple$1535;
  _bind$1327[4] = _tuple$1536;
  _bind$1327[5] = _tuple$1537;
  _bind$1327[6] = _tuple$1538;
  _bind$1327[7] = _tuple$1539;
  _tmp$1531 = _bind$1327;
  _tmp$1530
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 8, _tmp$1531
  };
  _tmp$1529 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1530);
  _tuple$1528
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1528)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1528->$0 = (moonbit_string_t)moonbit_string_literal_151.data;
  _tuple$1528->$1 = _tmp$1529;
  _bind$1326
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      1
    );
  _bind$1326[0] = _tuple$1528;
  _tmp$1527 = _bind$1326;
  _tmp$1526
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 1, _tmp$1527
  };
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1526
  );
  _bind$1329
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1565 = _bind$1329;
  _tmp$1564
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1565
  };
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1564
  );
  _bind$1328
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1567 = _bind$1328;
  _tmp$1566
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1567
  };
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1566
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1525;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1497;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1498;
  int32_t _len$1499;
  int32_t _i$1500;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1525
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1497
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1497)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1497->$0 = _tmp$1525;
  async_tests$1497->$1 = 0;
  _arr$1498
  = $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1498);
  _len$1499 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1498);
  _i$1500 = 0;
  while (1) {
    if (_i$1500 < _len$1499) {
      struct $$3c$String$2a$Int$3e$* arg$1501;
      moonbit_string_t _field$3771;
      moonbit_string_t _tmp$1522;
      int32_t _field$3770;
      int32_t _cnt$3897;
      int32_t _tmp$1523;
      int32_t _tmp$1524;
      moonbit_incref(_arr$1498);
      arg$1501
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1498, _i$1500
      );
      _field$3771 = arg$1501->$0;
      _tmp$1522 = _field$3771;
      _field$3770 = arg$1501->$1;
      _cnt$3897 = Moonbit_object_header(arg$1501)->rc;
      if (_cnt$3897 > 1) {
        int32_t _new_cnt$3898;
        moonbit_incref(_tmp$1522);
        _new_cnt$3898 = _cnt$3897 - 1;
        Moonbit_object_header(arg$1501)->rc = _new_cnt$3898;
      } else if (_cnt$3897 == 1) {
        moonbit_free(arg$1501);
      }
      _tmp$1523 = _field$3770;
      moonbit_incref(async_tests$1497);
      $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute(
        async_tests$1497, _tmp$1522, _tmp$1523
      );
      _tmp$1524 = _i$1500 + 1;
      _i$1500 = _tmp$1524;
      continue;
    } else {
      moonbit_decref(_arr$1498);
    }
    break;
  }
  $azimuth$telemetry$tests$azimuth$telemetry$basic$2d$enhanced_blackbox_test$moonbit_test_driver_internal_run_async_tests(
    async_tests$1497
  );
  return 0;
}