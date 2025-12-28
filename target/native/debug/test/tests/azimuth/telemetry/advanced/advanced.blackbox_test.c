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
struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$Int$3e$;

struct $$moonbitlang$core$builtin$Array$3c$String$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$3c$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $Ref$3c$Int$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $StringView;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

struct $$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $Moonbit_Test_Driver_Internal__TestCase;

struct $$3c$$3e$$3d$$3e$Unit;

struct $$3c$Int$2a$Int$3e$;

struct $$3c$String$3e$$3d$$3e$Int;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1;

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap;

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154;

struct $$3c$StringView$2a$StringView$3e$;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $Moonbit_Test_Driver_Internal__F$F2;

struct $$moonbitlang$core$builtin$Hasher;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $Option$3c$StringView$3e$$Some;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  int32_t $6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $5;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $StringView {
  int32_t $1;
  int32_t $2;
  moonbit_string_t $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
  moonbit_string_t $0;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
};

struct $Result$3c$Unit$2a$Error$3e$$Ok {
  int32_t $0;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
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

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$ {
  int32_t $1;
  struct $$3c$String$2a$Int$3e$** $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  moonbit_string_t $1;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
};

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap {
  int32_t(* code)(struct $$3c$String$3e$$3d$$3e$Int*, moonbit_string_t);
  struct $$3c$String$3e$$3d$$3e$Int* $0;
  
};

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** $0;
  
};

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$ {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0_0;
  void* $0_1;
  moonbit_string_t $1;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154 {
  void* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
};

struct $Result$3c$Unit$2a$Error$3e$$Err {
  void* $0;
  
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

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
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

struct $Option$3c$StringView$3e$$Some {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
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

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$937
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$1950,
  moonbit_string_t s$915,
  int32_t sep$916
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$902
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$1859,
  moonbit_bytes_t bytes$903
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$1852,
  moonbit_string_t s$897
);

#define $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$860,
  moonbit_string_t filename$821,
  int32_t index$822
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1845
);

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1841
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1839
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1823,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$861,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$862
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1834,
  int32_t _cont_param$881
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1831,
  void* _cont_param$882
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1825,
  void* _state$864
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1820,
  void* err$844
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1806,
  moonbit_string_t attr$837
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1790,
  moonbit_string_t test_name$824,
  moonbit_string_t file_name$825,
  moonbit_string_t message$826,
  int32_t skipped$827
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$819
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$817,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$818,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$815
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$778,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$791,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$804,
  moonbit_string_t file_filter$775,
  int32_t index_filter$776
);

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$767
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$765,
  struct $$moonbitlang$core$builtin$Logger logger$766
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$751,
  struct $$moonbitlang$core$builtin$Logger logger$764
);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$749);

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$748,
  struct $$moonbitlang$core$builtin$Hasher* hasher$747
);

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$746,
  struct $$moonbitlang$core$builtin$Hasher* hasher$745
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$743,
  moonbit_string_t value$741
);

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$737,
  struct $$3c$String$3e$$3d$$3e$Int* f$739
);

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1746,
  moonbit_string_t k$738
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$735,
  int32_t idx$736
);

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$733,
  int32_t idx$734
);

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$729,
  int32_t key$725
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$720,
  moonbit_string_t key$716
);

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$711,
  int32_t key$707
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$702,
  moonbit_string_t key$698
);

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$693,
  int32_t key$689
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$684,
  moonbit_string_t key$680
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$672
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$664
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$656
);

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$652,
  moonbit_string_t key$653,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$654
);

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$649,
  moonbit_string_t key$650,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$651
);

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$646,
  moonbit_string_t key$647,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$648
);

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$636
);

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$625
);

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$614
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$597,
  moonbit_string_t key$606,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$607,
  int32_t hash$605
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$581,
  moonbit_string_t key$590,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$591,
  int32_t hash$589
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$565,
  moonbit_string_t key$574,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$575,
  int32_t hash$573
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$559,
  int32_t idx$564,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$563
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$549,
  int32_t idx$554,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$553
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$539,
  int32_t idx$544,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$543
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$529,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$531,
  int32_t new_idx$530
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$523,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$525,
  int32_t new_idx$524
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$517,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$519,
  int32_t new_idx$518
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$514,
  int32_t idx$516,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$515
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$510,
  int32_t idx$512,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$511
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$506,
  int32_t idx$508,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$507
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$500
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$494
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$488
);

int32_t $Int$$next_power_of_two(int32_t self$486);

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$485);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$483
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$481
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$479
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$478
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$477
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$475
);

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1459
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$473
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$472,
  struct $$moonbitlang$core$builtin$Logger logger$471
);

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$469,
  struct $$3c$String$3e$$3d$$3e$Int* f$470
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$465,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$467
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$462,
  struct $$3c$String$2a$Int$3e$* value$464
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$459,
  moonbit_string_t value$461
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$457
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$454
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$451
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$447,
  int32_t new_capacity$445
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$441,
  int32_t new_capacity$439
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$435,
  int32_t new_capacity$433
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$431
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$429,
  struct $StringView str$430
);

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$426,
  int32_t i$427,
  int32_t start_offset$428,
  int64_t end_offset$424
);

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$421,
  int32_t n$419,
  int32_t start_offset$415,
  int32_t end_offset$416
);

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$413,
  int32_t n$411,
  int32_t start_offset$410,
  int32_t end_offset$409
);

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$399,
  int32_t len$402,
  int32_t start_offset$406,
  int64_t end_offset$397
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$395
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$394
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$393
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$392
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387
);

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1390,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$385
);

moonbit_string_t $$moonbitlang$core$builtin$Iterator$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$384
);

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$375,
  struct $$moonbitlang$core$builtin$Logger logger$373
);

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$369,
  int32_t seg$372,
  int32_t i$371
);

moonbit_string_t $Byte$$to_hex(int32_t b$367);

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$365);

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$363,
  int32_t that$364
);

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$361,
  int32_t that$362
);

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$359,
  int32_t that$360
);

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$357,
  int32_t that$358
);

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$354,
  int32_t start$352,
  int32_t end$353
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$351
);

moonbit_string_t $Int$$to_string$inner(int32_t self$335, int32_t radix$334);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$328,
  int32_t radix$331
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$326);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$325);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$315,
  uint32_t num$303,
  int32_t digit_start$306,
  int32_t total_len$305
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$297,
  uint32_t num$291,
  int32_t digit_start$289,
  int32_t total_len$288,
  int32_t radix$293
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$284,
  uint32_t num$280,
  int32_t digit_start$278,
  int32_t total_len$277
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$275
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$273
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$271
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$269
);

int32_t $StringView$$start_offset(struct $StringView self$267);

moonbit_string_t $StringView$$data(struct $StringView self$266);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$260,
  moonbit_string_t value$263,
  int32_t start$264,
  int32_t len$265
);

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$253,
  int32_t start$259,
  int64_t end$255
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$251
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$249
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$246
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$244
);

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$243
);

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$242
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$240,
  int32_t value$239
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$238,
  moonbit_string_t value$237
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$235,
  int32_t value$236
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$233,
  uint32_t value$234
);

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$231,
  uint32_t input$232
);

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$229, int32_t r$230);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$227,
  moonbit_string_t str$228
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$219,
  int32_t bytes_offset$214,
  moonbit_string_t str$221,
  int32_t str_offset$217,
  int32_t length$215
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$181
);

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$177
);

struct $StringView $StringView$$view$inner(
  struct $StringView self$174,
  int32_t start_offset$175,
  int64_t end_offset$172
);

int64_t $StringView$$rev_find(
  struct $StringView self$170,
  struct $StringView str$169
);

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$160,
  struct $StringView needle$162
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$149,
  struct $StringView needle$151
);

int64_t $StringView$$find(
  struct $StringView self$147,
  struct $StringView str$146
);

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$136,
  struct $StringView needle$138
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$122,
  struct $StringView needle$124
);

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$118,
  int32_t index$119
);

int32_t $StringView$$length(struct $StringView self$117);

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$115,
  int32_t index$116
);

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$113
);

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$112
);

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$111
);

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$110
);

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$109
);

moonbit_string_t $String$$escape(moonbit_string_t self$108);

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$105, int32_t y$106);

int32_t $Int$$is_trailing_surrogate(int32_t self$104);

int32_t $Int$$is_leading_surrogate(int32_t self$103);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$100,
  int32_t ch$102
);

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$95,
  int32_t required$96
);

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default();

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$89,
  int32_t offset$90,
  int32_t value$88
);

int32_t $UInt$$to_byte(uint32_t self$86);

uint32_t $Char$$to_uint(int32_t self$85);

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$84
);

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$79,
  int32_t offset$83,
  int64_t length$81
);

#define $moonbitlang$core$builtin$unsafe_sub_string moonbit_unsafe_bytes_sub_string

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$76
);

int32_t $Byte$$to_char(int32_t self$74);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$69,
  int32_t dst_offset$70,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$71,
  int32_t src_offset$72,
  int32_t len$73
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$64,
  int32_t dst_offset$65,
  struct $$3c$String$2a$Int$3e$** src$66,
  int32_t src_offset$67,
  int32_t len$68
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$59,
  int32_t dst_offset$60,
  moonbit_string_t* src$61,
  int32_t src_offset$62,
  int32_t len$63
);

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$50,
  int32_t dst_offset$52,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$51,
  int32_t src_offset$53,
  int32_t len$55
);

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$41,
  int32_t dst_offset$43,
  struct $$3c$String$2a$Int$3e$** src$42,
  int32_t src_offset$44,
  int32_t len$46
);

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$32,
  int32_t dst_offset$34,
  moonbit_string_t* src$33,
  int32_t src_offset$35,
  int32_t len$37
);

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$23,
  int32_t dst_offset$25,
  moonbit_bytes_t src$24,
  int32_t src_offset$26,
  int32_t len$28
);

int64_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$21,
  moonbit_string_t loc$22
);

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
);

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
);

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$11,
  struct $$moonbitlang$core$builtin$Logger _x_5272$14
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
  void* _x_5285$9,
  struct $$moonbitlang$core$builtin$Logger _x_5286$10
);

int32_t $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
  int32_t _x_5289$7,
  int32_t _x_5290$8
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$0(
  struct $$moonbitlang$core$builtin$Logger self$6,
  moonbit_string_t obj$5
);

int64_t $moonbitlang$core$abort$abort$3(moonbit_string_t msg$4);

struct $StringView $moonbitlang$core$abort$abort$2(moonbit_string_t msg$3);

int32_t $moonbitlang$core$abort$abort$1(moonbit_string_t msg$2);

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1);

moonbit_string_t $Error$to_string(void* _e$944);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$962,
  int32_t _param$961
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$959,
  struct $StringView _param$958
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$956,
  moonbit_string_t _param$953,
  int32_t _param$954,
  int32_t _param$955
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$951,
  moonbit_string_t _param$950
);

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    45, 45, 45, 45, 45, 32, 66, 69, 71, 73, 78, 32, 77, 79, 79, 78, 32, 
    84, 69, 83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_4 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[1]; 
} const moonbit_string_literal_3 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 0), 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 52, 57, 49, 58, 57, 45, 
    52, 57, 49, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_6 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_1 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 45, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_37 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_30 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 48, 58, 53, 45, 50, 
    57, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_27 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    73, 110, 118, 97, 108, 105, 100, 32, 105, 110, 100, 101, 120, 32, 
    102, 111, 114, 32, 86, 105, 101, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_25 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_22 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    92, 117, 123, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_35 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    73, 110, 100, 101, 120, 79, 117, 116, 79, 102, 66, 111, 117, 110, 
    100, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_5 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 32, 98, 
    108, 111, 99, 107, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[54]; 
} const moonbit_string_literal_28 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_34 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 41, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    73, 110, 118, 97, 108, 105, 100, 32, 115, 116, 97, 114, 116, 32, 
    105, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_20 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 73, 110, 115, 
    112, 101, 99, 116, 69, 114, 114, 111, 114, 46, 73, 110, 115, 112, 
    101, 99, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_32 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 10, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_13 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    45, 45, 45, 45, 45, 32, 69, 78, 68, 32, 77, 79, 79, 78, 32, 84, 69, 
    83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_0 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 58, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[131]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 130), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 109, 
    117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 47, 
    97, 100, 118, 97, 110, 99, 101, 100, 95, 98, 108, 97, 99, 107, 98, 
    111, 120, 95, 116, 101, 115, 116, 46, 77, 111, 111, 110, 66, 105, 
    116, 84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 73, 110, 116, 
    101, 114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 114, 46, 77, 
    111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 68, 114, 105, 118, 
    101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 74, 115, 69, 114, 
    114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_15 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_36 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    73, 110, 118, 97, 108, 105, 100, 73, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    70, 97, 105, 108, 117, 114, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_10 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    34, 44, 32, 34, 116, 101, 115, 116, 95, 110, 97, 109, 101, 34, 58, 
    32, 0
  };

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

struct { int32_t rc; uint32_t meta; uint16_t const data[79]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 78), 
    123, 34, 112, 97, 99, 107, 97, 103, 101, 34, 58, 32, 34, 97, 122, 
    105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 
    121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 109, 117, 116, 
    104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 47, 97, 100, 
    118, 97, 110, 99, 101, 100, 34, 44, 32, 34, 102, 105, 108, 101, 110, 
    97, 109, 101, 34, 58, 32, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct { int32_t rc; uint32_t meta; struct $$3c$String$3e$$3d$$3e$Int data; 
} const $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5
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

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$120;

int64_t $moonbitlang$core$builtin$brute_force_find$constr$134;

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_no_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_with_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_async_tests;

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$937
) {
  moonbit_decref(_tests$937);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$896 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$902 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$909 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$902;
  int32_t moonbit_test_driver_internal_split_mbt_string$914 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$1975 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$921 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$922;
  moonbit_string_t _tmp$1974;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$923;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$924;
  int32_t _len$925;
  int32_t _i$926;
  Moonbit_object_header(file_and_index$921)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$921->$0 = _tmp$1975;
  file_and_index$921->$1 = 0;
  cli_args$922
  = $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$909
  );
  _tmp$1974 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$922, 1);
  test_args$923
  = $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$914, _tmp$1974, 47
  );
  _arr$924 = test_args$923;
  moonbit_incref(_arr$924);
  _len$925 = $$moonbitlang$core$builtin$Array$$length$1(_arr$924);
  _i$926 = 0;
  while (1) {
    if (_i$926 < _len$925) {
      moonbit_string_t arg$927;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$928;
      moonbit_string_t file$929;
      moonbit_string_t range$930;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$931;
      moonbit_string_t _tmp$1972;
      int32_t start$932;
      moonbit_string_t _tmp$1971;
      int32_t end$933;
      int32_t i$934;
      int32_t _tmp$1973;
      moonbit_incref(_arr$924);
      arg$927
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$924, _i$926
      );
      file_and_range$928
      = $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$914, arg$927, 58
      );
      moonbit_incref(file_and_range$928);
      file$929
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$928, 0
      );
      range$930
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$928, 1
      );
      start_and_end$931
      = $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$914, range$930, 45
      );
      moonbit_incref(start_and_end$931);
      _tmp$1972
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$931, 0
      );
      start$932
      = $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$896, _tmp$1972
      );
      _tmp$1971
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$931, 1
      );
      end$933
      = $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$896, _tmp$1971
      );
      i$934 = start$932;
      while (1) {
        if (i$934 < end$933) {
          struct $$3c$String$2a$Int$3e$* _tuple$1969;
          int32_t _tmp$1970;
          moonbit_incref(file$929);
          _tuple$1969
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$1969)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$1969->$0 = file$929;
          _tuple$1969->$1 = i$934;
          moonbit_incref(file_and_index$921);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$921, _tuple$1969
          );
          _tmp$1970 = i$934 + 1;
          i$934 = _tmp$1970;
          continue;
        } else {
          moonbit_decref(file$929);
        }
        break;
      }
      _tmp$1973 = _i$926 + 1;
      _i$926 = _tmp$1973;
      continue;
    } else {
      moonbit_decref(_arr$924);
    }
    break;
  }
  return file_and_index$921;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$1950,
  moonbit_string_t s$915,
  int32_t sep$916
) {
  moonbit_string_t* _tmp$1968 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$917 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$918;
  struct $Ref$3c$Int$3e$* start$919;
  int32_t val$1963;
  int32_t _tmp$1964;
  Moonbit_object_header(res$917)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$917->$0 = _tmp$1968;
  res$917->$1 = 0;
  i$918
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$918)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$918->$0 = 0;
  start$919
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$919)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$919->$0 = 0;
  while (1) {
    int32_t val$1951 = i$918->$0;
    int32_t _tmp$1952 = Moonbit_array_length(s$915);
    if (val$1951 < _tmp$1952) {
      int32_t val$1955 = i$918->$0;
      int32_t _tmp$1954;
      int32_t _tmp$1953;
      int32_t val$1962;
      int32_t _tmp$1961;
      if (val$1955 < 0 || val$1955 >= Moonbit_array_length(s$915)) {
        moonbit_panic();
      }
      _tmp$1954 = s$915[val$1955];
      _tmp$1953 = _tmp$1954;
      if (_tmp$1953 == sep$916) {
        int32_t val$1957 = start$919->$0;
        int32_t val$1958 = i$918->$0;
        moonbit_string_t _tmp$1956;
        int32_t val$1960;
        int32_t _tmp$1959;
        moonbit_incref(s$915);
        _tmp$1956 = $String$$unsafe_substring(s$915, val$1957, val$1958);
        moonbit_incref(res$917);
        $$moonbitlang$core$builtin$Array$$push$0(res$917, _tmp$1956);
        val$1960 = i$918->$0;
        _tmp$1959 = val$1960 + 1;
        start$919->$0 = _tmp$1959;
      }
      val$1962 = i$918->$0;
      _tmp$1961 = val$1962 + 1;
      i$918->$0 = _tmp$1961;
      continue;
    } else {
      moonbit_decref(i$918);
    }
    break;
  }
  val$1963 = start$919->$0;
  _tmp$1964 = Moonbit_array_length(s$915);
  if (val$1963 < _tmp$1964) {
    int32_t _field$1976 = start$919->$0;
    int32_t val$1966;
    int32_t _tmp$1967;
    moonbit_string_t _tmp$1965;
    moonbit_decref(start$919);
    val$1966 = _field$1976;
    _tmp$1967 = Moonbit_array_length(s$915);
    _tmp$1965 = $String$$unsafe_substring(s$915, val$1966, _tmp$1967);
    moonbit_incref(res$917);
    $$moonbitlang$core$builtin$Array$$push$0(res$917, _tmp$1965);
  } else {
    moonbit_decref(start$919);
    moonbit_decref(s$915);
  }
  return res$917;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$902
) {
  moonbit_bytes_t* tmp$910 =
    $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$1949 = Moonbit_array_length(tmp$910);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$911 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$1949);
  int32_t i$912 = 0;
  while (1) {
    int32_t _tmp$1945 = Moonbit_array_length(tmp$910);
    if (i$912 < _tmp$1945) {
      moonbit_bytes_t _tmp$1977;
      moonbit_bytes_t _tmp$1947;
      moonbit_string_t _tmp$1946;
      int32_t _tmp$1948;
      if (i$912 < 0 || i$912 >= Moonbit_array_length(tmp$910)) {
        moonbit_panic();
      }
      _tmp$1977 = (moonbit_bytes_t)tmp$910[i$912];
      _tmp$1947 = _tmp$1977;
      moonbit_incref(_tmp$1947);
      _tmp$1946
      = $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$902, _tmp$1947
      );
      moonbit_incref(res$911);
      $$moonbitlang$core$builtin$Array$$push$0(res$911, _tmp$1946);
      _tmp$1948 = i$912 + 1;
      i$912 = _tmp$1948;
      continue;
    } else {
      moonbit_decref(tmp$910);
    }
    break;
  }
  return res$911;
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$1859,
  moonbit_bytes_t bytes$903
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$904 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$905 = Moonbit_array_length(bytes$903);
  struct $Ref$3c$Int$3e$* i$906 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$906)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$906->$0 = 0;
  while (1) {
    int32_t val$1860 = i$906->$0;
    if (val$1860 < len$905) {
      int32_t val$1944 = i$906->$0;
      int32_t _tmp$1943;
      int32_t _tmp$1942;
      struct $Ref$3c$Int$3e$* c$907;
      int32_t val$1861;
      if (val$1944 < 0 || val$1944 >= Moonbit_array_length(bytes$903)) {
        moonbit_panic();
      }
      _tmp$1943 = bytes$903[val$1944];
      _tmp$1942 = (int32_t)_tmp$1943;
      c$907
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$907)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$907->$0 = _tmp$1942;
      val$1861 = c$907->$0;
      if (val$1861 < 128) {
        int32_t _field$1978 = c$907->$0;
        int32_t val$1863;
        int32_t _tmp$1862;
        int32_t val$1865;
        int32_t _tmp$1864;
        moonbit_decref(c$907);
        val$1863 = _field$1978;
        _tmp$1862 = val$1863;
        moonbit_incref(res$904);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$904, _tmp$1862
        );
        val$1865 = i$906->$0;
        _tmp$1864 = val$1865 + 1;
        i$906->$0 = _tmp$1864;
      } else {
        int32_t val$1866 = c$907->$0;
        if (val$1866 < 224) {
          int32_t val$1868 = i$906->$0;
          int32_t _tmp$1867 = val$1868 + 1;
          int32_t val$1877;
          int32_t _tmp$1876;
          int32_t _tmp$1870;
          int32_t val$1875;
          int32_t _tmp$1874;
          int32_t _tmp$1873;
          int32_t _tmp$1872;
          int32_t _tmp$1871;
          int32_t _tmp$1869;
          int32_t _field$1979;
          int32_t val$1879;
          int32_t _tmp$1878;
          int32_t val$1881;
          int32_t _tmp$1880;
          if (_tmp$1867 >= len$905) {
            moonbit_decref(c$907);
            moonbit_decref(i$906);
            moonbit_decref(bytes$903);
            break;
          }
          val$1877 = c$907->$0;
          _tmp$1876 = val$1877 & 31;
          _tmp$1870 = _tmp$1876 << 6;
          val$1875 = i$906->$0;
          _tmp$1874 = val$1875 + 1;
          if (_tmp$1874 < 0 || _tmp$1874 >= Moonbit_array_length(bytes$903)) {
            moonbit_panic();
          }
          _tmp$1873 = bytes$903[_tmp$1874];
          _tmp$1872 = (int32_t)_tmp$1873;
          _tmp$1871 = _tmp$1872 & 63;
          _tmp$1869 = _tmp$1870 | _tmp$1871;
          c$907->$0 = _tmp$1869;
          _field$1979 = c$907->$0;
          moonbit_decref(c$907);
          val$1879 = _field$1979;
          _tmp$1878 = val$1879;
          moonbit_incref(res$904);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$904, _tmp$1878
          );
          val$1881 = i$906->$0;
          _tmp$1880 = val$1881 + 2;
          i$906->$0 = _tmp$1880;
        } else {
          int32_t val$1882 = c$907->$0;
          if (val$1882 < 240) {
            int32_t val$1884 = i$906->$0;
            int32_t _tmp$1883 = val$1884 + 2;
            int32_t val$1900;
            int32_t _tmp$1899;
            int32_t _tmp$1892;
            int32_t val$1898;
            int32_t _tmp$1897;
            int32_t _tmp$1896;
            int32_t _tmp$1895;
            int32_t _tmp$1894;
            int32_t _tmp$1893;
            int32_t _tmp$1886;
            int32_t val$1891;
            int32_t _tmp$1890;
            int32_t _tmp$1889;
            int32_t _tmp$1888;
            int32_t _tmp$1887;
            int32_t _tmp$1885;
            int32_t _field$1980;
            int32_t val$1902;
            int32_t _tmp$1901;
            int32_t val$1904;
            int32_t _tmp$1903;
            if (_tmp$1883 >= len$905) {
              moonbit_decref(c$907);
              moonbit_decref(i$906);
              moonbit_decref(bytes$903);
              break;
            }
            val$1900 = c$907->$0;
            _tmp$1899 = val$1900 & 15;
            _tmp$1892 = _tmp$1899 << 12;
            val$1898 = i$906->$0;
            _tmp$1897 = val$1898 + 1;
            if (
              _tmp$1897 < 0 || _tmp$1897 >= Moonbit_array_length(bytes$903)
            ) {
              moonbit_panic();
            }
            _tmp$1896 = bytes$903[_tmp$1897];
            _tmp$1895 = (int32_t)_tmp$1896;
            _tmp$1894 = _tmp$1895 & 63;
            _tmp$1893 = _tmp$1894 << 6;
            _tmp$1886 = _tmp$1892 | _tmp$1893;
            val$1891 = i$906->$0;
            _tmp$1890 = val$1891 + 2;
            if (
              _tmp$1890 < 0 || _tmp$1890 >= Moonbit_array_length(bytes$903)
            ) {
              moonbit_panic();
            }
            _tmp$1889 = bytes$903[_tmp$1890];
            _tmp$1888 = (int32_t)_tmp$1889;
            _tmp$1887 = _tmp$1888 & 63;
            _tmp$1885 = _tmp$1886 | _tmp$1887;
            c$907->$0 = _tmp$1885;
            _field$1980 = c$907->$0;
            moonbit_decref(c$907);
            val$1902 = _field$1980;
            _tmp$1901 = val$1902;
            moonbit_incref(res$904);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$904, _tmp$1901
            );
            val$1904 = i$906->$0;
            _tmp$1903 = val$1904 + 3;
            i$906->$0 = _tmp$1903;
          } else {
            int32_t val$1906 = i$906->$0;
            int32_t _tmp$1905 = val$1906 + 3;
            int32_t val$1929;
            int32_t _tmp$1928;
            int32_t _tmp$1921;
            int32_t val$1927;
            int32_t _tmp$1926;
            int32_t _tmp$1925;
            int32_t _tmp$1924;
            int32_t _tmp$1923;
            int32_t _tmp$1922;
            int32_t _tmp$1914;
            int32_t val$1920;
            int32_t _tmp$1919;
            int32_t _tmp$1918;
            int32_t _tmp$1917;
            int32_t _tmp$1916;
            int32_t _tmp$1915;
            int32_t _tmp$1908;
            int32_t val$1913;
            int32_t _tmp$1912;
            int32_t _tmp$1911;
            int32_t _tmp$1910;
            int32_t _tmp$1909;
            int32_t _tmp$1907;
            int32_t val$1931;
            int32_t _tmp$1930;
            int32_t val$1935;
            int32_t _tmp$1934;
            int32_t _tmp$1933;
            int32_t _tmp$1932;
            int32_t _field$1981;
            int32_t val$1939;
            int32_t _tmp$1938;
            int32_t _tmp$1937;
            int32_t _tmp$1936;
            int32_t val$1941;
            int32_t _tmp$1940;
            if (_tmp$1905 >= len$905) {
              moonbit_decref(c$907);
              moonbit_decref(i$906);
              moonbit_decref(bytes$903);
              break;
            }
            val$1929 = c$907->$0;
            _tmp$1928 = val$1929 & 7;
            _tmp$1921 = _tmp$1928 << 18;
            val$1927 = i$906->$0;
            _tmp$1926 = val$1927 + 1;
            if (
              _tmp$1926 < 0 || _tmp$1926 >= Moonbit_array_length(bytes$903)
            ) {
              moonbit_panic();
            }
            _tmp$1925 = bytes$903[_tmp$1926];
            _tmp$1924 = (int32_t)_tmp$1925;
            _tmp$1923 = _tmp$1924 & 63;
            _tmp$1922 = _tmp$1923 << 12;
            _tmp$1914 = _tmp$1921 | _tmp$1922;
            val$1920 = i$906->$0;
            _tmp$1919 = val$1920 + 2;
            if (
              _tmp$1919 < 0 || _tmp$1919 >= Moonbit_array_length(bytes$903)
            ) {
              moonbit_panic();
            }
            _tmp$1918 = bytes$903[_tmp$1919];
            _tmp$1917 = (int32_t)_tmp$1918;
            _tmp$1916 = _tmp$1917 & 63;
            _tmp$1915 = _tmp$1916 << 6;
            _tmp$1908 = _tmp$1914 | _tmp$1915;
            val$1913 = i$906->$0;
            _tmp$1912 = val$1913 + 3;
            if (
              _tmp$1912 < 0 || _tmp$1912 >= Moonbit_array_length(bytes$903)
            ) {
              moonbit_panic();
            }
            _tmp$1911 = bytes$903[_tmp$1912];
            _tmp$1910 = (int32_t)_tmp$1911;
            _tmp$1909 = _tmp$1910 & 63;
            _tmp$1907 = _tmp$1908 | _tmp$1909;
            c$907->$0 = _tmp$1907;
            val$1931 = c$907->$0;
            _tmp$1930 = val$1931 - 65536;
            c$907->$0 = _tmp$1930;
            val$1935 = c$907->$0;
            _tmp$1934 = val$1935 >> 10;
            _tmp$1933 = _tmp$1934 + 55296;
            _tmp$1932 = _tmp$1933;
            moonbit_incref(res$904);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$904, _tmp$1932
            );
            _field$1981 = c$907->$0;
            moonbit_decref(c$907);
            val$1939 = _field$1981;
            _tmp$1938 = val$1939 & 1023;
            _tmp$1937 = _tmp$1938 + 56320;
            _tmp$1936 = _tmp$1937;
            moonbit_incref(res$904);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$904, _tmp$1936
            );
            val$1941 = i$906->$0;
            _tmp$1940 = val$1941 + 4;
            i$906->$0 = _tmp$1940;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$906);
      moonbit_decref(bytes$903);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$904);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$1852,
  moonbit_string_t s$897
) {
  struct $Ref$3c$Int$3e$* res$898 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$899;
  int32_t i$900;
  int32_t _field$1982;
  Moonbit_object_header(res$898)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$898->$0 = 0;
  len$899 = Moonbit_array_length(s$897);
  i$900 = 0;
  while (1) {
    if (i$900 < len$899) {
      int32_t val$1857 = res$898->$0;
      int32_t _tmp$1854 = val$1857 * 10;
      int32_t _tmp$1856;
      int32_t _tmp$1855;
      int32_t _tmp$1853;
      int32_t _tmp$1858;
      if (i$900 < 0 || i$900 >= Moonbit_array_length(s$897)) {
        moonbit_panic();
      }
      _tmp$1856 = s$897[i$900];
      _tmp$1855 = _tmp$1856 - 48;
      _tmp$1853 = _tmp$1854 + _tmp$1855;
      res$898->$0 = _tmp$1853;
      _tmp$1858 = i$900 + 1;
      i$900 = _tmp$1858;
      continue;
    } else {
      moonbit_decref(s$897);
    }
    break;
  }
  _field$1982 = res$898->$0;
  moonbit_decref(res$898);
  return _field$1982;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$860,
  moonbit_string_t filename$821,
  int32_t index$822
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$820;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$2362;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$823;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$832;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$1992;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$1851;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$1991;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$833;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$1990;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$1850;
  moonbit_string_t _field$1989;
  moonbit_string_t file_name$834;
  moonbit_string_t name$835;
  int32_t _tmp$1847;
  moonbit_string_t name$836;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$1804;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1805;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$2364;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$843;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$859;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$884;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$886;
  void* _field$1986;
  int32_t _cnt$2238;
  void* _bind$887;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$2368;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1844;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$2369;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1837;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$2370;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1838;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$2371;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1822;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_async_tests
  );
  filtered_test$820
  = $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_async_tests,
      filename$821,
      index$822
  );
  _closure$2362
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$2362)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$2362->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$2362->$0 = index$822;
  handle_result$823
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$2362;
  if (filtered_test$820 == 0) {
    moonbit_decref(async_tests$860);
    if (filtered_test$820) {
      moonbit_decref(filtered_test$820);
    }
    $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$823,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$894 =
      filtered_test$820;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$895 = _Some$894;
    item$832 = _item$895;
    goto $join$831;
  }
  goto $joinlet$2363;
  $join$831:;
  _field$1992 = item$832->$1;
  meta$1851 = _field$1992;
  _field$1991 = meta$1851->$2;
  attrs$833 = _field$1991;
  _field$1990 = item$832->$1;
  meta$1850 = _field$1990;
  _field$1989 = meta$1850->$0;
  file_name$834 = _field$1989;
  moonbit_incref(attrs$833);
  moonbit_incref(file_name$834);
  moonbit_incref(attrs$833);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$833)) {
    name$835 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$833);
    name$835 = $$moonbitlang$core$builtin$Array$$at$0(attrs$833, 0);
  }
  _tmp$1847 = Moonbit_array_length(name$835);
  if (_tmp$1847 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$1988;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$1849;
    int32_t _field$1987;
    int32_t index$1848;
    moonbit_decref(name$835);
    _field$1988 = item$832->$1;
    meta$1849 = _field$1988;
    _field$1987 = meta$1849->$1;
    index$1848 = _field$1987;
    name$836 = $Int$$to_string$inner(index$1848, 10);
  } else {
    name$836 = name$835;
  }
  moonbit_incref(attrs$833);
  _tmp$1804 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$833);
  _tmp$1805
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$1804, _tmp$1805)) {
    moonbit_string_t _tmp$1819;
    moonbit_string_t _tmp$1818;
    moonbit_string_t _tmp$1815;
    moonbit_string_t _tmp$1817;
    moonbit_string_t _tmp$1816;
    moonbit_string_t _tmp$1814;
    moonbit_decref(async_tests$860);
    moonbit_decref(item$832);
    moonbit_incref(file_name$834);
    _tmp$1819
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$834
    );
    _tmp$1818
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$1819
    );
    _tmp$1815
    = moonbit_add_string(
      _tmp$1818, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$1817 = $$moonbitlang$core$builtin$Array$$at$0(attrs$833, 0);
    _tmp$1816 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$1817);
    _tmp$1814 = moonbit_add_string(_tmp$1815, _tmp$1816);
    $moonbitlang$core$builtin$println$0(_tmp$1814);
    $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$823,
        name$836,
        file_name$834,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$833);
  }
  moonbit_incref(name$836);
  moonbit_incref(file_name$834);
  moonbit_incref(handle_result$823);
  _closure$2364
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$2364)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2364->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$2364->$0 = name$836;
  _closure$2364->$1 = file_name$834;
  _closure$2364->$2 = handle_result$823;
  on_err$843 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2364;
  _field$1986 = item$832->$0;
  _cnt$2238 = Moonbit_object_header(item$832)->rc;
  if (_cnt$2238 > 1) {
    int32_t _new_cnt$2240;
    moonbit_incref(_field$1986);
    _new_cnt$2240 = _cnt$2238 - 1;
    Moonbit_object_header(item$832)->rc = _new_cnt$2240;
  } else if (_cnt$2238 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2239 = item$832->$1;
    moonbit_decref(_field$2239);
    moonbit_free(item$832);
  }
  _bind$887 = _field$1986;
  switch (Moonbit_object_tag(_bind$887)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$888;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$1983;
      int32_t _cnt$2241;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$889;
      moonbit_decref(async_tests$860);
      _F0$888 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$887;
      _field$1983 = _F0$888->$0;
      _cnt$2241 = Moonbit_object_header(_F0$888)->rc;
      if (_cnt$2241 > 1) {
        int32_t _new_cnt$2242;
        moonbit_incref(_field$1983);
        _new_cnt$2242 = _cnt$2241 - 1;
        Moonbit_object_header(_F0$888)->rc = _new_cnt$2242;
      } else if (_cnt$2241 == 1) {
        moonbit_free(_F0$888);
      }
      _f$889 = _field$1983;
      f$886 = _f$889;
      goto $join$885;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$890;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$1984;
      int32_t _cnt$2243;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$891;
      moonbit_decref(async_tests$860);
      _F1$890 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$887;
      _field$1984 = _F1$890->$0;
      _cnt$2243 = Moonbit_object_header(_F1$890)->rc;
      if (_cnt$2243 > 1) {
        int32_t _new_cnt$2244;
        moonbit_incref(_field$1984);
        _new_cnt$2244 = _cnt$2243 - 1;
        Moonbit_object_header(_F1$890)->rc = _new_cnt$2244;
      } else if (_cnt$2243 == 1) {
        moonbit_free(_F1$890);
      }
      _f$891 = _field$1984;
      f$884 = _f$891;
      goto $join$883;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$892 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$887;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$1985 =
        _F2$892->$0;
      int32_t _cnt$2245 = Moonbit_object_header(_F2$892)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$893;
      if (_cnt$2245 > 1) {
        int32_t _new_cnt$2246;
        moonbit_incref(_field$1985);
        _new_cnt$2246 = _cnt$2245 - 1;
        Moonbit_object_header(_F2$892)->rc = _new_cnt$2246;
      } else if (_cnt$2245 == 1) {
        moonbit_free(_F2$892);
      }
      _f$893 = _field$1985;
      f$859 = _f$893;
      goto $join$858;
      break;
    }
  }
  goto $joinlet$2367;
  $join$885:;
  _closure$2368
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$2368)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2368->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$2368->$0 = name$836;
  _closure$2368->$1 = file_name$834;
  _closure$2368->$2 = handle_result$823;
  _tmp$1844 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2368;
  $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_catch_error(
    f$886, _tmp$1844, on_err$843
  );
  $joinlet$2367:;
  goto $joinlet$2366;
  $join$883:;
  moonbit_incref(name$836);
  _closure$2369
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$2369)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$2369->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$2369->$0 = f$884;
  _closure$2369->$1 = name$836;
  _tmp$1837
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$2369;
  _closure$2370
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$2370)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2370->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$2370->$0 = name$836;
  _closure$2370->$1 = file_name$834;
  _closure$2370->$2 = handle_result$823;
  _tmp$1838 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2370;
  $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_catch_error(
    _tmp$1837, _tmp$1838, on_err$843
  );
  $joinlet$2366:;
  goto $joinlet$2365;
  $join$858:;
  _closure$2371
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$2371)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$2371->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$2371->$0 = f$859;
  _closure$2371->$1 = on_err$843;
  _closure$2371->$2 = name$836;
  _closure$2371->$3 = file_name$834;
  _closure$2371->$4 = handle_result$823;
  _tmp$1822
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$2371;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$860, _tmp$1822);
  $joinlet$2365:;
  $joinlet$2363:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1845
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$1846 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$1845;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$1995 =
    _casted_env$1846->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$823 =
    _field$1995;
  moonbit_string_t _field$1994 = _casted_env$1846->$1;
  moonbit_string_t file_name$834 = _field$1994;
  moonbit_string_t _field$1993 = _casted_env$1846->$0;
  int32_t _cnt$2247 = Moonbit_object_header(_casted_env$1846)->rc;
  moonbit_string_t name$836;
  if (_cnt$2247 > 1) {
    int32_t _new_cnt$2248;
    moonbit_incref(handle_result$823);
    moonbit_incref(file_name$834);
    moonbit_incref(_field$1993);
    _new_cnt$2248 = _cnt$2247 - 1;
    Moonbit_object_header(_casted_env$1846)->rc = _new_cnt$2248;
  } else if (_cnt$2247 == 1) {
    moonbit_free(_casted_env$1846);
  }
  name$836 = _field$1993;
  $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$823,
      name$836,
      file_name$834,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1841
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$1842 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$1841;
  moonbit_string_t _field$1997 = _casted_env$1842->$1;
  moonbit_string_t name$836 = _field$1997;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$1996 =
    _casted_env$1842->$0;
  int32_t _cnt$2249 = Moonbit_object_header(_casted_env$1842)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$884;
  int32_t _tmp$1843;
  if (_cnt$2249 > 1) {
    int32_t _new_cnt$2250;
    moonbit_incref(name$836);
    moonbit_incref(_field$1996);
    _new_cnt$2250 = _cnt$2249 - 1;
    Moonbit_object_header(_casted_env$1842)->rc = _new_cnt$2250;
  } else if (_cnt$2249 == 1) {
    moonbit_free(_casted_env$1842);
  }
  f$884 = _field$1996;
  _tmp$1843
  = $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$836
  );
  return f$884->code(f$884, _tmp$1843);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1839
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$1840 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$1839;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2000 =
    _casted_env$1840->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$823 =
    _field$2000;
  moonbit_string_t _field$1999 = _casted_env$1840->$1;
  moonbit_string_t file_name$834 = _field$1999;
  moonbit_string_t _field$1998 = _casted_env$1840->$0;
  int32_t _cnt$2251 = Moonbit_object_header(_casted_env$1840)->rc;
  moonbit_string_t name$836;
  if (_cnt$2251 > 1) {
    int32_t _new_cnt$2252;
    moonbit_incref(handle_result$823);
    moonbit_incref(file_name$834);
    moonbit_incref(_field$1998);
    _new_cnt$2252 = _cnt$2251 - 1;
    Moonbit_object_header(_casted_env$1840)->rc = _new_cnt$2252;
  } else if (_cnt$2251 == 1) {
    moonbit_free(_casted_env$1840);
  }
  name$836 = _field$1998;
  $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$823,
      name$836,
      file_name$834,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1823,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$861,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$862
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$1824 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$1823;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2005 =
    _casted_env$1824->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$823 =
    _field$2005;
  moonbit_string_t _field$2004 = _casted_env$1824->$3;
  moonbit_string_t file_name$834 = _field$2004;
  moonbit_string_t _field$2003 = _casted_env$1824->$2;
  moonbit_string_t name$836 = _field$2003;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2002 = _casted_env$1824->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$843 = _field$2002;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2001 =
    _casted_env$1824->$0;
  int32_t _cnt$2253 = Moonbit_object_header(_casted_env$1824)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$859;
  int32_t _async_driver$863;
  int32_t _tmp$1828;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$2372;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$1829;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$2373;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$1830;
  if (_cnt$2253 > 1) {
    int32_t _new_cnt$2254;
    moonbit_incref(handle_result$823);
    moonbit_incref(file_name$834);
    moonbit_incref(name$836);
    moonbit_incref(on_err$843);
    moonbit_incref(_field$2001);
    _new_cnt$2254 = _cnt$2253 - 1;
    Moonbit_object_header(_casted_env$1824)->rc = _new_cnt$2254;
  } else if (_cnt$2253 == 1) {
    moonbit_free(_casted_env$1824);
  }
  f$859 = _field$2001;
  _async_driver$863 = 0;
  moonbit_incref(name$836);
  _tmp$1828
  = $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$836
  );
  moonbit_incref(_cont$861);
  _closure$2372
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$2372)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$2372->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$2372->$0 = _async_driver$863;
  _closure$2372->$1 = _cont$861;
  _closure$2372->$2 = name$836;
  _closure$2372->$3 = file_name$834;
  _closure$2372->$4 = handle_result$823;
  _tmp$1829 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$2372;
  _closure$2373
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$2373)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$2373->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$2373->$0 = _async_driver$863;
  _closure$2373->$1 = _err_cont$862;
  _closure$2373->$2 = _cont$861;
  _closure$2373->$3 = on_err$843;
  _tmp$1830 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2373;
  f$859->code(f$859, _tmp$1828, _tmp$1829, _tmp$1830);
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1834,
  int32_t _cont_param$881
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$1835 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$1834;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2010 =
    _casted_env$1835->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$823 =
    _field$2010;
  moonbit_string_t _field$2009 = _casted_env$1835->$3;
  moonbit_string_t file_name$834 = _field$2009;
  moonbit_string_t _field$2008 = _casted_env$1835->$2;
  moonbit_string_t name$836 = _field$2008;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2007 = _casted_env$1835->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$861 = _field$2007;
  int32_t _field$2006 = _casted_env$1835->$0;
  int32_t _cnt$2255 = Moonbit_object_header(_casted_env$1835)->rc;
  int32_t _async_driver$863;
  void* State_1$1836;
  if (_cnt$2255 > 1) {
    int32_t _new_cnt$2256;
    moonbit_incref(handle_result$823);
    moonbit_incref(file_name$834);
    moonbit_incref(name$836);
    moonbit_incref(_cont$861);
    _new_cnt$2256 = _cnt$2255 - 1;
    Moonbit_object_header(_casted_env$1835)->rc = _new_cnt$2256;
  } else if (_cnt$2255 == 1) {
    moonbit_free(_casted_env$1835);
  }
  _async_driver$863 = _field$2006;
  State_1$1836
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1
      )
    );
  Moonbit_object_header(State_1$1836)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1*)State_1$1836)->$0
  = _cont_param$881;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1*)State_1$1836)->$1
  = handle_result$823;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1*)State_1$1836)->$2
  = file_name$834;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1*)State_1$1836)->$3
  = name$836;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1*)State_1$1836)->$4
  = _cont$861;
  $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$863, State_1$1836
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1831,
  void* _cont_param$882
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$1832 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$1831;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2014 = _casted_env$1832->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$843 = _field$2014;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2013 = _casted_env$1832->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$861 = _field$2013;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2012 = _casted_env$1832->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$862 = _field$2012;
  int32_t _field$2011 = _casted_env$1832->$0;
  int32_t _cnt$2257 = Moonbit_object_header(_casted_env$1832)->rc;
  int32_t _async_driver$863;
  void* _try$154$1833;
  if (_cnt$2257 > 1) {
    int32_t _new_cnt$2258;
    moonbit_incref(on_err$843);
    moonbit_incref(_cont$861);
    moonbit_incref(_err_cont$862);
    _new_cnt$2258 = _cnt$2257 - 1;
    Moonbit_object_header(_casted_env$1832)->rc = _new_cnt$2258;
  } else if (_cnt$2257 == 1) {
    moonbit_free(_casted_env$1832);
  }
  _async_driver$863 = _field$2011;
  _try$154$1833
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154
      )
    );
  Moonbit_object_header(_try$154$1833)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154*)_try$154$1833)->$0
  = _cont_param$882;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154*)_try$154$1833)->$1
  = on_err$843;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154*)_try$154$1833)->$2
  = _cont$861;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154*)_try$154$1833)->$3
  = _err_cont$862;
  $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$863, _try$154$1833
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1825,
  void* _state$864
) {
  switch (Moonbit_object_tag(_state$864)) {
    case 0: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154* _$2a$try$154$865 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$$2a$try$154*)_state$864;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2018 = _$2a$try$154$865->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$866 = _field$2018;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2017 = _$2a$try$154$865->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$867 = _field$2017;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2016 = _$2a$try$154$865->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$868 = _field$2016;
      void* _field$2015 = _$2a$try$154$865->$0;
      int32_t _cnt$2259 = Moonbit_object_header(_$2a$try$154$865)->rc;
      void* _try_err$869;
      void* err$871;
      void* err$873;
      int32_t _tmp$1827;
      if (_cnt$2259 > 1) {
        int32_t _new_cnt$2260;
        moonbit_incref(_err_cont$866);
        moonbit_incref(_cont$867);
        moonbit_incref(on_err$868);
        moonbit_incref(_field$2015);
        _new_cnt$2260 = _cnt$2259 - 1;
        Moonbit_object_header(_$2a$try$154$865)->rc = _new_cnt$2260;
      } else if (_cnt$2259 == 1) {
        moonbit_free(_$2a$try$154$865);
      }
      _try_err$869 = _field$2015;
      if (
        $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$868);
        moonbit_decref(_cont$867);
        err$873 = _try_err$869;
        goto $join$872;
      } else {
        moonbit_decref(_err_cont$866);
        err$871 = _try_err$869;
        goto $join$870;
      }
      goto $joinlet$2375;
      $join$872:;
      return _err_cont$866->code(_err_cont$866, err$873);
      $joinlet$2375:;
      goto $joinlet$2374;
      $join$870:;
      _tmp$1827 = on_err$868->code(on_err$868, err$871);
      _cont$867->code(_cont$867, _tmp$1827);
      $joinlet$2374:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1* _State_1$874 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$170$on_err$68$$2a$arm$162$lambda$188$State$State_1*)_state$864;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2022 = _State_1$874->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$875 = _field$2022;
      moonbit_string_t _field$2021 = _State_1$874->$3;
      moonbit_string_t name$876 = _field$2021;
      moonbit_string_t _field$2020 = _State_1$874->$2;
      moonbit_string_t file_name$877 = _field$2020;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2019 =
        _State_1$874->$1;
      int32_t _cnt$2261 = Moonbit_object_header(_State_1$874)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$878;
      int32_t _tmp$1826;
      if (_cnt$2261 > 1) {
        int32_t _new_cnt$2262;
        moonbit_incref(_cont$875);
        moonbit_incref(name$876);
        moonbit_incref(file_name$877);
        moonbit_incref(_field$2019);
        _new_cnt$2262 = _cnt$2261 - 1;
        Moonbit_object_header(_State_1$874)->rc = _new_cnt$2262;
      } else if (_cnt$2261 == 1) {
        moonbit_free(_State_1$874);
      }
      handle_result$878 = _field$2019;
      _tmp$1826
      = handle_result$878->code(
        handle_result$878,
          name$876,
          file_name$877,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$875->code(_cont$875, _tmp$1826);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1820,
  void* err$844
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$1821 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$1820;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2029 =
    _casted_env$1821->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$823 =
    _field$2029;
  moonbit_string_t _field$2028 = _casted_env$1821->$1;
  moonbit_string_t file_name$834 = _field$2028;
  moonbit_string_t _field$2027 = _casted_env$1821->$0;
  int32_t _cnt$2263 = Moonbit_object_header(_casted_env$1821)->rc;
  moonbit_string_t name$836;
  void* e$846;
  moonbit_string_t e$849;
  moonbit_string_t message$847;
  if (_cnt$2263 > 1) {
    int32_t _new_cnt$2264;
    moonbit_incref(handle_result$823);
    moonbit_incref(file_name$834);
    moonbit_incref(_field$2027);
    _new_cnt$2264 = _cnt$2263 - 1;
    Moonbit_object_header(_casted_env$1821)->rc = _new_cnt$2264;
  } else if (_cnt$2263 == 1) {
    moonbit_free(_casted_env$1821);
  }
  name$836 = _field$2027;
  switch (Moonbit_object_tag(err$844)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$850 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$844;
      moonbit_string_t _field$2023 = _Failure$850->$0;
      int32_t _cnt$2265 = Moonbit_object_header(_Failure$850)->rc;
      moonbit_string_t _e$851;
      if (_cnt$2265 > 1) {
        int32_t _new_cnt$2266;
        moonbit_incref(_field$2023);
        _new_cnt$2266 = _cnt$2265 - 1;
        Moonbit_object_header(_Failure$850)->rc = _new_cnt$2266;
      } else if (_cnt$2265 == 1) {
        moonbit_free(_Failure$850);
      }
      _e$851 = _field$2023;
      e$849 = _e$851;
      goto $join$848;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$852 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$844;
      moonbit_string_t _field$2024 = _InspectError$852->$0;
      int32_t _cnt$2267 = Moonbit_object_header(_InspectError$852)->rc;
      moonbit_string_t _e$853;
      if (_cnt$2267 > 1) {
        int32_t _new_cnt$2268;
        moonbit_incref(_field$2024);
        _new_cnt$2268 = _cnt$2267 - 1;
        Moonbit_object_header(_InspectError$852)->rc = _new_cnt$2268;
      } else if (_cnt$2267 == 1) {
        moonbit_free(_InspectError$852);
      }
      _e$853 = _field$2024;
      e$849 = _e$853;
      goto $join$848;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$854 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$844;
      moonbit_string_t _field$2025 = _SnapshotError$854->$0;
      int32_t _cnt$2269 = Moonbit_object_header(_SnapshotError$854)->rc;
      moonbit_string_t _e$855;
      if (_cnt$2269 > 1) {
        int32_t _new_cnt$2270;
        moonbit_incref(_field$2025);
        _new_cnt$2270 = _cnt$2269 - 1;
        Moonbit_object_header(_SnapshotError$854)->rc = _new_cnt$2270;
      } else if (_cnt$2269 == 1) {
        moonbit_free(_SnapshotError$854);
      }
      _e$855 = _field$2025;
      e$849 = _e$855;
      goto $join$848;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$856 =
        (struct $Error$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$844;
      moonbit_string_t _field$2026 =
        _MoonBitTestDriverInternalJsError$856->$0;
      int32_t _cnt$2271 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$856)->rc;
      moonbit_string_t _e$857;
      if (_cnt$2271 > 1) {
        int32_t _new_cnt$2272;
        moonbit_incref(_field$2026);
        _new_cnt$2272 = _cnt$2271 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$856)->rc
        = _new_cnt$2272;
      } else if (_cnt$2271 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$856);
      }
      _e$857 = _field$2026;
      e$849 = _e$857;
      goto $join$848;
      break;
    }
    default: {
      e$846 = err$844;
      goto $join$845;
      break;
    }
  }
  goto $joinlet$2377;
  $join$848:;
  $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$823, name$836, file_name$834, e$849, 0
  );
  $joinlet$2377:;
  goto $joinlet$2376;
  $join$845:;
  message$847 = $Error$to_string(e$846);
  $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$823, name$836, file_name$834, message$847, 0
  );
  $joinlet$2376:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1806,
  moonbit_string_t attr$837
) {
  int32_t _tmp$1808;
  int64_t _tmp$1807;
  moonbit_decref(_env$1806);
  _tmp$1808 = Moonbit_array_length(attr$837);
  _tmp$1807 = (int64_t)_tmp$1808;
  moonbit_incref(attr$837);
  if ($String$$char_length_ge$inner(attr$837, 5, 0, _tmp$1807)) {
    int32_t _tmp$1813 = attr$837[0];
    int32_t _x$838 = _tmp$1813;
    if (_x$838 == 112) {
      int32_t _tmp$1812 = attr$837[1];
      int32_t _x$839 = _tmp$1812;
      if (_x$839 == 97) {
        int32_t _tmp$1811 = attr$837[2];
        int32_t _x$840 = _tmp$1811;
        if (_x$840 == 110) {
          int32_t _tmp$1810 = attr$837[3];
          int32_t _x$841 = _tmp$1810;
          if (_x$841 == 105) {
            int32_t _tmp$2030 = attr$837[4];
            int32_t _tmp$1809;
            int32_t _x$842;
            moonbit_decref(attr$837);
            _tmp$1809 = _tmp$2030;
            _x$842 = _tmp$1809;
            return _x$842 == 99 || 0;
          } else {
            moonbit_decref(attr$837);
            return 0;
          }
        } else {
          moonbit_decref(attr$837);
          return 0;
        }
      } else {
        moonbit_decref(attr$837);
        return 0;
      }
    } else {
      moonbit_decref(attr$837);
      return 0;
    }
  } else {
    moonbit_decref(attr$837);
    return 0;
  }
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1790,
  moonbit_string_t test_name$824,
  moonbit_string_t file_name$825,
  moonbit_string_t message$826,
  int32_t skipped$827
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$1791 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$1790;
  int32_t _field$2031 = _casted_env$1791->$0;
  int32_t index$822;
  int32_t _if_result$2378;
  moonbit_string_t file_name$828;
  moonbit_string_t test_name$829;
  moonbit_string_t message$830;
  moonbit_string_t _tmp$1803;
  moonbit_string_t _tmp$1802;
  moonbit_string_t _tmp$1800;
  moonbit_string_t _tmp$1801;
  moonbit_string_t _tmp$1799;
  moonbit_string_t _tmp$1797;
  moonbit_string_t _tmp$1798;
  moonbit_string_t _tmp$1796;
  moonbit_string_t _tmp$1794;
  moonbit_string_t _tmp$1795;
  moonbit_string_t _tmp$1793;
  moonbit_string_t _tmp$1792;
  moonbit_decref(_casted_env$1791);
  index$822 = _field$2031;
  if (!skipped$827) {
    _if_result$2378 = 1;
  } else {
    _if_result$2378 = 0;
  }
  if (_if_result$2378) {
    
  }
  file_name$828 = $String$$escape(file_name$825);
  test_name$829 = $String$$escape(test_name$824);
  message$830 = $String$$escape(message$826);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$1803
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$828
  );
  _tmp$1802
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$1803
  );
  _tmp$1800
  = moonbit_add_string(
    _tmp$1802, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$1801
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$822
  );
  _tmp$1799 = moonbit_add_string(_tmp$1800, _tmp$1801);
  _tmp$1797
  = moonbit_add_string(
    _tmp$1799, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$1798
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$829
  );
  _tmp$1796 = moonbit_add_string(_tmp$1797, _tmp$1798);
  _tmp$1794
  = moonbit_add_string(
    _tmp$1796, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$1795 = $$moonbitlang$core$builtin$Show$$String$$to_string(message$830);
  _tmp$1793 = moonbit_add_string(_tmp$1794, _tmp$1795);
  _tmp$1792
  = moonbit_add_string(
    _tmp$1793, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$1792);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$819
) {
  moonbit_decref(_discard_$819);
  return 42;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$817,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$818,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$815
) {
  void* _try_err$813;
  struct moonbit_result_0 _tmp$2380 = f$817->code(f$817);
  void* err$814;
  if (_tmp$2380.tag) {
    int32_t const _ok$1788 = _tmp$2380.data.ok;
    moonbit_decref(on_err$815);
  } else {
    void* const _err$1789 = _tmp$2380.data.err;
    moonbit_decref(on_ok$818);
    _try_err$813 = _err$1789;
    goto $join$812;
  }
  on_ok$818->code(on_ok$818);
  goto $joinlet$2379;
  $join$812:;
  err$814 = _try_err$813;
  on_err$815->code(on_err$815, err$814);
  $joinlet$2379:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$778,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$791,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$804,
  moonbit_string_t file_filter$775,
  int32_t index_filter$776
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$772;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$773;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$777;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2037;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1779;
  void* F0$1776;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2036;
  int32_t _cnt$2273;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1778;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1777;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$774;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$787;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$788;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$790;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2035;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1783;
  void* F1$1780;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2034;
  int32_t _cnt$2276;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1782;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1781;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$789;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$800;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$801;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$803;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2033;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1787;
  void* F2$1784;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2032;
  int32_t _cnt$2279;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1786;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1785;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$802;
  moonbit_incref(file_filter$775);
  _bind$777
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$778, file_filter$775
  );
  if (_bind$777 == 0) {
    if (_bind$777) {
      moonbit_decref(_bind$777);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$779 =
      _bind$777;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$780 =
      _Some$779;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$782;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$783;
    moonbit_incref(_index_func_map$780);
    _bind$783
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$780, index_filter$776
    );
    if (_bind$783 == 0) {
      if (_bind$783) {
        moonbit_decref(_bind$783);
      }
      moonbit_decref(_index_func_map$780);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$784;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$785;
      moonbit_decref(async_tests$804);
      moonbit_decref(with_args_tests$791);
      _Some$784 = _bind$783;
      _func_attrs_tuple$785 = _Some$784;
      func_attrs_tuple$782 = _func_attrs_tuple$785;
      goto $join$781;
    }
    goto $joinlet$2382;
    $join$781:;
    index_func_map$772 = _index_func_map$780;
    func_attrs_tuple$773 = func_attrs_tuple$782;
    goto $join$771;
    $joinlet$2382:;
  }
  goto $joinlet$2381;
  $join$771:;
  moonbit_decref(index_func_map$772);
  _field$2037 = func_attrs_tuple$773->$0;
  _tmp$1779 = _field$2037;
  moonbit_incref(_tmp$1779);
  F0$1776
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$1776)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$1776)->$0 = _tmp$1779;
  _field$2036 = func_attrs_tuple$773->$1;
  _cnt$2273 = Moonbit_object_header(func_attrs_tuple$773)->rc;
  if (_cnt$2273 > 1) {
    int32_t _new_cnt$2275;
    moonbit_incref(_field$2036);
    _new_cnt$2275 = _cnt$2273 - 1;
    Moonbit_object_header(func_attrs_tuple$773)->rc = _new_cnt$2275;
  } else if (_cnt$2273 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2274 =
      func_attrs_tuple$773->$0;
    moonbit_decref(_field$2274);
    moonbit_free(func_attrs_tuple$773);
  }
  _tmp$1778 = _field$2036;
  _tmp$1777
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1777)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1777->$0 = file_filter$775;
  _tmp$1777->$1 = index_filter$776;
  _tmp$1777->$2 = _tmp$1778;
  k$774
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$774)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$774->$0 = F0$1776;
  k$774->$1 = _tmp$1777;
  return k$774;
  $joinlet$2381:;
  moonbit_incref(file_filter$775);
  _bind$790
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$791, file_filter$775
  );
  if (_bind$790 == 0) {
    if (_bind$790) {
      moonbit_decref(_bind$790);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$792 =
      _bind$790;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$793 =
      _Some$792;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$795;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$796;
    moonbit_incref(_index_func_map$793);
    _bind$796
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$793, index_filter$776
    );
    if (_bind$796 == 0) {
      if (_bind$796) {
        moonbit_decref(_bind$796);
      }
      moonbit_decref(_index_func_map$793);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$797;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$798;
      moonbit_decref(async_tests$804);
      _Some$797 = _bind$796;
      _func_attrs_tuple$798 = _Some$797;
      func_attrs_tuple$795 = _func_attrs_tuple$798;
      goto $join$794;
    }
    goto $joinlet$2384;
    $join$794:;
    index_func_map$787 = _index_func_map$793;
    func_attrs_tuple$788 = func_attrs_tuple$795;
    goto $join$786;
    $joinlet$2384:;
  }
  goto $joinlet$2383;
  $join$786:;
  moonbit_decref(index_func_map$787);
  _field$2035 = func_attrs_tuple$788->$0;
  _tmp$1783 = _field$2035;
  moonbit_incref(_tmp$1783);
  F1$1780
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$1780)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$1780)->$0 = _tmp$1783;
  _field$2034 = func_attrs_tuple$788->$1;
  _cnt$2276 = Moonbit_object_header(func_attrs_tuple$788)->rc;
  if (_cnt$2276 > 1) {
    int32_t _new_cnt$2278;
    moonbit_incref(_field$2034);
    _new_cnt$2278 = _cnt$2276 - 1;
    Moonbit_object_header(func_attrs_tuple$788)->rc = _new_cnt$2278;
  } else if (_cnt$2276 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2277 =
      func_attrs_tuple$788->$0;
    moonbit_decref(_field$2277);
    moonbit_free(func_attrs_tuple$788);
  }
  _tmp$1782 = _field$2034;
  _tmp$1781
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1781)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1781->$0 = file_filter$775;
  _tmp$1781->$1 = index_filter$776;
  _tmp$1781->$2 = _tmp$1782;
  k$789
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$789)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$789->$0 = F1$1780;
  k$789->$1 = _tmp$1781;
  return k$789;
  $joinlet$2383:;
  moonbit_incref(file_filter$775);
  _bind$803
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$804, file_filter$775
  );
  if (_bind$803 == 0) {
    if (_bind$803) {
      moonbit_decref(_bind$803);
    }
    moonbit_decref(file_filter$775);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$805 =
      _bind$803;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$806 =
      _Some$805;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$808;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$809;
    moonbit_incref(_index_func_map$806);
    _bind$809
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$806, index_filter$776
    );
    if (_bind$809 == 0) {
      if (_bind$809) {
        moonbit_decref(_bind$809);
      }
      moonbit_decref(_index_func_map$806);
      moonbit_decref(file_filter$775);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$810 =
        _bind$809;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$811 =
        _Some$810;
      func_attrs_tuple$808 = _func_attrs_tuple$811;
      goto $join$807;
    }
    goto $joinlet$2386;
    $join$807:;
    index_func_map$800 = _index_func_map$806;
    func_attrs_tuple$801 = func_attrs_tuple$808;
    goto $join$799;
    $joinlet$2386:;
  }
  goto $joinlet$2385;
  $join$799:;
  moonbit_decref(index_func_map$800);
  _field$2033 = func_attrs_tuple$801->$0;
  _tmp$1787 = _field$2033;
  moonbit_incref(_tmp$1787);
  F2$1784
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$1784)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$1784)->$0 = _tmp$1787;
  _field$2032 = func_attrs_tuple$801->$1;
  _cnt$2279 = Moonbit_object_header(func_attrs_tuple$801)->rc;
  if (_cnt$2279 > 1) {
    int32_t _new_cnt$2281;
    moonbit_incref(_field$2032);
    _new_cnt$2281 = _cnt$2279 - 1;
    Moonbit_object_header(func_attrs_tuple$801)->rc = _new_cnt$2281;
  } else if (_cnt$2279 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2280 =
      func_attrs_tuple$801->$0;
    moonbit_decref(_field$2280);
    moonbit_free(func_attrs_tuple$801);
  }
  _tmp$1786 = _field$2032;
  _tmp$1785
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1785)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1785->$0 = file_filter$775;
  _tmp$1785->$1 = index_filter$776;
  _tmp$1785->$2 = _tmp$1786;
  k$802
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$802)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$802->$0 = F2$1784;
  k$802->$1 = _tmp$1785;
  return k$802;
  $joinlet$2385:;
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$767
) {
  int32_t _field$2038 = self$767->$1;
  int32_t len$1775;
  moonbit_decref(self$767);
  len$1775 = _field$2038;
  return len$1775 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$765,
  struct $$moonbitlang$core$builtin$Logger logger$766
) {
  moonbit_string_t _tmp$1774 = self$765;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$1773 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$1774);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$1773, logger$766
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$751,
  struct $$moonbitlang$core$builtin$Logger logger$764
) {
  struct $StringView _field$2047 =
    (struct $StringView){self$751->$0_1, self$751->$0_2, self$751->$0_0};
  struct $StringView pkg$750 = _field$2047;
  int32_t _tmp$1772 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$1771;
  int64_t _bind$752;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$753;
  struct $StringView _field$2046;
  struct $StringView _module_name$760;
  void* _field$2045;
  int32_t _cnt$2282;
  void* _package_name$761;
  struct $StringView _field$2043;
  struct $StringView filename$1754;
  struct $StringView _field$2042;
  struct $StringView start_line$1755;
  struct $StringView _field$2041;
  struct $StringView start_column$1756;
  struct $StringView _field$2040;
  struct $StringView end_line$1757;
  struct $StringView _field$2039;
  int32_t _cnt$2286;
  struct $StringView end_column$1758;
  struct $$moonbitlang$core$builtin$Logger _bind$1753;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$1771
  = (struct $StringView){
    0, _tmp$1772, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$750.$0);
  moonbit_incref(pkg$750.$0);
  _bind$752 = $StringView$$find(pkg$750, _tmp$1771);
  if (_bind$752 == 4294967296ll) {
    void* None$1759 =
      (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
    _bind$753
    = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
        sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
      );
    Moonbit_object_header(_bind$753)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
      )
      >> 2,
        2,
        0
    );
    _bind$753->$0_0 = pkg$750.$0;
    _bind$753->$0_1 = pkg$750.$1;
    _bind$753->$0_2 = pkg$750.$2;
    _bind$753->$1 = None$1759;
  } else {
    int64_t _Some$754 = _bind$752;
    int32_t _first_slash$755 = (int32_t)_Some$754;
    int32_t _tmp$1770 = _first_slash$755 + 1;
    struct $StringView _tmp$1767;
    int32_t _tmp$1769;
    struct $StringView _tmp$1768;
    int64_t _bind$756;
    moonbit_incref(pkg$750.$0);
    _tmp$1767 = $StringView$$view$inner(pkg$750, _tmp$1770, 4294967296ll);
    _tmp$1769
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$1768
    = (struct $StringView){
      0, _tmp$1769, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$756 = $StringView$$find(_tmp$1767, _tmp$1768);
    if (_bind$756 == 4294967296ll) {
      void* None$1760 =
        (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$753
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$753)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$753->$0_0 = pkg$750.$0;
      _bind$753->$0_1 = pkg$750.$1;
      _bind$753->$0_2 = pkg$750.$2;
      _bind$753->$1 = None$1760;
    } else {
      int64_t _Some$757 = _bind$756;
      int32_t _second_slash$758 = (int32_t)_Some$757;
      int32_t _tmp$1766 = _first_slash$755 + 1;
      int32_t module_name_end$759 = _tmp$1766 + _second_slash$758;
      int64_t _tmp$1765 = (int64_t)module_name_end$759;
      struct $StringView _tmp$1761;
      int32_t _tmp$1764;
      struct $StringView _tmp$1763;
      void* Some$1762;
      moonbit_incref(pkg$750.$0);
      _tmp$1761 = $StringView$$view$inner(pkg$750, 0, _tmp$1765);
      _tmp$1764 = module_name_end$759 + 1;
      _tmp$1763 = $StringView$$view$inner(pkg$750, _tmp$1764, 4294967296ll);
      Some$1762
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$1762)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$1762)->$0_0
      = _tmp$1763.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1762)->$0_1
      = _tmp$1763.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1762)->$0_2
      = _tmp$1763.$2;
      _bind$753
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$753)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$753->$0_0 = _tmp$1761.$0;
      _bind$753->$0_1 = _tmp$1761.$1;
      _bind$753->$0_2 = _tmp$1761.$2;
      _bind$753->$1 = Some$1762;
    }
  }
  _field$2046
  = (struct $StringView){
    _bind$753->$0_1, _bind$753->$0_2, _bind$753->$0_0
  };
  _module_name$760 = _field$2046;
  _field$2045 = _bind$753->$1;
  _cnt$2282 = Moonbit_object_header(_bind$753)->rc;
  if (_cnt$2282 > 1) {
    int32_t _new_cnt$2283;
    moonbit_incref(_field$2045);
    moonbit_incref(_module_name$760.$0);
    _new_cnt$2283 = _cnt$2282 - 1;
    Moonbit_object_header(_bind$753)->rc = _new_cnt$2283;
  } else if (_cnt$2282 == 1) {
    moonbit_free(_bind$753);
  }
  _package_name$761 = _field$2045;
  switch (Moonbit_object_tag(_package_name$761)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$762 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$761;
      struct $StringView _field$2044 =
        (struct $StringView){
          _Some$762->$0_1, _Some$762->$0_2, _Some$762->$0_0
        };
      int32_t _cnt$2284 = Moonbit_object_header(_Some$762)->rc;
      struct $StringView _pkg_name$763;
      struct $$moonbitlang$core$builtin$Logger _bind$1752;
      if (_cnt$2284 > 1) {
        int32_t _new_cnt$2285;
        moonbit_incref(_field$2044.$0);
        _new_cnt$2285 = _cnt$2284 - 1;
        Moonbit_object_header(_Some$762)->rc = _new_cnt$2285;
      } else if (_cnt$2284 == 1) {
        moonbit_free(_Some$762);
      }
      _pkg_name$763 = _field$2044;
      if (logger$764.$1) {
        moonbit_incref(logger$764.$1);
      }
      logger$764.$0->$method_2(logger$764.$1, _pkg_name$763);
      _bind$1752 = logger$764;
      if (_bind$1752.$1) {
        moonbit_incref(_bind$1752.$1);
      }
      _bind$1752.$0->$method_3(_bind$1752.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$761);
      break;
    }
  }
  _field$2043
  = (struct $StringView){
    self$751->$1_1, self$751->$1_2, self$751->$1_0
  };
  filename$1754 = _field$2043;
  moonbit_incref(filename$1754.$0);
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_2(logger$764.$1, filename$1754);
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_3(logger$764.$1, 58);
  _field$2042
  = (struct $StringView){
    self$751->$2_1, self$751->$2_2, self$751->$2_0
  };
  start_line$1755 = _field$2042;
  moonbit_incref(start_line$1755.$0);
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_2(logger$764.$1, start_line$1755);
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_3(logger$764.$1, 58);
  _field$2041
  = (struct $StringView){
    self$751->$3_1, self$751->$3_2, self$751->$3_0
  };
  start_column$1756 = _field$2041;
  moonbit_incref(start_column$1756.$0);
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_2(logger$764.$1, start_column$1756);
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_3(logger$764.$1, 45);
  _field$2040
  = (struct $StringView){
    self$751->$4_1, self$751->$4_2, self$751->$4_0
  };
  end_line$1757 = _field$2040;
  moonbit_incref(end_line$1757.$0);
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_2(logger$764.$1, end_line$1757);
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_3(logger$764.$1, 58);
  _field$2039
  = (struct $StringView){
    self$751->$5_1, self$751->$5_2, self$751->$5_0
  };
  _cnt$2286 = Moonbit_object_header(self$751)->rc;
  if (_cnt$2286 > 1) {
    int32_t _new_cnt$2292;
    moonbit_incref(_field$2039.$0);
    _new_cnt$2292 = _cnt$2286 - 1;
    Moonbit_object_header(self$751)->rc = _new_cnt$2292;
  } else if (_cnt$2286 == 1) {
    struct $StringView _field$2291 =
      (struct $StringView){self$751->$4_1, self$751->$4_2, self$751->$4_0};
    struct $StringView _field$2290;
    struct $StringView _field$2289;
    struct $StringView _field$2288;
    struct $StringView _field$2287;
    moonbit_decref(_field$2291.$0);
    _field$2290
    = (struct $StringView){
      self$751->$3_1, self$751->$3_2, self$751->$3_0
    };
    moonbit_decref(_field$2290.$0);
    _field$2289
    = (struct $StringView){
      self$751->$2_1, self$751->$2_2, self$751->$2_0
    };
    moonbit_decref(_field$2289.$0);
    _field$2288
    = (struct $StringView){
      self$751->$1_1, self$751->$1_2, self$751->$1_0
    };
    moonbit_decref(_field$2288.$0);
    _field$2287
    = (struct $StringView){
      self$751->$0_1, self$751->$0_2, self$751->$0_0
    };
    moonbit_decref(_field$2287.$0);
    moonbit_free(self$751);
  }
  end_column$1758 = _field$2039;
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_2(logger$764.$1, end_column$1758);
  if (logger$764.$1) {
    moonbit_incref(logger$764.$1);
  }
  logger$764.$0->$method_3(logger$764.$1, 64);
  _bind$1753 = logger$764;
  _bind$1753.$0->$method_2(_bind$1753.$1, _module_name$760);
  return 0;
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$749) {
  moonbit_string_t _tmp$1751 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$749);
  moonbit_println(_tmp$1751);
  moonbit_decref(_tmp$1751);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$748,
  struct $$moonbitlang$core$builtin$Hasher* hasher$747
) {
  $$moonbitlang$core$builtin$Hasher$$combine_int(hasher$747, self$748);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$746,
  struct $$moonbitlang$core$builtin$Hasher* hasher$745
) {
  $$moonbitlang$core$builtin$Hasher$$combine_string(hasher$745, self$746);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$743,
  moonbit_string_t value$741
) {
  int32_t _end2448$740 = Moonbit_array_length(value$741);
  int32_t i$742 = 0;
  while (1) {
    if (i$742 < _end2448$740) {
      int32_t _tmp$1749 = value$741[i$742];
      uint32_t _tmp$1748 = *(uint32_t*)&_tmp$1749;
      int32_t _tmp$1750;
      moonbit_incref(self$743);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$743, _tmp$1748);
      _tmp$1750 = i$742 + 1;
      i$742 = _tmp$1750;
      continue;
    } else {
      moonbit_decref(self$743);
      moonbit_decref(value$741);
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$737,
  struct $$3c$String$3e$$3d$$3e$Int* f$739
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$2388 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1745;
  int32_t _tmp$1744;
  Moonbit_object_header(_closure$2388)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2388->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$2388->$0 = f$739;
  _tmp$1745 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$2388;
  _tmp$1744 = $$moonbitlang$core$builtin$Iter$$run$0(self$737, _tmp$1745);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$1744, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1746,
  moonbit_string_t k$738
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$1747 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$1746;
  struct $$3c$String$3e$$3d$$3e$Int* _field$2048 = _casted_env$1747->$0;
  int32_t _cnt$2293 = Moonbit_object_header(_casted_env$1747)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$739;
  if (_cnt$2293 > 1) {
    int32_t _new_cnt$2294;
    moonbit_incref(_field$2048);
    _new_cnt$2294 = _cnt$2293 - 1;
    Moonbit_object_header(_casted_env$1747)->rc = _new_cnt$2294;
  } else if (_cnt$2293 == 1) {
    moonbit_free(_casted_env$1747);
  }
  f$739 = _field$2048;
  if (f$739->code(f$739, k$738)) {
    return 0;
  } else {
    return 1;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$735,
  int32_t idx$736
) {
  moonbit_string_t* _tmp$1743 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$735);
  moonbit_string_t _tmp$2049;
  if (idx$736 < 0 || idx$736 >= Moonbit_array_length(_tmp$1743)) {
    moonbit_panic();
  }
  _tmp$2049 = (moonbit_string_t)_tmp$1743[idx$736];
  moonbit_incref(_tmp$2049);
  moonbit_decref(_tmp$1743);
  return _tmp$2049;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$733,
  int32_t idx$734
) {
  struct $$3c$String$2a$Int$3e$** _tmp$1742 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$733);
  struct $$3c$String$2a$Int$3e$* _tmp$2050;
  if (idx$734 < 0 || idx$734 >= Moonbit_array_length(_tmp$1742)) {
    moonbit_panic();
  }
  _tmp$2050 = (struct $$3c$String$2a$Int$3e$*)_tmp$1742[idx$734];
  if (_tmp$2050) {
    moonbit_incref(_tmp$2050);
  }
  moonbit_decref(_tmp$1742);
  return _tmp$2050;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$729,
  int32_t key$725
) {
  int32_t hash$724 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$725);
  int32_t capacity_mask$1741 = self$729->$3;
  int32_t _tmp$1740 = hash$724 & capacity_mask$1741;
  int32_t i$726 = 0;
  int32_t idx$727 = _tmp$1740;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2054 =
      self$729->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1739 =
      _field$2054;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2053;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$728;
    if (idx$727 < 0 || idx$727 >= Moonbit_array_length(entries$1739)) {
      moonbit_panic();
    }
    _tmp$2053
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1739[
        idx$727
      ];
    _bind$728 = _tmp$2053;
    if (_bind$728 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1728;
      if (_bind$728) {
        moonbit_incref(_bind$728);
      }
      moonbit_decref(self$729);
      if (_bind$728) {
        moonbit_decref(_bind$728);
      }
      _tmp$1728 = 0;
      return _tmp$1728;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$730 =
        _bind$728;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$731 =
        _Some$730;
      int32_t hash$1730 = _entry$731->$3;
      int32_t _if_result$2390;
      int32_t _field$2051;
      int32_t psl$1733;
      int32_t _tmp$1735;
      int32_t _tmp$1737;
      int32_t capacity_mask$1738;
      int32_t _tmp$1736;
      if (hash$1730 == hash$724) {
        int32_t key$1729 = _entry$731->$4;
        _if_result$2390 = key$1729 == key$725;
      } else {
        _if_result$2390 = 0;
      }
      if (_if_result$2390) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2052;
        int32_t _cnt$2295;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1732;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1731;
        moonbit_incref(_entry$731);
        moonbit_decref(self$729);
        _field$2052 = _entry$731->$5;
        _cnt$2295 = Moonbit_object_header(_entry$731)->rc;
        if (_cnt$2295 > 1) {
          int32_t _new_cnt$2297;
          moonbit_incref(_field$2052);
          _new_cnt$2297 = _cnt$2295 - 1;
          Moonbit_object_header(_entry$731)->rc = _new_cnt$2297;
        } else if (_cnt$2295 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2296 =
            _entry$731->$1;
          if (_field$2296) {
            moonbit_decref(_field$2296);
          }
          moonbit_free(_entry$731);
        }
        value$1732 = _field$2052;
        _tmp$1731 = value$1732;
        return _tmp$1731;
      } else {
        moonbit_incref(_entry$731);
      }
      _field$2051 = _entry$731->$2;
      moonbit_decref(_entry$731);
      psl$1733 = _field$2051;
      if (i$726 > psl$1733) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1734;
        moonbit_decref(self$729);
        _tmp$1734 = 0;
        return _tmp$1734;
      }
      _tmp$1735 = i$726 + 1;
      _tmp$1737 = idx$727 + 1;
      capacity_mask$1738 = self$729->$3;
      _tmp$1736 = _tmp$1737 & capacity_mask$1738;
      i$726 = _tmp$1735;
      idx$727 = _tmp$1736;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$720,
  moonbit_string_t key$716
) {
  int32_t hash$715;
  int32_t capacity_mask$1727;
  int32_t _tmp$1726;
  int32_t i$717;
  int32_t idx$718;
  moonbit_incref(key$716);
  hash$715 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$716);
  capacity_mask$1727 = self$720->$3;
  _tmp$1726 = hash$715 & capacity_mask$1727;
  i$717 = 0;
  idx$718 = _tmp$1726;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2060 =
      self$720->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1725 =
      _field$2060;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2059;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$719;
    if (idx$718 < 0 || idx$718 >= Moonbit_array_length(entries$1725)) {
      moonbit_panic();
    }
    _tmp$2059
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1725[
        idx$718
      ];
    _bind$719 = _tmp$2059;
    if (_bind$719 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1714;
      if (_bind$719) {
        moonbit_incref(_bind$719);
      }
      moonbit_decref(self$720);
      if (_bind$719) {
        moonbit_decref(_bind$719);
      }
      moonbit_decref(key$716);
      _tmp$1714 = 0;
      return _tmp$1714;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$721 =
        _bind$719;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$722 =
        _Some$721;
      int32_t hash$1716 = _entry$722->$3;
      int32_t _if_result$2392;
      int32_t _field$2055;
      int32_t psl$1719;
      int32_t _tmp$1721;
      int32_t _tmp$1723;
      int32_t capacity_mask$1724;
      int32_t _tmp$1722;
      if (hash$1716 == hash$715) {
        moonbit_string_t _field$2058 = _entry$722->$4;
        moonbit_string_t key$1715 = _field$2058;
        int32_t _tmp$2057 = moonbit_val_array_equal(key$1715, key$716);
        _if_result$2392 = _tmp$2057;
      } else {
        _if_result$2392 = 0;
      }
      if (_if_result$2392) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2056;
        int32_t _cnt$2298;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1718;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1717;
        moonbit_incref(_entry$722);
        moonbit_decref(self$720);
        moonbit_decref(key$716);
        _field$2056 = _entry$722->$5;
        _cnt$2298 = Moonbit_object_header(_entry$722)->rc;
        if (_cnt$2298 > 1) {
          int32_t _new_cnt$2301;
          moonbit_incref(_field$2056);
          _new_cnt$2301 = _cnt$2298 - 1;
          Moonbit_object_header(_entry$722)->rc = _new_cnt$2301;
        } else if (_cnt$2298 == 1) {
          moonbit_string_t _field$2300 = _entry$722->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2299;
          moonbit_decref(_field$2300);
          _field$2299 = _entry$722->$1;
          if (_field$2299) {
            moonbit_decref(_field$2299);
          }
          moonbit_free(_entry$722);
        }
        value$1718 = _field$2056;
        _tmp$1717 = value$1718;
        return _tmp$1717;
      } else {
        moonbit_incref(_entry$722);
      }
      _field$2055 = _entry$722->$2;
      moonbit_decref(_entry$722);
      psl$1719 = _field$2055;
      if (i$717 > psl$1719) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1720;
        moonbit_decref(self$720);
        moonbit_decref(key$716);
        _tmp$1720 = 0;
        return _tmp$1720;
      }
      _tmp$1721 = i$717 + 1;
      _tmp$1723 = idx$718 + 1;
      capacity_mask$1724 = self$720->$3;
      _tmp$1722 = _tmp$1723 & capacity_mask$1724;
      i$717 = _tmp$1721;
      idx$718 = _tmp$1722;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$711,
  int32_t key$707
) {
  int32_t hash$706 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$707);
  int32_t capacity_mask$1713 = self$711->$3;
  int32_t _tmp$1712 = hash$706 & capacity_mask$1713;
  int32_t i$708 = 0;
  int32_t idx$709 = _tmp$1712;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2064 =
      self$711->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1711 =
      _field$2064;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2063;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$710;
    if (idx$709 < 0 || idx$709 >= Moonbit_array_length(entries$1711)) {
      moonbit_panic();
    }
    _tmp$2063
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1711[
        idx$709
      ];
    _bind$710 = _tmp$2063;
    if (_bind$710 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1700;
      if (_bind$710) {
        moonbit_incref(_bind$710);
      }
      moonbit_decref(self$711);
      if (_bind$710) {
        moonbit_decref(_bind$710);
      }
      _tmp$1700 = 0;
      return _tmp$1700;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$712 =
        _bind$710;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$713 =
        _Some$712;
      int32_t hash$1702 = _entry$713->$3;
      int32_t _if_result$2394;
      int32_t _field$2061;
      int32_t psl$1705;
      int32_t _tmp$1707;
      int32_t _tmp$1709;
      int32_t capacity_mask$1710;
      int32_t _tmp$1708;
      if (hash$1702 == hash$706) {
        int32_t key$1701 = _entry$713->$4;
        _if_result$2394 = key$1701 == key$707;
      } else {
        _if_result$2394 = 0;
      }
      if (_if_result$2394) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2062;
        int32_t _cnt$2302;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1704;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1703;
        moonbit_incref(_entry$713);
        moonbit_decref(self$711);
        _field$2062 = _entry$713->$5;
        _cnt$2302 = Moonbit_object_header(_entry$713)->rc;
        if (_cnt$2302 > 1) {
          int32_t _new_cnt$2304;
          moonbit_incref(_field$2062);
          _new_cnt$2304 = _cnt$2302 - 1;
          Moonbit_object_header(_entry$713)->rc = _new_cnt$2304;
        } else if (_cnt$2302 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2303 =
            _entry$713->$1;
          if (_field$2303) {
            moonbit_decref(_field$2303);
          }
          moonbit_free(_entry$713);
        }
        value$1704 = _field$2062;
        _tmp$1703 = value$1704;
        return _tmp$1703;
      } else {
        moonbit_incref(_entry$713);
      }
      _field$2061 = _entry$713->$2;
      moonbit_decref(_entry$713);
      psl$1705 = _field$2061;
      if (i$708 > psl$1705) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1706;
        moonbit_decref(self$711);
        _tmp$1706 = 0;
        return _tmp$1706;
      }
      _tmp$1707 = i$708 + 1;
      _tmp$1709 = idx$709 + 1;
      capacity_mask$1710 = self$711->$3;
      _tmp$1708 = _tmp$1709 & capacity_mask$1710;
      i$708 = _tmp$1707;
      idx$709 = _tmp$1708;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$702,
  moonbit_string_t key$698
) {
  int32_t hash$697;
  int32_t capacity_mask$1699;
  int32_t _tmp$1698;
  int32_t i$699;
  int32_t idx$700;
  moonbit_incref(key$698);
  hash$697 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$698);
  capacity_mask$1699 = self$702->$3;
  _tmp$1698 = hash$697 & capacity_mask$1699;
  i$699 = 0;
  idx$700 = _tmp$1698;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2070 =
      self$702->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1697 =
      _field$2070;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2069;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$701;
    if (idx$700 < 0 || idx$700 >= Moonbit_array_length(entries$1697)) {
      moonbit_panic();
    }
    _tmp$2069
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1697[
        idx$700
      ];
    _bind$701 = _tmp$2069;
    if (_bind$701 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1686;
      if (_bind$701) {
        moonbit_incref(_bind$701);
      }
      moonbit_decref(self$702);
      if (_bind$701) {
        moonbit_decref(_bind$701);
      }
      moonbit_decref(key$698);
      _tmp$1686 = 0;
      return _tmp$1686;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$703 =
        _bind$701;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$704 =
        _Some$703;
      int32_t hash$1688 = _entry$704->$3;
      int32_t _if_result$2396;
      int32_t _field$2065;
      int32_t psl$1691;
      int32_t _tmp$1693;
      int32_t _tmp$1695;
      int32_t capacity_mask$1696;
      int32_t _tmp$1694;
      if (hash$1688 == hash$697) {
        moonbit_string_t _field$2068 = _entry$704->$4;
        moonbit_string_t key$1687 = _field$2068;
        int32_t _tmp$2067 = moonbit_val_array_equal(key$1687, key$698);
        _if_result$2396 = _tmp$2067;
      } else {
        _if_result$2396 = 0;
      }
      if (_if_result$2396) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2066;
        int32_t _cnt$2305;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1690;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1689;
        moonbit_incref(_entry$704);
        moonbit_decref(self$702);
        moonbit_decref(key$698);
        _field$2066 = _entry$704->$5;
        _cnt$2305 = Moonbit_object_header(_entry$704)->rc;
        if (_cnt$2305 > 1) {
          int32_t _new_cnt$2308;
          moonbit_incref(_field$2066);
          _new_cnt$2308 = _cnt$2305 - 1;
          Moonbit_object_header(_entry$704)->rc = _new_cnt$2308;
        } else if (_cnt$2305 == 1) {
          moonbit_string_t _field$2307 = _entry$704->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2306;
          moonbit_decref(_field$2307);
          _field$2306 = _entry$704->$1;
          if (_field$2306) {
            moonbit_decref(_field$2306);
          }
          moonbit_free(_entry$704);
        }
        value$1690 = _field$2066;
        _tmp$1689 = value$1690;
        return _tmp$1689;
      } else {
        moonbit_incref(_entry$704);
      }
      _field$2065 = _entry$704->$2;
      moonbit_decref(_entry$704);
      psl$1691 = _field$2065;
      if (i$699 > psl$1691) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1692;
        moonbit_decref(self$702);
        moonbit_decref(key$698);
        _tmp$1692 = 0;
        return _tmp$1692;
      }
      _tmp$1693 = i$699 + 1;
      _tmp$1695 = idx$700 + 1;
      capacity_mask$1696 = self$702->$3;
      _tmp$1694 = _tmp$1695 & capacity_mask$1696;
      i$699 = _tmp$1693;
      idx$700 = _tmp$1694;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$693,
  int32_t key$689
) {
  int32_t hash$688 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$689);
  int32_t capacity_mask$1685 = self$693->$3;
  int32_t _tmp$1684 = hash$688 & capacity_mask$1685;
  int32_t i$690 = 0;
  int32_t idx$691 = _tmp$1684;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2074 =
      self$693->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1683 =
      _field$2074;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2073;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$692;
    if (idx$691 < 0 || idx$691 >= Moonbit_array_length(entries$1683)) {
      moonbit_panic();
    }
    _tmp$2073
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1683[
        idx$691
      ];
    _bind$692 = _tmp$2073;
    if (_bind$692 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1672;
      if (_bind$692) {
        moonbit_incref(_bind$692);
      }
      moonbit_decref(self$693);
      if (_bind$692) {
        moonbit_decref(_bind$692);
      }
      _tmp$1672 = 0;
      return _tmp$1672;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$694 =
        _bind$692;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$695 =
        _Some$694;
      int32_t hash$1674 = _entry$695->$3;
      int32_t _if_result$2398;
      int32_t _field$2071;
      int32_t psl$1677;
      int32_t _tmp$1679;
      int32_t _tmp$1681;
      int32_t capacity_mask$1682;
      int32_t _tmp$1680;
      if (hash$1674 == hash$688) {
        int32_t key$1673 = _entry$695->$4;
        _if_result$2398 = key$1673 == key$689;
      } else {
        _if_result$2398 = 0;
      }
      if (_if_result$2398) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2072;
        int32_t _cnt$2309;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1676;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1675;
        moonbit_incref(_entry$695);
        moonbit_decref(self$693);
        _field$2072 = _entry$695->$5;
        _cnt$2309 = Moonbit_object_header(_entry$695)->rc;
        if (_cnt$2309 > 1) {
          int32_t _new_cnt$2311;
          moonbit_incref(_field$2072);
          _new_cnt$2311 = _cnt$2309 - 1;
          Moonbit_object_header(_entry$695)->rc = _new_cnt$2311;
        } else if (_cnt$2309 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2310 =
            _entry$695->$1;
          if (_field$2310) {
            moonbit_decref(_field$2310);
          }
          moonbit_free(_entry$695);
        }
        value$1676 = _field$2072;
        _tmp$1675 = value$1676;
        return _tmp$1675;
      } else {
        moonbit_incref(_entry$695);
      }
      _field$2071 = _entry$695->$2;
      moonbit_decref(_entry$695);
      psl$1677 = _field$2071;
      if (i$690 > psl$1677) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1678;
        moonbit_decref(self$693);
        _tmp$1678 = 0;
        return _tmp$1678;
      }
      _tmp$1679 = i$690 + 1;
      _tmp$1681 = idx$691 + 1;
      capacity_mask$1682 = self$693->$3;
      _tmp$1680 = _tmp$1681 & capacity_mask$1682;
      i$690 = _tmp$1679;
      idx$691 = _tmp$1680;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$684,
  moonbit_string_t key$680
) {
  int32_t hash$679;
  int32_t capacity_mask$1671;
  int32_t _tmp$1670;
  int32_t i$681;
  int32_t idx$682;
  moonbit_incref(key$680);
  hash$679 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$680);
  capacity_mask$1671 = self$684->$3;
  _tmp$1670 = hash$679 & capacity_mask$1671;
  i$681 = 0;
  idx$682 = _tmp$1670;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2080 =
      self$684->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1669 =
      _field$2080;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2079;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$683;
    if (idx$682 < 0 || idx$682 >= Moonbit_array_length(entries$1669)) {
      moonbit_panic();
    }
    _tmp$2079
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1669[
        idx$682
      ];
    _bind$683 = _tmp$2079;
    if (_bind$683 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1658;
      if (_bind$683) {
        moonbit_incref(_bind$683);
      }
      moonbit_decref(self$684);
      if (_bind$683) {
        moonbit_decref(_bind$683);
      }
      moonbit_decref(key$680);
      _tmp$1658 = 0;
      return _tmp$1658;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$685 =
        _bind$683;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$686 =
        _Some$685;
      int32_t hash$1660 = _entry$686->$3;
      int32_t _if_result$2400;
      int32_t _field$2075;
      int32_t psl$1663;
      int32_t _tmp$1665;
      int32_t _tmp$1667;
      int32_t capacity_mask$1668;
      int32_t _tmp$1666;
      if (hash$1660 == hash$679) {
        moonbit_string_t _field$2078 = _entry$686->$4;
        moonbit_string_t key$1659 = _field$2078;
        int32_t _tmp$2077 = moonbit_val_array_equal(key$1659, key$680);
        _if_result$2400 = _tmp$2077;
      } else {
        _if_result$2400 = 0;
      }
      if (_if_result$2400) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2076;
        int32_t _cnt$2312;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1662;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1661;
        moonbit_incref(_entry$686);
        moonbit_decref(self$684);
        moonbit_decref(key$680);
        _field$2076 = _entry$686->$5;
        _cnt$2312 = Moonbit_object_header(_entry$686)->rc;
        if (_cnt$2312 > 1) {
          int32_t _new_cnt$2315;
          moonbit_incref(_field$2076);
          _new_cnt$2315 = _cnt$2312 - 1;
          Moonbit_object_header(_entry$686)->rc = _new_cnt$2315;
        } else if (_cnt$2312 == 1) {
          moonbit_string_t _field$2314 = _entry$686->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2313;
          moonbit_decref(_field$2314);
          _field$2313 = _entry$686->$1;
          if (_field$2313) {
            moonbit_decref(_field$2313);
          }
          moonbit_free(_entry$686);
        }
        value$1662 = _field$2076;
        _tmp$1661 = value$1662;
        return _tmp$1661;
      } else {
        moonbit_incref(_entry$686);
      }
      _field$2075 = _entry$686->$2;
      moonbit_decref(_entry$686);
      psl$1663 = _field$2075;
      if (i$681 > psl$1663) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1664;
        moonbit_decref(self$684);
        moonbit_decref(key$680);
        _tmp$1664 = 0;
        return _tmp$1664;
      }
      _tmp$1665 = i$681 + 1;
      _tmp$1667 = idx$682 + 1;
      capacity_mask$1668 = self$684->$3;
      _tmp$1666 = _tmp$1667 & capacity_mask$1668;
      i$681 = _tmp$1665;
      idx$682 = _tmp$1666;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$672
) {
  int32_t length$671;
  int32_t capacity$673;
  int32_t _tmp$1649;
  int32_t _tmp$1648;
  int32_t _tmp$1657;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$674;
  int32_t _len$675;
  int32_t _i$676;
  moonbit_incref(arr$672.$0);
  length$671 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$672);
  capacity$673 = $Int$$next_power_of_two(length$671);
  _tmp$1649 = capacity$673;
  _tmp$1648 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1649);
  if (length$671 > _tmp$1648) {
    int32_t _tmp$1650 = capacity$673;
    capacity$673 = _tmp$1650 * 2;
  }
  _tmp$1657 = capacity$673;
  m$674 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$1657);
  moonbit_incref(arr$672.$0);
  _len$675 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$672);
  _i$676 = 0;
  while (1) {
    if (_i$676 < _len$675) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2084 =
        arr$672.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1653 =
        _field$2084;
      int32_t start$1655 = arr$672.$1;
      int32_t _tmp$1654 = start$1655 + _i$676;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2083 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1653[
          _tmp$1654
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$677 =
        _tmp$2083;
      moonbit_string_t _field$2082 = e$677->$0;
      moonbit_string_t _tmp$1651 = _field$2082;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2081 =
        e$677->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1652 =
        _field$2081;
      int32_t _tmp$1656;
      moonbit_incref(_tmp$1652);
      moonbit_incref(_tmp$1651);
      moonbit_incref(m$674);
      $$moonbitlang$core$builtin$Map$$set$2(m$674, _tmp$1651, _tmp$1652);
      _tmp$1656 = _i$676 + 1;
      _i$676 = _tmp$1656;
      continue;
    } else {
      moonbit_decref(arr$672.$0);
    }
    break;
  }
  return m$674;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$664
) {
  int32_t length$663;
  int32_t capacity$665;
  int32_t _tmp$1639;
  int32_t _tmp$1638;
  int32_t _tmp$1647;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$666;
  int32_t _len$667;
  int32_t _i$668;
  moonbit_incref(arr$664.$0);
  length$663 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$664);
  capacity$665 = $Int$$next_power_of_two(length$663);
  _tmp$1639 = capacity$665;
  _tmp$1638 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1639);
  if (length$663 > _tmp$1638) {
    int32_t _tmp$1640 = capacity$665;
    capacity$665 = _tmp$1640 * 2;
  }
  _tmp$1647 = capacity$665;
  m$666 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$1647);
  moonbit_incref(arr$664.$0);
  _len$667 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$664);
  _i$668 = 0;
  while (1) {
    if (_i$668 < _len$667) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2088 =
        arr$664.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1643 =
        _field$2088;
      int32_t start$1645 = arr$664.$1;
      int32_t _tmp$1644 = start$1645 + _i$668;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2087 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1643[
          _tmp$1644
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$669 =
        _tmp$2087;
      moonbit_string_t _field$2086 = e$669->$0;
      moonbit_string_t _tmp$1641 = _field$2086;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2085 =
        e$669->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1642 =
        _field$2085;
      int32_t _tmp$1646;
      moonbit_incref(_tmp$1642);
      moonbit_incref(_tmp$1641);
      moonbit_incref(m$666);
      $$moonbitlang$core$builtin$Map$$set$1(m$666, _tmp$1641, _tmp$1642);
      _tmp$1646 = _i$668 + 1;
      _i$668 = _tmp$1646;
      continue;
    } else {
      moonbit_decref(arr$664.$0);
    }
    break;
  }
  return m$666;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$656
) {
  int32_t length$655;
  int32_t capacity$657;
  int32_t _tmp$1629;
  int32_t _tmp$1628;
  int32_t _tmp$1637;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$658;
  int32_t _len$659;
  int32_t _i$660;
  moonbit_incref(arr$656.$0);
  length$655 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$656);
  capacity$657 = $Int$$next_power_of_two(length$655);
  _tmp$1629 = capacity$657;
  _tmp$1628 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1629);
  if (length$655 > _tmp$1628) {
    int32_t _tmp$1630 = capacity$657;
    capacity$657 = _tmp$1630 * 2;
  }
  _tmp$1637 = capacity$657;
  m$658 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$1637);
  moonbit_incref(arr$656.$0);
  _len$659 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$656);
  _i$660 = 0;
  while (1) {
    if (_i$660 < _len$659) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2092 =
        arr$656.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1633 =
        _field$2092;
      int32_t start$1635 = arr$656.$1;
      int32_t _tmp$1634 = start$1635 + _i$660;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2091 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1633[
          _tmp$1634
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$661 =
        _tmp$2091;
      moonbit_string_t _field$2090 = e$661->$0;
      moonbit_string_t _tmp$1631 = _field$2090;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2089 =
        e$661->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1632 =
        _field$2089;
      int32_t _tmp$1636;
      moonbit_incref(_tmp$1632);
      moonbit_incref(_tmp$1631);
      moonbit_incref(m$658);
      $$moonbitlang$core$builtin$Map$$set$0(m$658, _tmp$1631, _tmp$1632);
      _tmp$1636 = _i$660 + 1;
      _i$660 = _tmp$1636;
      continue;
    } else {
      moonbit_decref(arr$656.$0);
    }
    break;
  }
  return m$658;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$652,
  moonbit_string_t key$653,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$654
) {
  int32_t _tmp$1627;
  moonbit_incref(key$653);
  _tmp$1627 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$653);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$652, key$653, value$654, _tmp$1627
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$649,
  moonbit_string_t key$650,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$651
) {
  int32_t _tmp$1626;
  moonbit_incref(key$650);
  _tmp$1626 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$650);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$649, key$650, value$651, _tmp$1626
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$646,
  moonbit_string_t key$647,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$648
) {
  int32_t _tmp$1625;
  moonbit_incref(key$647);
  _tmp$1625 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$647);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$646, key$647, value$648, _tmp$1625
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$636
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2099 =
    self$636->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$635 =
    _field$2099;
  int32_t capacity$1624 = self$636->$2;
  int32_t new_capacity$637 = capacity$1624 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1619 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1618 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$637, _tmp$1619
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2098 =
    self$636->$0;
  int32_t _tmp$1620;
  int32_t capacity$1622;
  int32_t _tmp$1621;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1623;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2097;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$638;
  if (old_head$635) {
    moonbit_incref(old_head$635);
  }
  moonbit_decref(_old$2098);
  self$636->$0 = _tmp$1618;
  self$636->$2 = new_capacity$637;
  _tmp$1620 = new_capacity$637 - 1;
  self$636->$3 = _tmp$1620;
  capacity$1622 = self$636->$2;
  _tmp$1621 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1622);
  self$636->$4 = _tmp$1621;
  self$636->$1 = 0;
  _tmp$1623 = 0;
  _old$2097 = self$636->$5;
  if (_old$2097) {
    moonbit_decref(_old$2097);
  }
  self$636->$5 = _tmp$1623;
  self$636->$6 = -1;
  _param$638 = old_head$635;
  while (1) {
    if (_param$638 == 0) {
      if (_param$638) {
        moonbit_decref(_param$638);
      }
      moonbit_decref(self$636);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$639 =
        _param$638;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$640 =
        _Some$639;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2096 =
        _x$640->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$641 =
        _field$2096;
      moonbit_string_t _field$2095 = _x$640->$4;
      moonbit_string_t _key$642 = _field$2095;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2094 =
        _x$640->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$643 =
        _field$2094;
      int32_t _field$2093 = _x$640->$3;
      int32_t _cnt$2316 = Moonbit_object_header(_x$640)->rc;
      int32_t _hash$644;
      if (_cnt$2316 > 1) {
        int32_t _new_cnt$2317;
        moonbit_incref(_value$643);
        moonbit_incref(_key$642);
        if (_next$641) {
          moonbit_incref(_next$641);
        }
        _new_cnt$2317 = _cnt$2316 - 1;
        Moonbit_object_header(_x$640)->rc = _new_cnt$2317;
      } else if (_cnt$2316 == 1) {
        moonbit_free(_x$640);
      }
      _hash$644 = _field$2093;
      moonbit_incref(self$636);
      $$moonbitlang$core$builtin$Map$$set_with_hash$2(
        self$636, _key$642, _value$643, _hash$644
      );
      _param$638 = _next$641;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$625
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2106 =
    self$625->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$624 =
    _field$2106;
  int32_t capacity$1617 = self$625->$2;
  int32_t new_capacity$626 = capacity$1617 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1612 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1611 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$626, _tmp$1612
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2105 =
    self$625->$0;
  int32_t _tmp$1613;
  int32_t capacity$1615;
  int32_t _tmp$1614;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1616;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2104;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$627;
  if (old_head$624) {
    moonbit_incref(old_head$624);
  }
  moonbit_decref(_old$2105);
  self$625->$0 = _tmp$1611;
  self$625->$2 = new_capacity$626;
  _tmp$1613 = new_capacity$626 - 1;
  self$625->$3 = _tmp$1613;
  capacity$1615 = self$625->$2;
  _tmp$1614 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1615);
  self$625->$4 = _tmp$1614;
  self$625->$1 = 0;
  _tmp$1616 = 0;
  _old$2104 = self$625->$5;
  if (_old$2104) {
    moonbit_decref(_old$2104);
  }
  self$625->$5 = _tmp$1616;
  self$625->$6 = -1;
  _param$627 = old_head$624;
  while (1) {
    if (_param$627 == 0) {
      if (_param$627) {
        moonbit_decref(_param$627);
      }
      moonbit_decref(self$625);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$628 =
        _param$627;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$629 =
        _Some$628;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2103 =
        _x$629->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$630 =
        _field$2103;
      moonbit_string_t _field$2102 = _x$629->$4;
      moonbit_string_t _key$631 = _field$2102;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2101 =
        _x$629->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$632 =
        _field$2101;
      int32_t _field$2100 = _x$629->$3;
      int32_t _cnt$2318 = Moonbit_object_header(_x$629)->rc;
      int32_t _hash$633;
      if (_cnt$2318 > 1) {
        int32_t _new_cnt$2319;
        moonbit_incref(_value$632);
        moonbit_incref(_key$631);
        if (_next$630) {
          moonbit_incref(_next$630);
        }
        _new_cnt$2319 = _cnt$2318 - 1;
        Moonbit_object_header(_x$629)->rc = _new_cnt$2319;
      } else if (_cnt$2318 == 1) {
        moonbit_free(_x$629);
      }
      _hash$633 = _field$2100;
      moonbit_incref(self$625);
      $$moonbitlang$core$builtin$Map$$set_with_hash$1(
        self$625, _key$631, _value$632, _hash$633
      );
      _param$627 = _next$630;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$614
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2113 =
    self$614->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$613 =
    _field$2113;
  int32_t capacity$1610 = self$614->$2;
  int32_t new_capacity$615 = capacity$1610 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1605 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1604 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$615, _tmp$1605
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2112 =
    self$614->$0;
  int32_t _tmp$1606;
  int32_t capacity$1608;
  int32_t _tmp$1607;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1609;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2111;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$616;
  if (old_head$613) {
    moonbit_incref(old_head$613);
  }
  moonbit_decref(_old$2112);
  self$614->$0 = _tmp$1604;
  self$614->$2 = new_capacity$615;
  _tmp$1606 = new_capacity$615 - 1;
  self$614->$3 = _tmp$1606;
  capacity$1608 = self$614->$2;
  _tmp$1607 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1608);
  self$614->$4 = _tmp$1607;
  self$614->$1 = 0;
  _tmp$1609 = 0;
  _old$2111 = self$614->$5;
  if (_old$2111) {
    moonbit_decref(_old$2111);
  }
  self$614->$5 = _tmp$1609;
  self$614->$6 = -1;
  _param$616 = old_head$613;
  while (1) {
    if (_param$616 == 0) {
      if (_param$616) {
        moonbit_decref(_param$616);
      }
      moonbit_decref(self$614);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$617 =
        _param$616;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$618 =
        _Some$617;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2110 =
        _x$618->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$619 =
        _field$2110;
      moonbit_string_t _field$2109 = _x$618->$4;
      moonbit_string_t _key$620 = _field$2109;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2108 =
        _x$618->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$621 =
        _field$2108;
      int32_t _field$2107 = _x$618->$3;
      int32_t _cnt$2320 = Moonbit_object_header(_x$618)->rc;
      int32_t _hash$622;
      if (_cnt$2320 > 1) {
        int32_t _new_cnt$2321;
        moonbit_incref(_value$621);
        moonbit_incref(_key$620);
        if (_next$619) {
          moonbit_incref(_next$619);
        }
        _new_cnt$2321 = _cnt$2320 - 1;
        Moonbit_object_header(_x$618)->rc = _new_cnt$2321;
      } else if (_cnt$2320 == 1) {
        moonbit_free(_x$618);
      }
      _hash$622 = _field$2107;
      moonbit_incref(self$614);
      $$moonbitlang$core$builtin$Map$$set_with_hash$0(
        self$614, _key$620, _value$621, _hash$622
      );
      _param$616 = _next$619;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$597,
  moonbit_string_t key$606,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$607,
  int32_t hash$605
) {
  int32_t size$1590 = self$597->$1;
  int32_t grow_at$1591 = self$597->$4;
  int32_t capacity_mask$1603;
  int32_t _tmp$1602;
  struct $$3c$Int$2a$Int$3e$* _bind$598;
  int32_t psl$599;
  int32_t idx$600;
  int32_t _idx$608;
  int32_t _field$2114;
  int32_t _psl$609;
  int32_t _bind$610;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$611;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$612;
  if (size$1590 >= grow_at$1591) {
    moonbit_incref(self$597);
    $$moonbitlang$core$builtin$Map$$grow$2(self$597);
  }
  capacity_mask$1603 = self$597->$3;
  _tmp$1602 = hash$605 & capacity_mask$1603;
  psl$599 = 0;
  idx$600 = _tmp$1602;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2119 =
      self$597->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1601 =
      _field$2119;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2118;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$601;
    if (idx$600 < 0 || idx$600 >= Moonbit_array_length(entries$1601)) {
      moonbit_panic();
    }
    _tmp$2118
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1601[
        idx$600
      ];
    _bind$601 = _tmp$2118;
    if (_bind$601 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1592 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1592)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1592->$0 = idx$600;
      _tuple$1592->$1 = psl$599;
      _bind$598 = _tuple$1592;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$603 =
        _bind$601;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$604 =
        _Some$603;
      int32_t hash$1594 = _curr_entry$604->$3;
      int32_t _if_result$2408;
      int32_t psl$1595;
      int32_t _tmp$1597;
      int32_t _tmp$1599;
      int32_t capacity_mask$1600;
      int32_t _tmp$1598;
      if (hash$1594 == hash$605) {
        moonbit_string_t _field$2117 = _curr_entry$604->$4;
        moonbit_string_t key$1593 = _field$2117;
        int32_t _tmp$2116 = moonbit_val_array_equal(key$1593, key$606);
        _if_result$2408 = _tmp$2116;
      } else {
        _if_result$2408 = 0;
      }
      if (_if_result$2408) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2115;
        moonbit_incref(_curr_entry$604);
        moonbit_decref(key$606);
        moonbit_decref(self$597);
        _old$2115 = _curr_entry$604->$5;
        moonbit_decref(_old$2115);
        _curr_entry$604->$5 = value$607;
        moonbit_decref(_curr_entry$604);
        return 0;
      } else {
        moonbit_incref(_curr_entry$604);
      }
      psl$1595 = _curr_entry$604->$2;
      if (psl$599 > psl$1595) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1596;
        moonbit_incref(self$597);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$597, idx$600, _curr_entry$604
        );
        _tuple$1596
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1596)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1596->$0 = idx$600;
        _tuple$1596->$1 = psl$599;
        _bind$598 = _tuple$1596;
        break;
      } else {
        moonbit_decref(_curr_entry$604);
      }
      _tmp$1597 = psl$599 + 1;
      _tmp$1599 = idx$600 + 1;
      capacity_mask$1600 = self$597->$3;
      _tmp$1598 = _tmp$1599 & capacity_mask$1600;
      psl$599 = _tmp$1597;
      idx$600 = _tmp$1598;
      continue;
    }
    break;
  }
  _idx$608 = _bind$598->$0;
  _field$2114 = _bind$598->$1;
  moonbit_decref(_bind$598);
  _psl$609 = _field$2114;
  _bind$610 = self$597->$6;
  _bind$611 = 0;
  entry$612
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$612)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$612->$0 = _bind$610;
  entry$612->$1 = _bind$611;
  entry$612->$2 = _psl$609;
  entry$612->$3 = hash$605;
  entry$612->$4 = key$606;
  entry$612->$5 = value$607;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
    self$597, _idx$608, entry$612
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$581,
  moonbit_string_t key$590,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$591,
  int32_t hash$589
) {
  int32_t size$1576 = self$581->$1;
  int32_t grow_at$1577 = self$581->$4;
  int32_t capacity_mask$1589;
  int32_t _tmp$1588;
  struct $$3c$Int$2a$Int$3e$* _bind$582;
  int32_t psl$583;
  int32_t idx$584;
  int32_t _idx$592;
  int32_t _field$2120;
  int32_t _psl$593;
  int32_t _bind$594;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$595;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$596;
  if (size$1576 >= grow_at$1577) {
    moonbit_incref(self$581);
    $$moonbitlang$core$builtin$Map$$grow$1(self$581);
  }
  capacity_mask$1589 = self$581->$3;
  _tmp$1588 = hash$589 & capacity_mask$1589;
  psl$583 = 0;
  idx$584 = _tmp$1588;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2125 =
      self$581->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1587 =
      _field$2125;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2124;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$585;
    if (idx$584 < 0 || idx$584 >= Moonbit_array_length(entries$1587)) {
      moonbit_panic();
    }
    _tmp$2124
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1587[
        idx$584
      ];
    _bind$585 = _tmp$2124;
    if (_bind$585 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1578 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1578)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1578->$0 = idx$584;
      _tuple$1578->$1 = psl$583;
      _bind$582 = _tuple$1578;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$587 =
        _bind$585;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$588 =
        _Some$587;
      int32_t hash$1580 = _curr_entry$588->$3;
      int32_t _if_result$2410;
      int32_t psl$1581;
      int32_t _tmp$1583;
      int32_t _tmp$1585;
      int32_t capacity_mask$1586;
      int32_t _tmp$1584;
      if (hash$1580 == hash$589) {
        moonbit_string_t _field$2123 = _curr_entry$588->$4;
        moonbit_string_t key$1579 = _field$2123;
        int32_t _tmp$2122 = moonbit_val_array_equal(key$1579, key$590);
        _if_result$2410 = _tmp$2122;
      } else {
        _if_result$2410 = 0;
      }
      if (_if_result$2410) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2121;
        moonbit_incref(_curr_entry$588);
        moonbit_decref(key$590);
        moonbit_decref(self$581);
        _old$2121 = _curr_entry$588->$5;
        moonbit_decref(_old$2121);
        _curr_entry$588->$5 = value$591;
        moonbit_decref(_curr_entry$588);
        return 0;
      } else {
        moonbit_incref(_curr_entry$588);
      }
      psl$1581 = _curr_entry$588->$2;
      if (psl$583 > psl$1581) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1582;
        moonbit_incref(self$581);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$581, idx$584, _curr_entry$588
        );
        _tuple$1582
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1582)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1582->$0 = idx$584;
        _tuple$1582->$1 = psl$583;
        _bind$582 = _tuple$1582;
        break;
      } else {
        moonbit_decref(_curr_entry$588);
      }
      _tmp$1583 = psl$583 + 1;
      _tmp$1585 = idx$584 + 1;
      capacity_mask$1586 = self$581->$3;
      _tmp$1584 = _tmp$1585 & capacity_mask$1586;
      psl$583 = _tmp$1583;
      idx$584 = _tmp$1584;
      continue;
    }
    break;
  }
  _idx$592 = _bind$582->$0;
  _field$2120 = _bind$582->$1;
  moonbit_decref(_bind$582);
  _psl$593 = _field$2120;
  _bind$594 = self$581->$6;
  _bind$595 = 0;
  entry$596
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$596)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$596->$0 = _bind$594;
  entry$596->$1 = _bind$595;
  entry$596->$2 = _psl$593;
  entry$596->$3 = hash$589;
  entry$596->$4 = key$590;
  entry$596->$5 = value$591;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
    self$581, _idx$592, entry$596
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$565,
  moonbit_string_t key$574,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$575,
  int32_t hash$573
) {
  int32_t size$1562 = self$565->$1;
  int32_t grow_at$1563 = self$565->$4;
  int32_t capacity_mask$1575;
  int32_t _tmp$1574;
  struct $$3c$Int$2a$Int$3e$* _bind$566;
  int32_t psl$567;
  int32_t idx$568;
  int32_t _idx$576;
  int32_t _field$2126;
  int32_t _psl$577;
  int32_t _bind$578;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$579;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$580;
  if (size$1562 >= grow_at$1563) {
    moonbit_incref(self$565);
    $$moonbitlang$core$builtin$Map$$grow$0(self$565);
  }
  capacity_mask$1575 = self$565->$3;
  _tmp$1574 = hash$573 & capacity_mask$1575;
  psl$567 = 0;
  idx$568 = _tmp$1574;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2131 =
      self$565->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1573 =
      _field$2131;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2130;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$569;
    if (idx$568 < 0 || idx$568 >= Moonbit_array_length(entries$1573)) {
      moonbit_panic();
    }
    _tmp$2130
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1573[
        idx$568
      ];
    _bind$569 = _tmp$2130;
    if (_bind$569 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1564 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1564)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1564->$0 = idx$568;
      _tuple$1564->$1 = psl$567;
      _bind$566 = _tuple$1564;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$571 =
        _bind$569;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$572 =
        _Some$571;
      int32_t hash$1566 = _curr_entry$572->$3;
      int32_t _if_result$2412;
      int32_t psl$1567;
      int32_t _tmp$1569;
      int32_t _tmp$1571;
      int32_t capacity_mask$1572;
      int32_t _tmp$1570;
      if (hash$1566 == hash$573) {
        moonbit_string_t _field$2129 = _curr_entry$572->$4;
        moonbit_string_t key$1565 = _field$2129;
        int32_t _tmp$2128 = moonbit_val_array_equal(key$1565, key$574);
        _if_result$2412 = _tmp$2128;
      } else {
        _if_result$2412 = 0;
      }
      if (_if_result$2412) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2127;
        moonbit_incref(_curr_entry$572);
        moonbit_decref(key$574);
        moonbit_decref(self$565);
        _old$2127 = _curr_entry$572->$5;
        moonbit_decref(_old$2127);
        _curr_entry$572->$5 = value$575;
        moonbit_decref(_curr_entry$572);
        return 0;
      } else {
        moonbit_incref(_curr_entry$572);
      }
      psl$1567 = _curr_entry$572->$2;
      if (psl$567 > psl$1567) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1568;
        moonbit_incref(self$565);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$565, idx$568, _curr_entry$572
        );
        _tuple$1568
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1568)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1568->$0 = idx$568;
        _tuple$1568->$1 = psl$567;
        _bind$566 = _tuple$1568;
        break;
      } else {
        moonbit_decref(_curr_entry$572);
      }
      _tmp$1569 = psl$567 + 1;
      _tmp$1571 = idx$568 + 1;
      capacity_mask$1572 = self$565->$3;
      _tmp$1570 = _tmp$1571 & capacity_mask$1572;
      psl$567 = _tmp$1569;
      idx$568 = _tmp$1570;
      continue;
    }
    break;
  }
  _idx$576 = _bind$566->$0;
  _field$2126 = _bind$566->$1;
  moonbit_decref(_bind$566);
  _psl$577 = _field$2126;
  _bind$578 = self$565->$6;
  _bind$579 = 0;
  entry$580
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$580)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$580->$0 = _bind$578;
  entry$580->$1 = _bind$579;
  entry$580->$2 = _psl$577;
  entry$580->$3 = hash$573;
  entry$580->$4 = key$574;
  entry$580->$5 = value$575;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
    self$565, _idx$576, entry$580
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$559,
  int32_t idx$564,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$563
) {
  int32_t psl$1561 = entry$563->$2;
  int32_t _tmp$1557 = psl$1561 + 1;
  int32_t _tmp$1559 = idx$564 + 1;
  int32_t capacity_mask$1560 = self$559->$3;
  int32_t _tmp$1558 = _tmp$1559 & capacity_mask$1560;
  int32_t psl$555 = _tmp$1557;
  int32_t idx$556 = _tmp$1558;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$557 =
    entry$563;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2133 =
      self$559->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1556 =
      _field$2133;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2132;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$558;
    if (idx$556 < 0 || idx$556 >= Moonbit_array_length(entries$1556)) {
      moonbit_panic();
    }
    _tmp$2132
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1556[
        idx$556
      ];
    _bind$558 = _tmp$2132;
    if (_bind$558 == 0) {
      entry$557->$2 = psl$555;
      $$moonbitlang$core$builtin$Map$$set_entry$2(
        self$559, entry$557, idx$556
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$561 =
        _bind$558;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$562 =
        _Some$561;
      int32_t psl$1546 = _curr_entry$562->$2;
      if (psl$555 > psl$1546) {
        int32_t psl$1551;
        int32_t _tmp$1547;
        int32_t _tmp$1549;
        int32_t capacity_mask$1550;
        int32_t _tmp$1548;
        entry$557->$2 = psl$555;
        moonbit_incref(_curr_entry$562);
        moonbit_incref(self$559);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$559, entry$557, idx$556
        );
        psl$1551 = _curr_entry$562->$2;
        _tmp$1547 = psl$1551 + 1;
        _tmp$1549 = idx$556 + 1;
        capacity_mask$1550 = self$559->$3;
        _tmp$1548 = _tmp$1549 & capacity_mask$1550;
        psl$555 = _tmp$1547;
        idx$556 = _tmp$1548;
        entry$557 = _curr_entry$562;
        continue;
      } else {
        int32_t _tmp$1552 = psl$555 + 1;
        int32_t _tmp$1554 = idx$556 + 1;
        int32_t capacity_mask$1555 = self$559->$3;
        int32_t _tmp$1553 = _tmp$1554 & capacity_mask$1555;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2414 =
          entry$557;
        psl$555 = _tmp$1552;
        idx$556 = _tmp$1553;
        entry$557 = _tmp$2414;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$549,
  int32_t idx$554,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$553
) {
  int32_t psl$1545 = entry$553->$2;
  int32_t _tmp$1541 = psl$1545 + 1;
  int32_t _tmp$1543 = idx$554 + 1;
  int32_t capacity_mask$1544 = self$549->$3;
  int32_t _tmp$1542 = _tmp$1543 & capacity_mask$1544;
  int32_t psl$545 = _tmp$1541;
  int32_t idx$546 = _tmp$1542;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$547 =
    entry$553;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2135 =
      self$549->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1540 =
      _field$2135;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2134;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$548;
    if (idx$546 < 0 || idx$546 >= Moonbit_array_length(entries$1540)) {
      moonbit_panic();
    }
    _tmp$2134
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1540[
        idx$546
      ];
    _bind$548 = _tmp$2134;
    if (_bind$548 == 0) {
      entry$547->$2 = psl$545;
      $$moonbitlang$core$builtin$Map$$set_entry$1(
        self$549, entry$547, idx$546
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$551 =
        _bind$548;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$552 =
        _Some$551;
      int32_t psl$1530 = _curr_entry$552->$2;
      if (psl$545 > psl$1530) {
        int32_t psl$1535;
        int32_t _tmp$1531;
        int32_t _tmp$1533;
        int32_t capacity_mask$1534;
        int32_t _tmp$1532;
        entry$547->$2 = psl$545;
        moonbit_incref(_curr_entry$552);
        moonbit_incref(self$549);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$549, entry$547, idx$546
        );
        psl$1535 = _curr_entry$552->$2;
        _tmp$1531 = psl$1535 + 1;
        _tmp$1533 = idx$546 + 1;
        capacity_mask$1534 = self$549->$3;
        _tmp$1532 = _tmp$1533 & capacity_mask$1534;
        psl$545 = _tmp$1531;
        idx$546 = _tmp$1532;
        entry$547 = _curr_entry$552;
        continue;
      } else {
        int32_t _tmp$1536 = psl$545 + 1;
        int32_t _tmp$1538 = idx$546 + 1;
        int32_t capacity_mask$1539 = self$549->$3;
        int32_t _tmp$1537 = _tmp$1538 & capacity_mask$1539;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2416 =
          entry$547;
        psl$545 = _tmp$1536;
        idx$546 = _tmp$1537;
        entry$547 = _tmp$2416;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$539,
  int32_t idx$544,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$543
) {
  int32_t psl$1529 = entry$543->$2;
  int32_t _tmp$1525 = psl$1529 + 1;
  int32_t _tmp$1527 = idx$544 + 1;
  int32_t capacity_mask$1528 = self$539->$3;
  int32_t _tmp$1526 = _tmp$1527 & capacity_mask$1528;
  int32_t psl$535 = _tmp$1525;
  int32_t idx$536 = _tmp$1526;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$537 =
    entry$543;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2137 =
      self$539->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1524 =
      _field$2137;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2136;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$538;
    if (idx$536 < 0 || idx$536 >= Moonbit_array_length(entries$1524)) {
      moonbit_panic();
    }
    _tmp$2136
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1524[
        idx$536
      ];
    _bind$538 = _tmp$2136;
    if (_bind$538 == 0) {
      entry$537->$2 = psl$535;
      $$moonbitlang$core$builtin$Map$$set_entry$0(
        self$539, entry$537, idx$536
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$541 =
        _bind$538;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$542 =
        _Some$541;
      int32_t psl$1514 = _curr_entry$542->$2;
      if (psl$535 > psl$1514) {
        int32_t psl$1519;
        int32_t _tmp$1515;
        int32_t _tmp$1517;
        int32_t capacity_mask$1518;
        int32_t _tmp$1516;
        entry$537->$2 = psl$535;
        moonbit_incref(_curr_entry$542);
        moonbit_incref(self$539);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$539, entry$537, idx$536
        );
        psl$1519 = _curr_entry$542->$2;
        _tmp$1515 = psl$1519 + 1;
        _tmp$1517 = idx$536 + 1;
        capacity_mask$1518 = self$539->$3;
        _tmp$1516 = _tmp$1517 & capacity_mask$1518;
        psl$535 = _tmp$1515;
        idx$536 = _tmp$1516;
        entry$537 = _curr_entry$542;
        continue;
      } else {
        int32_t _tmp$1520 = psl$535 + 1;
        int32_t _tmp$1522 = idx$536 + 1;
        int32_t capacity_mask$1523 = self$539->$3;
        int32_t _tmp$1521 = _tmp$1522 & capacity_mask$1523;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2418 =
          entry$537;
        psl$535 = _tmp$1520;
        idx$536 = _tmp$1521;
        entry$537 = _tmp$2418;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$529,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$531,
  int32_t new_idx$530
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2140 =
    self$529->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1512 =
    _field$2140;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1513;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2139;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2138;
  int32_t _cnt$2322;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$532;
  moonbit_incref(entry$531);
  _tmp$1513 = entry$531;
  if (new_idx$530 < 0 || new_idx$530 >= Moonbit_array_length(entries$1512)) {
    moonbit_panic();
  }
  _old$2139
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1512[
      new_idx$530
    ];
  if (_old$2139) {
    moonbit_decref(_old$2139);
  }
  entries$1512[new_idx$530] = _tmp$1513;
  _field$2138 = entry$531->$1;
  _cnt$2322 = Moonbit_object_header(entry$531)->rc;
  if (_cnt$2322 > 1) {
    int32_t _new_cnt$2325;
    if (_field$2138) {
      moonbit_incref(_field$2138);
    }
    _new_cnt$2325 = _cnt$2322 - 1;
    Moonbit_object_header(entry$531)->rc = _new_cnt$2325;
  } else if (_cnt$2322 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2324 =
      entry$531->$5;
    moonbit_string_t _field$2323;
    moonbit_decref(_field$2324);
    _field$2323 = entry$531->$4;
    moonbit_decref(_field$2323);
    moonbit_free(entry$531);
  }
  _bind$532 = _field$2138;
  if (_bind$532 == 0) {
    if (_bind$532) {
      moonbit_decref(_bind$532);
    }
    self$529->$6 = new_idx$530;
    moonbit_decref(self$529);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$533;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$534;
    moonbit_decref(self$529);
    _Some$533 = _bind$532;
    _next$534 = _Some$533;
    _next$534->$0 = new_idx$530;
    moonbit_decref(_next$534);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$523,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$525,
  int32_t new_idx$524
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2143 =
    self$523->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1510 =
    _field$2143;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1511;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2142;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2141;
  int32_t _cnt$2326;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$526;
  moonbit_incref(entry$525);
  _tmp$1511 = entry$525;
  if (new_idx$524 < 0 || new_idx$524 >= Moonbit_array_length(entries$1510)) {
    moonbit_panic();
  }
  _old$2142
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1510[
      new_idx$524
    ];
  if (_old$2142) {
    moonbit_decref(_old$2142);
  }
  entries$1510[new_idx$524] = _tmp$1511;
  _field$2141 = entry$525->$1;
  _cnt$2326 = Moonbit_object_header(entry$525)->rc;
  if (_cnt$2326 > 1) {
    int32_t _new_cnt$2329;
    if (_field$2141) {
      moonbit_incref(_field$2141);
    }
    _new_cnt$2329 = _cnt$2326 - 1;
    Moonbit_object_header(entry$525)->rc = _new_cnt$2329;
  } else if (_cnt$2326 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2328 =
      entry$525->$5;
    moonbit_string_t _field$2327;
    moonbit_decref(_field$2328);
    _field$2327 = entry$525->$4;
    moonbit_decref(_field$2327);
    moonbit_free(entry$525);
  }
  _bind$526 = _field$2141;
  if (_bind$526 == 0) {
    if (_bind$526) {
      moonbit_decref(_bind$526);
    }
    self$523->$6 = new_idx$524;
    moonbit_decref(self$523);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$527;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$528;
    moonbit_decref(self$523);
    _Some$527 = _bind$526;
    _next$528 = _Some$527;
    _next$528->$0 = new_idx$524;
    moonbit_decref(_next$528);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$517,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$519,
  int32_t new_idx$518
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2146 =
    self$517->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1508 =
    _field$2146;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1509;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2145;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2144;
  int32_t _cnt$2330;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$520;
  moonbit_incref(entry$519);
  _tmp$1509 = entry$519;
  if (new_idx$518 < 0 || new_idx$518 >= Moonbit_array_length(entries$1508)) {
    moonbit_panic();
  }
  _old$2145
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1508[
      new_idx$518
    ];
  if (_old$2145) {
    moonbit_decref(_old$2145);
  }
  entries$1508[new_idx$518] = _tmp$1509;
  _field$2144 = entry$519->$1;
  _cnt$2330 = Moonbit_object_header(entry$519)->rc;
  if (_cnt$2330 > 1) {
    int32_t _new_cnt$2333;
    if (_field$2144) {
      moonbit_incref(_field$2144);
    }
    _new_cnt$2333 = _cnt$2330 - 1;
    Moonbit_object_header(entry$519)->rc = _new_cnt$2333;
  } else if (_cnt$2330 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2332 =
      entry$519->$5;
    moonbit_string_t _field$2331;
    moonbit_decref(_field$2332);
    _field$2331 = entry$519->$4;
    moonbit_decref(_field$2331);
    moonbit_free(entry$519);
  }
  _bind$520 = _field$2144;
  if (_bind$520 == 0) {
    if (_bind$520) {
      moonbit_decref(_bind$520);
    }
    self$517->$6 = new_idx$518;
    moonbit_decref(self$517);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$521;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$522;
    moonbit_decref(self$517);
    _Some$521 = _bind$520;
    _next$522 = _Some$521;
    _next$522->$0 = new_idx$518;
    moonbit_decref(_next$522);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$514,
  int32_t idx$516,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$515
) {
  int32_t _bind$513 = self$514->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2148;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1504;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1505;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2147;
  int32_t size$1507;
  int32_t _tmp$1506;
  switch (_bind$513) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1499;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2149;
      moonbit_incref(entry$515);
      _tmp$1499 = entry$515;
      _old$2149 = self$514->$5;
      if (_old$2149) {
        moonbit_decref(_old$2149);
      }
      self$514->$5 = _tmp$1499;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2152 =
        self$514->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1503 =
        _field$2152;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2151;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1502;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1500;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1501;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2150;
      if (_bind$513 < 0 || _bind$513 >= Moonbit_array_length(entries$1503)) {
        moonbit_panic();
      }
      _tmp$2151
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1503[
          _bind$513
        ];
      _tmp$1502 = _tmp$2151;
      if (_tmp$1502) {
        moonbit_incref(_tmp$1502);
      }
      _tmp$1500 = $Option$$unwrap$2(_tmp$1502);
      moonbit_incref(entry$515);
      _tmp$1501 = entry$515;
      _old$2150 = _tmp$1500->$1;
      if (_old$2150) {
        moonbit_decref(_old$2150);
      }
      _tmp$1500->$1 = _tmp$1501;
      moonbit_decref(_tmp$1500);
      break;
    }
  }
  self$514->$6 = idx$516;
  _field$2148 = self$514->$0;
  entries$1504 = _field$2148;
  _tmp$1505 = entry$515;
  if (idx$516 < 0 || idx$516 >= Moonbit_array_length(entries$1504)) {
    moonbit_panic();
  }
  _old$2147
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1504[
      idx$516
    ];
  if (_old$2147) {
    moonbit_decref(_old$2147);
  }
  entries$1504[idx$516] = _tmp$1505;
  size$1507 = self$514->$1;
  _tmp$1506 = size$1507 + 1;
  self$514->$1 = _tmp$1506;
  moonbit_decref(self$514);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$510,
  int32_t idx$512,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$511
) {
  int32_t _bind$509 = self$510->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2154;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1495;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1496;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2153;
  int32_t size$1498;
  int32_t _tmp$1497;
  switch (_bind$509) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1490;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2155;
      moonbit_incref(entry$511);
      _tmp$1490 = entry$511;
      _old$2155 = self$510->$5;
      if (_old$2155) {
        moonbit_decref(_old$2155);
      }
      self$510->$5 = _tmp$1490;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2158 =
        self$510->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1494 =
        _field$2158;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2157;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1493;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1491;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1492;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2156;
      if (_bind$509 < 0 || _bind$509 >= Moonbit_array_length(entries$1494)) {
        moonbit_panic();
      }
      _tmp$2157
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1494[
          _bind$509
        ];
      _tmp$1493 = _tmp$2157;
      if (_tmp$1493) {
        moonbit_incref(_tmp$1493);
      }
      _tmp$1491 = $Option$$unwrap$1(_tmp$1493);
      moonbit_incref(entry$511);
      _tmp$1492 = entry$511;
      _old$2156 = _tmp$1491->$1;
      if (_old$2156) {
        moonbit_decref(_old$2156);
      }
      _tmp$1491->$1 = _tmp$1492;
      moonbit_decref(_tmp$1491);
      break;
    }
  }
  self$510->$6 = idx$512;
  _field$2154 = self$510->$0;
  entries$1495 = _field$2154;
  _tmp$1496 = entry$511;
  if (idx$512 < 0 || idx$512 >= Moonbit_array_length(entries$1495)) {
    moonbit_panic();
  }
  _old$2153
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1495[
      idx$512
    ];
  if (_old$2153) {
    moonbit_decref(_old$2153);
  }
  entries$1495[idx$512] = _tmp$1496;
  size$1498 = self$510->$1;
  _tmp$1497 = size$1498 + 1;
  self$510->$1 = _tmp$1497;
  moonbit_decref(self$510);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$506,
  int32_t idx$508,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$507
) {
  int32_t _bind$505 = self$506->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2160;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1486;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1487;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2159;
  int32_t size$1489;
  int32_t _tmp$1488;
  switch (_bind$505) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1481;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2161;
      moonbit_incref(entry$507);
      _tmp$1481 = entry$507;
      _old$2161 = self$506->$5;
      if (_old$2161) {
        moonbit_decref(_old$2161);
      }
      self$506->$5 = _tmp$1481;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2164 =
        self$506->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1485 =
        _field$2164;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2163;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1484;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1482;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1483;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2162;
      if (_bind$505 < 0 || _bind$505 >= Moonbit_array_length(entries$1485)) {
        moonbit_panic();
      }
      _tmp$2163
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1485[
          _bind$505
        ];
      _tmp$1484 = _tmp$2163;
      if (_tmp$1484) {
        moonbit_incref(_tmp$1484);
      }
      _tmp$1482 = $Option$$unwrap$0(_tmp$1484);
      moonbit_incref(entry$507);
      _tmp$1483 = entry$507;
      _old$2162 = _tmp$1482->$1;
      if (_old$2162) {
        moonbit_decref(_old$2162);
      }
      _tmp$1482->$1 = _tmp$1483;
      moonbit_decref(_tmp$1482);
      break;
    }
  }
  self$506->$6 = idx$508;
  _field$2160 = self$506->$0;
  entries$1486 = _field$2160;
  _tmp$1487 = entry$507;
  if (idx$508 < 0 || idx$508 >= Moonbit_array_length(entries$1486)) {
    moonbit_panic();
  }
  _old$2159
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1486[
      idx$508
    ];
  if (_old$2159) {
    moonbit_decref(_old$2159);
  }
  entries$1486[idx$508] = _tmp$1487;
  size$1489 = self$506->$1;
  _tmp$1488 = size$1489 + 1;
  self$506->$1 = _tmp$1488;
  moonbit_decref(self$506);
  return 0;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$500
) {
  int32_t capacity$499 = $Int$$next_power_of_two(capacity$500);
  int32_t _bind$501 = capacity$499 - 1;
  int32_t _bind$502 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$499);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1480 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$503 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$499, _tmp$1480
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$504 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2419 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2419)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2419->$0 = _bind$503;
  _block$2419->$1 = 0;
  _block$2419->$2 = capacity$499;
  _block$2419->$3 = _bind$501;
  _block$2419->$4 = _bind$502;
  _block$2419->$5 = _bind$504;
  _block$2419->$6 = -1;
  return _block$2419;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$494
) {
  int32_t capacity$493 = $Int$$next_power_of_two(capacity$494);
  int32_t _bind$495 = capacity$493 - 1;
  int32_t _bind$496 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$493);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1479 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$497 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$493, _tmp$1479
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$498 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2420 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2420)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2420->$0 = _bind$497;
  _block$2420->$1 = 0;
  _block$2420->$2 = capacity$493;
  _block$2420->$3 = _bind$495;
  _block$2420->$4 = _bind$496;
  _block$2420->$5 = _bind$498;
  _block$2420->$6 = -1;
  return _block$2420;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$488
) {
  int32_t capacity$487 = $Int$$next_power_of_two(capacity$488);
  int32_t _bind$489 = capacity$487 - 1;
  int32_t _bind$490 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$487);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1478 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$491 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$487, _tmp$1478
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$492 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2421 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2421)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2421->$0 = _bind$491;
  _block$2421->$1 = 0;
  _block$2421->$2 = capacity$487;
  _block$2421->$3 = _bind$489;
  _block$2421->$4 = _bind$490;
  _block$2421->$5 = _bind$492;
  _block$2421->$6 = -1;
  return _block$2421;
}

int32_t $Int$$next_power_of_two(int32_t self$486) {
  if (self$486 >= 0) {
    int32_t _tmp$1477;
    int32_t _tmp$1476;
    int32_t _tmp$1475;
    int32_t _tmp$1474;
    if (self$486 <= 1) {
      return 1;
    }
    if (self$486 > 1073741824) {
      return 1073741824;
    }
    _tmp$1477 = self$486 - 1;
    _tmp$1476 = moonbit_clz32(_tmp$1477);
    _tmp$1475 = _tmp$1476 - 1;
    _tmp$1474 = 2147483647 >> (_tmp$1475 & 31);
    return _tmp$1474 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$485) {
  int32_t _tmp$1473 = capacity$485 * 13;
  return _tmp$1473 / 16;
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$483
) {
  if (self$483 == 0) {
    if (self$483) {
      moonbit_decref(self$483);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$484 =
      self$483;
    return _Some$484;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$481
) {
  if (self$481 == 0) {
    if (self$481) {
      moonbit_decref(self$481);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$482 =
      self$481;
    return _Some$482;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$479
) {
  if (self$479 == 0) {
    if (self$479) {
      moonbit_decref(self$479);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$480 =
      self$479;
    return _Some$480;
  }
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$478
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1472 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$478);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$1472);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$477
) {
  moonbit_string_t* _field$2166 = self$477->$0;
  moonbit_string_t* buf$1470 = _field$2166;
  int32_t _field$2165 = self$477->$1;
  int32_t _cnt$2334 = Moonbit_object_header(self$477)->rc;
  int32_t len$1471;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$1469;
  if (_cnt$2334 > 1) {
    int32_t _new_cnt$2335;
    moonbit_incref(buf$1470);
    _new_cnt$2335 = _cnt$2334 - 1;
    Moonbit_object_header(self$477)->rc = _new_cnt$2335;
  } else if (_cnt$2334 == 1) {
    moonbit_free(self$477);
  }
  len$1471 = _field$2165;
  _tmp$1469
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$1471, buf$1470
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$1469);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$475
) {
  struct $Ref$3c$Int$3e$* i$474 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$2422;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1458;
  Moonbit_object_header(i$474)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$474->$0 = 0;
  _closure$2422
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$2422)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$2422->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$2422->$0_0 = self$475.$0;
  _closure$2422->$0_1 = self$475.$1;
  _closure$2422->$0_2 = self$475.$2;
  _closure$2422->$1 = i$474;
  _tmp$1458 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$2422;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$1458);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1459
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$1460 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$1459;
  struct $Ref$3c$Int$3e$* _field$2171 = _casted_env$1460->$1;
  struct $Ref$3c$Int$3e$* i$474 = _field$2171;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$2170 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$1460->$0_1, _casted_env$1460->$0_2, _casted_env$1460->$0_0
    };
  int32_t _cnt$2336 = Moonbit_object_header(_casted_env$1460)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$475;
  int32_t val$1461;
  int32_t _tmp$1462;
  if (_cnt$2336 > 1) {
    int32_t _new_cnt$2337;
    moonbit_incref(i$474);
    moonbit_incref(_field$2170.$0);
    _new_cnt$2337 = _cnt$2336 - 1;
    Moonbit_object_header(_casted_env$1460)->rc = _new_cnt$2337;
  } else if (_cnt$2336 == 1) {
    moonbit_free(_casted_env$1460);
  }
  self$475 = _field$2170;
  val$1461 = i$474->$0;
  moonbit_incref(self$475.$0);
  _tmp$1462 = $$moonbitlang$core$builtin$ArrayView$$length$3(self$475);
  if (val$1461 < _tmp$1462) {
    moonbit_string_t* _field$2169 = self$475.$0;
    moonbit_string_t* buf$1465 = _field$2169;
    int32_t _field$2168 = self$475.$1;
    int32_t start$1467 = _field$2168;
    int32_t val$1468 = i$474->$0;
    int32_t _tmp$1466 = start$1467 + val$1468;
    moonbit_string_t _tmp$2167 = (moonbit_string_t)buf$1465[_tmp$1466];
    moonbit_string_t elem$476;
    int32_t val$1464;
    int32_t _tmp$1463;
    moonbit_incref(_tmp$2167);
    moonbit_decref(buf$1465);
    elem$476 = _tmp$2167;
    val$1464 = i$474->$0;
    _tmp$1463 = val$1464 + 1;
    i$474->$0 = _tmp$1463;
    moonbit_decref(i$474);
    return elem$476;
  } else {
    moonbit_decref(self$475.$0);
    moonbit_decref(i$474);
    return 0;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$473
) {
  return self$473;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$472,
  struct $$moonbitlang$core$builtin$Logger logger$471
) {
  moonbit_string_t _tmp$1457 = $Int$$to_string$inner(self$472, 10);
  logger$471.$0->$method_0(logger$471.$1, _tmp$1457);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$469,
  struct $$3c$String$3e$$3d$$3e$Int* f$470
) {
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _func$468 = self$469;
  return _func$468->code(_func$468, f$470);
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$465,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$467
) {
  int32_t len$1452 = self$465->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1454;
  int32_t _tmp$2174;
  int32_t _tmp$1453;
  int32_t length$466;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2173;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$1455;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2172;
  int32_t _tmp$1456;
  moonbit_incref(self$465);
  _tmp$1454 = $$moonbitlang$core$builtin$Array$$buffer$2(self$465);
  _tmp$2174 = Moonbit_array_length(_tmp$1454);
  moonbit_decref(_tmp$1454);
  _tmp$1453 = _tmp$2174;
  if (len$1452 == _tmp$1453) {
    moonbit_incref(self$465);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$465);
  }
  length$466 = self$465->$1;
  _field$2173 = self$465->$0;
  buf$1455 = _field$2173;
  _old$2172
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$1455[
      length$466
    ];
  if (_old$2172) {
    moonbit_decref(_old$2172);
  }
  buf$1455[length$466] = value$467;
  _tmp$1456 = length$466 + 1;
  self$465->$1 = _tmp$1456;
  moonbit_decref(self$465);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$462,
  struct $$3c$String$2a$Int$3e$* value$464
) {
  int32_t len$1447 = self$462->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$1449;
  int32_t _tmp$2177;
  int32_t _tmp$1448;
  int32_t length$463;
  struct $$3c$String$2a$Int$3e$** _field$2176;
  struct $$3c$String$2a$Int$3e$** buf$1450;
  struct $$3c$String$2a$Int$3e$* _old$2175;
  int32_t _tmp$1451;
  moonbit_incref(self$462);
  _tmp$1449 = $$moonbitlang$core$builtin$Array$$buffer$0(self$462);
  _tmp$2177 = Moonbit_array_length(_tmp$1449);
  moonbit_decref(_tmp$1449);
  _tmp$1448 = _tmp$2177;
  if (len$1447 == _tmp$1448) {
    moonbit_incref(self$462);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$462);
  }
  length$463 = self$462->$1;
  _field$2176 = self$462->$0;
  buf$1450 = _field$2176;
  _old$2175 = (struct $$3c$String$2a$Int$3e$*)buf$1450[length$463];
  if (_old$2175) {
    moonbit_decref(_old$2175);
  }
  buf$1450[length$463] = value$464;
  _tmp$1451 = length$463 + 1;
  self$462->$1 = _tmp$1451;
  moonbit_decref(self$462);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$459,
  moonbit_string_t value$461
) {
  int32_t len$1442 = self$459->$1;
  moonbit_string_t* _tmp$1444;
  int32_t _tmp$2180;
  int32_t _tmp$1443;
  int32_t length$460;
  moonbit_string_t* _field$2179;
  moonbit_string_t* buf$1445;
  moonbit_string_t _old$2178;
  int32_t _tmp$1446;
  moonbit_incref(self$459);
  _tmp$1444 = $$moonbitlang$core$builtin$Array$$buffer$1(self$459);
  _tmp$2180 = Moonbit_array_length(_tmp$1444);
  moonbit_decref(_tmp$1444);
  _tmp$1443 = _tmp$2180;
  if (len$1442 == _tmp$1443) {
    moonbit_incref(self$459);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$459);
  }
  length$460 = self$459->$1;
  _field$2179 = self$459->$0;
  buf$1445 = _field$2179;
  _old$2178 = (moonbit_string_t)buf$1445[length$460];
  moonbit_decref(_old$2178);
  buf$1445[length$460] = value$461;
  _tmp$1446 = length$460 + 1;
  self$459->$1 = _tmp$1446;
  moonbit_decref(self$459);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$457
) {
  int32_t old_cap$456 = self$457->$1;
  int32_t new_cap$458;
  if (old_cap$456 == 0) {
    new_cap$458 = 8;
  } else {
    new_cap$458 = old_cap$456 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$457, new_cap$458);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$454
) {
  int32_t old_cap$453 = self$454->$1;
  int32_t new_cap$455;
  if (old_cap$453 == 0) {
    new_cap$455 = 8;
  } else {
    new_cap$455 = old_cap$453 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$454, new_cap$455);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$451
) {
  int32_t old_cap$450 = self$451->$1;
  int32_t new_cap$452;
  if (old_cap$450 == 0) {
    new_cap$452 = 8;
  } else {
    new_cap$452 = old_cap$450 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$451, new_cap$452);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$447,
  int32_t new_capacity$445
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** new_buf$444 =
    (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_make_ref_array(
      new_capacity$445, 0
    );
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2182 =
    self$447->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$446 =
    _field$2182;
  int32_t old_cap$448 = Moonbit_array_length(old_buf$446);
  int32_t copy_len$449;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$2181;
  if (old_cap$448 < new_capacity$445) {
    copy_len$449 = old_cap$448;
  } else {
    copy_len$449 = new_capacity$445;
  }
  moonbit_incref(old_buf$446);
  moonbit_incref(new_buf$444);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$444, 0, old_buf$446, 0, copy_len$449
  );
  _old$2181 = self$447->$0;
  moonbit_decref(_old$2181);
  self$447->$0 = new_buf$444;
  moonbit_decref(self$447);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$441,
  int32_t new_capacity$439
) {
  struct $$3c$String$2a$Int$3e$** new_buf$438 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_make_ref_array(
      new_capacity$439, 0
    );
  struct $$3c$String$2a$Int$3e$** _field$2184 = self$441->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$440 = _field$2184;
  int32_t old_cap$442 = Moonbit_array_length(old_buf$440);
  int32_t copy_len$443;
  struct $$3c$String$2a$Int$3e$** _old$2183;
  if (old_cap$442 < new_capacity$439) {
    copy_len$443 = old_cap$442;
  } else {
    copy_len$443 = new_capacity$439;
  }
  moonbit_incref(old_buf$440);
  moonbit_incref(new_buf$438);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$438, 0, old_buf$440, 0, copy_len$443
  );
  _old$2183 = self$441->$0;
  moonbit_decref(_old$2183);
  self$441->$0 = new_buf$438;
  moonbit_decref(self$441);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$435,
  int32_t new_capacity$433
) {
  moonbit_string_t* new_buf$432 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$433, (moonbit_string_t)moonbit_string_literal_3.data
    );
  moonbit_string_t* _field$2186 = self$435->$0;
  moonbit_string_t* old_buf$434 = _field$2186;
  int32_t old_cap$436 = Moonbit_array_length(old_buf$434);
  int32_t copy_len$437;
  moonbit_string_t* _old$2185;
  if (old_cap$436 < new_capacity$433) {
    copy_len$437 = old_cap$436;
  } else {
    copy_len$437 = new_capacity$433;
  }
  moonbit_incref(old_buf$434);
  moonbit_incref(new_buf$432);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$432, 0, old_buf$434, 0, copy_len$437
  );
  _old$2185 = self$435->$0;
  moonbit_decref(_old$2185);
  self$435->$0 = new_buf$432;
  moonbit_decref(self$435);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$431
) {
  if (capacity$431 == 0) {
    moonbit_string_t* _tmp$1440 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2423 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2423)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2423->$0 = _tmp$1440;
    _block$2423->$1 = 0;
    return _block$2423;
  } else {
    moonbit_string_t* _tmp$1441 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$431, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2424 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2424)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2424->$0 = _tmp$1441;
    _block$2424->$1 = 0;
    return _block$2424;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$429,
  struct $StringView str$430
) {
  int32_t len$1428 = self$429->$1;
  int32_t _tmp$1430;
  int32_t _tmp$1429;
  int32_t _tmp$1427;
  moonbit_bytes_t _field$2187;
  moonbit_bytes_t data$1431;
  int32_t len$1432;
  moonbit_string_t _tmp$1433;
  int32_t _tmp$1434;
  int32_t _tmp$1435;
  int32_t len$1437;
  int32_t _tmp$1439;
  int32_t _tmp$1438;
  int32_t _tmp$1436;
  moonbit_incref(str$430.$0);
  _tmp$1430 = $StringView$$length(str$430);
  _tmp$1429 = _tmp$1430 * 2;
  _tmp$1427 = len$1428 + _tmp$1429;
  moonbit_incref(self$429);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$429, _tmp$1427
  );
  _field$2187 = self$429->$0;
  data$1431 = _field$2187;
  len$1432 = self$429->$1;
  moonbit_incref(data$1431);
  moonbit_incref(str$430.$0);
  _tmp$1433 = $StringView$$data(str$430);
  moonbit_incref(str$430.$0);
  _tmp$1434 = $StringView$$start_offset(str$430);
  moonbit_incref(str$430.$0);
  _tmp$1435 = $StringView$$length(str$430);
  $FixedArray$$blit_from_string(
    data$1431, len$1432, _tmp$1433, _tmp$1434, _tmp$1435
  );
  len$1437 = self$429->$1;
  _tmp$1439 = $StringView$$length(str$430);
  _tmp$1438 = _tmp$1439 * 2;
  _tmp$1436 = len$1437 + _tmp$1438;
  self$429->$1 = _tmp$1436;
  moonbit_decref(self$429);
  return 0;
}

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$426,
  int32_t i$427,
  int32_t start_offset$428,
  int64_t end_offset$424
) {
  int32_t end_offset$423;
  if (end_offset$424 == 4294967296ll) {
    end_offset$423 = Moonbit_array_length(self$426);
  } else {
    int64_t _Some$425 = end_offset$424;
    end_offset$423 = (int32_t)_Some$425;
  }
  if (i$427 >= 0) {
    return $String$$offset_of_nth_char_forward(
             self$426, i$427, start_offset$428, end_offset$423
           );
  } else {
    int32_t _tmp$1426 = -i$427;
    return $String$$offset_of_nth_char_backward(
             self$426, _tmp$1426, start_offset$428, end_offset$423
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$421,
  int32_t n$419,
  int32_t start_offset$415,
  int32_t end_offset$416
) {
  int32_t _if_result$2425;
  if (start_offset$415 >= 0) {
    _if_result$2425 = start_offset$415 <= end_offset$416;
  } else {
    _if_result$2425 = 0;
  }
  if (_if_result$2425) {
    int32_t utf16_offset$417 = start_offset$415;
    int32_t char_count$418 = 0;
    int32_t _tmp$1424;
    int32_t _if_result$2428;
    while (1) {
      int32_t _tmp$1418 = utf16_offset$417;
      int32_t _if_result$2427;
      if (_tmp$1418 < end_offset$416) {
        int32_t _tmp$1417 = char_count$418;
        _if_result$2427 = _tmp$1417 < n$419;
      } else {
        _if_result$2427 = 0;
      }
      if (_if_result$2427) {
        int32_t _tmp$1422 = utf16_offset$417;
        int32_t c$420 = self$421[_tmp$1422];
        int32_t _tmp$1421;
        if ($Int$$is_leading_surrogate(c$420)) {
          int32_t _tmp$1419 = utf16_offset$417;
          utf16_offset$417 = _tmp$1419 + 2;
        } else {
          int32_t _tmp$1420 = utf16_offset$417;
          utf16_offset$417 = _tmp$1420 + 1;
        }
        _tmp$1421 = char_count$418;
        char_count$418 = _tmp$1421 + 1;
        continue;
      } else {
        moonbit_decref(self$421);
      }
      break;
    }
    _tmp$1424 = char_count$418;
    if (_tmp$1424 < n$419) {
      _if_result$2428 = 1;
    } else {
      int32_t _tmp$1423 = utf16_offset$417;
      _if_result$2428 = _tmp$1423 >= end_offset$416;
    }
    if (_if_result$2428) {
      return 4294967296ll;
    } else {
      int32_t _tmp$1425 = utf16_offset$417;
      return (int64_t)_tmp$1425;
    }
  } else {
    moonbit_decref(self$421);
    return $moonbitlang$core$builtin$abort$3(
             (moonbit_string_t)moonbit_string_literal_14.data,
               (moonbit_string_t)moonbit_string_literal_15.data
           );
  }
}

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$413,
  int32_t n$411,
  int32_t start_offset$410,
  int32_t end_offset$409
) {
  int32_t char_count$407 = 0;
  int32_t utf16_offset$408 = end_offset$409;
  int32_t _tmp$1415;
  int32_t _if_result$2431;
  while (1) {
    int32_t _tmp$1408 = utf16_offset$408;
    int32_t _tmp$1407 = _tmp$1408 - 1;
    int32_t _if_result$2430;
    if (_tmp$1407 >= start_offset$410) {
      int32_t _tmp$1406 = char_count$407;
      _if_result$2430 = _tmp$1406 < n$411;
    } else {
      _if_result$2430 = 0;
    }
    if (_if_result$2430) {
      int32_t _tmp$1413 = utf16_offset$408;
      int32_t _tmp$1412 = _tmp$1413 - 1;
      int32_t c$412 = self$413[_tmp$1412];
      int32_t _tmp$1411;
      if ($Int$$is_trailing_surrogate(c$412)) {
        int32_t _tmp$1409 = utf16_offset$408;
        utf16_offset$408 = _tmp$1409 - 2;
      } else {
        int32_t _tmp$1410 = utf16_offset$408;
        utf16_offset$408 = _tmp$1410 - 1;
      }
      _tmp$1411 = char_count$407;
      char_count$407 = _tmp$1411 + 1;
      continue;
    } else {
      moonbit_decref(self$413);
    }
    break;
  }
  _tmp$1415 = char_count$407;
  if (_tmp$1415 < n$411) {
    _if_result$2431 = 1;
  } else {
    int32_t _tmp$1414 = utf16_offset$408;
    _if_result$2431 = _tmp$1414 < start_offset$410;
  }
  if (_if_result$2431) {
    return 4294967296ll;
  } else {
    int32_t _tmp$1416 = utf16_offset$408;
    return (int64_t)_tmp$1416;
  }
}

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$399,
  int32_t len$402,
  int32_t start_offset$406,
  int64_t end_offset$397
) {
  int32_t end_offset$396;
  int32_t index$400;
  int32_t count$401;
  if (end_offset$397 == 4294967296ll) {
    end_offset$396 = Moonbit_array_length(self$399);
  } else {
    int64_t _Some$398 = end_offset$397;
    end_offset$396 = (int32_t)_Some$398;
  }
  index$400 = start_offset$406;
  count$401 = 0;
  while (1) {
    int32_t _if_result$2433;
    if (index$400 < end_offset$396) {
      _if_result$2433 = count$401 < len$402;
    } else {
      _if_result$2433 = 0;
    }
    if (_if_result$2433) {
      int32_t c1$403 = self$399[index$400];
      int32_t _if_result$2434;
      int32_t _tmp$1404;
      int32_t _tmp$1405;
      if ($Int$$is_leading_surrogate(c1$403)) {
        int32_t _tmp$1400 = index$400 + 1;
        _if_result$2434 = _tmp$1400 < end_offset$396;
      } else {
        _if_result$2434 = 0;
      }
      if (_if_result$2434) {
        int32_t _tmp$1403 = index$400 + 1;
        int32_t c2$404 = self$399[_tmp$1403];
        if ($Int$$is_trailing_surrogate(c2$404)) {
          int32_t _tmp$1401 = index$400 + 2;
          int32_t _tmp$1402 = count$401 + 1;
          index$400 = _tmp$1401;
          count$401 = _tmp$1402;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_16.data,
              (moonbit_string_t)moonbit_string_literal_17.data
          );
        }
      }
      _tmp$1404 = index$400 + 1;
      _tmp$1405 = count$401 + 1;
      index$400 = _tmp$1404;
      count$401 = _tmp$1405;
      continue;
    } else {
      moonbit_decref(self$399);
      return count$401 >= len$402;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$395
) {
  int32_t end$1398 = self$395.$2;
  int32_t _field$2188 = self$395.$1;
  int32_t start$1399;
  moonbit_decref(self$395.$0);
  start$1399 = _field$2188;
  return end$1398 - start$1399;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$394
) {
  int32_t end$1396 = self$394.$2;
  int32_t _field$2189 = self$394.$1;
  int32_t start$1397;
  moonbit_decref(self$394.$0);
  start$1397 = _field$2189;
  return end$1396 - start$1397;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$393
) {
  int32_t end$1394 = self$393.$2;
  int32_t _field$2190 = self$393.$1;
  int32_t start$1395;
  moonbit_decref(self$393.$0);
  start$1395 = _field$2190;
  return end$1394 - start$1395;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$392
) {
  int32_t end$1392 = self$392.$2;
  int32_t _field$2191 = self$392.$1;
  int32_t start$1393;
  moonbit_decref(self$392.$0);
  start$1393 = _field$2191;
  return end$1392 - start$1393;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$2435 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$2435)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2435->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$2435->$0 = self$387;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$2435;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1390,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$385
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$1391 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$1390;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$2192 =
    _casted_env$1391->$0;
  int32_t _cnt$2338 = Moonbit_object_header(_casted_env$1391)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387;
  if (_cnt$2338 > 1) {
    int32_t _new_cnt$2339;
    moonbit_incref(_field$2192);
    _new_cnt$2339 = _cnt$2338 - 1;
    Moonbit_object_header(_casted_env$1391)->rc = _new_cnt$2339;
  } else if (_cnt$2338 == 1) {
    moonbit_free(_casted_env$1391);
  }
  self$387 = _field$2192;
  while (1) {
    moonbit_string_t _bind$386;
    moonbit_incref(self$387);
    _bind$386 = $$moonbitlang$core$builtin$Iterator$$next$0(self$387);
    if (_bind$386 == 0) {
      moonbit_decref(self$387);
      if (_bind$386) {
        moonbit_decref(_bind$386);
      }
      moonbit_decref(yield_$385);
      return 1;
    } else {
      moonbit_string_t _Some$388 = _bind$386;
      moonbit_string_t _x$389 = _Some$388;
      int32_t _bind$390;
      moonbit_incref(yield_$385);
      _bind$390 = yield_$385->code(yield_$385, _x$389);
      switch (_bind$390) {
        case 1:
          break;
        default: {
          moonbit_decref(self$387);
          moonbit_decref(yield_$385);
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$384
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _func$383 = self$384;
  return _func$383->code(_func$383);
}

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$375,
  struct $$moonbitlang$core$builtin$Logger logger$373
) {
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$374;
  int32_t len$376;
  int32_t i$377;
  int32_t seg$378;
  if (logger$373.$1) {
    moonbit_incref(logger$373.$1);
  }
  logger$373.$0->$method_3(logger$373.$1, 34);
  if (logger$373.$1) {
    moonbit_incref(logger$373.$1);
  }
  moonbit_incref(self$375);
  _env$374
  = (struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$)
    );
  Moonbit_object_header(_env$374)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$, $0_0
    )
    >> 2,
      3,
      0
  );
  _env$374->$0_0 = logger$373.$0;
  _env$374->$0_1 = logger$373.$1;
  _env$374->$1 = self$375;
  len$376 = Moonbit_array_length(self$375);
  i$377 = 0;
  seg$378 = 0;
  $$2a$for$379:;
  while (1) {
    int32_t code$380;
    int32_t c$382;
    struct $$moonbitlang$core$builtin$Logger _bind$1372;
    int32_t _tmp$1373;
    int32_t _tmp$1374;
    int32_t _tmp$1375;
    int32_t _tmp$2440;
    int32_t _tmp$2441;
    if (i$377 >= len$376) {
      moonbit_decref(self$375);
      $moonbitlang$core$builtin$output$flush_segment$7c$3831(
        _env$374, seg$378, i$377
      );
      break;
    }
    code$380 = self$375[i$377];
    switch (code$380) {
      case 34: {
        c$382 = code$380;
        goto $join$381;
        break;
      }
      
      case 92: {
        c$382 = code$380;
        goto $join$381;
        break;
      }
      
      case 10: {
        int32_t _tmp$1376;
        int32_t _tmp$1377;
        moonbit_incref(_env$374);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$374, seg$378, i$377
        );
        if (logger$373.$1) {
          moonbit_incref(logger$373.$1);
        }
        logger$373.$0->$method_0(
          logger$373.$1, (moonbit_string_t)moonbit_string_literal_18.data
        );
        _tmp$1376 = i$377 + 1;
        _tmp$1377 = i$377 + 1;
        i$377 = _tmp$1376;
        seg$378 = _tmp$1377;
        goto $$2a$for$379;
        break;
      }
      
      case 13: {
        int32_t _tmp$1378;
        int32_t _tmp$1379;
        moonbit_incref(_env$374);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$374, seg$378, i$377
        );
        if (logger$373.$1) {
          moonbit_incref(logger$373.$1);
        }
        logger$373.$0->$method_0(
          logger$373.$1, (moonbit_string_t)moonbit_string_literal_19.data
        );
        _tmp$1378 = i$377 + 1;
        _tmp$1379 = i$377 + 1;
        i$377 = _tmp$1378;
        seg$378 = _tmp$1379;
        goto $$2a$for$379;
        break;
      }
      
      case 8: {
        int32_t _tmp$1380;
        int32_t _tmp$1381;
        moonbit_incref(_env$374);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$374, seg$378, i$377
        );
        if (logger$373.$1) {
          moonbit_incref(logger$373.$1);
        }
        logger$373.$0->$method_0(
          logger$373.$1, (moonbit_string_t)moonbit_string_literal_20.data
        );
        _tmp$1380 = i$377 + 1;
        _tmp$1381 = i$377 + 1;
        i$377 = _tmp$1380;
        seg$378 = _tmp$1381;
        goto $$2a$for$379;
        break;
      }
      
      case 9: {
        int32_t _tmp$1382;
        int32_t _tmp$1383;
        moonbit_incref(_env$374);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$374, seg$378, i$377
        );
        if (logger$373.$1) {
          moonbit_incref(logger$373.$1);
        }
        logger$373.$0->$method_0(
          logger$373.$1, (moonbit_string_t)moonbit_string_literal_21.data
        );
        _tmp$1382 = i$377 + 1;
        _tmp$1383 = i$377 + 1;
        i$377 = _tmp$1382;
        seg$378 = _tmp$1383;
        goto $$2a$for$379;
        break;
      }
      default: {
        if (code$380 < 32) {
          int32_t _tmp$1386;
          moonbit_string_t _tmp$1385;
          struct $$moonbitlang$core$builtin$Logger _bind$1384;
          int32_t _tmp$1387;
          int32_t _tmp$1388;
          moonbit_incref(_env$374);
          $moonbitlang$core$builtin$output$flush_segment$7c$3831(
            _env$374, seg$378, i$377
          );
          if (logger$373.$1) {
            moonbit_incref(logger$373.$1);
          }
          logger$373.$0->$method_0(
            logger$373.$1, (moonbit_string_t)moonbit_string_literal_22.data
          );
          _tmp$1386 = code$380 & 0xff;
          _tmp$1385 = $Byte$$to_hex(_tmp$1386);
          if (logger$373.$1) {
            moonbit_incref(logger$373.$1);
          }
          logger$373.$0->$method_0(logger$373.$1, _tmp$1385);
          _bind$1384 = logger$373;
          if (_bind$1384.$1) {
            moonbit_incref(_bind$1384.$1);
          }
          _bind$1384.$0->$method_3(_bind$1384.$1, 125);
          _tmp$1387 = i$377 + 1;
          _tmp$1388 = i$377 + 1;
          i$377 = _tmp$1387;
          seg$378 = _tmp$1388;
          goto $$2a$for$379;
        } else {
          int32_t _tmp$1389 = i$377 + 1;
          int32_t _tmp$2439 = seg$378;
          i$377 = _tmp$1389;
          seg$378 = _tmp$2439;
          goto $$2a$for$379;
        }
        break;
      }
    }
    goto $joinlet$2438;
    $join$381:;
    moonbit_incref(_env$374);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$374, seg$378, i$377
    );
    if (logger$373.$1) {
      moonbit_incref(logger$373.$1);
    }
    logger$373.$0->$method_3(logger$373.$1, 92);
    _bind$1372 = logger$373;
    _tmp$1373 = c$382;
    if (_bind$1372.$1) {
      moonbit_incref(_bind$1372.$1);
    }
    _bind$1372.$0->$method_3(_bind$1372.$1, _tmp$1373);
    _tmp$1374 = i$377 + 1;
    _tmp$1375 = i$377 + 1;
    i$377 = _tmp$1374;
    seg$378 = _tmp$1375;
    continue;
    $joinlet$2438:;
    _tmp$2440 = i$377;
    _tmp$2441 = seg$378;
    i$377 = _tmp$2440;
    seg$378 = _tmp$2441;
    continue;
    break;
  }
  logger$373.$0->$method_3(logger$373.$1, 34);
  return 0;
}

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$369,
  int32_t seg$372,
  int32_t i$371
) {
  moonbit_string_t _field$2194 = _env$369->$1;
  moonbit_string_t self$368 = _field$2194;
  struct $$moonbitlang$core$builtin$Logger _field$2193 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$369->$0_0, _env$369->$0_1
    };
  int32_t _cnt$2340 = Moonbit_object_header(_env$369)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$370;
  if (_cnt$2340 > 1) {
    int32_t _new_cnt$2341;
    moonbit_incref(self$368);
    if (_field$2193.$1) {
      moonbit_incref(_field$2193.$1);
    }
    _new_cnt$2341 = _cnt$2340 - 1;
    Moonbit_object_header(_env$369)->rc = _new_cnt$2341;
  } else if (_cnt$2340 == 1) {
    moonbit_free(_env$369);
  }
  logger$370 = _field$2193;
  if (i$371 > seg$372) {
    int32_t _tmp$1371 = i$371 - seg$372;
    logger$370.$0->$method_1(logger$370.$1, self$368, seg$372, _tmp$1371);
  } else {
    if (logger$370.$1) {
      moonbit_decref(logger$370.$1);
    }
    moonbit_decref(self$368);
  }
  return 0;
}

moonbit_string_t $Byte$$to_hex(int32_t b$367) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$366 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _tmp$1368 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$367, 16);
  int32_t _tmp$1367 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$1368);
  int32_t _tmp$1370;
  int32_t _tmp$1369;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$1366;
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1367
  );
  _tmp$1370 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$367, 16);
  _tmp$1369
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$1370
  );
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1369
  );
  _tmp$1366 = _self$366;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$1366);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$365) {
  if (i$365 < 10) {
    int32_t _tmp$1363 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 48);
    return $Byte$$to_char(_tmp$1363);
  } else {
    int32_t _tmp$1365 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 97);
    int32_t _tmp$1364 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$1365, 10);
    return $Byte$$to_char(_tmp$1364);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$363,
  int32_t that$364
) {
  int32_t _tmp$1361 = (int32_t)self$363;
  int32_t _tmp$1362 = (int32_t)that$364;
  int32_t _tmp$1360 = _tmp$1361 - _tmp$1362;
  return _tmp$1360 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$361,
  int32_t that$362
) {
  int32_t _tmp$1358 = (int32_t)self$361;
  int32_t _tmp$1359 = (int32_t)that$362;
  int32_t _tmp$1357 = _tmp$1358 % _tmp$1359;
  return _tmp$1357 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$359,
  int32_t that$360
) {
  int32_t _tmp$1355 = (int32_t)self$359;
  int32_t _tmp$1356 = (int32_t)that$360;
  int32_t _tmp$1354 = _tmp$1355 / _tmp$1356;
  return _tmp$1354 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$357,
  int32_t that$358
) {
  int32_t _tmp$1352 = (int32_t)self$357;
  int32_t _tmp$1353 = (int32_t)that$358;
  int32_t _tmp$1351 = _tmp$1352 + _tmp$1353;
  return _tmp$1351 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$354,
  int32_t start$352,
  int32_t end$353
) {
  int32_t _if_result$2442;
  int32_t len$355;
  int32_t _tmp$1349;
  int32_t _tmp$1350;
  moonbit_bytes_t bytes$356;
  moonbit_bytes_t _tmp$1348;
  if (start$352 == 0) {
    int32_t _tmp$1347 = Moonbit_array_length(str$354);
    _if_result$2442 = end$353 == _tmp$1347;
  } else {
    _if_result$2442 = 0;
  }
  if (_if_result$2442) {
    return str$354;
  }
  len$355 = end$353 - start$352;
  _tmp$1349 = len$355 * 2;
  _tmp$1350 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$356 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1349, _tmp$1350);
  moonbit_incref(bytes$356);
  $FixedArray$$blit_from_string(bytes$356, 0, str$354, start$352, len$355);
  _tmp$1348 = bytes$356;
  return $Bytes$$to_unchecked_string$inner(_tmp$1348, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$351
) {
  return f$351;
}

moonbit_string_t $Int$$to_string$inner(int32_t self$335, int32_t radix$334) {
  int32_t _if_result$2443;
  int32_t is_negative$336;
  uint32_t num$337;
  uint16_t* buffer$338;
  if (radix$334 < 2) {
    _if_result$2443 = 1;
  } else {
    _if_result$2443 = radix$334 > 36;
  }
  if (_if_result$2443) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_23.data,
        (moonbit_string_t)moonbit_string_literal_24.data
    );
  }
  if (self$335 == 0) {
    return (moonbit_string_t)moonbit_string_literal_25.data;
  }
  is_negative$336 = self$335 < 0;
  if (is_negative$336) {
    int32_t _tmp$1346 = -self$335;
    num$337 = *(uint32_t*)&_tmp$1346;
  } else {
    num$337 = *(uint32_t*)&self$335;
  }
  switch (radix$334) {
    case 10: {
      int32_t digit_len$339 = $moonbitlang$core$builtin$dec_count32(num$337);
      int32_t _tmp$1343;
      int32_t total_len$340;
      uint16_t* buffer$341;
      int32_t digit_start$342;
      if (is_negative$336) {
        _tmp$1343 = 1;
      } else {
        _tmp$1343 = 0;
      }
      total_len$340 = digit_len$339 + _tmp$1343;
      buffer$341 = (uint16_t*)moonbit_make_string(total_len$340, 0);
      if (is_negative$336) {
        digit_start$342 = 1;
      } else {
        digit_start$342 = 0;
      }
      moonbit_incref(buffer$341);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$341, num$337, digit_start$342, total_len$340
      );
      buffer$338 = buffer$341;
      break;
    }
    
    case 16: {
      int32_t digit_len$343 = $moonbitlang$core$builtin$hex_count32(num$337);
      int32_t _tmp$1344;
      int32_t total_len$344;
      uint16_t* buffer$345;
      int32_t digit_start$346;
      if (is_negative$336) {
        _tmp$1344 = 1;
      } else {
        _tmp$1344 = 0;
      }
      total_len$344 = digit_len$343 + _tmp$1344;
      buffer$345 = (uint16_t*)moonbit_make_string(total_len$344, 0);
      if (is_negative$336) {
        digit_start$346 = 1;
      } else {
        digit_start$346 = 0;
      }
      moonbit_incref(buffer$345);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$345, num$337, digit_start$346, total_len$344
      );
      buffer$338 = buffer$345;
      break;
    }
    default: {
      int32_t digit_len$347 =
        $moonbitlang$core$builtin$radix_count32(num$337, radix$334);
      int32_t _tmp$1345;
      int32_t total_len$348;
      uint16_t* buffer$349;
      int32_t digit_start$350;
      if (is_negative$336) {
        _tmp$1345 = 1;
      } else {
        _tmp$1345 = 0;
      }
      total_len$348 = digit_len$347 + _tmp$1345;
      buffer$349 = (uint16_t*)moonbit_make_string(total_len$348, 0);
      if (is_negative$336) {
        digit_start$350 = 1;
      } else {
        digit_start$350 = 0;
      }
      moonbit_incref(buffer$349);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$349, num$337, digit_start$350, total_len$348, radix$334
      );
      buffer$338 = buffer$349;
      break;
    }
  }
  if (is_negative$336) {
    buffer$338[0] = 45;
  }
  return buffer$338;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$328,
  int32_t radix$331
) {
  uint32_t num$329;
  uint32_t base$330;
  int32_t count$332;
  if (value$328 == 0u) {
    return 1;
  }
  num$329 = value$328;
  base$330 = *(uint32_t*)&radix$331;
  count$332 = 0;
  while (1) {
    uint32_t _tmp$1340 = num$329;
    if (_tmp$1340 > 0u) {
      int32_t _tmp$1341 = count$332;
      uint32_t _tmp$1342;
      count$332 = _tmp$1341 + 1;
      _tmp$1342 = num$329;
      num$329 = _tmp$1342 / base$330;
      continue;
    }
    break;
  }
  return count$332;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$326) {
  if (value$326 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$327 = moonbit_clz32(value$326);
    int32_t _tmp$1339 = 31 - leading_zeros$327;
    int32_t _tmp$1338 = _tmp$1339 / 4;
    return _tmp$1338 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$325) {
  if (value$325 >= 100000u) {
    if (value$325 >= 10000000u) {
      if (value$325 >= 1000000000u) {
        return 10;
      } else if (value$325 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$325 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$325 >= 1000u) {
    if (value$325 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$325 >= 100u) {
    return 3;
  } else if (value$325 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$315,
  uint32_t num$303,
  int32_t digit_start$306,
  int32_t total_len$305
) {
  uint32_t num$302 = num$303;
  int32_t offset$304 = total_len$305 - digit_start$306;
  uint32_t _tmp$1337;
  int32_t remaining$317;
  int32_t _tmp$1318;
  while (1) {
    uint32_t _tmp$1281 = num$302;
    if (_tmp$1281 >= 10000u) {
      uint32_t _tmp$1304 = num$302;
      uint32_t t$307 = _tmp$1304 / 10000u;
      uint32_t _tmp$1303 = num$302;
      uint32_t _tmp$1302 = _tmp$1303 % 10000u;
      int32_t r$308 = *(int32_t*)&_tmp$1302;
      int32_t d1$309;
      int32_t d2$310;
      int32_t _tmp$1282;
      int32_t _tmp$1301;
      int32_t _tmp$1300;
      int32_t d1_hi$311;
      int32_t _tmp$1299;
      int32_t _tmp$1298;
      int32_t d1_lo$312;
      int32_t _tmp$1297;
      int32_t _tmp$1296;
      int32_t d2_hi$313;
      int32_t _tmp$1295;
      int32_t _tmp$1294;
      int32_t d2_lo$314;
      int32_t _tmp$1284;
      int32_t _tmp$1283;
      int32_t _tmp$1287;
      int32_t _tmp$1286;
      int32_t _tmp$1285;
      int32_t _tmp$1290;
      int32_t _tmp$1289;
      int32_t _tmp$1288;
      int32_t _tmp$1293;
      int32_t _tmp$1292;
      int32_t _tmp$1291;
      num$302 = t$307;
      d1$309 = r$308 / 100;
      d2$310 = r$308 % 100;
      _tmp$1282 = offset$304;
      offset$304 = _tmp$1282 - 4;
      _tmp$1301 = d1$309 / 10;
      _tmp$1300 = 48 + _tmp$1301;
      d1_hi$311 = (uint16_t)_tmp$1300;
      _tmp$1299 = d1$309 % 10;
      _tmp$1298 = 48 + _tmp$1299;
      d1_lo$312 = (uint16_t)_tmp$1298;
      _tmp$1297 = d2$310 / 10;
      _tmp$1296 = 48 + _tmp$1297;
      d2_hi$313 = (uint16_t)_tmp$1296;
      _tmp$1295 = d2$310 % 10;
      _tmp$1294 = 48 + _tmp$1295;
      d2_lo$314 = (uint16_t)_tmp$1294;
      _tmp$1284 = offset$304;
      _tmp$1283 = digit_start$306 + _tmp$1284;
      buffer$315[_tmp$1283] = d1_hi$311;
      _tmp$1287 = offset$304;
      _tmp$1286 = digit_start$306 + _tmp$1287;
      _tmp$1285 = _tmp$1286 + 1;
      buffer$315[_tmp$1285] = d1_lo$312;
      _tmp$1290 = offset$304;
      _tmp$1289 = digit_start$306 + _tmp$1290;
      _tmp$1288 = _tmp$1289 + 2;
      buffer$315[_tmp$1288] = d2_hi$313;
      _tmp$1293 = offset$304;
      _tmp$1292 = digit_start$306 + _tmp$1293;
      _tmp$1291 = _tmp$1292 + 3;
      buffer$315[_tmp$1291] = d2_lo$314;
      continue;
    }
    break;
  }
  _tmp$1337 = num$302;
  remaining$317 = *(int32_t*)&_tmp$1337;
  while (1) {
    int32_t _tmp$1305 = remaining$317;
    if (_tmp$1305 >= 100) {
      int32_t _tmp$1317 = remaining$317;
      int32_t t$318 = _tmp$1317 / 100;
      int32_t _tmp$1316 = remaining$317;
      int32_t d$319 = _tmp$1316 % 100;
      int32_t _tmp$1306;
      int32_t _tmp$1315;
      int32_t _tmp$1314;
      int32_t d_hi$320;
      int32_t _tmp$1313;
      int32_t _tmp$1312;
      int32_t d_lo$321;
      int32_t _tmp$1308;
      int32_t _tmp$1307;
      int32_t _tmp$1311;
      int32_t _tmp$1310;
      int32_t _tmp$1309;
      remaining$317 = t$318;
      _tmp$1306 = offset$304;
      offset$304 = _tmp$1306 - 2;
      _tmp$1315 = d$319 / 10;
      _tmp$1314 = 48 + _tmp$1315;
      d_hi$320 = (uint16_t)_tmp$1314;
      _tmp$1313 = d$319 % 10;
      _tmp$1312 = 48 + _tmp$1313;
      d_lo$321 = (uint16_t)_tmp$1312;
      _tmp$1308 = offset$304;
      _tmp$1307 = digit_start$306 + _tmp$1308;
      buffer$315[_tmp$1307] = d_hi$320;
      _tmp$1311 = offset$304;
      _tmp$1310 = digit_start$306 + _tmp$1311;
      _tmp$1309 = _tmp$1310 + 1;
      buffer$315[_tmp$1309] = d_lo$321;
      continue;
    }
    break;
  }
  _tmp$1318 = remaining$317;
  if (_tmp$1318 >= 10) {
    int32_t _tmp$1319 = offset$304;
    int32_t _tmp$1330;
    int32_t _tmp$1329;
    int32_t _tmp$1328;
    int32_t d_hi$323;
    int32_t _tmp$1327;
    int32_t _tmp$1326;
    int32_t _tmp$1325;
    int32_t d_lo$324;
    int32_t _tmp$1321;
    int32_t _tmp$1320;
    int32_t _tmp$1324;
    int32_t _tmp$1323;
    int32_t _tmp$1322;
    offset$304 = _tmp$1319 - 2;
    _tmp$1330 = remaining$317;
    _tmp$1329 = _tmp$1330 / 10;
    _tmp$1328 = 48 + _tmp$1329;
    d_hi$323 = (uint16_t)_tmp$1328;
    _tmp$1327 = remaining$317;
    _tmp$1326 = _tmp$1327 % 10;
    _tmp$1325 = 48 + _tmp$1326;
    d_lo$324 = (uint16_t)_tmp$1325;
    _tmp$1321 = offset$304;
    _tmp$1320 = digit_start$306 + _tmp$1321;
    buffer$315[_tmp$1320] = d_hi$323;
    _tmp$1324 = offset$304;
    _tmp$1323 = digit_start$306 + _tmp$1324;
    _tmp$1322 = _tmp$1323 + 1;
    buffer$315[_tmp$1322] = d_lo$324;
    moonbit_decref(buffer$315);
  } else {
    int32_t _tmp$1331 = offset$304;
    int32_t _tmp$1336;
    int32_t _tmp$1332;
    int32_t _tmp$1335;
    int32_t _tmp$1334;
    int32_t _tmp$1333;
    offset$304 = _tmp$1331 - 1;
    _tmp$1336 = offset$304;
    _tmp$1332 = digit_start$306 + _tmp$1336;
    _tmp$1335 = remaining$317;
    _tmp$1334 = 48 + _tmp$1335;
    _tmp$1333 = (uint16_t)_tmp$1334;
    buffer$315[_tmp$1332] = _tmp$1333;
    moonbit_decref(buffer$315);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$297,
  uint32_t num$291,
  int32_t digit_start$289,
  int32_t total_len$288,
  int32_t radix$293
) {
  int32_t offset$287 = total_len$288 - digit_start$289;
  uint32_t n$290 = num$291;
  uint32_t base$292 = *(uint32_t*)&radix$293;
  int32_t _tmp$1261 = radix$293 - 1;
  int32_t _tmp$1260 = radix$293 & _tmp$1261;
  if (_tmp$1260 == 0) {
    int32_t shift$294 = moonbit_ctz32(radix$293);
    uint32_t mask$295 = base$292 - 1u;
    while (1) {
      uint32_t _tmp$1262 = n$290;
      if (_tmp$1262 > 0u) {
        int32_t _tmp$1263 = offset$287;
        uint32_t _tmp$1270;
        uint32_t _tmp$1269;
        int32_t digit$296;
        int32_t _tmp$1267;
        int32_t _tmp$1264;
        int32_t _tmp$1266;
        int32_t _tmp$1265;
        uint32_t _tmp$1268;
        offset$287 = _tmp$1263 - 1;
        _tmp$1270 = n$290;
        _tmp$1269 = _tmp$1270 & mask$295;
        digit$296 = *(int32_t*)&_tmp$1269;
        _tmp$1267 = offset$287;
        _tmp$1264 = digit_start$289 + _tmp$1267;
        _tmp$1266
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$296
        ];
        _tmp$1265 = (uint16_t)_tmp$1266;
        buffer$297[_tmp$1264] = _tmp$1265;
        _tmp$1268 = n$290;
        n$290 = _tmp$1268 >> (shift$294 & 31);
        continue;
      } else {
        moonbit_decref(buffer$297);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1271 = n$290;
      if (_tmp$1271 > 0u) {
        int32_t _tmp$1272 = offset$287;
        uint32_t _tmp$1280;
        uint32_t q$299;
        uint32_t _tmp$1278;
        uint32_t _tmp$1279;
        uint32_t _tmp$1277;
        int32_t digit$300;
        int32_t _tmp$1276;
        int32_t _tmp$1273;
        int32_t _tmp$1275;
        int32_t _tmp$1274;
        offset$287 = _tmp$1272 - 1;
        _tmp$1280 = n$290;
        q$299 = _tmp$1280 / base$292;
        _tmp$1278 = n$290;
        _tmp$1279 = q$299 * base$292;
        _tmp$1277 = _tmp$1278 - _tmp$1279;
        digit$300 = *(int32_t*)&_tmp$1277;
        _tmp$1276 = offset$287;
        _tmp$1273 = digit_start$289 + _tmp$1276;
        _tmp$1275
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$300
        ];
        _tmp$1274 = (uint16_t)_tmp$1275;
        buffer$297[_tmp$1273] = _tmp$1274;
        n$290 = q$299;
        continue;
      } else {
        moonbit_decref(buffer$297);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$284,
  uint32_t num$280,
  int32_t digit_start$278,
  int32_t total_len$277
) {
  int32_t offset$276 = total_len$277 - digit_start$278;
  uint32_t n$279 = num$280;
  int32_t _tmp$1255;
  while (1) {
    int32_t _tmp$1241 = offset$276;
    if (_tmp$1241 >= 2) {
      int32_t _tmp$1242 = offset$276;
      uint32_t _tmp$1254;
      uint32_t _tmp$1253;
      int32_t byte_val$281;
      int32_t hi$282;
      int32_t lo$283;
      int32_t _tmp$1246;
      int32_t _tmp$1243;
      int32_t _tmp$1245;
      int32_t _tmp$1244;
      int32_t _tmp$1251;
      int32_t _tmp$1250;
      int32_t _tmp$1247;
      int32_t _tmp$1249;
      int32_t _tmp$1248;
      uint32_t _tmp$1252;
      offset$276 = _tmp$1242 - 2;
      _tmp$1254 = n$279;
      _tmp$1253 = _tmp$1254 & 255u;
      byte_val$281 = *(int32_t*)&_tmp$1253;
      hi$282 = byte_val$281 / 16;
      lo$283 = byte_val$281 % 16;
      _tmp$1246 = offset$276;
      _tmp$1243 = digit_start$278 + _tmp$1246;
      _tmp$1245 = ((moonbit_string_t)moonbit_string_literal_26.data)[hi$282];
      _tmp$1244 = (uint16_t)_tmp$1245;
      buffer$284[_tmp$1243] = _tmp$1244;
      _tmp$1251 = offset$276;
      _tmp$1250 = digit_start$278 + _tmp$1251;
      _tmp$1247 = _tmp$1250 + 1;
      _tmp$1249 = ((moonbit_string_t)moonbit_string_literal_26.data)[lo$283];
      _tmp$1248 = (uint16_t)_tmp$1249;
      buffer$284[_tmp$1247] = _tmp$1248;
      _tmp$1252 = n$279;
      n$279 = _tmp$1252 >> 8;
      continue;
    }
    break;
  }
  _tmp$1255 = offset$276;
  if (_tmp$1255 == 1) {
    uint32_t _tmp$1259 = n$279;
    uint32_t _tmp$1258 = _tmp$1259 & 15u;
    int32_t nibble$286 = *(int32_t*)&_tmp$1258;
    int32_t _tmp$1257 =
      ((moonbit_string_t)moonbit_string_literal_26.data)[nibble$286];
    int32_t _tmp$1256 = (uint16_t)_tmp$1257;
    buffer$284[digit_start$278] = _tmp$1256;
    moonbit_decref(buffer$284);
  } else {
    moonbit_decref(buffer$284);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$275
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$274 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1240;
  moonbit_incref(logger$274);
  _tmp$1240
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$274
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$275, _tmp$1240
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$274);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$273
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$272 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1239;
  moonbit_incref(logger$272);
  _tmp$1239
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$272
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$273, _tmp$1239
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$272);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$271
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$270 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1238;
  moonbit_incref(logger$270);
  _tmp$1238
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$270
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$271, _tmp$1238
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$270);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$269
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$268 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1237;
  moonbit_incref(logger$268);
  _tmp$1237
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$268
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$269, _tmp$1237);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$268);
}

int32_t $StringView$$start_offset(struct $StringView self$267) {
  int32_t _field$2195 = self$267.$1;
  moonbit_decref(self$267.$0);
  return _field$2195;
}

moonbit_string_t $StringView$$data(struct $StringView self$266) {
  moonbit_string_t _field$2196 = self$266.$0;
  return _field$2196;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$260,
  moonbit_string_t value$263,
  int32_t start$264,
  int32_t len$265
) {
  void* _try_err$262;
  struct $StringView _tmp$1232;
  int32_t _tmp$1234 = start$264 + len$265;
  int64_t _tmp$1233 = (int64_t)_tmp$1234;
  struct moonbit_result_1 _tmp$2451 =
    $String$$sub$inner(value$263, start$264, _tmp$1233);
  if (_tmp$2451.tag) {
    struct $StringView const _ok$1235 = _tmp$2451.data.ok;
    _tmp$1232 = _ok$1235;
  } else {
    void* const _err$1236 = _tmp$2451.data.err;
    _try_err$262 = _err$1236;
    goto $join$261;
  }
  goto $joinlet$2450;
  $join$261:;
  moonbit_decref(_try_err$262);
  moonbit_panic();
  $joinlet$2450:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$260, _tmp$1232
  );
  return 0;
}

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$253,
  int32_t start$259,
  int64_t end$255
) {
  int32_t len$252 = Moonbit_array_length(self$253);
  int32_t end$254;
  int32_t start$258;
  int32_t _if_result$2452;
  if (end$255 == 4294967296ll) {
    end$254 = len$252;
  } else {
    int64_t _Some$256 = end$255;
    int32_t _end$257 = (int32_t)_Some$256;
    if (_end$257 < 0) {
      end$254 = len$252 + _end$257;
    } else {
      end$254 = _end$257;
    }
  }
  if (start$259 < 0) {
    start$258 = len$252 + start$259;
  } else {
    start$258 = start$259;
  }
  if (start$258 >= 0) {
    if (start$258 <= end$254) {
      _if_result$2452 = end$254 <= len$252;
    } else {
      _if_result$2452 = 0;
    }
  } else {
    _if_result$2452 = 0;
  }
  if (_if_result$2452) {
    int32_t _if_result$2453;
    int32_t _if_result$2455;
    struct $StringView _tmp$1230;
    struct moonbit_result_1 _result$2457;
    if (start$258 < len$252) {
      int32_t _tmp$1226 = self$253[start$258];
      _if_result$2453 = $Int$$is_trailing_surrogate(_tmp$1226);
    } else {
      _if_result$2453 = 0;
    }
    if (_if_result$2453) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1227;
      struct moonbit_result_1 _result$2454;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1227
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2454.tag = 0;
      _result$2454.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1227;
      return _result$2454;
    }
    if (end$254 < len$252) {
      int32_t _tmp$1228 = self$253[end$254];
      _if_result$2455 = $Int$$is_trailing_surrogate(_tmp$1228);
    } else {
      _if_result$2455 = 0;
    }
    if (_if_result$2455) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1229;
      struct moonbit_result_1 _result$2456;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1229
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2456.tag = 0;
      _result$2456.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1229;
      return _result$2456;
    }
    _tmp$1230 = (struct $StringView){start$258, end$254, self$253};
    _result$2457.tag = 1;
    _result$2457.data.ok = _tmp$1230;
    return _result$2457;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1231;
    struct moonbit_result_1 _result$2458;
    moonbit_decref(self$253);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1231
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$2458.tag = 0;
    _result$2458.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1231;
    return _result$2458;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$251
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$250 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1225;
  moonbit_incref(_self$250);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$250, self$251);
  _tmp$1225 = _self$250;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1225);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$249
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$248 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1224;
  moonbit_incref(_self$248);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$248, self$249);
  _tmp$1224 = _self$248;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1224);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$246
) {
  int32_t seed$245;
  if (seed$opt$246 == 4294967296ll) {
    seed$245 = 0;
  } else {
    int64_t _Some$247 = seed$opt$246;
    seed$245 = (int32_t)_Some$247;
  }
  return $$moonbitlang$core$builtin$Hasher$$new$inner(seed$245);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$244
) {
  uint32_t _tmp$1223 = *(uint32_t*)&seed$244;
  uint32_t _tmp$1222 = _tmp$1223 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$2459 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$2459)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$2459->$0 = _tmp$1222;
  return _block$2459;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$243
) {
  uint32_t _tmp$1221 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$243);
  return *(int32_t*)&_tmp$1221;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$242
) {
  uint32_t _field$2197 = self$242->$0;
  uint32_t acc$241;
  uint32_t _tmp$1210;
  uint32_t _tmp$1212;
  uint32_t _tmp$1211;
  uint32_t _tmp$1213;
  uint32_t _tmp$1214;
  uint32_t _tmp$1216;
  uint32_t _tmp$1215;
  uint32_t _tmp$1217;
  uint32_t _tmp$1218;
  uint32_t _tmp$1220;
  uint32_t _tmp$1219;
  moonbit_decref(self$242);
  acc$241 = _field$2197;
  _tmp$1210 = acc$241;
  _tmp$1212 = acc$241;
  _tmp$1211 = _tmp$1212 >> 15;
  acc$241 = _tmp$1210 ^ _tmp$1211;
  _tmp$1213 = acc$241;
  acc$241 = _tmp$1213 * 2246822519u;
  _tmp$1214 = acc$241;
  _tmp$1216 = acc$241;
  _tmp$1215 = _tmp$1216 >> 13;
  acc$241 = _tmp$1214 ^ _tmp$1215;
  _tmp$1217 = acc$241;
  acc$241 = _tmp$1217 * 3266489917u;
  _tmp$1218 = acc$241;
  _tmp$1220 = acc$241;
  _tmp$1219 = _tmp$1220 >> 16;
  acc$241 = _tmp$1218 ^ _tmp$1219;
  return acc$241;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$240,
  int32_t value$239
) {
  $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(value$239, self$240);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$238,
  moonbit_string_t value$237
) {
  $$moonbitlang$core$builtin$Hash$$String$$hash_combine(value$237, self$238);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$235,
  int32_t value$236
) {
  uint32_t _tmp$1209 = *(uint32_t*)&value$236;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$235, _tmp$1209);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$233,
  uint32_t value$234
) {
  uint32_t acc$1208 = self$233->$0;
  uint32_t _tmp$1207 = acc$1208 + 4u;
  self$233->$0 = _tmp$1207;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$233, value$234);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$231,
  uint32_t input$232
) {
  uint32_t acc$1205 = self$231->$0;
  uint32_t _tmp$1206 = input$232 * 3266489917u;
  uint32_t _tmp$1204 = acc$1205 + _tmp$1206;
  uint32_t _tmp$1203 = $moonbitlang$core$builtin$rotl(_tmp$1204, 17);
  uint32_t _tmp$1202 = _tmp$1203 * 668265263u;
  self$231->$0 = _tmp$1202;
  moonbit_decref(self$231);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$229, int32_t r$230) {
  uint32_t _tmp$1199 = x$229 << (r$230 & 31);
  int32_t _tmp$1201 = 32 - r$230;
  uint32_t _tmp$1200 = x$229 >> (_tmp$1201 & 31);
  return _tmp$1199 | _tmp$1200;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$227,
  moonbit_string_t str$228
) {
  int32_t len$1189 = self$227->$1;
  int32_t _tmp$1191 = Moonbit_array_length(str$228);
  int32_t _tmp$1190 = _tmp$1191 * 2;
  int32_t _tmp$1188 = len$1189 + _tmp$1190;
  moonbit_bytes_t _field$2199;
  moonbit_bytes_t data$1192;
  int32_t len$1193;
  int32_t _tmp$1194;
  int32_t len$1196;
  int32_t _tmp$2198;
  int32_t _tmp$1198;
  int32_t _tmp$1197;
  int32_t _tmp$1195;
  moonbit_incref(self$227);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$227, _tmp$1188
  );
  _field$2199 = self$227->$0;
  data$1192 = _field$2199;
  len$1193 = self$227->$1;
  _tmp$1194 = Moonbit_array_length(str$228);
  moonbit_incref(data$1192);
  moonbit_incref(str$228);
  $FixedArray$$blit_from_string(data$1192, len$1193, str$228, 0, _tmp$1194);
  len$1196 = self$227->$1;
  _tmp$2198 = Moonbit_array_length(str$228);
  moonbit_decref(str$228);
  _tmp$1198 = _tmp$2198;
  _tmp$1197 = _tmp$1198 * 2;
  _tmp$1195 = len$1196 + _tmp$1197;
  self$227->$1 = _tmp$1195;
  moonbit_decref(self$227);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$219,
  int32_t bytes_offset$214,
  moonbit_string_t str$221,
  int32_t str_offset$217,
  int32_t length$215
) {
  int32_t _tmp$1187 = length$215 * 2;
  int32_t _tmp$1186 = bytes_offset$214 + _tmp$1187;
  int32_t e1$213 = _tmp$1186 - 1;
  int32_t _tmp$1185 = str_offset$217 + length$215;
  int32_t e2$216 = _tmp$1185 - 1;
  int32_t len1$218 = Moonbit_array_length(self$219);
  int32_t len2$220 = Moonbit_array_length(str$221);
  int32_t _if_result$2460;
  if (length$215 >= 0) {
    if (bytes_offset$214 >= 0) {
      if (e1$213 < len1$218) {
        if (str_offset$217 >= 0) {
          _if_result$2460 = e2$216 < len2$220;
        } else {
          _if_result$2460 = 0;
        }
      } else {
        _if_result$2460 = 0;
      }
    } else {
      _if_result$2460 = 0;
    }
  } else {
    _if_result$2460 = 0;
  }
  if (_if_result$2460) {
    int32_t end_str_offset$222 = str_offset$217 + length$215;
    int32_t i$223 = str_offset$217;
    int32_t j$224 = bytes_offset$214;
    while (1) {
      if (i$223 < end_str_offset$222) {
        int32_t _tmp$1182 = str$221[i$223];
        uint32_t c$225 = *(uint32_t*)&_tmp$1182;
        uint32_t _tmp$1178 = c$225 & 255u;
        int32_t _tmp$1177 = $UInt$$to_byte(_tmp$1178);
        int32_t _tmp$1179;
        uint32_t _tmp$1181;
        int32_t _tmp$1180;
        int32_t _tmp$1183;
        int32_t _tmp$1184;
        if (j$224 < 0 || j$224 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[j$224] = _tmp$1177;
        _tmp$1179 = j$224 + 1;
        _tmp$1181 = c$225 >> 8;
        _tmp$1180 = $UInt$$to_byte(_tmp$1181);
        if (_tmp$1179 < 0 || _tmp$1179 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[_tmp$1179] = _tmp$1180;
        _tmp$1183 = i$223 + 1;
        _tmp$1184 = j$224 + 2;
        i$223 = _tmp$1183;
        j$224 = _tmp$1184;
        continue;
      } else {
        moonbit_decref(str$221);
        moonbit_decref(self$219);
      }
      break;
    }
  } else {
    moonbit_decref(str$221);
    moonbit_decref(self$219);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$181
) {
  int32_t _tmp$1150 = Moonbit_array_length(repr$181);
  int64_t _tmp$1149 = (int64_t)_tmp$1150;
  moonbit_incref(repr$181);
  if ($String$$char_length_ge$inner(repr$181, 1, 0, _tmp$1149)) {
    int32_t _tmp$1176 = repr$181[0];
    int32_t _x$182 = _tmp$1176;
    if (_x$182 == 64) {
      int32_t _tmp$1175 = Moonbit_array_length(repr$181);
      int64_t _tmp$1174 = (int64_t)_tmp$1175;
      int64_t _bind$945;
      int32_t _tmp$1172;
      int32_t _tmp$1173;
      struct $StringView _x$183;
      int32_t _tmp$1171;
      struct $StringView _tmp$1170;
      int64_t _bind$185;
      moonbit_incref(repr$181);
      _bind$945
      = $String$$offset_of_nth_char$inner(
        repr$181, 1, 0, _tmp$1174
      );
      if (_bind$945 == 4294967296ll) {
        _tmp$1172 = Moonbit_array_length(repr$181);
      } else {
        int64_t _Some$184 = _bind$945;
        _tmp$1172 = (int32_t)_Some$184;
      }
      _tmp$1173 = Moonbit_array_length(repr$181);
      _x$183 = (struct $StringView){_tmp$1172, _tmp$1173, repr$181};
      _tmp$1171
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1170
      = (struct $StringView){
        0, _tmp$1171, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$183.$0);
      _bind$185 = $StringView$$find(_x$183, _tmp$1170);
      if (_bind$185 == 4294967296ll) {
        moonbit_decref(_x$183.$0);
        moonbit_panic();
      } else {
        int64_t _Some$186 = _bind$185;
        int32_t _pkg_end$187 = (int32_t)_Some$186;
        int64_t _tmp$1169 = (int64_t)_pkg_end$187;
        struct $StringView pkg$188;
        int32_t _tmp$1168;
        struct $StringView _tmp$1167;
        int64_t _bind$189;
        moonbit_incref(_x$183.$0);
        pkg$188 = $StringView$$view$inner(_x$183, 0, _tmp$1169);
        _tmp$1168
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1167
        = (struct $StringView){
          0, _tmp$1168, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$183.$0);
        _bind$189 = $StringView$$rev_find(_x$183, _tmp$1167);
        if (_bind$189 == 4294967296ll) {
          moonbit_decref(pkg$188.$0);
          moonbit_decref(_x$183.$0);
          moonbit_panic();
        } else {
          int64_t _Some$190 = _bind$189;
          int32_t _start_loc_end$191 = (int32_t)_Some$190;
          int32_t _tmp$1151 = _start_loc_end$191 + 1;
          int32_t _tmp$1152;
          moonbit_incref(_x$183.$0);
          _tmp$1152 = $StringView$$length(_x$183);
          if (_tmp$1151 < _tmp$1152) {
            int32_t _tmp$1166 = _start_loc_end$191 + 1;
            struct $StringView end_loc$192;
            struct $$3c$StringView$2a$StringView$3e$* _bind$193;
            moonbit_incref(_x$183.$0);
            end_loc$192
            = $StringView$$view$inner(
              _x$183, _tmp$1166, 4294967296ll
            );
            _bind$193
            = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
              end_loc$192
            );
            if (_bind$193 == 0) {
              if (_bind$193) {
                moonbit_decref(_bind$193);
              }
              moonbit_decref(pkg$188.$0);
              moonbit_decref(_x$183.$0);
              moonbit_panic();
            } else {
              struct $$3c$StringView$2a$StringView$3e$* _Some$194 = _bind$193;
              struct $$3c$StringView$2a$StringView$3e$* _x$195 = _Some$194;
              struct $StringView _field$2203 =
                (struct $StringView){
                  _x$195->$0_1, _x$195->$0_2, _x$195->$0_0
                };
              struct $StringView _end_line$196 = _field$2203;
              struct $StringView _field$2202 =
                (struct $StringView){
                  _x$195->$1_1, _x$195->$1_2, _x$195->$1_0
                };
              int32_t _cnt$2342 = Moonbit_object_header(_x$195)->rc;
              struct $StringView _end_column$197;
              int64_t _tmp$1165;
              struct $StringView rest$198;
              int32_t _tmp$1164;
              struct $StringView _tmp$1163;
              int64_t _bind$200;
              if (_cnt$2342 > 1) {
                int32_t _new_cnt$2343;
                moonbit_incref(_field$2202.$0);
                moonbit_incref(_end_line$196.$0);
                _new_cnt$2343 = _cnt$2342 - 1;
                Moonbit_object_header(_x$195)->rc = _new_cnt$2343;
              } else if (_cnt$2342 == 1) {
                moonbit_free(_x$195);
              }
              _end_column$197 = _field$2202;
              _tmp$1165 = (int64_t)_start_loc_end$191;
              rest$198 = $StringView$$view$inner(_x$183, 0, _tmp$1165);
              _tmp$1164
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1163
              = (struct $StringView){
                0,
                  _tmp$1164,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$198.$0);
              _bind$200 = $StringView$$rev_find(rest$198, _tmp$1163);
              if (_bind$200 == 4294967296ll) {
                moonbit_decref(rest$198.$0);
                moonbit_decref(_end_column$197.$0);
                moonbit_decref(_end_line$196.$0);
                moonbit_decref(pkg$188.$0);
                goto $join$199;
              } else {
                int64_t _Some$201 = _bind$200;
                int32_t _start_line_end$202 = (int32_t)_Some$201;
                int64_t _tmp$1162 = (int64_t)_start_line_end$202;
                struct $StringView _tmp$1159;
                int32_t _tmp$1161;
                struct $StringView _tmp$1160;
                int64_t _bind$203;
                moonbit_incref(rest$198.$0);
                _tmp$1159 = $StringView$$view$inner(rest$198, 0, _tmp$1162);
                _tmp$1161
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1160
                = (struct $StringView){
                  0,
                    _tmp$1161,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$203 = $StringView$$rev_find(_tmp$1159, _tmp$1160);
                if (_bind$203 == 4294967296ll) {
                  moonbit_decref(rest$198.$0);
                  moonbit_decref(_end_column$197.$0);
                  moonbit_decref(_end_line$196.$0);
                  moonbit_decref(pkg$188.$0);
                  goto $join$199;
                } else {
                  int64_t _Some$204 = _bind$203;
                  int32_t _filename_end$205 = (int32_t)_Some$204;
                  int32_t _tmp$1153 = _filename_end$205 + 1;
                  int32_t _tmp$1154;
                  moonbit_incref(rest$198.$0);
                  _tmp$1154 = $StringView$$length(rest$198);
                  if (_tmp$1153 < _tmp$1154) {
                    int32_t _tmp$1158 = _filename_end$205 + 1;
                    struct $StringView start_loc$206;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$207;
                    moonbit_incref(rest$198.$0);
                    start_loc$206
                    = $StringView$$view$inner(
                      rest$198, _tmp$1158, 4294967296ll
                    );
                    _bind$207
                    = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
                      start_loc$206
                    );
                    if (_bind$207 == 0) {
                      if (_bind$207) {
                        moonbit_decref(_bind$207);
                      }
                      moonbit_decref(rest$198.$0);
                      moonbit_decref(_end_column$197.$0);
                      moonbit_decref(_end_line$196.$0);
                      moonbit_decref(pkg$188.$0);
                      moonbit_panic();
                    } else {
                      struct $$3c$StringView$2a$StringView$3e$* _Some$208 =
                        _bind$207;
                      struct $$3c$StringView$2a$StringView$3e$* _x$209 =
                        _Some$208;
                      struct $StringView _field$2201 =
                        (struct $StringView){
                          _x$209->$0_1, _x$209->$0_2, _x$209->$0_0
                        };
                      struct $StringView _start_line$210 = _field$2201;
                      struct $StringView _field$2200 =
                        (struct $StringView){
                          _x$209->$1_1, _x$209->$1_2, _x$209->$1_0
                        };
                      int32_t _cnt$2344 = Moonbit_object_header(_x$209)->rc;
                      struct $StringView _start_column$211;
                      int32_t _tmp$1155;
                      if (_cnt$2344 > 1) {
                        int32_t _new_cnt$2345;
                        moonbit_incref(_field$2200.$0);
                        moonbit_incref(_start_line$210.$0);
                        _new_cnt$2345 = _cnt$2344 - 1;
                        Moonbit_object_header(_x$209)->rc = _new_cnt$2345;
                      } else if (_cnt$2344 == 1) {
                        moonbit_free(_x$209);
                      }
                      _start_column$211 = _field$2200;
                      _tmp$1155 = _pkg_end$187 + 1;
                      if (_filename_end$205 > _tmp$1155) {
                        int32_t _tmp$1156 = _pkg_end$187 + 1;
                        int64_t _tmp$1157 = (int64_t)_filename_end$205;
                        struct $StringView filename$212 =
                          $StringView$$view$inner(
                            rest$198, _tmp$1156, _tmp$1157
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$2464 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$2464)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$2464->$0_0 = pkg$188.$0;
                        _block$2464->$0_1 = pkg$188.$1;
                        _block$2464->$0_2 = pkg$188.$2;
                        _block$2464->$1_0 = filename$212.$0;
                        _block$2464->$1_1 = filename$212.$1;
                        _block$2464->$1_2 = filename$212.$2;
                        _block$2464->$2_0 = _start_line$210.$0;
                        _block$2464->$2_1 = _start_line$210.$1;
                        _block$2464->$2_2 = _start_line$210.$2;
                        _block$2464->$3_0 = _start_column$211.$0;
                        _block$2464->$3_1 = _start_column$211.$1;
                        _block$2464->$3_2 = _start_column$211.$2;
                        _block$2464->$4_0 = _end_line$196.$0;
                        _block$2464->$4_1 = _end_line$196.$1;
                        _block$2464->$4_2 = _end_line$196.$2;
                        _block$2464->$5_0 = _end_column$197.$0;
                        _block$2464->$5_1 = _end_column$197.$1;
                        _block$2464->$5_2 = _end_column$197.$2;
                        return _block$2464;
                      } else {
                        moonbit_decref(_start_column$211.$0);
                        moonbit_decref(_start_line$210.$0);
                        moonbit_decref(rest$198.$0);
                        moonbit_decref(_end_column$197.$0);
                        moonbit_decref(_end_line$196.$0);
                        moonbit_decref(pkg$188.$0);
                        moonbit_panic();
                      }
                    }
                  } else {
                    moonbit_decref(rest$198.$0);
                    moonbit_decref(_end_column$197.$0);
                    moonbit_decref(_end_line$196.$0);
                    moonbit_decref(pkg$188.$0);
                    moonbit_panic();
                  }
                }
              }
              $join$199:;
              moonbit_panic();
            }
          } else {
            moonbit_decref(pkg$188.$0);
            moonbit_decref(_x$183.$0);
            moonbit_panic();
          }
        }
      }
    } else {
      moonbit_decref(repr$181);
      goto $join$180;
    }
  } else {
    moonbit_decref(repr$181);
    goto $join$180;
  }
  $join$180:;
  moonbit_panic();
}

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$177
) {
  int32_t _tmp$1148 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1147;
  int64_t _bind$176;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1147
  = (struct $StringView){
    0, _tmp$1148, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$177.$0);
  _bind$176 = $StringView$$find(view$177, _tmp$1147);
  if (_bind$176 == 4294967296ll) {
    moonbit_decref(view$177.$0);
    return 0;
  } else {
    int64_t _Some$178 = _bind$176;
    int32_t _i$179 = (int32_t)_Some$178;
    int32_t _if_result$2465;
    if (_i$179 > 0) {
      int32_t _tmp$1140 = _i$179 + 1;
      int32_t _tmp$1141;
      moonbit_incref(view$177.$0);
      _tmp$1141 = $StringView$$length(view$177);
      _if_result$2465 = _tmp$1140 < _tmp$1141;
    } else {
      _if_result$2465 = 0;
    }
    if (_if_result$2465) {
      int64_t _tmp$1146 = (int64_t)_i$179;
      struct $StringView _tmp$1143;
      int32_t _tmp$1145;
      struct $StringView _tmp$1144;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1142;
      moonbit_incref(view$177.$0);
      _tmp$1143 = $StringView$$view$inner(view$177, 0, _tmp$1146);
      _tmp$1145 = _i$179 + 1;
      _tmp$1144 = $StringView$$view$inner(view$177, _tmp$1145, 4294967296ll);
      _tuple$1142
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1142)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1142->$0_0 = _tmp$1143.$0;
      _tuple$1142->$0_1 = _tmp$1143.$1;
      _tuple$1142->$0_2 = _tmp$1143.$2;
      _tuple$1142->$1_0 = _tmp$1144.$0;
      _tuple$1142->$1_1 = _tmp$1144.$1;
      _tuple$1142->$1_2 = _tmp$1144.$2;
      return _tuple$1142;
    } else {
      moonbit_decref(view$177.$0);
      return 0;
    }
  }
}

struct $StringView $StringView$$view$inner(
  struct $StringView self$174,
  int32_t start_offset$175,
  int64_t end_offset$172
) {
  int32_t end_offset$171;
  int32_t _if_result$2466;
  if (end_offset$172 == 4294967296ll) {
    moonbit_incref(self$174.$0);
    end_offset$171 = $StringView$$length(self$174);
  } else {
    int64_t _Some$173 = end_offset$172;
    end_offset$171 = (int32_t)_Some$173;
  }
  if (start_offset$175 >= 0) {
    if (start_offset$175 <= end_offset$171) {
      int32_t _tmp$1134;
      moonbit_incref(self$174.$0);
      _tmp$1134 = $StringView$$length(self$174);
      _if_result$2466 = end_offset$171 <= _tmp$1134;
    } else {
      _if_result$2466 = 0;
    }
  } else {
    _if_result$2466 = 0;
  }
  if (_if_result$2466) {
    moonbit_string_t _field$2205 = self$174.$0;
    moonbit_string_t str$1135 = _field$2205;
    int32_t start$1139 = self$174.$1;
    int32_t _tmp$1136 = start$1139 + start_offset$175;
    int32_t _field$2204 = self$174.$1;
    int32_t start$1138 = _field$2204;
    int32_t _tmp$1137 = start$1138 + end_offset$171;
    return (struct $StringView){_tmp$1136, _tmp$1137, str$1135};
  } else {
    moonbit_decref(self$174.$0);
    return $moonbitlang$core$builtin$abort$2(
             (moonbit_string_t)moonbit_string_literal_27.data,
               (moonbit_string_t)moonbit_string_literal_28.data
           );
  }
}

int64_t $StringView$$rev_find(
  struct $StringView self$170,
  struct $StringView str$169
) {
  int32_t _tmp$1133;
  moonbit_incref(str$169.$0);
  _tmp$1133 = $StringView$$length(str$169);
  if (_tmp$1133 <= 4) {
    return $moonbitlang$core$builtin$brute_force_rev_find(self$170, str$169);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
             self$170, str$169
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$160,
  struct $StringView needle$162
) {
  int32_t haystack_len$159;
  int32_t needle_len$161;
  moonbit_incref(haystack$160.$0);
  haystack_len$159 = $StringView$$length(haystack$160);
  moonbit_incref(needle$162.$0);
  needle_len$161 = $StringView$$length(needle$162);
  if (needle_len$161 > 0) {
    if (haystack_len$159 >= needle_len$161) {
      int32_t needle_first$163;
      int32_t i$164;
      moonbit_incref(needle$162.$0);
      needle_first$163 = $StringView$$unsafe_charcode_at(needle$162, 0);
      i$164 = haystack_len$159 - needle_len$161;
      while (1) {
        int32_t _tmp$1120 = i$164;
        if (_tmp$1120 >= 0) {
          int32_t _tmp$1125;
          while (1) {
            int32_t _tmp$1123 = i$164;
            int32_t _if_result$2469;
            if (_tmp$1123 >= 0) {
              int32_t _tmp$1122 = i$164;
              int32_t _tmp$1121;
              moonbit_incref(haystack$160.$0);
              _tmp$1121
              = $StringView$$unsafe_charcode_at(
                haystack$160, _tmp$1122
              );
              _if_result$2469 = _tmp$1121 != needle_first$163;
            } else {
              _if_result$2469 = 0;
            }
            if (_if_result$2469) {
              int32_t _tmp$1124 = i$164;
              i$164 = _tmp$1124 - 1;
              continue;
            }
            break;
          }
          _tmp$1125 = i$164;
          if (_tmp$1125 >= 0) {
            int32_t j$166 = 1;
            int32_t _tmp$1132;
            while (1) {
              if (j$166 < needle_len$161) {
                int32_t _tmp$1129 = i$164;
                int32_t _tmp$1128 = _tmp$1129 + j$166;
                int32_t _tmp$1126;
                int32_t _tmp$1127;
                int32_t _tmp$1130;
                moonbit_incref(haystack$160.$0);
                _tmp$1126
                = $StringView$$unsafe_charcode_at(
                  haystack$160, _tmp$1128
                );
                moonbit_incref(needle$162.$0);
                _tmp$1127
                = $StringView$$unsafe_charcode_at(
                  needle$162, j$166
                );
                if (_tmp$1126 != _tmp$1127) {
                  break;
                }
                _tmp$1130 = j$166 + 1;
                j$166 = _tmp$1130;
                continue;
              } else {
                int32_t _tmp$1131;
                moonbit_decref(needle$162.$0);
                moonbit_decref(haystack$160.$0);
                _tmp$1131 = i$164;
                return (int64_t)_tmp$1131;
              }
              break;
            }
            _tmp$1132 = i$164;
            i$164 = _tmp$1132 - 1;
          }
          continue;
        } else {
          moonbit_decref(needle$162.$0);
          moonbit_decref(haystack$160.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$162.$0);
      moonbit_decref(haystack$160.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$162.$0);
    moonbit_decref(haystack$160.$0);
    return (int64_t)haystack_len$159;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$149,
  struct $StringView needle$151
) {
  int32_t haystack_len$148;
  int32_t needle_len$150;
  moonbit_incref(haystack$149.$0);
  haystack_len$148 = $StringView$$length(haystack$149);
  moonbit_incref(needle$151.$0);
  needle_len$150 = $StringView$$length(needle$151);
  if (needle_len$150 > 0) {
    if (haystack_len$148 >= needle_len$150) {
      int32_t* skip_table$152 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$150);
      int32_t _tmp$1110 = needle_len$150 - 1;
      int32_t i$153 = _tmp$1110;
      int32_t _tmp$1119;
      int32_t i$155;
      while (1) {
        if (i$153 > 0) {
          int32_t _tmp$1108;
          int32_t _tmp$1107;
          int32_t _tmp$1109;
          moonbit_incref(needle$151.$0);
          _tmp$1108 = $StringView$$unsafe_charcode_at(needle$151, i$153);
          _tmp$1107 = _tmp$1108 & 255;
          if (
            _tmp$1107 < 0
            || _tmp$1107 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          skip_table$152[_tmp$1107] = i$153;
          _tmp$1109 = i$153 - 1;
          i$153 = _tmp$1109;
          continue;
        }
        break;
      }
      _tmp$1119 = haystack_len$148 - needle_len$150;
      i$155 = _tmp$1119;
      while (1) {
        if (i$155 >= 0) {
          int32_t j$156 = 0;
          int32_t _tmp$1118;
          int32_t _tmp$1117;
          int32_t _tmp$1116;
          int32_t _tmp$1115;
          while (1) {
            if (j$156 < needle_len$150) {
              int32_t _tmp$1113 = i$155 + j$156;
              int32_t _tmp$1111;
              int32_t _tmp$1112;
              int32_t _tmp$1114;
              moonbit_incref(haystack$149.$0);
              _tmp$1111
              = $StringView$$unsafe_charcode_at(
                haystack$149, _tmp$1113
              );
              moonbit_incref(needle$151.$0);
              _tmp$1112 = $StringView$$unsafe_charcode_at(needle$151, j$156);
              if (_tmp$1111 != _tmp$1112) {
                break;
              }
              _tmp$1114 = j$156 + 1;
              j$156 = _tmp$1114;
              continue;
            } else {
              moonbit_decref(skip_table$152);
              moonbit_decref(needle$151.$0);
              moonbit_decref(haystack$149.$0);
              return (int64_t)i$155;
            }
            break;
          }
          moonbit_incref(haystack$149.$0);
          _tmp$1118 = $StringView$$unsafe_charcode_at(haystack$149, i$155);
          _tmp$1117 = _tmp$1118 & 255;
          if (
            _tmp$1117 < 0
            || _tmp$1117 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          _tmp$1116 = (int32_t)skip_table$152[_tmp$1117];
          _tmp$1115 = i$155 - _tmp$1116;
          i$155 = _tmp$1115;
          continue;
        } else {
          moonbit_decref(skip_table$152);
          moonbit_decref(needle$151.$0);
          moonbit_decref(haystack$149.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$151.$0);
      moonbit_decref(haystack$149.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$151.$0);
    moonbit_decref(haystack$149.$0);
    return (int64_t)haystack_len$148;
  }
}

int64_t $StringView$$find(
  struct $StringView self$147,
  struct $StringView str$146
) {
  int32_t _tmp$1106;
  moonbit_incref(str$146.$0);
  _tmp$1106 = $StringView$$length(str$146);
  if (_tmp$1106 <= 4) {
    return $moonbitlang$core$builtin$brute_force_find(self$147, str$146);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_find(
             self$147, str$146
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$136,
  struct $StringView needle$138
) {
  int32_t haystack_len$135;
  int32_t needle_len$137;
  moonbit_incref(haystack$136.$0);
  haystack_len$135 = $StringView$$length(haystack$136);
  moonbit_incref(needle$138.$0);
  needle_len$137 = $StringView$$length(needle$138);
  if (needle_len$137 > 0) {
    if (haystack_len$135 >= needle_len$137) {
      int32_t needle_first$139;
      int32_t forward_len$140;
      int32_t i$141;
      moonbit_incref(needle$138.$0);
      needle_first$139 = $StringView$$unsafe_charcode_at(needle$138, 0);
      forward_len$140 = haystack_len$135 - needle_len$137;
      i$141 = 0;
      while (1) {
        int32_t _tmp$1093 = i$141;
        if (_tmp$1093 <= forward_len$140) {
          int32_t _tmp$1098;
          while (1) {
            int32_t _tmp$1096 = i$141;
            int32_t _if_result$2476;
            if (_tmp$1096 <= forward_len$140) {
              int32_t _tmp$1095 = i$141;
              int32_t _tmp$1094;
              moonbit_incref(haystack$136.$0);
              _tmp$1094
              = $StringView$$unsafe_charcode_at(
                haystack$136, _tmp$1095
              );
              _if_result$2476 = _tmp$1094 != needle_first$139;
            } else {
              _if_result$2476 = 0;
            }
            if (_if_result$2476) {
              int32_t _tmp$1097 = i$141;
              i$141 = _tmp$1097 + 1;
              continue;
            }
            break;
          }
          _tmp$1098 = i$141;
          if (_tmp$1098 <= forward_len$140) {
            int32_t j$143 = 1;
            int32_t _tmp$1105;
            while (1) {
              if (j$143 < needle_len$137) {
                int32_t _tmp$1102 = i$141;
                int32_t _tmp$1101 = _tmp$1102 + j$143;
                int32_t _tmp$1099;
                int32_t _tmp$1100;
                int32_t _tmp$1103;
                moonbit_incref(haystack$136.$0);
                _tmp$1099
                = $StringView$$unsafe_charcode_at(
                  haystack$136, _tmp$1101
                );
                moonbit_incref(needle$138.$0);
                _tmp$1100
                = $StringView$$unsafe_charcode_at(
                  needle$138, j$143
                );
                if (_tmp$1099 != _tmp$1100) {
                  break;
                }
                _tmp$1103 = j$143 + 1;
                j$143 = _tmp$1103;
                continue;
              } else {
                int32_t _tmp$1104;
                moonbit_decref(needle$138.$0);
                moonbit_decref(haystack$136.$0);
                _tmp$1104 = i$141;
                return (int64_t)_tmp$1104;
              }
              break;
            }
            _tmp$1105 = i$141;
            i$141 = _tmp$1105 + 1;
          }
          continue;
        } else {
          moonbit_decref(needle$138.$0);
          moonbit_decref(haystack$136.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$138.$0);
      moonbit_decref(haystack$136.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$138.$0);
    moonbit_decref(haystack$136.$0);
    return $moonbitlang$core$builtin$brute_force_find$constr$134;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$122,
  struct $StringView needle$124
) {
  int32_t haystack_len$121;
  int32_t needle_len$123;
  moonbit_incref(haystack$122.$0);
  haystack_len$121 = $StringView$$length(haystack$122);
  moonbit_incref(needle$124.$0);
  needle_len$123 = $StringView$$length(needle$124);
  if (needle_len$123 > 0) {
    if (haystack_len$121 >= needle_len$123) {
      int32_t* skip_table$125 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$123);
      int32_t _end4301$126 = needle_len$123 - 1;
      int32_t i$127 = 0;
      int32_t i$129;
      while (1) {
        if (i$127 < _end4301$126) {
          int32_t _tmp$1080;
          int32_t _tmp$1077;
          int32_t _tmp$1079;
          int32_t _tmp$1078;
          int32_t _tmp$1081;
          moonbit_incref(needle$124.$0);
          _tmp$1080 = $StringView$$unsafe_charcode_at(needle$124, i$127);
          _tmp$1077 = _tmp$1080 & 255;
          _tmp$1079 = needle_len$123 - 1;
          _tmp$1078 = _tmp$1079 - i$127;
          if (
            _tmp$1077 < 0
            || _tmp$1077 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          skip_table$125[_tmp$1077] = _tmp$1078;
          _tmp$1081 = i$127 + 1;
          i$127 = _tmp$1081;
          continue;
        }
        break;
      }
      i$129 = 0;
      while (1) {
        int32_t _tmp$1082 = haystack_len$121 - needle_len$123;
        if (i$129 <= _tmp$1082) {
          int32_t _end4307$130 = needle_len$123 - 1;
          int32_t j$131 = 0;
          int32_t _tmp$1092;
          int32_t _tmp$1091;
          int32_t _tmp$1090;
          int32_t _tmp$1089;
          int32_t _tmp$1088;
          int32_t _tmp$1087;
          while (1) {
            if (j$131 <= _end4307$130) {
              int32_t _tmp$1085 = i$129 + j$131;
              int32_t _tmp$1083;
              int32_t _tmp$1084;
              int32_t _tmp$1086;
              moonbit_incref(haystack$122.$0);
              _tmp$1083
              = $StringView$$unsafe_charcode_at(
                haystack$122, _tmp$1085
              );
              moonbit_incref(needle$124.$0);
              _tmp$1084 = $StringView$$unsafe_charcode_at(needle$124, j$131);
              if (_tmp$1083 != _tmp$1084) {
                break;
              }
              _tmp$1086 = j$131 + 1;
              j$131 = _tmp$1086;
              continue;
            } else {
              moonbit_decref(skip_table$125);
              moonbit_decref(needle$124.$0);
              moonbit_decref(haystack$122.$0);
              return (int64_t)i$129;
            }
            break;
          }
          _tmp$1092 = i$129 + needle_len$123;
          _tmp$1091 = _tmp$1092 - 1;
          moonbit_incref(haystack$122.$0);
          _tmp$1090
          = $StringView$$unsafe_charcode_at(
            haystack$122, _tmp$1091
          );
          _tmp$1089 = _tmp$1090 & 255;
          if (
            _tmp$1089 < 0
            || _tmp$1089 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          _tmp$1088 = (int32_t)skip_table$125[_tmp$1089];
          _tmp$1087 = i$129 + _tmp$1088;
          i$129 = _tmp$1087;
          continue;
        } else {
          moonbit_decref(skip_table$125);
          moonbit_decref(needle$124.$0);
          moonbit_decref(haystack$122.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$124.$0);
      moonbit_decref(haystack$122.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$124.$0);
    moonbit_decref(haystack$122.$0);
    return $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$120;
  }
}

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$118,
  int32_t index$119
) {
  moonbit_string_t _field$2208 = self$118.$0;
  moonbit_string_t str$1074 = _field$2208;
  int32_t _field$2207 = self$118.$1;
  int32_t start$1076 = _field$2207;
  int32_t _tmp$1075 = start$1076 + index$119;
  int32_t _tmp$2206 = str$1074[_tmp$1075];
  moonbit_decref(str$1074);
  return _tmp$2206;
}

int32_t $StringView$$length(struct $StringView self$117) {
  int32_t end$1072 = self$117.$2;
  int32_t _field$2209 = self$117.$1;
  int32_t start$1073;
  moonbit_decref(self$117.$0);
  start$1073 = _field$2209;
  return end$1072 - start$1073;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$115,
  int32_t index$116
) {
  int32_t len$114 = self$115->$1;
  int32_t _if_result$2481;
  if (index$116 >= 0) {
    _if_result$2481 = index$116 < len$114;
  } else {
    _if_result$2481 = 0;
  }
  if (_if_result$2481) {
    moonbit_string_t* _tmp$1071 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$115);
    moonbit_string_t _tmp$2210;
    if (index$116 < 0 || index$116 >= Moonbit_array_length(_tmp$1071)) {
      moonbit_panic();
    }
    _tmp$2210 = (moonbit_string_t)_tmp$1071[index$116];
    moonbit_incref(_tmp$2210);
    moonbit_decref(_tmp$1071);
    return _tmp$2210;
  } else {
    moonbit_decref(self$115);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$113
) {
  int32_t _field$2211 = self$113->$1;
  moonbit_decref(self$113);
  return _field$2211;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$112
) {
  int32_t _field$2212 = self$112->$1;
  moonbit_decref(self$112);
  return _field$2212;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$111
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2213 =
    self$111->$0;
  int32_t _cnt$2346 = Moonbit_object_header(self$111)->rc;
  if (_cnt$2346 > 1) {
    int32_t _new_cnt$2347;
    moonbit_incref(_field$2213);
    _new_cnt$2347 = _cnt$2346 - 1;
    Moonbit_object_header(self$111)->rc = _new_cnt$2347;
  } else if (_cnt$2346 == 1) {
    moonbit_free(self$111);
  }
  return _field$2213;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$110
) {
  moonbit_string_t* _field$2214 = self$110->$0;
  int32_t _cnt$2348 = Moonbit_object_header(self$110)->rc;
  if (_cnt$2348 > 1) {
    int32_t _new_cnt$2349;
    moonbit_incref(_field$2214);
    _new_cnt$2349 = _cnt$2348 - 1;
    Moonbit_object_header(self$110)->rc = _new_cnt$2349;
  } else if (_cnt$2348 == 1) {
    moonbit_free(self$110);
  }
  return _field$2214;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$109
) {
  struct $$3c$String$2a$Int$3e$** _field$2215 = self$109->$0;
  int32_t _cnt$2350 = Moonbit_object_header(self$109)->rc;
  if (_cnt$2350 > 1) {
    int32_t _new_cnt$2351;
    moonbit_incref(_field$2215);
    _new_cnt$2351 = _cnt$2350 - 1;
    Moonbit_object_header(self$109)->rc = _new_cnt$2351;
  } else if (_cnt$2350 == 1) {
    moonbit_free(self$109);
  }
  return _field$2215;
}

moonbit_string_t $String$$escape(moonbit_string_t self$108) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$107 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1070;
  moonbit_incref(buf$107);
  _tmp$1070
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$107
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$108, _tmp$1070);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$107);
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$105, int32_t y$106) {
  int32_t _tmp$1069 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$105, y$106
    );
  return !_tmp$1069;
}

int32_t $Int$$is_trailing_surrogate(int32_t self$104) {
  if (56320 <= self$104) {
    return self$104 <= 57343;
  } else {
    return 0;
  }
}

int32_t $Int$$is_leading_surrogate(int32_t self$103) {
  if (55296 <= self$103) {
    return self$103 <= 56319;
  } else {
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$100,
  int32_t ch$102
) {
  int32_t len$1064 = self$100->$1;
  int32_t _tmp$1063 = len$1064 + 4;
  moonbit_bytes_t _field$2216;
  moonbit_bytes_t data$1067;
  int32_t len$1068;
  int32_t inc$101;
  int32_t len$1066;
  int32_t _tmp$1065;
  moonbit_incref(self$100);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$100, _tmp$1063
  );
  _field$2216 = self$100->$0;
  data$1067 = _field$2216;
  len$1068 = self$100->$1;
  moonbit_incref(data$1067);
  inc$101 = $FixedArray$$set_utf16le_char(data$1067, len$1068, ch$102);
  len$1066 = self$100->$1;
  _tmp$1065 = len$1066 + inc$101;
  self$100->$1 = _tmp$1065;
  moonbit_decref(self$100);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$95,
  int32_t required$96
) {
  moonbit_bytes_t _field$2220 = self$95->$0;
  moonbit_bytes_t data$1062 = _field$2220;
  int32_t _tmp$2219 = Moonbit_array_length(data$1062);
  int32_t current_len$94 = _tmp$2219;
  int32_t enough_space$97;
  int32_t _tmp$1060;
  int32_t _tmp$1061;
  moonbit_bytes_t new_data$99;
  moonbit_bytes_t _field$2218;
  moonbit_bytes_t data$1058;
  int32_t len$1059;
  moonbit_bytes_t _old$2217;
  if (required$96 <= current_len$94) {
    moonbit_decref(self$95);
    return 0;
  }
  enough_space$97 = current_len$94;
  while (1) {
    int32_t _tmp$1056 = enough_space$97;
    if (_tmp$1056 < required$96) {
      int32_t _tmp$1057 = enough_space$97;
      enough_space$97 = _tmp$1057 * 2;
      continue;
    }
    break;
  }
  _tmp$1060 = enough_space$97;
  _tmp$1061 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$99 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1060, _tmp$1061);
  _field$2218 = self$95->$0;
  data$1058 = _field$2218;
  len$1059 = self$95->$1;
  moonbit_incref(data$1058);
  moonbit_incref(new_data$99);
  $FixedArray$$unsafe_blit$0(new_data$99, 0, data$1058, 0, len$1059);
  _old$2217 = self$95->$0;
  moonbit_decref(_old$2217);
  self$95->$0 = new_data$99;
  moonbit_decref(self$95);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default() {
  return 0;
}

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$89,
  int32_t offset$90,
  int32_t value$88
) {
  uint32_t code$87 = $Char$$to_uint(value$88);
  if (code$87 < 65536u) {
    uint32_t _tmp$1039 = code$87 & 255u;
    int32_t _tmp$1038 = $UInt$$to_byte(_tmp$1039);
    int32_t _tmp$1040;
    uint32_t _tmp$1042;
    int32_t _tmp$1041;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1038;
    _tmp$1040 = offset$90 + 1;
    _tmp$1042 = code$87 >> 8;
    _tmp$1041 = $UInt$$to_byte(_tmp$1042);
    if (_tmp$1040 < 0 || _tmp$1040 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1040] = _tmp$1041;
    moonbit_decref(self$89);
    return 2;
  } else if (code$87 < 1114112u) {
    uint32_t hi$91 = code$87 - 65536u;
    uint32_t _tmp$1055 = hi$91 >> 10;
    uint32_t lo$92 = _tmp$1055 | 55296u;
    uint32_t _tmp$1054 = hi$91 & 1023u;
    uint32_t hi$93 = _tmp$1054 | 56320u;
    uint32_t _tmp$1044 = lo$92 & 255u;
    int32_t _tmp$1043 = $UInt$$to_byte(_tmp$1044);
    int32_t _tmp$1045;
    uint32_t _tmp$1047;
    int32_t _tmp$1046;
    int32_t _tmp$1048;
    uint32_t _tmp$1050;
    int32_t _tmp$1049;
    int32_t _tmp$1051;
    uint32_t _tmp$1053;
    int32_t _tmp$1052;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1043;
    _tmp$1045 = offset$90 + 1;
    _tmp$1047 = lo$92 >> 8;
    _tmp$1046 = $UInt$$to_byte(_tmp$1047);
    if (_tmp$1045 < 0 || _tmp$1045 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1045] = _tmp$1046;
    _tmp$1048 = offset$90 + 2;
    _tmp$1050 = hi$93 & 255u;
    _tmp$1049 = $UInt$$to_byte(_tmp$1050);
    if (_tmp$1048 < 0 || _tmp$1048 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1048] = _tmp$1049;
    _tmp$1051 = offset$90 + 3;
    _tmp$1053 = hi$93 >> 8;
    _tmp$1052 = $UInt$$to_byte(_tmp$1053);
    if (_tmp$1051 < 0 || _tmp$1051 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1051] = _tmp$1052;
    moonbit_decref(self$89);
    return 4;
  } else {
    moonbit_decref(self$89);
    return $moonbitlang$core$builtin$abort$1(
             (moonbit_string_t)moonbit_string_literal_29.data,
               (moonbit_string_t)moonbit_string_literal_30.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$86) {
  int32_t _tmp$1037 = *(int32_t*)&self$86;
  return _tmp$1037 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$85) {
  int32_t _tmp$1036 = self$85;
  return *(uint32_t*)&_tmp$1036;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$84
) {
  moonbit_bytes_t _field$2222 = self$84->$0;
  moonbit_bytes_t data$1035 = _field$2222;
  moonbit_bytes_t _tmp$1032;
  int32_t _field$2221;
  int32_t len$1034;
  int64_t _tmp$1033;
  moonbit_incref(data$1035);
  _tmp$1032 = data$1035;
  _field$2221 = self$84->$1;
  moonbit_decref(self$84);
  len$1034 = _field$2221;
  _tmp$1033 = (int64_t)len$1034;
  return $Bytes$$to_unchecked_string$inner(_tmp$1032, 0, _tmp$1033);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$79,
  int32_t offset$83,
  int64_t length$81
) {
  int32_t len$78 = Moonbit_array_length(self$79);
  int32_t length$80;
  int32_t _if_result$2483;
  if (length$81 == 4294967296ll) {
    length$80 = len$78 - offset$83;
  } else {
    int64_t _Some$82 = length$81;
    length$80 = (int32_t)_Some$82;
  }
  if (offset$83 >= 0) {
    if (length$80 >= 0) {
      int32_t _tmp$1031 = offset$83 + length$80;
      _if_result$2483 = _tmp$1031 <= len$78;
    } else {
      _if_result$2483 = 0;
    }
  } else {
    _if_result$2483 = 0;
  }
  if (_if_result$2483) {
    return $moonbitlang$core$builtin$unsafe_sub_string(
             self$79, offset$83, length$80
           );
  } else {
    moonbit_decref(self$79);
    moonbit_panic();
  }
}

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$76
) {
  int32_t initial$75;
  moonbit_bytes_t data$77;
  struct $$moonbitlang$core$builtin$StringBuilder* _block$2484;
  if (size_hint$76 < 1) {
    initial$75 = 1;
  } else {
    initial$75 = size_hint$76;
  }
  data$77 = (moonbit_bytes_t)moonbit_make_bytes(initial$75, 0);
  _block$2484
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$2484)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$2484->$0 = data$77;
  _block$2484->$1 = 0;
  return _block$2484;
}

int32_t $Byte$$to_char(int32_t self$74) {
  int32_t _tmp$1030 = (int32_t)self$74;
  return _tmp$1030;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$69,
  int32_t dst_offset$70,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$71,
  int32_t src_offset$72,
  int32_t len$73
) {
  $FixedArray$$unsafe_blit$3(
    dst$69, dst_offset$70, src$71, src_offset$72, len$73
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$64,
  int32_t dst_offset$65,
  struct $$3c$String$2a$Int$3e$** src$66,
  int32_t src_offset$67,
  int32_t len$68
) {
  $FixedArray$$unsafe_blit$2(
    dst$64, dst_offset$65, src$66, src_offset$67, len$68
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$59,
  int32_t dst_offset$60,
  moonbit_string_t* src$61,
  int32_t src_offset$62,
  int32_t len$63
) {
  $FixedArray$$unsafe_blit$1(
    dst$59, dst_offset$60, src$61, src_offset$62, len$63
  );
  return 0;
}

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$50,
  int32_t dst_offset$52,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$51,
  int32_t src_offset$53,
  int32_t len$55
) {
  int32_t _if_result$2485;
  if (dst$50 == src$51) {
    _if_result$2485 = dst_offset$52 < src_offset$53;
  } else {
    _if_result$2485 = 0;
  }
  if (_if_result$2485) {
    int32_t i$54 = 0;
    while (1) {
      if (i$54 < len$55) {
        int32_t _tmp$1021 = dst_offset$52 + i$54;
        int32_t _tmp$1023 = src_offset$53 + i$54;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2224;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1022;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2223;
        int32_t _tmp$1024;
        if (_tmp$1023 < 0 || _tmp$1023 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2224
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1023
          ];
        _tmp$1022 = _tmp$2224;
        if (_tmp$1021 < 0 || _tmp$1021 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2223
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1021
          ];
        if (_tmp$1022) {
          moonbit_incref(_tmp$1022);
        }
        if (_old$2223) {
          moonbit_decref(_old$2223);
        }
        dst$50[_tmp$1021] = _tmp$1022;
        _tmp$1024 = i$54 + 1;
        i$54 = _tmp$1024;
        continue;
      } else {
        moonbit_decref(src$51);
        moonbit_decref(dst$50);
      }
      break;
    }
  } else {
    int32_t _tmp$1029 = len$55 - 1;
    int32_t i$57 = _tmp$1029;
    while (1) {
      if (i$57 >= 0) {
        int32_t _tmp$1025 = dst_offset$52 + i$57;
        int32_t _tmp$1027 = src_offset$53 + i$57;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2226;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1026;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2225;
        int32_t _tmp$1028;
        if (_tmp$1027 < 0 || _tmp$1027 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2226
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1027
          ];
        _tmp$1026 = _tmp$2226;
        if (_tmp$1025 < 0 || _tmp$1025 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2225
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1025
          ];
        if (_tmp$1026) {
          moonbit_incref(_tmp$1026);
        }
        if (_old$2225) {
          moonbit_decref(_old$2225);
        }
        dst$50[_tmp$1025] = _tmp$1026;
        _tmp$1028 = i$57 - 1;
        i$57 = _tmp$1028;
        continue;
      } else {
        moonbit_decref(src$51);
        moonbit_decref(dst$50);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$41,
  int32_t dst_offset$43,
  struct $$3c$String$2a$Int$3e$** src$42,
  int32_t src_offset$44,
  int32_t len$46
) {
  int32_t _if_result$2488;
  if (dst$41 == src$42) {
    _if_result$2488 = dst_offset$43 < src_offset$44;
  } else {
    _if_result$2488 = 0;
  }
  if (_if_result$2488) {
    int32_t i$45 = 0;
    while (1) {
      if (i$45 < len$46) {
        int32_t _tmp$1012 = dst_offset$43 + i$45;
        int32_t _tmp$1014 = src_offset$44 + i$45;
        struct $$3c$String$2a$Int$3e$* _tmp$2228;
        struct $$3c$String$2a$Int$3e$* _tmp$1013;
        struct $$3c$String$2a$Int$3e$* _old$2227;
        int32_t _tmp$1015;
        if (_tmp$1014 < 0 || _tmp$1014 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2228 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1014];
        _tmp$1013 = _tmp$2228;
        if (_tmp$1012 < 0 || _tmp$1012 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2227 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1012];
        if (_tmp$1013) {
          moonbit_incref(_tmp$1013);
        }
        if (_old$2227) {
          moonbit_decref(_old$2227);
        }
        dst$41[_tmp$1012] = _tmp$1013;
        _tmp$1015 = i$45 + 1;
        i$45 = _tmp$1015;
        continue;
      } else {
        moonbit_decref(src$42);
        moonbit_decref(dst$41);
      }
      break;
    }
  } else {
    int32_t _tmp$1020 = len$46 - 1;
    int32_t i$48 = _tmp$1020;
    while (1) {
      if (i$48 >= 0) {
        int32_t _tmp$1016 = dst_offset$43 + i$48;
        int32_t _tmp$1018 = src_offset$44 + i$48;
        struct $$3c$String$2a$Int$3e$* _tmp$2230;
        struct $$3c$String$2a$Int$3e$* _tmp$1017;
        struct $$3c$String$2a$Int$3e$* _old$2229;
        int32_t _tmp$1019;
        if (_tmp$1018 < 0 || _tmp$1018 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2230 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1018];
        _tmp$1017 = _tmp$2230;
        if (_tmp$1016 < 0 || _tmp$1016 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2229 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1016];
        if (_tmp$1017) {
          moonbit_incref(_tmp$1017);
        }
        if (_old$2229) {
          moonbit_decref(_old$2229);
        }
        dst$41[_tmp$1016] = _tmp$1017;
        _tmp$1019 = i$48 - 1;
        i$48 = _tmp$1019;
        continue;
      } else {
        moonbit_decref(src$42);
        moonbit_decref(dst$41);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$32,
  int32_t dst_offset$34,
  moonbit_string_t* src$33,
  int32_t src_offset$35,
  int32_t len$37
) {
  int32_t _if_result$2491;
  if (dst$32 == src$33) {
    _if_result$2491 = dst_offset$34 < src_offset$35;
  } else {
    _if_result$2491 = 0;
  }
  if (_if_result$2491) {
    int32_t i$36 = 0;
    while (1) {
      if (i$36 < len$37) {
        int32_t _tmp$1003 = dst_offset$34 + i$36;
        int32_t _tmp$1005 = src_offset$35 + i$36;
        moonbit_string_t _tmp$2232;
        moonbit_string_t _tmp$1004;
        moonbit_string_t _old$2231;
        int32_t _tmp$1006;
        if (_tmp$1005 < 0 || _tmp$1005 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2232 = (moonbit_string_t)src$33[_tmp$1005];
        _tmp$1004 = _tmp$2232;
        if (_tmp$1003 < 0 || _tmp$1003 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2231 = (moonbit_string_t)dst$32[_tmp$1003];
        moonbit_incref(_tmp$1004);
        moonbit_decref(_old$2231);
        dst$32[_tmp$1003] = _tmp$1004;
        _tmp$1006 = i$36 + 1;
        i$36 = _tmp$1006;
        continue;
      } else {
        moonbit_decref(src$33);
        moonbit_decref(dst$32);
      }
      break;
    }
  } else {
    int32_t _tmp$1011 = len$37 - 1;
    int32_t i$39 = _tmp$1011;
    while (1) {
      if (i$39 >= 0) {
        int32_t _tmp$1007 = dst_offset$34 + i$39;
        int32_t _tmp$1009 = src_offset$35 + i$39;
        moonbit_string_t _tmp$2234;
        moonbit_string_t _tmp$1008;
        moonbit_string_t _old$2233;
        int32_t _tmp$1010;
        if (_tmp$1009 < 0 || _tmp$1009 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2234 = (moonbit_string_t)src$33[_tmp$1009];
        _tmp$1008 = _tmp$2234;
        if (_tmp$1007 < 0 || _tmp$1007 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2233 = (moonbit_string_t)dst$32[_tmp$1007];
        moonbit_incref(_tmp$1008);
        moonbit_decref(_old$2233);
        dst$32[_tmp$1007] = _tmp$1008;
        _tmp$1010 = i$39 - 1;
        i$39 = _tmp$1010;
        continue;
      } else {
        moonbit_decref(src$33);
        moonbit_decref(dst$32);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$23,
  int32_t dst_offset$25,
  moonbit_bytes_t src$24,
  int32_t src_offset$26,
  int32_t len$28
) {
  int32_t _if_result$2494;
  if (dst$23 == src$24) {
    _if_result$2494 = dst_offset$25 < src_offset$26;
  } else {
    _if_result$2494 = 0;
  }
  if (_if_result$2494) {
    int32_t i$27 = 0;
    while (1) {
      if (i$27 < len$28) {
        int32_t _tmp$994 = dst_offset$25 + i$27;
        int32_t _tmp$996 = src_offset$26 + i$27;
        int32_t _tmp$995;
        int32_t _tmp$997;
        if (_tmp$996 < 0 || _tmp$996 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$995 = (int32_t)src$24[_tmp$996];
        if (_tmp$994 < 0 || _tmp$994 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$994] = _tmp$995;
        _tmp$997 = i$27 + 1;
        i$27 = _tmp$997;
        continue;
      } else {
        moonbit_decref(src$24);
        moonbit_decref(dst$23);
      }
      break;
    }
  } else {
    int32_t _tmp$1002 = len$28 - 1;
    int32_t i$30 = _tmp$1002;
    while (1) {
      if (i$30 >= 0) {
        int32_t _tmp$998 = dst_offset$25 + i$30;
        int32_t _tmp$1000 = src_offset$26 + i$30;
        int32_t _tmp$999;
        int32_t _tmp$1001;
        if (_tmp$1000 < 0 || _tmp$1000 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$999 = (int32_t)src$24[_tmp$1000];
        if (_tmp$998 < 0 || _tmp$998 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$998] = _tmp$999;
        _tmp$1001 = i$30 - 1;
        i$30 = _tmp$1001;
        continue;
      } else {
        moonbit_decref(src$24);
        moonbit_decref(dst$23);
      }
      break;
    }
  }
  return 0;
}

int64_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$21,
  moonbit_string_t loc$22
) {
  moonbit_string_t _tmp$993 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$991 =
    moonbit_add_string(
      _tmp$993, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$992 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$990 = moonbit_add_string(_tmp$991, _tmp$992);
  moonbit_string_t _tmp$989 =
    moonbit_add_string(
      _tmp$990, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$989);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$988 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$986 =
    moonbit_add_string(
      _tmp$988, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$987 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$985 = moonbit_add_string(_tmp$986, _tmp$987);
  moonbit_string_t _tmp$984 =
    moonbit_add_string(
      _tmp$985, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$984);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
) {
  moonbit_string_t _tmp$983 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$17);
  moonbit_string_t _tmp$981 =
    moonbit_add_string(
      _tmp$983, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$982 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$18);
  moonbit_string_t _tmp$980 = moonbit_add_string(_tmp$981, _tmp$982);
  moonbit_string_t _tmp$979 =
    moonbit_add_string(
      _tmp$980, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$979);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
) {
  moonbit_string_t _tmp$978 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$15);
  moonbit_string_t _tmp$976 =
    moonbit_add_string(
      _tmp$978, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$977 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$16);
  moonbit_string_t _tmp$975 = moonbit_add_string(_tmp$976, _tmp$977);
  moonbit_string_t _tmp$974 =
    moonbit_add_string(
      _tmp$975, (moonbit_string_t)moonbit_string_literal_32.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$974);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$11,
  struct $$moonbitlang$core$builtin$Logger _x_5272$14
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$12 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$11;
  moonbit_string_t _field$2235 = _Failure$12->$0;
  int32_t _cnt$2352 = Moonbit_object_header(_Failure$12)->rc;
  moonbit_string_t _$2a$err_payload_5273$13;
  struct $$moonbitlang$core$builtin$Logger _bind$973;
  if (_cnt$2352 > 1) {
    int32_t _new_cnt$2353;
    moonbit_incref(_field$2235);
    _new_cnt$2353 = _cnt$2352 - 1;
    Moonbit_object_header(_Failure$12)->rc = _new_cnt$2353;
  } else if (_cnt$2352 == 1) {
    moonbit_free(_Failure$12);
  }
  _$2a$err_payload_5273$13 = _field$2235;
  if (_x_5272$14.$1) {
    moonbit_incref(_x_5272$14.$1);
  }
  _x_5272$14.$0->$method_0(
    _x_5272$14.$1, (moonbit_string_t)moonbit_string_literal_33.data
  );
  if (_x_5272$14.$1) {
    moonbit_incref(_x_5272$14.$1);
  }
  $$moonbitlang$core$builtin$Logger$$write_object$0(
    _x_5272$14, _$2a$err_payload_5273$13
  );
  _bind$973 = _x_5272$14;
  _bind$973.$0->$method_0(
    _bind$973.$1, (moonbit_string_t)moonbit_string_literal_34.data
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
  void* _x_5285$9,
  struct $$moonbitlang$core$builtin$Logger _x_5286$10
) {
  switch (Moonbit_object_tag(_x_5285$9)) {
    case 1: {
      _x_5286$10.$0->$method_0(
        _x_5286$10.$1, (moonbit_string_t)moonbit_string_literal_35.data
      );
      break;
    }
    default: {
      moonbit_decref(_x_5285$9);
      _x_5286$10.$0->$method_0(
        _x_5286$10.$1, (moonbit_string_t)moonbit_string_literal_36.data
      );
      break;
    }
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
  int32_t _x_5289$7,
  int32_t _x_5290$8
) {
  switch (_x_5289$7) {
    case 0: {
      switch (_x_5290$8) {
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
      switch (_x_5290$8) {
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

int32_t $$moonbitlang$core$builtin$Logger$$write_object$0(
  struct $$moonbitlang$core$builtin$Logger self$6,
  moonbit_string_t obj$5
) {
  $$moonbitlang$core$builtin$Show$$String$$output(obj$5, self$6);
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

moonbit_string_t $Error$to_string(void* _e$944) {
  switch (Moonbit_object_tag(_e$944)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$944
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$944
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$944);
      return (moonbit_string_t)moonbit_string_literal_37.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
               _e$944
             );
      break;
    }
    
    case 3: {
      moonbit_decref(_e$944);
      return (moonbit_string_t)moonbit_string_literal_38.data;
      break;
    }
    default: {
      moonbit_decref(_e$944);
      return (moonbit_string_t)moonbit_string_literal_39.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$962,
  int32_t _param$961
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$960 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$962;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$960, _param$961
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$959,
  struct $StringView _param$958
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$957 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$959;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$957, _param$958
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$956,
  moonbit_string_t _param$953,
  int32_t _param$954,
  int32_t _param$955
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$952 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$956;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$952, _param$953, _param$954, _param$955
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$951,
  moonbit_string_t _param$950
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$949 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$951;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$949, _param$950
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$768;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$968;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$967;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$769;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$970;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$969;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$770;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$972;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$971;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$120 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$134 = (int64_t)0;
  _bind$768
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$968 = _bind$768;
  _tmp$967
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$968
  };
  $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$967
  );
  _bind$769
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$970 = _bind$769;
  _tmp$969
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$970
  };
  $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$1(
    _tmp$969
  );
  _bind$770
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$972 = _bind$770;
  _tmp$971
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$972
  };
  $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$971
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$966;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$938;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$939;
  int32_t _len$940;
  int32_t _i$941;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$966
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$938
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$938)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$938->$0 = _tmp$966;
  async_tests$938->$1 = 0;
  _arr$939
  = $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$939);
  _len$940 = $$moonbitlang$core$builtin$Array$$length$0(_arr$939);
  _i$941 = 0;
  while (1) {
    if (_i$941 < _len$940) {
      struct $$3c$String$2a$Int$3e$* arg$942;
      moonbit_string_t _field$2237;
      moonbit_string_t _tmp$963;
      int32_t _field$2236;
      int32_t _cnt$2354;
      int32_t _tmp$964;
      int32_t _tmp$965;
      moonbit_incref(_arr$939);
      arg$942
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$939, _i$941
      );
      _field$2237 = arg$942->$0;
      _tmp$963 = _field$2237;
      _field$2236 = arg$942->$1;
      _cnt$2354 = Moonbit_object_header(arg$942)->rc;
      if (_cnt$2354 > 1) {
        int32_t _new_cnt$2355;
        moonbit_incref(_tmp$963);
        _new_cnt$2355 = _cnt$2354 - 1;
        Moonbit_object_header(arg$942)->rc = _new_cnt$2355;
      } else if (_cnt$2354 == 1) {
        moonbit_free(arg$942);
      }
      _tmp$964 = _field$2236;
      moonbit_incref(async_tests$938);
      $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_do_execute(
        async_tests$938, _tmp$963, _tmp$964
      );
      _tmp$965 = _i$941 + 1;
      _i$941 = _tmp$965;
      continue;
    } else {
      moonbit_decref(_arr$939);
    }
    break;
  }
  $azimuth$telemetry$tests$azimuth$telemetry$advanced_blackbox_test$moonbit_test_driver_internal_run_async_tests(
    async_tests$938
  );
  return 0;
}