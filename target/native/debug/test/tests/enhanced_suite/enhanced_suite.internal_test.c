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

struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$Int$3e$;

struct $$moonbitlang$core$builtin$Array$3c$String$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$3c$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $Ref$3c$Int$3e$;

struct $StringView;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167;

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap;

struct $$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__TestCase;

struct $$3c$$3e$$3d$$3e$Unit;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1;

struct $$3c$Int$2a$Int$3e$;

struct $$3c$String$3e$$3d$$3e$Int;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $Option$3c$Option$3c$Int$3e$$3e$$Some;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $Error$azimuth$telemetry$tests$enhanced_suite$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Int$2a$String$2a$Bool$3e$;

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap;

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$3c$StringView$2a$StringView$3e$;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $Moonbit_Test_Driver_Internal__F$F2;

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$;

struct $$moonbitlang$core$builtin$Hasher;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $Option$3c$StringView$3e$$Some;

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Ok;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$;

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Err {
  void* $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$ {
  int32_t $1;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** $0;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  moonbit_string_t $1;
  
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

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  
};

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  void* $1;
  
};

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167 {
  void* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
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

struct $Option$3c$Option$3c$Int$3e$$3e$$Some {
  int64_t $0;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
};

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $Error$azimuth$telemetry$tests$enhanced_suite$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
  moonbit_string_t $0;
  
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

struct $$3c$Int$2a$String$2a$Bool$3e$ {
  int32_t $0;
  int32_t $2;
  moonbit_string_t $1;
  
};

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap {
  int32_t(* code)(struct $$3c$String$3e$$3d$$3e$Int*, moonbit_string_t);
  struct $$3c$String$3e$$3d$$3e$Int* $0;
  
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

struct $$moonbitlang$core$builtin$Array$3c$Char$3e$ {
  int32_t $1;
  int32_t* $0;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Ok {
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

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2703
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2702
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2701
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2700
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2699
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2698
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2697
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2696
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2695
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2694
);

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1190
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2668,
  moonbit_string_t s$1168,
  int32_t sep$1169
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1155
);

moonbit_string_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2577,
  moonbit_bytes_t bytes$1156
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$2570,
  moonbit_string_t s$1150
);

#define $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1113,
  moonbit_string_t filename$1074,
  int32_t index$1075
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2563
);

struct moonbit_result_0 $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2559
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2557
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2541,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1114,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1115
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2552,
  int32_t _cont_param$1134
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2549,
  void* _cont_param$1135
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$2543,
  void* _state$1117
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2538,
  void* err$1097
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2524,
  moonbit_string_t attr$1090
);

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$2508,
  moonbit_string_t test_name$1077,
  moonbit_string_t file_name$1078,
  moonbit_string_t message$1079,
  int32_t skipped$1080
);

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1072
);

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1070,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1071,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1068
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1031,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1044,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1057,
  moonbit_string_t file_filter$1028,
  int32_t index_filter$1029
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0(
  
);

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1007
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$1005,
  struct $$moonbitlang$core$builtin$Logger logger$1006
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$991,
  struct $$moonbitlang$core$builtin$Logger logger$1004
);

int32_t $$moonbitlang$core$builtin$Show$$Char$$output(
  int32_t self$989,
  struct $$moonbitlang$core$builtin$Logger logger$987
);

int32_t $Char$$is_printable(int32_t self$982);

int32_t $Char$$is_control(int32_t self$981);

moonbit_string_t $Char$$to_hex(int32_t char$980);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$978);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$973,
  moonbit_string_t msg$975,
  moonbit_string_t loc$977
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$968,
  moonbit_string_t msg$970,
  moonbit_string_t loc$972
);

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$967,
  struct $$moonbitlang$core$builtin$Hasher* hasher$966
);

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$965,
  struct $$moonbitlang$core$builtin$Hasher* hasher$964
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$962,
  moonbit_string_t value$960
);

int32_t $Int$$is_surrogate(int32_t self$958);

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$955,
  struct $$3c$String$3e$$3d$$3e$Int* f$957
);

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2193,
  moonbit_string_t k$956
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$953,
  int32_t idx$954
);

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$951,
  int32_t idx$952
);

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$947,
  int32_t key$943
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$938,
  moonbit_string_t key$934
);

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$929,
  int32_t key$925
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$920,
  moonbit_string_t key$916
);

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$911,
  int32_t key$907
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$902,
  moonbit_string_t key$898
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$890
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$882
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$874
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$866
);

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$862,
  moonbit_string_t key$863,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$864
);

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$859,
  moonbit_string_t key$860,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$861
);

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$856,
  int32_t key$857,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$858
);

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$853,
  moonbit_string_t key$854,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$855
);

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$843
);

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$832
);

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$821
);

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$810
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$793,
  moonbit_string_t key$802,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$803,
  int32_t hash$801
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$777,
  moonbit_string_t key$786,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$787,
  int32_t hash$785
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$761,
  int32_t key$770,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$771,
  int32_t hash$769
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$745,
  moonbit_string_t key$754,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$755,
  int32_t hash$753
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$739,
  int32_t idx$744,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$743
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$729,
  int32_t idx$734,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$733
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$719,
  int32_t idx$724,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$723
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$709,
  int32_t idx$714,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$713
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$699,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$701,
  int32_t new_idx$700
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$693,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$695,
  int32_t new_idx$694
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$687,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$689,
  int32_t new_idx$688
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$681,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$683,
  int32_t new_idx$682
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$678,
  int32_t idx$680,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$679
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$674,
  int32_t idx$676,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$675
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$670,
  int32_t idx$672,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$671
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$666,
  int32_t idx$668,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$667
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$660
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$654
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$648
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$642
);

int32_t $Int$$next_power_of_two(int32_t self$640);

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$639);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$637
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$635
);

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$633
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$631
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$3(
  int32_t self$625,
  int32_t other$626
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$2(
  moonbit_string_t self$619,
  moonbit_string_t other$620
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$1(
  void* self$613,
  void* other$614
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  int64_t self$607,
  int64_t other$608
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$606
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$605
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$603
);

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1846
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$3(
  void* self$598,
  struct $$moonbitlang$core$builtin$Logger logger$599
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$2(
  int32_t self$594,
  struct $$moonbitlang$core$builtin$Logger logger$595
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$1(
  moonbit_string_t self$590,
  struct $$moonbitlang$core$builtin$Logger logger$591
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  int64_t self$586,
  struct $$moonbitlang$core$builtin$Logger logger$587
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$585
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$584,
  struct $$moonbitlang$core$builtin$Logger logger$583
);

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$581,
  struct $$moonbitlang$core$builtin$Logger logger$582
);

int32_t $$moonbitlang$core$builtin$Show$$Unit$$output(
  int32_t _self$580,
  struct $$moonbitlang$core$builtin$Logger logger$579
);

int32_t $$moonbitlang$core$builtin$Compare$$String$$compare(
  moonbit_string_t self$573,
  moonbit_string_t other$575
);

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$570,
  struct $$3c$String$3e$$3d$$3e$Int* f$571
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$566,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$568
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$563,
  struct $$3c$String$2a$Int$3e$* value$565
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$560,
  moonbit_string_t value$562
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$558
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$555
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$552
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$548,
  int32_t new_capacity$546
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$542,
  int32_t new_capacity$540
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$536,
  int32_t new_capacity$534
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$532
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$530,
  struct $StringView str$531
);

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$527,
  int32_t i$528,
  int32_t start_offset$529,
  int64_t end_offset$525
);

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$522,
  int32_t n$520,
  int32_t start_offset$516,
  int32_t end_offset$517
);

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$514,
  int32_t n$512,
  int32_t start_offset$511,
  int32_t end_offset$510
);

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$500,
  int32_t len$503,
  int32_t start_offset$507,
  int64_t end_offset$498
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$496
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$495
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$494
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$493
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$492
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$487
);

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1767,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$485
);

moonbit_string_t $$moonbitlang$core$builtin$Iterator$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$484
);

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$475,
  struct $$moonbitlang$core$builtin$Logger logger$473
);

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$469,
  int32_t seg$472,
  int32_t i$471
);

moonbit_string_t $Byte$$to_hex(int32_t b$467);

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$465);

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$463,
  int32_t that$464
);

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$461,
  int32_t that$462
);

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$459,
  int32_t that$460
);

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$457,
  int32_t that$458
);

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$454,
  int32_t start$452,
  int32_t end$453
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$451
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$6(
  void* a$445,
  void* b$446,
  moonbit_string_t msg$448,
  moonbit_string_t loc$450
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$5(
  int32_t a$439,
  int32_t b$440,
  moonbit_string_t msg$442,
  moonbit_string_t loc$444
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  int32_t a$433,
  int32_t b$434,
  moonbit_string_t msg$436,
  moonbit_string_t loc$438
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  moonbit_string_t a$427,
  moonbit_string_t b$428,
  moonbit_string_t msg$430,
  moonbit_string_t loc$432
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  int64_t a$421,
  int64_t b$422,
  moonbit_string_t msg$424,
  moonbit_string_t loc$426
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  moonbit_string_t a$415,
  moonbit_string_t b$416,
  moonbit_string_t msg$418,
  moonbit_string_t loc$420
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  int32_t a$409,
  int32_t b$410,
  moonbit_string_t msg$412,
  moonbit_string_t loc$414
);

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$408,
  moonbit_string_t loc$407
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$6(void* t$406);

moonbit_string_t $moonbitlang$core$builtin$debug_string$5(int32_t t$404);

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(int32_t t$402);

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(
  moonbit_string_t t$400
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(int64_t t$398);

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(
  moonbit_string_t t$396
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(int32_t t$394);

moonbit_string_t $Int$$to_string$inner(int32_t self$377, int32_t radix$376);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$370,
  int32_t radix$373
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$368);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$367);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$357,
  uint32_t num$345,
  int32_t digit_start$348,
  int32_t total_len$347
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$339,
  uint32_t num$333,
  int32_t digit_start$331,
  int32_t total_len$330,
  int32_t radix$335
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$326,
  uint32_t num$322,
  int32_t digit_start$320,
  int32_t total_len$319
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$317
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$315
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$313
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$311
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$309
);

int32_t $StringView$$start_offset(struct $StringView self$307);

moonbit_string_t $StringView$$data(struct $StringView self$306);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$300,
  moonbit_string_t value$303,
  int32_t start$304,
  int32_t len$305
);

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$293,
  int32_t start$299,
  int64_t end$295
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$291
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$289
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$286
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$284
);

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$283
);

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$282
);

int32_t $$moonbitlang$core$builtin$Compare$$$default_impl$$op_ge$0(
  moonbit_string_t x$279,
  moonbit_string_t y$280
);

int32_t $$moonbitlang$core$builtin$Compare$$$default_impl$$op_le$0(
  moonbit_string_t x$277,
  moonbit_string_t y$278
);

int32_t $$moonbitlang$core$builtin$Compare$$$default_impl$$op_lt$0(
  moonbit_string_t x$275,
  moonbit_string_t y$276
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$274,
  int32_t value$273
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$272,
  moonbit_string_t value$271
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$269,
  int32_t value$270
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$267,
  uint32_t value$268
);

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$265,
  uint32_t input$266
);

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$263, int32_t r$264);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$261,
  moonbit_string_t str$262
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$253,
  int32_t bytes_offset$248,
  moonbit_string_t str$255,
  int32_t str_offset$251,
  int32_t length$249
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$215
);

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$211
);

struct $StringView $StringView$$view$inner(
  struct $StringView self$208,
  int32_t start_offset$209,
  int64_t end_offset$206
);

int64_t $StringView$$rev_find(
  struct $StringView self$204,
  struct $StringView str$203
);

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$194,
  struct $StringView needle$196
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$183,
  struct $StringView needle$185
);

int64_t $StringView$$find(
  struct $StringView self$181,
  struct $StringView str$180
);

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$170,
  struct $StringView needle$172
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$156,
  struct $StringView needle$158
);

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$152,
  int32_t index$153
);

int32_t $StringView$$length(struct $StringView self$151);

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* $$moonbitlang$core$builtin$Array$$at$3(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$149,
  int32_t index$150
);

int32_t $$moonbitlang$core$builtin$Array$$at$2(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$146,
  int32_t index$147
);

int32_t $$moonbitlang$core$builtin$Array$$at$1(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$143,
  int32_t index$144
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$140,
  int32_t index$141
);

int32_t $$moonbitlang$core$builtin$Array$$length$5(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$138
);

int32_t $$moonbitlang$core$builtin$Array$$length$4(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$137
);

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$* self$136
);

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$135
);

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$134
);

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$133
);

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$5(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$132
);

int32_t* $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$131
);

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$130
);

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$129
);

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$128
);

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$127
);

moonbit_string_t $String$$escape(moonbit_string_t self$126);

int32_t $moonbitlang$core$builtin$op_notequal$5(int32_t x$123, int32_t y$124);

int32_t $moonbitlang$core$builtin$op_notequal$4(
  moonbit_string_t x$121,
  moonbit_string_t y$122
);

int32_t $moonbitlang$core$builtin$op_notequal$3(void* x$119, void* y$120);

int32_t $moonbitlang$core$builtin$op_notequal$2(int64_t x$117, int64_t y$118);

int32_t $moonbitlang$core$builtin$op_notequal$1(
  moonbit_string_t x$115,
  moonbit_string_t y$116
);

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$113, int32_t y$114);

int32_t $Int$$is_trailing_surrogate(int32_t self$112);

int32_t $Int$$is_leading_surrogate(int32_t self$111);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$108,
  int32_t ch$110
);

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$103,
  int32_t required$104
);

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default();

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$97,
  int32_t offset$98,
  int32_t value$96
);

int32_t $UInt$$to_byte(uint32_t self$94);

uint32_t $Char$$to_uint(int32_t self$93);

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$92
);

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$87,
  int32_t offset$91,
  int64_t length$89
);

#define $moonbitlang$core$builtin$unsafe_sub_string moonbit_unsafe_bytes_sub_string

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$84
);

int32_t $Byte$$to_char(int32_t self$82);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$77,
  int32_t dst_offset$78,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$79,
  int32_t src_offset$80,
  int32_t len$81
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$72,
  int32_t dst_offset$73,
  struct $$3c$String$2a$Int$3e$** src$74,
  int32_t src_offset$75,
  int32_t len$76
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$67,
  int32_t dst_offset$68,
  moonbit_string_t* src$69,
  int32_t src_offset$70,
  int32_t len$71
);

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$58,
  int32_t dst_offset$60,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$59,
  int32_t src_offset$61,
  int32_t len$63
);

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$49,
  int32_t dst_offset$51,
  struct $$3c$String$2a$Int$3e$** src$50,
  int32_t src_offset$52,
  int32_t len$54
);

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$40,
  int32_t dst_offset$42,
  moonbit_string_t* src$41,
  int32_t src_offset$43,
  int32_t len$45
);

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$31,
  int32_t dst_offset$33,
  moonbit_bytes_t src$32,
  int32_t src_offset$34,
  int32_t len$36
);

int64_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$29,
  moonbit_string_t loc$30
);

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$27,
  moonbit_string_t loc$28
);

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$25,
  moonbit_string_t loc$26
);

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$23,
  moonbit_string_t loc$24
);

int32_t $$moonbitlang$core$builtin$Eq$$Unit$$equal(
  int32_t _discard_$21,
  int32_t _discard_$22
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$17,
  struct $$moonbitlang$core$builtin$Logger _x_5272$20
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
  void* _x_5285$15,
  struct $$moonbitlang$core$builtin$Logger _x_5286$16
);

int32_t $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
  int32_t _x_5289$13,
  int32_t _x_5290$14
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$3(
  struct $$moonbitlang$core$builtin$Logger self$12,
  int64_t obj$11
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$2(
  struct $$moonbitlang$core$builtin$Logger self$10,
  int32_t obj$9
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$1(
  struct $$moonbitlang$core$builtin$Logger self$8,
  moonbit_string_t obj$7
);

int32_t $$moonbitlang$core$builtin$Logger$$write_object$0(
  struct $$moonbitlang$core$builtin$Logger self$6,
  int32_t obj$5
);

int64_t $moonbitlang$core$abort$abort$3(moonbit_string_t msg$4);

struct $StringView $moonbitlang$core$abort$abort$2(moonbit_string_t msg$3);

int32_t $moonbitlang$core$abort$abort$1(moonbit_string_t msg$2);

int32_t $moonbitlang$core$abort$abort$0(moonbit_string_t msg$1);

moonbit_string_t $Error$to_string(void* _e$1197);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1215,
  int32_t _param$1214
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1212,
  struct $StringView _param$1211
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1209,
  moonbit_string_t _param$1206,
  int32_t _param$1207,
  int32_t _param$1208
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1204,
  moonbit_string_t _param$1203
);

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    104, 101, 108, 108, 111, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[1]; 
} const moonbit_string_literal_3 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 0), 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_85 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 55, 58, 51, 45, 49, 55, 
    58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_43 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 97, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_6 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_55 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 55, 51, 58, 51, 45, 55, 51, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 49, 48, 58, 51, 45, 49, 
    49, 48, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_53 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 55, 49, 58, 51, 45, 55, 49, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_129 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_56 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 55, 53, 58, 51, 45, 55, 53, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_96 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_59 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 53, 53, 58, 51, 45, 53, 53, 
    58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_32 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 57, 55, 58, 51, 45, 57, 55, 
    58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_104 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    78, 111, 110, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_88 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 50, 48, 58, 51, 45, 50, 48, 
    58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_84 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 54, 58, 51, 45, 49, 54, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_47 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 56, 56, 58, 51, 45, 56, 56, 
    58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_118 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 56, 50, 58, 51, 45, 56, 50, 
    58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 50, 56, 58, 51, 45, 49, 
    50, 56, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[54]; 
} const moonbit_string_literal_121 =
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
} const moonbit_string_literal_117 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_106 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 41, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_63 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_105 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    83, 111, 109, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_78 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 50, 57, 58, 51, 45, 50, 57, 
    58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_87 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 57, 58, 51, 45, 49, 57, 
    58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_75 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 49, 52, 58, 51, 45, 49, 
    49, 52, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_0 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 58, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_111 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_135 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    25968, 32452, 25805, 20316, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_132 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    22522, 30784, 31639, 26415, 36816, 31639, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_20 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 51, 48, 58, 51, 45, 49, 
    51, 48, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_133 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    24067, 23572, 36923, 36753, 36816, 31639, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_83 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 53, 58, 51, 45, 49, 53, 
    58, 50, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    98, 97, 110, 97, 110, 97, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_58 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 55, 55, 58, 51, 45, 55, 55, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_103 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 102, 97, 108, 115, 101, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_80 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 51, 48, 58, 51, 45, 51, 48, 
    58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_102 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_99 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_97 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_51 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 54, 57, 58, 51, 45, 54, 57, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_1 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 45, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_45 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_37 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    97, 112, 112, 108, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_69 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 98, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[73]; 
} const moonbit_string_literal_93 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 72), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 56, 58, 51, 45, 56, 58, 50, 
    51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_54 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 55, 50, 58, 51, 45, 55, 50, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 50, 52, 58, 51, 45, 49, 
    50, 52, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_77 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 50, 56, 58, 51, 45, 50, 56, 
    58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_30 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 49, 53, 58, 51, 45, 49, 
    49, 53, 58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_79 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    97, 122, 105, 109, 117, 116, 104, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_28 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 49, 50, 58, 51, 45, 49, 
    49, 50, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_44 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 56, 53, 58, 51, 45, 56, 53, 
    58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_130 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 97, 
    110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 46, 77, 111, 111, 
    110, 66, 105, 116, 84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 
    73, 110, 116, 101, 114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 
    114, 46, 77, 111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 68, 
    114, 105, 118, 101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 74, 
    115, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_110 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    73, 110, 118, 97, 108, 105, 100, 32, 115, 116, 97, 114, 116, 32, 
    105, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_64 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 54, 50, 58, 51, 45, 54, 50, 
    58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_27 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 49, 49, 58, 51, 45, 49, 
    49, 49, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[24]; 
} const moonbit_string_literal_142 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 23), 
    97, 122, 105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 
    101, 115, 116, 115, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_72 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 52, 52, 58, 51, 45, 52, 52, 
    58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_48 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 56, 57, 58, 51, 45, 56, 57, 
    58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_114 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    32, 33, 61, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_95 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 48, 58, 51, 45, 49, 48, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 57, 54, 58, 51, 45, 57, 54, 
    58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_4 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_139 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    25968, 32452, 32034, 24341, 21644, 20999, 29255, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_113 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 52, 57, 49, 58, 57, 45, 
    52, 57, 49, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_112 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 56, 51, 58, 51, 45, 56, 51, 
    58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_124 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 50, 51, 58, 51, 45, 49, 
    50, 51, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_116 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_115 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    32, 70, 65, 73, 76, 69, 68, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 51, 54, 58, 51, 45, 49, 
    51, 54, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_62 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 53, 56, 58, 51, 45, 53, 56, 
    58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_137 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    25968, 20540, 27604, 36739, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_108 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    102, 97, 108, 115, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_25 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 48, 57, 58, 51, 45, 49, 
    48, 57, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_36 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 48, 51, 58, 51, 45, 49, 
    48, 51, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_22 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 51, 53, 58, 51, 45, 49, 
    51, 53, 58, 52, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_127 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    73, 110, 100, 101, 120, 79, 117, 116, 79, 102, 66, 111, 117, 110, 
    100, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_46 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 56, 54, 58, 51, 45, 56, 54, 
    58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_5 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 32, 98, 
    108, 111, 99, 107, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_68 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 52, 48, 58, 51, 45, 52, 48, 
    58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_35 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 48, 50, 58, 51, 45, 49, 
    48, 50, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_134 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    23383, 31526, 20018, 25805, 20316, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[73]; 
} const moonbit_string_literal_94 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 72), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 57, 58, 51, 45, 57, 58, 50, 
    51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_73 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 52, 55, 58, 51, 45, 52, 55, 
    58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_66 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 51, 56, 58, 51, 45, 51, 56, 
    58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_138 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    23383, 31526, 20018, 27604, 36739, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_119 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_109 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    40, 41, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_81 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 51, 49, 58, 51, 45, 51, 49, 
    58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_74 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_71 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 52, 51, 58, 51, 45, 52, 51, 
    58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_131 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 73, 110, 115, 
    112, 101, 99, 116, 69, 114, 114, 111, 114, 46, 73, 110, 115, 112, 
    101, 99, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_125 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 10, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_13 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    45, 45, 45, 45, 45, 32, 69, 78, 68, 32, 77, 79, 79, 78, 32, 84, 69, 
    83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[73]; 
} const moonbit_string_literal_92 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 72), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 55, 58, 51, 45, 55, 58, 50, 
    50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_15 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 50, 50, 58, 51, 45, 49, 
    50, 50, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_126 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    70, 97, 105, 108, 117, 114, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_65 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 54, 51, 58, 51, 45, 54, 51, 
    58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_10 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    34, 44, 32, 34, 116, 101, 115, 116, 95, 110, 97, 109, 101, 34, 58, 
    32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    123, 34, 112, 97, 99, 107, 97, 103, 101, 34, 58, 32, 34, 97, 122, 
    105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 
    121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 97, 110, 99, 
    101, 100, 95, 115, 117, 105, 116, 101, 34, 44, 32, 34, 102, 105, 
    108, 101, 110, 97, 109, 101, 34, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_70 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_141 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    22797, 21512, 31867, 22411, 25805, 20316, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    45, 45, 45, 45, 45, 32, 66, 69, 71, 73, 78, 32, 77, 79, 79, 78, 32, 
    84, 69, 83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_86 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 56, 58, 51, 45, 49, 56, 
    58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_89 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 50, 49, 58, 51, 45, 50, 49, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_61 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 53, 55, 58, 51, 45, 53, 55, 
    58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_50 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 54, 56, 58, 51, 45, 54, 56, 
    58, 50, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_123 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 48, 58, 53, 45, 50, 
    57, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_57 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 55, 54, 58, 51, 45, 55, 54, 
    58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_120 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    73, 110, 118, 97, 108, 105, 100, 32, 105, 110, 100, 101, 120, 32, 
    102, 111, 114, 32, 86, 105, 101, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_140 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    36127, 25968, 36816, 31639, 27979, 35797, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_101 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 96, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 57, 56, 58, 51, 45, 57, 56, 
    58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_122 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_100 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    92, 117, 123, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_136 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    79, 112, 116, 105, 111, 110, 31867, 22411, 25805, 20316, 27979, 35797, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 48, 56, 58, 51, 45, 49, 
    48, 56, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 51, 49, 58, 51, 45, 49, 
    51, 49, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_107 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[73]; 
} const moonbit_string_literal_91 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 72), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 54, 58, 51, 45, 54, 58, 50, 
    50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_76 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 50, 55, 58, 51, 45, 50, 55, 
    58, 52, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 50, 57, 58, 51, 45, 49, 
    50, 57, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_60 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 53, 54, 58, 51, 45, 53, 54, 
    58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_98 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[77]; 
} const moonbit_string_literal_34 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 76), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 49, 48, 49, 58, 51, 45, 49, 
    48, 49, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_82 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 51, 50, 58, 51, 45, 51, 50, 
    58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_49 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 57, 48, 58, 51, 45, 57, 48, 
    58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_52 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 55, 48, 58, 51, 45, 55, 48, 
    58, 50, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_128 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    73, 110, 118, 97, 108, 105, 100, 73, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_90 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 50, 50, 58, 51, 45, 50, 50, 
    58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_67 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 51, 57, 58, 51, 45, 51, 57, 
    58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[75]; 
} const moonbit_string_literal_42 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 74), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 
    97, 110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 58, 97, 122, 
    105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 101, 
    115, 116, 115, 46, 109, 98, 116, 58, 56, 52, 58, 51, 45, 56, 52, 
    58, 51, 52, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6$dyncall
  };

struct { int32_t rc; uint32_t meta; struct $$3c$String$3e$$3d$$3e$Int data; 
} const $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$5
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1$dyncall
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

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$154;

int64_t $moonbitlang$core$builtin$brute_force_find$constr$168;

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_with_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_no_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_async_tests;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2$clo;

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2703
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2();
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2702
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7();
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2701
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5();
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2700
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1();
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2699
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4();
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2698
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0();
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2697
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8();
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2696
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6();
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2695
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3();
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2694
) {
  return $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9();
}

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1190
) {
  moonbit_decref(_tests$1190);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$1149 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1155 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$1162 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1155;
  int32_t moonbit_test_driver_internal_split_mbt_string$1167 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$2693 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$1174 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$1175;
  moonbit_string_t _tmp$2692;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$1176;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$1177;
  int32_t _len$1178;
  int32_t _i$1179;
  Moonbit_object_header(file_and_index$1174)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$1174->$0 = _tmp$2693;
  file_and_index$1174->$1 = 0;
  cli_args$1175
  = $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$1162
  );
  _tmp$2692 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$1175, 1);
  test_args$1176
  = $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$1167, _tmp$2692, 47
  );
  _arr$1177 = test_args$1176;
  moonbit_incref(_arr$1177);
  _len$1178 = $$moonbitlang$core$builtin$Array$$length$1(_arr$1177);
  _i$1179 = 0;
  while (1) {
    if (_i$1179 < _len$1178) {
      moonbit_string_t arg$1180;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$1181;
      moonbit_string_t file$1182;
      moonbit_string_t range$1183;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1184;
      moonbit_string_t _tmp$2690;
      int32_t start$1185;
      moonbit_string_t _tmp$2689;
      int32_t end$1186;
      int32_t i$1187;
      int32_t _tmp$2691;
      moonbit_incref(_arr$1177);
      arg$1180
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$1177, _i$1179
      );
      file_and_range$1181
      = $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1167, arg$1180, 58
      );
      moonbit_incref(file_and_range$1181);
      file$1182
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1181, 0
      );
      range$1183
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1181, 1
      );
      start_and_end$1184
      = $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1167, range$1183, 45
      );
      moonbit_incref(start_and_end$1184);
      _tmp$2690
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1184, 0
      );
      start$1185
      = $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1149, _tmp$2690
      );
      _tmp$2689
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1184, 1
      );
      end$1186
      = $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1149, _tmp$2689
      );
      i$1187 = start$1185;
      while (1) {
        if (i$1187 < end$1186) {
          struct $$3c$String$2a$Int$3e$* _tuple$2687;
          int32_t _tmp$2688;
          moonbit_incref(file$1182);
          _tuple$2687
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$2687)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$2687->$0 = file$1182;
          _tuple$2687->$1 = i$1187;
          moonbit_incref(file_and_index$1174);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$1174, _tuple$2687
          );
          _tmp$2688 = i$1187 + 1;
          i$1187 = _tmp$2688;
          continue;
        } else {
          moonbit_decref(file$1182);
        }
        break;
      }
      _tmp$2691 = _i$1179 + 1;
      _i$1179 = _tmp$2691;
      continue;
    } else {
      moonbit_decref(_arr$1177);
    }
    break;
  }
  return file_and_index$1174;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2668,
  moonbit_string_t s$1168,
  int32_t sep$1169
) {
  moonbit_string_t* _tmp$2686 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1170 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$1171;
  struct $Ref$3c$Int$3e$* start$1172;
  int32_t val$2681;
  int32_t _tmp$2682;
  Moonbit_object_header(res$1170)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$1170->$0 = _tmp$2686;
  res$1170->$1 = 0;
  i$1171
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1171)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1171->$0 = 0;
  start$1172
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$1172)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$1172->$0 = 0;
  while (1) {
    int32_t val$2669 = i$1171->$0;
    int32_t _tmp$2670 = Moonbit_array_length(s$1168);
    if (val$2669 < _tmp$2670) {
      int32_t val$2673 = i$1171->$0;
      int32_t _tmp$2672;
      int32_t _tmp$2671;
      int32_t val$2680;
      int32_t _tmp$2679;
      if (val$2673 < 0 || val$2673 >= Moonbit_array_length(s$1168)) {
        moonbit_panic();
      }
      _tmp$2672 = s$1168[val$2673];
      _tmp$2671 = _tmp$2672;
      if (_tmp$2671 == sep$1169) {
        int32_t val$2675 = start$1172->$0;
        int32_t val$2676 = i$1171->$0;
        moonbit_string_t _tmp$2674;
        int32_t val$2678;
        int32_t _tmp$2677;
        moonbit_incref(s$1168);
        _tmp$2674 = $String$$unsafe_substring(s$1168, val$2675, val$2676);
        moonbit_incref(res$1170);
        $$moonbitlang$core$builtin$Array$$push$0(res$1170, _tmp$2674);
        val$2678 = i$1171->$0;
        _tmp$2677 = val$2678 + 1;
        start$1172->$0 = _tmp$2677;
      }
      val$2680 = i$1171->$0;
      _tmp$2679 = val$2680 + 1;
      i$1171->$0 = _tmp$2679;
      continue;
    } else {
      moonbit_decref(i$1171);
    }
    break;
  }
  val$2681 = start$1172->$0;
  _tmp$2682 = Moonbit_array_length(s$1168);
  if (val$2681 < _tmp$2682) {
    int32_t _field$2704 = start$1172->$0;
    int32_t val$2684;
    int32_t _tmp$2685;
    moonbit_string_t _tmp$2683;
    moonbit_decref(start$1172);
    val$2684 = _field$2704;
    _tmp$2685 = Moonbit_array_length(s$1168);
    _tmp$2683 = $String$$unsafe_substring(s$1168, val$2684, _tmp$2685);
    moonbit_incref(res$1170);
    $$moonbitlang$core$builtin$Array$$push$0(res$1170, _tmp$2683);
  } else {
    moonbit_decref(start$1172);
    moonbit_decref(s$1168);
  }
  return res$1170;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1155
) {
  moonbit_bytes_t* tmp$1163 =
    $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$2667 = Moonbit_array_length(tmp$1163);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1164 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$2667);
  int32_t i$1165 = 0;
  while (1) {
    int32_t _tmp$2663 = Moonbit_array_length(tmp$1163);
    if (i$1165 < _tmp$2663) {
      moonbit_bytes_t _tmp$2705;
      moonbit_bytes_t _tmp$2665;
      moonbit_string_t _tmp$2664;
      int32_t _tmp$2666;
      if (i$1165 < 0 || i$1165 >= Moonbit_array_length(tmp$1163)) {
        moonbit_panic();
      }
      _tmp$2705 = (moonbit_bytes_t)tmp$1163[i$1165];
      _tmp$2665 = _tmp$2705;
      moonbit_incref(_tmp$2665);
      _tmp$2664
      = $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1155, _tmp$2665
      );
      moonbit_incref(res$1164);
      $$moonbitlang$core$builtin$Array$$push$0(res$1164, _tmp$2664);
      _tmp$2666 = i$1165 + 1;
      i$1165 = _tmp$2666;
      continue;
    } else {
      moonbit_decref(tmp$1163);
    }
    break;
  }
  return res$1164;
}

moonbit_string_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2577,
  moonbit_bytes_t bytes$1156
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$1157 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$1158 = Moonbit_array_length(bytes$1156);
  struct $Ref$3c$Int$3e$* i$1159 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1159)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1159->$0 = 0;
  while (1) {
    int32_t val$2578 = i$1159->$0;
    if (val$2578 < len$1158) {
      int32_t val$2662 = i$1159->$0;
      int32_t _tmp$2661;
      int32_t _tmp$2660;
      struct $Ref$3c$Int$3e$* c$1160;
      int32_t val$2579;
      if (val$2662 < 0 || val$2662 >= Moonbit_array_length(bytes$1156)) {
        moonbit_panic();
      }
      _tmp$2661 = bytes$1156[val$2662];
      _tmp$2660 = (int32_t)_tmp$2661;
      c$1160
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$1160)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$1160->$0 = _tmp$2660;
      val$2579 = c$1160->$0;
      if (val$2579 < 128) {
        int32_t _field$2706 = c$1160->$0;
        int32_t val$2581;
        int32_t _tmp$2580;
        int32_t val$2583;
        int32_t _tmp$2582;
        moonbit_decref(c$1160);
        val$2581 = _field$2706;
        _tmp$2580 = val$2581;
        moonbit_incref(res$1157);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$1157, _tmp$2580
        );
        val$2583 = i$1159->$0;
        _tmp$2582 = val$2583 + 1;
        i$1159->$0 = _tmp$2582;
      } else {
        int32_t val$2584 = c$1160->$0;
        if (val$2584 < 224) {
          int32_t val$2586 = i$1159->$0;
          int32_t _tmp$2585 = val$2586 + 1;
          int32_t val$2595;
          int32_t _tmp$2594;
          int32_t _tmp$2588;
          int32_t val$2593;
          int32_t _tmp$2592;
          int32_t _tmp$2591;
          int32_t _tmp$2590;
          int32_t _tmp$2589;
          int32_t _tmp$2587;
          int32_t _field$2707;
          int32_t val$2597;
          int32_t _tmp$2596;
          int32_t val$2599;
          int32_t _tmp$2598;
          if (_tmp$2585 >= len$1158) {
            moonbit_decref(c$1160);
            moonbit_decref(i$1159);
            moonbit_decref(bytes$1156);
            break;
          }
          val$2595 = c$1160->$0;
          _tmp$2594 = val$2595 & 31;
          _tmp$2588 = _tmp$2594 << 6;
          val$2593 = i$1159->$0;
          _tmp$2592 = val$2593 + 1;
          if (_tmp$2592 < 0 || _tmp$2592 >= Moonbit_array_length(bytes$1156)) {
            moonbit_panic();
          }
          _tmp$2591 = bytes$1156[_tmp$2592];
          _tmp$2590 = (int32_t)_tmp$2591;
          _tmp$2589 = _tmp$2590 & 63;
          _tmp$2587 = _tmp$2588 | _tmp$2589;
          c$1160->$0 = _tmp$2587;
          _field$2707 = c$1160->$0;
          moonbit_decref(c$1160);
          val$2597 = _field$2707;
          _tmp$2596 = val$2597;
          moonbit_incref(res$1157);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$1157, _tmp$2596
          );
          val$2599 = i$1159->$0;
          _tmp$2598 = val$2599 + 2;
          i$1159->$0 = _tmp$2598;
        } else {
          int32_t val$2600 = c$1160->$0;
          if (val$2600 < 240) {
            int32_t val$2602 = i$1159->$0;
            int32_t _tmp$2601 = val$2602 + 2;
            int32_t val$2618;
            int32_t _tmp$2617;
            int32_t _tmp$2610;
            int32_t val$2616;
            int32_t _tmp$2615;
            int32_t _tmp$2614;
            int32_t _tmp$2613;
            int32_t _tmp$2612;
            int32_t _tmp$2611;
            int32_t _tmp$2604;
            int32_t val$2609;
            int32_t _tmp$2608;
            int32_t _tmp$2607;
            int32_t _tmp$2606;
            int32_t _tmp$2605;
            int32_t _tmp$2603;
            int32_t _field$2708;
            int32_t val$2620;
            int32_t _tmp$2619;
            int32_t val$2622;
            int32_t _tmp$2621;
            if (_tmp$2601 >= len$1158) {
              moonbit_decref(c$1160);
              moonbit_decref(i$1159);
              moonbit_decref(bytes$1156);
              break;
            }
            val$2618 = c$1160->$0;
            _tmp$2617 = val$2618 & 15;
            _tmp$2610 = _tmp$2617 << 12;
            val$2616 = i$1159->$0;
            _tmp$2615 = val$2616 + 1;
            if (
              _tmp$2615 < 0 || _tmp$2615 >= Moonbit_array_length(bytes$1156)
            ) {
              moonbit_panic();
            }
            _tmp$2614 = bytes$1156[_tmp$2615];
            _tmp$2613 = (int32_t)_tmp$2614;
            _tmp$2612 = _tmp$2613 & 63;
            _tmp$2611 = _tmp$2612 << 6;
            _tmp$2604 = _tmp$2610 | _tmp$2611;
            val$2609 = i$1159->$0;
            _tmp$2608 = val$2609 + 2;
            if (
              _tmp$2608 < 0 || _tmp$2608 >= Moonbit_array_length(bytes$1156)
            ) {
              moonbit_panic();
            }
            _tmp$2607 = bytes$1156[_tmp$2608];
            _tmp$2606 = (int32_t)_tmp$2607;
            _tmp$2605 = _tmp$2606 & 63;
            _tmp$2603 = _tmp$2604 | _tmp$2605;
            c$1160->$0 = _tmp$2603;
            _field$2708 = c$1160->$0;
            moonbit_decref(c$1160);
            val$2620 = _field$2708;
            _tmp$2619 = val$2620;
            moonbit_incref(res$1157);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1157, _tmp$2619
            );
            val$2622 = i$1159->$0;
            _tmp$2621 = val$2622 + 3;
            i$1159->$0 = _tmp$2621;
          } else {
            int32_t val$2624 = i$1159->$0;
            int32_t _tmp$2623 = val$2624 + 3;
            int32_t val$2647;
            int32_t _tmp$2646;
            int32_t _tmp$2639;
            int32_t val$2645;
            int32_t _tmp$2644;
            int32_t _tmp$2643;
            int32_t _tmp$2642;
            int32_t _tmp$2641;
            int32_t _tmp$2640;
            int32_t _tmp$2632;
            int32_t val$2638;
            int32_t _tmp$2637;
            int32_t _tmp$2636;
            int32_t _tmp$2635;
            int32_t _tmp$2634;
            int32_t _tmp$2633;
            int32_t _tmp$2626;
            int32_t val$2631;
            int32_t _tmp$2630;
            int32_t _tmp$2629;
            int32_t _tmp$2628;
            int32_t _tmp$2627;
            int32_t _tmp$2625;
            int32_t val$2649;
            int32_t _tmp$2648;
            int32_t val$2653;
            int32_t _tmp$2652;
            int32_t _tmp$2651;
            int32_t _tmp$2650;
            int32_t _field$2709;
            int32_t val$2657;
            int32_t _tmp$2656;
            int32_t _tmp$2655;
            int32_t _tmp$2654;
            int32_t val$2659;
            int32_t _tmp$2658;
            if (_tmp$2623 >= len$1158) {
              moonbit_decref(c$1160);
              moonbit_decref(i$1159);
              moonbit_decref(bytes$1156);
              break;
            }
            val$2647 = c$1160->$0;
            _tmp$2646 = val$2647 & 7;
            _tmp$2639 = _tmp$2646 << 18;
            val$2645 = i$1159->$0;
            _tmp$2644 = val$2645 + 1;
            if (
              _tmp$2644 < 0 || _tmp$2644 >= Moonbit_array_length(bytes$1156)
            ) {
              moonbit_panic();
            }
            _tmp$2643 = bytes$1156[_tmp$2644];
            _tmp$2642 = (int32_t)_tmp$2643;
            _tmp$2641 = _tmp$2642 & 63;
            _tmp$2640 = _tmp$2641 << 12;
            _tmp$2632 = _tmp$2639 | _tmp$2640;
            val$2638 = i$1159->$0;
            _tmp$2637 = val$2638 + 2;
            if (
              _tmp$2637 < 0 || _tmp$2637 >= Moonbit_array_length(bytes$1156)
            ) {
              moonbit_panic();
            }
            _tmp$2636 = bytes$1156[_tmp$2637];
            _tmp$2635 = (int32_t)_tmp$2636;
            _tmp$2634 = _tmp$2635 & 63;
            _tmp$2633 = _tmp$2634 << 6;
            _tmp$2626 = _tmp$2632 | _tmp$2633;
            val$2631 = i$1159->$0;
            _tmp$2630 = val$2631 + 3;
            if (
              _tmp$2630 < 0 || _tmp$2630 >= Moonbit_array_length(bytes$1156)
            ) {
              moonbit_panic();
            }
            _tmp$2629 = bytes$1156[_tmp$2630];
            _tmp$2628 = (int32_t)_tmp$2629;
            _tmp$2627 = _tmp$2628 & 63;
            _tmp$2625 = _tmp$2626 | _tmp$2627;
            c$1160->$0 = _tmp$2625;
            val$2649 = c$1160->$0;
            _tmp$2648 = val$2649 - 65536;
            c$1160->$0 = _tmp$2648;
            val$2653 = c$1160->$0;
            _tmp$2652 = val$2653 >> 10;
            _tmp$2651 = _tmp$2652 + 55296;
            _tmp$2650 = _tmp$2651;
            moonbit_incref(res$1157);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1157, _tmp$2650
            );
            _field$2709 = c$1160->$0;
            moonbit_decref(c$1160);
            val$2657 = _field$2709;
            _tmp$2656 = val$2657 & 1023;
            _tmp$2655 = _tmp$2656 + 56320;
            _tmp$2654 = _tmp$2655;
            moonbit_incref(res$1157);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1157, _tmp$2654
            );
            val$2659 = i$1159->$0;
            _tmp$2658 = val$2659 + 4;
            i$1159->$0 = _tmp$2658;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$1159);
      moonbit_decref(bytes$1156);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$1157);
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$2570,
  moonbit_string_t s$1150
) {
  struct $Ref$3c$Int$3e$* res$1151 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$1152;
  int32_t i$1153;
  int32_t _field$2710;
  Moonbit_object_header(res$1151)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$1151->$0 = 0;
  len$1152 = Moonbit_array_length(s$1150);
  i$1153 = 0;
  while (1) {
    if (i$1153 < len$1152) {
      int32_t val$2575 = res$1151->$0;
      int32_t _tmp$2572 = val$2575 * 10;
      int32_t _tmp$2574;
      int32_t _tmp$2573;
      int32_t _tmp$2571;
      int32_t _tmp$2576;
      if (i$1153 < 0 || i$1153 >= Moonbit_array_length(s$1150)) {
        moonbit_panic();
      }
      _tmp$2574 = s$1150[i$1153];
      _tmp$2573 = _tmp$2574 - 48;
      _tmp$2571 = _tmp$2572 + _tmp$2573;
      res$1151->$0 = _tmp$2571;
      _tmp$2576 = i$1153 + 1;
      i$1153 = _tmp$2576;
      continue;
    } else {
      moonbit_decref(s$1150);
    }
    break;
  }
  _field$2710 = res$1151->$0;
  moonbit_decref(res$1151);
  return _field$2710;
}

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1113,
  moonbit_string_t filename$1074,
  int32_t index$1075
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$1073;
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$3144;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1076;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$1085;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2720;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$2569;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2719;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$1086;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2718;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$2568;
  moonbit_string_t _field$2717;
  moonbit_string_t file_name$1087;
  moonbit_string_t name$1088;
  int32_t _tmp$2565;
  moonbit_string_t name$1089;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$2522;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$2523;
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$3146;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1096;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1112;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1137;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1139;
  void* _field$2714;
  int32_t _cnt$3009;
  void* _bind$1140;
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$3150;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$2562;
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$3151;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2555;
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$3152;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$2556;
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$3153;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2540;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_async_tests
  );
  filtered_test$1073
  = $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_async_tests,
      filename$1074,
      index$1075
  );
  _closure$3144
  = (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$3144)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$3144->code
  = &$$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$3144->$0 = index$1075;
  handle_result$1076
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$3144;
  if (filtered_test$1073 == 0) {
    moonbit_decref(async_tests$1113);
    if (filtered_test$1073) {
      moonbit_decref(filtered_test$1073);
    }
    $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1076,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$1147 =
      filtered_test$1073;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$1148 = _Some$1147;
    item$1085 = _item$1148;
    goto $join$1084;
  }
  goto $joinlet$3145;
  $join$1084:;
  _field$2720 = item$1085->$1;
  meta$2569 = _field$2720;
  _field$2719 = meta$2569->$2;
  attrs$1086 = _field$2719;
  _field$2718 = item$1085->$1;
  meta$2568 = _field$2718;
  _field$2717 = meta$2568->$0;
  file_name$1087 = _field$2717;
  moonbit_incref(attrs$1086);
  moonbit_incref(file_name$1087);
  moonbit_incref(attrs$1086);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$1086)) {
    name$1088 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$1086);
    name$1088 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1086, 0);
  }
  _tmp$2565 = Moonbit_array_length(name$1088);
  if (_tmp$2565 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2716;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$2567;
    int32_t _field$2715;
    int32_t index$2566;
    moonbit_decref(name$1088);
    _field$2716 = item$1085->$1;
    meta$2567 = _field$2716;
    _field$2715 = meta$2567->$1;
    index$2566 = _field$2715;
    name$1089 = $Int$$to_string$inner(index$2566, 10);
  } else {
    name$1089 = name$1088;
  }
  moonbit_incref(attrs$1086);
  _tmp$2522 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$1086);
  _tmp$2523
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$2522, _tmp$2523)) {
    moonbit_string_t _tmp$2537;
    moonbit_string_t _tmp$2536;
    moonbit_string_t _tmp$2533;
    moonbit_string_t _tmp$2535;
    moonbit_string_t _tmp$2534;
    moonbit_string_t _tmp$2532;
    moonbit_decref(async_tests$1113);
    moonbit_decref(item$1085);
    moonbit_incref(file_name$1087);
    _tmp$2537
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$1087
    );
    _tmp$2536
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$2537
    );
    _tmp$2533
    = moonbit_add_string(
      _tmp$2536, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$2535 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1086, 0);
    _tmp$2534 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$2535);
    _tmp$2532 = moonbit_add_string(_tmp$2533, _tmp$2534);
    $moonbitlang$core$builtin$println$0(_tmp$2532);
    $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1076,
        name$1089,
        file_name$1087,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$1086);
  }
  moonbit_incref(name$1089);
  moonbit_incref(file_name$1087);
  moonbit_incref(handle_result$1076);
  _closure$3146
  = (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$3146)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$3146->code
  = &$$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$3146->$0 = name$1089;
  _closure$3146->$1 = file_name$1087;
  _closure$3146->$2 = handle_result$1076;
  on_err$1096 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$3146;
  _field$2714 = item$1085->$0;
  _cnt$3009 = Moonbit_object_header(item$1085)->rc;
  if (_cnt$3009 > 1) {
    int32_t _new_cnt$3011;
    moonbit_incref(_field$2714);
    _new_cnt$3011 = _cnt$3009 - 1;
    Moonbit_object_header(item$1085)->rc = _new_cnt$3011;
  } else if (_cnt$3009 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$3010 = item$1085->$1;
    moonbit_decref(_field$3010);
    moonbit_free(item$1085);
  }
  _bind$1140 = _field$2714;
  switch (Moonbit_object_tag(_bind$1140)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$1141;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2711;
      int32_t _cnt$3012;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1142;
      moonbit_decref(async_tests$1113);
      _F0$1141 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$1140;
      _field$2711 = _F0$1141->$0;
      _cnt$3012 = Moonbit_object_header(_F0$1141)->rc;
      if (_cnt$3012 > 1) {
        int32_t _new_cnt$3013;
        moonbit_incref(_field$2711);
        _new_cnt$3013 = _cnt$3012 - 1;
        Moonbit_object_header(_F0$1141)->rc = _new_cnt$3013;
      } else if (_cnt$3012 == 1) {
        moonbit_free(_F0$1141);
      }
      _f$1142 = _field$2711;
      f$1139 = _f$1142;
      goto $join$1138;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$1143;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2712;
      int32_t _cnt$3014;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1144;
      moonbit_decref(async_tests$1113);
      _F1$1143 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$1140;
      _field$2712 = _F1$1143->$0;
      _cnt$3014 = Moonbit_object_header(_F1$1143)->rc;
      if (_cnt$3014 > 1) {
        int32_t _new_cnt$3015;
        moonbit_incref(_field$2712);
        _new_cnt$3015 = _cnt$3014 - 1;
        Moonbit_object_header(_F1$1143)->rc = _new_cnt$3015;
      } else if (_cnt$3014 == 1) {
        moonbit_free(_F1$1143);
      }
      _f$1144 = _field$2712;
      f$1137 = _f$1144;
      goto $join$1136;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$1145 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$1140;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2713 =
        _F2$1145->$0;
      int32_t _cnt$3016 = Moonbit_object_header(_F2$1145)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$1146;
      if (_cnt$3016 > 1) {
        int32_t _new_cnt$3017;
        moonbit_incref(_field$2713);
        _new_cnt$3017 = _cnt$3016 - 1;
        Moonbit_object_header(_F2$1145)->rc = _new_cnt$3017;
      } else if (_cnt$3016 == 1) {
        moonbit_free(_F2$1145);
      }
      _f$1146 = _field$2713;
      f$1112 = _f$1146;
      goto $join$1111;
      break;
    }
  }
  goto $joinlet$3149;
  $join$1138:;
  _closure$3150
  = (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$3150)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$3150->code
  = &$$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$3150->$0 = name$1089;
  _closure$3150->$1 = file_name$1087;
  _closure$3150->$2 = handle_result$1076;
  _tmp$2562 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$3150;
  $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_catch_error(
    f$1139, _tmp$2562, on_err$1096
  );
  $joinlet$3149:;
  goto $joinlet$3148;
  $join$1136:;
  moonbit_incref(name$1089);
  _closure$3151
  = (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$3151)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$3151->code
  = &$$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$3151->$0 = f$1137;
  _closure$3151->$1 = name$1089;
  _tmp$2555
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$3151;
  _closure$3152
  = (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$3152)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$3152->code
  = &$$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$3152->$0 = name$1089;
  _closure$3152->$1 = file_name$1087;
  _closure$3152->$2 = handle_result$1076;
  _tmp$2556 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$3152;
  $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_catch_error(
    _tmp$2555, _tmp$2556, on_err$1096
  );
  $joinlet$3148:;
  goto $joinlet$3147;
  $join$1111:;
  _closure$3153
  = (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$3153)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$3153->code
  = &$$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$3153->$0 = f$1112;
  _closure$3153->$1 = on_err$1096;
  _closure$3153->$2 = name$1089;
  _closure$3153->$3 = file_name$1087;
  _closure$3153->$4 = handle_result$1076;
  _tmp$2540
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$3153;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$1113, _tmp$2540);
  $joinlet$3147:;
  $joinlet$3145:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2563
) {
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$2564 =
    (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$2563;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2723 =
    _casted_env$2564->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1076 =
    _field$2723;
  moonbit_string_t _field$2722 = _casted_env$2564->$1;
  moonbit_string_t file_name$1087 = _field$2722;
  moonbit_string_t _field$2721 = _casted_env$2564->$0;
  int32_t _cnt$3018 = Moonbit_object_header(_casted_env$2564)->rc;
  moonbit_string_t name$1089;
  if (_cnt$3018 > 1) {
    int32_t _new_cnt$3019;
    moonbit_incref(handle_result$1076);
    moonbit_incref(file_name$1087);
    moonbit_incref(_field$2721);
    _new_cnt$3019 = _cnt$3018 - 1;
    Moonbit_object_header(_casted_env$2564)->rc = _new_cnt$3019;
  } else if (_cnt$3018 == 1) {
    moonbit_free(_casted_env$2564);
  }
  name$1089 = _field$2721;
  $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1076,
      name$1089,
      file_name$1087,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2559
) {
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$2560 =
    (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$2559;
  moonbit_string_t _field$2725 = _casted_env$2560->$1;
  moonbit_string_t name$1089 = _field$2725;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2724 =
    _casted_env$2560->$0;
  int32_t _cnt$3020 = Moonbit_object_header(_casted_env$2560)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1137;
  int32_t _tmp$2561;
  if (_cnt$3020 > 1) {
    int32_t _new_cnt$3021;
    moonbit_incref(name$1089);
    moonbit_incref(_field$2724);
    _new_cnt$3021 = _cnt$3020 - 1;
    Moonbit_object_header(_casted_env$2560)->rc = _new_cnt$3021;
  } else if (_cnt$3020 == 1) {
    moonbit_free(_casted_env$2560);
  }
  f$1137 = _field$2724;
  _tmp$2561
  = $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_new_test_arg(
    name$1089
  );
  return f$1137->code(f$1137, _tmp$2561);
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2557
) {
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$2558 =
    (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$2557;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2728 =
    _casted_env$2558->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1076 =
    _field$2728;
  moonbit_string_t _field$2727 = _casted_env$2558->$1;
  moonbit_string_t file_name$1087 = _field$2727;
  moonbit_string_t _field$2726 = _casted_env$2558->$0;
  int32_t _cnt$3022 = Moonbit_object_header(_casted_env$2558)->rc;
  moonbit_string_t name$1089;
  if (_cnt$3022 > 1) {
    int32_t _new_cnt$3023;
    moonbit_incref(handle_result$1076);
    moonbit_incref(file_name$1087);
    moonbit_incref(_field$2726);
    _new_cnt$3023 = _cnt$3022 - 1;
    Moonbit_object_header(_casted_env$2558)->rc = _new_cnt$3023;
  } else if (_cnt$3022 == 1) {
    moonbit_free(_casted_env$2558);
  }
  name$1089 = _field$2726;
  $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1076,
      name$1089,
      file_name$1087,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2541,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1114,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1115
) {
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$2542 =
    (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$2541;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2733 =
    _casted_env$2542->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1076 =
    _field$2733;
  moonbit_string_t _field$2732 = _casted_env$2542->$3;
  moonbit_string_t file_name$1087 = _field$2732;
  moonbit_string_t _field$2731 = _casted_env$2542->$2;
  moonbit_string_t name$1089 = _field$2731;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2730 = _casted_env$2542->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1096 = _field$2730;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2729 =
    _casted_env$2542->$0;
  int32_t _cnt$3024 = Moonbit_object_header(_casted_env$2542)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1112;
  int32_t _async_driver$1116;
  int32_t _tmp$2546;
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$3154;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$2547;
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$3155;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$2548;
  if (_cnt$3024 > 1) {
    int32_t _new_cnt$3025;
    moonbit_incref(handle_result$1076);
    moonbit_incref(file_name$1087);
    moonbit_incref(name$1089);
    moonbit_incref(on_err$1096);
    moonbit_incref(_field$2729);
    _new_cnt$3025 = _cnt$3024 - 1;
    Moonbit_object_header(_casted_env$2542)->rc = _new_cnt$3025;
  } else if (_cnt$3024 == 1) {
    moonbit_free(_casted_env$2542);
  }
  f$1112 = _field$2729;
  _async_driver$1116 = 0;
  moonbit_incref(name$1089);
  _tmp$2546
  = $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_new_test_arg(
    name$1089
  );
  moonbit_incref(_cont$1114);
  _closure$3154
  = (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$3154)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$3154->code
  = &$$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$3154->$0 = _async_driver$1116;
  _closure$3154->$1 = _cont$1114;
  _closure$3154->$2 = name$1089;
  _closure$3154->$3 = file_name$1087;
  _closure$3154->$4 = handle_result$1076;
  _tmp$2547 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$3154;
  _closure$3155
  = (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$3155)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$3155->code
  = &$$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$3155->$0 = _async_driver$1116;
  _closure$3155->$1 = _err_cont$1115;
  _closure$3155->$2 = _cont$1114;
  _closure$3155->$3 = on_err$1096;
  _tmp$2548 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$3155;
  f$1112->code(f$1112, _tmp$2546, _tmp$2547, _tmp$2548);
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2552,
  int32_t _cont_param$1134
) {
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$2553 =
    (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$2552;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2738 =
    _casted_env$2553->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1076 =
    _field$2738;
  moonbit_string_t _field$2737 = _casted_env$2553->$3;
  moonbit_string_t file_name$1087 = _field$2737;
  moonbit_string_t _field$2736 = _casted_env$2553->$2;
  moonbit_string_t name$1089 = _field$2736;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2735 = _casted_env$2553->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1114 = _field$2735;
  int32_t _field$2734 = _casted_env$2553->$0;
  int32_t _cnt$3026 = Moonbit_object_header(_casted_env$2553)->rc;
  int32_t _async_driver$1116;
  void* State_1$2554;
  if (_cnt$3026 > 1) {
    int32_t _new_cnt$3027;
    moonbit_incref(handle_result$1076);
    moonbit_incref(file_name$1087);
    moonbit_incref(name$1089);
    moonbit_incref(_cont$1114);
    _new_cnt$3027 = _cnt$3026 - 1;
    Moonbit_object_header(_casted_env$2553)->rc = _new_cnt$3027;
  } else if (_cnt$3026 == 1) {
    moonbit_free(_casted_env$2553);
  }
  _async_driver$1116 = _field$2734;
  State_1$2554
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1
      )
    );
  Moonbit_object_header(State_1$2554)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1*)State_1$2554)->$0
  = _cont_param$1134;
  ((struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1*)State_1$2554)->$1
  = handle_result$1076;
  ((struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1*)State_1$2554)->$2
  = file_name$1087;
  ((struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1*)State_1$2554)->$3
  = name$1089;
  ((struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1*)State_1$2554)->$4
  = _cont$1114;
  $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1116, State_1$2554
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2549,
  void* _cont_param$1135
) {
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$2550 =
    (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$2549;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2742 = _casted_env$2550->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1096 = _field$2742;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2741 = _casted_env$2550->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1114 = _field$2741;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2740 = _casted_env$2550->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1115 = _field$2740;
  int32_t _field$2739 = _casted_env$2550->$0;
  int32_t _cnt$3028 = Moonbit_object_header(_casted_env$2550)->rc;
  int32_t _async_driver$1116;
  void* _try$167$2551;
  if (_cnt$3028 > 1) {
    int32_t _new_cnt$3029;
    moonbit_incref(on_err$1096);
    moonbit_incref(_cont$1114);
    moonbit_incref(_err_cont$1115);
    _new_cnt$3029 = _cnt$3028 - 1;
    Moonbit_object_header(_casted_env$2550)->rc = _new_cnt$3029;
  } else if (_cnt$3028 == 1) {
    moonbit_free(_casted_env$2550);
  }
  _async_driver$1116 = _field$2739;
  _try$167$2551
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167
      )
    );
  Moonbit_object_header(_try$167$2551)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167*)_try$167$2551)->$0
  = _cont_param$1135;
  ((struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167*)_try$167$2551)->$1
  = on_err$1096;
  ((struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167*)_try$167$2551)->$2
  = _cont$1114;
  ((struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167*)_try$167$2551)->$3
  = _err_cont$1115;
  $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1116, _try$167$2551
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$2543,
  void* _state$1117
) {
  switch (Moonbit_object_tag(_state$1117)) {
    case 0: {
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167* _$2a$try$167$1118 =
        (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$$2a$try$167*)_state$1117;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2746 = _$2a$try$167$1118->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1119 = _field$2746;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2745 = _$2a$try$167$1118->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1120 = _field$2745;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2744 = _$2a$try$167$1118->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1121 = _field$2744;
      void* _field$2743 = _$2a$try$167$1118->$0;
      int32_t _cnt$3030 = Moonbit_object_header(_$2a$try$167$1118)->rc;
      void* _try_err$1122;
      void* err$1124;
      void* err$1126;
      int32_t _tmp$2545;
      if (_cnt$3030 > 1) {
        int32_t _new_cnt$3031;
        moonbit_incref(_err_cont$1119);
        moonbit_incref(_cont$1120);
        moonbit_incref(on_err$1121);
        moonbit_incref(_field$2743);
        _new_cnt$3031 = _cnt$3030 - 1;
        Moonbit_object_header(_$2a$try$167$1118)->rc = _new_cnt$3031;
      } else if (_cnt$3030 == 1) {
        moonbit_free(_$2a$try$167$1118);
      }
      _try_err$1122 = _field$2743;
      if (
        $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$1121);
        moonbit_decref(_cont$1120);
        err$1126 = _try_err$1122;
        goto $join$1125;
      } else {
        moonbit_decref(_err_cont$1119);
        err$1124 = _try_err$1122;
        goto $join$1123;
      }
      goto $joinlet$3157;
      $join$1125:;
      return _err_cont$1119->code(_err_cont$1119, err$1126);
      $joinlet$3157:;
      goto $joinlet$3156;
      $join$1123:;
      _tmp$2545 = on_err$1121->code(on_err$1121, err$1124);
      _cont$1120->code(_cont$1120, _tmp$2545);
      $joinlet$3156:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1* _State_1$1127 =
        (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$183$on_err$68$$2a$arm$175$lambda$201$State$State_1*)_state$1117;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2750 = _State_1$1127->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1128 = _field$2750;
      moonbit_string_t _field$2749 = _State_1$1127->$3;
      moonbit_string_t name$1129 = _field$2749;
      moonbit_string_t _field$2748 = _State_1$1127->$2;
      moonbit_string_t file_name$1130 = _field$2748;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2747 =
        _State_1$1127->$1;
      int32_t _cnt$3032 = Moonbit_object_header(_State_1$1127)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1131;
      int32_t _tmp$2544;
      if (_cnt$3032 > 1) {
        int32_t _new_cnt$3033;
        moonbit_incref(_cont$1128);
        moonbit_incref(name$1129);
        moonbit_incref(file_name$1130);
        moonbit_incref(_field$2747);
        _new_cnt$3033 = _cnt$3032 - 1;
        Moonbit_object_header(_State_1$1127)->rc = _new_cnt$3033;
      } else if (_cnt$3032 == 1) {
        moonbit_free(_State_1$1127);
      }
      handle_result$1131 = _field$2747;
      _tmp$2544
      = handle_result$1131->code(
        handle_result$1131,
          name$1129,
          file_name$1130,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$1128->code(_cont$1128, _tmp$2544);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2538,
  void* err$1097
) {
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$2539 =
    (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$2538;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2757 =
    _casted_env$2539->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1076 =
    _field$2757;
  moonbit_string_t _field$2756 = _casted_env$2539->$1;
  moonbit_string_t file_name$1087 = _field$2756;
  moonbit_string_t _field$2755 = _casted_env$2539->$0;
  int32_t _cnt$3034 = Moonbit_object_header(_casted_env$2539)->rc;
  moonbit_string_t name$1089;
  void* e$1099;
  moonbit_string_t e$1102;
  moonbit_string_t message$1100;
  if (_cnt$3034 > 1) {
    int32_t _new_cnt$3035;
    moonbit_incref(handle_result$1076);
    moonbit_incref(file_name$1087);
    moonbit_incref(_field$2755);
    _new_cnt$3035 = _cnt$3034 - 1;
    Moonbit_object_header(_casted_env$2539)->rc = _new_cnt$3035;
  } else if (_cnt$3034 == 1) {
    moonbit_free(_casted_env$2539);
  }
  name$1089 = _field$2755;
  switch (Moonbit_object_tag(err$1097)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$1103 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$1097;
      moonbit_string_t _field$2751 = _Failure$1103->$0;
      int32_t _cnt$3036 = Moonbit_object_header(_Failure$1103)->rc;
      moonbit_string_t _e$1104;
      if (_cnt$3036 > 1) {
        int32_t _new_cnt$3037;
        moonbit_incref(_field$2751);
        _new_cnt$3037 = _cnt$3036 - 1;
        Moonbit_object_header(_Failure$1103)->rc = _new_cnt$3037;
      } else if (_cnt$3036 == 1) {
        moonbit_free(_Failure$1103);
      }
      _e$1104 = _field$2751;
      e$1102 = _e$1104;
      goto $join$1101;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$1105 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$1097;
      moonbit_string_t _field$2752 = _InspectError$1105->$0;
      int32_t _cnt$3038 = Moonbit_object_header(_InspectError$1105)->rc;
      moonbit_string_t _e$1106;
      if (_cnt$3038 > 1) {
        int32_t _new_cnt$3039;
        moonbit_incref(_field$2752);
        _new_cnt$3039 = _cnt$3038 - 1;
        Moonbit_object_header(_InspectError$1105)->rc = _new_cnt$3039;
      } else if (_cnt$3038 == 1) {
        moonbit_free(_InspectError$1105);
      }
      _e$1106 = _field$2752;
      e$1102 = _e$1106;
      goto $join$1101;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$1107 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$1097;
      moonbit_string_t _field$2753 = _SnapshotError$1107->$0;
      int32_t _cnt$3040 = Moonbit_object_header(_SnapshotError$1107)->rc;
      moonbit_string_t _e$1108;
      if (_cnt$3040 > 1) {
        int32_t _new_cnt$3041;
        moonbit_incref(_field$2753);
        _new_cnt$3041 = _cnt$3040 - 1;
        Moonbit_object_header(_SnapshotError$1107)->rc = _new_cnt$3041;
      } else if (_cnt$3040 == 1) {
        moonbit_free(_SnapshotError$1107);
      }
      _e$1108 = _field$2753;
      e$1102 = _e$1108;
      goto $join$1101;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$tests$enhanced_suite$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$1109 =
        (struct $Error$azimuth$telemetry$tests$enhanced_suite$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$1097;
      moonbit_string_t _field$2754 =
        _MoonBitTestDriverInternalJsError$1109->$0;
      int32_t _cnt$3042 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1109)->rc;
      moonbit_string_t _e$1110;
      if (_cnt$3042 > 1) {
        int32_t _new_cnt$3043;
        moonbit_incref(_field$2754);
        _new_cnt$3043 = _cnt$3042 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1109)->rc
        = _new_cnt$3043;
      } else if (_cnt$3042 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$1109);
      }
      _e$1110 = _field$2754;
      e$1102 = _e$1110;
      goto $join$1101;
      break;
    }
    default: {
      e$1099 = err$1097;
      goto $join$1098;
      break;
    }
  }
  goto $joinlet$3159;
  $join$1101:;
  $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1076, name$1089, file_name$1087, e$1102, 0
  );
  $joinlet$3159:;
  goto $joinlet$3158;
  $join$1098:;
  message$1100 = $Error$to_string(e$1099);
  $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1076, name$1089, file_name$1087, message$1100, 0
  );
  $joinlet$3158:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2524,
  moonbit_string_t attr$1090
) {
  int32_t _tmp$2526;
  int64_t _tmp$2525;
  moonbit_decref(_env$2524);
  _tmp$2526 = Moonbit_array_length(attr$1090);
  _tmp$2525 = (int64_t)_tmp$2526;
  moonbit_incref(attr$1090);
  if ($String$$char_length_ge$inner(attr$1090, 5, 0, _tmp$2525)) {
    int32_t _tmp$2531 = attr$1090[0];
    int32_t _x$1091 = _tmp$2531;
    if (_x$1091 == 112) {
      int32_t _tmp$2530 = attr$1090[1];
      int32_t _x$1092 = _tmp$2530;
      if (_x$1092 == 97) {
        int32_t _tmp$2529 = attr$1090[2];
        int32_t _x$1093 = _tmp$2529;
        if (_x$1093 == 110) {
          int32_t _tmp$2528 = attr$1090[3];
          int32_t _x$1094 = _tmp$2528;
          if (_x$1094 == 105) {
            int32_t _tmp$2758 = attr$1090[4];
            int32_t _tmp$2527;
            int32_t _x$1095;
            moonbit_decref(attr$1090);
            _tmp$2527 = _tmp$2758;
            _x$1095 = _tmp$2527;
            return _x$1095 == 99 || 0;
          } else {
            moonbit_decref(attr$1090);
            return 0;
          }
        } else {
          moonbit_decref(attr$1090);
          return 0;
        }
      } else {
        moonbit_decref(attr$1090);
        return 0;
      }
    } else {
      moonbit_decref(attr$1090);
      return 0;
    }
  } else {
    moonbit_decref(attr$1090);
    return 0;
  }
}

int32_t $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$2508,
  moonbit_string_t test_name$1077,
  moonbit_string_t file_name$1078,
  moonbit_string_t message$1079,
  int32_t skipped$1080
) {
  struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$2509 =
    (struct $$azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$2508;
  int32_t _field$2759 = _casted_env$2509->$0;
  int32_t index$1075;
  int32_t _if_result$3160;
  moonbit_string_t file_name$1081;
  moonbit_string_t test_name$1082;
  moonbit_string_t message$1083;
  moonbit_string_t _tmp$2521;
  moonbit_string_t _tmp$2520;
  moonbit_string_t _tmp$2518;
  moonbit_string_t _tmp$2519;
  moonbit_string_t _tmp$2517;
  moonbit_string_t _tmp$2515;
  moonbit_string_t _tmp$2516;
  moonbit_string_t _tmp$2514;
  moonbit_string_t _tmp$2512;
  moonbit_string_t _tmp$2513;
  moonbit_string_t _tmp$2511;
  moonbit_string_t _tmp$2510;
  moonbit_decref(_casted_env$2509);
  index$1075 = _field$2759;
  if (!skipped$1080) {
    _if_result$3160 = 1;
  } else {
    _if_result$3160 = 0;
  }
  if (_if_result$3160) {
    
  }
  file_name$1081 = $String$$escape(file_name$1078);
  test_name$1082 = $String$$escape(test_name$1077);
  message$1083 = $String$$escape(message$1079);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$2521
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$1081
  );
  _tmp$2520
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$2521
  );
  _tmp$2518
  = moonbit_add_string(
    _tmp$2520, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$2519
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$1075
  );
  _tmp$2517 = moonbit_add_string(_tmp$2518, _tmp$2519);
  _tmp$2515
  = moonbit_add_string(
    _tmp$2517, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$2516
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$1082
  );
  _tmp$2514 = moonbit_add_string(_tmp$2515, _tmp$2516);
  _tmp$2512
  = moonbit_add_string(
    _tmp$2514, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$2513
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    message$1083
  );
  _tmp$2511 = moonbit_add_string(_tmp$2512, _tmp$2513);
  _tmp$2510
  = moonbit_add_string(
    _tmp$2511, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$2510);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1072
) {
  moonbit_decref(_discard_$1072);
  return 42;
}

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1070,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1071,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1068
) {
  void* _try_err$1066;
  struct moonbit_result_0 _tmp$3162 = f$1070->code(f$1070);
  void* err$1067;
  if (_tmp$3162.tag) {
    int32_t const _ok$2506 = _tmp$3162.data.ok;
    moonbit_decref(on_err$1068);
  } else {
    void* const _err$2507 = _tmp$3162.data.err;
    moonbit_decref(on_ok$1071);
    _try_err$1066 = _err$2507;
    goto $join$1065;
  }
  on_ok$1071->code(on_ok$1071);
  goto $joinlet$3161;
  $join$1065:;
  err$1067 = _try_err$1066;
  on_err$1068->code(on_err$1068, err$1067);
  $joinlet$3161:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1031,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1044,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1057,
  moonbit_string_t file_filter$1028,
  int32_t index_filter$1029
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1025;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1026;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1030;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2765;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2497;
  void* F0$2494;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2764;
  int32_t _cnt$3044;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$2496;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$2495;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1027;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1040;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1041;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1043;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2763;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2501;
  void* F1$2498;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2762;
  int32_t _cnt$3047;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$2500;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$2499;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1042;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1053;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1054;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1056;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2761;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2505;
  void* F2$2502;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2760;
  int32_t _cnt$3050;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$2504;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$2503;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1055;
  moonbit_incref(file_filter$1028);
  _bind$1030
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$1031, file_filter$1028
  );
  if (_bind$1030 == 0) {
    if (_bind$1030) {
      moonbit_decref(_bind$1030);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1032 =
      _bind$1030;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1033 =
      _Some$1032;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1035;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1036;
    moonbit_incref(_index_func_map$1033);
    _bind$1036
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$1033, index_filter$1029
    );
    if (_bind$1036 == 0) {
      if (_bind$1036) {
        moonbit_decref(_bind$1036);
      }
      moonbit_decref(_index_func_map$1033);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1037;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1038;
      moonbit_decref(async_tests$1057);
      moonbit_decref(with_args_tests$1044);
      _Some$1037 = _bind$1036;
      _func_attrs_tuple$1038 = _Some$1037;
      func_attrs_tuple$1035 = _func_attrs_tuple$1038;
      goto $join$1034;
    }
    goto $joinlet$3164;
    $join$1034:;
    index_func_map$1025 = _index_func_map$1033;
    func_attrs_tuple$1026 = func_attrs_tuple$1035;
    goto $join$1024;
    $joinlet$3164:;
  }
  goto $joinlet$3163;
  $join$1024:;
  moonbit_decref(index_func_map$1025);
  _field$2765 = func_attrs_tuple$1026->$0;
  _tmp$2497 = _field$2765;
  moonbit_incref(_tmp$2497);
  F0$2494
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$2494)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$2494)->$0 = _tmp$2497;
  _field$2764 = func_attrs_tuple$1026->$1;
  _cnt$3044 = Moonbit_object_header(func_attrs_tuple$1026)->rc;
  if (_cnt$3044 > 1) {
    int32_t _new_cnt$3046;
    moonbit_incref(_field$2764);
    _new_cnt$3046 = _cnt$3044 - 1;
    Moonbit_object_header(func_attrs_tuple$1026)->rc = _new_cnt$3046;
  } else if (_cnt$3044 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3045 =
      func_attrs_tuple$1026->$0;
    moonbit_decref(_field$3045);
    moonbit_free(func_attrs_tuple$1026);
  }
  _tmp$2496 = _field$2764;
  _tmp$2495
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$2495)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$2495->$0 = file_filter$1028;
  _tmp$2495->$1 = index_filter$1029;
  _tmp$2495->$2 = _tmp$2496;
  k$1027
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1027)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1027->$0 = F0$2494;
  k$1027->$1 = _tmp$2495;
  return k$1027;
  $joinlet$3163:;
  moonbit_incref(file_filter$1028);
  _bind$1043
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$1044, file_filter$1028
  );
  if (_bind$1043 == 0) {
    if (_bind$1043) {
      moonbit_decref(_bind$1043);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1045 =
      _bind$1043;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1046 =
      _Some$1045;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1048;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1049;
    moonbit_incref(_index_func_map$1046);
    _bind$1049
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$1046, index_filter$1029
    );
    if (_bind$1049 == 0) {
      if (_bind$1049) {
        moonbit_decref(_bind$1049);
      }
      moonbit_decref(_index_func_map$1046);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1050;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1051;
      moonbit_decref(async_tests$1057);
      _Some$1050 = _bind$1049;
      _func_attrs_tuple$1051 = _Some$1050;
      func_attrs_tuple$1048 = _func_attrs_tuple$1051;
      goto $join$1047;
    }
    goto $joinlet$3166;
    $join$1047:;
    index_func_map$1040 = _index_func_map$1046;
    func_attrs_tuple$1041 = func_attrs_tuple$1048;
    goto $join$1039;
    $joinlet$3166:;
  }
  goto $joinlet$3165;
  $join$1039:;
  moonbit_decref(index_func_map$1040);
  _field$2763 = func_attrs_tuple$1041->$0;
  _tmp$2501 = _field$2763;
  moonbit_incref(_tmp$2501);
  F1$2498
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$2498)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$2498)->$0 = _tmp$2501;
  _field$2762 = func_attrs_tuple$1041->$1;
  _cnt$3047 = Moonbit_object_header(func_attrs_tuple$1041)->rc;
  if (_cnt$3047 > 1) {
    int32_t _new_cnt$3049;
    moonbit_incref(_field$2762);
    _new_cnt$3049 = _cnt$3047 - 1;
    Moonbit_object_header(func_attrs_tuple$1041)->rc = _new_cnt$3049;
  } else if (_cnt$3047 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3048 =
      func_attrs_tuple$1041->$0;
    moonbit_decref(_field$3048);
    moonbit_free(func_attrs_tuple$1041);
  }
  _tmp$2500 = _field$2762;
  _tmp$2499
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$2499)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$2499->$0 = file_filter$1028;
  _tmp$2499->$1 = index_filter$1029;
  _tmp$2499->$2 = _tmp$2500;
  k$1042
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1042)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1042->$0 = F1$2498;
  k$1042->$1 = _tmp$2499;
  return k$1042;
  $joinlet$3165:;
  moonbit_incref(file_filter$1028);
  _bind$1056
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$1057, file_filter$1028
  );
  if (_bind$1056 == 0) {
    if (_bind$1056) {
      moonbit_decref(_bind$1056);
    }
    moonbit_decref(file_filter$1028);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1058 =
      _bind$1056;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1059 =
      _Some$1058;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1061;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1062;
    moonbit_incref(_index_func_map$1059);
    _bind$1062
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$1059, index_filter$1029
    );
    if (_bind$1062 == 0) {
      if (_bind$1062) {
        moonbit_decref(_bind$1062);
      }
      moonbit_decref(_index_func_map$1059);
      moonbit_decref(file_filter$1028);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1063 =
        _bind$1062;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1064 =
        _Some$1063;
      func_attrs_tuple$1061 = _func_attrs_tuple$1064;
      goto $join$1060;
    }
    goto $joinlet$3168;
    $join$1060:;
    index_func_map$1053 = _index_func_map$1059;
    func_attrs_tuple$1054 = func_attrs_tuple$1061;
    goto $join$1052;
    $joinlet$3168:;
  }
  goto $joinlet$3167;
  $join$1052:;
  moonbit_decref(index_func_map$1053);
  _field$2761 = func_attrs_tuple$1054->$0;
  _tmp$2505 = _field$2761;
  moonbit_incref(_tmp$2505);
  F2$2502
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$2502)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$2502)->$0 = _tmp$2505;
  _field$2760 = func_attrs_tuple$1054->$1;
  _cnt$3050 = Moonbit_object_header(func_attrs_tuple$1054)->rc;
  if (_cnt$3050 > 1) {
    int32_t _new_cnt$3052;
    moonbit_incref(_field$2760);
    _new_cnt$3052 = _cnt$3050 - 1;
    Moonbit_object_header(func_attrs_tuple$1054)->rc = _new_cnt$3052;
  } else if (_cnt$3050 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3051 =
      func_attrs_tuple$1054->$0;
    moonbit_decref(_field$3051);
    moonbit_free(func_attrs_tuple$1054);
  }
  _tmp$2504 = _field$2760;
  _tmp$2503
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$2503)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$2503->$0 = file_filter$1028;
  _tmp$2503->$1 = index_filter$1029;
  _tmp$2503->$2 = _tmp$2504;
  k$1055
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1055)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1055->$0 = F2$2502;
  k$1055->$1 = _tmp$2503;
  return k$1055;
  $joinlet$3167:;
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9(
  
) {
  struct $$3c$Int$2a$String$2a$Bool$3e$* tuple$1017 =
    (struct $$3c$Int$2a$String$2a$Bool$3e$*)moonbit_malloc(
      sizeof(struct $$3c$Int$2a$String$2a$Bool$3e$)
    );
  int32_t _tmp$2447;
  moonbit_string_t _tmp$2448;
  struct moonbit_result_0 _tmp$3169;
  moonbit_string_t _field$2767;
  moonbit_string_t _tmp$2451;
  moonbit_string_t _tmp$2452;
  struct moonbit_result_0 _tmp$3171;
  int32_t _field$2766;
  int32_t _tmp$2455;
  moonbit_string_t _tmp$2456;
  struct moonbit_result_0 _tmp$3173;
  int32_t* _tmp$2493;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$2488;
  int32_t* _tmp$2492;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$2489;
  int32_t* _tmp$2491;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$2490;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** _tmp$2487;
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* nested$1018;
  int32_t _tmp$2459;
  moonbit_string_t _tmp$2460;
  struct moonbit_result_0 _tmp$3175;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$2465;
  int32_t _tmp$2463;
  moonbit_string_t _tmp$2464;
  struct moonbit_result_0 _tmp$3177;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$2470;
  int32_t _tmp$2468;
  moonbit_string_t _tmp$2469;
  struct moonbit_result_0 _tmp$3179;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$2475;
  int32_t _tmp$2473;
  moonbit_string_t _tmp$2474;
  struct moonbit_result_0 _tmp$3181;
  int64_t _tmp$2486;
  void* nested_option$1019;
  int64_t _tmp$2480;
  void* Some$2478;
  moonbit_string_t _tmp$2479;
  struct moonbit_result_0 _tmp$3183;
  void* None$2485;
  int32_t _tmp$2483;
  moonbit_string_t _tmp$2484;
  Moonbit_object_header(tuple$1017)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$Int$2a$String$2a$Bool$3e$, $1) >> 2, 1, 0
  );
  tuple$1017->$0 = 1;
  tuple$1017->$1 = (moonbit_string_t)moonbit_string_literal_14.data;
  tuple$1017->$2 = 1;
  _tmp$2447 = tuple$1017->$0;
  _tmp$2448 = 0;
  _tmp$3169
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2447, 1, _tmp$2448, (moonbit_string_t)moonbit_string_literal_15.data
  );
  if (_tmp$3169.tag) {
    int32_t const _ok$2449 = _tmp$3169.data.ok;
  } else {
    void* const _err$2450 = _tmp$3169.data.err;
    struct moonbit_result_0 _result$3170;
    moonbit_decref(tuple$1017);
    _result$3170.tag = 0;
    _result$3170.data.err = _err$2450;
    return _result$3170;
  }
  _field$2767 = tuple$1017->$1;
  _tmp$2451 = _field$2767;
  _tmp$2452 = 0;
  moonbit_incref(_tmp$2451);
  _tmp$3171
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$2451,
      (moonbit_string_t)moonbit_string_literal_14.data,
      _tmp$2452,
      (moonbit_string_t)moonbit_string_literal_16.data
  );
  if (_tmp$3171.tag) {
    int32_t const _ok$2453 = _tmp$3171.data.ok;
  } else {
    void* const _err$2454 = _tmp$3171.data.err;
    struct moonbit_result_0 _result$3172;
    moonbit_decref(tuple$1017);
    _result$3172.tag = 0;
    _result$3172.data.err = _err$2454;
    return _result$3172;
  }
  _field$2766 = tuple$1017->$2;
  moonbit_decref(tuple$1017);
  _tmp$2455 = _field$2766;
  _tmp$2456 = 0;
  _tmp$3173
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2455, _tmp$2456, (moonbit_string_t)moonbit_string_literal_17.data
  );
  if (_tmp$3173.tag) {
    int32_t const _ok$2457 = _tmp$3173.data.ok;
  } else {
    void* const _err$2458 = _tmp$3173.data.err;
    struct moonbit_result_0 _result$3174;
    _result$3174.tag = 0;
    _result$3174.data.err = _err$2458;
    return _result$3174;
  }
  _tmp$2493 = (int32_t*)moonbit_make_int32_array_raw(2);
  _tmp$2493[0] = 1;
  _tmp$2493[1] = 2;
  _tmp$2488
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(_tmp$2488)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$2488->$0 = _tmp$2493;
  _tmp$2488->$1 = 2;
  _tmp$2492 = (int32_t*)moonbit_make_int32_array_raw(2);
  _tmp$2492[0] = 3;
  _tmp$2492[1] = 4;
  _tmp$2489
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(_tmp$2489)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$2489->$0 = _tmp$2492;
  _tmp$2489->$1 = 2;
  _tmp$2491 = (int32_t*)moonbit_make_int32_array_raw(2);
  _tmp$2491[0] = 5;
  _tmp$2491[1] = 6;
  _tmp$2490
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(_tmp$2490)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$2490->$0 = _tmp$2491;
  _tmp$2490->$1 = 2;
  _tmp$2487
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$**)moonbit_make_ref_array_raw(
      3
    );
  _tmp$2487[0] = _tmp$2488;
  _tmp$2487[1] = _tmp$2489;
  _tmp$2487[2] = _tmp$2490;
  nested$1018
  = (struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$
      )
    );
  Moonbit_object_header(nested$1018)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  nested$1018->$0 = _tmp$2487;
  nested$1018->$1 = 3;
  moonbit_incref(nested$1018);
  _tmp$2459 = $$moonbitlang$core$builtin$Array$$length$5(nested$1018);
  _tmp$2460 = 0;
  _tmp$3175
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2459, 3, _tmp$2460, (moonbit_string_t)moonbit_string_literal_18.data
  );
  if (_tmp$3175.tag) {
    int32_t const _ok$2461 = _tmp$3175.data.ok;
  } else {
    void* const _err$2462 = _tmp$3175.data.err;
    struct moonbit_result_0 _result$3176;
    moonbit_decref(nested$1018);
    _result$3176.tag = 0;
    _result$3176.data.err = _err$2462;
    return _result$3176;
  }
  moonbit_incref(nested$1018);
  _tmp$2465 = $$moonbitlang$core$builtin$Array$$at$3(nested$1018, 0);
  _tmp$2463 = $$moonbitlang$core$builtin$Array$$at$1(_tmp$2465, 0);
  _tmp$2464 = 0;
  _tmp$3177
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2463, 1, _tmp$2464, (moonbit_string_t)moonbit_string_literal_19.data
  );
  if (_tmp$3177.tag) {
    int32_t const _ok$2466 = _tmp$3177.data.ok;
  } else {
    void* const _err$2467 = _tmp$3177.data.err;
    struct moonbit_result_0 _result$3178;
    moonbit_decref(nested$1018);
    _result$3178.tag = 0;
    _result$3178.data.err = _err$2467;
    return _result$3178;
  }
  moonbit_incref(nested$1018);
  _tmp$2470 = $$moonbitlang$core$builtin$Array$$at$3(nested$1018, 1);
  _tmp$2468 = $$moonbitlang$core$builtin$Array$$at$1(_tmp$2470, 1);
  _tmp$2469 = 0;
  _tmp$3179
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2468, 4, _tmp$2469, (moonbit_string_t)moonbit_string_literal_20.data
  );
  if (_tmp$3179.tag) {
    int32_t const _ok$2471 = _tmp$3179.data.ok;
  } else {
    void* const _err$2472 = _tmp$3179.data.err;
    struct moonbit_result_0 _result$3180;
    moonbit_decref(nested$1018);
    _result$3180.tag = 0;
    _result$3180.data.err = _err$2472;
    return _result$3180;
  }
  _tmp$2475 = $$moonbitlang$core$builtin$Array$$at$3(nested$1018, 2);
  _tmp$2473 = $$moonbitlang$core$builtin$Array$$at$1(_tmp$2475, 0);
  _tmp$2474 = 0;
  _tmp$3181
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2473, 5, _tmp$2474, (moonbit_string_t)moonbit_string_literal_21.data
  );
  if (_tmp$3181.tag) {
    int32_t const _ok$2476 = _tmp$3181.data.ok;
  } else {
    void* const _err$2477 = _tmp$3181.data.err;
    struct moonbit_result_0 _result$3182;
    _result$3182.tag = 0;
    _result$3182.data.err = _err$2477;
    return _result$3182;
  }
  _tmp$2486 = (int64_t)42;
  nested_option$1019
  = (void*)moonbit_malloc(
      sizeof(struct $Option$3c$Option$3c$Int$3e$$3e$$Some)
    );
  Moonbit_object_header(nested_option$1019)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Option$3c$Option$3c$Int$3e$$3e$$Some) >> 2, 0, 1
  );
  ((struct $Option$3c$Option$3c$Int$3e$$3e$$Some*)nested_option$1019)->$0
  = _tmp$2486;
  _tmp$2480 = (int64_t)42;
  Some$2478
  = (void*)moonbit_malloc(
      sizeof(struct $Option$3c$Option$3c$Int$3e$$3e$$Some)
    );
  Moonbit_object_header(Some$2478)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Option$3c$Option$3c$Int$3e$$3e$$Some) >> 2, 0, 1
  );
  ((struct $Option$3c$Option$3c$Int$3e$$3e$$Some*)Some$2478)->$0 = _tmp$2480;
  _tmp$2479 = 0;
  moonbit_incref(nested_option$1019);
  _tmp$3183
  = $moonbitlang$core$builtin$assert_eq$6(
    nested_option$1019,
      Some$2478,
      _tmp$2479,
      (moonbit_string_t)moonbit_string_literal_22.data
  );
  if (_tmp$3183.tag) {
    int32_t const _ok$2481 = _tmp$3183.data.ok;
  } else {
    void* const _err$2482 = _tmp$3183.data.err;
    struct moonbit_result_0 _result$3184;
    moonbit_decref(nested_option$1019);
    _result$3184.tag = 0;
    _result$3184.data.err = _err$2482;
    return _result$3184;
  }
  None$2485 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  _tmp$2483
  = $moonbitlang$core$builtin$op_notequal$3(
    nested_option$1019, None$2485
  );
  _tmp$2484 = 0;
  return $moonbitlang$core$builtin$assert_true(
           _tmp$2483,
             _tmp$2484,
             (moonbit_string_t)moonbit_string_literal_23.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8(
  
) {
  moonbit_string_t _tmp$2428 = 0;
  struct moonbit_result_0 _tmp$3185 =
    $moonbitlang$core$builtin$assert_eq$0(
      5, 5, _tmp$2428, (moonbit_string_t)moonbit_string_literal_24.data
    );
  moonbit_string_t _tmp$2431;
  struct moonbit_result_0 _tmp$3187;
  moonbit_string_t _tmp$2434;
  struct moonbit_result_0 _tmp$3189;
  moonbit_string_t _tmp$2437;
  struct moonbit_result_0 _tmp$3191;
  moonbit_string_t _tmp$2440;
  struct moonbit_result_0 _tmp$3193;
  moonbit_string_t _tmp$2443;
  struct moonbit_result_0 _tmp$3195;
  moonbit_string_t _tmp$2446;
  if (_tmp$3185.tag) {
    int32_t const _ok$2429 = _tmp$3185.data.ok;
  } else {
    void* const _err$2430 = _tmp$3185.data.err;
    struct moonbit_result_0 _result$3186;
    _result$3186.tag = 0;
    _result$3186.data.err = _err$2430;
    return _result$3186;
  }
  _tmp$2431 = 0;
  _tmp$3187
  = $moonbitlang$core$builtin$assert_eq$0(
    -6, -6, _tmp$2431, (moonbit_string_t)moonbit_string_literal_25.data
  );
  if (_tmp$3187.tag) {
    int32_t const _ok$2432 = _tmp$3187.data.ok;
  } else {
    void* const _err$2433 = _tmp$3187.data.err;
    struct moonbit_result_0 _result$3188;
    _result$3188.tag = 0;
    _result$3188.data.err = _err$2433;
    return _result$3188;
  }
  _tmp$2434 = 0;
  _tmp$3189
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2434, (moonbit_string_t)moonbit_string_literal_26.data
  );
  if (_tmp$3189.tag) {
    int32_t const _ok$2435 = _tmp$3189.data.ok;
  } else {
    void* const _err$2436 = _tmp$3189.data.err;
    struct moonbit_result_0 _result$3190;
    _result$3190.tag = 0;
    _result$3190.data.err = _err$2436;
    return _result$3190;
  }
  _tmp$2437 = 0;
  _tmp$3191
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2437, (moonbit_string_t)moonbit_string_literal_27.data
  );
  if (_tmp$3191.tag) {
    int32_t const _ok$2438 = _tmp$3191.data.ok;
  } else {
    void* const _err$2439 = _tmp$3191.data.err;
    struct moonbit_result_0 _result$3192;
    _result$3192.tag = 0;
    _result$3192.data.err = _err$2439;
    return _result$3192;
  }
  _tmp$2440 = 0;
  _tmp$3193
  = $moonbitlang$core$builtin$assert_eq$0(
    5, 5, _tmp$2440, (moonbit_string_t)moonbit_string_literal_28.data
  );
  if (_tmp$3193.tag) {
    int32_t const _ok$2441 = _tmp$3193.data.ok;
  } else {
    void* const _err$2442 = _tmp$3193.data.err;
    struct moonbit_result_0 _result$3194;
    _result$3194.tag = 0;
    _result$3194.data.err = _err$2442;
    return _result$3194;
  }
  _tmp$2443 = 0;
  _tmp$3195
  = $moonbitlang$core$builtin$assert_eq$0(
    -5, -5, _tmp$2443, (moonbit_string_t)moonbit_string_literal_29.data
  );
  if (_tmp$3195.tag) {
    int32_t const _ok$2444 = _tmp$3195.data.ok;
  } else {
    void* const _err$2445 = _tmp$3195.data.err;
    struct moonbit_result_0 _result$3196;
    _result$3196.tag = 0;
    _result$3196.data.err = _err$2445;
    return _result$3196;
  }
  _tmp$2446 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           -5,
             -5,
             _tmp$2446,
             (moonbit_string_t)moonbit_string_literal_30.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7(
  
) {
  int32_t* _tmp$2427 = (int32_t*)moonbit_make_int32_array_raw(5);
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* numbers$1015;
  int32_t _tmp$2404;
  moonbit_string_t _tmp$2405;
  struct moonbit_result_0 _tmp$3197;
  int32_t _tmp$2408;
  moonbit_string_t _tmp$2409;
  struct moonbit_result_0 _tmp$3199;
  int32_t _tmp$2412;
  moonbit_string_t _tmp$2413;
  struct moonbit_result_0 _tmp$3201;
  int32_t* _tmp$2426;
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* chars$1016;
  int32_t _tmp$2416;
  moonbit_string_t _tmp$2417;
  struct moonbit_result_0 _tmp$3203;
  int32_t _tmp$2420;
  moonbit_string_t _tmp$2421;
  struct moonbit_result_0 _tmp$3205;
  int32_t _tmp$2424;
  moonbit_string_t _tmp$2425;
  _tmp$2427[0] = 10;
  _tmp$2427[1] = 20;
  _tmp$2427[2] = 30;
  _tmp$2427[3] = 40;
  _tmp$2427[4] = 50;
  numbers$1015
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(numbers$1015)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  numbers$1015->$0 = _tmp$2427;
  numbers$1015->$1 = 5;
  moonbit_incref(numbers$1015);
  _tmp$2404 = $$moonbitlang$core$builtin$Array$$at$1(numbers$1015, 0);
  _tmp$2405 = 0;
  _tmp$3197
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2404,
      10,
      _tmp$2405,
      (moonbit_string_t)moonbit_string_literal_31.data
  );
  if (_tmp$3197.tag) {
    int32_t const _ok$2406 = _tmp$3197.data.ok;
  } else {
    void* const _err$2407 = _tmp$3197.data.err;
    struct moonbit_result_0 _result$3198;
    moonbit_decref(numbers$1015);
    _result$3198.tag = 0;
    _result$3198.data.err = _err$2407;
    return _result$3198;
  }
  moonbit_incref(numbers$1015);
  _tmp$2408 = $$moonbitlang$core$builtin$Array$$at$1(numbers$1015, 2);
  _tmp$2409 = 0;
  _tmp$3199
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2408,
      30,
      _tmp$2409,
      (moonbit_string_t)moonbit_string_literal_32.data
  );
  if (_tmp$3199.tag) {
    int32_t const _ok$2410 = _tmp$3199.data.ok;
  } else {
    void* const _err$2411 = _tmp$3199.data.err;
    struct moonbit_result_0 _result$3200;
    moonbit_decref(numbers$1015);
    _result$3200.tag = 0;
    _result$3200.data.err = _err$2411;
    return _result$3200;
  }
  _tmp$2412 = $$moonbitlang$core$builtin$Array$$at$1(numbers$1015, 4);
  _tmp$2413 = 0;
  _tmp$3201
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2412,
      50,
      _tmp$2413,
      (moonbit_string_t)moonbit_string_literal_33.data
  );
  if (_tmp$3201.tag) {
    int32_t const _ok$2414 = _tmp$3201.data.ok;
  } else {
    void* const _err$2415 = _tmp$3201.data.err;
    struct moonbit_result_0 _result$3202;
    _result$3202.tag = 0;
    _result$3202.data.err = _err$2415;
    return _result$3202;
  }
  _tmp$2426 = (int32_t*)moonbit_make_int32_array_raw(4);
  _tmp$2426[0] = 97;
  _tmp$2426[1] = 98;
  _tmp$2426[2] = 99;
  _tmp$2426[3] = 100;
  chars$1016
  = (struct $$moonbitlang$core$builtin$Array$3c$Char$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$)
    );
  Moonbit_object_header(chars$1016)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Char$3e$, $0) >> 2,
      1,
      0
  );
  chars$1016->$0 = _tmp$2426;
  chars$1016->$1 = 4;
  moonbit_incref(chars$1016);
  _tmp$2416 = $$moonbitlang$core$builtin$Array$$at$2(chars$1016, 0);
  _tmp$2417 = 0;
  _tmp$3203
  = $moonbitlang$core$builtin$assert_eq$5(
    _tmp$2416,
      97,
      _tmp$2417,
      (moonbit_string_t)moonbit_string_literal_34.data
  );
  if (_tmp$3203.tag) {
    int32_t const _ok$2418 = _tmp$3203.data.ok;
  } else {
    void* const _err$2419 = _tmp$3203.data.err;
    struct moonbit_result_0 _result$3204;
    moonbit_decref(chars$1016);
    _result$3204.tag = 0;
    _result$3204.data.err = _err$2419;
    return _result$3204;
  }
  moonbit_incref(chars$1016);
  _tmp$2420 = $$moonbitlang$core$builtin$Array$$at$2(chars$1016, 3);
  _tmp$2421 = 0;
  _tmp$3205
  = $moonbitlang$core$builtin$assert_eq$5(
    _tmp$2420,
      100,
      _tmp$2421,
      (moonbit_string_t)moonbit_string_literal_35.data
  );
  if (_tmp$3205.tag) {
    int32_t const _ok$2422 = _tmp$3205.data.ok;
  } else {
    void* const _err$2423 = _tmp$3205.data.err;
    struct moonbit_result_0 _result$3206;
    moonbit_decref(chars$1016);
    _result$3206.tag = 0;
    _result$3206.data.err = _err$2423;
    return _result$3206;
  }
  _tmp$2424 = $$moonbitlang$core$builtin$Array$$length$4(chars$1016);
  _tmp$2425 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           _tmp$2424,
             4,
             _tmp$2425,
             (moonbit_string_t)moonbit_string_literal_36.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6(
  
) {
  int32_t _tmp$2374 =
    $$moonbitlang$core$builtin$Compare$$$default_impl$$op_lt$0(
      (moonbit_string_t)moonbit_string_literal_37.data,
        (moonbit_string_t)moonbit_string_literal_38.data
    );
  moonbit_string_t _tmp$2375 = 0;
  struct moonbit_result_0 _tmp$3207 =
    $moonbitlang$core$builtin$assert_true(
      _tmp$2374, _tmp$2375, (moonbit_string_t)moonbit_string_literal_39.data
    );
  int32_t _tmp$2378;
  moonbit_string_t _tmp$2379;
  struct moonbit_result_0 _tmp$3209;
  int32_t _tmp$2382;
  moonbit_string_t _tmp$2383;
  struct moonbit_result_0 _tmp$3211;
  int32_t _tmp$2386;
  moonbit_string_t _tmp$2387;
  struct moonbit_result_0 _tmp$3213;
  int32_t _tmp$2390;
  moonbit_string_t _tmp$2391;
  struct moonbit_result_0 _tmp$3215;
  int32_t _tmp$2394;
  moonbit_string_t _tmp$2395;
  struct moonbit_result_0 _tmp$3217;
  int32_t _tmp$2398;
  moonbit_string_t _tmp$2399;
  struct moonbit_result_0 _tmp$3219;
  int32_t _tmp$2402;
  moonbit_string_t _tmp$2403;
  if (_tmp$3207.tag) {
    int32_t const _ok$2376 = _tmp$3207.data.ok;
  } else {
    void* const _err$2377 = _tmp$3207.data.err;
    struct moonbit_result_0 _result$3208;
    _result$3208.tag = 0;
    _result$3208.data.err = _err$2377;
    return _result$3208;
  }
  _tmp$2378
  = moonbit_val_array_equal(
    (moonbit_string_t)moonbit_string_literal_14.data,
      (moonbit_string_t)moonbit_string_literal_14.data
  );
  _tmp$2379 = 0;
  _tmp$3209
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2378, _tmp$2379, (moonbit_string_t)moonbit_string_literal_40.data
  );
  if (_tmp$3209.tag) {
    int32_t const _ok$2380 = _tmp$3209.data.ok;
  } else {
    void* const _err$2381 = _tmp$3209.data.err;
    struct moonbit_result_0 _result$3210;
    _result$3210.tag = 0;
    _result$3210.data.err = _err$2381;
    return _result$3210;
  }
  _tmp$2382
  = $moonbitlang$core$builtin$op_notequal$1(
    (moonbit_string_t)moonbit_string_literal_41.data,
      (moonbit_string_t)moonbit_string_literal_14.data
  );
  _tmp$2383 = 0;
  _tmp$3211
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2382, _tmp$2383, (moonbit_string_t)moonbit_string_literal_42.data
  );
  if (_tmp$3211.tag) {
    int32_t const _ok$2384 = _tmp$3211.data.ok;
  } else {
    void* const _err$2385 = _tmp$3211.data.err;
    struct moonbit_result_0 _result$3212;
    _result$3212.tag = 0;
    _result$3212.data.err = _err$2385;
    return _result$3212;
  }
  _tmp$2386
  = $$moonbitlang$core$builtin$Compare$$$default_impl$$op_le$0(
    (moonbit_string_t)moonbit_string_literal_43.data,
      (moonbit_string_t)moonbit_string_literal_43.data
  );
  _tmp$2387 = 0;
  _tmp$3213
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2386, _tmp$2387, (moonbit_string_t)moonbit_string_literal_44.data
  );
  if (_tmp$3213.tag) {
    int32_t const _ok$2388 = _tmp$3213.data.ok;
  } else {
    void* const _err$2389 = _tmp$3213.data.err;
    struct moonbit_result_0 _result$3214;
    _result$3214.tag = 0;
    _result$3214.data.err = _err$2389;
    return _result$3214;
  }
  _tmp$2390
  = $$moonbitlang$core$builtin$Compare$$$default_impl$$op_ge$0(
    (moonbit_string_t)moonbit_string_literal_45.data,
      (moonbit_string_t)moonbit_string_literal_43.data
  );
  _tmp$2391 = 0;
  _tmp$3215
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2390, _tmp$2391, (moonbit_string_t)moonbit_string_literal_46.data
  );
  if (_tmp$3215.tag) {
    int32_t const _ok$2392 = _tmp$3215.data.ok;
  } else {
    void* const _err$2393 = _tmp$3215.data.err;
    struct moonbit_result_0 _result$3216;
    _result$3216.tag = 0;
    _result$3216.data.err = _err$2393;
    return _result$3216;
  }
  _tmp$2394
  = moonbit_val_array_equal(
    (moonbit_string_t)moonbit_string_literal_3.data,
      (moonbit_string_t)moonbit_string_literal_3.data
  );
  _tmp$2395 = 0;
  _tmp$3217
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2394, _tmp$2395, (moonbit_string_t)moonbit_string_literal_47.data
  );
  if (_tmp$3217.tag) {
    int32_t const _ok$2396 = _tmp$3217.data.ok;
  } else {
    void* const _err$2397 = _tmp$3217.data.err;
    struct moonbit_result_0 _result$3218;
    _result$3218.tag = 0;
    _result$3218.data.err = _err$2397;
    return _result$3218;
  }
  _tmp$2398
  = $moonbitlang$core$builtin$op_notequal$1(
    (moonbit_string_t)moonbit_string_literal_43.data,
      (moonbit_string_t)moonbit_string_literal_3.data
  );
  _tmp$2399 = 0;
  _tmp$3219
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2398, _tmp$2399, (moonbit_string_t)moonbit_string_literal_48.data
  );
  if (_tmp$3219.tag) {
    int32_t const _ok$2400 = _tmp$3219.data.ok;
  } else {
    void* const _err$2401 = _tmp$3219.data.err;
    struct moonbit_result_0 _result$3220;
    _result$3220.tag = 0;
    _result$3220.data.err = _err$2401;
    return _result$3220;
  }
  _tmp$2402
  = $$moonbitlang$core$builtin$Compare$$$default_impl$$op_lt$0(
    (moonbit_string_t)moonbit_string_literal_3.data,
      (moonbit_string_t)moonbit_string_literal_43.data
  );
  _tmp$2403 = 0;
  return $moonbitlang$core$builtin$assert_true(
           _tmp$2402,
             _tmp$2403,
             (moonbit_string_t)moonbit_string_literal_49.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5(
  
) {
  moonbit_string_t _tmp$2349 = 0;
  struct moonbit_result_0 _tmp$3221 =
    $moonbitlang$core$builtin$assert_true(
      1, _tmp$2349, (moonbit_string_t)moonbit_string_literal_50.data
    );
  moonbit_string_t _tmp$2352;
  struct moonbit_result_0 _tmp$3223;
  moonbit_string_t _tmp$2355;
  struct moonbit_result_0 _tmp$3225;
  moonbit_string_t _tmp$2358;
  struct moonbit_result_0 _tmp$3227;
  moonbit_string_t _tmp$2361;
  struct moonbit_result_0 _tmp$3229;
  moonbit_string_t _tmp$2364;
  struct moonbit_result_0 _tmp$3231;
  moonbit_string_t _tmp$2367;
  struct moonbit_result_0 _tmp$3233;
  moonbit_string_t _tmp$2370;
  struct moonbit_result_0 _tmp$3235;
  moonbit_string_t _tmp$2373;
  if (_tmp$3221.tag) {
    int32_t const _ok$2350 = _tmp$3221.data.ok;
  } else {
    void* const _err$2351 = _tmp$3221.data.err;
    struct moonbit_result_0 _result$3222;
    _result$3222.tag = 0;
    _result$3222.data.err = _err$2351;
    return _result$3222;
  }
  _tmp$2352 = 0;
  _tmp$3223
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2352, (moonbit_string_t)moonbit_string_literal_51.data
  );
  if (_tmp$3223.tag) {
    int32_t const _ok$2353 = _tmp$3223.data.ok;
  } else {
    void* const _err$2354 = _tmp$3223.data.err;
    struct moonbit_result_0 _result$3224;
    _result$3224.tag = 0;
    _result$3224.data.err = _err$2354;
    return _result$3224;
  }
  _tmp$2355 = 0;
  _tmp$3225
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2355, (moonbit_string_t)moonbit_string_literal_52.data
  );
  if (_tmp$3225.tag) {
    int32_t const _ok$2356 = _tmp$3225.data.ok;
  } else {
    void* const _err$2357 = _tmp$3225.data.err;
    struct moonbit_result_0 _result$3226;
    _result$3226.tag = 0;
    _result$3226.data.err = _err$2357;
    return _result$3226;
  }
  _tmp$2358 = 0;
  _tmp$3227
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2358, (moonbit_string_t)moonbit_string_literal_53.data
  );
  if (_tmp$3227.tag) {
    int32_t const _ok$2359 = _tmp$3227.data.ok;
  } else {
    void* const _err$2360 = _tmp$3227.data.err;
    struct moonbit_result_0 _result$3228;
    _result$3228.tag = 0;
    _result$3228.data.err = _err$2360;
    return _result$3228;
  }
  _tmp$2361 = 0;
  _tmp$3229
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2361, (moonbit_string_t)moonbit_string_literal_54.data
  );
  if (_tmp$3229.tag) {
    int32_t const _ok$2362 = _tmp$3229.data.ok;
  } else {
    void* const _err$2363 = _tmp$3229.data.err;
    struct moonbit_result_0 _result$3230;
    _result$3230.tag = 0;
    _result$3230.data.err = _err$2363;
    return _result$3230;
  }
  _tmp$2364 = 0;
  _tmp$3231
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2364, (moonbit_string_t)moonbit_string_literal_55.data
  );
  if (_tmp$3231.tag) {
    int32_t const _ok$2365 = _tmp$3231.data.ok;
  } else {
    void* const _err$2366 = _tmp$3231.data.err;
    struct moonbit_result_0 _result$3232;
    _result$3232.tag = 0;
    _result$3232.data.err = _err$2366;
    return _result$3232;
  }
  _tmp$2367 = 0;
  _tmp$3233
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2367, (moonbit_string_t)moonbit_string_literal_56.data
  );
  if (_tmp$3233.tag) {
    int32_t const _ok$2368 = _tmp$3233.data.ok;
  } else {
    void* const _err$2369 = _tmp$3233.data.err;
    struct moonbit_result_0 _result$3234;
    _result$3234.tag = 0;
    _result$3234.data.err = _err$2369;
    return _result$3234;
  }
  _tmp$2370 = 0;
  _tmp$3235
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2370, (moonbit_string_t)moonbit_string_literal_57.data
  );
  if (_tmp$3235.tag) {
    int32_t const _ok$2371 = _tmp$3235.data.ok;
  } else {
    void* const _err$2372 = _tmp$3235.data.err;
    struct moonbit_result_0 _result$3236;
    _result$3236.tag = 0;
    _result$3236.data.err = _err$2372;
    return _result$3236;
  }
  _tmp$2373 = 0;
  return $moonbitlang$core$builtin$assert_true(
           1, _tmp$2373, (moonbit_string_t)moonbit_string_literal_58.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4(
  
) {
  int64_t some_value$1011 = (int64_t)42;
  int64_t none_value$1012 = 4294967296ll;
  int64_t _tmp$2328 = (int64_t)42;
  moonbit_string_t _tmp$2329 = 0;
  struct moonbit_result_0 _tmp$3237 =
    $moonbitlang$core$builtin$assert_eq$2(
      some_value$1011,
        _tmp$2328,
        _tmp$2329,
        (moonbit_string_t)moonbit_string_literal_59.data
    );
  moonbit_string_t _tmp$2332;
  struct moonbit_result_0 _tmp$3239;
  int32_t _tmp$2335;
  moonbit_string_t _tmp$2336;
  struct moonbit_result_0 _tmp$3241;
  int64_t _tmp$2341;
  int32_t _tmp$2339;
  moonbit_string_t _tmp$2340;
  struct moonbit_result_0 _tmp$3243;
  moonbit_string_t some_string$1013;
  int32_t none_string$1014;
  moonbit_string_t _tmp$2344;
  moonbit_string_t _tmp$2345;
  struct moonbit_result_0 _tmp$3245;
  moonbit_string_t _tmp$2348;
  if (_tmp$3237.tag) {
    int32_t const _ok$2330 = _tmp$3237.data.ok;
  } else {
    void* const _err$2331 = _tmp$3237.data.err;
    struct moonbit_result_0 _result$3238;
    _result$3238.tag = 0;
    _result$3238.data.err = _err$2331;
    return _result$3238;
  }
  _tmp$2332 = 0;
  _tmp$3239
  = $moonbitlang$core$builtin$assert_eq$2(
    none_value$1012,
      4294967296ll,
      _tmp$2332,
      (moonbit_string_t)moonbit_string_literal_60.data
  );
  if (_tmp$3239.tag) {
    int32_t const _ok$2333 = _tmp$3239.data.ok;
  } else {
    void* const _err$2334 = _tmp$3239.data.err;
    struct moonbit_result_0 _result$3240;
    _result$3240.tag = 0;
    _result$3240.data.err = _err$2334;
    return _result$3240;
  }
  _tmp$2335
  = $moonbitlang$core$builtin$op_notequal$2(
    some_value$1011, 4294967296ll
  );
  _tmp$2336 = 0;
  _tmp$3241
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2335, _tmp$2336, (moonbit_string_t)moonbit_string_literal_61.data
  );
  if (_tmp$3241.tag) {
    int32_t const _ok$2337 = _tmp$3241.data.ok;
  } else {
    void* const _err$2338 = _tmp$3241.data.err;
    struct moonbit_result_0 _result$3242;
    _result$3242.tag = 0;
    _result$3242.data.err = _err$2338;
    return _result$3242;
  }
  _tmp$2341 = (int64_t)0;
  _tmp$2339
  = $moonbitlang$core$builtin$op_notequal$2(
    none_value$1012, _tmp$2341
  );
  _tmp$2340 = 0;
  _tmp$3243
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2339, _tmp$2340, (moonbit_string_t)moonbit_string_literal_62.data
  );
  if (_tmp$3243.tag) {
    int32_t const _ok$2342 = _tmp$3243.data.ok;
  } else {
    void* const _err$2343 = _tmp$3243.data.err;
    struct moonbit_result_0 _result$3244;
    _result$3244.tag = 0;
    _result$3244.data.err = _err$2343;
    return _result$3244;
  }
  some_string$1013 = (moonbit_string_t)moonbit_string_literal_63.data;
  none_string$1014 = -1;
  _tmp$2344 = (moonbit_string_t)moonbit_string_literal_63.data;
  _tmp$2345 = 0;
  _tmp$3245
  = $moonbitlang$core$builtin$assert_eq$3(
    some_string$1013,
      _tmp$2344,
      _tmp$2345,
      (moonbit_string_t)moonbit_string_literal_64.data
  );
  if (_tmp$3245.tag) {
    int32_t const _ok$2346 = _tmp$3245.data.ok;
  } else {
    void* const _err$2347 = _tmp$3245.data.err;
    struct moonbit_result_0 _result$3246;
    _result$3246.tag = 0;
    _result$3246.data.err = _err$2347;
    return _result$3246;
  }
  _tmp$2348 = 0;
  return $moonbitlang$core$builtin$assert_eq$4(
           none_string$1014,
             -1,
             _tmp$2348,
             (moonbit_string_t)moonbit_string_literal_65.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3(
  
) {
  int32_t* _tmp$2327 = (int32_t*)moonbit_make_int32_array_raw(5);
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* number_array$1008;
  int32_t _tmp$2303;
  moonbit_string_t _tmp$2304;
  struct moonbit_result_0 _tmp$3247;
  int32_t _tmp$2307;
  moonbit_string_t _tmp$2308;
  struct moonbit_result_0 _tmp$3249;
  int32_t _tmp$2311;
  moonbit_string_t _tmp$2312;
  struct moonbit_result_0 _tmp$3251;
  moonbit_string_t* _tmp$2326;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* string_array$1009;
  int32_t _tmp$2315;
  moonbit_string_t _tmp$2316;
  struct moonbit_result_0 _tmp$3253;
  moonbit_string_t _tmp$2319;
  moonbit_string_t _tmp$2320;
  struct moonbit_result_0 _tmp$3255;
  int32_t* _tmp$2325;
  struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$* empty_array$1010;
  int32_t _tmp$2323;
  moonbit_string_t _tmp$2324;
  _tmp$2327[0] = 1;
  _tmp$2327[1] = 2;
  _tmp$2327[2] = 3;
  _tmp$2327[3] = 4;
  _tmp$2327[4] = 5;
  number_array$1008
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(number_array$1008)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  number_array$1008->$0 = _tmp$2327;
  number_array$1008->$1 = 5;
  moonbit_incref(number_array$1008);
  _tmp$2303 = $$moonbitlang$core$builtin$Array$$length$2(number_array$1008);
  _tmp$2304 = 0;
  _tmp$3247
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2303, 5, _tmp$2304, (moonbit_string_t)moonbit_string_literal_66.data
  );
  if (_tmp$3247.tag) {
    int32_t const _ok$2305 = _tmp$3247.data.ok;
  } else {
    void* const _err$2306 = _tmp$3247.data.err;
    struct moonbit_result_0 _result$3248;
    moonbit_decref(number_array$1008);
    _result$3248.tag = 0;
    _result$3248.data.err = _err$2306;
    return _result$3248;
  }
  moonbit_incref(number_array$1008);
  _tmp$2307 = $$moonbitlang$core$builtin$Array$$at$1(number_array$1008, 0);
  _tmp$2308 = 0;
  _tmp$3249
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2307, 1, _tmp$2308, (moonbit_string_t)moonbit_string_literal_67.data
  );
  if (_tmp$3249.tag) {
    int32_t const _ok$2309 = _tmp$3249.data.ok;
  } else {
    void* const _err$2310 = _tmp$3249.data.err;
    struct moonbit_result_0 _result$3250;
    moonbit_decref(number_array$1008);
    _result$3250.tag = 0;
    _result$3250.data.err = _err$2310;
    return _result$3250;
  }
  _tmp$2311 = $$moonbitlang$core$builtin$Array$$at$1(number_array$1008, 4);
  _tmp$2312 = 0;
  _tmp$3251
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2311, 5, _tmp$2312, (moonbit_string_t)moonbit_string_literal_68.data
  );
  if (_tmp$3251.tag) {
    int32_t const _ok$2313 = _tmp$3251.data.ok;
  } else {
    void* const _err$2314 = _tmp$3251.data.err;
    struct moonbit_result_0 _result$3252;
    _result$3252.tag = 0;
    _result$3252.data.err = _err$2314;
    return _result$3252;
  }
  _tmp$2326 = (moonbit_string_t*)moonbit_make_ref_array_raw(3);
  _tmp$2326[0] = (moonbit_string_t)moonbit_string_literal_43.data;
  _tmp$2326[1] = (moonbit_string_t)moonbit_string_literal_69.data;
  _tmp$2326[2] = (moonbit_string_t)moonbit_string_literal_70.data;
  string_array$1009
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(string_array$1009)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  string_array$1009->$0 = _tmp$2326;
  string_array$1009->$1 = 3;
  moonbit_incref(string_array$1009);
  _tmp$2315 = $$moonbitlang$core$builtin$Array$$length$1(string_array$1009);
  _tmp$2316 = 0;
  _tmp$3253
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2315, 3, _tmp$2316, (moonbit_string_t)moonbit_string_literal_71.data
  );
  if (_tmp$3253.tag) {
    int32_t const _ok$2317 = _tmp$3253.data.ok;
  } else {
    void* const _err$2318 = _tmp$3253.data.err;
    struct moonbit_result_0 _result$3254;
    moonbit_decref(string_array$1009);
    _result$3254.tag = 0;
    _result$3254.data.err = _err$2318;
    return _result$3254;
  }
  _tmp$2319 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1009, 1);
  _tmp$2320 = 0;
  _tmp$3255
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$2319,
      (moonbit_string_t)moonbit_string_literal_69.data,
      _tmp$2320,
      (moonbit_string_t)moonbit_string_literal_72.data
  );
  if (_tmp$3255.tag) {
    int32_t const _ok$2321 = _tmp$3255.data.ok;
  } else {
    void* const _err$2322 = _tmp$3255.data.err;
    struct moonbit_result_0 _result$3256;
    _result$3256.tag = 0;
    _result$3256.data.err = _err$2322;
    return _result$3256;
  }
  _tmp$2325 = (int32_t*)moonbit_empty_int32_array;
  empty_array$1010
  = (struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$)
    );
  Moonbit_object_header(empty_array$1010)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$, $0) >> 2,
      1,
      0
  );
  empty_array$1010->$0 = _tmp$2325;
  empty_array$1010->$1 = 0;
  _tmp$2323 = $$moonbitlang$core$builtin$Array$$length$3(empty_array$1010);
  _tmp$2324 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           _tmp$2323,
             0,
             _tmp$2324,
             (moonbit_string_t)moonbit_string_literal_73.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2(
  
) {
  moonbit_string_t _tmp$2282 =
    moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_14.data,
        (moonbit_string_t)moonbit_string_literal_74.data
    );
  moonbit_string_t _tmp$2283 = 0;
  struct moonbit_result_0 _tmp$3257 =
    $moonbitlang$core$builtin$assert_eq$1(
      _tmp$2282,
        (moonbit_string_t)moonbit_string_literal_75.data,
        _tmp$2283,
        (moonbit_string_t)moonbit_string_literal_76.data
    );
  int32_t _tmp$2286;
  moonbit_string_t _tmp$2287;
  struct moonbit_result_0 _tmp$3259;
  int32_t _tmp$2290;
  moonbit_string_t _tmp$2291;
  struct moonbit_result_0 _tmp$3261;
  int32_t _tmp$2294;
  moonbit_string_t _tmp$2295;
  struct moonbit_result_0 _tmp$3263;
  int32_t _tmp$2298;
  moonbit_string_t _tmp$2299;
  struct moonbit_result_0 _tmp$3265;
  moonbit_string_t _tmp$2302;
  if (_tmp$3257.tag) {
    int32_t const _ok$2284 = _tmp$3257.data.ok;
  } else {
    void* const _err$2285 = _tmp$3257.data.err;
    struct moonbit_result_0 _result$3258;
    _result$3258.tag = 0;
    _result$3258.data.err = _err$2285;
    return _result$3258;
  }
  _tmp$2286
  = Moonbit_array_length(
    (moonbit_string_t)moonbit_string_literal_63.data
  );
  _tmp$2287 = 0;
  _tmp$3259
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2286, 4, _tmp$2287, (moonbit_string_t)moonbit_string_literal_77.data
  );
  if (_tmp$3259.tag) {
    int32_t const _ok$2288 = _tmp$3259.data.ok;
  } else {
    void* const _err$2289 = _tmp$3259.data.err;
    struct moonbit_result_0 _result$3260;
    _result$3260.tag = 0;
    _result$3260.data.err = _err$2289;
    return _result$3260;
  }
  _tmp$2290
  = Moonbit_array_length(
    (moonbit_string_t)moonbit_string_literal_3.data
  );
  _tmp$2291 = 0;
  _tmp$3261
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2290, 0, _tmp$2291, (moonbit_string_t)moonbit_string_literal_78.data
  );
  if (_tmp$3261.tag) {
    int32_t const _ok$2292 = _tmp$3261.data.ok;
  } else {
    void* const _err$2293 = _tmp$3261.data.err;
    struct moonbit_result_0 _result$3262;
    _result$3262.tag = 0;
    _result$3262.data.err = _err$2293;
    return _result$3262;
  }
  _tmp$2294
  = Moonbit_array_length(
    (moonbit_string_t)moonbit_string_literal_79.data
  );
  _tmp$2295 = 0;
  _tmp$3263
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2294, 7, _tmp$2295, (moonbit_string_t)moonbit_string_literal_80.data
  );
  if (_tmp$3263.tag) {
    int32_t const _ok$2296 = _tmp$3263.data.ok;
  } else {
    void* const _err$2297 = _tmp$3263.data.err;
    struct moonbit_result_0 _result$3264;
    _result$3264.tag = 0;
    _result$3264.data.err = _err$2297;
    return _result$3264;
  }
  _tmp$2298
  = $moonbitlang$core$builtin$op_notequal$1(
    (moonbit_string_t)moonbit_string_literal_14.data,
      (moonbit_string_t)moonbit_string_literal_41.data
  );
  _tmp$2299 = 0;
  _tmp$3265
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2298, _tmp$2299, (moonbit_string_t)moonbit_string_literal_81.data
  );
  if (_tmp$3265.tag) {
    int32_t const _ok$2300 = _tmp$3265.data.ok;
  } else {
    void* const _err$2301 = _tmp$3265.data.err;
    struct moonbit_result_0 _result$3266;
    _result$3266.tag = 0;
    _result$3266.data.err = _err$2301;
    return _result$3266;
  }
  _tmp$2302 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           (moonbit_string_t)moonbit_string_literal_14.data,
             (moonbit_string_t)moonbit_string_literal_14.data,
             _tmp$2302,
             (moonbit_string_t)moonbit_string_literal_82.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1(
  
) {
  moonbit_string_t _tmp$2260 = 0;
  struct moonbit_result_0 _tmp$3267 =
    $moonbitlang$core$builtin$assert_true(
      1, _tmp$2260, (moonbit_string_t)moonbit_string_literal_83.data
    );
  moonbit_string_t _tmp$2263;
  struct moonbit_result_0 _tmp$3269;
  moonbit_string_t _tmp$2266;
  struct moonbit_result_0 _tmp$3271;
  moonbit_string_t _tmp$2269;
  struct moonbit_result_0 _tmp$3273;
  moonbit_string_t _tmp$2272;
  struct moonbit_result_0 _tmp$3275;
  moonbit_string_t _tmp$2275;
  struct moonbit_result_0 _tmp$3277;
  moonbit_string_t _tmp$2278;
  struct moonbit_result_0 _tmp$3279;
  moonbit_string_t _tmp$2281;
  if (_tmp$3267.tag) {
    int32_t const _ok$2261 = _tmp$3267.data.ok;
  } else {
    void* const _err$2262 = _tmp$3267.data.err;
    struct moonbit_result_0 _result$3268;
    _result$3268.tag = 0;
    _result$3268.data.err = _err$2262;
    return _result$3268;
  }
  _tmp$2263 = 0;
  _tmp$3269
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$2263, (moonbit_string_t)moonbit_string_literal_84.data
  );
  if (_tmp$3269.tag) {
    int32_t const _ok$2264 = _tmp$3269.data.ok;
  } else {
    void* const _err$2265 = _tmp$3269.data.err;
    struct moonbit_result_0 _result$3270;
    _result$3270.tag = 0;
    _result$3270.data.err = _err$2265;
    return _result$3270;
  }
  _tmp$2266 = 0;
  _tmp$3271
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2266, (moonbit_string_t)moonbit_string_literal_85.data
  );
  if (_tmp$3271.tag) {
    int32_t const _ok$2267 = _tmp$3271.data.ok;
  } else {
    void* const _err$2268 = _tmp$3271.data.err;
    struct moonbit_result_0 _result$3272;
    _result$3272.tag = 0;
    _result$3272.data.err = _err$2268;
    return _result$3272;
  }
  _tmp$2269 = 0;
  _tmp$3273
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$2269, (moonbit_string_t)moonbit_string_literal_86.data
  );
  if (_tmp$3273.tag) {
    int32_t const _ok$2270 = _tmp$3273.data.ok;
  } else {
    void* const _err$2271 = _tmp$3273.data.err;
    struct moonbit_result_0 _result$3274;
    _result$3274.tag = 0;
    _result$3274.data.err = _err$2271;
    return _result$3274;
  }
  _tmp$2272 = 0;
  _tmp$3275
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2272, (moonbit_string_t)moonbit_string_literal_87.data
  );
  if (_tmp$3275.tag) {
    int32_t const _ok$2273 = _tmp$3275.data.ok;
  } else {
    void* const _err$2274 = _tmp$3275.data.err;
    struct moonbit_result_0 _result$3276;
    _result$3276.tag = 0;
    _result$3276.data.err = _err$2274;
    return _result$3276;
  }
  _tmp$2275 = 0;
  _tmp$3277
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$2275, (moonbit_string_t)moonbit_string_literal_88.data
  );
  if (_tmp$3277.tag) {
    int32_t const _ok$2276 = _tmp$3277.data.ok;
  } else {
    void* const _err$2277 = _tmp$3277.data.err;
    struct moonbit_result_0 _result$3278;
    _result$3278.tag = 0;
    _result$3278.data.err = _err$2277;
    return _result$3278;
  }
  _tmp$2278 = 0;
  _tmp$3279
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$2278, (moonbit_string_t)moonbit_string_literal_89.data
  );
  if (_tmp$3279.tag) {
    int32_t const _ok$2279 = _tmp$3279.data.ok;
  } else {
    void* const _err$2280 = _tmp$3279.data.err;
    struct moonbit_result_0 _result$3280;
    _result$3280.tag = 0;
    _result$3280.data.err = _err$2280;
    return _result$3280;
  }
  _tmp$2281 = 0;
  return $moonbitlang$core$builtin$assert_false(
           0, _tmp$2281, (moonbit_string_t)moonbit_string_literal_90.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0(
  
) {
  moonbit_string_t _tmp$2247 = 0;
  struct moonbit_result_0 _tmp$3281 =
    $moonbitlang$core$builtin$assert_eq$0(
      2, 2, _tmp$2247, (moonbit_string_t)moonbit_string_literal_91.data
    );
  moonbit_string_t _tmp$2250;
  struct moonbit_result_0 _tmp$3283;
  moonbit_string_t _tmp$2253;
  struct moonbit_result_0 _tmp$3285;
  moonbit_string_t _tmp$2256;
  struct moonbit_result_0 _tmp$3287;
  moonbit_string_t _tmp$2259;
  if (_tmp$3281.tag) {
    int32_t const _ok$2248 = _tmp$3281.data.ok;
  } else {
    void* const _err$2249 = _tmp$3281.data.err;
    struct moonbit_result_0 _result$3282;
    _result$3282.tag = 0;
    _result$3282.data.err = _err$2249;
    return _result$3282;
  }
  _tmp$2250 = 0;
  _tmp$3283
  = $moonbitlang$core$builtin$assert_eq$0(
    6, 6, _tmp$2250, (moonbit_string_t)moonbit_string_literal_92.data
  );
  if (_tmp$3283.tag) {
    int32_t const _ok$2251 = _tmp$3283.data.ok;
  } else {
    void* const _err$2252 = _tmp$3283.data.err;
    struct moonbit_result_0 _result$3284;
    _result$3284.tag = 0;
    _result$3284.data.err = _err$2252;
    return _result$3284;
  }
  _tmp$2253 = 0;
  _tmp$3285
  = $moonbitlang$core$builtin$assert_eq$0(
    5, 5, _tmp$2253, (moonbit_string_t)moonbit_string_literal_93.data
  );
  if (_tmp$3285.tag) {
    int32_t const _ok$2254 = _tmp$3285.data.ok;
  } else {
    void* const _err$2255 = _tmp$3285.data.err;
    struct moonbit_result_0 _result$3286;
    _result$3286.tag = 0;
    _result$3286.data.err = _err$2255;
    return _result$3286;
  }
  _tmp$2256 = 0;
  _tmp$3287
  = $moonbitlang$core$builtin$assert_eq$0(
    5, 5, _tmp$2256, (moonbit_string_t)moonbit_string_literal_94.data
  );
  if (_tmp$3287.tag) {
    int32_t const _ok$2257 = _tmp$3287.data.ok;
  } else {
    void* const _err$2258 = _tmp$3287.data.err;
    struct moonbit_result_0 _result$3288;
    _result$3288.tag = 0;
    _result$3288.data.err = _err$2258;
    return _result$3288;
  }
  _tmp$2259 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           1, 1, _tmp$2259, (moonbit_string_t)moonbit_string_literal_95.data
         );
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1007
) {
  int32_t _field$2768 = self$1007->$1;
  int32_t len$2246;
  moonbit_decref(self$1007);
  len$2246 = _field$2768;
  return len$2246 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$1005,
  struct $$moonbitlang$core$builtin$Logger logger$1006
) {
  moonbit_string_t _tmp$2245 = self$1005;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$2244 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$2245);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$2244, logger$1006
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$991,
  struct $$moonbitlang$core$builtin$Logger logger$1004
) {
  struct $StringView _field$2777 =
    (struct $StringView){self$991->$0_1, self$991->$0_2, self$991->$0_0};
  struct $StringView pkg$990 = _field$2777;
  int32_t _tmp$2243 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$2242;
  int64_t _bind$992;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$993;
  struct $StringView _field$2776;
  struct $StringView _module_name$1000;
  void* _field$2775;
  int32_t _cnt$3053;
  void* _package_name$1001;
  struct $StringView _field$2773;
  struct $StringView filename$2225;
  struct $StringView _field$2772;
  struct $StringView start_line$2226;
  struct $StringView _field$2771;
  struct $StringView start_column$2227;
  struct $StringView _field$2770;
  struct $StringView end_line$2228;
  struct $StringView _field$2769;
  int32_t _cnt$3057;
  struct $StringView end_column$2229;
  struct $$moonbitlang$core$builtin$Logger _bind$2224;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$2242
  = (struct $StringView){
    0, _tmp$2243, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$990.$0);
  moonbit_incref(pkg$990.$0);
  _bind$992 = $StringView$$find(pkg$990, _tmp$2242);
  if (_bind$992 == 4294967296ll) {
    void* None$2230 =
      (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
    _bind$993
    = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
        sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
      );
    Moonbit_object_header(_bind$993)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
      )
      >> 2,
        2,
        0
    );
    _bind$993->$0_0 = pkg$990.$0;
    _bind$993->$0_1 = pkg$990.$1;
    _bind$993->$0_2 = pkg$990.$2;
    _bind$993->$1 = None$2230;
  } else {
    int64_t _Some$994 = _bind$992;
    int32_t _first_slash$995 = (int32_t)_Some$994;
    int32_t _tmp$2241 = _first_slash$995 + 1;
    struct $StringView _tmp$2238;
    int32_t _tmp$2240;
    struct $StringView _tmp$2239;
    int64_t _bind$996;
    moonbit_incref(pkg$990.$0);
    _tmp$2238 = $StringView$$view$inner(pkg$990, _tmp$2241, 4294967296ll);
    _tmp$2240
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$2239
    = (struct $StringView){
      0, _tmp$2240, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$996 = $StringView$$find(_tmp$2238, _tmp$2239);
    if (_bind$996 == 4294967296ll) {
      void* None$2231 =
        (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$993
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$993)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$993->$0_0 = pkg$990.$0;
      _bind$993->$0_1 = pkg$990.$1;
      _bind$993->$0_2 = pkg$990.$2;
      _bind$993->$1 = None$2231;
    } else {
      int64_t _Some$997 = _bind$996;
      int32_t _second_slash$998 = (int32_t)_Some$997;
      int32_t _tmp$2237 = _first_slash$995 + 1;
      int32_t module_name_end$999 = _tmp$2237 + _second_slash$998;
      int64_t _tmp$2236 = (int64_t)module_name_end$999;
      struct $StringView _tmp$2232;
      int32_t _tmp$2235;
      struct $StringView _tmp$2234;
      void* Some$2233;
      moonbit_incref(pkg$990.$0);
      _tmp$2232 = $StringView$$view$inner(pkg$990, 0, _tmp$2236);
      _tmp$2235 = module_name_end$999 + 1;
      _tmp$2234 = $StringView$$view$inner(pkg$990, _tmp$2235, 4294967296ll);
      Some$2233
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$2233)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$2233)->$0_0
      = _tmp$2234.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2233)->$0_1
      = _tmp$2234.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2233)->$0_2
      = _tmp$2234.$2;
      _bind$993
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$993)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$993->$0_0 = _tmp$2232.$0;
      _bind$993->$0_1 = _tmp$2232.$1;
      _bind$993->$0_2 = _tmp$2232.$2;
      _bind$993->$1 = Some$2233;
    }
  }
  _field$2776
  = (struct $StringView){
    _bind$993->$0_1, _bind$993->$0_2, _bind$993->$0_0
  };
  _module_name$1000 = _field$2776;
  _field$2775 = _bind$993->$1;
  _cnt$3053 = Moonbit_object_header(_bind$993)->rc;
  if (_cnt$3053 > 1) {
    int32_t _new_cnt$3054;
    moonbit_incref(_field$2775);
    moonbit_incref(_module_name$1000.$0);
    _new_cnt$3054 = _cnt$3053 - 1;
    Moonbit_object_header(_bind$993)->rc = _new_cnt$3054;
  } else if (_cnt$3053 == 1) {
    moonbit_free(_bind$993);
  }
  _package_name$1001 = _field$2775;
  switch (Moonbit_object_tag(_package_name$1001)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$1002 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$1001;
      struct $StringView _field$2774 =
        (struct $StringView){
          _Some$1002->$0_1, _Some$1002->$0_2, _Some$1002->$0_0
        };
      int32_t _cnt$3055 = Moonbit_object_header(_Some$1002)->rc;
      struct $StringView _pkg_name$1003;
      struct $$moonbitlang$core$builtin$Logger _bind$2223;
      if (_cnt$3055 > 1) {
        int32_t _new_cnt$3056;
        moonbit_incref(_field$2774.$0);
        _new_cnt$3056 = _cnt$3055 - 1;
        Moonbit_object_header(_Some$1002)->rc = _new_cnt$3056;
      } else if (_cnt$3055 == 1) {
        moonbit_free(_Some$1002);
      }
      _pkg_name$1003 = _field$2774;
      if (logger$1004.$1) {
        moonbit_incref(logger$1004.$1);
      }
      logger$1004.$0->$method_2(logger$1004.$1, _pkg_name$1003);
      _bind$2223 = logger$1004;
      if (_bind$2223.$1) {
        moonbit_incref(_bind$2223.$1);
      }
      _bind$2223.$0->$method_3(_bind$2223.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$1001);
      break;
    }
  }
  _field$2773
  = (struct $StringView){
    self$991->$1_1, self$991->$1_2, self$991->$1_0
  };
  filename$2225 = _field$2773;
  moonbit_incref(filename$2225.$0);
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_2(logger$1004.$1, filename$2225);
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_3(logger$1004.$1, 58);
  _field$2772
  = (struct $StringView){
    self$991->$2_1, self$991->$2_2, self$991->$2_0
  };
  start_line$2226 = _field$2772;
  moonbit_incref(start_line$2226.$0);
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_2(logger$1004.$1, start_line$2226);
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_3(logger$1004.$1, 58);
  _field$2771
  = (struct $StringView){
    self$991->$3_1, self$991->$3_2, self$991->$3_0
  };
  start_column$2227 = _field$2771;
  moonbit_incref(start_column$2227.$0);
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_2(logger$1004.$1, start_column$2227);
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_3(logger$1004.$1, 45);
  _field$2770
  = (struct $StringView){
    self$991->$4_1, self$991->$4_2, self$991->$4_0
  };
  end_line$2228 = _field$2770;
  moonbit_incref(end_line$2228.$0);
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_2(logger$1004.$1, end_line$2228);
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_3(logger$1004.$1, 58);
  _field$2769
  = (struct $StringView){
    self$991->$5_1, self$991->$5_2, self$991->$5_0
  };
  _cnt$3057 = Moonbit_object_header(self$991)->rc;
  if (_cnt$3057 > 1) {
    int32_t _new_cnt$3063;
    moonbit_incref(_field$2769.$0);
    _new_cnt$3063 = _cnt$3057 - 1;
    Moonbit_object_header(self$991)->rc = _new_cnt$3063;
  } else if (_cnt$3057 == 1) {
    struct $StringView _field$3062 =
      (struct $StringView){self$991->$4_1, self$991->$4_2, self$991->$4_0};
    struct $StringView _field$3061;
    struct $StringView _field$3060;
    struct $StringView _field$3059;
    struct $StringView _field$3058;
    moonbit_decref(_field$3062.$0);
    _field$3061
    = (struct $StringView){
      self$991->$3_1, self$991->$3_2, self$991->$3_0
    };
    moonbit_decref(_field$3061.$0);
    _field$3060
    = (struct $StringView){
      self$991->$2_1, self$991->$2_2, self$991->$2_0
    };
    moonbit_decref(_field$3060.$0);
    _field$3059
    = (struct $StringView){
      self$991->$1_1, self$991->$1_2, self$991->$1_0
    };
    moonbit_decref(_field$3059.$0);
    _field$3058
    = (struct $StringView){
      self$991->$0_1, self$991->$0_2, self$991->$0_0
    };
    moonbit_decref(_field$3058.$0);
    moonbit_free(self$991);
  }
  end_column$2229 = _field$2769;
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_2(logger$1004.$1, end_column$2229);
  if (logger$1004.$1) {
    moonbit_incref(logger$1004.$1);
  }
  logger$1004.$0->$method_3(logger$1004.$1, 64);
  _bind$2224 = logger$1004;
  _bind$2224.$0->$method_2(_bind$2224.$1, _module_name$1000);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Char$$output(
  int32_t self$989,
  struct $$moonbitlang$core$builtin$Logger logger$987
) {
  struct $$moonbitlang$core$builtin$Logger _bind$2220;
  if (logger$987.$1) {
    moonbit_incref(logger$987.$1);
  }
  logger$987.$0->$method_3(logger$987.$1, 39);
  if (self$989 == 39) {
    goto $join$988;
  } else if (self$989 == 92) {
    goto $join$988;
  } else if (self$989 == 10) {
    if (logger$987.$1) {
      moonbit_incref(logger$987.$1);
    }
    logger$987.$0->$method_0(
      logger$987.$1, (moonbit_string_t)moonbit_string_literal_96.data
    );
  } else if (self$989 == 13) {
    if (logger$987.$1) {
      moonbit_incref(logger$987.$1);
    }
    logger$987.$0->$method_0(
      logger$987.$1, (moonbit_string_t)moonbit_string_literal_97.data
    );
  } else if (self$989 == 8) {
    if (logger$987.$1) {
      moonbit_incref(logger$987.$1);
    }
    logger$987.$0->$method_0(
      logger$987.$1, (moonbit_string_t)moonbit_string_literal_98.data
    );
  } else if (self$989 == 9) {
    if (logger$987.$1) {
      moonbit_incref(logger$987.$1);
    }
    logger$987.$0->$method_0(
      logger$987.$1, (moonbit_string_t)moonbit_string_literal_99.data
    );
  } else if (self$989 >= 32 && self$989 <= 126) {
    if (logger$987.$1) {
      moonbit_incref(logger$987.$1);
    }
    logger$987.$0->$method_3(logger$987.$1, self$989);
  } else {
    int32_t _tmp$2221 = $Char$$is_printable(self$989);
    if (!_tmp$2221) {
      moonbit_string_t _tmp$2222;
      if (logger$987.$1) {
        moonbit_incref(logger$987.$1);
      }
      logger$987.$0->$method_0(
        logger$987.$1, (moonbit_string_t)moonbit_string_literal_100.data
      );
      _tmp$2222 = $Char$$to_hex(self$989);
      if (logger$987.$1) {
        moonbit_incref(logger$987.$1);
      }
      logger$987.$0->$method_0(logger$987.$1, _tmp$2222);
      if (logger$987.$1) {
        moonbit_incref(logger$987.$1);
      }
      logger$987.$0->$method_3(logger$987.$1, 125);
    } else {
      if (logger$987.$1) {
        moonbit_incref(logger$987.$1);
      }
      logger$987.$0->$method_3(logger$987.$1, self$989);
    }
  }
  goto $joinlet$3289;
  $join$988:;
  if (logger$987.$1) {
    moonbit_incref(logger$987.$1);
  }
  logger$987.$0->$method_3(logger$987.$1, 92);
  _bind$2220 = logger$987;
  if (_bind$2220.$1) {
    moonbit_incref(_bind$2220.$1);
  }
  _bind$2220.$0->$method_3(_bind$2220.$1, self$989);
  $joinlet$3289:;
  logger$987.$0->$method_3(logger$987.$1, 39);
  return 0;
}

int32_t $Char$$is_printable(int32_t self$982) {
  int32_t self$983;
  int32_t _if_result$3292;
  if ($Char$$is_control(self$982)) {
    return 0;
  }
  self$983 = self$982;
  if (self$983 >= 57344 && self$983 <= 63743) {
    goto $join$984;
  } else if (self$983 >= 983040 && self$983 <= 1048573) {
    goto $join$984;
  } else if (self$983 >= 1048576 && self$983 <= 1114109) {
    goto $join$984;
  }
  goto $joinlet$3290;
  $join$984:;
  return 0;
  $joinlet$3290:;
  if (self$983 == 173) {
    goto $join$985;
  } else if (self$983 >= 1536 && self$983 <= 1541) {
    goto $join$985;
  } else if (self$983 == 1564) {
    goto $join$985;
  } else if (self$983 == 1757) {
    goto $join$985;
  } else if (self$983 == 1807) {
    goto $join$985;
  } else if (self$983 >= 2192 && self$983 <= 2193) {
    goto $join$985;
  } else if (self$983 == 2274) {
    goto $join$985;
  } else if (self$983 == 6158) {
    goto $join$985;
  } else if (self$983 >= 8203 && self$983 <= 8207) {
    goto $join$985;
  } else if (self$983 >= 8234 && self$983 <= 8238) {
    goto $join$985;
  } else if (self$983 >= 8288 && self$983 <= 8292) {
    goto $join$985;
  } else if (self$983 >= 8294 && self$983 <= 8303) {
    goto $join$985;
  } else if (self$983 == 65279) {
    goto $join$985;
  } else if (self$983 >= 65529 && self$983 <= 65531) {
    goto $join$985;
  } else if (self$983 == 69821) {
    goto $join$985;
  } else if (self$983 == 69837) {
    goto $join$985;
  } else if (self$983 >= 78896 && self$983 <= 78911) {
    goto $join$985;
  } else if (self$983 >= 113824 && self$983 <= 113827) {
    goto $join$985;
  } else if (self$983 >= 119155 && self$983 <= 119162) {
    goto $join$985;
  } else if (self$983 == 917505) {
    goto $join$985;
  } else if (self$983 >= 917536 && self$983 <= 917631) {
    goto $join$985;
  }
  goto $joinlet$3291;
  $join$985:;
  return 0;
  $joinlet$3291:;
  if ($Int$$is_surrogate(self$983)) {
    return 0;
  }
  if (self$983 == 8232) {
    _if_result$3292 = 1;
  } else {
    _if_result$3292 = self$983 == 8233;
  }
  if (_if_result$3292) {
    return 0;
  }
  if (self$983 >= 64976 && self$983 <= 65007) {
    goto $join$986;
  } else if (self$983 >= 65534 && self$983 <= 65535) {
    goto $join$986;
  } else if (self$983 >= 131070 && self$983 <= 131071) {
    goto $join$986;
  } else if (self$983 >= 196606 && self$983 <= 196607) {
    goto $join$986;
  } else if (self$983 >= 262142 && self$983 <= 262143) {
    goto $join$986;
  } else if (self$983 >= 327678 && self$983 <= 327679) {
    goto $join$986;
  } else if (self$983 >= 393214 && self$983 <= 393215) {
    goto $join$986;
  } else if (self$983 >= 458750 && self$983 <= 458751) {
    goto $join$986;
  } else if (self$983 >= 524286 && self$983 <= 524287) {
    goto $join$986;
  } else if (self$983 >= 589822 && self$983 <= 589823) {
    goto $join$986;
  } else if (self$983 >= 655358 && self$983 <= 655359) {
    goto $join$986;
  } else if (self$983 >= 720894 && self$983 <= 720895) {
    goto $join$986;
  } else if (self$983 >= 786430 && self$983 <= 786431) {
    goto $join$986;
  } else if (self$983 >= 851966 && self$983 <= 851967) {
    goto $join$986;
  } else if (self$983 >= 917502 && self$983 <= 917503) {
    goto $join$986;
  } else if (self$983 >= 983038 && self$983 <= 983039) {
    goto $join$986;
  } else if (self$983 >= 1048574 && self$983 <= 1048575) {
    goto $join$986;
  } else if (self$983 >= 1114110 && self$983 <= 1114111) {
    goto $join$986;
  }
  goto $joinlet$3293;
  $join$986:;
  return 0;
  $joinlet$3293:;
  return 1;
}

int32_t $Char$$is_control(int32_t self$981) {
  return self$981 >= 0 && self$981 <= 31
         || self$981 >= 127 && self$981 <= 159
         || 0;
}

moonbit_string_t $Char$$to_hex(int32_t char$980) {
  int32_t code$979 = char$980;
  if (code$979 >= 0 && code$979 <= 255) {
    int32_t _tmp$2219 = code$979 & 0xff;
    return $Byte$$to_hex(_tmp$2219);
  } else if (code$979 <= 65535) {
    int32_t _tmp$2218 = code$979 >> 8;
    int32_t _tmp$2217 = _tmp$2218 & 0xff;
    moonbit_string_t _tmp$2214 = $Byte$$to_hex(_tmp$2217);
    int32_t _tmp$2216 = code$979 & 0xff;
    moonbit_string_t _tmp$2215 = $Byte$$to_hex(_tmp$2216);
    return moonbit_add_string(_tmp$2214, _tmp$2215);
  } else {
    int32_t _tmp$2213 = code$979 >> 16;
    int32_t _tmp$2212 = _tmp$2213 & 0xff;
    moonbit_string_t _tmp$2208 = $Byte$$to_hex(_tmp$2212);
    int32_t _tmp$2211 = code$979 >> 8;
    int32_t _tmp$2210 = _tmp$2211 & 0xff;
    moonbit_string_t _tmp$2209 = $Byte$$to_hex(_tmp$2210);
    moonbit_string_t _tmp$2205 = moonbit_add_string(_tmp$2208, _tmp$2209);
    int32_t _tmp$2207 = code$979 & 0xff;
    moonbit_string_t _tmp$2206 = $Byte$$to_hex(_tmp$2207);
    return moonbit_add_string(_tmp$2205, _tmp$2206);
  }
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$978) {
  moonbit_string_t _tmp$2204 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$978);
  moonbit_println(_tmp$2204);
  moonbit_decref(_tmp$2204);
  return 0;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$973,
  moonbit_string_t msg$975,
  moonbit_string_t loc$977
) {
  if (!x$973) {
    moonbit_string_t fail_msg$974;
    if (msg$975 == 0) {
      moonbit_string_t _tmp$2202;
      moonbit_string_t _tmp$2201;
      if (msg$975) {
        moonbit_decref(msg$975);
      }
      _tmp$2202
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$973
      );
      _tmp$2201
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_101.data, _tmp$2202
      );
      fail_msg$974
      = moonbit_add_string(
        _tmp$2201, (moonbit_string_t)moonbit_string_literal_102.data
      );
    } else {
      moonbit_string_t _Some$976 = msg$975;
      fail_msg$974 = _Some$976;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$974, loc$977);
  } else {
    int32_t _tmp$2203;
    struct moonbit_result_0 _result$3294;
    moonbit_decref(loc$977);
    if (msg$975) {
      moonbit_decref(msg$975);
    }
    _tmp$2203 = 0;
    _result$3294.tag = 1;
    _result$3294.data.ok = _tmp$2203;
    return _result$3294;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$968,
  moonbit_string_t msg$970,
  moonbit_string_t loc$972
) {
  if (x$968) {
    moonbit_string_t fail_msg$969;
    if (msg$970 == 0) {
      moonbit_string_t _tmp$2199;
      moonbit_string_t _tmp$2198;
      if (msg$970) {
        moonbit_decref(msg$970);
      }
      _tmp$2199
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$968
      );
      _tmp$2198
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_101.data, _tmp$2199
      );
      fail_msg$969
      = moonbit_add_string(
        _tmp$2198, (moonbit_string_t)moonbit_string_literal_103.data
      );
    } else {
      moonbit_string_t _Some$971 = msg$970;
      fail_msg$969 = _Some$971;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$969, loc$972);
  } else {
    int32_t _tmp$2200;
    struct moonbit_result_0 _result$3295;
    moonbit_decref(loc$972);
    if (msg$970) {
      moonbit_decref(msg$970);
    }
    _tmp$2200 = 0;
    _result$3295.tag = 1;
    _result$3295.data.ok = _tmp$2200;
    return _result$3295;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$967,
  struct $$moonbitlang$core$builtin$Hasher* hasher$966
) {
  $$moonbitlang$core$builtin$Hasher$$combine_int(hasher$966, self$967);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$965,
  struct $$moonbitlang$core$builtin$Hasher* hasher$964
) {
  $$moonbitlang$core$builtin$Hasher$$combine_string(hasher$964, self$965);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$962,
  moonbit_string_t value$960
) {
  int32_t _end2448$959 = Moonbit_array_length(value$960);
  int32_t i$961 = 0;
  while (1) {
    if (i$961 < _end2448$959) {
      int32_t _tmp$2196 = value$960[i$961];
      uint32_t _tmp$2195 = *(uint32_t*)&_tmp$2196;
      int32_t _tmp$2197;
      moonbit_incref(self$962);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$962, _tmp$2195);
      _tmp$2197 = i$961 + 1;
      i$961 = _tmp$2197;
      continue;
    } else {
      moonbit_decref(self$962);
      moonbit_decref(value$960);
    }
    break;
  }
  return 0;
}

int32_t $Int$$is_surrogate(int32_t self$958) {
  if (55296 <= self$958) {
    return self$958 <= 57343;
  } else {
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$955,
  struct $$3c$String$3e$$3d$$3e$Int* f$957
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$3297 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$2192;
  int32_t _tmp$2191;
  Moonbit_object_header(_closure$3297)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$3297->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$3297->$0 = f$957;
  _tmp$2192 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$3297;
  _tmp$2191 = $$moonbitlang$core$builtin$Iter$$run$0(self$955, _tmp$2192);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$2191, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2193,
  moonbit_string_t k$956
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$2194 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$2193;
  struct $$3c$String$3e$$3d$$3e$Int* _field$2778 = _casted_env$2194->$0;
  int32_t _cnt$3064 = Moonbit_object_header(_casted_env$2194)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$957;
  if (_cnt$3064 > 1) {
    int32_t _new_cnt$3065;
    moonbit_incref(_field$2778);
    _new_cnt$3065 = _cnt$3064 - 1;
    Moonbit_object_header(_casted_env$2194)->rc = _new_cnt$3065;
  } else if (_cnt$3064 == 1) {
    moonbit_free(_casted_env$2194);
  }
  f$957 = _field$2778;
  if (f$957->code(f$957, k$956)) {
    return 0;
  } else {
    return 1;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$953,
  int32_t idx$954
) {
  moonbit_string_t* _tmp$2190 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$953);
  moonbit_string_t _tmp$2779;
  if (idx$954 < 0 || idx$954 >= Moonbit_array_length(_tmp$2190)) {
    moonbit_panic();
  }
  _tmp$2779 = (moonbit_string_t)_tmp$2190[idx$954];
  moonbit_incref(_tmp$2779);
  moonbit_decref(_tmp$2190);
  return _tmp$2779;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$951,
  int32_t idx$952
) {
  struct $$3c$String$2a$Int$3e$** _tmp$2189 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$951);
  struct $$3c$String$2a$Int$3e$* _tmp$2780;
  if (idx$952 < 0 || idx$952 >= Moonbit_array_length(_tmp$2189)) {
    moonbit_panic();
  }
  _tmp$2780 = (struct $$3c$String$2a$Int$3e$*)_tmp$2189[idx$952];
  if (_tmp$2780) {
    moonbit_incref(_tmp$2780);
  }
  moonbit_decref(_tmp$2189);
  return _tmp$2780;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$947,
  int32_t key$943
) {
  int32_t hash$942 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$943);
  int32_t capacity_mask$2188 = self$947->$3;
  int32_t _tmp$2187 = hash$942 & capacity_mask$2188;
  int32_t i$944 = 0;
  int32_t idx$945 = _tmp$2187;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2784 =
      self$947->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2186 =
      _field$2784;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2783;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$946;
    if (idx$945 < 0 || idx$945 >= Moonbit_array_length(entries$2186)) {
      moonbit_panic();
    }
    _tmp$2783
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2186[
        idx$945
      ];
    _bind$946 = _tmp$2783;
    if (_bind$946 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2175;
      if (_bind$946) {
        moonbit_incref(_bind$946);
      }
      moonbit_decref(self$947);
      if (_bind$946) {
        moonbit_decref(_bind$946);
      }
      _tmp$2175 = 0;
      return _tmp$2175;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$948 =
        _bind$946;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$949 =
        _Some$948;
      int32_t hash$2177 = _entry$949->$3;
      int32_t _if_result$3299;
      int32_t _field$2781;
      int32_t psl$2180;
      int32_t _tmp$2182;
      int32_t _tmp$2184;
      int32_t capacity_mask$2185;
      int32_t _tmp$2183;
      if (hash$2177 == hash$942) {
        int32_t key$2176 = _entry$949->$4;
        _if_result$3299 = key$2176 == key$943;
      } else {
        _if_result$3299 = 0;
      }
      if (_if_result$3299) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2782;
        int32_t _cnt$3066;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2179;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2178;
        moonbit_incref(_entry$949);
        moonbit_decref(self$947);
        _field$2782 = _entry$949->$5;
        _cnt$3066 = Moonbit_object_header(_entry$949)->rc;
        if (_cnt$3066 > 1) {
          int32_t _new_cnt$3068;
          moonbit_incref(_field$2782);
          _new_cnt$3068 = _cnt$3066 - 1;
          Moonbit_object_header(_entry$949)->rc = _new_cnt$3068;
        } else if (_cnt$3066 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3067 =
            _entry$949->$1;
          if (_field$3067) {
            moonbit_decref(_field$3067);
          }
          moonbit_free(_entry$949);
        }
        value$2179 = _field$2782;
        _tmp$2178 = value$2179;
        return _tmp$2178;
      } else {
        moonbit_incref(_entry$949);
      }
      _field$2781 = _entry$949->$2;
      moonbit_decref(_entry$949);
      psl$2180 = _field$2781;
      if (i$944 > psl$2180) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2181;
        moonbit_decref(self$947);
        _tmp$2181 = 0;
        return _tmp$2181;
      }
      _tmp$2182 = i$944 + 1;
      _tmp$2184 = idx$945 + 1;
      capacity_mask$2185 = self$947->$3;
      _tmp$2183 = _tmp$2184 & capacity_mask$2185;
      i$944 = _tmp$2182;
      idx$945 = _tmp$2183;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$938,
  moonbit_string_t key$934
) {
  int32_t hash$933;
  int32_t capacity_mask$2174;
  int32_t _tmp$2173;
  int32_t i$935;
  int32_t idx$936;
  moonbit_incref(key$934);
  hash$933 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$934);
  capacity_mask$2174 = self$938->$3;
  _tmp$2173 = hash$933 & capacity_mask$2174;
  i$935 = 0;
  idx$936 = _tmp$2173;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2790 =
      self$938->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2172 =
      _field$2790;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2789;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$937;
    if (idx$936 < 0 || idx$936 >= Moonbit_array_length(entries$2172)) {
      moonbit_panic();
    }
    _tmp$2789
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2172[
        idx$936
      ];
    _bind$937 = _tmp$2789;
    if (_bind$937 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2161;
      if (_bind$937) {
        moonbit_incref(_bind$937);
      }
      moonbit_decref(self$938);
      if (_bind$937) {
        moonbit_decref(_bind$937);
      }
      moonbit_decref(key$934);
      _tmp$2161 = 0;
      return _tmp$2161;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$939 =
        _bind$937;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$940 =
        _Some$939;
      int32_t hash$2163 = _entry$940->$3;
      int32_t _if_result$3301;
      int32_t _field$2785;
      int32_t psl$2166;
      int32_t _tmp$2168;
      int32_t _tmp$2170;
      int32_t capacity_mask$2171;
      int32_t _tmp$2169;
      if (hash$2163 == hash$933) {
        moonbit_string_t _field$2788 = _entry$940->$4;
        moonbit_string_t key$2162 = _field$2788;
        int32_t _tmp$2787 = moonbit_val_array_equal(key$2162, key$934);
        _if_result$3301 = _tmp$2787;
      } else {
        _if_result$3301 = 0;
      }
      if (_if_result$3301) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2786;
        int32_t _cnt$3069;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2165;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2164;
        moonbit_incref(_entry$940);
        moonbit_decref(self$938);
        moonbit_decref(key$934);
        _field$2786 = _entry$940->$5;
        _cnt$3069 = Moonbit_object_header(_entry$940)->rc;
        if (_cnt$3069 > 1) {
          int32_t _new_cnt$3072;
          moonbit_incref(_field$2786);
          _new_cnt$3072 = _cnt$3069 - 1;
          Moonbit_object_header(_entry$940)->rc = _new_cnt$3072;
        } else if (_cnt$3069 == 1) {
          moonbit_string_t _field$3071 = _entry$940->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3070;
          moonbit_decref(_field$3071);
          _field$3070 = _entry$940->$1;
          if (_field$3070) {
            moonbit_decref(_field$3070);
          }
          moonbit_free(_entry$940);
        }
        value$2165 = _field$2786;
        _tmp$2164 = value$2165;
        return _tmp$2164;
      } else {
        moonbit_incref(_entry$940);
      }
      _field$2785 = _entry$940->$2;
      moonbit_decref(_entry$940);
      psl$2166 = _field$2785;
      if (i$935 > psl$2166) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2167;
        moonbit_decref(self$938);
        moonbit_decref(key$934);
        _tmp$2167 = 0;
        return _tmp$2167;
      }
      _tmp$2168 = i$935 + 1;
      _tmp$2170 = idx$936 + 1;
      capacity_mask$2171 = self$938->$3;
      _tmp$2169 = _tmp$2170 & capacity_mask$2171;
      i$935 = _tmp$2168;
      idx$936 = _tmp$2169;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$929,
  int32_t key$925
) {
  int32_t hash$924 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$925);
  int32_t capacity_mask$2160 = self$929->$3;
  int32_t _tmp$2159 = hash$924 & capacity_mask$2160;
  int32_t i$926 = 0;
  int32_t idx$927 = _tmp$2159;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2794 =
      self$929->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2158 =
      _field$2794;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2793;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$928;
    if (idx$927 < 0 || idx$927 >= Moonbit_array_length(entries$2158)) {
      moonbit_panic();
    }
    _tmp$2793
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2158[
        idx$927
      ];
    _bind$928 = _tmp$2793;
    if (_bind$928 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2147;
      if (_bind$928) {
        moonbit_incref(_bind$928);
      }
      moonbit_decref(self$929);
      if (_bind$928) {
        moonbit_decref(_bind$928);
      }
      _tmp$2147 = 0;
      return _tmp$2147;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$930 =
        _bind$928;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$931 =
        _Some$930;
      int32_t hash$2149 = _entry$931->$3;
      int32_t _if_result$3303;
      int32_t _field$2791;
      int32_t psl$2152;
      int32_t _tmp$2154;
      int32_t _tmp$2156;
      int32_t capacity_mask$2157;
      int32_t _tmp$2155;
      if (hash$2149 == hash$924) {
        int32_t key$2148 = _entry$931->$4;
        _if_result$3303 = key$2148 == key$925;
      } else {
        _if_result$3303 = 0;
      }
      if (_if_result$3303) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2792;
        int32_t _cnt$3073;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2151;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2150;
        moonbit_incref(_entry$931);
        moonbit_decref(self$929);
        _field$2792 = _entry$931->$5;
        _cnt$3073 = Moonbit_object_header(_entry$931)->rc;
        if (_cnt$3073 > 1) {
          int32_t _new_cnt$3075;
          moonbit_incref(_field$2792);
          _new_cnt$3075 = _cnt$3073 - 1;
          Moonbit_object_header(_entry$931)->rc = _new_cnt$3075;
        } else if (_cnt$3073 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3074 =
            _entry$931->$1;
          if (_field$3074) {
            moonbit_decref(_field$3074);
          }
          moonbit_free(_entry$931);
        }
        value$2151 = _field$2792;
        _tmp$2150 = value$2151;
        return _tmp$2150;
      } else {
        moonbit_incref(_entry$931);
      }
      _field$2791 = _entry$931->$2;
      moonbit_decref(_entry$931);
      psl$2152 = _field$2791;
      if (i$926 > psl$2152) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2153;
        moonbit_decref(self$929);
        _tmp$2153 = 0;
        return _tmp$2153;
      }
      _tmp$2154 = i$926 + 1;
      _tmp$2156 = idx$927 + 1;
      capacity_mask$2157 = self$929->$3;
      _tmp$2155 = _tmp$2156 & capacity_mask$2157;
      i$926 = _tmp$2154;
      idx$927 = _tmp$2155;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$920,
  moonbit_string_t key$916
) {
  int32_t hash$915;
  int32_t capacity_mask$2146;
  int32_t _tmp$2145;
  int32_t i$917;
  int32_t idx$918;
  moonbit_incref(key$916);
  hash$915 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$916);
  capacity_mask$2146 = self$920->$3;
  _tmp$2145 = hash$915 & capacity_mask$2146;
  i$917 = 0;
  idx$918 = _tmp$2145;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2800 =
      self$920->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2144 =
      _field$2800;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2799;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$919;
    if (idx$918 < 0 || idx$918 >= Moonbit_array_length(entries$2144)) {
      moonbit_panic();
    }
    _tmp$2799
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2144[
        idx$918
      ];
    _bind$919 = _tmp$2799;
    if (_bind$919 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2133;
      if (_bind$919) {
        moonbit_incref(_bind$919);
      }
      moonbit_decref(self$920);
      if (_bind$919) {
        moonbit_decref(_bind$919);
      }
      moonbit_decref(key$916);
      _tmp$2133 = 0;
      return _tmp$2133;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$921 =
        _bind$919;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$922 =
        _Some$921;
      int32_t hash$2135 = _entry$922->$3;
      int32_t _if_result$3305;
      int32_t _field$2795;
      int32_t psl$2138;
      int32_t _tmp$2140;
      int32_t _tmp$2142;
      int32_t capacity_mask$2143;
      int32_t _tmp$2141;
      if (hash$2135 == hash$915) {
        moonbit_string_t _field$2798 = _entry$922->$4;
        moonbit_string_t key$2134 = _field$2798;
        int32_t _tmp$2797 = moonbit_val_array_equal(key$2134, key$916);
        _if_result$3305 = _tmp$2797;
      } else {
        _if_result$3305 = 0;
      }
      if (_if_result$3305) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2796;
        int32_t _cnt$3076;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2137;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2136;
        moonbit_incref(_entry$922);
        moonbit_decref(self$920);
        moonbit_decref(key$916);
        _field$2796 = _entry$922->$5;
        _cnt$3076 = Moonbit_object_header(_entry$922)->rc;
        if (_cnt$3076 > 1) {
          int32_t _new_cnt$3079;
          moonbit_incref(_field$2796);
          _new_cnt$3079 = _cnt$3076 - 1;
          Moonbit_object_header(_entry$922)->rc = _new_cnt$3079;
        } else if (_cnt$3076 == 1) {
          moonbit_string_t _field$3078 = _entry$922->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3077;
          moonbit_decref(_field$3078);
          _field$3077 = _entry$922->$1;
          if (_field$3077) {
            moonbit_decref(_field$3077);
          }
          moonbit_free(_entry$922);
        }
        value$2137 = _field$2796;
        _tmp$2136 = value$2137;
        return _tmp$2136;
      } else {
        moonbit_incref(_entry$922);
      }
      _field$2795 = _entry$922->$2;
      moonbit_decref(_entry$922);
      psl$2138 = _field$2795;
      if (i$917 > psl$2138) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2139;
        moonbit_decref(self$920);
        moonbit_decref(key$916);
        _tmp$2139 = 0;
        return _tmp$2139;
      }
      _tmp$2140 = i$917 + 1;
      _tmp$2142 = idx$918 + 1;
      capacity_mask$2143 = self$920->$3;
      _tmp$2141 = _tmp$2142 & capacity_mask$2143;
      i$917 = _tmp$2140;
      idx$918 = _tmp$2141;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$911,
  int32_t key$907
) {
  int32_t hash$906 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$907);
  int32_t capacity_mask$2132 = self$911->$3;
  int32_t _tmp$2131 = hash$906 & capacity_mask$2132;
  int32_t i$908 = 0;
  int32_t idx$909 = _tmp$2131;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2804 =
      self$911->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2130 =
      _field$2804;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2803;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$910;
    if (idx$909 < 0 || idx$909 >= Moonbit_array_length(entries$2130)) {
      moonbit_panic();
    }
    _tmp$2803
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2130[
        idx$909
      ];
    _bind$910 = _tmp$2803;
    if (_bind$910 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2119;
      if (_bind$910) {
        moonbit_incref(_bind$910);
      }
      moonbit_decref(self$911);
      if (_bind$910) {
        moonbit_decref(_bind$910);
      }
      _tmp$2119 = 0;
      return _tmp$2119;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$912 =
        _bind$910;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$913 =
        _Some$912;
      int32_t hash$2121 = _entry$913->$3;
      int32_t _if_result$3307;
      int32_t _field$2801;
      int32_t psl$2124;
      int32_t _tmp$2126;
      int32_t _tmp$2128;
      int32_t capacity_mask$2129;
      int32_t _tmp$2127;
      if (hash$2121 == hash$906) {
        int32_t key$2120 = _entry$913->$4;
        _if_result$3307 = key$2120 == key$907;
      } else {
        _if_result$3307 = 0;
      }
      if (_if_result$3307) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2802;
        int32_t _cnt$3080;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2123;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2122;
        moonbit_incref(_entry$913);
        moonbit_decref(self$911);
        _field$2802 = _entry$913->$5;
        _cnt$3080 = Moonbit_object_header(_entry$913)->rc;
        if (_cnt$3080 > 1) {
          int32_t _new_cnt$3082;
          moonbit_incref(_field$2802);
          _new_cnt$3082 = _cnt$3080 - 1;
          Moonbit_object_header(_entry$913)->rc = _new_cnt$3082;
        } else if (_cnt$3080 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3081 =
            _entry$913->$1;
          if (_field$3081) {
            moonbit_decref(_field$3081);
          }
          moonbit_free(_entry$913);
        }
        value$2123 = _field$2802;
        _tmp$2122 = value$2123;
        return _tmp$2122;
      } else {
        moonbit_incref(_entry$913);
      }
      _field$2801 = _entry$913->$2;
      moonbit_decref(_entry$913);
      psl$2124 = _field$2801;
      if (i$908 > psl$2124) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2125;
        moonbit_decref(self$911);
        _tmp$2125 = 0;
        return _tmp$2125;
      }
      _tmp$2126 = i$908 + 1;
      _tmp$2128 = idx$909 + 1;
      capacity_mask$2129 = self$911->$3;
      _tmp$2127 = _tmp$2128 & capacity_mask$2129;
      i$908 = _tmp$2126;
      idx$909 = _tmp$2127;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$902,
  moonbit_string_t key$898
) {
  int32_t hash$897;
  int32_t capacity_mask$2118;
  int32_t _tmp$2117;
  int32_t i$899;
  int32_t idx$900;
  moonbit_incref(key$898);
  hash$897 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$898);
  capacity_mask$2118 = self$902->$3;
  _tmp$2117 = hash$897 & capacity_mask$2118;
  i$899 = 0;
  idx$900 = _tmp$2117;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2810 =
      self$902->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2116 =
      _field$2810;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2809;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$901;
    if (idx$900 < 0 || idx$900 >= Moonbit_array_length(entries$2116)) {
      moonbit_panic();
    }
    _tmp$2809
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2116[
        idx$900
      ];
    _bind$901 = _tmp$2809;
    if (_bind$901 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2105;
      if (_bind$901) {
        moonbit_incref(_bind$901);
      }
      moonbit_decref(self$902);
      if (_bind$901) {
        moonbit_decref(_bind$901);
      }
      moonbit_decref(key$898);
      _tmp$2105 = 0;
      return _tmp$2105;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$903 =
        _bind$901;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$904 =
        _Some$903;
      int32_t hash$2107 = _entry$904->$3;
      int32_t _if_result$3309;
      int32_t _field$2805;
      int32_t psl$2110;
      int32_t _tmp$2112;
      int32_t _tmp$2114;
      int32_t capacity_mask$2115;
      int32_t _tmp$2113;
      if (hash$2107 == hash$897) {
        moonbit_string_t _field$2808 = _entry$904->$4;
        moonbit_string_t key$2106 = _field$2808;
        int32_t _tmp$2807 = moonbit_val_array_equal(key$2106, key$898);
        _if_result$3309 = _tmp$2807;
      } else {
        _if_result$3309 = 0;
      }
      if (_if_result$3309) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2806;
        int32_t _cnt$3083;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2109;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2108;
        moonbit_incref(_entry$904);
        moonbit_decref(self$902);
        moonbit_decref(key$898);
        _field$2806 = _entry$904->$5;
        _cnt$3083 = Moonbit_object_header(_entry$904)->rc;
        if (_cnt$3083 > 1) {
          int32_t _new_cnt$3086;
          moonbit_incref(_field$2806);
          _new_cnt$3086 = _cnt$3083 - 1;
          Moonbit_object_header(_entry$904)->rc = _new_cnt$3086;
        } else if (_cnt$3083 == 1) {
          moonbit_string_t _field$3085 = _entry$904->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3084;
          moonbit_decref(_field$3085);
          _field$3084 = _entry$904->$1;
          if (_field$3084) {
            moonbit_decref(_field$3084);
          }
          moonbit_free(_entry$904);
        }
        value$2109 = _field$2806;
        _tmp$2108 = value$2109;
        return _tmp$2108;
      } else {
        moonbit_incref(_entry$904);
      }
      _field$2805 = _entry$904->$2;
      moonbit_decref(_entry$904);
      psl$2110 = _field$2805;
      if (i$899 > psl$2110) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2111;
        moonbit_decref(self$902);
        moonbit_decref(key$898);
        _tmp$2111 = 0;
        return _tmp$2111;
      }
      _tmp$2112 = i$899 + 1;
      _tmp$2114 = idx$900 + 1;
      capacity_mask$2115 = self$902->$3;
      _tmp$2113 = _tmp$2114 & capacity_mask$2115;
      i$899 = _tmp$2112;
      idx$900 = _tmp$2113;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$890
) {
  int32_t length$889;
  int32_t capacity$891;
  int32_t _tmp$2096;
  int32_t _tmp$2095;
  int32_t _tmp$2104;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$892;
  int32_t _len$893;
  int32_t _i$894;
  moonbit_incref(arr$890.$0);
  length$889 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$890);
  capacity$891 = $Int$$next_power_of_two(length$889);
  _tmp$2096 = capacity$891;
  _tmp$2095 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2096);
  if (length$889 > _tmp$2095) {
    int32_t _tmp$2097 = capacity$891;
    capacity$891 = _tmp$2097 * 2;
  }
  _tmp$2104 = capacity$891;
  m$892 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$2104);
  moonbit_incref(arr$890.$0);
  _len$893 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$890);
  _i$894 = 0;
  while (1) {
    if (_i$894 < _len$893) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2814 =
        arr$890.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2100 =
        _field$2814;
      int32_t start$2102 = arr$890.$1;
      int32_t _tmp$2101 = start$2102 + _i$894;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2813 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2100[
          _tmp$2101
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$895 =
        _tmp$2813;
      moonbit_string_t _field$2812 = e$895->$0;
      moonbit_string_t _tmp$2098 = _field$2812;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2811 =
        e$895->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2099 =
        _field$2811;
      int32_t _tmp$2103;
      moonbit_incref(_tmp$2099);
      moonbit_incref(_tmp$2098);
      moonbit_incref(m$892);
      $$moonbitlang$core$builtin$Map$$set$3(m$892, _tmp$2098, _tmp$2099);
      _tmp$2103 = _i$894 + 1;
      _i$894 = _tmp$2103;
      continue;
    } else {
      moonbit_decref(arr$890.$0);
    }
    break;
  }
  return m$892;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$882
) {
  int32_t length$881;
  int32_t capacity$883;
  int32_t _tmp$2086;
  int32_t _tmp$2085;
  int32_t _tmp$2094;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$884;
  int32_t _len$885;
  int32_t _i$886;
  moonbit_incref(arr$882.$0);
  length$881 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$882);
  capacity$883 = $Int$$next_power_of_two(length$881);
  _tmp$2086 = capacity$883;
  _tmp$2085 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2086);
  if (length$881 > _tmp$2085) {
    int32_t _tmp$2087 = capacity$883;
    capacity$883 = _tmp$2087 * 2;
  }
  _tmp$2094 = capacity$883;
  m$884 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$2094);
  moonbit_incref(arr$882.$0);
  _len$885 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$882);
  _i$886 = 0;
  while (1) {
    if (_i$886 < _len$885) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2818 =
        arr$882.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2090 =
        _field$2818;
      int32_t start$2092 = arr$882.$1;
      int32_t _tmp$2091 = start$2092 + _i$886;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2817 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2090[
          _tmp$2091
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$887 =
        _tmp$2817;
      moonbit_string_t _field$2816 = e$887->$0;
      moonbit_string_t _tmp$2088 = _field$2816;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2815 =
        e$887->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2089 =
        _field$2815;
      int32_t _tmp$2093;
      moonbit_incref(_tmp$2089);
      moonbit_incref(_tmp$2088);
      moonbit_incref(m$884);
      $$moonbitlang$core$builtin$Map$$set$2(m$884, _tmp$2088, _tmp$2089);
      _tmp$2093 = _i$886 + 1;
      _i$886 = _tmp$2093;
      continue;
    } else {
      moonbit_decref(arr$882.$0);
    }
    break;
  }
  return m$884;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$874
) {
  int32_t length$873;
  int32_t capacity$875;
  int32_t _tmp$2076;
  int32_t _tmp$2075;
  int32_t _tmp$2084;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$876;
  int32_t _len$877;
  int32_t _i$878;
  moonbit_incref(arr$874.$0);
  length$873 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$874);
  capacity$875 = $Int$$next_power_of_two(length$873);
  _tmp$2076 = capacity$875;
  _tmp$2075 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2076);
  if (length$873 > _tmp$2075) {
    int32_t _tmp$2077 = capacity$875;
    capacity$875 = _tmp$2077 * 2;
  }
  _tmp$2084 = capacity$875;
  m$876 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$2084);
  moonbit_incref(arr$874.$0);
  _len$877 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$874);
  _i$878 = 0;
  while (1) {
    if (_i$878 < _len$877) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2821 =
        arr$874.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$2080 =
        _field$2821;
      int32_t start$2082 = arr$874.$1;
      int32_t _tmp$2081 = start$2082 + _i$878;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2820 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$2080[
          _tmp$2081
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$879 =
        _tmp$2820;
      int32_t _tmp$2078 = e$879->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2819 =
        e$879->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2079 =
        _field$2819;
      int32_t _tmp$2083;
      moonbit_incref(_tmp$2079);
      moonbit_incref(m$876);
      $$moonbitlang$core$builtin$Map$$set$1(m$876, _tmp$2078, _tmp$2079);
      _tmp$2083 = _i$878 + 1;
      _i$878 = _tmp$2083;
      continue;
    } else {
      moonbit_decref(arr$874.$0);
    }
    break;
  }
  return m$876;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$866
) {
  int32_t length$865;
  int32_t capacity$867;
  int32_t _tmp$2066;
  int32_t _tmp$2065;
  int32_t _tmp$2074;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$868;
  int32_t _len$869;
  int32_t _i$870;
  moonbit_incref(arr$866.$0);
  length$865 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$866);
  capacity$867 = $Int$$next_power_of_two(length$865);
  _tmp$2066 = capacity$867;
  _tmp$2065 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2066);
  if (length$865 > _tmp$2065) {
    int32_t _tmp$2067 = capacity$867;
    capacity$867 = _tmp$2067 * 2;
  }
  _tmp$2074 = capacity$867;
  m$868 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$2074);
  moonbit_incref(arr$866.$0);
  _len$869 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$866);
  _i$870 = 0;
  while (1) {
    if (_i$870 < _len$869) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2825 =
        arr$866.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2070 =
        _field$2825;
      int32_t start$2072 = arr$866.$1;
      int32_t _tmp$2071 = start$2072 + _i$870;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2824 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2070[
          _tmp$2071
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$871 =
        _tmp$2824;
      moonbit_string_t _field$2823 = e$871->$0;
      moonbit_string_t _tmp$2068 = _field$2823;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2822 =
        e$871->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2069 =
        _field$2822;
      int32_t _tmp$2073;
      moonbit_incref(_tmp$2069);
      moonbit_incref(_tmp$2068);
      moonbit_incref(m$868);
      $$moonbitlang$core$builtin$Map$$set$0(m$868, _tmp$2068, _tmp$2069);
      _tmp$2073 = _i$870 + 1;
      _i$870 = _tmp$2073;
      continue;
    } else {
      moonbit_decref(arr$866.$0);
    }
    break;
  }
  return m$868;
}

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$862,
  moonbit_string_t key$863,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$864
) {
  int32_t _tmp$2064;
  moonbit_incref(key$863);
  _tmp$2064 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$863);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$862, key$863, value$864, _tmp$2064
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$859,
  moonbit_string_t key$860,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$861
) {
  int32_t _tmp$2063;
  moonbit_incref(key$860);
  _tmp$2063 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$860);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$859, key$860, value$861, _tmp$2063
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$856,
  int32_t key$857,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$858
) {
  int32_t _tmp$2062 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$857);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$856, key$857, value$858, _tmp$2062
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$853,
  moonbit_string_t key$854,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$855
) {
  int32_t _tmp$2061;
  moonbit_incref(key$854);
  _tmp$2061 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$854);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$853, key$854, value$855, _tmp$2061
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$843
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2832 =
    self$843->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$842 =
    _field$2832;
  int32_t capacity$2060 = self$843->$2;
  int32_t new_capacity$844 = capacity$2060 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2055 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2054 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$844, _tmp$2055
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2831 =
    self$843->$0;
  int32_t _tmp$2056;
  int32_t capacity$2058;
  int32_t _tmp$2057;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2059;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2830;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$845;
  if (old_head$842) {
    moonbit_incref(old_head$842);
  }
  moonbit_decref(_old$2831);
  self$843->$0 = _tmp$2054;
  self$843->$2 = new_capacity$844;
  _tmp$2056 = new_capacity$844 - 1;
  self$843->$3 = _tmp$2056;
  capacity$2058 = self$843->$2;
  _tmp$2057 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2058);
  self$843->$4 = _tmp$2057;
  self$843->$1 = 0;
  _tmp$2059 = 0;
  _old$2830 = self$843->$5;
  if (_old$2830) {
    moonbit_decref(_old$2830);
  }
  self$843->$5 = _tmp$2059;
  self$843->$6 = -1;
  _param$845 = old_head$842;
  while (1) {
    if (_param$845 == 0) {
      if (_param$845) {
        moonbit_decref(_param$845);
      }
      moonbit_decref(self$843);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$846 =
        _param$845;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$847 =
        _Some$846;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2829 =
        _x$847->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$848 =
        _field$2829;
      moonbit_string_t _field$2828 = _x$847->$4;
      moonbit_string_t _key$849 = _field$2828;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2827 =
        _x$847->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$850 =
        _field$2827;
      int32_t _field$2826 = _x$847->$3;
      int32_t _cnt$3087 = Moonbit_object_header(_x$847)->rc;
      int32_t _hash$851;
      if (_cnt$3087 > 1) {
        int32_t _new_cnt$3088;
        moonbit_incref(_value$850);
        moonbit_incref(_key$849);
        if (_next$848) {
          moonbit_incref(_next$848);
        }
        _new_cnt$3088 = _cnt$3087 - 1;
        Moonbit_object_header(_x$847)->rc = _new_cnt$3088;
      } else if (_cnt$3087 == 1) {
        moonbit_free(_x$847);
      }
      _hash$851 = _field$2826;
      moonbit_incref(self$843);
      $$moonbitlang$core$builtin$Map$$set_with_hash$3(
        self$843, _key$849, _value$850, _hash$851
      );
      _param$845 = _next$848;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$832
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2839 =
    self$832->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$831 =
    _field$2839;
  int32_t capacity$2053 = self$832->$2;
  int32_t new_capacity$833 = capacity$2053 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2048 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2047 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$833, _tmp$2048
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2838 =
    self$832->$0;
  int32_t _tmp$2049;
  int32_t capacity$2051;
  int32_t _tmp$2050;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2052;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2837;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$834;
  if (old_head$831) {
    moonbit_incref(old_head$831);
  }
  moonbit_decref(_old$2838);
  self$832->$0 = _tmp$2047;
  self$832->$2 = new_capacity$833;
  _tmp$2049 = new_capacity$833 - 1;
  self$832->$3 = _tmp$2049;
  capacity$2051 = self$832->$2;
  _tmp$2050 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2051);
  self$832->$4 = _tmp$2050;
  self$832->$1 = 0;
  _tmp$2052 = 0;
  _old$2837 = self$832->$5;
  if (_old$2837) {
    moonbit_decref(_old$2837);
  }
  self$832->$5 = _tmp$2052;
  self$832->$6 = -1;
  _param$834 = old_head$831;
  while (1) {
    if (_param$834 == 0) {
      if (_param$834) {
        moonbit_decref(_param$834);
      }
      moonbit_decref(self$832);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$835 =
        _param$834;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$836 =
        _Some$835;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2836 =
        _x$836->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$837 =
        _field$2836;
      moonbit_string_t _field$2835 = _x$836->$4;
      moonbit_string_t _key$838 = _field$2835;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2834 =
        _x$836->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$839 =
        _field$2834;
      int32_t _field$2833 = _x$836->$3;
      int32_t _cnt$3089 = Moonbit_object_header(_x$836)->rc;
      int32_t _hash$840;
      if (_cnt$3089 > 1) {
        int32_t _new_cnt$3090;
        moonbit_incref(_value$839);
        moonbit_incref(_key$838);
        if (_next$837) {
          moonbit_incref(_next$837);
        }
        _new_cnt$3090 = _cnt$3089 - 1;
        Moonbit_object_header(_x$836)->rc = _new_cnt$3090;
      } else if (_cnt$3089 == 1) {
        moonbit_free(_x$836);
      }
      _hash$840 = _field$2833;
      moonbit_incref(self$832);
      $$moonbitlang$core$builtin$Map$$set_with_hash$2(
        self$832, _key$838, _value$839, _hash$840
      );
      _param$834 = _next$837;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$821
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2845 =
    self$821->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$820 =
    _field$2845;
  int32_t capacity$2046 = self$821->$2;
  int32_t new_capacity$822 = capacity$2046 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2041 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$2040 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$822, _tmp$2041
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$2844 =
    self$821->$0;
  int32_t _tmp$2042;
  int32_t capacity$2044;
  int32_t _tmp$2043;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2045;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2843;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$823;
  if (old_head$820) {
    moonbit_incref(old_head$820);
  }
  moonbit_decref(_old$2844);
  self$821->$0 = _tmp$2040;
  self$821->$2 = new_capacity$822;
  _tmp$2042 = new_capacity$822 - 1;
  self$821->$3 = _tmp$2042;
  capacity$2044 = self$821->$2;
  _tmp$2043 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2044);
  self$821->$4 = _tmp$2043;
  self$821->$1 = 0;
  _tmp$2045 = 0;
  _old$2843 = self$821->$5;
  if (_old$2843) {
    moonbit_decref(_old$2843);
  }
  self$821->$5 = _tmp$2045;
  self$821->$6 = -1;
  _param$823 = old_head$820;
  while (1) {
    if (_param$823 == 0) {
      if (_param$823) {
        moonbit_decref(_param$823);
      }
      moonbit_decref(self$821);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$824 =
        _param$823;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _x$825 =
        _Some$824;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2842 =
        _x$825->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$826 =
        _field$2842;
      int32_t _key$827 = _x$825->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2841 =
        _x$825->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$828 =
        _field$2841;
      int32_t _field$2840 = _x$825->$3;
      int32_t _cnt$3091 = Moonbit_object_header(_x$825)->rc;
      int32_t _hash$829;
      if (_cnt$3091 > 1) {
        int32_t _new_cnt$3092;
        moonbit_incref(_value$828);
        if (_next$826) {
          moonbit_incref(_next$826);
        }
        _new_cnt$3092 = _cnt$3091 - 1;
        Moonbit_object_header(_x$825)->rc = _new_cnt$3092;
      } else if (_cnt$3091 == 1) {
        moonbit_free(_x$825);
      }
      _hash$829 = _field$2840;
      moonbit_incref(self$821);
      $$moonbitlang$core$builtin$Map$$set_with_hash$1(
        self$821, _key$827, _value$828, _hash$829
      );
      _param$823 = _next$826;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$810
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2852 =
    self$810->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$809 =
    _field$2852;
  int32_t capacity$2039 = self$810->$2;
  int32_t new_capacity$811 = capacity$2039 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2034 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2033 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$811, _tmp$2034
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2851 =
    self$810->$0;
  int32_t _tmp$2035;
  int32_t capacity$2037;
  int32_t _tmp$2036;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2038;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2850;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$812;
  if (old_head$809) {
    moonbit_incref(old_head$809);
  }
  moonbit_decref(_old$2851);
  self$810->$0 = _tmp$2033;
  self$810->$2 = new_capacity$811;
  _tmp$2035 = new_capacity$811 - 1;
  self$810->$3 = _tmp$2035;
  capacity$2037 = self$810->$2;
  _tmp$2036 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2037);
  self$810->$4 = _tmp$2036;
  self$810->$1 = 0;
  _tmp$2038 = 0;
  _old$2850 = self$810->$5;
  if (_old$2850) {
    moonbit_decref(_old$2850);
  }
  self$810->$5 = _tmp$2038;
  self$810->$6 = -1;
  _param$812 = old_head$809;
  while (1) {
    if (_param$812 == 0) {
      if (_param$812) {
        moonbit_decref(_param$812);
      }
      moonbit_decref(self$810);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$813 =
        _param$812;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$814 =
        _Some$813;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2849 =
        _x$814->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$815 =
        _field$2849;
      moonbit_string_t _field$2848 = _x$814->$4;
      moonbit_string_t _key$816 = _field$2848;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2847 =
        _x$814->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$817 =
        _field$2847;
      int32_t _field$2846 = _x$814->$3;
      int32_t _cnt$3093 = Moonbit_object_header(_x$814)->rc;
      int32_t _hash$818;
      if (_cnt$3093 > 1) {
        int32_t _new_cnt$3094;
        moonbit_incref(_value$817);
        moonbit_incref(_key$816);
        if (_next$815) {
          moonbit_incref(_next$815);
        }
        _new_cnt$3094 = _cnt$3093 - 1;
        Moonbit_object_header(_x$814)->rc = _new_cnt$3094;
      } else if (_cnt$3093 == 1) {
        moonbit_free(_x$814);
      }
      _hash$818 = _field$2846;
      moonbit_incref(self$810);
      $$moonbitlang$core$builtin$Map$$set_with_hash$0(
        self$810, _key$816, _value$817, _hash$818
      );
      _param$812 = _next$815;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$793,
  moonbit_string_t key$802,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$803,
  int32_t hash$801
) {
  int32_t size$2019 = self$793->$1;
  int32_t grow_at$2020 = self$793->$4;
  int32_t capacity_mask$2032;
  int32_t _tmp$2031;
  struct $$3c$Int$2a$Int$3e$* _bind$794;
  int32_t psl$795;
  int32_t idx$796;
  int32_t _idx$804;
  int32_t _field$2853;
  int32_t _psl$805;
  int32_t _bind$806;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$807;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$808;
  if (size$2019 >= grow_at$2020) {
    moonbit_incref(self$793);
    $$moonbitlang$core$builtin$Map$$grow$3(self$793);
  }
  capacity_mask$2032 = self$793->$3;
  _tmp$2031 = hash$801 & capacity_mask$2032;
  psl$795 = 0;
  idx$796 = _tmp$2031;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2858 =
      self$793->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2030 =
      _field$2858;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2857;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$797;
    if (idx$796 < 0 || idx$796 >= Moonbit_array_length(entries$2030)) {
      moonbit_panic();
    }
    _tmp$2857
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2030[
        idx$796
      ];
    _bind$797 = _tmp$2857;
    if (_bind$797 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2021 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2021)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2021->$0 = idx$796;
      _tuple$2021->$1 = psl$795;
      _bind$794 = _tuple$2021;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$799 =
        _bind$797;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$800 =
        _Some$799;
      int32_t hash$2023 = _curr_entry$800->$3;
      int32_t _if_result$3319;
      int32_t psl$2024;
      int32_t _tmp$2026;
      int32_t _tmp$2028;
      int32_t capacity_mask$2029;
      int32_t _tmp$2027;
      if (hash$2023 == hash$801) {
        moonbit_string_t _field$2856 = _curr_entry$800->$4;
        moonbit_string_t key$2022 = _field$2856;
        int32_t _tmp$2855 = moonbit_val_array_equal(key$2022, key$802);
        _if_result$3319 = _tmp$2855;
      } else {
        _if_result$3319 = 0;
      }
      if (_if_result$3319) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2854;
        moonbit_incref(_curr_entry$800);
        moonbit_decref(key$802);
        moonbit_decref(self$793);
        _old$2854 = _curr_entry$800->$5;
        moonbit_decref(_old$2854);
        _curr_entry$800->$5 = value$803;
        moonbit_decref(_curr_entry$800);
        return 0;
      } else {
        moonbit_incref(_curr_entry$800);
      }
      psl$2024 = _curr_entry$800->$2;
      if (psl$795 > psl$2024) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2025;
        moonbit_incref(self$793);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$793, idx$796, _curr_entry$800
        );
        _tuple$2025
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2025)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2025->$0 = idx$796;
        _tuple$2025->$1 = psl$795;
        _bind$794 = _tuple$2025;
        break;
      } else {
        moonbit_decref(_curr_entry$800);
      }
      _tmp$2026 = psl$795 + 1;
      _tmp$2028 = idx$796 + 1;
      capacity_mask$2029 = self$793->$3;
      _tmp$2027 = _tmp$2028 & capacity_mask$2029;
      psl$795 = _tmp$2026;
      idx$796 = _tmp$2027;
      continue;
    }
    break;
  }
  _idx$804 = _bind$794->$0;
  _field$2853 = _bind$794->$1;
  moonbit_decref(_bind$794);
  _psl$805 = _field$2853;
  _bind$806 = self$793->$6;
  _bind$807 = 0;
  entry$808
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$808)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$808->$0 = _bind$806;
  entry$808->$1 = _bind$807;
  entry$808->$2 = _psl$805;
  entry$808->$3 = hash$801;
  entry$808->$4 = key$802;
  entry$808->$5 = value$803;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
    self$793, _idx$804, entry$808
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$777,
  moonbit_string_t key$786,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$787,
  int32_t hash$785
) {
  int32_t size$2005 = self$777->$1;
  int32_t grow_at$2006 = self$777->$4;
  int32_t capacity_mask$2018;
  int32_t _tmp$2017;
  struct $$3c$Int$2a$Int$3e$* _bind$778;
  int32_t psl$779;
  int32_t idx$780;
  int32_t _idx$788;
  int32_t _field$2859;
  int32_t _psl$789;
  int32_t _bind$790;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$791;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$792;
  if (size$2005 >= grow_at$2006) {
    moonbit_incref(self$777);
    $$moonbitlang$core$builtin$Map$$grow$2(self$777);
  }
  capacity_mask$2018 = self$777->$3;
  _tmp$2017 = hash$785 & capacity_mask$2018;
  psl$779 = 0;
  idx$780 = _tmp$2017;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2864 =
      self$777->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2016 =
      _field$2864;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2863;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$781;
    if (idx$780 < 0 || idx$780 >= Moonbit_array_length(entries$2016)) {
      moonbit_panic();
    }
    _tmp$2863
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2016[
        idx$780
      ];
    _bind$781 = _tmp$2863;
    if (_bind$781 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2007 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2007)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2007->$0 = idx$780;
      _tuple$2007->$1 = psl$779;
      _bind$778 = _tuple$2007;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$783 =
        _bind$781;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$784 =
        _Some$783;
      int32_t hash$2009 = _curr_entry$784->$3;
      int32_t _if_result$3321;
      int32_t psl$2010;
      int32_t _tmp$2012;
      int32_t _tmp$2014;
      int32_t capacity_mask$2015;
      int32_t _tmp$2013;
      if (hash$2009 == hash$785) {
        moonbit_string_t _field$2862 = _curr_entry$784->$4;
        moonbit_string_t key$2008 = _field$2862;
        int32_t _tmp$2861 = moonbit_val_array_equal(key$2008, key$786);
        _if_result$3321 = _tmp$2861;
      } else {
        _if_result$3321 = 0;
      }
      if (_if_result$3321) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2860;
        moonbit_incref(_curr_entry$784);
        moonbit_decref(key$786);
        moonbit_decref(self$777);
        _old$2860 = _curr_entry$784->$5;
        moonbit_decref(_old$2860);
        _curr_entry$784->$5 = value$787;
        moonbit_decref(_curr_entry$784);
        return 0;
      } else {
        moonbit_incref(_curr_entry$784);
      }
      psl$2010 = _curr_entry$784->$2;
      if (psl$779 > psl$2010) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2011;
        moonbit_incref(self$777);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$777, idx$780, _curr_entry$784
        );
        _tuple$2011
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2011)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2011->$0 = idx$780;
        _tuple$2011->$1 = psl$779;
        _bind$778 = _tuple$2011;
        break;
      } else {
        moonbit_decref(_curr_entry$784);
      }
      _tmp$2012 = psl$779 + 1;
      _tmp$2014 = idx$780 + 1;
      capacity_mask$2015 = self$777->$3;
      _tmp$2013 = _tmp$2014 & capacity_mask$2015;
      psl$779 = _tmp$2012;
      idx$780 = _tmp$2013;
      continue;
    }
    break;
  }
  _idx$788 = _bind$778->$0;
  _field$2859 = _bind$778->$1;
  moonbit_decref(_bind$778);
  _psl$789 = _field$2859;
  _bind$790 = self$777->$6;
  _bind$791 = 0;
  entry$792
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$792)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$792->$0 = _bind$790;
  entry$792->$1 = _bind$791;
  entry$792->$2 = _psl$789;
  entry$792->$3 = hash$785;
  entry$792->$4 = key$786;
  entry$792->$5 = value$787;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
    self$777, _idx$788, entry$792
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$761,
  int32_t key$770,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$771,
  int32_t hash$769
) {
  int32_t size$1991 = self$761->$1;
  int32_t grow_at$1992 = self$761->$4;
  int32_t capacity_mask$2004;
  int32_t _tmp$2003;
  struct $$3c$Int$2a$Int$3e$* _bind$762;
  int32_t psl$763;
  int32_t idx$764;
  int32_t _idx$772;
  int32_t _field$2865;
  int32_t _psl$773;
  int32_t _bind$774;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$775;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$776;
  if (size$1991 >= grow_at$1992) {
    moonbit_incref(self$761);
    $$moonbitlang$core$builtin$Map$$grow$1(self$761);
  }
  capacity_mask$2004 = self$761->$3;
  _tmp$2003 = hash$769 & capacity_mask$2004;
  psl$763 = 0;
  idx$764 = _tmp$2003;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2868 =
      self$761->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2002 =
      _field$2868;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2867;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$765;
    if (idx$764 < 0 || idx$764 >= Moonbit_array_length(entries$2002)) {
      moonbit_panic();
    }
    _tmp$2867
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2002[
        idx$764
      ];
    _bind$765 = _tmp$2867;
    if (_bind$765 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1993 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1993)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1993->$0 = idx$764;
      _tuple$1993->$1 = psl$763;
      _bind$762 = _tuple$1993;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$767 =
        _bind$765;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$768 =
        _Some$767;
      int32_t hash$1995 = _curr_entry$768->$3;
      int32_t _if_result$3323;
      int32_t psl$1996;
      int32_t _tmp$1998;
      int32_t _tmp$2000;
      int32_t capacity_mask$2001;
      int32_t _tmp$1999;
      if (hash$1995 == hash$769) {
        int32_t key$1994 = _curr_entry$768->$4;
        _if_result$3323 = key$1994 == key$770;
      } else {
        _if_result$3323 = 0;
      }
      if (_if_result$3323) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$2866;
        moonbit_incref(_curr_entry$768);
        moonbit_decref(self$761);
        _old$2866 = _curr_entry$768->$5;
        moonbit_decref(_old$2866);
        _curr_entry$768->$5 = value$771;
        moonbit_decref(_curr_entry$768);
        return 0;
      } else {
        moonbit_incref(_curr_entry$768);
      }
      psl$1996 = _curr_entry$768->$2;
      if (psl$763 > psl$1996) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1997;
        moonbit_incref(self$761);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$761, idx$764, _curr_entry$768
        );
        _tuple$1997
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1997)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1997->$0 = idx$764;
        _tuple$1997->$1 = psl$763;
        _bind$762 = _tuple$1997;
        break;
      } else {
        moonbit_decref(_curr_entry$768);
      }
      _tmp$1998 = psl$763 + 1;
      _tmp$2000 = idx$764 + 1;
      capacity_mask$2001 = self$761->$3;
      _tmp$1999 = _tmp$2000 & capacity_mask$2001;
      psl$763 = _tmp$1998;
      idx$764 = _tmp$1999;
      continue;
    }
    break;
  }
  _idx$772 = _bind$762->$0;
  _field$2865 = _bind$762->$1;
  moonbit_decref(_bind$762);
  _psl$773 = _field$2865;
  _bind$774 = self$761->$6;
  _bind$775 = 0;
  entry$776
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$776)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      2,
      0
  );
  entry$776->$0 = _bind$774;
  entry$776->$1 = _bind$775;
  entry$776->$2 = _psl$773;
  entry$776->$3 = hash$769;
  entry$776->$4 = key$770;
  entry$776->$5 = value$771;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
    self$761, _idx$772, entry$776
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$745,
  moonbit_string_t key$754,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$755,
  int32_t hash$753
) {
  int32_t size$1977 = self$745->$1;
  int32_t grow_at$1978 = self$745->$4;
  int32_t capacity_mask$1990;
  int32_t _tmp$1989;
  struct $$3c$Int$2a$Int$3e$* _bind$746;
  int32_t psl$747;
  int32_t idx$748;
  int32_t _idx$756;
  int32_t _field$2869;
  int32_t _psl$757;
  int32_t _bind$758;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$759;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$760;
  if (size$1977 >= grow_at$1978) {
    moonbit_incref(self$745);
    $$moonbitlang$core$builtin$Map$$grow$0(self$745);
  }
  capacity_mask$1990 = self$745->$3;
  _tmp$1989 = hash$753 & capacity_mask$1990;
  psl$747 = 0;
  idx$748 = _tmp$1989;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2874 =
      self$745->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1988 =
      _field$2874;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2873;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$749;
    if (idx$748 < 0 || idx$748 >= Moonbit_array_length(entries$1988)) {
      moonbit_panic();
    }
    _tmp$2873
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1988[
        idx$748
      ];
    _bind$749 = _tmp$2873;
    if (_bind$749 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1979 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1979)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1979->$0 = idx$748;
      _tuple$1979->$1 = psl$747;
      _bind$746 = _tuple$1979;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$751 =
        _bind$749;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$752 =
        _Some$751;
      int32_t hash$1981 = _curr_entry$752->$3;
      int32_t _if_result$3325;
      int32_t psl$1982;
      int32_t _tmp$1984;
      int32_t _tmp$1986;
      int32_t capacity_mask$1987;
      int32_t _tmp$1985;
      if (hash$1981 == hash$753) {
        moonbit_string_t _field$2872 = _curr_entry$752->$4;
        moonbit_string_t key$1980 = _field$2872;
        int32_t _tmp$2871 = moonbit_val_array_equal(key$1980, key$754);
        _if_result$3325 = _tmp$2871;
      } else {
        _if_result$3325 = 0;
      }
      if (_if_result$3325) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2870;
        moonbit_incref(_curr_entry$752);
        moonbit_decref(key$754);
        moonbit_decref(self$745);
        _old$2870 = _curr_entry$752->$5;
        moonbit_decref(_old$2870);
        _curr_entry$752->$5 = value$755;
        moonbit_decref(_curr_entry$752);
        return 0;
      } else {
        moonbit_incref(_curr_entry$752);
      }
      psl$1982 = _curr_entry$752->$2;
      if (psl$747 > psl$1982) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1983;
        moonbit_incref(self$745);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$745, idx$748, _curr_entry$752
        );
        _tuple$1983
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1983)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1983->$0 = idx$748;
        _tuple$1983->$1 = psl$747;
        _bind$746 = _tuple$1983;
        break;
      } else {
        moonbit_decref(_curr_entry$752);
      }
      _tmp$1984 = psl$747 + 1;
      _tmp$1986 = idx$748 + 1;
      capacity_mask$1987 = self$745->$3;
      _tmp$1985 = _tmp$1986 & capacity_mask$1987;
      psl$747 = _tmp$1984;
      idx$748 = _tmp$1985;
      continue;
    }
    break;
  }
  _idx$756 = _bind$746->$0;
  _field$2869 = _bind$746->$1;
  moonbit_decref(_bind$746);
  _psl$757 = _field$2869;
  _bind$758 = self$745->$6;
  _bind$759 = 0;
  entry$760
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$760)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$760->$0 = _bind$758;
  entry$760->$1 = _bind$759;
  entry$760->$2 = _psl$757;
  entry$760->$3 = hash$753;
  entry$760->$4 = key$754;
  entry$760->$5 = value$755;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
    self$745, _idx$756, entry$760
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$739,
  int32_t idx$744,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$743
) {
  int32_t psl$1976 = entry$743->$2;
  int32_t _tmp$1972 = psl$1976 + 1;
  int32_t _tmp$1974 = idx$744 + 1;
  int32_t capacity_mask$1975 = self$739->$3;
  int32_t _tmp$1973 = _tmp$1974 & capacity_mask$1975;
  int32_t psl$735 = _tmp$1972;
  int32_t idx$736 = _tmp$1973;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$737 =
    entry$743;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2876 =
      self$739->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1971 =
      _field$2876;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2875;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$738;
    if (idx$736 < 0 || idx$736 >= Moonbit_array_length(entries$1971)) {
      moonbit_panic();
    }
    _tmp$2875
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1971[
        idx$736
      ];
    _bind$738 = _tmp$2875;
    if (_bind$738 == 0) {
      entry$737->$2 = psl$735;
      $$moonbitlang$core$builtin$Map$$set_entry$3(
        self$739, entry$737, idx$736
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$741 =
        _bind$738;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$742 =
        _Some$741;
      int32_t psl$1961 = _curr_entry$742->$2;
      if (psl$735 > psl$1961) {
        int32_t psl$1966;
        int32_t _tmp$1962;
        int32_t _tmp$1964;
        int32_t capacity_mask$1965;
        int32_t _tmp$1963;
        entry$737->$2 = psl$735;
        moonbit_incref(_curr_entry$742);
        moonbit_incref(self$739);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$739, entry$737, idx$736
        );
        psl$1966 = _curr_entry$742->$2;
        _tmp$1962 = psl$1966 + 1;
        _tmp$1964 = idx$736 + 1;
        capacity_mask$1965 = self$739->$3;
        _tmp$1963 = _tmp$1964 & capacity_mask$1965;
        psl$735 = _tmp$1962;
        idx$736 = _tmp$1963;
        entry$737 = _curr_entry$742;
        continue;
      } else {
        int32_t _tmp$1967 = psl$735 + 1;
        int32_t _tmp$1969 = idx$736 + 1;
        int32_t capacity_mask$1970 = self$739->$3;
        int32_t _tmp$1968 = _tmp$1969 & capacity_mask$1970;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3327 =
          entry$737;
        psl$735 = _tmp$1967;
        idx$736 = _tmp$1968;
        entry$737 = _tmp$3327;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$729,
  int32_t idx$734,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$733
) {
  int32_t psl$1960 = entry$733->$2;
  int32_t _tmp$1956 = psl$1960 + 1;
  int32_t _tmp$1958 = idx$734 + 1;
  int32_t capacity_mask$1959 = self$729->$3;
  int32_t _tmp$1957 = _tmp$1958 & capacity_mask$1959;
  int32_t psl$725 = _tmp$1956;
  int32_t idx$726 = _tmp$1957;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$727 =
    entry$733;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2878 =
      self$729->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1955 =
      _field$2878;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2877;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$728;
    if (idx$726 < 0 || idx$726 >= Moonbit_array_length(entries$1955)) {
      moonbit_panic();
    }
    _tmp$2877
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1955[
        idx$726
      ];
    _bind$728 = _tmp$2877;
    if (_bind$728 == 0) {
      entry$727->$2 = psl$725;
      $$moonbitlang$core$builtin$Map$$set_entry$2(
        self$729, entry$727, idx$726
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$731 =
        _bind$728;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$732 =
        _Some$731;
      int32_t psl$1945 = _curr_entry$732->$2;
      if (psl$725 > psl$1945) {
        int32_t psl$1950;
        int32_t _tmp$1946;
        int32_t _tmp$1948;
        int32_t capacity_mask$1949;
        int32_t _tmp$1947;
        entry$727->$2 = psl$725;
        moonbit_incref(_curr_entry$732);
        moonbit_incref(self$729);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$729, entry$727, idx$726
        );
        psl$1950 = _curr_entry$732->$2;
        _tmp$1946 = psl$1950 + 1;
        _tmp$1948 = idx$726 + 1;
        capacity_mask$1949 = self$729->$3;
        _tmp$1947 = _tmp$1948 & capacity_mask$1949;
        psl$725 = _tmp$1946;
        idx$726 = _tmp$1947;
        entry$727 = _curr_entry$732;
        continue;
      } else {
        int32_t _tmp$1951 = psl$725 + 1;
        int32_t _tmp$1953 = idx$726 + 1;
        int32_t capacity_mask$1954 = self$729->$3;
        int32_t _tmp$1952 = _tmp$1953 & capacity_mask$1954;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3329 =
          entry$727;
        psl$725 = _tmp$1951;
        idx$726 = _tmp$1952;
        entry$727 = _tmp$3329;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$719,
  int32_t idx$724,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$723
) {
  int32_t psl$1944 = entry$723->$2;
  int32_t _tmp$1940 = psl$1944 + 1;
  int32_t _tmp$1942 = idx$724 + 1;
  int32_t capacity_mask$1943 = self$719->$3;
  int32_t _tmp$1941 = _tmp$1942 & capacity_mask$1943;
  int32_t psl$715 = _tmp$1940;
  int32_t idx$716 = _tmp$1941;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$717 =
    entry$723;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2880 =
      self$719->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1939 =
      _field$2880;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2879;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$718;
    if (idx$716 < 0 || idx$716 >= Moonbit_array_length(entries$1939)) {
      moonbit_panic();
    }
    _tmp$2879
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1939[
        idx$716
      ];
    _bind$718 = _tmp$2879;
    if (_bind$718 == 0) {
      entry$717->$2 = psl$715;
      $$moonbitlang$core$builtin$Map$$set_entry$1(
        self$719, entry$717, idx$716
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$721 =
        _bind$718;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$722 =
        _Some$721;
      int32_t psl$1929 = _curr_entry$722->$2;
      if (psl$715 > psl$1929) {
        int32_t psl$1934;
        int32_t _tmp$1930;
        int32_t _tmp$1932;
        int32_t capacity_mask$1933;
        int32_t _tmp$1931;
        entry$717->$2 = psl$715;
        moonbit_incref(_curr_entry$722);
        moonbit_incref(self$719);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$719, entry$717, idx$716
        );
        psl$1934 = _curr_entry$722->$2;
        _tmp$1930 = psl$1934 + 1;
        _tmp$1932 = idx$716 + 1;
        capacity_mask$1933 = self$719->$3;
        _tmp$1931 = _tmp$1932 & capacity_mask$1933;
        psl$715 = _tmp$1930;
        idx$716 = _tmp$1931;
        entry$717 = _curr_entry$722;
        continue;
      } else {
        int32_t _tmp$1935 = psl$715 + 1;
        int32_t _tmp$1937 = idx$716 + 1;
        int32_t capacity_mask$1938 = self$719->$3;
        int32_t _tmp$1936 = _tmp$1937 & capacity_mask$1938;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3331 =
          entry$717;
        psl$715 = _tmp$1935;
        idx$716 = _tmp$1936;
        entry$717 = _tmp$3331;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$709,
  int32_t idx$714,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$713
) {
  int32_t psl$1928 = entry$713->$2;
  int32_t _tmp$1924 = psl$1928 + 1;
  int32_t _tmp$1926 = idx$714 + 1;
  int32_t capacity_mask$1927 = self$709->$3;
  int32_t _tmp$1925 = _tmp$1926 & capacity_mask$1927;
  int32_t psl$705 = _tmp$1924;
  int32_t idx$706 = _tmp$1925;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$707 =
    entry$713;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2882 =
      self$709->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1923 =
      _field$2882;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2881;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$708;
    if (idx$706 < 0 || idx$706 >= Moonbit_array_length(entries$1923)) {
      moonbit_panic();
    }
    _tmp$2881
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1923[
        idx$706
      ];
    _bind$708 = _tmp$2881;
    if (_bind$708 == 0) {
      entry$707->$2 = psl$705;
      $$moonbitlang$core$builtin$Map$$set_entry$0(
        self$709, entry$707, idx$706
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$711 =
        _bind$708;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$712 =
        _Some$711;
      int32_t psl$1913 = _curr_entry$712->$2;
      if (psl$705 > psl$1913) {
        int32_t psl$1918;
        int32_t _tmp$1914;
        int32_t _tmp$1916;
        int32_t capacity_mask$1917;
        int32_t _tmp$1915;
        entry$707->$2 = psl$705;
        moonbit_incref(_curr_entry$712);
        moonbit_incref(self$709);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$709, entry$707, idx$706
        );
        psl$1918 = _curr_entry$712->$2;
        _tmp$1914 = psl$1918 + 1;
        _tmp$1916 = idx$706 + 1;
        capacity_mask$1917 = self$709->$3;
        _tmp$1915 = _tmp$1916 & capacity_mask$1917;
        psl$705 = _tmp$1914;
        idx$706 = _tmp$1915;
        entry$707 = _curr_entry$712;
        continue;
      } else {
        int32_t _tmp$1919 = psl$705 + 1;
        int32_t _tmp$1921 = idx$706 + 1;
        int32_t capacity_mask$1922 = self$709->$3;
        int32_t _tmp$1920 = _tmp$1921 & capacity_mask$1922;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3333 =
          entry$707;
        psl$705 = _tmp$1919;
        idx$706 = _tmp$1920;
        entry$707 = _tmp$3333;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$699,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$701,
  int32_t new_idx$700
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2885 =
    self$699->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1911 =
    _field$2885;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1912;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2884;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2883;
  int32_t _cnt$3095;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$702;
  moonbit_incref(entry$701);
  _tmp$1912 = entry$701;
  if (new_idx$700 < 0 || new_idx$700 >= Moonbit_array_length(entries$1911)) {
    moonbit_panic();
  }
  _old$2884
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1911[
      new_idx$700
    ];
  if (_old$2884) {
    moonbit_decref(_old$2884);
  }
  entries$1911[new_idx$700] = _tmp$1912;
  _field$2883 = entry$701->$1;
  _cnt$3095 = Moonbit_object_header(entry$701)->rc;
  if (_cnt$3095 > 1) {
    int32_t _new_cnt$3098;
    if (_field$2883) {
      moonbit_incref(_field$2883);
    }
    _new_cnt$3098 = _cnt$3095 - 1;
    Moonbit_object_header(entry$701)->rc = _new_cnt$3098;
  } else if (_cnt$3095 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3097 =
      entry$701->$5;
    moonbit_string_t _field$3096;
    moonbit_decref(_field$3097);
    _field$3096 = entry$701->$4;
    moonbit_decref(_field$3096);
    moonbit_free(entry$701);
  }
  _bind$702 = _field$2883;
  if (_bind$702 == 0) {
    if (_bind$702) {
      moonbit_decref(_bind$702);
    }
    self$699->$6 = new_idx$700;
    moonbit_decref(self$699);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$703;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$704;
    moonbit_decref(self$699);
    _Some$703 = _bind$702;
    _next$704 = _Some$703;
    _next$704->$0 = new_idx$700;
    moonbit_decref(_next$704);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$693,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$695,
  int32_t new_idx$694
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2888 =
    self$693->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1909 =
    _field$2888;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1910;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2887;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2886;
  int32_t _cnt$3099;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$696;
  moonbit_incref(entry$695);
  _tmp$1910 = entry$695;
  if (new_idx$694 < 0 || new_idx$694 >= Moonbit_array_length(entries$1909)) {
    moonbit_panic();
  }
  _old$2887
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1909[
      new_idx$694
    ];
  if (_old$2887) {
    moonbit_decref(_old$2887);
  }
  entries$1909[new_idx$694] = _tmp$1910;
  _field$2886 = entry$695->$1;
  _cnt$3099 = Moonbit_object_header(entry$695)->rc;
  if (_cnt$3099 > 1) {
    int32_t _new_cnt$3102;
    if (_field$2886) {
      moonbit_incref(_field$2886);
    }
    _new_cnt$3102 = _cnt$3099 - 1;
    Moonbit_object_header(entry$695)->rc = _new_cnt$3102;
  } else if (_cnt$3099 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3101 =
      entry$695->$5;
    moonbit_string_t _field$3100;
    moonbit_decref(_field$3101);
    _field$3100 = entry$695->$4;
    moonbit_decref(_field$3100);
    moonbit_free(entry$695);
  }
  _bind$696 = _field$2886;
  if (_bind$696 == 0) {
    if (_bind$696) {
      moonbit_decref(_bind$696);
    }
    self$693->$6 = new_idx$694;
    moonbit_decref(self$693);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$697;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$698;
    moonbit_decref(self$693);
    _Some$697 = _bind$696;
    _next$698 = _Some$697;
    _next$698->$0 = new_idx$694;
    moonbit_decref(_next$698);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$687,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$689,
  int32_t new_idx$688
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2891 =
    self$687->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1907 =
    _field$2891;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1908;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2890;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2889;
  int32_t _cnt$3103;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$690;
  moonbit_incref(entry$689);
  _tmp$1908 = entry$689;
  if (new_idx$688 < 0 || new_idx$688 >= Moonbit_array_length(entries$1907)) {
    moonbit_panic();
  }
  _old$2890
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1907[
      new_idx$688
    ];
  if (_old$2890) {
    moonbit_decref(_old$2890);
  }
  entries$1907[new_idx$688] = _tmp$1908;
  _field$2889 = entry$689->$1;
  _cnt$3103 = Moonbit_object_header(entry$689)->rc;
  if (_cnt$3103 > 1) {
    int32_t _new_cnt$3105;
    if (_field$2889) {
      moonbit_incref(_field$2889);
    }
    _new_cnt$3105 = _cnt$3103 - 1;
    Moonbit_object_header(entry$689)->rc = _new_cnt$3105;
  } else if (_cnt$3103 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3104 =
      entry$689->$5;
    moonbit_decref(_field$3104);
    moonbit_free(entry$689);
  }
  _bind$690 = _field$2889;
  if (_bind$690 == 0) {
    if (_bind$690) {
      moonbit_decref(_bind$690);
    }
    self$687->$6 = new_idx$688;
    moonbit_decref(self$687);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$691;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$692;
    moonbit_decref(self$687);
    _Some$691 = _bind$690;
    _next$692 = _Some$691;
    _next$692->$0 = new_idx$688;
    moonbit_decref(_next$692);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$681,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$683,
  int32_t new_idx$682
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2894 =
    self$681->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1905 =
    _field$2894;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1906;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2893;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2892;
  int32_t _cnt$3106;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$684;
  moonbit_incref(entry$683);
  _tmp$1906 = entry$683;
  if (new_idx$682 < 0 || new_idx$682 >= Moonbit_array_length(entries$1905)) {
    moonbit_panic();
  }
  _old$2893
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1905[
      new_idx$682
    ];
  if (_old$2893) {
    moonbit_decref(_old$2893);
  }
  entries$1905[new_idx$682] = _tmp$1906;
  _field$2892 = entry$683->$1;
  _cnt$3106 = Moonbit_object_header(entry$683)->rc;
  if (_cnt$3106 > 1) {
    int32_t _new_cnt$3109;
    if (_field$2892) {
      moonbit_incref(_field$2892);
    }
    _new_cnt$3109 = _cnt$3106 - 1;
    Moonbit_object_header(entry$683)->rc = _new_cnt$3109;
  } else if (_cnt$3106 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3108 =
      entry$683->$5;
    moonbit_string_t _field$3107;
    moonbit_decref(_field$3108);
    _field$3107 = entry$683->$4;
    moonbit_decref(_field$3107);
    moonbit_free(entry$683);
  }
  _bind$684 = _field$2892;
  if (_bind$684 == 0) {
    if (_bind$684) {
      moonbit_decref(_bind$684);
    }
    self$681->$6 = new_idx$682;
    moonbit_decref(self$681);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$685;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$686;
    moonbit_decref(self$681);
    _Some$685 = _bind$684;
    _next$686 = _Some$685;
    _next$686->$0 = new_idx$682;
    moonbit_decref(_next$686);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$678,
  int32_t idx$680,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$679
) {
  int32_t _bind$677 = self$678->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2896;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1901;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1902;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2895;
  int32_t size$1904;
  int32_t _tmp$1903;
  switch (_bind$677) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1896;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2897;
      moonbit_incref(entry$679);
      _tmp$1896 = entry$679;
      _old$2897 = self$678->$5;
      if (_old$2897) {
        moonbit_decref(_old$2897);
      }
      self$678->$5 = _tmp$1896;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2900 =
        self$678->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1900 =
        _field$2900;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2899;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1899;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1897;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1898;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2898;
      if (_bind$677 < 0 || _bind$677 >= Moonbit_array_length(entries$1900)) {
        moonbit_panic();
      }
      _tmp$2899
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1900[
          _bind$677
        ];
      _tmp$1899 = _tmp$2899;
      if (_tmp$1899) {
        moonbit_incref(_tmp$1899);
      }
      _tmp$1897 = $Option$$unwrap$3(_tmp$1899);
      moonbit_incref(entry$679);
      _tmp$1898 = entry$679;
      _old$2898 = _tmp$1897->$1;
      if (_old$2898) {
        moonbit_decref(_old$2898);
      }
      _tmp$1897->$1 = _tmp$1898;
      moonbit_decref(_tmp$1897);
      break;
    }
  }
  self$678->$6 = idx$680;
  _field$2896 = self$678->$0;
  entries$1901 = _field$2896;
  _tmp$1902 = entry$679;
  if (idx$680 < 0 || idx$680 >= Moonbit_array_length(entries$1901)) {
    moonbit_panic();
  }
  _old$2895
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1901[
      idx$680
    ];
  if (_old$2895) {
    moonbit_decref(_old$2895);
  }
  entries$1901[idx$680] = _tmp$1902;
  size$1904 = self$678->$1;
  _tmp$1903 = size$1904 + 1;
  self$678->$1 = _tmp$1903;
  moonbit_decref(self$678);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$674,
  int32_t idx$676,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$675
) {
  int32_t _bind$673 = self$674->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2902;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1892;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1893;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2901;
  int32_t size$1895;
  int32_t _tmp$1894;
  switch (_bind$673) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1887;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2903;
      moonbit_incref(entry$675);
      _tmp$1887 = entry$675;
      _old$2903 = self$674->$5;
      if (_old$2903) {
        moonbit_decref(_old$2903);
      }
      self$674->$5 = _tmp$1887;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2906 =
        self$674->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1891 =
        _field$2906;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2905;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1890;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1888;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1889;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2904;
      if (_bind$673 < 0 || _bind$673 >= Moonbit_array_length(entries$1891)) {
        moonbit_panic();
      }
      _tmp$2905
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1891[
          _bind$673
        ];
      _tmp$1890 = _tmp$2905;
      if (_tmp$1890) {
        moonbit_incref(_tmp$1890);
      }
      _tmp$1888 = $Option$$unwrap$2(_tmp$1890);
      moonbit_incref(entry$675);
      _tmp$1889 = entry$675;
      _old$2904 = _tmp$1888->$1;
      if (_old$2904) {
        moonbit_decref(_old$2904);
      }
      _tmp$1888->$1 = _tmp$1889;
      moonbit_decref(_tmp$1888);
      break;
    }
  }
  self$674->$6 = idx$676;
  _field$2902 = self$674->$0;
  entries$1892 = _field$2902;
  _tmp$1893 = entry$675;
  if (idx$676 < 0 || idx$676 >= Moonbit_array_length(entries$1892)) {
    moonbit_panic();
  }
  _old$2901
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1892[
      idx$676
    ];
  if (_old$2901) {
    moonbit_decref(_old$2901);
  }
  entries$1892[idx$676] = _tmp$1893;
  size$1895 = self$674->$1;
  _tmp$1894 = size$1895 + 1;
  self$674->$1 = _tmp$1894;
  moonbit_decref(self$674);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$670,
  int32_t idx$672,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$671
) {
  int32_t _bind$669 = self$670->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2908;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1883;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1884;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2907;
  int32_t size$1886;
  int32_t _tmp$1885;
  switch (_bind$669) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1878;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2909;
      moonbit_incref(entry$671);
      _tmp$1878 = entry$671;
      _old$2909 = self$670->$5;
      if (_old$2909) {
        moonbit_decref(_old$2909);
      }
      self$670->$5 = _tmp$1878;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2912 =
        self$670->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1882 =
        _field$2912;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2911;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1881;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1879;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1880;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2910;
      if (_bind$669 < 0 || _bind$669 >= Moonbit_array_length(entries$1882)) {
        moonbit_panic();
      }
      _tmp$2911
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1882[
          _bind$669
        ];
      _tmp$1881 = _tmp$2911;
      if (_tmp$1881) {
        moonbit_incref(_tmp$1881);
      }
      _tmp$1879 = $Option$$unwrap$1(_tmp$1881);
      moonbit_incref(entry$671);
      _tmp$1880 = entry$671;
      _old$2910 = _tmp$1879->$1;
      if (_old$2910) {
        moonbit_decref(_old$2910);
      }
      _tmp$1879->$1 = _tmp$1880;
      moonbit_decref(_tmp$1879);
      break;
    }
  }
  self$670->$6 = idx$672;
  _field$2908 = self$670->$0;
  entries$1883 = _field$2908;
  _tmp$1884 = entry$671;
  if (idx$672 < 0 || idx$672 >= Moonbit_array_length(entries$1883)) {
    moonbit_panic();
  }
  _old$2907
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1883[
      idx$672
    ];
  if (_old$2907) {
    moonbit_decref(_old$2907);
  }
  entries$1883[idx$672] = _tmp$1884;
  size$1886 = self$670->$1;
  _tmp$1885 = size$1886 + 1;
  self$670->$1 = _tmp$1885;
  moonbit_decref(self$670);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$666,
  int32_t idx$668,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$667
) {
  int32_t _bind$665 = self$666->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2914;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1874;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1875;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2913;
  int32_t size$1877;
  int32_t _tmp$1876;
  switch (_bind$665) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1869;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2915;
      moonbit_incref(entry$667);
      _tmp$1869 = entry$667;
      _old$2915 = self$666->$5;
      if (_old$2915) {
        moonbit_decref(_old$2915);
      }
      self$666->$5 = _tmp$1869;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2918 =
        self$666->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1873 =
        _field$2918;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2917;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1872;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1870;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1871;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2916;
      if (_bind$665 < 0 || _bind$665 >= Moonbit_array_length(entries$1873)) {
        moonbit_panic();
      }
      _tmp$2917
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1873[
          _bind$665
        ];
      _tmp$1872 = _tmp$2917;
      if (_tmp$1872) {
        moonbit_incref(_tmp$1872);
      }
      _tmp$1870 = $Option$$unwrap$0(_tmp$1872);
      moonbit_incref(entry$667);
      _tmp$1871 = entry$667;
      _old$2916 = _tmp$1870->$1;
      if (_old$2916) {
        moonbit_decref(_old$2916);
      }
      _tmp$1870->$1 = _tmp$1871;
      moonbit_decref(_tmp$1870);
      break;
    }
  }
  self$666->$6 = idx$668;
  _field$2914 = self$666->$0;
  entries$1874 = _field$2914;
  _tmp$1875 = entry$667;
  if (idx$668 < 0 || idx$668 >= Moonbit_array_length(entries$1874)) {
    moonbit_panic();
  }
  _old$2913
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1874[
      idx$668
    ];
  if (_old$2913) {
    moonbit_decref(_old$2913);
  }
  entries$1874[idx$668] = _tmp$1875;
  size$1877 = self$666->$1;
  _tmp$1876 = size$1877 + 1;
  self$666->$1 = _tmp$1876;
  moonbit_decref(self$666);
  return 0;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$660
) {
  int32_t capacity$659 = $Int$$next_power_of_two(capacity$660);
  int32_t _bind$661 = capacity$659 - 1;
  int32_t _bind$662 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$659);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1868 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$663 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$659, _tmp$1868
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$664 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$3334 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$3334)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$3334->$0 = _bind$663;
  _block$3334->$1 = 0;
  _block$3334->$2 = capacity$659;
  _block$3334->$3 = _bind$661;
  _block$3334->$4 = _bind$662;
  _block$3334->$5 = _bind$664;
  _block$3334->$6 = -1;
  return _block$3334;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$654
) {
  int32_t capacity$653 = $Int$$next_power_of_two(capacity$654);
  int32_t _bind$655 = capacity$653 - 1;
  int32_t _bind$656 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$653);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1867 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$657 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$653, _tmp$1867
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$658 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$3335 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$3335)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$3335->$0 = _bind$657;
  _block$3335->$1 = 0;
  _block$3335->$2 = capacity$653;
  _block$3335->$3 = _bind$655;
  _block$3335->$4 = _bind$656;
  _block$3335->$5 = _bind$658;
  _block$3335->$6 = -1;
  return _block$3335;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$648
) {
  int32_t capacity$647 = $Int$$next_power_of_two(capacity$648);
  int32_t _bind$649 = capacity$647 - 1;
  int32_t _bind$650 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$647);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1866 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$651 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$647, _tmp$1866
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$652 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$3336 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$3336)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$3336->$0 = _bind$651;
  _block$3336->$1 = 0;
  _block$3336->$2 = capacity$647;
  _block$3336->$3 = _bind$649;
  _block$3336->$4 = _bind$650;
  _block$3336->$5 = _bind$652;
  _block$3336->$6 = -1;
  return _block$3336;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$642
) {
  int32_t capacity$641 = $Int$$next_power_of_two(capacity$642);
  int32_t _bind$643 = capacity$641 - 1;
  int32_t _bind$644 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$641);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1865 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$645 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$641, _tmp$1865
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$646 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$3337 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$3337)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$3337->$0 = _bind$645;
  _block$3337->$1 = 0;
  _block$3337->$2 = capacity$641;
  _block$3337->$3 = _bind$643;
  _block$3337->$4 = _bind$644;
  _block$3337->$5 = _bind$646;
  _block$3337->$6 = -1;
  return _block$3337;
}

int32_t $Int$$next_power_of_two(int32_t self$640) {
  if (self$640 >= 0) {
    int32_t _tmp$1864;
    int32_t _tmp$1863;
    int32_t _tmp$1862;
    int32_t _tmp$1861;
    if (self$640 <= 1) {
      return 1;
    }
    if (self$640 > 1073741824) {
      return 1073741824;
    }
    _tmp$1864 = self$640 - 1;
    _tmp$1863 = moonbit_clz32(_tmp$1864);
    _tmp$1862 = _tmp$1863 - 1;
    _tmp$1861 = 2147483647 >> (_tmp$1862 & 31);
    return _tmp$1861 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$639) {
  int32_t _tmp$1860 = capacity$639 * 13;
  return _tmp$1860 / 16;
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$637
) {
  if (self$637 == 0) {
    if (self$637) {
      moonbit_decref(self$637);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$638 =
      self$637;
    return _Some$638;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$635
) {
  if (self$635 == 0) {
    if (self$635) {
      moonbit_decref(self$635);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$636 =
      self$635;
    return _Some$636;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$633
) {
  if (self$633 == 0) {
    if (self$633) {
      moonbit_decref(self$633);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$634 =
      self$633;
    return _Some$634;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$631
) {
  if (self$631 == 0) {
    if (self$631) {
      moonbit_decref(self$631);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$632 =
      self$631;
    return _Some$632;
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$3(
  int32_t self$625,
  int32_t other$626
) {
  if (self$625 == -1) {
    return other$626 == -1;
  } else {
    int32_t _Some$627 = self$625;
    int32_t _x$628 = _Some$627;
    if (other$626 == -1) {
      return 0;
    } else {
      int32_t _Some$629 = other$626;
      int32_t _y$630 = _Some$629;
      return $$moonbitlang$core$builtin$Eq$$Unit$$equal(_x$628, _y$630);
    }
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$2(
  moonbit_string_t self$619,
  moonbit_string_t other$620
) {
  if (self$619 == 0) {
    int32_t _tmp$2919;
    if (self$619) {
      moonbit_decref(self$619);
    }
    _tmp$2919 = other$620 == 0;
    if (other$620) {
      moonbit_decref(other$620);
    }
    return _tmp$2919;
  } else {
    moonbit_string_t _Some$621 = self$619;
    moonbit_string_t _x$622 = _Some$621;
    if (other$620 == 0) {
      moonbit_decref(_x$622);
      if (other$620) {
        moonbit_decref(other$620);
      }
      return 0;
    } else {
      moonbit_string_t _Some$623 = other$620;
      moonbit_string_t _y$624 = _Some$623;
      int32_t _tmp$2920 = moonbit_val_array_equal(_x$622, _y$624);
      moonbit_decref(_x$622);
      moonbit_decref(_y$624);
      return _tmp$2920;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$1(
  void* self$613,
  void* other$614
) {
  switch (Moonbit_object_tag(self$613)) {
    case 0: {
      switch (Moonbit_object_tag(other$614)) {
        case 0: {
          return 1;
          break;
        }
        default: {
          moonbit_decref(other$614);
          return 0;
          break;
        }
      }
      break;
    }
    default: {
      struct $Option$3c$Option$3c$Int$3e$$3e$$Some* _Some$615 =
        (struct $Option$3c$Option$3c$Int$3e$$3e$$Some*)self$613;
      int64_t _field$2922 = _Some$615->$0;
      int64_t _x$616;
      moonbit_decref(_Some$615);
      _x$616 = _field$2922;
      switch (Moonbit_object_tag(other$614)) {
        case 1: {
          struct $Option$3c$Option$3c$Int$3e$$3e$$Some* _Some$617 =
            (struct $Option$3c$Option$3c$Int$3e$$3e$$Some*)other$614;
          int64_t _field$2921 = _Some$617->$0;
          int64_t _y$618;
          moonbit_decref(_Some$617);
          _y$618 = _field$2921;
          return $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
                   _x$616, _y$618
                 );
          break;
        }
        default: {
          moonbit_decref(other$614);
          return 0;
          break;
        }
      }
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  int64_t self$607,
  int64_t other$608
) {
  if (self$607 == 4294967296ll) {
    return other$608 == 4294967296ll;
  } else {
    int64_t _Some$609 = self$607;
    int32_t _x$610 = (int32_t)_Some$609;
    if (other$608 == 4294967296ll) {
      return 0;
    } else {
      int64_t _Some$611 = other$608;
      int32_t _y$612 = (int32_t)_Some$611;
      return _x$610 == _y$612;
    }
  }
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$606
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1859 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$606);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$1859);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$605
) {
  moonbit_string_t* _field$2924 = self$605->$0;
  moonbit_string_t* buf$1857 = _field$2924;
  int32_t _field$2923 = self$605->$1;
  int32_t _cnt$3110 = Moonbit_object_header(self$605)->rc;
  int32_t len$1858;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$1856;
  if (_cnt$3110 > 1) {
    int32_t _new_cnt$3111;
    moonbit_incref(buf$1857);
    _new_cnt$3111 = _cnt$3110 - 1;
    Moonbit_object_header(self$605)->rc = _new_cnt$3111;
  } else if (_cnt$3110 == 1) {
    moonbit_free(self$605);
  }
  len$1858 = _field$2923;
  _tmp$1856
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$1858, buf$1857
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$1856);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$603
) {
  struct $Ref$3c$Int$3e$* i$602 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$3338;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1845;
  Moonbit_object_header(i$602)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$602->$0 = 0;
  _closure$3338
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$3338)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$3338->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$3338->$0_0 = self$603.$0;
  _closure$3338->$0_1 = self$603.$1;
  _closure$3338->$0_2 = self$603.$2;
  _closure$3338->$1 = i$602;
  _tmp$1845 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$3338;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$1845);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1846
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$1847 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$1846;
  struct $Ref$3c$Int$3e$* _field$2929 = _casted_env$1847->$1;
  struct $Ref$3c$Int$3e$* i$602 = _field$2929;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$2928 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$1847->$0_1, _casted_env$1847->$0_2, _casted_env$1847->$0_0
    };
  int32_t _cnt$3112 = Moonbit_object_header(_casted_env$1847)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$603;
  int32_t val$1848;
  int32_t _tmp$1849;
  if (_cnt$3112 > 1) {
    int32_t _new_cnt$3113;
    moonbit_incref(i$602);
    moonbit_incref(_field$2928.$0);
    _new_cnt$3113 = _cnt$3112 - 1;
    Moonbit_object_header(_casted_env$1847)->rc = _new_cnt$3113;
  } else if (_cnt$3112 == 1) {
    moonbit_free(_casted_env$1847);
  }
  self$603 = _field$2928;
  val$1848 = i$602->$0;
  moonbit_incref(self$603.$0);
  _tmp$1849 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$603);
  if (val$1848 < _tmp$1849) {
    moonbit_string_t* _field$2927 = self$603.$0;
    moonbit_string_t* buf$1852 = _field$2927;
    int32_t _field$2926 = self$603.$1;
    int32_t start$1854 = _field$2926;
    int32_t val$1855 = i$602->$0;
    int32_t _tmp$1853 = start$1854 + val$1855;
    moonbit_string_t _tmp$2925 = (moonbit_string_t)buf$1852[_tmp$1853];
    moonbit_string_t elem$604;
    int32_t val$1851;
    int32_t _tmp$1850;
    moonbit_incref(_tmp$2925);
    moonbit_decref(buf$1852);
    elem$604 = _tmp$2925;
    val$1851 = i$602->$0;
    _tmp$1850 = val$1851 + 1;
    i$602->$0 = _tmp$1850;
    moonbit_decref(i$602);
    return elem$604;
  } else {
    moonbit_decref(self$603.$0);
    moonbit_decref(i$602);
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$3(
  void* self$598,
  struct $$moonbitlang$core$builtin$Logger logger$599
) {
  switch (Moonbit_object_tag(self$598)) {
    case 0: {
      logger$599.$0->$method_0(
        logger$599.$1, (moonbit_string_t)moonbit_string_literal_104.data
      );
      break;
    }
    default: {
      struct $Option$3c$Option$3c$Int$3e$$3e$$Some* _Some$600 =
        (struct $Option$3c$Option$3c$Int$3e$$3e$$Some*)self$598;
      int64_t _field$2930 = _Some$600->$0;
      int64_t _arg$601;
      struct $$moonbitlang$core$builtin$Logger _bind$1844;
      moonbit_decref(_Some$600);
      _arg$601 = _field$2930;
      if (logger$599.$1) {
        moonbit_incref(logger$599.$1);
      }
      logger$599.$0->$method_0(
        logger$599.$1, (moonbit_string_t)moonbit_string_literal_105.data
      );
      if (logger$599.$1) {
        moonbit_incref(logger$599.$1);
      }
      $$moonbitlang$core$builtin$Logger$$write_object$3(logger$599, _arg$601);
      _bind$1844 = logger$599;
      _bind$1844.$0->$method_0(
        _bind$1844.$1, (moonbit_string_t)moonbit_string_literal_106.data
      );
      break;
    }
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$2(
  int32_t self$594,
  struct $$moonbitlang$core$builtin$Logger logger$595
) {
  if (self$594 == -1) {
    logger$595.$0->$method_0(
      logger$595.$1, (moonbit_string_t)moonbit_string_literal_104.data
    );
  } else {
    int32_t _Some$596 = self$594;
    int32_t _arg$597 = _Some$596;
    struct $$moonbitlang$core$builtin$Logger _bind$1843;
    if (logger$595.$1) {
      moonbit_incref(logger$595.$1);
    }
    logger$595.$0->$method_0(
      logger$595.$1, (moonbit_string_t)moonbit_string_literal_105.data
    );
    if (logger$595.$1) {
      moonbit_incref(logger$595.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$2(logger$595, _arg$597);
    _bind$1843 = logger$595;
    _bind$1843.$0->$method_0(
      _bind$1843.$1, (moonbit_string_t)moonbit_string_literal_106.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$1(
  moonbit_string_t self$590,
  struct $$moonbitlang$core$builtin$Logger logger$591
) {
  if (self$590 == 0) {
    if (self$590) {
      moonbit_decref(self$590);
    }
    logger$591.$0->$method_0(
      logger$591.$1, (moonbit_string_t)moonbit_string_literal_104.data
    );
  } else {
    moonbit_string_t _Some$592 = self$590;
    moonbit_string_t _arg$593 = _Some$592;
    struct $$moonbitlang$core$builtin$Logger _bind$1842;
    if (logger$591.$1) {
      moonbit_incref(logger$591.$1);
    }
    logger$591.$0->$method_0(
      logger$591.$1, (moonbit_string_t)moonbit_string_literal_105.data
    );
    if (logger$591.$1) {
      moonbit_incref(logger$591.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$1(logger$591, _arg$593);
    _bind$1842 = logger$591;
    _bind$1842.$0->$method_0(
      _bind$1842.$1, (moonbit_string_t)moonbit_string_literal_106.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  int64_t self$586,
  struct $$moonbitlang$core$builtin$Logger logger$587
) {
  if (self$586 == 4294967296ll) {
    logger$587.$0->$method_0(
      logger$587.$1, (moonbit_string_t)moonbit_string_literal_104.data
    );
  } else {
    int64_t _Some$588 = self$586;
    int32_t _arg$589 = (int32_t)_Some$588;
    struct $$moonbitlang$core$builtin$Logger _bind$1841;
    if (logger$587.$1) {
      moonbit_incref(logger$587.$1);
    }
    logger$587.$0->$method_0(
      logger$587.$1, (moonbit_string_t)moonbit_string_literal_105.data
    );
    if (logger$587.$1) {
      moonbit_incref(logger$587.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$0(logger$587, _arg$589);
    _bind$1841 = logger$587;
    _bind$1841.$0->$method_0(
      _bind$1841.$1, (moonbit_string_t)moonbit_string_literal_106.data
    );
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$585
) {
  return self$585;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$584,
  struct $$moonbitlang$core$builtin$Logger logger$583
) {
  moonbit_string_t _tmp$1840 = $Int$$to_string$inner(self$584, 10);
  logger$583.$0->$method_0(logger$583.$1, _tmp$1840);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$581,
  struct $$moonbitlang$core$builtin$Logger logger$582
) {
  if (self$581) {
    logger$582.$0->$method_0(
      logger$582.$1, (moonbit_string_t)moonbit_string_literal_107.data
    );
  } else {
    logger$582.$0->$method_0(
      logger$582.$1, (moonbit_string_t)moonbit_string_literal_108.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Unit$$output(
  int32_t _self$580,
  struct $$moonbitlang$core$builtin$Logger logger$579
) {
  logger$579.$0->$method_0(
    logger$579.$1, (moonbit_string_t)moonbit_string_literal_109.data
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Compare$$String$$compare(
  moonbit_string_t self$573,
  moonbit_string_t other$575
) {
  int32_t len$572 = Moonbit_array_length(self$573);
  int32_t _tmp$1839 = Moonbit_array_length(other$575);
  int32_t _bind$574 = (len$572 >= _tmp$1839) - (len$572 <= _tmp$1839);
  switch (_bind$574) {
    case 0: {
      int32_t i$576 = 0;
      while (1) {
        if (i$576 < len$572) {
          int32_t _tmp$1836 = self$573[i$576];
          int32_t _tmp$1837 = other$575[i$576];
          int32_t order$577 =
            (_tmp$1836 >= _tmp$1837) - (_tmp$1836 <= _tmp$1837);
          int32_t _tmp$1838;
          if (order$577 != 0) {
            moonbit_decref(other$575);
            moonbit_decref(self$573);
            return order$577;
          }
          _tmp$1838 = i$576 + 1;
          i$576 = _tmp$1838;
          continue;
        } else {
          moonbit_decref(other$575);
          moonbit_decref(self$573);
        }
        break;
      }
      return 0;
      break;
    }
    default: {
      moonbit_decref(other$575);
      moonbit_decref(self$573);
      return _bind$574;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$570,
  struct $$3c$String$3e$$3d$$3e$Int* f$571
) {
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _func$569 = self$570;
  return _func$569->code(_func$569, f$571);
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$566,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$568
) {
  int32_t len$1831 = self$566->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1833;
  int32_t _tmp$2933;
  int32_t _tmp$1832;
  int32_t length$567;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2932;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$1834;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2931;
  int32_t _tmp$1835;
  moonbit_incref(self$566);
  _tmp$1833 = $$moonbitlang$core$builtin$Array$$buffer$2(self$566);
  _tmp$2933 = Moonbit_array_length(_tmp$1833);
  moonbit_decref(_tmp$1833);
  _tmp$1832 = _tmp$2933;
  if (len$1831 == _tmp$1832) {
    moonbit_incref(self$566);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$566);
  }
  length$567 = self$566->$1;
  _field$2932 = self$566->$0;
  buf$1834 = _field$2932;
  _old$2931
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$1834[
      length$567
    ];
  if (_old$2931) {
    moonbit_decref(_old$2931);
  }
  buf$1834[length$567] = value$568;
  _tmp$1835 = length$567 + 1;
  self$566->$1 = _tmp$1835;
  moonbit_decref(self$566);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$563,
  struct $$3c$String$2a$Int$3e$* value$565
) {
  int32_t len$1826 = self$563->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$1828;
  int32_t _tmp$2936;
  int32_t _tmp$1827;
  int32_t length$564;
  struct $$3c$String$2a$Int$3e$** _field$2935;
  struct $$3c$String$2a$Int$3e$** buf$1829;
  struct $$3c$String$2a$Int$3e$* _old$2934;
  int32_t _tmp$1830;
  moonbit_incref(self$563);
  _tmp$1828 = $$moonbitlang$core$builtin$Array$$buffer$0(self$563);
  _tmp$2936 = Moonbit_array_length(_tmp$1828);
  moonbit_decref(_tmp$1828);
  _tmp$1827 = _tmp$2936;
  if (len$1826 == _tmp$1827) {
    moonbit_incref(self$563);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$563);
  }
  length$564 = self$563->$1;
  _field$2935 = self$563->$0;
  buf$1829 = _field$2935;
  _old$2934 = (struct $$3c$String$2a$Int$3e$*)buf$1829[length$564];
  if (_old$2934) {
    moonbit_decref(_old$2934);
  }
  buf$1829[length$564] = value$565;
  _tmp$1830 = length$564 + 1;
  self$563->$1 = _tmp$1830;
  moonbit_decref(self$563);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$560,
  moonbit_string_t value$562
) {
  int32_t len$1821 = self$560->$1;
  moonbit_string_t* _tmp$1823;
  int32_t _tmp$2939;
  int32_t _tmp$1822;
  int32_t length$561;
  moonbit_string_t* _field$2938;
  moonbit_string_t* buf$1824;
  moonbit_string_t _old$2937;
  int32_t _tmp$1825;
  moonbit_incref(self$560);
  _tmp$1823 = $$moonbitlang$core$builtin$Array$$buffer$1(self$560);
  _tmp$2939 = Moonbit_array_length(_tmp$1823);
  moonbit_decref(_tmp$1823);
  _tmp$1822 = _tmp$2939;
  if (len$1821 == _tmp$1822) {
    moonbit_incref(self$560);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$560);
  }
  length$561 = self$560->$1;
  _field$2938 = self$560->$0;
  buf$1824 = _field$2938;
  _old$2937 = (moonbit_string_t)buf$1824[length$561];
  moonbit_decref(_old$2937);
  buf$1824[length$561] = value$562;
  _tmp$1825 = length$561 + 1;
  self$560->$1 = _tmp$1825;
  moonbit_decref(self$560);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$558
) {
  int32_t old_cap$557 = self$558->$1;
  int32_t new_cap$559;
  if (old_cap$557 == 0) {
    new_cap$559 = 8;
  } else {
    new_cap$559 = old_cap$557 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$558, new_cap$559);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$555
) {
  int32_t old_cap$554 = self$555->$1;
  int32_t new_cap$556;
  if (old_cap$554 == 0) {
    new_cap$556 = 8;
  } else {
    new_cap$556 = old_cap$554 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$555, new_cap$556);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$552
) {
  int32_t old_cap$551 = self$552->$1;
  int32_t new_cap$553;
  if (old_cap$551 == 0) {
    new_cap$553 = 8;
  } else {
    new_cap$553 = old_cap$551 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$552, new_cap$553);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$548,
  int32_t new_capacity$546
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** new_buf$545 =
    (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_make_ref_array(
      new_capacity$546, 0
    );
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2941 =
    self$548->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$547 =
    _field$2941;
  int32_t old_cap$549 = Moonbit_array_length(old_buf$547);
  int32_t copy_len$550;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$2940;
  if (old_cap$549 < new_capacity$546) {
    copy_len$550 = old_cap$549;
  } else {
    copy_len$550 = new_capacity$546;
  }
  moonbit_incref(old_buf$547);
  moonbit_incref(new_buf$545);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$545, 0, old_buf$547, 0, copy_len$550
  );
  _old$2940 = self$548->$0;
  moonbit_decref(_old$2940);
  self$548->$0 = new_buf$545;
  moonbit_decref(self$548);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$542,
  int32_t new_capacity$540
) {
  struct $$3c$String$2a$Int$3e$** new_buf$539 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_make_ref_array(
      new_capacity$540, 0
    );
  struct $$3c$String$2a$Int$3e$** _field$2943 = self$542->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$541 = _field$2943;
  int32_t old_cap$543 = Moonbit_array_length(old_buf$541);
  int32_t copy_len$544;
  struct $$3c$String$2a$Int$3e$** _old$2942;
  if (old_cap$543 < new_capacity$540) {
    copy_len$544 = old_cap$543;
  } else {
    copy_len$544 = new_capacity$540;
  }
  moonbit_incref(old_buf$541);
  moonbit_incref(new_buf$539);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$539, 0, old_buf$541, 0, copy_len$544
  );
  _old$2942 = self$542->$0;
  moonbit_decref(_old$2942);
  self$542->$0 = new_buf$539;
  moonbit_decref(self$542);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$536,
  int32_t new_capacity$534
) {
  moonbit_string_t* new_buf$533 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$534, (moonbit_string_t)moonbit_string_literal_3.data
    );
  moonbit_string_t* _field$2945 = self$536->$0;
  moonbit_string_t* old_buf$535 = _field$2945;
  int32_t old_cap$537 = Moonbit_array_length(old_buf$535);
  int32_t copy_len$538;
  moonbit_string_t* _old$2944;
  if (old_cap$537 < new_capacity$534) {
    copy_len$538 = old_cap$537;
  } else {
    copy_len$538 = new_capacity$534;
  }
  moonbit_incref(old_buf$535);
  moonbit_incref(new_buf$533);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$533, 0, old_buf$535, 0, copy_len$538
  );
  _old$2944 = self$536->$0;
  moonbit_decref(_old$2944);
  self$536->$0 = new_buf$533;
  moonbit_decref(self$536);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$532
) {
  if (capacity$532 == 0) {
    moonbit_string_t* _tmp$1819 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$3340 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$3340)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$3340->$0 = _tmp$1819;
    _block$3340->$1 = 0;
    return _block$3340;
  } else {
    moonbit_string_t* _tmp$1820 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$532, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$3341 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$3341)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$3341->$0 = _tmp$1820;
    _block$3341->$1 = 0;
    return _block$3341;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$530,
  struct $StringView str$531
) {
  int32_t len$1807 = self$530->$1;
  int32_t _tmp$1809;
  int32_t _tmp$1808;
  int32_t _tmp$1806;
  moonbit_bytes_t _field$2946;
  moonbit_bytes_t data$1810;
  int32_t len$1811;
  moonbit_string_t _tmp$1812;
  int32_t _tmp$1813;
  int32_t _tmp$1814;
  int32_t len$1816;
  int32_t _tmp$1818;
  int32_t _tmp$1817;
  int32_t _tmp$1815;
  moonbit_incref(str$531.$0);
  _tmp$1809 = $StringView$$length(str$531);
  _tmp$1808 = _tmp$1809 * 2;
  _tmp$1806 = len$1807 + _tmp$1808;
  moonbit_incref(self$530);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$530, _tmp$1806
  );
  _field$2946 = self$530->$0;
  data$1810 = _field$2946;
  len$1811 = self$530->$1;
  moonbit_incref(data$1810);
  moonbit_incref(str$531.$0);
  _tmp$1812 = $StringView$$data(str$531);
  moonbit_incref(str$531.$0);
  _tmp$1813 = $StringView$$start_offset(str$531);
  moonbit_incref(str$531.$0);
  _tmp$1814 = $StringView$$length(str$531);
  $FixedArray$$blit_from_string(
    data$1810, len$1811, _tmp$1812, _tmp$1813, _tmp$1814
  );
  len$1816 = self$530->$1;
  _tmp$1818 = $StringView$$length(str$531);
  _tmp$1817 = _tmp$1818 * 2;
  _tmp$1815 = len$1816 + _tmp$1817;
  self$530->$1 = _tmp$1815;
  moonbit_decref(self$530);
  return 0;
}

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$527,
  int32_t i$528,
  int32_t start_offset$529,
  int64_t end_offset$525
) {
  int32_t end_offset$524;
  if (end_offset$525 == 4294967296ll) {
    end_offset$524 = Moonbit_array_length(self$527);
  } else {
    int64_t _Some$526 = end_offset$525;
    end_offset$524 = (int32_t)_Some$526;
  }
  if (i$528 >= 0) {
    return $String$$offset_of_nth_char_forward(
             self$527, i$528, start_offset$529, end_offset$524
           );
  } else {
    int32_t _tmp$1805 = -i$528;
    return $String$$offset_of_nth_char_backward(
             self$527, _tmp$1805, start_offset$529, end_offset$524
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$522,
  int32_t n$520,
  int32_t start_offset$516,
  int32_t end_offset$517
) {
  int32_t _if_result$3342;
  if (start_offset$516 >= 0) {
    _if_result$3342 = start_offset$516 <= end_offset$517;
  } else {
    _if_result$3342 = 0;
  }
  if (_if_result$3342) {
    int32_t utf16_offset$518 = start_offset$516;
    int32_t char_count$519 = 0;
    int32_t _tmp$1803;
    int32_t _if_result$3345;
    while (1) {
      int32_t _tmp$1797 = utf16_offset$518;
      int32_t _if_result$3344;
      if (_tmp$1797 < end_offset$517) {
        int32_t _tmp$1796 = char_count$519;
        _if_result$3344 = _tmp$1796 < n$520;
      } else {
        _if_result$3344 = 0;
      }
      if (_if_result$3344) {
        int32_t _tmp$1801 = utf16_offset$518;
        int32_t c$521 = self$522[_tmp$1801];
        int32_t _tmp$1800;
        if ($Int$$is_leading_surrogate(c$521)) {
          int32_t _tmp$1798 = utf16_offset$518;
          utf16_offset$518 = _tmp$1798 + 2;
        } else {
          int32_t _tmp$1799 = utf16_offset$518;
          utf16_offset$518 = _tmp$1799 + 1;
        }
        _tmp$1800 = char_count$519;
        char_count$519 = _tmp$1800 + 1;
        continue;
      } else {
        moonbit_decref(self$522);
      }
      break;
    }
    _tmp$1803 = char_count$519;
    if (_tmp$1803 < n$520) {
      _if_result$3345 = 1;
    } else {
      int32_t _tmp$1802 = utf16_offset$518;
      _if_result$3345 = _tmp$1802 >= end_offset$517;
    }
    if (_if_result$3345) {
      return 4294967296ll;
    } else {
      int32_t _tmp$1804 = utf16_offset$518;
      return (int64_t)_tmp$1804;
    }
  } else {
    moonbit_decref(self$522);
    return $moonbitlang$core$builtin$abort$3(
             (moonbit_string_t)moonbit_string_literal_110.data,
               (moonbit_string_t)moonbit_string_literal_111.data
           );
  }
}

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$514,
  int32_t n$512,
  int32_t start_offset$511,
  int32_t end_offset$510
) {
  int32_t char_count$508 = 0;
  int32_t utf16_offset$509 = end_offset$510;
  int32_t _tmp$1794;
  int32_t _if_result$3348;
  while (1) {
    int32_t _tmp$1787 = utf16_offset$509;
    int32_t _tmp$1786 = _tmp$1787 - 1;
    int32_t _if_result$3347;
    if (_tmp$1786 >= start_offset$511) {
      int32_t _tmp$1785 = char_count$508;
      _if_result$3347 = _tmp$1785 < n$512;
    } else {
      _if_result$3347 = 0;
    }
    if (_if_result$3347) {
      int32_t _tmp$1792 = utf16_offset$509;
      int32_t _tmp$1791 = _tmp$1792 - 1;
      int32_t c$513 = self$514[_tmp$1791];
      int32_t _tmp$1790;
      if ($Int$$is_trailing_surrogate(c$513)) {
        int32_t _tmp$1788 = utf16_offset$509;
        utf16_offset$509 = _tmp$1788 - 2;
      } else {
        int32_t _tmp$1789 = utf16_offset$509;
        utf16_offset$509 = _tmp$1789 - 1;
      }
      _tmp$1790 = char_count$508;
      char_count$508 = _tmp$1790 + 1;
      continue;
    } else {
      moonbit_decref(self$514);
    }
    break;
  }
  _tmp$1794 = char_count$508;
  if (_tmp$1794 < n$512) {
    _if_result$3348 = 1;
  } else {
    int32_t _tmp$1793 = utf16_offset$509;
    _if_result$3348 = _tmp$1793 < start_offset$511;
  }
  if (_if_result$3348) {
    return 4294967296ll;
  } else {
    int32_t _tmp$1795 = utf16_offset$509;
    return (int64_t)_tmp$1795;
  }
}

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$500,
  int32_t len$503,
  int32_t start_offset$507,
  int64_t end_offset$498
) {
  int32_t end_offset$497;
  int32_t index$501;
  int32_t count$502;
  if (end_offset$498 == 4294967296ll) {
    end_offset$497 = Moonbit_array_length(self$500);
  } else {
    int64_t _Some$499 = end_offset$498;
    end_offset$497 = (int32_t)_Some$499;
  }
  index$501 = start_offset$507;
  count$502 = 0;
  while (1) {
    int32_t _if_result$3350;
    if (index$501 < end_offset$497) {
      _if_result$3350 = count$502 < len$503;
    } else {
      _if_result$3350 = 0;
    }
    if (_if_result$3350) {
      int32_t c1$504 = self$500[index$501];
      int32_t _if_result$3351;
      int32_t _tmp$1783;
      int32_t _tmp$1784;
      if ($Int$$is_leading_surrogate(c1$504)) {
        int32_t _tmp$1779 = index$501 + 1;
        _if_result$3351 = _tmp$1779 < end_offset$497;
      } else {
        _if_result$3351 = 0;
      }
      if (_if_result$3351) {
        int32_t _tmp$1782 = index$501 + 1;
        int32_t c2$505 = self$500[_tmp$1782];
        if ($Int$$is_trailing_surrogate(c2$505)) {
          int32_t _tmp$1780 = index$501 + 2;
          int32_t _tmp$1781 = count$502 + 1;
          index$501 = _tmp$1780;
          count$502 = _tmp$1781;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_112.data,
              (moonbit_string_t)moonbit_string_literal_113.data
          );
        }
      }
      _tmp$1783 = index$501 + 1;
      _tmp$1784 = count$502 + 1;
      index$501 = _tmp$1783;
      count$502 = _tmp$1784;
      continue;
    } else {
      moonbit_decref(self$500);
      return count$502 >= len$503;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$496
) {
  int32_t end$1777 = self$496.$2;
  int32_t _field$2947 = self$496.$1;
  int32_t start$1778;
  moonbit_decref(self$496.$0);
  start$1778 = _field$2947;
  return end$1777 - start$1778;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$495
) {
  int32_t end$1775 = self$495.$2;
  int32_t _field$2948 = self$495.$1;
  int32_t start$1776;
  moonbit_decref(self$495.$0);
  start$1776 = _field$2948;
  return end$1775 - start$1776;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$494
) {
  int32_t end$1773 = self$494.$2;
  int32_t _field$2949 = self$494.$1;
  int32_t start$1774;
  moonbit_decref(self$494.$0);
  start$1774 = _field$2949;
  return end$1773 - start$1774;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$493
) {
  int32_t end$1771 = self$493.$2;
  int32_t _field$2950 = self$493.$1;
  int32_t start$1772;
  moonbit_decref(self$493.$0);
  start$1772 = _field$2950;
  return end$1771 - start$1772;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$492
) {
  int32_t end$1769 = self$492.$2;
  int32_t _field$2951 = self$492.$1;
  int32_t start$1770;
  moonbit_decref(self$492.$0);
  start$1770 = _field$2951;
  return end$1769 - start$1770;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$487
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$3352 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$3352)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$3352->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$3352->$0 = self$487;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$3352;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1767,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$485
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$1768 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$1767;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$2952 =
    _casted_env$1768->$0;
  int32_t _cnt$3114 = Moonbit_object_header(_casted_env$1768)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$487;
  if (_cnt$3114 > 1) {
    int32_t _new_cnt$3115;
    moonbit_incref(_field$2952);
    _new_cnt$3115 = _cnt$3114 - 1;
    Moonbit_object_header(_casted_env$1768)->rc = _new_cnt$3115;
  } else if (_cnt$3114 == 1) {
    moonbit_free(_casted_env$1768);
  }
  self$487 = _field$2952;
  while (1) {
    moonbit_string_t _bind$486;
    moonbit_incref(self$487);
    _bind$486 = $$moonbitlang$core$builtin$Iterator$$next$0(self$487);
    if (_bind$486 == 0) {
      moonbit_decref(self$487);
      if (_bind$486) {
        moonbit_decref(_bind$486);
      }
      moonbit_decref(yield_$485);
      return 1;
    } else {
      moonbit_string_t _Some$488 = _bind$486;
      moonbit_string_t _x$489 = _Some$488;
      int32_t _bind$490;
      moonbit_incref(yield_$485);
      _bind$490 = yield_$485->code(yield_$485, _x$489);
      switch (_bind$490) {
        case 1:
          break;
        default: {
          moonbit_decref(self$487);
          moonbit_decref(yield_$485);
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$484
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _func$483 = self$484;
  return _func$483->code(_func$483);
}

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$475,
  struct $$moonbitlang$core$builtin$Logger logger$473
) {
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$474;
  int32_t len$476;
  int32_t i$477;
  int32_t seg$478;
  if (logger$473.$1) {
    moonbit_incref(logger$473.$1);
  }
  logger$473.$0->$method_3(logger$473.$1, 34);
  if (logger$473.$1) {
    moonbit_incref(logger$473.$1);
  }
  moonbit_incref(self$475);
  _env$474
  = (struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$)
    );
  Moonbit_object_header(_env$474)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$, $0_0
    )
    >> 2,
      3,
      0
  );
  _env$474->$0_0 = logger$473.$0;
  _env$474->$0_1 = logger$473.$1;
  _env$474->$1 = self$475;
  len$476 = Moonbit_array_length(self$475);
  i$477 = 0;
  seg$478 = 0;
  $$2a$for$479:;
  while (1) {
    int32_t code$480;
    int32_t c$482;
    struct $$moonbitlang$core$builtin$Logger _bind$1749;
    int32_t _tmp$1750;
    int32_t _tmp$1751;
    int32_t _tmp$1752;
    int32_t _tmp$3357;
    int32_t _tmp$3358;
    if (i$477 >= len$476) {
      moonbit_decref(self$475);
      $moonbitlang$core$builtin$output$flush_segment$7c$3831(
        _env$474, seg$478, i$477
      );
      break;
    }
    code$480 = self$475[i$477];
    switch (code$480) {
      case 34: {
        c$482 = code$480;
        goto $join$481;
        break;
      }
      
      case 92: {
        c$482 = code$480;
        goto $join$481;
        break;
      }
      
      case 10: {
        int32_t _tmp$1753;
        int32_t _tmp$1754;
        moonbit_incref(_env$474);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$474, seg$478, i$477
        );
        if (logger$473.$1) {
          moonbit_incref(logger$473.$1);
        }
        logger$473.$0->$method_0(
          logger$473.$1, (moonbit_string_t)moonbit_string_literal_96.data
        );
        _tmp$1753 = i$477 + 1;
        _tmp$1754 = i$477 + 1;
        i$477 = _tmp$1753;
        seg$478 = _tmp$1754;
        goto $$2a$for$479;
        break;
      }
      
      case 13: {
        int32_t _tmp$1755;
        int32_t _tmp$1756;
        moonbit_incref(_env$474);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$474, seg$478, i$477
        );
        if (logger$473.$1) {
          moonbit_incref(logger$473.$1);
        }
        logger$473.$0->$method_0(
          logger$473.$1, (moonbit_string_t)moonbit_string_literal_97.data
        );
        _tmp$1755 = i$477 + 1;
        _tmp$1756 = i$477 + 1;
        i$477 = _tmp$1755;
        seg$478 = _tmp$1756;
        goto $$2a$for$479;
        break;
      }
      
      case 8: {
        int32_t _tmp$1757;
        int32_t _tmp$1758;
        moonbit_incref(_env$474);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$474, seg$478, i$477
        );
        if (logger$473.$1) {
          moonbit_incref(logger$473.$1);
        }
        logger$473.$0->$method_0(
          logger$473.$1, (moonbit_string_t)moonbit_string_literal_98.data
        );
        _tmp$1757 = i$477 + 1;
        _tmp$1758 = i$477 + 1;
        i$477 = _tmp$1757;
        seg$478 = _tmp$1758;
        goto $$2a$for$479;
        break;
      }
      
      case 9: {
        int32_t _tmp$1759;
        int32_t _tmp$1760;
        moonbit_incref(_env$474);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$474, seg$478, i$477
        );
        if (logger$473.$1) {
          moonbit_incref(logger$473.$1);
        }
        logger$473.$0->$method_0(
          logger$473.$1, (moonbit_string_t)moonbit_string_literal_99.data
        );
        _tmp$1759 = i$477 + 1;
        _tmp$1760 = i$477 + 1;
        i$477 = _tmp$1759;
        seg$478 = _tmp$1760;
        goto $$2a$for$479;
        break;
      }
      default: {
        if (code$480 < 32) {
          int32_t _tmp$1763;
          moonbit_string_t _tmp$1762;
          struct $$moonbitlang$core$builtin$Logger _bind$1761;
          int32_t _tmp$1764;
          int32_t _tmp$1765;
          moonbit_incref(_env$474);
          $moonbitlang$core$builtin$output$flush_segment$7c$3831(
            _env$474, seg$478, i$477
          );
          if (logger$473.$1) {
            moonbit_incref(logger$473.$1);
          }
          logger$473.$0->$method_0(
            logger$473.$1, (moonbit_string_t)moonbit_string_literal_100.data
          );
          _tmp$1763 = code$480 & 0xff;
          _tmp$1762 = $Byte$$to_hex(_tmp$1763);
          if (logger$473.$1) {
            moonbit_incref(logger$473.$1);
          }
          logger$473.$0->$method_0(logger$473.$1, _tmp$1762);
          _bind$1761 = logger$473;
          if (_bind$1761.$1) {
            moonbit_incref(_bind$1761.$1);
          }
          _bind$1761.$0->$method_3(_bind$1761.$1, 125);
          _tmp$1764 = i$477 + 1;
          _tmp$1765 = i$477 + 1;
          i$477 = _tmp$1764;
          seg$478 = _tmp$1765;
          goto $$2a$for$479;
        } else {
          int32_t _tmp$1766 = i$477 + 1;
          int32_t _tmp$3356 = seg$478;
          i$477 = _tmp$1766;
          seg$478 = _tmp$3356;
          goto $$2a$for$479;
        }
        break;
      }
    }
    goto $joinlet$3355;
    $join$481:;
    moonbit_incref(_env$474);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$474, seg$478, i$477
    );
    if (logger$473.$1) {
      moonbit_incref(logger$473.$1);
    }
    logger$473.$0->$method_3(logger$473.$1, 92);
    _bind$1749 = logger$473;
    _tmp$1750 = c$482;
    if (_bind$1749.$1) {
      moonbit_incref(_bind$1749.$1);
    }
    _bind$1749.$0->$method_3(_bind$1749.$1, _tmp$1750);
    _tmp$1751 = i$477 + 1;
    _tmp$1752 = i$477 + 1;
    i$477 = _tmp$1751;
    seg$478 = _tmp$1752;
    continue;
    $joinlet$3355:;
    _tmp$3357 = i$477;
    _tmp$3358 = seg$478;
    i$477 = _tmp$3357;
    seg$478 = _tmp$3358;
    continue;
    break;
  }
  logger$473.$0->$method_3(logger$473.$1, 34);
  return 0;
}

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$469,
  int32_t seg$472,
  int32_t i$471
) {
  moonbit_string_t _field$2954 = _env$469->$1;
  moonbit_string_t self$468 = _field$2954;
  struct $$moonbitlang$core$builtin$Logger _field$2953 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$469->$0_0, _env$469->$0_1
    };
  int32_t _cnt$3116 = Moonbit_object_header(_env$469)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$470;
  if (_cnt$3116 > 1) {
    int32_t _new_cnt$3117;
    moonbit_incref(self$468);
    if (_field$2953.$1) {
      moonbit_incref(_field$2953.$1);
    }
    _new_cnt$3117 = _cnt$3116 - 1;
    Moonbit_object_header(_env$469)->rc = _new_cnt$3117;
  } else if (_cnt$3116 == 1) {
    moonbit_free(_env$469);
  }
  logger$470 = _field$2953;
  if (i$471 > seg$472) {
    int32_t _tmp$1748 = i$471 - seg$472;
    logger$470.$0->$method_1(logger$470.$1, self$468, seg$472, _tmp$1748);
  } else {
    if (logger$470.$1) {
      moonbit_decref(logger$470.$1);
    }
    moonbit_decref(self$468);
  }
  return 0;
}

moonbit_string_t $Byte$$to_hex(int32_t b$467) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$466 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _tmp$1745 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$467, 16);
  int32_t _tmp$1744 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$1745);
  int32_t _tmp$1747;
  int32_t _tmp$1746;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$1743;
  moonbit_incref(_self$466);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$466, _tmp$1744
  );
  _tmp$1747 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$467, 16);
  _tmp$1746
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$1747
  );
  moonbit_incref(_self$466);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$466, _tmp$1746
  );
  _tmp$1743 = _self$466;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$1743);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$465) {
  if (i$465 < 10) {
    int32_t _tmp$1740 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$465, 48);
    return $Byte$$to_char(_tmp$1740);
  } else {
    int32_t _tmp$1742 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$465, 97);
    int32_t _tmp$1741 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$1742, 10);
    return $Byte$$to_char(_tmp$1741);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$463,
  int32_t that$464
) {
  int32_t _tmp$1738 = (int32_t)self$463;
  int32_t _tmp$1739 = (int32_t)that$464;
  int32_t _tmp$1737 = _tmp$1738 - _tmp$1739;
  return _tmp$1737 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$461,
  int32_t that$462
) {
  int32_t _tmp$1735 = (int32_t)self$461;
  int32_t _tmp$1736 = (int32_t)that$462;
  int32_t _tmp$1734 = _tmp$1735 % _tmp$1736;
  return _tmp$1734 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$459,
  int32_t that$460
) {
  int32_t _tmp$1732 = (int32_t)self$459;
  int32_t _tmp$1733 = (int32_t)that$460;
  int32_t _tmp$1731 = _tmp$1732 / _tmp$1733;
  return _tmp$1731 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$457,
  int32_t that$458
) {
  int32_t _tmp$1729 = (int32_t)self$457;
  int32_t _tmp$1730 = (int32_t)that$458;
  int32_t _tmp$1728 = _tmp$1729 + _tmp$1730;
  return _tmp$1728 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$454,
  int32_t start$452,
  int32_t end$453
) {
  int32_t _if_result$3359;
  int32_t len$455;
  int32_t _tmp$1726;
  int32_t _tmp$1727;
  moonbit_bytes_t bytes$456;
  moonbit_bytes_t _tmp$1725;
  if (start$452 == 0) {
    int32_t _tmp$1724 = Moonbit_array_length(str$454);
    _if_result$3359 = end$453 == _tmp$1724;
  } else {
    _if_result$3359 = 0;
  }
  if (_if_result$3359) {
    return str$454;
  }
  len$455 = end$453 - start$452;
  _tmp$1726 = len$455 * 2;
  _tmp$1727 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$456 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1726, _tmp$1727);
  moonbit_incref(bytes$456);
  $FixedArray$$blit_from_string(bytes$456, 0, str$454, start$452, len$455);
  _tmp$1725 = bytes$456;
  return $Bytes$$to_unchecked_string$inner(_tmp$1725, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$451
) {
  return f$451;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$6(
  void* a$445,
  void* b$446,
  moonbit_string_t msg$448,
  moonbit_string_t loc$450
) {
  moonbit_incref(b$446);
  moonbit_incref(a$445);
  if ($moonbitlang$core$builtin$op_notequal$3(a$445, b$446)) {
    moonbit_string_t fail_msg$447;
    if (msg$448 == 0) {
      moonbit_string_t _tmp$1722;
      moonbit_string_t _tmp$1721;
      moonbit_string_t _tmp$1720;
      moonbit_string_t _tmp$1717;
      moonbit_string_t _tmp$1719;
      moonbit_string_t _tmp$1718;
      moonbit_string_t _tmp$1716;
      if (msg$448) {
        moonbit_decref(msg$448);
      }
      _tmp$1722 = $moonbitlang$core$builtin$debug_string$6(a$445);
      _tmp$1721
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1722
      );
      _tmp$1720
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_101.data, _tmp$1721
      );
      _tmp$1717
      = moonbit_add_string(
        _tmp$1720, (moonbit_string_t)moonbit_string_literal_114.data
      );
      _tmp$1719 = $moonbitlang$core$builtin$debug_string$6(b$446);
      _tmp$1718
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1719
      );
      _tmp$1716 = moonbit_add_string(_tmp$1717, _tmp$1718);
      fail_msg$447
      = moonbit_add_string(
        _tmp$1716, (moonbit_string_t)moonbit_string_literal_101.data
      );
    } else {
      moonbit_string_t _Some$449;
      moonbit_decref(b$446);
      moonbit_decref(a$445);
      _Some$449 = msg$448;
      fail_msg$447 = _Some$449;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$447, loc$450);
  } else {
    int32_t _tmp$1723;
    struct moonbit_result_0 _result$3360;
    moonbit_decref(loc$450);
    if (msg$448) {
      moonbit_decref(msg$448);
    }
    moonbit_decref(b$446);
    moonbit_decref(a$445);
    _tmp$1723 = 0;
    _result$3360.tag = 1;
    _result$3360.data.ok = _tmp$1723;
    return _result$3360;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$5(
  int32_t a$439,
  int32_t b$440,
  moonbit_string_t msg$442,
  moonbit_string_t loc$444
) {
  if (a$439 != b$440) {
    moonbit_string_t fail_msg$441;
    if (msg$442 == 0) {
      moonbit_string_t _tmp$1714;
      moonbit_string_t _tmp$1713;
      moonbit_string_t _tmp$1712;
      moonbit_string_t _tmp$1709;
      moonbit_string_t _tmp$1711;
      moonbit_string_t _tmp$1710;
      moonbit_string_t _tmp$1708;
      if (msg$442) {
        moonbit_decref(msg$442);
      }
      _tmp$1714 = $moonbitlang$core$builtin$debug_string$5(a$439);
      _tmp$1713
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1714
      );
      _tmp$1712
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_101.data, _tmp$1713
      );
      _tmp$1709
      = moonbit_add_string(
        _tmp$1712, (moonbit_string_t)moonbit_string_literal_114.data
      );
      _tmp$1711 = $moonbitlang$core$builtin$debug_string$5(b$440);
      _tmp$1710
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1711
      );
      _tmp$1708 = moonbit_add_string(_tmp$1709, _tmp$1710);
      fail_msg$441
      = moonbit_add_string(
        _tmp$1708, (moonbit_string_t)moonbit_string_literal_101.data
      );
    } else {
      moonbit_string_t _Some$443 = msg$442;
      fail_msg$441 = _Some$443;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$441, loc$444);
  } else {
    int32_t _tmp$1715;
    struct moonbit_result_0 _result$3361;
    moonbit_decref(loc$444);
    if (msg$442) {
      moonbit_decref(msg$442);
    }
    _tmp$1715 = 0;
    _result$3361.tag = 1;
    _result$3361.data.ok = _tmp$1715;
    return _result$3361;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  int32_t a$433,
  int32_t b$434,
  moonbit_string_t msg$436,
  moonbit_string_t loc$438
) {
  if ($moonbitlang$core$builtin$op_notequal$5(a$433, b$434)) {
    moonbit_string_t fail_msg$435;
    if (msg$436 == 0) {
      moonbit_string_t _tmp$1706;
      moonbit_string_t _tmp$1705;
      moonbit_string_t _tmp$1704;
      moonbit_string_t _tmp$1701;
      moonbit_string_t _tmp$1703;
      moonbit_string_t _tmp$1702;
      moonbit_string_t _tmp$1700;
      if (msg$436) {
        moonbit_decref(msg$436);
      }
      _tmp$1706 = $moonbitlang$core$builtin$debug_string$4(a$433);
      _tmp$1705
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1706
      );
      _tmp$1704
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_101.data, _tmp$1705
      );
      _tmp$1701
      = moonbit_add_string(
        _tmp$1704, (moonbit_string_t)moonbit_string_literal_114.data
      );
      _tmp$1703 = $moonbitlang$core$builtin$debug_string$4(b$434);
      _tmp$1702
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1703
      );
      _tmp$1700 = moonbit_add_string(_tmp$1701, _tmp$1702);
      fail_msg$435
      = moonbit_add_string(
        _tmp$1700, (moonbit_string_t)moonbit_string_literal_101.data
      );
    } else {
      moonbit_string_t _Some$437 = msg$436;
      fail_msg$435 = _Some$437;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$435, loc$438);
  } else {
    int32_t _tmp$1707;
    struct moonbit_result_0 _result$3362;
    moonbit_decref(loc$438);
    if (msg$436) {
      moonbit_decref(msg$436);
    }
    _tmp$1707 = 0;
    _result$3362.tag = 1;
    _result$3362.data.ok = _tmp$1707;
    return _result$3362;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  moonbit_string_t a$427,
  moonbit_string_t b$428,
  moonbit_string_t msg$430,
  moonbit_string_t loc$432
) {
  if (b$428) {
    moonbit_incref(b$428);
  }
  if (a$427) {
    moonbit_incref(a$427);
  }
  if ($moonbitlang$core$builtin$op_notequal$4(a$427, b$428)) {
    moonbit_string_t fail_msg$429;
    if (msg$430 == 0) {
      moonbit_string_t _tmp$1698;
      moonbit_string_t _tmp$1697;
      moonbit_string_t _tmp$1696;
      moonbit_string_t _tmp$1693;
      moonbit_string_t _tmp$1695;
      moonbit_string_t _tmp$1694;
      moonbit_string_t _tmp$1692;
      if (msg$430) {
        moonbit_decref(msg$430);
      }
      _tmp$1698 = $moonbitlang$core$builtin$debug_string$3(a$427);
      _tmp$1697
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1698
      );
      _tmp$1696
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_101.data, _tmp$1697
      );
      _tmp$1693
      = moonbit_add_string(
        _tmp$1696, (moonbit_string_t)moonbit_string_literal_114.data
      );
      _tmp$1695 = $moonbitlang$core$builtin$debug_string$3(b$428);
      _tmp$1694
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1695
      );
      _tmp$1692 = moonbit_add_string(_tmp$1693, _tmp$1694);
      fail_msg$429
      = moonbit_add_string(
        _tmp$1692, (moonbit_string_t)moonbit_string_literal_101.data
      );
    } else {
      moonbit_string_t _Some$431;
      if (b$428) {
        moonbit_decref(b$428);
      }
      if (a$427) {
        moonbit_decref(a$427);
      }
      _Some$431 = msg$430;
      fail_msg$429 = _Some$431;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$429, loc$432);
  } else {
    int32_t _tmp$1699;
    struct moonbit_result_0 _result$3363;
    moonbit_decref(loc$432);
    if (msg$430) {
      moonbit_decref(msg$430);
    }
    if (b$428) {
      moonbit_decref(b$428);
    }
    if (a$427) {
      moonbit_decref(a$427);
    }
    _tmp$1699 = 0;
    _result$3363.tag = 1;
    _result$3363.data.ok = _tmp$1699;
    return _result$3363;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  int64_t a$421,
  int64_t b$422,
  moonbit_string_t msg$424,
  moonbit_string_t loc$426
) {
  if ($moonbitlang$core$builtin$op_notequal$2(a$421, b$422)) {
    moonbit_string_t fail_msg$423;
    if (msg$424 == 0) {
      moonbit_string_t _tmp$1690;
      moonbit_string_t _tmp$1689;
      moonbit_string_t _tmp$1688;
      moonbit_string_t _tmp$1685;
      moonbit_string_t _tmp$1687;
      moonbit_string_t _tmp$1686;
      moonbit_string_t _tmp$1684;
      if (msg$424) {
        moonbit_decref(msg$424);
      }
      _tmp$1690 = $moonbitlang$core$builtin$debug_string$2(a$421);
      _tmp$1689
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1690
      );
      _tmp$1688
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_101.data, _tmp$1689
      );
      _tmp$1685
      = moonbit_add_string(
        _tmp$1688, (moonbit_string_t)moonbit_string_literal_114.data
      );
      _tmp$1687 = $moonbitlang$core$builtin$debug_string$2(b$422);
      _tmp$1686
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1687
      );
      _tmp$1684 = moonbit_add_string(_tmp$1685, _tmp$1686);
      fail_msg$423
      = moonbit_add_string(
        _tmp$1684, (moonbit_string_t)moonbit_string_literal_101.data
      );
    } else {
      moonbit_string_t _Some$425 = msg$424;
      fail_msg$423 = _Some$425;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$423, loc$426);
  } else {
    int32_t _tmp$1691;
    struct moonbit_result_0 _result$3364;
    moonbit_decref(loc$426);
    if (msg$424) {
      moonbit_decref(msg$424);
    }
    _tmp$1691 = 0;
    _result$3364.tag = 1;
    _result$3364.data.ok = _tmp$1691;
    return _result$3364;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  moonbit_string_t a$415,
  moonbit_string_t b$416,
  moonbit_string_t msg$418,
  moonbit_string_t loc$420
) {
  moonbit_incref(b$416);
  moonbit_incref(a$415);
  if ($moonbitlang$core$builtin$op_notequal$1(a$415, b$416)) {
    moonbit_string_t fail_msg$417;
    if (msg$418 == 0) {
      moonbit_string_t _tmp$1682;
      moonbit_string_t _tmp$1681;
      moonbit_string_t _tmp$1680;
      moonbit_string_t _tmp$1677;
      moonbit_string_t _tmp$1679;
      moonbit_string_t _tmp$1678;
      moonbit_string_t _tmp$1676;
      if (msg$418) {
        moonbit_decref(msg$418);
      }
      _tmp$1682 = $moonbitlang$core$builtin$debug_string$1(a$415);
      _tmp$1681
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1682
      );
      _tmp$1680
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_101.data, _tmp$1681
      );
      _tmp$1677
      = moonbit_add_string(
        _tmp$1680, (moonbit_string_t)moonbit_string_literal_114.data
      );
      _tmp$1679 = $moonbitlang$core$builtin$debug_string$1(b$416);
      _tmp$1678
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1679
      );
      _tmp$1676 = moonbit_add_string(_tmp$1677, _tmp$1678);
      fail_msg$417
      = moonbit_add_string(
        _tmp$1676, (moonbit_string_t)moonbit_string_literal_101.data
      );
    } else {
      moonbit_string_t _Some$419;
      moonbit_decref(b$416);
      moonbit_decref(a$415);
      _Some$419 = msg$418;
      fail_msg$417 = _Some$419;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$417, loc$420);
  } else {
    int32_t _tmp$1683;
    struct moonbit_result_0 _result$3365;
    moonbit_decref(loc$420);
    if (msg$418) {
      moonbit_decref(msg$418);
    }
    moonbit_decref(b$416);
    moonbit_decref(a$415);
    _tmp$1683 = 0;
    _result$3365.tag = 1;
    _result$3365.data.ok = _tmp$1683;
    return _result$3365;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  int32_t a$409,
  int32_t b$410,
  moonbit_string_t msg$412,
  moonbit_string_t loc$414
) {
  if (a$409 != b$410) {
    moonbit_string_t fail_msg$411;
    if (msg$412 == 0) {
      moonbit_string_t _tmp$1674;
      moonbit_string_t _tmp$1673;
      moonbit_string_t _tmp$1672;
      moonbit_string_t _tmp$1669;
      moonbit_string_t _tmp$1671;
      moonbit_string_t _tmp$1670;
      moonbit_string_t _tmp$1668;
      if (msg$412) {
        moonbit_decref(msg$412);
      }
      _tmp$1674 = $moonbitlang$core$builtin$debug_string$0(a$409);
      _tmp$1673
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1674
      );
      _tmp$1672
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_101.data, _tmp$1673
      );
      _tmp$1669
      = moonbit_add_string(
        _tmp$1672, (moonbit_string_t)moonbit_string_literal_114.data
      );
      _tmp$1671 = $moonbitlang$core$builtin$debug_string$0(b$410);
      _tmp$1670
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1671
      );
      _tmp$1668 = moonbit_add_string(_tmp$1669, _tmp$1670);
      fail_msg$411
      = moonbit_add_string(
        _tmp$1668, (moonbit_string_t)moonbit_string_literal_101.data
      );
    } else {
      moonbit_string_t _Some$413 = msg$412;
      fail_msg$411 = _Some$413;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$411, loc$414);
  } else {
    int32_t _tmp$1675;
    struct moonbit_result_0 _result$3366;
    moonbit_decref(loc$414);
    if (msg$412) {
      moonbit_decref(msg$412);
    }
    _tmp$1675 = 0;
    _result$3366.tag = 1;
    _result$3366.data.ok = _tmp$1675;
    return _result$3366;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$408,
  moonbit_string_t loc$407
) {
  moonbit_string_t _tmp$1667 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$407);
  moonbit_string_t _tmp$1665 =
    moonbit_add_string(
      _tmp$1667, (moonbit_string_t)moonbit_string_literal_115.data
    );
  moonbit_string_t _tmp$1666 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(msg$408);
  moonbit_string_t _tmp$1664 = moonbit_add_string(_tmp$1665, _tmp$1666);
  void* moonbitlang$core$builtin$Failure$Failure$1663 =
    (void*)moonbit_malloc(
      sizeof(struct $Error$moonbitlang$core$builtin$Failure$Failure)
    );
  struct moonbit_result_0 _result$3367;
  Moonbit_object_header(moonbitlang$core$builtin$Failure$Failure$1663)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Error$moonbitlang$core$builtin$Failure$Failure, $0) >> 2,
      1,
      2
  );
  ((struct $Error$moonbitlang$core$builtin$Failure$Failure*)moonbitlang$core$builtin$Failure$Failure$1663)->$0
  = _tmp$1664;
  _result$3367.tag = 0;
  _result$3367.data.err = moonbitlang$core$builtin$Failure$Failure$1663;
  return _result$3367;
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$6(void* t$406) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$405 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1662;
  moonbit_incref(buf$405);
  _tmp$1662
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$405
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$3(t$406, _tmp$1662);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$405);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$5(int32_t t$404) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$403 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1661;
  moonbit_incref(buf$403);
  _tmp$1661
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$403
  };
  $$moonbitlang$core$builtin$Show$$Char$$output(t$404, _tmp$1661);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$403);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(int32_t t$402) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$401 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1660;
  moonbit_incref(buf$401);
  _tmp$1660
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$401
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$2(t$402, _tmp$1660);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$401);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(
  moonbit_string_t t$400
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$399 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1659;
  moonbit_incref(buf$399);
  _tmp$1659
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$399
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$1(t$400, _tmp$1659);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$399);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(int64_t t$398) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$397 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1658;
  moonbit_incref(buf$397);
  _tmp$1658
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$397
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$0(t$398, _tmp$1658);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$397);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(
  moonbit_string_t t$396
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$395 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1657;
  moonbit_incref(buf$395);
  _tmp$1657
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$395
  };
  $$moonbitlang$core$builtin$Show$$String$$output(t$396, _tmp$1657);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$395);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(int32_t t$394) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$393 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1656;
  moonbit_incref(buf$393);
  _tmp$1656
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$393
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(t$394, _tmp$1656);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$393);
}

moonbit_string_t $Int$$to_string$inner(int32_t self$377, int32_t radix$376) {
  int32_t _if_result$3368;
  int32_t is_negative$378;
  uint32_t num$379;
  uint16_t* buffer$380;
  if (radix$376 < 2) {
    _if_result$3368 = 1;
  } else {
    _if_result$3368 = radix$376 > 36;
  }
  if (_if_result$3368) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_116.data,
        (moonbit_string_t)moonbit_string_literal_117.data
    );
  }
  if (self$377 == 0) {
    return (moonbit_string_t)moonbit_string_literal_118.data;
  }
  is_negative$378 = self$377 < 0;
  if (is_negative$378) {
    int32_t _tmp$1655 = -self$377;
    num$379 = *(uint32_t*)&_tmp$1655;
  } else {
    num$379 = *(uint32_t*)&self$377;
  }
  switch (radix$376) {
    case 10: {
      int32_t digit_len$381 = $moonbitlang$core$builtin$dec_count32(num$379);
      int32_t _tmp$1652;
      int32_t total_len$382;
      uint16_t* buffer$383;
      int32_t digit_start$384;
      if (is_negative$378) {
        _tmp$1652 = 1;
      } else {
        _tmp$1652 = 0;
      }
      total_len$382 = digit_len$381 + _tmp$1652;
      buffer$383 = (uint16_t*)moonbit_make_string(total_len$382, 0);
      if (is_negative$378) {
        digit_start$384 = 1;
      } else {
        digit_start$384 = 0;
      }
      moonbit_incref(buffer$383);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$383, num$379, digit_start$384, total_len$382
      );
      buffer$380 = buffer$383;
      break;
    }
    
    case 16: {
      int32_t digit_len$385 = $moonbitlang$core$builtin$hex_count32(num$379);
      int32_t _tmp$1653;
      int32_t total_len$386;
      uint16_t* buffer$387;
      int32_t digit_start$388;
      if (is_negative$378) {
        _tmp$1653 = 1;
      } else {
        _tmp$1653 = 0;
      }
      total_len$386 = digit_len$385 + _tmp$1653;
      buffer$387 = (uint16_t*)moonbit_make_string(total_len$386, 0);
      if (is_negative$378) {
        digit_start$388 = 1;
      } else {
        digit_start$388 = 0;
      }
      moonbit_incref(buffer$387);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$387, num$379, digit_start$388, total_len$386
      );
      buffer$380 = buffer$387;
      break;
    }
    default: {
      int32_t digit_len$389 =
        $moonbitlang$core$builtin$radix_count32(num$379, radix$376);
      int32_t _tmp$1654;
      int32_t total_len$390;
      uint16_t* buffer$391;
      int32_t digit_start$392;
      if (is_negative$378) {
        _tmp$1654 = 1;
      } else {
        _tmp$1654 = 0;
      }
      total_len$390 = digit_len$389 + _tmp$1654;
      buffer$391 = (uint16_t*)moonbit_make_string(total_len$390, 0);
      if (is_negative$378) {
        digit_start$392 = 1;
      } else {
        digit_start$392 = 0;
      }
      moonbit_incref(buffer$391);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$391, num$379, digit_start$392, total_len$390, radix$376
      );
      buffer$380 = buffer$391;
      break;
    }
  }
  if (is_negative$378) {
    buffer$380[0] = 45;
  }
  return buffer$380;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$370,
  int32_t radix$373
) {
  uint32_t num$371;
  uint32_t base$372;
  int32_t count$374;
  if (value$370 == 0u) {
    return 1;
  }
  num$371 = value$370;
  base$372 = *(uint32_t*)&radix$373;
  count$374 = 0;
  while (1) {
    uint32_t _tmp$1649 = num$371;
    if (_tmp$1649 > 0u) {
      int32_t _tmp$1650 = count$374;
      uint32_t _tmp$1651;
      count$374 = _tmp$1650 + 1;
      _tmp$1651 = num$371;
      num$371 = _tmp$1651 / base$372;
      continue;
    }
    break;
  }
  return count$374;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$368) {
  if (value$368 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$369 = moonbit_clz32(value$368);
    int32_t _tmp$1648 = 31 - leading_zeros$369;
    int32_t _tmp$1647 = _tmp$1648 / 4;
    return _tmp$1647 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$367) {
  if (value$367 >= 100000u) {
    if (value$367 >= 10000000u) {
      if (value$367 >= 1000000000u) {
        return 10;
      } else if (value$367 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$367 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$367 >= 1000u) {
    if (value$367 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$367 >= 100u) {
    return 3;
  } else if (value$367 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$357,
  uint32_t num$345,
  int32_t digit_start$348,
  int32_t total_len$347
) {
  uint32_t num$344 = num$345;
  int32_t offset$346 = total_len$347 - digit_start$348;
  uint32_t _tmp$1646;
  int32_t remaining$359;
  int32_t _tmp$1627;
  while (1) {
    uint32_t _tmp$1590 = num$344;
    if (_tmp$1590 >= 10000u) {
      uint32_t _tmp$1613 = num$344;
      uint32_t t$349 = _tmp$1613 / 10000u;
      uint32_t _tmp$1612 = num$344;
      uint32_t _tmp$1611 = _tmp$1612 % 10000u;
      int32_t r$350 = *(int32_t*)&_tmp$1611;
      int32_t d1$351;
      int32_t d2$352;
      int32_t _tmp$1591;
      int32_t _tmp$1610;
      int32_t _tmp$1609;
      int32_t d1_hi$353;
      int32_t _tmp$1608;
      int32_t _tmp$1607;
      int32_t d1_lo$354;
      int32_t _tmp$1606;
      int32_t _tmp$1605;
      int32_t d2_hi$355;
      int32_t _tmp$1604;
      int32_t _tmp$1603;
      int32_t d2_lo$356;
      int32_t _tmp$1593;
      int32_t _tmp$1592;
      int32_t _tmp$1596;
      int32_t _tmp$1595;
      int32_t _tmp$1594;
      int32_t _tmp$1599;
      int32_t _tmp$1598;
      int32_t _tmp$1597;
      int32_t _tmp$1602;
      int32_t _tmp$1601;
      int32_t _tmp$1600;
      num$344 = t$349;
      d1$351 = r$350 / 100;
      d2$352 = r$350 % 100;
      _tmp$1591 = offset$346;
      offset$346 = _tmp$1591 - 4;
      _tmp$1610 = d1$351 / 10;
      _tmp$1609 = 48 + _tmp$1610;
      d1_hi$353 = (uint16_t)_tmp$1609;
      _tmp$1608 = d1$351 % 10;
      _tmp$1607 = 48 + _tmp$1608;
      d1_lo$354 = (uint16_t)_tmp$1607;
      _tmp$1606 = d2$352 / 10;
      _tmp$1605 = 48 + _tmp$1606;
      d2_hi$355 = (uint16_t)_tmp$1605;
      _tmp$1604 = d2$352 % 10;
      _tmp$1603 = 48 + _tmp$1604;
      d2_lo$356 = (uint16_t)_tmp$1603;
      _tmp$1593 = offset$346;
      _tmp$1592 = digit_start$348 + _tmp$1593;
      buffer$357[_tmp$1592] = d1_hi$353;
      _tmp$1596 = offset$346;
      _tmp$1595 = digit_start$348 + _tmp$1596;
      _tmp$1594 = _tmp$1595 + 1;
      buffer$357[_tmp$1594] = d1_lo$354;
      _tmp$1599 = offset$346;
      _tmp$1598 = digit_start$348 + _tmp$1599;
      _tmp$1597 = _tmp$1598 + 2;
      buffer$357[_tmp$1597] = d2_hi$355;
      _tmp$1602 = offset$346;
      _tmp$1601 = digit_start$348 + _tmp$1602;
      _tmp$1600 = _tmp$1601 + 3;
      buffer$357[_tmp$1600] = d2_lo$356;
      continue;
    }
    break;
  }
  _tmp$1646 = num$344;
  remaining$359 = *(int32_t*)&_tmp$1646;
  while (1) {
    int32_t _tmp$1614 = remaining$359;
    if (_tmp$1614 >= 100) {
      int32_t _tmp$1626 = remaining$359;
      int32_t t$360 = _tmp$1626 / 100;
      int32_t _tmp$1625 = remaining$359;
      int32_t d$361 = _tmp$1625 % 100;
      int32_t _tmp$1615;
      int32_t _tmp$1624;
      int32_t _tmp$1623;
      int32_t d_hi$362;
      int32_t _tmp$1622;
      int32_t _tmp$1621;
      int32_t d_lo$363;
      int32_t _tmp$1617;
      int32_t _tmp$1616;
      int32_t _tmp$1620;
      int32_t _tmp$1619;
      int32_t _tmp$1618;
      remaining$359 = t$360;
      _tmp$1615 = offset$346;
      offset$346 = _tmp$1615 - 2;
      _tmp$1624 = d$361 / 10;
      _tmp$1623 = 48 + _tmp$1624;
      d_hi$362 = (uint16_t)_tmp$1623;
      _tmp$1622 = d$361 % 10;
      _tmp$1621 = 48 + _tmp$1622;
      d_lo$363 = (uint16_t)_tmp$1621;
      _tmp$1617 = offset$346;
      _tmp$1616 = digit_start$348 + _tmp$1617;
      buffer$357[_tmp$1616] = d_hi$362;
      _tmp$1620 = offset$346;
      _tmp$1619 = digit_start$348 + _tmp$1620;
      _tmp$1618 = _tmp$1619 + 1;
      buffer$357[_tmp$1618] = d_lo$363;
      continue;
    }
    break;
  }
  _tmp$1627 = remaining$359;
  if (_tmp$1627 >= 10) {
    int32_t _tmp$1628 = offset$346;
    int32_t _tmp$1639;
    int32_t _tmp$1638;
    int32_t _tmp$1637;
    int32_t d_hi$365;
    int32_t _tmp$1636;
    int32_t _tmp$1635;
    int32_t _tmp$1634;
    int32_t d_lo$366;
    int32_t _tmp$1630;
    int32_t _tmp$1629;
    int32_t _tmp$1633;
    int32_t _tmp$1632;
    int32_t _tmp$1631;
    offset$346 = _tmp$1628 - 2;
    _tmp$1639 = remaining$359;
    _tmp$1638 = _tmp$1639 / 10;
    _tmp$1637 = 48 + _tmp$1638;
    d_hi$365 = (uint16_t)_tmp$1637;
    _tmp$1636 = remaining$359;
    _tmp$1635 = _tmp$1636 % 10;
    _tmp$1634 = 48 + _tmp$1635;
    d_lo$366 = (uint16_t)_tmp$1634;
    _tmp$1630 = offset$346;
    _tmp$1629 = digit_start$348 + _tmp$1630;
    buffer$357[_tmp$1629] = d_hi$365;
    _tmp$1633 = offset$346;
    _tmp$1632 = digit_start$348 + _tmp$1633;
    _tmp$1631 = _tmp$1632 + 1;
    buffer$357[_tmp$1631] = d_lo$366;
    moonbit_decref(buffer$357);
  } else {
    int32_t _tmp$1640 = offset$346;
    int32_t _tmp$1645;
    int32_t _tmp$1641;
    int32_t _tmp$1644;
    int32_t _tmp$1643;
    int32_t _tmp$1642;
    offset$346 = _tmp$1640 - 1;
    _tmp$1645 = offset$346;
    _tmp$1641 = digit_start$348 + _tmp$1645;
    _tmp$1644 = remaining$359;
    _tmp$1643 = 48 + _tmp$1644;
    _tmp$1642 = (uint16_t)_tmp$1643;
    buffer$357[_tmp$1641] = _tmp$1642;
    moonbit_decref(buffer$357);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$339,
  uint32_t num$333,
  int32_t digit_start$331,
  int32_t total_len$330,
  int32_t radix$335
) {
  int32_t offset$329 = total_len$330 - digit_start$331;
  uint32_t n$332 = num$333;
  uint32_t base$334 = *(uint32_t*)&radix$335;
  int32_t _tmp$1570 = radix$335 - 1;
  int32_t _tmp$1569 = radix$335 & _tmp$1570;
  if (_tmp$1569 == 0) {
    int32_t shift$336 = moonbit_ctz32(radix$335);
    uint32_t mask$337 = base$334 - 1u;
    while (1) {
      uint32_t _tmp$1571 = n$332;
      if (_tmp$1571 > 0u) {
        int32_t _tmp$1572 = offset$329;
        uint32_t _tmp$1579;
        uint32_t _tmp$1578;
        int32_t digit$338;
        int32_t _tmp$1576;
        int32_t _tmp$1573;
        int32_t _tmp$1575;
        int32_t _tmp$1574;
        uint32_t _tmp$1577;
        offset$329 = _tmp$1572 - 1;
        _tmp$1579 = n$332;
        _tmp$1578 = _tmp$1579 & mask$337;
        digit$338 = *(int32_t*)&_tmp$1578;
        _tmp$1576 = offset$329;
        _tmp$1573 = digit_start$331 + _tmp$1576;
        _tmp$1575
        = ((moonbit_string_t)moonbit_string_literal_119.data)[
          digit$338
        ];
        _tmp$1574 = (uint16_t)_tmp$1575;
        buffer$339[_tmp$1573] = _tmp$1574;
        _tmp$1577 = n$332;
        n$332 = _tmp$1577 >> (shift$336 & 31);
        continue;
      } else {
        moonbit_decref(buffer$339);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1580 = n$332;
      if (_tmp$1580 > 0u) {
        int32_t _tmp$1581 = offset$329;
        uint32_t _tmp$1589;
        uint32_t q$341;
        uint32_t _tmp$1587;
        uint32_t _tmp$1588;
        uint32_t _tmp$1586;
        int32_t digit$342;
        int32_t _tmp$1585;
        int32_t _tmp$1582;
        int32_t _tmp$1584;
        int32_t _tmp$1583;
        offset$329 = _tmp$1581 - 1;
        _tmp$1589 = n$332;
        q$341 = _tmp$1589 / base$334;
        _tmp$1587 = n$332;
        _tmp$1588 = q$341 * base$334;
        _tmp$1586 = _tmp$1587 - _tmp$1588;
        digit$342 = *(int32_t*)&_tmp$1586;
        _tmp$1585 = offset$329;
        _tmp$1582 = digit_start$331 + _tmp$1585;
        _tmp$1584
        = ((moonbit_string_t)moonbit_string_literal_119.data)[
          digit$342
        ];
        _tmp$1583 = (uint16_t)_tmp$1584;
        buffer$339[_tmp$1582] = _tmp$1583;
        n$332 = q$341;
        continue;
      } else {
        moonbit_decref(buffer$339);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$326,
  uint32_t num$322,
  int32_t digit_start$320,
  int32_t total_len$319
) {
  int32_t offset$318 = total_len$319 - digit_start$320;
  uint32_t n$321 = num$322;
  int32_t _tmp$1564;
  while (1) {
    int32_t _tmp$1550 = offset$318;
    if (_tmp$1550 >= 2) {
      int32_t _tmp$1551 = offset$318;
      uint32_t _tmp$1563;
      uint32_t _tmp$1562;
      int32_t byte_val$323;
      int32_t hi$324;
      int32_t lo$325;
      int32_t _tmp$1555;
      int32_t _tmp$1552;
      int32_t _tmp$1554;
      int32_t _tmp$1553;
      int32_t _tmp$1560;
      int32_t _tmp$1559;
      int32_t _tmp$1556;
      int32_t _tmp$1558;
      int32_t _tmp$1557;
      uint32_t _tmp$1561;
      offset$318 = _tmp$1551 - 2;
      _tmp$1563 = n$321;
      _tmp$1562 = _tmp$1563 & 255u;
      byte_val$323 = *(int32_t*)&_tmp$1562;
      hi$324 = byte_val$323 / 16;
      lo$325 = byte_val$323 % 16;
      _tmp$1555 = offset$318;
      _tmp$1552 = digit_start$320 + _tmp$1555;
      _tmp$1554 = ((moonbit_string_t)moonbit_string_literal_119.data)[hi$324];
      _tmp$1553 = (uint16_t)_tmp$1554;
      buffer$326[_tmp$1552] = _tmp$1553;
      _tmp$1560 = offset$318;
      _tmp$1559 = digit_start$320 + _tmp$1560;
      _tmp$1556 = _tmp$1559 + 1;
      _tmp$1558 = ((moonbit_string_t)moonbit_string_literal_119.data)[lo$325];
      _tmp$1557 = (uint16_t)_tmp$1558;
      buffer$326[_tmp$1556] = _tmp$1557;
      _tmp$1561 = n$321;
      n$321 = _tmp$1561 >> 8;
      continue;
    }
    break;
  }
  _tmp$1564 = offset$318;
  if (_tmp$1564 == 1) {
    uint32_t _tmp$1568 = n$321;
    uint32_t _tmp$1567 = _tmp$1568 & 15u;
    int32_t nibble$328 = *(int32_t*)&_tmp$1567;
    int32_t _tmp$1566 =
      ((moonbit_string_t)moonbit_string_literal_119.data)[nibble$328];
    int32_t _tmp$1565 = (uint16_t)_tmp$1566;
    buffer$326[digit_start$320] = _tmp$1565;
    moonbit_decref(buffer$326);
  } else {
    moonbit_decref(buffer$326);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$317
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$316 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1549;
  moonbit_incref(logger$316);
  _tmp$1549
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$316
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$317, _tmp$1549
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$316);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$315
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$314 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1548;
  moonbit_incref(logger$314);
  _tmp$1548
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$314
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$315, _tmp$1548
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$314);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$313
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$312 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1547;
  moonbit_incref(logger$312);
  _tmp$1547
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$312
  };
  $$moonbitlang$core$builtin$Show$$Bool$$output(self$313, _tmp$1547);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$312);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$311
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$310 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1546;
  moonbit_incref(logger$310);
  _tmp$1546
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$310
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$311, _tmp$1546
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$310);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$309
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$308 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1545;
  moonbit_incref(logger$308);
  _tmp$1545
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$308
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$309, _tmp$1545);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$308);
}

int32_t $StringView$$start_offset(struct $StringView self$307) {
  int32_t _field$2955 = self$307.$1;
  moonbit_decref(self$307.$0);
  return _field$2955;
}

moonbit_string_t $StringView$$data(struct $StringView self$306) {
  moonbit_string_t _field$2956 = self$306.$0;
  return _field$2956;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$300,
  moonbit_string_t value$303,
  int32_t start$304,
  int32_t len$305
) {
  void* _try_err$302;
  struct $StringView _tmp$1540;
  int32_t _tmp$1542 = start$304 + len$305;
  int64_t _tmp$1541 = (int64_t)_tmp$1542;
  struct moonbit_result_1 _tmp$3376 =
    $String$$sub$inner(value$303, start$304, _tmp$1541);
  if (_tmp$3376.tag) {
    struct $StringView const _ok$1543 = _tmp$3376.data.ok;
    _tmp$1540 = _ok$1543;
  } else {
    void* const _err$1544 = _tmp$3376.data.err;
    _try_err$302 = _err$1544;
    goto $join$301;
  }
  goto $joinlet$3375;
  $join$301:;
  moonbit_decref(_try_err$302);
  moonbit_panic();
  $joinlet$3375:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$300, _tmp$1540
  );
  return 0;
}

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$293,
  int32_t start$299,
  int64_t end$295
) {
  int32_t len$292 = Moonbit_array_length(self$293);
  int32_t end$294;
  int32_t start$298;
  int32_t _if_result$3377;
  if (end$295 == 4294967296ll) {
    end$294 = len$292;
  } else {
    int64_t _Some$296 = end$295;
    int32_t _end$297 = (int32_t)_Some$296;
    if (_end$297 < 0) {
      end$294 = len$292 + _end$297;
    } else {
      end$294 = _end$297;
    }
  }
  if (start$299 < 0) {
    start$298 = len$292 + start$299;
  } else {
    start$298 = start$299;
  }
  if (start$298 >= 0) {
    if (start$298 <= end$294) {
      _if_result$3377 = end$294 <= len$292;
    } else {
      _if_result$3377 = 0;
    }
  } else {
    _if_result$3377 = 0;
  }
  if (_if_result$3377) {
    int32_t _if_result$3378;
    int32_t _if_result$3380;
    struct $StringView _tmp$1538;
    struct moonbit_result_1 _result$3382;
    if (start$298 < len$292) {
      int32_t _tmp$1534 = self$293[start$298];
      _if_result$3378 = $Int$$is_trailing_surrogate(_tmp$1534);
    } else {
      _if_result$3378 = 0;
    }
    if (_if_result$3378) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1535;
      struct moonbit_result_1 _result$3379;
      moonbit_decref(self$293);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1535
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$3379.tag = 0;
      _result$3379.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1535;
      return _result$3379;
    }
    if (end$294 < len$292) {
      int32_t _tmp$1536 = self$293[end$294];
      _if_result$3380 = $Int$$is_trailing_surrogate(_tmp$1536);
    } else {
      _if_result$3380 = 0;
    }
    if (_if_result$3380) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1537;
      struct moonbit_result_1 _result$3381;
      moonbit_decref(self$293);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1537
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$3381.tag = 0;
      _result$3381.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1537;
      return _result$3381;
    }
    _tmp$1538 = (struct $StringView){start$298, end$294, self$293};
    _result$3382.tag = 1;
    _result$3382.data.ok = _tmp$1538;
    return _result$3382;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1539;
    struct moonbit_result_1 _result$3383;
    moonbit_decref(self$293);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1539
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$3383.tag = 0;
    _result$3383.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1539;
    return _result$3383;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$291
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$290 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1533;
  moonbit_incref(_self$290);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$290, self$291);
  _tmp$1533 = _self$290;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1533);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$289
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$288 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1532;
  moonbit_incref(_self$288);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$288, self$289);
  _tmp$1532 = _self$288;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1532);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$286
) {
  int32_t seed$285;
  if (seed$opt$286 == 4294967296ll) {
    seed$285 = 0;
  } else {
    int64_t _Some$287 = seed$opt$286;
    seed$285 = (int32_t)_Some$287;
  }
  return $$moonbitlang$core$builtin$Hasher$$new$inner(seed$285);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$284
) {
  uint32_t _tmp$1531 = *(uint32_t*)&seed$284;
  uint32_t _tmp$1530 = _tmp$1531 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$3384 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$3384)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$3384->$0 = _tmp$1530;
  return _block$3384;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$283
) {
  uint32_t _tmp$1529 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$283);
  return *(int32_t*)&_tmp$1529;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$282
) {
  uint32_t _field$2957 = self$282->$0;
  uint32_t acc$281;
  uint32_t _tmp$1518;
  uint32_t _tmp$1520;
  uint32_t _tmp$1519;
  uint32_t _tmp$1521;
  uint32_t _tmp$1522;
  uint32_t _tmp$1524;
  uint32_t _tmp$1523;
  uint32_t _tmp$1525;
  uint32_t _tmp$1526;
  uint32_t _tmp$1528;
  uint32_t _tmp$1527;
  moonbit_decref(self$282);
  acc$281 = _field$2957;
  _tmp$1518 = acc$281;
  _tmp$1520 = acc$281;
  _tmp$1519 = _tmp$1520 >> 15;
  acc$281 = _tmp$1518 ^ _tmp$1519;
  _tmp$1521 = acc$281;
  acc$281 = _tmp$1521 * 2246822519u;
  _tmp$1522 = acc$281;
  _tmp$1524 = acc$281;
  _tmp$1523 = _tmp$1524 >> 13;
  acc$281 = _tmp$1522 ^ _tmp$1523;
  _tmp$1525 = acc$281;
  acc$281 = _tmp$1525 * 3266489917u;
  _tmp$1526 = acc$281;
  _tmp$1528 = acc$281;
  _tmp$1527 = _tmp$1528 >> 16;
  acc$281 = _tmp$1526 ^ _tmp$1527;
  return acc$281;
}

int32_t $$moonbitlang$core$builtin$Compare$$$default_impl$$op_ge$0(
  moonbit_string_t x$279,
  moonbit_string_t y$280
) {
  int32_t _tmp$1517 =
    $$moonbitlang$core$builtin$Compare$$String$$compare(x$279, y$280);
  return _tmp$1517 >= 0;
}

int32_t $$moonbitlang$core$builtin$Compare$$$default_impl$$op_le$0(
  moonbit_string_t x$277,
  moonbit_string_t y$278
) {
  int32_t _tmp$1516 =
    $$moonbitlang$core$builtin$Compare$$String$$compare(x$277, y$278);
  return _tmp$1516 <= 0;
}

int32_t $$moonbitlang$core$builtin$Compare$$$default_impl$$op_lt$0(
  moonbit_string_t x$275,
  moonbit_string_t y$276
) {
  int32_t _tmp$1515 =
    $$moonbitlang$core$builtin$Compare$$String$$compare(x$275, y$276);
  return _tmp$1515 < 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$274,
  int32_t value$273
) {
  $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(value$273, self$274);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$272,
  moonbit_string_t value$271
) {
  $$moonbitlang$core$builtin$Hash$$String$$hash_combine(value$271, self$272);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$269,
  int32_t value$270
) {
  uint32_t _tmp$1514 = *(uint32_t*)&value$270;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$269, _tmp$1514);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$267,
  uint32_t value$268
) {
  uint32_t acc$1513 = self$267->$0;
  uint32_t _tmp$1512 = acc$1513 + 4u;
  self$267->$0 = _tmp$1512;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$267, value$268);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$265,
  uint32_t input$266
) {
  uint32_t acc$1510 = self$265->$0;
  uint32_t _tmp$1511 = input$266 * 3266489917u;
  uint32_t _tmp$1509 = acc$1510 + _tmp$1511;
  uint32_t _tmp$1508 = $moonbitlang$core$builtin$rotl(_tmp$1509, 17);
  uint32_t _tmp$1507 = _tmp$1508 * 668265263u;
  self$265->$0 = _tmp$1507;
  moonbit_decref(self$265);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$263, int32_t r$264) {
  uint32_t _tmp$1504 = x$263 << (r$264 & 31);
  int32_t _tmp$1506 = 32 - r$264;
  uint32_t _tmp$1505 = x$263 >> (_tmp$1506 & 31);
  return _tmp$1504 | _tmp$1505;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$261,
  moonbit_string_t str$262
) {
  int32_t len$1494 = self$261->$1;
  int32_t _tmp$1496 = Moonbit_array_length(str$262);
  int32_t _tmp$1495 = _tmp$1496 * 2;
  int32_t _tmp$1493 = len$1494 + _tmp$1495;
  moonbit_bytes_t _field$2959;
  moonbit_bytes_t data$1497;
  int32_t len$1498;
  int32_t _tmp$1499;
  int32_t len$1501;
  int32_t _tmp$2958;
  int32_t _tmp$1503;
  int32_t _tmp$1502;
  int32_t _tmp$1500;
  moonbit_incref(self$261);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$261, _tmp$1493
  );
  _field$2959 = self$261->$0;
  data$1497 = _field$2959;
  len$1498 = self$261->$1;
  _tmp$1499 = Moonbit_array_length(str$262);
  moonbit_incref(data$1497);
  moonbit_incref(str$262);
  $FixedArray$$blit_from_string(data$1497, len$1498, str$262, 0, _tmp$1499);
  len$1501 = self$261->$1;
  _tmp$2958 = Moonbit_array_length(str$262);
  moonbit_decref(str$262);
  _tmp$1503 = _tmp$2958;
  _tmp$1502 = _tmp$1503 * 2;
  _tmp$1500 = len$1501 + _tmp$1502;
  self$261->$1 = _tmp$1500;
  moonbit_decref(self$261);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$253,
  int32_t bytes_offset$248,
  moonbit_string_t str$255,
  int32_t str_offset$251,
  int32_t length$249
) {
  int32_t _tmp$1492 = length$249 * 2;
  int32_t _tmp$1491 = bytes_offset$248 + _tmp$1492;
  int32_t e1$247 = _tmp$1491 - 1;
  int32_t _tmp$1490 = str_offset$251 + length$249;
  int32_t e2$250 = _tmp$1490 - 1;
  int32_t len1$252 = Moonbit_array_length(self$253);
  int32_t len2$254 = Moonbit_array_length(str$255);
  int32_t _if_result$3385;
  if (length$249 >= 0) {
    if (bytes_offset$248 >= 0) {
      if (e1$247 < len1$252) {
        if (str_offset$251 >= 0) {
          _if_result$3385 = e2$250 < len2$254;
        } else {
          _if_result$3385 = 0;
        }
      } else {
        _if_result$3385 = 0;
      }
    } else {
      _if_result$3385 = 0;
    }
  } else {
    _if_result$3385 = 0;
  }
  if (_if_result$3385) {
    int32_t end_str_offset$256 = str_offset$251 + length$249;
    int32_t i$257 = str_offset$251;
    int32_t j$258 = bytes_offset$248;
    while (1) {
      if (i$257 < end_str_offset$256) {
        int32_t _tmp$1487 = str$255[i$257];
        uint32_t c$259 = *(uint32_t*)&_tmp$1487;
        uint32_t _tmp$1483 = c$259 & 255u;
        int32_t _tmp$1482 = $UInt$$to_byte(_tmp$1483);
        int32_t _tmp$1484;
        uint32_t _tmp$1486;
        int32_t _tmp$1485;
        int32_t _tmp$1488;
        int32_t _tmp$1489;
        if (j$258 < 0 || j$258 >= Moonbit_array_length(self$253)) {
          moonbit_panic();
        }
        self$253[j$258] = _tmp$1482;
        _tmp$1484 = j$258 + 1;
        _tmp$1486 = c$259 >> 8;
        _tmp$1485 = $UInt$$to_byte(_tmp$1486);
        if (_tmp$1484 < 0 || _tmp$1484 >= Moonbit_array_length(self$253)) {
          moonbit_panic();
        }
        self$253[_tmp$1484] = _tmp$1485;
        _tmp$1488 = i$257 + 1;
        _tmp$1489 = j$258 + 2;
        i$257 = _tmp$1488;
        j$258 = _tmp$1489;
        continue;
      } else {
        moonbit_decref(str$255);
        moonbit_decref(self$253);
      }
      break;
    }
  } else {
    moonbit_decref(str$255);
    moonbit_decref(self$253);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$215
) {
  int32_t _tmp$1455 = Moonbit_array_length(repr$215);
  int64_t _tmp$1454 = (int64_t)_tmp$1455;
  moonbit_incref(repr$215);
  if ($String$$char_length_ge$inner(repr$215, 1, 0, _tmp$1454)) {
    int32_t _tmp$1481 = repr$215[0];
    int32_t _x$216 = _tmp$1481;
    if (_x$216 == 64) {
      int32_t _tmp$1480 = Moonbit_array_length(repr$215);
      int64_t _tmp$1479 = (int64_t)_tmp$1480;
      int64_t _bind$1198;
      int32_t _tmp$1477;
      int32_t _tmp$1478;
      struct $StringView _x$217;
      int32_t _tmp$1476;
      struct $StringView _tmp$1475;
      int64_t _bind$219;
      moonbit_incref(repr$215);
      _bind$1198
      = $String$$offset_of_nth_char$inner(
        repr$215, 1, 0, _tmp$1479
      );
      if (_bind$1198 == 4294967296ll) {
        _tmp$1477 = Moonbit_array_length(repr$215);
      } else {
        int64_t _Some$218 = _bind$1198;
        _tmp$1477 = (int32_t)_Some$218;
      }
      _tmp$1478 = Moonbit_array_length(repr$215);
      _x$217 = (struct $StringView){_tmp$1477, _tmp$1478, repr$215};
      _tmp$1476
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1475
      = (struct $StringView){
        0, _tmp$1476, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$217.$0);
      _bind$219 = $StringView$$find(_x$217, _tmp$1475);
      if (_bind$219 == 4294967296ll) {
        moonbit_decref(_x$217.$0);
        moonbit_panic();
      } else {
        int64_t _Some$220 = _bind$219;
        int32_t _pkg_end$221 = (int32_t)_Some$220;
        int64_t _tmp$1474 = (int64_t)_pkg_end$221;
        struct $StringView pkg$222;
        int32_t _tmp$1473;
        struct $StringView _tmp$1472;
        int64_t _bind$223;
        moonbit_incref(_x$217.$0);
        pkg$222 = $StringView$$view$inner(_x$217, 0, _tmp$1474);
        _tmp$1473
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1472
        = (struct $StringView){
          0, _tmp$1473, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$217.$0);
        _bind$223 = $StringView$$rev_find(_x$217, _tmp$1472);
        if (_bind$223 == 4294967296ll) {
          moonbit_decref(pkg$222.$0);
          moonbit_decref(_x$217.$0);
          moonbit_panic();
        } else {
          int64_t _Some$224 = _bind$223;
          int32_t _start_loc_end$225 = (int32_t)_Some$224;
          int32_t _tmp$1456 = _start_loc_end$225 + 1;
          int32_t _tmp$1457;
          moonbit_incref(_x$217.$0);
          _tmp$1457 = $StringView$$length(_x$217);
          if (_tmp$1456 < _tmp$1457) {
            int32_t _tmp$1471 = _start_loc_end$225 + 1;
            struct $StringView end_loc$226;
            struct $$3c$StringView$2a$StringView$3e$* _bind$227;
            moonbit_incref(_x$217.$0);
            end_loc$226
            = $StringView$$view$inner(
              _x$217, _tmp$1471, 4294967296ll
            );
            _bind$227
            = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
              end_loc$226
            );
            if (_bind$227 == 0) {
              if (_bind$227) {
                moonbit_decref(_bind$227);
              }
              moonbit_decref(pkg$222.$0);
              moonbit_decref(_x$217.$0);
              moonbit_panic();
            } else {
              struct $$3c$StringView$2a$StringView$3e$* _Some$228 = _bind$227;
              struct $$3c$StringView$2a$StringView$3e$* _x$229 = _Some$228;
              struct $StringView _field$2963 =
                (struct $StringView){
                  _x$229->$0_1, _x$229->$0_2, _x$229->$0_0
                };
              struct $StringView _end_line$230 = _field$2963;
              struct $StringView _field$2962 =
                (struct $StringView){
                  _x$229->$1_1, _x$229->$1_2, _x$229->$1_0
                };
              int32_t _cnt$3118 = Moonbit_object_header(_x$229)->rc;
              struct $StringView _end_column$231;
              int64_t _tmp$1470;
              struct $StringView rest$232;
              int32_t _tmp$1469;
              struct $StringView _tmp$1468;
              int64_t _bind$234;
              if (_cnt$3118 > 1) {
                int32_t _new_cnt$3119;
                moonbit_incref(_field$2962.$0);
                moonbit_incref(_end_line$230.$0);
                _new_cnt$3119 = _cnt$3118 - 1;
                Moonbit_object_header(_x$229)->rc = _new_cnt$3119;
              } else if (_cnt$3118 == 1) {
                moonbit_free(_x$229);
              }
              _end_column$231 = _field$2962;
              _tmp$1470 = (int64_t)_start_loc_end$225;
              rest$232 = $StringView$$view$inner(_x$217, 0, _tmp$1470);
              _tmp$1469
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1468
              = (struct $StringView){
                0,
                  _tmp$1469,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$232.$0);
              _bind$234 = $StringView$$rev_find(rest$232, _tmp$1468);
              if (_bind$234 == 4294967296ll) {
                moonbit_decref(rest$232.$0);
                moonbit_decref(_end_column$231.$0);
                moonbit_decref(_end_line$230.$0);
                moonbit_decref(pkg$222.$0);
                goto $join$233;
              } else {
                int64_t _Some$235 = _bind$234;
                int32_t _start_line_end$236 = (int32_t)_Some$235;
                int64_t _tmp$1467 = (int64_t)_start_line_end$236;
                struct $StringView _tmp$1464;
                int32_t _tmp$1466;
                struct $StringView _tmp$1465;
                int64_t _bind$237;
                moonbit_incref(rest$232.$0);
                _tmp$1464 = $StringView$$view$inner(rest$232, 0, _tmp$1467);
                _tmp$1466
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1465
                = (struct $StringView){
                  0,
                    _tmp$1466,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$237 = $StringView$$rev_find(_tmp$1464, _tmp$1465);
                if (_bind$237 == 4294967296ll) {
                  moonbit_decref(rest$232.$0);
                  moonbit_decref(_end_column$231.$0);
                  moonbit_decref(_end_line$230.$0);
                  moonbit_decref(pkg$222.$0);
                  goto $join$233;
                } else {
                  int64_t _Some$238 = _bind$237;
                  int32_t _filename_end$239 = (int32_t)_Some$238;
                  int32_t _tmp$1458 = _filename_end$239 + 1;
                  int32_t _tmp$1459;
                  moonbit_incref(rest$232.$0);
                  _tmp$1459 = $StringView$$length(rest$232);
                  if (_tmp$1458 < _tmp$1459) {
                    int32_t _tmp$1463 = _filename_end$239 + 1;
                    struct $StringView start_loc$240;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$241;
                    moonbit_incref(rest$232.$0);
                    start_loc$240
                    = $StringView$$view$inner(
                      rest$232, _tmp$1463, 4294967296ll
                    );
                    _bind$241
                    = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
                      start_loc$240
                    );
                    if (_bind$241 == 0) {
                      if (_bind$241) {
                        moonbit_decref(_bind$241);
                      }
                      moonbit_decref(rest$232.$0);
                      moonbit_decref(_end_column$231.$0);
                      moonbit_decref(_end_line$230.$0);
                      moonbit_decref(pkg$222.$0);
                      moonbit_panic();
                    } else {
                      struct $$3c$StringView$2a$StringView$3e$* _Some$242 =
                        _bind$241;
                      struct $$3c$StringView$2a$StringView$3e$* _x$243 =
                        _Some$242;
                      struct $StringView _field$2961 =
                        (struct $StringView){
                          _x$243->$0_1, _x$243->$0_2, _x$243->$0_0
                        };
                      struct $StringView _start_line$244 = _field$2961;
                      struct $StringView _field$2960 =
                        (struct $StringView){
                          _x$243->$1_1, _x$243->$1_2, _x$243->$1_0
                        };
                      int32_t _cnt$3120 = Moonbit_object_header(_x$243)->rc;
                      struct $StringView _start_column$245;
                      int32_t _tmp$1460;
                      if (_cnt$3120 > 1) {
                        int32_t _new_cnt$3121;
                        moonbit_incref(_field$2960.$0);
                        moonbit_incref(_start_line$244.$0);
                        _new_cnt$3121 = _cnt$3120 - 1;
                        Moonbit_object_header(_x$243)->rc = _new_cnt$3121;
                      } else if (_cnt$3120 == 1) {
                        moonbit_free(_x$243);
                      }
                      _start_column$245 = _field$2960;
                      _tmp$1460 = _pkg_end$221 + 1;
                      if (_filename_end$239 > _tmp$1460) {
                        int32_t _tmp$1461 = _pkg_end$221 + 1;
                        int64_t _tmp$1462 = (int64_t)_filename_end$239;
                        struct $StringView filename$246 =
                          $StringView$$view$inner(
                            rest$232, _tmp$1461, _tmp$1462
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$3389 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$3389)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$3389->$0_0 = pkg$222.$0;
                        _block$3389->$0_1 = pkg$222.$1;
                        _block$3389->$0_2 = pkg$222.$2;
                        _block$3389->$1_0 = filename$246.$0;
                        _block$3389->$1_1 = filename$246.$1;
                        _block$3389->$1_2 = filename$246.$2;
                        _block$3389->$2_0 = _start_line$244.$0;
                        _block$3389->$2_1 = _start_line$244.$1;
                        _block$3389->$2_2 = _start_line$244.$2;
                        _block$3389->$3_0 = _start_column$245.$0;
                        _block$3389->$3_1 = _start_column$245.$1;
                        _block$3389->$3_2 = _start_column$245.$2;
                        _block$3389->$4_0 = _end_line$230.$0;
                        _block$3389->$4_1 = _end_line$230.$1;
                        _block$3389->$4_2 = _end_line$230.$2;
                        _block$3389->$5_0 = _end_column$231.$0;
                        _block$3389->$5_1 = _end_column$231.$1;
                        _block$3389->$5_2 = _end_column$231.$2;
                        return _block$3389;
                      } else {
                        moonbit_decref(_start_column$245.$0);
                        moonbit_decref(_start_line$244.$0);
                        moonbit_decref(rest$232.$0);
                        moonbit_decref(_end_column$231.$0);
                        moonbit_decref(_end_line$230.$0);
                        moonbit_decref(pkg$222.$0);
                        moonbit_panic();
                      }
                    }
                  } else {
                    moonbit_decref(rest$232.$0);
                    moonbit_decref(_end_column$231.$0);
                    moonbit_decref(_end_line$230.$0);
                    moonbit_decref(pkg$222.$0);
                    moonbit_panic();
                  }
                }
              }
              $join$233:;
              moonbit_panic();
            }
          } else {
            moonbit_decref(pkg$222.$0);
            moonbit_decref(_x$217.$0);
            moonbit_panic();
          }
        }
      }
    } else {
      moonbit_decref(repr$215);
      goto $join$214;
    }
  } else {
    moonbit_decref(repr$215);
    goto $join$214;
  }
  $join$214:;
  moonbit_panic();
}

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$211
) {
  int32_t _tmp$1453 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1452;
  int64_t _bind$210;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1452
  = (struct $StringView){
    0, _tmp$1453, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$211.$0);
  _bind$210 = $StringView$$find(view$211, _tmp$1452);
  if (_bind$210 == 4294967296ll) {
    moonbit_decref(view$211.$0);
    return 0;
  } else {
    int64_t _Some$212 = _bind$210;
    int32_t _i$213 = (int32_t)_Some$212;
    int32_t _if_result$3390;
    if (_i$213 > 0) {
      int32_t _tmp$1445 = _i$213 + 1;
      int32_t _tmp$1446;
      moonbit_incref(view$211.$0);
      _tmp$1446 = $StringView$$length(view$211);
      _if_result$3390 = _tmp$1445 < _tmp$1446;
    } else {
      _if_result$3390 = 0;
    }
    if (_if_result$3390) {
      int64_t _tmp$1451 = (int64_t)_i$213;
      struct $StringView _tmp$1448;
      int32_t _tmp$1450;
      struct $StringView _tmp$1449;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1447;
      moonbit_incref(view$211.$0);
      _tmp$1448 = $StringView$$view$inner(view$211, 0, _tmp$1451);
      _tmp$1450 = _i$213 + 1;
      _tmp$1449 = $StringView$$view$inner(view$211, _tmp$1450, 4294967296ll);
      _tuple$1447
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1447)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1447->$0_0 = _tmp$1448.$0;
      _tuple$1447->$0_1 = _tmp$1448.$1;
      _tuple$1447->$0_2 = _tmp$1448.$2;
      _tuple$1447->$1_0 = _tmp$1449.$0;
      _tuple$1447->$1_1 = _tmp$1449.$1;
      _tuple$1447->$1_2 = _tmp$1449.$2;
      return _tuple$1447;
    } else {
      moonbit_decref(view$211.$0);
      return 0;
    }
  }
}

struct $StringView $StringView$$view$inner(
  struct $StringView self$208,
  int32_t start_offset$209,
  int64_t end_offset$206
) {
  int32_t end_offset$205;
  int32_t _if_result$3391;
  if (end_offset$206 == 4294967296ll) {
    moonbit_incref(self$208.$0);
    end_offset$205 = $StringView$$length(self$208);
  } else {
    int64_t _Some$207 = end_offset$206;
    end_offset$205 = (int32_t)_Some$207;
  }
  if (start_offset$209 >= 0) {
    if (start_offset$209 <= end_offset$205) {
      int32_t _tmp$1439;
      moonbit_incref(self$208.$0);
      _tmp$1439 = $StringView$$length(self$208);
      _if_result$3391 = end_offset$205 <= _tmp$1439;
    } else {
      _if_result$3391 = 0;
    }
  } else {
    _if_result$3391 = 0;
  }
  if (_if_result$3391) {
    moonbit_string_t _field$2965 = self$208.$0;
    moonbit_string_t str$1440 = _field$2965;
    int32_t start$1444 = self$208.$1;
    int32_t _tmp$1441 = start$1444 + start_offset$209;
    int32_t _field$2964 = self$208.$1;
    int32_t start$1443 = _field$2964;
    int32_t _tmp$1442 = start$1443 + end_offset$205;
    return (struct $StringView){_tmp$1441, _tmp$1442, str$1440};
  } else {
    moonbit_decref(self$208.$0);
    return $moonbitlang$core$builtin$abort$2(
             (moonbit_string_t)moonbit_string_literal_120.data,
               (moonbit_string_t)moonbit_string_literal_121.data
           );
  }
}

int64_t $StringView$$rev_find(
  struct $StringView self$204,
  struct $StringView str$203
) {
  int32_t _tmp$1438;
  moonbit_incref(str$203.$0);
  _tmp$1438 = $StringView$$length(str$203);
  if (_tmp$1438 <= 4) {
    return $moonbitlang$core$builtin$brute_force_rev_find(self$204, str$203);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
             self$204, str$203
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$194,
  struct $StringView needle$196
) {
  int32_t haystack_len$193;
  int32_t needle_len$195;
  moonbit_incref(haystack$194.$0);
  haystack_len$193 = $StringView$$length(haystack$194);
  moonbit_incref(needle$196.$0);
  needle_len$195 = $StringView$$length(needle$196);
  if (needle_len$195 > 0) {
    if (haystack_len$193 >= needle_len$195) {
      int32_t needle_first$197;
      int32_t i$198;
      moonbit_incref(needle$196.$0);
      needle_first$197 = $StringView$$unsafe_charcode_at(needle$196, 0);
      i$198 = haystack_len$193 - needle_len$195;
      while (1) {
        int32_t _tmp$1425 = i$198;
        if (_tmp$1425 >= 0) {
          int32_t _tmp$1430;
          while (1) {
            int32_t _tmp$1428 = i$198;
            int32_t _if_result$3394;
            if (_tmp$1428 >= 0) {
              int32_t _tmp$1427 = i$198;
              int32_t _tmp$1426;
              moonbit_incref(haystack$194.$0);
              _tmp$1426
              = $StringView$$unsafe_charcode_at(
                haystack$194, _tmp$1427
              );
              _if_result$3394 = _tmp$1426 != needle_first$197;
            } else {
              _if_result$3394 = 0;
            }
            if (_if_result$3394) {
              int32_t _tmp$1429 = i$198;
              i$198 = _tmp$1429 - 1;
              continue;
            }
            break;
          }
          _tmp$1430 = i$198;
          if (_tmp$1430 >= 0) {
            int32_t j$200 = 1;
            int32_t _tmp$1437;
            while (1) {
              if (j$200 < needle_len$195) {
                int32_t _tmp$1434 = i$198;
                int32_t _tmp$1433 = _tmp$1434 + j$200;
                int32_t _tmp$1431;
                int32_t _tmp$1432;
                int32_t _tmp$1435;
                moonbit_incref(haystack$194.$0);
                _tmp$1431
                = $StringView$$unsafe_charcode_at(
                  haystack$194, _tmp$1433
                );
                moonbit_incref(needle$196.$0);
                _tmp$1432
                = $StringView$$unsafe_charcode_at(
                  needle$196, j$200
                );
                if (_tmp$1431 != _tmp$1432) {
                  break;
                }
                _tmp$1435 = j$200 + 1;
                j$200 = _tmp$1435;
                continue;
              } else {
                int32_t _tmp$1436;
                moonbit_decref(needle$196.$0);
                moonbit_decref(haystack$194.$0);
                _tmp$1436 = i$198;
                return (int64_t)_tmp$1436;
              }
              break;
            }
            _tmp$1437 = i$198;
            i$198 = _tmp$1437 - 1;
          }
          continue;
        } else {
          moonbit_decref(needle$196.$0);
          moonbit_decref(haystack$194.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$196.$0);
      moonbit_decref(haystack$194.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$196.$0);
    moonbit_decref(haystack$194.$0);
    return (int64_t)haystack_len$193;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$183,
  struct $StringView needle$185
) {
  int32_t haystack_len$182;
  int32_t needle_len$184;
  moonbit_incref(haystack$183.$0);
  haystack_len$182 = $StringView$$length(haystack$183);
  moonbit_incref(needle$185.$0);
  needle_len$184 = $StringView$$length(needle$185);
  if (needle_len$184 > 0) {
    if (haystack_len$182 >= needle_len$184) {
      int32_t* skip_table$186 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$184);
      int32_t _tmp$1415 = needle_len$184 - 1;
      int32_t i$187 = _tmp$1415;
      int32_t _tmp$1424;
      int32_t i$189;
      while (1) {
        if (i$187 > 0) {
          int32_t _tmp$1413;
          int32_t _tmp$1412;
          int32_t _tmp$1414;
          moonbit_incref(needle$185.$0);
          _tmp$1413 = $StringView$$unsafe_charcode_at(needle$185, i$187);
          _tmp$1412 = _tmp$1413 & 255;
          if (
            _tmp$1412 < 0
            || _tmp$1412 >= Moonbit_array_length(skip_table$186)
          ) {
            moonbit_panic();
          }
          skip_table$186[_tmp$1412] = i$187;
          _tmp$1414 = i$187 - 1;
          i$187 = _tmp$1414;
          continue;
        }
        break;
      }
      _tmp$1424 = haystack_len$182 - needle_len$184;
      i$189 = _tmp$1424;
      while (1) {
        if (i$189 >= 0) {
          int32_t j$190 = 0;
          int32_t _tmp$1423;
          int32_t _tmp$1422;
          int32_t _tmp$1421;
          int32_t _tmp$1420;
          while (1) {
            if (j$190 < needle_len$184) {
              int32_t _tmp$1418 = i$189 + j$190;
              int32_t _tmp$1416;
              int32_t _tmp$1417;
              int32_t _tmp$1419;
              moonbit_incref(haystack$183.$0);
              _tmp$1416
              = $StringView$$unsafe_charcode_at(
                haystack$183, _tmp$1418
              );
              moonbit_incref(needle$185.$0);
              _tmp$1417 = $StringView$$unsafe_charcode_at(needle$185, j$190);
              if (_tmp$1416 != _tmp$1417) {
                break;
              }
              _tmp$1419 = j$190 + 1;
              j$190 = _tmp$1419;
              continue;
            } else {
              moonbit_decref(skip_table$186);
              moonbit_decref(needle$185.$0);
              moonbit_decref(haystack$183.$0);
              return (int64_t)i$189;
            }
            break;
          }
          moonbit_incref(haystack$183.$0);
          _tmp$1423 = $StringView$$unsafe_charcode_at(haystack$183, i$189);
          _tmp$1422 = _tmp$1423 & 255;
          if (
            _tmp$1422 < 0
            || _tmp$1422 >= Moonbit_array_length(skip_table$186)
          ) {
            moonbit_panic();
          }
          _tmp$1421 = (int32_t)skip_table$186[_tmp$1422];
          _tmp$1420 = i$189 - _tmp$1421;
          i$189 = _tmp$1420;
          continue;
        } else {
          moonbit_decref(skip_table$186);
          moonbit_decref(needle$185.$0);
          moonbit_decref(haystack$183.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$185.$0);
      moonbit_decref(haystack$183.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$185.$0);
    moonbit_decref(haystack$183.$0);
    return (int64_t)haystack_len$182;
  }
}

int64_t $StringView$$find(
  struct $StringView self$181,
  struct $StringView str$180
) {
  int32_t _tmp$1411;
  moonbit_incref(str$180.$0);
  _tmp$1411 = $StringView$$length(str$180);
  if (_tmp$1411 <= 4) {
    return $moonbitlang$core$builtin$brute_force_find(self$181, str$180);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_find(
             self$181, str$180
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$170,
  struct $StringView needle$172
) {
  int32_t haystack_len$169;
  int32_t needle_len$171;
  moonbit_incref(haystack$170.$0);
  haystack_len$169 = $StringView$$length(haystack$170);
  moonbit_incref(needle$172.$0);
  needle_len$171 = $StringView$$length(needle$172);
  if (needle_len$171 > 0) {
    if (haystack_len$169 >= needle_len$171) {
      int32_t needle_first$173;
      int32_t forward_len$174;
      int32_t i$175;
      moonbit_incref(needle$172.$0);
      needle_first$173 = $StringView$$unsafe_charcode_at(needle$172, 0);
      forward_len$174 = haystack_len$169 - needle_len$171;
      i$175 = 0;
      while (1) {
        int32_t _tmp$1398 = i$175;
        if (_tmp$1398 <= forward_len$174) {
          int32_t _tmp$1403;
          while (1) {
            int32_t _tmp$1401 = i$175;
            int32_t _if_result$3401;
            if (_tmp$1401 <= forward_len$174) {
              int32_t _tmp$1400 = i$175;
              int32_t _tmp$1399;
              moonbit_incref(haystack$170.$0);
              _tmp$1399
              = $StringView$$unsafe_charcode_at(
                haystack$170, _tmp$1400
              );
              _if_result$3401 = _tmp$1399 != needle_first$173;
            } else {
              _if_result$3401 = 0;
            }
            if (_if_result$3401) {
              int32_t _tmp$1402 = i$175;
              i$175 = _tmp$1402 + 1;
              continue;
            }
            break;
          }
          _tmp$1403 = i$175;
          if (_tmp$1403 <= forward_len$174) {
            int32_t j$177 = 1;
            int32_t _tmp$1410;
            while (1) {
              if (j$177 < needle_len$171) {
                int32_t _tmp$1407 = i$175;
                int32_t _tmp$1406 = _tmp$1407 + j$177;
                int32_t _tmp$1404;
                int32_t _tmp$1405;
                int32_t _tmp$1408;
                moonbit_incref(haystack$170.$0);
                _tmp$1404
                = $StringView$$unsafe_charcode_at(
                  haystack$170, _tmp$1406
                );
                moonbit_incref(needle$172.$0);
                _tmp$1405
                = $StringView$$unsafe_charcode_at(
                  needle$172, j$177
                );
                if (_tmp$1404 != _tmp$1405) {
                  break;
                }
                _tmp$1408 = j$177 + 1;
                j$177 = _tmp$1408;
                continue;
              } else {
                int32_t _tmp$1409;
                moonbit_decref(needle$172.$0);
                moonbit_decref(haystack$170.$0);
                _tmp$1409 = i$175;
                return (int64_t)_tmp$1409;
              }
              break;
            }
            _tmp$1410 = i$175;
            i$175 = _tmp$1410 + 1;
          }
          continue;
        } else {
          moonbit_decref(needle$172.$0);
          moonbit_decref(haystack$170.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$172.$0);
      moonbit_decref(haystack$170.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$172.$0);
    moonbit_decref(haystack$170.$0);
    return $moonbitlang$core$builtin$brute_force_find$constr$168;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$156,
  struct $StringView needle$158
) {
  int32_t haystack_len$155;
  int32_t needle_len$157;
  moonbit_incref(haystack$156.$0);
  haystack_len$155 = $StringView$$length(haystack$156);
  moonbit_incref(needle$158.$0);
  needle_len$157 = $StringView$$length(needle$158);
  if (needle_len$157 > 0) {
    if (haystack_len$155 >= needle_len$157) {
      int32_t* skip_table$159 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$157);
      int32_t _end4301$160 = needle_len$157 - 1;
      int32_t i$161 = 0;
      int32_t i$163;
      while (1) {
        if (i$161 < _end4301$160) {
          int32_t _tmp$1385;
          int32_t _tmp$1382;
          int32_t _tmp$1384;
          int32_t _tmp$1383;
          int32_t _tmp$1386;
          moonbit_incref(needle$158.$0);
          _tmp$1385 = $StringView$$unsafe_charcode_at(needle$158, i$161);
          _tmp$1382 = _tmp$1385 & 255;
          _tmp$1384 = needle_len$157 - 1;
          _tmp$1383 = _tmp$1384 - i$161;
          if (
            _tmp$1382 < 0
            || _tmp$1382 >= Moonbit_array_length(skip_table$159)
          ) {
            moonbit_panic();
          }
          skip_table$159[_tmp$1382] = _tmp$1383;
          _tmp$1386 = i$161 + 1;
          i$161 = _tmp$1386;
          continue;
        }
        break;
      }
      i$163 = 0;
      while (1) {
        int32_t _tmp$1387 = haystack_len$155 - needle_len$157;
        if (i$163 <= _tmp$1387) {
          int32_t _end4307$164 = needle_len$157 - 1;
          int32_t j$165 = 0;
          int32_t _tmp$1397;
          int32_t _tmp$1396;
          int32_t _tmp$1395;
          int32_t _tmp$1394;
          int32_t _tmp$1393;
          int32_t _tmp$1392;
          while (1) {
            if (j$165 <= _end4307$164) {
              int32_t _tmp$1390 = i$163 + j$165;
              int32_t _tmp$1388;
              int32_t _tmp$1389;
              int32_t _tmp$1391;
              moonbit_incref(haystack$156.$0);
              _tmp$1388
              = $StringView$$unsafe_charcode_at(
                haystack$156, _tmp$1390
              );
              moonbit_incref(needle$158.$0);
              _tmp$1389 = $StringView$$unsafe_charcode_at(needle$158, j$165);
              if (_tmp$1388 != _tmp$1389) {
                break;
              }
              _tmp$1391 = j$165 + 1;
              j$165 = _tmp$1391;
              continue;
            } else {
              moonbit_decref(skip_table$159);
              moonbit_decref(needle$158.$0);
              moonbit_decref(haystack$156.$0);
              return (int64_t)i$163;
            }
            break;
          }
          _tmp$1397 = i$163 + needle_len$157;
          _tmp$1396 = _tmp$1397 - 1;
          moonbit_incref(haystack$156.$0);
          _tmp$1395
          = $StringView$$unsafe_charcode_at(
            haystack$156, _tmp$1396
          );
          _tmp$1394 = _tmp$1395 & 255;
          if (
            _tmp$1394 < 0
            || _tmp$1394 >= Moonbit_array_length(skip_table$159)
          ) {
            moonbit_panic();
          }
          _tmp$1393 = (int32_t)skip_table$159[_tmp$1394];
          _tmp$1392 = i$163 + _tmp$1393;
          i$163 = _tmp$1392;
          continue;
        } else {
          moonbit_decref(skip_table$159);
          moonbit_decref(needle$158.$0);
          moonbit_decref(haystack$156.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$158.$0);
      moonbit_decref(haystack$156.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$158.$0);
    moonbit_decref(haystack$156.$0);
    return $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$154;
  }
}

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$152,
  int32_t index$153
) {
  moonbit_string_t _field$2968 = self$152.$0;
  moonbit_string_t str$1379 = _field$2968;
  int32_t _field$2967 = self$152.$1;
  int32_t start$1381 = _field$2967;
  int32_t _tmp$1380 = start$1381 + index$153;
  int32_t _tmp$2966 = str$1379[_tmp$1380];
  moonbit_decref(str$1379);
  return _tmp$2966;
}

int32_t $StringView$$length(struct $StringView self$151) {
  int32_t end$1377 = self$151.$2;
  int32_t _field$2969 = self$151.$1;
  int32_t start$1378;
  moonbit_decref(self$151.$0);
  start$1378 = _field$2969;
  return end$1377 - start$1378;
}

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* $$moonbitlang$core$builtin$Array$$at$3(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$149,
  int32_t index$150
) {
  int32_t len$148 = self$149->$1;
  int32_t _if_result$3406;
  if (index$150 >= 0) {
    _if_result$3406 = index$150 < len$148;
  } else {
    _if_result$3406 = 0;
  }
  if (_if_result$3406) {
    struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** _tmp$1376 =
      $$moonbitlang$core$builtin$Array$$buffer$5(self$149);
    struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$2970;
    if (index$150 < 0 || index$150 >= Moonbit_array_length(_tmp$1376)) {
      moonbit_panic();
    }
    _tmp$2970
    = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)_tmp$1376[
        index$150
      ];
    if (_tmp$2970) {
      moonbit_incref(_tmp$2970);
    }
    moonbit_decref(_tmp$1376);
    return _tmp$2970;
  } else {
    moonbit_decref(self$149);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$at$2(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$146,
  int32_t index$147
) {
  int32_t len$145 = self$146->$1;
  int32_t _if_result$3407;
  if (index$147 >= 0) {
    _if_result$3407 = index$147 < len$145;
  } else {
    _if_result$3407 = 0;
  }
  if (_if_result$3407) {
    int32_t* _tmp$1375 = $$moonbitlang$core$builtin$Array$$buffer$4(self$146);
    int32_t _tmp$2971;
    if (index$147 < 0 || index$147 >= Moonbit_array_length(_tmp$1375)) {
      moonbit_panic();
    }
    _tmp$2971 = (int32_t)_tmp$1375[index$147];
    moonbit_decref(_tmp$1375);
    return _tmp$2971;
  } else {
    moonbit_decref(self$146);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$at$1(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$143,
  int32_t index$144
) {
  int32_t len$142 = self$143->$1;
  int32_t _if_result$3408;
  if (index$144 >= 0) {
    _if_result$3408 = index$144 < len$142;
  } else {
    _if_result$3408 = 0;
  }
  if (_if_result$3408) {
    int32_t* _tmp$1374 = $$moonbitlang$core$builtin$Array$$buffer$3(self$143);
    int32_t _tmp$2972;
    if (index$144 < 0 || index$144 >= Moonbit_array_length(_tmp$1374)) {
      moonbit_panic();
    }
    _tmp$2972 = (int32_t)_tmp$1374[index$144];
    moonbit_decref(_tmp$1374);
    return _tmp$2972;
  } else {
    moonbit_decref(self$143);
    moonbit_panic();
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$140,
  int32_t index$141
) {
  int32_t len$139 = self$140->$1;
  int32_t _if_result$3409;
  if (index$141 >= 0) {
    _if_result$3409 = index$141 < len$139;
  } else {
    _if_result$3409 = 0;
  }
  if (_if_result$3409) {
    moonbit_string_t* _tmp$1373 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$140);
    moonbit_string_t _tmp$2973;
    if (index$141 < 0 || index$141 >= Moonbit_array_length(_tmp$1373)) {
      moonbit_panic();
    }
    _tmp$2973 = (moonbit_string_t)_tmp$1373[index$141];
    moonbit_incref(_tmp$2973);
    moonbit_decref(_tmp$1373);
    return _tmp$2973;
  } else {
    moonbit_decref(self$140);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$5(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$138
) {
  int32_t _field$2974 = self$138->$1;
  moonbit_decref(self$138);
  return _field$2974;
}

int32_t $$moonbitlang$core$builtin$Array$$length$4(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$137
) {
  int32_t _field$2975 = self$137->$1;
  moonbit_decref(self$137);
  return _field$2975;
}

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$Unit$3e$* self$136
) {
  int32_t _field$2976 = self$136->$1;
  moonbit_decref(self$136);
  return _field$2976;
}

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$135
) {
  int32_t _field$2977 = self$135->$1;
  moonbit_decref(self$135);
  return _field$2977;
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$134
) {
  int32_t _field$2978 = self$134->$1;
  moonbit_decref(self$134);
  return _field$2978;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$133
) {
  int32_t _field$2979 = self$133->$1;
  moonbit_decref(self$133);
  return _field$2979;
}

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$5(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$132
) {
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** _field$2980 =
    self$132->$0;
  int32_t _cnt$3122 = Moonbit_object_header(self$132)->rc;
  if (_cnt$3122 > 1) {
    int32_t _new_cnt$3123;
    moonbit_incref(_field$2980);
    _new_cnt$3123 = _cnt$3122 - 1;
    Moonbit_object_header(self$132)->rc = _new_cnt$3123;
  } else if (_cnt$3122 == 1) {
    moonbit_free(self$132);
  }
  return _field$2980;
}

int32_t* $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$Char$3e$* self$131
) {
  int32_t* _field$2981 = self$131->$0;
  int32_t _cnt$3124 = Moonbit_object_header(self$131)->rc;
  if (_cnt$3124 > 1) {
    int32_t _new_cnt$3125;
    moonbit_incref(_field$2981);
    _new_cnt$3125 = _cnt$3124 - 1;
    Moonbit_object_header(self$131)->rc = _new_cnt$3125;
  } else if (_cnt$3124 == 1) {
    moonbit_free(self$131);
  }
  return _field$2981;
}

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$130
) {
  int32_t* _field$2982 = self$130->$0;
  int32_t _cnt$3126 = Moonbit_object_header(self$130)->rc;
  if (_cnt$3126 > 1) {
    int32_t _new_cnt$3127;
    moonbit_incref(_field$2982);
    _new_cnt$3127 = _cnt$3126 - 1;
    Moonbit_object_header(self$130)->rc = _new_cnt$3127;
  } else if (_cnt$3126 == 1) {
    moonbit_free(self$130);
  }
  return _field$2982;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$129
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2983 =
    self$129->$0;
  int32_t _cnt$3128 = Moonbit_object_header(self$129)->rc;
  if (_cnt$3128 > 1) {
    int32_t _new_cnt$3129;
    moonbit_incref(_field$2983);
    _new_cnt$3129 = _cnt$3128 - 1;
    Moonbit_object_header(self$129)->rc = _new_cnt$3129;
  } else if (_cnt$3128 == 1) {
    moonbit_free(self$129);
  }
  return _field$2983;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$128
) {
  moonbit_string_t* _field$2984 = self$128->$0;
  int32_t _cnt$3130 = Moonbit_object_header(self$128)->rc;
  if (_cnt$3130 > 1) {
    int32_t _new_cnt$3131;
    moonbit_incref(_field$2984);
    _new_cnt$3131 = _cnt$3130 - 1;
    Moonbit_object_header(self$128)->rc = _new_cnt$3131;
  } else if (_cnt$3130 == 1) {
    moonbit_free(self$128);
  }
  return _field$2984;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$127
) {
  struct $$3c$String$2a$Int$3e$** _field$2985 = self$127->$0;
  int32_t _cnt$3132 = Moonbit_object_header(self$127)->rc;
  if (_cnt$3132 > 1) {
    int32_t _new_cnt$3133;
    moonbit_incref(_field$2985);
    _new_cnt$3133 = _cnt$3132 - 1;
    Moonbit_object_header(self$127)->rc = _new_cnt$3133;
  } else if (_cnt$3132 == 1) {
    moonbit_free(self$127);
  }
  return _field$2985;
}

moonbit_string_t $String$$escape(moonbit_string_t self$126) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$125 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1372;
  moonbit_incref(buf$125);
  _tmp$1372
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$125
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$126, _tmp$1372);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$125);
}

int32_t $moonbitlang$core$builtin$op_notequal$5(int32_t x$123, int32_t y$124) {
  int32_t _tmp$1371 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$3(x$123, y$124);
  return !_tmp$1371;
}

int32_t $moonbitlang$core$builtin$op_notequal$4(
  moonbit_string_t x$121,
  moonbit_string_t y$122
) {
  int32_t _tmp$1370 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$2(x$121, y$122);
  return !_tmp$1370;
}

int32_t $moonbitlang$core$builtin$op_notequal$3(void* x$119, void* y$120) {
  int32_t _tmp$1369 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$1(x$119, y$120);
  return !_tmp$1369;
}

int32_t $moonbitlang$core$builtin$op_notequal$2(int64_t x$117, int64_t y$118) {
  int32_t _tmp$1368 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$0(x$117, y$118);
  return !_tmp$1368;
}

int32_t $moonbitlang$core$builtin$op_notequal$1(
  moonbit_string_t x$115,
  moonbit_string_t y$116
) {
  int32_t _tmp$2986 = moonbit_val_array_equal(x$115, y$116);
  int32_t _tmp$1367;
  moonbit_decref(x$115);
  moonbit_decref(y$116);
  _tmp$1367 = _tmp$2986;
  return !_tmp$1367;
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$113, int32_t y$114) {
  int32_t _tmp$1366 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$113, y$114
    );
  return !_tmp$1366;
}

int32_t $Int$$is_trailing_surrogate(int32_t self$112) {
  if (56320 <= self$112) {
    return self$112 <= 57343;
  } else {
    return 0;
  }
}

int32_t $Int$$is_leading_surrogate(int32_t self$111) {
  if (55296 <= self$111) {
    return self$111 <= 56319;
  } else {
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$108,
  int32_t ch$110
) {
  int32_t len$1361 = self$108->$1;
  int32_t _tmp$1360 = len$1361 + 4;
  moonbit_bytes_t _field$2987;
  moonbit_bytes_t data$1364;
  int32_t len$1365;
  int32_t inc$109;
  int32_t len$1363;
  int32_t _tmp$1362;
  moonbit_incref(self$108);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$108, _tmp$1360
  );
  _field$2987 = self$108->$0;
  data$1364 = _field$2987;
  len$1365 = self$108->$1;
  moonbit_incref(data$1364);
  inc$109 = $FixedArray$$set_utf16le_char(data$1364, len$1365, ch$110);
  len$1363 = self$108->$1;
  _tmp$1362 = len$1363 + inc$109;
  self$108->$1 = _tmp$1362;
  moonbit_decref(self$108);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$103,
  int32_t required$104
) {
  moonbit_bytes_t _field$2991 = self$103->$0;
  moonbit_bytes_t data$1359 = _field$2991;
  int32_t _tmp$2990 = Moonbit_array_length(data$1359);
  int32_t current_len$102 = _tmp$2990;
  int32_t enough_space$105;
  int32_t _tmp$1357;
  int32_t _tmp$1358;
  moonbit_bytes_t new_data$107;
  moonbit_bytes_t _field$2989;
  moonbit_bytes_t data$1355;
  int32_t len$1356;
  moonbit_bytes_t _old$2988;
  if (required$104 <= current_len$102) {
    moonbit_decref(self$103);
    return 0;
  }
  enough_space$105 = current_len$102;
  while (1) {
    int32_t _tmp$1353 = enough_space$105;
    if (_tmp$1353 < required$104) {
      int32_t _tmp$1354 = enough_space$105;
      enough_space$105 = _tmp$1354 * 2;
      continue;
    }
    break;
  }
  _tmp$1357 = enough_space$105;
  _tmp$1358 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$107 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1357, _tmp$1358);
  _field$2989 = self$103->$0;
  data$1355 = _field$2989;
  len$1356 = self$103->$1;
  moonbit_incref(data$1355);
  moonbit_incref(new_data$107);
  $FixedArray$$unsafe_blit$0(new_data$107, 0, data$1355, 0, len$1356);
  _old$2988 = self$103->$0;
  moonbit_decref(_old$2988);
  self$103->$0 = new_data$107;
  moonbit_decref(self$103);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default() {
  return 0;
}

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$97,
  int32_t offset$98,
  int32_t value$96
) {
  uint32_t code$95 = $Char$$to_uint(value$96);
  if (code$95 < 65536u) {
    uint32_t _tmp$1336 = code$95 & 255u;
    int32_t _tmp$1335 = $UInt$$to_byte(_tmp$1336);
    int32_t _tmp$1337;
    uint32_t _tmp$1339;
    int32_t _tmp$1338;
    if (offset$98 < 0 || offset$98 >= Moonbit_array_length(self$97)) {
      moonbit_panic();
    }
    self$97[offset$98] = _tmp$1335;
    _tmp$1337 = offset$98 + 1;
    _tmp$1339 = code$95 >> 8;
    _tmp$1338 = $UInt$$to_byte(_tmp$1339);
    if (_tmp$1337 < 0 || _tmp$1337 >= Moonbit_array_length(self$97)) {
      moonbit_panic();
    }
    self$97[_tmp$1337] = _tmp$1338;
    moonbit_decref(self$97);
    return 2;
  } else if (code$95 < 1114112u) {
    uint32_t hi$99 = code$95 - 65536u;
    uint32_t _tmp$1352 = hi$99 >> 10;
    uint32_t lo$100 = _tmp$1352 | 55296u;
    uint32_t _tmp$1351 = hi$99 & 1023u;
    uint32_t hi$101 = _tmp$1351 | 56320u;
    uint32_t _tmp$1341 = lo$100 & 255u;
    int32_t _tmp$1340 = $UInt$$to_byte(_tmp$1341);
    int32_t _tmp$1342;
    uint32_t _tmp$1344;
    int32_t _tmp$1343;
    int32_t _tmp$1345;
    uint32_t _tmp$1347;
    int32_t _tmp$1346;
    int32_t _tmp$1348;
    uint32_t _tmp$1350;
    int32_t _tmp$1349;
    if (offset$98 < 0 || offset$98 >= Moonbit_array_length(self$97)) {
      moonbit_panic();
    }
    self$97[offset$98] = _tmp$1340;
    _tmp$1342 = offset$98 + 1;
    _tmp$1344 = lo$100 >> 8;
    _tmp$1343 = $UInt$$to_byte(_tmp$1344);
    if (_tmp$1342 < 0 || _tmp$1342 >= Moonbit_array_length(self$97)) {
      moonbit_panic();
    }
    self$97[_tmp$1342] = _tmp$1343;
    _tmp$1345 = offset$98 + 2;
    _tmp$1347 = hi$101 & 255u;
    _tmp$1346 = $UInt$$to_byte(_tmp$1347);
    if (_tmp$1345 < 0 || _tmp$1345 >= Moonbit_array_length(self$97)) {
      moonbit_panic();
    }
    self$97[_tmp$1345] = _tmp$1346;
    _tmp$1348 = offset$98 + 3;
    _tmp$1350 = hi$101 >> 8;
    _tmp$1349 = $UInt$$to_byte(_tmp$1350);
    if (_tmp$1348 < 0 || _tmp$1348 >= Moonbit_array_length(self$97)) {
      moonbit_panic();
    }
    self$97[_tmp$1348] = _tmp$1349;
    moonbit_decref(self$97);
    return 4;
  } else {
    moonbit_decref(self$97);
    return $moonbitlang$core$builtin$abort$1(
             (moonbit_string_t)moonbit_string_literal_122.data,
               (moonbit_string_t)moonbit_string_literal_123.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$94) {
  int32_t _tmp$1334 = *(int32_t*)&self$94;
  return _tmp$1334 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$93) {
  int32_t _tmp$1333 = self$93;
  return *(uint32_t*)&_tmp$1333;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$92
) {
  moonbit_bytes_t _field$2993 = self$92->$0;
  moonbit_bytes_t data$1332 = _field$2993;
  moonbit_bytes_t _tmp$1329;
  int32_t _field$2992;
  int32_t len$1331;
  int64_t _tmp$1330;
  moonbit_incref(data$1332);
  _tmp$1329 = data$1332;
  _field$2992 = self$92->$1;
  moonbit_decref(self$92);
  len$1331 = _field$2992;
  _tmp$1330 = (int64_t)len$1331;
  return $Bytes$$to_unchecked_string$inner(_tmp$1329, 0, _tmp$1330);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$87,
  int32_t offset$91,
  int64_t length$89
) {
  int32_t len$86 = Moonbit_array_length(self$87);
  int32_t length$88;
  int32_t _if_result$3411;
  if (length$89 == 4294967296ll) {
    length$88 = len$86 - offset$91;
  } else {
    int64_t _Some$90 = length$89;
    length$88 = (int32_t)_Some$90;
  }
  if (offset$91 >= 0) {
    if (length$88 >= 0) {
      int32_t _tmp$1328 = offset$91 + length$88;
      _if_result$3411 = _tmp$1328 <= len$86;
    } else {
      _if_result$3411 = 0;
    }
  } else {
    _if_result$3411 = 0;
  }
  if (_if_result$3411) {
    return $moonbitlang$core$builtin$unsafe_sub_string(
             self$87, offset$91, length$88
           );
  } else {
    moonbit_decref(self$87);
    moonbit_panic();
  }
}

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$84
) {
  int32_t initial$83;
  moonbit_bytes_t data$85;
  struct $$moonbitlang$core$builtin$StringBuilder* _block$3412;
  if (size_hint$84 < 1) {
    initial$83 = 1;
  } else {
    initial$83 = size_hint$84;
  }
  data$85 = (moonbit_bytes_t)moonbit_make_bytes(initial$83, 0);
  _block$3412
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$3412)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$3412->$0 = data$85;
  _block$3412->$1 = 0;
  return _block$3412;
}

int32_t $Byte$$to_char(int32_t self$82) {
  int32_t _tmp$1327 = (int32_t)self$82;
  return _tmp$1327;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$77,
  int32_t dst_offset$78,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$79,
  int32_t src_offset$80,
  int32_t len$81
) {
  $FixedArray$$unsafe_blit$3(
    dst$77, dst_offset$78, src$79, src_offset$80, len$81
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$72,
  int32_t dst_offset$73,
  struct $$3c$String$2a$Int$3e$** src$74,
  int32_t src_offset$75,
  int32_t len$76
) {
  $FixedArray$$unsafe_blit$2(
    dst$72, dst_offset$73, src$74, src_offset$75, len$76
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$67,
  int32_t dst_offset$68,
  moonbit_string_t* src$69,
  int32_t src_offset$70,
  int32_t len$71
) {
  $FixedArray$$unsafe_blit$1(
    dst$67, dst_offset$68, src$69, src_offset$70, len$71
  );
  return 0;
}

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$58,
  int32_t dst_offset$60,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$59,
  int32_t src_offset$61,
  int32_t len$63
) {
  int32_t _if_result$3413;
  if (dst$58 == src$59) {
    _if_result$3413 = dst_offset$60 < src_offset$61;
  } else {
    _if_result$3413 = 0;
  }
  if (_if_result$3413) {
    int32_t i$62 = 0;
    while (1) {
      if (i$62 < len$63) {
        int32_t _tmp$1318 = dst_offset$60 + i$62;
        int32_t _tmp$1320 = src_offset$61 + i$62;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2995;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1319;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2994;
        int32_t _tmp$1321;
        if (_tmp$1320 < 0 || _tmp$1320 >= Moonbit_array_length(src$59)) {
          moonbit_panic();
        }
        _tmp$2995
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$59[
            _tmp$1320
          ];
        _tmp$1319 = _tmp$2995;
        if (_tmp$1318 < 0 || _tmp$1318 >= Moonbit_array_length(dst$58)) {
          moonbit_panic();
        }
        _old$2994
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$58[
            _tmp$1318
          ];
        if (_tmp$1319) {
          moonbit_incref(_tmp$1319);
        }
        if (_old$2994) {
          moonbit_decref(_old$2994);
        }
        dst$58[_tmp$1318] = _tmp$1319;
        _tmp$1321 = i$62 + 1;
        i$62 = _tmp$1321;
        continue;
      } else {
        moonbit_decref(src$59);
        moonbit_decref(dst$58);
      }
      break;
    }
  } else {
    int32_t _tmp$1326 = len$63 - 1;
    int32_t i$65 = _tmp$1326;
    while (1) {
      if (i$65 >= 0) {
        int32_t _tmp$1322 = dst_offset$60 + i$65;
        int32_t _tmp$1324 = src_offset$61 + i$65;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2997;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1323;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2996;
        int32_t _tmp$1325;
        if (_tmp$1324 < 0 || _tmp$1324 >= Moonbit_array_length(src$59)) {
          moonbit_panic();
        }
        _tmp$2997
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$59[
            _tmp$1324
          ];
        _tmp$1323 = _tmp$2997;
        if (_tmp$1322 < 0 || _tmp$1322 >= Moonbit_array_length(dst$58)) {
          moonbit_panic();
        }
        _old$2996
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$58[
            _tmp$1322
          ];
        if (_tmp$1323) {
          moonbit_incref(_tmp$1323);
        }
        if (_old$2996) {
          moonbit_decref(_old$2996);
        }
        dst$58[_tmp$1322] = _tmp$1323;
        _tmp$1325 = i$65 - 1;
        i$65 = _tmp$1325;
        continue;
      } else {
        moonbit_decref(src$59);
        moonbit_decref(dst$58);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$49,
  int32_t dst_offset$51,
  struct $$3c$String$2a$Int$3e$** src$50,
  int32_t src_offset$52,
  int32_t len$54
) {
  int32_t _if_result$3416;
  if (dst$49 == src$50) {
    _if_result$3416 = dst_offset$51 < src_offset$52;
  } else {
    _if_result$3416 = 0;
  }
  if (_if_result$3416) {
    int32_t i$53 = 0;
    while (1) {
      if (i$53 < len$54) {
        int32_t _tmp$1309 = dst_offset$51 + i$53;
        int32_t _tmp$1311 = src_offset$52 + i$53;
        struct $$3c$String$2a$Int$3e$* _tmp$2999;
        struct $$3c$String$2a$Int$3e$* _tmp$1310;
        struct $$3c$String$2a$Int$3e$* _old$2998;
        int32_t _tmp$1312;
        if (_tmp$1311 < 0 || _tmp$1311 >= Moonbit_array_length(src$50)) {
          moonbit_panic();
        }
        _tmp$2999 = (struct $$3c$String$2a$Int$3e$*)src$50[_tmp$1311];
        _tmp$1310 = _tmp$2999;
        if (_tmp$1309 < 0 || _tmp$1309 >= Moonbit_array_length(dst$49)) {
          moonbit_panic();
        }
        _old$2998 = (struct $$3c$String$2a$Int$3e$*)dst$49[_tmp$1309];
        if (_tmp$1310) {
          moonbit_incref(_tmp$1310);
        }
        if (_old$2998) {
          moonbit_decref(_old$2998);
        }
        dst$49[_tmp$1309] = _tmp$1310;
        _tmp$1312 = i$53 + 1;
        i$53 = _tmp$1312;
        continue;
      } else {
        moonbit_decref(src$50);
        moonbit_decref(dst$49);
      }
      break;
    }
  } else {
    int32_t _tmp$1317 = len$54 - 1;
    int32_t i$56 = _tmp$1317;
    while (1) {
      if (i$56 >= 0) {
        int32_t _tmp$1313 = dst_offset$51 + i$56;
        int32_t _tmp$1315 = src_offset$52 + i$56;
        struct $$3c$String$2a$Int$3e$* _tmp$3001;
        struct $$3c$String$2a$Int$3e$* _tmp$1314;
        struct $$3c$String$2a$Int$3e$* _old$3000;
        int32_t _tmp$1316;
        if (_tmp$1315 < 0 || _tmp$1315 >= Moonbit_array_length(src$50)) {
          moonbit_panic();
        }
        _tmp$3001 = (struct $$3c$String$2a$Int$3e$*)src$50[_tmp$1315];
        _tmp$1314 = _tmp$3001;
        if (_tmp$1313 < 0 || _tmp$1313 >= Moonbit_array_length(dst$49)) {
          moonbit_panic();
        }
        _old$3000 = (struct $$3c$String$2a$Int$3e$*)dst$49[_tmp$1313];
        if (_tmp$1314) {
          moonbit_incref(_tmp$1314);
        }
        if (_old$3000) {
          moonbit_decref(_old$3000);
        }
        dst$49[_tmp$1313] = _tmp$1314;
        _tmp$1316 = i$56 - 1;
        i$56 = _tmp$1316;
        continue;
      } else {
        moonbit_decref(src$50);
        moonbit_decref(dst$49);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$40,
  int32_t dst_offset$42,
  moonbit_string_t* src$41,
  int32_t src_offset$43,
  int32_t len$45
) {
  int32_t _if_result$3419;
  if (dst$40 == src$41) {
    _if_result$3419 = dst_offset$42 < src_offset$43;
  } else {
    _if_result$3419 = 0;
  }
  if (_if_result$3419) {
    int32_t i$44 = 0;
    while (1) {
      if (i$44 < len$45) {
        int32_t _tmp$1300 = dst_offset$42 + i$44;
        int32_t _tmp$1302 = src_offset$43 + i$44;
        moonbit_string_t _tmp$3003;
        moonbit_string_t _tmp$1301;
        moonbit_string_t _old$3002;
        int32_t _tmp$1303;
        if (_tmp$1302 < 0 || _tmp$1302 >= Moonbit_array_length(src$41)) {
          moonbit_panic();
        }
        _tmp$3003 = (moonbit_string_t)src$41[_tmp$1302];
        _tmp$1301 = _tmp$3003;
        if (_tmp$1300 < 0 || _tmp$1300 >= Moonbit_array_length(dst$40)) {
          moonbit_panic();
        }
        _old$3002 = (moonbit_string_t)dst$40[_tmp$1300];
        moonbit_incref(_tmp$1301);
        moonbit_decref(_old$3002);
        dst$40[_tmp$1300] = _tmp$1301;
        _tmp$1303 = i$44 + 1;
        i$44 = _tmp$1303;
        continue;
      } else {
        moonbit_decref(src$41);
        moonbit_decref(dst$40);
      }
      break;
    }
  } else {
    int32_t _tmp$1308 = len$45 - 1;
    int32_t i$47 = _tmp$1308;
    while (1) {
      if (i$47 >= 0) {
        int32_t _tmp$1304 = dst_offset$42 + i$47;
        int32_t _tmp$1306 = src_offset$43 + i$47;
        moonbit_string_t _tmp$3005;
        moonbit_string_t _tmp$1305;
        moonbit_string_t _old$3004;
        int32_t _tmp$1307;
        if (_tmp$1306 < 0 || _tmp$1306 >= Moonbit_array_length(src$41)) {
          moonbit_panic();
        }
        _tmp$3005 = (moonbit_string_t)src$41[_tmp$1306];
        _tmp$1305 = _tmp$3005;
        if (_tmp$1304 < 0 || _tmp$1304 >= Moonbit_array_length(dst$40)) {
          moonbit_panic();
        }
        _old$3004 = (moonbit_string_t)dst$40[_tmp$1304];
        moonbit_incref(_tmp$1305);
        moonbit_decref(_old$3004);
        dst$40[_tmp$1304] = _tmp$1305;
        _tmp$1307 = i$47 - 1;
        i$47 = _tmp$1307;
        continue;
      } else {
        moonbit_decref(src$41);
        moonbit_decref(dst$40);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$31,
  int32_t dst_offset$33,
  moonbit_bytes_t src$32,
  int32_t src_offset$34,
  int32_t len$36
) {
  int32_t _if_result$3422;
  if (dst$31 == src$32) {
    _if_result$3422 = dst_offset$33 < src_offset$34;
  } else {
    _if_result$3422 = 0;
  }
  if (_if_result$3422) {
    int32_t i$35 = 0;
    while (1) {
      if (i$35 < len$36) {
        int32_t _tmp$1291 = dst_offset$33 + i$35;
        int32_t _tmp$1293 = src_offset$34 + i$35;
        int32_t _tmp$1292;
        int32_t _tmp$1294;
        if (_tmp$1293 < 0 || _tmp$1293 >= Moonbit_array_length(src$32)) {
          moonbit_panic();
        }
        _tmp$1292 = (int32_t)src$32[_tmp$1293];
        if (_tmp$1291 < 0 || _tmp$1291 >= Moonbit_array_length(dst$31)) {
          moonbit_panic();
        }
        dst$31[_tmp$1291] = _tmp$1292;
        _tmp$1294 = i$35 + 1;
        i$35 = _tmp$1294;
        continue;
      } else {
        moonbit_decref(src$32);
        moonbit_decref(dst$31);
      }
      break;
    }
  } else {
    int32_t _tmp$1299 = len$36 - 1;
    int32_t i$38 = _tmp$1299;
    while (1) {
      if (i$38 >= 0) {
        int32_t _tmp$1295 = dst_offset$33 + i$38;
        int32_t _tmp$1297 = src_offset$34 + i$38;
        int32_t _tmp$1296;
        int32_t _tmp$1298;
        if (_tmp$1297 < 0 || _tmp$1297 >= Moonbit_array_length(src$32)) {
          moonbit_panic();
        }
        _tmp$1296 = (int32_t)src$32[_tmp$1297];
        if (_tmp$1295 < 0 || _tmp$1295 >= Moonbit_array_length(dst$31)) {
          moonbit_panic();
        }
        dst$31[_tmp$1295] = _tmp$1296;
        _tmp$1298 = i$38 - 1;
        i$38 = _tmp$1298;
        continue;
      } else {
        moonbit_decref(src$32);
        moonbit_decref(dst$31);
      }
      break;
    }
  }
  return 0;
}

int64_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$29,
  moonbit_string_t loc$30
) {
  moonbit_string_t _tmp$1290 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$29);
  moonbit_string_t _tmp$1288 =
    moonbit_add_string(
      _tmp$1290, (moonbit_string_t)moonbit_string_literal_124.data
    );
  moonbit_string_t _tmp$1289 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$30);
  moonbit_string_t _tmp$1287 = moonbit_add_string(_tmp$1288, _tmp$1289);
  moonbit_string_t _tmp$1286 =
    moonbit_add_string(
      _tmp$1287, (moonbit_string_t)moonbit_string_literal_125.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1286);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$27,
  moonbit_string_t loc$28
) {
  moonbit_string_t _tmp$1285 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$27);
  moonbit_string_t _tmp$1283 =
    moonbit_add_string(
      _tmp$1285, (moonbit_string_t)moonbit_string_literal_124.data
    );
  moonbit_string_t _tmp$1284 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$28);
  moonbit_string_t _tmp$1282 = moonbit_add_string(_tmp$1283, _tmp$1284);
  moonbit_string_t _tmp$1281 =
    moonbit_add_string(
      _tmp$1282, (moonbit_string_t)moonbit_string_literal_125.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1281);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$25,
  moonbit_string_t loc$26
) {
  moonbit_string_t _tmp$1280 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$25);
  moonbit_string_t _tmp$1278 =
    moonbit_add_string(
      _tmp$1280, (moonbit_string_t)moonbit_string_literal_124.data
    );
  moonbit_string_t _tmp$1279 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$26);
  moonbit_string_t _tmp$1277 = moonbit_add_string(_tmp$1278, _tmp$1279);
  moonbit_string_t _tmp$1276 =
    moonbit_add_string(
      _tmp$1277, (moonbit_string_t)moonbit_string_literal_125.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1276);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$23,
  moonbit_string_t loc$24
) {
  moonbit_string_t _tmp$1275 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$23);
  moonbit_string_t _tmp$1273 =
    moonbit_add_string(
      _tmp$1275, (moonbit_string_t)moonbit_string_literal_124.data
    );
  moonbit_string_t _tmp$1274 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$24);
  moonbit_string_t _tmp$1272 = moonbit_add_string(_tmp$1273, _tmp$1274);
  moonbit_string_t _tmp$1271 =
    moonbit_add_string(
      _tmp$1272, (moonbit_string_t)moonbit_string_literal_125.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1271);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Eq$$Unit$$equal(
  int32_t _discard_$21,
  int32_t _discard_$22
) {
  return 1;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$17,
  struct $$moonbitlang$core$builtin$Logger _x_5272$20
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$18 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$17;
  moonbit_string_t _field$3006 = _Failure$18->$0;
  int32_t _cnt$3134 = Moonbit_object_header(_Failure$18)->rc;
  moonbit_string_t _$2a$err_payload_5273$19;
  struct $$moonbitlang$core$builtin$Logger _bind$1270;
  if (_cnt$3134 > 1) {
    int32_t _new_cnt$3135;
    moonbit_incref(_field$3006);
    _new_cnt$3135 = _cnt$3134 - 1;
    Moonbit_object_header(_Failure$18)->rc = _new_cnt$3135;
  } else if (_cnt$3134 == 1) {
    moonbit_free(_Failure$18);
  }
  _$2a$err_payload_5273$19 = _field$3006;
  if (_x_5272$20.$1) {
    moonbit_incref(_x_5272$20.$1);
  }
  _x_5272$20.$0->$method_0(
    _x_5272$20.$1, (moonbit_string_t)moonbit_string_literal_126.data
  );
  if (_x_5272$20.$1) {
    moonbit_incref(_x_5272$20.$1);
  }
  $$moonbitlang$core$builtin$Logger$$write_object$1(
    _x_5272$20, _$2a$err_payload_5273$19
  );
  _bind$1270 = _x_5272$20;
  _bind$1270.$0->$method_0(
    _bind$1270.$1, (moonbit_string_t)moonbit_string_literal_106.data
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
  void* _x_5285$15,
  struct $$moonbitlang$core$builtin$Logger _x_5286$16
) {
  switch (Moonbit_object_tag(_x_5285$15)) {
    case 1: {
      _x_5286$16.$0->$method_0(
        _x_5286$16.$1, (moonbit_string_t)moonbit_string_literal_127.data
      );
      break;
    }
    default: {
      moonbit_decref(_x_5285$15);
      _x_5286$16.$0->$method_0(
        _x_5286$16.$1, (moonbit_string_t)moonbit_string_literal_128.data
      );
      break;
    }
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
  int32_t _x_5289$13,
  int32_t _x_5290$14
) {
  switch (_x_5289$13) {
    case 0: {
      switch (_x_5290$14) {
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
      switch (_x_5290$14) {
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

int32_t $$moonbitlang$core$builtin$Logger$$write_object$3(
  struct $$moonbitlang$core$builtin$Logger self$12,
  int64_t obj$11
) {
  $$moonbitlang$core$builtin$Show$$Option$$output$0(obj$11, self$12);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$write_object$2(
  struct $$moonbitlang$core$builtin$Logger self$10,
  int32_t obj$9
) {
  $$moonbitlang$core$builtin$Show$$Unit$$output(obj$9, self$10);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$write_object$1(
  struct $$moonbitlang$core$builtin$Logger self$8,
  moonbit_string_t obj$7
) {
  $$moonbitlang$core$builtin$Show$$String$$output(obj$7, self$8);
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

moonbit_string_t $Error$to_string(void* _e$1197) {
  switch (Moonbit_object_tag(_e$1197)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
               _e$1197
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
               _e$1197
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1197);
      return (moonbit_string_t)moonbit_string_literal_129.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1197
             );
      break;
    }
    
    case 5: {
      moonbit_decref(_e$1197);
      return (moonbit_string_t)moonbit_string_literal_130.data;
      break;
    }
    default: {
      moonbit_decref(_e$1197);
      return (moonbit_string_t)moonbit_string_literal_131.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1215,
  int32_t _param$1214
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1213 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1215;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1213, _param$1214
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1212,
  struct $StringView _param$1211
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1210 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1212;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1210, _param$1211
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1209,
  moonbit_string_t _param$1206,
  int32_t _param$1207,
  int32_t _param$1208
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1205 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1209;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1205, _param$1206, _param$1207, _param$1208
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1204,
  moonbit_string_t _param$1203
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1202 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1204;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1202, _param$1203
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1022;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1221;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1220;
  moonbit_string_t* _tmp$1267;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1266;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1265;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1228;
  moonbit_string_t* _tmp$1264;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1263;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1262;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1229;
  moonbit_string_t* _tmp$1261;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1260;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1259;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1230;
  moonbit_string_t* _tmp$1258;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1257;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1256;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1231;
  moonbit_string_t* _tmp$1255;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1254;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1253;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1232;
  moonbit_string_t* _tmp$1252;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1251;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1250;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1233;
  moonbit_string_t* _tmp$1249;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1248;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1247;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1234;
  moonbit_string_t* _tmp$1246;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1245;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1244;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1235;
  moonbit_string_t* _tmp$1243;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1242;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1241;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1236;
  moonbit_string_t* _tmp$1240;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1239;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1238;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1237;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$1021;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1227;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1226;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1225;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1224;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1020;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1223;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1222;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1023;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1269;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1268;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9$dyncall$closure.data;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3$dyncall$closure.data;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6$dyncall$closure.data;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8$dyncall$closure.data;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0$dyncall$closure.data;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4$dyncall$closure.data;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1$dyncall$closure.data;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5$dyncall$closure.data;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7$dyncall$closure.data;
  $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2$dyncall$closure.data;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$154 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$168 = (int64_t)0;
  _bind$1022
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1221 = _bind$1022;
  _tmp$1220
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1221
  };
  $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1220
  );
  _tmp$1267 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1267[0] = (moonbit_string_t)moonbit_string_literal_132.data;
  _tmp$1266
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1266)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1266->$0 = _tmp$1267;
  _tmp$1266->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0$clo
  );
  _tuple$1265
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1265)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1265->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_0$clo;
  _tuple$1265->$1 = _tmp$1266;
  _tuple$1228
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1228)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1228->$0 = 0;
  _tuple$1228->$1 = _tuple$1265;
  _tmp$1264 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1264[0] = (moonbit_string_t)moonbit_string_literal_133.data;
  _tmp$1263
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1263)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1263->$0 = _tmp$1264;
  _tmp$1263->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1$clo
  );
  _tuple$1262
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1262)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1262->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_1$clo;
  _tuple$1262->$1 = _tmp$1263;
  _tuple$1229
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1229)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1229->$0 = 1;
  _tuple$1229->$1 = _tuple$1262;
  _tmp$1261 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1261[0] = (moonbit_string_t)moonbit_string_literal_134.data;
  _tmp$1260
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1260)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1260->$0 = _tmp$1261;
  _tmp$1260->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2$clo
  );
  _tuple$1259
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1259)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1259->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_2$clo;
  _tuple$1259->$1 = _tmp$1260;
  _tuple$1230
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1230)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1230->$0 = 2;
  _tuple$1230->$1 = _tuple$1259;
  _tmp$1258 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1258[0] = (moonbit_string_t)moonbit_string_literal_135.data;
  _tmp$1257
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1257)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1257->$0 = _tmp$1258;
  _tmp$1257->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3$clo
  );
  _tuple$1256
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1256)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1256->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_3$clo;
  _tuple$1256->$1 = _tmp$1257;
  _tuple$1231
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1231)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1231->$0 = 3;
  _tuple$1231->$1 = _tuple$1256;
  _tmp$1255 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1255[0] = (moonbit_string_t)moonbit_string_literal_136.data;
  _tmp$1254
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1254)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1254->$0 = _tmp$1255;
  _tmp$1254->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4$clo
  );
  _tuple$1253
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1253)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1253->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_4$clo;
  _tuple$1253->$1 = _tmp$1254;
  _tuple$1232
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1232)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1232->$0 = 4;
  _tuple$1232->$1 = _tuple$1253;
  _tmp$1252 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1252[0] = (moonbit_string_t)moonbit_string_literal_137.data;
  _tmp$1251
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1251)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1251->$0 = _tmp$1252;
  _tmp$1251->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5$clo
  );
  _tuple$1250
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1250)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1250->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_5$clo;
  _tuple$1250->$1 = _tmp$1251;
  _tuple$1233
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1233)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1233->$0 = 5;
  _tuple$1233->$1 = _tuple$1250;
  _tmp$1249 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1249[0] = (moonbit_string_t)moonbit_string_literal_138.data;
  _tmp$1248
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1248)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1248->$0 = _tmp$1249;
  _tmp$1248->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6$clo
  );
  _tuple$1247
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1247)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1247->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_6$clo;
  _tuple$1247->$1 = _tmp$1248;
  _tuple$1234
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1234)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1234->$0 = 6;
  _tuple$1234->$1 = _tuple$1247;
  _tmp$1246 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1246[0] = (moonbit_string_t)moonbit_string_literal_139.data;
  _tmp$1245
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1245)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1245->$0 = _tmp$1246;
  _tmp$1245->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7$clo
  );
  _tuple$1244
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1244)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1244->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_7$clo;
  _tuple$1244->$1 = _tmp$1245;
  _tuple$1235
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1235)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1235->$0 = 7;
  _tuple$1235->$1 = _tuple$1244;
  _tmp$1243 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1243[0] = (moonbit_string_t)moonbit_string_literal_140.data;
  _tmp$1242
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1242)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1242->$0 = _tmp$1243;
  _tmp$1242->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8$clo
  );
  _tuple$1241
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1241)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1241->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_8$clo;
  _tuple$1241->$1 = _tmp$1242;
  _tuple$1236
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1236)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1236->$0 = 8;
  _tuple$1236->$1 = _tuple$1241;
  _tmp$1240 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1240[0] = (moonbit_string_t)moonbit_string_literal_141.data;
  _tmp$1239
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1239)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1239->$0 = _tmp$1240;
  _tmp$1239->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9$clo
  );
  _tuple$1238
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1238)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1238->$0
  = $azimuth$telemetry$tests$enhanced_suite$__test_617a696d7574685f62617369635f74657374732e6d6274_9$clo;
  _tuple$1238->$1 = _tmp$1239;
  _tuple$1237
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1237)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1237->$0 = 9;
  _tuple$1237->$1 = _tuple$1238;
  _bind$1021
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      10
    );
  _bind$1021[0] = _tuple$1228;
  _bind$1021[1] = _tuple$1229;
  _bind$1021[2] = _tuple$1230;
  _bind$1021[3] = _tuple$1231;
  _bind$1021[4] = _tuple$1232;
  _bind$1021[5] = _tuple$1233;
  _bind$1021[6] = _tuple$1234;
  _bind$1021[7] = _tuple$1235;
  _bind$1021[8] = _tuple$1236;
  _bind$1021[9] = _tuple$1237;
  _tmp$1227 = _bind$1021;
  _tmp$1226
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 10, _tmp$1227
  };
  _tmp$1225 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1226);
  _tuple$1224
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1224)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1224->$0 = (moonbit_string_t)moonbit_string_literal_142.data;
  _tuple$1224->$1 = _tmp$1225;
  _bind$1020
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      1
    );
  _bind$1020[0] = _tuple$1224;
  _tmp$1223 = _bind$1020;
  _tmp$1222
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 1, _tmp$1223
  };
  $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1222
  );
  _bind$1023
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1269 = _bind$1023;
  _tmp$1268
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1269
  };
  $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1268
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1219;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1191;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1192;
  int32_t _len$1193;
  int32_t _i$1194;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1219
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1191
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1191)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1191->$0 = _tmp$1219;
  async_tests$1191->$1 = 0;
  _arr$1192
  = $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1192);
  _len$1193 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1192);
  _i$1194 = 0;
  while (1) {
    if (_i$1194 < _len$1193) {
      struct $$3c$String$2a$Int$3e$* arg$1195;
      moonbit_string_t _field$3008;
      moonbit_string_t _tmp$1216;
      int32_t _field$3007;
      int32_t _cnt$3136;
      int32_t _tmp$1217;
      int32_t _tmp$1218;
      moonbit_incref(_arr$1192);
      arg$1195
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1192, _i$1194
      );
      _field$3008 = arg$1195->$0;
      _tmp$1216 = _field$3008;
      _field$3007 = arg$1195->$1;
      _cnt$3136 = Moonbit_object_header(arg$1195)->rc;
      if (_cnt$3136 > 1) {
        int32_t _new_cnt$3137;
        moonbit_incref(_tmp$1216);
        _new_cnt$3137 = _cnt$3136 - 1;
        Moonbit_object_header(arg$1195)->rc = _new_cnt$3137;
      } else if (_cnt$3136 == 1) {
        moonbit_free(arg$1195);
      }
      _tmp$1217 = _field$3007;
      moonbit_incref(async_tests$1191);
      $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_do_execute(
        async_tests$1191, _tmp$1216, _tmp$1217
      );
      _tmp$1218 = _i$1194 + 1;
      _i$1194 = _tmp$1218;
      continue;
    } else {
      moonbit_decref(_arr$1192);
    }
    break;
  }
  $azimuth$telemetry$tests$enhanced_suite$moonbit_test_driver_internal_run_async_tests(
    async_tests$1191
  );
  return 0;
}