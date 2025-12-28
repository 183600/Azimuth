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

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

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

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

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

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157;

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap;

struct $Error$azimuth$telemetry$azimuth_test_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$3c$StringView$2a$StringView$3e$;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1;

struct $Moonbit_Test_Driver_Internal__F$F2;

struct $$moonbitlang$core$builtin$Hasher;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

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

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $Moonbit_Test_Driver_Internal_Meta {
  int32_t $1;
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $2;
  
};

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  moonbit_string_t $1;
  
};

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ {
  int32_t $1;
  int32_t $2;
  moonbit_string_t* $0;
  
};

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
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

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
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

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157 {
  void* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
};

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap {
  int32_t(* code)(struct $$3c$String$3e$$3d$$3e$Int*, moonbit_string_t);
  struct $$3c$String$3e$$3d$$3e$Int* $0;
  
};

struct $Error$azimuth$telemetry$azimuth_test_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
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

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$ {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0_0;
  void* $0_1;
  moonbit_string_t $1;
  
};

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
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

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1007
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2094,
  moonbit_string_t s$985,
  int32_t sep$986
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$972
);

moonbit_string_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2003,
  moonbit_bytes_t bytes$973
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$1996,
  moonbit_string_t s$967
);

#define $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$930,
  moonbit_string_t filename$891,
  int32_t index$892
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1989
);

struct moonbit_result_0 $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1985
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1983
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1967,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$931,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$932
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1978,
  int32_t _cont_param$951
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1975,
  void* _cont_param$952
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1969,
  void* _state$934
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1964,
  void* err$914
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1950,
  moonbit_string_t attr$907
);

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1934,
  moonbit_string_t test_name$894,
  moonbit_string_t file_name$895,
  moonbit_string_t message$896,
  int32_t skipped$897
);

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$889
);

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$887,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$888,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$885
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$848,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$861,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$874,
  moonbit_string_t file_filter$845,
  int32_t index_filter$846
);

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$834
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$832,
  struct $$moonbitlang$core$builtin$Logger logger$833
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$818,
  struct $$moonbitlang$core$builtin$Logger logger$831
);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$816);

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$815,
  struct $$moonbitlang$core$builtin$Hasher* hasher$814
);

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$813,
  struct $$moonbitlang$core$builtin$Hasher* hasher$812
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$810,
  moonbit_string_t value$808
);

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$804,
  struct $$3c$String$3e$$3d$$3e$Int* f$806
);

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1890,
  moonbit_string_t k$805
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$802,
  int32_t idx$803
);

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$800,
  int32_t idx$801
);

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$796,
  int32_t key$792
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$787,
  moonbit_string_t key$783
);

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$778,
  int32_t key$774
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$769,
  moonbit_string_t key$765
);

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$760,
  int32_t key$756
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$751,
  moonbit_string_t key$747
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$739
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$731
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$723
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$715
);

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$711,
  moonbit_string_t key$712,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$713
);

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$708,
  moonbit_string_t key$709,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$710
);

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$705,
  int32_t key$706,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$707
);

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$702,
  moonbit_string_t key$703,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$704
);

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$692
);

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$681
);

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$670
);

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$659
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$642,
  moonbit_string_t key$651,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$652,
  int32_t hash$650
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$626,
  moonbit_string_t key$635,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$636,
  int32_t hash$634
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$610,
  int32_t key$619,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$620,
  int32_t hash$618
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$594,
  moonbit_string_t key$603,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$604,
  int32_t hash$602
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$588,
  int32_t idx$593,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$592
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$578,
  int32_t idx$583,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$582
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$568,
  int32_t idx$573,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$572
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$558,
  int32_t idx$563,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$562
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$548,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$550,
  int32_t new_idx$549
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$542,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$544,
  int32_t new_idx$543
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$536,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$538,
  int32_t new_idx$537
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$530,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$532,
  int32_t new_idx$531
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$527,
  int32_t idx$529,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$528
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$523,
  int32_t idx$525,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$524
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$519,
  int32_t idx$521,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$520
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$515,
  int32_t idx$517,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$516
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$509
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$503
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$497
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$491
);

int32_t $Int$$next_power_of_two(int32_t self$489);

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$488);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$486
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$484
);

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$482
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$480
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$479
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$478
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$476
);

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1543
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$474
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$473,
  struct $$moonbitlang$core$builtin$Logger logger$472
);

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$470,
  struct $$3c$String$3e$$3d$$3e$Int* f$471
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$466,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$468
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$463,
  struct $$3c$String$2a$Int$3e$* value$465
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$460,
  moonbit_string_t value$462
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$458
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$455
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$452
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$448,
  int32_t new_capacity$446
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$442,
  int32_t new_capacity$440
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$436,
  int32_t new_capacity$434
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$432
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$430,
  struct $StringView str$431
);

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$427,
  int32_t i$428,
  int32_t start_offset$429,
  int64_t end_offset$425
);

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$422,
  int32_t n$420,
  int32_t start_offset$416,
  int32_t end_offset$417
);

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$414,
  int32_t n$412,
  int32_t start_offset$411,
  int32_t end_offset$410
);

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$400,
  int32_t len$403,
  int32_t start_offset$407,
  int64_t end_offset$398
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$396
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$395
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$394
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$393
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$392
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387
);

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1472,
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

moonbit_string_t $Error$to_string(void* _e$1014);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1032,
  int32_t _param$1031
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1029,
  struct $StringView _param$1028
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1026,
  moonbit_string_t _param$1023,
  int32_t _param$1024,
  int32_t _param$1025
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1021,
  moonbit_string_t _param$1020
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

struct { int32_t rc; uint32_t meta; uint16_t const data[27]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 26), 
    110, 101, 119, 95, 102, 111, 99, 117, 115, 101, 100, 95, 116, 101, 
    115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    110, 101, 119, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 115, 46, 109, 98, 116, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[111]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 110), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 97, 122, 105, 109, 117, 116, 104, 95, 116, 101, 
    115, 116, 95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 
    116, 46, 77, 111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 68, 
    114, 105, 118, 101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 74, 
    115, 69, 114, 114, 111, 114, 46, 77, 111, 111, 110, 66, 105, 116, 
    84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 73, 110, 116, 101, 
    114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 114, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_42 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    97, 122, 105, 109, 117, 116, 104, 46, 109, 98, 116, 0
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
} const moonbit_string_literal_39 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[59]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 58), 
    123, 34, 112, 97, 99, 107, 97, 103, 101, 34, 58, 32, 34, 97, 122, 
    105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 
    121, 47, 97, 122, 105, 109, 117, 116, 104, 95, 116, 101, 115, 116, 
    34, 44, 32, 34, 102, 105, 108, 101, 110, 97, 109, 101, 34, 58, 32, 
    0
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

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct { int32_t rc; uint32_t meta; struct $$3c$String$3e$$3d$$3e$Int data; 
} const $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5
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

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_with_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_async_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_no_args_tests;

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1007
) {
  moonbit_decref(_tests$1007);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$966 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$972 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$979 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$972;
  int32_t moonbit_test_driver_internal_split_mbt_string$984 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$2119 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$991 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$992;
  moonbit_string_t _tmp$2118;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$993;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$994;
  int32_t _len$995;
  int32_t _i$996;
  Moonbit_object_header(file_and_index$991)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$991->$0 = _tmp$2119;
  file_and_index$991->$1 = 0;
  cli_args$992
  = $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$979
  );
  _tmp$2118 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$992, 1);
  test_args$993
  = $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$984, _tmp$2118, 47
  );
  _arr$994 = test_args$993;
  moonbit_incref(_arr$994);
  _len$995 = $$moonbitlang$core$builtin$Array$$length$1(_arr$994);
  _i$996 = 0;
  while (1) {
    if (_i$996 < _len$995) {
      moonbit_string_t arg$997;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$998;
      moonbit_string_t file$999;
      moonbit_string_t range$1000;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1001;
      moonbit_string_t _tmp$2116;
      int32_t start$1002;
      moonbit_string_t _tmp$2115;
      int32_t end$1003;
      int32_t i$1004;
      int32_t _tmp$2117;
      moonbit_incref(_arr$994);
      arg$997
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$994, _i$996
      );
      file_and_range$998
      = $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$984, arg$997, 58
      );
      moonbit_incref(file_and_range$998);
      file$999
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$998, 0
      );
      range$1000
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$998, 1
      );
      start_and_end$1001
      = $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$984, range$1000, 45
      );
      moonbit_incref(start_and_end$1001);
      _tmp$2116
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1001, 0
      );
      start$1002
      = $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$966, _tmp$2116
      );
      _tmp$2115
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1001, 1
      );
      end$1003
      = $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$966, _tmp$2115
      );
      i$1004 = start$1002;
      while (1) {
        if (i$1004 < end$1003) {
          struct $$3c$String$2a$Int$3e$* _tuple$2113;
          int32_t _tmp$2114;
          moonbit_incref(file$999);
          _tuple$2113
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$2113)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$2113->$0 = file$999;
          _tuple$2113->$1 = i$1004;
          moonbit_incref(file_and_index$991);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$991, _tuple$2113
          );
          _tmp$2114 = i$1004 + 1;
          i$1004 = _tmp$2114;
          continue;
        } else {
          moonbit_decref(file$999);
        }
        break;
      }
      _tmp$2117 = _i$996 + 1;
      _i$996 = _tmp$2117;
      continue;
    } else {
      moonbit_decref(_arr$994);
    }
    break;
  }
  return file_and_index$991;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2094,
  moonbit_string_t s$985,
  int32_t sep$986
) {
  moonbit_string_t* _tmp$2112 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$987 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$988;
  struct $Ref$3c$Int$3e$* start$989;
  int32_t val$2107;
  int32_t _tmp$2108;
  Moonbit_object_header(res$987)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$987->$0 = _tmp$2112;
  res$987->$1 = 0;
  i$988
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$988)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$988->$0 = 0;
  start$989
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$989)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$989->$0 = 0;
  while (1) {
    int32_t val$2095 = i$988->$0;
    int32_t _tmp$2096 = Moonbit_array_length(s$985);
    if (val$2095 < _tmp$2096) {
      int32_t val$2099 = i$988->$0;
      int32_t _tmp$2098;
      int32_t _tmp$2097;
      int32_t val$2106;
      int32_t _tmp$2105;
      if (val$2099 < 0 || val$2099 >= Moonbit_array_length(s$985)) {
        moonbit_panic();
      }
      _tmp$2098 = s$985[val$2099];
      _tmp$2097 = _tmp$2098;
      if (_tmp$2097 == sep$986) {
        int32_t val$2101 = start$989->$0;
        int32_t val$2102 = i$988->$0;
        moonbit_string_t _tmp$2100;
        int32_t val$2104;
        int32_t _tmp$2103;
        moonbit_incref(s$985);
        _tmp$2100 = $String$$unsafe_substring(s$985, val$2101, val$2102);
        moonbit_incref(res$987);
        $$moonbitlang$core$builtin$Array$$push$0(res$987, _tmp$2100);
        val$2104 = i$988->$0;
        _tmp$2103 = val$2104 + 1;
        start$989->$0 = _tmp$2103;
      }
      val$2106 = i$988->$0;
      _tmp$2105 = val$2106 + 1;
      i$988->$0 = _tmp$2105;
      continue;
    } else {
      moonbit_decref(i$988);
    }
    break;
  }
  val$2107 = start$989->$0;
  _tmp$2108 = Moonbit_array_length(s$985);
  if (val$2107 < _tmp$2108) {
    int32_t _field$2120 = start$989->$0;
    int32_t val$2110;
    int32_t _tmp$2111;
    moonbit_string_t _tmp$2109;
    moonbit_decref(start$989);
    val$2110 = _field$2120;
    _tmp$2111 = Moonbit_array_length(s$985);
    _tmp$2109 = $String$$unsafe_substring(s$985, val$2110, _tmp$2111);
    moonbit_incref(res$987);
    $$moonbitlang$core$builtin$Array$$push$0(res$987, _tmp$2109);
  } else {
    moonbit_decref(start$989);
    moonbit_decref(s$985);
  }
  return res$987;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$972
) {
  moonbit_bytes_t* tmp$980 =
    $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$2093 = Moonbit_array_length(tmp$980);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$981 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$2093);
  int32_t i$982 = 0;
  while (1) {
    int32_t _tmp$2089 = Moonbit_array_length(tmp$980);
    if (i$982 < _tmp$2089) {
      moonbit_bytes_t _tmp$2121;
      moonbit_bytes_t _tmp$2091;
      moonbit_string_t _tmp$2090;
      int32_t _tmp$2092;
      if (i$982 < 0 || i$982 >= Moonbit_array_length(tmp$980)) {
        moonbit_panic();
      }
      _tmp$2121 = (moonbit_bytes_t)tmp$980[i$982];
      _tmp$2091 = _tmp$2121;
      moonbit_incref(_tmp$2091);
      _tmp$2090
      = $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$972, _tmp$2091
      );
      moonbit_incref(res$981);
      $$moonbitlang$core$builtin$Array$$push$0(res$981, _tmp$2090);
      _tmp$2092 = i$982 + 1;
      i$982 = _tmp$2092;
      continue;
    } else {
      moonbit_decref(tmp$980);
    }
    break;
  }
  return res$981;
}

moonbit_string_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2003,
  moonbit_bytes_t bytes$973
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$974 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$975 = Moonbit_array_length(bytes$973);
  struct $Ref$3c$Int$3e$* i$976 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$976)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$976->$0 = 0;
  while (1) {
    int32_t val$2004 = i$976->$0;
    if (val$2004 < len$975) {
      int32_t val$2088 = i$976->$0;
      int32_t _tmp$2087;
      int32_t _tmp$2086;
      struct $Ref$3c$Int$3e$* c$977;
      int32_t val$2005;
      if (val$2088 < 0 || val$2088 >= Moonbit_array_length(bytes$973)) {
        moonbit_panic();
      }
      _tmp$2087 = bytes$973[val$2088];
      _tmp$2086 = (int32_t)_tmp$2087;
      c$977
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$977)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$977->$0 = _tmp$2086;
      val$2005 = c$977->$0;
      if (val$2005 < 128) {
        int32_t _field$2122 = c$977->$0;
        int32_t val$2007;
        int32_t _tmp$2006;
        int32_t val$2009;
        int32_t _tmp$2008;
        moonbit_decref(c$977);
        val$2007 = _field$2122;
        _tmp$2006 = val$2007;
        moonbit_incref(res$974);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$974, _tmp$2006
        );
        val$2009 = i$976->$0;
        _tmp$2008 = val$2009 + 1;
        i$976->$0 = _tmp$2008;
      } else {
        int32_t val$2010 = c$977->$0;
        if (val$2010 < 224) {
          int32_t val$2012 = i$976->$0;
          int32_t _tmp$2011 = val$2012 + 1;
          int32_t val$2021;
          int32_t _tmp$2020;
          int32_t _tmp$2014;
          int32_t val$2019;
          int32_t _tmp$2018;
          int32_t _tmp$2017;
          int32_t _tmp$2016;
          int32_t _tmp$2015;
          int32_t _tmp$2013;
          int32_t _field$2123;
          int32_t val$2023;
          int32_t _tmp$2022;
          int32_t val$2025;
          int32_t _tmp$2024;
          if (_tmp$2011 >= len$975) {
            moonbit_decref(c$977);
            moonbit_decref(i$976);
            moonbit_decref(bytes$973);
            break;
          }
          val$2021 = c$977->$0;
          _tmp$2020 = val$2021 & 31;
          _tmp$2014 = _tmp$2020 << 6;
          val$2019 = i$976->$0;
          _tmp$2018 = val$2019 + 1;
          if (_tmp$2018 < 0 || _tmp$2018 >= Moonbit_array_length(bytes$973)) {
            moonbit_panic();
          }
          _tmp$2017 = bytes$973[_tmp$2018];
          _tmp$2016 = (int32_t)_tmp$2017;
          _tmp$2015 = _tmp$2016 & 63;
          _tmp$2013 = _tmp$2014 | _tmp$2015;
          c$977->$0 = _tmp$2013;
          _field$2123 = c$977->$0;
          moonbit_decref(c$977);
          val$2023 = _field$2123;
          _tmp$2022 = val$2023;
          moonbit_incref(res$974);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$974, _tmp$2022
          );
          val$2025 = i$976->$0;
          _tmp$2024 = val$2025 + 2;
          i$976->$0 = _tmp$2024;
        } else {
          int32_t val$2026 = c$977->$0;
          if (val$2026 < 240) {
            int32_t val$2028 = i$976->$0;
            int32_t _tmp$2027 = val$2028 + 2;
            int32_t val$2044;
            int32_t _tmp$2043;
            int32_t _tmp$2036;
            int32_t val$2042;
            int32_t _tmp$2041;
            int32_t _tmp$2040;
            int32_t _tmp$2039;
            int32_t _tmp$2038;
            int32_t _tmp$2037;
            int32_t _tmp$2030;
            int32_t val$2035;
            int32_t _tmp$2034;
            int32_t _tmp$2033;
            int32_t _tmp$2032;
            int32_t _tmp$2031;
            int32_t _tmp$2029;
            int32_t _field$2124;
            int32_t val$2046;
            int32_t _tmp$2045;
            int32_t val$2048;
            int32_t _tmp$2047;
            if (_tmp$2027 >= len$975) {
              moonbit_decref(c$977);
              moonbit_decref(i$976);
              moonbit_decref(bytes$973);
              break;
            }
            val$2044 = c$977->$0;
            _tmp$2043 = val$2044 & 15;
            _tmp$2036 = _tmp$2043 << 12;
            val$2042 = i$976->$0;
            _tmp$2041 = val$2042 + 1;
            if (
              _tmp$2041 < 0 || _tmp$2041 >= Moonbit_array_length(bytes$973)
            ) {
              moonbit_panic();
            }
            _tmp$2040 = bytes$973[_tmp$2041];
            _tmp$2039 = (int32_t)_tmp$2040;
            _tmp$2038 = _tmp$2039 & 63;
            _tmp$2037 = _tmp$2038 << 6;
            _tmp$2030 = _tmp$2036 | _tmp$2037;
            val$2035 = i$976->$0;
            _tmp$2034 = val$2035 + 2;
            if (
              _tmp$2034 < 0 || _tmp$2034 >= Moonbit_array_length(bytes$973)
            ) {
              moonbit_panic();
            }
            _tmp$2033 = bytes$973[_tmp$2034];
            _tmp$2032 = (int32_t)_tmp$2033;
            _tmp$2031 = _tmp$2032 & 63;
            _tmp$2029 = _tmp$2030 | _tmp$2031;
            c$977->$0 = _tmp$2029;
            _field$2124 = c$977->$0;
            moonbit_decref(c$977);
            val$2046 = _field$2124;
            _tmp$2045 = val$2046;
            moonbit_incref(res$974);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$974, _tmp$2045
            );
            val$2048 = i$976->$0;
            _tmp$2047 = val$2048 + 3;
            i$976->$0 = _tmp$2047;
          } else {
            int32_t val$2050 = i$976->$0;
            int32_t _tmp$2049 = val$2050 + 3;
            int32_t val$2073;
            int32_t _tmp$2072;
            int32_t _tmp$2065;
            int32_t val$2071;
            int32_t _tmp$2070;
            int32_t _tmp$2069;
            int32_t _tmp$2068;
            int32_t _tmp$2067;
            int32_t _tmp$2066;
            int32_t _tmp$2058;
            int32_t val$2064;
            int32_t _tmp$2063;
            int32_t _tmp$2062;
            int32_t _tmp$2061;
            int32_t _tmp$2060;
            int32_t _tmp$2059;
            int32_t _tmp$2052;
            int32_t val$2057;
            int32_t _tmp$2056;
            int32_t _tmp$2055;
            int32_t _tmp$2054;
            int32_t _tmp$2053;
            int32_t _tmp$2051;
            int32_t val$2075;
            int32_t _tmp$2074;
            int32_t val$2079;
            int32_t _tmp$2078;
            int32_t _tmp$2077;
            int32_t _tmp$2076;
            int32_t _field$2125;
            int32_t val$2083;
            int32_t _tmp$2082;
            int32_t _tmp$2081;
            int32_t _tmp$2080;
            int32_t val$2085;
            int32_t _tmp$2084;
            if (_tmp$2049 >= len$975) {
              moonbit_decref(c$977);
              moonbit_decref(i$976);
              moonbit_decref(bytes$973);
              break;
            }
            val$2073 = c$977->$0;
            _tmp$2072 = val$2073 & 7;
            _tmp$2065 = _tmp$2072 << 18;
            val$2071 = i$976->$0;
            _tmp$2070 = val$2071 + 1;
            if (
              _tmp$2070 < 0 || _tmp$2070 >= Moonbit_array_length(bytes$973)
            ) {
              moonbit_panic();
            }
            _tmp$2069 = bytes$973[_tmp$2070];
            _tmp$2068 = (int32_t)_tmp$2069;
            _tmp$2067 = _tmp$2068 & 63;
            _tmp$2066 = _tmp$2067 << 12;
            _tmp$2058 = _tmp$2065 | _tmp$2066;
            val$2064 = i$976->$0;
            _tmp$2063 = val$2064 + 2;
            if (
              _tmp$2063 < 0 || _tmp$2063 >= Moonbit_array_length(bytes$973)
            ) {
              moonbit_panic();
            }
            _tmp$2062 = bytes$973[_tmp$2063];
            _tmp$2061 = (int32_t)_tmp$2062;
            _tmp$2060 = _tmp$2061 & 63;
            _tmp$2059 = _tmp$2060 << 6;
            _tmp$2052 = _tmp$2058 | _tmp$2059;
            val$2057 = i$976->$0;
            _tmp$2056 = val$2057 + 3;
            if (
              _tmp$2056 < 0 || _tmp$2056 >= Moonbit_array_length(bytes$973)
            ) {
              moonbit_panic();
            }
            _tmp$2055 = bytes$973[_tmp$2056];
            _tmp$2054 = (int32_t)_tmp$2055;
            _tmp$2053 = _tmp$2054 & 63;
            _tmp$2051 = _tmp$2052 | _tmp$2053;
            c$977->$0 = _tmp$2051;
            val$2075 = c$977->$0;
            _tmp$2074 = val$2075 - 65536;
            c$977->$0 = _tmp$2074;
            val$2079 = c$977->$0;
            _tmp$2078 = val$2079 >> 10;
            _tmp$2077 = _tmp$2078 + 55296;
            _tmp$2076 = _tmp$2077;
            moonbit_incref(res$974);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$974, _tmp$2076
            );
            _field$2125 = c$977->$0;
            moonbit_decref(c$977);
            val$2083 = _field$2125;
            _tmp$2082 = val$2083 & 1023;
            _tmp$2081 = _tmp$2082 + 56320;
            _tmp$2080 = _tmp$2081;
            moonbit_incref(res$974);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$974, _tmp$2080
            );
            val$2085 = i$976->$0;
            _tmp$2084 = val$2085 + 4;
            i$976->$0 = _tmp$2084;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$976);
      moonbit_decref(bytes$973);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$974);
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$1996,
  moonbit_string_t s$967
) {
  struct $Ref$3c$Int$3e$* res$968 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$969;
  int32_t i$970;
  int32_t _field$2126;
  Moonbit_object_header(res$968)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$968->$0 = 0;
  len$969 = Moonbit_array_length(s$967);
  i$970 = 0;
  while (1) {
    if (i$970 < len$969) {
      int32_t val$2001 = res$968->$0;
      int32_t _tmp$1998 = val$2001 * 10;
      int32_t _tmp$2000;
      int32_t _tmp$1999;
      int32_t _tmp$1997;
      int32_t _tmp$2002;
      if (i$970 < 0 || i$970 >= Moonbit_array_length(s$967)) {
        moonbit_panic();
      }
      _tmp$2000 = s$967[i$970];
      _tmp$1999 = _tmp$2000 - 48;
      _tmp$1997 = _tmp$1998 + _tmp$1999;
      res$968->$0 = _tmp$1997;
      _tmp$2002 = i$970 + 1;
      i$970 = _tmp$2002;
      continue;
    } else {
      moonbit_decref(s$967);
    }
    break;
  }
  _field$2126 = res$968->$0;
  moonbit_decref(res$968);
  return _field$2126;
}

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$930,
  moonbit_string_t filename$891,
  int32_t index$892
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$890;
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$2536;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$893;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$902;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2136;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$1995;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2135;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$903;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2134;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$1994;
  moonbit_string_t _field$2133;
  moonbit_string_t file_name$904;
  moonbit_string_t name$905;
  int32_t _tmp$1991;
  moonbit_string_t name$906;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$1948;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1949;
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$2538;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$913;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$929;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$954;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$956;
  void* _field$2130;
  int32_t _cnt$2407;
  void* _bind$957;
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$2542;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1988;
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$2543;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1981;
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$2544;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1982;
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$2545;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1966;
  moonbit_incref(
    $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_async_tests
  );
  filtered_test$890
  = $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_async_tests,
      filename$891,
      index$892
  );
  _closure$2536
  = (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$2536)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$2536->code
  = &$$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$2536->$0 = index$892;
  handle_result$893
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$2536;
  if (filtered_test$890 == 0) {
    moonbit_decref(async_tests$930);
    if (filtered_test$890) {
      moonbit_decref(filtered_test$890);
    }
    $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$893,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$964 =
      filtered_test$890;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$965 = _Some$964;
    item$902 = _item$965;
    goto $join$901;
  }
  goto $joinlet$2537;
  $join$901:;
  _field$2136 = item$902->$1;
  meta$1995 = _field$2136;
  _field$2135 = meta$1995->$2;
  attrs$903 = _field$2135;
  _field$2134 = item$902->$1;
  meta$1994 = _field$2134;
  _field$2133 = meta$1994->$0;
  file_name$904 = _field$2133;
  moonbit_incref(attrs$903);
  moonbit_incref(file_name$904);
  moonbit_incref(attrs$903);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$903)) {
    name$905 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$903);
    name$905 = $$moonbitlang$core$builtin$Array$$at$0(attrs$903, 0);
  }
  _tmp$1991 = Moonbit_array_length(name$905);
  if (_tmp$1991 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2132;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$1993;
    int32_t _field$2131;
    int32_t index$1992;
    moonbit_decref(name$905);
    _field$2132 = item$902->$1;
    meta$1993 = _field$2132;
    _field$2131 = meta$1993->$1;
    index$1992 = _field$2131;
    name$906 = $Int$$to_string$inner(index$1992, 10);
  } else {
    name$906 = name$905;
  }
  moonbit_incref(attrs$903);
  _tmp$1948 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$903);
  _tmp$1949
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$1948, _tmp$1949)) {
    moonbit_string_t _tmp$1963;
    moonbit_string_t _tmp$1962;
    moonbit_string_t _tmp$1959;
    moonbit_string_t _tmp$1961;
    moonbit_string_t _tmp$1960;
    moonbit_string_t _tmp$1958;
    moonbit_decref(async_tests$930);
    moonbit_decref(item$902);
    moonbit_incref(file_name$904);
    _tmp$1963
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$904
    );
    _tmp$1962
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$1963
    );
    _tmp$1959
    = moonbit_add_string(
      _tmp$1962, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$1961 = $$moonbitlang$core$builtin$Array$$at$0(attrs$903, 0);
    _tmp$1960 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$1961);
    _tmp$1958 = moonbit_add_string(_tmp$1959, _tmp$1960);
    $moonbitlang$core$builtin$println$0(_tmp$1958);
    $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$893,
        name$906,
        file_name$904,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$903);
  }
  moonbit_incref(name$906);
  moonbit_incref(file_name$904);
  moonbit_incref(handle_result$893);
  _closure$2538
  = (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$2538)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2538->code
  = &$$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$2538->$0 = name$906;
  _closure$2538->$1 = file_name$904;
  _closure$2538->$2 = handle_result$893;
  on_err$913 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2538;
  _field$2130 = item$902->$0;
  _cnt$2407 = Moonbit_object_header(item$902)->rc;
  if (_cnt$2407 > 1) {
    int32_t _new_cnt$2409;
    moonbit_incref(_field$2130);
    _new_cnt$2409 = _cnt$2407 - 1;
    Moonbit_object_header(item$902)->rc = _new_cnt$2409;
  } else if (_cnt$2407 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2408 = item$902->$1;
    moonbit_decref(_field$2408);
    moonbit_free(item$902);
  }
  _bind$957 = _field$2130;
  switch (Moonbit_object_tag(_bind$957)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$958;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2127;
      int32_t _cnt$2410;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$959;
      moonbit_decref(async_tests$930);
      _F0$958 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$957;
      _field$2127 = _F0$958->$0;
      _cnt$2410 = Moonbit_object_header(_F0$958)->rc;
      if (_cnt$2410 > 1) {
        int32_t _new_cnt$2411;
        moonbit_incref(_field$2127);
        _new_cnt$2411 = _cnt$2410 - 1;
        Moonbit_object_header(_F0$958)->rc = _new_cnt$2411;
      } else if (_cnt$2410 == 1) {
        moonbit_free(_F0$958);
      }
      _f$959 = _field$2127;
      f$956 = _f$959;
      goto $join$955;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$960;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2128;
      int32_t _cnt$2412;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$961;
      moonbit_decref(async_tests$930);
      _F1$960 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$957;
      _field$2128 = _F1$960->$0;
      _cnt$2412 = Moonbit_object_header(_F1$960)->rc;
      if (_cnt$2412 > 1) {
        int32_t _new_cnt$2413;
        moonbit_incref(_field$2128);
        _new_cnt$2413 = _cnt$2412 - 1;
        Moonbit_object_header(_F1$960)->rc = _new_cnt$2413;
      } else if (_cnt$2412 == 1) {
        moonbit_free(_F1$960);
      }
      _f$961 = _field$2128;
      f$954 = _f$961;
      goto $join$953;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$962 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$957;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2129 =
        _F2$962->$0;
      int32_t _cnt$2414 = Moonbit_object_header(_F2$962)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$963;
      if (_cnt$2414 > 1) {
        int32_t _new_cnt$2415;
        moonbit_incref(_field$2129);
        _new_cnt$2415 = _cnt$2414 - 1;
        Moonbit_object_header(_F2$962)->rc = _new_cnt$2415;
      } else if (_cnt$2414 == 1) {
        moonbit_free(_F2$962);
      }
      _f$963 = _field$2129;
      f$929 = _f$963;
      goto $join$928;
      break;
    }
  }
  goto $joinlet$2541;
  $join$955:;
  _closure$2542
  = (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$2542)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2542->code
  = &$$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$2542->$0 = name$906;
  _closure$2542->$1 = file_name$904;
  _closure$2542->$2 = handle_result$893;
  _tmp$1988 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2542;
  $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_catch_error(
    f$956, _tmp$1988, on_err$913
  );
  $joinlet$2541:;
  goto $joinlet$2540;
  $join$953:;
  moonbit_incref(name$906);
  _closure$2543
  = (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$2543)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$2543->code
  = &$$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$2543->$0 = f$954;
  _closure$2543->$1 = name$906;
  _tmp$1981
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$2543;
  _closure$2544
  = (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$2544)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2544->code
  = &$$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$2544->$0 = name$906;
  _closure$2544->$1 = file_name$904;
  _closure$2544->$2 = handle_result$893;
  _tmp$1982 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2544;
  $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_catch_error(
    _tmp$1981, _tmp$1982, on_err$913
  );
  $joinlet$2540:;
  goto $joinlet$2539;
  $join$928:;
  _closure$2545
  = (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$2545)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$2545->code
  = &$$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$2545->$0 = f$929;
  _closure$2545->$1 = on_err$913;
  _closure$2545->$2 = name$906;
  _closure$2545->$3 = file_name$904;
  _closure$2545->$4 = handle_result$893;
  _tmp$1966
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$2545;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$930, _tmp$1966);
  $joinlet$2539:;
  $joinlet$2537:;
  return 0;
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1989
) {
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$1990 =
    (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$1989;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2139 =
    _casted_env$1990->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$893 =
    _field$2139;
  moonbit_string_t _field$2138 = _casted_env$1990->$1;
  moonbit_string_t file_name$904 = _field$2138;
  moonbit_string_t _field$2137 = _casted_env$1990->$0;
  int32_t _cnt$2416 = Moonbit_object_header(_casted_env$1990)->rc;
  moonbit_string_t name$906;
  if (_cnt$2416 > 1) {
    int32_t _new_cnt$2417;
    moonbit_incref(handle_result$893);
    moonbit_incref(file_name$904);
    moonbit_incref(_field$2137);
    _new_cnt$2417 = _cnt$2416 - 1;
    Moonbit_object_header(_casted_env$1990)->rc = _new_cnt$2417;
  } else if (_cnt$2416 == 1) {
    moonbit_free(_casted_env$1990);
  }
  name$906 = _field$2137;
  $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$893,
      name$906,
      file_name$904,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1985
) {
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$1986 =
    (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$1985;
  moonbit_string_t _field$2141 = _casted_env$1986->$1;
  moonbit_string_t name$906 = _field$2141;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2140 =
    _casted_env$1986->$0;
  int32_t _cnt$2418 = Moonbit_object_header(_casted_env$1986)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$954;
  int32_t _tmp$1987;
  if (_cnt$2418 > 1) {
    int32_t _new_cnt$2419;
    moonbit_incref(name$906);
    moonbit_incref(_field$2140);
    _new_cnt$2419 = _cnt$2418 - 1;
    Moonbit_object_header(_casted_env$1986)->rc = _new_cnt$2419;
  } else if (_cnt$2418 == 1) {
    moonbit_free(_casted_env$1986);
  }
  f$954 = _field$2140;
  _tmp$1987
  = $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$906
  );
  return f$954->code(f$954, _tmp$1987);
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1983
) {
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$1984 =
    (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$1983;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2144 =
    _casted_env$1984->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$893 =
    _field$2144;
  moonbit_string_t _field$2143 = _casted_env$1984->$1;
  moonbit_string_t file_name$904 = _field$2143;
  moonbit_string_t _field$2142 = _casted_env$1984->$0;
  int32_t _cnt$2420 = Moonbit_object_header(_casted_env$1984)->rc;
  moonbit_string_t name$906;
  if (_cnt$2420 > 1) {
    int32_t _new_cnt$2421;
    moonbit_incref(handle_result$893);
    moonbit_incref(file_name$904);
    moonbit_incref(_field$2142);
    _new_cnt$2421 = _cnt$2420 - 1;
    Moonbit_object_header(_casted_env$1984)->rc = _new_cnt$2421;
  } else if (_cnt$2420 == 1) {
    moonbit_free(_casted_env$1984);
  }
  name$906 = _field$2142;
  $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$893,
      name$906,
      file_name$904,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1967,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$931,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$932
) {
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$1968 =
    (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$1967;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2149 =
    _casted_env$1968->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$893 =
    _field$2149;
  moonbit_string_t _field$2148 = _casted_env$1968->$3;
  moonbit_string_t file_name$904 = _field$2148;
  moonbit_string_t _field$2147 = _casted_env$1968->$2;
  moonbit_string_t name$906 = _field$2147;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2146 = _casted_env$1968->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$913 = _field$2146;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2145 =
    _casted_env$1968->$0;
  int32_t _cnt$2422 = Moonbit_object_header(_casted_env$1968)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$929;
  int32_t _async_driver$933;
  int32_t _tmp$1972;
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$2546;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$1973;
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$2547;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$1974;
  if (_cnt$2422 > 1) {
    int32_t _new_cnt$2423;
    moonbit_incref(handle_result$893);
    moonbit_incref(file_name$904);
    moonbit_incref(name$906);
    moonbit_incref(on_err$913);
    moonbit_incref(_field$2145);
    _new_cnt$2423 = _cnt$2422 - 1;
    Moonbit_object_header(_casted_env$1968)->rc = _new_cnt$2423;
  } else if (_cnt$2422 == 1) {
    moonbit_free(_casted_env$1968);
  }
  f$929 = _field$2145;
  _async_driver$933 = 0;
  moonbit_incref(name$906);
  _tmp$1972
  = $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$906
  );
  moonbit_incref(_cont$931);
  _closure$2546
  = (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$2546)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$2546->code
  = &$$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$2546->$0 = _async_driver$933;
  _closure$2546->$1 = _cont$931;
  _closure$2546->$2 = name$906;
  _closure$2546->$3 = file_name$904;
  _closure$2546->$4 = handle_result$893;
  _tmp$1973 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$2546;
  _closure$2547
  = (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$2547)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$2547->code
  = &$$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$2547->$0 = _async_driver$933;
  _closure$2547->$1 = _err_cont$932;
  _closure$2547->$2 = _cont$931;
  _closure$2547->$3 = on_err$913;
  _tmp$1974 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2547;
  f$929->code(f$929, _tmp$1972, _tmp$1973, _tmp$1974);
  return 0;
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1978,
  int32_t _cont_param$951
) {
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$1979 =
    (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$1978;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2154 =
    _casted_env$1979->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$893 =
    _field$2154;
  moonbit_string_t _field$2153 = _casted_env$1979->$3;
  moonbit_string_t file_name$904 = _field$2153;
  moonbit_string_t _field$2152 = _casted_env$1979->$2;
  moonbit_string_t name$906 = _field$2152;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2151 = _casted_env$1979->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$931 = _field$2151;
  int32_t _field$2150 = _casted_env$1979->$0;
  int32_t _cnt$2424 = Moonbit_object_header(_casted_env$1979)->rc;
  int32_t _async_driver$933;
  void* State_1$1980;
  if (_cnt$2424 > 1) {
    int32_t _new_cnt$2425;
    moonbit_incref(handle_result$893);
    moonbit_incref(file_name$904);
    moonbit_incref(name$906);
    moonbit_incref(_cont$931);
    _new_cnt$2425 = _cnt$2424 - 1;
    Moonbit_object_header(_casted_env$1979)->rc = _new_cnt$2425;
  } else if (_cnt$2424 == 1) {
    moonbit_free(_casted_env$1979);
  }
  _async_driver$933 = _field$2150;
  State_1$1980
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1
      )
    );
  Moonbit_object_header(State_1$1980)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1*)State_1$1980)->$0
  = _cont_param$951;
  ((struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1*)State_1$1980)->$1
  = handle_result$893;
  ((struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1*)State_1$1980)->$2
  = file_name$904;
  ((struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1*)State_1$1980)->$3
  = name$906;
  ((struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1*)State_1$1980)->$4
  = _cont$931;
  $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$933, State_1$1980
  );
  return 0;
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1975,
  void* _cont_param$952
) {
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$1976 =
    (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$1975;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2158 = _casted_env$1976->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$913 = _field$2158;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2157 = _casted_env$1976->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$931 = _field$2157;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2156 = _casted_env$1976->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$932 = _field$2156;
  int32_t _field$2155 = _casted_env$1976->$0;
  int32_t _cnt$2426 = Moonbit_object_header(_casted_env$1976)->rc;
  int32_t _async_driver$933;
  void* _try$157$1977;
  if (_cnt$2426 > 1) {
    int32_t _new_cnt$2427;
    moonbit_incref(on_err$913);
    moonbit_incref(_cont$931);
    moonbit_incref(_err_cont$932);
    _new_cnt$2427 = _cnt$2426 - 1;
    Moonbit_object_header(_casted_env$1976)->rc = _new_cnt$2427;
  } else if (_cnt$2426 == 1) {
    moonbit_free(_casted_env$1976);
  }
  _async_driver$933 = _field$2155;
  _try$157$1977
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157
      )
    );
  Moonbit_object_header(_try$157$1977)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157*)_try$157$1977)->$0
  = _cont_param$952;
  ((struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157*)_try$157$1977)->$1
  = on_err$913;
  ((struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157*)_try$157$1977)->$2
  = _cont$931;
  ((struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157*)_try$157$1977)->$3
  = _err_cont$932;
  $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$933, _try$157$1977
  );
  return 0;
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1969,
  void* _state$934
) {
  switch (Moonbit_object_tag(_state$934)) {
    case 0: {
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157* _$2a$try$157$935 =
        (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$$2a$try$157*)_state$934;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2162 = _$2a$try$157$935->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$936 = _field$2162;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2161 = _$2a$try$157$935->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$937 = _field$2161;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2160 = _$2a$try$157$935->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$938 = _field$2160;
      void* _field$2159 = _$2a$try$157$935->$0;
      int32_t _cnt$2428 = Moonbit_object_header(_$2a$try$157$935)->rc;
      void* _try_err$939;
      void* err$941;
      void* err$943;
      int32_t _tmp$1971;
      if (_cnt$2428 > 1) {
        int32_t _new_cnt$2429;
        moonbit_incref(_err_cont$936);
        moonbit_incref(_cont$937);
        moonbit_incref(on_err$938);
        moonbit_incref(_field$2159);
        _new_cnt$2429 = _cnt$2428 - 1;
        Moonbit_object_header(_$2a$try$157$935)->rc = _new_cnt$2429;
      } else if (_cnt$2428 == 1) {
        moonbit_free(_$2a$try$157$935);
      }
      _try_err$939 = _field$2159;
      if (
        $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$938);
        moonbit_decref(_cont$937);
        err$943 = _try_err$939;
        goto $join$942;
      } else {
        moonbit_decref(_err_cont$936);
        err$941 = _try_err$939;
        goto $join$940;
      }
      goto $joinlet$2549;
      $join$942:;
      return _err_cont$936->code(_err_cont$936, err$943);
      $joinlet$2549:;
      goto $joinlet$2548;
      $join$940:;
      _tmp$1971 = on_err$938->code(on_err$938, err$941);
      _cont$937->code(_cont$937, _tmp$1971);
      $joinlet$2548:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1* _State_1$944 =
        (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$173$on_err$68$$2a$arm$165$lambda$191$State$State_1*)_state$934;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2166 = _State_1$944->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$945 = _field$2166;
      moonbit_string_t _field$2165 = _State_1$944->$3;
      moonbit_string_t name$946 = _field$2165;
      moonbit_string_t _field$2164 = _State_1$944->$2;
      moonbit_string_t file_name$947 = _field$2164;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2163 =
        _State_1$944->$1;
      int32_t _cnt$2430 = Moonbit_object_header(_State_1$944)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$948;
      int32_t _tmp$1970;
      if (_cnt$2430 > 1) {
        int32_t _new_cnt$2431;
        moonbit_incref(_cont$945);
        moonbit_incref(name$946);
        moonbit_incref(file_name$947);
        moonbit_incref(_field$2163);
        _new_cnt$2431 = _cnt$2430 - 1;
        Moonbit_object_header(_State_1$944)->rc = _new_cnt$2431;
      } else if (_cnt$2430 == 1) {
        moonbit_free(_State_1$944);
      }
      handle_result$948 = _field$2163;
      _tmp$1970
      = handle_result$948->code(
        handle_result$948,
          name$946,
          file_name$947,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$945->code(_cont$945, _tmp$1970);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1964,
  void* err$914
) {
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$1965 =
    (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$1964;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2173 =
    _casted_env$1965->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$893 =
    _field$2173;
  moonbit_string_t _field$2172 = _casted_env$1965->$1;
  moonbit_string_t file_name$904 = _field$2172;
  moonbit_string_t _field$2171 = _casted_env$1965->$0;
  int32_t _cnt$2432 = Moonbit_object_header(_casted_env$1965)->rc;
  moonbit_string_t name$906;
  void* e$916;
  moonbit_string_t e$919;
  moonbit_string_t message$917;
  if (_cnt$2432 > 1) {
    int32_t _new_cnt$2433;
    moonbit_incref(handle_result$893);
    moonbit_incref(file_name$904);
    moonbit_incref(_field$2171);
    _new_cnt$2433 = _cnt$2432 - 1;
    Moonbit_object_header(_casted_env$1965)->rc = _new_cnt$2433;
  } else if (_cnt$2432 == 1) {
    moonbit_free(_casted_env$1965);
  }
  name$906 = _field$2171;
  switch (Moonbit_object_tag(err$914)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$920 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$914;
      moonbit_string_t _field$2167 = _Failure$920->$0;
      int32_t _cnt$2434 = Moonbit_object_header(_Failure$920)->rc;
      moonbit_string_t _e$921;
      if (_cnt$2434 > 1) {
        int32_t _new_cnt$2435;
        moonbit_incref(_field$2167);
        _new_cnt$2435 = _cnt$2434 - 1;
        Moonbit_object_header(_Failure$920)->rc = _new_cnt$2435;
      } else if (_cnt$2434 == 1) {
        moonbit_free(_Failure$920);
      }
      _e$921 = _field$2167;
      e$919 = _e$921;
      goto $join$918;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$922 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$914;
      moonbit_string_t _field$2168 = _InspectError$922->$0;
      int32_t _cnt$2436 = Moonbit_object_header(_InspectError$922)->rc;
      moonbit_string_t _e$923;
      if (_cnt$2436 > 1) {
        int32_t _new_cnt$2437;
        moonbit_incref(_field$2168);
        _new_cnt$2437 = _cnt$2436 - 1;
        Moonbit_object_header(_InspectError$922)->rc = _new_cnt$2437;
      } else if (_cnt$2436 == 1) {
        moonbit_free(_InspectError$922);
      }
      _e$923 = _field$2168;
      e$919 = _e$923;
      goto $join$918;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$924 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$914;
      moonbit_string_t _field$2169 = _SnapshotError$924->$0;
      int32_t _cnt$2438 = Moonbit_object_header(_SnapshotError$924)->rc;
      moonbit_string_t _e$925;
      if (_cnt$2438 > 1) {
        int32_t _new_cnt$2439;
        moonbit_incref(_field$2169);
        _new_cnt$2439 = _cnt$2438 - 1;
        Moonbit_object_header(_SnapshotError$924)->rc = _new_cnt$2439;
      } else if (_cnt$2438 == 1) {
        moonbit_free(_SnapshotError$924);
      }
      _e$925 = _field$2169;
      e$919 = _e$925;
      goto $join$918;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$azimuth_test_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$926 =
        (struct $Error$azimuth$telemetry$azimuth_test_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$914;
      moonbit_string_t _field$2170 =
        _MoonBitTestDriverInternalJsError$926->$0;
      int32_t _cnt$2440 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$926)->rc;
      moonbit_string_t _e$927;
      if (_cnt$2440 > 1) {
        int32_t _new_cnt$2441;
        moonbit_incref(_field$2170);
        _new_cnt$2441 = _cnt$2440 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$926)->rc
        = _new_cnt$2441;
      } else if (_cnt$2440 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$926);
      }
      _e$927 = _field$2170;
      e$919 = _e$927;
      goto $join$918;
      break;
    }
    default: {
      e$916 = err$914;
      goto $join$915;
      break;
    }
  }
  goto $joinlet$2551;
  $join$918:;
  $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$893, name$906, file_name$904, e$919, 0
  );
  $joinlet$2551:;
  goto $joinlet$2550;
  $join$915:;
  message$917 = $Error$to_string(e$916);
  $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$893, name$906, file_name$904, message$917, 0
  );
  $joinlet$2550:;
  return 0;
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1950,
  moonbit_string_t attr$907
) {
  int32_t _tmp$1952;
  int64_t _tmp$1951;
  moonbit_decref(_env$1950);
  _tmp$1952 = Moonbit_array_length(attr$907);
  _tmp$1951 = (int64_t)_tmp$1952;
  moonbit_incref(attr$907);
  if ($String$$char_length_ge$inner(attr$907, 5, 0, _tmp$1951)) {
    int32_t _tmp$1957 = attr$907[0];
    int32_t _x$908 = _tmp$1957;
    if (_x$908 == 112) {
      int32_t _tmp$1956 = attr$907[1];
      int32_t _x$909 = _tmp$1956;
      if (_x$909 == 97) {
        int32_t _tmp$1955 = attr$907[2];
        int32_t _x$910 = _tmp$1955;
        if (_x$910 == 110) {
          int32_t _tmp$1954 = attr$907[3];
          int32_t _x$911 = _tmp$1954;
          if (_x$911 == 105) {
            int32_t _tmp$2174 = attr$907[4];
            int32_t _tmp$1953;
            int32_t _x$912;
            moonbit_decref(attr$907);
            _tmp$1953 = _tmp$2174;
            _x$912 = _tmp$1953;
            return _x$912 == 99 || 0;
          } else {
            moonbit_decref(attr$907);
            return 0;
          }
        } else {
          moonbit_decref(attr$907);
          return 0;
        }
      } else {
        moonbit_decref(attr$907);
        return 0;
      }
    } else {
      moonbit_decref(attr$907);
      return 0;
    }
  } else {
    moonbit_decref(attr$907);
    return 0;
  }
}

int32_t $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1934,
  moonbit_string_t test_name$894,
  moonbit_string_t file_name$895,
  moonbit_string_t message$896,
  int32_t skipped$897
) {
  struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$1935 =
    (struct $$azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$1934;
  int32_t _field$2175 = _casted_env$1935->$0;
  int32_t index$892;
  int32_t _if_result$2552;
  moonbit_string_t file_name$898;
  moonbit_string_t test_name$899;
  moonbit_string_t message$900;
  moonbit_string_t _tmp$1947;
  moonbit_string_t _tmp$1946;
  moonbit_string_t _tmp$1944;
  moonbit_string_t _tmp$1945;
  moonbit_string_t _tmp$1943;
  moonbit_string_t _tmp$1941;
  moonbit_string_t _tmp$1942;
  moonbit_string_t _tmp$1940;
  moonbit_string_t _tmp$1938;
  moonbit_string_t _tmp$1939;
  moonbit_string_t _tmp$1937;
  moonbit_string_t _tmp$1936;
  moonbit_decref(_casted_env$1935);
  index$892 = _field$2175;
  if (!skipped$897) {
    _if_result$2552 = 1;
  } else {
    _if_result$2552 = 0;
  }
  if (_if_result$2552) {
    
  }
  file_name$898 = $String$$escape(file_name$895);
  test_name$899 = $String$$escape(test_name$894);
  message$900 = $String$$escape(message$896);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$1947
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$898
  );
  _tmp$1946
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$1947
  );
  _tmp$1944
  = moonbit_add_string(
    _tmp$1946, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$1945
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$892
  );
  _tmp$1943 = moonbit_add_string(_tmp$1944, _tmp$1945);
  _tmp$1941
  = moonbit_add_string(
    _tmp$1943, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$1942
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$899
  );
  _tmp$1940 = moonbit_add_string(_tmp$1941, _tmp$1942);
  _tmp$1938
  = moonbit_add_string(
    _tmp$1940, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$1939 = $$moonbitlang$core$builtin$Show$$String$$to_string(message$900);
  _tmp$1937 = moonbit_add_string(_tmp$1938, _tmp$1939);
  _tmp$1936
  = moonbit_add_string(
    _tmp$1937, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$1936);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$889
) {
  moonbit_decref(_discard_$889);
  return 42;
}

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$887,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$888,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$885
) {
  void* _try_err$883;
  struct moonbit_result_0 _tmp$2554 = f$887->code(f$887);
  void* err$884;
  if (_tmp$2554.tag) {
    int32_t const _ok$1932 = _tmp$2554.data.ok;
    moonbit_decref(on_err$885);
  } else {
    void* const _err$1933 = _tmp$2554.data.err;
    moonbit_decref(on_ok$888);
    _try_err$883 = _err$1933;
    goto $join$882;
  }
  on_ok$888->code(on_ok$888);
  goto $joinlet$2553;
  $join$882:;
  err$884 = _try_err$883;
  on_err$885->code(on_err$885, err$884);
  $joinlet$2553:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$848,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$861,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$874,
  moonbit_string_t file_filter$845,
  int32_t index_filter$846
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$842;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$843;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$847;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2181;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1923;
  void* F0$1920;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2180;
  int32_t _cnt$2442;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1922;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1921;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$844;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$857;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$858;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$860;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2179;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1927;
  void* F1$1924;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2178;
  int32_t _cnt$2445;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1926;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1925;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$859;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$870;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$871;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$873;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2177;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1931;
  void* F2$1928;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2176;
  int32_t _cnt$2448;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1930;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1929;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$872;
  moonbit_incref(file_filter$845);
  _bind$847
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$848, file_filter$845
  );
  if (_bind$847 == 0) {
    if (_bind$847) {
      moonbit_decref(_bind$847);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$849 =
      _bind$847;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$850 =
      _Some$849;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$852;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$853;
    moonbit_incref(_index_func_map$850);
    _bind$853
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$850, index_filter$846
    );
    if (_bind$853 == 0) {
      if (_bind$853) {
        moonbit_decref(_bind$853);
      }
      moonbit_decref(_index_func_map$850);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$854;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$855;
      moonbit_decref(async_tests$874);
      moonbit_decref(with_args_tests$861);
      _Some$854 = _bind$853;
      _func_attrs_tuple$855 = _Some$854;
      func_attrs_tuple$852 = _func_attrs_tuple$855;
      goto $join$851;
    }
    goto $joinlet$2556;
    $join$851:;
    index_func_map$842 = _index_func_map$850;
    func_attrs_tuple$843 = func_attrs_tuple$852;
    goto $join$841;
    $joinlet$2556:;
  }
  goto $joinlet$2555;
  $join$841:;
  moonbit_decref(index_func_map$842);
  _field$2181 = func_attrs_tuple$843->$0;
  _tmp$1923 = _field$2181;
  moonbit_incref(_tmp$1923);
  F0$1920
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$1920)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$1920)->$0 = _tmp$1923;
  _field$2180 = func_attrs_tuple$843->$1;
  _cnt$2442 = Moonbit_object_header(func_attrs_tuple$843)->rc;
  if (_cnt$2442 > 1) {
    int32_t _new_cnt$2444;
    moonbit_incref(_field$2180);
    _new_cnt$2444 = _cnt$2442 - 1;
    Moonbit_object_header(func_attrs_tuple$843)->rc = _new_cnt$2444;
  } else if (_cnt$2442 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2443 =
      func_attrs_tuple$843->$0;
    moonbit_decref(_field$2443);
    moonbit_free(func_attrs_tuple$843);
  }
  _tmp$1922 = _field$2180;
  _tmp$1921
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1921)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1921->$0 = file_filter$845;
  _tmp$1921->$1 = index_filter$846;
  _tmp$1921->$2 = _tmp$1922;
  k$844
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$844)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$844->$0 = F0$1920;
  k$844->$1 = _tmp$1921;
  return k$844;
  $joinlet$2555:;
  moonbit_incref(file_filter$845);
  _bind$860
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$861, file_filter$845
  );
  if (_bind$860 == 0) {
    if (_bind$860) {
      moonbit_decref(_bind$860);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$862 =
      _bind$860;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$863 =
      _Some$862;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$865;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$866;
    moonbit_incref(_index_func_map$863);
    _bind$866
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$863, index_filter$846
    );
    if (_bind$866 == 0) {
      if (_bind$866) {
        moonbit_decref(_bind$866);
      }
      moonbit_decref(_index_func_map$863);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$867;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$868;
      moonbit_decref(async_tests$874);
      _Some$867 = _bind$866;
      _func_attrs_tuple$868 = _Some$867;
      func_attrs_tuple$865 = _func_attrs_tuple$868;
      goto $join$864;
    }
    goto $joinlet$2558;
    $join$864:;
    index_func_map$857 = _index_func_map$863;
    func_attrs_tuple$858 = func_attrs_tuple$865;
    goto $join$856;
    $joinlet$2558:;
  }
  goto $joinlet$2557;
  $join$856:;
  moonbit_decref(index_func_map$857);
  _field$2179 = func_attrs_tuple$858->$0;
  _tmp$1927 = _field$2179;
  moonbit_incref(_tmp$1927);
  F1$1924
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$1924)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$1924)->$0 = _tmp$1927;
  _field$2178 = func_attrs_tuple$858->$1;
  _cnt$2445 = Moonbit_object_header(func_attrs_tuple$858)->rc;
  if (_cnt$2445 > 1) {
    int32_t _new_cnt$2447;
    moonbit_incref(_field$2178);
    _new_cnt$2447 = _cnt$2445 - 1;
    Moonbit_object_header(func_attrs_tuple$858)->rc = _new_cnt$2447;
  } else if (_cnt$2445 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2446 =
      func_attrs_tuple$858->$0;
    moonbit_decref(_field$2446);
    moonbit_free(func_attrs_tuple$858);
  }
  _tmp$1926 = _field$2178;
  _tmp$1925
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1925)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1925->$0 = file_filter$845;
  _tmp$1925->$1 = index_filter$846;
  _tmp$1925->$2 = _tmp$1926;
  k$859
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$859)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$859->$0 = F1$1924;
  k$859->$1 = _tmp$1925;
  return k$859;
  $joinlet$2557:;
  moonbit_incref(file_filter$845);
  _bind$873
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$874, file_filter$845
  );
  if (_bind$873 == 0) {
    if (_bind$873) {
      moonbit_decref(_bind$873);
    }
    moonbit_decref(file_filter$845);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$875 =
      _bind$873;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$876 =
      _Some$875;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$878;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$879;
    moonbit_incref(_index_func_map$876);
    _bind$879
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$876, index_filter$846
    );
    if (_bind$879 == 0) {
      if (_bind$879) {
        moonbit_decref(_bind$879);
      }
      moonbit_decref(_index_func_map$876);
      moonbit_decref(file_filter$845);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$880 =
        _bind$879;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$881 =
        _Some$880;
      func_attrs_tuple$878 = _func_attrs_tuple$881;
      goto $join$877;
    }
    goto $joinlet$2560;
    $join$877:;
    index_func_map$870 = _index_func_map$876;
    func_attrs_tuple$871 = func_attrs_tuple$878;
    goto $join$869;
    $joinlet$2560:;
  }
  goto $joinlet$2559;
  $join$869:;
  moonbit_decref(index_func_map$870);
  _field$2177 = func_attrs_tuple$871->$0;
  _tmp$1931 = _field$2177;
  moonbit_incref(_tmp$1931);
  F2$1928
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$1928)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$1928)->$0 = _tmp$1931;
  _field$2176 = func_attrs_tuple$871->$1;
  _cnt$2448 = Moonbit_object_header(func_attrs_tuple$871)->rc;
  if (_cnt$2448 > 1) {
    int32_t _new_cnt$2450;
    moonbit_incref(_field$2176);
    _new_cnt$2450 = _cnt$2448 - 1;
    Moonbit_object_header(func_attrs_tuple$871)->rc = _new_cnt$2450;
  } else if (_cnt$2448 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2449 =
      func_attrs_tuple$871->$0;
    moonbit_decref(_field$2449);
    moonbit_free(func_attrs_tuple$871);
  }
  _tmp$1930 = _field$2176;
  _tmp$1929
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1929)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1929->$0 = file_filter$845;
  _tmp$1929->$1 = index_filter$846;
  _tmp$1929->$2 = _tmp$1930;
  k$872
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$872)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$872->$0 = F2$1928;
  k$872->$1 = _tmp$1929;
  return k$872;
  $joinlet$2559:;
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$834
) {
  int32_t _field$2182 = self$834->$1;
  int32_t len$1919;
  moonbit_decref(self$834);
  len$1919 = _field$2182;
  return len$1919 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$832,
  struct $$moonbitlang$core$builtin$Logger logger$833
) {
  moonbit_string_t _tmp$1918 = self$832;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$1917 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$1918);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$1917, logger$833
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$818,
  struct $$moonbitlang$core$builtin$Logger logger$831
) {
  struct $StringView _field$2191 =
    (struct $StringView){self$818->$0_1, self$818->$0_2, self$818->$0_0};
  struct $StringView pkg$817 = _field$2191;
  int32_t _tmp$1916 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$1915;
  int64_t _bind$819;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$820;
  struct $StringView _field$2190;
  struct $StringView _module_name$827;
  void* _field$2189;
  int32_t _cnt$2451;
  void* _package_name$828;
  struct $StringView _field$2187;
  struct $StringView filename$1898;
  struct $StringView _field$2186;
  struct $StringView start_line$1899;
  struct $StringView _field$2185;
  struct $StringView start_column$1900;
  struct $StringView _field$2184;
  struct $StringView end_line$1901;
  struct $StringView _field$2183;
  int32_t _cnt$2455;
  struct $StringView end_column$1902;
  struct $$moonbitlang$core$builtin$Logger _bind$1897;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$1915
  = (struct $StringView){
    0, _tmp$1916, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$817.$0);
  moonbit_incref(pkg$817.$0);
  _bind$819 = $StringView$$find(pkg$817, _tmp$1915);
  if (_bind$819 == 4294967296ll) {
    void* None$1903 =
      (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
    _bind$820
    = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
        sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
      );
    Moonbit_object_header(_bind$820)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
      )
      >> 2,
        2,
        0
    );
    _bind$820->$0_0 = pkg$817.$0;
    _bind$820->$0_1 = pkg$817.$1;
    _bind$820->$0_2 = pkg$817.$2;
    _bind$820->$1 = None$1903;
  } else {
    int64_t _Some$821 = _bind$819;
    int32_t _first_slash$822 = (int32_t)_Some$821;
    int32_t _tmp$1914 = _first_slash$822 + 1;
    struct $StringView _tmp$1911;
    int32_t _tmp$1913;
    struct $StringView _tmp$1912;
    int64_t _bind$823;
    moonbit_incref(pkg$817.$0);
    _tmp$1911 = $StringView$$view$inner(pkg$817, _tmp$1914, 4294967296ll);
    _tmp$1913
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$1912
    = (struct $StringView){
      0, _tmp$1913, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$823 = $StringView$$find(_tmp$1911, _tmp$1912);
    if (_bind$823 == 4294967296ll) {
      void* None$1904 =
        (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$820
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$820)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$820->$0_0 = pkg$817.$0;
      _bind$820->$0_1 = pkg$817.$1;
      _bind$820->$0_2 = pkg$817.$2;
      _bind$820->$1 = None$1904;
    } else {
      int64_t _Some$824 = _bind$823;
      int32_t _second_slash$825 = (int32_t)_Some$824;
      int32_t _tmp$1910 = _first_slash$822 + 1;
      int32_t module_name_end$826 = _tmp$1910 + _second_slash$825;
      int64_t _tmp$1909 = (int64_t)module_name_end$826;
      struct $StringView _tmp$1905;
      int32_t _tmp$1908;
      struct $StringView _tmp$1907;
      void* Some$1906;
      moonbit_incref(pkg$817.$0);
      _tmp$1905 = $StringView$$view$inner(pkg$817, 0, _tmp$1909);
      _tmp$1908 = module_name_end$826 + 1;
      _tmp$1907 = $StringView$$view$inner(pkg$817, _tmp$1908, 4294967296ll);
      Some$1906
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$1906)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$1906)->$0_0
      = _tmp$1907.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1906)->$0_1
      = _tmp$1907.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1906)->$0_2
      = _tmp$1907.$2;
      _bind$820
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$820)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$820->$0_0 = _tmp$1905.$0;
      _bind$820->$0_1 = _tmp$1905.$1;
      _bind$820->$0_2 = _tmp$1905.$2;
      _bind$820->$1 = Some$1906;
    }
  }
  _field$2190
  = (struct $StringView){
    _bind$820->$0_1, _bind$820->$0_2, _bind$820->$0_0
  };
  _module_name$827 = _field$2190;
  _field$2189 = _bind$820->$1;
  _cnt$2451 = Moonbit_object_header(_bind$820)->rc;
  if (_cnt$2451 > 1) {
    int32_t _new_cnt$2452;
    moonbit_incref(_field$2189);
    moonbit_incref(_module_name$827.$0);
    _new_cnt$2452 = _cnt$2451 - 1;
    Moonbit_object_header(_bind$820)->rc = _new_cnt$2452;
  } else if (_cnt$2451 == 1) {
    moonbit_free(_bind$820);
  }
  _package_name$828 = _field$2189;
  switch (Moonbit_object_tag(_package_name$828)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$829 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$828;
      struct $StringView _field$2188 =
        (struct $StringView){
          _Some$829->$0_1, _Some$829->$0_2, _Some$829->$0_0
        };
      int32_t _cnt$2453 = Moonbit_object_header(_Some$829)->rc;
      struct $StringView _pkg_name$830;
      struct $$moonbitlang$core$builtin$Logger _bind$1896;
      if (_cnt$2453 > 1) {
        int32_t _new_cnt$2454;
        moonbit_incref(_field$2188.$0);
        _new_cnt$2454 = _cnt$2453 - 1;
        Moonbit_object_header(_Some$829)->rc = _new_cnt$2454;
      } else if (_cnt$2453 == 1) {
        moonbit_free(_Some$829);
      }
      _pkg_name$830 = _field$2188;
      if (logger$831.$1) {
        moonbit_incref(logger$831.$1);
      }
      logger$831.$0->$method_2(logger$831.$1, _pkg_name$830);
      _bind$1896 = logger$831;
      if (_bind$1896.$1) {
        moonbit_incref(_bind$1896.$1);
      }
      _bind$1896.$0->$method_3(_bind$1896.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$828);
      break;
    }
  }
  _field$2187
  = (struct $StringView){
    self$818->$1_1, self$818->$1_2, self$818->$1_0
  };
  filename$1898 = _field$2187;
  moonbit_incref(filename$1898.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, filename$1898);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2186
  = (struct $StringView){
    self$818->$2_1, self$818->$2_2, self$818->$2_0
  };
  start_line$1899 = _field$2186;
  moonbit_incref(start_line$1899.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, start_line$1899);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2185
  = (struct $StringView){
    self$818->$3_1, self$818->$3_2, self$818->$3_0
  };
  start_column$1900 = _field$2185;
  moonbit_incref(start_column$1900.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, start_column$1900);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 45);
  _field$2184
  = (struct $StringView){
    self$818->$4_1, self$818->$4_2, self$818->$4_0
  };
  end_line$1901 = _field$2184;
  moonbit_incref(end_line$1901.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, end_line$1901);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2183
  = (struct $StringView){
    self$818->$5_1, self$818->$5_2, self$818->$5_0
  };
  _cnt$2455 = Moonbit_object_header(self$818)->rc;
  if (_cnt$2455 > 1) {
    int32_t _new_cnt$2461;
    moonbit_incref(_field$2183.$0);
    _new_cnt$2461 = _cnt$2455 - 1;
    Moonbit_object_header(self$818)->rc = _new_cnt$2461;
  } else if (_cnt$2455 == 1) {
    struct $StringView _field$2460 =
      (struct $StringView){self$818->$4_1, self$818->$4_2, self$818->$4_0};
    struct $StringView _field$2459;
    struct $StringView _field$2458;
    struct $StringView _field$2457;
    struct $StringView _field$2456;
    moonbit_decref(_field$2460.$0);
    _field$2459
    = (struct $StringView){
      self$818->$3_1, self$818->$3_2, self$818->$3_0
    };
    moonbit_decref(_field$2459.$0);
    _field$2458
    = (struct $StringView){
      self$818->$2_1, self$818->$2_2, self$818->$2_0
    };
    moonbit_decref(_field$2458.$0);
    _field$2457
    = (struct $StringView){
      self$818->$1_1, self$818->$1_2, self$818->$1_0
    };
    moonbit_decref(_field$2457.$0);
    _field$2456
    = (struct $StringView){
      self$818->$0_1, self$818->$0_2, self$818->$0_0
    };
    moonbit_decref(_field$2456.$0);
    moonbit_free(self$818);
  }
  end_column$1902 = _field$2183;
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, end_column$1902);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 64);
  _bind$1897 = logger$831;
  _bind$1897.$0->$method_2(_bind$1897.$1, _module_name$827);
  return 0;
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$816) {
  moonbit_string_t _tmp$1895 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$816);
  moonbit_println(_tmp$1895);
  moonbit_decref(_tmp$1895);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$815,
  struct $$moonbitlang$core$builtin$Hasher* hasher$814
) {
  $$moonbitlang$core$builtin$Hasher$$combine_int(hasher$814, self$815);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$813,
  struct $$moonbitlang$core$builtin$Hasher* hasher$812
) {
  $$moonbitlang$core$builtin$Hasher$$combine_string(hasher$812, self$813);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$810,
  moonbit_string_t value$808
) {
  int32_t _end2448$807 = Moonbit_array_length(value$808);
  int32_t i$809 = 0;
  while (1) {
    if (i$809 < _end2448$807) {
      int32_t _tmp$1893 = value$808[i$809];
      uint32_t _tmp$1892 = *(uint32_t*)&_tmp$1893;
      int32_t _tmp$1894;
      moonbit_incref(self$810);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$810, _tmp$1892);
      _tmp$1894 = i$809 + 1;
      i$809 = _tmp$1894;
      continue;
    } else {
      moonbit_decref(self$810);
      moonbit_decref(value$808);
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$804,
  struct $$3c$String$3e$$3d$$3e$Int* f$806
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$2562 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1889;
  int32_t _tmp$1888;
  Moonbit_object_header(_closure$2562)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2562->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$2562->$0 = f$806;
  _tmp$1889 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$2562;
  _tmp$1888 = $$moonbitlang$core$builtin$Iter$$run$0(self$804, _tmp$1889);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$1888, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1890,
  moonbit_string_t k$805
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$1891 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$1890;
  struct $$3c$String$3e$$3d$$3e$Int* _field$2192 = _casted_env$1891->$0;
  int32_t _cnt$2462 = Moonbit_object_header(_casted_env$1891)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$806;
  if (_cnt$2462 > 1) {
    int32_t _new_cnt$2463;
    moonbit_incref(_field$2192);
    _new_cnt$2463 = _cnt$2462 - 1;
    Moonbit_object_header(_casted_env$1891)->rc = _new_cnt$2463;
  } else if (_cnt$2462 == 1) {
    moonbit_free(_casted_env$1891);
  }
  f$806 = _field$2192;
  if (f$806->code(f$806, k$805)) {
    return 0;
  } else {
    return 1;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$802,
  int32_t idx$803
) {
  moonbit_string_t* _tmp$1887 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$802);
  moonbit_string_t _tmp$2193;
  if (idx$803 < 0 || idx$803 >= Moonbit_array_length(_tmp$1887)) {
    moonbit_panic();
  }
  _tmp$2193 = (moonbit_string_t)_tmp$1887[idx$803];
  moonbit_incref(_tmp$2193);
  moonbit_decref(_tmp$1887);
  return _tmp$2193;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$800,
  int32_t idx$801
) {
  struct $$3c$String$2a$Int$3e$** _tmp$1886 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$800);
  struct $$3c$String$2a$Int$3e$* _tmp$2194;
  if (idx$801 < 0 || idx$801 >= Moonbit_array_length(_tmp$1886)) {
    moonbit_panic();
  }
  _tmp$2194 = (struct $$3c$String$2a$Int$3e$*)_tmp$1886[idx$801];
  if (_tmp$2194) {
    moonbit_incref(_tmp$2194);
  }
  moonbit_decref(_tmp$1886);
  return _tmp$2194;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$796,
  int32_t key$792
) {
  int32_t hash$791 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$792);
  int32_t capacity_mask$1885 = self$796->$3;
  int32_t _tmp$1884 = hash$791 & capacity_mask$1885;
  int32_t i$793 = 0;
  int32_t idx$794 = _tmp$1884;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2198 =
      self$796->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1883 =
      _field$2198;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2197;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$795;
    if (idx$794 < 0 || idx$794 >= Moonbit_array_length(entries$1883)) {
      moonbit_panic();
    }
    _tmp$2197
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1883[
        idx$794
      ];
    _bind$795 = _tmp$2197;
    if (_bind$795 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1872;
      if (_bind$795) {
        moonbit_incref(_bind$795);
      }
      moonbit_decref(self$796);
      if (_bind$795) {
        moonbit_decref(_bind$795);
      }
      _tmp$1872 = 0;
      return _tmp$1872;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$797 =
        _bind$795;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$798 =
        _Some$797;
      int32_t hash$1874 = _entry$798->$3;
      int32_t _if_result$2564;
      int32_t _field$2195;
      int32_t psl$1877;
      int32_t _tmp$1879;
      int32_t _tmp$1881;
      int32_t capacity_mask$1882;
      int32_t _tmp$1880;
      if (hash$1874 == hash$791) {
        int32_t key$1873 = _entry$798->$4;
        _if_result$2564 = key$1873 == key$792;
      } else {
        _if_result$2564 = 0;
      }
      if (_if_result$2564) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2196;
        int32_t _cnt$2464;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1876;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1875;
        moonbit_incref(_entry$798);
        moonbit_decref(self$796);
        _field$2196 = _entry$798->$5;
        _cnt$2464 = Moonbit_object_header(_entry$798)->rc;
        if (_cnt$2464 > 1) {
          int32_t _new_cnt$2466;
          moonbit_incref(_field$2196);
          _new_cnt$2466 = _cnt$2464 - 1;
          Moonbit_object_header(_entry$798)->rc = _new_cnt$2466;
        } else if (_cnt$2464 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2465 =
            _entry$798->$1;
          if (_field$2465) {
            moonbit_decref(_field$2465);
          }
          moonbit_free(_entry$798);
        }
        value$1876 = _field$2196;
        _tmp$1875 = value$1876;
        return _tmp$1875;
      } else {
        moonbit_incref(_entry$798);
      }
      _field$2195 = _entry$798->$2;
      moonbit_decref(_entry$798);
      psl$1877 = _field$2195;
      if (i$793 > psl$1877) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1878;
        moonbit_decref(self$796);
        _tmp$1878 = 0;
        return _tmp$1878;
      }
      _tmp$1879 = i$793 + 1;
      _tmp$1881 = idx$794 + 1;
      capacity_mask$1882 = self$796->$3;
      _tmp$1880 = _tmp$1881 & capacity_mask$1882;
      i$793 = _tmp$1879;
      idx$794 = _tmp$1880;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$787,
  moonbit_string_t key$783
) {
  int32_t hash$782;
  int32_t capacity_mask$1871;
  int32_t _tmp$1870;
  int32_t i$784;
  int32_t idx$785;
  moonbit_incref(key$783);
  hash$782 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$783);
  capacity_mask$1871 = self$787->$3;
  _tmp$1870 = hash$782 & capacity_mask$1871;
  i$784 = 0;
  idx$785 = _tmp$1870;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2204 =
      self$787->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1869 =
      _field$2204;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2203;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$786;
    if (idx$785 < 0 || idx$785 >= Moonbit_array_length(entries$1869)) {
      moonbit_panic();
    }
    _tmp$2203
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1869[
        idx$785
      ];
    _bind$786 = _tmp$2203;
    if (_bind$786 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1858;
      if (_bind$786) {
        moonbit_incref(_bind$786);
      }
      moonbit_decref(self$787);
      if (_bind$786) {
        moonbit_decref(_bind$786);
      }
      moonbit_decref(key$783);
      _tmp$1858 = 0;
      return _tmp$1858;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$788 =
        _bind$786;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$789 =
        _Some$788;
      int32_t hash$1860 = _entry$789->$3;
      int32_t _if_result$2566;
      int32_t _field$2199;
      int32_t psl$1863;
      int32_t _tmp$1865;
      int32_t _tmp$1867;
      int32_t capacity_mask$1868;
      int32_t _tmp$1866;
      if (hash$1860 == hash$782) {
        moonbit_string_t _field$2202 = _entry$789->$4;
        moonbit_string_t key$1859 = _field$2202;
        int32_t _tmp$2201 = moonbit_val_array_equal(key$1859, key$783);
        _if_result$2566 = _tmp$2201;
      } else {
        _if_result$2566 = 0;
      }
      if (_if_result$2566) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2200;
        int32_t _cnt$2467;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1862;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1861;
        moonbit_incref(_entry$789);
        moonbit_decref(self$787);
        moonbit_decref(key$783);
        _field$2200 = _entry$789->$5;
        _cnt$2467 = Moonbit_object_header(_entry$789)->rc;
        if (_cnt$2467 > 1) {
          int32_t _new_cnt$2470;
          moonbit_incref(_field$2200);
          _new_cnt$2470 = _cnt$2467 - 1;
          Moonbit_object_header(_entry$789)->rc = _new_cnt$2470;
        } else if (_cnt$2467 == 1) {
          moonbit_string_t _field$2469 = _entry$789->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2468;
          moonbit_decref(_field$2469);
          _field$2468 = _entry$789->$1;
          if (_field$2468) {
            moonbit_decref(_field$2468);
          }
          moonbit_free(_entry$789);
        }
        value$1862 = _field$2200;
        _tmp$1861 = value$1862;
        return _tmp$1861;
      } else {
        moonbit_incref(_entry$789);
      }
      _field$2199 = _entry$789->$2;
      moonbit_decref(_entry$789);
      psl$1863 = _field$2199;
      if (i$784 > psl$1863) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1864;
        moonbit_decref(self$787);
        moonbit_decref(key$783);
        _tmp$1864 = 0;
        return _tmp$1864;
      }
      _tmp$1865 = i$784 + 1;
      _tmp$1867 = idx$785 + 1;
      capacity_mask$1868 = self$787->$3;
      _tmp$1866 = _tmp$1867 & capacity_mask$1868;
      i$784 = _tmp$1865;
      idx$785 = _tmp$1866;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$778,
  int32_t key$774
) {
  int32_t hash$773 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$774);
  int32_t capacity_mask$1857 = self$778->$3;
  int32_t _tmp$1856 = hash$773 & capacity_mask$1857;
  int32_t i$775 = 0;
  int32_t idx$776 = _tmp$1856;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2208 =
      self$778->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1855 =
      _field$2208;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2207;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$777;
    if (idx$776 < 0 || idx$776 >= Moonbit_array_length(entries$1855)) {
      moonbit_panic();
    }
    _tmp$2207
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1855[
        idx$776
      ];
    _bind$777 = _tmp$2207;
    if (_bind$777 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1844;
      if (_bind$777) {
        moonbit_incref(_bind$777);
      }
      moonbit_decref(self$778);
      if (_bind$777) {
        moonbit_decref(_bind$777);
      }
      _tmp$1844 = 0;
      return _tmp$1844;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$779 =
        _bind$777;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$780 =
        _Some$779;
      int32_t hash$1846 = _entry$780->$3;
      int32_t _if_result$2568;
      int32_t _field$2205;
      int32_t psl$1849;
      int32_t _tmp$1851;
      int32_t _tmp$1853;
      int32_t capacity_mask$1854;
      int32_t _tmp$1852;
      if (hash$1846 == hash$773) {
        int32_t key$1845 = _entry$780->$4;
        _if_result$2568 = key$1845 == key$774;
      } else {
        _if_result$2568 = 0;
      }
      if (_if_result$2568) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2206;
        int32_t _cnt$2471;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1848;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1847;
        moonbit_incref(_entry$780);
        moonbit_decref(self$778);
        _field$2206 = _entry$780->$5;
        _cnt$2471 = Moonbit_object_header(_entry$780)->rc;
        if (_cnt$2471 > 1) {
          int32_t _new_cnt$2473;
          moonbit_incref(_field$2206);
          _new_cnt$2473 = _cnt$2471 - 1;
          Moonbit_object_header(_entry$780)->rc = _new_cnt$2473;
        } else if (_cnt$2471 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2472 =
            _entry$780->$1;
          if (_field$2472) {
            moonbit_decref(_field$2472);
          }
          moonbit_free(_entry$780);
        }
        value$1848 = _field$2206;
        _tmp$1847 = value$1848;
        return _tmp$1847;
      } else {
        moonbit_incref(_entry$780);
      }
      _field$2205 = _entry$780->$2;
      moonbit_decref(_entry$780);
      psl$1849 = _field$2205;
      if (i$775 > psl$1849) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1850;
        moonbit_decref(self$778);
        _tmp$1850 = 0;
        return _tmp$1850;
      }
      _tmp$1851 = i$775 + 1;
      _tmp$1853 = idx$776 + 1;
      capacity_mask$1854 = self$778->$3;
      _tmp$1852 = _tmp$1853 & capacity_mask$1854;
      i$775 = _tmp$1851;
      idx$776 = _tmp$1852;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$769,
  moonbit_string_t key$765
) {
  int32_t hash$764;
  int32_t capacity_mask$1843;
  int32_t _tmp$1842;
  int32_t i$766;
  int32_t idx$767;
  moonbit_incref(key$765);
  hash$764 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$765);
  capacity_mask$1843 = self$769->$3;
  _tmp$1842 = hash$764 & capacity_mask$1843;
  i$766 = 0;
  idx$767 = _tmp$1842;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2214 =
      self$769->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1841 =
      _field$2214;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2213;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$768;
    if (idx$767 < 0 || idx$767 >= Moonbit_array_length(entries$1841)) {
      moonbit_panic();
    }
    _tmp$2213
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1841[
        idx$767
      ];
    _bind$768 = _tmp$2213;
    if (_bind$768 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1830;
      if (_bind$768) {
        moonbit_incref(_bind$768);
      }
      moonbit_decref(self$769);
      if (_bind$768) {
        moonbit_decref(_bind$768);
      }
      moonbit_decref(key$765);
      _tmp$1830 = 0;
      return _tmp$1830;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$770 =
        _bind$768;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$771 =
        _Some$770;
      int32_t hash$1832 = _entry$771->$3;
      int32_t _if_result$2570;
      int32_t _field$2209;
      int32_t psl$1835;
      int32_t _tmp$1837;
      int32_t _tmp$1839;
      int32_t capacity_mask$1840;
      int32_t _tmp$1838;
      if (hash$1832 == hash$764) {
        moonbit_string_t _field$2212 = _entry$771->$4;
        moonbit_string_t key$1831 = _field$2212;
        int32_t _tmp$2211 = moonbit_val_array_equal(key$1831, key$765);
        _if_result$2570 = _tmp$2211;
      } else {
        _if_result$2570 = 0;
      }
      if (_if_result$2570) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2210;
        int32_t _cnt$2474;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1834;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1833;
        moonbit_incref(_entry$771);
        moonbit_decref(self$769);
        moonbit_decref(key$765);
        _field$2210 = _entry$771->$5;
        _cnt$2474 = Moonbit_object_header(_entry$771)->rc;
        if (_cnt$2474 > 1) {
          int32_t _new_cnt$2477;
          moonbit_incref(_field$2210);
          _new_cnt$2477 = _cnt$2474 - 1;
          Moonbit_object_header(_entry$771)->rc = _new_cnt$2477;
        } else if (_cnt$2474 == 1) {
          moonbit_string_t _field$2476 = _entry$771->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2475;
          moonbit_decref(_field$2476);
          _field$2475 = _entry$771->$1;
          if (_field$2475) {
            moonbit_decref(_field$2475);
          }
          moonbit_free(_entry$771);
        }
        value$1834 = _field$2210;
        _tmp$1833 = value$1834;
        return _tmp$1833;
      } else {
        moonbit_incref(_entry$771);
      }
      _field$2209 = _entry$771->$2;
      moonbit_decref(_entry$771);
      psl$1835 = _field$2209;
      if (i$766 > psl$1835) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1836;
        moonbit_decref(self$769);
        moonbit_decref(key$765);
        _tmp$1836 = 0;
        return _tmp$1836;
      }
      _tmp$1837 = i$766 + 1;
      _tmp$1839 = idx$767 + 1;
      capacity_mask$1840 = self$769->$3;
      _tmp$1838 = _tmp$1839 & capacity_mask$1840;
      i$766 = _tmp$1837;
      idx$767 = _tmp$1838;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$760,
  int32_t key$756
) {
  int32_t hash$755 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$756);
  int32_t capacity_mask$1829 = self$760->$3;
  int32_t _tmp$1828 = hash$755 & capacity_mask$1829;
  int32_t i$757 = 0;
  int32_t idx$758 = _tmp$1828;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2218 =
      self$760->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1827 =
      _field$2218;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2217;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$759;
    if (idx$758 < 0 || idx$758 >= Moonbit_array_length(entries$1827)) {
      moonbit_panic();
    }
    _tmp$2217
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1827[
        idx$758
      ];
    _bind$759 = _tmp$2217;
    if (_bind$759 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1816;
      if (_bind$759) {
        moonbit_incref(_bind$759);
      }
      moonbit_decref(self$760);
      if (_bind$759) {
        moonbit_decref(_bind$759);
      }
      _tmp$1816 = 0;
      return _tmp$1816;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$761 =
        _bind$759;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$762 =
        _Some$761;
      int32_t hash$1818 = _entry$762->$3;
      int32_t _if_result$2572;
      int32_t _field$2215;
      int32_t psl$1821;
      int32_t _tmp$1823;
      int32_t _tmp$1825;
      int32_t capacity_mask$1826;
      int32_t _tmp$1824;
      if (hash$1818 == hash$755) {
        int32_t key$1817 = _entry$762->$4;
        _if_result$2572 = key$1817 == key$756;
      } else {
        _if_result$2572 = 0;
      }
      if (_if_result$2572) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2216;
        int32_t _cnt$2478;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1820;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1819;
        moonbit_incref(_entry$762);
        moonbit_decref(self$760);
        _field$2216 = _entry$762->$5;
        _cnt$2478 = Moonbit_object_header(_entry$762)->rc;
        if (_cnt$2478 > 1) {
          int32_t _new_cnt$2480;
          moonbit_incref(_field$2216);
          _new_cnt$2480 = _cnt$2478 - 1;
          Moonbit_object_header(_entry$762)->rc = _new_cnt$2480;
        } else if (_cnt$2478 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2479 =
            _entry$762->$1;
          if (_field$2479) {
            moonbit_decref(_field$2479);
          }
          moonbit_free(_entry$762);
        }
        value$1820 = _field$2216;
        _tmp$1819 = value$1820;
        return _tmp$1819;
      } else {
        moonbit_incref(_entry$762);
      }
      _field$2215 = _entry$762->$2;
      moonbit_decref(_entry$762);
      psl$1821 = _field$2215;
      if (i$757 > psl$1821) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1822;
        moonbit_decref(self$760);
        _tmp$1822 = 0;
        return _tmp$1822;
      }
      _tmp$1823 = i$757 + 1;
      _tmp$1825 = idx$758 + 1;
      capacity_mask$1826 = self$760->$3;
      _tmp$1824 = _tmp$1825 & capacity_mask$1826;
      i$757 = _tmp$1823;
      idx$758 = _tmp$1824;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$751,
  moonbit_string_t key$747
) {
  int32_t hash$746;
  int32_t capacity_mask$1815;
  int32_t _tmp$1814;
  int32_t i$748;
  int32_t idx$749;
  moonbit_incref(key$747);
  hash$746 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$747);
  capacity_mask$1815 = self$751->$3;
  _tmp$1814 = hash$746 & capacity_mask$1815;
  i$748 = 0;
  idx$749 = _tmp$1814;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2224 =
      self$751->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1813 =
      _field$2224;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2223;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$750;
    if (idx$749 < 0 || idx$749 >= Moonbit_array_length(entries$1813)) {
      moonbit_panic();
    }
    _tmp$2223
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1813[
        idx$749
      ];
    _bind$750 = _tmp$2223;
    if (_bind$750 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1802;
      if (_bind$750) {
        moonbit_incref(_bind$750);
      }
      moonbit_decref(self$751);
      if (_bind$750) {
        moonbit_decref(_bind$750);
      }
      moonbit_decref(key$747);
      _tmp$1802 = 0;
      return _tmp$1802;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$752 =
        _bind$750;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$753 =
        _Some$752;
      int32_t hash$1804 = _entry$753->$3;
      int32_t _if_result$2574;
      int32_t _field$2219;
      int32_t psl$1807;
      int32_t _tmp$1809;
      int32_t _tmp$1811;
      int32_t capacity_mask$1812;
      int32_t _tmp$1810;
      if (hash$1804 == hash$746) {
        moonbit_string_t _field$2222 = _entry$753->$4;
        moonbit_string_t key$1803 = _field$2222;
        int32_t _tmp$2221 = moonbit_val_array_equal(key$1803, key$747);
        _if_result$2574 = _tmp$2221;
      } else {
        _if_result$2574 = 0;
      }
      if (_if_result$2574) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2220;
        int32_t _cnt$2481;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1806;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1805;
        moonbit_incref(_entry$753);
        moonbit_decref(self$751);
        moonbit_decref(key$747);
        _field$2220 = _entry$753->$5;
        _cnt$2481 = Moonbit_object_header(_entry$753)->rc;
        if (_cnt$2481 > 1) {
          int32_t _new_cnt$2484;
          moonbit_incref(_field$2220);
          _new_cnt$2484 = _cnt$2481 - 1;
          Moonbit_object_header(_entry$753)->rc = _new_cnt$2484;
        } else if (_cnt$2481 == 1) {
          moonbit_string_t _field$2483 = _entry$753->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2482;
          moonbit_decref(_field$2483);
          _field$2482 = _entry$753->$1;
          if (_field$2482) {
            moonbit_decref(_field$2482);
          }
          moonbit_free(_entry$753);
        }
        value$1806 = _field$2220;
        _tmp$1805 = value$1806;
        return _tmp$1805;
      } else {
        moonbit_incref(_entry$753);
      }
      _field$2219 = _entry$753->$2;
      moonbit_decref(_entry$753);
      psl$1807 = _field$2219;
      if (i$748 > psl$1807) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1808;
        moonbit_decref(self$751);
        moonbit_decref(key$747);
        _tmp$1808 = 0;
        return _tmp$1808;
      }
      _tmp$1809 = i$748 + 1;
      _tmp$1811 = idx$749 + 1;
      capacity_mask$1812 = self$751->$3;
      _tmp$1810 = _tmp$1811 & capacity_mask$1812;
      i$748 = _tmp$1809;
      idx$749 = _tmp$1810;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$739
) {
  int32_t length$738;
  int32_t capacity$740;
  int32_t _tmp$1793;
  int32_t _tmp$1792;
  int32_t _tmp$1801;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$741;
  int32_t _len$742;
  int32_t _i$743;
  moonbit_incref(arr$739.$0);
  length$738 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$739);
  capacity$740 = $Int$$next_power_of_two(length$738);
  _tmp$1793 = capacity$740;
  _tmp$1792 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1793);
  if (length$738 > _tmp$1792) {
    int32_t _tmp$1794 = capacity$740;
    capacity$740 = _tmp$1794 * 2;
  }
  _tmp$1801 = capacity$740;
  m$741 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$1801);
  moonbit_incref(arr$739.$0);
  _len$742 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$739);
  _i$743 = 0;
  while (1) {
    if (_i$743 < _len$742) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2228 =
        arr$739.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1797 =
        _field$2228;
      int32_t start$1799 = arr$739.$1;
      int32_t _tmp$1798 = start$1799 + _i$743;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2227 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1797[
          _tmp$1798
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$744 =
        _tmp$2227;
      moonbit_string_t _field$2226 = e$744->$0;
      moonbit_string_t _tmp$1795 = _field$2226;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2225 =
        e$744->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1796 =
        _field$2225;
      int32_t _tmp$1800;
      moonbit_incref(_tmp$1796);
      moonbit_incref(_tmp$1795);
      moonbit_incref(m$741);
      $$moonbitlang$core$builtin$Map$$set$3(m$741, _tmp$1795, _tmp$1796);
      _tmp$1800 = _i$743 + 1;
      _i$743 = _tmp$1800;
      continue;
    } else {
      moonbit_decref(arr$739.$0);
    }
    break;
  }
  return m$741;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$731
) {
  int32_t length$730;
  int32_t capacity$732;
  int32_t _tmp$1783;
  int32_t _tmp$1782;
  int32_t _tmp$1791;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$733;
  int32_t _len$734;
  int32_t _i$735;
  moonbit_incref(arr$731.$0);
  length$730 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$731);
  capacity$732 = $Int$$next_power_of_two(length$730);
  _tmp$1783 = capacity$732;
  _tmp$1782 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1783);
  if (length$730 > _tmp$1782) {
    int32_t _tmp$1784 = capacity$732;
    capacity$732 = _tmp$1784 * 2;
  }
  _tmp$1791 = capacity$732;
  m$733 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$1791);
  moonbit_incref(arr$731.$0);
  _len$734 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$731);
  _i$735 = 0;
  while (1) {
    if (_i$735 < _len$734) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2232 =
        arr$731.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1787 =
        _field$2232;
      int32_t start$1789 = arr$731.$1;
      int32_t _tmp$1788 = start$1789 + _i$735;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2231 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1787[
          _tmp$1788
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$736 =
        _tmp$2231;
      moonbit_string_t _field$2230 = e$736->$0;
      moonbit_string_t _tmp$1785 = _field$2230;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2229 =
        e$736->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1786 =
        _field$2229;
      int32_t _tmp$1790;
      moonbit_incref(_tmp$1786);
      moonbit_incref(_tmp$1785);
      moonbit_incref(m$733);
      $$moonbitlang$core$builtin$Map$$set$2(m$733, _tmp$1785, _tmp$1786);
      _tmp$1790 = _i$735 + 1;
      _i$735 = _tmp$1790;
      continue;
    } else {
      moonbit_decref(arr$731.$0);
    }
    break;
  }
  return m$733;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$723
) {
  int32_t length$722;
  int32_t capacity$724;
  int32_t _tmp$1773;
  int32_t _tmp$1772;
  int32_t _tmp$1781;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$725;
  int32_t _len$726;
  int32_t _i$727;
  moonbit_incref(arr$723.$0);
  length$722 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$723);
  capacity$724 = $Int$$next_power_of_two(length$722);
  _tmp$1773 = capacity$724;
  _tmp$1772 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1773);
  if (length$722 > _tmp$1772) {
    int32_t _tmp$1774 = capacity$724;
    capacity$724 = _tmp$1774 * 2;
  }
  _tmp$1781 = capacity$724;
  m$725 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$1781);
  moonbit_incref(arr$723.$0);
  _len$726 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$723);
  _i$727 = 0;
  while (1) {
    if (_i$727 < _len$726) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2235 =
        arr$723.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$1777 =
        _field$2235;
      int32_t start$1779 = arr$723.$1;
      int32_t _tmp$1778 = start$1779 + _i$727;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2234 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$1777[
          _tmp$1778
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$728 =
        _tmp$2234;
      int32_t _tmp$1775 = e$728->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2233 =
        e$728->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1776 =
        _field$2233;
      int32_t _tmp$1780;
      moonbit_incref(_tmp$1776);
      moonbit_incref(m$725);
      $$moonbitlang$core$builtin$Map$$set$1(m$725, _tmp$1775, _tmp$1776);
      _tmp$1780 = _i$727 + 1;
      _i$727 = _tmp$1780;
      continue;
    } else {
      moonbit_decref(arr$723.$0);
    }
    break;
  }
  return m$725;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$715
) {
  int32_t length$714;
  int32_t capacity$716;
  int32_t _tmp$1763;
  int32_t _tmp$1762;
  int32_t _tmp$1771;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$717;
  int32_t _len$718;
  int32_t _i$719;
  moonbit_incref(arr$715.$0);
  length$714 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$715);
  capacity$716 = $Int$$next_power_of_two(length$714);
  _tmp$1763 = capacity$716;
  _tmp$1762 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1763);
  if (length$714 > _tmp$1762) {
    int32_t _tmp$1764 = capacity$716;
    capacity$716 = _tmp$1764 * 2;
  }
  _tmp$1771 = capacity$716;
  m$717 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$1771);
  moonbit_incref(arr$715.$0);
  _len$718 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$715);
  _i$719 = 0;
  while (1) {
    if (_i$719 < _len$718) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2239 =
        arr$715.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1767 =
        _field$2239;
      int32_t start$1769 = arr$715.$1;
      int32_t _tmp$1768 = start$1769 + _i$719;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2238 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1767[
          _tmp$1768
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$720 =
        _tmp$2238;
      moonbit_string_t _field$2237 = e$720->$0;
      moonbit_string_t _tmp$1765 = _field$2237;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2236 =
        e$720->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1766 =
        _field$2236;
      int32_t _tmp$1770;
      moonbit_incref(_tmp$1766);
      moonbit_incref(_tmp$1765);
      moonbit_incref(m$717);
      $$moonbitlang$core$builtin$Map$$set$0(m$717, _tmp$1765, _tmp$1766);
      _tmp$1770 = _i$719 + 1;
      _i$719 = _tmp$1770;
      continue;
    } else {
      moonbit_decref(arr$715.$0);
    }
    break;
  }
  return m$717;
}

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$711,
  moonbit_string_t key$712,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$713
) {
  int32_t _tmp$1761;
  moonbit_incref(key$712);
  _tmp$1761 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$712);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$711, key$712, value$713, _tmp$1761
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$708,
  moonbit_string_t key$709,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$710
) {
  int32_t _tmp$1760;
  moonbit_incref(key$709);
  _tmp$1760 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$709);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$708, key$709, value$710, _tmp$1760
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$705,
  int32_t key$706,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$707
) {
  int32_t _tmp$1759 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$706);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$705, key$706, value$707, _tmp$1759
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$702,
  moonbit_string_t key$703,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$704
) {
  int32_t _tmp$1758;
  moonbit_incref(key$703);
  _tmp$1758 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$703);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$702, key$703, value$704, _tmp$1758
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$692
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2246 =
    self$692->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$691 =
    _field$2246;
  int32_t capacity$1757 = self$692->$2;
  int32_t new_capacity$693 = capacity$1757 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1752 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1751 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$693, _tmp$1752
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2245 =
    self$692->$0;
  int32_t _tmp$1753;
  int32_t capacity$1755;
  int32_t _tmp$1754;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1756;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2244;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$694;
  if (old_head$691) {
    moonbit_incref(old_head$691);
  }
  moonbit_decref(_old$2245);
  self$692->$0 = _tmp$1751;
  self$692->$2 = new_capacity$693;
  _tmp$1753 = new_capacity$693 - 1;
  self$692->$3 = _tmp$1753;
  capacity$1755 = self$692->$2;
  _tmp$1754 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1755);
  self$692->$4 = _tmp$1754;
  self$692->$1 = 0;
  _tmp$1756 = 0;
  _old$2244 = self$692->$5;
  if (_old$2244) {
    moonbit_decref(_old$2244);
  }
  self$692->$5 = _tmp$1756;
  self$692->$6 = -1;
  _param$694 = old_head$691;
  while (1) {
    if (_param$694 == 0) {
      if (_param$694) {
        moonbit_decref(_param$694);
      }
      moonbit_decref(self$692);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$695 =
        _param$694;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$696 =
        _Some$695;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2243 =
        _x$696->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$697 =
        _field$2243;
      moonbit_string_t _field$2242 = _x$696->$4;
      moonbit_string_t _key$698 = _field$2242;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2241 =
        _x$696->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$699 =
        _field$2241;
      int32_t _field$2240 = _x$696->$3;
      int32_t _cnt$2485 = Moonbit_object_header(_x$696)->rc;
      int32_t _hash$700;
      if (_cnt$2485 > 1) {
        int32_t _new_cnt$2486;
        moonbit_incref(_value$699);
        moonbit_incref(_key$698);
        if (_next$697) {
          moonbit_incref(_next$697);
        }
        _new_cnt$2486 = _cnt$2485 - 1;
        Moonbit_object_header(_x$696)->rc = _new_cnt$2486;
      } else if (_cnt$2485 == 1) {
        moonbit_free(_x$696);
      }
      _hash$700 = _field$2240;
      moonbit_incref(self$692);
      $$moonbitlang$core$builtin$Map$$set_with_hash$3(
        self$692, _key$698, _value$699, _hash$700
      );
      _param$694 = _next$697;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$681
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2253 =
    self$681->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$680 =
    _field$2253;
  int32_t capacity$1750 = self$681->$2;
  int32_t new_capacity$682 = capacity$1750 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1745 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1744 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$682, _tmp$1745
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2252 =
    self$681->$0;
  int32_t _tmp$1746;
  int32_t capacity$1748;
  int32_t _tmp$1747;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1749;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2251;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$683;
  if (old_head$680) {
    moonbit_incref(old_head$680);
  }
  moonbit_decref(_old$2252);
  self$681->$0 = _tmp$1744;
  self$681->$2 = new_capacity$682;
  _tmp$1746 = new_capacity$682 - 1;
  self$681->$3 = _tmp$1746;
  capacity$1748 = self$681->$2;
  _tmp$1747 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1748);
  self$681->$4 = _tmp$1747;
  self$681->$1 = 0;
  _tmp$1749 = 0;
  _old$2251 = self$681->$5;
  if (_old$2251) {
    moonbit_decref(_old$2251);
  }
  self$681->$5 = _tmp$1749;
  self$681->$6 = -1;
  _param$683 = old_head$680;
  while (1) {
    if (_param$683 == 0) {
      if (_param$683) {
        moonbit_decref(_param$683);
      }
      moonbit_decref(self$681);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$684 =
        _param$683;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$685 =
        _Some$684;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2250 =
        _x$685->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$686 =
        _field$2250;
      moonbit_string_t _field$2249 = _x$685->$4;
      moonbit_string_t _key$687 = _field$2249;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2248 =
        _x$685->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$688 =
        _field$2248;
      int32_t _field$2247 = _x$685->$3;
      int32_t _cnt$2487 = Moonbit_object_header(_x$685)->rc;
      int32_t _hash$689;
      if (_cnt$2487 > 1) {
        int32_t _new_cnt$2488;
        moonbit_incref(_value$688);
        moonbit_incref(_key$687);
        if (_next$686) {
          moonbit_incref(_next$686);
        }
        _new_cnt$2488 = _cnt$2487 - 1;
        Moonbit_object_header(_x$685)->rc = _new_cnt$2488;
      } else if (_cnt$2487 == 1) {
        moonbit_free(_x$685);
      }
      _hash$689 = _field$2247;
      moonbit_incref(self$681);
      $$moonbitlang$core$builtin$Map$$set_with_hash$2(
        self$681, _key$687, _value$688, _hash$689
      );
      _param$683 = _next$686;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$670
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2259 =
    self$670->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$669 =
    _field$2259;
  int32_t capacity$1743 = self$670->$2;
  int32_t new_capacity$671 = capacity$1743 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1738 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1737 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$671, _tmp$1738
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$2258 =
    self$670->$0;
  int32_t _tmp$1739;
  int32_t capacity$1741;
  int32_t _tmp$1740;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1742;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2257;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$672;
  if (old_head$669) {
    moonbit_incref(old_head$669);
  }
  moonbit_decref(_old$2258);
  self$670->$0 = _tmp$1737;
  self$670->$2 = new_capacity$671;
  _tmp$1739 = new_capacity$671 - 1;
  self$670->$3 = _tmp$1739;
  capacity$1741 = self$670->$2;
  _tmp$1740 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1741);
  self$670->$4 = _tmp$1740;
  self$670->$1 = 0;
  _tmp$1742 = 0;
  _old$2257 = self$670->$5;
  if (_old$2257) {
    moonbit_decref(_old$2257);
  }
  self$670->$5 = _tmp$1742;
  self$670->$6 = -1;
  _param$672 = old_head$669;
  while (1) {
    if (_param$672 == 0) {
      if (_param$672) {
        moonbit_decref(_param$672);
      }
      moonbit_decref(self$670);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$673 =
        _param$672;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _x$674 =
        _Some$673;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2256 =
        _x$674->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$675 =
        _field$2256;
      int32_t _key$676 = _x$674->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2255 =
        _x$674->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$677 =
        _field$2255;
      int32_t _field$2254 = _x$674->$3;
      int32_t _cnt$2489 = Moonbit_object_header(_x$674)->rc;
      int32_t _hash$678;
      if (_cnt$2489 > 1) {
        int32_t _new_cnt$2490;
        moonbit_incref(_value$677);
        if (_next$675) {
          moonbit_incref(_next$675);
        }
        _new_cnt$2490 = _cnt$2489 - 1;
        Moonbit_object_header(_x$674)->rc = _new_cnt$2490;
      } else if (_cnt$2489 == 1) {
        moonbit_free(_x$674);
      }
      _hash$678 = _field$2254;
      moonbit_incref(self$670);
      $$moonbitlang$core$builtin$Map$$set_with_hash$1(
        self$670, _key$676, _value$677, _hash$678
      );
      _param$672 = _next$675;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$659
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2266 =
    self$659->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$658 =
    _field$2266;
  int32_t capacity$1736 = self$659->$2;
  int32_t new_capacity$660 = capacity$1736 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1731 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1730 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$660, _tmp$1731
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2265 =
    self$659->$0;
  int32_t _tmp$1732;
  int32_t capacity$1734;
  int32_t _tmp$1733;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1735;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2264;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$661;
  if (old_head$658) {
    moonbit_incref(old_head$658);
  }
  moonbit_decref(_old$2265);
  self$659->$0 = _tmp$1730;
  self$659->$2 = new_capacity$660;
  _tmp$1732 = new_capacity$660 - 1;
  self$659->$3 = _tmp$1732;
  capacity$1734 = self$659->$2;
  _tmp$1733 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1734);
  self$659->$4 = _tmp$1733;
  self$659->$1 = 0;
  _tmp$1735 = 0;
  _old$2264 = self$659->$5;
  if (_old$2264) {
    moonbit_decref(_old$2264);
  }
  self$659->$5 = _tmp$1735;
  self$659->$6 = -1;
  _param$661 = old_head$658;
  while (1) {
    if (_param$661 == 0) {
      if (_param$661) {
        moonbit_decref(_param$661);
      }
      moonbit_decref(self$659);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$662 =
        _param$661;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$663 =
        _Some$662;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2263 =
        _x$663->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$664 =
        _field$2263;
      moonbit_string_t _field$2262 = _x$663->$4;
      moonbit_string_t _key$665 = _field$2262;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2261 =
        _x$663->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$666 =
        _field$2261;
      int32_t _field$2260 = _x$663->$3;
      int32_t _cnt$2491 = Moonbit_object_header(_x$663)->rc;
      int32_t _hash$667;
      if (_cnt$2491 > 1) {
        int32_t _new_cnt$2492;
        moonbit_incref(_value$666);
        moonbit_incref(_key$665);
        if (_next$664) {
          moonbit_incref(_next$664);
        }
        _new_cnt$2492 = _cnt$2491 - 1;
        Moonbit_object_header(_x$663)->rc = _new_cnt$2492;
      } else if (_cnt$2491 == 1) {
        moonbit_free(_x$663);
      }
      _hash$667 = _field$2260;
      moonbit_incref(self$659);
      $$moonbitlang$core$builtin$Map$$set_with_hash$0(
        self$659, _key$665, _value$666, _hash$667
      );
      _param$661 = _next$664;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$642,
  moonbit_string_t key$651,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$652,
  int32_t hash$650
) {
  int32_t size$1716 = self$642->$1;
  int32_t grow_at$1717 = self$642->$4;
  int32_t capacity_mask$1729;
  int32_t _tmp$1728;
  struct $$3c$Int$2a$Int$3e$* _bind$643;
  int32_t psl$644;
  int32_t idx$645;
  int32_t _idx$653;
  int32_t _field$2267;
  int32_t _psl$654;
  int32_t _bind$655;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$656;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$657;
  if (size$1716 >= grow_at$1717) {
    moonbit_incref(self$642);
    $$moonbitlang$core$builtin$Map$$grow$3(self$642);
  }
  capacity_mask$1729 = self$642->$3;
  _tmp$1728 = hash$650 & capacity_mask$1729;
  psl$644 = 0;
  idx$645 = _tmp$1728;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2272 =
      self$642->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1727 =
      _field$2272;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2271;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$646;
    if (idx$645 < 0 || idx$645 >= Moonbit_array_length(entries$1727)) {
      moonbit_panic();
    }
    _tmp$2271
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1727[
        idx$645
      ];
    _bind$646 = _tmp$2271;
    if (_bind$646 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1718 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1718)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1718->$0 = idx$645;
      _tuple$1718->$1 = psl$644;
      _bind$643 = _tuple$1718;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$648 =
        _bind$646;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$649 =
        _Some$648;
      int32_t hash$1720 = _curr_entry$649->$3;
      int32_t _if_result$2584;
      int32_t psl$1721;
      int32_t _tmp$1723;
      int32_t _tmp$1725;
      int32_t capacity_mask$1726;
      int32_t _tmp$1724;
      if (hash$1720 == hash$650) {
        moonbit_string_t _field$2270 = _curr_entry$649->$4;
        moonbit_string_t key$1719 = _field$2270;
        int32_t _tmp$2269 = moonbit_val_array_equal(key$1719, key$651);
        _if_result$2584 = _tmp$2269;
      } else {
        _if_result$2584 = 0;
      }
      if (_if_result$2584) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2268;
        moonbit_incref(_curr_entry$649);
        moonbit_decref(key$651);
        moonbit_decref(self$642);
        _old$2268 = _curr_entry$649->$5;
        moonbit_decref(_old$2268);
        _curr_entry$649->$5 = value$652;
        moonbit_decref(_curr_entry$649);
        return 0;
      } else {
        moonbit_incref(_curr_entry$649);
      }
      psl$1721 = _curr_entry$649->$2;
      if (psl$644 > psl$1721) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1722;
        moonbit_incref(self$642);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$642, idx$645, _curr_entry$649
        );
        _tuple$1722
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1722)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1722->$0 = idx$645;
        _tuple$1722->$1 = psl$644;
        _bind$643 = _tuple$1722;
        break;
      } else {
        moonbit_decref(_curr_entry$649);
      }
      _tmp$1723 = psl$644 + 1;
      _tmp$1725 = idx$645 + 1;
      capacity_mask$1726 = self$642->$3;
      _tmp$1724 = _tmp$1725 & capacity_mask$1726;
      psl$644 = _tmp$1723;
      idx$645 = _tmp$1724;
      continue;
    }
    break;
  }
  _idx$653 = _bind$643->$0;
  _field$2267 = _bind$643->$1;
  moonbit_decref(_bind$643);
  _psl$654 = _field$2267;
  _bind$655 = self$642->$6;
  _bind$656 = 0;
  entry$657
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$657)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$657->$0 = _bind$655;
  entry$657->$1 = _bind$656;
  entry$657->$2 = _psl$654;
  entry$657->$3 = hash$650;
  entry$657->$4 = key$651;
  entry$657->$5 = value$652;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
    self$642, _idx$653, entry$657
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$626,
  moonbit_string_t key$635,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$636,
  int32_t hash$634
) {
  int32_t size$1702 = self$626->$1;
  int32_t grow_at$1703 = self$626->$4;
  int32_t capacity_mask$1715;
  int32_t _tmp$1714;
  struct $$3c$Int$2a$Int$3e$* _bind$627;
  int32_t psl$628;
  int32_t idx$629;
  int32_t _idx$637;
  int32_t _field$2273;
  int32_t _psl$638;
  int32_t _bind$639;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$640;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$641;
  if (size$1702 >= grow_at$1703) {
    moonbit_incref(self$626);
    $$moonbitlang$core$builtin$Map$$grow$2(self$626);
  }
  capacity_mask$1715 = self$626->$3;
  _tmp$1714 = hash$634 & capacity_mask$1715;
  psl$628 = 0;
  idx$629 = _tmp$1714;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2278 =
      self$626->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1713 =
      _field$2278;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2277;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$630;
    if (idx$629 < 0 || idx$629 >= Moonbit_array_length(entries$1713)) {
      moonbit_panic();
    }
    _tmp$2277
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1713[
        idx$629
      ];
    _bind$630 = _tmp$2277;
    if (_bind$630 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1704 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1704)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1704->$0 = idx$629;
      _tuple$1704->$1 = psl$628;
      _bind$627 = _tuple$1704;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$632 =
        _bind$630;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$633 =
        _Some$632;
      int32_t hash$1706 = _curr_entry$633->$3;
      int32_t _if_result$2586;
      int32_t psl$1707;
      int32_t _tmp$1709;
      int32_t _tmp$1711;
      int32_t capacity_mask$1712;
      int32_t _tmp$1710;
      if (hash$1706 == hash$634) {
        moonbit_string_t _field$2276 = _curr_entry$633->$4;
        moonbit_string_t key$1705 = _field$2276;
        int32_t _tmp$2275 = moonbit_val_array_equal(key$1705, key$635);
        _if_result$2586 = _tmp$2275;
      } else {
        _if_result$2586 = 0;
      }
      if (_if_result$2586) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2274;
        moonbit_incref(_curr_entry$633);
        moonbit_decref(key$635);
        moonbit_decref(self$626);
        _old$2274 = _curr_entry$633->$5;
        moonbit_decref(_old$2274);
        _curr_entry$633->$5 = value$636;
        moonbit_decref(_curr_entry$633);
        return 0;
      } else {
        moonbit_incref(_curr_entry$633);
      }
      psl$1707 = _curr_entry$633->$2;
      if (psl$628 > psl$1707) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1708;
        moonbit_incref(self$626);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$626, idx$629, _curr_entry$633
        );
        _tuple$1708
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1708)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1708->$0 = idx$629;
        _tuple$1708->$1 = psl$628;
        _bind$627 = _tuple$1708;
        break;
      } else {
        moonbit_decref(_curr_entry$633);
      }
      _tmp$1709 = psl$628 + 1;
      _tmp$1711 = idx$629 + 1;
      capacity_mask$1712 = self$626->$3;
      _tmp$1710 = _tmp$1711 & capacity_mask$1712;
      psl$628 = _tmp$1709;
      idx$629 = _tmp$1710;
      continue;
    }
    break;
  }
  _idx$637 = _bind$627->$0;
  _field$2273 = _bind$627->$1;
  moonbit_decref(_bind$627);
  _psl$638 = _field$2273;
  _bind$639 = self$626->$6;
  _bind$640 = 0;
  entry$641
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$641)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$641->$0 = _bind$639;
  entry$641->$1 = _bind$640;
  entry$641->$2 = _psl$638;
  entry$641->$3 = hash$634;
  entry$641->$4 = key$635;
  entry$641->$5 = value$636;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
    self$626, _idx$637, entry$641
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$610,
  int32_t key$619,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$620,
  int32_t hash$618
) {
  int32_t size$1688 = self$610->$1;
  int32_t grow_at$1689 = self$610->$4;
  int32_t capacity_mask$1701;
  int32_t _tmp$1700;
  struct $$3c$Int$2a$Int$3e$* _bind$611;
  int32_t psl$612;
  int32_t idx$613;
  int32_t _idx$621;
  int32_t _field$2279;
  int32_t _psl$622;
  int32_t _bind$623;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$624;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$625;
  if (size$1688 >= grow_at$1689) {
    moonbit_incref(self$610);
    $$moonbitlang$core$builtin$Map$$grow$1(self$610);
  }
  capacity_mask$1701 = self$610->$3;
  _tmp$1700 = hash$618 & capacity_mask$1701;
  psl$612 = 0;
  idx$613 = _tmp$1700;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2282 =
      self$610->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1699 =
      _field$2282;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2281;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$614;
    if (idx$613 < 0 || idx$613 >= Moonbit_array_length(entries$1699)) {
      moonbit_panic();
    }
    _tmp$2281
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1699[
        idx$613
      ];
    _bind$614 = _tmp$2281;
    if (_bind$614 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1690 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1690)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1690->$0 = idx$613;
      _tuple$1690->$1 = psl$612;
      _bind$611 = _tuple$1690;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$616 =
        _bind$614;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$617 =
        _Some$616;
      int32_t hash$1692 = _curr_entry$617->$3;
      int32_t _if_result$2588;
      int32_t psl$1693;
      int32_t _tmp$1695;
      int32_t _tmp$1697;
      int32_t capacity_mask$1698;
      int32_t _tmp$1696;
      if (hash$1692 == hash$618) {
        int32_t key$1691 = _curr_entry$617->$4;
        _if_result$2588 = key$1691 == key$619;
      } else {
        _if_result$2588 = 0;
      }
      if (_if_result$2588) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$2280;
        moonbit_incref(_curr_entry$617);
        moonbit_decref(self$610);
        _old$2280 = _curr_entry$617->$5;
        moonbit_decref(_old$2280);
        _curr_entry$617->$5 = value$620;
        moonbit_decref(_curr_entry$617);
        return 0;
      } else {
        moonbit_incref(_curr_entry$617);
      }
      psl$1693 = _curr_entry$617->$2;
      if (psl$612 > psl$1693) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1694;
        moonbit_incref(self$610);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$610, idx$613, _curr_entry$617
        );
        _tuple$1694
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1694)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1694->$0 = idx$613;
        _tuple$1694->$1 = psl$612;
        _bind$611 = _tuple$1694;
        break;
      } else {
        moonbit_decref(_curr_entry$617);
      }
      _tmp$1695 = psl$612 + 1;
      _tmp$1697 = idx$613 + 1;
      capacity_mask$1698 = self$610->$3;
      _tmp$1696 = _tmp$1697 & capacity_mask$1698;
      psl$612 = _tmp$1695;
      idx$613 = _tmp$1696;
      continue;
    }
    break;
  }
  _idx$621 = _bind$611->$0;
  _field$2279 = _bind$611->$1;
  moonbit_decref(_bind$611);
  _psl$622 = _field$2279;
  _bind$623 = self$610->$6;
  _bind$624 = 0;
  entry$625
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$625)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      2,
      0
  );
  entry$625->$0 = _bind$623;
  entry$625->$1 = _bind$624;
  entry$625->$2 = _psl$622;
  entry$625->$3 = hash$618;
  entry$625->$4 = key$619;
  entry$625->$5 = value$620;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
    self$610, _idx$621, entry$625
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$594,
  moonbit_string_t key$603,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$604,
  int32_t hash$602
) {
  int32_t size$1674 = self$594->$1;
  int32_t grow_at$1675 = self$594->$4;
  int32_t capacity_mask$1687;
  int32_t _tmp$1686;
  struct $$3c$Int$2a$Int$3e$* _bind$595;
  int32_t psl$596;
  int32_t idx$597;
  int32_t _idx$605;
  int32_t _field$2283;
  int32_t _psl$606;
  int32_t _bind$607;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$608;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$609;
  if (size$1674 >= grow_at$1675) {
    moonbit_incref(self$594);
    $$moonbitlang$core$builtin$Map$$grow$0(self$594);
  }
  capacity_mask$1687 = self$594->$3;
  _tmp$1686 = hash$602 & capacity_mask$1687;
  psl$596 = 0;
  idx$597 = _tmp$1686;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2288 =
      self$594->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1685 =
      _field$2288;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2287;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$598;
    if (idx$597 < 0 || idx$597 >= Moonbit_array_length(entries$1685)) {
      moonbit_panic();
    }
    _tmp$2287
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1685[
        idx$597
      ];
    _bind$598 = _tmp$2287;
    if (_bind$598 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1676 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1676)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1676->$0 = idx$597;
      _tuple$1676->$1 = psl$596;
      _bind$595 = _tuple$1676;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$600 =
        _bind$598;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$601 =
        _Some$600;
      int32_t hash$1678 = _curr_entry$601->$3;
      int32_t _if_result$2590;
      int32_t psl$1679;
      int32_t _tmp$1681;
      int32_t _tmp$1683;
      int32_t capacity_mask$1684;
      int32_t _tmp$1682;
      if (hash$1678 == hash$602) {
        moonbit_string_t _field$2286 = _curr_entry$601->$4;
        moonbit_string_t key$1677 = _field$2286;
        int32_t _tmp$2285 = moonbit_val_array_equal(key$1677, key$603);
        _if_result$2590 = _tmp$2285;
      } else {
        _if_result$2590 = 0;
      }
      if (_if_result$2590) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2284;
        moonbit_incref(_curr_entry$601);
        moonbit_decref(key$603);
        moonbit_decref(self$594);
        _old$2284 = _curr_entry$601->$5;
        moonbit_decref(_old$2284);
        _curr_entry$601->$5 = value$604;
        moonbit_decref(_curr_entry$601);
        return 0;
      } else {
        moonbit_incref(_curr_entry$601);
      }
      psl$1679 = _curr_entry$601->$2;
      if (psl$596 > psl$1679) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1680;
        moonbit_incref(self$594);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$594, idx$597, _curr_entry$601
        );
        _tuple$1680
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1680)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1680->$0 = idx$597;
        _tuple$1680->$1 = psl$596;
        _bind$595 = _tuple$1680;
        break;
      } else {
        moonbit_decref(_curr_entry$601);
      }
      _tmp$1681 = psl$596 + 1;
      _tmp$1683 = idx$597 + 1;
      capacity_mask$1684 = self$594->$3;
      _tmp$1682 = _tmp$1683 & capacity_mask$1684;
      psl$596 = _tmp$1681;
      idx$597 = _tmp$1682;
      continue;
    }
    break;
  }
  _idx$605 = _bind$595->$0;
  _field$2283 = _bind$595->$1;
  moonbit_decref(_bind$595);
  _psl$606 = _field$2283;
  _bind$607 = self$594->$6;
  _bind$608 = 0;
  entry$609
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$609)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$609->$0 = _bind$607;
  entry$609->$1 = _bind$608;
  entry$609->$2 = _psl$606;
  entry$609->$3 = hash$602;
  entry$609->$4 = key$603;
  entry$609->$5 = value$604;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
    self$594, _idx$605, entry$609
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$588,
  int32_t idx$593,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$592
) {
  int32_t psl$1673 = entry$592->$2;
  int32_t _tmp$1669 = psl$1673 + 1;
  int32_t _tmp$1671 = idx$593 + 1;
  int32_t capacity_mask$1672 = self$588->$3;
  int32_t _tmp$1670 = _tmp$1671 & capacity_mask$1672;
  int32_t psl$584 = _tmp$1669;
  int32_t idx$585 = _tmp$1670;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$586 =
    entry$592;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2290 =
      self$588->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1668 =
      _field$2290;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2289;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$587;
    if (idx$585 < 0 || idx$585 >= Moonbit_array_length(entries$1668)) {
      moonbit_panic();
    }
    _tmp$2289
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1668[
        idx$585
      ];
    _bind$587 = _tmp$2289;
    if (_bind$587 == 0) {
      entry$586->$2 = psl$584;
      $$moonbitlang$core$builtin$Map$$set_entry$3(
        self$588, entry$586, idx$585
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$590 =
        _bind$587;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$591 =
        _Some$590;
      int32_t psl$1658 = _curr_entry$591->$2;
      if (psl$584 > psl$1658) {
        int32_t psl$1663;
        int32_t _tmp$1659;
        int32_t _tmp$1661;
        int32_t capacity_mask$1662;
        int32_t _tmp$1660;
        entry$586->$2 = psl$584;
        moonbit_incref(_curr_entry$591);
        moonbit_incref(self$588);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$588, entry$586, idx$585
        );
        psl$1663 = _curr_entry$591->$2;
        _tmp$1659 = psl$1663 + 1;
        _tmp$1661 = idx$585 + 1;
        capacity_mask$1662 = self$588->$3;
        _tmp$1660 = _tmp$1661 & capacity_mask$1662;
        psl$584 = _tmp$1659;
        idx$585 = _tmp$1660;
        entry$586 = _curr_entry$591;
        continue;
      } else {
        int32_t _tmp$1664 = psl$584 + 1;
        int32_t _tmp$1666 = idx$585 + 1;
        int32_t capacity_mask$1667 = self$588->$3;
        int32_t _tmp$1665 = _tmp$1666 & capacity_mask$1667;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2592 =
          entry$586;
        psl$584 = _tmp$1664;
        idx$585 = _tmp$1665;
        entry$586 = _tmp$2592;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$578,
  int32_t idx$583,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$582
) {
  int32_t psl$1657 = entry$582->$2;
  int32_t _tmp$1653 = psl$1657 + 1;
  int32_t _tmp$1655 = idx$583 + 1;
  int32_t capacity_mask$1656 = self$578->$3;
  int32_t _tmp$1654 = _tmp$1655 & capacity_mask$1656;
  int32_t psl$574 = _tmp$1653;
  int32_t idx$575 = _tmp$1654;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$576 =
    entry$582;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2292 =
      self$578->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1652 =
      _field$2292;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2291;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$577;
    if (idx$575 < 0 || idx$575 >= Moonbit_array_length(entries$1652)) {
      moonbit_panic();
    }
    _tmp$2291
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1652[
        idx$575
      ];
    _bind$577 = _tmp$2291;
    if (_bind$577 == 0) {
      entry$576->$2 = psl$574;
      $$moonbitlang$core$builtin$Map$$set_entry$2(
        self$578, entry$576, idx$575
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$580 =
        _bind$577;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$581 =
        _Some$580;
      int32_t psl$1642 = _curr_entry$581->$2;
      if (psl$574 > psl$1642) {
        int32_t psl$1647;
        int32_t _tmp$1643;
        int32_t _tmp$1645;
        int32_t capacity_mask$1646;
        int32_t _tmp$1644;
        entry$576->$2 = psl$574;
        moonbit_incref(_curr_entry$581);
        moonbit_incref(self$578);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$578, entry$576, idx$575
        );
        psl$1647 = _curr_entry$581->$2;
        _tmp$1643 = psl$1647 + 1;
        _tmp$1645 = idx$575 + 1;
        capacity_mask$1646 = self$578->$3;
        _tmp$1644 = _tmp$1645 & capacity_mask$1646;
        psl$574 = _tmp$1643;
        idx$575 = _tmp$1644;
        entry$576 = _curr_entry$581;
        continue;
      } else {
        int32_t _tmp$1648 = psl$574 + 1;
        int32_t _tmp$1650 = idx$575 + 1;
        int32_t capacity_mask$1651 = self$578->$3;
        int32_t _tmp$1649 = _tmp$1650 & capacity_mask$1651;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2594 =
          entry$576;
        psl$574 = _tmp$1648;
        idx$575 = _tmp$1649;
        entry$576 = _tmp$2594;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$568,
  int32_t idx$573,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$572
) {
  int32_t psl$1641 = entry$572->$2;
  int32_t _tmp$1637 = psl$1641 + 1;
  int32_t _tmp$1639 = idx$573 + 1;
  int32_t capacity_mask$1640 = self$568->$3;
  int32_t _tmp$1638 = _tmp$1639 & capacity_mask$1640;
  int32_t psl$564 = _tmp$1637;
  int32_t idx$565 = _tmp$1638;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$566 =
    entry$572;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2294 =
      self$568->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1636 =
      _field$2294;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2293;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$567;
    if (idx$565 < 0 || idx$565 >= Moonbit_array_length(entries$1636)) {
      moonbit_panic();
    }
    _tmp$2293
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1636[
        idx$565
      ];
    _bind$567 = _tmp$2293;
    if (_bind$567 == 0) {
      entry$566->$2 = psl$564;
      $$moonbitlang$core$builtin$Map$$set_entry$1(
        self$568, entry$566, idx$565
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$570 =
        _bind$567;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$571 =
        _Some$570;
      int32_t psl$1626 = _curr_entry$571->$2;
      if (psl$564 > psl$1626) {
        int32_t psl$1631;
        int32_t _tmp$1627;
        int32_t _tmp$1629;
        int32_t capacity_mask$1630;
        int32_t _tmp$1628;
        entry$566->$2 = psl$564;
        moonbit_incref(_curr_entry$571);
        moonbit_incref(self$568);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$568, entry$566, idx$565
        );
        psl$1631 = _curr_entry$571->$2;
        _tmp$1627 = psl$1631 + 1;
        _tmp$1629 = idx$565 + 1;
        capacity_mask$1630 = self$568->$3;
        _tmp$1628 = _tmp$1629 & capacity_mask$1630;
        psl$564 = _tmp$1627;
        idx$565 = _tmp$1628;
        entry$566 = _curr_entry$571;
        continue;
      } else {
        int32_t _tmp$1632 = psl$564 + 1;
        int32_t _tmp$1634 = idx$565 + 1;
        int32_t capacity_mask$1635 = self$568->$3;
        int32_t _tmp$1633 = _tmp$1634 & capacity_mask$1635;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2596 =
          entry$566;
        psl$564 = _tmp$1632;
        idx$565 = _tmp$1633;
        entry$566 = _tmp$2596;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$558,
  int32_t idx$563,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$562
) {
  int32_t psl$1625 = entry$562->$2;
  int32_t _tmp$1621 = psl$1625 + 1;
  int32_t _tmp$1623 = idx$563 + 1;
  int32_t capacity_mask$1624 = self$558->$3;
  int32_t _tmp$1622 = _tmp$1623 & capacity_mask$1624;
  int32_t psl$554 = _tmp$1621;
  int32_t idx$555 = _tmp$1622;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$556 =
    entry$562;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2296 =
      self$558->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1620 =
      _field$2296;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2295;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$557;
    if (idx$555 < 0 || idx$555 >= Moonbit_array_length(entries$1620)) {
      moonbit_panic();
    }
    _tmp$2295
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1620[
        idx$555
      ];
    _bind$557 = _tmp$2295;
    if (_bind$557 == 0) {
      entry$556->$2 = psl$554;
      $$moonbitlang$core$builtin$Map$$set_entry$0(
        self$558, entry$556, idx$555
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$560 =
        _bind$557;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$561 =
        _Some$560;
      int32_t psl$1610 = _curr_entry$561->$2;
      if (psl$554 > psl$1610) {
        int32_t psl$1615;
        int32_t _tmp$1611;
        int32_t _tmp$1613;
        int32_t capacity_mask$1614;
        int32_t _tmp$1612;
        entry$556->$2 = psl$554;
        moonbit_incref(_curr_entry$561);
        moonbit_incref(self$558);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$558, entry$556, idx$555
        );
        psl$1615 = _curr_entry$561->$2;
        _tmp$1611 = psl$1615 + 1;
        _tmp$1613 = idx$555 + 1;
        capacity_mask$1614 = self$558->$3;
        _tmp$1612 = _tmp$1613 & capacity_mask$1614;
        psl$554 = _tmp$1611;
        idx$555 = _tmp$1612;
        entry$556 = _curr_entry$561;
        continue;
      } else {
        int32_t _tmp$1616 = psl$554 + 1;
        int32_t _tmp$1618 = idx$555 + 1;
        int32_t capacity_mask$1619 = self$558->$3;
        int32_t _tmp$1617 = _tmp$1618 & capacity_mask$1619;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2598 =
          entry$556;
        psl$554 = _tmp$1616;
        idx$555 = _tmp$1617;
        entry$556 = _tmp$2598;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$548,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$550,
  int32_t new_idx$549
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2299 =
    self$548->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1608 =
    _field$2299;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1609;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2298;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2297;
  int32_t _cnt$2493;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$551;
  moonbit_incref(entry$550);
  _tmp$1609 = entry$550;
  if (new_idx$549 < 0 || new_idx$549 >= Moonbit_array_length(entries$1608)) {
    moonbit_panic();
  }
  _old$2298
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1608[
      new_idx$549
    ];
  if (_old$2298) {
    moonbit_decref(_old$2298);
  }
  entries$1608[new_idx$549] = _tmp$1609;
  _field$2297 = entry$550->$1;
  _cnt$2493 = Moonbit_object_header(entry$550)->rc;
  if (_cnt$2493 > 1) {
    int32_t _new_cnt$2496;
    if (_field$2297) {
      moonbit_incref(_field$2297);
    }
    _new_cnt$2496 = _cnt$2493 - 1;
    Moonbit_object_header(entry$550)->rc = _new_cnt$2496;
  } else if (_cnt$2493 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2495 =
      entry$550->$5;
    moonbit_string_t _field$2494;
    moonbit_decref(_field$2495);
    _field$2494 = entry$550->$4;
    moonbit_decref(_field$2494);
    moonbit_free(entry$550);
  }
  _bind$551 = _field$2297;
  if (_bind$551 == 0) {
    if (_bind$551) {
      moonbit_decref(_bind$551);
    }
    self$548->$6 = new_idx$549;
    moonbit_decref(self$548);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$552;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$553;
    moonbit_decref(self$548);
    _Some$552 = _bind$551;
    _next$553 = _Some$552;
    _next$553->$0 = new_idx$549;
    moonbit_decref(_next$553);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$542,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$544,
  int32_t new_idx$543
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2302 =
    self$542->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1606 =
    _field$2302;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1607;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2301;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2300;
  int32_t _cnt$2497;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$545;
  moonbit_incref(entry$544);
  _tmp$1607 = entry$544;
  if (new_idx$543 < 0 || new_idx$543 >= Moonbit_array_length(entries$1606)) {
    moonbit_panic();
  }
  _old$2301
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1606[
      new_idx$543
    ];
  if (_old$2301) {
    moonbit_decref(_old$2301);
  }
  entries$1606[new_idx$543] = _tmp$1607;
  _field$2300 = entry$544->$1;
  _cnt$2497 = Moonbit_object_header(entry$544)->rc;
  if (_cnt$2497 > 1) {
    int32_t _new_cnt$2500;
    if (_field$2300) {
      moonbit_incref(_field$2300);
    }
    _new_cnt$2500 = _cnt$2497 - 1;
    Moonbit_object_header(entry$544)->rc = _new_cnt$2500;
  } else if (_cnt$2497 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2499 =
      entry$544->$5;
    moonbit_string_t _field$2498;
    moonbit_decref(_field$2499);
    _field$2498 = entry$544->$4;
    moonbit_decref(_field$2498);
    moonbit_free(entry$544);
  }
  _bind$545 = _field$2300;
  if (_bind$545 == 0) {
    if (_bind$545) {
      moonbit_decref(_bind$545);
    }
    self$542->$6 = new_idx$543;
    moonbit_decref(self$542);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$546;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$547;
    moonbit_decref(self$542);
    _Some$546 = _bind$545;
    _next$547 = _Some$546;
    _next$547->$0 = new_idx$543;
    moonbit_decref(_next$547);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$536,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$538,
  int32_t new_idx$537
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2305 =
    self$536->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1604 =
    _field$2305;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1605;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2304;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2303;
  int32_t _cnt$2501;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$539;
  moonbit_incref(entry$538);
  _tmp$1605 = entry$538;
  if (new_idx$537 < 0 || new_idx$537 >= Moonbit_array_length(entries$1604)) {
    moonbit_panic();
  }
  _old$2304
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1604[
      new_idx$537
    ];
  if (_old$2304) {
    moonbit_decref(_old$2304);
  }
  entries$1604[new_idx$537] = _tmp$1605;
  _field$2303 = entry$538->$1;
  _cnt$2501 = Moonbit_object_header(entry$538)->rc;
  if (_cnt$2501 > 1) {
    int32_t _new_cnt$2503;
    if (_field$2303) {
      moonbit_incref(_field$2303);
    }
    _new_cnt$2503 = _cnt$2501 - 1;
    Moonbit_object_header(entry$538)->rc = _new_cnt$2503;
  } else if (_cnt$2501 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2502 =
      entry$538->$5;
    moonbit_decref(_field$2502);
    moonbit_free(entry$538);
  }
  _bind$539 = _field$2303;
  if (_bind$539 == 0) {
    if (_bind$539) {
      moonbit_decref(_bind$539);
    }
    self$536->$6 = new_idx$537;
    moonbit_decref(self$536);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$540;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$541;
    moonbit_decref(self$536);
    _Some$540 = _bind$539;
    _next$541 = _Some$540;
    _next$541->$0 = new_idx$537;
    moonbit_decref(_next$541);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$530,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$532,
  int32_t new_idx$531
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2308 =
    self$530->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1602 =
    _field$2308;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1603;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2307;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2306;
  int32_t _cnt$2504;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$533;
  moonbit_incref(entry$532);
  _tmp$1603 = entry$532;
  if (new_idx$531 < 0 || new_idx$531 >= Moonbit_array_length(entries$1602)) {
    moonbit_panic();
  }
  _old$2307
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1602[
      new_idx$531
    ];
  if (_old$2307) {
    moonbit_decref(_old$2307);
  }
  entries$1602[new_idx$531] = _tmp$1603;
  _field$2306 = entry$532->$1;
  _cnt$2504 = Moonbit_object_header(entry$532)->rc;
  if (_cnt$2504 > 1) {
    int32_t _new_cnt$2507;
    if (_field$2306) {
      moonbit_incref(_field$2306);
    }
    _new_cnt$2507 = _cnt$2504 - 1;
    Moonbit_object_header(entry$532)->rc = _new_cnt$2507;
  } else if (_cnt$2504 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2506 =
      entry$532->$5;
    moonbit_string_t _field$2505;
    moonbit_decref(_field$2506);
    _field$2505 = entry$532->$4;
    moonbit_decref(_field$2505);
    moonbit_free(entry$532);
  }
  _bind$533 = _field$2306;
  if (_bind$533 == 0) {
    if (_bind$533) {
      moonbit_decref(_bind$533);
    }
    self$530->$6 = new_idx$531;
    moonbit_decref(self$530);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$534;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$535;
    moonbit_decref(self$530);
    _Some$534 = _bind$533;
    _next$535 = _Some$534;
    _next$535->$0 = new_idx$531;
    moonbit_decref(_next$535);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$527,
  int32_t idx$529,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$528
) {
  int32_t _bind$526 = self$527->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2310;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1598;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1599;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2309;
  int32_t size$1601;
  int32_t _tmp$1600;
  switch (_bind$526) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1593;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2311;
      moonbit_incref(entry$528);
      _tmp$1593 = entry$528;
      _old$2311 = self$527->$5;
      if (_old$2311) {
        moonbit_decref(_old$2311);
      }
      self$527->$5 = _tmp$1593;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2314 =
        self$527->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1597 =
        _field$2314;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2313;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1596;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1594;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1595;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2312;
      if (_bind$526 < 0 || _bind$526 >= Moonbit_array_length(entries$1597)) {
        moonbit_panic();
      }
      _tmp$2313
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1597[
          _bind$526
        ];
      _tmp$1596 = _tmp$2313;
      if (_tmp$1596) {
        moonbit_incref(_tmp$1596);
      }
      _tmp$1594 = $Option$$unwrap$3(_tmp$1596);
      moonbit_incref(entry$528);
      _tmp$1595 = entry$528;
      _old$2312 = _tmp$1594->$1;
      if (_old$2312) {
        moonbit_decref(_old$2312);
      }
      _tmp$1594->$1 = _tmp$1595;
      moonbit_decref(_tmp$1594);
      break;
    }
  }
  self$527->$6 = idx$529;
  _field$2310 = self$527->$0;
  entries$1598 = _field$2310;
  _tmp$1599 = entry$528;
  if (idx$529 < 0 || idx$529 >= Moonbit_array_length(entries$1598)) {
    moonbit_panic();
  }
  _old$2309
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1598[
      idx$529
    ];
  if (_old$2309) {
    moonbit_decref(_old$2309);
  }
  entries$1598[idx$529] = _tmp$1599;
  size$1601 = self$527->$1;
  _tmp$1600 = size$1601 + 1;
  self$527->$1 = _tmp$1600;
  moonbit_decref(self$527);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$523,
  int32_t idx$525,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$524
) {
  int32_t _bind$522 = self$523->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2316;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1589;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1590;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2315;
  int32_t size$1592;
  int32_t _tmp$1591;
  switch (_bind$522) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1584;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2317;
      moonbit_incref(entry$524);
      _tmp$1584 = entry$524;
      _old$2317 = self$523->$5;
      if (_old$2317) {
        moonbit_decref(_old$2317);
      }
      self$523->$5 = _tmp$1584;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2320 =
        self$523->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1588 =
        _field$2320;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2319;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1587;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1585;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1586;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2318;
      if (_bind$522 < 0 || _bind$522 >= Moonbit_array_length(entries$1588)) {
        moonbit_panic();
      }
      _tmp$2319
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1588[
          _bind$522
        ];
      _tmp$1587 = _tmp$2319;
      if (_tmp$1587) {
        moonbit_incref(_tmp$1587);
      }
      _tmp$1585 = $Option$$unwrap$2(_tmp$1587);
      moonbit_incref(entry$524);
      _tmp$1586 = entry$524;
      _old$2318 = _tmp$1585->$1;
      if (_old$2318) {
        moonbit_decref(_old$2318);
      }
      _tmp$1585->$1 = _tmp$1586;
      moonbit_decref(_tmp$1585);
      break;
    }
  }
  self$523->$6 = idx$525;
  _field$2316 = self$523->$0;
  entries$1589 = _field$2316;
  _tmp$1590 = entry$524;
  if (idx$525 < 0 || idx$525 >= Moonbit_array_length(entries$1589)) {
    moonbit_panic();
  }
  _old$2315
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1589[
      idx$525
    ];
  if (_old$2315) {
    moonbit_decref(_old$2315);
  }
  entries$1589[idx$525] = _tmp$1590;
  size$1592 = self$523->$1;
  _tmp$1591 = size$1592 + 1;
  self$523->$1 = _tmp$1591;
  moonbit_decref(self$523);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$519,
  int32_t idx$521,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$520
) {
  int32_t _bind$518 = self$519->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2322;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1580;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1581;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2321;
  int32_t size$1583;
  int32_t _tmp$1582;
  switch (_bind$518) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1575;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2323;
      moonbit_incref(entry$520);
      _tmp$1575 = entry$520;
      _old$2323 = self$519->$5;
      if (_old$2323) {
        moonbit_decref(_old$2323);
      }
      self$519->$5 = _tmp$1575;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2326 =
        self$519->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1579 =
        _field$2326;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2325;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1578;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1576;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1577;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2324;
      if (_bind$518 < 0 || _bind$518 >= Moonbit_array_length(entries$1579)) {
        moonbit_panic();
      }
      _tmp$2325
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1579[
          _bind$518
        ];
      _tmp$1578 = _tmp$2325;
      if (_tmp$1578) {
        moonbit_incref(_tmp$1578);
      }
      _tmp$1576 = $Option$$unwrap$1(_tmp$1578);
      moonbit_incref(entry$520);
      _tmp$1577 = entry$520;
      _old$2324 = _tmp$1576->$1;
      if (_old$2324) {
        moonbit_decref(_old$2324);
      }
      _tmp$1576->$1 = _tmp$1577;
      moonbit_decref(_tmp$1576);
      break;
    }
  }
  self$519->$6 = idx$521;
  _field$2322 = self$519->$0;
  entries$1580 = _field$2322;
  _tmp$1581 = entry$520;
  if (idx$521 < 0 || idx$521 >= Moonbit_array_length(entries$1580)) {
    moonbit_panic();
  }
  _old$2321
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1580[
      idx$521
    ];
  if (_old$2321) {
    moonbit_decref(_old$2321);
  }
  entries$1580[idx$521] = _tmp$1581;
  size$1583 = self$519->$1;
  _tmp$1582 = size$1583 + 1;
  self$519->$1 = _tmp$1582;
  moonbit_decref(self$519);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$515,
  int32_t idx$517,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$516
) {
  int32_t _bind$514 = self$515->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2328;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1571;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1572;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2327;
  int32_t size$1574;
  int32_t _tmp$1573;
  switch (_bind$514) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1566;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2329;
      moonbit_incref(entry$516);
      _tmp$1566 = entry$516;
      _old$2329 = self$515->$5;
      if (_old$2329) {
        moonbit_decref(_old$2329);
      }
      self$515->$5 = _tmp$1566;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2332 =
        self$515->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1570 =
        _field$2332;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2331;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1569;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1567;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1568;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2330;
      if (_bind$514 < 0 || _bind$514 >= Moonbit_array_length(entries$1570)) {
        moonbit_panic();
      }
      _tmp$2331
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1570[
          _bind$514
        ];
      _tmp$1569 = _tmp$2331;
      if (_tmp$1569) {
        moonbit_incref(_tmp$1569);
      }
      _tmp$1567 = $Option$$unwrap$0(_tmp$1569);
      moonbit_incref(entry$516);
      _tmp$1568 = entry$516;
      _old$2330 = _tmp$1567->$1;
      if (_old$2330) {
        moonbit_decref(_old$2330);
      }
      _tmp$1567->$1 = _tmp$1568;
      moonbit_decref(_tmp$1567);
      break;
    }
  }
  self$515->$6 = idx$517;
  _field$2328 = self$515->$0;
  entries$1571 = _field$2328;
  _tmp$1572 = entry$516;
  if (idx$517 < 0 || idx$517 >= Moonbit_array_length(entries$1571)) {
    moonbit_panic();
  }
  _old$2327
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1571[
      idx$517
    ];
  if (_old$2327) {
    moonbit_decref(_old$2327);
  }
  entries$1571[idx$517] = _tmp$1572;
  size$1574 = self$515->$1;
  _tmp$1573 = size$1574 + 1;
  self$515->$1 = _tmp$1573;
  moonbit_decref(self$515);
  return 0;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$509
) {
  int32_t capacity$508 = $Int$$next_power_of_two(capacity$509);
  int32_t _bind$510 = capacity$508 - 1;
  int32_t _bind$511 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$508);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1565 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$512 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$508, _tmp$1565
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$513 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2599 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2599)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2599->$0 = _bind$512;
  _block$2599->$1 = 0;
  _block$2599->$2 = capacity$508;
  _block$2599->$3 = _bind$510;
  _block$2599->$4 = _bind$511;
  _block$2599->$5 = _bind$513;
  _block$2599->$6 = -1;
  return _block$2599;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$503
) {
  int32_t capacity$502 = $Int$$next_power_of_two(capacity$503);
  int32_t _bind$504 = capacity$502 - 1;
  int32_t _bind$505 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$502);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1564 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$506 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$502, _tmp$1564
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$507 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2600 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2600)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2600->$0 = _bind$506;
  _block$2600->$1 = 0;
  _block$2600->$2 = capacity$502;
  _block$2600->$3 = _bind$504;
  _block$2600->$4 = _bind$505;
  _block$2600->$5 = _bind$507;
  _block$2600->$6 = -1;
  return _block$2600;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$497
) {
  int32_t capacity$496 = $Int$$next_power_of_two(capacity$497);
  int32_t _bind$498 = capacity$496 - 1;
  int32_t _bind$499 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$496);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1563 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$500 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$496, _tmp$1563
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$501 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$2601 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2601)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2601->$0 = _bind$500;
  _block$2601->$1 = 0;
  _block$2601->$2 = capacity$496;
  _block$2601->$3 = _bind$498;
  _block$2601->$4 = _bind$499;
  _block$2601->$5 = _bind$501;
  _block$2601->$6 = -1;
  return _block$2601;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$491
) {
  int32_t capacity$490 = $Int$$next_power_of_two(capacity$491);
  int32_t _bind$492 = capacity$490 - 1;
  int32_t _bind$493 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$490);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1562 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$494 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$490, _tmp$1562
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$495 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2602 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2602)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2602->$0 = _bind$494;
  _block$2602->$1 = 0;
  _block$2602->$2 = capacity$490;
  _block$2602->$3 = _bind$492;
  _block$2602->$4 = _bind$493;
  _block$2602->$5 = _bind$495;
  _block$2602->$6 = -1;
  return _block$2602;
}

int32_t $Int$$next_power_of_two(int32_t self$489) {
  if (self$489 >= 0) {
    int32_t _tmp$1561;
    int32_t _tmp$1560;
    int32_t _tmp$1559;
    int32_t _tmp$1558;
    if (self$489 <= 1) {
      return 1;
    }
    if (self$489 > 1073741824) {
      return 1073741824;
    }
    _tmp$1561 = self$489 - 1;
    _tmp$1560 = moonbit_clz32(_tmp$1561);
    _tmp$1559 = _tmp$1560 - 1;
    _tmp$1558 = 2147483647 >> (_tmp$1559 & 31);
    return _tmp$1558 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$488) {
  int32_t _tmp$1557 = capacity$488 * 13;
  return _tmp$1557 / 16;
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$486
) {
  if (self$486 == 0) {
    if (self$486) {
      moonbit_decref(self$486);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$487 =
      self$486;
    return _Some$487;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$484
) {
  if (self$484 == 0) {
    if (self$484) {
      moonbit_decref(self$484);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$485 =
      self$484;
    return _Some$485;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$482
) {
  if (self$482 == 0) {
    if (self$482) {
      moonbit_decref(self$482);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$483 =
      self$482;
    return _Some$483;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$480
) {
  if (self$480 == 0) {
    if (self$480) {
      moonbit_decref(self$480);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$481 =
      self$480;
    return _Some$481;
  }
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$479
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1556 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$479);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$1556);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$478
) {
  moonbit_string_t* _field$2334 = self$478->$0;
  moonbit_string_t* buf$1554 = _field$2334;
  int32_t _field$2333 = self$478->$1;
  int32_t _cnt$2508 = Moonbit_object_header(self$478)->rc;
  int32_t len$1555;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$1553;
  if (_cnt$2508 > 1) {
    int32_t _new_cnt$2509;
    moonbit_incref(buf$1554);
    _new_cnt$2509 = _cnt$2508 - 1;
    Moonbit_object_header(self$478)->rc = _new_cnt$2509;
  } else if (_cnt$2508 == 1) {
    moonbit_free(self$478);
  }
  len$1555 = _field$2333;
  _tmp$1553
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$1555, buf$1554
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$1553);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$476
) {
  struct $Ref$3c$Int$3e$* i$475 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$2603;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1542;
  Moonbit_object_header(i$475)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$475->$0 = 0;
  _closure$2603
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$2603)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$2603->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$2603->$0_0 = self$476.$0;
  _closure$2603->$0_1 = self$476.$1;
  _closure$2603->$0_2 = self$476.$2;
  _closure$2603->$1 = i$475;
  _tmp$1542 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$2603;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$1542);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1543
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$1544 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$1543;
  struct $Ref$3c$Int$3e$* _field$2339 = _casted_env$1544->$1;
  struct $Ref$3c$Int$3e$* i$475 = _field$2339;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$2338 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$1544->$0_1, _casted_env$1544->$0_2, _casted_env$1544->$0_0
    };
  int32_t _cnt$2510 = Moonbit_object_header(_casted_env$1544)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$476;
  int32_t val$1545;
  int32_t _tmp$1546;
  if (_cnt$2510 > 1) {
    int32_t _new_cnt$2511;
    moonbit_incref(i$475);
    moonbit_incref(_field$2338.$0);
    _new_cnt$2511 = _cnt$2510 - 1;
    Moonbit_object_header(_casted_env$1544)->rc = _new_cnt$2511;
  } else if (_cnt$2510 == 1) {
    moonbit_free(_casted_env$1544);
  }
  self$476 = _field$2338;
  val$1545 = i$475->$0;
  moonbit_incref(self$476.$0);
  _tmp$1546 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$476);
  if (val$1545 < _tmp$1546) {
    moonbit_string_t* _field$2337 = self$476.$0;
    moonbit_string_t* buf$1549 = _field$2337;
    int32_t _field$2336 = self$476.$1;
    int32_t start$1551 = _field$2336;
    int32_t val$1552 = i$475->$0;
    int32_t _tmp$1550 = start$1551 + val$1552;
    moonbit_string_t _tmp$2335 = (moonbit_string_t)buf$1549[_tmp$1550];
    moonbit_string_t elem$477;
    int32_t val$1548;
    int32_t _tmp$1547;
    moonbit_incref(_tmp$2335);
    moonbit_decref(buf$1549);
    elem$477 = _tmp$2335;
    val$1548 = i$475->$0;
    _tmp$1547 = val$1548 + 1;
    i$475->$0 = _tmp$1547;
    moonbit_decref(i$475);
    return elem$477;
  } else {
    moonbit_decref(self$476.$0);
    moonbit_decref(i$475);
    return 0;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$474
) {
  return self$474;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$473,
  struct $$moonbitlang$core$builtin$Logger logger$472
) {
  moonbit_string_t _tmp$1541 = $Int$$to_string$inner(self$473, 10);
  logger$472.$0->$method_0(logger$472.$1, _tmp$1541);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$470,
  struct $$3c$String$3e$$3d$$3e$Int* f$471
) {
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _func$469 = self$470;
  return _func$469->code(_func$469, f$471);
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$466,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$468
) {
  int32_t len$1536 = self$466->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1538;
  int32_t _tmp$2342;
  int32_t _tmp$1537;
  int32_t length$467;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2341;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$1539;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2340;
  int32_t _tmp$1540;
  moonbit_incref(self$466);
  _tmp$1538 = $$moonbitlang$core$builtin$Array$$buffer$2(self$466);
  _tmp$2342 = Moonbit_array_length(_tmp$1538);
  moonbit_decref(_tmp$1538);
  _tmp$1537 = _tmp$2342;
  if (len$1536 == _tmp$1537) {
    moonbit_incref(self$466);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$466);
  }
  length$467 = self$466->$1;
  _field$2341 = self$466->$0;
  buf$1539 = _field$2341;
  _old$2340
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$1539[
      length$467
    ];
  if (_old$2340) {
    moonbit_decref(_old$2340);
  }
  buf$1539[length$467] = value$468;
  _tmp$1540 = length$467 + 1;
  self$466->$1 = _tmp$1540;
  moonbit_decref(self$466);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$463,
  struct $$3c$String$2a$Int$3e$* value$465
) {
  int32_t len$1531 = self$463->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$1533;
  int32_t _tmp$2345;
  int32_t _tmp$1532;
  int32_t length$464;
  struct $$3c$String$2a$Int$3e$** _field$2344;
  struct $$3c$String$2a$Int$3e$** buf$1534;
  struct $$3c$String$2a$Int$3e$* _old$2343;
  int32_t _tmp$1535;
  moonbit_incref(self$463);
  _tmp$1533 = $$moonbitlang$core$builtin$Array$$buffer$0(self$463);
  _tmp$2345 = Moonbit_array_length(_tmp$1533);
  moonbit_decref(_tmp$1533);
  _tmp$1532 = _tmp$2345;
  if (len$1531 == _tmp$1532) {
    moonbit_incref(self$463);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$463);
  }
  length$464 = self$463->$1;
  _field$2344 = self$463->$0;
  buf$1534 = _field$2344;
  _old$2343 = (struct $$3c$String$2a$Int$3e$*)buf$1534[length$464];
  if (_old$2343) {
    moonbit_decref(_old$2343);
  }
  buf$1534[length$464] = value$465;
  _tmp$1535 = length$464 + 1;
  self$463->$1 = _tmp$1535;
  moonbit_decref(self$463);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$460,
  moonbit_string_t value$462
) {
  int32_t len$1526 = self$460->$1;
  moonbit_string_t* _tmp$1528;
  int32_t _tmp$2348;
  int32_t _tmp$1527;
  int32_t length$461;
  moonbit_string_t* _field$2347;
  moonbit_string_t* buf$1529;
  moonbit_string_t _old$2346;
  int32_t _tmp$1530;
  moonbit_incref(self$460);
  _tmp$1528 = $$moonbitlang$core$builtin$Array$$buffer$1(self$460);
  _tmp$2348 = Moonbit_array_length(_tmp$1528);
  moonbit_decref(_tmp$1528);
  _tmp$1527 = _tmp$2348;
  if (len$1526 == _tmp$1527) {
    moonbit_incref(self$460);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$460);
  }
  length$461 = self$460->$1;
  _field$2347 = self$460->$0;
  buf$1529 = _field$2347;
  _old$2346 = (moonbit_string_t)buf$1529[length$461];
  moonbit_decref(_old$2346);
  buf$1529[length$461] = value$462;
  _tmp$1530 = length$461 + 1;
  self$460->$1 = _tmp$1530;
  moonbit_decref(self$460);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$458
) {
  int32_t old_cap$457 = self$458->$1;
  int32_t new_cap$459;
  if (old_cap$457 == 0) {
    new_cap$459 = 8;
  } else {
    new_cap$459 = old_cap$457 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$458, new_cap$459);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$455
) {
  int32_t old_cap$454 = self$455->$1;
  int32_t new_cap$456;
  if (old_cap$454 == 0) {
    new_cap$456 = 8;
  } else {
    new_cap$456 = old_cap$454 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$455, new_cap$456);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$452
) {
  int32_t old_cap$451 = self$452->$1;
  int32_t new_cap$453;
  if (old_cap$451 == 0) {
    new_cap$453 = 8;
  } else {
    new_cap$453 = old_cap$451 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$452, new_cap$453);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$448,
  int32_t new_capacity$446
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** new_buf$445 =
    (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_make_ref_array(
      new_capacity$446, 0
    );
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2350 =
    self$448->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$447 =
    _field$2350;
  int32_t old_cap$449 = Moonbit_array_length(old_buf$447);
  int32_t copy_len$450;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$2349;
  if (old_cap$449 < new_capacity$446) {
    copy_len$450 = old_cap$449;
  } else {
    copy_len$450 = new_capacity$446;
  }
  moonbit_incref(old_buf$447);
  moonbit_incref(new_buf$445);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$445, 0, old_buf$447, 0, copy_len$450
  );
  _old$2349 = self$448->$0;
  moonbit_decref(_old$2349);
  self$448->$0 = new_buf$445;
  moonbit_decref(self$448);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$442,
  int32_t new_capacity$440
) {
  struct $$3c$String$2a$Int$3e$** new_buf$439 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_make_ref_array(
      new_capacity$440, 0
    );
  struct $$3c$String$2a$Int$3e$** _field$2352 = self$442->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$441 = _field$2352;
  int32_t old_cap$443 = Moonbit_array_length(old_buf$441);
  int32_t copy_len$444;
  struct $$3c$String$2a$Int$3e$** _old$2351;
  if (old_cap$443 < new_capacity$440) {
    copy_len$444 = old_cap$443;
  } else {
    copy_len$444 = new_capacity$440;
  }
  moonbit_incref(old_buf$441);
  moonbit_incref(new_buf$439);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$439, 0, old_buf$441, 0, copy_len$444
  );
  _old$2351 = self$442->$0;
  moonbit_decref(_old$2351);
  self$442->$0 = new_buf$439;
  moonbit_decref(self$442);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$436,
  int32_t new_capacity$434
) {
  moonbit_string_t* new_buf$433 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$434, (moonbit_string_t)moonbit_string_literal_3.data
    );
  moonbit_string_t* _field$2354 = self$436->$0;
  moonbit_string_t* old_buf$435 = _field$2354;
  int32_t old_cap$437 = Moonbit_array_length(old_buf$435);
  int32_t copy_len$438;
  moonbit_string_t* _old$2353;
  if (old_cap$437 < new_capacity$434) {
    copy_len$438 = old_cap$437;
  } else {
    copy_len$438 = new_capacity$434;
  }
  moonbit_incref(old_buf$435);
  moonbit_incref(new_buf$433);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$433, 0, old_buf$435, 0, copy_len$438
  );
  _old$2353 = self$436->$0;
  moonbit_decref(_old$2353);
  self$436->$0 = new_buf$433;
  moonbit_decref(self$436);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$432
) {
  if (capacity$432 == 0) {
    moonbit_string_t* _tmp$1524 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2604 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2604)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2604->$0 = _tmp$1524;
    _block$2604->$1 = 0;
    return _block$2604;
  } else {
    moonbit_string_t* _tmp$1525 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$432, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2605 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2605)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2605->$0 = _tmp$1525;
    _block$2605->$1 = 0;
    return _block$2605;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$430,
  struct $StringView str$431
) {
  int32_t len$1512 = self$430->$1;
  int32_t _tmp$1514;
  int32_t _tmp$1513;
  int32_t _tmp$1511;
  moonbit_bytes_t _field$2355;
  moonbit_bytes_t data$1515;
  int32_t len$1516;
  moonbit_string_t _tmp$1517;
  int32_t _tmp$1518;
  int32_t _tmp$1519;
  int32_t len$1521;
  int32_t _tmp$1523;
  int32_t _tmp$1522;
  int32_t _tmp$1520;
  moonbit_incref(str$431.$0);
  _tmp$1514 = $StringView$$length(str$431);
  _tmp$1513 = _tmp$1514 * 2;
  _tmp$1511 = len$1512 + _tmp$1513;
  moonbit_incref(self$430);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$430, _tmp$1511
  );
  _field$2355 = self$430->$0;
  data$1515 = _field$2355;
  len$1516 = self$430->$1;
  moonbit_incref(data$1515);
  moonbit_incref(str$431.$0);
  _tmp$1517 = $StringView$$data(str$431);
  moonbit_incref(str$431.$0);
  _tmp$1518 = $StringView$$start_offset(str$431);
  moonbit_incref(str$431.$0);
  _tmp$1519 = $StringView$$length(str$431);
  $FixedArray$$blit_from_string(
    data$1515, len$1516, _tmp$1517, _tmp$1518, _tmp$1519
  );
  len$1521 = self$430->$1;
  _tmp$1523 = $StringView$$length(str$431);
  _tmp$1522 = _tmp$1523 * 2;
  _tmp$1520 = len$1521 + _tmp$1522;
  self$430->$1 = _tmp$1520;
  moonbit_decref(self$430);
  return 0;
}

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$427,
  int32_t i$428,
  int32_t start_offset$429,
  int64_t end_offset$425
) {
  int32_t end_offset$424;
  if (end_offset$425 == 4294967296ll) {
    end_offset$424 = Moonbit_array_length(self$427);
  } else {
    int64_t _Some$426 = end_offset$425;
    end_offset$424 = (int32_t)_Some$426;
  }
  if (i$428 >= 0) {
    return $String$$offset_of_nth_char_forward(
             self$427, i$428, start_offset$429, end_offset$424
           );
  } else {
    int32_t _tmp$1510 = -i$428;
    return $String$$offset_of_nth_char_backward(
             self$427, _tmp$1510, start_offset$429, end_offset$424
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$422,
  int32_t n$420,
  int32_t start_offset$416,
  int32_t end_offset$417
) {
  int32_t _if_result$2606;
  if (start_offset$416 >= 0) {
    _if_result$2606 = start_offset$416 <= end_offset$417;
  } else {
    _if_result$2606 = 0;
  }
  if (_if_result$2606) {
    int32_t utf16_offset$418 = start_offset$416;
    int32_t char_count$419 = 0;
    int32_t _tmp$1508;
    int32_t _if_result$2609;
    while (1) {
      int32_t _tmp$1502 = utf16_offset$418;
      int32_t _if_result$2608;
      if (_tmp$1502 < end_offset$417) {
        int32_t _tmp$1501 = char_count$419;
        _if_result$2608 = _tmp$1501 < n$420;
      } else {
        _if_result$2608 = 0;
      }
      if (_if_result$2608) {
        int32_t _tmp$1506 = utf16_offset$418;
        int32_t c$421 = self$422[_tmp$1506];
        int32_t _tmp$1505;
        if ($Int$$is_leading_surrogate(c$421)) {
          int32_t _tmp$1503 = utf16_offset$418;
          utf16_offset$418 = _tmp$1503 + 2;
        } else {
          int32_t _tmp$1504 = utf16_offset$418;
          utf16_offset$418 = _tmp$1504 + 1;
        }
        _tmp$1505 = char_count$419;
        char_count$419 = _tmp$1505 + 1;
        continue;
      } else {
        moonbit_decref(self$422);
      }
      break;
    }
    _tmp$1508 = char_count$419;
    if (_tmp$1508 < n$420) {
      _if_result$2609 = 1;
    } else {
      int32_t _tmp$1507 = utf16_offset$418;
      _if_result$2609 = _tmp$1507 >= end_offset$417;
    }
    if (_if_result$2609) {
      return 4294967296ll;
    } else {
      int32_t _tmp$1509 = utf16_offset$418;
      return (int64_t)_tmp$1509;
    }
  } else {
    moonbit_decref(self$422);
    return $moonbitlang$core$builtin$abort$3(
             (moonbit_string_t)moonbit_string_literal_14.data,
               (moonbit_string_t)moonbit_string_literal_15.data
           );
  }
}

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$414,
  int32_t n$412,
  int32_t start_offset$411,
  int32_t end_offset$410
) {
  int32_t char_count$408 = 0;
  int32_t utf16_offset$409 = end_offset$410;
  int32_t _tmp$1499;
  int32_t _if_result$2612;
  while (1) {
    int32_t _tmp$1492 = utf16_offset$409;
    int32_t _tmp$1491 = _tmp$1492 - 1;
    int32_t _if_result$2611;
    if (_tmp$1491 >= start_offset$411) {
      int32_t _tmp$1490 = char_count$408;
      _if_result$2611 = _tmp$1490 < n$412;
    } else {
      _if_result$2611 = 0;
    }
    if (_if_result$2611) {
      int32_t _tmp$1497 = utf16_offset$409;
      int32_t _tmp$1496 = _tmp$1497 - 1;
      int32_t c$413 = self$414[_tmp$1496];
      int32_t _tmp$1495;
      if ($Int$$is_trailing_surrogate(c$413)) {
        int32_t _tmp$1493 = utf16_offset$409;
        utf16_offset$409 = _tmp$1493 - 2;
      } else {
        int32_t _tmp$1494 = utf16_offset$409;
        utf16_offset$409 = _tmp$1494 - 1;
      }
      _tmp$1495 = char_count$408;
      char_count$408 = _tmp$1495 + 1;
      continue;
    } else {
      moonbit_decref(self$414);
    }
    break;
  }
  _tmp$1499 = char_count$408;
  if (_tmp$1499 < n$412) {
    _if_result$2612 = 1;
  } else {
    int32_t _tmp$1498 = utf16_offset$409;
    _if_result$2612 = _tmp$1498 < start_offset$411;
  }
  if (_if_result$2612) {
    return 4294967296ll;
  } else {
    int32_t _tmp$1500 = utf16_offset$409;
    return (int64_t)_tmp$1500;
  }
}

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$400,
  int32_t len$403,
  int32_t start_offset$407,
  int64_t end_offset$398
) {
  int32_t end_offset$397;
  int32_t index$401;
  int32_t count$402;
  if (end_offset$398 == 4294967296ll) {
    end_offset$397 = Moonbit_array_length(self$400);
  } else {
    int64_t _Some$399 = end_offset$398;
    end_offset$397 = (int32_t)_Some$399;
  }
  index$401 = start_offset$407;
  count$402 = 0;
  while (1) {
    int32_t _if_result$2614;
    if (index$401 < end_offset$397) {
      _if_result$2614 = count$402 < len$403;
    } else {
      _if_result$2614 = 0;
    }
    if (_if_result$2614) {
      int32_t c1$404 = self$400[index$401];
      int32_t _if_result$2615;
      int32_t _tmp$1488;
      int32_t _tmp$1489;
      if ($Int$$is_leading_surrogate(c1$404)) {
        int32_t _tmp$1484 = index$401 + 1;
        _if_result$2615 = _tmp$1484 < end_offset$397;
      } else {
        _if_result$2615 = 0;
      }
      if (_if_result$2615) {
        int32_t _tmp$1487 = index$401 + 1;
        int32_t c2$405 = self$400[_tmp$1487];
        if ($Int$$is_trailing_surrogate(c2$405)) {
          int32_t _tmp$1485 = index$401 + 2;
          int32_t _tmp$1486 = count$402 + 1;
          index$401 = _tmp$1485;
          count$402 = _tmp$1486;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_16.data,
              (moonbit_string_t)moonbit_string_literal_17.data
          );
        }
      }
      _tmp$1488 = index$401 + 1;
      _tmp$1489 = count$402 + 1;
      index$401 = _tmp$1488;
      count$402 = _tmp$1489;
      continue;
    } else {
      moonbit_decref(self$400);
      return count$402 >= len$403;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$396
) {
  int32_t end$1482 = self$396.$2;
  int32_t _field$2356 = self$396.$1;
  int32_t start$1483;
  moonbit_decref(self$396.$0);
  start$1483 = _field$2356;
  return end$1482 - start$1483;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$395
) {
  int32_t end$1480 = self$395.$2;
  int32_t _field$2357 = self$395.$1;
  int32_t start$1481;
  moonbit_decref(self$395.$0);
  start$1481 = _field$2357;
  return end$1480 - start$1481;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$394
) {
  int32_t end$1478 = self$394.$2;
  int32_t _field$2358 = self$394.$1;
  int32_t start$1479;
  moonbit_decref(self$394.$0);
  start$1479 = _field$2358;
  return end$1478 - start$1479;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$393
) {
  int32_t end$1476 = self$393.$2;
  int32_t _field$2359 = self$393.$1;
  int32_t start$1477;
  moonbit_decref(self$393.$0);
  start$1477 = _field$2359;
  return end$1476 - start$1477;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$392
) {
  int32_t end$1474 = self$392.$2;
  int32_t _field$2360 = self$392.$1;
  int32_t start$1475;
  moonbit_decref(self$392.$0);
  start$1475 = _field$2360;
  return end$1474 - start$1475;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$2616 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$2616)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2616->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$2616->$0 = self$387;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$2616;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1472,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$385
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$1473 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$1472;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$2361 =
    _casted_env$1473->$0;
  int32_t _cnt$2512 = Moonbit_object_header(_casted_env$1473)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387;
  if (_cnt$2512 > 1) {
    int32_t _new_cnt$2513;
    moonbit_incref(_field$2361);
    _new_cnt$2513 = _cnt$2512 - 1;
    Moonbit_object_header(_casted_env$1473)->rc = _new_cnt$2513;
  } else if (_cnt$2512 == 1) {
    moonbit_free(_casted_env$1473);
  }
  self$387 = _field$2361;
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
    struct $$moonbitlang$core$builtin$Logger _bind$1454;
    int32_t _tmp$1455;
    int32_t _tmp$1456;
    int32_t _tmp$1457;
    int32_t _tmp$2621;
    int32_t _tmp$2622;
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
        int32_t _tmp$1458;
        int32_t _tmp$1459;
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
        _tmp$1458 = i$377 + 1;
        _tmp$1459 = i$377 + 1;
        i$377 = _tmp$1458;
        seg$378 = _tmp$1459;
        goto $$2a$for$379;
        break;
      }
      
      case 13: {
        int32_t _tmp$1460;
        int32_t _tmp$1461;
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
        _tmp$1460 = i$377 + 1;
        _tmp$1461 = i$377 + 1;
        i$377 = _tmp$1460;
        seg$378 = _tmp$1461;
        goto $$2a$for$379;
        break;
      }
      
      case 8: {
        int32_t _tmp$1462;
        int32_t _tmp$1463;
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
        _tmp$1462 = i$377 + 1;
        _tmp$1463 = i$377 + 1;
        i$377 = _tmp$1462;
        seg$378 = _tmp$1463;
        goto $$2a$for$379;
        break;
      }
      
      case 9: {
        int32_t _tmp$1464;
        int32_t _tmp$1465;
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
        _tmp$1464 = i$377 + 1;
        _tmp$1465 = i$377 + 1;
        i$377 = _tmp$1464;
        seg$378 = _tmp$1465;
        goto $$2a$for$379;
        break;
      }
      default: {
        if (code$380 < 32) {
          int32_t _tmp$1468;
          moonbit_string_t _tmp$1467;
          struct $$moonbitlang$core$builtin$Logger _bind$1466;
          int32_t _tmp$1469;
          int32_t _tmp$1470;
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
          _tmp$1468 = code$380 & 0xff;
          _tmp$1467 = $Byte$$to_hex(_tmp$1468);
          if (logger$373.$1) {
            moonbit_incref(logger$373.$1);
          }
          logger$373.$0->$method_0(logger$373.$1, _tmp$1467);
          _bind$1466 = logger$373;
          if (_bind$1466.$1) {
            moonbit_incref(_bind$1466.$1);
          }
          _bind$1466.$0->$method_3(_bind$1466.$1, 125);
          _tmp$1469 = i$377 + 1;
          _tmp$1470 = i$377 + 1;
          i$377 = _tmp$1469;
          seg$378 = _tmp$1470;
          goto $$2a$for$379;
        } else {
          int32_t _tmp$1471 = i$377 + 1;
          int32_t _tmp$2620 = seg$378;
          i$377 = _tmp$1471;
          seg$378 = _tmp$2620;
          goto $$2a$for$379;
        }
        break;
      }
    }
    goto $joinlet$2619;
    $join$381:;
    moonbit_incref(_env$374);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$374, seg$378, i$377
    );
    if (logger$373.$1) {
      moonbit_incref(logger$373.$1);
    }
    logger$373.$0->$method_3(logger$373.$1, 92);
    _bind$1454 = logger$373;
    _tmp$1455 = c$382;
    if (_bind$1454.$1) {
      moonbit_incref(_bind$1454.$1);
    }
    _bind$1454.$0->$method_3(_bind$1454.$1, _tmp$1455);
    _tmp$1456 = i$377 + 1;
    _tmp$1457 = i$377 + 1;
    i$377 = _tmp$1456;
    seg$378 = _tmp$1457;
    continue;
    $joinlet$2619:;
    _tmp$2621 = i$377;
    _tmp$2622 = seg$378;
    i$377 = _tmp$2621;
    seg$378 = _tmp$2622;
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
  moonbit_string_t _field$2363 = _env$369->$1;
  moonbit_string_t self$368 = _field$2363;
  struct $$moonbitlang$core$builtin$Logger _field$2362 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$369->$0_0, _env$369->$0_1
    };
  int32_t _cnt$2514 = Moonbit_object_header(_env$369)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$370;
  if (_cnt$2514 > 1) {
    int32_t _new_cnt$2515;
    moonbit_incref(self$368);
    if (_field$2362.$1) {
      moonbit_incref(_field$2362.$1);
    }
    _new_cnt$2515 = _cnt$2514 - 1;
    Moonbit_object_header(_env$369)->rc = _new_cnt$2515;
  } else if (_cnt$2514 == 1) {
    moonbit_free(_env$369);
  }
  logger$370 = _field$2362;
  if (i$371 > seg$372) {
    int32_t _tmp$1453 = i$371 - seg$372;
    logger$370.$0->$method_1(logger$370.$1, self$368, seg$372, _tmp$1453);
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
  int32_t _tmp$1450 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$367, 16);
  int32_t _tmp$1449 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$1450);
  int32_t _tmp$1452;
  int32_t _tmp$1451;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$1448;
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1449
  );
  _tmp$1452 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$367, 16);
  _tmp$1451
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$1452
  );
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1451
  );
  _tmp$1448 = _self$366;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$1448);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$365) {
  if (i$365 < 10) {
    int32_t _tmp$1445 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 48);
    return $Byte$$to_char(_tmp$1445);
  } else {
    int32_t _tmp$1447 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 97);
    int32_t _tmp$1446 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$1447, 10);
    return $Byte$$to_char(_tmp$1446);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$363,
  int32_t that$364
) {
  int32_t _tmp$1443 = (int32_t)self$363;
  int32_t _tmp$1444 = (int32_t)that$364;
  int32_t _tmp$1442 = _tmp$1443 - _tmp$1444;
  return _tmp$1442 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$361,
  int32_t that$362
) {
  int32_t _tmp$1440 = (int32_t)self$361;
  int32_t _tmp$1441 = (int32_t)that$362;
  int32_t _tmp$1439 = _tmp$1440 % _tmp$1441;
  return _tmp$1439 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$359,
  int32_t that$360
) {
  int32_t _tmp$1437 = (int32_t)self$359;
  int32_t _tmp$1438 = (int32_t)that$360;
  int32_t _tmp$1436 = _tmp$1437 / _tmp$1438;
  return _tmp$1436 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$357,
  int32_t that$358
) {
  int32_t _tmp$1434 = (int32_t)self$357;
  int32_t _tmp$1435 = (int32_t)that$358;
  int32_t _tmp$1433 = _tmp$1434 + _tmp$1435;
  return _tmp$1433 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$354,
  int32_t start$352,
  int32_t end$353
) {
  int32_t _if_result$2623;
  int32_t len$355;
  int32_t _tmp$1431;
  int32_t _tmp$1432;
  moonbit_bytes_t bytes$356;
  moonbit_bytes_t _tmp$1430;
  if (start$352 == 0) {
    int32_t _tmp$1429 = Moonbit_array_length(str$354);
    _if_result$2623 = end$353 == _tmp$1429;
  } else {
    _if_result$2623 = 0;
  }
  if (_if_result$2623) {
    return str$354;
  }
  len$355 = end$353 - start$352;
  _tmp$1431 = len$355 * 2;
  _tmp$1432 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$356 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1431, _tmp$1432);
  moonbit_incref(bytes$356);
  $FixedArray$$blit_from_string(bytes$356, 0, str$354, start$352, len$355);
  _tmp$1430 = bytes$356;
  return $Bytes$$to_unchecked_string$inner(_tmp$1430, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$351
) {
  return f$351;
}

moonbit_string_t $Int$$to_string$inner(int32_t self$335, int32_t radix$334) {
  int32_t _if_result$2624;
  int32_t is_negative$336;
  uint32_t num$337;
  uint16_t* buffer$338;
  if (radix$334 < 2) {
    _if_result$2624 = 1;
  } else {
    _if_result$2624 = radix$334 > 36;
  }
  if (_if_result$2624) {
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
    int32_t _tmp$1428 = -self$335;
    num$337 = *(uint32_t*)&_tmp$1428;
  } else {
    num$337 = *(uint32_t*)&self$335;
  }
  switch (radix$334) {
    case 10: {
      int32_t digit_len$339 = $moonbitlang$core$builtin$dec_count32(num$337);
      int32_t _tmp$1425;
      int32_t total_len$340;
      uint16_t* buffer$341;
      int32_t digit_start$342;
      if (is_negative$336) {
        _tmp$1425 = 1;
      } else {
        _tmp$1425 = 0;
      }
      total_len$340 = digit_len$339 + _tmp$1425;
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
      int32_t _tmp$1426;
      int32_t total_len$344;
      uint16_t* buffer$345;
      int32_t digit_start$346;
      if (is_negative$336) {
        _tmp$1426 = 1;
      } else {
        _tmp$1426 = 0;
      }
      total_len$344 = digit_len$343 + _tmp$1426;
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
      int32_t _tmp$1427;
      int32_t total_len$348;
      uint16_t* buffer$349;
      int32_t digit_start$350;
      if (is_negative$336) {
        _tmp$1427 = 1;
      } else {
        _tmp$1427 = 0;
      }
      total_len$348 = digit_len$347 + _tmp$1427;
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
    uint32_t _tmp$1422 = num$329;
    if (_tmp$1422 > 0u) {
      int32_t _tmp$1423 = count$332;
      uint32_t _tmp$1424;
      count$332 = _tmp$1423 + 1;
      _tmp$1424 = num$329;
      num$329 = _tmp$1424 / base$330;
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
    int32_t _tmp$1421 = 31 - leading_zeros$327;
    int32_t _tmp$1420 = _tmp$1421 / 4;
    return _tmp$1420 + 1;
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
  uint32_t _tmp$1419;
  int32_t remaining$317;
  int32_t _tmp$1400;
  while (1) {
    uint32_t _tmp$1363 = num$302;
    if (_tmp$1363 >= 10000u) {
      uint32_t _tmp$1386 = num$302;
      uint32_t t$307 = _tmp$1386 / 10000u;
      uint32_t _tmp$1385 = num$302;
      uint32_t _tmp$1384 = _tmp$1385 % 10000u;
      int32_t r$308 = *(int32_t*)&_tmp$1384;
      int32_t d1$309;
      int32_t d2$310;
      int32_t _tmp$1364;
      int32_t _tmp$1383;
      int32_t _tmp$1382;
      int32_t d1_hi$311;
      int32_t _tmp$1381;
      int32_t _tmp$1380;
      int32_t d1_lo$312;
      int32_t _tmp$1379;
      int32_t _tmp$1378;
      int32_t d2_hi$313;
      int32_t _tmp$1377;
      int32_t _tmp$1376;
      int32_t d2_lo$314;
      int32_t _tmp$1366;
      int32_t _tmp$1365;
      int32_t _tmp$1369;
      int32_t _tmp$1368;
      int32_t _tmp$1367;
      int32_t _tmp$1372;
      int32_t _tmp$1371;
      int32_t _tmp$1370;
      int32_t _tmp$1375;
      int32_t _tmp$1374;
      int32_t _tmp$1373;
      num$302 = t$307;
      d1$309 = r$308 / 100;
      d2$310 = r$308 % 100;
      _tmp$1364 = offset$304;
      offset$304 = _tmp$1364 - 4;
      _tmp$1383 = d1$309 / 10;
      _tmp$1382 = 48 + _tmp$1383;
      d1_hi$311 = (uint16_t)_tmp$1382;
      _tmp$1381 = d1$309 % 10;
      _tmp$1380 = 48 + _tmp$1381;
      d1_lo$312 = (uint16_t)_tmp$1380;
      _tmp$1379 = d2$310 / 10;
      _tmp$1378 = 48 + _tmp$1379;
      d2_hi$313 = (uint16_t)_tmp$1378;
      _tmp$1377 = d2$310 % 10;
      _tmp$1376 = 48 + _tmp$1377;
      d2_lo$314 = (uint16_t)_tmp$1376;
      _tmp$1366 = offset$304;
      _tmp$1365 = digit_start$306 + _tmp$1366;
      buffer$315[_tmp$1365] = d1_hi$311;
      _tmp$1369 = offset$304;
      _tmp$1368 = digit_start$306 + _tmp$1369;
      _tmp$1367 = _tmp$1368 + 1;
      buffer$315[_tmp$1367] = d1_lo$312;
      _tmp$1372 = offset$304;
      _tmp$1371 = digit_start$306 + _tmp$1372;
      _tmp$1370 = _tmp$1371 + 2;
      buffer$315[_tmp$1370] = d2_hi$313;
      _tmp$1375 = offset$304;
      _tmp$1374 = digit_start$306 + _tmp$1375;
      _tmp$1373 = _tmp$1374 + 3;
      buffer$315[_tmp$1373] = d2_lo$314;
      continue;
    }
    break;
  }
  _tmp$1419 = num$302;
  remaining$317 = *(int32_t*)&_tmp$1419;
  while (1) {
    int32_t _tmp$1387 = remaining$317;
    if (_tmp$1387 >= 100) {
      int32_t _tmp$1399 = remaining$317;
      int32_t t$318 = _tmp$1399 / 100;
      int32_t _tmp$1398 = remaining$317;
      int32_t d$319 = _tmp$1398 % 100;
      int32_t _tmp$1388;
      int32_t _tmp$1397;
      int32_t _tmp$1396;
      int32_t d_hi$320;
      int32_t _tmp$1395;
      int32_t _tmp$1394;
      int32_t d_lo$321;
      int32_t _tmp$1390;
      int32_t _tmp$1389;
      int32_t _tmp$1393;
      int32_t _tmp$1392;
      int32_t _tmp$1391;
      remaining$317 = t$318;
      _tmp$1388 = offset$304;
      offset$304 = _tmp$1388 - 2;
      _tmp$1397 = d$319 / 10;
      _tmp$1396 = 48 + _tmp$1397;
      d_hi$320 = (uint16_t)_tmp$1396;
      _tmp$1395 = d$319 % 10;
      _tmp$1394 = 48 + _tmp$1395;
      d_lo$321 = (uint16_t)_tmp$1394;
      _tmp$1390 = offset$304;
      _tmp$1389 = digit_start$306 + _tmp$1390;
      buffer$315[_tmp$1389] = d_hi$320;
      _tmp$1393 = offset$304;
      _tmp$1392 = digit_start$306 + _tmp$1393;
      _tmp$1391 = _tmp$1392 + 1;
      buffer$315[_tmp$1391] = d_lo$321;
      continue;
    }
    break;
  }
  _tmp$1400 = remaining$317;
  if (_tmp$1400 >= 10) {
    int32_t _tmp$1401 = offset$304;
    int32_t _tmp$1412;
    int32_t _tmp$1411;
    int32_t _tmp$1410;
    int32_t d_hi$323;
    int32_t _tmp$1409;
    int32_t _tmp$1408;
    int32_t _tmp$1407;
    int32_t d_lo$324;
    int32_t _tmp$1403;
    int32_t _tmp$1402;
    int32_t _tmp$1406;
    int32_t _tmp$1405;
    int32_t _tmp$1404;
    offset$304 = _tmp$1401 - 2;
    _tmp$1412 = remaining$317;
    _tmp$1411 = _tmp$1412 / 10;
    _tmp$1410 = 48 + _tmp$1411;
    d_hi$323 = (uint16_t)_tmp$1410;
    _tmp$1409 = remaining$317;
    _tmp$1408 = _tmp$1409 % 10;
    _tmp$1407 = 48 + _tmp$1408;
    d_lo$324 = (uint16_t)_tmp$1407;
    _tmp$1403 = offset$304;
    _tmp$1402 = digit_start$306 + _tmp$1403;
    buffer$315[_tmp$1402] = d_hi$323;
    _tmp$1406 = offset$304;
    _tmp$1405 = digit_start$306 + _tmp$1406;
    _tmp$1404 = _tmp$1405 + 1;
    buffer$315[_tmp$1404] = d_lo$324;
    moonbit_decref(buffer$315);
  } else {
    int32_t _tmp$1413 = offset$304;
    int32_t _tmp$1418;
    int32_t _tmp$1414;
    int32_t _tmp$1417;
    int32_t _tmp$1416;
    int32_t _tmp$1415;
    offset$304 = _tmp$1413 - 1;
    _tmp$1418 = offset$304;
    _tmp$1414 = digit_start$306 + _tmp$1418;
    _tmp$1417 = remaining$317;
    _tmp$1416 = 48 + _tmp$1417;
    _tmp$1415 = (uint16_t)_tmp$1416;
    buffer$315[_tmp$1414] = _tmp$1415;
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
  int32_t _tmp$1343 = radix$293 - 1;
  int32_t _tmp$1342 = radix$293 & _tmp$1343;
  if (_tmp$1342 == 0) {
    int32_t shift$294 = moonbit_ctz32(radix$293);
    uint32_t mask$295 = base$292 - 1u;
    while (1) {
      uint32_t _tmp$1344 = n$290;
      if (_tmp$1344 > 0u) {
        int32_t _tmp$1345 = offset$287;
        uint32_t _tmp$1352;
        uint32_t _tmp$1351;
        int32_t digit$296;
        int32_t _tmp$1349;
        int32_t _tmp$1346;
        int32_t _tmp$1348;
        int32_t _tmp$1347;
        uint32_t _tmp$1350;
        offset$287 = _tmp$1345 - 1;
        _tmp$1352 = n$290;
        _tmp$1351 = _tmp$1352 & mask$295;
        digit$296 = *(int32_t*)&_tmp$1351;
        _tmp$1349 = offset$287;
        _tmp$1346 = digit_start$289 + _tmp$1349;
        _tmp$1348
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$296
        ];
        _tmp$1347 = (uint16_t)_tmp$1348;
        buffer$297[_tmp$1346] = _tmp$1347;
        _tmp$1350 = n$290;
        n$290 = _tmp$1350 >> (shift$294 & 31);
        continue;
      } else {
        moonbit_decref(buffer$297);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1353 = n$290;
      if (_tmp$1353 > 0u) {
        int32_t _tmp$1354 = offset$287;
        uint32_t _tmp$1362;
        uint32_t q$299;
        uint32_t _tmp$1360;
        uint32_t _tmp$1361;
        uint32_t _tmp$1359;
        int32_t digit$300;
        int32_t _tmp$1358;
        int32_t _tmp$1355;
        int32_t _tmp$1357;
        int32_t _tmp$1356;
        offset$287 = _tmp$1354 - 1;
        _tmp$1362 = n$290;
        q$299 = _tmp$1362 / base$292;
        _tmp$1360 = n$290;
        _tmp$1361 = q$299 * base$292;
        _tmp$1359 = _tmp$1360 - _tmp$1361;
        digit$300 = *(int32_t*)&_tmp$1359;
        _tmp$1358 = offset$287;
        _tmp$1355 = digit_start$289 + _tmp$1358;
        _tmp$1357
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$300
        ];
        _tmp$1356 = (uint16_t)_tmp$1357;
        buffer$297[_tmp$1355] = _tmp$1356;
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
  int32_t _tmp$1337;
  while (1) {
    int32_t _tmp$1323 = offset$276;
    if (_tmp$1323 >= 2) {
      int32_t _tmp$1324 = offset$276;
      uint32_t _tmp$1336;
      uint32_t _tmp$1335;
      int32_t byte_val$281;
      int32_t hi$282;
      int32_t lo$283;
      int32_t _tmp$1328;
      int32_t _tmp$1325;
      int32_t _tmp$1327;
      int32_t _tmp$1326;
      int32_t _tmp$1333;
      int32_t _tmp$1332;
      int32_t _tmp$1329;
      int32_t _tmp$1331;
      int32_t _tmp$1330;
      uint32_t _tmp$1334;
      offset$276 = _tmp$1324 - 2;
      _tmp$1336 = n$279;
      _tmp$1335 = _tmp$1336 & 255u;
      byte_val$281 = *(int32_t*)&_tmp$1335;
      hi$282 = byte_val$281 / 16;
      lo$283 = byte_val$281 % 16;
      _tmp$1328 = offset$276;
      _tmp$1325 = digit_start$278 + _tmp$1328;
      _tmp$1327 = ((moonbit_string_t)moonbit_string_literal_26.data)[hi$282];
      _tmp$1326 = (uint16_t)_tmp$1327;
      buffer$284[_tmp$1325] = _tmp$1326;
      _tmp$1333 = offset$276;
      _tmp$1332 = digit_start$278 + _tmp$1333;
      _tmp$1329 = _tmp$1332 + 1;
      _tmp$1331 = ((moonbit_string_t)moonbit_string_literal_26.data)[lo$283];
      _tmp$1330 = (uint16_t)_tmp$1331;
      buffer$284[_tmp$1329] = _tmp$1330;
      _tmp$1334 = n$279;
      n$279 = _tmp$1334 >> 8;
      continue;
    }
    break;
  }
  _tmp$1337 = offset$276;
  if (_tmp$1337 == 1) {
    uint32_t _tmp$1341 = n$279;
    uint32_t _tmp$1340 = _tmp$1341 & 15u;
    int32_t nibble$286 = *(int32_t*)&_tmp$1340;
    int32_t _tmp$1339 =
      ((moonbit_string_t)moonbit_string_literal_26.data)[nibble$286];
    int32_t _tmp$1338 = (uint16_t)_tmp$1339;
    buffer$284[digit_start$278] = _tmp$1338;
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
  struct $$moonbitlang$core$builtin$Logger _tmp$1322;
  moonbit_incref(logger$274);
  _tmp$1322
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$274
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$275, _tmp$1322
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$274);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$273
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$272 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1321;
  moonbit_incref(logger$272);
  _tmp$1321
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$272
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$273, _tmp$1321
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$272);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$271
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$270 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1320;
  moonbit_incref(logger$270);
  _tmp$1320
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$270
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$271, _tmp$1320
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$270);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$269
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$268 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1319;
  moonbit_incref(logger$268);
  _tmp$1319
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$268
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$269, _tmp$1319);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$268);
}

int32_t $StringView$$start_offset(struct $StringView self$267) {
  int32_t _field$2364 = self$267.$1;
  moonbit_decref(self$267.$0);
  return _field$2364;
}

moonbit_string_t $StringView$$data(struct $StringView self$266) {
  moonbit_string_t _field$2365 = self$266.$0;
  return _field$2365;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$260,
  moonbit_string_t value$263,
  int32_t start$264,
  int32_t len$265
) {
  void* _try_err$262;
  struct $StringView _tmp$1314;
  int32_t _tmp$1316 = start$264 + len$265;
  int64_t _tmp$1315 = (int64_t)_tmp$1316;
  struct moonbit_result_1 _tmp$2632 =
    $String$$sub$inner(value$263, start$264, _tmp$1315);
  if (_tmp$2632.tag) {
    struct $StringView const _ok$1317 = _tmp$2632.data.ok;
    _tmp$1314 = _ok$1317;
  } else {
    void* const _err$1318 = _tmp$2632.data.err;
    _try_err$262 = _err$1318;
    goto $join$261;
  }
  goto $joinlet$2631;
  $join$261:;
  moonbit_decref(_try_err$262);
  moonbit_panic();
  $joinlet$2631:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$260, _tmp$1314
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
  int32_t _if_result$2633;
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
      _if_result$2633 = end$254 <= len$252;
    } else {
      _if_result$2633 = 0;
    }
  } else {
    _if_result$2633 = 0;
  }
  if (_if_result$2633) {
    int32_t _if_result$2634;
    int32_t _if_result$2636;
    struct $StringView _tmp$1312;
    struct moonbit_result_1 _result$2638;
    if (start$258 < len$252) {
      int32_t _tmp$1308 = self$253[start$258];
      _if_result$2634 = $Int$$is_trailing_surrogate(_tmp$1308);
    } else {
      _if_result$2634 = 0;
    }
    if (_if_result$2634) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1309;
      struct moonbit_result_1 _result$2635;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1309
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2635.tag = 0;
      _result$2635.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1309;
      return _result$2635;
    }
    if (end$254 < len$252) {
      int32_t _tmp$1310 = self$253[end$254];
      _if_result$2636 = $Int$$is_trailing_surrogate(_tmp$1310);
    } else {
      _if_result$2636 = 0;
    }
    if (_if_result$2636) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1311;
      struct moonbit_result_1 _result$2637;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1311
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2637.tag = 0;
      _result$2637.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1311;
      return _result$2637;
    }
    _tmp$1312 = (struct $StringView){start$258, end$254, self$253};
    _result$2638.tag = 1;
    _result$2638.data.ok = _tmp$1312;
    return _result$2638;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1313;
    struct moonbit_result_1 _result$2639;
    moonbit_decref(self$253);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1313
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$2639.tag = 0;
    _result$2639.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1313;
    return _result$2639;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$251
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$250 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1307;
  moonbit_incref(_self$250);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$250, self$251);
  _tmp$1307 = _self$250;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1307);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$249
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$248 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1306;
  moonbit_incref(_self$248);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$248, self$249);
  _tmp$1306 = _self$248;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1306);
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
  uint32_t _tmp$1305 = *(uint32_t*)&seed$244;
  uint32_t _tmp$1304 = _tmp$1305 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$2640 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$2640)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$2640->$0 = _tmp$1304;
  return _block$2640;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$243
) {
  uint32_t _tmp$1303 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$243);
  return *(int32_t*)&_tmp$1303;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$242
) {
  uint32_t _field$2366 = self$242->$0;
  uint32_t acc$241;
  uint32_t _tmp$1292;
  uint32_t _tmp$1294;
  uint32_t _tmp$1293;
  uint32_t _tmp$1295;
  uint32_t _tmp$1296;
  uint32_t _tmp$1298;
  uint32_t _tmp$1297;
  uint32_t _tmp$1299;
  uint32_t _tmp$1300;
  uint32_t _tmp$1302;
  uint32_t _tmp$1301;
  moonbit_decref(self$242);
  acc$241 = _field$2366;
  _tmp$1292 = acc$241;
  _tmp$1294 = acc$241;
  _tmp$1293 = _tmp$1294 >> 15;
  acc$241 = _tmp$1292 ^ _tmp$1293;
  _tmp$1295 = acc$241;
  acc$241 = _tmp$1295 * 2246822519u;
  _tmp$1296 = acc$241;
  _tmp$1298 = acc$241;
  _tmp$1297 = _tmp$1298 >> 13;
  acc$241 = _tmp$1296 ^ _tmp$1297;
  _tmp$1299 = acc$241;
  acc$241 = _tmp$1299 * 3266489917u;
  _tmp$1300 = acc$241;
  _tmp$1302 = acc$241;
  _tmp$1301 = _tmp$1302 >> 16;
  acc$241 = _tmp$1300 ^ _tmp$1301;
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
  uint32_t _tmp$1291 = *(uint32_t*)&value$236;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$235, _tmp$1291);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$233,
  uint32_t value$234
) {
  uint32_t acc$1290 = self$233->$0;
  uint32_t _tmp$1289 = acc$1290 + 4u;
  self$233->$0 = _tmp$1289;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$233, value$234);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$231,
  uint32_t input$232
) {
  uint32_t acc$1287 = self$231->$0;
  uint32_t _tmp$1288 = input$232 * 3266489917u;
  uint32_t _tmp$1286 = acc$1287 + _tmp$1288;
  uint32_t _tmp$1285 = $moonbitlang$core$builtin$rotl(_tmp$1286, 17);
  uint32_t _tmp$1284 = _tmp$1285 * 668265263u;
  self$231->$0 = _tmp$1284;
  moonbit_decref(self$231);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$229, int32_t r$230) {
  uint32_t _tmp$1281 = x$229 << (r$230 & 31);
  int32_t _tmp$1283 = 32 - r$230;
  uint32_t _tmp$1282 = x$229 >> (_tmp$1283 & 31);
  return _tmp$1281 | _tmp$1282;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$227,
  moonbit_string_t str$228
) {
  int32_t len$1271 = self$227->$1;
  int32_t _tmp$1273 = Moonbit_array_length(str$228);
  int32_t _tmp$1272 = _tmp$1273 * 2;
  int32_t _tmp$1270 = len$1271 + _tmp$1272;
  moonbit_bytes_t _field$2368;
  moonbit_bytes_t data$1274;
  int32_t len$1275;
  int32_t _tmp$1276;
  int32_t len$1278;
  int32_t _tmp$2367;
  int32_t _tmp$1280;
  int32_t _tmp$1279;
  int32_t _tmp$1277;
  moonbit_incref(self$227);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$227, _tmp$1270
  );
  _field$2368 = self$227->$0;
  data$1274 = _field$2368;
  len$1275 = self$227->$1;
  _tmp$1276 = Moonbit_array_length(str$228);
  moonbit_incref(data$1274);
  moonbit_incref(str$228);
  $FixedArray$$blit_from_string(data$1274, len$1275, str$228, 0, _tmp$1276);
  len$1278 = self$227->$1;
  _tmp$2367 = Moonbit_array_length(str$228);
  moonbit_decref(str$228);
  _tmp$1280 = _tmp$2367;
  _tmp$1279 = _tmp$1280 * 2;
  _tmp$1277 = len$1278 + _tmp$1279;
  self$227->$1 = _tmp$1277;
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
  int32_t _tmp$1269 = length$215 * 2;
  int32_t _tmp$1268 = bytes_offset$214 + _tmp$1269;
  int32_t e1$213 = _tmp$1268 - 1;
  int32_t _tmp$1267 = str_offset$217 + length$215;
  int32_t e2$216 = _tmp$1267 - 1;
  int32_t len1$218 = Moonbit_array_length(self$219);
  int32_t len2$220 = Moonbit_array_length(str$221);
  int32_t _if_result$2641;
  if (length$215 >= 0) {
    if (bytes_offset$214 >= 0) {
      if (e1$213 < len1$218) {
        if (str_offset$217 >= 0) {
          _if_result$2641 = e2$216 < len2$220;
        } else {
          _if_result$2641 = 0;
        }
      } else {
        _if_result$2641 = 0;
      }
    } else {
      _if_result$2641 = 0;
    }
  } else {
    _if_result$2641 = 0;
  }
  if (_if_result$2641) {
    int32_t end_str_offset$222 = str_offset$217 + length$215;
    int32_t i$223 = str_offset$217;
    int32_t j$224 = bytes_offset$214;
    while (1) {
      if (i$223 < end_str_offset$222) {
        int32_t _tmp$1264 = str$221[i$223];
        uint32_t c$225 = *(uint32_t*)&_tmp$1264;
        uint32_t _tmp$1260 = c$225 & 255u;
        int32_t _tmp$1259 = $UInt$$to_byte(_tmp$1260);
        int32_t _tmp$1261;
        uint32_t _tmp$1263;
        int32_t _tmp$1262;
        int32_t _tmp$1265;
        int32_t _tmp$1266;
        if (j$224 < 0 || j$224 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[j$224] = _tmp$1259;
        _tmp$1261 = j$224 + 1;
        _tmp$1263 = c$225 >> 8;
        _tmp$1262 = $UInt$$to_byte(_tmp$1263);
        if (_tmp$1261 < 0 || _tmp$1261 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[_tmp$1261] = _tmp$1262;
        _tmp$1265 = i$223 + 1;
        _tmp$1266 = j$224 + 2;
        i$223 = _tmp$1265;
        j$224 = _tmp$1266;
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
  int32_t _tmp$1232 = Moonbit_array_length(repr$181);
  int64_t _tmp$1231 = (int64_t)_tmp$1232;
  moonbit_incref(repr$181);
  if ($String$$char_length_ge$inner(repr$181, 1, 0, _tmp$1231)) {
    int32_t _tmp$1258 = repr$181[0];
    int32_t _x$182 = _tmp$1258;
    if (_x$182 == 64) {
      int32_t _tmp$1257 = Moonbit_array_length(repr$181);
      int64_t _tmp$1256 = (int64_t)_tmp$1257;
      int64_t _bind$1015;
      int32_t _tmp$1254;
      int32_t _tmp$1255;
      struct $StringView _x$183;
      int32_t _tmp$1253;
      struct $StringView _tmp$1252;
      int64_t _bind$185;
      moonbit_incref(repr$181);
      _bind$1015
      = $String$$offset_of_nth_char$inner(
        repr$181, 1, 0, _tmp$1256
      );
      if (_bind$1015 == 4294967296ll) {
        _tmp$1254 = Moonbit_array_length(repr$181);
      } else {
        int64_t _Some$184 = _bind$1015;
        _tmp$1254 = (int32_t)_Some$184;
      }
      _tmp$1255 = Moonbit_array_length(repr$181);
      _x$183 = (struct $StringView){_tmp$1254, _tmp$1255, repr$181};
      _tmp$1253
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1252
      = (struct $StringView){
        0, _tmp$1253, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$183.$0);
      _bind$185 = $StringView$$find(_x$183, _tmp$1252);
      if (_bind$185 == 4294967296ll) {
        moonbit_decref(_x$183.$0);
        moonbit_panic();
      } else {
        int64_t _Some$186 = _bind$185;
        int32_t _pkg_end$187 = (int32_t)_Some$186;
        int64_t _tmp$1251 = (int64_t)_pkg_end$187;
        struct $StringView pkg$188;
        int32_t _tmp$1250;
        struct $StringView _tmp$1249;
        int64_t _bind$189;
        moonbit_incref(_x$183.$0);
        pkg$188 = $StringView$$view$inner(_x$183, 0, _tmp$1251);
        _tmp$1250
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1249
        = (struct $StringView){
          0, _tmp$1250, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$183.$0);
        _bind$189 = $StringView$$rev_find(_x$183, _tmp$1249);
        if (_bind$189 == 4294967296ll) {
          moonbit_decref(pkg$188.$0);
          moonbit_decref(_x$183.$0);
          moonbit_panic();
        } else {
          int64_t _Some$190 = _bind$189;
          int32_t _start_loc_end$191 = (int32_t)_Some$190;
          int32_t _tmp$1233 = _start_loc_end$191 + 1;
          int32_t _tmp$1234;
          moonbit_incref(_x$183.$0);
          _tmp$1234 = $StringView$$length(_x$183);
          if (_tmp$1233 < _tmp$1234) {
            int32_t _tmp$1248 = _start_loc_end$191 + 1;
            struct $StringView end_loc$192;
            struct $$3c$StringView$2a$StringView$3e$* _bind$193;
            moonbit_incref(_x$183.$0);
            end_loc$192
            = $StringView$$view$inner(
              _x$183, _tmp$1248, 4294967296ll
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
              struct $StringView _field$2372 =
                (struct $StringView){
                  _x$195->$0_1, _x$195->$0_2, _x$195->$0_0
                };
              struct $StringView _end_line$196 = _field$2372;
              struct $StringView _field$2371 =
                (struct $StringView){
                  _x$195->$1_1, _x$195->$1_2, _x$195->$1_0
                };
              int32_t _cnt$2516 = Moonbit_object_header(_x$195)->rc;
              struct $StringView _end_column$197;
              int64_t _tmp$1247;
              struct $StringView rest$198;
              int32_t _tmp$1246;
              struct $StringView _tmp$1245;
              int64_t _bind$200;
              if (_cnt$2516 > 1) {
                int32_t _new_cnt$2517;
                moonbit_incref(_field$2371.$0);
                moonbit_incref(_end_line$196.$0);
                _new_cnt$2517 = _cnt$2516 - 1;
                Moonbit_object_header(_x$195)->rc = _new_cnt$2517;
              } else if (_cnt$2516 == 1) {
                moonbit_free(_x$195);
              }
              _end_column$197 = _field$2371;
              _tmp$1247 = (int64_t)_start_loc_end$191;
              rest$198 = $StringView$$view$inner(_x$183, 0, _tmp$1247);
              _tmp$1246
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1245
              = (struct $StringView){
                0,
                  _tmp$1246,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$198.$0);
              _bind$200 = $StringView$$rev_find(rest$198, _tmp$1245);
              if (_bind$200 == 4294967296ll) {
                moonbit_decref(rest$198.$0);
                moonbit_decref(_end_column$197.$0);
                moonbit_decref(_end_line$196.$0);
                moonbit_decref(pkg$188.$0);
                goto $join$199;
              } else {
                int64_t _Some$201 = _bind$200;
                int32_t _start_line_end$202 = (int32_t)_Some$201;
                int64_t _tmp$1244 = (int64_t)_start_line_end$202;
                struct $StringView _tmp$1241;
                int32_t _tmp$1243;
                struct $StringView _tmp$1242;
                int64_t _bind$203;
                moonbit_incref(rest$198.$0);
                _tmp$1241 = $StringView$$view$inner(rest$198, 0, _tmp$1244);
                _tmp$1243
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1242
                = (struct $StringView){
                  0,
                    _tmp$1243,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$203 = $StringView$$rev_find(_tmp$1241, _tmp$1242);
                if (_bind$203 == 4294967296ll) {
                  moonbit_decref(rest$198.$0);
                  moonbit_decref(_end_column$197.$0);
                  moonbit_decref(_end_line$196.$0);
                  moonbit_decref(pkg$188.$0);
                  goto $join$199;
                } else {
                  int64_t _Some$204 = _bind$203;
                  int32_t _filename_end$205 = (int32_t)_Some$204;
                  int32_t _tmp$1235 = _filename_end$205 + 1;
                  int32_t _tmp$1236;
                  moonbit_incref(rest$198.$0);
                  _tmp$1236 = $StringView$$length(rest$198);
                  if (_tmp$1235 < _tmp$1236) {
                    int32_t _tmp$1240 = _filename_end$205 + 1;
                    struct $StringView start_loc$206;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$207;
                    moonbit_incref(rest$198.$0);
                    start_loc$206
                    = $StringView$$view$inner(
                      rest$198, _tmp$1240, 4294967296ll
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
                      struct $StringView _field$2370 =
                        (struct $StringView){
                          _x$209->$0_1, _x$209->$0_2, _x$209->$0_0
                        };
                      struct $StringView _start_line$210 = _field$2370;
                      struct $StringView _field$2369 =
                        (struct $StringView){
                          _x$209->$1_1, _x$209->$1_2, _x$209->$1_0
                        };
                      int32_t _cnt$2518 = Moonbit_object_header(_x$209)->rc;
                      struct $StringView _start_column$211;
                      int32_t _tmp$1237;
                      if (_cnt$2518 > 1) {
                        int32_t _new_cnt$2519;
                        moonbit_incref(_field$2369.$0);
                        moonbit_incref(_start_line$210.$0);
                        _new_cnt$2519 = _cnt$2518 - 1;
                        Moonbit_object_header(_x$209)->rc = _new_cnt$2519;
                      } else if (_cnt$2518 == 1) {
                        moonbit_free(_x$209);
                      }
                      _start_column$211 = _field$2369;
                      _tmp$1237 = _pkg_end$187 + 1;
                      if (_filename_end$205 > _tmp$1237) {
                        int32_t _tmp$1238 = _pkg_end$187 + 1;
                        int64_t _tmp$1239 = (int64_t)_filename_end$205;
                        struct $StringView filename$212 =
                          $StringView$$view$inner(
                            rest$198, _tmp$1238, _tmp$1239
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$2645 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$2645)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$2645->$0_0 = pkg$188.$0;
                        _block$2645->$0_1 = pkg$188.$1;
                        _block$2645->$0_2 = pkg$188.$2;
                        _block$2645->$1_0 = filename$212.$0;
                        _block$2645->$1_1 = filename$212.$1;
                        _block$2645->$1_2 = filename$212.$2;
                        _block$2645->$2_0 = _start_line$210.$0;
                        _block$2645->$2_1 = _start_line$210.$1;
                        _block$2645->$2_2 = _start_line$210.$2;
                        _block$2645->$3_0 = _start_column$211.$0;
                        _block$2645->$3_1 = _start_column$211.$1;
                        _block$2645->$3_2 = _start_column$211.$2;
                        _block$2645->$4_0 = _end_line$196.$0;
                        _block$2645->$4_1 = _end_line$196.$1;
                        _block$2645->$4_2 = _end_line$196.$2;
                        _block$2645->$5_0 = _end_column$197.$0;
                        _block$2645->$5_1 = _end_column$197.$1;
                        _block$2645->$5_2 = _end_column$197.$2;
                        return _block$2645;
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
  int32_t _tmp$1230 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1229;
  int64_t _bind$176;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1229
  = (struct $StringView){
    0, _tmp$1230, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$177.$0);
  _bind$176 = $StringView$$find(view$177, _tmp$1229);
  if (_bind$176 == 4294967296ll) {
    moonbit_decref(view$177.$0);
    return 0;
  } else {
    int64_t _Some$178 = _bind$176;
    int32_t _i$179 = (int32_t)_Some$178;
    int32_t _if_result$2646;
    if (_i$179 > 0) {
      int32_t _tmp$1222 = _i$179 + 1;
      int32_t _tmp$1223;
      moonbit_incref(view$177.$0);
      _tmp$1223 = $StringView$$length(view$177);
      _if_result$2646 = _tmp$1222 < _tmp$1223;
    } else {
      _if_result$2646 = 0;
    }
    if (_if_result$2646) {
      int64_t _tmp$1228 = (int64_t)_i$179;
      struct $StringView _tmp$1225;
      int32_t _tmp$1227;
      struct $StringView _tmp$1226;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1224;
      moonbit_incref(view$177.$0);
      _tmp$1225 = $StringView$$view$inner(view$177, 0, _tmp$1228);
      _tmp$1227 = _i$179 + 1;
      _tmp$1226 = $StringView$$view$inner(view$177, _tmp$1227, 4294967296ll);
      _tuple$1224
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1224)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1224->$0_0 = _tmp$1225.$0;
      _tuple$1224->$0_1 = _tmp$1225.$1;
      _tuple$1224->$0_2 = _tmp$1225.$2;
      _tuple$1224->$1_0 = _tmp$1226.$0;
      _tuple$1224->$1_1 = _tmp$1226.$1;
      _tuple$1224->$1_2 = _tmp$1226.$2;
      return _tuple$1224;
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
  int32_t _if_result$2647;
  if (end_offset$172 == 4294967296ll) {
    moonbit_incref(self$174.$0);
    end_offset$171 = $StringView$$length(self$174);
  } else {
    int64_t _Some$173 = end_offset$172;
    end_offset$171 = (int32_t)_Some$173;
  }
  if (start_offset$175 >= 0) {
    if (start_offset$175 <= end_offset$171) {
      int32_t _tmp$1216;
      moonbit_incref(self$174.$0);
      _tmp$1216 = $StringView$$length(self$174);
      _if_result$2647 = end_offset$171 <= _tmp$1216;
    } else {
      _if_result$2647 = 0;
    }
  } else {
    _if_result$2647 = 0;
  }
  if (_if_result$2647) {
    moonbit_string_t _field$2374 = self$174.$0;
    moonbit_string_t str$1217 = _field$2374;
    int32_t start$1221 = self$174.$1;
    int32_t _tmp$1218 = start$1221 + start_offset$175;
    int32_t _field$2373 = self$174.$1;
    int32_t start$1220 = _field$2373;
    int32_t _tmp$1219 = start$1220 + end_offset$171;
    return (struct $StringView){_tmp$1218, _tmp$1219, str$1217};
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
  int32_t _tmp$1215;
  moonbit_incref(str$169.$0);
  _tmp$1215 = $StringView$$length(str$169);
  if (_tmp$1215 <= 4) {
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
        int32_t _tmp$1202 = i$164;
        if (_tmp$1202 >= 0) {
          int32_t _tmp$1207;
          while (1) {
            int32_t _tmp$1205 = i$164;
            int32_t _if_result$2650;
            if (_tmp$1205 >= 0) {
              int32_t _tmp$1204 = i$164;
              int32_t _tmp$1203;
              moonbit_incref(haystack$160.$0);
              _tmp$1203
              = $StringView$$unsafe_charcode_at(
                haystack$160, _tmp$1204
              );
              _if_result$2650 = _tmp$1203 != needle_first$163;
            } else {
              _if_result$2650 = 0;
            }
            if (_if_result$2650) {
              int32_t _tmp$1206 = i$164;
              i$164 = _tmp$1206 - 1;
              continue;
            }
            break;
          }
          _tmp$1207 = i$164;
          if (_tmp$1207 >= 0) {
            int32_t j$166 = 1;
            int32_t _tmp$1214;
            while (1) {
              if (j$166 < needle_len$161) {
                int32_t _tmp$1211 = i$164;
                int32_t _tmp$1210 = _tmp$1211 + j$166;
                int32_t _tmp$1208;
                int32_t _tmp$1209;
                int32_t _tmp$1212;
                moonbit_incref(haystack$160.$0);
                _tmp$1208
                = $StringView$$unsafe_charcode_at(
                  haystack$160, _tmp$1210
                );
                moonbit_incref(needle$162.$0);
                _tmp$1209
                = $StringView$$unsafe_charcode_at(
                  needle$162, j$166
                );
                if (_tmp$1208 != _tmp$1209) {
                  break;
                }
                _tmp$1212 = j$166 + 1;
                j$166 = _tmp$1212;
                continue;
              } else {
                int32_t _tmp$1213;
                moonbit_decref(needle$162.$0);
                moonbit_decref(haystack$160.$0);
                _tmp$1213 = i$164;
                return (int64_t)_tmp$1213;
              }
              break;
            }
            _tmp$1214 = i$164;
            i$164 = _tmp$1214 - 1;
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
      int32_t _tmp$1192 = needle_len$150 - 1;
      int32_t i$153 = _tmp$1192;
      int32_t _tmp$1201;
      int32_t i$155;
      while (1) {
        if (i$153 > 0) {
          int32_t _tmp$1190;
          int32_t _tmp$1189;
          int32_t _tmp$1191;
          moonbit_incref(needle$151.$0);
          _tmp$1190 = $StringView$$unsafe_charcode_at(needle$151, i$153);
          _tmp$1189 = _tmp$1190 & 255;
          if (
            _tmp$1189 < 0
            || _tmp$1189 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          skip_table$152[_tmp$1189] = i$153;
          _tmp$1191 = i$153 - 1;
          i$153 = _tmp$1191;
          continue;
        }
        break;
      }
      _tmp$1201 = haystack_len$148 - needle_len$150;
      i$155 = _tmp$1201;
      while (1) {
        if (i$155 >= 0) {
          int32_t j$156 = 0;
          int32_t _tmp$1200;
          int32_t _tmp$1199;
          int32_t _tmp$1198;
          int32_t _tmp$1197;
          while (1) {
            if (j$156 < needle_len$150) {
              int32_t _tmp$1195 = i$155 + j$156;
              int32_t _tmp$1193;
              int32_t _tmp$1194;
              int32_t _tmp$1196;
              moonbit_incref(haystack$149.$0);
              _tmp$1193
              = $StringView$$unsafe_charcode_at(
                haystack$149, _tmp$1195
              );
              moonbit_incref(needle$151.$0);
              _tmp$1194 = $StringView$$unsafe_charcode_at(needle$151, j$156);
              if (_tmp$1193 != _tmp$1194) {
                break;
              }
              _tmp$1196 = j$156 + 1;
              j$156 = _tmp$1196;
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
          _tmp$1200 = $StringView$$unsafe_charcode_at(haystack$149, i$155);
          _tmp$1199 = _tmp$1200 & 255;
          if (
            _tmp$1199 < 0
            || _tmp$1199 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          _tmp$1198 = (int32_t)skip_table$152[_tmp$1199];
          _tmp$1197 = i$155 - _tmp$1198;
          i$155 = _tmp$1197;
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
  int32_t _tmp$1188;
  moonbit_incref(str$146.$0);
  _tmp$1188 = $StringView$$length(str$146);
  if (_tmp$1188 <= 4) {
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
        int32_t _tmp$1175 = i$141;
        if (_tmp$1175 <= forward_len$140) {
          int32_t _tmp$1180;
          while (1) {
            int32_t _tmp$1178 = i$141;
            int32_t _if_result$2657;
            if (_tmp$1178 <= forward_len$140) {
              int32_t _tmp$1177 = i$141;
              int32_t _tmp$1176;
              moonbit_incref(haystack$136.$0);
              _tmp$1176
              = $StringView$$unsafe_charcode_at(
                haystack$136, _tmp$1177
              );
              _if_result$2657 = _tmp$1176 != needle_first$139;
            } else {
              _if_result$2657 = 0;
            }
            if (_if_result$2657) {
              int32_t _tmp$1179 = i$141;
              i$141 = _tmp$1179 + 1;
              continue;
            }
            break;
          }
          _tmp$1180 = i$141;
          if (_tmp$1180 <= forward_len$140) {
            int32_t j$143 = 1;
            int32_t _tmp$1187;
            while (1) {
              if (j$143 < needle_len$137) {
                int32_t _tmp$1184 = i$141;
                int32_t _tmp$1183 = _tmp$1184 + j$143;
                int32_t _tmp$1181;
                int32_t _tmp$1182;
                int32_t _tmp$1185;
                moonbit_incref(haystack$136.$0);
                _tmp$1181
                = $StringView$$unsafe_charcode_at(
                  haystack$136, _tmp$1183
                );
                moonbit_incref(needle$138.$0);
                _tmp$1182
                = $StringView$$unsafe_charcode_at(
                  needle$138, j$143
                );
                if (_tmp$1181 != _tmp$1182) {
                  break;
                }
                _tmp$1185 = j$143 + 1;
                j$143 = _tmp$1185;
                continue;
              } else {
                int32_t _tmp$1186;
                moonbit_decref(needle$138.$0);
                moonbit_decref(haystack$136.$0);
                _tmp$1186 = i$141;
                return (int64_t)_tmp$1186;
              }
              break;
            }
            _tmp$1187 = i$141;
            i$141 = _tmp$1187 + 1;
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
          int32_t _tmp$1162;
          int32_t _tmp$1159;
          int32_t _tmp$1161;
          int32_t _tmp$1160;
          int32_t _tmp$1163;
          moonbit_incref(needle$124.$0);
          _tmp$1162 = $StringView$$unsafe_charcode_at(needle$124, i$127);
          _tmp$1159 = _tmp$1162 & 255;
          _tmp$1161 = needle_len$123 - 1;
          _tmp$1160 = _tmp$1161 - i$127;
          if (
            _tmp$1159 < 0
            || _tmp$1159 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          skip_table$125[_tmp$1159] = _tmp$1160;
          _tmp$1163 = i$127 + 1;
          i$127 = _tmp$1163;
          continue;
        }
        break;
      }
      i$129 = 0;
      while (1) {
        int32_t _tmp$1164 = haystack_len$121 - needle_len$123;
        if (i$129 <= _tmp$1164) {
          int32_t _end4307$130 = needle_len$123 - 1;
          int32_t j$131 = 0;
          int32_t _tmp$1174;
          int32_t _tmp$1173;
          int32_t _tmp$1172;
          int32_t _tmp$1171;
          int32_t _tmp$1170;
          int32_t _tmp$1169;
          while (1) {
            if (j$131 <= _end4307$130) {
              int32_t _tmp$1167 = i$129 + j$131;
              int32_t _tmp$1165;
              int32_t _tmp$1166;
              int32_t _tmp$1168;
              moonbit_incref(haystack$122.$0);
              _tmp$1165
              = $StringView$$unsafe_charcode_at(
                haystack$122, _tmp$1167
              );
              moonbit_incref(needle$124.$0);
              _tmp$1166 = $StringView$$unsafe_charcode_at(needle$124, j$131);
              if (_tmp$1165 != _tmp$1166) {
                break;
              }
              _tmp$1168 = j$131 + 1;
              j$131 = _tmp$1168;
              continue;
            } else {
              moonbit_decref(skip_table$125);
              moonbit_decref(needle$124.$0);
              moonbit_decref(haystack$122.$0);
              return (int64_t)i$129;
            }
            break;
          }
          _tmp$1174 = i$129 + needle_len$123;
          _tmp$1173 = _tmp$1174 - 1;
          moonbit_incref(haystack$122.$0);
          _tmp$1172
          = $StringView$$unsafe_charcode_at(
            haystack$122, _tmp$1173
          );
          _tmp$1171 = _tmp$1172 & 255;
          if (
            _tmp$1171 < 0
            || _tmp$1171 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          _tmp$1170 = (int32_t)skip_table$125[_tmp$1171];
          _tmp$1169 = i$129 + _tmp$1170;
          i$129 = _tmp$1169;
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
  moonbit_string_t _field$2377 = self$118.$0;
  moonbit_string_t str$1156 = _field$2377;
  int32_t _field$2376 = self$118.$1;
  int32_t start$1158 = _field$2376;
  int32_t _tmp$1157 = start$1158 + index$119;
  int32_t _tmp$2375 = str$1156[_tmp$1157];
  moonbit_decref(str$1156);
  return _tmp$2375;
}

int32_t $StringView$$length(struct $StringView self$117) {
  int32_t end$1154 = self$117.$2;
  int32_t _field$2378 = self$117.$1;
  int32_t start$1155;
  moonbit_decref(self$117.$0);
  start$1155 = _field$2378;
  return end$1154 - start$1155;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$115,
  int32_t index$116
) {
  int32_t len$114 = self$115->$1;
  int32_t _if_result$2662;
  if (index$116 >= 0) {
    _if_result$2662 = index$116 < len$114;
  } else {
    _if_result$2662 = 0;
  }
  if (_if_result$2662) {
    moonbit_string_t* _tmp$1153 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$115);
    moonbit_string_t _tmp$2379;
    if (index$116 < 0 || index$116 >= Moonbit_array_length(_tmp$1153)) {
      moonbit_panic();
    }
    _tmp$2379 = (moonbit_string_t)_tmp$1153[index$116];
    moonbit_incref(_tmp$2379);
    moonbit_decref(_tmp$1153);
    return _tmp$2379;
  } else {
    moonbit_decref(self$115);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$113
) {
  int32_t _field$2380 = self$113->$1;
  moonbit_decref(self$113);
  return _field$2380;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$112
) {
  int32_t _field$2381 = self$112->$1;
  moonbit_decref(self$112);
  return _field$2381;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$111
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2382 =
    self$111->$0;
  int32_t _cnt$2520 = Moonbit_object_header(self$111)->rc;
  if (_cnt$2520 > 1) {
    int32_t _new_cnt$2521;
    moonbit_incref(_field$2382);
    _new_cnt$2521 = _cnt$2520 - 1;
    Moonbit_object_header(self$111)->rc = _new_cnt$2521;
  } else if (_cnt$2520 == 1) {
    moonbit_free(self$111);
  }
  return _field$2382;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$110
) {
  moonbit_string_t* _field$2383 = self$110->$0;
  int32_t _cnt$2522 = Moonbit_object_header(self$110)->rc;
  if (_cnt$2522 > 1) {
    int32_t _new_cnt$2523;
    moonbit_incref(_field$2383);
    _new_cnt$2523 = _cnt$2522 - 1;
    Moonbit_object_header(self$110)->rc = _new_cnt$2523;
  } else if (_cnt$2522 == 1) {
    moonbit_free(self$110);
  }
  return _field$2383;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$109
) {
  struct $$3c$String$2a$Int$3e$** _field$2384 = self$109->$0;
  int32_t _cnt$2524 = Moonbit_object_header(self$109)->rc;
  if (_cnt$2524 > 1) {
    int32_t _new_cnt$2525;
    moonbit_incref(_field$2384);
    _new_cnt$2525 = _cnt$2524 - 1;
    Moonbit_object_header(self$109)->rc = _new_cnt$2525;
  } else if (_cnt$2524 == 1) {
    moonbit_free(self$109);
  }
  return _field$2384;
}

moonbit_string_t $String$$escape(moonbit_string_t self$108) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$107 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1152;
  moonbit_incref(buf$107);
  _tmp$1152
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$107
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$108, _tmp$1152);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$107);
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$105, int32_t y$106) {
  int32_t _tmp$1151 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$105, y$106
    );
  return !_tmp$1151;
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
  int32_t len$1146 = self$100->$1;
  int32_t _tmp$1145 = len$1146 + 4;
  moonbit_bytes_t _field$2385;
  moonbit_bytes_t data$1149;
  int32_t len$1150;
  int32_t inc$101;
  int32_t len$1148;
  int32_t _tmp$1147;
  moonbit_incref(self$100);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$100, _tmp$1145
  );
  _field$2385 = self$100->$0;
  data$1149 = _field$2385;
  len$1150 = self$100->$1;
  moonbit_incref(data$1149);
  inc$101 = $FixedArray$$set_utf16le_char(data$1149, len$1150, ch$102);
  len$1148 = self$100->$1;
  _tmp$1147 = len$1148 + inc$101;
  self$100->$1 = _tmp$1147;
  moonbit_decref(self$100);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$95,
  int32_t required$96
) {
  moonbit_bytes_t _field$2389 = self$95->$0;
  moonbit_bytes_t data$1144 = _field$2389;
  int32_t _tmp$2388 = Moonbit_array_length(data$1144);
  int32_t current_len$94 = _tmp$2388;
  int32_t enough_space$97;
  int32_t _tmp$1142;
  int32_t _tmp$1143;
  moonbit_bytes_t new_data$99;
  moonbit_bytes_t _field$2387;
  moonbit_bytes_t data$1140;
  int32_t len$1141;
  moonbit_bytes_t _old$2386;
  if (required$96 <= current_len$94) {
    moonbit_decref(self$95);
    return 0;
  }
  enough_space$97 = current_len$94;
  while (1) {
    int32_t _tmp$1138 = enough_space$97;
    if (_tmp$1138 < required$96) {
      int32_t _tmp$1139 = enough_space$97;
      enough_space$97 = _tmp$1139 * 2;
      continue;
    }
    break;
  }
  _tmp$1142 = enough_space$97;
  _tmp$1143 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$99 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1142, _tmp$1143);
  _field$2387 = self$95->$0;
  data$1140 = _field$2387;
  len$1141 = self$95->$1;
  moonbit_incref(data$1140);
  moonbit_incref(new_data$99);
  $FixedArray$$unsafe_blit$0(new_data$99, 0, data$1140, 0, len$1141);
  _old$2386 = self$95->$0;
  moonbit_decref(_old$2386);
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
    uint32_t _tmp$1121 = code$87 & 255u;
    int32_t _tmp$1120 = $UInt$$to_byte(_tmp$1121);
    int32_t _tmp$1122;
    uint32_t _tmp$1124;
    int32_t _tmp$1123;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1120;
    _tmp$1122 = offset$90 + 1;
    _tmp$1124 = code$87 >> 8;
    _tmp$1123 = $UInt$$to_byte(_tmp$1124);
    if (_tmp$1122 < 0 || _tmp$1122 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1122] = _tmp$1123;
    moonbit_decref(self$89);
    return 2;
  } else if (code$87 < 1114112u) {
    uint32_t hi$91 = code$87 - 65536u;
    uint32_t _tmp$1137 = hi$91 >> 10;
    uint32_t lo$92 = _tmp$1137 | 55296u;
    uint32_t _tmp$1136 = hi$91 & 1023u;
    uint32_t hi$93 = _tmp$1136 | 56320u;
    uint32_t _tmp$1126 = lo$92 & 255u;
    int32_t _tmp$1125 = $UInt$$to_byte(_tmp$1126);
    int32_t _tmp$1127;
    uint32_t _tmp$1129;
    int32_t _tmp$1128;
    int32_t _tmp$1130;
    uint32_t _tmp$1132;
    int32_t _tmp$1131;
    int32_t _tmp$1133;
    uint32_t _tmp$1135;
    int32_t _tmp$1134;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1125;
    _tmp$1127 = offset$90 + 1;
    _tmp$1129 = lo$92 >> 8;
    _tmp$1128 = $UInt$$to_byte(_tmp$1129);
    if (_tmp$1127 < 0 || _tmp$1127 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1127] = _tmp$1128;
    _tmp$1130 = offset$90 + 2;
    _tmp$1132 = hi$93 & 255u;
    _tmp$1131 = $UInt$$to_byte(_tmp$1132);
    if (_tmp$1130 < 0 || _tmp$1130 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1130] = _tmp$1131;
    _tmp$1133 = offset$90 + 3;
    _tmp$1135 = hi$93 >> 8;
    _tmp$1134 = $UInt$$to_byte(_tmp$1135);
    if (_tmp$1133 < 0 || _tmp$1133 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1133] = _tmp$1134;
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
  int32_t _tmp$1119 = *(int32_t*)&self$86;
  return _tmp$1119 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$85) {
  int32_t _tmp$1118 = self$85;
  return *(uint32_t*)&_tmp$1118;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$84
) {
  moonbit_bytes_t _field$2391 = self$84->$0;
  moonbit_bytes_t data$1117 = _field$2391;
  moonbit_bytes_t _tmp$1114;
  int32_t _field$2390;
  int32_t len$1116;
  int64_t _tmp$1115;
  moonbit_incref(data$1117);
  _tmp$1114 = data$1117;
  _field$2390 = self$84->$1;
  moonbit_decref(self$84);
  len$1116 = _field$2390;
  _tmp$1115 = (int64_t)len$1116;
  return $Bytes$$to_unchecked_string$inner(_tmp$1114, 0, _tmp$1115);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$79,
  int32_t offset$83,
  int64_t length$81
) {
  int32_t len$78 = Moonbit_array_length(self$79);
  int32_t length$80;
  int32_t _if_result$2664;
  if (length$81 == 4294967296ll) {
    length$80 = len$78 - offset$83;
  } else {
    int64_t _Some$82 = length$81;
    length$80 = (int32_t)_Some$82;
  }
  if (offset$83 >= 0) {
    if (length$80 >= 0) {
      int32_t _tmp$1113 = offset$83 + length$80;
      _if_result$2664 = _tmp$1113 <= len$78;
    } else {
      _if_result$2664 = 0;
    }
  } else {
    _if_result$2664 = 0;
  }
  if (_if_result$2664) {
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
  struct $$moonbitlang$core$builtin$StringBuilder* _block$2665;
  if (size_hint$76 < 1) {
    initial$75 = 1;
  } else {
    initial$75 = size_hint$76;
  }
  data$77 = (moonbit_bytes_t)moonbit_make_bytes(initial$75, 0);
  _block$2665
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$2665)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$2665->$0 = data$77;
  _block$2665->$1 = 0;
  return _block$2665;
}

int32_t $Byte$$to_char(int32_t self$74) {
  int32_t _tmp$1112 = (int32_t)self$74;
  return _tmp$1112;
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
  int32_t _if_result$2666;
  if (dst$50 == src$51) {
    _if_result$2666 = dst_offset$52 < src_offset$53;
  } else {
    _if_result$2666 = 0;
  }
  if (_if_result$2666) {
    int32_t i$54 = 0;
    while (1) {
      if (i$54 < len$55) {
        int32_t _tmp$1103 = dst_offset$52 + i$54;
        int32_t _tmp$1105 = src_offset$53 + i$54;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2393;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1104;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2392;
        int32_t _tmp$1106;
        if (_tmp$1105 < 0 || _tmp$1105 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2393
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1105
          ];
        _tmp$1104 = _tmp$2393;
        if (_tmp$1103 < 0 || _tmp$1103 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2392
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1103
          ];
        if (_tmp$1104) {
          moonbit_incref(_tmp$1104);
        }
        if (_old$2392) {
          moonbit_decref(_old$2392);
        }
        dst$50[_tmp$1103] = _tmp$1104;
        _tmp$1106 = i$54 + 1;
        i$54 = _tmp$1106;
        continue;
      } else {
        moonbit_decref(src$51);
        moonbit_decref(dst$50);
      }
      break;
    }
  } else {
    int32_t _tmp$1111 = len$55 - 1;
    int32_t i$57 = _tmp$1111;
    while (1) {
      if (i$57 >= 0) {
        int32_t _tmp$1107 = dst_offset$52 + i$57;
        int32_t _tmp$1109 = src_offset$53 + i$57;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2395;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1108;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2394;
        int32_t _tmp$1110;
        if (_tmp$1109 < 0 || _tmp$1109 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2395
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1109
          ];
        _tmp$1108 = _tmp$2395;
        if (_tmp$1107 < 0 || _tmp$1107 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2394
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1107
          ];
        if (_tmp$1108) {
          moonbit_incref(_tmp$1108);
        }
        if (_old$2394) {
          moonbit_decref(_old$2394);
        }
        dst$50[_tmp$1107] = _tmp$1108;
        _tmp$1110 = i$57 - 1;
        i$57 = _tmp$1110;
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
  int32_t _if_result$2669;
  if (dst$41 == src$42) {
    _if_result$2669 = dst_offset$43 < src_offset$44;
  } else {
    _if_result$2669 = 0;
  }
  if (_if_result$2669) {
    int32_t i$45 = 0;
    while (1) {
      if (i$45 < len$46) {
        int32_t _tmp$1094 = dst_offset$43 + i$45;
        int32_t _tmp$1096 = src_offset$44 + i$45;
        struct $$3c$String$2a$Int$3e$* _tmp$2397;
        struct $$3c$String$2a$Int$3e$* _tmp$1095;
        struct $$3c$String$2a$Int$3e$* _old$2396;
        int32_t _tmp$1097;
        if (_tmp$1096 < 0 || _tmp$1096 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2397 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1096];
        _tmp$1095 = _tmp$2397;
        if (_tmp$1094 < 0 || _tmp$1094 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2396 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1094];
        if (_tmp$1095) {
          moonbit_incref(_tmp$1095);
        }
        if (_old$2396) {
          moonbit_decref(_old$2396);
        }
        dst$41[_tmp$1094] = _tmp$1095;
        _tmp$1097 = i$45 + 1;
        i$45 = _tmp$1097;
        continue;
      } else {
        moonbit_decref(src$42);
        moonbit_decref(dst$41);
      }
      break;
    }
  } else {
    int32_t _tmp$1102 = len$46 - 1;
    int32_t i$48 = _tmp$1102;
    while (1) {
      if (i$48 >= 0) {
        int32_t _tmp$1098 = dst_offset$43 + i$48;
        int32_t _tmp$1100 = src_offset$44 + i$48;
        struct $$3c$String$2a$Int$3e$* _tmp$2399;
        struct $$3c$String$2a$Int$3e$* _tmp$1099;
        struct $$3c$String$2a$Int$3e$* _old$2398;
        int32_t _tmp$1101;
        if (_tmp$1100 < 0 || _tmp$1100 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2399 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1100];
        _tmp$1099 = _tmp$2399;
        if (_tmp$1098 < 0 || _tmp$1098 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2398 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1098];
        if (_tmp$1099) {
          moonbit_incref(_tmp$1099);
        }
        if (_old$2398) {
          moonbit_decref(_old$2398);
        }
        dst$41[_tmp$1098] = _tmp$1099;
        _tmp$1101 = i$48 - 1;
        i$48 = _tmp$1101;
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
  int32_t _if_result$2672;
  if (dst$32 == src$33) {
    _if_result$2672 = dst_offset$34 < src_offset$35;
  } else {
    _if_result$2672 = 0;
  }
  if (_if_result$2672) {
    int32_t i$36 = 0;
    while (1) {
      if (i$36 < len$37) {
        int32_t _tmp$1085 = dst_offset$34 + i$36;
        int32_t _tmp$1087 = src_offset$35 + i$36;
        moonbit_string_t _tmp$2401;
        moonbit_string_t _tmp$1086;
        moonbit_string_t _old$2400;
        int32_t _tmp$1088;
        if (_tmp$1087 < 0 || _tmp$1087 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2401 = (moonbit_string_t)src$33[_tmp$1087];
        _tmp$1086 = _tmp$2401;
        if (_tmp$1085 < 0 || _tmp$1085 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2400 = (moonbit_string_t)dst$32[_tmp$1085];
        moonbit_incref(_tmp$1086);
        moonbit_decref(_old$2400);
        dst$32[_tmp$1085] = _tmp$1086;
        _tmp$1088 = i$36 + 1;
        i$36 = _tmp$1088;
        continue;
      } else {
        moonbit_decref(src$33);
        moonbit_decref(dst$32);
      }
      break;
    }
  } else {
    int32_t _tmp$1093 = len$37 - 1;
    int32_t i$39 = _tmp$1093;
    while (1) {
      if (i$39 >= 0) {
        int32_t _tmp$1089 = dst_offset$34 + i$39;
        int32_t _tmp$1091 = src_offset$35 + i$39;
        moonbit_string_t _tmp$2403;
        moonbit_string_t _tmp$1090;
        moonbit_string_t _old$2402;
        int32_t _tmp$1092;
        if (_tmp$1091 < 0 || _tmp$1091 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2403 = (moonbit_string_t)src$33[_tmp$1091];
        _tmp$1090 = _tmp$2403;
        if (_tmp$1089 < 0 || _tmp$1089 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2402 = (moonbit_string_t)dst$32[_tmp$1089];
        moonbit_incref(_tmp$1090);
        moonbit_decref(_old$2402);
        dst$32[_tmp$1089] = _tmp$1090;
        _tmp$1092 = i$39 - 1;
        i$39 = _tmp$1092;
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
  int32_t _if_result$2675;
  if (dst$23 == src$24) {
    _if_result$2675 = dst_offset$25 < src_offset$26;
  } else {
    _if_result$2675 = 0;
  }
  if (_if_result$2675) {
    int32_t i$27 = 0;
    while (1) {
      if (i$27 < len$28) {
        int32_t _tmp$1076 = dst_offset$25 + i$27;
        int32_t _tmp$1078 = src_offset$26 + i$27;
        int32_t _tmp$1077;
        int32_t _tmp$1079;
        if (_tmp$1078 < 0 || _tmp$1078 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1077 = (int32_t)src$24[_tmp$1078];
        if (_tmp$1076 < 0 || _tmp$1076 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1076] = _tmp$1077;
        _tmp$1079 = i$27 + 1;
        i$27 = _tmp$1079;
        continue;
      } else {
        moonbit_decref(src$24);
        moonbit_decref(dst$23);
      }
      break;
    }
  } else {
    int32_t _tmp$1084 = len$28 - 1;
    int32_t i$30 = _tmp$1084;
    while (1) {
      if (i$30 >= 0) {
        int32_t _tmp$1080 = dst_offset$25 + i$30;
        int32_t _tmp$1082 = src_offset$26 + i$30;
        int32_t _tmp$1081;
        int32_t _tmp$1083;
        if (_tmp$1082 < 0 || _tmp$1082 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1081 = (int32_t)src$24[_tmp$1082];
        if (_tmp$1080 < 0 || _tmp$1080 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1080] = _tmp$1081;
        _tmp$1083 = i$30 - 1;
        i$30 = _tmp$1083;
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
  moonbit_string_t _tmp$1075 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$1073 =
    moonbit_add_string(
      _tmp$1075, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1074 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$1072 = moonbit_add_string(_tmp$1073, _tmp$1074);
  moonbit_string_t _tmp$1071 =
    moonbit_add_string(
      _tmp$1072, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1071);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$1070 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$1068 =
    moonbit_add_string(
      _tmp$1070, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1069 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$1067 = moonbit_add_string(_tmp$1068, _tmp$1069);
  moonbit_string_t _tmp$1066 =
    moonbit_add_string(
      _tmp$1067, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1066);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
) {
  moonbit_string_t _tmp$1065 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$17);
  moonbit_string_t _tmp$1063 =
    moonbit_add_string(
      _tmp$1065, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1064 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$18);
  moonbit_string_t _tmp$1062 = moonbit_add_string(_tmp$1063, _tmp$1064);
  moonbit_string_t _tmp$1061 =
    moonbit_add_string(
      _tmp$1062, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1061);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
) {
  moonbit_string_t _tmp$1060 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$15);
  moonbit_string_t _tmp$1058 =
    moonbit_add_string(
      _tmp$1060, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1059 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$16);
  moonbit_string_t _tmp$1057 = moonbit_add_string(_tmp$1058, _tmp$1059);
  moonbit_string_t _tmp$1056 =
    moonbit_add_string(
      _tmp$1057, (moonbit_string_t)moonbit_string_literal_32.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1056);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$11,
  struct $$moonbitlang$core$builtin$Logger _x_5272$14
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$12 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$11;
  moonbit_string_t _field$2404 = _Failure$12->$0;
  int32_t _cnt$2526 = Moonbit_object_header(_Failure$12)->rc;
  moonbit_string_t _$2a$err_payload_5273$13;
  struct $$moonbitlang$core$builtin$Logger _bind$1055;
  if (_cnt$2526 > 1) {
    int32_t _new_cnt$2527;
    moonbit_incref(_field$2404);
    _new_cnt$2527 = _cnt$2526 - 1;
    Moonbit_object_header(_Failure$12)->rc = _new_cnt$2527;
  } else if (_cnt$2526 == 1) {
    moonbit_free(_Failure$12);
  }
  _$2a$err_payload_5273$13 = _field$2404;
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
  _bind$1055 = _x_5272$14;
  _bind$1055.$0->$method_0(
    _bind$1055.$1, (moonbit_string_t)moonbit_string_literal_34.data
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

moonbit_string_t $Error$to_string(void* _e$1014) {
  switch (Moonbit_object_tag(_e$1014)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1014
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1014
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1014);
      return (moonbit_string_t)moonbit_string_literal_37.data;
      break;
    }
    
    case 5: {
      moonbit_decref(_e$1014);
      return (moonbit_string_t)moonbit_string_literal_38.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
               _e$1014
             );
      break;
    }
    default: {
      moonbit_decref(_e$1014);
      return (moonbit_string_t)moonbit_string_literal_39.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1032,
  int32_t _param$1031
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1030 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1032;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1030, _param$1031
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1029,
  struct $StringView _param$1028
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1027 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1029;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1027, _param$1028
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1026,
  moonbit_string_t _param$1023,
  int32_t _param$1024,
  int32_t _param$1025
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1022 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1026;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1022, _param$1023, _param$1024, _param$1025
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1021,
  moonbit_string_t _param$1020
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1019 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1021;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1019, _param$1020
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$839;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1038;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1037;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$840;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1040;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1039;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$836;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1054;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1053;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1052;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1043;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$837;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1051;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1050;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1049;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1044;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$838;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1048;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1047;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1046;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1045;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$835;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1042;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1041;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$120 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$134 = (int64_t)0;
  _bind$839
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1038 = _bind$839;
  _tmp$1037
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1038
  };
  $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1037
  );
  _bind$840
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1040 = _bind$840;
  _tmp$1039
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1040
  };
  $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1039
  );
  _bind$836
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1054 = _bind$836;
  _tmp$1053
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1054
  };
  _tmp$1052 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1053);
  _tuple$1043
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1043)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1043->$0 = (moonbit_string_t)moonbit_string_literal_40.data;
  _tuple$1043->$1 = _tmp$1052;
  _bind$837
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1051 = _bind$837;
  _tmp$1050
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1051
  };
  _tmp$1049 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1050);
  _tuple$1044
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1044)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1044->$0 = (moonbit_string_t)moonbit_string_literal_41.data;
  _tuple$1044->$1 = _tmp$1049;
  _bind$838
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1048 = _bind$838;
  _tmp$1047
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1048
  };
  _tmp$1046 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1047);
  _tuple$1045
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1045)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1045->$0 = (moonbit_string_t)moonbit_string_literal_42.data;
  _tuple$1045->$1 = _tmp$1046;
  _bind$835
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      3
    );
  _bind$835[0] = _tuple$1043;
  _bind$835[1] = _tuple$1044;
  _bind$835[2] = _tuple$1045;
  _tmp$1042 = _bind$835;
  _tmp$1041
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 3, _tmp$1042
  };
  $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1041
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1036;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1008;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1009;
  int32_t _len$1010;
  int32_t _i$1011;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1036
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1008
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1008)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1008->$0 = _tmp$1036;
  async_tests$1008->$1 = 0;
  _arr$1009
  = $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1009);
  _len$1010 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1009);
  _i$1011 = 0;
  while (1) {
    if (_i$1011 < _len$1010) {
      struct $$3c$String$2a$Int$3e$* arg$1012;
      moonbit_string_t _field$2406;
      moonbit_string_t _tmp$1033;
      int32_t _field$2405;
      int32_t _cnt$2528;
      int32_t _tmp$1034;
      int32_t _tmp$1035;
      moonbit_incref(_arr$1009);
      arg$1012
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1009, _i$1011
      );
      _field$2406 = arg$1012->$0;
      _tmp$1033 = _field$2406;
      _field$2405 = arg$1012->$1;
      _cnt$2528 = Moonbit_object_header(arg$1012)->rc;
      if (_cnt$2528 > 1) {
        int32_t _new_cnt$2529;
        moonbit_incref(_tmp$1033);
        _new_cnt$2529 = _cnt$2528 - 1;
        Moonbit_object_header(arg$1012)->rc = _new_cnt$2529;
      } else if (_cnt$2528 == 1) {
        moonbit_free(arg$1012);
      }
      _tmp$1034 = _field$2405;
      moonbit_incref(async_tests$1008);
      $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_do_execute(
        async_tests$1008, _tmp$1033, _tmp$1034
      );
      _tmp$1035 = _i$1011 + 1;
      _i$1011 = _tmp$1035;
      continue;
    } else {
      moonbit_decref(_arr$1009);
    }
    break;
  }
  $azimuth$telemetry$azimuth_test_blackbox_test$moonbit_test_driver_internal_run_async_tests(
    async_tests$1008
  );
  return 0;
}