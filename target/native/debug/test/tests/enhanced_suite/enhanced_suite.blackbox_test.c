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

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1;

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $Ref$3c$Int$3e$;

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155;

struct $StringView;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

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

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap;

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$3c$StringView$2a$StringView$3e$;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $Moonbit_Test_Driver_Internal__F$F2;

struct $$moonbitlang$core$builtin$Hasher;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $Option$3c$StringView$3e$$Some;

struct $Error$azimuth$telemetry$tests$enhanced_suite_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

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

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
};

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155 {
  void* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ {
  int32_t $1;
  int32_t $2;
  moonbit_string_t* $0;
  
};

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  moonbit_string_t $1;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
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

struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $Option$3c$StringView$3e$$Some {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $Error$azimuth$telemetry$tests$enhanced_suite_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
  moonbit_string_t $0;
  
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

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1005
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2084,
  moonbit_string_t s$983,
  int32_t sep$984
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$970
);

moonbit_string_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$1993,
  moonbit_bytes_t bytes$971
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$1986,
  moonbit_string_t s$965
);

#define $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$928,
  moonbit_string_t filename$889,
  int32_t index$890
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1979
);

struct moonbit_result_0 $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1975
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1973
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1957,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$929,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$930
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1968,
  int32_t _cont_param$949
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1965,
  void* _cont_param$950
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1959,
  void* _state$932
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1954,
  void* err$912
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1940,
  moonbit_string_t attr$905
);

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1924,
  moonbit_string_t test_name$892,
  moonbit_string_t file_name$893,
  moonbit_string_t message$894,
  int32_t skipped$895
);

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$887
);

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$885,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$886,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$883
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$846,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$859,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$872,
  moonbit_string_t file_filter$843,
  int32_t index_filter$844
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
  struct $$3c$String$3e$$3d$$3e$Int* _env$1880,
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1533
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
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1462,
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

moonbit_string_t $Error$to_string(void* _e$1012);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1030,
  int32_t _param$1029
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1027,
  struct $StringView _param$1026
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1024,
  moonbit_string_t _param$1021,
  int32_t _param$1022,
  int32_t _param$1023
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1019,
  moonbit_string_t _param$1018
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

struct { int32_t rc; uint32_t meta; uint16_t const data[24]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 23), 
    97, 122, 105, 109, 117, 116, 104, 95, 98, 97, 115, 105, 99, 95, 116, 
    101, 115, 116, 115, 46, 109, 98, 116, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[119]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 118), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 101, 110, 104, 97, 
    110, 99, 101, 100, 95, 115, 117, 105, 116, 101, 95, 98, 108, 97, 
    99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 46, 77, 111, 111, 
    110, 66, 105, 116, 84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 
    73, 110, 116, 101, 114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 
    114, 46, 77, 111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 68, 
    114, 105, 118, 101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 74, 
    115, 69, 114, 114, 111, 114, 0
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
} const $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5
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

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_async_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_with_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_no_args_tests;

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1005
) {
  moonbit_decref(_tests$1005);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$964 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$970 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$977 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$970;
  int32_t moonbit_test_driver_internal_split_mbt_string$982 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$2109 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$989 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$990;
  moonbit_string_t _tmp$2108;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$991;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$992;
  int32_t _len$993;
  int32_t _i$994;
  Moonbit_object_header(file_and_index$989)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$989->$0 = _tmp$2109;
  file_and_index$989->$1 = 0;
  cli_args$990
  = $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$977
  );
  _tmp$2108 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$990, 1);
  test_args$991
  = $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$982, _tmp$2108, 47
  );
  _arr$992 = test_args$991;
  moonbit_incref(_arr$992);
  _len$993 = $$moonbitlang$core$builtin$Array$$length$1(_arr$992);
  _i$994 = 0;
  while (1) {
    if (_i$994 < _len$993) {
      moonbit_string_t arg$995;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$996;
      moonbit_string_t file$997;
      moonbit_string_t range$998;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$999;
      moonbit_string_t _tmp$2106;
      int32_t start$1000;
      moonbit_string_t _tmp$2105;
      int32_t end$1001;
      int32_t i$1002;
      int32_t _tmp$2107;
      moonbit_incref(_arr$992);
      arg$995
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$992, _i$994
      );
      file_and_range$996
      = $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$982, arg$995, 58
      );
      moonbit_incref(file_and_range$996);
      file$997
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$996, 0
      );
      range$998
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$996, 1
      );
      start_and_end$999
      = $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$982, range$998, 45
      );
      moonbit_incref(start_and_end$999);
      _tmp$2106
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$999, 0
      );
      start$1000
      = $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$964, _tmp$2106
      );
      _tmp$2105
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$999, 1
      );
      end$1001
      = $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$964, _tmp$2105
      );
      i$1002 = start$1000;
      while (1) {
        if (i$1002 < end$1001) {
          struct $$3c$String$2a$Int$3e$* _tuple$2103;
          int32_t _tmp$2104;
          moonbit_incref(file$997);
          _tuple$2103
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$2103)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$2103->$0 = file$997;
          _tuple$2103->$1 = i$1002;
          moonbit_incref(file_and_index$989);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$989, _tuple$2103
          );
          _tmp$2104 = i$1002 + 1;
          i$1002 = _tmp$2104;
          continue;
        } else {
          moonbit_decref(file$997);
        }
        break;
      }
      _tmp$2107 = _i$994 + 1;
      _i$994 = _tmp$2107;
      continue;
    } else {
      moonbit_decref(_arr$992);
    }
    break;
  }
  return file_and_index$989;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2084,
  moonbit_string_t s$983,
  int32_t sep$984
) {
  moonbit_string_t* _tmp$2102 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$985 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$986;
  struct $Ref$3c$Int$3e$* start$987;
  int32_t val$2097;
  int32_t _tmp$2098;
  Moonbit_object_header(res$985)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$985->$0 = _tmp$2102;
  res$985->$1 = 0;
  i$986
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$986)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$986->$0 = 0;
  start$987
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$987)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$987->$0 = 0;
  while (1) {
    int32_t val$2085 = i$986->$0;
    int32_t _tmp$2086 = Moonbit_array_length(s$983);
    if (val$2085 < _tmp$2086) {
      int32_t val$2089 = i$986->$0;
      int32_t _tmp$2088;
      int32_t _tmp$2087;
      int32_t val$2096;
      int32_t _tmp$2095;
      if (val$2089 < 0 || val$2089 >= Moonbit_array_length(s$983)) {
        moonbit_panic();
      }
      _tmp$2088 = s$983[val$2089];
      _tmp$2087 = _tmp$2088;
      if (_tmp$2087 == sep$984) {
        int32_t val$2091 = start$987->$0;
        int32_t val$2092 = i$986->$0;
        moonbit_string_t _tmp$2090;
        int32_t val$2094;
        int32_t _tmp$2093;
        moonbit_incref(s$983);
        _tmp$2090 = $String$$unsafe_substring(s$983, val$2091, val$2092);
        moonbit_incref(res$985);
        $$moonbitlang$core$builtin$Array$$push$0(res$985, _tmp$2090);
        val$2094 = i$986->$0;
        _tmp$2093 = val$2094 + 1;
        start$987->$0 = _tmp$2093;
      }
      val$2096 = i$986->$0;
      _tmp$2095 = val$2096 + 1;
      i$986->$0 = _tmp$2095;
      continue;
    } else {
      moonbit_decref(i$986);
    }
    break;
  }
  val$2097 = start$987->$0;
  _tmp$2098 = Moonbit_array_length(s$983);
  if (val$2097 < _tmp$2098) {
    int32_t _field$2110 = start$987->$0;
    int32_t val$2100;
    int32_t _tmp$2101;
    moonbit_string_t _tmp$2099;
    moonbit_decref(start$987);
    val$2100 = _field$2110;
    _tmp$2101 = Moonbit_array_length(s$983);
    _tmp$2099 = $String$$unsafe_substring(s$983, val$2100, _tmp$2101);
    moonbit_incref(res$985);
    $$moonbitlang$core$builtin$Array$$push$0(res$985, _tmp$2099);
  } else {
    moonbit_decref(start$987);
    moonbit_decref(s$983);
  }
  return res$985;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$970
) {
  moonbit_bytes_t* tmp$978 =
    $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$2083 = Moonbit_array_length(tmp$978);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$979 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$2083);
  int32_t i$980 = 0;
  while (1) {
    int32_t _tmp$2079 = Moonbit_array_length(tmp$978);
    if (i$980 < _tmp$2079) {
      moonbit_bytes_t _tmp$2111;
      moonbit_bytes_t _tmp$2081;
      moonbit_string_t _tmp$2080;
      int32_t _tmp$2082;
      if (i$980 < 0 || i$980 >= Moonbit_array_length(tmp$978)) {
        moonbit_panic();
      }
      _tmp$2111 = (moonbit_bytes_t)tmp$978[i$980];
      _tmp$2081 = _tmp$2111;
      moonbit_incref(_tmp$2081);
      _tmp$2080
      = $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$970, _tmp$2081
      );
      moonbit_incref(res$979);
      $$moonbitlang$core$builtin$Array$$push$0(res$979, _tmp$2080);
      _tmp$2082 = i$980 + 1;
      i$980 = _tmp$2082;
      continue;
    } else {
      moonbit_decref(tmp$978);
    }
    break;
  }
  return res$979;
}

moonbit_string_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$1993,
  moonbit_bytes_t bytes$971
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$972 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$973 = Moonbit_array_length(bytes$971);
  struct $Ref$3c$Int$3e$* i$974 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$974)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$974->$0 = 0;
  while (1) {
    int32_t val$1994 = i$974->$0;
    if (val$1994 < len$973) {
      int32_t val$2078 = i$974->$0;
      int32_t _tmp$2077;
      int32_t _tmp$2076;
      struct $Ref$3c$Int$3e$* c$975;
      int32_t val$1995;
      if (val$2078 < 0 || val$2078 >= Moonbit_array_length(bytes$971)) {
        moonbit_panic();
      }
      _tmp$2077 = bytes$971[val$2078];
      _tmp$2076 = (int32_t)_tmp$2077;
      c$975
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$975)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$975->$0 = _tmp$2076;
      val$1995 = c$975->$0;
      if (val$1995 < 128) {
        int32_t _field$2112 = c$975->$0;
        int32_t val$1997;
        int32_t _tmp$1996;
        int32_t val$1999;
        int32_t _tmp$1998;
        moonbit_decref(c$975);
        val$1997 = _field$2112;
        _tmp$1996 = val$1997;
        moonbit_incref(res$972);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$972, _tmp$1996
        );
        val$1999 = i$974->$0;
        _tmp$1998 = val$1999 + 1;
        i$974->$0 = _tmp$1998;
      } else {
        int32_t val$2000 = c$975->$0;
        if (val$2000 < 224) {
          int32_t val$2002 = i$974->$0;
          int32_t _tmp$2001 = val$2002 + 1;
          int32_t val$2011;
          int32_t _tmp$2010;
          int32_t _tmp$2004;
          int32_t val$2009;
          int32_t _tmp$2008;
          int32_t _tmp$2007;
          int32_t _tmp$2006;
          int32_t _tmp$2005;
          int32_t _tmp$2003;
          int32_t _field$2113;
          int32_t val$2013;
          int32_t _tmp$2012;
          int32_t val$2015;
          int32_t _tmp$2014;
          if (_tmp$2001 >= len$973) {
            moonbit_decref(c$975);
            moonbit_decref(i$974);
            moonbit_decref(bytes$971);
            break;
          }
          val$2011 = c$975->$0;
          _tmp$2010 = val$2011 & 31;
          _tmp$2004 = _tmp$2010 << 6;
          val$2009 = i$974->$0;
          _tmp$2008 = val$2009 + 1;
          if (_tmp$2008 < 0 || _tmp$2008 >= Moonbit_array_length(bytes$971)) {
            moonbit_panic();
          }
          _tmp$2007 = bytes$971[_tmp$2008];
          _tmp$2006 = (int32_t)_tmp$2007;
          _tmp$2005 = _tmp$2006 & 63;
          _tmp$2003 = _tmp$2004 | _tmp$2005;
          c$975->$0 = _tmp$2003;
          _field$2113 = c$975->$0;
          moonbit_decref(c$975);
          val$2013 = _field$2113;
          _tmp$2012 = val$2013;
          moonbit_incref(res$972);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$972, _tmp$2012
          );
          val$2015 = i$974->$0;
          _tmp$2014 = val$2015 + 2;
          i$974->$0 = _tmp$2014;
        } else {
          int32_t val$2016 = c$975->$0;
          if (val$2016 < 240) {
            int32_t val$2018 = i$974->$0;
            int32_t _tmp$2017 = val$2018 + 2;
            int32_t val$2034;
            int32_t _tmp$2033;
            int32_t _tmp$2026;
            int32_t val$2032;
            int32_t _tmp$2031;
            int32_t _tmp$2030;
            int32_t _tmp$2029;
            int32_t _tmp$2028;
            int32_t _tmp$2027;
            int32_t _tmp$2020;
            int32_t val$2025;
            int32_t _tmp$2024;
            int32_t _tmp$2023;
            int32_t _tmp$2022;
            int32_t _tmp$2021;
            int32_t _tmp$2019;
            int32_t _field$2114;
            int32_t val$2036;
            int32_t _tmp$2035;
            int32_t val$2038;
            int32_t _tmp$2037;
            if (_tmp$2017 >= len$973) {
              moonbit_decref(c$975);
              moonbit_decref(i$974);
              moonbit_decref(bytes$971);
              break;
            }
            val$2034 = c$975->$0;
            _tmp$2033 = val$2034 & 15;
            _tmp$2026 = _tmp$2033 << 12;
            val$2032 = i$974->$0;
            _tmp$2031 = val$2032 + 1;
            if (
              _tmp$2031 < 0 || _tmp$2031 >= Moonbit_array_length(bytes$971)
            ) {
              moonbit_panic();
            }
            _tmp$2030 = bytes$971[_tmp$2031];
            _tmp$2029 = (int32_t)_tmp$2030;
            _tmp$2028 = _tmp$2029 & 63;
            _tmp$2027 = _tmp$2028 << 6;
            _tmp$2020 = _tmp$2026 | _tmp$2027;
            val$2025 = i$974->$0;
            _tmp$2024 = val$2025 + 2;
            if (
              _tmp$2024 < 0 || _tmp$2024 >= Moonbit_array_length(bytes$971)
            ) {
              moonbit_panic();
            }
            _tmp$2023 = bytes$971[_tmp$2024];
            _tmp$2022 = (int32_t)_tmp$2023;
            _tmp$2021 = _tmp$2022 & 63;
            _tmp$2019 = _tmp$2020 | _tmp$2021;
            c$975->$0 = _tmp$2019;
            _field$2114 = c$975->$0;
            moonbit_decref(c$975);
            val$2036 = _field$2114;
            _tmp$2035 = val$2036;
            moonbit_incref(res$972);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$972, _tmp$2035
            );
            val$2038 = i$974->$0;
            _tmp$2037 = val$2038 + 3;
            i$974->$0 = _tmp$2037;
          } else {
            int32_t val$2040 = i$974->$0;
            int32_t _tmp$2039 = val$2040 + 3;
            int32_t val$2063;
            int32_t _tmp$2062;
            int32_t _tmp$2055;
            int32_t val$2061;
            int32_t _tmp$2060;
            int32_t _tmp$2059;
            int32_t _tmp$2058;
            int32_t _tmp$2057;
            int32_t _tmp$2056;
            int32_t _tmp$2048;
            int32_t val$2054;
            int32_t _tmp$2053;
            int32_t _tmp$2052;
            int32_t _tmp$2051;
            int32_t _tmp$2050;
            int32_t _tmp$2049;
            int32_t _tmp$2042;
            int32_t val$2047;
            int32_t _tmp$2046;
            int32_t _tmp$2045;
            int32_t _tmp$2044;
            int32_t _tmp$2043;
            int32_t _tmp$2041;
            int32_t val$2065;
            int32_t _tmp$2064;
            int32_t val$2069;
            int32_t _tmp$2068;
            int32_t _tmp$2067;
            int32_t _tmp$2066;
            int32_t _field$2115;
            int32_t val$2073;
            int32_t _tmp$2072;
            int32_t _tmp$2071;
            int32_t _tmp$2070;
            int32_t val$2075;
            int32_t _tmp$2074;
            if (_tmp$2039 >= len$973) {
              moonbit_decref(c$975);
              moonbit_decref(i$974);
              moonbit_decref(bytes$971);
              break;
            }
            val$2063 = c$975->$0;
            _tmp$2062 = val$2063 & 7;
            _tmp$2055 = _tmp$2062 << 18;
            val$2061 = i$974->$0;
            _tmp$2060 = val$2061 + 1;
            if (
              _tmp$2060 < 0 || _tmp$2060 >= Moonbit_array_length(bytes$971)
            ) {
              moonbit_panic();
            }
            _tmp$2059 = bytes$971[_tmp$2060];
            _tmp$2058 = (int32_t)_tmp$2059;
            _tmp$2057 = _tmp$2058 & 63;
            _tmp$2056 = _tmp$2057 << 12;
            _tmp$2048 = _tmp$2055 | _tmp$2056;
            val$2054 = i$974->$0;
            _tmp$2053 = val$2054 + 2;
            if (
              _tmp$2053 < 0 || _tmp$2053 >= Moonbit_array_length(bytes$971)
            ) {
              moonbit_panic();
            }
            _tmp$2052 = bytes$971[_tmp$2053];
            _tmp$2051 = (int32_t)_tmp$2052;
            _tmp$2050 = _tmp$2051 & 63;
            _tmp$2049 = _tmp$2050 << 6;
            _tmp$2042 = _tmp$2048 | _tmp$2049;
            val$2047 = i$974->$0;
            _tmp$2046 = val$2047 + 3;
            if (
              _tmp$2046 < 0 || _tmp$2046 >= Moonbit_array_length(bytes$971)
            ) {
              moonbit_panic();
            }
            _tmp$2045 = bytes$971[_tmp$2046];
            _tmp$2044 = (int32_t)_tmp$2045;
            _tmp$2043 = _tmp$2044 & 63;
            _tmp$2041 = _tmp$2042 | _tmp$2043;
            c$975->$0 = _tmp$2041;
            val$2065 = c$975->$0;
            _tmp$2064 = val$2065 - 65536;
            c$975->$0 = _tmp$2064;
            val$2069 = c$975->$0;
            _tmp$2068 = val$2069 >> 10;
            _tmp$2067 = _tmp$2068 + 55296;
            _tmp$2066 = _tmp$2067;
            moonbit_incref(res$972);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$972, _tmp$2066
            );
            _field$2115 = c$975->$0;
            moonbit_decref(c$975);
            val$2073 = _field$2115;
            _tmp$2072 = val$2073 & 1023;
            _tmp$2071 = _tmp$2072 + 56320;
            _tmp$2070 = _tmp$2071;
            moonbit_incref(res$972);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$972, _tmp$2070
            );
            val$2075 = i$974->$0;
            _tmp$2074 = val$2075 + 4;
            i$974->$0 = _tmp$2074;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$974);
      moonbit_decref(bytes$971);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$972);
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$1986,
  moonbit_string_t s$965
) {
  struct $Ref$3c$Int$3e$* res$966 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$967;
  int32_t i$968;
  int32_t _field$2116;
  Moonbit_object_header(res$966)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$966->$0 = 0;
  len$967 = Moonbit_array_length(s$965);
  i$968 = 0;
  while (1) {
    if (i$968 < len$967) {
      int32_t val$1991 = res$966->$0;
      int32_t _tmp$1988 = val$1991 * 10;
      int32_t _tmp$1990;
      int32_t _tmp$1989;
      int32_t _tmp$1987;
      int32_t _tmp$1992;
      if (i$968 < 0 || i$968 >= Moonbit_array_length(s$965)) {
        moonbit_panic();
      }
      _tmp$1990 = s$965[i$968];
      _tmp$1989 = _tmp$1990 - 48;
      _tmp$1987 = _tmp$1988 + _tmp$1989;
      res$966->$0 = _tmp$1987;
      _tmp$1992 = i$968 + 1;
      i$968 = _tmp$1992;
      continue;
    } else {
      moonbit_decref(s$965);
    }
    break;
  }
  _field$2116 = res$966->$0;
  moonbit_decref(res$966);
  return _field$2116;
}

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$928,
  moonbit_string_t filename$889,
  int32_t index$890
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$888;
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$2526;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$891;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$900;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2126;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$1985;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2125;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$901;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2124;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$1984;
  moonbit_string_t _field$2123;
  moonbit_string_t file_name$902;
  moonbit_string_t name$903;
  int32_t _tmp$1981;
  moonbit_string_t name$904;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$1938;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1939;
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$2528;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$911;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$927;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$952;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$954;
  void* _field$2120;
  int32_t _cnt$2397;
  void* _bind$955;
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$2532;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1978;
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$2533;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1971;
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$2534;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1972;
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$2535;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1956;
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_async_tests
  );
  filtered_test$888
  = $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_async_tests,
      filename$889,
      index$890
  );
  _closure$2526
  = (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$2526)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$2526->code
  = &$$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$2526->$0 = index$890;
  handle_result$891
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$2526;
  if (filtered_test$888 == 0) {
    moonbit_decref(async_tests$928);
    if (filtered_test$888) {
      moonbit_decref(filtered_test$888);
    }
    $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$891,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$962 =
      filtered_test$888;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$963 = _Some$962;
    item$900 = _item$963;
    goto $join$899;
  }
  goto $joinlet$2527;
  $join$899:;
  _field$2126 = item$900->$1;
  meta$1985 = _field$2126;
  _field$2125 = meta$1985->$2;
  attrs$901 = _field$2125;
  _field$2124 = item$900->$1;
  meta$1984 = _field$2124;
  _field$2123 = meta$1984->$0;
  file_name$902 = _field$2123;
  moonbit_incref(attrs$901);
  moonbit_incref(file_name$902);
  moonbit_incref(attrs$901);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$901)) {
    name$903 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$901);
    name$903 = $$moonbitlang$core$builtin$Array$$at$0(attrs$901, 0);
  }
  _tmp$1981 = Moonbit_array_length(name$903);
  if (_tmp$1981 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2122;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$1983;
    int32_t _field$2121;
    int32_t index$1982;
    moonbit_decref(name$903);
    _field$2122 = item$900->$1;
    meta$1983 = _field$2122;
    _field$2121 = meta$1983->$1;
    index$1982 = _field$2121;
    name$904 = $Int$$to_string$inner(index$1982, 10);
  } else {
    name$904 = name$903;
  }
  moonbit_incref(attrs$901);
  _tmp$1938 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$901);
  _tmp$1939
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$1938, _tmp$1939)) {
    moonbit_string_t _tmp$1953;
    moonbit_string_t _tmp$1952;
    moonbit_string_t _tmp$1949;
    moonbit_string_t _tmp$1951;
    moonbit_string_t _tmp$1950;
    moonbit_string_t _tmp$1948;
    moonbit_decref(async_tests$928);
    moonbit_decref(item$900);
    moonbit_incref(file_name$902);
    _tmp$1953
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$902
    );
    _tmp$1952
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$1953
    );
    _tmp$1949
    = moonbit_add_string(
      _tmp$1952, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$1951 = $$moonbitlang$core$builtin$Array$$at$0(attrs$901, 0);
    _tmp$1950 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$1951);
    _tmp$1948 = moonbit_add_string(_tmp$1949, _tmp$1950);
    $moonbitlang$core$builtin$println$0(_tmp$1948);
    $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$891,
        name$904,
        file_name$902,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$901);
  }
  moonbit_incref(name$904);
  moonbit_incref(file_name$902);
  moonbit_incref(handle_result$891);
  _closure$2528
  = (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$2528)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2528->code
  = &$$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$2528->$0 = name$904;
  _closure$2528->$1 = file_name$902;
  _closure$2528->$2 = handle_result$891;
  on_err$911 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2528;
  _field$2120 = item$900->$0;
  _cnt$2397 = Moonbit_object_header(item$900)->rc;
  if (_cnt$2397 > 1) {
    int32_t _new_cnt$2399;
    moonbit_incref(_field$2120);
    _new_cnt$2399 = _cnt$2397 - 1;
    Moonbit_object_header(item$900)->rc = _new_cnt$2399;
  } else if (_cnt$2397 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2398 = item$900->$1;
    moonbit_decref(_field$2398);
    moonbit_free(item$900);
  }
  _bind$955 = _field$2120;
  switch (Moonbit_object_tag(_bind$955)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$956;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2117;
      int32_t _cnt$2400;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$957;
      moonbit_decref(async_tests$928);
      _F0$956 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$955;
      _field$2117 = _F0$956->$0;
      _cnt$2400 = Moonbit_object_header(_F0$956)->rc;
      if (_cnt$2400 > 1) {
        int32_t _new_cnt$2401;
        moonbit_incref(_field$2117);
        _new_cnt$2401 = _cnt$2400 - 1;
        Moonbit_object_header(_F0$956)->rc = _new_cnt$2401;
      } else if (_cnt$2400 == 1) {
        moonbit_free(_F0$956);
      }
      _f$957 = _field$2117;
      f$954 = _f$957;
      goto $join$953;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$958;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2118;
      int32_t _cnt$2402;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$959;
      moonbit_decref(async_tests$928);
      _F1$958 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$955;
      _field$2118 = _F1$958->$0;
      _cnt$2402 = Moonbit_object_header(_F1$958)->rc;
      if (_cnt$2402 > 1) {
        int32_t _new_cnt$2403;
        moonbit_incref(_field$2118);
        _new_cnt$2403 = _cnt$2402 - 1;
        Moonbit_object_header(_F1$958)->rc = _new_cnt$2403;
      } else if (_cnt$2402 == 1) {
        moonbit_free(_F1$958);
      }
      _f$959 = _field$2118;
      f$952 = _f$959;
      goto $join$951;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$960 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$955;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2119 =
        _F2$960->$0;
      int32_t _cnt$2404 = Moonbit_object_header(_F2$960)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$961;
      if (_cnt$2404 > 1) {
        int32_t _new_cnt$2405;
        moonbit_incref(_field$2119);
        _new_cnt$2405 = _cnt$2404 - 1;
        Moonbit_object_header(_F2$960)->rc = _new_cnt$2405;
      } else if (_cnt$2404 == 1) {
        moonbit_free(_F2$960);
      }
      _f$961 = _field$2119;
      f$927 = _f$961;
      goto $join$926;
      break;
    }
  }
  goto $joinlet$2531;
  $join$953:;
  _closure$2532
  = (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$2532)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2532->code
  = &$$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$2532->$0 = name$904;
  _closure$2532->$1 = file_name$902;
  _closure$2532->$2 = handle_result$891;
  _tmp$1978 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2532;
  $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_catch_error(
    f$954, _tmp$1978, on_err$911
  );
  $joinlet$2531:;
  goto $joinlet$2530;
  $join$951:;
  moonbit_incref(name$904);
  _closure$2533
  = (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$2533)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$2533->code
  = &$$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$2533->$0 = f$952;
  _closure$2533->$1 = name$904;
  _tmp$1971
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$2533;
  _closure$2534
  = (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$2534)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2534->code
  = &$$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$2534->$0 = name$904;
  _closure$2534->$1 = file_name$902;
  _closure$2534->$2 = handle_result$891;
  _tmp$1972 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2534;
  $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_catch_error(
    _tmp$1971, _tmp$1972, on_err$911
  );
  $joinlet$2530:;
  goto $joinlet$2529;
  $join$926:;
  _closure$2535
  = (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$2535)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$2535->code
  = &$$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$2535->$0 = f$927;
  _closure$2535->$1 = on_err$911;
  _closure$2535->$2 = name$904;
  _closure$2535->$3 = file_name$902;
  _closure$2535->$4 = handle_result$891;
  _tmp$1956
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$2535;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$928, _tmp$1956);
  $joinlet$2529:;
  $joinlet$2527:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1979
) {
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$1980 =
    (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$1979;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2129 =
    _casted_env$1980->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$891 =
    _field$2129;
  moonbit_string_t _field$2128 = _casted_env$1980->$1;
  moonbit_string_t file_name$902 = _field$2128;
  moonbit_string_t _field$2127 = _casted_env$1980->$0;
  int32_t _cnt$2406 = Moonbit_object_header(_casted_env$1980)->rc;
  moonbit_string_t name$904;
  if (_cnt$2406 > 1) {
    int32_t _new_cnt$2407;
    moonbit_incref(handle_result$891);
    moonbit_incref(file_name$902);
    moonbit_incref(_field$2127);
    _new_cnt$2407 = _cnt$2406 - 1;
    Moonbit_object_header(_casted_env$1980)->rc = _new_cnt$2407;
  } else if (_cnt$2406 == 1) {
    moonbit_free(_casted_env$1980);
  }
  name$904 = _field$2127;
  $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$891,
      name$904,
      file_name$902,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1975
) {
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$1976 =
    (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$1975;
  moonbit_string_t _field$2131 = _casted_env$1976->$1;
  moonbit_string_t name$904 = _field$2131;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2130 =
    _casted_env$1976->$0;
  int32_t _cnt$2408 = Moonbit_object_header(_casted_env$1976)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$952;
  int32_t _tmp$1977;
  if (_cnt$2408 > 1) {
    int32_t _new_cnt$2409;
    moonbit_incref(name$904);
    moonbit_incref(_field$2130);
    _new_cnt$2409 = _cnt$2408 - 1;
    Moonbit_object_header(_casted_env$1976)->rc = _new_cnt$2409;
  } else if (_cnt$2408 == 1) {
    moonbit_free(_casted_env$1976);
  }
  f$952 = _field$2130;
  _tmp$1977
  = $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$904
  );
  return f$952->code(f$952, _tmp$1977);
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1973
) {
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$1974 =
    (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$1973;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2134 =
    _casted_env$1974->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$891 =
    _field$2134;
  moonbit_string_t _field$2133 = _casted_env$1974->$1;
  moonbit_string_t file_name$902 = _field$2133;
  moonbit_string_t _field$2132 = _casted_env$1974->$0;
  int32_t _cnt$2410 = Moonbit_object_header(_casted_env$1974)->rc;
  moonbit_string_t name$904;
  if (_cnt$2410 > 1) {
    int32_t _new_cnt$2411;
    moonbit_incref(handle_result$891);
    moonbit_incref(file_name$902);
    moonbit_incref(_field$2132);
    _new_cnt$2411 = _cnt$2410 - 1;
    Moonbit_object_header(_casted_env$1974)->rc = _new_cnt$2411;
  } else if (_cnt$2410 == 1) {
    moonbit_free(_casted_env$1974);
  }
  name$904 = _field$2132;
  $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$891,
      name$904,
      file_name$902,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1957,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$929,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$930
) {
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$1958 =
    (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$1957;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2139 =
    _casted_env$1958->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$891 =
    _field$2139;
  moonbit_string_t _field$2138 = _casted_env$1958->$3;
  moonbit_string_t file_name$902 = _field$2138;
  moonbit_string_t _field$2137 = _casted_env$1958->$2;
  moonbit_string_t name$904 = _field$2137;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2136 = _casted_env$1958->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$911 = _field$2136;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2135 =
    _casted_env$1958->$0;
  int32_t _cnt$2412 = Moonbit_object_header(_casted_env$1958)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$927;
  int32_t _async_driver$931;
  int32_t _tmp$1962;
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$2536;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$1963;
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$2537;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$1964;
  if (_cnt$2412 > 1) {
    int32_t _new_cnt$2413;
    moonbit_incref(handle_result$891);
    moonbit_incref(file_name$902);
    moonbit_incref(name$904);
    moonbit_incref(on_err$911);
    moonbit_incref(_field$2135);
    _new_cnt$2413 = _cnt$2412 - 1;
    Moonbit_object_header(_casted_env$1958)->rc = _new_cnt$2413;
  } else if (_cnt$2412 == 1) {
    moonbit_free(_casted_env$1958);
  }
  f$927 = _field$2135;
  _async_driver$931 = 0;
  moonbit_incref(name$904);
  _tmp$1962
  = $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$904
  );
  moonbit_incref(_cont$929);
  _closure$2536
  = (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$2536)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$2536->code
  = &$$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$2536->$0 = _async_driver$931;
  _closure$2536->$1 = _cont$929;
  _closure$2536->$2 = name$904;
  _closure$2536->$3 = file_name$902;
  _closure$2536->$4 = handle_result$891;
  _tmp$1963 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$2536;
  _closure$2537
  = (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$2537)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$2537->code
  = &$$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$2537->$0 = _async_driver$931;
  _closure$2537->$1 = _err_cont$930;
  _closure$2537->$2 = _cont$929;
  _closure$2537->$3 = on_err$911;
  _tmp$1964 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2537;
  f$927->code(f$927, _tmp$1962, _tmp$1963, _tmp$1964);
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1968,
  int32_t _cont_param$949
) {
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$1969 =
    (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$1968;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2144 =
    _casted_env$1969->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$891 =
    _field$2144;
  moonbit_string_t _field$2143 = _casted_env$1969->$3;
  moonbit_string_t file_name$902 = _field$2143;
  moonbit_string_t _field$2142 = _casted_env$1969->$2;
  moonbit_string_t name$904 = _field$2142;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2141 = _casted_env$1969->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$929 = _field$2141;
  int32_t _field$2140 = _casted_env$1969->$0;
  int32_t _cnt$2414 = Moonbit_object_header(_casted_env$1969)->rc;
  int32_t _async_driver$931;
  void* State_1$1970;
  if (_cnt$2414 > 1) {
    int32_t _new_cnt$2415;
    moonbit_incref(handle_result$891);
    moonbit_incref(file_name$902);
    moonbit_incref(name$904);
    moonbit_incref(_cont$929);
    _new_cnt$2415 = _cnt$2414 - 1;
    Moonbit_object_header(_casted_env$1969)->rc = _new_cnt$2415;
  } else if (_cnt$2414 == 1) {
    moonbit_free(_casted_env$1969);
  }
  _async_driver$931 = _field$2140;
  State_1$1970
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1
      )
    );
  Moonbit_object_header(State_1$1970)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1*)State_1$1970)->$0
  = _cont_param$949;
  ((struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1*)State_1$1970)->$1
  = handle_result$891;
  ((struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1*)State_1$1970)->$2
  = file_name$902;
  ((struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1*)State_1$1970)->$3
  = name$904;
  ((struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1*)State_1$1970)->$4
  = _cont$929;
  $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$931, State_1$1970
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1965,
  void* _cont_param$950
) {
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$1966 =
    (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$1965;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2148 = _casted_env$1966->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$911 = _field$2148;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2147 = _casted_env$1966->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$929 = _field$2147;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2146 = _casted_env$1966->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$930 = _field$2146;
  int32_t _field$2145 = _casted_env$1966->$0;
  int32_t _cnt$2416 = Moonbit_object_header(_casted_env$1966)->rc;
  int32_t _async_driver$931;
  void* _try$155$1967;
  if (_cnt$2416 > 1) {
    int32_t _new_cnt$2417;
    moonbit_incref(on_err$911);
    moonbit_incref(_cont$929);
    moonbit_incref(_err_cont$930);
    _new_cnt$2417 = _cnt$2416 - 1;
    Moonbit_object_header(_casted_env$1966)->rc = _new_cnt$2417;
  } else if (_cnt$2416 == 1) {
    moonbit_free(_casted_env$1966);
  }
  _async_driver$931 = _field$2145;
  _try$155$1967
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155
      )
    );
  Moonbit_object_header(_try$155$1967)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155*)_try$155$1967)->$0
  = _cont_param$950;
  ((struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155*)_try$155$1967)->$1
  = on_err$911;
  ((struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155*)_try$155$1967)->$2
  = _cont$929;
  ((struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155*)_try$155$1967)->$3
  = _err_cont$930;
  $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$931, _try$155$1967
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1959,
  void* _state$932
) {
  switch (Moonbit_object_tag(_state$932)) {
    case 0: {
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155* _$2a$try$155$933 =
        (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$$2a$try$155*)_state$932;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2152 = _$2a$try$155$933->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$934 = _field$2152;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2151 = _$2a$try$155$933->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$935 = _field$2151;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2150 = _$2a$try$155$933->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$936 = _field$2150;
      void* _field$2149 = _$2a$try$155$933->$0;
      int32_t _cnt$2418 = Moonbit_object_header(_$2a$try$155$933)->rc;
      void* _try_err$937;
      void* err$939;
      void* err$941;
      int32_t _tmp$1961;
      if (_cnt$2418 > 1) {
        int32_t _new_cnt$2419;
        moonbit_incref(_err_cont$934);
        moonbit_incref(_cont$935);
        moonbit_incref(on_err$936);
        moonbit_incref(_field$2149);
        _new_cnt$2419 = _cnt$2418 - 1;
        Moonbit_object_header(_$2a$try$155$933)->rc = _new_cnt$2419;
      } else if (_cnt$2418 == 1) {
        moonbit_free(_$2a$try$155$933);
      }
      _try_err$937 = _field$2149;
      if (
        $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$936);
        moonbit_decref(_cont$935);
        err$941 = _try_err$937;
        goto $join$940;
      } else {
        moonbit_decref(_err_cont$934);
        err$939 = _try_err$937;
        goto $join$938;
      }
      goto $joinlet$2539;
      $join$940:;
      return _err_cont$934->code(_err_cont$934, err$941);
      $joinlet$2539:;
      goto $joinlet$2538;
      $join$938:;
      _tmp$1961 = on_err$936->code(on_err$936, err$939);
      _cont$935->code(_cont$935, _tmp$1961);
      $joinlet$2538:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1* _State_1$942 =
        (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$171$on_err$68$$2a$arm$163$lambda$189$State$State_1*)_state$932;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2156 = _State_1$942->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$943 = _field$2156;
      moonbit_string_t _field$2155 = _State_1$942->$3;
      moonbit_string_t name$944 = _field$2155;
      moonbit_string_t _field$2154 = _State_1$942->$2;
      moonbit_string_t file_name$945 = _field$2154;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2153 =
        _State_1$942->$1;
      int32_t _cnt$2420 = Moonbit_object_header(_State_1$942)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$946;
      int32_t _tmp$1960;
      if (_cnt$2420 > 1) {
        int32_t _new_cnt$2421;
        moonbit_incref(_cont$943);
        moonbit_incref(name$944);
        moonbit_incref(file_name$945);
        moonbit_incref(_field$2153);
        _new_cnt$2421 = _cnt$2420 - 1;
        Moonbit_object_header(_State_1$942)->rc = _new_cnt$2421;
      } else if (_cnt$2420 == 1) {
        moonbit_free(_State_1$942);
      }
      handle_result$946 = _field$2153;
      _tmp$1960
      = handle_result$946->code(
        handle_result$946,
          name$944,
          file_name$945,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$943->code(_cont$943, _tmp$1960);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1954,
  void* err$912
) {
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$1955 =
    (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$1954;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2163 =
    _casted_env$1955->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$891 =
    _field$2163;
  moonbit_string_t _field$2162 = _casted_env$1955->$1;
  moonbit_string_t file_name$902 = _field$2162;
  moonbit_string_t _field$2161 = _casted_env$1955->$0;
  int32_t _cnt$2422 = Moonbit_object_header(_casted_env$1955)->rc;
  moonbit_string_t name$904;
  void* e$914;
  moonbit_string_t e$917;
  moonbit_string_t message$915;
  if (_cnt$2422 > 1) {
    int32_t _new_cnt$2423;
    moonbit_incref(handle_result$891);
    moonbit_incref(file_name$902);
    moonbit_incref(_field$2161);
    _new_cnt$2423 = _cnt$2422 - 1;
    Moonbit_object_header(_casted_env$1955)->rc = _new_cnt$2423;
  } else if (_cnt$2422 == 1) {
    moonbit_free(_casted_env$1955);
  }
  name$904 = _field$2161;
  switch (Moonbit_object_tag(err$912)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$918 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$912;
      moonbit_string_t _field$2157 = _Failure$918->$0;
      int32_t _cnt$2424 = Moonbit_object_header(_Failure$918)->rc;
      moonbit_string_t _e$919;
      if (_cnt$2424 > 1) {
        int32_t _new_cnt$2425;
        moonbit_incref(_field$2157);
        _new_cnt$2425 = _cnt$2424 - 1;
        Moonbit_object_header(_Failure$918)->rc = _new_cnt$2425;
      } else if (_cnt$2424 == 1) {
        moonbit_free(_Failure$918);
      }
      _e$919 = _field$2157;
      e$917 = _e$919;
      goto $join$916;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$920 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$912;
      moonbit_string_t _field$2158 = _InspectError$920->$0;
      int32_t _cnt$2426 = Moonbit_object_header(_InspectError$920)->rc;
      moonbit_string_t _e$921;
      if (_cnt$2426 > 1) {
        int32_t _new_cnt$2427;
        moonbit_incref(_field$2158);
        _new_cnt$2427 = _cnt$2426 - 1;
        Moonbit_object_header(_InspectError$920)->rc = _new_cnt$2427;
      } else if (_cnt$2426 == 1) {
        moonbit_free(_InspectError$920);
      }
      _e$921 = _field$2158;
      e$917 = _e$921;
      goto $join$916;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$922 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$912;
      moonbit_string_t _field$2159 = _SnapshotError$922->$0;
      int32_t _cnt$2428 = Moonbit_object_header(_SnapshotError$922)->rc;
      moonbit_string_t _e$923;
      if (_cnt$2428 > 1) {
        int32_t _new_cnt$2429;
        moonbit_incref(_field$2159);
        _new_cnt$2429 = _cnt$2428 - 1;
        Moonbit_object_header(_SnapshotError$922)->rc = _new_cnt$2429;
      } else if (_cnt$2428 == 1) {
        moonbit_free(_SnapshotError$922);
      }
      _e$923 = _field$2159;
      e$917 = _e$923;
      goto $join$916;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$tests$enhanced_suite_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$924 =
        (struct $Error$azimuth$telemetry$tests$enhanced_suite_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$912;
      moonbit_string_t _field$2160 =
        _MoonBitTestDriverInternalJsError$924->$0;
      int32_t _cnt$2430 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$924)->rc;
      moonbit_string_t _e$925;
      if (_cnt$2430 > 1) {
        int32_t _new_cnt$2431;
        moonbit_incref(_field$2160);
        _new_cnt$2431 = _cnt$2430 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$924)->rc
        = _new_cnt$2431;
      } else if (_cnt$2430 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$924);
      }
      _e$925 = _field$2160;
      e$917 = _e$925;
      goto $join$916;
      break;
    }
    default: {
      e$914 = err$912;
      goto $join$913;
      break;
    }
  }
  goto $joinlet$2541;
  $join$916:;
  $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$891, name$904, file_name$902, e$917, 0
  );
  $joinlet$2541:;
  goto $joinlet$2540;
  $join$913:;
  message$915 = $Error$to_string(e$914);
  $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$891, name$904, file_name$902, message$915, 0
  );
  $joinlet$2540:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1940,
  moonbit_string_t attr$905
) {
  int32_t _tmp$1942;
  int64_t _tmp$1941;
  moonbit_decref(_env$1940);
  _tmp$1942 = Moonbit_array_length(attr$905);
  _tmp$1941 = (int64_t)_tmp$1942;
  moonbit_incref(attr$905);
  if ($String$$char_length_ge$inner(attr$905, 5, 0, _tmp$1941)) {
    int32_t _tmp$1947 = attr$905[0];
    int32_t _x$906 = _tmp$1947;
    if (_x$906 == 112) {
      int32_t _tmp$1946 = attr$905[1];
      int32_t _x$907 = _tmp$1946;
      if (_x$907 == 97) {
        int32_t _tmp$1945 = attr$905[2];
        int32_t _x$908 = _tmp$1945;
        if (_x$908 == 110) {
          int32_t _tmp$1944 = attr$905[3];
          int32_t _x$909 = _tmp$1944;
          if (_x$909 == 105) {
            int32_t _tmp$2164 = attr$905[4];
            int32_t _tmp$1943;
            int32_t _x$910;
            moonbit_decref(attr$905);
            _tmp$1943 = _tmp$2164;
            _x$910 = _tmp$1943;
            return _x$910 == 99 || 0;
          } else {
            moonbit_decref(attr$905);
            return 0;
          }
        } else {
          moonbit_decref(attr$905);
          return 0;
        }
      } else {
        moonbit_decref(attr$905);
        return 0;
      }
    } else {
      moonbit_decref(attr$905);
      return 0;
    }
  } else {
    moonbit_decref(attr$905);
    return 0;
  }
}

int32_t $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1924,
  moonbit_string_t test_name$892,
  moonbit_string_t file_name$893,
  moonbit_string_t message$894,
  int32_t skipped$895
) {
  struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$1925 =
    (struct $$azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$1924;
  int32_t _field$2165 = _casted_env$1925->$0;
  int32_t index$890;
  int32_t _if_result$2542;
  moonbit_string_t file_name$896;
  moonbit_string_t test_name$897;
  moonbit_string_t message$898;
  moonbit_string_t _tmp$1937;
  moonbit_string_t _tmp$1936;
  moonbit_string_t _tmp$1934;
  moonbit_string_t _tmp$1935;
  moonbit_string_t _tmp$1933;
  moonbit_string_t _tmp$1931;
  moonbit_string_t _tmp$1932;
  moonbit_string_t _tmp$1930;
  moonbit_string_t _tmp$1928;
  moonbit_string_t _tmp$1929;
  moonbit_string_t _tmp$1927;
  moonbit_string_t _tmp$1926;
  moonbit_decref(_casted_env$1925);
  index$890 = _field$2165;
  if (!skipped$895) {
    _if_result$2542 = 1;
  } else {
    _if_result$2542 = 0;
  }
  if (_if_result$2542) {
    
  }
  file_name$896 = $String$$escape(file_name$893);
  test_name$897 = $String$$escape(test_name$892);
  message$898 = $String$$escape(message$894);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$1937
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$896
  );
  _tmp$1936
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$1937
  );
  _tmp$1934
  = moonbit_add_string(
    _tmp$1936, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$1935
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$890
  );
  _tmp$1933 = moonbit_add_string(_tmp$1934, _tmp$1935);
  _tmp$1931
  = moonbit_add_string(
    _tmp$1933, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$1932
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$897
  );
  _tmp$1930 = moonbit_add_string(_tmp$1931, _tmp$1932);
  _tmp$1928
  = moonbit_add_string(
    _tmp$1930, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$1929 = $$moonbitlang$core$builtin$Show$$String$$to_string(message$898);
  _tmp$1927 = moonbit_add_string(_tmp$1928, _tmp$1929);
  _tmp$1926
  = moonbit_add_string(
    _tmp$1927, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$1926);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$887
) {
  moonbit_decref(_discard_$887);
  return 42;
}

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$885,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$886,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$883
) {
  void* _try_err$881;
  struct moonbit_result_0 _tmp$2544 = f$885->code(f$885);
  void* err$882;
  if (_tmp$2544.tag) {
    int32_t const _ok$1922 = _tmp$2544.data.ok;
    moonbit_decref(on_err$883);
  } else {
    void* const _err$1923 = _tmp$2544.data.err;
    moonbit_decref(on_ok$886);
    _try_err$881 = _err$1923;
    goto $join$880;
  }
  on_ok$886->code(on_ok$886);
  goto $joinlet$2543;
  $join$880:;
  err$882 = _try_err$881;
  on_err$883->code(on_err$883, err$882);
  $joinlet$2543:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$846,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$859,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$872,
  moonbit_string_t file_filter$843,
  int32_t index_filter$844
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$840;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$841;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$845;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2171;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1913;
  void* F0$1910;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2170;
  int32_t _cnt$2432;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1912;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1911;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$842;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$855;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$856;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$858;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2169;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1917;
  void* F1$1914;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2168;
  int32_t _cnt$2435;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1916;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1915;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$857;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$868;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$869;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$871;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2167;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1921;
  void* F2$1918;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2166;
  int32_t _cnt$2438;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1920;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1919;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$870;
  moonbit_incref(file_filter$843);
  _bind$845
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$846, file_filter$843
  );
  if (_bind$845 == 0) {
    if (_bind$845) {
      moonbit_decref(_bind$845);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$847 =
      _bind$845;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$848 =
      _Some$847;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$850;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$851;
    moonbit_incref(_index_func_map$848);
    _bind$851
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$848, index_filter$844
    );
    if (_bind$851 == 0) {
      if (_bind$851) {
        moonbit_decref(_bind$851);
      }
      moonbit_decref(_index_func_map$848);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$852;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$853;
      moonbit_decref(async_tests$872);
      moonbit_decref(with_args_tests$859);
      _Some$852 = _bind$851;
      _func_attrs_tuple$853 = _Some$852;
      func_attrs_tuple$850 = _func_attrs_tuple$853;
      goto $join$849;
    }
    goto $joinlet$2546;
    $join$849:;
    index_func_map$840 = _index_func_map$848;
    func_attrs_tuple$841 = func_attrs_tuple$850;
    goto $join$839;
    $joinlet$2546:;
  }
  goto $joinlet$2545;
  $join$839:;
  moonbit_decref(index_func_map$840);
  _field$2171 = func_attrs_tuple$841->$0;
  _tmp$1913 = _field$2171;
  moonbit_incref(_tmp$1913);
  F0$1910
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$1910)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$1910)->$0 = _tmp$1913;
  _field$2170 = func_attrs_tuple$841->$1;
  _cnt$2432 = Moonbit_object_header(func_attrs_tuple$841)->rc;
  if (_cnt$2432 > 1) {
    int32_t _new_cnt$2434;
    moonbit_incref(_field$2170);
    _new_cnt$2434 = _cnt$2432 - 1;
    Moonbit_object_header(func_attrs_tuple$841)->rc = _new_cnt$2434;
  } else if (_cnt$2432 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2433 =
      func_attrs_tuple$841->$0;
    moonbit_decref(_field$2433);
    moonbit_free(func_attrs_tuple$841);
  }
  _tmp$1912 = _field$2170;
  _tmp$1911
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1911)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1911->$0 = file_filter$843;
  _tmp$1911->$1 = index_filter$844;
  _tmp$1911->$2 = _tmp$1912;
  k$842
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$842)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$842->$0 = F0$1910;
  k$842->$1 = _tmp$1911;
  return k$842;
  $joinlet$2545:;
  moonbit_incref(file_filter$843);
  _bind$858
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$859, file_filter$843
  );
  if (_bind$858 == 0) {
    if (_bind$858) {
      moonbit_decref(_bind$858);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$860 =
      _bind$858;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$861 =
      _Some$860;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$863;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$864;
    moonbit_incref(_index_func_map$861);
    _bind$864
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$861, index_filter$844
    );
    if (_bind$864 == 0) {
      if (_bind$864) {
        moonbit_decref(_bind$864);
      }
      moonbit_decref(_index_func_map$861);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$865;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$866;
      moonbit_decref(async_tests$872);
      _Some$865 = _bind$864;
      _func_attrs_tuple$866 = _Some$865;
      func_attrs_tuple$863 = _func_attrs_tuple$866;
      goto $join$862;
    }
    goto $joinlet$2548;
    $join$862:;
    index_func_map$855 = _index_func_map$861;
    func_attrs_tuple$856 = func_attrs_tuple$863;
    goto $join$854;
    $joinlet$2548:;
  }
  goto $joinlet$2547;
  $join$854:;
  moonbit_decref(index_func_map$855);
  _field$2169 = func_attrs_tuple$856->$0;
  _tmp$1917 = _field$2169;
  moonbit_incref(_tmp$1917);
  F1$1914
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$1914)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$1914)->$0 = _tmp$1917;
  _field$2168 = func_attrs_tuple$856->$1;
  _cnt$2435 = Moonbit_object_header(func_attrs_tuple$856)->rc;
  if (_cnt$2435 > 1) {
    int32_t _new_cnt$2437;
    moonbit_incref(_field$2168);
    _new_cnt$2437 = _cnt$2435 - 1;
    Moonbit_object_header(func_attrs_tuple$856)->rc = _new_cnt$2437;
  } else if (_cnt$2435 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2436 =
      func_attrs_tuple$856->$0;
    moonbit_decref(_field$2436);
    moonbit_free(func_attrs_tuple$856);
  }
  _tmp$1916 = _field$2168;
  _tmp$1915
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1915)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1915->$0 = file_filter$843;
  _tmp$1915->$1 = index_filter$844;
  _tmp$1915->$2 = _tmp$1916;
  k$857
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$857)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$857->$0 = F1$1914;
  k$857->$1 = _tmp$1915;
  return k$857;
  $joinlet$2547:;
  moonbit_incref(file_filter$843);
  _bind$871
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$872, file_filter$843
  );
  if (_bind$871 == 0) {
    if (_bind$871) {
      moonbit_decref(_bind$871);
    }
    moonbit_decref(file_filter$843);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$873 =
      _bind$871;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$874 =
      _Some$873;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$876;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$877;
    moonbit_incref(_index_func_map$874);
    _bind$877
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$874, index_filter$844
    );
    if (_bind$877 == 0) {
      if (_bind$877) {
        moonbit_decref(_bind$877);
      }
      moonbit_decref(_index_func_map$874);
      moonbit_decref(file_filter$843);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$878 =
        _bind$877;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$879 =
        _Some$878;
      func_attrs_tuple$876 = _func_attrs_tuple$879;
      goto $join$875;
    }
    goto $joinlet$2550;
    $join$875:;
    index_func_map$868 = _index_func_map$874;
    func_attrs_tuple$869 = func_attrs_tuple$876;
    goto $join$867;
    $joinlet$2550:;
  }
  goto $joinlet$2549;
  $join$867:;
  moonbit_decref(index_func_map$868);
  _field$2167 = func_attrs_tuple$869->$0;
  _tmp$1921 = _field$2167;
  moonbit_incref(_tmp$1921);
  F2$1918
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$1918)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$1918)->$0 = _tmp$1921;
  _field$2166 = func_attrs_tuple$869->$1;
  _cnt$2438 = Moonbit_object_header(func_attrs_tuple$869)->rc;
  if (_cnt$2438 > 1) {
    int32_t _new_cnt$2440;
    moonbit_incref(_field$2166);
    _new_cnt$2440 = _cnt$2438 - 1;
    Moonbit_object_header(func_attrs_tuple$869)->rc = _new_cnt$2440;
  } else if (_cnt$2438 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2439 =
      func_attrs_tuple$869->$0;
    moonbit_decref(_field$2439);
    moonbit_free(func_attrs_tuple$869);
  }
  _tmp$1920 = _field$2166;
  _tmp$1919
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1919)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1919->$0 = file_filter$843;
  _tmp$1919->$1 = index_filter$844;
  _tmp$1919->$2 = _tmp$1920;
  k$870
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$870)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$870->$0 = F2$1918;
  k$870->$1 = _tmp$1919;
  return k$870;
  $joinlet$2549:;
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$834
) {
  int32_t _field$2172 = self$834->$1;
  int32_t len$1909;
  moonbit_decref(self$834);
  len$1909 = _field$2172;
  return len$1909 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$832,
  struct $$moonbitlang$core$builtin$Logger logger$833
) {
  moonbit_string_t _tmp$1908 = self$832;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$1907 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$1908);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$1907, logger$833
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$818,
  struct $$moonbitlang$core$builtin$Logger logger$831
) {
  struct $StringView _field$2181 =
    (struct $StringView){self$818->$0_1, self$818->$0_2, self$818->$0_0};
  struct $StringView pkg$817 = _field$2181;
  int32_t _tmp$1906 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$1905;
  int64_t _bind$819;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$820;
  struct $StringView _field$2180;
  struct $StringView _module_name$827;
  void* _field$2179;
  int32_t _cnt$2441;
  void* _package_name$828;
  struct $StringView _field$2177;
  struct $StringView filename$1888;
  struct $StringView _field$2176;
  struct $StringView start_line$1889;
  struct $StringView _field$2175;
  struct $StringView start_column$1890;
  struct $StringView _field$2174;
  struct $StringView end_line$1891;
  struct $StringView _field$2173;
  int32_t _cnt$2445;
  struct $StringView end_column$1892;
  struct $$moonbitlang$core$builtin$Logger _bind$1887;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$1905
  = (struct $StringView){
    0, _tmp$1906, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$817.$0);
  moonbit_incref(pkg$817.$0);
  _bind$819 = $StringView$$find(pkg$817, _tmp$1905);
  if (_bind$819 == 4294967296ll) {
    void* None$1893 =
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
    _bind$820->$1 = None$1893;
  } else {
    int64_t _Some$821 = _bind$819;
    int32_t _first_slash$822 = (int32_t)_Some$821;
    int32_t _tmp$1904 = _first_slash$822 + 1;
    struct $StringView _tmp$1901;
    int32_t _tmp$1903;
    struct $StringView _tmp$1902;
    int64_t _bind$823;
    moonbit_incref(pkg$817.$0);
    _tmp$1901 = $StringView$$view$inner(pkg$817, _tmp$1904, 4294967296ll);
    _tmp$1903
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$1902
    = (struct $StringView){
      0, _tmp$1903, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$823 = $StringView$$find(_tmp$1901, _tmp$1902);
    if (_bind$823 == 4294967296ll) {
      void* None$1894 =
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
      _bind$820->$1 = None$1894;
    } else {
      int64_t _Some$824 = _bind$823;
      int32_t _second_slash$825 = (int32_t)_Some$824;
      int32_t _tmp$1900 = _first_slash$822 + 1;
      int32_t module_name_end$826 = _tmp$1900 + _second_slash$825;
      int64_t _tmp$1899 = (int64_t)module_name_end$826;
      struct $StringView _tmp$1895;
      int32_t _tmp$1898;
      struct $StringView _tmp$1897;
      void* Some$1896;
      moonbit_incref(pkg$817.$0);
      _tmp$1895 = $StringView$$view$inner(pkg$817, 0, _tmp$1899);
      _tmp$1898 = module_name_end$826 + 1;
      _tmp$1897 = $StringView$$view$inner(pkg$817, _tmp$1898, 4294967296ll);
      Some$1896
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$1896)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$1896)->$0_0
      = _tmp$1897.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1896)->$0_1
      = _tmp$1897.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1896)->$0_2
      = _tmp$1897.$2;
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
      _bind$820->$0_0 = _tmp$1895.$0;
      _bind$820->$0_1 = _tmp$1895.$1;
      _bind$820->$0_2 = _tmp$1895.$2;
      _bind$820->$1 = Some$1896;
    }
  }
  _field$2180
  = (struct $StringView){
    _bind$820->$0_1, _bind$820->$0_2, _bind$820->$0_0
  };
  _module_name$827 = _field$2180;
  _field$2179 = _bind$820->$1;
  _cnt$2441 = Moonbit_object_header(_bind$820)->rc;
  if (_cnt$2441 > 1) {
    int32_t _new_cnt$2442;
    moonbit_incref(_field$2179);
    moonbit_incref(_module_name$827.$0);
    _new_cnt$2442 = _cnt$2441 - 1;
    Moonbit_object_header(_bind$820)->rc = _new_cnt$2442;
  } else if (_cnt$2441 == 1) {
    moonbit_free(_bind$820);
  }
  _package_name$828 = _field$2179;
  switch (Moonbit_object_tag(_package_name$828)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$829 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$828;
      struct $StringView _field$2178 =
        (struct $StringView){
          _Some$829->$0_1, _Some$829->$0_2, _Some$829->$0_0
        };
      int32_t _cnt$2443 = Moonbit_object_header(_Some$829)->rc;
      struct $StringView _pkg_name$830;
      struct $$moonbitlang$core$builtin$Logger _bind$1886;
      if (_cnt$2443 > 1) {
        int32_t _new_cnt$2444;
        moonbit_incref(_field$2178.$0);
        _new_cnt$2444 = _cnt$2443 - 1;
        Moonbit_object_header(_Some$829)->rc = _new_cnt$2444;
      } else if (_cnt$2443 == 1) {
        moonbit_free(_Some$829);
      }
      _pkg_name$830 = _field$2178;
      if (logger$831.$1) {
        moonbit_incref(logger$831.$1);
      }
      logger$831.$0->$method_2(logger$831.$1, _pkg_name$830);
      _bind$1886 = logger$831;
      if (_bind$1886.$1) {
        moonbit_incref(_bind$1886.$1);
      }
      _bind$1886.$0->$method_3(_bind$1886.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$828);
      break;
    }
  }
  _field$2177
  = (struct $StringView){
    self$818->$1_1, self$818->$1_2, self$818->$1_0
  };
  filename$1888 = _field$2177;
  moonbit_incref(filename$1888.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, filename$1888);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2176
  = (struct $StringView){
    self$818->$2_1, self$818->$2_2, self$818->$2_0
  };
  start_line$1889 = _field$2176;
  moonbit_incref(start_line$1889.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, start_line$1889);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2175
  = (struct $StringView){
    self$818->$3_1, self$818->$3_2, self$818->$3_0
  };
  start_column$1890 = _field$2175;
  moonbit_incref(start_column$1890.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, start_column$1890);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 45);
  _field$2174
  = (struct $StringView){
    self$818->$4_1, self$818->$4_2, self$818->$4_0
  };
  end_line$1891 = _field$2174;
  moonbit_incref(end_line$1891.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, end_line$1891);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2173
  = (struct $StringView){
    self$818->$5_1, self$818->$5_2, self$818->$5_0
  };
  _cnt$2445 = Moonbit_object_header(self$818)->rc;
  if (_cnt$2445 > 1) {
    int32_t _new_cnt$2451;
    moonbit_incref(_field$2173.$0);
    _new_cnt$2451 = _cnt$2445 - 1;
    Moonbit_object_header(self$818)->rc = _new_cnt$2451;
  } else if (_cnt$2445 == 1) {
    struct $StringView _field$2450 =
      (struct $StringView){self$818->$4_1, self$818->$4_2, self$818->$4_0};
    struct $StringView _field$2449;
    struct $StringView _field$2448;
    struct $StringView _field$2447;
    struct $StringView _field$2446;
    moonbit_decref(_field$2450.$0);
    _field$2449
    = (struct $StringView){
      self$818->$3_1, self$818->$3_2, self$818->$3_0
    };
    moonbit_decref(_field$2449.$0);
    _field$2448
    = (struct $StringView){
      self$818->$2_1, self$818->$2_2, self$818->$2_0
    };
    moonbit_decref(_field$2448.$0);
    _field$2447
    = (struct $StringView){
      self$818->$1_1, self$818->$1_2, self$818->$1_0
    };
    moonbit_decref(_field$2447.$0);
    _field$2446
    = (struct $StringView){
      self$818->$0_1, self$818->$0_2, self$818->$0_0
    };
    moonbit_decref(_field$2446.$0);
    moonbit_free(self$818);
  }
  end_column$1892 = _field$2173;
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, end_column$1892);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 64);
  _bind$1887 = logger$831;
  _bind$1887.$0->$method_2(_bind$1887.$1, _module_name$827);
  return 0;
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$816) {
  moonbit_string_t _tmp$1885 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$816);
  moonbit_println(_tmp$1885);
  moonbit_decref(_tmp$1885);
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
      int32_t _tmp$1883 = value$808[i$809];
      uint32_t _tmp$1882 = *(uint32_t*)&_tmp$1883;
      int32_t _tmp$1884;
      moonbit_incref(self$810);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$810, _tmp$1882);
      _tmp$1884 = i$809 + 1;
      i$809 = _tmp$1884;
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
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$2552 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1879;
  int32_t _tmp$1878;
  Moonbit_object_header(_closure$2552)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2552->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$2552->$0 = f$806;
  _tmp$1879 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$2552;
  _tmp$1878 = $$moonbitlang$core$builtin$Iter$$run$0(self$804, _tmp$1879);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$1878, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1880,
  moonbit_string_t k$805
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$1881 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$1880;
  struct $$3c$String$3e$$3d$$3e$Int* _field$2182 = _casted_env$1881->$0;
  int32_t _cnt$2452 = Moonbit_object_header(_casted_env$1881)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$806;
  if (_cnt$2452 > 1) {
    int32_t _new_cnt$2453;
    moonbit_incref(_field$2182);
    _new_cnt$2453 = _cnt$2452 - 1;
    Moonbit_object_header(_casted_env$1881)->rc = _new_cnt$2453;
  } else if (_cnt$2452 == 1) {
    moonbit_free(_casted_env$1881);
  }
  f$806 = _field$2182;
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
  moonbit_string_t* _tmp$1877 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$802);
  moonbit_string_t _tmp$2183;
  if (idx$803 < 0 || idx$803 >= Moonbit_array_length(_tmp$1877)) {
    moonbit_panic();
  }
  _tmp$2183 = (moonbit_string_t)_tmp$1877[idx$803];
  moonbit_incref(_tmp$2183);
  moonbit_decref(_tmp$1877);
  return _tmp$2183;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$800,
  int32_t idx$801
) {
  struct $$3c$String$2a$Int$3e$** _tmp$1876 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$800);
  struct $$3c$String$2a$Int$3e$* _tmp$2184;
  if (idx$801 < 0 || idx$801 >= Moonbit_array_length(_tmp$1876)) {
    moonbit_panic();
  }
  _tmp$2184 = (struct $$3c$String$2a$Int$3e$*)_tmp$1876[idx$801];
  if (_tmp$2184) {
    moonbit_incref(_tmp$2184);
  }
  moonbit_decref(_tmp$1876);
  return _tmp$2184;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$796,
  int32_t key$792
) {
  int32_t hash$791 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$792);
  int32_t capacity_mask$1875 = self$796->$3;
  int32_t _tmp$1874 = hash$791 & capacity_mask$1875;
  int32_t i$793 = 0;
  int32_t idx$794 = _tmp$1874;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2188 =
      self$796->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1873 =
      _field$2188;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2187;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$795;
    if (idx$794 < 0 || idx$794 >= Moonbit_array_length(entries$1873)) {
      moonbit_panic();
    }
    _tmp$2187
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1873[
        idx$794
      ];
    _bind$795 = _tmp$2187;
    if (_bind$795 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1862;
      if (_bind$795) {
        moonbit_incref(_bind$795);
      }
      moonbit_decref(self$796);
      if (_bind$795) {
        moonbit_decref(_bind$795);
      }
      _tmp$1862 = 0;
      return _tmp$1862;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$797 =
        _bind$795;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$798 =
        _Some$797;
      int32_t hash$1864 = _entry$798->$3;
      int32_t _if_result$2554;
      int32_t _field$2185;
      int32_t psl$1867;
      int32_t _tmp$1869;
      int32_t _tmp$1871;
      int32_t capacity_mask$1872;
      int32_t _tmp$1870;
      if (hash$1864 == hash$791) {
        int32_t key$1863 = _entry$798->$4;
        _if_result$2554 = key$1863 == key$792;
      } else {
        _if_result$2554 = 0;
      }
      if (_if_result$2554) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2186;
        int32_t _cnt$2454;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1866;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1865;
        moonbit_incref(_entry$798);
        moonbit_decref(self$796);
        _field$2186 = _entry$798->$5;
        _cnt$2454 = Moonbit_object_header(_entry$798)->rc;
        if (_cnt$2454 > 1) {
          int32_t _new_cnt$2456;
          moonbit_incref(_field$2186);
          _new_cnt$2456 = _cnt$2454 - 1;
          Moonbit_object_header(_entry$798)->rc = _new_cnt$2456;
        } else if (_cnt$2454 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2455 =
            _entry$798->$1;
          if (_field$2455) {
            moonbit_decref(_field$2455);
          }
          moonbit_free(_entry$798);
        }
        value$1866 = _field$2186;
        _tmp$1865 = value$1866;
        return _tmp$1865;
      } else {
        moonbit_incref(_entry$798);
      }
      _field$2185 = _entry$798->$2;
      moonbit_decref(_entry$798);
      psl$1867 = _field$2185;
      if (i$793 > psl$1867) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1868;
        moonbit_decref(self$796);
        _tmp$1868 = 0;
        return _tmp$1868;
      }
      _tmp$1869 = i$793 + 1;
      _tmp$1871 = idx$794 + 1;
      capacity_mask$1872 = self$796->$3;
      _tmp$1870 = _tmp$1871 & capacity_mask$1872;
      i$793 = _tmp$1869;
      idx$794 = _tmp$1870;
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
  int32_t capacity_mask$1861;
  int32_t _tmp$1860;
  int32_t i$784;
  int32_t idx$785;
  moonbit_incref(key$783);
  hash$782 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$783);
  capacity_mask$1861 = self$787->$3;
  _tmp$1860 = hash$782 & capacity_mask$1861;
  i$784 = 0;
  idx$785 = _tmp$1860;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2194 =
      self$787->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1859 =
      _field$2194;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2193;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$786;
    if (idx$785 < 0 || idx$785 >= Moonbit_array_length(entries$1859)) {
      moonbit_panic();
    }
    _tmp$2193
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1859[
        idx$785
      ];
    _bind$786 = _tmp$2193;
    if (_bind$786 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1848;
      if (_bind$786) {
        moonbit_incref(_bind$786);
      }
      moonbit_decref(self$787);
      if (_bind$786) {
        moonbit_decref(_bind$786);
      }
      moonbit_decref(key$783);
      _tmp$1848 = 0;
      return _tmp$1848;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$788 =
        _bind$786;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$789 =
        _Some$788;
      int32_t hash$1850 = _entry$789->$3;
      int32_t _if_result$2556;
      int32_t _field$2189;
      int32_t psl$1853;
      int32_t _tmp$1855;
      int32_t _tmp$1857;
      int32_t capacity_mask$1858;
      int32_t _tmp$1856;
      if (hash$1850 == hash$782) {
        moonbit_string_t _field$2192 = _entry$789->$4;
        moonbit_string_t key$1849 = _field$2192;
        int32_t _tmp$2191 = moonbit_val_array_equal(key$1849, key$783);
        _if_result$2556 = _tmp$2191;
      } else {
        _if_result$2556 = 0;
      }
      if (_if_result$2556) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2190;
        int32_t _cnt$2457;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1852;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1851;
        moonbit_incref(_entry$789);
        moonbit_decref(self$787);
        moonbit_decref(key$783);
        _field$2190 = _entry$789->$5;
        _cnt$2457 = Moonbit_object_header(_entry$789)->rc;
        if (_cnt$2457 > 1) {
          int32_t _new_cnt$2460;
          moonbit_incref(_field$2190);
          _new_cnt$2460 = _cnt$2457 - 1;
          Moonbit_object_header(_entry$789)->rc = _new_cnt$2460;
        } else if (_cnt$2457 == 1) {
          moonbit_string_t _field$2459 = _entry$789->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2458;
          moonbit_decref(_field$2459);
          _field$2458 = _entry$789->$1;
          if (_field$2458) {
            moonbit_decref(_field$2458);
          }
          moonbit_free(_entry$789);
        }
        value$1852 = _field$2190;
        _tmp$1851 = value$1852;
        return _tmp$1851;
      } else {
        moonbit_incref(_entry$789);
      }
      _field$2189 = _entry$789->$2;
      moonbit_decref(_entry$789);
      psl$1853 = _field$2189;
      if (i$784 > psl$1853) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1854;
        moonbit_decref(self$787);
        moonbit_decref(key$783);
        _tmp$1854 = 0;
        return _tmp$1854;
      }
      _tmp$1855 = i$784 + 1;
      _tmp$1857 = idx$785 + 1;
      capacity_mask$1858 = self$787->$3;
      _tmp$1856 = _tmp$1857 & capacity_mask$1858;
      i$784 = _tmp$1855;
      idx$785 = _tmp$1856;
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
  int32_t capacity_mask$1847 = self$778->$3;
  int32_t _tmp$1846 = hash$773 & capacity_mask$1847;
  int32_t i$775 = 0;
  int32_t idx$776 = _tmp$1846;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2198 =
      self$778->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1845 =
      _field$2198;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2197;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$777;
    if (idx$776 < 0 || idx$776 >= Moonbit_array_length(entries$1845)) {
      moonbit_panic();
    }
    _tmp$2197
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1845[
        idx$776
      ];
    _bind$777 = _tmp$2197;
    if (_bind$777 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1834;
      if (_bind$777) {
        moonbit_incref(_bind$777);
      }
      moonbit_decref(self$778);
      if (_bind$777) {
        moonbit_decref(_bind$777);
      }
      _tmp$1834 = 0;
      return _tmp$1834;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$779 =
        _bind$777;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$780 =
        _Some$779;
      int32_t hash$1836 = _entry$780->$3;
      int32_t _if_result$2558;
      int32_t _field$2195;
      int32_t psl$1839;
      int32_t _tmp$1841;
      int32_t _tmp$1843;
      int32_t capacity_mask$1844;
      int32_t _tmp$1842;
      if (hash$1836 == hash$773) {
        int32_t key$1835 = _entry$780->$4;
        _if_result$2558 = key$1835 == key$774;
      } else {
        _if_result$2558 = 0;
      }
      if (_if_result$2558) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2196;
        int32_t _cnt$2461;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1838;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1837;
        moonbit_incref(_entry$780);
        moonbit_decref(self$778);
        _field$2196 = _entry$780->$5;
        _cnt$2461 = Moonbit_object_header(_entry$780)->rc;
        if (_cnt$2461 > 1) {
          int32_t _new_cnt$2463;
          moonbit_incref(_field$2196);
          _new_cnt$2463 = _cnt$2461 - 1;
          Moonbit_object_header(_entry$780)->rc = _new_cnt$2463;
        } else if (_cnt$2461 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2462 =
            _entry$780->$1;
          if (_field$2462) {
            moonbit_decref(_field$2462);
          }
          moonbit_free(_entry$780);
        }
        value$1838 = _field$2196;
        _tmp$1837 = value$1838;
        return _tmp$1837;
      } else {
        moonbit_incref(_entry$780);
      }
      _field$2195 = _entry$780->$2;
      moonbit_decref(_entry$780);
      psl$1839 = _field$2195;
      if (i$775 > psl$1839) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1840;
        moonbit_decref(self$778);
        _tmp$1840 = 0;
        return _tmp$1840;
      }
      _tmp$1841 = i$775 + 1;
      _tmp$1843 = idx$776 + 1;
      capacity_mask$1844 = self$778->$3;
      _tmp$1842 = _tmp$1843 & capacity_mask$1844;
      i$775 = _tmp$1841;
      idx$776 = _tmp$1842;
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
  int32_t capacity_mask$1833;
  int32_t _tmp$1832;
  int32_t i$766;
  int32_t idx$767;
  moonbit_incref(key$765);
  hash$764 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$765);
  capacity_mask$1833 = self$769->$3;
  _tmp$1832 = hash$764 & capacity_mask$1833;
  i$766 = 0;
  idx$767 = _tmp$1832;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2204 =
      self$769->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1831 =
      _field$2204;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2203;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$768;
    if (idx$767 < 0 || idx$767 >= Moonbit_array_length(entries$1831)) {
      moonbit_panic();
    }
    _tmp$2203
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1831[
        idx$767
      ];
    _bind$768 = _tmp$2203;
    if (_bind$768 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1820;
      if (_bind$768) {
        moonbit_incref(_bind$768);
      }
      moonbit_decref(self$769);
      if (_bind$768) {
        moonbit_decref(_bind$768);
      }
      moonbit_decref(key$765);
      _tmp$1820 = 0;
      return _tmp$1820;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$770 =
        _bind$768;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$771 =
        _Some$770;
      int32_t hash$1822 = _entry$771->$3;
      int32_t _if_result$2560;
      int32_t _field$2199;
      int32_t psl$1825;
      int32_t _tmp$1827;
      int32_t _tmp$1829;
      int32_t capacity_mask$1830;
      int32_t _tmp$1828;
      if (hash$1822 == hash$764) {
        moonbit_string_t _field$2202 = _entry$771->$4;
        moonbit_string_t key$1821 = _field$2202;
        int32_t _tmp$2201 = moonbit_val_array_equal(key$1821, key$765);
        _if_result$2560 = _tmp$2201;
      } else {
        _if_result$2560 = 0;
      }
      if (_if_result$2560) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2200;
        int32_t _cnt$2464;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1824;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1823;
        moonbit_incref(_entry$771);
        moonbit_decref(self$769);
        moonbit_decref(key$765);
        _field$2200 = _entry$771->$5;
        _cnt$2464 = Moonbit_object_header(_entry$771)->rc;
        if (_cnt$2464 > 1) {
          int32_t _new_cnt$2467;
          moonbit_incref(_field$2200);
          _new_cnt$2467 = _cnt$2464 - 1;
          Moonbit_object_header(_entry$771)->rc = _new_cnt$2467;
        } else if (_cnt$2464 == 1) {
          moonbit_string_t _field$2466 = _entry$771->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2465;
          moonbit_decref(_field$2466);
          _field$2465 = _entry$771->$1;
          if (_field$2465) {
            moonbit_decref(_field$2465);
          }
          moonbit_free(_entry$771);
        }
        value$1824 = _field$2200;
        _tmp$1823 = value$1824;
        return _tmp$1823;
      } else {
        moonbit_incref(_entry$771);
      }
      _field$2199 = _entry$771->$2;
      moonbit_decref(_entry$771);
      psl$1825 = _field$2199;
      if (i$766 > psl$1825) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1826;
        moonbit_decref(self$769);
        moonbit_decref(key$765);
        _tmp$1826 = 0;
        return _tmp$1826;
      }
      _tmp$1827 = i$766 + 1;
      _tmp$1829 = idx$767 + 1;
      capacity_mask$1830 = self$769->$3;
      _tmp$1828 = _tmp$1829 & capacity_mask$1830;
      i$766 = _tmp$1827;
      idx$767 = _tmp$1828;
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
  int32_t capacity_mask$1819 = self$760->$3;
  int32_t _tmp$1818 = hash$755 & capacity_mask$1819;
  int32_t i$757 = 0;
  int32_t idx$758 = _tmp$1818;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2208 =
      self$760->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1817 =
      _field$2208;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2207;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$759;
    if (idx$758 < 0 || idx$758 >= Moonbit_array_length(entries$1817)) {
      moonbit_panic();
    }
    _tmp$2207
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1817[
        idx$758
      ];
    _bind$759 = _tmp$2207;
    if (_bind$759 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1806;
      if (_bind$759) {
        moonbit_incref(_bind$759);
      }
      moonbit_decref(self$760);
      if (_bind$759) {
        moonbit_decref(_bind$759);
      }
      _tmp$1806 = 0;
      return _tmp$1806;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$761 =
        _bind$759;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$762 =
        _Some$761;
      int32_t hash$1808 = _entry$762->$3;
      int32_t _if_result$2562;
      int32_t _field$2205;
      int32_t psl$1811;
      int32_t _tmp$1813;
      int32_t _tmp$1815;
      int32_t capacity_mask$1816;
      int32_t _tmp$1814;
      if (hash$1808 == hash$755) {
        int32_t key$1807 = _entry$762->$4;
        _if_result$2562 = key$1807 == key$756;
      } else {
        _if_result$2562 = 0;
      }
      if (_if_result$2562) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2206;
        int32_t _cnt$2468;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1810;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1809;
        moonbit_incref(_entry$762);
        moonbit_decref(self$760);
        _field$2206 = _entry$762->$5;
        _cnt$2468 = Moonbit_object_header(_entry$762)->rc;
        if (_cnt$2468 > 1) {
          int32_t _new_cnt$2470;
          moonbit_incref(_field$2206);
          _new_cnt$2470 = _cnt$2468 - 1;
          Moonbit_object_header(_entry$762)->rc = _new_cnt$2470;
        } else if (_cnt$2468 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2469 =
            _entry$762->$1;
          if (_field$2469) {
            moonbit_decref(_field$2469);
          }
          moonbit_free(_entry$762);
        }
        value$1810 = _field$2206;
        _tmp$1809 = value$1810;
        return _tmp$1809;
      } else {
        moonbit_incref(_entry$762);
      }
      _field$2205 = _entry$762->$2;
      moonbit_decref(_entry$762);
      psl$1811 = _field$2205;
      if (i$757 > psl$1811) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1812;
        moonbit_decref(self$760);
        _tmp$1812 = 0;
        return _tmp$1812;
      }
      _tmp$1813 = i$757 + 1;
      _tmp$1815 = idx$758 + 1;
      capacity_mask$1816 = self$760->$3;
      _tmp$1814 = _tmp$1815 & capacity_mask$1816;
      i$757 = _tmp$1813;
      idx$758 = _tmp$1814;
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
  int32_t capacity_mask$1805;
  int32_t _tmp$1804;
  int32_t i$748;
  int32_t idx$749;
  moonbit_incref(key$747);
  hash$746 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$747);
  capacity_mask$1805 = self$751->$3;
  _tmp$1804 = hash$746 & capacity_mask$1805;
  i$748 = 0;
  idx$749 = _tmp$1804;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2214 =
      self$751->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1803 =
      _field$2214;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2213;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$750;
    if (idx$749 < 0 || idx$749 >= Moonbit_array_length(entries$1803)) {
      moonbit_panic();
    }
    _tmp$2213
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1803[
        idx$749
      ];
    _bind$750 = _tmp$2213;
    if (_bind$750 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1792;
      if (_bind$750) {
        moonbit_incref(_bind$750);
      }
      moonbit_decref(self$751);
      if (_bind$750) {
        moonbit_decref(_bind$750);
      }
      moonbit_decref(key$747);
      _tmp$1792 = 0;
      return _tmp$1792;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$752 =
        _bind$750;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$753 =
        _Some$752;
      int32_t hash$1794 = _entry$753->$3;
      int32_t _if_result$2564;
      int32_t _field$2209;
      int32_t psl$1797;
      int32_t _tmp$1799;
      int32_t _tmp$1801;
      int32_t capacity_mask$1802;
      int32_t _tmp$1800;
      if (hash$1794 == hash$746) {
        moonbit_string_t _field$2212 = _entry$753->$4;
        moonbit_string_t key$1793 = _field$2212;
        int32_t _tmp$2211 = moonbit_val_array_equal(key$1793, key$747);
        _if_result$2564 = _tmp$2211;
      } else {
        _if_result$2564 = 0;
      }
      if (_if_result$2564) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2210;
        int32_t _cnt$2471;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1796;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1795;
        moonbit_incref(_entry$753);
        moonbit_decref(self$751);
        moonbit_decref(key$747);
        _field$2210 = _entry$753->$5;
        _cnt$2471 = Moonbit_object_header(_entry$753)->rc;
        if (_cnt$2471 > 1) {
          int32_t _new_cnt$2474;
          moonbit_incref(_field$2210);
          _new_cnt$2474 = _cnt$2471 - 1;
          Moonbit_object_header(_entry$753)->rc = _new_cnt$2474;
        } else if (_cnt$2471 == 1) {
          moonbit_string_t _field$2473 = _entry$753->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2472;
          moonbit_decref(_field$2473);
          _field$2472 = _entry$753->$1;
          if (_field$2472) {
            moonbit_decref(_field$2472);
          }
          moonbit_free(_entry$753);
        }
        value$1796 = _field$2210;
        _tmp$1795 = value$1796;
        return _tmp$1795;
      } else {
        moonbit_incref(_entry$753);
      }
      _field$2209 = _entry$753->$2;
      moonbit_decref(_entry$753);
      psl$1797 = _field$2209;
      if (i$748 > psl$1797) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1798;
        moonbit_decref(self$751);
        moonbit_decref(key$747);
        _tmp$1798 = 0;
        return _tmp$1798;
      }
      _tmp$1799 = i$748 + 1;
      _tmp$1801 = idx$749 + 1;
      capacity_mask$1802 = self$751->$3;
      _tmp$1800 = _tmp$1801 & capacity_mask$1802;
      i$748 = _tmp$1799;
      idx$749 = _tmp$1800;
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
  int32_t _tmp$1783;
  int32_t _tmp$1782;
  int32_t _tmp$1791;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$741;
  int32_t _len$742;
  int32_t _i$743;
  moonbit_incref(arr$739.$0);
  length$738 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$739);
  capacity$740 = $Int$$next_power_of_two(length$738);
  _tmp$1783 = capacity$740;
  _tmp$1782 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1783);
  if (length$738 > _tmp$1782) {
    int32_t _tmp$1784 = capacity$740;
    capacity$740 = _tmp$1784 * 2;
  }
  _tmp$1791 = capacity$740;
  m$741 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$1791);
  moonbit_incref(arr$739.$0);
  _len$742 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$739);
  _i$743 = 0;
  while (1) {
    if (_i$743 < _len$742) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2218 =
        arr$739.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1787 =
        _field$2218;
      int32_t start$1789 = arr$739.$1;
      int32_t _tmp$1788 = start$1789 + _i$743;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2217 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1787[
          _tmp$1788
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$744 =
        _tmp$2217;
      moonbit_string_t _field$2216 = e$744->$0;
      moonbit_string_t _tmp$1785 = _field$2216;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2215 =
        e$744->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1786 =
        _field$2215;
      int32_t _tmp$1790;
      moonbit_incref(_tmp$1786);
      moonbit_incref(_tmp$1785);
      moonbit_incref(m$741);
      $$moonbitlang$core$builtin$Map$$set$3(m$741, _tmp$1785, _tmp$1786);
      _tmp$1790 = _i$743 + 1;
      _i$743 = _tmp$1790;
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
  int32_t _tmp$1773;
  int32_t _tmp$1772;
  int32_t _tmp$1781;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$733;
  int32_t _len$734;
  int32_t _i$735;
  moonbit_incref(arr$731.$0);
  length$730 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$731);
  capacity$732 = $Int$$next_power_of_two(length$730);
  _tmp$1773 = capacity$732;
  _tmp$1772 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1773);
  if (length$730 > _tmp$1772) {
    int32_t _tmp$1774 = capacity$732;
    capacity$732 = _tmp$1774 * 2;
  }
  _tmp$1781 = capacity$732;
  m$733 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$1781);
  moonbit_incref(arr$731.$0);
  _len$734 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$731);
  _i$735 = 0;
  while (1) {
    if (_i$735 < _len$734) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2222 =
        arr$731.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1777 =
        _field$2222;
      int32_t start$1779 = arr$731.$1;
      int32_t _tmp$1778 = start$1779 + _i$735;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2221 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1777[
          _tmp$1778
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$736 =
        _tmp$2221;
      moonbit_string_t _field$2220 = e$736->$0;
      moonbit_string_t _tmp$1775 = _field$2220;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2219 =
        e$736->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1776 =
        _field$2219;
      int32_t _tmp$1780;
      moonbit_incref(_tmp$1776);
      moonbit_incref(_tmp$1775);
      moonbit_incref(m$733);
      $$moonbitlang$core$builtin$Map$$set$2(m$733, _tmp$1775, _tmp$1776);
      _tmp$1780 = _i$735 + 1;
      _i$735 = _tmp$1780;
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
  int32_t _tmp$1763;
  int32_t _tmp$1762;
  int32_t _tmp$1771;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$725;
  int32_t _len$726;
  int32_t _i$727;
  moonbit_incref(arr$723.$0);
  length$722 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$723);
  capacity$724 = $Int$$next_power_of_two(length$722);
  _tmp$1763 = capacity$724;
  _tmp$1762 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1763);
  if (length$722 > _tmp$1762) {
    int32_t _tmp$1764 = capacity$724;
    capacity$724 = _tmp$1764 * 2;
  }
  _tmp$1771 = capacity$724;
  m$725 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$1771);
  moonbit_incref(arr$723.$0);
  _len$726 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$723);
  _i$727 = 0;
  while (1) {
    if (_i$727 < _len$726) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2225 =
        arr$723.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$1767 =
        _field$2225;
      int32_t start$1769 = arr$723.$1;
      int32_t _tmp$1768 = start$1769 + _i$727;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2224 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$1767[
          _tmp$1768
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$728 =
        _tmp$2224;
      int32_t _tmp$1765 = e$728->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2223 =
        e$728->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1766 =
        _field$2223;
      int32_t _tmp$1770;
      moonbit_incref(_tmp$1766);
      moonbit_incref(m$725);
      $$moonbitlang$core$builtin$Map$$set$1(m$725, _tmp$1765, _tmp$1766);
      _tmp$1770 = _i$727 + 1;
      _i$727 = _tmp$1770;
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
  int32_t _tmp$1753;
  int32_t _tmp$1752;
  int32_t _tmp$1761;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$717;
  int32_t _len$718;
  int32_t _i$719;
  moonbit_incref(arr$715.$0);
  length$714 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$715);
  capacity$716 = $Int$$next_power_of_two(length$714);
  _tmp$1753 = capacity$716;
  _tmp$1752 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1753);
  if (length$714 > _tmp$1752) {
    int32_t _tmp$1754 = capacity$716;
    capacity$716 = _tmp$1754 * 2;
  }
  _tmp$1761 = capacity$716;
  m$717 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$1761);
  moonbit_incref(arr$715.$0);
  _len$718 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$715);
  _i$719 = 0;
  while (1) {
    if (_i$719 < _len$718) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2229 =
        arr$715.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1757 =
        _field$2229;
      int32_t start$1759 = arr$715.$1;
      int32_t _tmp$1758 = start$1759 + _i$719;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2228 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1757[
          _tmp$1758
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$720 =
        _tmp$2228;
      moonbit_string_t _field$2227 = e$720->$0;
      moonbit_string_t _tmp$1755 = _field$2227;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2226 =
        e$720->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1756 =
        _field$2226;
      int32_t _tmp$1760;
      moonbit_incref(_tmp$1756);
      moonbit_incref(_tmp$1755);
      moonbit_incref(m$717);
      $$moonbitlang$core$builtin$Map$$set$0(m$717, _tmp$1755, _tmp$1756);
      _tmp$1760 = _i$719 + 1;
      _i$719 = _tmp$1760;
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
  int32_t _tmp$1751;
  moonbit_incref(key$712);
  _tmp$1751 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$712);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$711, key$712, value$713, _tmp$1751
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$708,
  moonbit_string_t key$709,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$710
) {
  int32_t _tmp$1750;
  moonbit_incref(key$709);
  _tmp$1750 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$709);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$708, key$709, value$710, _tmp$1750
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$705,
  int32_t key$706,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$707
) {
  int32_t _tmp$1749 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$706);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$705, key$706, value$707, _tmp$1749
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$702,
  moonbit_string_t key$703,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$704
) {
  int32_t _tmp$1748;
  moonbit_incref(key$703);
  _tmp$1748 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$703);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$702, key$703, value$704, _tmp$1748
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$692
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2236 =
    self$692->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$691 =
    _field$2236;
  int32_t capacity$1747 = self$692->$2;
  int32_t new_capacity$693 = capacity$1747 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1742 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1741 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$693, _tmp$1742
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2235 =
    self$692->$0;
  int32_t _tmp$1743;
  int32_t capacity$1745;
  int32_t _tmp$1744;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1746;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2234;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$694;
  if (old_head$691) {
    moonbit_incref(old_head$691);
  }
  moonbit_decref(_old$2235);
  self$692->$0 = _tmp$1741;
  self$692->$2 = new_capacity$693;
  _tmp$1743 = new_capacity$693 - 1;
  self$692->$3 = _tmp$1743;
  capacity$1745 = self$692->$2;
  _tmp$1744 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1745);
  self$692->$4 = _tmp$1744;
  self$692->$1 = 0;
  _tmp$1746 = 0;
  _old$2234 = self$692->$5;
  if (_old$2234) {
    moonbit_decref(_old$2234);
  }
  self$692->$5 = _tmp$1746;
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
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2233 =
        _x$696->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$697 =
        _field$2233;
      moonbit_string_t _field$2232 = _x$696->$4;
      moonbit_string_t _key$698 = _field$2232;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2231 =
        _x$696->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$699 =
        _field$2231;
      int32_t _field$2230 = _x$696->$3;
      int32_t _cnt$2475 = Moonbit_object_header(_x$696)->rc;
      int32_t _hash$700;
      if (_cnt$2475 > 1) {
        int32_t _new_cnt$2476;
        moonbit_incref(_value$699);
        moonbit_incref(_key$698);
        if (_next$697) {
          moonbit_incref(_next$697);
        }
        _new_cnt$2476 = _cnt$2475 - 1;
        Moonbit_object_header(_x$696)->rc = _new_cnt$2476;
      } else if (_cnt$2475 == 1) {
        moonbit_free(_x$696);
      }
      _hash$700 = _field$2230;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2243 =
    self$681->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$680 =
    _field$2243;
  int32_t capacity$1740 = self$681->$2;
  int32_t new_capacity$682 = capacity$1740 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1735 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1734 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$682, _tmp$1735
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2242 =
    self$681->$0;
  int32_t _tmp$1736;
  int32_t capacity$1738;
  int32_t _tmp$1737;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1739;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2241;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$683;
  if (old_head$680) {
    moonbit_incref(old_head$680);
  }
  moonbit_decref(_old$2242);
  self$681->$0 = _tmp$1734;
  self$681->$2 = new_capacity$682;
  _tmp$1736 = new_capacity$682 - 1;
  self$681->$3 = _tmp$1736;
  capacity$1738 = self$681->$2;
  _tmp$1737 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1738);
  self$681->$4 = _tmp$1737;
  self$681->$1 = 0;
  _tmp$1739 = 0;
  _old$2241 = self$681->$5;
  if (_old$2241) {
    moonbit_decref(_old$2241);
  }
  self$681->$5 = _tmp$1739;
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
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2240 =
        _x$685->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$686 =
        _field$2240;
      moonbit_string_t _field$2239 = _x$685->$4;
      moonbit_string_t _key$687 = _field$2239;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2238 =
        _x$685->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$688 =
        _field$2238;
      int32_t _field$2237 = _x$685->$3;
      int32_t _cnt$2477 = Moonbit_object_header(_x$685)->rc;
      int32_t _hash$689;
      if (_cnt$2477 > 1) {
        int32_t _new_cnt$2478;
        moonbit_incref(_value$688);
        moonbit_incref(_key$687);
        if (_next$686) {
          moonbit_incref(_next$686);
        }
        _new_cnt$2478 = _cnt$2477 - 1;
        Moonbit_object_header(_x$685)->rc = _new_cnt$2478;
      } else if (_cnt$2477 == 1) {
        moonbit_free(_x$685);
      }
      _hash$689 = _field$2237;
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
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2249 =
    self$670->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$669 =
    _field$2249;
  int32_t capacity$1733 = self$670->$2;
  int32_t new_capacity$671 = capacity$1733 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1728 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1727 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$671, _tmp$1728
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$2248 =
    self$670->$0;
  int32_t _tmp$1729;
  int32_t capacity$1731;
  int32_t _tmp$1730;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1732;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2247;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$672;
  if (old_head$669) {
    moonbit_incref(old_head$669);
  }
  moonbit_decref(_old$2248);
  self$670->$0 = _tmp$1727;
  self$670->$2 = new_capacity$671;
  _tmp$1729 = new_capacity$671 - 1;
  self$670->$3 = _tmp$1729;
  capacity$1731 = self$670->$2;
  _tmp$1730 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1731);
  self$670->$4 = _tmp$1730;
  self$670->$1 = 0;
  _tmp$1732 = 0;
  _old$2247 = self$670->$5;
  if (_old$2247) {
    moonbit_decref(_old$2247);
  }
  self$670->$5 = _tmp$1732;
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
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2246 =
        _x$674->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$675 =
        _field$2246;
      int32_t _key$676 = _x$674->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2245 =
        _x$674->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$677 =
        _field$2245;
      int32_t _field$2244 = _x$674->$3;
      int32_t _cnt$2479 = Moonbit_object_header(_x$674)->rc;
      int32_t _hash$678;
      if (_cnt$2479 > 1) {
        int32_t _new_cnt$2480;
        moonbit_incref(_value$677);
        if (_next$675) {
          moonbit_incref(_next$675);
        }
        _new_cnt$2480 = _cnt$2479 - 1;
        Moonbit_object_header(_x$674)->rc = _new_cnt$2480;
      } else if (_cnt$2479 == 1) {
        moonbit_free(_x$674);
      }
      _hash$678 = _field$2244;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2256 =
    self$659->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$658 =
    _field$2256;
  int32_t capacity$1726 = self$659->$2;
  int32_t new_capacity$660 = capacity$1726 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1721 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1720 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$660, _tmp$1721
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2255 =
    self$659->$0;
  int32_t _tmp$1722;
  int32_t capacity$1724;
  int32_t _tmp$1723;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1725;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2254;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$661;
  if (old_head$658) {
    moonbit_incref(old_head$658);
  }
  moonbit_decref(_old$2255);
  self$659->$0 = _tmp$1720;
  self$659->$2 = new_capacity$660;
  _tmp$1722 = new_capacity$660 - 1;
  self$659->$3 = _tmp$1722;
  capacity$1724 = self$659->$2;
  _tmp$1723 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1724);
  self$659->$4 = _tmp$1723;
  self$659->$1 = 0;
  _tmp$1725 = 0;
  _old$2254 = self$659->$5;
  if (_old$2254) {
    moonbit_decref(_old$2254);
  }
  self$659->$5 = _tmp$1725;
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
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2253 =
        _x$663->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$664 =
        _field$2253;
      moonbit_string_t _field$2252 = _x$663->$4;
      moonbit_string_t _key$665 = _field$2252;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2251 =
        _x$663->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$666 =
        _field$2251;
      int32_t _field$2250 = _x$663->$3;
      int32_t _cnt$2481 = Moonbit_object_header(_x$663)->rc;
      int32_t _hash$667;
      if (_cnt$2481 > 1) {
        int32_t _new_cnt$2482;
        moonbit_incref(_value$666);
        moonbit_incref(_key$665);
        if (_next$664) {
          moonbit_incref(_next$664);
        }
        _new_cnt$2482 = _cnt$2481 - 1;
        Moonbit_object_header(_x$663)->rc = _new_cnt$2482;
      } else if (_cnt$2481 == 1) {
        moonbit_free(_x$663);
      }
      _hash$667 = _field$2250;
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
  int32_t size$1706 = self$642->$1;
  int32_t grow_at$1707 = self$642->$4;
  int32_t capacity_mask$1719;
  int32_t _tmp$1718;
  struct $$3c$Int$2a$Int$3e$* _bind$643;
  int32_t psl$644;
  int32_t idx$645;
  int32_t _idx$653;
  int32_t _field$2257;
  int32_t _psl$654;
  int32_t _bind$655;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$656;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$657;
  if (size$1706 >= grow_at$1707) {
    moonbit_incref(self$642);
    $$moonbitlang$core$builtin$Map$$grow$3(self$642);
  }
  capacity_mask$1719 = self$642->$3;
  _tmp$1718 = hash$650 & capacity_mask$1719;
  psl$644 = 0;
  idx$645 = _tmp$1718;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2262 =
      self$642->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1717 =
      _field$2262;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2261;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$646;
    if (idx$645 < 0 || idx$645 >= Moonbit_array_length(entries$1717)) {
      moonbit_panic();
    }
    _tmp$2261
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1717[
        idx$645
      ];
    _bind$646 = _tmp$2261;
    if (_bind$646 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1708 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1708)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1708->$0 = idx$645;
      _tuple$1708->$1 = psl$644;
      _bind$643 = _tuple$1708;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$648 =
        _bind$646;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$649 =
        _Some$648;
      int32_t hash$1710 = _curr_entry$649->$3;
      int32_t _if_result$2574;
      int32_t psl$1711;
      int32_t _tmp$1713;
      int32_t _tmp$1715;
      int32_t capacity_mask$1716;
      int32_t _tmp$1714;
      if (hash$1710 == hash$650) {
        moonbit_string_t _field$2260 = _curr_entry$649->$4;
        moonbit_string_t key$1709 = _field$2260;
        int32_t _tmp$2259 = moonbit_val_array_equal(key$1709, key$651);
        _if_result$2574 = _tmp$2259;
      } else {
        _if_result$2574 = 0;
      }
      if (_if_result$2574) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2258;
        moonbit_incref(_curr_entry$649);
        moonbit_decref(key$651);
        moonbit_decref(self$642);
        _old$2258 = _curr_entry$649->$5;
        moonbit_decref(_old$2258);
        _curr_entry$649->$5 = value$652;
        moonbit_decref(_curr_entry$649);
        return 0;
      } else {
        moonbit_incref(_curr_entry$649);
      }
      psl$1711 = _curr_entry$649->$2;
      if (psl$644 > psl$1711) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1712;
        moonbit_incref(self$642);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$642, idx$645, _curr_entry$649
        );
        _tuple$1712
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1712)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1712->$0 = idx$645;
        _tuple$1712->$1 = psl$644;
        _bind$643 = _tuple$1712;
        break;
      } else {
        moonbit_decref(_curr_entry$649);
      }
      _tmp$1713 = psl$644 + 1;
      _tmp$1715 = idx$645 + 1;
      capacity_mask$1716 = self$642->$3;
      _tmp$1714 = _tmp$1715 & capacity_mask$1716;
      psl$644 = _tmp$1713;
      idx$645 = _tmp$1714;
      continue;
    }
    break;
  }
  _idx$653 = _bind$643->$0;
  _field$2257 = _bind$643->$1;
  moonbit_decref(_bind$643);
  _psl$654 = _field$2257;
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
  int32_t size$1692 = self$626->$1;
  int32_t grow_at$1693 = self$626->$4;
  int32_t capacity_mask$1705;
  int32_t _tmp$1704;
  struct $$3c$Int$2a$Int$3e$* _bind$627;
  int32_t psl$628;
  int32_t idx$629;
  int32_t _idx$637;
  int32_t _field$2263;
  int32_t _psl$638;
  int32_t _bind$639;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$640;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$641;
  if (size$1692 >= grow_at$1693) {
    moonbit_incref(self$626);
    $$moonbitlang$core$builtin$Map$$grow$2(self$626);
  }
  capacity_mask$1705 = self$626->$3;
  _tmp$1704 = hash$634 & capacity_mask$1705;
  psl$628 = 0;
  idx$629 = _tmp$1704;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2268 =
      self$626->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1703 =
      _field$2268;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2267;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$630;
    if (idx$629 < 0 || idx$629 >= Moonbit_array_length(entries$1703)) {
      moonbit_panic();
    }
    _tmp$2267
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1703[
        idx$629
      ];
    _bind$630 = _tmp$2267;
    if (_bind$630 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1694 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1694)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1694->$0 = idx$629;
      _tuple$1694->$1 = psl$628;
      _bind$627 = _tuple$1694;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$632 =
        _bind$630;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$633 =
        _Some$632;
      int32_t hash$1696 = _curr_entry$633->$3;
      int32_t _if_result$2576;
      int32_t psl$1697;
      int32_t _tmp$1699;
      int32_t _tmp$1701;
      int32_t capacity_mask$1702;
      int32_t _tmp$1700;
      if (hash$1696 == hash$634) {
        moonbit_string_t _field$2266 = _curr_entry$633->$4;
        moonbit_string_t key$1695 = _field$2266;
        int32_t _tmp$2265 = moonbit_val_array_equal(key$1695, key$635);
        _if_result$2576 = _tmp$2265;
      } else {
        _if_result$2576 = 0;
      }
      if (_if_result$2576) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2264;
        moonbit_incref(_curr_entry$633);
        moonbit_decref(key$635);
        moonbit_decref(self$626);
        _old$2264 = _curr_entry$633->$5;
        moonbit_decref(_old$2264);
        _curr_entry$633->$5 = value$636;
        moonbit_decref(_curr_entry$633);
        return 0;
      } else {
        moonbit_incref(_curr_entry$633);
      }
      psl$1697 = _curr_entry$633->$2;
      if (psl$628 > psl$1697) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1698;
        moonbit_incref(self$626);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$626, idx$629, _curr_entry$633
        );
        _tuple$1698
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1698)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1698->$0 = idx$629;
        _tuple$1698->$1 = psl$628;
        _bind$627 = _tuple$1698;
        break;
      } else {
        moonbit_decref(_curr_entry$633);
      }
      _tmp$1699 = psl$628 + 1;
      _tmp$1701 = idx$629 + 1;
      capacity_mask$1702 = self$626->$3;
      _tmp$1700 = _tmp$1701 & capacity_mask$1702;
      psl$628 = _tmp$1699;
      idx$629 = _tmp$1700;
      continue;
    }
    break;
  }
  _idx$637 = _bind$627->$0;
  _field$2263 = _bind$627->$1;
  moonbit_decref(_bind$627);
  _psl$638 = _field$2263;
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
  int32_t size$1678 = self$610->$1;
  int32_t grow_at$1679 = self$610->$4;
  int32_t capacity_mask$1691;
  int32_t _tmp$1690;
  struct $$3c$Int$2a$Int$3e$* _bind$611;
  int32_t psl$612;
  int32_t idx$613;
  int32_t _idx$621;
  int32_t _field$2269;
  int32_t _psl$622;
  int32_t _bind$623;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$624;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$625;
  if (size$1678 >= grow_at$1679) {
    moonbit_incref(self$610);
    $$moonbitlang$core$builtin$Map$$grow$1(self$610);
  }
  capacity_mask$1691 = self$610->$3;
  _tmp$1690 = hash$618 & capacity_mask$1691;
  psl$612 = 0;
  idx$613 = _tmp$1690;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2272 =
      self$610->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1689 =
      _field$2272;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2271;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$614;
    if (idx$613 < 0 || idx$613 >= Moonbit_array_length(entries$1689)) {
      moonbit_panic();
    }
    _tmp$2271
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1689[
        idx$613
      ];
    _bind$614 = _tmp$2271;
    if (_bind$614 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1680 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1680)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1680->$0 = idx$613;
      _tuple$1680->$1 = psl$612;
      _bind$611 = _tuple$1680;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$616 =
        _bind$614;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$617 =
        _Some$616;
      int32_t hash$1682 = _curr_entry$617->$3;
      int32_t _if_result$2578;
      int32_t psl$1683;
      int32_t _tmp$1685;
      int32_t _tmp$1687;
      int32_t capacity_mask$1688;
      int32_t _tmp$1686;
      if (hash$1682 == hash$618) {
        int32_t key$1681 = _curr_entry$617->$4;
        _if_result$2578 = key$1681 == key$619;
      } else {
        _if_result$2578 = 0;
      }
      if (_if_result$2578) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$2270;
        moonbit_incref(_curr_entry$617);
        moonbit_decref(self$610);
        _old$2270 = _curr_entry$617->$5;
        moonbit_decref(_old$2270);
        _curr_entry$617->$5 = value$620;
        moonbit_decref(_curr_entry$617);
        return 0;
      } else {
        moonbit_incref(_curr_entry$617);
      }
      psl$1683 = _curr_entry$617->$2;
      if (psl$612 > psl$1683) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1684;
        moonbit_incref(self$610);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$610, idx$613, _curr_entry$617
        );
        _tuple$1684
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1684)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1684->$0 = idx$613;
        _tuple$1684->$1 = psl$612;
        _bind$611 = _tuple$1684;
        break;
      } else {
        moonbit_decref(_curr_entry$617);
      }
      _tmp$1685 = psl$612 + 1;
      _tmp$1687 = idx$613 + 1;
      capacity_mask$1688 = self$610->$3;
      _tmp$1686 = _tmp$1687 & capacity_mask$1688;
      psl$612 = _tmp$1685;
      idx$613 = _tmp$1686;
      continue;
    }
    break;
  }
  _idx$621 = _bind$611->$0;
  _field$2269 = _bind$611->$1;
  moonbit_decref(_bind$611);
  _psl$622 = _field$2269;
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
  int32_t size$1664 = self$594->$1;
  int32_t grow_at$1665 = self$594->$4;
  int32_t capacity_mask$1677;
  int32_t _tmp$1676;
  struct $$3c$Int$2a$Int$3e$* _bind$595;
  int32_t psl$596;
  int32_t idx$597;
  int32_t _idx$605;
  int32_t _field$2273;
  int32_t _psl$606;
  int32_t _bind$607;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$608;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$609;
  if (size$1664 >= grow_at$1665) {
    moonbit_incref(self$594);
    $$moonbitlang$core$builtin$Map$$grow$0(self$594);
  }
  capacity_mask$1677 = self$594->$3;
  _tmp$1676 = hash$602 & capacity_mask$1677;
  psl$596 = 0;
  idx$597 = _tmp$1676;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2278 =
      self$594->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1675 =
      _field$2278;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2277;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$598;
    if (idx$597 < 0 || idx$597 >= Moonbit_array_length(entries$1675)) {
      moonbit_panic();
    }
    _tmp$2277
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1675[
        idx$597
      ];
    _bind$598 = _tmp$2277;
    if (_bind$598 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1666 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1666)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1666->$0 = idx$597;
      _tuple$1666->$1 = psl$596;
      _bind$595 = _tuple$1666;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$600 =
        _bind$598;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$601 =
        _Some$600;
      int32_t hash$1668 = _curr_entry$601->$3;
      int32_t _if_result$2580;
      int32_t psl$1669;
      int32_t _tmp$1671;
      int32_t _tmp$1673;
      int32_t capacity_mask$1674;
      int32_t _tmp$1672;
      if (hash$1668 == hash$602) {
        moonbit_string_t _field$2276 = _curr_entry$601->$4;
        moonbit_string_t key$1667 = _field$2276;
        int32_t _tmp$2275 = moonbit_val_array_equal(key$1667, key$603);
        _if_result$2580 = _tmp$2275;
      } else {
        _if_result$2580 = 0;
      }
      if (_if_result$2580) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2274;
        moonbit_incref(_curr_entry$601);
        moonbit_decref(key$603);
        moonbit_decref(self$594);
        _old$2274 = _curr_entry$601->$5;
        moonbit_decref(_old$2274);
        _curr_entry$601->$5 = value$604;
        moonbit_decref(_curr_entry$601);
        return 0;
      } else {
        moonbit_incref(_curr_entry$601);
      }
      psl$1669 = _curr_entry$601->$2;
      if (psl$596 > psl$1669) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1670;
        moonbit_incref(self$594);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$594, idx$597, _curr_entry$601
        );
        _tuple$1670
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1670)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1670->$0 = idx$597;
        _tuple$1670->$1 = psl$596;
        _bind$595 = _tuple$1670;
        break;
      } else {
        moonbit_decref(_curr_entry$601);
      }
      _tmp$1671 = psl$596 + 1;
      _tmp$1673 = idx$597 + 1;
      capacity_mask$1674 = self$594->$3;
      _tmp$1672 = _tmp$1673 & capacity_mask$1674;
      psl$596 = _tmp$1671;
      idx$597 = _tmp$1672;
      continue;
    }
    break;
  }
  _idx$605 = _bind$595->$0;
  _field$2273 = _bind$595->$1;
  moonbit_decref(_bind$595);
  _psl$606 = _field$2273;
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
  int32_t psl$1663 = entry$592->$2;
  int32_t _tmp$1659 = psl$1663 + 1;
  int32_t _tmp$1661 = idx$593 + 1;
  int32_t capacity_mask$1662 = self$588->$3;
  int32_t _tmp$1660 = _tmp$1661 & capacity_mask$1662;
  int32_t psl$584 = _tmp$1659;
  int32_t idx$585 = _tmp$1660;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$586 =
    entry$592;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2280 =
      self$588->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1658 =
      _field$2280;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2279;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$587;
    if (idx$585 < 0 || idx$585 >= Moonbit_array_length(entries$1658)) {
      moonbit_panic();
    }
    _tmp$2279
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1658[
        idx$585
      ];
    _bind$587 = _tmp$2279;
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
      int32_t psl$1648 = _curr_entry$591->$2;
      if (psl$584 > psl$1648) {
        int32_t psl$1653;
        int32_t _tmp$1649;
        int32_t _tmp$1651;
        int32_t capacity_mask$1652;
        int32_t _tmp$1650;
        entry$586->$2 = psl$584;
        moonbit_incref(_curr_entry$591);
        moonbit_incref(self$588);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$588, entry$586, idx$585
        );
        psl$1653 = _curr_entry$591->$2;
        _tmp$1649 = psl$1653 + 1;
        _tmp$1651 = idx$585 + 1;
        capacity_mask$1652 = self$588->$3;
        _tmp$1650 = _tmp$1651 & capacity_mask$1652;
        psl$584 = _tmp$1649;
        idx$585 = _tmp$1650;
        entry$586 = _curr_entry$591;
        continue;
      } else {
        int32_t _tmp$1654 = psl$584 + 1;
        int32_t _tmp$1656 = idx$585 + 1;
        int32_t capacity_mask$1657 = self$588->$3;
        int32_t _tmp$1655 = _tmp$1656 & capacity_mask$1657;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2582 =
          entry$586;
        psl$584 = _tmp$1654;
        idx$585 = _tmp$1655;
        entry$586 = _tmp$2582;
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
  int32_t psl$1647 = entry$582->$2;
  int32_t _tmp$1643 = psl$1647 + 1;
  int32_t _tmp$1645 = idx$583 + 1;
  int32_t capacity_mask$1646 = self$578->$3;
  int32_t _tmp$1644 = _tmp$1645 & capacity_mask$1646;
  int32_t psl$574 = _tmp$1643;
  int32_t idx$575 = _tmp$1644;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$576 =
    entry$582;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2282 =
      self$578->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1642 =
      _field$2282;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2281;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$577;
    if (idx$575 < 0 || idx$575 >= Moonbit_array_length(entries$1642)) {
      moonbit_panic();
    }
    _tmp$2281
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1642[
        idx$575
      ];
    _bind$577 = _tmp$2281;
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
      int32_t psl$1632 = _curr_entry$581->$2;
      if (psl$574 > psl$1632) {
        int32_t psl$1637;
        int32_t _tmp$1633;
        int32_t _tmp$1635;
        int32_t capacity_mask$1636;
        int32_t _tmp$1634;
        entry$576->$2 = psl$574;
        moonbit_incref(_curr_entry$581);
        moonbit_incref(self$578);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$578, entry$576, idx$575
        );
        psl$1637 = _curr_entry$581->$2;
        _tmp$1633 = psl$1637 + 1;
        _tmp$1635 = idx$575 + 1;
        capacity_mask$1636 = self$578->$3;
        _tmp$1634 = _tmp$1635 & capacity_mask$1636;
        psl$574 = _tmp$1633;
        idx$575 = _tmp$1634;
        entry$576 = _curr_entry$581;
        continue;
      } else {
        int32_t _tmp$1638 = psl$574 + 1;
        int32_t _tmp$1640 = idx$575 + 1;
        int32_t capacity_mask$1641 = self$578->$3;
        int32_t _tmp$1639 = _tmp$1640 & capacity_mask$1641;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2584 =
          entry$576;
        psl$574 = _tmp$1638;
        idx$575 = _tmp$1639;
        entry$576 = _tmp$2584;
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
  int32_t psl$1631 = entry$572->$2;
  int32_t _tmp$1627 = psl$1631 + 1;
  int32_t _tmp$1629 = idx$573 + 1;
  int32_t capacity_mask$1630 = self$568->$3;
  int32_t _tmp$1628 = _tmp$1629 & capacity_mask$1630;
  int32_t psl$564 = _tmp$1627;
  int32_t idx$565 = _tmp$1628;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$566 =
    entry$572;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2284 =
      self$568->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1626 =
      _field$2284;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2283;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$567;
    if (idx$565 < 0 || idx$565 >= Moonbit_array_length(entries$1626)) {
      moonbit_panic();
    }
    _tmp$2283
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1626[
        idx$565
      ];
    _bind$567 = _tmp$2283;
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
      int32_t psl$1616 = _curr_entry$571->$2;
      if (psl$564 > psl$1616) {
        int32_t psl$1621;
        int32_t _tmp$1617;
        int32_t _tmp$1619;
        int32_t capacity_mask$1620;
        int32_t _tmp$1618;
        entry$566->$2 = psl$564;
        moonbit_incref(_curr_entry$571);
        moonbit_incref(self$568);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$568, entry$566, idx$565
        );
        psl$1621 = _curr_entry$571->$2;
        _tmp$1617 = psl$1621 + 1;
        _tmp$1619 = idx$565 + 1;
        capacity_mask$1620 = self$568->$3;
        _tmp$1618 = _tmp$1619 & capacity_mask$1620;
        psl$564 = _tmp$1617;
        idx$565 = _tmp$1618;
        entry$566 = _curr_entry$571;
        continue;
      } else {
        int32_t _tmp$1622 = psl$564 + 1;
        int32_t _tmp$1624 = idx$565 + 1;
        int32_t capacity_mask$1625 = self$568->$3;
        int32_t _tmp$1623 = _tmp$1624 & capacity_mask$1625;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2586 =
          entry$566;
        psl$564 = _tmp$1622;
        idx$565 = _tmp$1623;
        entry$566 = _tmp$2586;
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
  int32_t psl$1615 = entry$562->$2;
  int32_t _tmp$1611 = psl$1615 + 1;
  int32_t _tmp$1613 = idx$563 + 1;
  int32_t capacity_mask$1614 = self$558->$3;
  int32_t _tmp$1612 = _tmp$1613 & capacity_mask$1614;
  int32_t psl$554 = _tmp$1611;
  int32_t idx$555 = _tmp$1612;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$556 =
    entry$562;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2286 =
      self$558->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1610 =
      _field$2286;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2285;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$557;
    if (idx$555 < 0 || idx$555 >= Moonbit_array_length(entries$1610)) {
      moonbit_panic();
    }
    _tmp$2285
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1610[
        idx$555
      ];
    _bind$557 = _tmp$2285;
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
      int32_t psl$1600 = _curr_entry$561->$2;
      if (psl$554 > psl$1600) {
        int32_t psl$1605;
        int32_t _tmp$1601;
        int32_t _tmp$1603;
        int32_t capacity_mask$1604;
        int32_t _tmp$1602;
        entry$556->$2 = psl$554;
        moonbit_incref(_curr_entry$561);
        moonbit_incref(self$558);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$558, entry$556, idx$555
        );
        psl$1605 = _curr_entry$561->$2;
        _tmp$1601 = psl$1605 + 1;
        _tmp$1603 = idx$555 + 1;
        capacity_mask$1604 = self$558->$3;
        _tmp$1602 = _tmp$1603 & capacity_mask$1604;
        psl$554 = _tmp$1601;
        idx$555 = _tmp$1602;
        entry$556 = _curr_entry$561;
        continue;
      } else {
        int32_t _tmp$1606 = psl$554 + 1;
        int32_t _tmp$1608 = idx$555 + 1;
        int32_t capacity_mask$1609 = self$558->$3;
        int32_t _tmp$1607 = _tmp$1608 & capacity_mask$1609;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2588 =
          entry$556;
        psl$554 = _tmp$1606;
        idx$555 = _tmp$1607;
        entry$556 = _tmp$2588;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2289 =
    self$548->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1598 =
    _field$2289;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1599;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2288;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2287;
  int32_t _cnt$2483;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$551;
  moonbit_incref(entry$550);
  _tmp$1599 = entry$550;
  if (new_idx$549 < 0 || new_idx$549 >= Moonbit_array_length(entries$1598)) {
    moonbit_panic();
  }
  _old$2288
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1598[
      new_idx$549
    ];
  if (_old$2288) {
    moonbit_decref(_old$2288);
  }
  entries$1598[new_idx$549] = _tmp$1599;
  _field$2287 = entry$550->$1;
  _cnt$2483 = Moonbit_object_header(entry$550)->rc;
  if (_cnt$2483 > 1) {
    int32_t _new_cnt$2486;
    if (_field$2287) {
      moonbit_incref(_field$2287);
    }
    _new_cnt$2486 = _cnt$2483 - 1;
    Moonbit_object_header(entry$550)->rc = _new_cnt$2486;
  } else if (_cnt$2483 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2485 =
      entry$550->$5;
    moonbit_string_t _field$2484;
    moonbit_decref(_field$2485);
    _field$2484 = entry$550->$4;
    moonbit_decref(_field$2484);
    moonbit_free(entry$550);
  }
  _bind$551 = _field$2287;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2292 =
    self$542->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1596 =
    _field$2292;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1597;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2291;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2290;
  int32_t _cnt$2487;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$545;
  moonbit_incref(entry$544);
  _tmp$1597 = entry$544;
  if (new_idx$543 < 0 || new_idx$543 >= Moonbit_array_length(entries$1596)) {
    moonbit_panic();
  }
  _old$2291
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1596[
      new_idx$543
    ];
  if (_old$2291) {
    moonbit_decref(_old$2291);
  }
  entries$1596[new_idx$543] = _tmp$1597;
  _field$2290 = entry$544->$1;
  _cnt$2487 = Moonbit_object_header(entry$544)->rc;
  if (_cnt$2487 > 1) {
    int32_t _new_cnt$2490;
    if (_field$2290) {
      moonbit_incref(_field$2290);
    }
    _new_cnt$2490 = _cnt$2487 - 1;
    Moonbit_object_header(entry$544)->rc = _new_cnt$2490;
  } else if (_cnt$2487 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2489 =
      entry$544->$5;
    moonbit_string_t _field$2488;
    moonbit_decref(_field$2489);
    _field$2488 = entry$544->$4;
    moonbit_decref(_field$2488);
    moonbit_free(entry$544);
  }
  _bind$545 = _field$2290;
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
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2295 =
    self$536->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1594 =
    _field$2295;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1595;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2294;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2293;
  int32_t _cnt$2491;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$539;
  moonbit_incref(entry$538);
  _tmp$1595 = entry$538;
  if (new_idx$537 < 0 || new_idx$537 >= Moonbit_array_length(entries$1594)) {
    moonbit_panic();
  }
  _old$2294
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1594[
      new_idx$537
    ];
  if (_old$2294) {
    moonbit_decref(_old$2294);
  }
  entries$1594[new_idx$537] = _tmp$1595;
  _field$2293 = entry$538->$1;
  _cnt$2491 = Moonbit_object_header(entry$538)->rc;
  if (_cnt$2491 > 1) {
    int32_t _new_cnt$2493;
    if (_field$2293) {
      moonbit_incref(_field$2293);
    }
    _new_cnt$2493 = _cnt$2491 - 1;
    Moonbit_object_header(entry$538)->rc = _new_cnt$2493;
  } else if (_cnt$2491 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2492 =
      entry$538->$5;
    moonbit_decref(_field$2492);
    moonbit_free(entry$538);
  }
  _bind$539 = _field$2293;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2298 =
    self$530->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1592 =
    _field$2298;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1593;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2297;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2296;
  int32_t _cnt$2494;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$533;
  moonbit_incref(entry$532);
  _tmp$1593 = entry$532;
  if (new_idx$531 < 0 || new_idx$531 >= Moonbit_array_length(entries$1592)) {
    moonbit_panic();
  }
  _old$2297
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1592[
      new_idx$531
    ];
  if (_old$2297) {
    moonbit_decref(_old$2297);
  }
  entries$1592[new_idx$531] = _tmp$1593;
  _field$2296 = entry$532->$1;
  _cnt$2494 = Moonbit_object_header(entry$532)->rc;
  if (_cnt$2494 > 1) {
    int32_t _new_cnt$2497;
    if (_field$2296) {
      moonbit_incref(_field$2296);
    }
    _new_cnt$2497 = _cnt$2494 - 1;
    Moonbit_object_header(entry$532)->rc = _new_cnt$2497;
  } else if (_cnt$2494 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2496 =
      entry$532->$5;
    moonbit_string_t _field$2495;
    moonbit_decref(_field$2496);
    _field$2495 = entry$532->$4;
    moonbit_decref(_field$2495);
    moonbit_free(entry$532);
  }
  _bind$533 = _field$2296;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2300;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1588;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1589;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2299;
  int32_t size$1591;
  int32_t _tmp$1590;
  switch (_bind$526) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1583;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2301;
      moonbit_incref(entry$528);
      _tmp$1583 = entry$528;
      _old$2301 = self$527->$5;
      if (_old$2301) {
        moonbit_decref(_old$2301);
      }
      self$527->$5 = _tmp$1583;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2304 =
        self$527->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1587 =
        _field$2304;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2303;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1586;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1584;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1585;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2302;
      if (_bind$526 < 0 || _bind$526 >= Moonbit_array_length(entries$1587)) {
        moonbit_panic();
      }
      _tmp$2303
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1587[
          _bind$526
        ];
      _tmp$1586 = _tmp$2303;
      if (_tmp$1586) {
        moonbit_incref(_tmp$1586);
      }
      _tmp$1584 = $Option$$unwrap$3(_tmp$1586);
      moonbit_incref(entry$528);
      _tmp$1585 = entry$528;
      _old$2302 = _tmp$1584->$1;
      if (_old$2302) {
        moonbit_decref(_old$2302);
      }
      _tmp$1584->$1 = _tmp$1585;
      moonbit_decref(_tmp$1584);
      break;
    }
  }
  self$527->$6 = idx$529;
  _field$2300 = self$527->$0;
  entries$1588 = _field$2300;
  _tmp$1589 = entry$528;
  if (idx$529 < 0 || idx$529 >= Moonbit_array_length(entries$1588)) {
    moonbit_panic();
  }
  _old$2299
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1588[
      idx$529
    ];
  if (_old$2299) {
    moonbit_decref(_old$2299);
  }
  entries$1588[idx$529] = _tmp$1589;
  size$1591 = self$527->$1;
  _tmp$1590 = size$1591 + 1;
  self$527->$1 = _tmp$1590;
  moonbit_decref(self$527);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$523,
  int32_t idx$525,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$524
) {
  int32_t _bind$522 = self$523->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2306;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1579;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1580;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2305;
  int32_t size$1582;
  int32_t _tmp$1581;
  switch (_bind$522) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1574;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2307;
      moonbit_incref(entry$524);
      _tmp$1574 = entry$524;
      _old$2307 = self$523->$5;
      if (_old$2307) {
        moonbit_decref(_old$2307);
      }
      self$523->$5 = _tmp$1574;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2310 =
        self$523->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1578 =
        _field$2310;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2309;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1577;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1575;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1576;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2308;
      if (_bind$522 < 0 || _bind$522 >= Moonbit_array_length(entries$1578)) {
        moonbit_panic();
      }
      _tmp$2309
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1578[
          _bind$522
        ];
      _tmp$1577 = _tmp$2309;
      if (_tmp$1577) {
        moonbit_incref(_tmp$1577);
      }
      _tmp$1575 = $Option$$unwrap$2(_tmp$1577);
      moonbit_incref(entry$524);
      _tmp$1576 = entry$524;
      _old$2308 = _tmp$1575->$1;
      if (_old$2308) {
        moonbit_decref(_old$2308);
      }
      _tmp$1575->$1 = _tmp$1576;
      moonbit_decref(_tmp$1575);
      break;
    }
  }
  self$523->$6 = idx$525;
  _field$2306 = self$523->$0;
  entries$1579 = _field$2306;
  _tmp$1580 = entry$524;
  if (idx$525 < 0 || idx$525 >= Moonbit_array_length(entries$1579)) {
    moonbit_panic();
  }
  _old$2305
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1579[
      idx$525
    ];
  if (_old$2305) {
    moonbit_decref(_old$2305);
  }
  entries$1579[idx$525] = _tmp$1580;
  size$1582 = self$523->$1;
  _tmp$1581 = size$1582 + 1;
  self$523->$1 = _tmp$1581;
  moonbit_decref(self$523);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$519,
  int32_t idx$521,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$520
) {
  int32_t _bind$518 = self$519->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2312;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1570;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1571;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2311;
  int32_t size$1573;
  int32_t _tmp$1572;
  switch (_bind$518) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1565;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2313;
      moonbit_incref(entry$520);
      _tmp$1565 = entry$520;
      _old$2313 = self$519->$5;
      if (_old$2313) {
        moonbit_decref(_old$2313);
      }
      self$519->$5 = _tmp$1565;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2316 =
        self$519->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1569 =
        _field$2316;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2315;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1568;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1566;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1567;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2314;
      if (_bind$518 < 0 || _bind$518 >= Moonbit_array_length(entries$1569)) {
        moonbit_panic();
      }
      _tmp$2315
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1569[
          _bind$518
        ];
      _tmp$1568 = _tmp$2315;
      if (_tmp$1568) {
        moonbit_incref(_tmp$1568);
      }
      _tmp$1566 = $Option$$unwrap$1(_tmp$1568);
      moonbit_incref(entry$520);
      _tmp$1567 = entry$520;
      _old$2314 = _tmp$1566->$1;
      if (_old$2314) {
        moonbit_decref(_old$2314);
      }
      _tmp$1566->$1 = _tmp$1567;
      moonbit_decref(_tmp$1566);
      break;
    }
  }
  self$519->$6 = idx$521;
  _field$2312 = self$519->$0;
  entries$1570 = _field$2312;
  _tmp$1571 = entry$520;
  if (idx$521 < 0 || idx$521 >= Moonbit_array_length(entries$1570)) {
    moonbit_panic();
  }
  _old$2311
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1570[
      idx$521
    ];
  if (_old$2311) {
    moonbit_decref(_old$2311);
  }
  entries$1570[idx$521] = _tmp$1571;
  size$1573 = self$519->$1;
  _tmp$1572 = size$1573 + 1;
  self$519->$1 = _tmp$1572;
  moonbit_decref(self$519);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$515,
  int32_t idx$517,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$516
) {
  int32_t _bind$514 = self$515->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2318;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1561;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1562;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2317;
  int32_t size$1564;
  int32_t _tmp$1563;
  switch (_bind$514) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1556;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2319;
      moonbit_incref(entry$516);
      _tmp$1556 = entry$516;
      _old$2319 = self$515->$5;
      if (_old$2319) {
        moonbit_decref(_old$2319);
      }
      self$515->$5 = _tmp$1556;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2322 =
        self$515->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1560 =
        _field$2322;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2321;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1559;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1557;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1558;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2320;
      if (_bind$514 < 0 || _bind$514 >= Moonbit_array_length(entries$1560)) {
        moonbit_panic();
      }
      _tmp$2321
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1560[
          _bind$514
        ];
      _tmp$1559 = _tmp$2321;
      if (_tmp$1559) {
        moonbit_incref(_tmp$1559);
      }
      _tmp$1557 = $Option$$unwrap$0(_tmp$1559);
      moonbit_incref(entry$516);
      _tmp$1558 = entry$516;
      _old$2320 = _tmp$1557->$1;
      if (_old$2320) {
        moonbit_decref(_old$2320);
      }
      _tmp$1557->$1 = _tmp$1558;
      moonbit_decref(_tmp$1557);
      break;
    }
  }
  self$515->$6 = idx$517;
  _field$2318 = self$515->$0;
  entries$1561 = _field$2318;
  _tmp$1562 = entry$516;
  if (idx$517 < 0 || idx$517 >= Moonbit_array_length(entries$1561)) {
    moonbit_panic();
  }
  _old$2317
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1561[
      idx$517
    ];
  if (_old$2317) {
    moonbit_decref(_old$2317);
  }
  entries$1561[idx$517] = _tmp$1562;
  size$1564 = self$515->$1;
  _tmp$1563 = size$1564 + 1;
  self$515->$1 = _tmp$1563;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1555 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$512 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$508, _tmp$1555
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$513 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2589 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2589)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2589->$0 = _bind$512;
  _block$2589->$1 = 0;
  _block$2589->$2 = capacity$508;
  _block$2589->$3 = _bind$510;
  _block$2589->$4 = _bind$511;
  _block$2589->$5 = _bind$513;
  _block$2589->$6 = -1;
  return _block$2589;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$503
) {
  int32_t capacity$502 = $Int$$next_power_of_two(capacity$503);
  int32_t _bind$504 = capacity$502 - 1;
  int32_t _bind$505 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$502);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1554 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$506 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$502, _tmp$1554
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$507 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2590 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2590)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2590->$0 = _bind$506;
  _block$2590->$1 = 0;
  _block$2590->$2 = capacity$502;
  _block$2590->$3 = _bind$504;
  _block$2590->$4 = _bind$505;
  _block$2590->$5 = _bind$507;
  _block$2590->$6 = -1;
  return _block$2590;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$497
) {
  int32_t capacity$496 = $Int$$next_power_of_two(capacity$497);
  int32_t _bind$498 = capacity$496 - 1;
  int32_t _bind$499 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$496);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1553 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$500 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$496, _tmp$1553
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$501 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$2591 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2591)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2591->$0 = _bind$500;
  _block$2591->$1 = 0;
  _block$2591->$2 = capacity$496;
  _block$2591->$3 = _bind$498;
  _block$2591->$4 = _bind$499;
  _block$2591->$5 = _bind$501;
  _block$2591->$6 = -1;
  return _block$2591;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$491
) {
  int32_t capacity$490 = $Int$$next_power_of_two(capacity$491);
  int32_t _bind$492 = capacity$490 - 1;
  int32_t _bind$493 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$490);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1552 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$494 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$490, _tmp$1552
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$495 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2592 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2592)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2592->$0 = _bind$494;
  _block$2592->$1 = 0;
  _block$2592->$2 = capacity$490;
  _block$2592->$3 = _bind$492;
  _block$2592->$4 = _bind$493;
  _block$2592->$5 = _bind$495;
  _block$2592->$6 = -1;
  return _block$2592;
}

int32_t $Int$$next_power_of_two(int32_t self$489) {
  if (self$489 >= 0) {
    int32_t _tmp$1551;
    int32_t _tmp$1550;
    int32_t _tmp$1549;
    int32_t _tmp$1548;
    if (self$489 <= 1) {
      return 1;
    }
    if (self$489 > 1073741824) {
      return 1073741824;
    }
    _tmp$1551 = self$489 - 1;
    _tmp$1550 = moonbit_clz32(_tmp$1551);
    _tmp$1549 = _tmp$1550 - 1;
    _tmp$1548 = 2147483647 >> (_tmp$1549 & 31);
    return _tmp$1548 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$488) {
  int32_t _tmp$1547 = capacity$488 * 13;
  return _tmp$1547 / 16;
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1546 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$479);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$1546);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$478
) {
  moonbit_string_t* _field$2324 = self$478->$0;
  moonbit_string_t* buf$1544 = _field$2324;
  int32_t _field$2323 = self$478->$1;
  int32_t _cnt$2498 = Moonbit_object_header(self$478)->rc;
  int32_t len$1545;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$1543;
  if (_cnt$2498 > 1) {
    int32_t _new_cnt$2499;
    moonbit_incref(buf$1544);
    _new_cnt$2499 = _cnt$2498 - 1;
    Moonbit_object_header(self$478)->rc = _new_cnt$2499;
  } else if (_cnt$2498 == 1) {
    moonbit_free(self$478);
  }
  len$1545 = _field$2323;
  _tmp$1543
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$1545, buf$1544
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$1543);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$476
) {
  struct $Ref$3c$Int$3e$* i$475 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$2593;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1532;
  Moonbit_object_header(i$475)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$475->$0 = 0;
  _closure$2593
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$2593)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$2593->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$2593->$0_0 = self$476.$0;
  _closure$2593->$0_1 = self$476.$1;
  _closure$2593->$0_2 = self$476.$2;
  _closure$2593->$1 = i$475;
  _tmp$1532 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$2593;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$1532);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1533
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$1534 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$1533;
  struct $Ref$3c$Int$3e$* _field$2329 = _casted_env$1534->$1;
  struct $Ref$3c$Int$3e$* i$475 = _field$2329;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$2328 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$1534->$0_1, _casted_env$1534->$0_2, _casted_env$1534->$0_0
    };
  int32_t _cnt$2500 = Moonbit_object_header(_casted_env$1534)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$476;
  int32_t val$1535;
  int32_t _tmp$1536;
  if (_cnt$2500 > 1) {
    int32_t _new_cnt$2501;
    moonbit_incref(i$475);
    moonbit_incref(_field$2328.$0);
    _new_cnt$2501 = _cnt$2500 - 1;
    Moonbit_object_header(_casted_env$1534)->rc = _new_cnt$2501;
  } else if (_cnt$2500 == 1) {
    moonbit_free(_casted_env$1534);
  }
  self$476 = _field$2328;
  val$1535 = i$475->$0;
  moonbit_incref(self$476.$0);
  _tmp$1536 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$476);
  if (val$1535 < _tmp$1536) {
    moonbit_string_t* _field$2327 = self$476.$0;
    moonbit_string_t* buf$1539 = _field$2327;
    int32_t _field$2326 = self$476.$1;
    int32_t start$1541 = _field$2326;
    int32_t val$1542 = i$475->$0;
    int32_t _tmp$1540 = start$1541 + val$1542;
    moonbit_string_t _tmp$2325 = (moonbit_string_t)buf$1539[_tmp$1540];
    moonbit_string_t elem$477;
    int32_t val$1538;
    int32_t _tmp$1537;
    moonbit_incref(_tmp$2325);
    moonbit_decref(buf$1539);
    elem$477 = _tmp$2325;
    val$1538 = i$475->$0;
    _tmp$1537 = val$1538 + 1;
    i$475->$0 = _tmp$1537;
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
  moonbit_string_t _tmp$1531 = $Int$$to_string$inner(self$473, 10);
  logger$472.$0->$method_0(logger$472.$1, _tmp$1531);
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
  int32_t len$1526 = self$466->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1528;
  int32_t _tmp$2332;
  int32_t _tmp$1527;
  int32_t length$467;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2331;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$1529;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2330;
  int32_t _tmp$1530;
  moonbit_incref(self$466);
  _tmp$1528 = $$moonbitlang$core$builtin$Array$$buffer$2(self$466);
  _tmp$2332 = Moonbit_array_length(_tmp$1528);
  moonbit_decref(_tmp$1528);
  _tmp$1527 = _tmp$2332;
  if (len$1526 == _tmp$1527) {
    moonbit_incref(self$466);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$466);
  }
  length$467 = self$466->$1;
  _field$2331 = self$466->$0;
  buf$1529 = _field$2331;
  _old$2330
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$1529[
      length$467
    ];
  if (_old$2330) {
    moonbit_decref(_old$2330);
  }
  buf$1529[length$467] = value$468;
  _tmp$1530 = length$467 + 1;
  self$466->$1 = _tmp$1530;
  moonbit_decref(self$466);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$463,
  struct $$3c$String$2a$Int$3e$* value$465
) {
  int32_t len$1521 = self$463->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$1523;
  int32_t _tmp$2335;
  int32_t _tmp$1522;
  int32_t length$464;
  struct $$3c$String$2a$Int$3e$** _field$2334;
  struct $$3c$String$2a$Int$3e$** buf$1524;
  struct $$3c$String$2a$Int$3e$* _old$2333;
  int32_t _tmp$1525;
  moonbit_incref(self$463);
  _tmp$1523 = $$moonbitlang$core$builtin$Array$$buffer$0(self$463);
  _tmp$2335 = Moonbit_array_length(_tmp$1523);
  moonbit_decref(_tmp$1523);
  _tmp$1522 = _tmp$2335;
  if (len$1521 == _tmp$1522) {
    moonbit_incref(self$463);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$463);
  }
  length$464 = self$463->$1;
  _field$2334 = self$463->$0;
  buf$1524 = _field$2334;
  _old$2333 = (struct $$3c$String$2a$Int$3e$*)buf$1524[length$464];
  if (_old$2333) {
    moonbit_decref(_old$2333);
  }
  buf$1524[length$464] = value$465;
  _tmp$1525 = length$464 + 1;
  self$463->$1 = _tmp$1525;
  moonbit_decref(self$463);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$460,
  moonbit_string_t value$462
) {
  int32_t len$1516 = self$460->$1;
  moonbit_string_t* _tmp$1518;
  int32_t _tmp$2338;
  int32_t _tmp$1517;
  int32_t length$461;
  moonbit_string_t* _field$2337;
  moonbit_string_t* buf$1519;
  moonbit_string_t _old$2336;
  int32_t _tmp$1520;
  moonbit_incref(self$460);
  _tmp$1518 = $$moonbitlang$core$builtin$Array$$buffer$1(self$460);
  _tmp$2338 = Moonbit_array_length(_tmp$1518);
  moonbit_decref(_tmp$1518);
  _tmp$1517 = _tmp$2338;
  if (len$1516 == _tmp$1517) {
    moonbit_incref(self$460);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$460);
  }
  length$461 = self$460->$1;
  _field$2337 = self$460->$0;
  buf$1519 = _field$2337;
  _old$2336 = (moonbit_string_t)buf$1519[length$461];
  moonbit_decref(_old$2336);
  buf$1519[length$461] = value$462;
  _tmp$1520 = length$461 + 1;
  self$460->$1 = _tmp$1520;
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
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2340 =
    self$448->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$447 =
    _field$2340;
  int32_t old_cap$449 = Moonbit_array_length(old_buf$447);
  int32_t copy_len$450;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$2339;
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
  _old$2339 = self$448->$0;
  moonbit_decref(_old$2339);
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
  struct $$3c$String$2a$Int$3e$** _field$2342 = self$442->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$441 = _field$2342;
  int32_t old_cap$443 = Moonbit_array_length(old_buf$441);
  int32_t copy_len$444;
  struct $$3c$String$2a$Int$3e$** _old$2341;
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
  _old$2341 = self$442->$0;
  moonbit_decref(_old$2341);
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
  moonbit_string_t* _field$2344 = self$436->$0;
  moonbit_string_t* old_buf$435 = _field$2344;
  int32_t old_cap$437 = Moonbit_array_length(old_buf$435);
  int32_t copy_len$438;
  moonbit_string_t* _old$2343;
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
  _old$2343 = self$436->$0;
  moonbit_decref(_old$2343);
  self$436->$0 = new_buf$433;
  moonbit_decref(self$436);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$432
) {
  if (capacity$432 == 0) {
    moonbit_string_t* _tmp$1514 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2594 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2594)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2594->$0 = _tmp$1514;
    _block$2594->$1 = 0;
    return _block$2594;
  } else {
    moonbit_string_t* _tmp$1515 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$432, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2595 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2595)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2595->$0 = _tmp$1515;
    _block$2595->$1 = 0;
    return _block$2595;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$430,
  struct $StringView str$431
) {
  int32_t len$1502 = self$430->$1;
  int32_t _tmp$1504;
  int32_t _tmp$1503;
  int32_t _tmp$1501;
  moonbit_bytes_t _field$2345;
  moonbit_bytes_t data$1505;
  int32_t len$1506;
  moonbit_string_t _tmp$1507;
  int32_t _tmp$1508;
  int32_t _tmp$1509;
  int32_t len$1511;
  int32_t _tmp$1513;
  int32_t _tmp$1512;
  int32_t _tmp$1510;
  moonbit_incref(str$431.$0);
  _tmp$1504 = $StringView$$length(str$431);
  _tmp$1503 = _tmp$1504 * 2;
  _tmp$1501 = len$1502 + _tmp$1503;
  moonbit_incref(self$430);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$430, _tmp$1501
  );
  _field$2345 = self$430->$0;
  data$1505 = _field$2345;
  len$1506 = self$430->$1;
  moonbit_incref(data$1505);
  moonbit_incref(str$431.$0);
  _tmp$1507 = $StringView$$data(str$431);
  moonbit_incref(str$431.$0);
  _tmp$1508 = $StringView$$start_offset(str$431);
  moonbit_incref(str$431.$0);
  _tmp$1509 = $StringView$$length(str$431);
  $FixedArray$$blit_from_string(
    data$1505, len$1506, _tmp$1507, _tmp$1508, _tmp$1509
  );
  len$1511 = self$430->$1;
  _tmp$1513 = $StringView$$length(str$431);
  _tmp$1512 = _tmp$1513 * 2;
  _tmp$1510 = len$1511 + _tmp$1512;
  self$430->$1 = _tmp$1510;
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
    int32_t _tmp$1500 = -i$428;
    return $String$$offset_of_nth_char_backward(
             self$427, _tmp$1500, start_offset$429, end_offset$424
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$422,
  int32_t n$420,
  int32_t start_offset$416,
  int32_t end_offset$417
) {
  int32_t _if_result$2596;
  if (start_offset$416 >= 0) {
    _if_result$2596 = start_offset$416 <= end_offset$417;
  } else {
    _if_result$2596 = 0;
  }
  if (_if_result$2596) {
    int32_t utf16_offset$418 = start_offset$416;
    int32_t char_count$419 = 0;
    int32_t _tmp$1498;
    int32_t _if_result$2599;
    while (1) {
      int32_t _tmp$1492 = utf16_offset$418;
      int32_t _if_result$2598;
      if (_tmp$1492 < end_offset$417) {
        int32_t _tmp$1491 = char_count$419;
        _if_result$2598 = _tmp$1491 < n$420;
      } else {
        _if_result$2598 = 0;
      }
      if (_if_result$2598) {
        int32_t _tmp$1496 = utf16_offset$418;
        int32_t c$421 = self$422[_tmp$1496];
        int32_t _tmp$1495;
        if ($Int$$is_leading_surrogate(c$421)) {
          int32_t _tmp$1493 = utf16_offset$418;
          utf16_offset$418 = _tmp$1493 + 2;
        } else {
          int32_t _tmp$1494 = utf16_offset$418;
          utf16_offset$418 = _tmp$1494 + 1;
        }
        _tmp$1495 = char_count$419;
        char_count$419 = _tmp$1495 + 1;
        continue;
      } else {
        moonbit_decref(self$422);
      }
      break;
    }
    _tmp$1498 = char_count$419;
    if (_tmp$1498 < n$420) {
      _if_result$2599 = 1;
    } else {
      int32_t _tmp$1497 = utf16_offset$418;
      _if_result$2599 = _tmp$1497 >= end_offset$417;
    }
    if (_if_result$2599) {
      return 4294967296ll;
    } else {
      int32_t _tmp$1499 = utf16_offset$418;
      return (int64_t)_tmp$1499;
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
  int32_t _tmp$1489;
  int32_t _if_result$2602;
  while (1) {
    int32_t _tmp$1482 = utf16_offset$409;
    int32_t _tmp$1481 = _tmp$1482 - 1;
    int32_t _if_result$2601;
    if (_tmp$1481 >= start_offset$411) {
      int32_t _tmp$1480 = char_count$408;
      _if_result$2601 = _tmp$1480 < n$412;
    } else {
      _if_result$2601 = 0;
    }
    if (_if_result$2601) {
      int32_t _tmp$1487 = utf16_offset$409;
      int32_t _tmp$1486 = _tmp$1487 - 1;
      int32_t c$413 = self$414[_tmp$1486];
      int32_t _tmp$1485;
      if ($Int$$is_trailing_surrogate(c$413)) {
        int32_t _tmp$1483 = utf16_offset$409;
        utf16_offset$409 = _tmp$1483 - 2;
      } else {
        int32_t _tmp$1484 = utf16_offset$409;
        utf16_offset$409 = _tmp$1484 - 1;
      }
      _tmp$1485 = char_count$408;
      char_count$408 = _tmp$1485 + 1;
      continue;
    } else {
      moonbit_decref(self$414);
    }
    break;
  }
  _tmp$1489 = char_count$408;
  if (_tmp$1489 < n$412) {
    _if_result$2602 = 1;
  } else {
    int32_t _tmp$1488 = utf16_offset$409;
    _if_result$2602 = _tmp$1488 < start_offset$411;
  }
  if (_if_result$2602) {
    return 4294967296ll;
  } else {
    int32_t _tmp$1490 = utf16_offset$409;
    return (int64_t)_tmp$1490;
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
    int32_t _if_result$2604;
    if (index$401 < end_offset$397) {
      _if_result$2604 = count$402 < len$403;
    } else {
      _if_result$2604 = 0;
    }
    if (_if_result$2604) {
      int32_t c1$404 = self$400[index$401];
      int32_t _if_result$2605;
      int32_t _tmp$1478;
      int32_t _tmp$1479;
      if ($Int$$is_leading_surrogate(c1$404)) {
        int32_t _tmp$1474 = index$401 + 1;
        _if_result$2605 = _tmp$1474 < end_offset$397;
      } else {
        _if_result$2605 = 0;
      }
      if (_if_result$2605) {
        int32_t _tmp$1477 = index$401 + 1;
        int32_t c2$405 = self$400[_tmp$1477];
        if ($Int$$is_trailing_surrogate(c2$405)) {
          int32_t _tmp$1475 = index$401 + 2;
          int32_t _tmp$1476 = count$402 + 1;
          index$401 = _tmp$1475;
          count$402 = _tmp$1476;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_16.data,
              (moonbit_string_t)moonbit_string_literal_17.data
          );
        }
      }
      _tmp$1478 = index$401 + 1;
      _tmp$1479 = count$402 + 1;
      index$401 = _tmp$1478;
      count$402 = _tmp$1479;
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
  int32_t end$1472 = self$396.$2;
  int32_t _field$2346 = self$396.$1;
  int32_t start$1473;
  moonbit_decref(self$396.$0);
  start$1473 = _field$2346;
  return end$1472 - start$1473;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$395
) {
  int32_t end$1470 = self$395.$2;
  int32_t _field$2347 = self$395.$1;
  int32_t start$1471;
  moonbit_decref(self$395.$0);
  start$1471 = _field$2347;
  return end$1470 - start$1471;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$394
) {
  int32_t end$1468 = self$394.$2;
  int32_t _field$2348 = self$394.$1;
  int32_t start$1469;
  moonbit_decref(self$394.$0);
  start$1469 = _field$2348;
  return end$1468 - start$1469;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$393
) {
  int32_t end$1466 = self$393.$2;
  int32_t _field$2349 = self$393.$1;
  int32_t start$1467;
  moonbit_decref(self$393.$0);
  start$1467 = _field$2349;
  return end$1466 - start$1467;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$392
) {
  int32_t end$1464 = self$392.$2;
  int32_t _field$2350 = self$392.$1;
  int32_t start$1465;
  moonbit_decref(self$392.$0);
  start$1465 = _field$2350;
  return end$1464 - start$1465;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$2606 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$2606)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2606->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$2606->$0 = self$387;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$2606;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1462,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$385
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$1463 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$1462;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$2351 =
    _casted_env$1463->$0;
  int32_t _cnt$2502 = Moonbit_object_header(_casted_env$1463)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387;
  if (_cnt$2502 > 1) {
    int32_t _new_cnt$2503;
    moonbit_incref(_field$2351);
    _new_cnt$2503 = _cnt$2502 - 1;
    Moonbit_object_header(_casted_env$1463)->rc = _new_cnt$2503;
  } else if (_cnt$2502 == 1) {
    moonbit_free(_casted_env$1463);
  }
  self$387 = _field$2351;
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
    struct $$moonbitlang$core$builtin$Logger _bind$1444;
    int32_t _tmp$1445;
    int32_t _tmp$1446;
    int32_t _tmp$1447;
    int32_t _tmp$2611;
    int32_t _tmp$2612;
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
        int32_t _tmp$1448;
        int32_t _tmp$1449;
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
        _tmp$1448 = i$377 + 1;
        _tmp$1449 = i$377 + 1;
        i$377 = _tmp$1448;
        seg$378 = _tmp$1449;
        goto $$2a$for$379;
        break;
      }
      
      case 13: {
        int32_t _tmp$1450;
        int32_t _tmp$1451;
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
        _tmp$1450 = i$377 + 1;
        _tmp$1451 = i$377 + 1;
        i$377 = _tmp$1450;
        seg$378 = _tmp$1451;
        goto $$2a$for$379;
        break;
      }
      
      case 8: {
        int32_t _tmp$1452;
        int32_t _tmp$1453;
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
        _tmp$1452 = i$377 + 1;
        _tmp$1453 = i$377 + 1;
        i$377 = _tmp$1452;
        seg$378 = _tmp$1453;
        goto $$2a$for$379;
        break;
      }
      
      case 9: {
        int32_t _tmp$1454;
        int32_t _tmp$1455;
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
        _tmp$1454 = i$377 + 1;
        _tmp$1455 = i$377 + 1;
        i$377 = _tmp$1454;
        seg$378 = _tmp$1455;
        goto $$2a$for$379;
        break;
      }
      default: {
        if (code$380 < 32) {
          int32_t _tmp$1458;
          moonbit_string_t _tmp$1457;
          struct $$moonbitlang$core$builtin$Logger _bind$1456;
          int32_t _tmp$1459;
          int32_t _tmp$1460;
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
          _tmp$1458 = code$380 & 0xff;
          _tmp$1457 = $Byte$$to_hex(_tmp$1458);
          if (logger$373.$1) {
            moonbit_incref(logger$373.$1);
          }
          logger$373.$0->$method_0(logger$373.$1, _tmp$1457);
          _bind$1456 = logger$373;
          if (_bind$1456.$1) {
            moonbit_incref(_bind$1456.$1);
          }
          _bind$1456.$0->$method_3(_bind$1456.$1, 125);
          _tmp$1459 = i$377 + 1;
          _tmp$1460 = i$377 + 1;
          i$377 = _tmp$1459;
          seg$378 = _tmp$1460;
          goto $$2a$for$379;
        } else {
          int32_t _tmp$1461 = i$377 + 1;
          int32_t _tmp$2610 = seg$378;
          i$377 = _tmp$1461;
          seg$378 = _tmp$2610;
          goto $$2a$for$379;
        }
        break;
      }
    }
    goto $joinlet$2609;
    $join$381:;
    moonbit_incref(_env$374);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$374, seg$378, i$377
    );
    if (logger$373.$1) {
      moonbit_incref(logger$373.$1);
    }
    logger$373.$0->$method_3(logger$373.$1, 92);
    _bind$1444 = logger$373;
    _tmp$1445 = c$382;
    if (_bind$1444.$1) {
      moonbit_incref(_bind$1444.$1);
    }
    _bind$1444.$0->$method_3(_bind$1444.$1, _tmp$1445);
    _tmp$1446 = i$377 + 1;
    _tmp$1447 = i$377 + 1;
    i$377 = _tmp$1446;
    seg$378 = _tmp$1447;
    continue;
    $joinlet$2609:;
    _tmp$2611 = i$377;
    _tmp$2612 = seg$378;
    i$377 = _tmp$2611;
    seg$378 = _tmp$2612;
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
  moonbit_string_t _field$2353 = _env$369->$1;
  moonbit_string_t self$368 = _field$2353;
  struct $$moonbitlang$core$builtin$Logger _field$2352 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$369->$0_0, _env$369->$0_1
    };
  int32_t _cnt$2504 = Moonbit_object_header(_env$369)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$370;
  if (_cnt$2504 > 1) {
    int32_t _new_cnt$2505;
    moonbit_incref(self$368);
    if (_field$2352.$1) {
      moonbit_incref(_field$2352.$1);
    }
    _new_cnt$2505 = _cnt$2504 - 1;
    Moonbit_object_header(_env$369)->rc = _new_cnt$2505;
  } else if (_cnt$2504 == 1) {
    moonbit_free(_env$369);
  }
  logger$370 = _field$2352;
  if (i$371 > seg$372) {
    int32_t _tmp$1443 = i$371 - seg$372;
    logger$370.$0->$method_1(logger$370.$1, self$368, seg$372, _tmp$1443);
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
  int32_t _tmp$1440 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$367, 16);
  int32_t _tmp$1439 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$1440);
  int32_t _tmp$1442;
  int32_t _tmp$1441;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$1438;
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1439
  );
  _tmp$1442 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$367, 16);
  _tmp$1441
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$1442
  );
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1441
  );
  _tmp$1438 = _self$366;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$1438);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$365) {
  if (i$365 < 10) {
    int32_t _tmp$1435 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 48);
    return $Byte$$to_char(_tmp$1435);
  } else {
    int32_t _tmp$1437 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 97);
    int32_t _tmp$1436 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$1437, 10);
    return $Byte$$to_char(_tmp$1436);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$363,
  int32_t that$364
) {
  int32_t _tmp$1433 = (int32_t)self$363;
  int32_t _tmp$1434 = (int32_t)that$364;
  int32_t _tmp$1432 = _tmp$1433 - _tmp$1434;
  return _tmp$1432 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$361,
  int32_t that$362
) {
  int32_t _tmp$1430 = (int32_t)self$361;
  int32_t _tmp$1431 = (int32_t)that$362;
  int32_t _tmp$1429 = _tmp$1430 % _tmp$1431;
  return _tmp$1429 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$359,
  int32_t that$360
) {
  int32_t _tmp$1427 = (int32_t)self$359;
  int32_t _tmp$1428 = (int32_t)that$360;
  int32_t _tmp$1426 = _tmp$1427 / _tmp$1428;
  return _tmp$1426 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$357,
  int32_t that$358
) {
  int32_t _tmp$1424 = (int32_t)self$357;
  int32_t _tmp$1425 = (int32_t)that$358;
  int32_t _tmp$1423 = _tmp$1424 + _tmp$1425;
  return _tmp$1423 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$354,
  int32_t start$352,
  int32_t end$353
) {
  int32_t _if_result$2613;
  int32_t len$355;
  int32_t _tmp$1421;
  int32_t _tmp$1422;
  moonbit_bytes_t bytes$356;
  moonbit_bytes_t _tmp$1420;
  if (start$352 == 0) {
    int32_t _tmp$1419 = Moonbit_array_length(str$354);
    _if_result$2613 = end$353 == _tmp$1419;
  } else {
    _if_result$2613 = 0;
  }
  if (_if_result$2613) {
    return str$354;
  }
  len$355 = end$353 - start$352;
  _tmp$1421 = len$355 * 2;
  _tmp$1422 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$356 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1421, _tmp$1422);
  moonbit_incref(bytes$356);
  $FixedArray$$blit_from_string(bytes$356, 0, str$354, start$352, len$355);
  _tmp$1420 = bytes$356;
  return $Bytes$$to_unchecked_string$inner(_tmp$1420, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$351
) {
  return f$351;
}

moonbit_string_t $Int$$to_string$inner(int32_t self$335, int32_t radix$334) {
  int32_t _if_result$2614;
  int32_t is_negative$336;
  uint32_t num$337;
  uint16_t* buffer$338;
  if (radix$334 < 2) {
    _if_result$2614 = 1;
  } else {
    _if_result$2614 = radix$334 > 36;
  }
  if (_if_result$2614) {
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
    int32_t _tmp$1418 = -self$335;
    num$337 = *(uint32_t*)&_tmp$1418;
  } else {
    num$337 = *(uint32_t*)&self$335;
  }
  switch (radix$334) {
    case 10: {
      int32_t digit_len$339 = $moonbitlang$core$builtin$dec_count32(num$337);
      int32_t _tmp$1415;
      int32_t total_len$340;
      uint16_t* buffer$341;
      int32_t digit_start$342;
      if (is_negative$336) {
        _tmp$1415 = 1;
      } else {
        _tmp$1415 = 0;
      }
      total_len$340 = digit_len$339 + _tmp$1415;
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
      int32_t _tmp$1416;
      int32_t total_len$344;
      uint16_t* buffer$345;
      int32_t digit_start$346;
      if (is_negative$336) {
        _tmp$1416 = 1;
      } else {
        _tmp$1416 = 0;
      }
      total_len$344 = digit_len$343 + _tmp$1416;
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
      int32_t _tmp$1417;
      int32_t total_len$348;
      uint16_t* buffer$349;
      int32_t digit_start$350;
      if (is_negative$336) {
        _tmp$1417 = 1;
      } else {
        _tmp$1417 = 0;
      }
      total_len$348 = digit_len$347 + _tmp$1417;
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
    uint32_t _tmp$1412 = num$329;
    if (_tmp$1412 > 0u) {
      int32_t _tmp$1413 = count$332;
      uint32_t _tmp$1414;
      count$332 = _tmp$1413 + 1;
      _tmp$1414 = num$329;
      num$329 = _tmp$1414 / base$330;
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
    int32_t _tmp$1411 = 31 - leading_zeros$327;
    int32_t _tmp$1410 = _tmp$1411 / 4;
    return _tmp$1410 + 1;
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
  uint32_t _tmp$1409;
  int32_t remaining$317;
  int32_t _tmp$1390;
  while (1) {
    uint32_t _tmp$1353 = num$302;
    if (_tmp$1353 >= 10000u) {
      uint32_t _tmp$1376 = num$302;
      uint32_t t$307 = _tmp$1376 / 10000u;
      uint32_t _tmp$1375 = num$302;
      uint32_t _tmp$1374 = _tmp$1375 % 10000u;
      int32_t r$308 = *(int32_t*)&_tmp$1374;
      int32_t d1$309;
      int32_t d2$310;
      int32_t _tmp$1354;
      int32_t _tmp$1373;
      int32_t _tmp$1372;
      int32_t d1_hi$311;
      int32_t _tmp$1371;
      int32_t _tmp$1370;
      int32_t d1_lo$312;
      int32_t _tmp$1369;
      int32_t _tmp$1368;
      int32_t d2_hi$313;
      int32_t _tmp$1367;
      int32_t _tmp$1366;
      int32_t d2_lo$314;
      int32_t _tmp$1356;
      int32_t _tmp$1355;
      int32_t _tmp$1359;
      int32_t _tmp$1358;
      int32_t _tmp$1357;
      int32_t _tmp$1362;
      int32_t _tmp$1361;
      int32_t _tmp$1360;
      int32_t _tmp$1365;
      int32_t _tmp$1364;
      int32_t _tmp$1363;
      num$302 = t$307;
      d1$309 = r$308 / 100;
      d2$310 = r$308 % 100;
      _tmp$1354 = offset$304;
      offset$304 = _tmp$1354 - 4;
      _tmp$1373 = d1$309 / 10;
      _tmp$1372 = 48 + _tmp$1373;
      d1_hi$311 = (uint16_t)_tmp$1372;
      _tmp$1371 = d1$309 % 10;
      _tmp$1370 = 48 + _tmp$1371;
      d1_lo$312 = (uint16_t)_tmp$1370;
      _tmp$1369 = d2$310 / 10;
      _tmp$1368 = 48 + _tmp$1369;
      d2_hi$313 = (uint16_t)_tmp$1368;
      _tmp$1367 = d2$310 % 10;
      _tmp$1366 = 48 + _tmp$1367;
      d2_lo$314 = (uint16_t)_tmp$1366;
      _tmp$1356 = offset$304;
      _tmp$1355 = digit_start$306 + _tmp$1356;
      buffer$315[_tmp$1355] = d1_hi$311;
      _tmp$1359 = offset$304;
      _tmp$1358 = digit_start$306 + _tmp$1359;
      _tmp$1357 = _tmp$1358 + 1;
      buffer$315[_tmp$1357] = d1_lo$312;
      _tmp$1362 = offset$304;
      _tmp$1361 = digit_start$306 + _tmp$1362;
      _tmp$1360 = _tmp$1361 + 2;
      buffer$315[_tmp$1360] = d2_hi$313;
      _tmp$1365 = offset$304;
      _tmp$1364 = digit_start$306 + _tmp$1365;
      _tmp$1363 = _tmp$1364 + 3;
      buffer$315[_tmp$1363] = d2_lo$314;
      continue;
    }
    break;
  }
  _tmp$1409 = num$302;
  remaining$317 = *(int32_t*)&_tmp$1409;
  while (1) {
    int32_t _tmp$1377 = remaining$317;
    if (_tmp$1377 >= 100) {
      int32_t _tmp$1389 = remaining$317;
      int32_t t$318 = _tmp$1389 / 100;
      int32_t _tmp$1388 = remaining$317;
      int32_t d$319 = _tmp$1388 % 100;
      int32_t _tmp$1378;
      int32_t _tmp$1387;
      int32_t _tmp$1386;
      int32_t d_hi$320;
      int32_t _tmp$1385;
      int32_t _tmp$1384;
      int32_t d_lo$321;
      int32_t _tmp$1380;
      int32_t _tmp$1379;
      int32_t _tmp$1383;
      int32_t _tmp$1382;
      int32_t _tmp$1381;
      remaining$317 = t$318;
      _tmp$1378 = offset$304;
      offset$304 = _tmp$1378 - 2;
      _tmp$1387 = d$319 / 10;
      _tmp$1386 = 48 + _tmp$1387;
      d_hi$320 = (uint16_t)_tmp$1386;
      _tmp$1385 = d$319 % 10;
      _tmp$1384 = 48 + _tmp$1385;
      d_lo$321 = (uint16_t)_tmp$1384;
      _tmp$1380 = offset$304;
      _tmp$1379 = digit_start$306 + _tmp$1380;
      buffer$315[_tmp$1379] = d_hi$320;
      _tmp$1383 = offset$304;
      _tmp$1382 = digit_start$306 + _tmp$1383;
      _tmp$1381 = _tmp$1382 + 1;
      buffer$315[_tmp$1381] = d_lo$321;
      continue;
    }
    break;
  }
  _tmp$1390 = remaining$317;
  if (_tmp$1390 >= 10) {
    int32_t _tmp$1391 = offset$304;
    int32_t _tmp$1402;
    int32_t _tmp$1401;
    int32_t _tmp$1400;
    int32_t d_hi$323;
    int32_t _tmp$1399;
    int32_t _tmp$1398;
    int32_t _tmp$1397;
    int32_t d_lo$324;
    int32_t _tmp$1393;
    int32_t _tmp$1392;
    int32_t _tmp$1396;
    int32_t _tmp$1395;
    int32_t _tmp$1394;
    offset$304 = _tmp$1391 - 2;
    _tmp$1402 = remaining$317;
    _tmp$1401 = _tmp$1402 / 10;
    _tmp$1400 = 48 + _tmp$1401;
    d_hi$323 = (uint16_t)_tmp$1400;
    _tmp$1399 = remaining$317;
    _tmp$1398 = _tmp$1399 % 10;
    _tmp$1397 = 48 + _tmp$1398;
    d_lo$324 = (uint16_t)_tmp$1397;
    _tmp$1393 = offset$304;
    _tmp$1392 = digit_start$306 + _tmp$1393;
    buffer$315[_tmp$1392] = d_hi$323;
    _tmp$1396 = offset$304;
    _tmp$1395 = digit_start$306 + _tmp$1396;
    _tmp$1394 = _tmp$1395 + 1;
    buffer$315[_tmp$1394] = d_lo$324;
    moonbit_decref(buffer$315);
  } else {
    int32_t _tmp$1403 = offset$304;
    int32_t _tmp$1408;
    int32_t _tmp$1404;
    int32_t _tmp$1407;
    int32_t _tmp$1406;
    int32_t _tmp$1405;
    offset$304 = _tmp$1403 - 1;
    _tmp$1408 = offset$304;
    _tmp$1404 = digit_start$306 + _tmp$1408;
    _tmp$1407 = remaining$317;
    _tmp$1406 = 48 + _tmp$1407;
    _tmp$1405 = (uint16_t)_tmp$1406;
    buffer$315[_tmp$1404] = _tmp$1405;
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
  int32_t _tmp$1333 = radix$293 - 1;
  int32_t _tmp$1332 = radix$293 & _tmp$1333;
  if (_tmp$1332 == 0) {
    int32_t shift$294 = moonbit_ctz32(radix$293);
    uint32_t mask$295 = base$292 - 1u;
    while (1) {
      uint32_t _tmp$1334 = n$290;
      if (_tmp$1334 > 0u) {
        int32_t _tmp$1335 = offset$287;
        uint32_t _tmp$1342;
        uint32_t _tmp$1341;
        int32_t digit$296;
        int32_t _tmp$1339;
        int32_t _tmp$1336;
        int32_t _tmp$1338;
        int32_t _tmp$1337;
        uint32_t _tmp$1340;
        offset$287 = _tmp$1335 - 1;
        _tmp$1342 = n$290;
        _tmp$1341 = _tmp$1342 & mask$295;
        digit$296 = *(int32_t*)&_tmp$1341;
        _tmp$1339 = offset$287;
        _tmp$1336 = digit_start$289 + _tmp$1339;
        _tmp$1338
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$296
        ];
        _tmp$1337 = (uint16_t)_tmp$1338;
        buffer$297[_tmp$1336] = _tmp$1337;
        _tmp$1340 = n$290;
        n$290 = _tmp$1340 >> (shift$294 & 31);
        continue;
      } else {
        moonbit_decref(buffer$297);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1343 = n$290;
      if (_tmp$1343 > 0u) {
        int32_t _tmp$1344 = offset$287;
        uint32_t _tmp$1352;
        uint32_t q$299;
        uint32_t _tmp$1350;
        uint32_t _tmp$1351;
        uint32_t _tmp$1349;
        int32_t digit$300;
        int32_t _tmp$1348;
        int32_t _tmp$1345;
        int32_t _tmp$1347;
        int32_t _tmp$1346;
        offset$287 = _tmp$1344 - 1;
        _tmp$1352 = n$290;
        q$299 = _tmp$1352 / base$292;
        _tmp$1350 = n$290;
        _tmp$1351 = q$299 * base$292;
        _tmp$1349 = _tmp$1350 - _tmp$1351;
        digit$300 = *(int32_t*)&_tmp$1349;
        _tmp$1348 = offset$287;
        _tmp$1345 = digit_start$289 + _tmp$1348;
        _tmp$1347
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$300
        ];
        _tmp$1346 = (uint16_t)_tmp$1347;
        buffer$297[_tmp$1345] = _tmp$1346;
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
  int32_t _tmp$1327;
  while (1) {
    int32_t _tmp$1313 = offset$276;
    if (_tmp$1313 >= 2) {
      int32_t _tmp$1314 = offset$276;
      uint32_t _tmp$1326;
      uint32_t _tmp$1325;
      int32_t byte_val$281;
      int32_t hi$282;
      int32_t lo$283;
      int32_t _tmp$1318;
      int32_t _tmp$1315;
      int32_t _tmp$1317;
      int32_t _tmp$1316;
      int32_t _tmp$1323;
      int32_t _tmp$1322;
      int32_t _tmp$1319;
      int32_t _tmp$1321;
      int32_t _tmp$1320;
      uint32_t _tmp$1324;
      offset$276 = _tmp$1314 - 2;
      _tmp$1326 = n$279;
      _tmp$1325 = _tmp$1326 & 255u;
      byte_val$281 = *(int32_t*)&_tmp$1325;
      hi$282 = byte_val$281 / 16;
      lo$283 = byte_val$281 % 16;
      _tmp$1318 = offset$276;
      _tmp$1315 = digit_start$278 + _tmp$1318;
      _tmp$1317 = ((moonbit_string_t)moonbit_string_literal_26.data)[hi$282];
      _tmp$1316 = (uint16_t)_tmp$1317;
      buffer$284[_tmp$1315] = _tmp$1316;
      _tmp$1323 = offset$276;
      _tmp$1322 = digit_start$278 + _tmp$1323;
      _tmp$1319 = _tmp$1322 + 1;
      _tmp$1321 = ((moonbit_string_t)moonbit_string_literal_26.data)[lo$283];
      _tmp$1320 = (uint16_t)_tmp$1321;
      buffer$284[_tmp$1319] = _tmp$1320;
      _tmp$1324 = n$279;
      n$279 = _tmp$1324 >> 8;
      continue;
    }
    break;
  }
  _tmp$1327 = offset$276;
  if (_tmp$1327 == 1) {
    uint32_t _tmp$1331 = n$279;
    uint32_t _tmp$1330 = _tmp$1331 & 15u;
    int32_t nibble$286 = *(int32_t*)&_tmp$1330;
    int32_t _tmp$1329 =
      ((moonbit_string_t)moonbit_string_literal_26.data)[nibble$286];
    int32_t _tmp$1328 = (uint16_t)_tmp$1329;
    buffer$284[digit_start$278] = _tmp$1328;
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
  struct $$moonbitlang$core$builtin$Logger _tmp$1312;
  moonbit_incref(logger$274);
  _tmp$1312
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$274
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$275, _tmp$1312
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$274);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$273
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$272 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1311;
  moonbit_incref(logger$272);
  _tmp$1311
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$272
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$273, _tmp$1311
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$272);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$271
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$270 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1310;
  moonbit_incref(logger$270);
  _tmp$1310
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$270
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$271, _tmp$1310
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$270);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$269
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$268 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1309;
  moonbit_incref(logger$268);
  _tmp$1309
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$268
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$269, _tmp$1309);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$268);
}

int32_t $StringView$$start_offset(struct $StringView self$267) {
  int32_t _field$2354 = self$267.$1;
  moonbit_decref(self$267.$0);
  return _field$2354;
}

moonbit_string_t $StringView$$data(struct $StringView self$266) {
  moonbit_string_t _field$2355 = self$266.$0;
  return _field$2355;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$260,
  moonbit_string_t value$263,
  int32_t start$264,
  int32_t len$265
) {
  void* _try_err$262;
  struct $StringView _tmp$1304;
  int32_t _tmp$1306 = start$264 + len$265;
  int64_t _tmp$1305 = (int64_t)_tmp$1306;
  struct moonbit_result_1 _tmp$2622 =
    $String$$sub$inner(value$263, start$264, _tmp$1305);
  if (_tmp$2622.tag) {
    struct $StringView const _ok$1307 = _tmp$2622.data.ok;
    _tmp$1304 = _ok$1307;
  } else {
    void* const _err$1308 = _tmp$2622.data.err;
    _try_err$262 = _err$1308;
    goto $join$261;
  }
  goto $joinlet$2621;
  $join$261:;
  moonbit_decref(_try_err$262);
  moonbit_panic();
  $joinlet$2621:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$260, _tmp$1304
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
  int32_t _if_result$2623;
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
      _if_result$2623 = end$254 <= len$252;
    } else {
      _if_result$2623 = 0;
    }
  } else {
    _if_result$2623 = 0;
  }
  if (_if_result$2623) {
    int32_t _if_result$2624;
    int32_t _if_result$2626;
    struct $StringView _tmp$1302;
    struct moonbit_result_1 _result$2628;
    if (start$258 < len$252) {
      int32_t _tmp$1298 = self$253[start$258];
      _if_result$2624 = $Int$$is_trailing_surrogate(_tmp$1298);
    } else {
      _if_result$2624 = 0;
    }
    if (_if_result$2624) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1299;
      struct moonbit_result_1 _result$2625;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1299
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2625.tag = 0;
      _result$2625.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1299;
      return _result$2625;
    }
    if (end$254 < len$252) {
      int32_t _tmp$1300 = self$253[end$254];
      _if_result$2626 = $Int$$is_trailing_surrogate(_tmp$1300);
    } else {
      _if_result$2626 = 0;
    }
    if (_if_result$2626) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1301;
      struct moonbit_result_1 _result$2627;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1301
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2627.tag = 0;
      _result$2627.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1301;
      return _result$2627;
    }
    _tmp$1302 = (struct $StringView){start$258, end$254, self$253};
    _result$2628.tag = 1;
    _result$2628.data.ok = _tmp$1302;
    return _result$2628;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1303;
    struct moonbit_result_1 _result$2629;
    moonbit_decref(self$253);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1303
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$2629.tag = 0;
    _result$2629.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1303;
    return _result$2629;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$251
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$250 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1297;
  moonbit_incref(_self$250);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$250, self$251);
  _tmp$1297 = _self$250;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1297);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$249
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$248 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1296;
  moonbit_incref(_self$248);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$248, self$249);
  _tmp$1296 = _self$248;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1296);
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
  uint32_t _tmp$1295 = *(uint32_t*)&seed$244;
  uint32_t _tmp$1294 = _tmp$1295 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$2630 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$2630)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$2630->$0 = _tmp$1294;
  return _block$2630;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$243
) {
  uint32_t _tmp$1293 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$243);
  return *(int32_t*)&_tmp$1293;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$242
) {
  uint32_t _field$2356 = self$242->$0;
  uint32_t acc$241;
  uint32_t _tmp$1282;
  uint32_t _tmp$1284;
  uint32_t _tmp$1283;
  uint32_t _tmp$1285;
  uint32_t _tmp$1286;
  uint32_t _tmp$1288;
  uint32_t _tmp$1287;
  uint32_t _tmp$1289;
  uint32_t _tmp$1290;
  uint32_t _tmp$1292;
  uint32_t _tmp$1291;
  moonbit_decref(self$242);
  acc$241 = _field$2356;
  _tmp$1282 = acc$241;
  _tmp$1284 = acc$241;
  _tmp$1283 = _tmp$1284 >> 15;
  acc$241 = _tmp$1282 ^ _tmp$1283;
  _tmp$1285 = acc$241;
  acc$241 = _tmp$1285 * 2246822519u;
  _tmp$1286 = acc$241;
  _tmp$1288 = acc$241;
  _tmp$1287 = _tmp$1288 >> 13;
  acc$241 = _tmp$1286 ^ _tmp$1287;
  _tmp$1289 = acc$241;
  acc$241 = _tmp$1289 * 3266489917u;
  _tmp$1290 = acc$241;
  _tmp$1292 = acc$241;
  _tmp$1291 = _tmp$1292 >> 16;
  acc$241 = _tmp$1290 ^ _tmp$1291;
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
  uint32_t _tmp$1281 = *(uint32_t*)&value$236;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$235, _tmp$1281);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$233,
  uint32_t value$234
) {
  uint32_t acc$1280 = self$233->$0;
  uint32_t _tmp$1279 = acc$1280 + 4u;
  self$233->$0 = _tmp$1279;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$233, value$234);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$231,
  uint32_t input$232
) {
  uint32_t acc$1277 = self$231->$0;
  uint32_t _tmp$1278 = input$232 * 3266489917u;
  uint32_t _tmp$1276 = acc$1277 + _tmp$1278;
  uint32_t _tmp$1275 = $moonbitlang$core$builtin$rotl(_tmp$1276, 17);
  uint32_t _tmp$1274 = _tmp$1275 * 668265263u;
  self$231->$0 = _tmp$1274;
  moonbit_decref(self$231);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$229, int32_t r$230) {
  uint32_t _tmp$1271 = x$229 << (r$230 & 31);
  int32_t _tmp$1273 = 32 - r$230;
  uint32_t _tmp$1272 = x$229 >> (_tmp$1273 & 31);
  return _tmp$1271 | _tmp$1272;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$227,
  moonbit_string_t str$228
) {
  int32_t len$1261 = self$227->$1;
  int32_t _tmp$1263 = Moonbit_array_length(str$228);
  int32_t _tmp$1262 = _tmp$1263 * 2;
  int32_t _tmp$1260 = len$1261 + _tmp$1262;
  moonbit_bytes_t _field$2358;
  moonbit_bytes_t data$1264;
  int32_t len$1265;
  int32_t _tmp$1266;
  int32_t len$1268;
  int32_t _tmp$2357;
  int32_t _tmp$1270;
  int32_t _tmp$1269;
  int32_t _tmp$1267;
  moonbit_incref(self$227);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$227, _tmp$1260
  );
  _field$2358 = self$227->$0;
  data$1264 = _field$2358;
  len$1265 = self$227->$1;
  _tmp$1266 = Moonbit_array_length(str$228);
  moonbit_incref(data$1264);
  moonbit_incref(str$228);
  $FixedArray$$blit_from_string(data$1264, len$1265, str$228, 0, _tmp$1266);
  len$1268 = self$227->$1;
  _tmp$2357 = Moonbit_array_length(str$228);
  moonbit_decref(str$228);
  _tmp$1270 = _tmp$2357;
  _tmp$1269 = _tmp$1270 * 2;
  _tmp$1267 = len$1268 + _tmp$1269;
  self$227->$1 = _tmp$1267;
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
  int32_t _tmp$1259 = length$215 * 2;
  int32_t _tmp$1258 = bytes_offset$214 + _tmp$1259;
  int32_t e1$213 = _tmp$1258 - 1;
  int32_t _tmp$1257 = str_offset$217 + length$215;
  int32_t e2$216 = _tmp$1257 - 1;
  int32_t len1$218 = Moonbit_array_length(self$219);
  int32_t len2$220 = Moonbit_array_length(str$221);
  int32_t _if_result$2631;
  if (length$215 >= 0) {
    if (bytes_offset$214 >= 0) {
      if (e1$213 < len1$218) {
        if (str_offset$217 >= 0) {
          _if_result$2631 = e2$216 < len2$220;
        } else {
          _if_result$2631 = 0;
        }
      } else {
        _if_result$2631 = 0;
      }
    } else {
      _if_result$2631 = 0;
    }
  } else {
    _if_result$2631 = 0;
  }
  if (_if_result$2631) {
    int32_t end_str_offset$222 = str_offset$217 + length$215;
    int32_t i$223 = str_offset$217;
    int32_t j$224 = bytes_offset$214;
    while (1) {
      if (i$223 < end_str_offset$222) {
        int32_t _tmp$1254 = str$221[i$223];
        uint32_t c$225 = *(uint32_t*)&_tmp$1254;
        uint32_t _tmp$1250 = c$225 & 255u;
        int32_t _tmp$1249 = $UInt$$to_byte(_tmp$1250);
        int32_t _tmp$1251;
        uint32_t _tmp$1253;
        int32_t _tmp$1252;
        int32_t _tmp$1255;
        int32_t _tmp$1256;
        if (j$224 < 0 || j$224 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[j$224] = _tmp$1249;
        _tmp$1251 = j$224 + 1;
        _tmp$1253 = c$225 >> 8;
        _tmp$1252 = $UInt$$to_byte(_tmp$1253);
        if (_tmp$1251 < 0 || _tmp$1251 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[_tmp$1251] = _tmp$1252;
        _tmp$1255 = i$223 + 1;
        _tmp$1256 = j$224 + 2;
        i$223 = _tmp$1255;
        j$224 = _tmp$1256;
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
  int32_t _tmp$1222 = Moonbit_array_length(repr$181);
  int64_t _tmp$1221 = (int64_t)_tmp$1222;
  moonbit_incref(repr$181);
  if ($String$$char_length_ge$inner(repr$181, 1, 0, _tmp$1221)) {
    int32_t _tmp$1248 = repr$181[0];
    int32_t _x$182 = _tmp$1248;
    if (_x$182 == 64) {
      int32_t _tmp$1247 = Moonbit_array_length(repr$181);
      int64_t _tmp$1246 = (int64_t)_tmp$1247;
      int64_t _bind$1013;
      int32_t _tmp$1244;
      int32_t _tmp$1245;
      struct $StringView _x$183;
      int32_t _tmp$1243;
      struct $StringView _tmp$1242;
      int64_t _bind$185;
      moonbit_incref(repr$181);
      _bind$1013
      = $String$$offset_of_nth_char$inner(
        repr$181, 1, 0, _tmp$1246
      );
      if (_bind$1013 == 4294967296ll) {
        _tmp$1244 = Moonbit_array_length(repr$181);
      } else {
        int64_t _Some$184 = _bind$1013;
        _tmp$1244 = (int32_t)_Some$184;
      }
      _tmp$1245 = Moonbit_array_length(repr$181);
      _x$183 = (struct $StringView){_tmp$1244, _tmp$1245, repr$181};
      _tmp$1243
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1242
      = (struct $StringView){
        0, _tmp$1243, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$183.$0);
      _bind$185 = $StringView$$find(_x$183, _tmp$1242);
      if (_bind$185 == 4294967296ll) {
        moonbit_decref(_x$183.$0);
        moonbit_panic();
      } else {
        int64_t _Some$186 = _bind$185;
        int32_t _pkg_end$187 = (int32_t)_Some$186;
        int64_t _tmp$1241 = (int64_t)_pkg_end$187;
        struct $StringView pkg$188;
        int32_t _tmp$1240;
        struct $StringView _tmp$1239;
        int64_t _bind$189;
        moonbit_incref(_x$183.$0);
        pkg$188 = $StringView$$view$inner(_x$183, 0, _tmp$1241);
        _tmp$1240
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1239
        = (struct $StringView){
          0, _tmp$1240, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$183.$0);
        _bind$189 = $StringView$$rev_find(_x$183, _tmp$1239);
        if (_bind$189 == 4294967296ll) {
          moonbit_decref(pkg$188.$0);
          moonbit_decref(_x$183.$0);
          moonbit_panic();
        } else {
          int64_t _Some$190 = _bind$189;
          int32_t _start_loc_end$191 = (int32_t)_Some$190;
          int32_t _tmp$1223 = _start_loc_end$191 + 1;
          int32_t _tmp$1224;
          moonbit_incref(_x$183.$0);
          _tmp$1224 = $StringView$$length(_x$183);
          if (_tmp$1223 < _tmp$1224) {
            int32_t _tmp$1238 = _start_loc_end$191 + 1;
            struct $StringView end_loc$192;
            struct $$3c$StringView$2a$StringView$3e$* _bind$193;
            moonbit_incref(_x$183.$0);
            end_loc$192
            = $StringView$$view$inner(
              _x$183, _tmp$1238, 4294967296ll
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
              struct $StringView _field$2362 =
                (struct $StringView){
                  _x$195->$0_1, _x$195->$0_2, _x$195->$0_0
                };
              struct $StringView _end_line$196 = _field$2362;
              struct $StringView _field$2361 =
                (struct $StringView){
                  _x$195->$1_1, _x$195->$1_2, _x$195->$1_0
                };
              int32_t _cnt$2506 = Moonbit_object_header(_x$195)->rc;
              struct $StringView _end_column$197;
              int64_t _tmp$1237;
              struct $StringView rest$198;
              int32_t _tmp$1236;
              struct $StringView _tmp$1235;
              int64_t _bind$200;
              if (_cnt$2506 > 1) {
                int32_t _new_cnt$2507;
                moonbit_incref(_field$2361.$0);
                moonbit_incref(_end_line$196.$0);
                _new_cnt$2507 = _cnt$2506 - 1;
                Moonbit_object_header(_x$195)->rc = _new_cnt$2507;
              } else if (_cnt$2506 == 1) {
                moonbit_free(_x$195);
              }
              _end_column$197 = _field$2361;
              _tmp$1237 = (int64_t)_start_loc_end$191;
              rest$198 = $StringView$$view$inner(_x$183, 0, _tmp$1237);
              _tmp$1236
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1235
              = (struct $StringView){
                0,
                  _tmp$1236,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$198.$0);
              _bind$200 = $StringView$$rev_find(rest$198, _tmp$1235);
              if (_bind$200 == 4294967296ll) {
                moonbit_decref(rest$198.$0);
                moonbit_decref(_end_column$197.$0);
                moonbit_decref(_end_line$196.$0);
                moonbit_decref(pkg$188.$0);
                goto $join$199;
              } else {
                int64_t _Some$201 = _bind$200;
                int32_t _start_line_end$202 = (int32_t)_Some$201;
                int64_t _tmp$1234 = (int64_t)_start_line_end$202;
                struct $StringView _tmp$1231;
                int32_t _tmp$1233;
                struct $StringView _tmp$1232;
                int64_t _bind$203;
                moonbit_incref(rest$198.$0);
                _tmp$1231 = $StringView$$view$inner(rest$198, 0, _tmp$1234);
                _tmp$1233
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1232
                = (struct $StringView){
                  0,
                    _tmp$1233,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$203 = $StringView$$rev_find(_tmp$1231, _tmp$1232);
                if (_bind$203 == 4294967296ll) {
                  moonbit_decref(rest$198.$0);
                  moonbit_decref(_end_column$197.$0);
                  moonbit_decref(_end_line$196.$0);
                  moonbit_decref(pkg$188.$0);
                  goto $join$199;
                } else {
                  int64_t _Some$204 = _bind$203;
                  int32_t _filename_end$205 = (int32_t)_Some$204;
                  int32_t _tmp$1225 = _filename_end$205 + 1;
                  int32_t _tmp$1226;
                  moonbit_incref(rest$198.$0);
                  _tmp$1226 = $StringView$$length(rest$198);
                  if (_tmp$1225 < _tmp$1226) {
                    int32_t _tmp$1230 = _filename_end$205 + 1;
                    struct $StringView start_loc$206;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$207;
                    moonbit_incref(rest$198.$0);
                    start_loc$206
                    = $StringView$$view$inner(
                      rest$198, _tmp$1230, 4294967296ll
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
                      struct $StringView _field$2360 =
                        (struct $StringView){
                          _x$209->$0_1, _x$209->$0_2, _x$209->$0_0
                        };
                      struct $StringView _start_line$210 = _field$2360;
                      struct $StringView _field$2359 =
                        (struct $StringView){
                          _x$209->$1_1, _x$209->$1_2, _x$209->$1_0
                        };
                      int32_t _cnt$2508 = Moonbit_object_header(_x$209)->rc;
                      struct $StringView _start_column$211;
                      int32_t _tmp$1227;
                      if (_cnt$2508 > 1) {
                        int32_t _new_cnt$2509;
                        moonbit_incref(_field$2359.$0);
                        moonbit_incref(_start_line$210.$0);
                        _new_cnt$2509 = _cnt$2508 - 1;
                        Moonbit_object_header(_x$209)->rc = _new_cnt$2509;
                      } else if (_cnt$2508 == 1) {
                        moonbit_free(_x$209);
                      }
                      _start_column$211 = _field$2359;
                      _tmp$1227 = _pkg_end$187 + 1;
                      if (_filename_end$205 > _tmp$1227) {
                        int32_t _tmp$1228 = _pkg_end$187 + 1;
                        int64_t _tmp$1229 = (int64_t)_filename_end$205;
                        struct $StringView filename$212 =
                          $StringView$$view$inner(
                            rest$198, _tmp$1228, _tmp$1229
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$2635 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$2635)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$2635->$0_0 = pkg$188.$0;
                        _block$2635->$0_1 = pkg$188.$1;
                        _block$2635->$0_2 = pkg$188.$2;
                        _block$2635->$1_0 = filename$212.$0;
                        _block$2635->$1_1 = filename$212.$1;
                        _block$2635->$1_2 = filename$212.$2;
                        _block$2635->$2_0 = _start_line$210.$0;
                        _block$2635->$2_1 = _start_line$210.$1;
                        _block$2635->$2_2 = _start_line$210.$2;
                        _block$2635->$3_0 = _start_column$211.$0;
                        _block$2635->$3_1 = _start_column$211.$1;
                        _block$2635->$3_2 = _start_column$211.$2;
                        _block$2635->$4_0 = _end_line$196.$0;
                        _block$2635->$4_1 = _end_line$196.$1;
                        _block$2635->$4_2 = _end_line$196.$2;
                        _block$2635->$5_0 = _end_column$197.$0;
                        _block$2635->$5_1 = _end_column$197.$1;
                        _block$2635->$5_2 = _end_column$197.$2;
                        return _block$2635;
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
  int32_t _tmp$1220 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1219;
  int64_t _bind$176;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1219
  = (struct $StringView){
    0, _tmp$1220, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$177.$0);
  _bind$176 = $StringView$$find(view$177, _tmp$1219);
  if (_bind$176 == 4294967296ll) {
    moonbit_decref(view$177.$0);
    return 0;
  } else {
    int64_t _Some$178 = _bind$176;
    int32_t _i$179 = (int32_t)_Some$178;
    int32_t _if_result$2636;
    if (_i$179 > 0) {
      int32_t _tmp$1212 = _i$179 + 1;
      int32_t _tmp$1213;
      moonbit_incref(view$177.$0);
      _tmp$1213 = $StringView$$length(view$177);
      _if_result$2636 = _tmp$1212 < _tmp$1213;
    } else {
      _if_result$2636 = 0;
    }
    if (_if_result$2636) {
      int64_t _tmp$1218 = (int64_t)_i$179;
      struct $StringView _tmp$1215;
      int32_t _tmp$1217;
      struct $StringView _tmp$1216;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1214;
      moonbit_incref(view$177.$0);
      _tmp$1215 = $StringView$$view$inner(view$177, 0, _tmp$1218);
      _tmp$1217 = _i$179 + 1;
      _tmp$1216 = $StringView$$view$inner(view$177, _tmp$1217, 4294967296ll);
      _tuple$1214
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1214)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1214->$0_0 = _tmp$1215.$0;
      _tuple$1214->$0_1 = _tmp$1215.$1;
      _tuple$1214->$0_2 = _tmp$1215.$2;
      _tuple$1214->$1_0 = _tmp$1216.$0;
      _tuple$1214->$1_1 = _tmp$1216.$1;
      _tuple$1214->$1_2 = _tmp$1216.$2;
      return _tuple$1214;
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
  int32_t _if_result$2637;
  if (end_offset$172 == 4294967296ll) {
    moonbit_incref(self$174.$0);
    end_offset$171 = $StringView$$length(self$174);
  } else {
    int64_t _Some$173 = end_offset$172;
    end_offset$171 = (int32_t)_Some$173;
  }
  if (start_offset$175 >= 0) {
    if (start_offset$175 <= end_offset$171) {
      int32_t _tmp$1206;
      moonbit_incref(self$174.$0);
      _tmp$1206 = $StringView$$length(self$174);
      _if_result$2637 = end_offset$171 <= _tmp$1206;
    } else {
      _if_result$2637 = 0;
    }
  } else {
    _if_result$2637 = 0;
  }
  if (_if_result$2637) {
    moonbit_string_t _field$2364 = self$174.$0;
    moonbit_string_t str$1207 = _field$2364;
    int32_t start$1211 = self$174.$1;
    int32_t _tmp$1208 = start$1211 + start_offset$175;
    int32_t _field$2363 = self$174.$1;
    int32_t start$1210 = _field$2363;
    int32_t _tmp$1209 = start$1210 + end_offset$171;
    return (struct $StringView){_tmp$1208, _tmp$1209, str$1207};
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
  int32_t _tmp$1205;
  moonbit_incref(str$169.$0);
  _tmp$1205 = $StringView$$length(str$169);
  if (_tmp$1205 <= 4) {
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
        int32_t _tmp$1192 = i$164;
        if (_tmp$1192 >= 0) {
          int32_t _tmp$1197;
          while (1) {
            int32_t _tmp$1195 = i$164;
            int32_t _if_result$2640;
            if (_tmp$1195 >= 0) {
              int32_t _tmp$1194 = i$164;
              int32_t _tmp$1193;
              moonbit_incref(haystack$160.$0);
              _tmp$1193
              = $StringView$$unsafe_charcode_at(
                haystack$160, _tmp$1194
              );
              _if_result$2640 = _tmp$1193 != needle_first$163;
            } else {
              _if_result$2640 = 0;
            }
            if (_if_result$2640) {
              int32_t _tmp$1196 = i$164;
              i$164 = _tmp$1196 - 1;
              continue;
            }
            break;
          }
          _tmp$1197 = i$164;
          if (_tmp$1197 >= 0) {
            int32_t j$166 = 1;
            int32_t _tmp$1204;
            while (1) {
              if (j$166 < needle_len$161) {
                int32_t _tmp$1201 = i$164;
                int32_t _tmp$1200 = _tmp$1201 + j$166;
                int32_t _tmp$1198;
                int32_t _tmp$1199;
                int32_t _tmp$1202;
                moonbit_incref(haystack$160.$0);
                _tmp$1198
                = $StringView$$unsafe_charcode_at(
                  haystack$160, _tmp$1200
                );
                moonbit_incref(needle$162.$0);
                _tmp$1199
                = $StringView$$unsafe_charcode_at(
                  needle$162, j$166
                );
                if (_tmp$1198 != _tmp$1199) {
                  break;
                }
                _tmp$1202 = j$166 + 1;
                j$166 = _tmp$1202;
                continue;
              } else {
                int32_t _tmp$1203;
                moonbit_decref(needle$162.$0);
                moonbit_decref(haystack$160.$0);
                _tmp$1203 = i$164;
                return (int64_t)_tmp$1203;
              }
              break;
            }
            _tmp$1204 = i$164;
            i$164 = _tmp$1204 - 1;
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
      int32_t _tmp$1182 = needle_len$150 - 1;
      int32_t i$153 = _tmp$1182;
      int32_t _tmp$1191;
      int32_t i$155;
      while (1) {
        if (i$153 > 0) {
          int32_t _tmp$1180;
          int32_t _tmp$1179;
          int32_t _tmp$1181;
          moonbit_incref(needle$151.$0);
          _tmp$1180 = $StringView$$unsafe_charcode_at(needle$151, i$153);
          _tmp$1179 = _tmp$1180 & 255;
          if (
            _tmp$1179 < 0
            || _tmp$1179 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          skip_table$152[_tmp$1179] = i$153;
          _tmp$1181 = i$153 - 1;
          i$153 = _tmp$1181;
          continue;
        }
        break;
      }
      _tmp$1191 = haystack_len$148 - needle_len$150;
      i$155 = _tmp$1191;
      while (1) {
        if (i$155 >= 0) {
          int32_t j$156 = 0;
          int32_t _tmp$1190;
          int32_t _tmp$1189;
          int32_t _tmp$1188;
          int32_t _tmp$1187;
          while (1) {
            if (j$156 < needle_len$150) {
              int32_t _tmp$1185 = i$155 + j$156;
              int32_t _tmp$1183;
              int32_t _tmp$1184;
              int32_t _tmp$1186;
              moonbit_incref(haystack$149.$0);
              _tmp$1183
              = $StringView$$unsafe_charcode_at(
                haystack$149, _tmp$1185
              );
              moonbit_incref(needle$151.$0);
              _tmp$1184 = $StringView$$unsafe_charcode_at(needle$151, j$156);
              if (_tmp$1183 != _tmp$1184) {
                break;
              }
              _tmp$1186 = j$156 + 1;
              j$156 = _tmp$1186;
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
          _tmp$1190 = $StringView$$unsafe_charcode_at(haystack$149, i$155);
          _tmp$1189 = _tmp$1190 & 255;
          if (
            _tmp$1189 < 0
            || _tmp$1189 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          _tmp$1188 = (int32_t)skip_table$152[_tmp$1189];
          _tmp$1187 = i$155 - _tmp$1188;
          i$155 = _tmp$1187;
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
  int32_t _tmp$1178;
  moonbit_incref(str$146.$0);
  _tmp$1178 = $StringView$$length(str$146);
  if (_tmp$1178 <= 4) {
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
        int32_t _tmp$1165 = i$141;
        if (_tmp$1165 <= forward_len$140) {
          int32_t _tmp$1170;
          while (1) {
            int32_t _tmp$1168 = i$141;
            int32_t _if_result$2647;
            if (_tmp$1168 <= forward_len$140) {
              int32_t _tmp$1167 = i$141;
              int32_t _tmp$1166;
              moonbit_incref(haystack$136.$0);
              _tmp$1166
              = $StringView$$unsafe_charcode_at(
                haystack$136, _tmp$1167
              );
              _if_result$2647 = _tmp$1166 != needle_first$139;
            } else {
              _if_result$2647 = 0;
            }
            if (_if_result$2647) {
              int32_t _tmp$1169 = i$141;
              i$141 = _tmp$1169 + 1;
              continue;
            }
            break;
          }
          _tmp$1170 = i$141;
          if (_tmp$1170 <= forward_len$140) {
            int32_t j$143 = 1;
            int32_t _tmp$1177;
            while (1) {
              if (j$143 < needle_len$137) {
                int32_t _tmp$1174 = i$141;
                int32_t _tmp$1173 = _tmp$1174 + j$143;
                int32_t _tmp$1171;
                int32_t _tmp$1172;
                int32_t _tmp$1175;
                moonbit_incref(haystack$136.$0);
                _tmp$1171
                = $StringView$$unsafe_charcode_at(
                  haystack$136, _tmp$1173
                );
                moonbit_incref(needle$138.$0);
                _tmp$1172
                = $StringView$$unsafe_charcode_at(
                  needle$138, j$143
                );
                if (_tmp$1171 != _tmp$1172) {
                  break;
                }
                _tmp$1175 = j$143 + 1;
                j$143 = _tmp$1175;
                continue;
              } else {
                int32_t _tmp$1176;
                moonbit_decref(needle$138.$0);
                moonbit_decref(haystack$136.$0);
                _tmp$1176 = i$141;
                return (int64_t)_tmp$1176;
              }
              break;
            }
            _tmp$1177 = i$141;
            i$141 = _tmp$1177 + 1;
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
          int32_t _tmp$1152;
          int32_t _tmp$1149;
          int32_t _tmp$1151;
          int32_t _tmp$1150;
          int32_t _tmp$1153;
          moonbit_incref(needle$124.$0);
          _tmp$1152 = $StringView$$unsafe_charcode_at(needle$124, i$127);
          _tmp$1149 = _tmp$1152 & 255;
          _tmp$1151 = needle_len$123 - 1;
          _tmp$1150 = _tmp$1151 - i$127;
          if (
            _tmp$1149 < 0
            || _tmp$1149 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          skip_table$125[_tmp$1149] = _tmp$1150;
          _tmp$1153 = i$127 + 1;
          i$127 = _tmp$1153;
          continue;
        }
        break;
      }
      i$129 = 0;
      while (1) {
        int32_t _tmp$1154 = haystack_len$121 - needle_len$123;
        if (i$129 <= _tmp$1154) {
          int32_t _end4307$130 = needle_len$123 - 1;
          int32_t j$131 = 0;
          int32_t _tmp$1164;
          int32_t _tmp$1163;
          int32_t _tmp$1162;
          int32_t _tmp$1161;
          int32_t _tmp$1160;
          int32_t _tmp$1159;
          while (1) {
            if (j$131 <= _end4307$130) {
              int32_t _tmp$1157 = i$129 + j$131;
              int32_t _tmp$1155;
              int32_t _tmp$1156;
              int32_t _tmp$1158;
              moonbit_incref(haystack$122.$0);
              _tmp$1155
              = $StringView$$unsafe_charcode_at(
                haystack$122, _tmp$1157
              );
              moonbit_incref(needle$124.$0);
              _tmp$1156 = $StringView$$unsafe_charcode_at(needle$124, j$131);
              if (_tmp$1155 != _tmp$1156) {
                break;
              }
              _tmp$1158 = j$131 + 1;
              j$131 = _tmp$1158;
              continue;
            } else {
              moonbit_decref(skip_table$125);
              moonbit_decref(needle$124.$0);
              moonbit_decref(haystack$122.$0);
              return (int64_t)i$129;
            }
            break;
          }
          _tmp$1164 = i$129 + needle_len$123;
          _tmp$1163 = _tmp$1164 - 1;
          moonbit_incref(haystack$122.$0);
          _tmp$1162
          = $StringView$$unsafe_charcode_at(
            haystack$122, _tmp$1163
          );
          _tmp$1161 = _tmp$1162 & 255;
          if (
            _tmp$1161 < 0
            || _tmp$1161 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          _tmp$1160 = (int32_t)skip_table$125[_tmp$1161];
          _tmp$1159 = i$129 + _tmp$1160;
          i$129 = _tmp$1159;
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
  moonbit_string_t _field$2367 = self$118.$0;
  moonbit_string_t str$1146 = _field$2367;
  int32_t _field$2366 = self$118.$1;
  int32_t start$1148 = _field$2366;
  int32_t _tmp$1147 = start$1148 + index$119;
  int32_t _tmp$2365 = str$1146[_tmp$1147];
  moonbit_decref(str$1146);
  return _tmp$2365;
}

int32_t $StringView$$length(struct $StringView self$117) {
  int32_t end$1144 = self$117.$2;
  int32_t _field$2368 = self$117.$1;
  int32_t start$1145;
  moonbit_decref(self$117.$0);
  start$1145 = _field$2368;
  return end$1144 - start$1145;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$115,
  int32_t index$116
) {
  int32_t len$114 = self$115->$1;
  int32_t _if_result$2652;
  if (index$116 >= 0) {
    _if_result$2652 = index$116 < len$114;
  } else {
    _if_result$2652 = 0;
  }
  if (_if_result$2652) {
    moonbit_string_t* _tmp$1143 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$115);
    moonbit_string_t _tmp$2369;
    if (index$116 < 0 || index$116 >= Moonbit_array_length(_tmp$1143)) {
      moonbit_panic();
    }
    _tmp$2369 = (moonbit_string_t)_tmp$1143[index$116];
    moonbit_incref(_tmp$2369);
    moonbit_decref(_tmp$1143);
    return _tmp$2369;
  } else {
    moonbit_decref(self$115);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$113
) {
  int32_t _field$2370 = self$113->$1;
  moonbit_decref(self$113);
  return _field$2370;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$112
) {
  int32_t _field$2371 = self$112->$1;
  moonbit_decref(self$112);
  return _field$2371;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$111
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2372 =
    self$111->$0;
  int32_t _cnt$2510 = Moonbit_object_header(self$111)->rc;
  if (_cnt$2510 > 1) {
    int32_t _new_cnt$2511;
    moonbit_incref(_field$2372);
    _new_cnt$2511 = _cnt$2510 - 1;
    Moonbit_object_header(self$111)->rc = _new_cnt$2511;
  } else if (_cnt$2510 == 1) {
    moonbit_free(self$111);
  }
  return _field$2372;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$110
) {
  moonbit_string_t* _field$2373 = self$110->$0;
  int32_t _cnt$2512 = Moonbit_object_header(self$110)->rc;
  if (_cnt$2512 > 1) {
    int32_t _new_cnt$2513;
    moonbit_incref(_field$2373);
    _new_cnt$2513 = _cnt$2512 - 1;
    Moonbit_object_header(self$110)->rc = _new_cnt$2513;
  } else if (_cnt$2512 == 1) {
    moonbit_free(self$110);
  }
  return _field$2373;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$109
) {
  struct $$3c$String$2a$Int$3e$** _field$2374 = self$109->$0;
  int32_t _cnt$2514 = Moonbit_object_header(self$109)->rc;
  if (_cnt$2514 > 1) {
    int32_t _new_cnt$2515;
    moonbit_incref(_field$2374);
    _new_cnt$2515 = _cnt$2514 - 1;
    Moonbit_object_header(self$109)->rc = _new_cnt$2515;
  } else if (_cnt$2514 == 1) {
    moonbit_free(self$109);
  }
  return _field$2374;
}

moonbit_string_t $String$$escape(moonbit_string_t self$108) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$107 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1142;
  moonbit_incref(buf$107);
  _tmp$1142
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$107
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$108, _tmp$1142);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$107);
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$105, int32_t y$106) {
  int32_t _tmp$1141 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$105, y$106
    );
  return !_tmp$1141;
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
  int32_t len$1136 = self$100->$1;
  int32_t _tmp$1135 = len$1136 + 4;
  moonbit_bytes_t _field$2375;
  moonbit_bytes_t data$1139;
  int32_t len$1140;
  int32_t inc$101;
  int32_t len$1138;
  int32_t _tmp$1137;
  moonbit_incref(self$100);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$100, _tmp$1135
  );
  _field$2375 = self$100->$0;
  data$1139 = _field$2375;
  len$1140 = self$100->$1;
  moonbit_incref(data$1139);
  inc$101 = $FixedArray$$set_utf16le_char(data$1139, len$1140, ch$102);
  len$1138 = self$100->$1;
  _tmp$1137 = len$1138 + inc$101;
  self$100->$1 = _tmp$1137;
  moonbit_decref(self$100);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$95,
  int32_t required$96
) {
  moonbit_bytes_t _field$2379 = self$95->$0;
  moonbit_bytes_t data$1134 = _field$2379;
  int32_t _tmp$2378 = Moonbit_array_length(data$1134);
  int32_t current_len$94 = _tmp$2378;
  int32_t enough_space$97;
  int32_t _tmp$1132;
  int32_t _tmp$1133;
  moonbit_bytes_t new_data$99;
  moonbit_bytes_t _field$2377;
  moonbit_bytes_t data$1130;
  int32_t len$1131;
  moonbit_bytes_t _old$2376;
  if (required$96 <= current_len$94) {
    moonbit_decref(self$95);
    return 0;
  }
  enough_space$97 = current_len$94;
  while (1) {
    int32_t _tmp$1128 = enough_space$97;
    if (_tmp$1128 < required$96) {
      int32_t _tmp$1129 = enough_space$97;
      enough_space$97 = _tmp$1129 * 2;
      continue;
    }
    break;
  }
  _tmp$1132 = enough_space$97;
  _tmp$1133 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$99 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1132, _tmp$1133);
  _field$2377 = self$95->$0;
  data$1130 = _field$2377;
  len$1131 = self$95->$1;
  moonbit_incref(data$1130);
  moonbit_incref(new_data$99);
  $FixedArray$$unsafe_blit$0(new_data$99, 0, data$1130, 0, len$1131);
  _old$2376 = self$95->$0;
  moonbit_decref(_old$2376);
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
    uint32_t _tmp$1111 = code$87 & 255u;
    int32_t _tmp$1110 = $UInt$$to_byte(_tmp$1111);
    int32_t _tmp$1112;
    uint32_t _tmp$1114;
    int32_t _tmp$1113;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1110;
    _tmp$1112 = offset$90 + 1;
    _tmp$1114 = code$87 >> 8;
    _tmp$1113 = $UInt$$to_byte(_tmp$1114);
    if (_tmp$1112 < 0 || _tmp$1112 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1112] = _tmp$1113;
    moonbit_decref(self$89);
    return 2;
  } else if (code$87 < 1114112u) {
    uint32_t hi$91 = code$87 - 65536u;
    uint32_t _tmp$1127 = hi$91 >> 10;
    uint32_t lo$92 = _tmp$1127 | 55296u;
    uint32_t _tmp$1126 = hi$91 & 1023u;
    uint32_t hi$93 = _tmp$1126 | 56320u;
    uint32_t _tmp$1116 = lo$92 & 255u;
    int32_t _tmp$1115 = $UInt$$to_byte(_tmp$1116);
    int32_t _tmp$1117;
    uint32_t _tmp$1119;
    int32_t _tmp$1118;
    int32_t _tmp$1120;
    uint32_t _tmp$1122;
    int32_t _tmp$1121;
    int32_t _tmp$1123;
    uint32_t _tmp$1125;
    int32_t _tmp$1124;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1115;
    _tmp$1117 = offset$90 + 1;
    _tmp$1119 = lo$92 >> 8;
    _tmp$1118 = $UInt$$to_byte(_tmp$1119);
    if (_tmp$1117 < 0 || _tmp$1117 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1117] = _tmp$1118;
    _tmp$1120 = offset$90 + 2;
    _tmp$1122 = hi$93 & 255u;
    _tmp$1121 = $UInt$$to_byte(_tmp$1122);
    if (_tmp$1120 < 0 || _tmp$1120 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1120] = _tmp$1121;
    _tmp$1123 = offset$90 + 3;
    _tmp$1125 = hi$93 >> 8;
    _tmp$1124 = $UInt$$to_byte(_tmp$1125);
    if (_tmp$1123 < 0 || _tmp$1123 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1123] = _tmp$1124;
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
  int32_t _tmp$1109 = *(int32_t*)&self$86;
  return _tmp$1109 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$85) {
  int32_t _tmp$1108 = self$85;
  return *(uint32_t*)&_tmp$1108;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$84
) {
  moonbit_bytes_t _field$2381 = self$84->$0;
  moonbit_bytes_t data$1107 = _field$2381;
  moonbit_bytes_t _tmp$1104;
  int32_t _field$2380;
  int32_t len$1106;
  int64_t _tmp$1105;
  moonbit_incref(data$1107);
  _tmp$1104 = data$1107;
  _field$2380 = self$84->$1;
  moonbit_decref(self$84);
  len$1106 = _field$2380;
  _tmp$1105 = (int64_t)len$1106;
  return $Bytes$$to_unchecked_string$inner(_tmp$1104, 0, _tmp$1105);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$79,
  int32_t offset$83,
  int64_t length$81
) {
  int32_t len$78 = Moonbit_array_length(self$79);
  int32_t length$80;
  int32_t _if_result$2654;
  if (length$81 == 4294967296ll) {
    length$80 = len$78 - offset$83;
  } else {
    int64_t _Some$82 = length$81;
    length$80 = (int32_t)_Some$82;
  }
  if (offset$83 >= 0) {
    if (length$80 >= 0) {
      int32_t _tmp$1103 = offset$83 + length$80;
      _if_result$2654 = _tmp$1103 <= len$78;
    } else {
      _if_result$2654 = 0;
    }
  } else {
    _if_result$2654 = 0;
  }
  if (_if_result$2654) {
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
  struct $$moonbitlang$core$builtin$StringBuilder* _block$2655;
  if (size_hint$76 < 1) {
    initial$75 = 1;
  } else {
    initial$75 = size_hint$76;
  }
  data$77 = (moonbit_bytes_t)moonbit_make_bytes(initial$75, 0);
  _block$2655
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$2655)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$2655->$0 = data$77;
  _block$2655->$1 = 0;
  return _block$2655;
}

int32_t $Byte$$to_char(int32_t self$74) {
  int32_t _tmp$1102 = (int32_t)self$74;
  return _tmp$1102;
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
  int32_t _if_result$2656;
  if (dst$50 == src$51) {
    _if_result$2656 = dst_offset$52 < src_offset$53;
  } else {
    _if_result$2656 = 0;
  }
  if (_if_result$2656) {
    int32_t i$54 = 0;
    while (1) {
      if (i$54 < len$55) {
        int32_t _tmp$1093 = dst_offset$52 + i$54;
        int32_t _tmp$1095 = src_offset$53 + i$54;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2383;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1094;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2382;
        int32_t _tmp$1096;
        if (_tmp$1095 < 0 || _tmp$1095 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2383
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1095
          ];
        _tmp$1094 = _tmp$2383;
        if (_tmp$1093 < 0 || _tmp$1093 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2382
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1093
          ];
        if (_tmp$1094) {
          moonbit_incref(_tmp$1094);
        }
        if (_old$2382) {
          moonbit_decref(_old$2382);
        }
        dst$50[_tmp$1093] = _tmp$1094;
        _tmp$1096 = i$54 + 1;
        i$54 = _tmp$1096;
        continue;
      } else {
        moonbit_decref(src$51);
        moonbit_decref(dst$50);
      }
      break;
    }
  } else {
    int32_t _tmp$1101 = len$55 - 1;
    int32_t i$57 = _tmp$1101;
    while (1) {
      if (i$57 >= 0) {
        int32_t _tmp$1097 = dst_offset$52 + i$57;
        int32_t _tmp$1099 = src_offset$53 + i$57;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2385;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1098;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2384;
        int32_t _tmp$1100;
        if (_tmp$1099 < 0 || _tmp$1099 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2385
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1099
          ];
        _tmp$1098 = _tmp$2385;
        if (_tmp$1097 < 0 || _tmp$1097 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2384
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1097
          ];
        if (_tmp$1098) {
          moonbit_incref(_tmp$1098);
        }
        if (_old$2384) {
          moonbit_decref(_old$2384);
        }
        dst$50[_tmp$1097] = _tmp$1098;
        _tmp$1100 = i$57 - 1;
        i$57 = _tmp$1100;
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
  int32_t _if_result$2659;
  if (dst$41 == src$42) {
    _if_result$2659 = dst_offset$43 < src_offset$44;
  } else {
    _if_result$2659 = 0;
  }
  if (_if_result$2659) {
    int32_t i$45 = 0;
    while (1) {
      if (i$45 < len$46) {
        int32_t _tmp$1084 = dst_offset$43 + i$45;
        int32_t _tmp$1086 = src_offset$44 + i$45;
        struct $$3c$String$2a$Int$3e$* _tmp$2387;
        struct $$3c$String$2a$Int$3e$* _tmp$1085;
        struct $$3c$String$2a$Int$3e$* _old$2386;
        int32_t _tmp$1087;
        if (_tmp$1086 < 0 || _tmp$1086 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2387 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1086];
        _tmp$1085 = _tmp$2387;
        if (_tmp$1084 < 0 || _tmp$1084 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2386 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1084];
        if (_tmp$1085) {
          moonbit_incref(_tmp$1085);
        }
        if (_old$2386) {
          moonbit_decref(_old$2386);
        }
        dst$41[_tmp$1084] = _tmp$1085;
        _tmp$1087 = i$45 + 1;
        i$45 = _tmp$1087;
        continue;
      } else {
        moonbit_decref(src$42);
        moonbit_decref(dst$41);
      }
      break;
    }
  } else {
    int32_t _tmp$1092 = len$46 - 1;
    int32_t i$48 = _tmp$1092;
    while (1) {
      if (i$48 >= 0) {
        int32_t _tmp$1088 = dst_offset$43 + i$48;
        int32_t _tmp$1090 = src_offset$44 + i$48;
        struct $$3c$String$2a$Int$3e$* _tmp$2389;
        struct $$3c$String$2a$Int$3e$* _tmp$1089;
        struct $$3c$String$2a$Int$3e$* _old$2388;
        int32_t _tmp$1091;
        if (_tmp$1090 < 0 || _tmp$1090 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2389 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1090];
        _tmp$1089 = _tmp$2389;
        if (_tmp$1088 < 0 || _tmp$1088 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2388 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1088];
        if (_tmp$1089) {
          moonbit_incref(_tmp$1089);
        }
        if (_old$2388) {
          moonbit_decref(_old$2388);
        }
        dst$41[_tmp$1088] = _tmp$1089;
        _tmp$1091 = i$48 - 1;
        i$48 = _tmp$1091;
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
  int32_t _if_result$2662;
  if (dst$32 == src$33) {
    _if_result$2662 = dst_offset$34 < src_offset$35;
  } else {
    _if_result$2662 = 0;
  }
  if (_if_result$2662) {
    int32_t i$36 = 0;
    while (1) {
      if (i$36 < len$37) {
        int32_t _tmp$1075 = dst_offset$34 + i$36;
        int32_t _tmp$1077 = src_offset$35 + i$36;
        moonbit_string_t _tmp$2391;
        moonbit_string_t _tmp$1076;
        moonbit_string_t _old$2390;
        int32_t _tmp$1078;
        if (_tmp$1077 < 0 || _tmp$1077 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2391 = (moonbit_string_t)src$33[_tmp$1077];
        _tmp$1076 = _tmp$2391;
        if (_tmp$1075 < 0 || _tmp$1075 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2390 = (moonbit_string_t)dst$32[_tmp$1075];
        moonbit_incref(_tmp$1076);
        moonbit_decref(_old$2390);
        dst$32[_tmp$1075] = _tmp$1076;
        _tmp$1078 = i$36 + 1;
        i$36 = _tmp$1078;
        continue;
      } else {
        moonbit_decref(src$33);
        moonbit_decref(dst$32);
      }
      break;
    }
  } else {
    int32_t _tmp$1083 = len$37 - 1;
    int32_t i$39 = _tmp$1083;
    while (1) {
      if (i$39 >= 0) {
        int32_t _tmp$1079 = dst_offset$34 + i$39;
        int32_t _tmp$1081 = src_offset$35 + i$39;
        moonbit_string_t _tmp$2393;
        moonbit_string_t _tmp$1080;
        moonbit_string_t _old$2392;
        int32_t _tmp$1082;
        if (_tmp$1081 < 0 || _tmp$1081 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2393 = (moonbit_string_t)src$33[_tmp$1081];
        _tmp$1080 = _tmp$2393;
        if (_tmp$1079 < 0 || _tmp$1079 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2392 = (moonbit_string_t)dst$32[_tmp$1079];
        moonbit_incref(_tmp$1080);
        moonbit_decref(_old$2392);
        dst$32[_tmp$1079] = _tmp$1080;
        _tmp$1082 = i$39 - 1;
        i$39 = _tmp$1082;
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
  int32_t _if_result$2665;
  if (dst$23 == src$24) {
    _if_result$2665 = dst_offset$25 < src_offset$26;
  } else {
    _if_result$2665 = 0;
  }
  if (_if_result$2665) {
    int32_t i$27 = 0;
    while (1) {
      if (i$27 < len$28) {
        int32_t _tmp$1066 = dst_offset$25 + i$27;
        int32_t _tmp$1068 = src_offset$26 + i$27;
        int32_t _tmp$1067;
        int32_t _tmp$1069;
        if (_tmp$1068 < 0 || _tmp$1068 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1067 = (int32_t)src$24[_tmp$1068];
        if (_tmp$1066 < 0 || _tmp$1066 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1066] = _tmp$1067;
        _tmp$1069 = i$27 + 1;
        i$27 = _tmp$1069;
        continue;
      } else {
        moonbit_decref(src$24);
        moonbit_decref(dst$23);
      }
      break;
    }
  } else {
    int32_t _tmp$1074 = len$28 - 1;
    int32_t i$30 = _tmp$1074;
    while (1) {
      if (i$30 >= 0) {
        int32_t _tmp$1070 = dst_offset$25 + i$30;
        int32_t _tmp$1072 = src_offset$26 + i$30;
        int32_t _tmp$1071;
        int32_t _tmp$1073;
        if (_tmp$1072 < 0 || _tmp$1072 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1071 = (int32_t)src$24[_tmp$1072];
        if (_tmp$1070 < 0 || _tmp$1070 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1070] = _tmp$1071;
        _tmp$1073 = i$30 - 1;
        i$30 = _tmp$1073;
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
  moonbit_string_t _tmp$1065 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$1063 =
    moonbit_add_string(
      _tmp$1065, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1064 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$1062 = moonbit_add_string(_tmp$1063, _tmp$1064);
  moonbit_string_t _tmp$1061 =
    moonbit_add_string(
      _tmp$1062, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1061);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$1060 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$1058 =
    moonbit_add_string(
      _tmp$1060, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1059 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$1057 = moonbit_add_string(_tmp$1058, _tmp$1059);
  moonbit_string_t _tmp$1056 =
    moonbit_add_string(
      _tmp$1057, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1056);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
) {
  moonbit_string_t _tmp$1055 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$17);
  moonbit_string_t _tmp$1053 =
    moonbit_add_string(
      _tmp$1055, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1054 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$18);
  moonbit_string_t _tmp$1052 = moonbit_add_string(_tmp$1053, _tmp$1054);
  moonbit_string_t _tmp$1051 =
    moonbit_add_string(
      _tmp$1052, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1051);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
) {
  moonbit_string_t _tmp$1050 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$15);
  moonbit_string_t _tmp$1048 =
    moonbit_add_string(
      _tmp$1050, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1049 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$16);
  moonbit_string_t _tmp$1047 = moonbit_add_string(_tmp$1048, _tmp$1049);
  moonbit_string_t _tmp$1046 =
    moonbit_add_string(
      _tmp$1047, (moonbit_string_t)moonbit_string_literal_32.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1046);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$11,
  struct $$moonbitlang$core$builtin$Logger _x_5272$14
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$12 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$11;
  moonbit_string_t _field$2394 = _Failure$12->$0;
  int32_t _cnt$2516 = Moonbit_object_header(_Failure$12)->rc;
  moonbit_string_t _$2a$err_payload_5273$13;
  struct $$moonbitlang$core$builtin$Logger _bind$1045;
  if (_cnt$2516 > 1) {
    int32_t _new_cnt$2517;
    moonbit_incref(_field$2394);
    _new_cnt$2517 = _cnt$2516 - 1;
    Moonbit_object_header(_Failure$12)->rc = _new_cnt$2517;
  } else if (_cnt$2516 == 1) {
    moonbit_free(_Failure$12);
  }
  _$2a$err_payload_5273$13 = _field$2394;
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
  _bind$1045 = _x_5272$14;
  _bind$1045.$0->$method_0(
    _bind$1045.$1, (moonbit_string_t)moonbit_string_literal_34.data
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

moonbit_string_t $Error$to_string(void* _e$1012) {
  switch (Moonbit_object_tag(_e$1012)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1012
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1012
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1012);
      return (moonbit_string_t)moonbit_string_literal_37.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
               _e$1012
             );
      break;
    }
    
    case 3: {
      moonbit_decref(_e$1012);
      return (moonbit_string_t)moonbit_string_literal_38.data;
      break;
    }
    default: {
      moonbit_decref(_e$1012);
      return (moonbit_string_t)moonbit_string_literal_39.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1030,
  int32_t _param$1029
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1028 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1030;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1028, _param$1029
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1027,
  struct $StringView _param$1026
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1025 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1027;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1025, _param$1026
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1024,
  moonbit_string_t _param$1021,
  int32_t _param$1022,
  int32_t _param$1023
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1020 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1024;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1020, _param$1021, _param$1022, _param$1023
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1019,
  moonbit_string_t _param$1018
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1017 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1019;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1017, _param$1018
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$838;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1036;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1035;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$837;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1038;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1037;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$836;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1044;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1043;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1042;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1041;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$835;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1040;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1039;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$120 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$134 = (int64_t)0;
  _bind$838
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1036 = _bind$838;
  _tmp$1035
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1036
  };
  $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1035
  );
  _bind$837
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1038 = _bind$837;
  _tmp$1037
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1038
  };
  $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1037
  );
  _bind$836
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1044 = _bind$836;
  _tmp$1043
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1044
  };
  _tmp$1042 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1043);
  _tuple$1041
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1041)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1041->$0 = (moonbit_string_t)moonbit_string_literal_40.data;
  _tuple$1041->$1 = _tmp$1042;
  _bind$835
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      1
    );
  _bind$835[0] = _tuple$1041;
  _tmp$1040 = _bind$835;
  _tmp$1039
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 1, _tmp$1040
  };
  $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1039
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1034;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1006;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1007;
  int32_t _len$1008;
  int32_t _i$1009;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1034
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1006
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1006)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1006->$0 = _tmp$1034;
  async_tests$1006->$1 = 0;
  _arr$1007
  = $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1007);
  _len$1008 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1007);
  _i$1009 = 0;
  while (1) {
    if (_i$1009 < _len$1008) {
      struct $$3c$String$2a$Int$3e$* arg$1010;
      moonbit_string_t _field$2396;
      moonbit_string_t _tmp$1031;
      int32_t _field$2395;
      int32_t _cnt$2518;
      int32_t _tmp$1032;
      int32_t _tmp$1033;
      moonbit_incref(_arr$1007);
      arg$1010
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1007, _i$1009
      );
      _field$2396 = arg$1010->$0;
      _tmp$1031 = _field$2396;
      _field$2395 = arg$1010->$1;
      _cnt$2518 = Moonbit_object_header(arg$1010)->rc;
      if (_cnt$2518 > 1) {
        int32_t _new_cnt$2519;
        moonbit_incref(_tmp$1031);
        _new_cnt$2519 = _cnt$2518 - 1;
        Moonbit_object_header(arg$1010)->rc = _new_cnt$2519;
      } else if (_cnt$2518 == 1) {
        moonbit_free(arg$1010);
      }
      _tmp$1032 = _field$2395;
      moonbit_incref(async_tests$1006);
      $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_do_execute(
        async_tests$1006, _tmp$1031, _tmp$1032
      );
      _tmp$1033 = _i$1009 + 1;
      _i$1009 = _tmp$1033;
      continue;
    } else {
      moonbit_decref(_arr$1007);
    }
    break;
  }
  $azimuth$telemetry$tests$enhanced_suite_blackbox_test$moonbit_test_driver_internal_run_async_tests(
    async_tests$1006
  );
  return 0;
}