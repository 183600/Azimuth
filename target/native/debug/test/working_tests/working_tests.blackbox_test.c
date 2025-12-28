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

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $Ref$3c$Int$3e$;

struct $StringView;

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

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

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

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

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $$3c$StringView$2a$StringView$3e$;

struct $Error$azimuth$telemetry$working_tests_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $Moonbit_Test_Driver_Internal__F$F2;

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

struct $$moonbitlang$core$builtin$Hasher;

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

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

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
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

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
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

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
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

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156 {
  void* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$3c$StringView$2a$StringView$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  int32_t $1_1;
  int32_t $1_2;
  moonbit_string_t $0_0;
  moonbit_string_t $1_0;
  
};

struct $Error$azimuth$telemetry$working_tests_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
  moonbit_string_t $0;
  
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

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
};

struct $$moonbitlang$core$builtin$Hasher {
  uint32_t $0;
  
};

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1006
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2089,
  moonbit_string_t s$984,
  int32_t sep$985
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$971
);

moonbit_string_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$1998,
  moonbit_bytes_t bytes$972
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$1991,
  moonbit_string_t s$966
);

#define $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$929,
  moonbit_string_t filename$890,
  int32_t index$891
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1984
);

struct moonbit_result_0 $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1980
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1978
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1962,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$930,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$931
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1973,
  int32_t _cont_param$950
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1970,
  void* _cont_param$951
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1964,
  void* _state$933
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1959,
  void* err$913
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1945,
  moonbit_string_t attr$906
);

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1929,
  moonbit_string_t test_name$893,
  moonbit_string_t file_name$894,
  moonbit_string_t message$895,
  int32_t skipped$896
);

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$888
);

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$886,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$887,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$884
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$847,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$860,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$873,
  moonbit_string_t file_filter$844,
  int32_t index_filter$845
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
  struct $$3c$String$3e$$3d$$3e$Int* _env$1885,
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1538
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
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1467,
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

moonbit_string_t $Error$to_string(void* _e$1013);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1031,
  int32_t _param$1030
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1028,
  struct $StringView _param$1027
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1025,
  moonbit_string_t _param$1022,
  int32_t _param$1023,
  int32_t _param$1024
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1020,
  moonbit_string_t _param$1019
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

struct { int32_t rc; uint32_t meta; uint16_t const data[112]; 
} const moonbit_string_literal_37 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 111), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 119, 111, 114, 107, 105, 110, 103, 95, 116, 101, 
    115, 116, 115, 95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 
    115, 116, 46, 77, 111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 
    68, 114, 105, 118, 101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 
    74, 115, 69, 114, 114, 111, 114, 46, 77, 111, 111, 110, 66, 105, 
    116, 84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 73, 110, 116, 
    101, 114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_38 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    119, 111, 114, 107, 105, 110, 103, 95, 116, 101, 115, 116, 115, 46, 
    109, 98, 116, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[60]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 59), 
    123, 34, 112, 97, 99, 107, 97, 103, 101, 34, 58, 32, 34, 97, 122, 
    105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 
    121, 47, 119, 111, 114, 107, 105, 110, 103, 95, 116, 101, 115, 116, 
    115, 34, 44, 32, 34, 102, 105, 108, 101, 110, 97, 109, 101, 34, 58, 
    32, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_41 =
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
} const $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5
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

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_async_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_no_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_with_args_tests;

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1006
) {
  moonbit_decref(_tests$1006);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$965 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$971 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$978 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$971;
  int32_t moonbit_test_driver_internal_split_mbt_string$983 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$2114 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$990 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$991;
  moonbit_string_t _tmp$2113;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$992;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$993;
  int32_t _len$994;
  int32_t _i$995;
  Moonbit_object_header(file_and_index$990)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$990->$0 = _tmp$2114;
  file_and_index$990->$1 = 0;
  cli_args$991
  = $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$978
  );
  _tmp$2113 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$991, 1);
  test_args$992
  = $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$983, _tmp$2113, 47
  );
  _arr$993 = test_args$992;
  moonbit_incref(_arr$993);
  _len$994 = $$moonbitlang$core$builtin$Array$$length$1(_arr$993);
  _i$995 = 0;
  while (1) {
    if (_i$995 < _len$994) {
      moonbit_string_t arg$996;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$997;
      moonbit_string_t file$998;
      moonbit_string_t range$999;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1000;
      moonbit_string_t _tmp$2111;
      int32_t start$1001;
      moonbit_string_t _tmp$2110;
      int32_t end$1002;
      int32_t i$1003;
      int32_t _tmp$2112;
      moonbit_incref(_arr$993);
      arg$996
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$993, _i$995
      );
      file_and_range$997
      = $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$983, arg$996, 58
      );
      moonbit_incref(file_and_range$997);
      file$998
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$997, 0
      );
      range$999
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$997, 1
      );
      start_and_end$1000
      = $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$983, range$999, 45
      );
      moonbit_incref(start_and_end$1000);
      _tmp$2111
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1000, 0
      );
      start$1001
      = $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$965, _tmp$2111
      );
      _tmp$2110
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1000, 1
      );
      end$1002
      = $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$965, _tmp$2110
      );
      i$1003 = start$1001;
      while (1) {
        if (i$1003 < end$1002) {
          struct $$3c$String$2a$Int$3e$* _tuple$2108;
          int32_t _tmp$2109;
          moonbit_incref(file$998);
          _tuple$2108
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$2108)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$2108->$0 = file$998;
          _tuple$2108->$1 = i$1003;
          moonbit_incref(file_and_index$990);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$990, _tuple$2108
          );
          _tmp$2109 = i$1003 + 1;
          i$1003 = _tmp$2109;
          continue;
        } else {
          moonbit_decref(file$998);
        }
        break;
      }
      _tmp$2112 = _i$995 + 1;
      _i$995 = _tmp$2112;
      continue;
    } else {
      moonbit_decref(_arr$993);
    }
    break;
  }
  return file_and_index$990;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2089,
  moonbit_string_t s$984,
  int32_t sep$985
) {
  moonbit_string_t* _tmp$2107 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$986 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$987;
  struct $Ref$3c$Int$3e$* start$988;
  int32_t val$2102;
  int32_t _tmp$2103;
  Moonbit_object_header(res$986)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$986->$0 = _tmp$2107;
  res$986->$1 = 0;
  i$987
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$987)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$987->$0 = 0;
  start$988
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$988)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$988->$0 = 0;
  while (1) {
    int32_t val$2090 = i$987->$0;
    int32_t _tmp$2091 = Moonbit_array_length(s$984);
    if (val$2090 < _tmp$2091) {
      int32_t val$2094 = i$987->$0;
      int32_t _tmp$2093;
      int32_t _tmp$2092;
      int32_t val$2101;
      int32_t _tmp$2100;
      if (val$2094 < 0 || val$2094 >= Moonbit_array_length(s$984)) {
        moonbit_panic();
      }
      _tmp$2093 = s$984[val$2094];
      _tmp$2092 = _tmp$2093;
      if (_tmp$2092 == sep$985) {
        int32_t val$2096 = start$988->$0;
        int32_t val$2097 = i$987->$0;
        moonbit_string_t _tmp$2095;
        int32_t val$2099;
        int32_t _tmp$2098;
        moonbit_incref(s$984);
        _tmp$2095 = $String$$unsafe_substring(s$984, val$2096, val$2097);
        moonbit_incref(res$986);
        $$moonbitlang$core$builtin$Array$$push$0(res$986, _tmp$2095);
        val$2099 = i$987->$0;
        _tmp$2098 = val$2099 + 1;
        start$988->$0 = _tmp$2098;
      }
      val$2101 = i$987->$0;
      _tmp$2100 = val$2101 + 1;
      i$987->$0 = _tmp$2100;
      continue;
    } else {
      moonbit_decref(i$987);
    }
    break;
  }
  val$2102 = start$988->$0;
  _tmp$2103 = Moonbit_array_length(s$984);
  if (val$2102 < _tmp$2103) {
    int32_t _field$2115 = start$988->$0;
    int32_t val$2105;
    int32_t _tmp$2106;
    moonbit_string_t _tmp$2104;
    moonbit_decref(start$988);
    val$2105 = _field$2115;
    _tmp$2106 = Moonbit_array_length(s$984);
    _tmp$2104 = $String$$unsafe_substring(s$984, val$2105, _tmp$2106);
    moonbit_incref(res$986);
    $$moonbitlang$core$builtin$Array$$push$0(res$986, _tmp$2104);
  } else {
    moonbit_decref(start$988);
    moonbit_decref(s$984);
  }
  return res$986;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$971
) {
  moonbit_bytes_t* tmp$979 =
    $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$2088 = Moonbit_array_length(tmp$979);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$980 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$2088);
  int32_t i$981 = 0;
  while (1) {
    int32_t _tmp$2084 = Moonbit_array_length(tmp$979);
    if (i$981 < _tmp$2084) {
      moonbit_bytes_t _tmp$2116;
      moonbit_bytes_t _tmp$2086;
      moonbit_string_t _tmp$2085;
      int32_t _tmp$2087;
      if (i$981 < 0 || i$981 >= Moonbit_array_length(tmp$979)) {
        moonbit_panic();
      }
      _tmp$2116 = (moonbit_bytes_t)tmp$979[i$981];
      _tmp$2086 = _tmp$2116;
      moonbit_incref(_tmp$2086);
      _tmp$2085
      = $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$971, _tmp$2086
      );
      moonbit_incref(res$980);
      $$moonbitlang$core$builtin$Array$$push$0(res$980, _tmp$2085);
      _tmp$2087 = i$981 + 1;
      i$981 = _tmp$2087;
      continue;
    } else {
      moonbit_decref(tmp$979);
    }
    break;
  }
  return res$980;
}

moonbit_string_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$1998,
  moonbit_bytes_t bytes$972
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$973 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$974 = Moonbit_array_length(bytes$972);
  struct $Ref$3c$Int$3e$* i$975 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$975)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$975->$0 = 0;
  while (1) {
    int32_t val$1999 = i$975->$0;
    if (val$1999 < len$974) {
      int32_t val$2083 = i$975->$0;
      int32_t _tmp$2082;
      int32_t _tmp$2081;
      struct $Ref$3c$Int$3e$* c$976;
      int32_t val$2000;
      if (val$2083 < 0 || val$2083 >= Moonbit_array_length(bytes$972)) {
        moonbit_panic();
      }
      _tmp$2082 = bytes$972[val$2083];
      _tmp$2081 = (int32_t)_tmp$2082;
      c$976
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$976)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$976->$0 = _tmp$2081;
      val$2000 = c$976->$0;
      if (val$2000 < 128) {
        int32_t _field$2117 = c$976->$0;
        int32_t val$2002;
        int32_t _tmp$2001;
        int32_t val$2004;
        int32_t _tmp$2003;
        moonbit_decref(c$976);
        val$2002 = _field$2117;
        _tmp$2001 = val$2002;
        moonbit_incref(res$973);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$973, _tmp$2001
        );
        val$2004 = i$975->$0;
        _tmp$2003 = val$2004 + 1;
        i$975->$0 = _tmp$2003;
      } else {
        int32_t val$2005 = c$976->$0;
        if (val$2005 < 224) {
          int32_t val$2007 = i$975->$0;
          int32_t _tmp$2006 = val$2007 + 1;
          int32_t val$2016;
          int32_t _tmp$2015;
          int32_t _tmp$2009;
          int32_t val$2014;
          int32_t _tmp$2013;
          int32_t _tmp$2012;
          int32_t _tmp$2011;
          int32_t _tmp$2010;
          int32_t _tmp$2008;
          int32_t _field$2118;
          int32_t val$2018;
          int32_t _tmp$2017;
          int32_t val$2020;
          int32_t _tmp$2019;
          if (_tmp$2006 >= len$974) {
            moonbit_decref(c$976);
            moonbit_decref(i$975);
            moonbit_decref(bytes$972);
            break;
          }
          val$2016 = c$976->$0;
          _tmp$2015 = val$2016 & 31;
          _tmp$2009 = _tmp$2015 << 6;
          val$2014 = i$975->$0;
          _tmp$2013 = val$2014 + 1;
          if (_tmp$2013 < 0 || _tmp$2013 >= Moonbit_array_length(bytes$972)) {
            moonbit_panic();
          }
          _tmp$2012 = bytes$972[_tmp$2013];
          _tmp$2011 = (int32_t)_tmp$2012;
          _tmp$2010 = _tmp$2011 & 63;
          _tmp$2008 = _tmp$2009 | _tmp$2010;
          c$976->$0 = _tmp$2008;
          _field$2118 = c$976->$0;
          moonbit_decref(c$976);
          val$2018 = _field$2118;
          _tmp$2017 = val$2018;
          moonbit_incref(res$973);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$973, _tmp$2017
          );
          val$2020 = i$975->$0;
          _tmp$2019 = val$2020 + 2;
          i$975->$0 = _tmp$2019;
        } else {
          int32_t val$2021 = c$976->$0;
          if (val$2021 < 240) {
            int32_t val$2023 = i$975->$0;
            int32_t _tmp$2022 = val$2023 + 2;
            int32_t val$2039;
            int32_t _tmp$2038;
            int32_t _tmp$2031;
            int32_t val$2037;
            int32_t _tmp$2036;
            int32_t _tmp$2035;
            int32_t _tmp$2034;
            int32_t _tmp$2033;
            int32_t _tmp$2032;
            int32_t _tmp$2025;
            int32_t val$2030;
            int32_t _tmp$2029;
            int32_t _tmp$2028;
            int32_t _tmp$2027;
            int32_t _tmp$2026;
            int32_t _tmp$2024;
            int32_t _field$2119;
            int32_t val$2041;
            int32_t _tmp$2040;
            int32_t val$2043;
            int32_t _tmp$2042;
            if (_tmp$2022 >= len$974) {
              moonbit_decref(c$976);
              moonbit_decref(i$975);
              moonbit_decref(bytes$972);
              break;
            }
            val$2039 = c$976->$0;
            _tmp$2038 = val$2039 & 15;
            _tmp$2031 = _tmp$2038 << 12;
            val$2037 = i$975->$0;
            _tmp$2036 = val$2037 + 1;
            if (
              _tmp$2036 < 0 || _tmp$2036 >= Moonbit_array_length(bytes$972)
            ) {
              moonbit_panic();
            }
            _tmp$2035 = bytes$972[_tmp$2036];
            _tmp$2034 = (int32_t)_tmp$2035;
            _tmp$2033 = _tmp$2034 & 63;
            _tmp$2032 = _tmp$2033 << 6;
            _tmp$2025 = _tmp$2031 | _tmp$2032;
            val$2030 = i$975->$0;
            _tmp$2029 = val$2030 + 2;
            if (
              _tmp$2029 < 0 || _tmp$2029 >= Moonbit_array_length(bytes$972)
            ) {
              moonbit_panic();
            }
            _tmp$2028 = bytes$972[_tmp$2029];
            _tmp$2027 = (int32_t)_tmp$2028;
            _tmp$2026 = _tmp$2027 & 63;
            _tmp$2024 = _tmp$2025 | _tmp$2026;
            c$976->$0 = _tmp$2024;
            _field$2119 = c$976->$0;
            moonbit_decref(c$976);
            val$2041 = _field$2119;
            _tmp$2040 = val$2041;
            moonbit_incref(res$973);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$973, _tmp$2040
            );
            val$2043 = i$975->$0;
            _tmp$2042 = val$2043 + 3;
            i$975->$0 = _tmp$2042;
          } else {
            int32_t val$2045 = i$975->$0;
            int32_t _tmp$2044 = val$2045 + 3;
            int32_t val$2068;
            int32_t _tmp$2067;
            int32_t _tmp$2060;
            int32_t val$2066;
            int32_t _tmp$2065;
            int32_t _tmp$2064;
            int32_t _tmp$2063;
            int32_t _tmp$2062;
            int32_t _tmp$2061;
            int32_t _tmp$2053;
            int32_t val$2059;
            int32_t _tmp$2058;
            int32_t _tmp$2057;
            int32_t _tmp$2056;
            int32_t _tmp$2055;
            int32_t _tmp$2054;
            int32_t _tmp$2047;
            int32_t val$2052;
            int32_t _tmp$2051;
            int32_t _tmp$2050;
            int32_t _tmp$2049;
            int32_t _tmp$2048;
            int32_t _tmp$2046;
            int32_t val$2070;
            int32_t _tmp$2069;
            int32_t val$2074;
            int32_t _tmp$2073;
            int32_t _tmp$2072;
            int32_t _tmp$2071;
            int32_t _field$2120;
            int32_t val$2078;
            int32_t _tmp$2077;
            int32_t _tmp$2076;
            int32_t _tmp$2075;
            int32_t val$2080;
            int32_t _tmp$2079;
            if (_tmp$2044 >= len$974) {
              moonbit_decref(c$976);
              moonbit_decref(i$975);
              moonbit_decref(bytes$972);
              break;
            }
            val$2068 = c$976->$0;
            _tmp$2067 = val$2068 & 7;
            _tmp$2060 = _tmp$2067 << 18;
            val$2066 = i$975->$0;
            _tmp$2065 = val$2066 + 1;
            if (
              _tmp$2065 < 0 || _tmp$2065 >= Moonbit_array_length(bytes$972)
            ) {
              moonbit_panic();
            }
            _tmp$2064 = bytes$972[_tmp$2065];
            _tmp$2063 = (int32_t)_tmp$2064;
            _tmp$2062 = _tmp$2063 & 63;
            _tmp$2061 = _tmp$2062 << 12;
            _tmp$2053 = _tmp$2060 | _tmp$2061;
            val$2059 = i$975->$0;
            _tmp$2058 = val$2059 + 2;
            if (
              _tmp$2058 < 0 || _tmp$2058 >= Moonbit_array_length(bytes$972)
            ) {
              moonbit_panic();
            }
            _tmp$2057 = bytes$972[_tmp$2058];
            _tmp$2056 = (int32_t)_tmp$2057;
            _tmp$2055 = _tmp$2056 & 63;
            _tmp$2054 = _tmp$2055 << 6;
            _tmp$2047 = _tmp$2053 | _tmp$2054;
            val$2052 = i$975->$0;
            _tmp$2051 = val$2052 + 3;
            if (
              _tmp$2051 < 0 || _tmp$2051 >= Moonbit_array_length(bytes$972)
            ) {
              moonbit_panic();
            }
            _tmp$2050 = bytes$972[_tmp$2051];
            _tmp$2049 = (int32_t)_tmp$2050;
            _tmp$2048 = _tmp$2049 & 63;
            _tmp$2046 = _tmp$2047 | _tmp$2048;
            c$976->$0 = _tmp$2046;
            val$2070 = c$976->$0;
            _tmp$2069 = val$2070 - 65536;
            c$976->$0 = _tmp$2069;
            val$2074 = c$976->$0;
            _tmp$2073 = val$2074 >> 10;
            _tmp$2072 = _tmp$2073 + 55296;
            _tmp$2071 = _tmp$2072;
            moonbit_incref(res$973);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$973, _tmp$2071
            );
            _field$2120 = c$976->$0;
            moonbit_decref(c$976);
            val$2078 = _field$2120;
            _tmp$2077 = val$2078 & 1023;
            _tmp$2076 = _tmp$2077 + 56320;
            _tmp$2075 = _tmp$2076;
            moonbit_incref(res$973);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$973, _tmp$2075
            );
            val$2080 = i$975->$0;
            _tmp$2079 = val$2080 + 4;
            i$975->$0 = _tmp$2079;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$975);
      moonbit_decref(bytes$972);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$973);
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$1991,
  moonbit_string_t s$966
) {
  struct $Ref$3c$Int$3e$* res$967 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$968;
  int32_t i$969;
  int32_t _field$2121;
  Moonbit_object_header(res$967)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$967->$0 = 0;
  len$968 = Moonbit_array_length(s$966);
  i$969 = 0;
  while (1) {
    if (i$969 < len$968) {
      int32_t val$1996 = res$967->$0;
      int32_t _tmp$1993 = val$1996 * 10;
      int32_t _tmp$1995;
      int32_t _tmp$1994;
      int32_t _tmp$1992;
      int32_t _tmp$1997;
      if (i$969 < 0 || i$969 >= Moonbit_array_length(s$966)) {
        moonbit_panic();
      }
      _tmp$1995 = s$966[i$969];
      _tmp$1994 = _tmp$1995 - 48;
      _tmp$1992 = _tmp$1993 + _tmp$1994;
      res$967->$0 = _tmp$1992;
      _tmp$1997 = i$969 + 1;
      i$969 = _tmp$1997;
      continue;
    } else {
      moonbit_decref(s$966);
    }
    break;
  }
  _field$2121 = res$967->$0;
  moonbit_decref(res$967);
  return _field$2121;
}

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$929,
  moonbit_string_t filename$890,
  int32_t index$891
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$889;
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$2531;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$901;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2131;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$1990;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2130;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$902;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2129;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$1989;
  moonbit_string_t _field$2128;
  moonbit_string_t file_name$903;
  moonbit_string_t name$904;
  int32_t _tmp$1986;
  moonbit_string_t name$905;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$1943;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1944;
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$2533;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$912;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$928;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$953;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$955;
  void* _field$2125;
  int32_t _cnt$2402;
  void* _bind$956;
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$2537;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1983;
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$2538;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1976;
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$2539;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1977;
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$2540;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1961;
  moonbit_incref(
    $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_async_tests
  );
  filtered_test$889
  = $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_async_tests,
      filename$890,
      index$891
  );
  _closure$2531
  = (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$2531)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$2531->code
  = &$$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$2531->$0 = index$891;
  handle_result$892
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$2531;
  if (filtered_test$889 == 0) {
    moonbit_decref(async_tests$929);
    if (filtered_test$889) {
      moonbit_decref(filtered_test$889);
    }
    $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$892,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$963 =
      filtered_test$889;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$964 = _Some$963;
    item$901 = _item$964;
    goto $join$900;
  }
  goto $joinlet$2532;
  $join$900:;
  _field$2131 = item$901->$1;
  meta$1990 = _field$2131;
  _field$2130 = meta$1990->$2;
  attrs$902 = _field$2130;
  _field$2129 = item$901->$1;
  meta$1989 = _field$2129;
  _field$2128 = meta$1989->$0;
  file_name$903 = _field$2128;
  moonbit_incref(attrs$902);
  moonbit_incref(file_name$903);
  moonbit_incref(attrs$902);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$902)) {
    name$904 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$902);
    name$904 = $$moonbitlang$core$builtin$Array$$at$0(attrs$902, 0);
  }
  _tmp$1986 = Moonbit_array_length(name$904);
  if (_tmp$1986 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2127;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$1988;
    int32_t _field$2126;
    int32_t index$1987;
    moonbit_decref(name$904);
    _field$2127 = item$901->$1;
    meta$1988 = _field$2127;
    _field$2126 = meta$1988->$1;
    index$1987 = _field$2126;
    name$905 = $Int$$to_string$inner(index$1987, 10);
  } else {
    name$905 = name$904;
  }
  moonbit_incref(attrs$902);
  _tmp$1943 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$902);
  _tmp$1944
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$1943, _tmp$1944)) {
    moonbit_string_t _tmp$1958;
    moonbit_string_t _tmp$1957;
    moonbit_string_t _tmp$1954;
    moonbit_string_t _tmp$1956;
    moonbit_string_t _tmp$1955;
    moonbit_string_t _tmp$1953;
    moonbit_decref(async_tests$929);
    moonbit_decref(item$901);
    moonbit_incref(file_name$903);
    _tmp$1958
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$903
    );
    _tmp$1957
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$1958
    );
    _tmp$1954
    = moonbit_add_string(
      _tmp$1957, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$1956 = $$moonbitlang$core$builtin$Array$$at$0(attrs$902, 0);
    _tmp$1955 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$1956);
    _tmp$1953 = moonbit_add_string(_tmp$1954, _tmp$1955);
    $moonbitlang$core$builtin$println$0(_tmp$1953);
    $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$892,
        name$905,
        file_name$903,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$902);
  }
  moonbit_incref(name$905);
  moonbit_incref(file_name$903);
  moonbit_incref(handle_result$892);
  _closure$2533
  = (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$2533)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2533->code
  = &$$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$2533->$0 = name$905;
  _closure$2533->$1 = file_name$903;
  _closure$2533->$2 = handle_result$892;
  on_err$912 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2533;
  _field$2125 = item$901->$0;
  _cnt$2402 = Moonbit_object_header(item$901)->rc;
  if (_cnt$2402 > 1) {
    int32_t _new_cnt$2404;
    moonbit_incref(_field$2125);
    _new_cnt$2404 = _cnt$2402 - 1;
    Moonbit_object_header(item$901)->rc = _new_cnt$2404;
  } else if (_cnt$2402 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2403 = item$901->$1;
    moonbit_decref(_field$2403);
    moonbit_free(item$901);
  }
  _bind$956 = _field$2125;
  switch (Moonbit_object_tag(_bind$956)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$957;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2122;
      int32_t _cnt$2405;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$958;
      moonbit_decref(async_tests$929);
      _F0$957 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$956;
      _field$2122 = _F0$957->$0;
      _cnt$2405 = Moonbit_object_header(_F0$957)->rc;
      if (_cnt$2405 > 1) {
        int32_t _new_cnt$2406;
        moonbit_incref(_field$2122);
        _new_cnt$2406 = _cnt$2405 - 1;
        Moonbit_object_header(_F0$957)->rc = _new_cnt$2406;
      } else if (_cnt$2405 == 1) {
        moonbit_free(_F0$957);
      }
      _f$958 = _field$2122;
      f$955 = _f$958;
      goto $join$954;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$959;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2123;
      int32_t _cnt$2407;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$960;
      moonbit_decref(async_tests$929);
      _F1$959 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$956;
      _field$2123 = _F1$959->$0;
      _cnt$2407 = Moonbit_object_header(_F1$959)->rc;
      if (_cnt$2407 > 1) {
        int32_t _new_cnt$2408;
        moonbit_incref(_field$2123);
        _new_cnt$2408 = _cnt$2407 - 1;
        Moonbit_object_header(_F1$959)->rc = _new_cnt$2408;
      } else if (_cnt$2407 == 1) {
        moonbit_free(_F1$959);
      }
      _f$960 = _field$2123;
      f$953 = _f$960;
      goto $join$952;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$961 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$956;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2124 =
        _F2$961->$0;
      int32_t _cnt$2409 = Moonbit_object_header(_F2$961)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$962;
      if (_cnt$2409 > 1) {
        int32_t _new_cnt$2410;
        moonbit_incref(_field$2124);
        _new_cnt$2410 = _cnt$2409 - 1;
        Moonbit_object_header(_F2$961)->rc = _new_cnt$2410;
      } else if (_cnt$2409 == 1) {
        moonbit_free(_F2$961);
      }
      _f$962 = _field$2124;
      f$928 = _f$962;
      goto $join$927;
      break;
    }
  }
  goto $joinlet$2536;
  $join$954:;
  _closure$2537
  = (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$2537)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2537->code
  = &$$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$2537->$0 = name$905;
  _closure$2537->$1 = file_name$903;
  _closure$2537->$2 = handle_result$892;
  _tmp$1983 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2537;
  $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_catch_error(
    f$955, _tmp$1983, on_err$912
  );
  $joinlet$2536:;
  goto $joinlet$2535;
  $join$952:;
  moonbit_incref(name$905);
  _closure$2538
  = (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$2538)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$2538->code
  = &$$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$2538->$0 = f$953;
  _closure$2538->$1 = name$905;
  _tmp$1976
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$2538;
  _closure$2539
  = (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$2539)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2539->code
  = &$$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$2539->$0 = name$905;
  _closure$2539->$1 = file_name$903;
  _closure$2539->$2 = handle_result$892;
  _tmp$1977 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2539;
  $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_catch_error(
    _tmp$1976, _tmp$1977, on_err$912
  );
  $joinlet$2535:;
  goto $joinlet$2534;
  $join$927:;
  _closure$2540
  = (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$2540)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$2540->code
  = &$$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$2540->$0 = f$928;
  _closure$2540->$1 = on_err$912;
  _closure$2540->$2 = name$905;
  _closure$2540->$3 = file_name$903;
  _closure$2540->$4 = handle_result$892;
  _tmp$1961
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$2540;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$929, _tmp$1961);
  $joinlet$2534:;
  $joinlet$2532:;
  return 0;
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1984
) {
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$1985 =
    (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$1984;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2134 =
    _casted_env$1985->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2134;
  moonbit_string_t _field$2133 = _casted_env$1985->$1;
  moonbit_string_t file_name$903 = _field$2133;
  moonbit_string_t _field$2132 = _casted_env$1985->$0;
  int32_t _cnt$2411 = Moonbit_object_header(_casted_env$1985)->rc;
  moonbit_string_t name$905;
  if (_cnt$2411 > 1) {
    int32_t _new_cnt$2412;
    moonbit_incref(handle_result$892);
    moonbit_incref(file_name$903);
    moonbit_incref(_field$2132);
    _new_cnt$2412 = _cnt$2411 - 1;
    Moonbit_object_header(_casted_env$1985)->rc = _new_cnt$2412;
  } else if (_cnt$2411 == 1) {
    moonbit_free(_casted_env$1985);
  }
  name$905 = _field$2132;
  $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$892,
      name$905,
      file_name$903,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1980
) {
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$1981 =
    (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$1980;
  moonbit_string_t _field$2136 = _casted_env$1981->$1;
  moonbit_string_t name$905 = _field$2136;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2135 =
    _casted_env$1981->$0;
  int32_t _cnt$2413 = Moonbit_object_header(_casted_env$1981)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$953;
  int32_t _tmp$1982;
  if (_cnt$2413 > 1) {
    int32_t _new_cnt$2414;
    moonbit_incref(name$905);
    moonbit_incref(_field$2135);
    _new_cnt$2414 = _cnt$2413 - 1;
    Moonbit_object_header(_casted_env$1981)->rc = _new_cnt$2414;
  } else if (_cnt$2413 == 1) {
    moonbit_free(_casted_env$1981);
  }
  f$953 = _field$2135;
  _tmp$1982
  = $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$905
  );
  return f$953->code(f$953, _tmp$1982);
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1978
) {
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$1979 =
    (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$1978;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2139 =
    _casted_env$1979->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2139;
  moonbit_string_t _field$2138 = _casted_env$1979->$1;
  moonbit_string_t file_name$903 = _field$2138;
  moonbit_string_t _field$2137 = _casted_env$1979->$0;
  int32_t _cnt$2415 = Moonbit_object_header(_casted_env$1979)->rc;
  moonbit_string_t name$905;
  if (_cnt$2415 > 1) {
    int32_t _new_cnt$2416;
    moonbit_incref(handle_result$892);
    moonbit_incref(file_name$903);
    moonbit_incref(_field$2137);
    _new_cnt$2416 = _cnt$2415 - 1;
    Moonbit_object_header(_casted_env$1979)->rc = _new_cnt$2416;
  } else if (_cnt$2415 == 1) {
    moonbit_free(_casted_env$1979);
  }
  name$905 = _field$2137;
  $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$892,
      name$905,
      file_name$903,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1962,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$930,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$931
) {
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$1963 =
    (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$1962;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2144 =
    _casted_env$1963->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2144;
  moonbit_string_t _field$2143 = _casted_env$1963->$3;
  moonbit_string_t file_name$903 = _field$2143;
  moonbit_string_t _field$2142 = _casted_env$1963->$2;
  moonbit_string_t name$905 = _field$2142;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2141 = _casted_env$1963->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$912 = _field$2141;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2140 =
    _casted_env$1963->$0;
  int32_t _cnt$2417 = Moonbit_object_header(_casted_env$1963)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$928;
  int32_t _async_driver$932;
  int32_t _tmp$1967;
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$2541;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$1968;
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$2542;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$1969;
  if (_cnt$2417 > 1) {
    int32_t _new_cnt$2418;
    moonbit_incref(handle_result$892);
    moonbit_incref(file_name$903);
    moonbit_incref(name$905);
    moonbit_incref(on_err$912);
    moonbit_incref(_field$2140);
    _new_cnt$2418 = _cnt$2417 - 1;
    Moonbit_object_header(_casted_env$1963)->rc = _new_cnt$2418;
  } else if (_cnt$2417 == 1) {
    moonbit_free(_casted_env$1963);
  }
  f$928 = _field$2140;
  _async_driver$932 = 0;
  moonbit_incref(name$905);
  _tmp$1967
  = $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$905
  );
  moonbit_incref(_cont$930);
  _closure$2541
  = (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$2541)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$2541->code
  = &$$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$2541->$0 = _async_driver$932;
  _closure$2541->$1 = _cont$930;
  _closure$2541->$2 = name$905;
  _closure$2541->$3 = file_name$903;
  _closure$2541->$4 = handle_result$892;
  _tmp$1968 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$2541;
  _closure$2542
  = (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$2542)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$2542->code
  = &$$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$2542->$0 = _async_driver$932;
  _closure$2542->$1 = _err_cont$931;
  _closure$2542->$2 = _cont$930;
  _closure$2542->$3 = on_err$912;
  _tmp$1969 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2542;
  f$928->code(f$928, _tmp$1967, _tmp$1968, _tmp$1969);
  return 0;
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1973,
  int32_t _cont_param$950
) {
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$1974 =
    (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$1973;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2149 =
    _casted_env$1974->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2149;
  moonbit_string_t _field$2148 = _casted_env$1974->$3;
  moonbit_string_t file_name$903 = _field$2148;
  moonbit_string_t _field$2147 = _casted_env$1974->$2;
  moonbit_string_t name$905 = _field$2147;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2146 = _casted_env$1974->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$930 = _field$2146;
  int32_t _field$2145 = _casted_env$1974->$0;
  int32_t _cnt$2419 = Moonbit_object_header(_casted_env$1974)->rc;
  int32_t _async_driver$932;
  void* State_1$1975;
  if (_cnt$2419 > 1) {
    int32_t _new_cnt$2420;
    moonbit_incref(handle_result$892);
    moonbit_incref(file_name$903);
    moonbit_incref(name$905);
    moonbit_incref(_cont$930);
    _new_cnt$2420 = _cnt$2419 - 1;
    Moonbit_object_header(_casted_env$1974)->rc = _new_cnt$2420;
  } else if (_cnt$2419 == 1) {
    moonbit_free(_casted_env$1974);
  }
  _async_driver$932 = _field$2145;
  State_1$1975
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1
      )
    );
  Moonbit_object_header(State_1$1975)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1*)State_1$1975)->$0
  = _cont_param$950;
  ((struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1*)State_1$1975)->$1
  = handle_result$892;
  ((struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1*)State_1$1975)->$2
  = file_name$903;
  ((struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1*)State_1$1975)->$3
  = name$905;
  ((struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1*)State_1$1975)->$4
  = _cont$930;
  $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$932, State_1$1975
  );
  return 0;
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1970,
  void* _cont_param$951
) {
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$1971 =
    (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$1970;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2153 = _casted_env$1971->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$912 = _field$2153;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2152 = _casted_env$1971->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$930 = _field$2152;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2151 = _casted_env$1971->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$931 = _field$2151;
  int32_t _field$2150 = _casted_env$1971->$0;
  int32_t _cnt$2421 = Moonbit_object_header(_casted_env$1971)->rc;
  int32_t _async_driver$932;
  void* _try$156$1972;
  if (_cnt$2421 > 1) {
    int32_t _new_cnt$2422;
    moonbit_incref(on_err$912);
    moonbit_incref(_cont$930);
    moonbit_incref(_err_cont$931);
    _new_cnt$2422 = _cnt$2421 - 1;
    Moonbit_object_header(_casted_env$1971)->rc = _new_cnt$2422;
  } else if (_cnt$2421 == 1) {
    moonbit_free(_casted_env$1971);
  }
  _async_driver$932 = _field$2150;
  _try$156$1972
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156
      )
    );
  Moonbit_object_header(_try$156$1972)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156*)_try$156$1972)->$0
  = _cont_param$951;
  ((struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156*)_try$156$1972)->$1
  = on_err$912;
  ((struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156*)_try$156$1972)->$2
  = _cont$930;
  ((struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156*)_try$156$1972)->$3
  = _err_cont$931;
  $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$932, _try$156$1972
  );
  return 0;
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1964,
  void* _state$933
) {
  switch (Moonbit_object_tag(_state$933)) {
    case 0: {
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156* _$2a$try$156$934 =
        (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$$2a$try$156*)_state$933;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2157 = _$2a$try$156$934->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$935 = _field$2157;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2156 = _$2a$try$156$934->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$936 = _field$2156;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2155 = _$2a$try$156$934->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$937 = _field$2155;
      void* _field$2154 = _$2a$try$156$934->$0;
      int32_t _cnt$2423 = Moonbit_object_header(_$2a$try$156$934)->rc;
      void* _try_err$938;
      void* err$940;
      void* err$942;
      int32_t _tmp$1966;
      if (_cnt$2423 > 1) {
        int32_t _new_cnt$2424;
        moonbit_incref(_err_cont$935);
        moonbit_incref(_cont$936);
        moonbit_incref(on_err$937);
        moonbit_incref(_field$2154);
        _new_cnt$2424 = _cnt$2423 - 1;
        Moonbit_object_header(_$2a$try$156$934)->rc = _new_cnt$2424;
      } else if (_cnt$2423 == 1) {
        moonbit_free(_$2a$try$156$934);
      }
      _try_err$938 = _field$2154;
      if (
        $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$937);
        moonbit_decref(_cont$936);
        err$942 = _try_err$938;
        goto $join$941;
      } else {
        moonbit_decref(_err_cont$935);
        err$940 = _try_err$938;
        goto $join$939;
      }
      goto $joinlet$2544;
      $join$941:;
      return _err_cont$935->code(_err_cont$935, err$942);
      $joinlet$2544:;
      goto $joinlet$2543;
      $join$939:;
      _tmp$1966 = on_err$937->code(on_err$937, err$940);
      _cont$936->code(_cont$936, _tmp$1966);
      $joinlet$2543:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1* _State_1$943 =
        (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$172$on_err$68$$2a$arm$164$lambda$190$State$State_1*)_state$933;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2161 = _State_1$943->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$944 = _field$2161;
      moonbit_string_t _field$2160 = _State_1$943->$3;
      moonbit_string_t name$945 = _field$2160;
      moonbit_string_t _field$2159 = _State_1$943->$2;
      moonbit_string_t file_name$946 = _field$2159;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2158 =
        _State_1$943->$1;
      int32_t _cnt$2425 = Moonbit_object_header(_State_1$943)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$947;
      int32_t _tmp$1965;
      if (_cnt$2425 > 1) {
        int32_t _new_cnt$2426;
        moonbit_incref(_cont$944);
        moonbit_incref(name$945);
        moonbit_incref(file_name$946);
        moonbit_incref(_field$2158);
        _new_cnt$2426 = _cnt$2425 - 1;
        Moonbit_object_header(_State_1$943)->rc = _new_cnt$2426;
      } else if (_cnt$2425 == 1) {
        moonbit_free(_State_1$943);
      }
      handle_result$947 = _field$2158;
      _tmp$1965
      = handle_result$947->code(
        handle_result$947,
          name$945,
          file_name$946,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$944->code(_cont$944, _tmp$1965);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1959,
  void* err$913
) {
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$1960 =
    (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$1959;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2168 =
    _casted_env$1960->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$892 =
    _field$2168;
  moonbit_string_t _field$2167 = _casted_env$1960->$1;
  moonbit_string_t file_name$903 = _field$2167;
  moonbit_string_t _field$2166 = _casted_env$1960->$0;
  int32_t _cnt$2427 = Moonbit_object_header(_casted_env$1960)->rc;
  moonbit_string_t name$905;
  void* e$915;
  moonbit_string_t e$918;
  moonbit_string_t message$916;
  if (_cnt$2427 > 1) {
    int32_t _new_cnt$2428;
    moonbit_incref(handle_result$892);
    moonbit_incref(file_name$903);
    moonbit_incref(_field$2166);
    _new_cnt$2428 = _cnt$2427 - 1;
    Moonbit_object_header(_casted_env$1960)->rc = _new_cnt$2428;
  } else if (_cnt$2427 == 1) {
    moonbit_free(_casted_env$1960);
  }
  name$905 = _field$2166;
  switch (Moonbit_object_tag(err$913)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$919 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$913;
      moonbit_string_t _field$2162 = _Failure$919->$0;
      int32_t _cnt$2429 = Moonbit_object_header(_Failure$919)->rc;
      moonbit_string_t _e$920;
      if (_cnt$2429 > 1) {
        int32_t _new_cnt$2430;
        moonbit_incref(_field$2162);
        _new_cnt$2430 = _cnt$2429 - 1;
        Moonbit_object_header(_Failure$919)->rc = _new_cnt$2430;
      } else if (_cnt$2429 == 1) {
        moonbit_free(_Failure$919);
      }
      _e$920 = _field$2162;
      e$918 = _e$920;
      goto $join$917;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$921 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$913;
      moonbit_string_t _field$2163 = _InspectError$921->$0;
      int32_t _cnt$2431 = Moonbit_object_header(_InspectError$921)->rc;
      moonbit_string_t _e$922;
      if (_cnt$2431 > 1) {
        int32_t _new_cnt$2432;
        moonbit_incref(_field$2163);
        _new_cnt$2432 = _cnt$2431 - 1;
        Moonbit_object_header(_InspectError$921)->rc = _new_cnt$2432;
      } else if (_cnt$2431 == 1) {
        moonbit_free(_InspectError$921);
      }
      _e$922 = _field$2163;
      e$918 = _e$922;
      goto $join$917;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$923 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$913;
      moonbit_string_t _field$2164 = _SnapshotError$923->$0;
      int32_t _cnt$2433 = Moonbit_object_header(_SnapshotError$923)->rc;
      moonbit_string_t _e$924;
      if (_cnt$2433 > 1) {
        int32_t _new_cnt$2434;
        moonbit_incref(_field$2164);
        _new_cnt$2434 = _cnt$2433 - 1;
        Moonbit_object_header(_SnapshotError$923)->rc = _new_cnt$2434;
      } else if (_cnt$2433 == 1) {
        moonbit_free(_SnapshotError$923);
      }
      _e$924 = _field$2164;
      e$918 = _e$924;
      goto $join$917;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$working_tests_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$925 =
        (struct $Error$azimuth$telemetry$working_tests_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$913;
      moonbit_string_t _field$2165 =
        _MoonBitTestDriverInternalJsError$925->$0;
      int32_t _cnt$2435 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$925)->rc;
      moonbit_string_t _e$926;
      if (_cnt$2435 > 1) {
        int32_t _new_cnt$2436;
        moonbit_incref(_field$2165);
        _new_cnt$2436 = _cnt$2435 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$925)->rc
        = _new_cnt$2436;
      } else if (_cnt$2435 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$925);
      }
      _e$926 = _field$2165;
      e$918 = _e$926;
      goto $join$917;
      break;
    }
    default: {
      e$915 = err$913;
      goto $join$914;
      break;
    }
  }
  goto $joinlet$2546;
  $join$917:;
  $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$892, name$905, file_name$903, e$918, 0
  );
  $joinlet$2546:;
  goto $joinlet$2545;
  $join$914:;
  message$916 = $Error$to_string(e$915);
  $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$892, name$905, file_name$903, message$916, 0
  );
  $joinlet$2545:;
  return 0;
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1945,
  moonbit_string_t attr$906
) {
  int32_t _tmp$1947;
  int64_t _tmp$1946;
  moonbit_decref(_env$1945);
  _tmp$1947 = Moonbit_array_length(attr$906);
  _tmp$1946 = (int64_t)_tmp$1947;
  moonbit_incref(attr$906);
  if ($String$$char_length_ge$inner(attr$906, 5, 0, _tmp$1946)) {
    int32_t _tmp$1952 = attr$906[0];
    int32_t _x$907 = _tmp$1952;
    if (_x$907 == 112) {
      int32_t _tmp$1951 = attr$906[1];
      int32_t _x$908 = _tmp$1951;
      if (_x$908 == 97) {
        int32_t _tmp$1950 = attr$906[2];
        int32_t _x$909 = _tmp$1950;
        if (_x$909 == 110) {
          int32_t _tmp$1949 = attr$906[3];
          int32_t _x$910 = _tmp$1949;
          if (_x$910 == 105) {
            int32_t _tmp$2169 = attr$906[4];
            int32_t _tmp$1948;
            int32_t _x$911;
            moonbit_decref(attr$906);
            _tmp$1948 = _tmp$2169;
            _x$911 = _tmp$1948;
            return _x$911 == 99 || 0;
          } else {
            moonbit_decref(attr$906);
            return 0;
          }
        } else {
          moonbit_decref(attr$906);
          return 0;
        }
      } else {
        moonbit_decref(attr$906);
        return 0;
      }
    } else {
      moonbit_decref(attr$906);
      return 0;
    }
  } else {
    moonbit_decref(attr$906);
    return 0;
  }
}

int32_t $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1929,
  moonbit_string_t test_name$893,
  moonbit_string_t file_name$894,
  moonbit_string_t message$895,
  int32_t skipped$896
) {
  struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$1930 =
    (struct $$azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$1929;
  int32_t _field$2170 = _casted_env$1930->$0;
  int32_t index$891;
  int32_t _if_result$2547;
  moonbit_string_t file_name$897;
  moonbit_string_t test_name$898;
  moonbit_string_t message$899;
  moonbit_string_t _tmp$1942;
  moonbit_string_t _tmp$1941;
  moonbit_string_t _tmp$1939;
  moonbit_string_t _tmp$1940;
  moonbit_string_t _tmp$1938;
  moonbit_string_t _tmp$1936;
  moonbit_string_t _tmp$1937;
  moonbit_string_t _tmp$1935;
  moonbit_string_t _tmp$1933;
  moonbit_string_t _tmp$1934;
  moonbit_string_t _tmp$1932;
  moonbit_string_t _tmp$1931;
  moonbit_decref(_casted_env$1930);
  index$891 = _field$2170;
  if (!skipped$896) {
    _if_result$2547 = 1;
  } else {
    _if_result$2547 = 0;
  }
  if (_if_result$2547) {
    
  }
  file_name$897 = $String$$escape(file_name$894);
  test_name$898 = $String$$escape(test_name$893);
  message$899 = $String$$escape(message$895);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$1942
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$897
  );
  _tmp$1941
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$1942
  );
  _tmp$1939
  = moonbit_add_string(
    _tmp$1941, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$1940
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$891
  );
  _tmp$1938 = moonbit_add_string(_tmp$1939, _tmp$1940);
  _tmp$1936
  = moonbit_add_string(
    _tmp$1938, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$1937
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$898
  );
  _tmp$1935 = moonbit_add_string(_tmp$1936, _tmp$1937);
  _tmp$1933
  = moonbit_add_string(
    _tmp$1935, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$1934 = $$moonbitlang$core$builtin$Show$$String$$to_string(message$899);
  _tmp$1932 = moonbit_add_string(_tmp$1933, _tmp$1934);
  _tmp$1931
  = moonbit_add_string(
    _tmp$1932, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$1931);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$888
) {
  moonbit_decref(_discard_$888);
  return 42;
}

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$886,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$887,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$884
) {
  void* _try_err$882;
  struct moonbit_result_0 _tmp$2549 = f$886->code(f$886);
  void* err$883;
  if (_tmp$2549.tag) {
    int32_t const _ok$1927 = _tmp$2549.data.ok;
    moonbit_decref(on_err$884);
  } else {
    void* const _err$1928 = _tmp$2549.data.err;
    moonbit_decref(on_ok$887);
    _try_err$882 = _err$1928;
    goto $join$881;
  }
  on_ok$887->code(on_ok$887);
  goto $joinlet$2548;
  $join$881:;
  err$883 = _try_err$882;
  on_err$884->code(on_err$884, err$883);
  $joinlet$2548:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$847,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$860,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$873,
  moonbit_string_t file_filter$844,
  int32_t index_filter$845
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$841;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$842;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$846;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2176;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1918;
  void* F0$1915;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2175;
  int32_t _cnt$2437;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1917;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1916;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$843;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$856;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$857;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$859;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2174;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1922;
  void* F1$1919;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2173;
  int32_t _cnt$2440;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1921;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1920;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$858;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$869;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$870;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$872;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2172;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1926;
  void* F2$1923;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2171;
  int32_t _cnt$2443;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1925;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1924;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$871;
  moonbit_incref(file_filter$844);
  _bind$846
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$847, file_filter$844
  );
  if (_bind$846 == 0) {
    if (_bind$846) {
      moonbit_decref(_bind$846);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$848 =
      _bind$846;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$849 =
      _Some$848;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$851;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$852;
    moonbit_incref(_index_func_map$849);
    _bind$852
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$849, index_filter$845
    );
    if (_bind$852 == 0) {
      if (_bind$852) {
        moonbit_decref(_bind$852);
      }
      moonbit_decref(_index_func_map$849);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$853;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$854;
      moonbit_decref(async_tests$873);
      moonbit_decref(with_args_tests$860);
      _Some$853 = _bind$852;
      _func_attrs_tuple$854 = _Some$853;
      func_attrs_tuple$851 = _func_attrs_tuple$854;
      goto $join$850;
    }
    goto $joinlet$2551;
    $join$850:;
    index_func_map$841 = _index_func_map$849;
    func_attrs_tuple$842 = func_attrs_tuple$851;
    goto $join$840;
    $joinlet$2551:;
  }
  goto $joinlet$2550;
  $join$840:;
  moonbit_decref(index_func_map$841);
  _field$2176 = func_attrs_tuple$842->$0;
  _tmp$1918 = _field$2176;
  moonbit_incref(_tmp$1918);
  F0$1915
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$1915)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$1915)->$0 = _tmp$1918;
  _field$2175 = func_attrs_tuple$842->$1;
  _cnt$2437 = Moonbit_object_header(func_attrs_tuple$842)->rc;
  if (_cnt$2437 > 1) {
    int32_t _new_cnt$2439;
    moonbit_incref(_field$2175);
    _new_cnt$2439 = _cnt$2437 - 1;
    Moonbit_object_header(func_attrs_tuple$842)->rc = _new_cnt$2439;
  } else if (_cnt$2437 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2438 =
      func_attrs_tuple$842->$0;
    moonbit_decref(_field$2438);
    moonbit_free(func_attrs_tuple$842);
  }
  _tmp$1917 = _field$2175;
  _tmp$1916
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1916)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1916->$0 = file_filter$844;
  _tmp$1916->$1 = index_filter$845;
  _tmp$1916->$2 = _tmp$1917;
  k$843
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$843)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$843->$0 = F0$1915;
  k$843->$1 = _tmp$1916;
  return k$843;
  $joinlet$2550:;
  moonbit_incref(file_filter$844);
  _bind$859
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$860, file_filter$844
  );
  if (_bind$859 == 0) {
    if (_bind$859) {
      moonbit_decref(_bind$859);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$861 =
      _bind$859;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$862 =
      _Some$861;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$864;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$865;
    moonbit_incref(_index_func_map$862);
    _bind$865
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$862, index_filter$845
    );
    if (_bind$865 == 0) {
      if (_bind$865) {
        moonbit_decref(_bind$865);
      }
      moonbit_decref(_index_func_map$862);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$866;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$867;
      moonbit_decref(async_tests$873);
      _Some$866 = _bind$865;
      _func_attrs_tuple$867 = _Some$866;
      func_attrs_tuple$864 = _func_attrs_tuple$867;
      goto $join$863;
    }
    goto $joinlet$2553;
    $join$863:;
    index_func_map$856 = _index_func_map$862;
    func_attrs_tuple$857 = func_attrs_tuple$864;
    goto $join$855;
    $joinlet$2553:;
  }
  goto $joinlet$2552;
  $join$855:;
  moonbit_decref(index_func_map$856);
  _field$2174 = func_attrs_tuple$857->$0;
  _tmp$1922 = _field$2174;
  moonbit_incref(_tmp$1922);
  F1$1919
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$1919)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$1919)->$0 = _tmp$1922;
  _field$2173 = func_attrs_tuple$857->$1;
  _cnt$2440 = Moonbit_object_header(func_attrs_tuple$857)->rc;
  if (_cnt$2440 > 1) {
    int32_t _new_cnt$2442;
    moonbit_incref(_field$2173);
    _new_cnt$2442 = _cnt$2440 - 1;
    Moonbit_object_header(func_attrs_tuple$857)->rc = _new_cnt$2442;
  } else if (_cnt$2440 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2441 =
      func_attrs_tuple$857->$0;
    moonbit_decref(_field$2441);
    moonbit_free(func_attrs_tuple$857);
  }
  _tmp$1921 = _field$2173;
  _tmp$1920
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1920)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1920->$0 = file_filter$844;
  _tmp$1920->$1 = index_filter$845;
  _tmp$1920->$2 = _tmp$1921;
  k$858
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$858)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$858->$0 = F1$1919;
  k$858->$1 = _tmp$1920;
  return k$858;
  $joinlet$2552:;
  moonbit_incref(file_filter$844);
  _bind$872
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$873, file_filter$844
  );
  if (_bind$872 == 0) {
    if (_bind$872) {
      moonbit_decref(_bind$872);
    }
    moonbit_decref(file_filter$844);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$874 =
      _bind$872;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$875 =
      _Some$874;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$877;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$878;
    moonbit_incref(_index_func_map$875);
    _bind$878
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$875, index_filter$845
    );
    if (_bind$878 == 0) {
      if (_bind$878) {
        moonbit_decref(_bind$878);
      }
      moonbit_decref(_index_func_map$875);
      moonbit_decref(file_filter$844);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$879 =
        _bind$878;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$880 =
        _Some$879;
      func_attrs_tuple$877 = _func_attrs_tuple$880;
      goto $join$876;
    }
    goto $joinlet$2555;
    $join$876:;
    index_func_map$869 = _index_func_map$875;
    func_attrs_tuple$870 = func_attrs_tuple$877;
    goto $join$868;
    $joinlet$2555:;
  }
  goto $joinlet$2554;
  $join$868:;
  moonbit_decref(index_func_map$869);
  _field$2172 = func_attrs_tuple$870->$0;
  _tmp$1926 = _field$2172;
  moonbit_incref(_tmp$1926);
  F2$1923
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$1923)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$1923)->$0 = _tmp$1926;
  _field$2171 = func_attrs_tuple$870->$1;
  _cnt$2443 = Moonbit_object_header(func_attrs_tuple$870)->rc;
  if (_cnt$2443 > 1) {
    int32_t _new_cnt$2445;
    moonbit_incref(_field$2171);
    _new_cnt$2445 = _cnt$2443 - 1;
    Moonbit_object_header(func_attrs_tuple$870)->rc = _new_cnt$2445;
  } else if (_cnt$2443 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2444 =
      func_attrs_tuple$870->$0;
    moonbit_decref(_field$2444);
    moonbit_free(func_attrs_tuple$870);
  }
  _tmp$1925 = _field$2171;
  _tmp$1924
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1924)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1924->$0 = file_filter$844;
  _tmp$1924->$1 = index_filter$845;
  _tmp$1924->$2 = _tmp$1925;
  k$871
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$871)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$871->$0 = F2$1923;
  k$871->$1 = _tmp$1924;
  return k$871;
  $joinlet$2554:;
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$834
) {
  int32_t _field$2177 = self$834->$1;
  int32_t len$1914;
  moonbit_decref(self$834);
  len$1914 = _field$2177;
  return len$1914 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$832,
  struct $$moonbitlang$core$builtin$Logger logger$833
) {
  moonbit_string_t _tmp$1913 = self$832;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$1912 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$1913);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$1912, logger$833
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$818,
  struct $$moonbitlang$core$builtin$Logger logger$831
) {
  struct $StringView _field$2186 =
    (struct $StringView){self$818->$0_1, self$818->$0_2, self$818->$0_0};
  struct $StringView pkg$817 = _field$2186;
  int32_t _tmp$1911 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$1910;
  int64_t _bind$819;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$820;
  struct $StringView _field$2185;
  struct $StringView _module_name$827;
  void* _field$2184;
  int32_t _cnt$2446;
  void* _package_name$828;
  struct $StringView _field$2182;
  struct $StringView filename$1893;
  struct $StringView _field$2181;
  struct $StringView start_line$1894;
  struct $StringView _field$2180;
  struct $StringView start_column$1895;
  struct $StringView _field$2179;
  struct $StringView end_line$1896;
  struct $StringView _field$2178;
  int32_t _cnt$2450;
  struct $StringView end_column$1897;
  struct $$moonbitlang$core$builtin$Logger _bind$1892;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$1910
  = (struct $StringView){
    0, _tmp$1911, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$817.$0);
  moonbit_incref(pkg$817.$0);
  _bind$819 = $StringView$$find(pkg$817, _tmp$1910);
  if (_bind$819 == 4294967296ll) {
    void* None$1898 =
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
    _bind$820->$1 = None$1898;
  } else {
    int64_t _Some$821 = _bind$819;
    int32_t _first_slash$822 = (int32_t)_Some$821;
    int32_t _tmp$1909 = _first_slash$822 + 1;
    struct $StringView _tmp$1906;
    int32_t _tmp$1908;
    struct $StringView _tmp$1907;
    int64_t _bind$823;
    moonbit_incref(pkg$817.$0);
    _tmp$1906 = $StringView$$view$inner(pkg$817, _tmp$1909, 4294967296ll);
    _tmp$1908
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$1907
    = (struct $StringView){
      0, _tmp$1908, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$823 = $StringView$$find(_tmp$1906, _tmp$1907);
    if (_bind$823 == 4294967296ll) {
      void* None$1899 =
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
      _bind$820->$1 = None$1899;
    } else {
      int64_t _Some$824 = _bind$823;
      int32_t _second_slash$825 = (int32_t)_Some$824;
      int32_t _tmp$1905 = _first_slash$822 + 1;
      int32_t module_name_end$826 = _tmp$1905 + _second_slash$825;
      int64_t _tmp$1904 = (int64_t)module_name_end$826;
      struct $StringView _tmp$1900;
      int32_t _tmp$1903;
      struct $StringView _tmp$1902;
      void* Some$1901;
      moonbit_incref(pkg$817.$0);
      _tmp$1900 = $StringView$$view$inner(pkg$817, 0, _tmp$1904);
      _tmp$1903 = module_name_end$826 + 1;
      _tmp$1902 = $StringView$$view$inner(pkg$817, _tmp$1903, 4294967296ll);
      Some$1901
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$1901)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$1901)->$0_0
      = _tmp$1902.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1901)->$0_1
      = _tmp$1902.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1901)->$0_2
      = _tmp$1902.$2;
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
      _bind$820->$0_0 = _tmp$1900.$0;
      _bind$820->$0_1 = _tmp$1900.$1;
      _bind$820->$0_2 = _tmp$1900.$2;
      _bind$820->$1 = Some$1901;
    }
  }
  _field$2185
  = (struct $StringView){
    _bind$820->$0_1, _bind$820->$0_2, _bind$820->$0_0
  };
  _module_name$827 = _field$2185;
  _field$2184 = _bind$820->$1;
  _cnt$2446 = Moonbit_object_header(_bind$820)->rc;
  if (_cnt$2446 > 1) {
    int32_t _new_cnt$2447;
    moonbit_incref(_field$2184);
    moonbit_incref(_module_name$827.$0);
    _new_cnt$2447 = _cnt$2446 - 1;
    Moonbit_object_header(_bind$820)->rc = _new_cnt$2447;
  } else if (_cnt$2446 == 1) {
    moonbit_free(_bind$820);
  }
  _package_name$828 = _field$2184;
  switch (Moonbit_object_tag(_package_name$828)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$829 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$828;
      struct $StringView _field$2183 =
        (struct $StringView){
          _Some$829->$0_1, _Some$829->$0_2, _Some$829->$0_0
        };
      int32_t _cnt$2448 = Moonbit_object_header(_Some$829)->rc;
      struct $StringView _pkg_name$830;
      struct $$moonbitlang$core$builtin$Logger _bind$1891;
      if (_cnt$2448 > 1) {
        int32_t _new_cnt$2449;
        moonbit_incref(_field$2183.$0);
        _new_cnt$2449 = _cnt$2448 - 1;
        Moonbit_object_header(_Some$829)->rc = _new_cnt$2449;
      } else if (_cnt$2448 == 1) {
        moonbit_free(_Some$829);
      }
      _pkg_name$830 = _field$2183;
      if (logger$831.$1) {
        moonbit_incref(logger$831.$1);
      }
      logger$831.$0->$method_2(logger$831.$1, _pkg_name$830);
      _bind$1891 = logger$831;
      if (_bind$1891.$1) {
        moonbit_incref(_bind$1891.$1);
      }
      _bind$1891.$0->$method_3(_bind$1891.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$828);
      break;
    }
  }
  _field$2182
  = (struct $StringView){
    self$818->$1_1, self$818->$1_2, self$818->$1_0
  };
  filename$1893 = _field$2182;
  moonbit_incref(filename$1893.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, filename$1893);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2181
  = (struct $StringView){
    self$818->$2_1, self$818->$2_2, self$818->$2_0
  };
  start_line$1894 = _field$2181;
  moonbit_incref(start_line$1894.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, start_line$1894);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2180
  = (struct $StringView){
    self$818->$3_1, self$818->$3_2, self$818->$3_0
  };
  start_column$1895 = _field$2180;
  moonbit_incref(start_column$1895.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, start_column$1895);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 45);
  _field$2179
  = (struct $StringView){
    self$818->$4_1, self$818->$4_2, self$818->$4_0
  };
  end_line$1896 = _field$2179;
  moonbit_incref(end_line$1896.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, end_line$1896);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2178
  = (struct $StringView){
    self$818->$5_1, self$818->$5_2, self$818->$5_0
  };
  _cnt$2450 = Moonbit_object_header(self$818)->rc;
  if (_cnt$2450 > 1) {
    int32_t _new_cnt$2456;
    moonbit_incref(_field$2178.$0);
    _new_cnt$2456 = _cnt$2450 - 1;
    Moonbit_object_header(self$818)->rc = _new_cnt$2456;
  } else if (_cnt$2450 == 1) {
    struct $StringView _field$2455 =
      (struct $StringView){self$818->$4_1, self$818->$4_2, self$818->$4_0};
    struct $StringView _field$2454;
    struct $StringView _field$2453;
    struct $StringView _field$2452;
    struct $StringView _field$2451;
    moonbit_decref(_field$2455.$0);
    _field$2454
    = (struct $StringView){
      self$818->$3_1, self$818->$3_2, self$818->$3_0
    };
    moonbit_decref(_field$2454.$0);
    _field$2453
    = (struct $StringView){
      self$818->$2_1, self$818->$2_2, self$818->$2_0
    };
    moonbit_decref(_field$2453.$0);
    _field$2452
    = (struct $StringView){
      self$818->$1_1, self$818->$1_2, self$818->$1_0
    };
    moonbit_decref(_field$2452.$0);
    _field$2451
    = (struct $StringView){
      self$818->$0_1, self$818->$0_2, self$818->$0_0
    };
    moonbit_decref(_field$2451.$0);
    moonbit_free(self$818);
  }
  end_column$1897 = _field$2178;
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, end_column$1897);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 64);
  _bind$1892 = logger$831;
  _bind$1892.$0->$method_2(_bind$1892.$1, _module_name$827);
  return 0;
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$816) {
  moonbit_string_t _tmp$1890 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$816);
  moonbit_println(_tmp$1890);
  moonbit_decref(_tmp$1890);
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
      int32_t _tmp$1888 = value$808[i$809];
      uint32_t _tmp$1887 = *(uint32_t*)&_tmp$1888;
      int32_t _tmp$1889;
      moonbit_incref(self$810);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$810, _tmp$1887);
      _tmp$1889 = i$809 + 1;
      i$809 = _tmp$1889;
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
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$2557 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1884;
  int32_t _tmp$1883;
  Moonbit_object_header(_closure$2557)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2557->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$2557->$0 = f$806;
  _tmp$1884 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$2557;
  _tmp$1883 = $$moonbitlang$core$builtin$Iter$$run$0(self$804, _tmp$1884);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$1883, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1885,
  moonbit_string_t k$805
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$1886 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$1885;
  struct $$3c$String$3e$$3d$$3e$Int* _field$2187 = _casted_env$1886->$0;
  int32_t _cnt$2457 = Moonbit_object_header(_casted_env$1886)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$806;
  if (_cnt$2457 > 1) {
    int32_t _new_cnt$2458;
    moonbit_incref(_field$2187);
    _new_cnt$2458 = _cnt$2457 - 1;
    Moonbit_object_header(_casted_env$1886)->rc = _new_cnt$2458;
  } else if (_cnt$2457 == 1) {
    moonbit_free(_casted_env$1886);
  }
  f$806 = _field$2187;
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
  moonbit_string_t* _tmp$1882 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$802);
  moonbit_string_t _tmp$2188;
  if (idx$803 < 0 || idx$803 >= Moonbit_array_length(_tmp$1882)) {
    moonbit_panic();
  }
  _tmp$2188 = (moonbit_string_t)_tmp$1882[idx$803];
  moonbit_incref(_tmp$2188);
  moonbit_decref(_tmp$1882);
  return _tmp$2188;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$800,
  int32_t idx$801
) {
  struct $$3c$String$2a$Int$3e$** _tmp$1881 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$800);
  struct $$3c$String$2a$Int$3e$* _tmp$2189;
  if (idx$801 < 0 || idx$801 >= Moonbit_array_length(_tmp$1881)) {
    moonbit_panic();
  }
  _tmp$2189 = (struct $$3c$String$2a$Int$3e$*)_tmp$1881[idx$801];
  if (_tmp$2189) {
    moonbit_incref(_tmp$2189);
  }
  moonbit_decref(_tmp$1881);
  return _tmp$2189;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$796,
  int32_t key$792
) {
  int32_t hash$791 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$792);
  int32_t capacity_mask$1880 = self$796->$3;
  int32_t _tmp$1879 = hash$791 & capacity_mask$1880;
  int32_t i$793 = 0;
  int32_t idx$794 = _tmp$1879;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2193 =
      self$796->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1878 =
      _field$2193;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2192;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$795;
    if (idx$794 < 0 || idx$794 >= Moonbit_array_length(entries$1878)) {
      moonbit_panic();
    }
    _tmp$2192
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1878[
        idx$794
      ];
    _bind$795 = _tmp$2192;
    if (_bind$795 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1867;
      if (_bind$795) {
        moonbit_incref(_bind$795);
      }
      moonbit_decref(self$796);
      if (_bind$795) {
        moonbit_decref(_bind$795);
      }
      _tmp$1867 = 0;
      return _tmp$1867;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$797 =
        _bind$795;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$798 =
        _Some$797;
      int32_t hash$1869 = _entry$798->$3;
      int32_t _if_result$2559;
      int32_t _field$2190;
      int32_t psl$1872;
      int32_t _tmp$1874;
      int32_t _tmp$1876;
      int32_t capacity_mask$1877;
      int32_t _tmp$1875;
      if (hash$1869 == hash$791) {
        int32_t key$1868 = _entry$798->$4;
        _if_result$2559 = key$1868 == key$792;
      } else {
        _if_result$2559 = 0;
      }
      if (_if_result$2559) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2191;
        int32_t _cnt$2459;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1871;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1870;
        moonbit_incref(_entry$798);
        moonbit_decref(self$796);
        _field$2191 = _entry$798->$5;
        _cnt$2459 = Moonbit_object_header(_entry$798)->rc;
        if (_cnt$2459 > 1) {
          int32_t _new_cnt$2461;
          moonbit_incref(_field$2191);
          _new_cnt$2461 = _cnt$2459 - 1;
          Moonbit_object_header(_entry$798)->rc = _new_cnt$2461;
        } else if (_cnt$2459 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2460 =
            _entry$798->$1;
          if (_field$2460) {
            moonbit_decref(_field$2460);
          }
          moonbit_free(_entry$798);
        }
        value$1871 = _field$2191;
        _tmp$1870 = value$1871;
        return _tmp$1870;
      } else {
        moonbit_incref(_entry$798);
      }
      _field$2190 = _entry$798->$2;
      moonbit_decref(_entry$798);
      psl$1872 = _field$2190;
      if (i$793 > psl$1872) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1873;
        moonbit_decref(self$796);
        _tmp$1873 = 0;
        return _tmp$1873;
      }
      _tmp$1874 = i$793 + 1;
      _tmp$1876 = idx$794 + 1;
      capacity_mask$1877 = self$796->$3;
      _tmp$1875 = _tmp$1876 & capacity_mask$1877;
      i$793 = _tmp$1874;
      idx$794 = _tmp$1875;
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
  int32_t capacity_mask$1866;
  int32_t _tmp$1865;
  int32_t i$784;
  int32_t idx$785;
  moonbit_incref(key$783);
  hash$782 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$783);
  capacity_mask$1866 = self$787->$3;
  _tmp$1865 = hash$782 & capacity_mask$1866;
  i$784 = 0;
  idx$785 = _tmp$1865;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2199 =
      self$787->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1864 =
      _field$2199;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2198;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$786;
    if (idx$785 < 0 || idx$785 >= Moonbit_array_length(entries$1864)) {
      moonbit_panic();
    }
    _tmp$2198
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1864[
        idx$785
      ];
    _bind$786 = _tmp$2198;
    if (_bind$786 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1853;
      if (_bind$786) {
        moonbit_incref(_bind$786);
      }
      moonbit_decref(self$787);
      if (_bind$786) {
        moonbit_decref(_bind$786);
      }
      moonbit_decref(key$783);
      _tmp$1853 = 0;
      return _tmp$1853;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$788 =
        _bind$786;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$789 =
        _Some$788;
      int32_t hash$1855 = _entry$789->$3;
      int32_t _if_result$2561;
      int32_t _field$2194;
      int32_t psl$1858;
      int32_t _tmp$1860;
      int32_t _tmp$1862;
      int32_t capacity_mask$1863;
      int32_t _tmp$1861;
      if (hash$1855 == hash$782) {
        moonbit_string_t _field$2197 = _entry$789->$4;
        moonbit_string_t key$1854 = _field$2197;
        int32_t _tmp$2196 = moonbit_val_array_equal(key$1854, key$783);
        _if_result$2561 = _tmp$2196;
      } else {
        _if_result$2561 = 0;
      }
      if (_if_result$2561) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2195;
        int32_t _cnt$2462;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1857;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1856;
        moonbit_incref(_entry$789);
        moonbit_decref(self$787);
        moonbit_decref(key$783);
        _field$2195 = _entry$789->$5;
        _cnt$2462 = Moonbit_object_header(_entry$789)->rc;
        if (_cnt$2462 > 1) {
          int32_t _new_cnt$2465;
          moonbit_incref(_field$2195);
          _new_cnt$2465 = _cnt$2462 - 1;
          Moonbit_object_header(_entry$789)->rc = _new_cnt$2465;
        } else if (_cnt$2462 == 1) {
          moonbit_string_t _field$2464 = _entry$789->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2463;
          moonbit_decref(_field$2464);
          _field$2463 = _entry$789->$1;
          if (_field$2463) {
            moonbit_decref(_field$2463);
          }
          moonbit_free(_entry$789);
        }
        value$1857 = _field$2195;
        _tmp$1856 = value$1857;
        return _tmp$1856;
      } else {
        moonbit_incref(_entry$789);
      }
      _field$2194 = _entry$789->$2;
      moonbit_decref(_entry$789);
      psl$1858 = _field$2194;
      if (i$784 > psl$1858) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1859;
        moonbit_decref(self$787);
        moonbit_decref(key$783);
        _tmp$1859 = 0;
        return _tmp$1859;
      }
      _tmp$1860 = i$784 + 1;
      _tmp$1862 = idx$785 + 1;
      capacity_mask$1863 = self$787->$3;
      _tmp$1861 = _tmp$1862 & capacity_mask$1863;
      i$784 = _tmp$1860;
      idx$785 = _tmp$1861;
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
  int32_t capacity_mask$1852 = self$778->$3;
  int32_t _tmp$1851 = hash$773 & capacity_mask$1852;
  int32_t i$775 = 0;
  int32_t idx$776 = _tmp$1851;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2203 =
      self$778->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1850 =
      _field$2203;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2202;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$777;
    if (idx$776 < 0 || idx$776 >= Moonbit_array_length(entries$1850)) {
      moonbit_panic();
    }
    _tmp$2202
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1850[
        idx$776
      ];
    _bind$777 = _tmp$2202;
    if (_bind$777 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1839;
      if (_bind$777) {
        moonbit_incref(_bind$777);
      }
      moonbit_decref(self$778);
      if (_bind$777) {
        moonbit_decref(_bind$777);
      }
      _tmp$1839 = 0;
      return _tmp$1839;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$779 =
        _bind$777;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$780 =
        _Some$779;
      int32_t hash$1841 = _entry$780->$3;
      int32_t _if_result$2563;
      int32_t _field$2200;
      int32_t psl$1844;
      int32_t _tmp$1846;
      int32_t _tmp$1848;
      int32_t capacity_mask$1849;
      int32_t _tmp$1847;
      if (hash$1841 == hash$773) {
        int32_t key$1840 = _entry$780->$4;
        _if_result$2563 = key$1840 == key$774;
      } else {
        _if_result$2563 = 0;
      }
      if (_if_result$2563) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2201;
        int32_t _cnt$2466;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1843;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1842;
        moonbit_incref(_entry$780);
        moonbit_decref(self$778);
        _field$2201 = _entry$780->$5;
        _cnt$2466 = Moonbit_object_header(_entry$780)->rc;
        if (_cnt$2466 > 1) {
          int32_t _new_cnt$2468;
          moonbit_incref(_field$2201);
          _new_cnt$2468 = _cnt$2466 - 1;
          Moonbit_object_header(_entry$780)->rc = _new_cnt$2468;
        } else if (_cnt$2466 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2467 =
            _entry$780->$1;
          if (_field$2467) {
            moonbit_decref(_field$2467);
          }
          moonbit_free(_entry$780);
        }
        value$1843 = _field$2201;
        _tmp$1842 = value$1843;
        return _tmp$1842;
      } else {
        moonbit_incref(_entry$780);
      }
      _field$2200 = _entry$780->$2;
      moonbit_decref(_entry$780);
      psl$1844 = _field$2200;
      if (i$775 > psl$1844) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1845;
        moonbit_decref(self$778);
        _tmp$1845 = 0;
        return _tmp$1845;
      }
      _tmp$1846 = i$775 + 1;
      _tmp$1848 = idx$776 + 1;
      capacity_mask$1849 = self$778->$3;
      _tmp$1847 = _tmp$1848 & capacity_mask$1849;
      i$775 = _tmp$1846;
      idx$776 = _tmp$1847;
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
  int32_t capacity_mask$1838;
  int32_t _tmp$1837;
  int32_t i$766;
  int32_t idx$767;
  moonbit_incref(key$765);
  hash$764 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$765);
  capacity_mask$1838 = self$769->$3;
  _tmp$1837 = hash$764 & capacity_mask$1838;
  i$766 = 0;
  idx$767 = _tmp$1837;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2209 =
      self$769->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1836 =
      _field$2209;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2208;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$768;
    if (idx$767 < 0 || idx$767 >= Moonbit_array_length(entries$1836)) {
      moonbit_panic();
    }
    _tmp$2208
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1836[
        idx$767
      ];
    _bind$768 = _tmp$2208;
    if (_bind$768 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1825;
      if (_bind$768) {
        moonbit_incref(_bind$768);
      }
      moonbit_decref(self$769);
      if (_bind$768) {
        moonbit_decref(_bind$768);
      }
      moonbit_decref(key$765);
      _tmp$1825 = 0;
      return _tmp$1825;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$770 =
        _bind$768;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$771 =
        _Some$770;
      int32_t hash$1827 = _entry$771->$3;
      int32_t _if_result$2565;
      int32_t _field$2204;
      int32_t psl$1830;
      int32_t _tmp$1832;
      int32_t _tmp$1834;
      int32_t capacity_mask$1835;
      int32_t _tmp$1833;
      if (hash$1827 == hash$764) {
        moonbit_string_t _field$2207 = _entry$771->$4;
        moonbit_string_t key$1826 = _field$2207;
        int32_t _tmp$2206 = moonbit_val_array_equal(key$1826, key$765);
        _if_result$2565 = _tmp$2206;
      } else {
        _if_result$2565 = 0;
      }
      if (_if_result$2565) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2205;
        int32_t _cnt$2469;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1829;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1828;
        moonbit_incref(_entry$771);
        moonbit_decref(self$769);
        moonbit_decref(key$765);
        _field$2205 = _entry$771->$5;
        _cnt$2469 = Moonbit_object_header(_entry$771)->rc;
        if (_cnt$2469 > 1) {
          int32_t _new_cnt$2472;
          moonbit_incref(_field$2205);
          _new_cnt$2472 = _cnt$2469 - 1;
          Moonbit_object_header(_entry$771)->rc = _new_cnt$2472;
        } else if (_cnt$2469 == 1) {
          moonbit_string_t _field$2471 = _entry$771->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2470;
          moonbit_decref(_field$2471);
          _field$2470 = _entry$771->$1;
          if (_field$2470) {
            moonbit_decref(_field$2470);
          }
          moonbit_free(_entry$771);
        }
        value$1829 = _field$2205;
        _tmp$1828 = value$1829;
        return _tmp$1828;
      } else {
        moonbit_incref(_entry$771);
      }
      _field$2204 = _entry$771->$2;
      moonbit_decref(_entry$771);
      psl$1830 = _field$2204;
      if (i$766 > psl$1830) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1831;
        moonbit_decref(self$769);
        moonbit_decref(key$765);
        _tmp$1831 = 0;
        return _tmp$1831;
      }
      _tmp$1832 = i$766 + 1;
      _tmp$1834 = idx$767 + 1;
      capacity_mask$1835 = self$769->$3;
      _tmp$1833 = _tmp$1834 & capacity_mask$1835;
      i$766 = _tmp$1832;
      idx$767 = _tmp$1833;
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
  int32_t capacity_mask$1824 = self$760->$3;
  int32_t _tmp$1823 = hash$755 & capacity_mask$1824;
  int32_t i$757 = 0;
  int32_t idx$758 = _tmp$1823;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2213 =
      self$760->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1822 =
      _field$2213;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2212;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$759;
    if (idx$758 < 0 || idx$758 >= Moonbit_array_length(entries$1822)) {
      moonbit_panic();
    }
    _tmp$2212
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1822[
        idx$758
      ];
    _bind$759 = _tmp$2212;
    if (_bind$759 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1811;
      if (_bind$759) {
        moonbit_incref(_bind$759);
      }
      moonbit_decref(self$760);
      if (_bind$759) {
        moonbit_decref(_bind$759);
      }
      _tmp$1811 = 0;
      return _tmp$1811;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$761 =
        _bind$759;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$762 =
        _Some$761;
      int32_t hash$1813 = _entry$762->$3;
      int32_t _if_result$2567;
      int32_t _field$2210;
      int32_t psl$1816;
      int32_t _tmp$1818;
      int32_t _tmp$1820;
      int32_t capacity_mask$1821;
      int32_t _tmp$1819;
      if (hash$1813 == hash$755) {
        int32_t key$1812 = _entry$762->$4;
        _if_result$2567 = key$1812 == key$756;
      } else {
        _if_result$2567 = 0;
      }
      if (_if_result$2567) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2211;
        int32_t _cnt$2473;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1815;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1814;
        moonbit_incref(_entry$762);
        moonbit_decref(self$760);
        _field$2211 = _entry$762->$5;
        _cnt$2473 = Moonbit_object_header(_entry$762)->rc;
        if (_cnt$2473 > 1) {
          int32_t _new_cnt$2475;
          moonbit_incref(_field$2211);
          _new_cnt$2475 = _cnt$2473 - 1;
          Moonbit_object_header(_entry$762)->rc = _new_cnt$2475;
        } else if (_cnt$2473 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2474 =
            _entry$762->$1;
          if (_field$2474) {
            moonbit_decref(_field$2474);
          }
          moonbit_free(_entry$762);
        }
        value$1815 = _field$2211;
        _tmp$1814 = value$1815;
        return _tmp$1814;
      } else {
        moonbit_incref(_entry$762);
      }
      _field$2210 = _entry$762->$2;
      moonbit_decref(_entry$762);
      psl$1816 = _field$2210;
      if (i$757 > psl$1816) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1817;
        moonbit_decref(self$760);
        _tmp$1817 = 0;
        return _tmp$1817;
      }
      _tmp$1818 = i$757 + 1;
      _tmp$1820 = idx$758 + 1;
      capacity_mask$1821 = self$760->$3;
      _tmp$1819 = _tmp$1820 & capacity_mask$1821;
      i$757 = _tmp$1818;
      idx$758 = _tmp$1819;
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
  int32_t capacity_mask$1810;
  int32_t _tmp$1809;
  int32_t i$748;
  int32_t idx$749;
  moonbit_incref(key$747);
  hash$746 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$747);
  capacity_mask$1810 = self$751->$3;
  _tmp$1809 = hash$746 & capacity_mask$1810;
  i$748 = 0;
  idx$749 = _tmp$1809;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2219 =
      self$751->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1808 =
      _field$2219;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2218;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$750;
    if (idx$749 < 0 || idx$749 >= Moonbit_array_length(entries$1808)) {
      moonbit_panic();
    }
    _tmp$2218
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1808[
        idx$749
      ];
    _bind$750 = _tmp$2218;
    if (_bind$750 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1797;
      if (_bind$750) {
        moonbit_incref(_bind$750);
      }
      moonbit_decref(self$751);
      if (_bind$750) {
        moonbit_decref(_bind$750);
      }
      moonbit_decref(key$747);
      _tmp$1797 = 0;
      return _tmp$1797;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$752 =
        _bind$750;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$753 =
        _Some$752;
      int32_t hash$1799 = _entry$753->$3;
      int32_t _if_result$2569;
      int32_t _field$2214;
      int32_t psl$1802;
      int32_t _tmp$1804;
      int32_t _tmp$1806;
      int32_t capacity_mask$1807;
      int32_t _tmp$1805;
      if (hash$1799 == hash$746) {
        moonbit_string_t _field$2217 = _entry$753->$4;
        moonbit_string_t key$1798 = _field$2217;
        int32_t _tmp$2216 = moonbit_val_array_equal(key$1798, key$747);
        _if_result$2569 = _tmp$2216;
      } else {
        _if_result$2569 = 0;
      }
      if (_if_result$2569) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2215;
        int32_t _cnt$2476;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1801;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1800;
        moonbit_incref(_entry$753);
        moonbit_decref(self$751);
        moonbit_decref(key$747);
        _field$2215 = _entry$753->$5;
        _cnt$2476 = Moonbit_object_header(_entry$753)->rc;
        if (_cnt$2476 > 1) {
          int32_t _new_cnt$2479;
          moonbit_incref(_field$2215);
          _new_cnt$2479 = _cnt$2476 - 1;
          Moonbit_object_header(_entry$753)->rc = _new_cnt$2479;
        } else if (_cnt$2476 == 1) {
          moonbit_string_t _field$2478 = _entry$753->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2477;
          moonbit_decref(_field$2478);
          _field$2477 = _entry$753->$1;
          if (_field$2477) {
            moonbit_decref(_field$2477);
          }
          moonbit_free(_entry$753);
        }
        value$1801 = _field$2215;
        _tmp$1800 = value$1801;
        return _tmp$1800;
      } else {
        moonbit_incref(_entry$753);
      }
      _field$2214 = _entry$753->$2;
      moonbit_decref(_entry$753);
      psl$1802 = _field$2214;
      if (i$748 > psl$1802) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1803;
        moonbit_decref(self$751);
        moonbit_decref(key$747);
        _tmp$1803 = 0;
        return _tmp$1803;
      }
      _tmp$1804 = i$748 + 1;
      _tmp$1806 = idx$749 + 1;
      capacity_mask$1807 = self$751->$3;
      _tmp$1805 = _tmp$1806 & capacity_mask$1807;
      i$748 = _tmp$1804;
      idx$749 = _tmp$1805;
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
  int32_t _tmp$1788;
  int32_t _tmp$1787;
  int32_t _tmp$1796;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$741;
  int32_t _len$742;
  int32_t _i$743;
  moonbit_incref(arr$739.$0);
  length$738 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$739);
  capacity$740 = $Int$$next_power_of_two(length$738);
  _tmp$1788 = capacity$740;
  _tmp$1787 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1788);
  if (length$738 > _tmp$1787) {
    int32_t _tmp$1789 = capacity$740;
    capacity$740 = _tmp$1789 * 2;
  }
  _tmp$1796 = capacity$740;
  m$741 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$1796);
  moonbit_incref(arr$739.$0);
  _len$742 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$739);
  _i$743 = 0;
  while (1) {
    if (_i$743 < _len$742) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2223 =
        arr$739.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1792 =
        _field$2223;
      int32_t start$1794 = arr$739.$1;
      int32_t _tmp$1793 = start$1794 + _i$743;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2222 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1792[
          _tmp$1793
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$744 =
        _tmp$2222;
      moonbit_string_t _field$2221 = e$744->$0;
      moonbit_string_t _tmp$1790 = _field$2221;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2220 =
        e$744->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1791 =
        _field$2220;
      int32_t _tmp$1795;
      moonbit_incref(_tmp$1791);
      moonbit_incref(_tmp$1790);
      moonbit_incref(m$741);
      $$moonbitlang$core$builtin$Map$$set$3(m$741, _tmp$1790, _tmp$1791);
      _tmp$1795 = _i$743 + 1;
      _i$743 = _tmp$1795;
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
  int32_t _tmp$1778;
  int32_t _tmp$1777;
  int32_t _tmp$1786;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$733;
  int32_t _len$734;
  int32_t _i$735;
  moonbit_incref(arr$731.$0);
  length$730 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$731);
  capacity$732 = $Int$$next_power_of_two(length$730);
  _tmp$1778 = capacity$732;
  _tmp$1777 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1778);
  if (length$730 > _tmp$1777) {
    int32_t _tmp$1779 = capacity$732;
    capacity$732 = _tmp$1779 * 2;
  }
  _tmp$1786 = capacity$732;
  m$733 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$1786);
  moonbit_incref(arr$731.$0);
  _len$734 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$731);
  _i$735 = 0;
  while (1) {
    if (_i$735 < _len$734) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2227 =
        arr$731.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1782 =
        _field$2227;
      int32_t start$1784 = arr$731.$1;
      int32_t _tmp$1783 = start$1784 + _i$735;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2226 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1782[
          _tmp$1783
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$736 =
        _tmp$2226;
      moonbit_string_t _field$2225 = e$736->$0;
      moonbit_string_t _tmp$1780 = _field$2225;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2224 =
        e$736->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1781 =
        _field$2224;
      int32_t _tmp$1785;
      moonbit_incref(_tmp$1781);
      moonbit_incref(_tmp$1780);
      moonbit_incref(m$733);
      $$moonbitlang$core$builtin$Map$$set$2(m$733, _tmp$1780, _tmp$1781);
      _tmp$1785 = _i$735 + 1;
      _i$735 = _tmp$1785;
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
  int32_t _tmp$1768;
  int32_t _tmp$1767;
  int32_t _tmp$1776;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$725;
  int32_t _len$726;
  int32_t _i$727;
  moonbit_incref(arr$723.$0);
  length$722 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$723);
  capacity$724 = $Int$$next_power_of_two(length$722);
  _tmp$1768 = capacity$724;
  _tmp$1767 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1768);
  if (length$722 > _tmp$1767) {
    int32_t _tmp$1769 = capacity$724;
    capacity$724 = _tmp$1769 * 2;
  }
  _tmp$1776 = capacity$724;
  m$725 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$1776);
  moonbit_incref(arr$723.$0);
  _len$726 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$723);
  _i$727 = 0;
  while (1) {
    if (_i$727 < _len$726) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2230 =
        arr$723.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$1772 =
        _field$2230;
      int32_t start$1774 = arr$723.$1;
      int32_t _tmp$1773 = start$1774 + _i$727;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2229 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$1772[
          _tmp$1773
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$728 =
        _tmp$2229;
      int32_t _tmp$1770 = e$728->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2228 =
        e$728->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1771 =
        _field$2228;
      int32_t _tmp$1775;
      moonbit_incref(_tmp$1771);
      moonbit_incref(m$725);
      $$moonbitlang$core$builtin$Map$$set$1(m$725, _tmp$1770, _tmp$1771);
      _tmp$1775 = _i$727 + 1;
      _i$727 = _tmp$1775;
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
  int32_t _tmp$1758;
  int32_t _tmp$1757;
  int32_t _tmp$1766;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$717;
  int32_t _len$718;
  int32_t _i$719;
  moonbit_incref(arr$715.$0);
  length$714 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$715);
  capacity$716 = $Int$$next_power_of_two(length$714);
  _tmp$1758 = capacity$716;
  _tmp$1757 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1758);
  if (length$714 > _tmp$1757) {
    int32_t _tmp$1759 = capacity$716;
    capacity$716 = _tmp$1759 * 2;
  }
  _tmp$1766 = capacity$716;
  m$717 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$1766);
  moonbit_incref(arr$715.$0);
  _len$718 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$715);
  _i$719 = 0;
  while (1) {
    if (_i$719 < _len$718) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2234 =
        arr$715.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1762 =
        _field$2234;
      int32_t start$1764 = arr$715.$1;
      int32_t _tmp$1763 = start$1764 + _i$719;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2233 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1762[
          _tmp$1763
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$720 =
        _tmp$2233;
      moonbit_string_t _field$2232 = e$720->$0;
      moonbit_string_t _tmp$1760 = _field$2232;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2231 =
        e$720->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1761 =
        _field$2231;
      int32_t _tmp$1765;
      moonbit_incref(_tmp$1761);
      moonbit_incref(_tmp$1760);
      moonbit_incref(m$717);
      $$moonbitlang$core$builtin$Map$$set$0(m$717, _tmp$1760, _tmp$1761);
      _tmp$1765 = _i$719 + 1;
      _i$719 = _tmp$1765;
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
  int32_t _tmp$1756;
  moonbit_incref(key$712);
  _tmp$1756 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$712);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$711, key$712, value$713, _tmp$1756
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$708,
  moonbit_string_t key$709,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$710
) {
  int32_t _tmp$1755;
  moonbit_incref(key$709);
  _tmp$1755 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$709);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$708, key$709, value$710, _tmp$1755
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$705,
  int32_t key$706,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$707
) {
  int32_t _tmp$1754 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$706);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$705, key$706, value$707, _tmp$1754
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$702,
  moonbit_string_t key$703,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$704
) {
  int32_t _tmp$1753;
  moonbit_incref(key$703);
  _tmp$1753 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$703);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$702, key$703, value$704, _tmp$1753
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$692
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2241 =
    self$692->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$691 =
    _field$2241;
  int32_t capacity$1752 = self$692->$2;
  int32_t new_capacity$693 = capacity$1752 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1747 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1746 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$693, _tmp$1747
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2240 =
    self$692->$0;
  int32_t _tmp$1748;
  int32_t capacity$1750;
  int32_t _tmp$1749;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1751;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2239;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$694;
  if (old_head$691) {
    moonbit_incref(old_head$691);
  }
  moonbit_decref(_old$2240);
  self$692->$0 = _tmp$1746;
  self$692->$2 = new_capacity$693;
  _tmp$1748 = new_capacity$693 - 1;
  self$692->$3 = _tmp$1748;
  capacity$1750 = self$692->$2;
  _tmp$1749 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1750);
  self$692->$4 = _tmp$1749;
  self$692->$1 = 0;
  _tmp$1751 = 0;
  _old$2239 = self$692->$5;
  if (_old$2239) {
    moonbit_decref(_old$2239);
  }
  self$692->$5 = _tmp$1751;
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
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2238 =
        _x$696->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$697 =
        _field$2238;
      moonbit_string_t _field$2237 = _x$696->$4;
      moonbit_string_t _key$698 = _field$2237;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2236 =
        _x$696->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$699 =
        _field$2236;
      int32_t _field$2235 = _x$696->$3;
      int32_t _cnt$2480 = Moonbit_object_header(_x$696)->rc;
      int32_t _hash$700;
      if (_cnt$2480 > 1) {
        int32_t _new_cnt$2481;
        moonbit_incref(_value$699);
        moonbit_incref(_key$698);
        if (_next$697) {
          moonbit_incref(_next$697);
        }
        _new_cnt$2481 = _cnt$2480 - 1;
        Moonbit_object_header(_x$696)->rc = _new_cnt$2481;
      } else if (_cnt$2480 == 1) {
        moonbit_free(_x$696);
      }
      _hash$700 = _field$2235;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2248 =
    self$681->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$680 =
    _field$2248;
  int32_t capacity$1745 = self$681->$2;
  int32_t new_capacity$682 = capacity$1745 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1740 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1739 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$682, _tmp$1740
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2247 =
    self$681->$0;
  int32_t _tmp$1741;
  int32_t capacity$1743;
  int32_t _tmp$1742;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1744;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2246;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$683;
  if (old_head$680) {
    moonbit_incref(old_head$680);
  }
  moonbit_decref(_old$2247);
  self$681->$0 = _tmp$1739;
  self$681->$2 = new_capacity$682;
  _tmp$1741 = new_capacity$682 - 1;
  self$681->$3 = _tmp$1741;
  capacity$1743 = self$681->$2;
  _tmp$1742 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1743);
  self$681->$4 = _tmp$1742;
  self$681->$1 = 0;
  _tmp$1744 = 0;
  _old$2246 = self$681->$5;
  if (_old$2246) {
    moonbit_decref(_old$2246);
  }
  self$681->$5 = _tmp$1744;
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
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2245 =
        _x$685->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$686 =
        _field$2245;
      moonbit_string_t _field$2244 = _x$685->$4;
      moonbit_string_t _key$687 = _field$2244;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2243 =
        _x$685->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$688 =
        _field$2243;
      int32_t _field$2242 = _x$685->$3;
      int32_t _cnt$2482 = Moonbit_object_header(_x$685)->rc;
      int32_t _hash$689;
      if (_cnt$2482 > 1) {
        int32_t _new_cnt$2483;
        moonbit_incref(_value$688);
        moonbit_incref(_key$687);
        if (_next$686) {
          moonbit_incref(_next$686);
        }
        _new_cnt$2483 = _cnt$2482 - 1;
        Moonbit_object_header(_x$685)->rc = _new_cnt$2483;
      } else if (_cnt$2482 == 1) {
        moonbit_free(_x$685);
      }
      _hash$689 = _field$2242;
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
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2254 =
    self$670->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$669 =
    _field$2254;
  int32_t capacity$1738 = self$670->$2;
  int32_t new_capacity$671 = capacity$1738 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1733 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1732 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$671, _tmp$1733
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$2253 =
    self$670->$0;
  int32_t _tmp$1734;
  int32_t capacity$1736;
  int32_t _tmp$1735;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1737;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2252;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$672;
  if (old_head$669) {
    moonbit_incref(old_head$669);
  }
  moonbit_decref(_old$2253);
  self$670->$0 = _tmp$1732;
  self$670->$2 = new_capacity$671;
  _tmp$1734 = new_capacity$671 - 1;
  self$670->$3 = _tmp$1734;
  capacity$1736 = self$670->$2;
  _tmp$1735 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1736);
  self$670->$4 = _tmp$1735;
  self$670->$1 = 0;
  _tmp$1737 = 0;
  _old$2252 = self$670->$5;
  if (_old$2252) {
    moonbit_decref(_old$2252);
  }
  self$670->$5 = _tmp$1737;
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
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2251 =
        _x$674->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$675 =
        _field$2251;
      int32_t _key$676 = _x$674->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2250 =
        _x$674->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$677 =
        _field$2250;
      int32_t _field$2249 = _x$674->$3;
      int32_t _cnt$2484 = Moonbit_object_header(_x$674)->rc;
      int32_t _hash$678;
      if (_cnt$2484 > 1) {
        int32_t _new_cnt$2485;
        moonbit_incref(_value$677);
        if (_next$675) {
          moonbit_incref(_next$675);
        }
        _new_cnt$2485 = _cnt$2484 - 1;
        Moonbit_object_header(_x$674)->rc = _new_cnt$2485;
      } else if (_cnt$2484 == 1) {
        moonbit_free(_x$674);
      }
      _hash$678 = _field$2249;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2261 =
    self$659->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$658 =
    _field$2261;
  int32_t capacity$1731 = self$659->$2;
  int32_t new_capacity$660 = capacity$1731 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1726 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1725 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$660, _tmp$1726
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2260 =
    self$659->$0;
  int32_t _tmp$1727;
  int32_t capacity$1729;
  int32_t _tmp$1728;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1730;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2259;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$661;
  if (old_head$658) {
    moonbit_incref(old_head$658);
  }
  moonbit_decref(_old$2260);
  self$659->$0 = _tmp$1725;
  self$659->$2 = new_capacity$660;
  _tmp$1727 = new_capacity$660 - 1;
  self$659->$3 = _tmp$1727;
  capacity$1729 = self$659->$2;
  _tmp$1728 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1729);
  self$659->$4 = _tmp$1728;
  self$659->$1 = 0;
  _tmp$1730 = 0;
  _old$2259 = self$659->$5;
  if (_old$2259) {
    moonbit_decref(_old$2259);
  }
  self$659->$5 = _tmp$1730;
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
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2258 =
        _x$663->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$664 =
        _field$2258;
      moonbit_string_t _field$2257 = _x$663->$4;
      moonbit_string_t _key$665 = _field$2257;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2256 =
        _x$663->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$666 =
        _field$2256;
      int32_t _field$2255 = _x$663->$3;
      int32_t _cnt$2486 = Moonbit_object_header(_x$663)->rc;
      int32_t _hash$667;
      if (_cnt$2486 > 1) {
        int32_t _new_cnt$2487;
        moonbit_incref(_value$666);
        moonbit_incref(_key$665);
        if (_next$664) {
          moonbit_incref(_next$664);
        }
        _new_cnt$2487 = _cnt$2486 - 1;
        Moonbit_object_header(_x$663)->rc = _new_cnt$2487;
      } else if (_cnt$2486 == 1) {
        moonbit_free(_x$663);
      }
      _hash$667 = _field$2255;
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
  int32_t size$1711 = self$642->$1;
  int32_t grow_at$1712 = self$642->$4;
  int32_t capacity_mask$1724;
  int32_t _tmp$1723;
  struct $$3c$Int$2a$Int$3e$* _bind$643;
  int32_t psl$644;
  int32_t idx$645;
  int32_t _idx$653;
  int32_t _field$2262;
  int32_t _psl$654;
  int32_t _bind$655;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$656;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$657;
  if (size$1711 >= grow_at$1712) {
    moonbit_incref(self$642);
    $$moonbitlang$core$builtin$Map$$grow$3(self$642);
  }
  capacity_mask$1724 = self$642->$3;
  _tmp$1723 = hash$650 & capacity_mask$1724;
  psl$644 = 0;
  idx$645 = _tmp$1723;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2267 =
      self$642->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1722 =
      _field$2267;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2266;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$646;
    if (idx$645 < 0 || idx$645 >= Moonbit_array_length(entries$1722)) {
      moonbit_panic();
    }
    _tmp$2266
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1722[
        idx$645
      ];
    _bind$646 = _tmp$2266;
    if (_bind$646 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1713 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1713)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1713->$0 = idx$645;
      _tuple$1713->$1 = psl$644;
      _bind$643 = _tuple$1713;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$648 =
        _bind$646;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$649 =
        _Some$648;
      int32_t hash$1715 = _curr_entry$649->$3;
      int32_t _if_result$2579;
      int32_t psl$1716;
      int32_t _tmp$1718;
      int32_t _tmp$1720;
      int32_t capacity_mask$1721;
      int32_t _tmp$1719;
      if (hash$1715 == hash$650) {
        moonbit_string_t _field$2265 = _curr_entry$649->$4;
        moonbit_string_t key$1714 = _field$2265;
        int32_t _tmp$2264 = moonbit_val_array_equal(key$1714, key$651);
        _if_result$2579 = _tmp$2264;
      } else {
        _if_result$2579 = 0;
      }
      if (_if_result$2579) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2263;
        moonbit_incref(_curr_entry$649);
        moonbit_decref(key$651);
        moonbit_decref(self$642);
        _old$2263 = _curr_entry$649->$5;
        moonbit_decref(_old$2263);
        _curr_entry$649->$5 = value$652;
        moonbit_decref(_curr_entry$649);
        return 0;
      } else {
        moonbit_incref(_curr_entry$649);
      }
      psl$1716 = _curr_entry$649->$2;
      if (psl$644 > psl$1716) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1717;
        moonbit_incref(self$642);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$642, idx$645, _curr_entry$649
        );
        _tuple$1717
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1717)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1717->$0 = idx$645;
        _tuple$1717->$1 = psl$644;
        _bind$643 = _tuple$1717;
        break;
      } else {
        moonbit_decref(_curr_entry$649);
      }
      _tmp$1718 = psl$644 + 1;
      _tmp$1720 = idx$645 + 1;
      capacity_mask$1721 = self$642->$3;
      _tmp$1719 = _tmp$1720 & capacity_mask$1721;
      psl$644 = _tmp$1718;
      idx$645 = _tmp$1719;
      continue;
    }
    break;
  }
  _idx$653 = _bind$643->$0;
  _field$2262 = _bind$643->$1;
  moonbit_decref(_bind$643);
  _psl$654 = _field$2262;
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
  int32_t size$1697 = self$626->$1;
  int32_t grow_at$1698 = self$626->$4;
  int32_t capacity_mask$1710;
  int32_t _tmp$1709;
  struct $$3c$Int$2a$Int$3e$* _bind$627;
  int32_t psl$628;
  int32_t idx$629;
  int32_t _idx$637;
  int32_t _field$2268;
  int32_t _psl$638;
  int32_t _bind$639;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$640;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$641;
  if (size$1697 >= grow_at$1698) {
    moonbit_incref(self$626);
    $$moonbitlang$core$builtin$Map$$grow$2(self$626);
  }
  capacity_mask$1710 = self$626->$3;
  _tmp$1709 = hash$634 & capacity_mask$1710;
  psl$628 = 0;
  idx$629 = _tmp$1709;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2273 =
      self$626->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1708 =
      _field$2273;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2272;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$630;
    if (idx$629 < 0 || idx$629 >= Moonbit_array_length(entries$1708)) {
      moonbit_panic();
    }
    _tmp$2272
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1708[
        idx$629
      ];
    _bind$630 = _tmp$2272;
    if (_bind$630 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1699 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1699)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1699->$0 = idx$629;
      _tuple$1699->$1 = psl$628;
      _bind$627 = _tuple$1699;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$632 =
        _bind$630;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$633 =
        _Some$632;
      int32_t hash$1701 = _curr_entry$633->$3;
      int32_t _if_result$2581;
      int32_t psl$1702;
      int32_t _tmp$1704;
      int32_t _tmp$1706;
      int32_t capacity_mask$1707;
      int32_t _tmp$1705;
      if (hash$1701 == hash$634) {
        moonbit_string_t _field$2271 = _curr_entry$633->$4;
        moonbit_string_t key$1700 = _field$2271;
        int32_t _tmp$2270 = moonbit_val_array_equal(key$1700, key$635);
        _if_result$2581 = _tmp$2270;
      } else {
        _if_result$2581 = 0;
      }
      if (_if_result$2581) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2269;
        moonbit_incref(_curr_entry$633);
        moonbit_decref(key$635);
        moonbit_decref(self$626);
        _old$2269 = _curr_entry$633->$5;
        moonbit_decref(_old$2269);
        _curr_entry$633->$5 = value$636;
        moonbit_decref(_curr_entry$633);
        return 0;
      } else {
        moonbit_incref(_curr_entry$633);
      }
      psl$1702 = _curr_entry$633->$2;
      if (psl$628 > psl$1702) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1703;
        moonbit_incref(self$626);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$626, idx$629, _curr_entry$633
        );
        _tuple$1703
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1703)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1703->$0 = idx$629;
        _tuple$1703->$1 = psl$628;
        _bind$627 = _tuple$1703;
        break;
      } else {
        moonbit_decref(_curr_entry$633);
      }
      _tmp$1704 = psl$628 + 1;
      _tmp$1706 = idx$629 + 1;
      capacity_mask$1707 = self$626->$3;
      _tmp$1705 = _tmp$1706 & capacity_mask$1707;
      psl$628 = _tmp$1704;
      idx$629 = _tmp$1705;
      continue;
    }
    break;
  }
  _idx$637 = _bind$627->$0;
  _field$2268 = _bind$627->$1;
  moonbit_decref(_bind$627);
  _psl$638 = _field$2268;
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
  int32_t size$1683 = self$610->$1;
  int32_t grow_at$1684 = self$610->$4;
  int32_t capacity_mask$1696;
  int32_t _tmp$1695;
  struct $$3c$Int$2a$Int$3e$* _bind$611;
  int32_t psl$612;
  int32_t idx$613;
  int32_t _idx$621;
  int32_t _field$2274;
  int32_t _psl$622;
  int32_t _bind$623;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$624;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$625;
  if (size$1683 >= grow_at$1684) {
    moonbit_incref(self$610);
    $$moonbitlang$core$builtin$Map$$grow$1(self$610);
  }
  capacity_mask$1696 = self$610->$3;
  _tmp$1695 = hash$618 & capacity_mask$1696;
  psl$612 = 0;
  idx$613 = _tmp$1695;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2277 =
      self$610->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1694 =
      _field$2277;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2276;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$614;
    if (idx$613 < 0 || idx$613 >= Moonbit_array_length(entries$1694)) {
      moonbit_panic();
    }
    _tmp$2276
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1694[
        idx$613
      ];
    _bind$614 = _tmp$2276;
    if (_bind$614 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1685 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1685)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1685->$0 = idx$613;
      _tuple$1685->$1 = psl$612;
      _bind$611 = _tuple$1685;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$616 =
        _bind$614;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$617 =
        _Some$616;
      int32_t hash$1687 = _curr_entry$617->$3;
      int32_t _if_result$2583;
      int32_t psl$1688;
      int32_t _tmp$1690;
      int32_t _tmp$1692;
      int32_t capacity_mask$1693;
      int32_t _tmp$1691;
      if (hash$1687 == hash$618) {
        int32_t key$1686 = _curr_entry$617->$4;
        _if_result$2583 = key$1686 == key$619;
      } else {
        _if_result$2583 = 0;
      }
      if (_if_result$2583) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$2275;
        moonbit_incref(_curr_entry$617);
        moonbit_decref(self$610);
        _old$2275 = _curr_entry$617->$5;
        moonbit_decref(_old$2275);
        _curr_entry$617->$5 = value$620;
        moonbit_decref(_curr_entry$617);
        return 0;
      } else {
        moonbit_incref(_curr_entry$617);
      }
      psl$1688 = _curr_entry$617->$2;
      if (psl$612 > psl$1688) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1689;
        moonbit_incref(self$610);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$610, idx$613, _curr_entry$617
        );
        _tuple$1689
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1689)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1689->$0 = idx$613;
        _tuple$1689->$1 = psl$612;
        _bind$611 = _tuple$1689;
        break;
      } else {
        moonbit_decref(_curr_entry$617);
      }
      _tmp$1690 = psl$612 + 1;
      _tmp$1692 = idx$613 + 1;
      capacity_mask$1693 = self$610->$3;
      _tmp$1691 = _tmp$1692 & capacity_mask$1693;
      psl$612 = _tmp$1690;
      idx$613 = _tmp$1691;
      continue;
    }
    break;
  }
  _idx$621 = _bind$611->$0;
  _field$2274 = _bind$611->$1;
  moonbit_decref(_bind$611);
  _psl$622 = _field$2274;
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
  int32_t size$1669 = self$594->$1;
  int32_t grow_at$1670 = self$594->$4;
  int32_t capacity_mask$1682;
  int32_t _tmp$1681;
  struct $$3c$Int$2a$Int$3e$* _bind$595;
  int32_t psl$596;
  int32_t idx$597;
  int32_t _idx$605;
  int32_t _field$2278;
  int32_t _psl$606;
  int32_t _bind$607;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$608;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$609;
  if (size$1669 >= grow_at$1670) {
    moonbit_incref(self$594);
    $$moonbitlang$core$builtin$Map$$grow$0(self$594);
  }
  capacity_mask$1682 = self$594->$3;
  _tmp$1681 = hash$602 & capacity_mask$1682;
  psl$596 = 0;
  idx$597 = _tmp$1681;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2283 =
      self$594->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1680 =
      _field$2283;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2282;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$598;
    if (idx$597 < 0 || idx$597 >= Moonbit_array_length(entries$1680)) {
      moonbit_panic();
    }
    _tmp$2282
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1680[
        idx$597
      ];
    _bind$598 = _tmp$2282;
    if (_bind$598 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1671 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1671)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1671->$0 = idx$597;
      _tuple$1671->$1 = psl$596;
      _bind$595 = _tuple$1671;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$600 =
        _bind$598;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$601 =
        _Some$600;
      int32_t hash$1673 = _curr_entry$601->$3;
      int32_t _if_result$2585;
      int32_t psl$1674;
      int32_t _tmp$1676;
      int32_t _tmp$1678;
      int32_t capacity_mask$1679;
      int32_t _tmp$1677;
      if (hash$1673 == hash$602) {
        moonbit_string_t _field$2281 = _curr_entry$601->$4;
        moonbit_string_t key$1672 = _field$2281;
        int32_t _tmp$2280 = moonbit_val_array_equal(key$1672, key$603);
        _if_result$2585 = _tmp$2280;
      } else {
        _if_result$2585 = 0;
      }
      if (_if_result$2585) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2279;
        moonbit_incref(_curr_entry$601);
        moonbit_decref(key$603);
        moonbit_decref(self$594);
        _old$2279 = _curr_entry$601->$5;
        moonbit_decref(_old$2279);
        _curr_entry$601->$5 = value$604;
        moonbit_decref(_curr_entry$601);
        return 0;
      } else {
        moonbit_incref(_curr_entry$601);
      }
      psl$1674 = _curr_entry$601->$2;
      if (psl$596 > psl$1674) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1675;
        moonbit_incref(self$594);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$594, idx$597, _curr_entry$601
        );
        _tuple$1675
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1675)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1675->$0 = idx$597;
        _tuple$1675->$1 = psl$596;
        _bind$595 = _tuple$1675;
        break;
      } else {
        moonbit_decref(_curr_entry$601);
      }
      _tmp$1676 = psl$596 + 1;
      _tmp$1678 = idx$597 + 1;
      capacity_mask$1679 = self$594->$3;
      _tmp$1677 = _tmp$1678 & capacity_mask$1679;
      psl$596 = _tmp$1676;
      idx$597 = _tmp$1677;
      continue;
    }
    break;
  }
  _idx$605 = _bind$595->$0;
  _field$2278 = _bind$595->$1;
  moonbit_decref(_bind$595);
  _psl$606 = _field$2278;
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
  int32_t psl$1668 = entry$592->$2;
  int32_t _tmp$1664 = psl$1668 + 1;
  int32_t _tmp$1666 = idx$593 + 1;
  int32_t capacity_mask$1667 = self$588->$3;
  int32_t _tmp$1665 = _tmp$1666 & capacity_mask$1667;
  int32_t psl$584 = _tmp$1664;
  int32_t idx$585 = _tmp$1665;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$586 =
    entry$592;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2285 =
      self$588->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1663 =
      _field$2285;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2284;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$587;
    if (idx$585 < 0 || idx$585 >= Moonbit_array_length(entries$1663)) {
      moonbit_panic();
    }
    _tmp$2284
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1663[
        idx$585
      ];
    _bind$587 = _tmp$2284;
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
      int32_t psl$1653 = _curr_entry$591->$2;
      if (psl$584 > psl$1653) {
        int32_t psl$1658;
        int32_t _tmp$1654;
        int32_t _tmp$1656;
        int32_t capacity_mask$1657;
        int32_t _tmp$1655;
        entry$586->$2 = psl$584;
        moonbit_incref(_curr_entry$591);
        moonbit_incref(self$588);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$588, entry$586, idx$585
        );
        psl$1658 = _curr_entry$591->$2;
        _tmp$1654 = psl$1658 + 1;
        _tmp$1656 = idx$585 + 1;
        capacity_mask$1657 = self$588->$3;
        _tmp$1655 = _tmp$1656 & capacity_mask$1657;
        psl$584 = _tmp$1654;
        idx$585 = _tmp$1655;
        entry$586 = _curr_entry$591;
        continue;
      } else {
        int32_t _tmp$1659 = psl$584 + 1;
        int32_t _tmp$1661 = idx$585 + 1;
        int32_t capacity_mask$1662 = self$588->$3;
        int32_t _tmp$1660 = _tmp$1661 & capacity_mask$1662;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2587 =
          entry$586;
        psl$584 = _tmp$1659;
        idx$585 = _tmp$1660;
        entry$586 = _tmp$2587;
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
  int32_t psl$1652 = entry$582->$2;
  int32_t _tmp$1648 = psl$1652 + 1;
  int32_t _tmp$1650 = idx$583 + 1;
  int32_t capacity_mask$1651 = self$578->$3;
  int32_t _tmp$1649 = _tmp$1650 & capacity_mask$1651;
  int32_t psl$574 = _tmp$1648;
  int32_t idx$575 = _tmp$1649;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$576 =
    entry$582;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2287 =
      self$578->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1647 =
      _field$2287;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2286;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$577;
    if (idx$575 < 0 || idx$575 >= Moonbit_array_length(entries$1647)) {
      moonbit_panic();
    }
    _tmp$2286
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1647[
        idx$575
      ];
    _bind$577 = _tmp$2286;
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
      int32_t psl$1637 = _curr_entry$581->$2;
      if (psl$574 > psl$1637) {
        int32_t psl$1642;
        int32_t _tmp$1638;
        int32_t _tmp$1640;
        int32_t capacity_mask$1641;
        int32_t _tmp$1639;
        entry$576->$2 = psl$574;
        moonbit_incref(_curr_entry$581);
        moonbit_incref(self$578);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$578, entry$576, idx$575
        );
        psl$1642 = _curr_entry$581->$2;
        _tmp$1638 = psl$1642 + 1;
        _tmp$1640 = idx$575 + 1;
        capacity_mask$1641 = self$578->$3;
        _tmp$1639 = _tmp$1640 & capacity_mask$1641;
        psl$574 = _tmp$1638;
        idx$575 = _tmp$1639;
        entry$576 = _curr_entry$581;
        continue;
      } else {
        int32_t _tmp$1643 = psl$574 + 1;
        int32_t _tmp$1645 = idx$575 + 1;
        int32_t capacity_mask$1646 = self$578->$3;
        int32_t _tmp$1644 = _tmp$1645 & capacity_mask$1646;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2589 =
          entry$576;
        psl$574 = _tmp$1643;
        idx$575 = _tmp$1644;
        entry$576 = _tmp$2589;
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
  int32_t psl$1636 = entry$572->$2;
  int32_t _tmp$1632 = psl$1636 + 1;
  int32_t _tmp$1634 = idx$573 + 1;
  int32_t capacity_mask$1635 = self$568->$3;
  int32_t _tmp$1633 = _tmp$1634 & capacity_mask$1635;
  int32_t psl$564 = _tmp$1632;
  int32_t idx$565 = _tmp$1633;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$566 =
    entry$572;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2289 =
      self$568->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1631 =
      _field$2289;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2288;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$567;
    if (idx$565 < 0 || idx$565 >= Moonbit_array_length(entries$1631)) {
      moonbit_panic();
    }
    _tmp$2288
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1631[
        idx$565
      ];
    _bind$567 = _tmp$2288;
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
      int32_t psl$1621 = _curr_entry$571->$2;
      if (psl$564 > psl$1621) {
        int32_t psl$1626;
        int32_t _tmp$1622;
        int32_t _tmp$1624;
        int32_t capacity_mask$1625;
        int32_t _tmp$1623;
        entry$566->$2 = psl$564;
        moonbit_incref(_curr_entry$571);
        moonbit_incref(self$568);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$568, entry$566, idx$565
        );
        psl$1626 = _curr_entry$571->$2;
        _tmp$1622 = psl$1626 + 1;
        _tmp$1624 = idx$565 + 1;
        capacity_mask$1625 = self$568->$3;
        _tmp$1623 = _tmp$1624 & capacity_mask$1625;
        psl$564 = _tmp$1622;
        idx$565 = _tmp$1623;
        entry$566 = _curr_entry$571;
        continue;
      } else {
        int32_t _tmp$1627 = psl$564 + 1;
        int32_t _tmp$1629 = idx$565 + 1;
        int32_t capacity_mask$1630 = self$568->$3;
        int32_t _tmp$1628 = _tmp$1629 & capacity_mask$1630;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2591 =
          entry$566;
        psl$564 = _tmp$1627;
        idx$565 = _tmp$1628;
        entry$566 = _tmp$2591;
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
  int32_t psl$1620 = entry$562->$2;
  int32_t _tmp$1616 = psl$1620 + 1;
  int32_t _tmp$1618 = idx$563 + 1;
  int32_t capacity_mask$1619 = self$558->$3;
  int32_t _tmp$1617 = _tmp$1618 & capacity_mask$1619;
  int32_t psl$554 = _tmp$1616;
  int32_t idx$555 = _tmp$1617;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$556 =
    entry$562;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2291 =
      self$558->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1615 =
      _field$2291;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2290;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$557;
    if (idx$555 < 0 || idx$555 >= Moonbit_array_length(entries$1615)) {
      moonbit_panic();
    }
    _tmp$2290
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1615[
        idx$555
      ];
    _bind$557 = _tmp$2290;
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
      int32_t psl$1605 = _curr_entry$561->$2;
      if (psl$554 > psl$1605) {
        int32_t psl$1610;
        int32_t _tmp$1606;
        int32_t _tmp$1608;
        int32_t capacity_mask$1609;
        int32_t _tmp$1607;
        entry$556->$2 = psl$554;
        moonbit_incref(_curr_entry$561);
        moonbit_incref(self$558);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$558, entry$556, idx$555
        );
        psl$1610 = _curr_entry$561->$2;
        _tmp$1606 = psl$1610 + 1;
        _tmp$1608 = idx$555 + 1;
        capacity_mask$1609 = self$558->$3;
        _tmp$1607 = _tmp$1608 & capacity_mask$1609;
        psl$554 = _tmp$1606;
        idx$555 = _tmp$1607;
        entry$556 = _curr_entry$561;
        continue;
      } else {
        int32_t _tmp$1611 = psl$554 + 1;
        int32_t _tmp$1613 = idx$555 + 1;
        int32_t capacity_mask$1614 = self$558->$3;
        int32_t _tmp$1612 = _tmp$1613 & capacity_mask$1614;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2593 =
          entry$556;
        psl$554 = _tmp$1611;
        idx$555 = _tmp$1612;
        entry$556 = _tmp$2593;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2294 =
    self$548->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1603 =
    _field$2294;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1604;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2293;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2292;
  int32_t _cnt$2488;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$551;
  moonbit_incref(entry$550);
  _tmp$1604 = entry$550;
  if (new_idx$549 < 0 || new_idx$549 >= Moonbit_array_length(entries$1603)) {
    moonbit_panic();
  }
  _old$2293
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1603[
      new_idx$549
    ];
  if (_old$2293) {
    moonbit_decref(_old$2293);
  }
  entries$1603[new_idx$549] = _tmp$1604;
  _field$2292 = entry$550->$1;
  _cnt$2488 = Moonbit_object_header(entry$550)->rc;
  if (_cnt$2488 > 1) {
    int32_t _new_cnt$2491;
    if (_field$2292) {
      moonbit_incref(_field$2292);
    }
    _new_cnt$2491 = _cnt$2488 - 1;
    Moonbit_object_header(entry$550)->rc = _new_cnt$2491;
  } else if (_cnt$2488 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2490 =
      entry$550->$5;
    moonbit_string_t _field$2489;
    moonbit_decref(_field$2490);
    _field$2489 = entry$550->$4;
    moonbit_decref(_field$2489);
    moonbit_free(entry$550);
  }
  _bind$551 = _field$2292;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2297 =
    self$542->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1601 =
    _field$2297;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1602;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2296;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2295;
  int32_t _cnt$2492;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$545;
  moonbit_incref(entry$544);
  _tmp$1602 = entry$544;
  if (new_idx$543 < 0 || new_idx$543 >= Moonbit_array_length(entries$1601)) {
    moonbit_panic();
  }
  _old$2296
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1601[
      new_idx$543
    ];
  if (_old$2296) {
    moonbit_decref(_old$2296);
  }
  entries$1601[new_idx$543] = _tmp$1602;
  _field$2295 = entry$544->$1;
  _cnt$2492 = Moonbit_object_header(entry$544)->rc;
  if (_cnt$2492 > 1) {
    int32_t _new_cnt$2495;
    if (_field$2295) {
      moonbit_incref(_field$2295);
    }
    _new_cnt$2495 = _cnt$2492 - 1;
    Moonbit_object_header(entry$544)->rc = _new_cnt$2495;
  } else if (_cnt$2492 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2494 =
      entry$544->$5;
    moonbit_string_t _field$2493;
    moonbit_decref(_field$2494);
    _field$2493 = entry$544->$4;
    moonbit_decref(_field$2493);
    moonbit_free(entry$544);
  }
  _bind$545 = _field$2295;
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
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2300 =
    self$536->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1599 =
    _field$2300;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1600;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2299;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2298;
  int32_t _cnt$2496;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$539;
  moonbit_incref(entry$538);
  _tmp$1600 = entry$538;
  if (new_idx$537 < 0 || new_idx$537 >= Moonbit_array_length(entries$1599)) {
    moonbit_panic();
  }
  _old$2299
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1599[
      new_idx$537
    ];
  if (_old$2299) {
    moonbit_decref(_old$2299);
  }
  entries$1599[new_idx$537] = _tmp$1600;
  _field$2298 = entry$538->$1;
  _cnt$2496 = Moonbit_object_header(entry$538)->rc;
  if (_cnt$2496 > 1) {
    int32_t _new_cnt$2498;
    if (_field$2298) {
      moonbit_incref(_field$2298);
    }
    _new_cnt$2498 = _cnt$2496 - 1;
    Moonbit_object_header(entry$538)->rc = _new_cnt$2498;
  } else if (_cnt$2496 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2497 =
      entry$538->$5;
    moonbit_decref(_field$2497);
    moonbit_free(entry$538);
  }
  _bind$539 = _field$2298;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2303 =
    self$530->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1597 =
    _field$2303;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1598;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2302;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2301;
  int32_t _cnt$2499;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$533;
  moonbit_incref(entry$532);
  _tmp$1598 = entry$532;
  if (new_idx$531 < 0 || new_idx$531 >= Moonbit_array_length(entries$1597)) {
    moonbit_panic();
  }
  _old$2302
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1597[
      new_idx$531
    ];
  if (_old$2302) {
    moonbit_decref(_old$2302);
  }
  entries$1597[new_idx$531] = _tmp$1598;
  _field$2301 = entry$532->$1;
  _cnt$2499 = Moonbit_object_header(entry$532)->rc;
  if (_cnt$2499 > 1) {
    int32_t _new_cnt$2502;
    if (_field$2301) {
      moonbit_incref(_field$2301);
    }
    _new_cnt$2502 = _cnt$2499 - 1;
    Moonbit_object_header(entry$532)->rc = _new_cnt$2502;
  } else if (_cnt$2499 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2501 =
      entry$532->$5;
    moonbit_string_t _field$2500;
    moonbit_decref(_field$2501);
    _field$2500 = entry$532->$4;
    moonbit_decref(_field$2500);
    moonbit_free(entry$532);
  }
  _bind$533 = _field$2301;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2305;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1593;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1594;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2304;
  int32_t size$1596;
  int32_t _tmp$1595;
  switch (_bind$526) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1588;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2306;
      moonbit_incref(entry$528);
      _tmp$1588 = entry$528;
      _old$2306 = self$527->$5;
      if (_old$2306) {
        moonbit_decref(_old$2306);
      }
      self$527->$5 = _tmp$1588;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2309 =
        self$527->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1592 =
        _field$2309;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2308;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1591;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1589;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1590;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2307;
      if (_bind$526 < 0 || _bind$526 >= Moonbit_array_length(entries$1592)) {
        moonbit_panic();
      }
      _tmp$2308
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1592[
          _bind$526
        ];
      _tmp$1591 = _tmp$2308;
      if (_tmp$1591) {
        moonbit_incref(_tmp$1591);
      }
      _tmp$1589 = $Option$$unwrap$3(_tmp$1591);
      moonbit_incref(entry$528);
      _tmp$1590 = entry$528;
      _old$2307 = _tmp$1589->$1;
      if (_old$2307) {
        moonbit_decref(_old$2307);
      }
      _tmp$1589->$1 = _tmp$1590;
      moonbit_decref(_tmp$1589);
      break;
    }
  }
  self$527->$6 = idx$529;
  _field$2305 = self$527->$0;
  entries$1593 = _field$2305;
  _tmp$1594 = entry$528;
  if (idx$529 < 0 || idx$529 >= Moonbit_array_length(entries$1593)) {
    moonbit_panic();
  }
  _old$2304
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1593[
      idx$529
    ];
  if (_old$2304) {
    moonbit_decref(_old$2304);
  }
  entries$1593[idx$529] = _tmp$1594;
  size$1596 = self$527->$1;
  _tmp$1595 = size$1596 + 1;
  self$527->$1 = _tmp$1595;
  moonbit_decref(self$527);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$523,
  int32_t idx$525,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$524
) {
  int32_t _bind$522 = self$523->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2311;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1584;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1585;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2310;
  int32_t size$1587;
  int32_t _tmp$1586;
  switch (_bind$522) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1579;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2312;
      moonbit_incref(entry$524);
      _tmp$1579 = entry$524;
      _old$2312 = self$523->$5;
      if (_old$2312) {
        moonbit_decref(_old$2312);
      }
      self$523->$5 = _tmp$1579;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2315 =
        self$523->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1583 =
        _field$2315;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2314;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1582;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1580;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1581;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2313;
      if (_bind$522 < 0 || _bind$522 >= Moonbit_array_length(entries$1583)) {
        moonbit_panic();
      }
      _tmp$2314
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1583[
          _bind$522
        ];
      _tmp$1582 = _tmp$2314;
      if (_tmp$1582) {
        moonbit_incref(_tmp$1582);
      }
      _tmp$1580 = $Option$$unwrap$2(_tmp$1582);
      moonbit_incref(entry$524);
      _tmp$1581 = entry$524;
      _old$2313 = _tmp$1580->$1;
      if (_old$2313) {
        moonbit_decref(_old$2313);
      }
      _tmp$1580->$1 = _tmp$1581;
      moonbit_decref(_tmp$1580);
      break;
    }
  }
  self$523->$6 = idx$525;
  _field$2311 = self$523->$0;
  entries$1584 = _field$2311;
  _tmp$1585 = entry$524;
  if (idx$525 < 0 || idx$525 >= Moonbit_array_length(entries$1584)) {
    moonbit_panic();
  }
  _old$2310
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1584[
      idx$525
    ];
  if (_old$2310) {
    moonbit_decref(_old$2310);
  }
  entries$1584[idx$525] = _tmp$1585;
  size$1587 = self$523->$1;
  _tmp$1586 = size$1587 + 1;
  self$523->$1 = _tmp$1586;
  moonbit_decref(self$523);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$519,
  int32_t idx$521,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$520
) {
  int32_t _bind$518 = self$519->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2317;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1575;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1576;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2316;
  int32_t size$1578;
  int32_t _tmp$1577;
  switch (_bind$518) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1570;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2318;
      moonbit_incref(entry$520);
      _tmp$1570 = entry$520;
      _old$2318 = self$519->$5;
      if (_old$2318) {
        moonbit_decref(_old$2318);
      }
      self$519->$5 = _tmp$1570;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2321 =
        self$519->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1574 =
        _field$2321;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2320;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1573;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1571;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1572;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2319;
      if (_bind$518 < 0 || _bind$518 >= Moonbit_array_length(entries$1574)) {
        moonbit_panic();
      }
      _tmp$2320
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1574[
          _bind$518
        ];
      _tmp$1573 = _tmp$2320;
      if (_tmp$1573) {
        moonbit_incref(_tmp$1573);
      }
      _tmp$1571 = $Option$$unwrap$1(_tmp$1573);
      moonbit_incref(entry$520);
      _tmp$1572 = entry$520;
      _old$2319 = _tmp$1571->$1;
      if (_old$2319) {
        moonbit_decref(_old$2319);
      }
      _tmp$1571->$1 = _tmp$1572;
      moonbit_decref(_tmp$1571);
      break;
    }
  }
  self$519->$6 = idx$521;
  _field$2317 = self$519->$0;
  entries$1575 = _field$2317;
  _tmp$1576 = entry$520;
  if (idx$521 < 0 || idx$521 >= Moonbit_array_length(entries$1575)) {
    moonbit_panic();
  }
  _old$2316
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1575[
      idx$521
    ];
  if (_old$2316) {
    moonbit_decref(_old$2316);
  }
  entries$1575[idx$521] = _tmp$1576;
  size$1578 = self$519->$1;
  _tmp$1577 = size$1578 + 1;
  self$519->$1 = _tmp$1577;
  moonbit_decref(self$519);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$515,
  int32_t idx$517,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$516
) {
  int32_t _bind$514 = self$515->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2323;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1566;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1567;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2322;
  int32_t size$1569;
  int32_t _tmp$1568;
  switch (_bind$514) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1561;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2324;
      moonbit_incref(entry$516);
      _tmp$1561 = entry$516;
      _old$2324 = self$515->$5;
      if (_old$2324) {
        moonbit_decref(_old$2324);
      }
      self$515->$5 = _tmp$1561;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2327 =
        self$515->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1565 =
        _field$2327;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2326;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1564;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1562;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1563;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2325;
      if (_bind$514 < 0 || _bind$514 >= Moonbit_array_length(entries$1565)) {
        moonbit_panic();
      }
      _tmp$2326
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1565[
          _bind$514
        ];
      _tmp$1564 = _tmp$2326;
      if (_tmp$1564) {
        moonbit_incref(_tmp$1564);
      }
      _tmp$1562 = $Option$$unwrap$0(_tmp$1564);
      moonbit_incref(entry$516);
      _tmp$1563 = entry$516;
      _old$2325 = _tmp$1562->$1;
      if (_old$2325) {
        moonbit_decref(_old$2325);
      }
      _tmp$1562->$1 = _tmp$1563;
      moonbit_decref(_tmp$1562);
      break;
    }
  }
  self$515->$6 = idx$517;
  _field$2323 = self$515->$0;
  entries$1566 = _field$2323;
  _tmp$1567 = entry$516;
  if (idx$517 < 0 || idx$517 >= Moonbit_array_length(entries$1566)) {
    moonbit_panic();
  }
  _old$2322
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1566[
      idx$517
    ];
  if (_old$2322) {
    moonbit_decref(_old$2322);
  }
  entries$1566[idx$517] = _tmp$1567;
  size$1569 = self$515->$1;
  _tmp$1568 = size$1569 + 1;
  self$515->$1 = _tmp$1568;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1560 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$512 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$508, _tmp$1560
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$513 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2594 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2594)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2594->$0 = _bind$512;
  _block$2594->$1 = 0;
  _block$2594->$2 = capacity$508;
  _block$2594->$3 = _bind$510;
  _block$2594->$4 = _bind$511;
  _block$2594->$5 = _bind$513;
  _block$2594->$6 = -1;
  return _block$2594;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$503
) {
  int32_t capacity$502 = $Int$$next_power_of_two(capacity$503);
  int32_t _bind$504 = capacity$502 - 1;
  int32_t _bind$505 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$502);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1559 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$506 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$502, _tmp$1559
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$507 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2595 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2595)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2595->$0 = _bind$506;
  _block$2595->$1 = 0;
  _block$2595->$2 = capacity$502;
  _block$2595->$3 = _bind$504;
  _block$2595->$4 = _bind$505;
  _block$2595->$5 = _bind$507;
  _block$2595->$6 = -1;
  return _block$2595;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$497
) {
  int32_t capacity$496 = $Int$$next_power_of_two(capacity$497);
  int32_t _bind$498 = capacity$496 - 1;
  int32_t _bind$499 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$496);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1558 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$500 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$496, _tmp$1558
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$501 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$2596 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2596)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2596->$0 = _bind$500;
  _block$2596->$1 = 0;
  _block$2596->$2 = capacity$496;
  _block$2596->$3 = _bind$498;
  _block$2596->$4 = _bind$499;
  _block$2596->$5 = _bind$501;
  _block$2596->$6 = -1;
  return _block$2596;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$491
) {
  int32_t capacity$490 = $Int$$next_power_of_two(capacity$491);
  int32_t _bind$492 = capacity$490 - 1;
  int32_t _bind$493 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$490);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1557 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$494 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$490, _tmp$1557
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$495 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2597 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2597)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2597->$0 = _bind$494;
  _block$2597->$1 = 0;
  _block$2597->$2 = capacity$490;
  _block$2597->$3 = _bind$492;
  _block$2597->$4 = _bind$493;
  _block$2597->$5 = _bind$495;
  _block$2597->$6 = -1;
  return _block$2597;
}

int32_t $Int$$next_power_of_two(int32_t self$489) {
  if (self$489 >= 0) {
    int32_t _tmp$1556;
    int32_t _tmp$1555;
    int32_t _tmp$1554;
    int32_t _tmp$1553;
    if (self$489 <= 1) {
      return 1;
    }
    if (self$489 > 1073741824) {
      return 1073741824;
    }
    _tmp$1556 = self$489 - 1;
    _tmp$1555 = moonbit_clz32(_tmp$1556);
    _tmp$1554 = _tmp$1555 - 1;
    _tmp$1553 = 2147483647 >> (_tmp$1554 & 31);
    return _tmp$1553 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$488) {
  int32_t _tmp$1552 = capacity$488 * 13;
  return _tmp$1552 / 16;
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1551 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$479);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$1551);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$478
) {
  moonbit_string_t* _field$2329 = self$478->$0;
  moonbit_string_t* buf$1549 = _field$2329;
  int32_t _field$2328 = self$478->$1;
  int32_t _cnt$2503 = Moonbit_object_header(self$478)->rc;
  int32_t len$1550;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$1548;
  if (_cnt$2503 > 1) {
    int32_t _new_cnt$2504;
    moonbit_incref(buf$1549);
    _new_cnt$2504 = _cnt$2503 - 1;
    Moonbit_object_header(self$478)->rc = _new_cnt$2504;
  } else if (_cnt$2503 == 1) {
    moonbit_free(self$478);
  }
  len$1550 = _field$2328;
  _tmp$1548
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$1550, buf$1549
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$1548);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$476
) {
  struct $Ref$3c$Int$3e$* i$475 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$2598;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1537;
  Moonbit_object_header(i$475)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$475->$0 = 0;
  _closure$2598
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$2598)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$2598->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$2598->$0_0 = self$476.$0;
  _closure$2598->$0_1 = self$476.$1;
  _closure$2598->$0_2 = self$476.$2;
  _closure$2598->$1 = i$475;
  _tmp$1537 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$2598;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$1537);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1538
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$1539 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$1538;
  struct $Ref$3c$Int$3e$* _field$2334 = _casted_env$1539->$1;
  struct $Ref$3c$Int$3e$* i$475 = _field$2334;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$2333 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$1539->$0_1, _casted_env$1539->$0_2, _casted_env$1539->$0_0
    };
  int32_t _cnt$2505 = Moonbit_object_header(_casted_env$1539)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$476;
  int32_t val$1540;
  int32_t _tmp$1541;
  if (_cnt$2505 > 1) {
    int32_t _new_cnt$2506;
    moonbit_incref(i$475);
    moonbit_incref(_field$2333.$0);
    _new_cnt$2506 = _cnt$2505 - 1;
    Moonbit_object_header(_casted_env$1539)->rc = _new_cnt$2506;
  } else if (_cnt$2505 == 1) {
    moonbit_free(_casted_env$1539);
  }
  self$476 = _field$2333;
  val$1540 = i$475->$0;
  moonbit_incref(self$476.$0);
  _tmp$1541 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$476);
  if (val$1540 < _tmp$1541) {
    moonbit_string_t* _field$2332 = self$476.$0;
    moonbit_string_t* buf$1544 = _field$2332;
    int32_t _field$2331 = self$476.$1;
    int32_t start$1546 = _field$2331;
    int32_t val$1547 = i$475->$0;
    int32_t _tmp$1545 = start$1546 + val$1547;
    moonbit_string_t _tmp$2330 = (moonbit_string_t)buf$1544[_tmp$1545];
    moonbit_string_t elem$477;
    int32_t val$1543;
    int32_t _tmp$1542;
    moonbit_incref(_tmp$2330);
    moonbit_decref(buf$1544);
    elem$477 = _tmp$2330;
    val$1543 = i$475->$0;
    _tmp$1542 = val$1543 + 1;
    i$475->$0 = _tmp$1542;
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
  moonbit_string_t _tmp$1536 = $Int$$to_string$inner(self$473, 10);
  logger$472.$0->$method_0(logger$472.$1, _tmp$1536);
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
  int32_t len$1531 = self$466->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1533;
  int32_t _tmp$2337;
  int32_t _tmp$1532;
  int32_t length$467;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2336;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$1534;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2335;
  int32_t _tmp$1535;
  moonbit_incref(self$466);
  _tmp$1533 = $$moonbitlang$core$builtin$Array$$buffer$2(self$466);
  _tmp$2337 = Moonbit_array_length(_tmp$1533);
  moonbit_decref(_tmp$1533);
  _tmp$1532 = _tmp$2337;
  if (len$1531 == _tmp$1532) {
    moonbit_incref(self$466);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$466);
  }
  length$467 = self$466->$1;
  _field$2336 = self$466->$0;
  buf$1534 = _field$2336;
  _old$2335
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$1534[
      length$467
    ];
  if (_old$2335) {
    moonbit_decref(_old$2335);
  }
  buf$1534[length$467] = value$468;
  _tmp$1535 = length$467 + 1;
  self$466->$1 = _tmp$1535;
  moonbit_decref(self$466);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$463,
  struct $$3c$String$2a$Int$3e$* value$465
) {
  int32_t len$1526 = self$463->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$1528;
  int32_t _tmp$2340;
  int32_t _tmp$1527;
  int32_t length$464;
  struct $$3c$String$2a$Int$3e$** _field$2339;
  struct $$3c$String$2a$Int$3e$** buf$1529;
  struct $$3c$String$2a$Int$3e$* _old$2338;
  int32_t _tmp$1530;
  moonbit_incref(self$463);
  _tmp$1528 = $$moonbitlang$core$builtin$Array$$buffer$0(self$463);
  _tmp$2340 = Moonbit_array_length(_tmp$1528);
  moonbit_decref(_tmp$1528);
  _tmp$1527 = _tmp$2340;
  if (len$1526 == _tmp$1527) {
    moonbit_incref(self$463);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$463);
  }
  length$464 = self$463->$1;
  _field$2339 = self$463->$0;
  buf$1529 = _field$2339;
  _old$2338 = (struct $$3c$String$2a$Int$3e$*)buf$1529[length$464];
  if (_old$2338) {
    moonbit_decref(_old$2338);
  }
  buf$1529[length$464] = value$465;
  _tmp$1530 = length$464 + 1;
  self$463->$1 = _tmp$1530;
  moonbit_decref(self$463);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$460,
  moonbit_string_t value$462
) {
  int32_t len$1521 = self$460->$1;
  moonbit_string_t* _tmp$1523;
  int32_t _tmp$2343;
  int32_t _tmp$1522;
  int32_t length$461;
  moonbit_string_t* _field$2342;
  moonbit_string_t* buf$1524;
  moonbit_string_t _old$2341;
  int32_t _tmp$1525;
  moonbit_incref(self$460);
  _tmp$1523 = $$moonbitlang$core$builtin$Array$$buffer$1(self$460);
  _tmp$2343 = Moonbit_array_length(_tmp$1523);
  moonbit_decref(_tmp$1523);
  _tmp$1522 = _tmp$2343;
  if (len$1521 == _tmp$1522) {
    moonbit_incref(self$460);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$460);
  }
  length$461 = self$460->$1;
  _field$2342 = self$460->$0;
  buf$1524 = _field$2342;
  _old$2341 = (moonbit_string_t)buf$1524[length$461];
  moonbit_decref(_old$2341);
  buf$1524[length$461] = value$462;
  _tmp$1525 = length$461 + 1;
  self$460->$1 = _tmp$1525;
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
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2345 =
    self$448->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$447 =
    _field$2345;
  int32_t old_cap$449 = Moonbit_array_length(old_buf$447);
  int32_t copy_len$450;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$2344;
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
  _old$2344 = self$448->$0;
  moonbit_decref(_old$2344);
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
  struct $$3c$String$2a$Int$3e$** _field$2347 = self$442->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$441 = _field$2347;
  int32_t old_cap$443 = Moonbit_array_length(old_buf$441);
  int32_t copy_len$444;
  struct $$3c$String$2a$Int$3e$** _old$2346;
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
  _old$2346 = self$442->$0;
  moonbit_decref(_old$2346);
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
  moonbit_string_t* _field$2349 = self$436->$0;
  moonbit_string_t* old_buf$435 = _field$2349;
  int32_t old_cap$437 = Moonbit_array_length(old_buf$435);
  int32_t copy_len$438;
  moonbit_string_t* _old$2348;
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
  _old$2348 = self$436->$0;
  moonbit_decref(_old$2348);
  self$436->$0 = new_buf$433;
  moonbit_decref(self$436);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$432
) {
  if (capacity$432 == 0) {
    moonbit_string_t* _tmp$1519 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2599 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2599)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2599->$0 = _tmp$1519;
    _block$2599->$1 = 0;
    return _block$2599;
  } else {
    moonbit_string_t* _tmp$1520 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$432, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2600 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2600)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2600->$0 = _tmp$1520;
    _block$2600->$1 = 0;
    return _block$2600;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$430,
  struct $StringView str$431
) {
  int32_t len$1507 = self$430->$1;
  int32_t _tmp$1509;
  int32_t _tmp$1508;
  int32_t _tmp$1506;
  moonbit_bytes_t _field$2350;
  moonbit_bytes_t data$1510;
  int32_t len$1511;
  moonbit_string_t _tmp$1512;
  int32_t _tmp$1513;
  int32_t _tmp$1514;
  int32_t len$1516;
  int32_t _tmp$1518;
  int32_t _tmp$1517;
  int32_t _tmp$1515;
  moonbit_incref(str$431.$0);
  _tmp$1509 = $StringView$$length(str$431);
  _tmp$1508 = _tmp$1509 * 2;
  _tmp$1506 = len$1507 + _tmp$1508;
  moonbit_incref(self$430);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$430, _tmp$1506
  );
  _field$2350 = self$430->$0;
  data$1510 = _field$2350;
  len$1511 = self$430->$1;
  moonbit_incref(data$1510);
  moonbit_incref(str$431.$0);
  _tmp$1512 = $StringView$$data(str$431);
  moonbit_incref(str$431.$0);
  _tmp$1513 = $StringView$$start_offset(str$431);
  moonbit_incref(str$431.$0);
  _tmp$1514 = $StringView$$length(str$431);
  $FixedArray$$blit_from_string(
    data$1510, len$1511, _tmp$1512, _tmp$1513, _tmp$1514
  );
  len$1516 = self$430->$1;
  _tmp$1518 = $StringView$$length(str$431);
  _tmp$1517 = _tmp$1518 * 2;
  _tmp$1515 = len$1516 + _tmp$1517;
  self$430->$1 = _tmp$1515;
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
    int32_t _tmp$1505 = -i$428;
    return $String$$offset_of_nth_char_backward(
             self$427, _tmp$1505, start_offset$429, end_offset$424
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$422,
  int32_t n$420,
  int32_t start_offset$416,
  int32_t end_offset$417
) {
  int32_t _if_result$2601;
  if (start_offset$416 >= 0) {
    _if_result$2601 = start_offset$416 <= end_offset$417;
  } else {
    _if_result$2601 = 0;
  }
  if (_if_result$2601) {
    int32_t utf16_offset$418 = start_offset$416;
    int32_t char_count$419 = 0;
    int32_t _tmp$1503;
    int32_t _if_result$2604;
    while (1) {
      int32_t _tmp$1497 = utf16_offset$418;
      int32_t _if_result$2603;
      if (_tmp$1497 < end_offset$417) {
        int32_t _tmp$1496 = char_count$419;
        _if_result$2603 = _tmp$1496 < n$420;
      } else {
        _if_result$2603 = 0;
      }
      if (_if_result$2603) {
        int32_t _tmp$1501 = utf16_offset$418;
        int32_t c$421 = self$422[_tmp$1501];
        int32_t _tmp$1500;
        if ($Int$$is_leading_surrogate(c$421)) {
          int32_t _tmp$1498 = utf16_offset$418;
          utf16_offset$418 = _tmp$1498 + 2;
        } else {
          int32_t _tmp$1499 = utf16_offset$418;
          utf16_offset$418 = _tmp$1499 + 1;
        }
        _tmp$1500 = char_count$419;
        char_count$419 = _tmp$1500 + 1;
        continue;
      } else {
        moonbit_decref(self$422);
      }
      break;
    }
    _tmp$1503 = char_count$419;
    if (_tmp$1503 < n$420) {
      _if_result$2604 = 1;
    } else {
      int32_t _tmp$1502 = utf16_offset$418;
      _if_result$2604 = _tmp$1502 >= end_offset$417;
    }
    if (_if_result$2604) {
      return 4294967296ll;
    } else {
      int32_t _tmp$1504 = utf16_offset$418;
      return (int64_t)_tmp$1504;
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
  int32_t _tmp$1494;
  int32_t _if_result$2607;
  while (1) {
    int32_t _tmp$1487 = utf16_offset$409;
    int32_t _tmp$1486 = _tmp$1487 - 1;
    int32_t _if_result$2606;
    if (_tmp$1486 >= start_offset$411) {
      int32_t _tmp$1485 = char_count$408;
      _if_result$2606 = _tmp$1485 < n$412;
    } else {
      _if_result$2606 = 0;
    }
    if (_if_result$2606) {
      int32_t _tmp$1492 = utf16_offset$409;
      int32_t _tmp$1491 = _tmp$1492 - 1;
      int32_t c$413 = self$414[_tmp$1491];
      int32_t _tmp$1490;
      if ($Int$$is_trailing_surrogate(c$413)) {
        int32_t _tmp$1488 = utf16_offset$409;
        utf16_offset$409 = _tmp$1488 - 2;
      } else {
        int32_t _tmp$1489 = utf16_offset$409;
        utf16_offset$409 = _tmp$1489 - 1;
      }
      _tmp$1490 = char_count$408;
      char_count$408 = _tmp$1490 + 1;
      continue;
    } else {
      moonbit_decref(self$414);
    }
    break;
  }
  _tmp$1494 = char_count$408;
  if (_tmp$1494 < n$412) {
    _if_result$2607 = 1;
  } else {
    int32_t _tmp$1493 = utf16_offset$409;
    _if_result$2607 = _tmp$1493 < start_offset$411;
  }
  if (_if_result$2607) {
    return 4294967296ll;
  } else {
    int32_t _tmp$1495 = utf16_offset$409;
    return (int64_t)_tmp$1495;
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
    int32_t _if_result$2609;
    if (index$401 < end_offset$397) {
      _if_result$2609 = count$402 < len$403;
    } else {
      _if_result$2609 = 0;
    }
    if (_if_result$2609) {
      int32_t c1$404 = self$400[index$401];
      int32_t _if_result$2610;
      int32_t _tmp$1483;
      int32_t _tmp$1484;
      if ($Int$$is_leading_surrogate(c1$404)) {
        int32_t _tmp$1479 = index$401 + 1;
        _if_result$2610 = _tmp$1479 < end_offset$397;
      } else {
        _if_result$2610 = 0;
      }
      if (_if_result$2610) {
        int32_t _tmp$1482 = index$401 + 1;
        int32_t c2$405 = self$400[_tmp$1482];
        if ($Int$$is_trailing_surrogate(c2$405)) {
          int32_t _tmp$1480 = index$401 + 2;
          int32_t _tmp$1481 = count$402 + 1;
          index$401 = _tmp$1480;
          count$402 = _tmp$1481;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_16.data,
              (moonbit_string_t)moonbit_string_literal_17.data
          );
        }
      }
      _tmp$1483 = index$401 + 1;
      _tmp$1484 = count$402 + 1;
      index$401 = _tmp$1483;
      count$402 = _tmp$1484;
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
  int32_t end$1477 = self$396.$2;
  int32_t _field$2351 = self$396.$1;
  int32_t start$1478;
  moonbit_decref(self$396.$0);
  start$1478 = _field$2351;
  return end$1477 - start$1478;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$395
) {
  int32_t end$1475 = self$395.$2;
  int32_t _field$2352 = self$395.$1;
  int32_t start$1476;
  moonbit_decref(self$395.$0);
  start$1476 = _field$2352;
  return end$1475 - start$1476;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$394
) {
  int32_t end$1473 = self$394.$2;
  int32_t _field$2353 = self$394.$1;
  int32_t start$1474;
  moonbit_decref(self$394.$0);
  start$1474 = _field$2353;
  return end$1473 - start$1474;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$393
) {
  int32_t end$1471 = self$393.$2;
  int32_t _field$2354 = self$393.$1;
  int32_t start$1472;
  moonbit_decref(self$393.$0);
  start$1472 = _field$2354;
  return end$1471 - start$1472;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$392
) {
  int32_t end$1469 = self$392.$2;
  int32_t _field$2355 = self$392.$1;
  int32_t start$1470;
  moonbit_decref(self$392.$0);
  start$1470 = _field$2355;
  return end$1469 - start$1470;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$2611 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$2611)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2611->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$2611->$0 = self$387;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$2611;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1467,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$385
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$1468 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$1467;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$2356 =
    _casted_env$1468->$0;
  int32_t _cnt$2507 = Moonbit_object_header(_casted_env$1468)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387;
  if (_cnt$2507 > 1) {
    int32_t _new_cnt$2508;
    moonbit_incref(_field$2356);
    _new_cnt$2508 = _cnt$2507 - 1;
    Moonbit_object_header(_casted_env$1468)->rc = _new_cnt$2508;
  } else if (_cnt$2507 == 1) {
    moonbit_free(_casted_env$1468);
  }
  self$387 = _field$2356;
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
    struct $$moonbitlang$core$builtin$Logger _bind$1449;
    int32_t _tmp$1450;
    int32_t _tmp$1451;
    int32_t _tmp$1452;
    int32_t _tmp$2616;
    int32_t _tmp$2617;
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
        int32_t _tmp$1453;
        int32_t _tmp$1454;
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
        _tmp$1453 = i$377 + 1;
        _tmp$1454 = i$377 + 1;
        i$377 = _tmp$1453;
        seg$378 = _tmp$1454;
        goto $$2a$for$379;
        break;
      }
      
      case 13: {
        int32_t _tmp$1455;
        int32_t _tmp$1456;
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
        _tmp$1455 = i$377 + 1;
        _tmp$1456 = i$377 + 1;
        i$377 = _tmp$1455;
        seg$378 = _tmp$1456;
        goto $$2a$for$379;
        break;
      }
      
      case 8: {
        int32_t _tmp$1457;
        int32_t _tmp$1458;
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
        _tmp$1457 = i$377 + 1;
        _tmp$1458 = i$377 + 1;
        i$377 = _tmp$1457;
        seg$378 = _tmp$1458;
        goto $$2a$for$379;
        break;
      }
      
      case 9: {
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
          logger$373.$1, (moonbit_string_t)moonbit_string_literal_21.data
        );
        _tmp$1459 = i$377 + 1;
        _tmp$1460 = i$377 + 1;
        i$377 = _tmp$1459;
        seg$378 = _tmp$1460;
        goto $$2a$for$379;
        break;
      }
      default: {
        if (code$380 < 32) {
          int32_t _tmp$1463;
          moonbit_string_t _tmp$1462;
          struct $$moonbitlang$core$builtin$Logger _bind$1461;
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
            logger$373.$1, (moonbit_string_t)moonbit_string_literal_22.data
          );
          _tmp$1463 = code$380 & 0xff;
          _tmp$1462 = $Byte$$to_hex(_tmp$1463);
          if (logger$373.$1) {
            moonbit_incref(logger$373.$1);
          }
          logger$373.$0->$method_0(logger$373.$1, _tmp$1462);
          _bind$1461 = logger$373;
          if (_bind$1461.$1) {
            moonbit_incref(_bind$1461.$1);
          }
          _bind$1461.$0->$method_3(_bind$1461.$1, 125);
          _tmp$1464 = i$377 + 1;
          _tmp$1465 = i$377 + 1;
          i$377 = _tmp$1464;
          seg$378 = _tmp$1465;
          goto $$2a$for$379;
        } else {
          int32_t _tmp$1466 = i$377 + 1;
          int32_t _tmp$2615 = seg$378;
          i$377 = _tmp$1466;
          seg$378 = _tmp$2615;
          goto $$2a$for$379;
        }
        break;
      }
    }
    goto $joinlet$2614;
    $join$381:;
    moonbit_incref(_env$374);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$374, seg$378, i$377
    );
    if (logger$373.$1) {
      moonbit_incref(logger$373.$1);
    }
    logger$373.$0->$method_3(logger$373.$1, 92);
    _bind$1449 = logger$373;
    _tmp$1450 = c$382;
    if (_bind$1449.$1) {
      moonbit_incref(_bind$1449.$1);
    }
    _bind$1449.$0->$method_3(_bind$1449.$1, _tmp$1450);
    _tmp$1451 = i$377 + 1;
    _tmp$1452 = i$377 + 1;
    i$377 = _tmp$1451;
    seg$378 = _tmp$1452;
    continue;
    $joinlet$2614:;
    _tmp$2616 = i$377;
    _tmp$2617 = seg$378;
    i$377 = _tmp$2616;
    seg$378 = _tmp$2617;
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
  moonbit_string_t _field$2358 = _env$369->$1;
  moonbit_string_t self$368 = _field$2358;
  struct $$moonbitlang$core$builtin$Logger _field$2357 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$369->$0_0, _env$369->$0_1
    };
  int32_t _cnt$2509 = Moonbit_object_header(_env$369)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$370;
  if (_cnt$2509 > 1) {
    int32_t _new_cnt$2510;
    moonbit_incref(self$368);
    if (_field$2357.$1) {
      moonbit_incref(_field$2357.$1);
    }
    _new_cnt$2510 = _cnt$2509 - 1;
    Moonbit_object_header(_env$369)->rc = _new_cnt$2510;
  } else if (_cnt$2509 == 1) {
    moonbit_free(_env$369);
  }
  logger$370 = _field$2357;
  if (i$371 > seg$372) {
    int32_t _tmp$1448 = i$371 - seg$372;
    logger$370.$0->$method_1(logger$370.$1, self$368, seg$372, _tmp$1448);
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
  int32_t _tmp$1445 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$367, 16);
  int32_t _tmp$1444 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$1445);
  int32_t _tmp$1447;
  int32_t _tmp$1446;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$1443;
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1444
  );
  _tmp$1447 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$367, 16);
  _tmp$1446
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$1447
  );
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1446
  );
  _tmp$1443 = _self$366;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$1443);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$365) {
  if (i$365 < 10) {
    int32_t _tmp$1440 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 48);
    return $Byte$$to_char(_tmp$1440);
  } else {
    int32_t _tmp$1442 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 97);
    int32_t _tmp$1441 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$1442, 10);
    return $Byte$$to_char(_tmp$1441);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$363,
  int32_t that$364
) {
  int32_t _tmp$1438 = (int32_t)self$363;
  int32_t _tmp$1439 = (int32_t)that$364;
  int32_t _tmp$1437 = _tmp$1438 - _tmp$1439;
  return _tmp$1437 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$361,
  int32_t that$362
) {
  int32_t _tmp$1435 = (int32_t)self$361;
  int32_t _tmp$1436 = (int32_t)that$362;
  int32_t _tmp$1434 = _tmp$1435 % _tmp$1436;
  return _tmp$1434 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$359,
  int32_t that$360
) {
  int32_t _tmp$1432 = (int32_t)self$359;
  int32_t _tmp$1433 = (int32_t)that$360;
  int32_t _tmp$1431 = _tmp$1432 / _tmp$1433;
  return _tmp$1431 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$357,
  int32_t that$358
) {
  int32_t _tmp$1429 = (int32_t)self$357;
  int32_t _tmp$1430 = (int32_t)that$358;
  int32_t _tmp$1428 = _tmp$1429 + _tmp$1430;
  return _tmp$1428 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$354,
  int32_t start$352,
  int32_t end$353
) {
  int32_t _if_result$2618;
  int32_t len$355;
  int32_t _tmp$1426;
  int32_t _tmp$1427;
  moonbit_bytes_t bytes$356;
  moonbit_bytes_t _tmp$1425;
  if (start$352 == 0) {
    int32_t _tmp$1424 = Moonbit_array_length(str$354);
    _if_result$2618 = end$353 == _tmp$1424;
  } else {
    _if_result$2618 = 0;
  }
  if (_if_result$2618) {
    return str$354;
  }
  len$355 = end$353 - start$352;
  _tmp$1426 = len$355 * 2;
  _tmp$1427 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$356 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1426, _tmp$1427);
  moonbit_incref(bytes$356);
  $FixedArray$$blit_from_string(bytes$356, 0, str$354, start$352, len$355);
  _tmp$1425 = bytes$356;
  return $Bytes$$to_unchecked_string$inner(_tmp$1425, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$351
) {
  return f$351;
}

moonbit_string_t $Int$$to_string$inner(int32_t self$335, int32_t radix$334) {
  int32_t _if_result$2619;
  int32_t is_negative$336;
  uint32_t num$337;
  uint16_t* buffer$338;
  if (radix$334 < 2) {
    _if_result$2619 = 1;
  } else {
    _if_result$2619 = radix$334 > 36;
  }
  if (_if_result$2619) {
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
    int32_t _tmp$1423 = -self$335;
    num$337 = *(uint32_t*)&_tmp$1423;
  } else {
    num$337 = *(uint32_t*)&self$335;
  }
  switch (radix$334) {
    case 10: {
      int32_t digit_len$339 = $moonbitlang$core$builtin$dec_count32(num$337);
      int32_t _tmp$1420;
      int32_t total_len$340;
      uint16_t* buffer$341;
      int32_t digit_start$342;
      if (is_negative$336) {
        _tmp$1420 = 1;
      } else {
        _tmp$1420 = 0;
      }
      total_len$340 = digit_len$339 + _tmp$1420;
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
      int32_t _tmp$1421;
      int32_t total_len$344;
      uint16_t* buffer$345;
      int32_t digit_start$346;
      if (is_negative$336) {
        _tmp$1421 = 1;
      } else {
        _tmp$1421 = 0;
      }
      total_len$344 = digit_len$343 + _tmp$1421;
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
      int32_t _tmp$1422;
      int32_t total_len$348;
      uint16_t* buffer$349;
      int32_t digit_start$350;
      if (is_negative$336) {
        _tmp$1422 = 1;
      } else {
        _tmp$1422 = 0;
      }
      total_len$348 = digit_len$347 + _tmp$1422;
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
    uint32_t _tmp$1417 = num$329;
    if (_tmp$1417 > 0u) {
      int32_t _tmp$1418 = count$332;
      uint32_t _tmp$1419;
      count$332 = _tmp$1418 + 1;
      _tmp$1419 = num$329;
      num$329 = _tmp$1419 / base$330;
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
    int32_t _tmp$1416 = 31 - leading_zeros$327;
    int32_t _tmp$1415 = _tmp$1416 / 4;
    return _tmp$1415 + 1;
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
  uint32_t _tmp$1414;
  int32_t remaining$317;
  int32_t _tmp$1395;
  while (1) {
    uint32_t _tmp$1358 = num$302;
    if (_tmp$1358 >= 10000u) {
      uint32_t _tmp$1381 = num$302;
      uint32_t t$307 = _tmp$1381 / 10000u;
      uint32_t _tmp$1380 = num$302;
      uint32_t _tmp$1379 = _tmp$1380 % 10000u;
      int32_t r$308 = *(int32_t*)&_tmp$1379;
      int32_t d1$309;
      int32_t d2$310;
      int32_t _tmp$1359;
      int32_t _tmp$1378;
      int32_t _tmp$1377;
      int32_t d1_hi$311;
      int32_t _tmp$1376;
      int32_t _tmp$1375;
      int32_t d1_lo$312;
      int32_t _tmp$1374;
      int32_t _tmp$1373;
      int32_t d2_hi$313;
      int32_t _tmp$1372;
      int32_t _tmp$1371;
      int32_t d2_lo$314;
      int32_t _tmp$1361;
      int32_t _tmp$1360;
      int32_t _tmp$1364;
      int32_t _tmp$1363;
      int32_t _tmp$1362;
      int32_t _tmp$1367;
      int32_t _tmp$1366;
      int32_t _tmp$1365;
      int32_t _tmp$1370;
      int32_t _tmp$1369;
      int32_t _tmp$1368;
      num$302 = t$307;
      d1$309 = r$308 / 100;
      d2$310 = r$308 % 100;
      _tmp$1359 = offset$304;
      offset$304 = _tmp$1359 - 4;
      _tmp$1378 = d1$309 / 10;
      _tmp$1377 = 48 + _tmp$1378;
      d1_hi$311 = (uint16_t)_tmp$1377;
      _tmp$1376 = d1$309 % 10;
      _tmp$1375 = 48 + _tmp$1376;
      d1_lo$312 = (uint16_t)_tmp$1375;
      _tmp$1374 = d2$310 / 10;
      _tmp$1373 = 48 + _tmp$1374;
      d2_hi$313 = (uint16_t)_tmp$1373;
      _tmp$1372 = d2$310 % 10;
      _tmp$1371 = 48 + _tmp$1372;
      d2_lo$314 = (uint16_t)_tmp$1371;
      _tmp$1361 = offset$304;
      _tmp$1360 = digit_start$306 + _tmp$1361;
      buffer$315[_tmp$1360] = d1_hi$311;
      _tmp$1364 = offset$304;
      _tmp$1363 = digit_start$306 + _tmp$1364;
      _tmp$1362 = _tmp$1363 + 1;
      buffer$315[_tmp$1362] = d1_lo$312;
      _tmp$1367 = offset$304;
      _tmp$1366 = digit_start$306 + _tmp$1367;
      _tmp$1365 = _tmp$1366 + 2;
      buffer$315[_tmp$1365] = d2_hi$313;
      _tmp$1370 = offset$304;
      _tmp$1369 = digit_start$306 + _tmp$1370;
      _tmp$1368 = _tmp$1369 + 3;
      buffer$315[_tmp$1368] = d2_lo$314;
      continue;
    }
    break;
  }
  _tmp$1414 = num$302;
  remaining$317 = *(int32_t*)&_tmp$1414;
  while (1) {
    int32_t _tmp$1382 = remaining$317;
    if (_tmp$1382 >= 100) {
      int32_t _tmp$1394 = remaining$317;
      int32_t t$318 = _tmp$1394 / 100;
      int32_t _tmp$1393 = remaining$317;
      int32_t d$319 = _tmp$1393 % 100;
      int32_t _tmp$1383;
      int32_t _tmp$1392;
      int32_t _tmp$1391;
      int32_t d_hi$320;
      int32_t _tmp$1390;
      int32_t _tmp$1389;
      int32_t d_lo$321;
      int32_t _tmp$1385;
      int32_t _tmp$1384;
      int32_t _tmp$1388;
      int32_t _tmp$1387;
      int32_t _tmp$1386;
      remaining$317 = t$318;
      _tmp$1383 = offset$304;
      offset$304 = _tmp$1383 - 2;
      _tmp$1392 = d$319 / 10;
      _tmp$1391 = 48 + _tmp$1392;
      d_hi$320 = (uint16_t)_tmp$1391;
      _tmp$1390 = d$319 % 10;
      _tmp$1389 = 48 + _tmp$1390;
      d_lo$321 = (uint16_t)_tmp$1389;
      _tmp$1385 = offset$304;
      _tmp$1384 = digit_start$306 + _tmp$1385;
      buffer$315[_tmp$1384] = d_hi$320;
      _tmp$1388 = offset$304;
      _tmp$1387 = digit_start$306 + _tmp$1388;
      _tmp$1386 = _tmp$1387 + 1;
      buffer$315[_tmp$1386] = d_lo$321;
      continue;
    }
    break;
  }
  _tmp$1395 = remaining$317;
  if (_tmp$1395 >= 10) {
    int32_t _tmp$1396 = offset$304;
    int32_t _tmp$1407;
    int32_t _tmp$1406;
    int32_t _tmp$1405;
    int32_t d_hi$323;
    int32_t _tmp$1404;
    int32_t _tmp$1403;
    int32_t _tmp$1402;
    int32_t d_lo$324;
    int32_t _tmp$1398;
    int32_t _tmp$1397;
    int32_t _tmp$1401;
    int32_t _tmp$1400;
    int32_t _tmp$1399;
    offset$304 = _tmp$1396 - 2;
    _tmp$1407 = remaining$317;
    _tmp$1406 = _tmp$1407 / 10;
    _tmp$1405 = 48 + _tmp$1406;
    d_hi$323 = (uint16_t)_tmp$1405;
    _tmp$1404 = remaining$317;
    _tmp$1403 = _tmp$1404 % 10;
    _tmp$1402 = 48 + _tmp$1403;
    d_lo$324 = (uint16_t)_tmp$1402;
    _tmp$1398 = offset$304;
    _tmp$1397 = digit_start$306 + _tmp$1398;
    buffer$315[_tmp$1397] = d_hi$323;
    _tmp$1401 = offset$304;
    _tmp$1400 = digit_start$306 + _tmp$1401;
    _tmp$1399 = _tmp$1400 + 1;
    buffer$315[_tmp$1399] = d_lo$324;
    moonbit_decref(buffer$315);
  } else {
    int32_t _tmp$1408 = offset$304;
    int32_t _tmp$1413;
    int32_t _tmp$1409;
    int32_t _tmp$1412;
    int32_t _tmp$1411;
    int32_t _tmp$1410;
    offset$304 = _tmp$1408 - 1;
    _tmp$1413 = offset$304;
    _tmp$1409 = digit_start$306 + _tmp$1413;
    _tmp$1412 = remaining$317;
    _tmp$1411 = 48 + _tmp$1412;
    _tmp$1410 = (uint16_t)_tmp$1411;
    buffer$315[_tmp$1409] = _tmp$1410;
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
  int32_t _tmp$1338 = radix$293 - 1;
  int32_t _tmp$1337 = radix$293 & _tmp$1338;
  if (_tmp$1337 == 0) {
    int32_t shift$294 = moonbit_ctz32(radix$293);
    uint32_t mask$295 = base$292 - 1u;
    while (1) {
      uint32_t _tmp$1339 = n$290;
      if (_tmp$1339 > 0u) {
        int32_t _tmp$1340 = offset$287;
        uint32_t _tmp$1347;
        uint32_t _tmp$1346;
        int32_t digit$296;
        int32_t _tmp$1344;
        int32_t _tmp$1341;
        int32_t _tmp$1343;
        int32_t _tmp$1342;
        uint32_t _tmp$1345;
        offset$287 = _tmp$1340 - 1;
        _tmp$1347 = n$290;
        _tmp$1346 = _tmp$1347 & mask$295;
        digit$296 = *(int32_t*)&_tmp$1346;
        _tmp$1344 = offset$287;
        _tmp$1341 = digit_start$289 + _tmp$1344;
        _tmp$1343
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$296
        ];
        _tmp$1342 = (uint16_t)_tmp$1343;
        buffer$297[_tmp$1341] = _tmp$1342;
        _tmp$1345 = n$290;
        n$290 = _tmp$1345 >> (shift$294 & 31);
        continue;
      } else {
        moonbit_decref(buffer$297);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1348 = n$290;
      if (_tmp$1348 > 0u) {
        int32_t _tmp$1349 = offset$287;
        uint32_t _tmp$1357;
        uint32_t q$299;
        uint32_t _tmp$1355;
        uint32_t _tmp$1356;
        uint32_t _tmp$1354;
        int32_t digit$300;
        int32_t _tmp$1353;
        int32_t _tmp$1350;
        int32_t _tmp$1352;
        int32_t _tmp$1351;
        offset$287 = _tmp$1349 - 1;
        _tmp$1357 = n$290;
        q$299 = _tmp$1357 / base$292;
        _tmp$1355 = n$290;
        _tmp$1356 = q$299 * base$292;
        _tmp$1354 = _tmp$1355 - _tmp$1356;
        digit$300 = *(int32_t*)&_tmp$1354;
        _tmp$1353 = offset$287;
        _tmp$1350 = digit_start$289 + _tmp$1353;
        _tmp$1352
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$300
        ];
        _tmp$1351 = (uint16_t)_tmp$1352;
        buffer$297[_tmp$1350] = _tmp$1351;
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
  int32_t _tmp$1332;
  while (1) {
    int32_t _tmp$1318 = offset$276;
    if (_tmp$1318 >= 2) {
      int32_t _tmp$1319 = offset$276;
      uint32_t _tmp$1331;
      uint32_t _tmp$1330;
      int32_t byte_val$281;
      int32_t hi$282;
      int32_t lo$283;
      int32_t _tmp$1323;
      int32_t _tmp$1320;
      int32_t _tmp$1322;
      int32_t _tmp$1321;
      int32_t _tmp$1328;
      int32_t _tmp$1327;
      int32_t _tmp$1324;
      int32_t _tmp$1326;
      int32_t _tmp$1325;
      uint32_t _tmp$1329;
      offset$276 = _tmp$1319 - 2;
      _tmp$1331 = n$279;
      _tmp$1330 = _tmp$1331 & 255u;
      byte_val$281 = *(int32_t*)&_tmp$1330;
      hi$282 = byte_val$281 / 16;
      lo$283 = byte_val$281 % 16;
      _tmp$1323 = offset$276;
      _tmp$1320 = digit_start$278 + _tmp$1323;
      _tmp$1322 = ((moonbit_string_t)moonbit_string_literal_26.data)[hi$282];
      _tmp$1321 = (uint16_t)_tmp$1322;
      buffer$284[_tmp$1320] = _tmp$1321;
      _tmp$1328 = offset$276;
      _tmp$1327 = digit_start$278 + _tmp$1328;
      _tmp$1324 = _tmp$1327 + 1;
      _tmp$1326 = ((moonbit_string_t)moonbit_string_literal_26.data)[lo$283];
      _tmp$1325 = (uint16_t)_tmp$1326;
      buffer$284[_tmp$1324] = _tmp$1325;
      _tmp$1329 = n$279;
      n$279 = _tmp$1329 >> 8;
      continue;
    }
    break;
  }
  _tmp$1332 = offset$276;
  if (_tmp$1332 == 1) {
    uint32_t _tmp$1336 = n$279;
    uint32_t _tmp$1335 = _tmp$1336 & 15u;
    int32_t nibble$286 = *(int32_t*)&_tmp$1335;
    int32_t _tmp$1334 =
      ((moonbit_string_t)moonbit_string_literal_26.data)[nibble$286];
    int32_t _tmp$1333 = (uint16_t)_tmp$1334;
    buffer$284[digit_start$278] = _tmp$1333;
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
  struct $$moonbitlang$core$builtin$Logger _tmp$1317;
  moonbit_incref(logger$274);
  _tmp$1317
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$274
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$275, _tmp$1317
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$274);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$273
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$272 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1316;
  moonbit_incref(logger$272);
  _tmp$1316
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$272
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$273, _tmp$1316
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$272);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$271
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$270 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1315;
  moonbit_incref(logger$270);
  _tmp$1315
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$270
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$271, _tmp$1315
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$270);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$269
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$268 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1314;
  moonbit_incref(logger$268);
  _tmp$1314
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$268
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$269, _tmp$1314);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$268);
}

int32_t $StringView$$start_offset(struct $StringView self$267) {
  int32_t _field$2359 = self$267.$1;
  moonbit_decref(self$267.$0);
  return _field$2359;
}

moonbit_string_t $StringView$$data(struct $StringView self$266) {
  moonbit_string_t _field$2360 = self$266.$0;
  return _field$2360;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$260,
  moonbit_string_t value$263,
  int32_t start$264,
  int32_t len$265
) {
  void* _try_err$262;
  struct $StringView _tmp$1309;
  int32_t _tmp$1311 = start$264 + len$265;
  int64_t _tmp$1310 = (int64_t)_tmp$1311;
  struct moonbit_result_1 _tmp$2627 =
    $String$$sub$inner(value$263, start$264, _tmp$1310);
  if (_tmp$2627.tag) {
    struct $StringView const _ok$1312 = _tmp$2627.data.ok;
    _tmp$1309 = _ok$1312;
  } else {
    void* const _err$1313 = _tmp$2627.data.err;
    _try_err$262 = _err$1313;
    goto $join$261;
  }
  goto $joinlet$2626;
  $join$261:;
  moonbit_decref(_try_err$262);
  moonbit_panic();
  $joinlet$2626:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$260, _tmp$1309
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
  int32_t _if_result$2628;
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
      _if_result$2628 = end$254 <= len$252;
    } else {
      _if_result$2628 = 0;
    }
  } else {
    _if_result$2628 = 0;
  }
  if (_if_result$2628) {
    int32_t _if_result$2629;
    int32_t _if_result$2631;
    struct $StringView _tmp$1307;
    struct moonbit_result_1 _result$2633;
    if (start$258 < len$252) {
      int32_t _tmp$1303 = self$253[start$258];
      _if_result$2629 = $Int$$is_trailing_surrogate(_tmp$1303);
    } else {
      _if_result$2629 = 0;
    }
    if (_if_result$2629) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1304;
      struct moonbit_result_1 _result$2630;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1304
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2630.tag = 0;
      _result$2630.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1304;
      return _result$2630;
    }
    if (end$254 < len$252) {
      int32_t _tmp$1305 = self$253[end$254];
      _if_result$2631 = $Int$$is_trailing_surrogate(_tmp$1305);
    } else {
      _if_result$2631 = 0;
    }
    if (_if_result$2631) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1306;
      struct moonbit_result_1 _result$2632;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1306
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2632.tag = 0;
      _result$2632.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1306;
      return _result$2632;
    }
    _tmp$1307 = (struct $StringView){start$258, end$254, self$253};
    _result$2633.tag = 1;
    _result$2633.data.ok = _tmp$1307;
    return _result$2633;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1308;
    struct moonbit_result_1 _result$2634;
    moonbit_decref(self$253);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1308
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$2634.tag = 0;
    _result$2634.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1308;
    return _result$2634;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$251
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$250 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1302;
  moonbit_incref(_self$250);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$250, self$251);
  _tmp$1302 = _self$250;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1302);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$249
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$248 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1301;
  moonbit_incref(_self$248);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$248, self$249);
  _tmp$1301 = _self$248;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1301);
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
  uint32_t _tmp$1300 = *(uint32_t*)&seed$244;
  uint32_t _tmp$1299 = _tmp$1300 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$2635 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$2635)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$2635->$0 = _tmp$1299;
  return _block$2635;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$243
) {
  uint32_t _tmp$1298 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$243);
  return *(int32_t*)&_tmp$1298;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$242
) {
  uint32_t _field$2361 = self$242->$0;
  uint32_t acc$241;
  uint32_t _tmp$1287;
  uint32_t _tmp$1289;
  uint32_t _tmp$1288;
  uint32_t _tmp$1290;
  uint32_t _tmp$1291;
  uint32_t _tmp$1293;
  uint32_t _tmp$1292;
  uint32_t _tmp$1294;
  uint32_t _tmp$1295;
  uint32_t _tmp$1297;
  uint32_t _tmp$1296;
  moonbit_decref(self$242);
  acc$241 = _field$2361;
  _tmp$1287 = acc$241;
  _tmp$1289 = acc$241;
  _tmp$1288 = _tmp$1289 >> 15;
  acc$241 = _tmp$1287 ^ _tmp$1288;
  _tmp$1290 = acc$241;
  acc$241 = _tmp$1290 * 2246822519u;
  _tmp$1291 = acc$241;
  _tmp$1293 = acc$241;
  _tmp$1292 = _tmp$1293 >> 13;
  acc$241 = _tmp$1291 ^ _tmp$1292;
  _tmp$1294 = acc$241;
  acc$241 = _tmp$1294 * 3266489917u;
  _tmp$1295 = acc$241;
  _tmp$1297 = acc$241;
  _tmp$1296 = _tmp$1297 >> 16;
  acc$241 = _tmp$1295 ^ _tmp$1296;
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
  uint32_t _tmp$1286 = *(uint32_t*)&value$236;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$235, _tmp$1286);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$233,
  uint32_t value$234
) {
  uint32_t acc$1285 = self$233->$0;
  uint32_t _tmp$1284 = acc$1285 + 4u;
  self$233->$0 = _tmp$1284;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$233, value$234);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$231,
  uint32_t input$232
) {
  uint32_t acc$1282 = self$231->$0;
  uint32_t _tmp$1283 = input$232 * 3266489917u;
  uint32_t _tmp$1281 = acc$1282 + _tmp$1283;
  uint32_t _tmp$1280 = $moonbitlang$core$builtin$rotl(_tmp$1281, 17);
  uint32_t _tmp$1279 = _tmp$1280 * 668265263u;
  self$231->$0 = _tmp$1279;
  moonbit_decref(self$231);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$229, int32_t r$230) {
  uint32_t _tmp$1276 = x$229 << (r$230 & 31);
  int32_t _tmp$1278 = 32 - r$230;
  uint32_t _tmp$1277 = x$229 >> (_tmp$1278 & 31);
  return _tmp$1276 | _tmp$1277;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$227,
  moonbit_string_t str$228
) {
  int32_t len$1266 = self$227->$1;
  int32_t _tmp$1268 = Moonbit_array_length(str$228);
  int32_t _tmp$1267 = _tmp$1268 * 2;
  int32_t _tmp$1265 = len$1266 + _tmp$1267;
  moonbit_bytes_t _field$2363;
  moonbit_bytes_t data$1269;
  int32_t len$1270;
  int32_t _tmp$1271;
  int32_t len$1273;
  int32_t _tmp$2362;
  int32_t _tmp$1275;
  int32_t _tmp$1274;
  int32_t _tmp$1272;
  moonbit_incref(self$227);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$227, _tmp$1265
  );
  _field$2363 = self$227->$0;
  data$1269 = _field$2363;
  len$1270 = self$227->$1;
  _tmp$1271 = Moonbit_array_length(str$228);
  moonbit_incref(data$1269);
  moonbit_incref(str$228);
  $FixedArray$$blit_from_string(data$1269, len$1270, str$228, 0, _tmp$1271);
  len$1273 = self$227->$1;
  _tmp$2362 = Moonbit_array_length(str$228);
  moonbit_decref(str$228);
  _tmp$1275 = _tmp$2362;
  _tmp$1274 = _tmp$1275 * 2;
  _tmp$1272 = len$1273 + _tmp$1274;
  self$227->$1 = _tmp$1272;
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
  int32_t _tmp$1264 = length$215 * 2;
  int32_t _tmp$1263 = bytes_offset$214 + _tmp$1264;
  int32_t e1$213 = _tmp$1263 - 1;
  int32_t _tmp$1262 = str_offset$217 + length$215;
  int32_t e2$216 = _tmp$1262 - 1;
  int32_t len1$218 = Moonbit_array_length(self$219);
  int32_t len2$220 = Moonbit_array_length(str$221);
  int32_t _if_result$2636;
  if (length$215 >= 0) {
    if (bytes_offset$214 >= 0) {
      if (e1$213 < len1$218) {
        if (str_offset$217 >= 0) {
          _if_result$2636 = e2$216 < len2$220;
        } else {
          _if_result$2636 = 0;
        }
      } else {
        _if_result$2636 = 0;
      }
    } else {
      _if_result$2636 = 0;
    }
  } else {
    _if_result$2636 = 0;
  }
  if (_if_result$2636) {
    int32_t end_str_offset$222 = str_offset$217 + length$215;
    int32_t i$223 = str_offset$217;
    int32_t j$224 = bytes_offset$214;
    while (1) {
      if (i$223 < end_str_offset$222) {
        int32_t _tmp$1259 = str$221[i$223];
        uint32_t c$225 = *(uint32_t*)&_tmp$1259;
        uint32_t _tmp$1255 = c$225 & 255u;
        int32_t _tmp$1254 = $UInt$$to_byte(_tmp$1255);
        int32_t _tmp$1256;
        uint32_t _tmp$1258;
        int32_t _tmp$1257;
        int32_t _tmp$1260;
        int32_t _tmp$1261;
        if (j$224 < 0 || j$224 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[j$224] = _tmp$1254;
        _tmp$1256 = j$224 + 1;
        _tmp$1258 = c$225 >> 8;
        _tmp$1257 = $UInt$$to_byte(_tmp$1258);
        if (_tmp$1256 < 0 || _tmp$1256 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[_tmp$1256] = _tmp$1257;
        _tmp$1260 = i$223 + 1;
        _tmp$1261 = j$224 + 2;
        i$223 = _tmp$1260;
        j$224 = _tmp$1261;
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
  int32_t _tmp$1227 = Moonbit_array_length(repr$181);
  int64_t _tmp$1226 = (int64_t)_tmp$1227;
  moonbit_incref(repr$181);
  if ($String$$char_length_ge$inner(repr$181, 1, 0, _tmp$1226)) {
    int32_t _tmp$1253 = repr$181[0];
    int32_t _x$182 = _tmp$1253;
    if (_x$182 == 64) {
      int32_t _tmp$1252 = Moonbit_array_length(repr$181);
      int64_t _tmp$1251 = (int64_t)_tmp$1252;
      int64_t _bind$1014;
      int32_t _tmp$1249;
      int32_t _tmp$1250;
      struct $StringView _x$183;
      int32_t _tmp$1248;
      struct $StringView _tmp$1247;
      int64_t _bind$185;
      moonbit_incref(repr$181);
      _bind$1014
      = $String$$offset_of_nth_char$inner(
        repr$181, 1, 0, _tmp$1251
      );
      if (_bind$1014 == 4294967296ll) {
        _tmp$1249 = Moonbit_array_length(repr$181);
      } else {
        int64_t _Some$184 = _bind$1014;
        _tmp$1249 = (int32_t)_Some$184;
      }
      _tmp$1250 = Moonbit_array_length(repr$181);
      _x$183 = (struct $StringView){_tmp$1249, _tmp$1250, repr$181};
      _tmp$1248
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1247
      = (struct $StringView){
        0, _tmp$1248, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$183.$0);
      _bind$185 = $StringView$$find(_x$183, _tmp$1247);
      if (_bind$185 == 4294967296ll) {
        moonbit_decref(_x$183.$0);
        moonbit_panic();
      } else {
        int64_t _Some$186 = _bind$185;
        int32_t _pkg_end$187 = (int32_t)_Some$186;
        int64_t _tmp$1246 = (int64_t)_pkg_end$187;
        struct $StringView pkg$188;
        int32_t _tmp$1245;
        struct $StringView _tmp$1244;
        int64_t _bind$189;
        moonbit_incref(_x$183.$0);
        pkg$188 = $StringView$$view$inner(_x$183, 0, _tmp$1246);
        _tmp$1245
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1244
        = (struct $StringView){
          0, _tmp$1245, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$183.$0);
        _bind$189 = $StringView$$rev_find(_x$183, _tmp$1244);
        if (_bind$189 == 4294967296ll) {
          moonbit_decref(pkg$188.$0);
          moonbit_decref(_x$183.$0);
          moonbit_panic();
        } else {
          int64_t _Some$190 = _bind$189;
          int32_t _start_loc_end$191 = (int32_t)_Some$190;
          int32_t _tmp$1228 = _start_loc_end$191 + 1;
          int32_t _tmp$1229;
          moonbit_incref(_x$183.$0);
          _tmp$1229 = $StringView$$length(_x$183);
          if (_tmp$1228 < _tmp$1229) {
            int32_t _tmp$1243 = _start_loc_end$191 + 1;
            struct $StringView end_loc$192;
            struct $$3c$StringView$2a$StringView$3e$* _bind$193;
            moonbit_incref(_x$183.$0);
            end_loc$192
            = $StringView$$view$inner(
              _x$183, _tmp$1243, 4294967296ll
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
              struct $StringView _field$2367 =
                (struct $StringView){
                  _x$195->$0_1, _x$195->$0_2, _x$195->$0_0
                };
              struct $StringView _end_line$196 = _field$2367;
              struct $StringView _field$2366 =
                (struct $StringView){
                  _x$195->$1_1, _x$195->$1_2, _x$195->$1_0
                };
              int32_t _cnt$2511 = Moonbit_object_header(_x$195)->rc;
              struct $StringView _end_column$197;
              int64_t _tmp$1242;
              struct $StringView rest$198;
              int32_t _tmp$1241;
              struct $StringView _tmp$1240;
              int64_t _bind$200;
              if (_cnt$2511 > 1) {
                int32_t _new_cnt$2512;
                moonbit_incref(_field$2366.$0);
                moonbit_incref(_end_line$196.$0);
                _new_cnt$2512 = _cnt$2511 - 1;
                Moonbit_object_header(_x$195)->rc = _new_cnt$2512;
              } else if (_cnt$2511 == 1) {
                moonbit_free(_x$195);
              }
              _end_column$197 = _field$2366;
              _tmp$1242 = (int64_t)_start_loc_end$191;
              rest$198 = $StringView$$view$inner(_x$183, 0, _tmp$1242);
              _tmp$1241
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1240
              = (struct $StringView){
                0,
                  _tmp$1241,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$198.$0);
              _bind$200 = $StringView$$rev_find(rest$198, _tmp$1240);
              if (_bind$200 == 4294967296ll) {
                moonbit_decref(rest$198.$0);
                moonbit_decref(_end_column$197.$0);
                moonbit_decref(_end_line$196.$0);
                moonbit_decref(pkg$188.$0);
                goto $join$199;
              } else {
                int64_t _Some$201 = _bind$200;
                int32_t _start_line_end$202 = (int32_t)_Some$201;
                int64_t _tmp$1239 = (int64_t)_start_line_end$202;
                struct $StringView _tmp$1236;
                int32_t _tmp$1238;
                struct $StringView _tmp$1237;
                int64_t _bind$203;
                moonbit_incref(rest$198.$0);
                _tmp$1236 = $StringView$$view$inner(rest$198, 0, _tmp$1239);
                _tmp$1238
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1237
                = (struct $StringView){
                  0,
                    _tmp$1238,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$203 = $StringView$$rev_find(_tmp$1236, _tmp$1237);
                if (_bind$203 == 4294967296ll) {
                  moonbit_decref(rest$198.$0);
                  moonbit_decref(_end_column$197.$0);
                  moonbit_decref(_end_line$196.$0);
                  moonbit_decref(pkg$188.$0);
                  goto $join$199;
                } else {
                  int64_t _Some$204 = _bind$203;
                  int32_t _filename_end$205 = (int32_t)_Some$204;
                  int32_t _tmp$1230 = _filename_end$205 + 1;
                  int32_t _tmp$1231;
                  moonbit_incref(rest$198.$0);
                  _tmp$1231 = $StringView$$length(rest$198);
                  if (_tmp$1230 < _tmp$1231) {
                    int32_t _tmp$1235 = _filename_end$205 + 1;
                    struct $StringView start_loc$206;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$207;
                    moonbit_incref(rest$198.$0);
                    start_loc$206
                    = $StringView$$view$inner(
                      rest$198, _tmp$1235, 4294967296ll
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
                      struct $StringView _field$2365 =
                        (struct $StringView){
                          _x$209->$0_1, _x$209->$0_2, _x$209->$0_0
                        };
                      struct $StringView _start_line$210 = _field$2365;
                      struct $StringView _field$2364 =
                        (struct $StringView){
                          _x$209->$1_1, _x$209->$1_2, _x$209->$1_0
                        };
                      int32_t _cnt$2513 = Moonbit_object_header(_x$209)->rc;
                      struct $StringView _start_column$211;
                      int32_t _tmp$1232;
                      if (_cnt$2513 > 1) {
                        int32_t _new_cnt$2514;
                        moonbit_incref(_field$2364.$0);
                        moonbit_incref(_start_line$210.$0);
                        _new_cnt$2514 = _cnt$2513 - 1;
                        Moonbit_object_header(_x$209)->rc = _new_cnt$2514;
                      } else if (_cnt$2513 == 1) {
                        moonbit_free(_x$209);
                      }
                      _start_column$211 = _field$2364;
                      _tmp$1232 = _pkg_end$187 + 1;
                      if (_filename_end$205 > _tmp$1232) {
                        int32_t _tmp$1233 = _pkg_end$187 + 1;
                        int64_t _tmp$1234 = (int64_t)_filename_end$205;
                        struct $StringView filename$212 =
                          $StringView$$view$inner(
                            rest$198, _tmp$1233, _tmp$1234
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$2640 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$2640)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$2640->$0_0 = pkg$188.$0;
                        _block$2640->$0_1 = pkg$188.$1;
                        _block$2640->$0_2 = pkg$188.$2;
                        _block$2640->$1_0 = filename$212.$0;
                        _block$2640->$1_1 = filename$212.$1;
                        _block$2640->$1_2 = filename$212.$2;
                        _block$2640->$2_0 = _start_line$210.$0;
                        _block$2640->$2_1 = _start_line$210.$1;
                        _block$2640->$2_2 = _start_line$210.$2;
                        _block$2640->$3_0 = _start_column$211.$0;
                        _block$2640->$3_1 = _start_column$211.$1;
                        _block$2640->$3_2 = _start_column$211.$2;
                        _block$2640->$4_0 = _end_line$196.$0;
                        _block$2640->$4_1 = _end_line$196.$1;
                        _block$2640->$4_2 = _end_line$196.$2;
                        _block$2640->$5_0 = _end_column$197.$0;
                        _block$2640->$5_1 = _end_column$197.$1;
                        _block$2640->$5_2 = _end_column$197.$2;
                        return _block$2640;
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
  int32_t _tmp$1225 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1224;
  int64_t _bind$176;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1224
  = (struct $StringView){
    0, _tmp$1225, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$177.$0);
  _bind$176 = $StringView$$find(view$177, _tmp$1224);
  if (_bind$176 == 4294967296ll) {
    moonbit_decref(view$177.$0);
    return 0;
  } else {
    int64_t _Some$178 = _bind$176;
    int32_t _i$179 = (int32_t)_Some$178;
    int32_t _if_result$2641;
    if (_i$179 > 0) {
      int32_t _tmp$1217 = _i$179 + 1;
      int32_t _tmp$1218;
      moonbit_incref(view$177.$0);
      _tmp$1218 = $StringView$$length(view$177);
      _if_result$2641 = _tmp$1217 < _tmp$1218;
    } else {
      _if_result$2641 = 0;
    }
    if (_if_result$2641) {
      int64_t _tmp$1223 = (int64_t)_i$179;
      struct $StringView _tmp$1220;
      int32_t _tmp$1222;
      struct $StringView _tmp$1221;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1219;
      moonbit_incref(view$177.$0);
      _tmp$1220 = $StringView$$view$inner(view$177, 0, _tmp$1223);
      _tmp$1222 = _i$179 + 1;
      _tmp$1221 = $StringView$$view$inner(view$177, _tmp$1222, 4294967296ll);
      _tuple$1219
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1219)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1219->$0_0 = _tmp$1220.$0;
      _tuple$1219->$0_1 = _tmp$1220.$1;
      _tuple$1219->$0_2 = _tmp$1220.$2;
      _tuple$1219->$1_0 = _tmp$1221.$0;
      _tuple$1219->$1_1 = _tmp$1221.$1;
      _tuple$1219->$1_2 = _tmp$1221.$2;
      return _tuple$1219;
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
  int32_t _if_result$2642;
  if (end_offset$172 == 4294967296ll) {
    moonbit_incref(self$174.$0);
    end_offset$171 = $StringView$$length(self$174);
  } else {
    int64_t _Some$173 = end_offset$172;
    end_offset$171 = (int32_t)_Some$173;
  }
  if (start_offset$175 >= 0) {
    if (start_offset$175 <= end_offset$171) {
      int32_t _tmp$1211;
      moonbit_incref(self$174.$0);
      _tmp$1211 = $StringView$$length(self$174);
      _if_result$2642 = end_offset$171 <= _tmp$1211;
    } else {
      _if_result$2642 = 0;
    }
  } else {
    _if_result$2642 = 0;
  }
  if (_if_result$2642) {
    moonbit_string_t _field$2369 = self$174.$0;
    moonbit_string_t str$1212 = _field$2369;
    int32_t start$1216 = self$174.$1;
    int32_t _tmp$1213 = start$1216 + start_offset$175;
    int32_t _field$2368 = self$174.$1;
    int32_t start$1215 = _field$2368;
    int32_t _tmp$1214 = start$1215 + end_offset$171;
    return (struct $StringView){_tmp$1213, _tmp$1214, str$1212};
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
  int32_t _tmp$1210;
  moonbit_incref(str$169.$0);
  _tmp$1210 = $StringView$$length(str$169);
  if (_tmp$1210 <= 4) {
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
        int32_t _tmp$1197 = i$164;
        if (_tmp$1197 >= 0) {
          int32_t _tmp$1202;
          while (1) {
            int32_t _tmp$1200 = i$164;
            int32_t _if_result$2645;
            if (_tmp$1200 >= 0) {
              int32_t _tmp$1199 = i$164;
              int32_t _tmp$1198;
              moonbit_incref(haystack$160.$0);
              _tmp$1198
              = $StringView$$unsafe_charcode_at(
                haystack$160, _tmp$1199
              );
              _if_result$2645 = _tmp$1198 != needle_first$163;
            } else {
              _if_result$2645 = 0;
            }
            if (_if_result$2645) {
              int32_t _tmp$1201 = i$164;
              i$164 = _tmp$1201 - 1;
              continue;
            }
            break;
          }
          _tmp$1202 = i$164;
          if (_tmp$1202 >= 0) {
            int32_t j$166 = 1;
            int32_t _tmp$1209;
            while (1) {
              if (j$166 < needle_len$161) {
                int32_t _tmp$1206 = i$164;
                int32_t _tmp$1205 = _tmp$1206 + j$166;
                int32_t _tmp$1203;
                int32_t _tmp$1204;
                int32_t _tmp$1207;
                moonbit_incref(haystack$160.$0);
                _tmp$1203
                = $StringView$$unsafe_charcode_at(
                  haystack$160, _tmp$1205
                );
                moonbit_incref(needle$162.$0);
                _tmp$1204
                = $StringView$$unsafe_charcode_at(
                  needle$162, j$166
                );
                if (_tmp$1203 != _tmp$1204) {
                  break;
                }
                _tmp$1207 = j$166 + 1;
                j$166 = _tmp$1207;
                continue;
              } else {
                int32_t _tmp$1208;
                moonbit_decref(needle$162.$0);
                moonbit_decref(haystack$160.$0);
                _tmp$1208 = i$164;
                return (int64_t)_tmp$1208;
              }
              break;
            }
            _tmp$1209 = i$164;
            i$164 = _tmp$1209 - 1;
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
      int32_t _tmp$1187 = needle_len$150 - 1;
      int32_t i$153 = _tmp$1187;
      int32_t _tmp$1196;
      int32_t i$155;
      while (1) {
        if (i$153 > 0) {
          int32_t _tmp$1185;
          int32_t _tmp$1184;
          int32_t _tmp$1186;
          moonbit_incref(needle$151.$0);
          _tmp$1185 = $StringView$$unsafe_charcode_at(needle$151, i$153);
          _tmp$1184 = _tmp$1185 & 255;
          if (
            _tmp$1184 < 0
            || _tmp$1184 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          skip_table$152[_tmp$1184] = i$153;
          _tmp$1186 = i$153 - 1;
          i$153 = _tmp$1186;
          continue;
        }
        break;
      }
      _tmp$1196 = haystack_len$148 - needle_len$150;
      i$155 = _tmp$1196;
      while (1) {
        if (i$155 >= 0) {
          int32_t j$156 = 0;
          int32_t _tmp$1195;
          int32_t _tmp$1194;
          int32_t _tmp$1193;
          int32_t _tmp$1192;
          while (1) {
            if (j$156 < needle_len$150) {
              int32_t _tmp$1190 = i$155 + j$156;
              int32_t _tmp$1188;
              int32_t _tmp$1189;
              int32_t _tmp$1191;
              moonbit_incref(haystack$149.$0);
              _tmp$1188
              = $StringView$$unsafe_charcode_at(
                haystack$149, _tmp$1190
              );
              moonbit_incref(needle$151.$0);
              _tmp$1189 = $StringView$$unsafe_charcode_at(needle$151, j$156);
              if (_tmp$1188 != _tmp$1189) {
                break;
              }
              _tmp$1191 = j$156 + 1;
              j$156 = _tmp$1191;
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
          _tmp$1195 = $StringView$$unsafe_charcode_at(haystack$149, i$155);
          _tmp$1194 = _tmp$1195 & 255;
          if (
            _tmp$1194 < 0
            || _tmp$1194 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          _tmp$1193 = (int32_t)skip_table$152[_tmp$1194];
          _tmp$1192 = i$155 - _tmp$1193;
          i$155 = _tmp$1192;
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
  int32_t _tmp$1183;
  moonbit_incref(str$146.$0);
  _tmp$1183 = $StringView$$length(str$146);
  if (_tmp$1183 <= 4) {
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
        int32_t _tmp$1170 = i$141;
        if (_tmp$1170 <= forward_len$140) {
          int32_t _tmp$1175;
          while (1) {
            int32_t _tmp$1173 = i$141;
            int32_t _if_result$2652;
            if (_tmp$1173 <= forward_len$140) {
              int32_t _tmp$1172 = i$141;
              int32_t _tmp$1171;
              moonbit_incref(haystack$136.$0);
              _tmp$1171
              = $StringView$$unsafe_charcode_at(
                haystack$136, _tmp$1172
              );
              _if_result$2652 = _tmp$1171 != needle_first$139;
            } else {
              _if_result$2652 = 0;
            }
            if (_if_result$2652) {
              int32_t _tmp$1174 = i$141;
              i$141 = _tmp$1174 + 1;
              continue;
            }
            break;
          }
          _tmp$1175 = i$141;
          if (_tmp$1175 <= forward_len$140) {
            int32_t j$143 = 1;
            int32_t _tmp$1182;
            while (1) {
              if (j$143 < needle_len$137) {
                int32_t _tmp$1179 = i$141;
                int32_t _tmp$1178 = _tmp$1179 + j$143;
                int32_t _tmp$1176;
                int32_t _tmp$1177;
                int32_t _tmp$1180;
                moonbit_incref(haystack$136.$0);
                _tmp$1176
                = $StringView$$unsafe_charcode_at(
                  haystack$136, _tmp$1178
                );
                moonbit_incref(needle$138.$0);
                _tmp$1177
                = $StringView$$unsafe_charcode_at(
                  needle$138, j$143
                );
                if (_tmp$1176 != _tmp$1177) {
                  break;
                }
                _tmp$1180 = j$143 + 1;
                j$143 = _tmp$1180;
                continue;
              } else {
                int32_t _tmp$1181;
                moonbit_decref(needle$138.$0);
                moonbit_decref(haystack$136.$0);
                _tmp$1181 = i$141;
                return (int64_t)_tmp$1181;
              }
              break;
            }
            _tmp$1182 = i$141;
            i$141 = _tmp$1182 + 1;
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
          int32_t _tmp$1157;
          int32_t _tmp$1154;
          int32_t _tmp$1156;
          int32_t _tmp$1155;
          int32_t _tmp$1158;
          moonbit_incref(needle$124.$0);
          _tmp$1157 = $StringView$$unsafe_charcode_at(needle$124, i$127);
          _tmp$1154 = _tmp$1157 & 255;
          _tmp$1156 = needle_len$123 - 1;
          _tmp$1155 = _tmp$1156 - i$127;
          if (
            _tmp$1154 < 0
            || _tmp$1154 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          skip_table$125[_tmp$1154] = _tmp$1155;
          _tmp$1158 = i$127 + 1;
          i$127 = _tmp$1158;
          continue;
        }
        break;
      }
      i$129 = 0;
      while (1) {
        int32_t _tmp$1159 = haystack_len$121 - needle_len$123;
        if (i$129 <= _tmp$1159) {
          int32_t _end4307$130 = needle_len$123 - 1;
          int32_t j$131 = 0;
          int32_t _tmp$1169;
          int32_t _tmp$1168;
          int32_t _tmp$1167;
          int32_t _tmp$1166;
          int32_t _tmp$1165;
          int32_t _tmp$1164;
          while (1) {
            if (j$131 <= _end4307$130) {
              int32_t _tmp$1162 = i$129 + j$131;
              int32_t _tmp$1160;
              int32_t _tmp$1161;
              int32_t _tmp$1163;
              moonbit_incref(haystack$122.$0);
              _tmp$1160
              = $StringView$$unsafe_charcode_at(
                haystack$122, _tmp$1162
              );
              moonbit_incref(needle$124.$0);
              _tmp$1161 = $StringView$$unsafe_charcode_at(needle$124, j$131);
              if (_tmp$1160 != _tmp$1161) {
                break;
              }
              _tmp$1163 = j$131 + 1;
              j$131 = _tmp$1163;
              continue;
            } else {
              moonbit_decref(skip_table$125);
              moonbit_decref(needle$124.$0);
              moonbit_decref(haystack$122.$0);
              return (int64_t)i$129;
            }
            break;
          }
          _tmp$1169 = i$129 + needle_len$123;
          _tmp$1168 = _tmp$1169 - 1;
          moonbit_incref(haystack$122.$0);
          _tmp$1167
          = $StringView$$unsafe_charcode_at(
            haystack$122, _tmp$1168
          );
          _tmp$1166 = _tmp$1167 & 255;
          if (
            _tmp$1166 < 0
            || _tmp$1166 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          _tmp$1165 = (int32_t)skip_table$125[_tmp$1166];
          _tmp$1164 = i$129 + _tmp$1165;
          i$129 = _tmp$1164;
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
  moonbit_string_t _field$2372 = self$118.$0;
  moonbit_string_t str$1151 = _field$2372;
  int32_t _field$2371 = self$118.$1;
  int32_t start$1153 = _field$2371;
  int32_t _tmp$1152 = start$1153 + index$119;
  int32_t _tmp$2370 = str$1151[_tmp$1152];
  moonbit_decref(str$1151);
  return _tmp$2370;
}

int32_t $StringView$$length(struct $StringView self$117) {
  int32_t end$1149 = self$117.$2;
  int32_t _field$2373 = self$117.$1;
  int32_t start$1150;
  moonbit_decref(self$117.$0);
  start$1150 = _field$2373;
  return end$1149 - start$1150;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$115,
  int32_t index$116
) {
  int32_t len$114 = self$115->$1;
  int32_t _if_result$2657;
  if (index$116 >= 0) {
    _if_result$2657 = index$116 < len$114;
  } else {
    _if_result$2657 = 0;
  }
  if (_if_result$2657) {
    moonbit_string_t* _tmp$1148 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$115);
    moonbit_string_t _tmp$2374;
    if (index$116 < 0 || index$116 >= Moonbit_array_length(_tmp$1148)) {
      moonbit_panic();
    }
    _tmp$2374 = (moonbit_string_t)_tmp$1148[index$116];
    moonbit_incref(_tmp$2374);
    moonbit_decref(_tmp$1148);
    return _tmp$2374;
  } else {
    moonbit_decref(self$115);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$113
) {
  int32_t _field$2375 = self$113->$1;
  moonbit_decref(self$113);
  return _field$2375;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$112
) {
  int32_t _field$2376 = self$112->$1;
  moonbit_decref(self$112);
  return _field$2376;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$111
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2377 =
    self$111->$0;
  int32_t _cnt$2515 = Moonbit_object_header(self$111)->rc;
  if (_cnt$2515 > 1) {
    int32_t _new_cnt$2516;
    moonbit_incref(_field$2377);
    _new_cnt$2516 = _cnt$2515 - 1;
    Moonbit_object_header(self$111)->rc = _new_cnt$2516;
  } else if (_cnt$2515 == 1) {
    moonbit_free(self$111);
  }
  return _field$2377;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$110
) {
  moonbit_string_t* _field$2378 = self$110->$0;
  int32_t _cnt$2517 = Moonbit_object_header(self$110)->rc;
  if (_cnt$2517 > 1) {
    int32_t _new_cnt$2518;
    moonbit_incref(_field$2378);
    _new_cnt$2518 = _cnt$2517 - 1;
    Moonbit_object_header(self$110)->rc = _new_cnt$2518;
  } else if (_cnt$2517 == 1) {
    moonbit_free(self$110);
  }
  return _field$2378;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$109
) {
  struct $$3c$String$2a$Int$3e$** _field$2379 = self$109->$0;
  int32_t _cnt$2519 = Moonbit_object_header(self$109)->rc;
  if (_cnt$2519 > 1) {
    int32_t _new_cnt$2520;
    moonbit_incref(_field$2379);
    _new_cnt$2520 = _cnt$2519 - 1;
    Moonbit_object_header(self$109)->rc = _new_cnt$2520;
  } else if (_cnt$2519 == 1) {
    moonbit_free(self$109);
  }
  return _field$2379;
}

moonbit_string_t $String$$escape(moonbit_string_t self$108) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$107 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1147;
  moonbit_incref(buf$107);
  _tmp$1147
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$107
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$108, _tmp$1147);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$107);
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$105, int32_t y$106) {
  int32_t _tmp$1146 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$105, y$106
    );
  return !_tmp$1146;
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
  int32_t len$1141 = self$100->$1;
  int32_t _tmp$1140 = len$1141 + 4;
  moonbit_bytes_t _field$2380;
  moonbit_bytes_t data$1144;
  int32_t len$1145;
  int32_t inc$101;
  int32_t len$1143;
  int32_t _tmp$1142;
  moonbit_incref(self$100);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$100, _tmp$1140
  );
  _field$2380 = self$100->$0;
  data$1144 = _field$2380;
  len$1145 = self$100->$1;
  moonbit_incref(data$1144);
  inc$101 = $FixedArray$$set_utf16le_char(data$1144, len$1145, ch$102);
  len$1143 = self$100->$1;
  _tmp$1142 = len$1143 + inc$101;
  self$100->$1 = _tmp$1142;
  moonbit_decref(self$100);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$95,
  int32_t required$96
) {
  moonbit_bytes_t _field$2384 = self$95->$0;
  moonbit_bytes_t data$1139 = _field$2384;
  int32_t _tmp$2383 = Moonbit_array_length(data$1139);
  int32_t current_len$94 = _tmp$2383;
  int32_t enough_space$97;
  int32_t _tmp$1137;
  int32_t _tmp$1138;
  moonbit_bytes_t new_data$99;
  moonbit_bytes_t _field$2382;
  moonbit_bytes_t data$1135;
  int32_t len$1136;
  moonbit_bytes_t _old$2381;
  if (required$96 <= current_len$94) {
    moonbit_decref(self$95);
    return 0;
  }
  enough_space$97 = current_len$94;
  while (1) {
    int32_t _tmp$1133 = enough_space$97;
    if (_tmp$1133 < required$96) {
      int32_t _tmp$1134 = enough_space$97;
      enough_space$97 = _tmp$1134 * 2;
      continue;
    }
    break;
  }
  _tmp$1137 = enough_space$97;
  _tmp$1138 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$99 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1137, _tmp$1138);
  _field$2382 = self$95->$0;
  data$1135 = _field$2382;
  len$1136 = self$95->$1;
  moonbit_incref(data$1135);
  moonbit_incref(new_data$99);
  $FixedArray$$unsafe_blit$0(new_data$99, 0, data$1135, 0, len$1136);
  _old$2381 = self$95->$0;
  moonbit_decref(_old$2381);
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
    uint32_t _tmp$1116 = code$87 & 255u;
    int32_t _tmp$1115 = $UInt$$to_byte(_tmp$1116);
    int32_t _tmp$1117;
    uint32_t _tmp$1119;
    int32_t _tmp$1118;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1115;
    _tmp$1117 = offset$90 + 1;
    _tmp$1119 = code$87 >> 8;
    _tmp$1118 = $UInt$$to_byte(_tmp$1119);
    if (_tmp$1117 < 0 || _tmp$1117 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1117] = _tmp$1118;
    moonbit_decref(self$89);
    return 2;
  } else if (code$87 < 1114112u) {
    uint32_t hi$91 = code$87 - 65536u;
    uint32_t _tmp$1132 = hi$91 >> 10;
    uint32_t lo$92 = _tmp$1132 | 55296u;
    uint32_t _tmp$1131 = hi$91 & 1023u;
    uint32_t hi$93 = _tmp$1131 | 56320u;
    uint32_t _tmp$1121 = lo$92 & 255u;
    int32_t _tmp$1120 = $UInt$$to_byte(_tmp$1121);
    int32_t _tmp$1122;
    uint32_t _tmp$1124;
    int32_t _tmp$1123;
    int32_t _tmp$1125;
    uint32_t _tmp$1127;
    int32_t _tmp$1126;
    int32_t _tmp$1128;
    uint32_t _tmp$1130;
    int32_t _tmp$1129;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1120;
    _tmp$1122 = offset$90 + 1;
    _tmp$1124 = lo$92 >> 8;
    _tmp$1123 = $UInt$$to_byte(_tmp$1124);
    if (_tmp$1122 < 0 || _tmp$1122 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1122] = _tmp$1123;
    _tmp$1125 = offset$90 + 2;
    _tmp$1127 = hi$93 & 255u;
    _tmp$1126 = $UInt$$to_byte(_tmp$1127);
    if (_tmp$1125 < 0 || _tmp$1125 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1125] = _tmp$1126;
    _tmp$1128 = offset$90 + 3;
    _tmp$1130 = hi$93 >> 8;
    _tmp$1129 = $UInt$$to_byte(_tmp$1130);
    if (_tmp$1128 < 0 || _tmp$1128 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1128] = _tmp$1129;
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
  int32_t _tmp$1114 = *(int32_t*)&self$86;
  return _tmp$1114 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$85) {
  int32_t _tmp$1113 = self$85;
  return *(uint32_t*)&_tmp$1113;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$84
) {
  moonbit_bytes_t _field$2386 = self$84->$0;
  moonbit_bytes_t data$1112 = _field$2386;
  moonbit_bytes_t _tmp$1109;
  int32_t _field$2385;
  int32_t len$1111;
  int64_t _tmp$1110;
  moonbit_incref(data$1112);
  _tmp$1109 = data$1112;
  _field$2385 = self$84->$1;
  moonbit_decref(self$84);
  len$1111 = _field$2385;
  _tmp$1110 = (int64_t)len$1111;
  return $Bytes$$to_unchecked_string$inner(_tmp$1109, 0, _tmp$1110);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$79,
  int32_t offset$83,
  int64_t length$81
) {
  int32_t len$78 = Moonbit_array_length(self$79);
  int32_t length$80;
  int32_t _if_result$2659;
  if (length$81 == 4294967296ll) {
    length$80 = len$78 - offset$83;
  } else {
    int64_t _Some$82 = length$81;
    length$80 = (int32_t)_Some$82;
  }
  if (offset$83 >= 0) {
    if (length$80 >= 0) {
      int32_t _tmp$1108 = offset$83 + length$80;
      _if_result$2659 = _tmp$1108 <= len$78;
    } else {
      _if_result$2659 = 0;
    }
  } else {
    _if_result$2659 = 0;
  }
  if (_if_result$2659) {
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
  struct $$moonbitlang$core$builtin$StringBuilder* _block$2660;
  if (size_hint$76 < 1) {
    initial$75 = 1;
  } else {
    initial$75 = size_hint$76;
  }
  data$77 = (moonbit_bytes_t)moonbit_make_bytes(initial$75, 0);
  _block$2660
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$2660)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$2660->$0 = data$77;
  _block$2660->$1 = 0;
  return _block$2660;
}

int32_t $Byte$$to_char(int32_t self$74) {
  int32_t _tmp$1107 = (int32_t)self$74;
  return _tmp$1107;
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
  int32_t _if_result$2661;
  if (dst$50 == src$51) {
    _if_result$2661 = dst_offset$52 < src_offset$53;
  } else {
    _if_result$2661 = 0;
  }
  if (_if_result$2661) {
    int32_t i$54 = 0;
    while (1) {
      if (i$54 < len$55) {
        int32_t _tmp$1098 = dst_offset$52 + i$54;
        int32_t _tmp$1100 = src_offset$53 + i$54;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2388;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1099;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2387;
        int32_t _tmp$1101;
        if (_tmp$1100 < 0 || _tmp$1100 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2388
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1100
          ];
        _tmp$1099 = _tmp$2388;
        if (_tmp$1098 < 0 || _tmp$1098 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2387
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1098
          ];
        if (_tmp$1099) {
          moonbit_incref(_tmp$1099);
        }
        if (_old$2387) {
          moonbit_decref(_old$2387);
        }
        dst$50[_tmp$1098] = _tmp$1099;
        _tmp$1101 = i$54 + 1;
        i$54 = _tmp$1101;
        continue;
      } else {
        moonbit_decref(src$51);
        moonbit_decref(dst$50);
      }
      break;
    }
  } else {
    int32_t _tmp$1106 = len$55 - 1;
    int32_t i$57 = _tmp$1106;
    while (1) {
      if (i$57 >= 0) {
        int32_t _tmp$1102 = dst_offset$52 + i$57;
        int32_t _tmp$1104 = src_offset$53 + i$57;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2390;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1103;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2389;
        int32_t _tmp$1105;
        if (_tmp$1104 < 0 || _tmp$1104 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2390
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1104
          ];
        _tmp$1103 = _tmp$2390;
        if (_tmp$1102 < 0 || _tmp$1102 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2389
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1102
          ];
        if (_tmp$1103) {
          moonbit_incref(_tmp$1103);
        }
        if (_old$2389) {
          moonbit_decref(_old$2389);
        }
        dst$50[_tmp$1102] = _tmp$1103;
        _tmp$1105 = i$57 - 1;
        i$57 = _tmp$1105;
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
  int32_t _if_result$2664;
  if (dst$41 == src$42) {
    _if_result$2664 = dst_offset$43 < src_offset$44;
  } else {
    _if_result$2664 = 0;
  }
  if (_if_result$2664) {
    int32_t i$45 = 0;
    while (1) {
      if (i$45 < len$46) {
        int32_t _tmp$1089 = dst_offset$43 + i$45;
        int32_t _tmp$1091 = src_offset$44 + i$45;
        struct $$3c$String$2a$Int$3e$* _tmp$2392;
        struct $$3c$String$2a$Int$3e$* _tmp$1090;
        struct $$3c$String$2a$Int$3e$* _old$2391;
        int32_t _tmp$1092;
        if (_tmp$1091 < 0 || _tmp$1091 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2392 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1091];
        _tmp$1090 = _tmp$2392;
        if (_tmp$1089 < 0 || _tmp$1089 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2391 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1089];
        if (_tmp$1090) {
          moonbit_incref(_tmp$1090);
        }
        if (_old$2391) {
          moonbit_decref(_old$2391);
        }
        dst$41[_tmp$1089] = _tmp$1090;
        _tmp$1092 = i$45 + 1;
        i$45 = _tmp$1092;
        continue;
      } else {
        moonbit_decref(src$42);
        moonbit_decref(dst$41);
      }
      break;
    }
  } else {
    int32_t _tmp$1097 = len$46 - 1;
    int32_t i$48 = _tmp$1097;
    while (1) {
      if (i$48 >= 0) {
        int32_t _tmp$1093 = dst_offset$43 + i$48;
        int32_t _tmp$1095 = src_offset$44 + i$48;
        struct $$3c$String$2a$Int$3e$* _tmp$2394;
        struct $$3c$String$2a$Int$3e$* _tmp$1094;
        struct $$3c$String$2a$Int$3e$* _old$2393;
        int32_t _tmp$1096;
        if (_tmp$1095 < 0 || _tmp$1095 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2394 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1095];
        _tmp$1094 = _tmp$2394;
        if (_tmp$1093 < 0 || _tmp$1093 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2393 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1093];
        if (_tmp$1094) {
          moonbit_incref(_tmp$1094);
        }
        if (_old$2393) {
          moonbit_decref(_old$2393);
        }
        dst$41[_tmp$1093] = _tmp$1094;
        _tmp$1096 = i$48 - 1;
        i$48 = _tmp$1096;
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
  int32_t _if_result$2667;
  if (dst$32 == src$33) {
    _if_result$2667 = dst_offset$34 < src_offset$35;
  } else {
    _if_result$2667 = 0;
  }
  if (_if_result$2667) {
    int32_t i$36 = 0;
    while (1) {
      if (i$36 < len$37) {
        int32_t _tmp$1080 = dst_offset$34 + i$36;
        int32_t _tmp$1082 = src_offset$35 + i$36;
        moonbit_string_t _tmp$2396;
        moonbit_string_t _tmp$1081;
        moonbit_string_t _old$2395;
        int32_t _tmp$1083;
        if (_tmp$1082 < 0 || _tmp$1082 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2396 = (moonbit_string_t)src$33[_tmp$1082];
        _tmp$1081 = _tmp$2396;
        if (_tmp$1080 < 0 || _tmp$1080 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2395 = (moonbit_string_t)dst$32[_tmp$1080];
        moonbit_incref(_tmp$1081);
        moonbit_decref(_old$2395);
        dst$32[_tmp$1080] = _tmp$1081;
        _tmp$1083 = i$36 + 1;
        i$36 = _tmp$1083;
        continue;
      } else {
        moonbit_decref(src$33);
        moonbit_decref(dst$32);
      }
      break;
    }
  } else {
    int32_t _tmp$1088 = len$37 - 1;
    int32_t i$39 = _tmp$1088;
    while (1) {
      if (i$39 >= 0) {
        int32_t _tmp$1084 = dst_offset$34 + i$39;
        int32_t _tmp$1086 = src_offset$35 + i$39;
        moonbit_string_t _tmp$2398;
        moonbit_string_t _tmp$1085;
        moonbit_string_t _old$2397;
        int32_t _tmp$1087;
        if (_tmp$1086 < 0 || _tmp$1086 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2398 = (moonbit_string_t)src$33[_tmp$1086];
        _tmp$1085 = _tmp$2398;
        if (_tmp$1084 < 0 || _tmp$1084 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2397 = (moonbit_string_t)dst$32[_tmp$1084];
        moonbit_incref(_tmp$1085);
        moonbit_decref(_old$2397);
        dst$32[_tmp$1084] = _tmp$1085;
        _tmp$1087 = i$39 - 1;
        i$39 = _tmp$1087;
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
  int32_t _if_result$2670;
  if (dst$23 == src$24) {
    _if_result$2670 = dst_offset$25 < src_offset$26;
  } else {
    _if_result$2670 = 0;
  }
  if (_if_result$2670) {
    int32_t i$27 = 0;
    while (1) {
      if (i$27 < len$28) {
        int32_t _tmp$1071 = dst_offset$25 + i$27;
        int32_t _tmp$1073 = src_offset$26 + i$27;
        int32_t _tmp$1072;
        int32_t _tmp$1074;
        if (_tmp$1073 < 0 || _tmp$1073 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1072 = (int32_t)src$24[_tmp$1073];
        if (_tmp$1071 < 0 || _tmp$1071 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1071] = _tmp$1072;
        _tmp$1074 = i$27 + 1;
        i$27 = _tmp$1074;
        continue;
      } else {
        moonbit_decref(src$24);
        moonbit_decref(dst$23);
      }
      break;
    }
  } else {
    int32_t _tmp$1079 = len$28 - 1;
    int32_t i$30 = _tmp$1079;
    while (1) {
      if (i$30 >= 0) {
        int32_t _tmp$1075 = dst_offset$25 + i$30;
        int32_t _tmp$1077 = src_offset$26 + i$30;
        int32_t _tmp$1076;
        int32_t _tmp$1078;
        if (_tmp$1077 < 0 || _tmp$1077 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1076 = (int32_t)src$24[_tmp$1077];
        if (_tmp$1075 < 0 || _tmp$1075 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1075] = _tmp$1076;
        _tmp$1078 = i$30 - 1;
        i$30 = _tmp$1078;
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
  moonbit_string_t _tmp$1070 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$1068 =
    moonbit_add_string(
      _tmp$1070, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1069 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$1067 = moonbit_add_string(_tmp$1068, _tmp$1069);
  moonbit_string_t _tmp$1066 =
    moonbit_add_string(
      _tmp$1067, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1066);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$1065 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$1063 =
    moonbit_add_string(
      _tmp$1065, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1064 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$1062 = moonbit_add_string(_tmp$1063, _tmp$1064);
  moonbit_string_t _tmp$1061 =
    moonbit_add_string(
      _tmp$1062, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1061);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
) {
  moonbit_string_t _tmp$1060 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$17);
  moonbit_string_t _tmp$1058 =
    moonbit_add_string(
      _tmp$1060, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1059 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$18);
  moonbit_string_t _tmp$1057 = moonbit_add_string(_tmp$1058, _tmp$1059);
  moonbit_string_t _tmp$1056 =
    moonbit_add_string(
      _tmp$1057, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1056);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
) {
  moonbit_string_t _tmp$1055 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$15);
  moonbit_string_t _tmp$1053 =
    moonbit_add_string(
      _tmp$1055, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1054 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$16);
  moonbit_string_t _tmp$1052 = moonbit_add_string(_tmp$1053, _tmp$1054);
  moonbit_string_t _tmp$1051 =
    moonbit_add_string(
      _tmp$1052, (moonbit_string_t)moonbit_string_literal_32.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1051);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$11,
  struct $$moonbitlang$core$builtin$Logger _x_5272$14
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$12 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$11;
  moonbit_string_t _field$2399 = _Failure$12->$0;
  int32_t _cnt$2521 = Moonbit_object_header(_Failure$12)->rc;
  moonbit_string_t _$2a$err_payload_5273$13;
  struct $$moonbitlang$core$builtin$Logger _bind$1050;
  if (_cnt$2521 > 1) {
    int32_t _new_cnt$2522;
    moonbit_incref(_field$2399);
    _new_cnt$2522 = _cnt$2521 - 1;
    Moonbit_object_header(_Failure$12)->rc = _new_cnt$2522;
  } else if (_cnt$2521 == 1) {
    moonbit_free(_Failure$12);
  }
  _$2a$err_payload_5273$13 = _field$2399;
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
  _bind$1050 = _x_5272$14;
  _bind$1050.$0->$method_0(
    _bind$1050.$1, (moonbit_string_t)moonbit_string_literal_34.data
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

moonbit_string_t $Error$to_string(void* _e$1013) {
  switch (Moonbit_object_tag(_e$1013)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1013
             );
      break;
    }
    
    case 5: {
      moonbit_decref(_e$1013);
      return (moonbit_string_t)moonbit_string_literal_37.data;
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1013
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1013);
      return (moonbit_string_t)moonbit_string_literal_38.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
               _e$1013
             );
      break;
    }
    default: {
      moonbit_decref(_e$1013);
      return (moonbit_string_t)moonbit_string_literal_39.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1031,
  int32_t _param$1030
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1029 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1031;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1029, _param$1030
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1028,
  struct $StringView _param$1027
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1026 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1028;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1026, _param$1027
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1025,
  moonbit_string_t _param$1022,
  int32_t _param$1023,
  int32_t _param$1024
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1021 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1025;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1021, _param$1022, _param$1023, _param$1024
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1020,
  moonbit_string_t _param$1019
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1018 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1020;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1018, _param$1019
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$839;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1037;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1036;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$836;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1047;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1046;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1045;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1040;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$837;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1044;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1043;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1042;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1041;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$835;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1039;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1038;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$838;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1049;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1048;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$120 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$134 = (int64_t)0;
  _bind$839
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1037 = _bind$839;
  _tmp$1036
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1037
  };
  $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1036
  );
  _bind$836
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1047 = _bind$836;
  _tmp$1046
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1047
  };
  _tmp$1045 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1046);
  _tuple$1040
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1040)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1040->$0 = (moonbit_string_t)moonbit_string_literal_40.data;
  _tuple$1040->$1 = _tmp$1045;
  _bind$837
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1044 = _bind$837;
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
  _tuple$1041->$0 = (moonbit_string_t)moonbit_string_literal_41.data;
  _tuple$1041->$1 = _tmp$1042;
  _bind$835
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      2
    );
  _bind$835[0] = _tuple$1040;
  _bind$835[1] = _tuple$1041;
  _tmp$1039 = _bind$835;
  _tmp$1038
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 2, _tmp$1039
  };
  $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1038
  );
  _bind$838
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1049 = _bind$838;
  _tmp$1048
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1049
  };
  $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1048
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1035;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1007;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1008;
  int32_t _len$1009;
  int32_t _i$1010;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1035
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1007
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1007)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1007->$0 = _tmp$1035;
  async_tests$1007->$1 = 0;
  _arr$1008
  = $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1008);
  _len$1009 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1008);
  _i$1010 = 0;
  while (1) {
    if (_i$1010 < _len$1009) {
      struct $$3c$String$2a$Int$3e$* arg$1011;
      moonbit_string_t _field$2401;
      moonbit_string_t _tmp$1032;
      int32_t _field$2400;
      int32_t _cnt$2523;
      int32_t _tmp$1033;
      int32_t _tmp$1034;
      moonbit_incref(_arr$1008);
      arg$1011
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1008, _i$1010
      );
      _field$2401 = arg$1011->$0;
      _tmp$1032 = _field$2401;
      _field$2400 = arg$1011->$1;
      _cnt$2523 = Moonbit_object_header(arg$1011)->rc;
      if (_cnt$2523 > 1) {
        int32_t _new_cnt$2524;
        moonbit_incref(_tmp$1032);
        _new_cnt$2524 = _cnt$2523 - 1;
        Moonbit_object_header(arg$1011)->rc = _new_cnt$2524;
      } else if (_cnt$2523 == 1) {
        moonbit_free(arg$1011);
      }
      _tmp$1033 = _field$2400;
      moonbit_incref(async_tests$1007);
      $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_do_execute(
        async_tests$1007, _tmp$1032, _tmp$1033
      );
      _tmp$1034 = _i$1010 + 1;
      _i$1010 = _tmp$1034;
      continue;
    } else {
      moonbit_decref(_arr$1008);
    }
    break;
  }
  $azimuth$telemetry$working_tests_blackbox_test$moonbit_test_driver_internal_run_async_tests(
    async_tests$1007
  );
  return 0;
}