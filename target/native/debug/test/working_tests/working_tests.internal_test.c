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
struct $$3c$Unit$3e$$3d$$3e$Unit;

struct $TextMapCarrier;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $StringView;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $SpanContext;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $HttpResponse;

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Logger;

struct $Moonbit_Test_Driver_Internal__TestCase;

struct $$3c$$3e$$3d$$3e$Unit;

struct $Option$3c$Option$3c$Attributes$3e$$3e$$Some;

struct $$3c$String$2a$AttributeValue$3e$;

struct $$3c$Int$2a$Int$3e$;

struct $$3c$String$3e$$3d$$3e$Int;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$String$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$3c$StringView$2a$StringView$3e$;

struct $LogRecord;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $InstrumentationScope;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$Int$3e$;

struct $$moonbitlang$core$builtin$Array$3c$String$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $AttributeValue$FloatValue;

struct $Option$3c$Option$3c$String$3e$$3e$$Some;

struct $Ref$3c$Int$3e$;

struct $AttributeValue$ArrayStringValue;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $Option$3c$Int64$3e$$Some;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $ContextKey$3c$String$3e$;

struct $HttpRequest;

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap;

struct $AttributeValue$IntValue;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$;

struct $Counter;

struct $Attributes;

struct $Context;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $AttributeValue$ArrayIntValue;

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

struct $Meter;

struct $AttributeValue$BoolValue;

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap;

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$;

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $Moonbit_Test_Driver_Internal__F$F2;

struct $$moonbitlang$core$builtin$Hasher;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $Option$3c$StringView$3e$$Some;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $Error$azimuth$telemetry$working_tests$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $AttributeValue$StringValue;

struct $$3c$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  
};

struct $TextMapCarrier {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* $0;
  
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

struct $StringView {
  int32_t $1;
  int32_t $2;
  moonbit_string_t $0;
  
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

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $SpanContext {
  int32_t $2;
  moonbit_string_t $0;
  moonbit_string_t $1;
  moonbit_string_t $3;
  
};

struct $Result$3c$Unit$2a$Error$3e$$Ok {
  int32_t $0;
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $HttpResponse {
  int32_t $0;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* $1;
  moonbit_string_t $2;
  
};

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    int32_t,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  moonbit_string_t $0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $Moonbit_Test_Driver_Internal__TestCase {
  void* $0;
  struct $Moonbit_Test_Driver_Internal_Meta* $1;
  
};

struct $$3c$$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  
};

struct $Option$3c$Option$3c$Attributes$3e$$3e$$Some {
  struct $Attributes* $0;
  
};

struct $$3c$String$2a$AttributeValue$3e$ {
  moonbit_string_t $0;
  void* $1;
  
};

struct $$3c$Int$2a$Int$3e$ {
  int32_t $0;
  int32_t $1;
  
};

struct $$3c$String$3e$$3d$$3e$Int {
  int32_t(* code)(struct $$3c$String$3e$$3d$$3e$Int*, moonbit_string_t);
  
};

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $$3c$String$2a$String$3e$ {
  moonbit_string_t $0;
  moonbit_string_t $1;
  
};

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $1;
  int32_t $2;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** $0;
  
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

struct $LogRecord {
  int32_t $0;
  moonbit_string_t $1;
  struct $Attributes* $2;
  void* $3;
  void* $4;
  moonbit_string_t $5;
  moonbit_string_t $6;
  struct $Context* $7;
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514 {
  void* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
};

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$ {
  int32_t $1;
  struct $$3c$String$2a$AttributeValue$3e$** $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$ {
  int32_t $1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $0;
  
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

struct $InstrumentationScope {
  moonbit_string_t $0;
  moonbit_string_t $1;
  moonbit_string_t $2;
  
};

struct $Result$3c$Unit$2a$Error$3e$$Err {
  void* $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$ {
  int32_t $1;
  int32_t* $0;
  
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

struct $$moonbitlang$core$builtin$Logger$static_method_table {
  int32_t(* $method_0)(void*, moonbit_string_t);
  int32_t(* $method_1)(void*, moonbit_string_t, int32_t, int32_t);
  int32_t(* $method_2)(void*, struct $StringView);
  int32_t(* $method_3)(void*, int32_t);
  
};

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  
};

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ {
  struct moonbit_result_0(* code)(
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*,
    int32_t
  );
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $1;
  moonbit_string_t $4;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $5;
  
};

struct $AttributeValue$FloatValue {
  double $0;
  
};

struct $Option$3c$Option$3c$String$3e$$3e$$Some {
  moonbit_string_t $0;
  
};

struct $Ref$3c$Int$3e$ {
  int32_t $0;
  
};

struct $AttributeValue$ArrayStringValue {
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $0;
  
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

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(
    struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Unit$3e$$3d$$3e$Unit*,
    struct $$3c$Error$3e$$3d$$3e$Unit*
  );
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  moonbit_string_t $1;
  
};

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  void* $1;
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
};

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $1;
  
};

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
};

struct $Option$3c$Int64$3e$$Some {
  int64_t $0;
  
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

struct $ContextKey$3c$String$3e$ {
  moonbit_string_t $0;
  
};

struct $HttpRequest {
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* $2;
  moonbit_string_t $3;
  
};

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap {
  moonbit_string_t(* code)(struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*);
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t* $0_0;
  struct $Ref$3c$Int$3e$* $1;
  
};

struct $AttributeValue$IntValue {
  int32_t $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$ {
  int32_t $1;
  struct $$3c$String$2a$Int$3e$** $0;
  
};

struct $Counter {
  moonbit_string_t $0;
  moonbit_string_t $1;
  moonbit_string_t $2;
  
};

struct $Attributes {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* $0;
  
};

struct $Context {
  struct $$3c$String$2a$String$3e$* $0;
  
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

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger {
  struct $$moonbitlang$core$builtin$Logger$static_method_table* $0;
  void* $1;
  
};

struct $AttributeValue$ArrayIntValue {
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* $0;
  
};

struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $Meter {
  struct $InstrumentationScope* $0;
  
};

struct $AttributeValue$BoolValue {
  int32_t $0;
  
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

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$ {
  int32_t $1;
  struct $$3c$String$2a$String$3e$** $0;
  
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

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
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

struct $Option$3c$StringView$3e$$Some {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  
};

struct $$moonbitlang$core$builtin$StringBuilder {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $Error$azimuth$telemetry$working_tests$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
  moonbit_string_t $0;
  
};

struct $AttributeValue$StringValue {
  moonbit_string_t $0;
  
};

struct moonbit_result_0 {
  int tag;
  union { int32_t ok; void* err;  } data;
  
};

struct moonbit_result_1 {
  int tag;
  union { struct $StringView ok; void* err;  } data;
  
};

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2327
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2326
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2325
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2324
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2323
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2322
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2321
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2320
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2319
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2318
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2317
);

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1126
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2291,
  moonbit_string_t s$1104,
  int32_t sep$1105
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1091
);

moonbit_string_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2200,
  moonbit_bytes_t bytes$1092
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$2193,
  moonbit_string_t s$1086
);

#define $azimuth$telemetry$working_tests$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1049,
  moonbit_string_t filename$1010,
  int32_t index$1011
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2186
);

struct moonbit_result_0 $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2182
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2180
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2164,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1050,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1051
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2175,
  int32_t _cont_param$1070
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2172,
  void* _cont_param$1071
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$2166,
  void* _state$1053
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2161,
  void* err$1033
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2147,
  moonbit_string_t attr$1026
);

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$2131,
  moonbit_string_t test_name$1013,
  moonbit_string_t file_name$1014,
  moonbit_string_t message$1015,
  int32_t skipped$1016
);

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1008
);

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1006,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1007,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1004
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$working_tests$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$967,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$980,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$993,
  moonbit_string_t file_filter$964,
  int32_t index_filter$965
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10(
  
);

int32_t $$azimuth$telemetry$working_tests$Random$$system();

uint64_t $$azimuth$telemetry$working_tests$Random$$next_u64(
  int32_t random$950
);

int32_t $$azimuth$telemetry$working_tests$Clock$$system();

int64_t $$azimuth$telemetry$working_tests$Clock$$now_unix_nanos(
  int32_t clock$949
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9(
  
);

int32_t $$azimuth$telemetry$working_tests$HttpResponse$$status_code(
  struct $HttpResponse* response$940
);

struct $HttpResponse* $$azimuth$telemetry$working_tests$HttpResponse$$new(
  int32_t status_code$938,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$939,
  void* body$opt$936
);

struct $HttpResponse* $$azimuth$telemetry$working_tests$HttpResponse$$new$inner(
  int32_t status_code$932,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$933,
  moonbit_string_t body$934
);

moonbit_string_t $$azimuth$telemetry$working_tests$HttpResponse$$body(
  struct $HttpResponse* response$931
);

moonbit_string_t $$azimuth$telemetry$working_tests$HttpRequest$$url(
  struct $HttpRequest* request$930
);

struct $HttpRequest* $$azimuth$telemetry$working_tests$HttpRequest$$new(
  moonbit_string_t http_method$927,
  moonbit_string_t url$928,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$929,
  void* body$opt$925
);

struct $HttpRequest* $$azimuth$telemetry$working_tests$HttpRequest$$new$inner(
  moonbit_string_t http_method$920,
  moonbit_string_t url$921,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$922,
  moonbit_string_t body$923
);

moonbit_string_t $$azimuth$telemetry$working_tests$HttpRequest$$http_method(
  struct $HttpRequest* request$919
);

moonbit_string_t $$azimuth$telemetry$working_tests$HttpRequest$$body(
  struct $HttpRequest* request$918
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8(
  
);

int32_t $$azimuth$telemetry$working_tests$TextMapCarrier$$set(
  struct $TextMapCarrier* carrier$912,
  moonbit_string_t key$913,
  moonbit_string_t value$914
);

struct $TextMapCarrier* $$azimuth$telemetry$working_tests$TextMapCarrier$$new(
  
);

moonbit_string_t $$azimuth$telemetry$working_tests$TextMapCarrier$$get(
  struct $TextMapCarrier* carrier$911,
  moonbit_string_t key$910
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7(
  
);

int32_t $$azimuth$telemetry$working_tests$LogRecord$$severity_number(
  struct $LogRecord* record$906
);

struct $LogRecord* $$azimuth$telemetry$working_tests$LogRecord$$new(
  int32_t severity$904,
  moonbit_string_t body$905
);

moonbit_string_t $$azimuth$telemetry$working_tests$LogRecord$$body(
  struct $LogRecord* record$903
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6(
  
);

struct $Meter* $$azimuth$telemetry$working_tests$MeterProvider$$get_meter(
  int32_t provider$898,
  moonbit_string_t name$897
);

int32_t $$azimuth$telemetry$working_tests$MeterProvider$$default();

struct $Counter* $$azimuth$telemetry$working_tests$Meter$$create_counter(
  struct $Meter* meter$895,
  moonbit_string_t name$894
);

int32_t $$azimuth$telemetry$working_tests$Counter$$add(
  struct $Counter* counter$892,
  double value$893,
  void* attributes$opt$890
);

int32_t $$azimuth$telemetry$working_tests$Counter$$add$inner(
  struct $Counter* counter$886,
  double value$887,
  struct $Attributes* attributes$888
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5(
  
);

struct $ContextKey$3c$String$3e$* $$azimuth$telemetry$working_tests$ContextKey$$new(
  moonbit_string_t key$881
);

struct $Context* $$azimuth$telemetry$working_tests$Context$$with_value(
  struct $Context* ctx$880,
  struct $ContextKey$3c$String$3e$* key$878,
  moonbit_string_t value$879
);

struct $Context* $$azimuth$telemetry$working_tests$Context$$root();

moonbit_string_t $$azimuth$telemetry$working_tests$Context$$get(
  struct $Context* ctx$872,
  struct $ContextKey$3c$String$3e$* key$877
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4(
  
);

moonbit_string_t $$azimuth$telemetry$working_tests$SpanContext$$trace_id(
  struct $SpanContext* ctx$859
);

moonbit_string_t $$azimuth$telemetry$working_tests$SpanContext$$span_id(
  struct $SpanContext* ctx$858
);

struct $SpanContext* $$azimuth$telemetry$working_tests$SpanContext$$new(
  moonbit_string_t trace_id$854,
  moonbit_string_t span_id$855,
  int32_t sampled$856,
  moonbit_string_t trace_state$857
);

int32_t $$azimuth$telemetry$working_tests$SpanContext$$is_valid(
  struct $SpanContext* ctx$853
);

int32_t $$azimuth$telemetry$working_tests$SpanContext$$is_sampled(
  struct $SpanContext* ctx$852
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3(
  
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2(
  
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1(
  
);

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0(
  
);

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$836
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$834,
  struct $$moonbitlang$core$builtin$Logger logger$835
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$820,
  struct $$moonbitlang$core$builtin$Logger logger$833
);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$818);

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$817,
  struct $$moonbitlang$core$builtin$Hasher* hasher$816
);

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$815,
  struct $$moonbitlang$core$builtin$Hasher* hasher$814
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$812,
  moonbit_string_t value$810
);

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$806,
  struct $$3c$String$3e$$3d$$3e$Int* f$808
);

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2050,
  moonbit_string_t k$807
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$804,
  int32_t idx$805
);

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$802,
  int32_t idx$803
);

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$798,
  int32_t key$794
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$789,
  moonbit_string_t key$785
);

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$780,
  int32_t key$776
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$771,
  moonbit_string_t key$767
);

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$762,
  int32_t key$758
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$753,
  moonbit_string_t key$749
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$741
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$733
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$725
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$717
);

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$713,
  moonbit_string_t key$714,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$715
);

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$710,
  moonbit_string_t key$711,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$712
);

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$707,
  int32_t key$708,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$709
);

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$704,
  moonbit_string_t key$705,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$706
);

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$694
);

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$683
);

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$672
);

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$661
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$644,
  moonbit_string_t key$653,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$654,
  int32_t hash$652
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$628,
  moonbit_string_t key$637,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$638,
  int32_t hash$636
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$612,
  int32_t key$621,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$622,
  int32_t hash$620
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$596,
  moonbit_string_t key$605,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$606,
  int32_t hash$604
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$590,
  int32_t idx$595,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$594
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$580,
  int32_t idx$585,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$584
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$570,
  int32_t idx$575,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$574
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$560,
  int32_t idx$565,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$564
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$550,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$552,
  int32_t new_idx$551
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$544,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$546,
  int32_t new_idx$545
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$538,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$540,
  int32_t new_idx$539
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$532,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$534,
  int32_t new_idx$533
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$529,
  int32_t idx$531,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$530
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$525,
  int32_t idx$527,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$526
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$521,
  int32_t idx$523,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$522
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$517,
  int32_t idx$519,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$518
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$511
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$505
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$499
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$493
);

int32_t $Int$$next_power_of_two(int32_t self$491);

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$490);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$488
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$486
);

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$484
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$482
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$481
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$480
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$478
);

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1703
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$476
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$475,
  struct $$moonbitlang$core$builtin$Logger logger$474
);

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$472,
  struct $$3c$String$3e$$3d$$3e$Int* f$473
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$468,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$470
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$465,
  struct $$3c$String$2a$Int$3e$* value$467
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$462,
  moonbit_string_t value$464
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$460
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$457
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$454
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$450,
  int32_t new_capacity$448
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$444,
  int32_t new_capacity$442
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$438,
  int32_t new_capacity$436
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$434
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$432,
  struct $StringView str$433
);

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$429,
  int32_t i$430,
  int32_t start_offset$431,
  int64_t end_offset$427
);

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$424,
  int32_t n$422,
  int32_t start_offset$418,
  int32_t end_offset$419
);

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$416,
  int32_t n$414,
  int32_t start_offset$413,
  int32_t end_offset$412
);

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$402,
  int32_t len$405,
  int32_t start_offset$409,
  int64_t end_offset$400
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$398
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$397
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$396
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$395
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$394
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$389
);

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1632,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$387
);

moonbit_string_t $$moonbitlang$core$builtin$Iterator$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$386
);

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$377,
  struct $$moonbitlang$core$builtin$Logger logger$375
);

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$371,
  int32_t seg$374,
  int32_t i$373
);

moonbit_string_t $Byte$$to_hex(int32_t b$369);

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$367);

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$365,
  int32_t that$366
);

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$363,
  int32_t that$364
);

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$361,
  int32_t that$362
);

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$359,
  int32_t that$360
);

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$356,
  int32_t start$354,
  int32_t end$355
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$353
);

moonbit_string_t $Int$$to_string$inner(int32_t self$337, int32_t radix$336);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$330,
  int32_t radix$333
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$328);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$327);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$317,
  uint32_t num$305,
  int32_t digit_start$308,
  int32_t total_len$307
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$299,
  uint32_t num$293,
  int32_t digit_start$291,
  int32_t total_len$290,
  int32_t radix$295
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$286,
  uint32_t num$282,
  int32_t digit_start$280,
  int32_t total_len$279
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$277
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$275
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$273
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$271
);

int32_t $StringView$$start_offset(struct $StringView self$269);

moonbit_string_t $StringView$$data(struct $StringView self$268);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$262,
  moonbit_string_t value$265,
  int32_t start$266,
  int32_t len$267
);

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$255,
  int32_t start$261,
  int64_t end$257
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$253
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$251
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$248
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$246
);

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$245
);

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$244
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$242,
  int32_t value$241
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$240,
  moonbit_string_t value$239
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$237,
  int32_t value$238
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$235,
  uint32_t value$236
);

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$233,
  uint32_t input$234
);

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$231, int32_t r$232);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$229,
  moonbit_string_t str$230
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$221,
  int32_t bytes_offset$216,
  moonbit_string_t str$223,
  int32_t str_offset$219,
  int32_t length$217
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$183
);

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$179
);

struct $StringView $StringView$$view$inner(
  struct $StringView self$176,
  int32_t start_offset$177,
  int64_t end_offset$174
);

int64_t $StringView$$rev_find(
  struct $StringView self$172,
  struct $StringView str$171
);

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$162,
  struct $StringView needle$164
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$151,
  struct $StringView needle$153
);

int64_t $StringView$$find(
  struct $StringView self$149,
  struct $StringView str$148
);

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$138,
  struct $StringView needle$140
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$124,
  struct $StringView needle$126
);

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$120,
  int32_t index$121
);

int32_t $StringView$$length(struct $StringView self$119);

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$117,
  int32_t index$118
);

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$115
);

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$114
);

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$113
);

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$112
);

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$111
);

moonbit_string_t $String$$escape(moonbit_string_t self$110);

int32_t $moonbitlang$core$builtin$op_notequal$1(
  moonbit_string_t x$107,
  moonbit_string_t y$108
);

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

moonbit_string_t $Error$to_string(void* _e$1133);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1151,
  int32_t _param$1150
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1148,
  struct $StringView _param$1147
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1145,
  moonbit_string_t _param$1142,
  int32_t _param$1143,
  int32_t _param$1144
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1140,
  moonbit_string_t _param$1139
);

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    104, 101, 108, 108, 111, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[56]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 55), 
    48, 48, 45, 48, 97, 102, 55, 54, 53, 49, 57, 49, 54, 99, 100, 52, 
    51, 100, 100, 56, 52, 52, 56, 101, 98, 50, 49, 49, 99, 56, 48, 51, 
    49, 57, 99, 45, 98, 55, 97, 100, 54, 98, 55, 49, 54, 57, 50, 48, 
    51, 51, 51, 49, 45, 48, 49, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_20 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    109, 105, 115, 115, 105, 110, 103, 45, 104, 101, 97, 100, 101, 114, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_34 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_54 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_48 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_35 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[19]; 
} const moonbit_string_literal_57 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 18), 
    119, 111, 114, 107, 105, 110, 103, 32, 97, 114, 105, 116, 104, 109, 
    101, 116, 105, 99, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_42 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_58 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    119, 111, 114, 107, 105, 110, 103, 32, 98, 111, 111, 108, 101, 97, 
    110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_68 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    119, 111, 114, 107, 105, 110, 103, 95, 116, 101, 115, 116, 115, 46, 
    109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_59 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    119, 111, 114, 107, 105, 110, 103, 32, 115, 116, 114, 105, 110, 103, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_27 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    98, 55, 97, 100, 54, 98, 55, 49, 54, 57, 50, 48, 51, 51, 51, 49, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    104, 116, 116, 112, 46, 114, 101, 113, 117, 101, 115, 116, 115, 46, 
    116, 111, 116, 97, 108, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_62 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    119, 111, 114, 107, 105, 110, 103, 32, 99, 111, 110, 116, 101, 120, 
    116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[98]; 
} const moonbit_string_literal_55 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 97), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 119, 111, 114, 107, 105, 110, 103, 95, 116, 101, 
    115, 116, 115, 46, 77, 111, 111, 110, 66, 105, 116, 84, 101, 115, 
    116, 68, 114, 105, 118, 101, 114, 73, 110, 116, 101, 114, 110, 97, 
    108, 74, 115, 69, 114, 114, 111, 114, 46, 77, 111, 111, 110, 66, 
    105, 116, 84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 73, 110, 
    116, 101, 114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_52 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    73, 110, 100, 101, 120, 79, 117, 116, 79, 102, 66, 111, 117, 110, 
    100, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    84, 101, 115, 116, 32, 108, 111, 103, 32, 109, 101, 115, 115, 97, 
    103, 101, 0
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
} const moonbit_string_literal_45 =
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
} const moonbit_string_literal_69 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    97, 122, 105, 109, 117, 116, 104, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_51 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 41, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_43 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_30 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_56 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 73, 110, 115, 
    112, 101, 99, 116, 69, 114, 114, 111, 114, 46, 73, 110, 115, 112, 
    101, 99, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_49 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    71, 69, 84, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_32 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 51, 54, 54, 58, 53, 45, 
    51, 54, 54, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    48, 97, 102, 55, 54, 53, 49, 57, 49, 54, 99, 100, 52, 51, 100, 100, 
    56, 52, 52, 56, 101, 98, 50, 49, 49, 99, 56, 48, 51, 49, 57, 99, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_2 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 47, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_25 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    116, 101, 115, 116, 95, 118, 97, 108, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_50 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_22 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    116, 101, 115, 116, 45, 109, 101, 116, 101, 114, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    45, 45, 45, 45, 45, 32, 66, 69, 71, 73, 78, 32, 77, 79, 79, 78, 32, 
    84, 69, 83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_36 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_1 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 45, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_63 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    119, 111, 114, 107, 105, 110, 103, 32, 109, 101, 116, 114, 105, 99, 
    115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[30]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 29), 
    104, 116, 116, 112, 115, 58, 47, 47, 97, 112, 105, 46, 101, 120, 
    97, 109, 112, 108, 101, 46, 99, 111, 109, 47, 117, 115, 101, 114, 
    115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_15 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    97, 112, 112, 108, 105, 99, 97, 116, 105, 111, 110, 47, 106, 115, 
    111, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[19]; 
} const moonbit_string_literal_64 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 18), 
    119, 111, 114, 107, 105, 110, 103, 32, 108, 111, 103, 32, 114, 101, 
    99, 111, 114, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_47 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 48, 58, 53, 45, 50, 
    57, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_44 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    73, 110, 118, 97, 108, 105, 100, 32, 105, 110, 100, 101, 120, 32, 
    102, 111, 114, 32, 86, 105, 101, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_61 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    119, 111, 114, 107, 105, 110, 103, 32, 115, 112, 97, 110, 32, 99, 
    111, 110, 116, 101, 120, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_46 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_60 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    119, 111, 114, 107, 105, 110, 103, 32, 111, 112, 116, 105, 111, 110, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    116, 114, 97, 99, 101, 112, 97, 114, 101, 110, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    92, 117, 123, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    116, 101, 115, 116, 46, 107, 101, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    73, 110, 118, 97, 108, 105, 100, 32, 115, 116, 97, 114, 116, 32, 
    105, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_37 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[25]; 
} const moonbit_string_literal_65 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 24), 
    119, 111, 114, 107, 105, 110, 103, 32, 116, 101, 120, 116, 32, 109, 
    97, 112, 32, 99, 97, 114, 114, 105, 101, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_66 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    119, 111, 114, 107, 105, 110, 103, 32, 104, 116, 116, 112, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_53 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    73, 110, 118, 97, 108, 105, 100, 73, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    67, 111, 110, 116, 101, 110, 116, 45, 84, 121, 112, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[25]; 
} const moonbit_string_literal_67 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 24), 
    119, 111, 114, 107, 105, 110, 103, 32, 99, 108, 111, 99, 107, 32, 
    97, 110, 100, 32, 114, 97, 110, 100, 111, 109, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_28 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    107, 101, 121, 49, 61, 118, 97, 108, 117, 101, 49, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9$dyncall
  };

struct { int32_t rc; uint32_t meta; struct $$3c$String$3e$$3d$$3e$Int data; 
} const $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$5
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3$dyncall
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

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$122;

int64_t $moonbitlang$core$builtin$brute_force_find$constr$136;

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$working_tests$moonbit_test_driver_internal_async_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$working_tests$moonbit_test_driver_internal_no_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$working_tests$moonbit_test_driver_internal_with_args_tests;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7$clo;

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2327
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2326
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2325
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2324
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2323
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2322
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2321
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2320
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2319
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2318
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0();
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2317
) {
  return $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2();
}

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1126
) {
  moonbit_decref(_tests$1126);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$1085 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1091 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$1098 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1091;
  int32_t moonbit_test_driver_internal_split_mbt_string$1103 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$2316 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$1110 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$1111;
  moonbit_string_t _tmp$2315;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$1112;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$1113;
  int32_t _len$1114;
  int32_t _i$1115;
  Moonbit_object_header(file_and_index$1110)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$1110->$0 = _tmp$2316;
  file_and_index$1110->$1 = 0;
  cli_args$1111
  = $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$1098
  );
  _tmp$2315 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$1111, 1);
  test_args$1112
  = $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$1103, _tmp$2315, 47
  );
  _arr$1113 = test_args$1112;
  moonbit_incref(_arr$1113);
  _len$1114 = $$moonbitlang$core$builtin$Array$$length$1(_arr$1113);
  _i$1115 = 0;
  while (1) {
    if (_i$1115 < _len$1114) {
      moonbit_string_t arg$1116;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$1117;
      moonbit_string_t file$1118;
      moonbit_string_t range$1119;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1120;
      moonbit_string_t _tmp$2313;
      int32_t start$1121;
      moonbit_string_t _tmp$2312;
      int32_t end$1122;
      int32_t i$1123;
      int32_t _tmp$2314;
      moonbit_incref(_arr$1113);
      arg$1116
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$1113, _i$1115
      );
      file_and_range$1117
      = $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1103, arg$1116, 58
      );
      moonbit_incref(file_and_range$1117);
      file$1118
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1117, 0
      );
      range$1119
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1117, 1
      );
      start_and_end$1120
      = $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1103, range$1119, 45
      );
      moonbit_incref(start_and_end$1120);
      _tmp$2313
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1120, 0
      );
      start$1121
      = $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1085, _tmp$2313
      );
      _tmp$2312
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1120, 1
      );
      end$1122
      = $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1085, _tmp$2312
      );
      i$1123 = start$1121;
      while (1) {
        if (i$1123 < end$1122) {
          struct $$3c$String$2a$Int$3e$* _tuple$2310;
          int32_t _tmp$2311;
          moonbit_incref(file$1118);
          _tuple$2310
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$2310)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$2310->$0 = file$1118;
          _tuple$2310->$1 = i$1123;
          moonbit_incref(file_and_index$1110);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$1110, _tuple$2310
          );
          _tmp$2311 = i$1123 + 1;
          i$1123 = _tmp$2311;
          continue;
        } else {
          moonbit_decref(file$1118);
        }
        break;
      }
      _tmp$2314 = _i$1115 + 1;
      _i$1115 = _tmp$2314;
      continue;
    } else {
      moonbit_decref(_arr$1113);
    }
    break;
  }
  return file_and_index$1110;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2291,
  moonbit_string_t s$1104,
  int32_t sep$1105
) {
  moonbit_string_t* _tmp$2309 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1106 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$1107;
  struct $Ref$3c$Int$3e$* start$1108;
  int32_t val$2304;
  int32_t _tmp$2305;
  Moonbit_object_header(res$1106)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$1106->$0 = _tmp$2309;
  res$1106->$1 = 0;
  i$1107
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1107)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1107->$0 = 0;
  start$1108
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$1108)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$1108->$0 = 0;
  while (1) {
    int32_t val$2292 = i$1107->$0;
    int32_t _tmp$2293 = Moonbit_array_length(s$1104);
    if (val$2292 < _tmp$2293) {
      int32_t val$2296 = i$1107->$0;
      int32_t _tmp$2295;
      int32_t _tmp$2294;
      int32_t val$2303;
      int32_t _tmp$2302;
      if (val$2296 < 0 || val$2296 >= Moonbit_array_length(s$1104)) {
        moonbit_panic();
      }
      _tmp$2295 = s$1104[val$2296];
      _tmp$2294 = _tmp$2295;
      if (_tmp$2294 == sep$1105) {
        int32_t val$2298 = start$1108->$0;
        int32_t val$2299 = i$1107->$0;
        moonbit_string_t _tmp$2297;
        int32_t val$2301;
        int32_t _tmp$2300;
        moonbit_incref(s$1104);
        _tmp$2297 = $String$$unsafe_substring(s$1104, val$2298, val$2299);
        moonbit_incref(res$1106);
        $$moonbitlang$core$builtin$Array$$push$0(res$1106, _tmp$2297);
        val$2301 = i$1107->$0;
        _tmp$2300 = val$2301 + 1;
        start$1108->$0 = _tmp$2300;
      }
      val$2303 = i$1107->$0;
      _tmp$2302 = val$2303 + 1;
      i$1107->$0 = _tmp$2302;
      continue;
    } else {
      moonbit_decref(i$1107);
    }
    break;
  }
  val$2304 = start$1108->$0;
  _tmp$2305 = Moonbit_array_length(s$1104);
  if (val$2304 < _tmp$2305) {
    int32_t _field$2328 = start$1108->$0;
    int32_t val$2307;
    int32_t _tmp$2308;
    moonbit_string_t _tmp$2306;
    moonbit_decref(start$1108);
    val$2307 = _field$2328;
    _tmp$2308 = Moonbit_array_length(s$1104);
    _tmp$2306 = $String$$unsafe_substring(s$1104, val$2307, _tmp$2308);
    moonbit_incref(res$1106);
    $$moonbitlang$core$builtin$Array$$push$0(res$1106, _tmp$2306);
  } else {
    moonbit_decref(start$1108);
    moonbit_decref(s$1104);
  }
  return res$1106;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1091
) {
  moonbit_bytes_t* tmp$1099 =
    $azimuth$telemetry$working_tests$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$2290 = Moonbit_array_length(tmp$1099);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1100 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$2290);
  int32_t i$1101 = 0;
  while (1) {
    int32_t _tmp$2286 = Moonbit_array_length(tmp$1099);
    if (i$1101 < _tmp$2286) {
      moonbit_bytes_t _tmp$2329;
      moonbit_bytes_t _tmp$2288;
      moonbit_string_t _tmp$2287;
      int32_t _tmp$2289;
      if (i$1101 < 0 || i$1101 >= Moonbit_array_length(tmp$1099)) {
        moonbit_panic();
      }
      _tmp$2329 = (moonbit_bytes_t)tmp$1099[i$1101];
      _tmp$2288 = _tmp$2329;
      moonbit_incref(_tmp$2288);
      _tmp$2287
      = $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1091, _tmp$2288
      );
      moonbit_incref(res$1100);
      $$moonbitlang$core$builtin$Array$$push$0(res$1100, _tmp$2287);
      _tmp$2289 = i$1101 + 1;
      i$1101 = _tmp$2289;
      continue;
    } else {
      moonbit_decref(tmp$1099);
    }
    break;
  }
  return res$1100;
}

moonbit_string_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2200,
  moonbit_bytes_t bytes$1092
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$1093 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$1094 = Moonbit_array_length(bytes$1092);
  struct $Ref$3c$Int$3e$* i$1095 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1095)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1095->$0 = 0;
  while (1) {
    int32_t val$2201 = i$1095->$0;
    if (val$2201 < len$1094) {
      int32_t val$2285 = i$1095->$0;
      int32_t _tmp$2284;
      int32_t _tmp$2283;
      struct $Ref$3c$Int$3e$* c$1096;
      int32_t val$2202;
      if (val$2285 < 0 || val$2285 >= Moonbit_array_length(bytes$1092)) {
        moonbit_panic();
      }
      _tmp$2284 = bytes$1092[val$2285];
      _tmp$2283 = (int32_t)_tmp$2284;
      c$1096
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$1096)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$1096->$0 = _tmp$2283;
      val$2202 = c$1096->$0;
      if (val$2202 < 128) {
        int32_t _field$2330 = c$1096->$0;
        int32_t val$2204;
        int32_t _tmp$2203;
        int32_t val$2206;
        int32_t _tmp$2205;
        moonbit_decref(c$1096);
        val$2204 = _field$2330;
        _tmp$2203 = val$2204;
        moonbit_incref(res$1093);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$1093, _tmp$2203
        );
        val$2206 = i$1095->$0;
        _tmp$2205 = val$2206 + 1;
        i$1095->$0 = _tmp$2205;
      } else {
        int32_t val$2207 = c$1096->$0;
        if (val$2207 < 224) {
          int32_t val$2209 = i$1095->$0;
          int32_t _tmp$2208 = val$2209 + 1;
          int32_t val$2218;
          int32_t _tmp$2217;
          int32_t _tmp$2211;
          int32_t val$2216;
          int32_t _tmp$2215;
          int32_t _tmp$2214;
          int32_t _tmp$2213;
          int32_t _tmp$2212;
          int32_t _tmp$2210;
          int32_t _field$2331;
          int32_t val$2220;
          int32_t _tmp$2219;
          int32_t val$2222;
          int32_t _tmp$2221;
          if (_tmp$2208 >= len$1094) {
            moonbit_decref(c$1096);
            moonbit_decref(i$1095);
            moonbit_decref(bytes$1092);
            break;
          }
          val$2218 = c$1096->$0;
          _tmp$2217 = val$2218 & 31;
          _tmp$2211 = _tmp$2217 << 6;
          val$2216 = i$1095->$0;
          _tmp$2215 = val$2216 + 1;
          if (_tmp$2215 < 0 || _tmp$2215 >= Moonbit_array_length(bytes$1092)) {
            moonbit_panic();
          }
          _tmp$2214 = bytes$1092[_tmp$2215];
          _tmp$2213 = (int32_t)_tmp$2214;
          _tmp$2212 = _tmp$2213 & 63;
          _tmp$2210 = _tmp$2211 | _tmp$2212;
          c$1096->$0 = _tmp$2210;
          _field$2331 = c$1096->$0;
          moonbit_decref(c$1096);
          val$2220 = _field$2331;
          _tmp$2219 = val$2220;
          moonbit_incref(res$1093);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$1093, _tmp$2219
          );
          val$2222 = i$1095->$0;
          _tmp$2221 = val$2222 + 2;
          i$1095->$0 = _tmp$2221;
        } else {
          int32_t val$2223 = c$1096->$0;
          if (val$2223 < 240) {
            int32_t val$2225 = i$1095->$0;
            int32_t _tmp$2224 = val$2225 + 2;
            int32_t val$2241;
            int32_t _tmp$2240;
            int32_t _tmp$2233;
            int32_t val$2239;
            int32_t _tmp$2238;
            int32_t _tmp$2237;
            int32_t _tmp$2236;
            int32_t _tmp$2235;
            int32_t _tmp$2234;
            int32_t _tmp$2227;
            int32_t val$2232;
            int32_t _tmp$2231;
            int32_t _tmp$2230;
            int32_t _tmp$2229;
            int32_t _tmp$2228;
            int32_t _tmp$2226;
            int32_t _field$2332;
            int32_t val$2243;
            int32_t _tmp$2242;
            int32_t val$2245;
            int32_t _tmp$2244;
            if (_tmp$2224 >= len$1094) {
              moonbit_decref(c$1096);
              moonbit_decref(i$1095);
              moonbit_decref(bytes$1092);
              break;
            }
            val$2241 = c$1096->$0;
            _tmp$2240 = val$2241 & 15;
            _tmp$2233 = _tmp$2240 << 12;
            val$2239 = i$1095->$0;
            _tmp$2238 = val$2239 + 1;
            if (
              _tmp$2238 < 0 || _tmp$2238 >= Moonbit_array_length(bytes$1092)
            ) {
              moonbit_panic();
            }
            _tmp$2237 = bytes$1092[_tmp$2238];
            _tmp$2236 = (int32_t)_tmp$2237;
            _tmp$2235 = _tmp$2236 & 63;
            _tmp$2234 = _tmp$2235 << 6;
            _tmp$2227 = _tmp$2233 | _tmp$2234;
            val$2232 = i$1095->$0;
            _tmp$2231 = val$2232 + 2;
            if (
              _tmp$2231 < 0 || _tmp$2231 >= Moonbit_array_length(bytes$1092)
            ) {
              moonbit_panic();
            }
            _tmp$2230 = bytes$1092[_tmp$2231];
            _tmp$2229 = (int32_t)_tmp$2230;
            _tmp$2228 = _tmp$2229 & 63;
            _tmp$2226 = _tmp$2227 | _tmp$2228;
            c$1096->$0 = _tmp$2226;
            _field$2332 = c$1096->$0;
            moonbit_decref(c$1096);
            val$2243 = _field$2332;
            _tmp$2242 = val$2243;
            moonbit_incref(res$1093);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1093, _tmp$2242
            );
            val$2245 = i$1095->$0;
            _tmp$2244 = val$2245 + 3;
            i$1095->$0 = _tmp$2244;
          } else {
            int32_t val$2247 = i$1095->$0;
            int32_t _tmp$2246 = val$2247 + 3;
            int32_t val$2270;
            int32_t _tmp$2269;
            int32_t _tmp$2262;
            int32_t val$2268;
            int32_t _tmp$2267;
            int32_t _tmp$2266;
            int32_t _tmp$2265;
            int32_t _tmp$2264;
            int32_t _tmp$2263;
            int32_t _tmp$2255;
            int32_t val$2261;
            int32_t _tmp$2260;
            int32_t _tmp$2259;
            int32_t _tmp$2258;
            int32_t _tmp$2257;
            int32_t _tmp$2256;
            int32_t _tmp$2249;
            int32_t val$2254;
            int32_t _tmp$2253;
            int32_t _tmp$2252;
            int32_t _tmp$2251;
            int32_t _tmp$2250;
            int32_t _tmp$2248;
            int32_t val$2272;
            int32_t _tmp$2271;
            int32_t val$2276;
            int32_t _tmp$2275;
            int32_t _tmp$2274;
            int32_t _tmp$2273;
            int32_t _field$2333;
            int32_t val$2280;
            int32_t _tmp$2279;
            int32_t _tmp$2278;
            int32_t _tmp$2277;
            int32_t val$2282;
            int32_t _tmp$2281;
            if (_tmp$2246 >= len$1094) {
              moonbit_decref(c$1096);
              moonbit_decref(i$1095);
              moonbit_decref(bytes$1092);
              break;
            }
            val$2270 = c$1096->$0;
            _tmp$2269 = val$2270 & 7;
            _tmp$2262 = _tmp$2269 << 18;
            val$2268 = i$1095->$0;
            _tmp$2267 = val$2268 + 1;
            if (
              _tmp$2267 < 0 || _tmp$2267 >= Moonbit_array_length(bytes$1092)
            ) {
              moonbit_panic();
            }
            _tmp$2266 = bytes$1092[_tmp$2267];
            _tmp$2265 = (int32_t)_tmp$2266;
            _tmp$2264 = _tmp$2265 & 63;
            _tmp$2263 = _tmp$2264 << 12;
            _tmp$2255 = _tmp$2262 | _tmp$2263;
            val$2261 = i$1095->$0;
            _tmp$2260 = val$2261 + 2;
            if (
              _tmp$2260 < 0 || _tmp$2260 >= Moonbit_array_length(bytes$1092)
            ) {
              moonbit_panic();
            }
            _tmp$2259 = bytes$1092[_tmp$2260];
            _tmp$2258 = (int32_t)_tmp$2259;
            _tmp$2257 = _tmp$2258 & 63;
            _tmp$2256 = _tmp$2257 << 6;
            _tmp$2249 = _tmp$2255 | _tmp$2256;
            val$2254 = i$1095->$0;
            _tmp$2253 = val$2254 + 3;
            if (
              _tmp$2253 < 0 || _tmp$2253 >= Moonbit_array_length(bytes$1092)
            ) {
              moonbit_panic();
            }
            _tmp$2252 = bytes$1092[_tmp$2253];
            _tmp$2251 = (int32_t)_tmp$2252;
            _tmp$2250 = _tmp$2251 & 63;
            _tmp$2248 = _tmp$2249 | _tmp$2250;
            c$1096->$0 = _tmp$2248;
            val$2272 = c$1096->$0;
            _tmp$2271 = val$2272 - 65536;
            c$1096->$0 = _tmp$2271;
            val$2276 = c$1096->$0;
            _tmp$2275 = val$2276 >> 10;
            _tmp$2274 = _tmp$2275 + 55296;
            _tmp$2273 = _tmp$2274;
            moonbit_incref(res$1093);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1093, _tmp$2273
            );
            _field$2333 = c$1096->$0;
            moonbit_decref(c$1096);
            val$2280 = _field$2333;
            _tmp$2279 = val$2280 & 1023;
            _tmp$2278 = _tmp$2279 + 56320;
            _tmp$2277 = _tmp$2278;
            moonbit_incref(res$1093);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1093, _tmp$2277
            );
            val$2282 = i$1095->$0;
            _tmp$2281 = val$2282 + 4;
            i$1095->$0 = _tmp$2281;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$1095);
      moonbit_decref(bytes$1092);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$1093);
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$2193,
  moonbit_string_t s$1086
) {
  struct $Ref$3c$Int$3e$* res$1087 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$1088;
  int32_t i$1089;
  int32_t _field$2334;
  Moonbit_object_header(res$1087)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$1087->$0 = 0;
  len$1088 = Moonbit_array_length(s$1086);
  i$1089 = 0;
  while (1) {
    if (i$1089 < len$1088) {
      int32_t val$2198 = res$1087->$0;
      int32_t _tmp$2195 = val$2198 * 10;
      int32_t _tmp$2197;
      int32_t _tmp$2196;
      int32_t _tmp$2194;
      int32_t _tmp$2199;
      if (i$1089 < 0 || i$1089 >= Moonbit_array_length(s$1086)) {
        moonbit_panic();
      }
      _tmp$2197 = s$1086[i$1089];
      _tmp$2196 = _tmp$2197 - 48;
      _tmp$2194 = _tmp$2195 + _tmp$2196;
      res$1087->$0 = _tmp$2194;
      _tmp$2199 = i$1089 + 1;
      i$1089 = _tmp$2199;
      continue;
    } else {
      moonbit_decref(s$1086);
    }
    break;
  }
  _field$2334 = res$1087->$0;
  moonbit_decref(res$1087);
  return _field$2334;
}

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1049,
  moonbit_string_t filename$1010,
  int32_t index$1011
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$1009;
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$2830;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1012;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$1021;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2344;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$2192;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2343;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$1022;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2342;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$2191;
  moonbit_string_t _field$2341;
  moonbit_string_t file_name$1023;
  moonbit_string_t name$1024;
  int32_t _tmp$2188;
  moonbit_string_t name$1025;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$2145;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$2146;
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$2832;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1032;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1048;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1073;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1075;
  void* _field$2338;
  int32_t _cnt$2649;
  void* _bind$1076;
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$2836;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$2185;
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$2837;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2178;
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$2838;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$2179;
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$2839;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2163;
  moonbit_incref(
    $azimuth$telemetry$working_tests$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$working_tests$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$working_tests$moonbit_test_driver_internal_async_tests
  );
  filtered_test$1009
  = $azimuth$telemetry$working_tests$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$working_tests$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$working_tests$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$working_tests$moonbit_test_driver_internal_async_tests,
      filename$1010,
      index$1011
  );
  _closure$2830
  = (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$2830)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$2830->code
  = &$$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$2830->$0 = index$1011;
  handle_result$1012
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$2830;
  if (filtered_test$1009 == 0) {
    moonbit_decref(async_tests$1049);
    if (filtered_test$1009) {
      moonbit_decref(filtered_test$1009);
    }
    $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1012,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$1083 =
      filtered_test$1009;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$1084 = _Some$1083;
    item$1021 = _item$1084;
    goto $join$1020;
  }
  goto $joinlet$2831;
  $join$1020:;
  _field$2344 = item$1021->$1;
  meta$2192 = _field$2344;
  _field$2343 = meta$2192->$2;
  attrs$1022 = _field$2343;
  _field$2342 = item$1021->$1;
  meta$2191 = _field$2342;
  _field$2341 = meta$2191->$0;
  file_name$1023 = _field$2341;
  moonbit_incref(attrs$1022);
  moonbit_incref(file_name$1023);
  moonbit_incref(attrs$1022);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$1022)) {
    name$1024 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$1022);
    name$1024 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1022, 0);
  }
  _tmp$2188 = Moonbit_array_length(name$1024);
  if (_tmp$2188 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2340;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$2190;
    int32_t _field$2339;
    int32_t index$2189;
    moonbit_decref(name$1024);
    _field$2340 = item$1021->$1;
    meta$2190 = _field$2340;
    _field$2339 = meta$2190->$1;
    index$2189 = _field$2339;
    name$1025 = $Int$$to_string$inner(index$2189, 10);
  } else {
    name$1025 = name$1024;
  }
  moonbit_incref(attrs$1022);
  _tmp$2145 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$1022);
  _tmp$2146
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$2145, _tmp$2146)) {
    moonbit_string_t _tmp$2160;
    moonbit_string_t _tmp$2159;
    moonbit_string_t _tmp$2156;
    moonbit_string_t _tmp$2158;
    moonbit_string_t _tmp$2157;
    moonbit_string_t _tmp$2155;
    moonbit_decref(async_tests$1049);
    moonbit_decref(item$1021);
    moonbit_incref(file_name$1023);
    _tmp$2160
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$1023
    );
    _tmp$2159
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$2160
    );
    _tmp$2156
    = moonbit_add_string(
      _tmp$2159, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$2158 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1022, 0);
    _tmp$2157 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$2158);
    _tmp$2155 = moonbit_add_string(_tmp$2156, _tmp$2157);
    $moonbitlang$core$builtin$println$0(_tmp$2155);
    $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1012,
        name$1025,
        file_name$1023,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$1022);
  }
  moonbit_incref(name$1025);
  moonbit_incref(file_name$1023);
  moonbit_incref(handle_result$1012);
  _closure$2832
  = (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$2832)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2832->code
  = &$$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$2832->$0 = name$1025;
  _closure$2832->$1 = file_name$1023;
  _closure$2832->$2 = handle_result$1012;
  on_err$1032 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2832;
  _field$2338 = item$1021->$0;
  _cnt$2649 = Moonbit_object_header(item$1021)->rc;
  if (_cnt$2649 > 1) {
    int32_t _new_cnt$2651;
    moonbit_incref(_field$2338);
    _new_cnt$2651 = _cnt$2649 - 1;
    Moonbit_object_header(item$1021)->rc = _new_cnt$2651;
  } else if (_cnt$2649 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2650 = item$1021->$1;
    moonbit_decref(_field$2650);
    moonbit_free(item$1021);
  }
  _bind$1076 = _field$2338;
  switch (Moonbit_object_tag(_bind$1076)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$1077;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2335;
      int32_t _cnt$2652;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1078;
      moonbit_decref(async_tests$1049);
      _F0$1077 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$1076;
      _field$2335 = _F0$1077->$0;
      _cnt$2652 = Moonbit_object_header(_F0$1077)->rc;
      if (_cnt$2652 > 1) {
        int32_t _new_cnt$2653;
        moonbit_incref(_field$2335);
        _new_cnt$2653 = _cnt$2652 - 1;
        Moonbit_object_header(_F0$1077)->rc = _new_cnt$2653;
      } else if (_cnt$2652 == 1) {
        moonbit_free(_F0$1077);
      }
      _f$1078 = _field$2335;
      f$1075 = _f$1078;
      goto $join$1074;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$1079;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2336;
      int32_t _cnt$2654;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1080;
      moonbit_decref(async_tests$1049);
      _F1$1079 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$1076;
      _field$2336 = _F1$1079->$0;
      _cnt$2654 = Moonbit_object_header(_F1$1079)->rc;
      if (_cnt$2654 > 1) {
        int32_t _new_cnt$2655;
        moonbit_incref(_field$2336);
        _new_cnt$2655 = _cnt$2654 - 1;
        Moonbit_object_header(_F1$1079)->rc = _new_cnt$2655;
      } else if (_cnt$2654 == 1) {
        moonbit_free(_F1$1079);
      }
      _f$1080 = _field$2336;
      f$1073 = _f$1080;
      goto $join$1072;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$1081 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$1076;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2337 =
        _F2$1081->$0;
      int32_t _cnt$2656 = Moonbit_object_header(_F2$1081)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$1082;
      if (_cnt$2656 > 1) {
        int32_t _new_cnt$2657;
        moonbit_incref(_field$2337);
        _new_cnt$2657 = _cnt$2656 - 1;
        Moonbit_object_header(_F2$1081)->rc = _new_cnt$2657;
      } else if (_cnt$2656 == 1) {
        moonbit_free(_F2$1081);
      }
      _f$1082 = _field$2337;
      f$1048 = _f$1082;
      goto $join$1047;
      break;
    }
  }
  goto $joinlet$2835;
  $join$1074:;
  _closure$2836
  = (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$2836)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2836->code
  = &$$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$2836->$0 = name$1025;
  _closure$2836->$1 = file_name$1023;
  _closure$2836->$2 = handle_result$1012;
  _tmp$2185 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2836;
  $azimuth$telemetry$working_tests$moonbit_test_driver_internal_catch_error(
    f$1075, _tmp$2185, on_err$1032
  );
  $joinlet$2835:;
  goto $joinlet$2834;
  $join$1072:;
  moonbit_incref(name$1025);
  _closure$2837
  = (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$2837)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$2837->code
  = &$$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$2837->$0 = f$1073;
  _closure$2837->$1 = name$1025;
  _tmp$2178
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$2837;
  _closure$2838
  = (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$2838)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2838->code
  = &$$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$2838->$0 = name$1025;
  _closure$2838->$1 = file_name$1023;
  _closure$2838->$2 = handle_result$1012;
  _tmp$2179 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2838;
  $azimuth$telemetry$working_tests$moonbit_test_driver_internal_catch_error(
    _tmp$2178, _tmp$2179, on_err$1032
  );
  $joinlet$2834:;
  goto $joinlet$2833;
  $join$1047:;
  _closure$2839
  = (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$2839)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$2839->code
  = &$$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$2839->$0 = f$1048;
  _closure$2839->$1 = on_err$1032;
  _closure$2839->$2 = name$1025;
  _closure$2839->$3 = file_name$1023;
  _closure$2839->$4 = handle_result$1012;
  _tmp$2163
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$2839;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$1049, _tmp$2163);
  $joinlet$2833:;
  $joinlet$2831:;
  return 0;
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2186
) {
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$2187 =
    (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$2186;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2347 =
    _casted_env$2187->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1012 =
    _field$2347;
  moonbit_string_t _field$2346 = _casted_env$2187->$1;
  moonbit_string_t file_name$1023 = _field$2346;
  moonbit_string_t _field$2345 = _casted_env$2187->$0;
  int32_t _cnt$2658 = Moonbit_object_header(_casted_env$2187)->rc;
  moonbit_string_t name$1025;
  if (_cnt$2658 > 1) {
    int32_t _new_cnt$2659;
    moonbit_incref(handle_result$1012);
    moonbit_incref(file_name$1023);
    moonbit_incref(_field$2345);
    _new_cnt$2659 = _cnt$2658 - 1;
    Moonbit_object_header(_casted_env$2187)->rc = _new_cnt$2659;
  } else if (_cnt$2658 == 1) {
    moonbit_free(_casted_env$2187);
  }
  name$1025 = _field$2345;
  $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1012,
      name$1025,
      file_name$1023,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2182
) {
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$2183 =
    (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$2182;
  moonbit_string_t _field$2349 = _casted_env$2183->$1;
  moonbit_string_t name$1025 = _field$2349;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2348 =
    _casted_env$2183->$0;
  int32_t _cnt$2660 = Moonbit_object_header(_casted_env$2183)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1073;
  int32_t _tmp$2184;
  if (_cnt$2660 > 1) {
    int32_t _new_cnt$2661;
    moonbit_incref(name$1025);
    moonbit_incref(_field$2348);
    _new_cnt$2661 = _cnt$2660 - 1;
    Moonbit_object_header(_casted_env$2183)->rc = _new_cnt$2661;
  } else if (_cnt$2660 == 1) {
    moonbit_free(_casted_env$2183);
  }
  f$1073 = _field$2348;
  _tmp$2184
  = $azimuth$telemetry$working_tests$moonbit_test_driver_internal_new_test_arg(
    name$1025
  );
  return f$1073->code(f$1073, _tmp$2184);
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2180
) {
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$2181 =
    (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$2180;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2352 =
    _casted_env$2181->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1012 =
    _field$2352;
  moonbit_string_t _field$2351 = _casted_env$2181->$1;
  moonbit_string_t file_name$1023 = _field$2351;
  moonbit_string_t _field$2350 = _casted_env$2181->$0;
  int32_t _cnt$2662 = Moonbit_object_header(_casted_env$2181)->rc;
  moonbit_string_t name$1025;
  if (_cnt$2662 > 1) {
    int32_t _new_cnt$2663;
    moonbit_incref(handle_result$1012);
    moonbit_incref(file_name$1023);
    moonbit_incref(_field$2350);
    _new_cnt$2663 = _cnt$2662 - 1;
    Moonbit_object_header(_casted_env$2181)->rc = _new_cnt$2663;
  } else if (_cnt$2662 == 1) {
    moonbit_free(_casted_env$2181);
  }
  name$1025 = _field$2350;
  $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1012,
      name$1025,
      file_name$1023,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2164,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1050,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1051
) {
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$2165 =
    (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$2164;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2357 =
    _casted_env$2165->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1012 =
    _field$2357;
  moonbit_string_t _field$2356 = _casted_env$2165->$3;
  moonbit_string_t file_name$1023 = _field$2356;
  moonbit_string_t _field$2355 = _casted_env$2165->$2;
  moonbit_string_t name$1025 = _field$2355;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2354 = _casted_env$2165->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1032 = _field$2354;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2353 =
    _casted_env$2165->$0;
  int32_t _cnt$2664 = Moonbit_object_header(_casted_env$2165)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1048;
  int32_t _async_driver$1052;
  int32_t _tmp$2169;
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$2840;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$2170;
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$2841;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$2171;
  if (_cnt$2664 > 1) {
    int32_t _new_cnt$2665;
    moonbit_incref(handle_result$1012);
    moonbit_incref(file_name$1023);
    moonbit_incref(name$1025);
    moonbit_incref(on_err$1032);
    moonbit_incref(_field$2353);
    _new_cnt$2665 = _cnt$2664 - 1;
    Moonbit_object_header(_casted_env$2165)->rc = _new_cnt$2665;
  } else if (_cnt$2664 == 1) {
    moonbit_free(_casted_env$2165);
  }
  f$1048 = _field$2353;
  _async_driver$1052 = 0;
  moonbit_incref(name$1025);
  _tmp$2169
  = $azimuth$telemetry$working_tests$moonbit_test_driver_internal_new_test_arg(
    name$1025
  );
  moonbit_incref(_cont$1050);
  _closure$2840
  = (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$2840)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$2840->code
  = &$$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$2840->$0 = _async_driver$1052;
  _closure$2840->$1 = _cont$1050;
  _closure$2840->$2 = name$1025;
  _closure$2840->$3 = file_name$1023;
  _closure$2840->$4 = handle_result$1012;
  _tmp$2170 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$2840;
  _closure$2841
  = (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$2841)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$2841->code
  = &$$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$2841->$0 = _async_driver$1052;
  _closure$2841->$1 = _err_cont$1051;
  _closure$2841->$2 = _cont$1050;
  _closure$2841->$3 = on_err$1032;
  _tmp$2171 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2841;
  f$1048->code(f$1048, _tmp$2169, _tmp$2170, _tmp$2171);
  return 0;
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2175,
  int32_t _cont_param$1070
) {
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$2176 =
    (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$2175;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2362 =
    _casted_env$2176->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1012 =
    _field$2362;
  moonbit_string_t _field$2361 = _casted_env$2176->$3;
  moonbit_string_t file_name$1023 = _field$2361;
  moonbit_string_t _field$2360 = _casted_env$2176->$2;
  moonbit_string_t name$1025 = _field$2360;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2359 = _casted_env$2176->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1050 = _field$2359;
  int32_t _field$2358 = _casted_env$2176->$0;
  int32_t _cnt$2666 = Moonbit_object_header(_casted_env$2176)->rc;
  int32_t _async_driver$1052;
  void* State_1$2177;
  if (_cnt$2666 > 1) {
    int32_t _new_cnt$2667;
    moonbit_incref(handle_result$1012);
    moonbit_incref(file_name$1023);
    moonbit_incref(name$1025);
    moonbit_incref(_cont$1050);
    _new_cnt$2667 = _cnt$2666 - 1;
    Moonbit_object_header(_casted_env$2176)->rc = _new_cnt$2667;
  } else if (_cnt$2666 == 1) {
    moonbit_free(_casted_env$2176);
  }
  _async_driver$1052 = _field$2358;
  State_1$2177
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1
      )
    );
  Moonbit_object_header(State_1$2177)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1*)State_1$2177)->$0
  = _cont_param$1070;
  ((struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1*)State_1$2177)->$1
  = handle_result$1012;
  ((struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1*)State_1$2177)->$2
  = file_name$1023;
  ((struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1*)State_1$2177)->$3
  = name$1025;
  ((struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1*)State_1$2177)->$4
  = _cont$1050;
  $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1052, State_1$2177
  );
  return 0;
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2172,
  void* _cont_param$1071
) {
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$2173 =
    (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$2172;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2366 = _casted_env$2173->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1032 = _field$2366;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2365 = _casted_env$2173->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1050 = _field$2365;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2364 = _casted_env$2173->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1051 = _field$2364;
  int32_t _field$2363 = _casted_env$2173->$0;
  int32_t _cnt$2668 = Moonbit_object_header(_casted_env$2173)->rc;
  int32_t _async_driver$1052;
  void* _try$514$2174;
  if (_cnt$2668 > 1) {
    int32_t _new_cnt$2669;
    moonbit_incref(on_err$1032);
    moonbit_incref(_cont$1050);
    moonbit_incref(_err_cont$1051);
    _new_cnt$2669 = _cnt$2668 - 1;
    Moonbit_object_header(_casted_env$2173)->rc = _new_cnt$2669;
  } else if (_cnt$2668 == 1) {
    moonbit_free(_casted_env$2173);
  }
  _async_driver$1052 = _field$2363;
  _try$514$2174
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514
      )
    );
  Moonbit_object_header(_try$514$2174)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514*)_try$514$2174)->$0
  = _cont_param$1071;
  ((struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514*)_try$514$2174)->$1
  = on_err$1032;
  ((struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514*)_try$514$2174)->$2
  = _cont$1050;
  ((struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514*)_try$514$2174)->$3
  = _err_cont$1051;
  $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1052, _try$514$2174
  );
  return 0;
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$2166,
  void* _state$1053
) {
  switch (Moonbit_object_tag(_state$1053)) {
    case 0: {
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514* _$2a$try$514$1054 =
        (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$$2a$try$514*)_state$1053;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2370 = _$2a$try$514$1054->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1055 = _field$2370;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2369 = _$2a$try$514$1054->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1056 = _field$2369;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2368 = _$2a$try$514$1054->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1057 = _field$2368;
      void* _field$2367 = _$2a$try$514$1054->$0;
      int32_t _cnt$2670 = Moonbit_object_header(_$2a$try$514$1054)->rc;
      void* _try_err$1058;
      void* err$1060;
      void* err$1062;
      int32_t _tmp$2168;
      if (_cnt$2670 > 1) {
        int32_t _new_cnt$2671;
        moonbit_incref(_err_cont$1055);
        moonbit_incref(_cont$1056);
        moonbit_incref(on_err$1057);
        moonbit_incref(_field$2367);
        _new_cnt$2671 = _cnt$2670 - 1;
        Moonbit_object_header(_$2a$try$514$1054)->rc = _new_cnt$2671;
      } else if (_cnt$2670 == 1) {
        moonbit_free(_$2a$try$514$1054);
      }
      _try_err$1058 = _field$2367;
      if (
        $azimuth$telemetry$working_tests$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$1057);
        moonbit_decref(_cont$1056);
        err$1062 = _try_err$1058;
        goto $join$1061;
      } else {
        moonbit_decref(_err_cont$1055);
        err$1060 = _try_err$1058;
        goto $join$1059;
      }
      goto $joinlet$2843;
      $join$1061:;
      return _err_cont$1055->code(_err_cont$1055, err$1062);
      $joinlet$2843:;
      goto $joinlet$2842;
      $join$1059:;
      _tmp$2168 = on_err$1057->code(on_err$1057, err$1060);
      _cont$1056->code(_cont$1056, _tmp$2168);
      $joinlet$2842:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1* _State_1$1063 =
        (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$530$on_err$68$$2a$arm$522$lambda$548$State$State_1*)_state$1053;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2374 = _State_1$1063->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1064 = _field$2374;
      moonbit_string_t _field$2373 = _State_1$1063->$3;
      moonbit_string_t name$1065 = _field$2373;
      moonbit_string_t _field$2372 = _State_1$1063->$2;
      moonbit_string_t file_name$1066 = _field$2372;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2371 =
        _State_1$1063->$1;
      int32_t _cnt$2672 = Moonbit_object_header(_State_1$1063)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1067;
      int32_t _tmp$2167;
      if (_cnt$2672 > 1) {
        int32_t _new_cnt$2673;
        moonbit_incref(_cont$1064);
        moonbit_incref(name$1065);
        moonbit_incref(file_name$1066);
        moonbit_incref(_field$2371);
        _new_cnt$2673 = _cnt$2672 - 1;
        Moonbit_object_header(_State_1$1063)->rc = _new_cnt$2673;
      } else if (_cnt$2672 == 1) {
        moonbit_free(_State_1$1063);
      }
      handle_result$1067 = _field$2371;
      _tmp$2167
      = handle_result$1067->code(
        handle_result$1067,
          name$1065,
          file_name$1066,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$1064->code(_cont$1064, _tmp$2167);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2161,
  void* err$1033
) {
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$2162 =
    (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$2161;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2381 =
    _casted_env$2162->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1012 =
    _field$2381;
  moonbit_string_t _field$2380 = _casted_env$2162->$1;
  moonbit_string_t file_name$1023 = _field$2380;
  moonbit_string_t _field$2379 = _casted_env$2162->$0;
  int32_t _cnt$2674 = Moonbit_object_header(_casted_env$2162)->rc;
  moonbit_string_t name$1025;
  void* e$1035;
  moonbit_string_t e$1038;
  moonbit_string_t message$1036;
  if (_cnt$2674 > 1) {
    int32_t _new_cnt$2675;
    moonbit_incref(handle_result$1012);
    moonbit_incref(file_name$1023);
    moonbit_incref(_field$2379);
    _new_cnt$2675 = _cnt$2674 - 1;
    Moonbit_object_header(_casted_env$2162)->rc = _new_cnt$2675;
  } else if (_cnt$2674 == 1) {
    moonbit_free(_casted_env$2162);
  }
  name$1025 = _field$2379;
  switch (Moonbit_object_tag(err$1033)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$1039 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$1033;
      moonbit_string_t _field$2375 = _Failure$1039->$0;
      int32_t _cnt$2676 = Moonbit_object_header(_Failure$1039)->rc;
      moonbit_string_t _e$1040;
      if (_cnt$2676 > 1) {
        int32_t _new_cnt$2677;
        moonbit_incref(_field$2375);
        _new_cnt$2677 = _cnt$2676 - 1;
        Moonbit_object_header(_Failure$1039)->rc = _new_cnt$2677;
      } else if (_cnt$2676 == 1) {
        moonbit_free(_Failure$1039);
      }
      _e$1040 = _field$2375;
      e$1038 = _e$1040;
      goto $join$1037;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$1041 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$1033;
      moonbit_string_t _field$2376 = _InspectError$1041->$0;
      int32_t _cnt$2678 = Moonbit_object_header(_InspectError$1041)->rc;
      moonbit_string_t _e$1042;
      if (_cnt$2678 > 1) {
        int32_t _new_cnt$2679;
        moonbit_incref(_field$2376);
        _new_cnt$2679 = _cnt$2678 - 1;
        Moonbit_object_header(_InspectError$1041)->rc = _new_cnt$2679;
      } else if (_cnt$2678 == 1) {
        moonbit_free(_InspectError$1041);
      }
      _e$1042 = _field$2376;
      e$1038 = _e$1042;
      goto $join$1037;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$1043 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$1033;
      moonbit_string_t _field$2377 = _SnapshotError$1043->$0;
      int32_t _cnt$2680 = Moonbit_object_header(_SnapshotError$1043)->rc;
      moonbit_string_t _e$1044;
      if (_cnt$2680 > 1) {
        int32_t _new_cnt$2681;
        moonbit_incref(_field$2377);
        _new_cnt$2681 = _cnt$2680 - 1;
        Moonbit_object_header(_SnapshotError$1043)->rc = _new_cnt$2681;
      } else if (_cnt$2680 == 1) {
        moonbit_free(_SnapshotError$1043);
      }
      _e$1044 = _field$2377;
      e$1038 = _e$1044;
      goto $join$1037;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$working_tests$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$1045 =
        (struct $Error$azimuth$telemetry$working_tests$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$1033;
      moonbit_string_t _field$2378 =
        _MoonBitTestDriverInternalJsError$1045->$0;
      int32_t _cnt$2682 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1045)->rc;
      moonbit_string_t _e$1046;
      if (_cnt$2682 > 1) {
        int32_t _new_cnt$2683;
        moonbit_incref(_field$2378);
        _new_cnt$2683 = _cnt$2682 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1045)->rc
        = _new_cnt$2683;
      } else if (_cnt$2682 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$1045);
      }
      _e$1046 = _field$2378;
      e$1038 = _e$1046;
      goto $join$1037;
      break;
    }
    default: {
      e$1035 = err$1033;
      goto $join$1034;
      break;
    }
  }
  goto $joinlet$2845;
  $join$1037:;
  $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1012, name$1025, file_name$1023, e$1038, 0
  );
  $joinlet$2845:;
  goto $joinlet$2844;
  $join$1034:;
  message$1036 = $Error$to_string(e$1035);
  $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1012, name$1025, file_name$1023, message$1036, 0
  );
  $joinlet$2844:;
  return 0;
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2147,
  moonbit_string_t attr$1026
) {
  int32_t _tmp$2149;
  int64_t _tmp$2148;
  moonbit_decref(_env$2147);
  _tmp$2149 = Moonbit_array_length(attr$1026);
  _tmp$2148 = (int64_t)_tmp$2149;
  moonbit_incref(attr$1026);
  if ($String$$char_length_ge$inner(attr$1026, 5, 0, _tmp$2148)) {
    int32_t _tmp$2154 = attr$1026[0];
    int32_t _x$1027 = _tmp$2154;
    if (_x$1027 == 112) {
      int32_t _tmp$2153 = attr$1026[1];
      int32_t _x$1028 = _tmp$2153;
      if (_x$1028 == 97) {
        int32_t _tmp$2152 = attr$1026[2];
        int32_t _x$1029 = _tmp$2152;
        if (_x$1029 == 110) {
          int32_t _tmp$2151 = attr$1026[3];
          int32_t _x$1030 = _tmp$2151;
          if (_x$1030 == 105) {
            int32_t _tmp$2382 = attr$1026[4];
            int32_t _tmp$2150;
            int32_t _x$1031;
            moonbit_decref(attr$1026);
            _tmp$2150 = _tmp$2382;
            _x$1031 = _tmp$2150;
            return _x$1031 == 99 || 0;
          } else {
            moonbit_decref(attr$1026);
            return 0;
          }
        } else {
          moonbit_decref(attr$1026);
          return 0;
        }
      } else {
        moonbit_decref(attr$1026);
        return 0;
      }
    } else {
      moonbit_decref(attr$1026);
      return 0;
    }
  } else {
    moonbit_decref(attr$1026);
    return 0;
  }
}

int32_t $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$2131,
  moonbit_string_t test_name$1013,
  moonbit_string_t file_name$1014,
  moonbit_string_t message$1015,
  int32_t skipped$1016
) {
  struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$2132 =
    (struct $$azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$2131;
  int32_t _field$2383 = _casted_env$2132->$0;
  int32_t index$1011;
  int32_t _if_result$2846;
  moonbit_string_t file_name$1017;
  moonbit_string_t test_name$1018;
  moonbit_string_t message$1019;
  moonbit_string_t _tmp$2144;
  moonbit_string_t _tmp$2143;
  moonbit_string_t _tmp$2141;
  moonbit_string_t _tmp$2142;
  moonbit_string_t _tmp$2140;
  moonbit_string_t _tmp$2138;
  moonbit_string_t _tmp$2139;
  moonbit_string_t _tmp$2137;
  moonbit_string_t _tmp$2135;
  moonbit_string_t _tmp$2136;
  moonbit_string_t _tmp$2134;
  moonbit_string_t _tmp$2133;
  moonbit_decref(_casted_env$2132);
  index$1011 = _field$2383;
  if (!skipped$1016) {
    _if_result$2846 = 1;
  } else {
    _if_result$2846 = 0;
  }
  if (_if_result$2846) {
    
  }
  file_name$1017 = $String$$escape(file_name$1014);
  test_name$1018 = $String$$escape(test_name$1013);
  message$1019 = $String$$escape(message$1015);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$2144
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$1017
  );
  _tmp$2143
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$2144
  );
  _tmp$2141
  = moonbit_add_string(
    _tmp$2143, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$2142
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$1011
  );
  _tmp$2140 = moonbit_add_string(_tmp$2141, _tmp$2142);
  _tmp$2138
  = moonbit_add_string(
    _tmp$2140, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$2139
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$1018
  );
  _tmp$2137 = moonbit_add_string(_tmp$2138, _tmp$2139);
  _tmp$2135
  = moonbit_add_string(
    _tmp$2137, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$2136
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    message$1019
  );
  _tmp$2134 = moonbit_add_string(_tmp$2135, _tmp$2136);
  _tmp$2133
  = moonbit_add_string(
    _tmp$2134, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$2133);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1008
) {
  moonbit_decref(_discard_$1008);
  return 42;
}

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$working_tests$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1006,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1007,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1004
) {
  void* _try_err$1002;
  struct moonbit_result_0 _tmp$2848 = f$1006->code(f$1006);
  void* err$1003;
  if (_tmp$2848.tag) {
    int32_t const _ok$2129 = _tmp$2848.data.ok;
    moonbit_decref(on_err$1004);
  } else {
    void* const _err$2130 = _tmp$2848.data.err;
    moonbit_decref(on_ok$1007);
    _try_err$1002 = _err$2130;
    goto $join$1001;
  }
  on_ok$1007->code(on_ok$1007);
  goto $joinlet$2847;
  $join$1001:;
  err$1003 = _try_err$1002;
  on_err$1004->code(on_err$1004, err$1003);
  $joinlet$2847:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$working_tests$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$967,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$980,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$993,
  moonbit_string_t file_filter$964,
  int32_t index_filter$965
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$961;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$962;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$966;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2389;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2120;
  void* F0$2117;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2388;
  int32_t _cnt$2684;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$2119;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$2118;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$963;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$976;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$977;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$979;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2387;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2124;
  void* F1$2121;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2386;
  int32_t _cnt$2687;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$2123;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$2122;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$978;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$989;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$990;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$992;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2385;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2128;
  void* F2$2125;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2384;
  int32_t _cnt$2690;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$2127;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$2126;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$991;
  moonbit_incref(file_filter$964);
  _bind$966
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$967, file_filter$964
  );
  if (_bind$966 == 0) {
    if (_bind$966) {
      moonbit_decref(_bind$966);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$968 =
      _bind$966;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$969 =
      _Some$968;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$971;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$972;
    moonbit_incref(_index_func_map$969);
    _bind$972
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$969, index_filter$965
    );
    if (_bind$972 == 0) {
      if (_bind$972) {
        moonbit_decref(_bind$972);
      }
      moonbit_decref(_index_func_map$969);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$973;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$974;
      moonbit_decref(async_tests$993);
      moonbit_decref(with_args_tests$980);
      _Some$973 = _bind$972;
      _func_attrs_tuple$974 = _Some$973;
      func_attrs_tuple$971 = _func_attrs_tuple$974;
      goto $join$970;
    }
    goto $joinlet$2850;
    $join$970:;
    index_func_map$961 = _index_func_map$969;
    func_attrs_tuple$962 = func_attrs_tuple$971;
    goto $join$960;
    $joinlet$2850:;
  }
  goto $joinlet$2849;
  $join$960:;
  moonbit_decref(index_func_map$961);
  _field$2389 = func_attrs_tuple$962->$0;
  _tmp$2120 = _field$2389;
  moonbit_incref(_tmp$2120);
  F0$2117
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$2117)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$2117)->$0 = _tmp$2120;
  _field$2388 = func_attrs_tuple$962->$1;
  _cnt$2684 = Moonbit_object_header(func_attrs_tuple$962)->rc;
  if (_cnt$2684 > 1) {
    int32_t _new_cnt$2686;
    moonbit_incref(_field$2388);
    _new_cnt$2686 = _cnt$2684 - 1;
    Moonbit_object_header(func_attrs_tuple$962)->rc = _new_cnt$2686;
  } else if (_cnt$2684 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2685 =
      func_attrs_tuple$962->$0;
    moonbit_decref(_field$2685);
    moonbit_free(func_attrs_tuple$962);
  }
  _tmp$2119 = _field$2388;
  _tmp$2118
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$2118)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$2118->$0 = file_filter$964;
  _tmp$2118->$1 = index_filter$965;
  _tmp$2118->$2 = _tmp$2119;
  k$963
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$963)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$963->$0 = F0$2117;
  k$963->$1 = _tmp$2118;
  return k$963;
  $joinlet$2849:;
  moonbit_incref(file_filter$964);
  _bind$979
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$980, file_filter$964
  );
  if (_bind$979 == 0) {
    if (_bind$979) {
      moonbit_decref(_bind$979);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$981 =
      _bind$979;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$982 =
      _Some$981;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$984;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$985;
    moonbit_incref(_index_func_map$982);
    _bind$985
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$982, index_filter$965
    );
    if (_bind$985 == 0) {
      if (_bind$985) {
        moonbit_decref(_bind$985);
      }
      moonbit_decref(_index_func_map$982);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$986;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$987;
      moonbit_decref(async_tests$993);
      _Some$986 = _bind$985;
      _func_attrs_tuple$987 = _Some$986;
      func_attrs_tuple$984 = _func_attrs_tuple$987;
      goto $join$983;
    }
    goto $joinlet$2852;
    $join$983:;
    index_func_map$976 = _index_func_map$982;
    func_attrs_tuple$977 = func_attrs_tuple$984;
    goto $join$975;
    $joinlet$2852:;
  }
  goto $joinlet$2851;
  $join$975:;
  moonbit_decref(index_func_map$976);
  _field$2387 = func_attrs_tuple$977->$0;
  _tmp$2124 = _field$2387;
  moonbit_incref(_tmp$2124);
  F1$2121
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$2121)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$2121)->$0 = _tmp$2124;
  _field$2386 = func_attrs_tuple$977->$1;
  _cnt$2687 = Moonbit_object_header(func_attrs_tuple$977)->rc;
  if (_cnt$2687 > 1) {
    int32_t _new_cnt$2689;
    moonbit_incref(_field$2386);
    _new_cnt$2689 = _cnt$2687 - 1;
    Moonbit_object_header(func_attrs_tuple$977)->rc = _new_cnt$2689;
  } else if (_cnt$2687 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2688 =
      func_attrs_tuple$977->$0;
    moonbit_decref(_field$2688);
    moonbit_free(func_attrs_tuple$977);
  }
  _tmp$2123 = _field$2386;
  _tmp$2122
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$2122)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$2122->$0 = file_filter$964;
  _tmp$2122->$1 = index_filter$965;
  _tmp$2122->$2 = _tmp$2123;
  k$978
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$978)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$978->$0 = F1$2121;
  k$978->$1 = _tmp$2122;
  return k$978;
  $joinlet$2851:;
  moonbit_incref(file_filter$964);
  _bind$992
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$993, file_filter$964
  );
  if (_bind$992 == 0) {
    if (_bind$992) {
      moonbit_decref(_bind$992);
    }
    moonbit_decref(file_filter$964);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$994 =
      _bind$992;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$995 =
      _Some$994;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$997;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$998;
    moonbit_incref(_index_func_map$995);
    _bind$998
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$995, index_filter$965
    );
    if (_bind$998 == 0) {
      if (_bind$998) {
        moonbit_decref(_bind$998);
      }
      moonbit_decref(_index_func_map$995);
      moonbit_decref(file_filter$964);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$999 =
        _bind$998;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1000 =
        _Some$999;
      func_attrs_tuple$997 = _func_attrs_tuple$1000;
      goto $join$996;
    }
    goto $joinlet$2854;
    $join$996:;
    index_func_map$989 = _index_func_map$995;
    func_attrs_tuple$990 = func_attrs_tuple$997;
    goto $join$988;
    $joinlet$2854:;
  }
  goto $joinlet$2853;
  $join$988:;
  moonbit_decref(index_func_map$989);
  _field$2385 = func_attrs_tuple$990->$0;
  _tmp$2128 = _field$2385;
  moonbit_incref(_tmp$2128);
  F2$2125
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$2125)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$2125)->$0 = _tmp$2128;
  _field$2384 = func_attrs_tuple$990->$1;
  _cnt$2690 = Moonbit_object_header(func_attrs_tuple$990)->rc;
  if (_cnt$2690 > 1) {
    int32_t _new_cnt$2692;
    moonbit_incref(_field$2384);
    _new_cnt$2692 = _cnt$2690 - 1;
    Moonbit_object_header(func_attrs_tuple$990)->rc = _new_cnt$2692;
  } else if (_cnt$2690 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2691 =
      func_attrs_tuple$990->$0;
    moonbit_decref(_field$2691);
    moonbit_free(func_attrs_tuple$990);
  }
  _tmp$2127 = _field$2384;
  _tmp$2126
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$2126)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$2126->$0 = file_filter$964;
  _tmp$2126->$1 = index_filter$965;
  _tmp$2126->$2 = _tmp$2127;
  k$991
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$991)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$991->$0 = F2$2125;
  k$991->$1 = _tmp$2126;
  return k$991;
  $joinlet$2853:;
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10(
  
) {
  int32_t clock$951 = $$azimuth$telemetry$working_tests$Clock$$system();
  int32_t random$953;
  int32_t _tmp$2116;
  struct moonbit_result_0 _result$2855;
  $$azimuth$telemetry$working_tests$Clock$$now_unix_nanos(clock$951);
  random$953 = $$azimuth$telemetry$working_tests$Random$$system();
  $$azimuth$telemetry$working_tests$Random$$next_u64(random$953);
  _tmp$2116 = 0;
  _result$2855.tag = 1;
  _result$2855.data.ok = _tmp$2116;
  return _result$2855;
}

int32_t $$azimuth$telemetry$working_tests$Random$$system() {
  return 0;
}

uint64_t $$azimuth$telemetry$working_tests$Random$$next_u64(
  int32_t random$950
) {
  return 12345ull;
}

int32_t $$azimuth$telemetry$working_tests$Clock$$system() {
  return 0;
}

int64_t $$azimuth$telemetry$working_tests$Clock$$now_unix_nanos(
  int32_t clock$949
) {
  return 1735689600000000000ll;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9(
  
) {
  struct $$3c$String$2a$String$3e$* _tuple$2115 =
    (struct $$3c$String$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$String$3e$)
    );
  struct $$3c$String$2a$String$3e$** _tmp$2114;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$941;
  void* None$2113;
  struct $HttpRequest* request$942;
  moonbit_string_t _tmp$2393;
  moonbit_string_t _tmp$2392;
  moonbit_string_t _tmp$2391;
  void* None$2112;
  struct $HttpResponse* response$946;
  moonbit_string_t _tmp$2390;
  int32_t _tmp$2111;
  struct moonbit_result_0 _result$2856;
  Moonbit_object_header(_tuple$2115)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$String$3e$, $0) >> 2, 2, 0
  );
  _tuple$2115->$0 = (moonbit_string_t)moonbit_string_literal_14.data;
  _tuple$2115->$1 = (moonbit_string_t)moonbit_string_literal_15.data;
  _tmp$2114
  = (struct $$3c$String$2a$String$3e$**)moonbit_make_ref_array_raw(1);
  _tmp$2114[0] = _tuple$2115;
  headers$941
  = (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$
      )
    );
  Moonbit_object_header(headers$941)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  headers$941->$0 = _tmp$2114;
  headers$941->$1 = 1;
  None$2113 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  moonbit_incref(headers$941);
  request$942
  = $$azimuth$telemetry$working_tests$HttpRequest$$new(
    (moonbit_string_t)moonbit_string_literal_16.data,
      (moonbit_string_t)moonbit_string_literal_17.data,
      headers$941,
      None$2113
  );
  moonbit_incref(request$942);
  _tmp$2393
  = $$azimuth$telemetry$working_tests$HttpRequest$$http_method(
    request$942
  );
  moonbit_decref(_tmp$2393);
  moonbit_incref(request$942);
  _tmp$2392 = $$azimuth$telemetry$working_tests$HttpRequest$$url(request$942);
  moonbit_decref(_tmp$2392);
  _tmp$2391
  = $$azimuth$telemetry$working_tests$HttpRequest$$body(
    request$942
  );
  if (_tmp$2391) {
    moonbit_decref(_tmp$2391);
  }
  None$2112 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  response$946
  = $$azimuth$telemetry$working_tests$HttpResponse$$new(
    200, headers$941, None$2112
  );
  moonbit_incref(response$946);
  $$azimuth$telemetry$working_tests$HttpResponse$$status_code(response$946);
  _tmp$2390
  = $$azimuth$telemetry$working_tests$HttpResponse$$body(
    response$946
  );
  if (_tmp$2390) {
    moonbit_decref(_tmp$2390);
  }
  _tmp$2111 = 0;
  _result$2856.tag = 1;
  _result$2856.data.ok = _tmp$2111;
  return _result$2856;
}

int32_t $$azimuth$telemetry$working_tests$HttpResponse$$status_code(
  struct $HttpResponse* response$940
) {
  int32_t _field$2394 = response$940->$0;
  moonbit_decref(response$940);
  return _field$2394;
}

struct $HttpResponse* $$azimuth$telemetry$working_tests$HttpResponse$$new(
  int32_t status_code$938,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$939,
  void* body$opt$936
) {
  moonbit_string_t body$935;
  switch (Moonbit_object_tag(body$opt$936)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$937 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)body$opt$936;
      moonbit_string_t _field$2395 = _Some$937->$0;
      int32_t _cnt$2693 = Moonbit_object_header(_Some$937)->rc;
      if (_cnt$2693 > 1) {
        int32_t _new_cnt$2694;
        if (_field$2395) {
          moonbit_incref(_field$2395);
        }
        _new_cnt$2694 = _cnt$2693 - 1;
        Moonbit_object_header(_Some$937)->rc = _new_cnt$2694;
      } else if (_cnt$2693 == 1) {
        moonbit_free(_Some$937);
      }
      body$935 = _field$2395;
      break;
    }
    default: {
      moonbit_decref(body$opt$936);
      body$935 = 0;
      break;
    }
  }
  return $$azimuth$telemetry$working_tests$HttpResponse$$new$inner(
           status_code$938, headers$939, body$935
         );
}

struct $HttpResponse* $$azimuth$telemetry$working_tests$HttpResponse$$new$inner(
  int32_t status_code$932,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$933,
  moonbit_string_t body$934
) {
  struct $HttpResponse* _block$2857 =
    (struct $HttpResponse*)moonbit_malloc(sizeof(struct $HttpResponse));
  Moonbit_object_header(_block$2857)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $HttpResponse, $1) >> 2, 2, 0
  );
  _block$2857->$0 = status_code$932;
  _block$2857->$1 = headers$933;
  _block$2857->$2 = body$934;
  return _block$2857;
}

moonbit_string_t $$azimuth$telemetry$working_tests$HttpResponse$$body(
  struct $HttpResponse* response$931
) {
  moonbit_string_t _field$2396 = response$931->$2;
  int32_t _cnt$2695 = Moonbit_object_header(response$931)->rc;
  if (_cnt$2695 > 1) {
    int32_t _new_cnt$2697;
    if (_field$2396) {
      moonbit_incref(_field$2396);
    }
    _new_cnt$2697 = _cnt$2695 - 1;
    Moonbit_object_header(response$931)->rc = _new_cnt$2697;
  } else if (_cnt$2695 == 1) {
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2696 =
      response$931->$1;
    moonbit_decref(_field$2696);
    moonbit_free(response$931);
  }
  return _field$2396;
}

moonbit_string_t $$azimuth$telemetry$working_tests$HttpRequest$$url(
  struct $HttpRequest* request$930
) {
  moonbit_string_t _field$2397 = request$930->$1;
  int32_t _cnt$2698 = Moonbit_object_header(request$930)->rc;
  if (_cnt$2698 > 1) {
    int32_t _new_cnt$2702;
    moonbit_incref(_field$2397);
    _new_cnt$2702 = _cnt$2698 - 1;
    Moonbit_object_header(request$930)->rc = _new_cnt$2702;
  } else if (_cnt$2698 == 1) {
    moonbit_string_t _field$2701 = request$930->$3;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2700;
    moonbit_string_t _field$2699;
    if (_field$2701) {
      moonbit_decref(_field$2701);
    }
    _field$2700 = request$930->$2;
    moonbit_decref(_field$2700);
    _field$2699 = request$930->$0;
    moonbit_decref(_field$2699);
    moonbit_free(request$930);
  }
  return _field$2397;
}

struct $HttpRequest* $$azimuth$telemetry$working_tests$HttpRequest$$new(
  moonbit_string_t http_method$927,
  moonbit_string_t url$928,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$929,
  void* body$opt$925
) {
  moonbit_string_t body$924;
  switch (Moonbit_object_tag(body$opt$925)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$926 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)body$opt$925;
      moonbit_string_t _field$2398 = _Some$926->$0;
      int32_t _cnt$2703 = Moonbit_object_header(_Some$926)->rc;
      if (_cnt$2703 > 1) {
        int32_t _new_cnt$2704;
        if (_field$2398) {
          moonbit_incref(_field$2398);
        }
        _new_cnt$2704 = _cnt$2703 - 1;
        Moonbit_object_header(_Some$926)->rc = _new_cnt$2704;
      } else if (_cnt$2703 == 1) {
        moonbit_free(_Some$926);
      }
      body$924 = _field$2398;
      break;
    }
    default: {
      moonbit_decref(body$opt$925);
      body$924 = 0;
      break;
    }
  }
  return $$azimuth$telemetry$working_tests$HttpRequest$$new$inner(
           http_method$927, url$928, headers$929, body$924
         );
}

struct $HttpRequest* $$azimuth$telemetry$working_tests$HttpRequest$$new$inner(
  moonbit_string_t http_method$920,
  moonbit_string_t url$921,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$922,
  moonbit_string_t body$923
) {
  struct $HttpRequest* _block$2858 =
    (struct $HttpRequest*)moonbit_malloc(sizeof(struct $HttpRequest));
  Moonbit_object_header(_block$2858)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $HttpRequest, $0) >> 2, 4, 0
  );
  _block$2858->$0 = http_method$920;
  _block$2858->$1 = url$921;
  _block$2858->$2 = headers$922;
  _block$2858->$3 = body$923;
  return _block$2858;
}

moonbit_string_t $$azimuth$telemetry$working_tests$HttpRequest$$http_method(
  struct $HttpRequest* request$919
) {
  moonbit_string_t _field$2399 = request$919->$0;
  int32_t _cnt$2705 = Moonbit_object_header(request$919)->rc;
  if (_cnt$2705 > 1) {
    int32_t _new_cnt$2709;
    moonbit_incref(_field$2399);
    _new_cnt$2709 = _cnt$2705 - 1;
    Moonbit_object_header(request$919)->rc = _new_cnt$2709;
  } else if (_cnt$2705 == 1) {
    moonbit_string_t _field$2708 = request$919->$3;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2707;
    moonbit_string_t _field$2706;
    if (_field$2708) {
      moonbit_decref(_field$2708);
    }
    _field$2707 = request$919->$2;
    moonbit_decref(_field$2707);
    _field$2706 = request$919->$1;
    moonbit_decref(_field$2706);
    moonbit_free(request$919);
  }
  return _field$2399;
}

moonbit_string_t $$azimuth$telemetry$working_tests$HttpRequest$$body(
  struct $HttpRequest* request$918
) {
  moonbit_string_t _field$2400 = request$918->$3;
  int32_t _cnt$2710 = Moonbit_object_header(request$918)->rc;
  if (_cnt$2710 > 1) {
    int32_t _new_cnt$2714;
    if (_field$2400) {
      moonbit_incref(_field$2400);
    }
    _new_cnt$2714 = _cnt$2710 - 1;
    Moonbit_object_header(request$918)->rc = _new_cnt$2714;
  } else if (_cnt$2710 == 1) {
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$2713 =
      request$918->$2;
    moonbit_string_t _field$2712;
    moonbit_string_t _field$2711;
    moonbit_decref(_field$2713);
    _field$2712 = request$918->$1;
    moonbit_decref(_field$2712);
    _field$2711 = request$918->$0;
    moonbit_decref(_field$2711);
    moonbit_free(request$918);
  }
  return _field$2400;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8(
  
) {
  struct $TextMapCarrier* carrier$915 =
    $$azimuth$telemetry$working_tests$TextMapCarrier$$new();
  moonbit_string_t _tmp$2402;
  moonbit_string_t _tmp$2401;
  int32_t _tmp$2110;
  struct moonbit_result_0 _result$2859;
  moonbit_incref(carrier$915);
  $$azimuth$telemetry$working_tests$TextMapCarrier$$set(
    carrier$915,
      (moonbit_string_t)moonbit_string_literal_18.data,
      (moonbit_string_t)moonbit_string_literal_19.data
  );
  moonbit_incref(carrier$915);
  _tmp$2402
  = $$azimuth$telemetry$working_tests$TextMapCarrier$$get(
    carrier$915, (moonbit_string_t)moonbit_string_literal_18.data
  );
  if (_tmp$2402) {
    moonbit_decref(_tmp$2402);
  }
  _tmp$2401
  = $$azimuth$telemetry$working_tests$TextMapCarrier$$get(
    carrier$915, (moonbit_string_t)moonbit_string_literal_20.data
  );
  if (_tmp$2401) {
    moonbit_decref(_tmp$2401);
  }
  _tmp$2110 = 0;
  _result$2859.tag = 1;
  _result$2859.data.ok = _tmp$2110;
  return _result$2859;
}

int32_t $$azimuth$telemetry$working_tests$TextMapCarrier$$set(
  struct $TextMapCarrier* carrier$912,
  moonbit_string_t key$913,
  moonbit_string_t value$914
) {
  moonbit_decref(value$914);
  moonbit_decref(key$913);
  moonbit_decref(carrier$912);
  return 0;
}

struct $TextMapCarrier* $$azimuth$telemetry$working_tests$TextMapCarrier$$new(
  
) {
  struct $$3c$String$2a$String$3e$** _tmp$2109 =
    (struct $$3c$String$2a$String$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _tmp$2108 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$
      )
    );
  struct $TextMapCarrier* _block$2860;
  Moonbit_object_header(_tmp$2108)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  _tmp$2108->$0 = _tmp$2109;
  _tmp$2108->$1 = 0;
  _block$2860
  = (struct $TextMapCarrier*)moonbit_malloc(sizeof(struct $TextMapCarrier));
  Moonbit_object_header(_block$2860)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $TextMapCarrier, $0) >> 2, 1, 0
  );
  _block$2860->$0 = _tmp$2108;
  return _block$2860;
}

moonbit_string_t $$azimuth$telemetry$working_tests$TextMapCarrier$$get(
  struct $TextMapCarrier* carrier$911,
  moonbit_string_t key$910
) {
  int32_t _tmp$2403;
  moonbit_decref(carrier$911);
  _tmp$2403
  = moonbit_val_array_equal(
    key$910, (moonbit_string_t)moonbit_string_literal_18.data
  );
  moonbit_decref(key$910);
  if (_tmp$2403) {
    return (moonbit_string_t)moonbit_string_literal_19.data;
  } else {
    return 0;
  }
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7(
  
) {
  struct $LogRecord* record$907 =
    $$azimuth$telemetry$working_tests$LogRecord$$new(
      2, (moonbit_string_t)moonbit_string_literal_21.data
    );
  moonbit_string_t _tmp$2404;
  int32_t _tmp$2107;
  struct moonbit_result_0 _result$2861;
  moonbit_incref(record$907);
  $$azimuth$telemetry$working_tests$LogRecord$$severity_number(record$907);
  _tmp$2404 = $$azimuth$telemetry$working_tests$LogRecord$$body(record$907);
  if (_tmp$2404) {
    moonbit_decref(_tmp$2404);
  }
  _tmp$2107 = 0;
  _result$2861.tag = 1;
  _result$2861.data.ok = _tmp$2107;
  return _result$2861;
}

int32_t $$azimuth$telemetry$working_tests$LogRecord$$severity_number(
  struct $LogRecord* record$906
) {
  int32_t _field$2405 = record$906->$0;
  moonbit_decref(record$906);
  return _field$2405;
}

struct $LogRecord* $$azimuth$telemetry$working_tests$LogRecord$$new(
  int32_t severity$904,
  moonbit_string_t body$905
) {
  moonbit_string_t _tmp$2100 = body$905;
  struct $Attributes* _tmp$2101 = 0;
  void* None$2102 =
    (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  void* None$2103 =
    (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  moonbit_string_t _tmp$2104 = 0;
  moonbit_string_t _tmp$2105 = 0;
  struct $Context* _tmp$2106 = 0;
  struct $LogRecord* _block$2862 =
    (struct $LogRecord*)moonbit_malloc(sizeof(struct $LogRecord));
  Moonbit_object_header(_block$2862)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $LogRecord, $1) >> 2, 7, 0
  );
  _block$2862->$0 = severity$904;
  _block$2862->$1 = _tmp$2100;
  _block$2862->$2 = _tmp$2101;
  _block$2862->$3 = None$2102;
  _block$2862->$4 = None$2103;
  _block$2862->$5 = _tmp$2104;
  _block$2862->$6 = _tmp$2105;
  _block$2862->$7 = _tmp$2106;
  return _block$2862;
}

moonbit_string_t $$azimuth$telemetry$working_tests$LogRecord$$body(
  struct $LogRecord* record$903
) {
  moonbit_string_t _field$2406 = record$903->$1;
  int32_t _cnt$2715 = Moonbit_object_header(record$903)->rc;
  if (_cnt$2715 > 1) {
    int32_t _new_cnt$2722;
    if (_field$2406) {
      moonbit_incref(_field$2406);
    }
    _new_cnt$2722 = _cnt$2715 - 1;
    Moonbit_object_header(record$903)->rc = _new_cnt$2722;
  } else if (_cnt$2715 == 1) {
    struct $Context* _field$2721 = record$903->$7;
    moonbit_string_t _field$2720;
    moonbit_string_t _field$2719;
    void* _field$2718;
    void* _field$2717;
    struct $Attributes* _field$2716;
    if (_field$2721) {
      moonbit_decref(_field$2721);
    }
    _field$2720 = record$903->$6;
    if (_field$2720) {
      moonbit_decref(_field$2720);
    }
    _field$2719 = record$903->$5;
    if (_field$2719) {
      moonbit_decref(_field$2719);
    }
    _field$2718 = record$903->$4;
    moonbit_decref(_field$2718);
    _field$2717 = record$903->$3;
    moonbit_decref(_field$2717);
    _field$2716 = record$903->$2;
    if (_field$2716) {
      moonbit_decref(_field$2716);
    }
    moonbit_free(record$903);
  }
  return _field$2406;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6(
  
) {
  int32_t provider$899 =
    $$azimuth$telemetry$working_tests$MeterProvider$$default();
  struct $Meter* meter$900 =
    $$azimuth$telemetry$working_tests$MeterProvider$$get_meter(
      provider$899, (moonbit_string_t)moonbit_string_literal_22.data
    );
  struct $Counter* counter$901 =
    $$azimuth$telemetry$working_tests$Meter$$create_counter(
      meter$900, (moonbit_string_t)moonbit_string_literal_23.data
    );
  void* None$2097 =
    (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  void* None$2099;
  int32_t _tmp$2098;
  struct moonbit_result_0 _result$2863;
  moonbit_incref(counter$901);
  $$azimuth$telemetry$working_tests$Counter$$add(
    counter$901, 0x1p+0, None$2097
  );
  None$2099 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  _tmp$2098
  = $$azimuth$telemetry$working_tests$Counter$$add(
    counter$901, 0x1.4p+2, None$2099
  );
  _result$2863.tag = 1;
  _result$2863.data.ok = _tmp$2098;
  return _result$2863;
}

struct $Meter* $$azimuth$telemetry$working_tests$MeterProvider$$get_meter(
  int32_t provider$898,
  moonbit_string_t name$897
) {
  moonbit_string_t _tmp$2095 = 0;
  moonbit_string_t _tmp$2096 = 0;
  struct $InstrumentationScope* scope$896 =
    (struct $InstrumentationScope*)moonbit_malloc(
      sizeof(struct $InstrumentationScope)
    );
  struct $Meter* _block$2864;
  Moonbit_object_header(scope$896)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $InstrumentationScope, $0) >> 2, 3, 0
  );
  scope$896->$0 = name$897;
  scope$896->$1 = _tmp$2095;
  scope$896->$2 = _tmp$2096;
  _block$2864 = (struct $Meter*)moonbit_malloc(sizeof(struct $Meter));
  Moonbit_object_header(_block$2864)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Meter, $0) >> 2, 1, 0
  );
  _block$2864->$0 = scope$896;
  return _block$2864;
}

int32_t $$azimuth$telemetry$working_tests$MeterProvider$$default() {
  return 0;
}

struct $Counter* $$azimuth$telemetry$working_tests$Meter$$create_counter(
  struct $Meter* meter$895,
  moonbit_string_t name$894
) {
  moonbit_string_t _tmp$2093;
  moonbit_string_t _tmp$2094;
  struct $Counter* _block$2865;
  moonbit_decref(meter$895);
  _tmp$2093 = 0;
  _tmp$2094 = 0;
  _block$2865 = (struct $Counter*)moonbit_malloc(sizeof(struct $Counter));
  Moonbit_object_header(_block$2865)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Counter, $0) >> 2, 3, 0
  );
  _block$2865->$0 = name$894;
  _block$2865->$1 = _tmp$2093;
  _block$2865->$2 = _tmp$2094;
  return _block$2865;
}

int32_t $$azimuth$telemetry$working_tests$Counter$$add(
  struct $Counter* counter$892,
  double value$893,
  void* attributes$opt$890
) {
  struct $Attributes* attributes$889;
  switch (Moonbit_object_tag(attributes$opt$890)) {
    case 1: {
      struct $Option$3c$Option$3c$Attributes$3e$$3e$$Some* _Some$891 =
        (struct $Option$3c$Option$3c$Attributes$3e$$3e$$Some*)attributes$opt$890;
      struct $Attributes* _field$2407 = _Some$891->$0;
      int32_t _cnt$2723 = Moonbit_object_header(_Some$891)->rc;
      if (_cnt$2723 > 1) {
        int32_t _new_cnt$2724;
        if (_field$2407) {
          moonbit_incref(_field$2407);
        }
        _new_cnt$2724 = _cnt$2723 - 1;
        Moonbit_object_header(_Some$891)->rc = _new_cnt$2724;
      } else if (_cnt$2723 == 1) {
        moonbit_free(_Some$891);
      }
      attributes$889 = _field$2407;
      break;
    }
    default: {
      moonbit_decref(attributes$opt$890);
      attributes$889 = 0;
      break;
    }
  }
  $$azimuth$telemetry$working_tests$Counter$$add$inner(
    counter$892, value$893, attributes$889
  );
  return 0;
}

int32_t $$azimuth$telemetry$working_tests$Counter$$add$inner(
  struct $Counter* counter$886,
  double value$887,
  struct $Attributes* attributes$888
) {
  if (attributes$888) {
    moonbit_decref(attributes$888);
  }
  moonbit_decref(counter$886);
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5(
  
) {
  struct $Context* ctx$882 =
    $$azimuth$telemetry$working_tests$Context$$root();
  struct $ContextKey$3c$String$3e$* key$883 =
    $$azimuth$telemetry$working_tests$ContextKey$$new(
      (moonbit_string_t)moonbit_string_literal_24.data
    );
  struct $Context* ctx_with_value$884;
  moonbit_string_t _tmp$2408;
  int32_t _tmp$2092;
  struct moonbit_result_0 _result$2866;
  moonbit_incref(key$883);
  ctx_with_value$884
  = $$azimuth$telemetry$working_tests$Context$$with_value(
    ctx$882, key$883, (moonbit_string_t)moonbit_string_literal_25.data
  );
  _tmp$2408
  = $$azimuth$telemetry$working_tests$Context$$get(
    ctx_with_value$884, key$883
  );
  if (_tmp$2408) {
    moonbit_decref(_tmp$2408);
  }
  _tmp$2092 = 0;
  _result$2866.tag = 1;
  _result$2866.data.ok = _tmp$2092;
  return _result$2866;
}

struct $ContextKey$3c$String$3e$* $$azimuth$telemetry$working_tests$ContextKey$$new(
  moonbit_string_t key$881
) {
  struct $ContextKey$3c$String$3e$* _block$2867 =
    (struct $ContextKey$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $ContextKey$3c$String$3e$)
    );
  Moonbit_object_header(_block$2867)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $ContextKey$3c$String$3e$, $0) >> 2, 1, 0
  );
  _block$2867->$0 = key$881;
  return _block$2867;
}

struct $Context* $$azimuth$telemetry$working_tests$Context$$with_value(
  struct $Context* ctx$880,
  struct $ContextKey$3c$String$3e$* key$878,
  moonbit_string_t value$879
) {
  moonbit_string_t _field$2409;
  int32_t _cnt$2725;
  moonbit_string_t key$2091;
  struct $$3c$String$2a$String$3e$* _tuple$2090;
  struct $$3c$String$2a$String$3e$* _tmp$2089;
  struct $Context* _block$2868;
  moonbit_decref(ctx$880);
  _field$2409 = key$878->$0;
  _cnt$2725 = Moonbit_object_header(key$878)->rc;
  if (_cnt$2725 > 1) {
    int32_t _new_cnt$2726;
    moonbit_incref(_field$2409);
    _new_cnt$2726 = _cnt$2725 - 1;
    Moonbit_object_header(key$878)->rc = _new_cnt$2726;
  } else if (_cnt$2725 == 1) {
    moonbit_free(key$878);
  }
  key$2091 = _field$2409;
  _tuple$2090
  = (struct $$3c$String$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$String$3e$)
    );
  Moonbit_object_header(_tuple$2090)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$String$3e$, $0) >> 2, 2, 0
  );
  _tuple$2090->$0 = key$2091;
  _tuple$2090->$1 = value$879;
  _tmp$2089 = _tuple$2090;
  _block$2868 = (struct $Context*)moonbit_malloc(sizeof(struct $Context));
  Moonbit_object_header(_block$2868)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Context, $0) >> 2, 1, 0
  );
  _block$2868->$0 = _tmp$2089;
  return _block$2868;
}

struct $Context* $$azimuth$telemetry$working_tests$Context$$root() {
  struct $$3c$String$2a$String$3e$* _tmp$2088 = 0;
  struct $Context* _block$2869 =
    (struct $Context*)moonbit_malloc(sizeof(struct $Context));
  Moonbit_object_header(_block$2869)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Context, $0) >> 2, 1, 0
  );
  _block$2869->$0 = _tmp$2088;
  return _block$2869;
}

moonbit_string_t $$azimuth$telemetry$working_tests$Context$$get(
  struct $Context* ctx$872,
  struct $ContextKey$3c$String$3e$* key$877
) {
  moonbit_string_t k$869;
  moonbit_string_t v$870;
  struct $$3c$String$2a$String$3e$* _field$2414 = ctx$872->$0;
  int32_t _cnt$2727 = Moonbit_object_header(ctx$872)->rc;
  struct $$3c$String$2a$String$3e$* _bind$871;
  if (_cnt$2727 > 1) {
    int32_t _new_cnt$2728;
    if (_field$2414) {
      moonbit_incref(_field$2414);
    }
    _new_cnt$2728 = _cnt$2727 - 1;
    Moonbit_object_header(ctx$872)->rc = _new_cnt$2728;
  } else if (_cnt$2727 == 1) {
    moonbit_free(ctx$872);
  }
  _bind$871 = _field$2414;
  if (_bind$871 == 0) {
    moonbit_decref(key$877);
    if (_bind$871) {
      moonbit_decref(_bind$871);
    }
    goto $join$867;
  } else {
    struct $$3c$String$2a$String$3e$* _Some$873 = _bind$871;
    struct $$3c$String$2a$String$3e$* _x$874 = _Some$873;
    moonbit_string_t _field$2413 = _x$874->$0;
    moonbit_string_t _k$875 = _field$2413;
    moonbit_string_t _field$2412 = _x$874->$1;
    int32_t _cnt$2729 = Moonbit_object_header(_x$874)->rc;
    moonbit_string_t _v$876;
    moonbit_string_t _field$2411;
    int32_t _cnt$2731;
    moonbit_string_t key$2087;
    int32_t _tmp$2410;
    if (_cnt$2729 > 1) {
      int32_t _new_cnt$2730;
      moonbit_incref(_field$2412);
      moonbit_incref(_k$875);
      _new_cnt$2730 = _cnt$2729 - 1;
      Moonbit_object_header(_x$874)->rc = _new_cnt$2730;
    } else if (_cnt$2729 == 1) {
      moonbit_free(_x$874);
    }
    _v$876 = _field$2412;
    _field$2411 = key$877->$0;
    _cnt$2731 = Moonbit_object_header(key$877)->rc;
    if (_cnt$2731 > 1) {
      int32_t _new_cnt$2732;
      moonbit_incref(_field$2411);
      _new_cnt$2732 = _cnt$2731 - 1;
      Moonbit_object_header(key$877)->rc = _new_cnt$2732;
    } else if (_cnt$2731 == 1) {
      moonbit_free(key$877);
    }
    key$2087 = _field$2411;
    _tmp$2410 = moonbit_val_array_equal(_k$875, key$2087);
    moonbit_decref(key$2087);
    if (_tmp$2410) {
      k$869 = _k$875;
      v$870 = _v$876;
      goto $join$868;
    } else {
      moonbit_decref(_v$876);
      moonbit_decref(_k$875);
      goto $join$867;
    }
  }
  $join$868:;
  moonbit_decref(k$869);
  return v$870;
  $join$867:;
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4(
  
) {
  moonbit_string_t trace_id$860 =
    (moonbit_string_t)moonbit_string_literal_26.data;
  moonbit_string_t span_id$861 =
    (moonbit_string_t)moonbit_string_literal_27.data;
  struct $SpanContext* span_ctx$862 =
    $$azimuth$telemetry$working_tests$SpanContext$$new(
      trace_id$860,
        span_id$861,
        1,
        (moonbit_string_t)moonbit_string_literal_28.data
    );
  moonbit_string_t _tmp$2416;
  moonbit_string_t _tmp$2415;
  int32_t _tmp$2086;
  struct moonbit_result_0 _result$2872;
  moonbit_incref(span_ctx$862);
  _tmp$2416
  = $$azimuth$telemetry$working_tests$SpanContext$$trace_id(
    span_ctx$862
  );
  moonbit_decref(_tmp$2416);
  moonbit_incref(span_ctx$862);
  _tmp$2415
  = $$azimuth$telemetry$working_tests$SpanContext$$span_id(
    span_ctx$862
  );
  moonbit_decref(_tmp$2415);
  moonbit_incref(span_ctx$862);
  $$azimuth$telemetry$working_tests$SpanContext$$is_sampled(span_ctx$862);
  $$azimuth$telemetry$working_tests$SpanContext$$is_valid(span_ctx$862);
  _tmp$2086 = 0;
  _result$2872.tag = 1;
  _result$2872.data.ok = _tmp$2086;
  return _result$2872;
}

moonbit_string_t $$azimuth$telemetry$working_tests$SpanContext$$trace_id(
  struct $SpanContext* ctx$859
) {
  moonbit_string_t _field$2417 = ctx$859->$0;
  int32_t _cnt$2733 = Moonbit_object_header(ctx$859)->rc;
  if (_cnt$2733 > 1) {
    int32_t _new_cnt$2736;
    moonbit_incref(_field$2417);
    _new_cnt$2736 = _cnt$2733 - 1;
    Moonbit_object_header(ctx$859)->rc = _new_cnt$2736;
  } else if (_cnt$2733 == 1) {
    moonbit_string_t _field$2735 = ctx$859->$3;
    moonbit_string_t _field$2734;
    moonbit_decref(_field$2735);
    _field$2734 = ctx$859->$1;
    moonbit_decref(_field$2734);
    moonbit_free(ctx$859);
  }
  return _field$2417;
}

moonbit_string_t $$azimuth$telemetry$working_tests$SpanContext$$span_id(
  struct $SpanContext* ctx$858
) {
  moonbit_string_t _field$2418 = ctx$858->$1;
  int32_t _cnt$2737 = Moonbit_object_header(ctx$858)->rc;
  if (_cnt$2737 > 1) {
    int32_t _new_cnt$2740;
    moonbit_incref(_field$2418);
    _new_cnt$2740 = _cnt$2737 - 1;
    Moonbit_object_header(ctx$858)->rc = _new_cnt$2740;
  } else if (_cnt$2737 == 1) {
    moonbit_string_t _field$2739 = ctx$858->$3;
    moonbit_string_t _field$2738;
    moonbit_decref(_field$2739);
    _field$2738 = ctx$858->$0;
    moonbit_decref(_field$2738);
    moonbit_free(ctx$858);
  }
  return _field$2418;
}

struct $SpanContext* $$azimuth$telemetry$working_tests$SpanContext$$new(
  moonbit_string_t trace_id$854,
  moonbit_string_t span_id$855,
  int32_t sampled$856,
  moonbit_string_t trace_state$857
) {
  struct $SpanContext* _block$2873 =
    (struct $SpanContext*)moonbit_malloc(sizeof(struct $SpanContext));
  Moonbit_object_header(_block$2873)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $SpanContext, $0) >> 2, 3, 0
  );
  _block$2873->$0 = trace_id$854;
  _block$2873->$1 = span_id$855;
  _block$2873->$2 = sampled$856;
  _block$2873->$3 = trace_state$857;
  return _block$2873;
}

int32_t $$azimuth$telemetry$working_tests$SpanContext$$is_valid(
  struct $SpanContext* ctx$853
) {
  moonbit_string_t _field$2420 = ctx$853->$0;
  moonbit_string_t trace_id$2085 = _field$2420;
  moonbit_incref(trace_id$2085);
  if (
    $moonbitlang$core$builtin$op_notequal$1(
      trace_id$2085, (moonbit_string_t)moonbit_string_literal_3.data
    )
  ) {
    moonbit_string_t _field$2419 = ctx$853->$1;
    int32_t _cnt$2741 = Moonbit_object_header(ctx$853)->rc;
    moonbit_string_t span_id$2084;
    if (_cnt$2741 > 1) {
      int32_t _new_cnt$2744;
      moonbit_incref(_field$2419);
      _new_cnt$2744 = _cnt$2741 - 1;
      Moonbit_object_header(ctx$853)->rc = _new_cnt$2744;
    } else if (_cnt$2741 == 1) {
      moonbit_string_t _field$2743 = ctx$853->$3;
      moonbit_string_t _field$2742;
      moonbit_decref(_field$2743);
      _field$2742 = ctx$853->$0;
      moonbit_decref(_field$2742);
      moonbit_free(ctx$853);
    }
    span_id$2084 = _field$2419;
    return $moonbitlang$core$builtin$op_notequal$1(
             span_id$2084, (moonbit_string_t)moonbit_string_literal_3.data
           );
  } else {
    moonbit_decref(ctx$853);
    return 0;
  }
}

int32_t $$azimuth$telemetry$working_tests$SpanContext$$is_sampled(
  struct $SpanContext* ctx$852
) {
  int32_t _field$2421 = ctx$852->$2;
  moonbit_decref(ctx$852);
  return _field$2421;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3(
  
) {
  int32_t _tmp$2083 = 0;
  struct moonbit_result_0 _result$2874;
  _result$2874.tag = 1;
  _result$2874.data.ok = _tmp$2083;
  return _result$2874;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2(
  
) {
  moonbit_string_t _tmp$2422 =
    moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_29.data,
        (moonbit_string_t)moonbit_string_literal_30.data
    );
  int32_t _tmp$2082;
  struct moonbit_result_0 _result$2875;
  moonbit_decref(_tmp$2422);
  _tmp$2082 = 0;
  _result$2875.tag = 1;
  _result$2875.data.ok = _tmp$2082;
  return _result$2875;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1(
  
) {
  int32_t _tmp$2081 = 0;
  struct moonbit_result_0 _result$2876;
  _result$2876.tag = 1;
  _result$2876.data.ok = _tmp$2081;
  return _result$2876;
}

struct moonbit_result_0 $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0(
  
) {
  int32_t _tmp$2080 = 0;
  struct moonbit_result_0 _result$2877;
  _result$2877.tag = 1;
  _result$2877.data.ok = _tmp$2080;
  return _result$2877;
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$836
) {
  int32_t _field$2423 = self$836->$1;
  int32_t len$2079;
  moonbit_decref(self$836);
  len$2079 = _field$2423;
  return len$2079 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$834,
  struct $$moonbitlang$core$builtin$Logger logger$835
) {
  moonbit_string_t _tmp$2078 = self$834;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$2077 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$2078);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$2077, logger$835
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$820,
  struct $$moonbitlang$core$builtin$Logger logger$833
) {
  struct $StringView _field$2432 =
    (struct $StringView){self$820->$0_1, self$820->$0_2, self$820->$0_0};
  struct $StringView pkg$819 = _field$2432;
  int32_t _tmp$2076 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$2075;
  int64_t _bind$821;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$822;
  struct $StringView _field$2431;
  struct $StringView _module_name$829;
  void* _field$2430;
  int32_t _cnt$2745;
  void* _package_name$830;
  struct $StringView _field$2428;
  struct $StringView filename$2058;
  struct $StringView _field$2427;
  struct $StringView start_line$2059;
  struct $StringView _field$2426;
  struct $StringView start_column$2060;
  struct $StringView _field$2425;
  struct $StringView end_line$2061;
  struct $StringView _field$2424;
  int32_t _cnt$2749;
  struct $StringView end_column$2062;
  struct $$moonbitlang$core$builtin$Logger _bind$2057;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$2075
  = (struct $StringView){
    0, _tmp$2076, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$819.$0);
  moonbit_incref(pkg$819.$0);
  _bind$821 = $StringView$$find(pkg$819, _tmp$2075);
  if (_bind$821 == 4294967296ll) {
    void* None$2063 =
      (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
    _bind$822
    = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
        sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
      );
    Moonbit_object_header(_bind$822)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
      )
      >> 2,
        2,
        0
    );
    _bind$822->$0_0 = pkg$819.$0;
    _bind$822->$0_1 = pkg$819.$1;
    _bind$822->$0_2 = pkg$819.$2;
    _bind$822->$1 = None$2063;
  } else {
    int64_t _Some$823 = _bind$821;
    int32_t _first_slash$824 = (int32_t)_Some$823;
    int32_t _tmp$2074 = _first_slash$824 + 1;
    struct $StringView _tmp$2071;
    int32_t _tmp$2073;
    struct $StringView _tmp$2072;
    int64_t _bind$825;
    moonbit_incref(pkg$819.$0);
    _tmp$2071 = $StringView$$view$inner(pkg$819, _tmp$2074, 4294967296ll);
    _tmp$2073
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$2072
    = (struct $StringView){
      0, _tmp$2073, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$825 = $StringView$$find(_tmp$2071, _tmp$2072);
    if (_bind$825 == 4294967296ll) {
      void* None$2064 =
        (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$822
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$822)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$822->$0_0 = pkg$819.$0;
      _bind$822->$0_1 = pkg$819.$1;
      _bind$822->$0_2 = pkg$819.$2;
      _bind$822->$1 = None$2064;
    } else {
      int64_t _Some$826 = _bind$825;
      int32_t _second_slash$827 = (int32_t)_Some$826;
      int32_t _tmp$2070 = _first_slash$824 + 1;
      int32_t module_name_end$828 = _tmp$2070 + _second_slash$827;
      int64_t _tmp$2069 = (int64_t)module_name_end$828;
      struct $StringView _tmp$2065;
      int32_t _tmp$2068;
      struct $StringView _tmp$2067;
      void* Some$2066;
      moonbit_incref(pkg$819.$0);
      _tmp$2065 = $StringView$$view$inner(pkg$819, 0, _tmp$2069);
      _tmp$2068 = module_name_end$828 + 1;
      _tmp$2067 = $StringView$$view$inner(pkg$819, _tmp$2068, 4294967296ll);
      Some$2066
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$2066)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$2066)->$0_0
      = _tmp$2067.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2066)->$0_1
      = _tmp$2067.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2066)->$0_2
      = _tmp$2067.$2;
      _bind$822
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$822)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$822->$0_0 = _tmp$2065.$0;
      _bind$822->$0_1 = _tmp$2065.$1;
      _bind$822->$0_2 = _tmp$2065.$2;
      _bind$822->$1 = Some$2066;
    }
  }
  _field$2431
  = (struct $StringView){
    _bind$822->$0_1, _bind$822->$0_2, _bind$822->$0_0
  };
  _module_name$829 = _field$2431;
  _field$2430 = _bind$822->$1;
  _cnt$2745 = Moonbit_object_header(_bind$822)->rc;
  if (_cnt$2745 > 1) {
    int32_t _new_cnt$2746;
    moonbit_incref(_field$2430);
    moonbit_incref(_module_name$829.$0);
    _new_cnt$2746 = _cnt$2745 - 1;
    Moonbit_object_header(_bind$822)->rc = _new_cnt$2746;
  } else if (_cnt$2745 == 1) {
    moonbit_free(_bind$822);
  }
  _package_name$830 = _field$2430;
  switch (Moonbit_object_tag(_package_name$830)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$831 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$830;
      struct $StringView _field$2429 =
        (struct $StringView){
          _Some$831->$0_1, _Some$831->$0_2, _Some$831->$0_0
        };
      int32_t _cnt$2747 = Moonbit_object_header(_Some$831)->rc;
      struct $StringView _pkg_name$832;
      struct $$moonbitlang$core$builtin$Logger _bind$2056;
      if (_cnt$2747 > 1) {
        int32_t _new_cnt$2748;
        moonbit_incref(_field$2429.$0);
        _new_cnt$2748 = _cnt$2747 - 1;
        Moonbit_object_header(_Some$831)->rc = _new_cnt$2748;
      } else if (_cnt$2747 == 1) {
        moonbit_free(_Some$831);
      }
      _pkg_name$832 = _field$2429;
      if (logger$833.$1) {
        moonbit_incref(logger$833.$1);
      }
      logger$833.$0->$method_2(logger$833.$1, _pkg_name$832);
      _bind$2056 = logger$833;
      if (_bind$2056.$1) {
        moonbit_incref(_bind$2056.$1);
      }
      _bind$2056.$0->$method_3(_bind$2056.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$830);
      break;
    }
  }
  _field$2428
  = (struct $StringView){
    self$820->$1_1, self$820->$1_2, self$820->$1_0
  };
  filename$2058 = _field$2428;
  moonbit_incref(filename$2058.$0);
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_2(logger$833.$1, filename$2058);
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_3(logger$833.$1, 58);
  _field$2427
  = (struct $StringView){
    self$820->$2_1, self$820->$2_2, self$820->$2_0
  };
  start_line$2059 = _field$2427;
  moonbit_incref(start_line$2059.$0);
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_2(logger$833.$1, start_line$2059);
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_3(logger$833.$1, 58);
  _field$2426
  = (struct $StringView){
    self$820->$3_1, self$820->$3_2, self$820->$3_0
  };
  start_column$2060 = _field$2426;
  moonbit_incref(start_column$2060.$0);
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_2(logger$833.$1, start_column$2060);
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_3(logger$833.$1, 45);
  _field$2425
  = (struct $StringView){
    self$820->$4_1, self$820->$4_2, self$820->$4_0
  };
  end_line$2061 = _field$2425;
  moonbit_incref(end_line$2061.$0);
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_2(logger$833.$1, end_line$2061);
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_3(logger$833.$1, 58);
  _field$2424
  = (struct $StringView){
    self$820->$5_1, self$820->$5_2, self$820->$5_0
  };
  _cnt$2749 = Moonbit_object_header(self$820)->rc;
  if (_cnt$2749 > 1) {
    int32_t _new_cnt$2755;
    moonbit_incref(_field$2424.$0);
    _new_cnt$2755 = _cnt$2749 - 1;
    Moonbit_object_header(self$820)->rc = _new_cnt$2755;
  } else if (_cnt$2749 == 1) {
    struct $StringView _field$2754 =
      (struct $StringView){self$820->$4_1, self$820->$4_2, self$820->$4_0};
    struct $StringView _field$2753;
    struct $StringView _field$2752;
    struct $StringView _field$2751;
    struct $StringView _field$2750;
    moonbit_decref(_field$2754.$0);
    _field$2753
    = (struct $StringView){
      self$820->$3_1, self$820->$3_2, self$820->$3_0
    };
    moonbit_decref(_field$2753.$0);
    _field$2752
    = (struct $StringView){
      self$820->$2_1, self$820->$2_2, self$820->$2_0
    };
    moonbit_decref(_field$2752.$0);
    _field$2751
    = (struct $StringView){
      self$820->$1_1, self$820->$1_2, self$820->$1_0
    };
    moonbit_decref(_field$2751.$0);
    _field$2750
    = (struct $StringView){
      self$820->$0_1, self$820->$0_2, self$820->$0_0
    };
    moonbit_decref(_field$2750.$0);
    moonbit_free(self$820);
  }
  end_column$2062 = _field$2424;
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_2(logger$833.$1, end_column$2062);
  if (logger$833.$1) {
    moonbit_incref(logger$833.$1);
  }
  logger$833.$0->$method_3(logger$833.$1, 64);
  _bind$2057 = logger$833;
  _bind$2057.$0->$method_2(_bind$2057.$1, _module_name$829);
  return 0;
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$818) {
  moonbit_string_t _tmp$2055 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$818);
  moonbit_println(_tmp$2055);
  moonbit_decref(_tmp$2055);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$817,
  struct $$moonbitlang$core$builtin$Hasher* hasher$816
) {
  $$moonbitlang$core$builtin$Hasher$$combine_int(hasher$816, self$817);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$815,
  struct $$moonbitlang$core$builtin$Hasher* hasher$814
) {
  $$moonbitlang$core$builtin$Hasher$$combine_string(hasher$814, self$815);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$812,
  moonbit_string_t value$810
) {
  int32_t _end2448$809 = Moonbit_array_length(value$810);
  int32_t i$811 = 0;
  while (1) {
    if (i$811 < _end2448$809) {
      int32_t _tmp$2053 = value$810[i$811];
      uint32_t _tmp$2052 = *(uint32_t*)&_tmp$2053;
      int32_t _tmp$2054;
      moonbit_incref(self$812);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$812, _tmp$2052);
      _tmp$2054 = i$811 + 1;
      i$811 = _tmp$2054;
      continue;
    } else {
      moonbit_decref(self$812);
      moonbit_decref(value$810);
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$806,
  struct $$3c$String$3e$$3d$$3e$Int* f$808
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$2879 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$2049;
  int32_t _tmp$2048;
  Moonbit_object_header(_closure$2879)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2879->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$2879->$0 = f$808;
  _tmp$2049 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$2879;
  _tmp$2048 = $$moonbitlang$core$builtin$Iter$$run$0(self$806, _tmp$2049);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$2048, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2050,
  moonbit_string_t k$807
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$2051 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$2050;
  struct $$3c$String$3e$$3d$$3e$Int* _field$2433 = _casted_env$2051->$0;
  int32_t _cnt$2756 = Moonbit_object_header(_casted_env$2051)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$808;
  if (_cnt$2756 > 1) {
    int32_t _new_cnt$2757;
    moonbit_incref(_field$2433);
    _new_cnt$2757 = _cnt$2756 - 1;
    Moonbit_object_header(_casted_env$2051)->rc = _new_cnt$2757;
  } else if (_cnt$2756 == 1) {
    moonbit_free(_casted_env$2051);
  }
  f$808 = _field$2433;
  if (f$808->code(f$808, k$807)) {
    return 0;
  } else {
    return 1;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$804,
  int32_t idx$805
) {
  moonbit_string_t* _tmp$2047 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$804);
  moonbit_string_t _tmp$2434;
  if (idx$805 < 0 || idx$805 >= Moonbit_array_length(_tmp$2047)) {
    moonbit_panic();
  }
  _tmp$2434 = (moonbit_string_t)_tmp$2047[idx$805];
  moonbit_incref(_tmp$2434);
  moonbit_decref(_tmp$2047);
  return _tmp$2434;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$802,
  int32_t idx$803
) {
  struct $$3c$String$2a$Int$3e$** _tmp$2046 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$802);
  struct $$3c$String$2a$Int$3e$* _tmp$2435;
  if (idx$803 < 0 || idx$803 >= Moonbit_array_length(_tmp$2046)) {
    moonbit_panic();
  }
  _tmp$2435 = (struct $$3c$String$2a$Int$3e$*)_tmp$2046[idx$803];
  if (_tmp$2435) {
    moonbit_incref(_tmp$2435);
  }
  moonbit_decref(_tmp$2046);
  return _tmp$2435;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$798,
  int32_t key$794
) {
  int32_t hash$793 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$794);
  int32_t capacity_mask$2045 = self$798->$3;
  int32_t _tmp$2044 = hash$793 & capacity_mask$2045;
  int32_t i$795 = 0;
  int32_t idx$796 = _tmp$2044;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2439 =
      self$798->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2043 =
      _field$2439;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2438;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$797;
    if (idx$796 < 0 || idx$796 >= Moonbit_array_length(entries$2043)) {
      moonbit_panic();
    }
    _tmp$2438
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2043[
        idx$796
      ];
    _bind$797 = _tmp$2438;
    if (_bind$797 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2032;
      if (_bind$797) {
        moonbit_incref(_bind$797);
      }
      moonbit_decref(self$798);
      if (_bind$797) {
        moonbit_decref(_bind$797);
      }
      _tmp$2032 = 0;
      return _tmp$2032;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$799 =
        _bind$797;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$800 =
        _Some$799;
      int32_t hash$2034 = _entry$800->$3;
      int32_t _if_result$2881;
      int32_t _field$2436;
      int32_t psl$2037;
      int32_t _tmp$2039;
      int32_t _tmp$2041;
      int32_t capacity_mask$2042;
      int32_t _tmp$2040;
      if (hash$2034 == hash$793) {
        int32_t key$2033 = _entry$800->$4;
        _if_result$2881 = key$2033 == key$794;
      } else {
        _if_result$2881 = 0;
      }
      if (_if_result$2881) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2437;
        int32_t _cnt$2758;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2036;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2035;
        moonbit_incref(_entry$800);
        moonbit_decref(self$798);
        _field$2437 = _entry$800->$5;
        _cnt$2758 = Moonbit_object_header(_entry$800)->rc;
        if (_cnt$2758 > 1) {
          int32_t _new_cnt$2760;
          moonbit_incref(_field$2437);
          _new_cnt$2760 = _cnt$2758 - 1;
          Moonbit_object_header(_entry$800)->rc = _new_cnt$2760;
        } else if (_cnt$2758 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2759 =
            _entry$800->$1;
          if (_field$2759) {
            moonbit_decref(_field$2759);
          }
          moonbit_free(_entry$800);
        }
        value$2036 = _field$2437;
        _tmp$2035 = value$2036;
        return _tmp$2035;
      } else {
        moonbit_incref(_entry$800);
      }
      _field$2436 = _entry$800->$2;
      moonbit_decref(_entry$800);
      psl$2037 = _field$2436;
      if (i$795 > psl$2037) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2038;
        moonbit_decref(self$798);
        _tmp$2038 = 0;
        return _tmp$2038;
      }
      _tmp$2039 = i$795 + 1;
      _tmp$2041 = idx$796 + 1;
      capacity_mask$2042 = self$798->$3;
      _tmp$2040 = _tmp$2041 & capacity_mask$2042;
      i$795 = _tmp$2039;
      idx$796 = _tmp$2040;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$789,
  moonbit_string_t key$785
) {
  int32_t hash$784;
  int32_t capacity_mask$2031;
  int32_t _tmp$2030;
  int32_t i$786;
  int32_t idx$787;
  moonbit_incref(key$785);
  hash$784 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$785);
  capacity_mask$2031 = self$789->$3;
  _tmp$2030 = hash$784 & capacity_mask$2031;
  i$786 = 0;
  idx$787 = _tmp$2030;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2445 =
      self$789->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2029 =
      _field$2445;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2444;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$788;
    if (idx$787 < 0 || idx$787 >= Moonbit_array_length(entries$2029)) {
      moonbit_panic();
    }
    _tmp$2444
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2029[
        idx$787
      ];
    _bind$788 = _tmp$2444;
    if (_bind$788 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2018;
      if (_bind$788) {
        moonbit_incref(_bind$788);
      }
      moonbit_decref(self$789);
      if (_bind$788) {
        moonbit_decref(_bind$788);
      }
      moonbit_decref(key$785);
      _tmp$2018 = 0;
      return _tmp$2018;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$790 =
        _bind$788;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$791 =
        _Some$790;
      int32_t hash$2020 = _entry$791->$3;
      int32_t _if_result$2883;
      int32_t _field$2440;
      int32_t psl$2023;
      int32_t _tmp$2025;
      int32_t _tmp$2027;
      int32_t capacity_mask$2028;
      int32_t _tmp$2026;
      if (hash$2020 == hash$784) {
        moonbit_string_t _field$2443 = _entry$791->$4;
        moonbit_string_t key$2019 = _field$2443;
        int32_t _tmp$2442 = moonbit_val_array_equal(key$2019, key$785);
        _if_result$2883 = _tmp$2442;
      } else {
        _if_result$2883 = 0;
      }
      if (_if_result$2883) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2441;
        int32_t _cnt$2761;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2022;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2021;
        moonbit_incref(_entry$791);
        moonbit_decref(self$789);
        moonbit_decref(key$785);
        _field$2441 = _entry$791->$5;
        _cnt$2761 = Moonbit_object_header(_entry$791)->rc;
        if (_cnt$2761 > 1) {
          int32_t _new_cnt$2764;
          moonbit_incref(_field$2441);
          _new_cnt$2764 = _cnt$2761 - 1;
          Moonbit_object_header(_entry$791)->rc = _new_cnt$2764;
        } else if (_cnt$2761 == 1) {
          moonbit_string_t _field$2763 = _entry$791->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2762;
          moonbit_decref(_field$2763);
          _field$2762 = _entry$791->$1;
          if (_field$2762) {
            moonbit_decref(_field$2762);
          }
          moonbit_free(_entry$791);
        }
        value$2022 = _field$2441;
        _tmp$2021 = value$2022;
        return _tmp$2021;
      } else {
        moonbit_incref(_entry$791);
      }
      _field$2440 = _entry$791->$2;
      moonbit_decref(_entry$791);
      psl$2023 = _field$2440;
      if (i$786 > psl$2023) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2024;
        moonbit_decref(self$789);
        moonbit_decref(key$785);
        _tmp$2024 = 0;
        return _tmp$2024;
      }
      _tmp$2025 = i$786 + 1;
      _tmp$2027 = idx$787 + 1;
      capacity_mask$2028 = self$789->$3;
      _tmp$2026 = _tmp$2027 & capacity_mask$2028;
      i$786 = _tmp$2025;
      idx$787 = _tmp$2026;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$780,
  int32_t key$776
) {
  int32_t hash$775 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$776);
  int32_t capacity_mask$2017 = self$780->$3;
  int32_t _tmp$2016 = hash$775 & capacity_mask$2017;
  int32_t i$777 = 0;
  int32_t idx$778 = _tmp$2016;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2449 =
      self$780->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2015 =
      _field$2449;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2448;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$779;
    if (idx$778 < 0 || idx$778 >= Moonbit_array_length(entries$2015)) {
      moonbit_panic();
    }
    _tmp$2448
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2015[
        idx$778
      ];
    _bind$779 = _tmp$2448;
    if (_bind$779 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2004;
      if (_bind$779) {
        moonbit_incref(_bind$779);
      }
      moonbit_decref(self$780);
      if (_bind$779) {
        moonbit_decref(_bind$779);
      }
      _tmp$2004 = 0;
      return _tmp$2004;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$781 =
        _bind$779;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$782 =
        _Some$781;
      int32_t hash$2006 = _entry$782->$3;
      int32_t _if_result$2885;
      int32_t _field$2446;
      int32_t psl$2009;
      int32_t _tmp$2011;
      int32_t _tmp$2013;
      int32_t capacity_mask$2014;
      int32_t _tmp$2012;
      if (hash$2006 == hash$775) {
        int32_t key$2005 = _entry$782->$4;
        _if_result$2885 = key$2005 == key$776;
      } else {
        _if_result$2885 = 0;
      }
      if (_if_result$2885) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2447;
        int32_t _cnt$2765;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2008;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2007;
        moonbit_incref(_entry$782);
        moonbit_decref(self$780);
        _field$2447 = _entry$782->$5;
        _cnt$2765 = Moonbit_object_header(_entry$782)->rc;
        if (_cnt$2765 > 1) {
          int32_t _new_cnt$2767;
          moonbit_incref(_field$2447);
          _new_cnt$2767 = _cnt$2765 - 1;
          Moonbit_object_header(_entry$782)->rc = _new_cnt$2767;
        } else if (_cnt$2765 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2766 =
            _entry$782->$1;
          if (_field$2766) {
            moonbit_decref(_field$2766);
          }
          moonbit_free(_entry$782);
        }
        value$2008 = _field$2447;
        _tmp$2007 = value$2008;
        return _tmp$2007;
      } else {
        moonbit_incref(_entry$782);
      }
      _field$2446 = _entry$782->$2;
      moonbit_decref(_entry$782);
      psl$2009 = _field$2446;
      if (i$777 > psl$2009) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2010;
        moonbit_decref(self$780);
        _tmp$2010 = 0;
        return _tmp$2010;
      }
      _tmp$2011 = i$777 + 1;
      _tmp$2013 = idx$778 + 1;
      capacity_mask$2014 = self$780->$3;
      _tmp$2012 = _tmp$2013 & capacity_mask$2014;
      i$777 = _tmp$2011;
      idx$778 = _tmp$2012;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$771,
  moonbit_string_t key$767
) {
  int32_t hash$766;
  int32_t capacity_mask$2003;
  int32_t _tmp$2002;
  int32_t i$768;
  int32_t idx$769;
  moonbit_incref(key$767);
  hash$766 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$767);
  capacity_mask$2003 = self$771->$3;
  _tmp$2002 = hash$766 & capacity_mask$2003;
  i$768 = 0;
  idx$769 = _tmp$2002;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2455 =
      self$771->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2001 =
      _field$2455;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2454;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$770;
    if (idx$769 < 0 || idx$769 >= Moonbit_array_length(entries$2001)) {
      moonbit_panic();
    }
    _tmp$2454
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2001[
        idx$769
      ];
    _bind$770 = _tmp$2454;
    if (_bind$770 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1990;
      if (_bind$770) {
        moonbit_incref(_bind$770);
      }
      moonbit_decref(self$771);
      if (_bind$770) {
        moonbit_decref(_bind$770);
      }
      moonbit_decref(key$767);
      _tmp$1990 = 0;
      return _tmp$1990;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$772 =
        _bind$770;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$773 =
        _Some$772;
      int32_t hash$1992 = _entry$773->$3;
      int32_t _if_result$2887;
      int32_t _field$2450;
      int32_t psl$1995;
      int32_t _tmp$1997;
      int32_t _tmp$1999;
      int32_t capacity_mask$2000;
      int32_t _tmp$1998;
      if (hash$1992 == hash$766) {
        moonbit_string_t _field$2453 = _entry$773->$4;
        moonbit_string_t key$1991 = _field$2453;
        int32_t _tmp$2452 = moonbit_val_array_equal(key$1991, key$767);
        _if_result$2887 = _tmp$2452;
      } else {
        _if_result$2887 = 0;
      }
      if (_if_result$2887) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2451;
        int32_t _cnt$2768;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1994;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1993;
        moonbit_incref(_entry$773);
        moonbit_decref(self$771);
        moonbit_decref(key$767);
        _field$2451 = _entry$773->$5;
        _cnt$2768 = Moonbit_object_header(_entry$773)->rc;
        if (_cnt$2768 > 1) {
          int32_t _new_cnt$2771;
          moonbit_incref(_field$2451);
          _new_cnt$2771 = _cnt$2768 - 1;
          Moonbit_object_header(_entry$773)->rc = _new_cnt$2771;
        } else if (_cnt$2768 == 1) {
          moonbit_string_t _field$2770 = _entry$773->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2769;
          moonbit_decref(_field$2770);
          _field$2769 = _entry$773->$1;
          if (_field$2769) {
            moonbit_decref(_field$2769);
          }
          moonbit_free(_entry$773);
        }
        value$1994 = _field$2451;
        _tmp$1993 = value$1994;
        return _tmp$1993;
      } else {
        moonbit_incref(_entry$773);
      }
      _field$2450 = _entry$773->$2;
      moonbit_decref(_entry$773);
      psl$1995 = _field$2450;
      if (i$768 > psl$1995) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1996;
        moonbit_decref(self$771);
        moonbit_decref(key$767);
        _tmp$1996 = 0;
        return _tmp$1996;
      }
      _tmp$1997 = i$768 + 1;
      _tmp$1999 = idx$769 + 1;
      capacity_mask$2000 = self$771->$3;
      _tmp$1998 = _tmp$1999 & capacity_mask$2000;
      i$768 = _tmp$1997;
      idx$769 = _tmp$1998;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$762,
  int32_t key$758
) {
  int32_t hash$757 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$758);
  int32_t capacity_mask$1989 = self$762->$3;
  int32_t _tmp$1988 = hash$757 & capacity_mask$1989;
  int32_t i$759 = 0;
  int32_t idx$760 = _tmp$1988;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2459 =
      self$762->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1987 =
      _field$2459;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2458;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$761;
    if (idx$760 < 0 || idx$760 >= Moonbit_array_length(entries$1987)) {
      moonbit_panic();
    }
    _tmp$2458
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1987[
        idx$760
      ];
    _bind$761 = _tmp$2458;
    if (_bind$761 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1976;
      if (_bind$761) {
        moonbit_incref(_bind$761);
      }
      moonbit_decref(self$762);
      if (_bind$761) {
        moonbit_decref(_bind$761);
      }
      _tmp$1976 = 0;
      return _tmp$1976;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$763 =
        _bind$761;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$764 =
        _Some$763;
      int32_t hash$1978 = _entry$764->$3;
      int32_t _if_result$2889;
      int32_t _field$2456;
      int32_t psl$1981;
      int32_t _tmp$1983;
      int32_t _tmp$1985;
      int32_t capacity_mask$1986;
      int32_t _tmp$1984;
      if (hash$1978 == hash$757) {
        int32_t key$1977 = _entry$764->$4;
        _if_result$2889 = key$1977 == key$758;
      } else {
        _if_result$2889 = 0;
      }
      if (_if_result$2889) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2457;
        int32_t _cnt$2772;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1980;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1979;
        moonbit_incref(_entry$764);
        moonbit_decref(self$762);
        _field$2457 = _entry$764->$5;
        _cnt$2772 = Moonbit_object_header(_entry$764)->rc;
        if (_cnt$2772 > 1) {
          int32_t _new_cnt$2774;
          moonbit_incref(_field$2457);
          _new_cnt$2774 = _cnt$2772 - 1;
          Moonbit_object_header(_entry$764)->rc = _new_cnt$2774;
        } else if (_cnt$2772 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2773 =
            _entry$764->$1;
          if (_field$2773) {
            moonbit_decref(_field$2773);
          }
          moonbit_free(_entry$764);
        }
        value$1980 = _field$2457;
        _tmp$1979 = value$1980;
        return _tmp$1979;
      } else {
        moonbit_incref(_entry$764);
      }
      _field$2456 = _entry$764->$2;
      moonbit_decref(_entry$764);
      psl$1981 = _field$2456;
      if (i$759 > psl$1981) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1982;
        moonbit_decref(self$762);
        _tmp$1982 = 0;
        return _tmp$1982;
      }
      _tmp$1983 = i$759 + 1;
      _tmp$1985 = idx$760 + 1;
      capacity_mask$1986 = self$762->$3;
      _tmp$1984 = _tmp$1985 & capacity_mask$1986;
      i$759 = _tmp$1983;
      idx$760 = _tmp$1984;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$753,
  moonbit_string_t key$749
) {
  int32_t hash$748;
  int32_t capacity_mask$1975;
  int32_t _tmp$1974;
  int32_t i$750;
  int32_t idx$751;
  moonbit_incref(key$749);
  hash$748 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$749);
  capacity_mask$1975 = self$753->$3;
  _tmp$1974 = hash$748 & capacity_mask$1975;
  i$750 = 0;
  idx$751 = _tmp$1974;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2465 =
      self$753->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1973 =
      _field$2465;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2464;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$752;
    if (idx$751 < 0 || idx$751 >= Moonbit_array_length(entries$1973)) {
      moonbit_panic();
    }
    _tmp$2464
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1973[
        idx$751
      ];
    _bind$752 = _tmp$2464;
    if (_bind$752 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1962;
      if (_bind$752) {
        moonbit_incref(_bind$752);
      }
      moonbit_decref(self$753);
      if (_bind$752) {
        moonbit_decref(_bind$752);
      }
      moonbit_decref(key$749);
      _tmp$1962 = 0;
      return _tmp$1962;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$754 =
        _bind$752;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$755 =
        _Some$754;
      int32_t hash$1964 = _entry$755->$3;
      int32_t _if_result$2891;
      int32_t _field$2460;
      int32_t psl$1967;
      int32_t _tmp$1969;
      int32_t _tmp$1971;
      int32_t capacity_mask$1972;
      int32_t _tmp$1970;
      if (hash$1964 == hash$748) {
        moonbit_string_t _field$2463 = _entry$755->$4;
        moonbit_string_t key$1963 = _field$2463;
        int32_t _tmp$2462 = moonbit_val_array_equal(key$1963, key$749);
        _if_result$2891 = _tmp$2462;
      } else {
        _if_result$2891 = 0;
      }
      if (_if_result$2891) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2461;
        int32_t _cnt$2775;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1966;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1965;
        moonbit_incref(_entry$755);
        moonbit_decref(self$753);
        moonbit_decref(key$749);
        _field$2461 = _entry$755->$5;
        _cnt$2775 = Moonbit_object_header(_entry$755)->rc;
        if (_cnt$2775 > 1) {
          int32_t _new_cnt$2778;
          moonbit_incref(_field$2461);
          _new_cnt$2778 = _cnt$2775 - 1;
          Moonbit_object_header(_entry$755)->rc = _new_cnt$2778;
        } else if (_cnt$2775 == 1) {
          moonbit_string_t _field$2777 = _entry$755->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2776;
          moonbit_decref(_field$2777);
          _field$2776 = _entry$755->$1;
          if (_field$2776) {
            moonbit_decref(_field$2776);
          }
          moonbit_free(_entry$755);
        }
        value$1966 = _field$2461;
        _tmp$1965 = value$1966;
        return _tmp$1965;
      } else {
        moonbit_incref(_entry$755);
      }
      _field$2460 = _entry$755->$2;
      moonbit_decref(_entry$755);
      psl$1967 = _field$2460;
      if (i$750 > psl$1967) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1968;
        moonbit_decref(self$753);
        moonbit_decref(key$749);
        _tmp$1968 = 0;
        return _tmp$1968;
      }
      _tmp$1969 = i$750 + 1;
      _tmp$1971 = idx$751 + 1;
      capacity_mask$1972 = self$753->$3;
      _tmp$1970 = _tmp$1971 & capacity_mask$1972;
      i$750 = _tmp$1969;
      idx$751 = _tmp$1970;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$741
) {
  int32_t length$740;
  int32_t capacity$742;
  int32_t _tmp$1953;
  int32_t _tmp$1952;
  int32_t _tmp$1961;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$743;
  int32_t _len$744;
  int32_t _i$745;
  moonbit_incref(arr$741.$0);
  length$740 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$741);
  capacity$742 = $Int$$next_power_of_two(length$740);
  _tmp$1953 = capacity$742;
  _tmp$1952 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1953);
  if (length$740 > _tmp$1952) {
    int32_t _tmp$1954 = capacity$742;
    capacity$742 = _tmp$1954 * 2;
  }
  _tmp$1961 = capacity$742;
  m$743 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$1961);
  moonbit_incref(arr$741.$0);
  _len$744 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$741);
  _i$745 = 0;
  while (1) {
    if (_i$745 < _len$744) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2469 =
        arr$741.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1957 =
        _field$2469;
      int32_t start$1959 = arr$741.$1;
      int32_t _tmp$1958 = start$1959 + _i$745;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2468 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1957[
          _tmp$1958
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$746 =
        _tmp$2468;
      moonbit_string_t _field$2467 = e$746->$0;
      moonbit_string_t _tmp$1955 = _field$2467;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2466 =
        e$746->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1956 =
        _field$2466;
      int32_t _tmp$1960;
      moonbit_incref(_tmp$1956);
      moonbit_incref(_tmp$1955);
      moonbit_incref(m$743);
      $$moonbitlang$core$builtin$Map$$set$3(m$743, _tmp$1955, _tmp$1956);
      _tmp$1960 = _i$745 + 1;
      _i$745 = _tmp$1960;
      continue;
    } else {
      moonbit_decref(arr$741.$0);
    }
    break;
  }
  return m$743;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$733
) {
  int32_t length$732;
  int32_t capacity$734;
  int32_t _tmp$1943;
  int32_t _tmp$1942;
  int32_t _tmp$1951;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$735;
  int32_t _len$736;
  int32_t _i$737;
  moonbit_incref(arr$733.$0);
  length$732 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$733);
  capacity$734 = $Int$$next_power_of_two(length$732);
  _tmp$1943 = capacity$734;
  _tmp$1942 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1943);
  if (length$732 > _tmp$1942) {
    int32_t _tmp$1944 = capacity$734;
    capacity$734 = _tmp$1944 * 2;
  }
  _tmp$1951 = capacity$734;
  m$735 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$1951);
  moonbit_incref(arr$733.$0);
  _len$736 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$733);
  _i$737 = 0;
  while (1) {
    if (_i$737 < _len$736) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2473 =
        arr$733.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1947 =
        _field$2473;
      int32_t start$1949 = arr$733.$1;
      int32_t _tmp$1948 = start$1949 + _i$737;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2472 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1947[
          _tmp$1948
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$738 =
        _tmp$2472;
      moonbit_string_t _field$2471 = e$738->$0;
      moonbit_string_t _tmp$1945 = _field$2471;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2470 =
        e$738->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1946 =
        _field$2470;
      int32_t _tmp$1950;
      moonbit_incref(_tmp$1946);
      moonbit_incref(_tmp$1945);
      moonbit_incref(m$735);
      $$moonbitlang$core$builtin$Map$$set$2(m$735, _tmp$1945, _tmp$1946);
      _tmp$1950 = _i$737 + 1;
      _i$737 = _tmp$1950;
      continue;
    } else {
      moonbit_decref(arr$733.$0);
    }
    break;
  }
  return m$735;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$725
) {
  int32_t length$724;
  int32_t capacity$726;
  int32_t _tmp$1933;
  int32_t _tmp$1932;
  int32_t _tmp$1941;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$727;
  int32_t _len$728;
  int32_t _i$729;
  moonbit_incref(arr$725.$0);
  length$724 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$725);
  capacity$726 = $Int$$next_power_of_two(length$724);
  _tmp$1933 = capacity$726;
  _tmp$1932 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1933);
  if (length$724 > _tmp$1932) {
    int32_t _tmp$1934 = capacity$726;
    capacity$726 = _tmp$1934 * 2;
  }
  _tmp$1941 = capacity$726;
  m$727 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$1941);
  moonbit_incref(arr$725.$0);
  _len$728 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$725);
  _i$729 = 0;
  while (1) {
    if (_i$729 < _len$728) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2476 =
        arr$725.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$1937 =
        _field$2476;
      int32_t start$1939 = arr$725.$1;
      int32_t _tmp$1938 = start$1939 + _i$729;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2475 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$1937[
          _tmp$1938
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$730 =
        _tmp$2475;
      int32_t _tmp$1935 = e$730->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2474 =
        e$730->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1936 =
        _field$2474;
      int32_t _tmp$1940;
      moonbit_incref(_tmp$1936);
      moonbit_incref(m$727);
      $$moonbitlang$core$builtin$Map$$set$1(m$727, _tmp$1935, _tmp$1936);
      _tmp$1940 = _i$729 + 1;
      _i$729 = _tmp$1940;
      continue;
    } else {
      moonbit_decref(arr$725.$0);
    }
    break;
  }
  return m$727;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$717
) {
  int32_t length$716;
  int32_t capacity$718;
  int32_t _tmp$1923;
  int32_t _tmp$1922;
  int32_t _tmp$1931;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$719;
  int32_t _len$720;
  int32_t _i$721;
  moonbit_incref(arr$717.$0);
  length$716 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$717);
  capacity$718 = $Int$$next_power_of_two(length$716);
  _tmp$1923 = capacity$718;
  _tmp$1922 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1923);
  if (length$716 > _tmp$1922) {
    int32_t _tmp$1924 = capacity$718;
    capacity$718 = _tmp$1924 * 2;
  }
  _tmp$1931 = capacity$718;
  m$719 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$1931);
  moonbit_incref(arr$717.$0);
  _len$720 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$717);
  _i$721 = 0;
  while (1) {
    if (_i$721 < _len$720) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2480 =
        arr$717.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1927 =
        _field$2480;
      int32_t start$1929 = arr$717.$1;
      int32_t _tmp$1928 = start$1929 + _i$721;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2479 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1927[
          _tmp$1928
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$722 =
        _tmp$2479;
      moonbit_string_t _field$2478 = e$722->$0;
      moonbit_string_t _tmp$1925 = _field$2478;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2477 =
        e$722->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1926 =
        _field$2477;
      int32_t _tmp$1930;
      moonbit_incref(_tmp$1926);
      moonbit_incref(_tmp$1925);
      moonbit_incref(m$719);
      $$moonbitlang$core$builtin$Map$$set$0(m$719, _tmp$1925, _tmp$1926);
      _tmp$1930 = _i$721 + 1;
      _i$721 = _tmp$1930;
      continue;
    } else {
      moonbit_decref(arr$717.$0);
    }
    break;
  }
  return m$719;
}

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$713,
  moonbit_string_t key$714,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$715
) {
  int32_t _tmp$1921;
  moonbit_incref(key$714);
  _tmp$1921 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$714);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$713, key$714, value$715, _tmp$1921
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$710,
  moonbit_string_t key$711,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$712
) {
  int32_t _tmp$1920;
  moonbit_incref(key$711);
  _tmp$1920 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$711);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$710, key$711, value$712, _tmp$1920
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$707,
  int32_t key$708,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$709
) {
  int32_t _tmp$1919 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$708);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$707, key$708, value$709, _tmp$1919
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$704,
  moonbit_string_t key$705,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$706
) {
  int32_t _tmp$1918;
  moonbit_incref(key$705);
  _tmp$1918 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$705);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$704, key$705, value$706, _tmp$1918
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$694
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2487 =
    self$694->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$693 =
    _field$2487;
  int32_t capacity$1917 = self$694->$2;
  int32_t new_capacity$695 = capacity$1917 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1912 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1911 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$695, _tmp$1912
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2486 =
    self$694->$0;
  int32_t _tmp$1913;
  int32_t capacity$1915;
  int32_t _tmp$1914;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1916;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2485;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$696;
  if (old_head$693) {
    moonbit_incref(old_head$693);
  }
  moonbit_decref(_old$2486);
  self$694->$0 = _tmp$1911;
  self$694->$2 = new_capacity$695;
  _tmp$1913 = new_capacity$695 - 1;
  self$694->$3 = _tmp$1913;
  capacity$1915 = self$694->$2;
  _tmp$1914 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1915);
  self$694->$4 = _tmp$1914;
  self$694->$1 = 0;
  _tmp$1916 = 0;
  _old$2485 = self$694->$5;
  if (_old$2485) {
    moonbit_decref(_old$2485);
  }
  self$694->$5 = _tmp$1916;
  self$694->$6 = -1;
  _param$696 = old_head$693;
  while (1) {
    if (_param$696 == 0) {
      if (_param$696) {
        moonbit_decref(_param$696);
      }
      moonbit_decref(self$694);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$697 =
        _param$696;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$698 =
        _Some$697;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2484 =
        _x$698->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$699 =
        _field$2484;
      moonbit_string_t _field$2483 = _x$698->$4;
      moonbit_string_t _key$700 = _field$2483;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2482 =
        _x$698->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$701 =
        _field$2482;
      int32_t _field$2481 = _x$698->$3;
      int32_t _cnt$2779 = Moonbit_object_header(_x$698)->rc;
      int32_t _hash$702;
      if (_cnt$2779 > 1) {
        int32_t _new_cnt$2780;
        moonbit_incref(_value$701);
        moonbit_incref(_key$700);
        if (_next$699) {
          moonbit_incref(_next$699);
        }
        _new_cnt$2780 = _cnt$2779 - 1;
        Moonbit_object_header(_x$698)->rc = _new_cnt$2780;
      } else if (_cnt$2779 == 1) {
        moonbit_free(_x$698);
      }
      _hash$702 = _field$2481;
      moonbit_incref(self$694);
      $$moonbitlang$core$builtin$Map$$set_with_hash$3(
        self$694, _key$700, _value$701, _hash$702
      );
      _param$696 = _next$699;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$683
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2494 =
    self$683->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$682 =
    _field$2494;
  int32_t capacity$1910 = self$683->$2;
  int32_t new_capacity$684 = capacity$1910 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1905 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1904 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$684, _tmp$1905
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2493 =
    self$683->$0;
  int32_t _tmp$1906;
  int32_t capacity$1908;
  int32_t _tmp$1907;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1909;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2492;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$685;
  if (old_head$682) {
    moonbit_incref(old_head$682);
  }
  moonbit_decref(_old$2493);
  self$683->$0 = _tmp$1904;
  self$683->$2 = new_capacity$684;
  _tmp$1906 = new_capacity$684 - 1;
  self$683->$3 = _tmp$1906;
  capacity$1908 = self$683->$2;
  _tmp$1907 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1908);
  self$683->$4 = _tmp$1907;
  self$683->$1 = 0;
  _tmp$1909 = 0;
  _old$2492 = self$683->$5;
  if (_old$2492) {
    moonbit_decref(_old$2492);
  }
  self$683->$5 = _tmp$1909;
  self$683->$6 = -1;
  _param$685 = old_head$682;
  while (1) {
    if (_param$685 == 0) {
      if (_param$685) {
        moonbit_decref(_param$685);
      }
      moonbit_decref(self$683);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$686 =
        _param$685;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$687 =
        _Some$686;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2491 =
        _x$687->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$688 =
        _field$2491;
      moonbit_string_t _field$2490 = _x$687->$4;
      moonbit_string_t _key$689 = _field$2490;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2489 =
        _x$687->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$690 =
        _field$2489;
      int32_t _field$2488 = _x$687->$3;
      int32_t _cnt$2781 = Moonbit_object_header(_x$687)->rc;
      int32_t _hash$691;
      if (_cnt$2781 > 1) {
        int32_t _new_cnt$2782;
        moonbit_incref(_value$690);
        moonbit_incref(_key$689);
        if (_next$688) {
          moonbit_incref(_next$688);
        }
        _new_cnt$2782 = _cnt$2781 - 1;
        Moonbit_object_header(_x$687)->rc = _new_cnt$2782;
      } else if (_cnt$2781 == 1) {
        moonbit_free(_x$687);
      }
      _hash$691 = _field$2488;
      moonbit_incref(self$683);
      $$moonbitlang$core$builtin$Map$$set_with_hash$2(
        self$683, _key$689, _value$690, _hash$691
      );
      _param$685 = _next$688;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$672
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2500 =
    self$672->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$671 =
    _field$2500;
  int32_t capacity$1903 = self$672->$2;
  int32_t new_capacity$673 = capacity$1903 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1898 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1897 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$673, _tmp$1898
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$2499 =
    self$672->$0;
  int32_t _tmp$1899;
  int32_t capacity$1901;
  int32_t _tmp$1900;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1902;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2498;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$674;
  if (old_head$671) {
    moonbit_incref(old_head$671);
  }
  moonbit_decref(_old$2499);
  self$672->$0 = _tmp$1897;
  self$672->$2 = new_capacity$673;
  _tmp$1899 = new_capacity$673 - 1;
  self$672->$3 = _tmp$1899;
  capacity$1901 = self$672->$2;
  _tmp$1900 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1901);
  self$672->$4 = _tmp$1900;
  self$672->$1 = 0;
  _tmp$1902 = 0;
  _old$2498 = self$672->$5;
  if (_old$2498) {
    moonbit_decref(_old$2498);
  }
  self$672->$5 = _tmp$1902;
  self$672->$6 = -1;
  _param$674 = old_head$671;
  while (1) {
    if (_param$674 == 0) {
      if (_param$674) {
        moonbit_decref(_param$674);
      }
      moonbit_decref(self$672);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$675 =
        _param$674;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _x$676 =
        _Some$675;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2497 =
        _x$676->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$677 =
        _field$2497;
      int32_t _key$678 = _x$676->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2496 =
        _x$676->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$679 =
        _field$2496;
      int32_t _field$2495 = _x$676->$3;
      int32_t _cnt$2783 = Moonbit_object_header(_x$676)->rc;
      int32_t _hash$680;
      if (_cnt$2783 > 1) {
        int32_t _new_cnt$2784;
        moonbit_incref(_value$679);
        if (_next$677) {
          moonbit_incref(_next$677);
        }
        _new_cnt$2784 = _cnt$2783 - 1;
        Moonbit_object_header(_x$676)->rc = _new_cnt$2784;
      } else if (_cnt$2783 == 1) {
        moonbit_free(_x$676);
      }
      _hash$680 = _field$2495;
      moonbit_incref(self$672);
      $$moonbitlang$core$builtin$Map$$set_with_hash$1(
        self$672, _key$678, _value$679, _hash$680
      );
      _param$674 = _next$677;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$661
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2507 =
    self$661->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$660 =
    _field$2507;
  int32_t capacity$1896 = self$661->$2;
  int32_t new_capacity$662 = capacity$1896 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1891 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1890 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$662, _tmp$1891
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2506 =
    self$661->$0;
  int32_t _tmp$1892;
  int32_t capacity$1894;
  int32_t _tmp$1893;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1895;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2505;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$663;
  if (old_head$660) {
    moonbit_incref(old_head$660);
  }
  moonbit_decref(_old$2506);
  self$661->$0 = _tmp$1890;
  self$661->$2 = new_capacity$662;
  _tmp$1892 = new_capacity$662 - 1;
  self$661->$3 = _tmp$1892;
  capacity$1894 = self$661->$2;
  _tmp$1893 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1894);
  self$661->$4 = _tmp$1893;
  self$661->$1 = 0;
  _tmp$1895 = 0;
  _old$2505 = self$661->$5;
  if (_old$2505) {
    moonbit_decref(_old$2505);
  }
  self$661->$5 = _tmp$1895;
  self$661->$6 = -1;
  _param$663 = old_head$660;
  while (1) {
    if (_param$663 == 0) {
      if (_param$663) {
        moonbit_decref(_param$663);
      }
      moonbit_decref(self$661);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$664 =
        _param$663;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$665 =
        _Some$664;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2504 =
        _x$665->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$666 =
        _field$2504;
      moonbit_string_t _field$2503 = _x$665->$4;
      moonbit_string_t _key$667 = _field$2503;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2502 =
        _x$665->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$668 =
        _field$2502;
      int32_t _field$2501 = _x$665->$3;
      int32_t _cnt$2785 = Moonbit_object_header(_x$665)->rc;
      int32_t _hash$669;
      if (_cnt$2785 > 1) {
        int32_t _new_cnt$2786;
        moonbit_incref(_value$668);
        moonbit_incref(_key$667);
        if (_next$666) {
          moonbit_incref(_next$666);
        }
        _new_cnt$2786 = _cnt$2785 - 1;
        Moonbit_object_header(_x$665)->rc = _new_cnt$2786;
      } else if (_cnt$2785 == 1) {
        moonbit_free(_x$665);
      }
      _hash$669 = _field$2501;
      moonbit_incref(self$661);
      $$moonbitlang$core$builtin$Map$$set_with_hash$0(
        self$661, _key$667, _value$668, _hash$669
      );
      _param$663 = _next$666;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$644,
  moonbit_string_t key$653,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$654,
  int32_t hash$652
) {
  int32_t size$1876 = self$644->$1;
  int32_t grow_at$1877 = self$644->$4;
  int32_t capacity_mask$1889;
  int32_t _tmp$1888;
  struct $$3c$Int$2a$Int$3e$* _bind$645;
  int32_t psl$646;
  int32_t idx$647;
  int32_t _idx$655;
  int32_t _field$2508;
  int32_t _psl$656;
  int32_t _bind$657;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$658;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$659;
  if (size$1876 >= grow_at$1877) {
    moonbit_incref(self$644);
    $$moonbitlang$core$builtin$Map$$grow$3(self$644);
  }
  capacity_mask$1889 = self$644->$3;
  _tmp$1888 = hash$652 & capacity_mask$1889;
  psl$646 = 0;
  idx$647 = _tmp$1888;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2513 =
      self$644->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1887 =
      _field$2513;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2512;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$648;
    if (idx$647 < 0 || idx$647 >= Moonbit_array_length(entries$1887)) {
      moonbit_panic();
    }
    _tmp$2512
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1887[
        idx$647
      ];
    _bind$648 = _tmp$2512;
    if (_bind$648 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1878 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1878)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1878->$0 = idx$647;
      _tuple$1878->$1 = psl$646;
      _bind$645 = _tuple$1878;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$650 =
        _bind$648;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$651 =
        _Some$650;
      int32_t hash$1880 = _curr_entry$651->$3;
      int32_t _if_result$2901;
      int32_t psl$1881;
      int32_t _tmp$1883;
      int32_t _tmp$1885;
      int32_t capacity_mask$1886;
      int32_t _tmp$1884;
      if (hash$1880 == hash$652) {
        moonbit_string_t _field$2511 = _curr_entry$651->$4;
        moonbit_string_t key$1879 = _field$2511;
        int32_t _tmp$2510 = moonbit_val_array_equal(key$1879, key$653);
        _if_result$2901 = _tmp$2510;
      } else {
        _if_result$2901 = 0;
      }
      if (_if_result$2901) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2509;
        moonbit_incref(_curr_entry$651);
        moonbit_decref(key$653);
        moonbit_decref(self$644);
        _old$2509 = _curr_entry$651->$5;
        moonbit_decref(_old$2509);
        _curr_entry$651->$5 = value$654;
        moonbit_decref(_curr_entry$651);
        return 0;
      } else {
        moonbit_incref(_curr_entry$651);
      }
      psl$1881 = _curr_entry$651->$2;
      if (psl$646 > psl$1881) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1882;
        moonbit_incref(self$644);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$644, idx$647, _curr_entry$651
        );
        _tuple$1882
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1882)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1882->$0 = idx$647;
        _tuple$1882->$1 = psl$646;
        _bind$645 = _tuple$1882;
        break;
      } else {
        moonbit_decref(_curr_entry$651);
      }
      _tmp$1883 = psl$646 + 1;
      _tmp$1885 = idx$647 + 1;
      capacity_mask$1886 = self$644->$3;
      _tmp$1884 = _tmp$1885 & capacity_mask$1886;
      psl$646 = _tmp$1883;
      idx$647 = _tmp$1884;
      continue;
    }
    break;
  }
  _idx$655 = _bind$645->$0;
  _field$2508 = _bind$645->$1;
  moonbit_decref(_bind$645);
  _psl$656 = _field$2508;
  _bind$657 = self$644->$6;
  _bind$658 = 0;
  entry$659
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$659)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$659->$0 = _bind$657;
  entry$659->$1 = _bind$658;
  entry$659->$2 = _psl$656;
  entry$659->$3 = hash$652;
  entry$659->$4 = key$653;
  entry$659->$5 = value$654;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
    self$644, _idx$655, entry$659
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$628,
  moonbit_string_t key$637,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$638,
  int32_t hash$636
) {
  int32_t size$1862 = self$628->$1;
  int32_t grow_at$1863 = self$628->$4;
  int32_t capacity_mask$1875;
  int32_t _tmp$1874;
  struct $$3c$Int$2a$Int$3e$* _bind$629;
  int32_t psl$630;
  int32_t idx$631;
  int32_t _idx$639;
  int32_t _field$2514;
  int32_t _psl$640;
  int32_t _bind$641;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$642;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$643;
  if (size$1862 >= grow_at$1863) {
    moonbit_incref(self$628);
    $$moonbitlang$core$builtin$Map$$grow$2(self$628);
  }
  capacity_mask$1875 = self$628->$3;
  _tmp$1874 = hash$636 & capacity_mask$1875;
  psl$630 = 0;
  idx$631 = _tmp$1874;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2519 =
      self$628->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1873 =
      _field$2519;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2518;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$632;
    if (idx$631 < 0 || idx$631 >= Moonbit_array_length(entries$1873)) {
      moonbit_panic();
    }
    _tmp$2518
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1873[
        idx$631
      ];
    _bind$632 = _tmp$2518;
    if (_bind$632 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1864 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1864)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1864->$0 = idx$631;
      _tuple$1864->$1 = psl$630;
      _bind$629 = _tuple$1864;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$634 =
        _bind$632;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$635 =
        _Some$634;
      int32_t hash$1866 = _curr_entry$635->$3;
      int32_t _if_result$2903;
      int32_t psl$1867;
      int32_t _tmp$1869;
      int32_t _tmp$1871;
      int32_t capacity_mask$1872;
      int32_t _tmp$1870;
      if (hash$1866 == hash$636) {
        moonbit_string_t _field$2517 = _curr_entry$635->$4;
        moonbit_string_t key$1865 = _field$2517;
        int32_t _tmp$2516 = moonbit_val_array_equal(key$1865, key$637);
        _if_result$2903 = _tmp$2516;
      } else {
        _if_result$2903 = 0;
      }
      if (_if_result$2903) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2515;
        moonbit_incref(_curr_entry$635);
        moonbit_decref(key$637);
        moonbit_decref(self$628);
        _old$2515 = _curr_entry$635->$5;
        moonbit_decref(_old$2515);
        _curr_entry$635->$5 = value$638;
        moonbit_decref(_curr_entry$635);
        return 0;
      } else {
        moonbit_incref(_curr_entry$635);
      }
      psl$1867 = _curr_entry$635->$2;
      if (psl$630 > psl$1867) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1868;
        moonbit_incref(self$628);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$628, idx$631, _curr_entry$635
        );
        _tuple$1868
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1868)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1868->$0 = idx$631;
        _tuple$1868->$1 = psl$630;
        _bind$629 = _tuple$1868;
        break;
      } else {
        moonbit_decref(_curr_entry$635);
      }
      _tmp$1869 = psl$630 + 1;
      _tmp$1871 = idx$631 + 1;
      capacity_mask$1872 = self$628->$3;
      _tmp$1870 = _tmp$1871 & capacity_mask$1872;
      psl$630 = _tmp$1869;
      idx$631 = _tmp$1870;
      continue;
    }
    break;
  }
  _idx$639 = _bind$629->$0;
  _field$2514 = _bind$629->$1;
  moonbit_decref(_bind$629);
  _psl$640 = _field$2514;
  _bind$641 = self$628->$6;
  _bind$642 = 0;
  entry$643
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$643)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$643->$0 = _bind$641;
  entry$643->$1 = _bind$642;
  entry$643->$2 = _psl$640;
  entry$643->$3 = hash$636;
  entry$643->$4 = key$637;
  entry$643->$5 = value$638;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
    self$628, _idx$639, entry$643
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$612,
  int32_t key$621,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$622,
  int32_t hash$620
) {
  int32_t size$1848 = self$612->$1;
  int32_t grow_at$1849 = self$612->$4;
  int32_t capacity_mask$1861;
  int32_t _tmp$1860;
  struct $$3c$Int$2a$Int$3e$* _bind$613;
  int32_t psl$614;
  int32_t idx$615;
  int32_t _idx$623;
  int32_t _field$2520;
  int32_t _psl$624;
  int32_t _bind$625;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$626;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$627;
  if (size$1848 >= grow_at$1849) {
    moonbit_incref(self$612);
    $$moonbitlang$core$builtin$Map$$grow$1(self$612);
  }
  capacity_mask$1861 = self$612->$3;
  _tmp$1860 = hash$620 & capacity_mask$1861;
  psl$614 = 0;
  idx$615 = _tmp$1860;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2523 =
      self$612->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1859 =
      _field$2523;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2522;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$616;
    if (idx$615 < 0 || idx$615 >= Moonbit_array_length(entries$1859)) {
      moonbit_panic();
    }
    _tmp$2522
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1859[
        idx$615
      ];
    _bind$616 = _tmp$2522;
    if (_bind$616 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1850 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1850)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1850->$0 = idx$615;
      _tuple$1850->$1 = psl$614;
      _bind$613 = _tuple$1850;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$618 =
        _bind$616;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$619 =
        _Some$618;
      int32_t hash$1852 = _curr_entry$619->$3;
      int32_t _if_result$2905;
      int32_t psl$1853;
      int32_t _tmp$1855;
      int32_t _tmp$1857;
      int32_t capacity_mask$1858;
      int32_t _tmp$1856;
      if (hash$1852 == hash$620) {
        int32_t key$1851 = _curr_entry$619->$4;
        _if_result$2905 = key$1851 == key$621;
      } else {
        _if_result$2905 = 0;
      }
      if (_if_result$2905) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$2521;
        moonbit_incref(_curr_entry$619);
        moonbit_decref(self$612);
        _old$2521 = _curr_entry$619->$5;
        moonbit_decref(_old$2521);
        _curr_entry$619->$5 = value$622;
        moonbit_decref(_curr_entry$619);
        return 0;
      } else {
        moonbit_incref(_curr_entry$619);
      }
      psl$1853 = _curr_entry$619->$2;
      if (psl$614 > psl$1853) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1854;
        moonbit_incref(self$612);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$612, idx$615, _curr_entry$619
        );
        _tuple$1854
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1854)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1854->$0 = idx$615;
        _tuple$1854->$1 = psl$614;
        _bind$613 = _tuple$1854;
        break;
      } else {
        moonbit_decref(_curr_entry$619);
      }
      _tmp$1855 = psl$614 + 1;
      _tmp$1857 = idx$615 + 1;
      capacity_mask$1858 = self$612->$3;
      _tmp$1856 = _tmp$1857 & capacity_mask$1858;
      psl$614 = _tmp$1855;
      idx$615 = _tmp$1856;
      continue;
    }
    break;
  }
  _idx$623 = _bind$613->$0;
  _field$2520 = _bind$613->$1;
  moonbit_decref(_bind$613);
  _psl$624 = _field$2520;
  _bind$625 = self$612->$6;
  _bind$626 = 0;
  entry$627
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$627)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      2,
      0
  );
  entry$627->$0 = _bind$625;
  entry$627->$1 = _bind$626;
  entry$627->$2 = _psl$624;
  entry$627->$3 = hash$620;
  entry$627->$4 = key$621;
  entry$627->$5 = value$622;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
    self$612, _idx$623, entry$627
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$596,
  moonbit_string_t key$605,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$606,
  int32_t hash$604
) {
  int32_t size$1834 = self$596->$1;
  int32_t grow_at$1835 = self$596->$4;
  int32_t capacity_mask$1847;
  int32_t _tmp$1846;
  struct $$3c$Int$2a$Int$3e$* _bind$597;
  int32_t psl$598;
  int32_t idx$599;
  int32_t _idx$607;
  int32_t _field$2524;
  int32_t _psl$608;
  int32_t _bind$609;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$610;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$611;
  if (size$1834 >= grow_at$1835) {
    moonbit_incref(self$596);
    $$moonbitlang$core$builtin$Map$$grow$0(self$596);
  }
  capacity_mask$1847 = self$596->$3;
  _tmp$1846 = hash$604 & capacity_mask$1847;
  psl$598 = 0;
  idx$599 = _tmp$1846;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2529 =
      self$596->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1845 =
      _field$2529;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2528;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$600;
    if (idx$599 < 0 || idx$599 >= Moonbit_array_length(entries$1845)) {
      moonbit_panic();
    }
    _tmp$2528
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1845[
        idx$599
      ];
    _bind$600 = _tmp$2528;
    if (_bind$600 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1836 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1836)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1836->$0 = idx$599;
      _tuple$1836->$1 = psl$598;
      _bind$597 = _tuple$1836;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$602 =
        _bind$600;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$603 =
        _Some$602;
      int32_t hash$1838 = _curr_entry$603->$3;
      int32_t _if_result$2907;
      int32_t psl$1839;
      int32_t _tmp$1841;
      int32_t _tmp$1843;
      int32_t capacity_mask$1844;
      int32_t _tmp$1842;
      if (hash$1838 == hash$604) {
        moonbit_string_t _field$2527 = _curr_entry$603->$4;
        moonbit_string_t key$1837 = _field$2527;
        int32_t _tmp$2526 = moonbit_val_array_equal(key$1837, key$605);
        _if_result$2907 = _tmp$2526;
      } else {
        _if_result$2907 = 0;
      }
      if (_if_result$2907) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2525;
        moonbit_incref(_curr_entry$603);
        moonbit_decref(key$605);
        moonbit_decref(self$596);
        _old$2525 = _curr_entry$603->$5;
        moonbit_decref(_old$2525);
        _curr_entry$603->$5 = value$606;
        moonbit_decref(_curr_entry$603);
        return 0;
      } else {
        moonbit_incref(_curr_entry$603);
      }
      psl$1839 = _curr_entry$603->$2;
      if (psl$598 > psl$1839) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1840;
        moonbit_incref(self$596);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$596, idx$599, _curr_entry$603
        );
        _tuple$1840
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1840)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1840->$0 = idx$599;
        _tuple$1840->$1 = psl$598;
        _bind$597 = _tuple$1840;
        break;
      } else {
        moonbit_decref(_curr_entry$603);
      }
      _tmp$1841 = psl$598 + 1;
      _tmp$1843 = idx$599 + 1;
      capacity_mask$1844 = self$596->$3;
      _tmp$1842 = _tmp$1843 & capacity_mask$1844;
      psl$598 = _tmp$1841;
      idx$599 = _tmp$1842;
      continue;
    }
    break;
  }
  _idx$607 = _bind$597->$0;
  _field$2524 = _bind$597->$1;
  moonbit_decref(_bind$597);
  _psl$608 = _field$2524;
  _bind$609 = self$596->$6;
  _bind$610 = 0;
  entry$611
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$611)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$611->$0 = _bind$609;
  entry$611->$1 = _bind$610;
  entry$611->$2 = _psl$608;
  entry$611->$3 = hash$604;
  entry$611->$4 = key$605;
  entry$611->$5 = value$606;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
    self$596, _idx$607, entry$611
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$590,
  int32_t idx$595,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$594
) {
  int32_t psl$1833 = entry$594->$2;
  int32_t _tmp$1829 = psl$1833 + 1;
  int32_t _tmp$1831 = idx$595 + 1;
  int32_t capacity_mask$1832 = self$590->$3;
  int32_t _tmp$1830 = _tmp$1831 & capacity_mask$1832;
  int32_t psl$586 = _tmp$1829;
  int32_t idx$587 = _tmp$1830;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$588 =
    entry$594;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2531 =
      self$590->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1828 =
      _field$2531;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2530;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$589;
    if (idx$587 < 0 || idx$587 >= Moonbit_array_length(entries$1828)) {
      moonbit_panic();
    }
    _tmp$2530
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1828[
        idx$587
      ];
    _bind$589 = _tmp$2530;
    if (_bind$589 == 0) {
      entry$588->$2 = psl$586;
      $$moonbitlang$core$builtin$Map$$set_entry$3(
        self$590, entry$588, idx$587
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$592 =
        _bind$589;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$593 =
        _Some$592;
      int32_t psl$1818 = _curr_entry$593->$2;
      if (psl$586 > psl$1818) {
        int32_t psl$1823;
        int32_t _tmp$1819;
        int32_t _tmp$1821;
        int32_t capacity_mask$1822;
        int32_t _tmp$1820;
        entry$588->$2 = psl$586;
        moonbit_incref(_curr_entry$593);
        moonbit_incref(self$590);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$590, entry$588, idx$587
        );
        psl$1823 = _curr_entry$593->$2;
        _tmp$1819 = psl$1823 + 1;
        _tmp$1821 = idx$587 + 1;
        capacity_mask$1822 = self$590->$3;
        _tmp$1820 = _tmp$1821 & capacity_mask$1822;
        psl$586 = _tmp$1819;
        idx$587 = _tmp$1820;
        entry$588 = _curr_entry$593;
        continue;
      } else {
        int32_t _tmp$1824 = psl$586 + 1;
        int32_t _tmp$1826 = idx$587 + 1;
        int32_t capacity_mask$1827 = self$590->$3;
        int32_t _tmp$1825 = _tmp$1826 & capacity_mask$1827;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2909 =
          entry$588;
        psl$586 = _tmp$1824;
        idx$587 = _tmp$1825;
        entry$588 = _tmp$2909;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$580,
  int32_t idx$585,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$584
) {
  int32_t psl$1817 = entry$584->$2;
  int32_t _tmp$1813 = psl$1817 + 1;
  int32_t _tmp$1815 = idx$585 + 1;
  int32_t capacity_mask$1816 = self$580->$3;
  int32_t _tmp$1814 = _tmp$1815 & capacity_mask$1816;
  int32_t psl$576 = _tmp$1813;
  int32_t idx$577 = _tmp$1814;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$578 =
    entry$584;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2533 =
      self$580->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1812 =
      _field$2533;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2532;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$579;
    if (idx$577 < 0 || idx$577 >= Moonbit_array_length(entries$1812)) {
      moonbit_panic();
    }
    _tmp$2532
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1812[
        idx$577
      ];
    _bind$579 = _tmp$2532;
    if (_bind$579 == 0) {
      entry$578->$2 = psl$576;
      $$moonbitlang$core$builtin$Map$$set_entry$2(
        self$580, entry$578, idx$577
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$582 =
        _bind$579;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$583 =
        _Some$582;
      int32_t psl$1802 = _curr_entry$583->$2;
      if (psl$576 > psl$1802) {
        int32_t psl$1807;
        int32_t _tmp$1803;
        int32_t _tmp$1805;
        int32_t capacity_mask$1806;
        int32_t _tmp$1804;
        entry$578->$2 = psl$576;
        moonbit_incref(_curr_entry$583);
        moonbit_incref(self$580);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$580, entry$578, idx$577
        );
        psl$1807 = _curr_entry$583->$2;
        _tmp$1803 = psl$1807 + 1;
        _tmp$1805 = idx$577 + 1;
        capacity_mask$1806 = self$580->$3;
        _tmp$1804 = _tmp$1805 & capacity_mask$1806;
        psl$576 = _tmp$1803;
        idx$577 = _tmp$1804;
        entry$578 = _curr_entry$583;
        continue;
      } else {
        int32_t _tmp$1808 = psl$576 + 1;
        int32_t _tmp$1810 = idx$577 + 1;
        int32_t capacity_mask$1811 = self$580->$3;
        int32_t _tmp$1809 = _tmp$1810 & capacity_mask$1811;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2911 =
          entry$578;
        psl$576 = _tmp$1808;
        idx$577 = _tmp$1809;
        entry$578 = _tmp$2911;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$570,
  int32_t idx$575,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$574
) {
  int32_t psl$1801 = entry$574->$2;
  int32_t _tmp$1797 = psl$1801 + 1;
  int32_t _tmp$1799 = idx$575 + 1;
  int32_t capacity_mask$1800 = self$570->$3;
  int32_t _tmp$1798 = _tmp$1799 & capacity_mask$1800;
  int32_t psl$566 = _tmp$1797;
  int32_t idx$567 = _tmp$1798;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$568 =
    entry$574;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2535 =
      self$570->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1796 =
      _field$2535;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2534;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$569;
    if (idx$567 < 0 || idx$567 >= Moonbit_array_length(entries$1796)) {
      moonbit_panic();
    }
    _tmp$2534
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1796[
        idx$567
      ];
    _bind$569 = _tmp$2534;
    if (_bind$569 == 0) {
      entry$568->$2 = psl$566;
      $$moonbitlang$core$builtin$Map$$set_entry$1(
        self$570, entry$568, idx$567
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$572 =
        _bind$569;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$573 =
        _Some$572;
      int32_t psl$1786 = _curr_entry$573->$2;
      if (psl$566 > psl$1786) {
        int32_t psl$1791;
        int32_t _tmp$1787;
        int32_t _tmp$1789;
        int32_t capacity_mask$1790;
        int32_t _tmp$1788;
        entry$568->$2 = psl$566;
        moonbit_incref(_curr_entry$573);
        moonbit_incref(self$570);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$570, entry$568, idx$567
        );
        psl$1791 = _curr_entry$573->$2;
        _tmp$1787 = psl$1791 + 1;
        _tmp$1789 = idx$567 + 1;
        capacity_mask$1790 = self$570->$3;
        _tmp$1788 = _tmp$1789 & capacity_mask$1790;
        psl$566 = _tmp$1787;
        idx$567 = _tmp$1788;
        entry$568 = _curr_entry$573;
        continue;
      } else {
        int32_t _tmp$1792 = psl$566 + 1;
        int32_t _tmp$1794 = idx$567 + 1;
        int32_t capacity_mask$1795 = self$570->$3;
        int32_t _tmp$1793 = _tmp$1794 & capacity_mask$1795;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2913 =
          entry$568;
        psl$566 = _tmp$1792;
        idx$567 = _tmp$1793;
        entry$568 = _tmp$2913;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$560,
  int32_t idx$565,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$564
) {
  int32_t psl$1785 = entry$564->$2;
  int32_t _tmp$1781 = psl$1785 + 1;
  int32_t _tmp$1783 = idx$565 + 1;
  int32_t capacity_mask$1784 = self$560->$3;
  int32_t _tmp$1782 = _tmp$1783 & capacity_mask$1784;
  int32_t psl$556 = _tmp$1781;
  int32_t idx$557 = _tmp$1782;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$558 =
    entry$564;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2537 =
      self$560->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1780 =
      _field$2537;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2536;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$559;
    if (idx$557 < 0 || idx$557 >= Moonbit_array_length(entries$1780)) {
      moonbit_panic();
    }
    _tmp$2536
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1780[
        idx$557
      ];
    _bind$559 = _tmp$2536;
    if (_bind$559 == 0) {
      entry$558->$2 = psl$556;
      $$moonbitlang$core$builtin$Map$$set_entry$0(
        self$560, entry$558, idx$557
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$562 =
        _bind$559;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$563 =
        _Some$562;
      int32_t psl$1770 = _curr_entry$563->$2;
      if (psl$556 > psl$1770) {
        int32_t psl$1775;
        int32_t _tmp$1771;
        int32_t _tmp$1773;
        int32_t capacity_mask$1774;
        int32_t _tmp$1772;
        entry$558->$2 = psl$556;
        moonbit_incref(_curr_entry$563);
        moonbit_incref(self$560);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$560, entry$558, idx$557
        );
        psl$1775 = _curr_entry$563->$2;
        _tmp$1771 = psl$1775 + 1;
        _tmp$1773 = idx$557 + 1;
        capacity_mask$1774 = self$560->$3;
        _tmp$1772 = _tmp$1773 & capacity_mask$1774;
        psl$556 = _tmp$1771;
        idx$557 = _tmp$1772;
        entry$558 = _curr_entry$563;
        continue;
      } else {
        int32_t _tmp$1776 = psl$556 + 1;
        int32_t _tmp$1778 = idx$557 + 1;
        int32_t capacity_mask$1779 = self$560->$3;
        int32_t _tmp$1777 = _tmp$1778 & capacity_mask$1779;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2915 =
          entry$558;
        psl$556 = _tmp$1776;
        idx$557 = _tmp$1777;
        entry$558 = _tmp$2915;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$550,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$552,
  int32_t new_idx$551
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2540 =
    self$550->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1768 =
    _field$2540;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1769;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2539;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2538;
  int32_t _cnt$2787;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$553;
  moonbit_incref(entry$552);
  _tmp$1769 = entry$552;
  if (new_idx$551 < 0 || new_idx$551 >= Moonbit_array_length(entries$1768)) {
    moonbit_panic();
  }
  _old$2539
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1768[
      new_idx$551
    ];
  if (_old$2539) {
    moonbit_decref(_old$2539);
  }
  entries$1768[new_idx$551] = _tmp$1769;
  _field$2538 = entry$552->$1;
  _cnt$2787 = Moonbit_object_header(entry$552)->rc;
  if (_cnt$2787 > 1) {
    int32_t _new_cnt$2790;
    if (_field$2538) {
      moonbit_incref(_field$2538);
    }
    _new_cnt$2790 = _cnt$2787 - 1;
    Moonbit_object_header(entry$552)->rc = _new_cnt$2790;
  } else if (_cnt$2787 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2789 =
      entry$552->$5;
    moonbit_string_t _field$2788;
    moonbit_decref(_field$2789);
    _field$2788 = entry$552->$4;
    moonbit_decref(_field$2788);
    moonbit_free(entry$552);
  }
  _bind$553 = _field$2538;
  if (_bind$553 == 0) {
    if (_bind$553) {
      moonbit_decref(_bind$553);
    }
    self$550->$6 = new_idx$551;
    moonbit_decref(self$550);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$554;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$555;
    moonbit_decref(self$550);
    _Some$554 = _bind$553;
    _next$555 = _Some$554;
    _next$555->$0 = new_idx$551;
    moonbit_decref(_next$555);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$544,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$546,
  int32_t new_idx$545
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2543 =
    self$544->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1766 =
    _field$2543;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1767;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2542;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2541;
  int32_t _cnt$2791;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$547;
  moonbit_incref(entry$546);
  _tmp$1767 = entry$546;
  if (new_idx$545 < 0 || new_idx$545 >= Moonbit_array_length(entries$1766)) {
    moonbit_panic();
  }
  _old$2542
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1766[
      new_idx$545
    ];
  if (_old$2542) {
    moonbit_decref(_old$2542);
  }
  entries$1766[new_idx$545] = _tmp$1767;
  _field$2541 = entry$546->$1;
  _cnt$2791 = Moonbit_object_header(entry$546)->rc;
  if (_cnt$2791 > 1) {
    int32_t _new_cnt$2794;
    if (_field$2541) {
      moonbit_incref(_field$2541);
    }
    _new_cnt$2794 = _cnt$2791 - 1;
    Moonbit_object_header(entry$546)->rc = _new_cnt$2794;
  } else if (_cnt$2791 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2793 =
      entry$546->$5;
    moonbit_string_t _field$2792;
    moonbit_decref(_field$2793);
    _field$2792 = entry$546->$4;
    moonbit_decref(_field$2792);
    moonbit_free(entry$546);
  }
  _bind$547 = _field$2541;
  if (_bind$547 == 0) {
    if (_bind$547) {
      moonbit_decref(_bind$547);
    }
    self$544->$6 = new_idx$545;
    moonbit_decref(self$544);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$548;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$549;
    moonbit_decref(self$544);
    _Some$548 = _bind$547;
    _next$549 = _Some$548;
    _next$549->$0 = new_idx$545;
    moonbit_decref(_next$549);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$538,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$540,
  int32_t new_idx$539
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2546 =
    self$538->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1764 =
    _field$2546;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1765;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2545;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2544;
  int32_t _cnt$2795;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$541;
  moonbit_incref(entry$540);
  _tmp$1765 = entry$540;
  if (new_idx$539 < 0 || new_idx$539 >= Moonbit_array_length(entries$1764)) {
    moonbit_panic();
  }
  _old$2545
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1764[
      new_idx$539
    ];
  if (_old$2545) {
    moonbit_decref(_old$2545);
  }
  entries$1764[new_idx$539] = _tmp$1765;
  _field$2544 = entry$540->$1;
  _cnt$2795 = Moonbit_object_header(entry$540)->rc;
  if (_cnt$2795 > 1) {
    int32_t _new_cnt$2797;
    if (_field$2544) {
      moonbit_incref(_field$2544);
    }
    _new_cnt$2797 = _cnt$2795 - 1;
    Moonbit_object_header(entry$540)->rc = _new_cnt$2797;
  } else if (_cnt$2795 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2796 =
      entry$540->$5;
    moonbit_decref(_field$2796);
    moonbit_free(entry$540);
  }
  _bind$541 = _field$2544;
  if (_bind$541 == 0) {
    if (_bind$541) {
      moonbit_decref(_bind$541);
    }
    self$538->$6 = new_idx$539;
    moonbit_decref(self$538);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$542;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$543;
    moonbit_decref(self$538);
    _Some$542 = _bind$541;
    _next$543 = _Some$542;
    _next$543->$0 = new_idx$539;
    moonbit_decref(_next$543);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$532,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$534,
  int32_t new_idx$533
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2549 =
    self$532->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1762 =
    _field$2549;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1763;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2548;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2547;
  int32_t _cnt$2798;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$535;
  moonbit_incref(entry$534);
  _tmp$1763 = entry$534;
  if (new_idx$533 < 0 || new_idx$533 >= Moonbit_array_length(entries$1762)) {
    moonbit_panic();
  }
  _old$2548
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1762[
      new_idx$533
    ];
  if (_old$2548) {
    moonbit_decref(_old$2548);
  }
  entries$1762[new_idx$533] = _tmp$1763;
  _field$2547 = entry$534->$1;
  _cnt$2798 = Moonbit_object_header(entry$534)->rc;
  if (_cnt$2798 > 1) {
    int32_t _new_cnt$2801;
    if (_field$2547) {
      moonbit_incref(_field$2547);
    }
    _new_cnt$2801 = _cnt$2798 - 1;
    Moonbit_object_header(entry$534)->rc = _new_cnt$2801;
  } else if (_cnt$2798 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2800 =
      entry$534->$5;
    moonbit_string_t _field$2799;
    moonbit_decref(_field$2800);
    _field$2799 = entry$534->$4;
    moonbit_decref(_field$2799);
    moonbit_free(entry$534);
  }
  _bind$535 = _field$2547;
  if (_bind$535 == 0) {
    if (_bind$535) {
      moonbit_decref(_bind$535);
    }
    self$532->$6 = new_idx$533;
    moonbit_decref(self$532);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$536;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$537;
    moonbit_decref(self$532);
    _Some$536 = _bind$535;
    _next$537 = _Some$536;
    _next$537->$0 = new_idx$533;
    moonbit_decref(_next$537);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$529,
  int32_t idx$531,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$530
) {
  int32_t _bind$528 = self$529->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2551;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1758;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1759;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2550;
  int32_t size$1761;
  int32_t _tmp$1760;
  switch (_bind$528) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1753;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2552;
      moonbit_incref(entry$530);
      _tmp$1753 = entry$530;
      _old$2552 = self$529->$5;
      if (_old$2552) {
        moonbit_decref(_old$2552);
      }
      self$529->$5 = _tmp$1753;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2555 =
        self$529->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1757 =
        _field$2555;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2554;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1756;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1754;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1755;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2553;
      if (_bind$528 < 0 || _bind$528 >= Moonbit_array_length(entries$1757)) {
        moonbit_panic();
      }
      _tmp$2554
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1757[
          _bind$528
        ];
      _tmp$1756 = _tmp$2554;
      if (_tmp$1756) {
        moonbit_incref(_tmp$1756);
      }
      _tmp$1754 = $Option$$unwrap$3(_tmp$1756);
      moonbit_incref(entry$530);
      _tmp$1755 = entry$530;
      _old$2553 = _tmp$1754->$1;
      if (_old$2553) {
        moonbit_decref(_old$2553);
      }
      _tmp$1754->$1 = _tmp$1755;
      moonbit_decref(_tmp$1754);
      break;
    }
  }
  self$529->$6 = idx$531;
  _field$2551 = self$529->$0;
  entries$1758 = _field$2551;
  _tmp$1759 = entry$530;
  if (idx$531 < 0 || idx$531 >= Moonbit_array_length(entries$1758)) {
    moonbit_panic();
  }
  _old$2550
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1758[
      idx$531
    ];
  if (_old$2550) {
    moonbit_decref(_old$2550);
  }
  entries$1758[idx$531] = _tmp$1759;
  size$1761 = self$529->$1;
  _tmp$1760 = size$1761 + 1;
  self$529->$1 = _tmp$1760;
  moonbit_decref(self$529);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$525,
  int32_t idx$527,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$526
) {
  int32_t _bind$524 = self$525->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2557;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1749;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1750;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2556;
  int32_t size$1752;
  int32_t _tmp$1751;
  switch (_bind$524) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1744;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2558;
      moonbit_incref(entry$526);
      _tmp$1744 = entry$526;
      _old$2558 = self$525->$5;
      if (_old$2558) {
        moonbit_decref(_old$2558);
      }
      self$525->$5 = _tmp$1744;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2561 =
        self$525->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1748 =
        _field$2561;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2560;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1747;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1745;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1746;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2559;
      if (_bind$524 < 0 || _bind$524 >= Moonbit_array_length(entries$1748)) {
        moonbit_panic();
      }
      _tmp$2560
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1748[
          _bind$524
        ];
      _tmp$1747 = _tmp$2560;
      if (_tmp$1747) {
        moonbit_incref(_tmp$1747);
      }
      _tmp$1745 = $Option$$unwrap$2(_tmp$1747);
      moonbit_incref(entry$526);
      _tmp$1746 = entry$526;
      _old$2559 = _tmp$1745->$1;
      if (_old$2559) {
        moonbit_decref(_old$2559);
      }
      _tmp$1745->$1 = _tmp$1746;
      moonbit_decref(_tmp$1745);
      break;
    }
  }
  self$525->$6 = idx$527;
  _field$2557 = self$525->$0;
  entries$1749 = _field$2557;
  _tmp$1750 = entry$526;
  if (idx$527 < 0 || idx$527 >= Moonbit_array_length(entries$1749)) {
    moonbit_panic();
  }
  _old$2556
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1749[
      idx$527
    ];
  if (_old$2556) {
    moonbit_decref(_old$2556);
  }
  entries$1749[idx$527] = _tmp$1750;
  size$1752 = self$525->$1;
  _tmp$1751 = size$1752 + 1;
  self$525->$1 = _tmp$1751;
  moonbit_decref(self$525);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$521,
  int32_t idx$523,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$522
) {
  int32_t _bind$520 = self$521->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2563;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1740;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1741;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2562;
  int32_t size$1743;
  int32_t _tmp$1742;
  switch (_bind$520) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1735;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2564;
      moonbit_incref(entry$522);
      _tmp$1735 = entry$522;
      _old$2564 = self$521->$5;
      if (_old$2564) {
        moonbit_decref(_old$2564);
      }
      self$521->$5 = _tmp$1735;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2567 =
        self$521->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1739 =
        _field$2567;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2566;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1738;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1736;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1737;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2565;
      if (_bind$520 < 0 || _bind$520 >= Moonbit_array_length(entries$1739)) {
        moonbit_panic();
      }
      _tmp$2566
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1739[
          _bind$520
        ];
      _tmp$1738 = _tmp$2566;
      if (_tmp$1738) {
        moonbit_incref(_tmp$1738);
      }
      _tmp$1736 = $Option$$unwrap$1(_tmp$1738);
      moonbit_incref(entry$522);
      _tmp$1737 = entry$522;
      _old$2565 = _tmp$1736->$1;
      if (_old$2565) {
        moonbit_decref(_old$2565);
      }
      _tmp$1736->$1 = _tmp$1737;
      moonbit_decref(_tmp$1736);
      break;
    }
  }
  self$521->$6 = idx$523;
  _field$2563 = self$521->$0;
  entries$1740 = _field$2563;
  _tmp$1741 = entry$522;
  if (idx$523 < 0 || idx$523 >= Moonbit_array_length(entries$1740)) {
    moonbit_panic();
  }
  _old$2562
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1740[
      idx$523
    ];
  if (_old$2562) {
    moonbit_decref(_old$2562);
  }
  entries$1740[idx$523] = _tmp$1741;
  size$1743 = self$521->$1;
  _tmp$1742 = size$1743 + 1;
  self$521->$1 = _tmp$1742;
  moonbit_decref(self$521);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$517,
  int32_t idx$519,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$518
) {
  int32_t _bind$516 = self$517->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2569;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1731;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1732;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2568;
  int32_t size$1734;
  int32_t _tmp$1733;
  switch (_bind$516) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1726;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2570;
      moonbit_incref(entry$518);
      _tmp$1726 = entry$518;
      _old$2570 = self$517->$5;
      if (_old$2570) {
        moonbit_decref(_old$2570);
      }
      self$517->$5 = _tmp$1726;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2573 =
        self$517->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1730 =
        _field$2573;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2572;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1729;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1727;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1728;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2571;
      if (_bind$516 < 0 || _bind$516 >= Moonbit_array_length(entries$1730)) {
        moonbit_panic();
      }
      _tmp$2572
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1730[
          _bind$516
        ];
      _tmp$1729 = _tmp$2572;
      if (_tmp$1729) {
        moonbit_incref(_tmp$1729);
      }
      _tmp$1727 = $Option$$unwrap$0(_tmp$1729);
      moonbit_incref(entry$518);
      _tmp$1728 = entry$518;
      _old$2571 = _tmp$1727->$1;
      if (_old$2571) {
        moonbit_decref(_old$2571);
      }
      _tmp$1727->$1 = _tmp$1728;
      moonbit_decref(_tmp$1727);
      break;
    }
  }
  self$517->$6 = idx$519;
  _field$2569 = self$517->$0;
  entries$1731 = _field$2569;
  _tmp$1732 = entry$518;
  if (idx$519 < 0 || idx$519 >= Moonbit_array_length(entries$1731)) {
    moonbit_panic();
  }
  _old$2568
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1731[
      idx$519
    ];
  if (_old$2568) {
    moonbit_decref(_old$2568);
  }
  entries$1731[idx$519] = _tmp$1732;
  size$1734 = self$517->$1;
  _tmp$1733 = size$1734 + 1;
  self$517->$1 = _tmp$1733;
  moonbit_decref(self$517);
  return 0;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$511
) {
  int32_t capacity$510 = $Int$$next_power_of_two(capacity$511);
  int32_t _bind$512 = capacity$510 - 1;
  int32_t _bind$513 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$510);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1725 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$514 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$510, _tmp$1725
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$515 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2916 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2916)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2916->$0 = _bind$514;
  _block$2916->$1 = 0;
  _block$2916->$2 = capacity$510;
  _block$2916->$3 = _bind$512;
  _block$2916->$4 = _bind$513;
  _block$2916->$5 = _bind$515;
  _block$2916->$6 = -1;
  return _block$2916;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$505
) {
  int32_t capacity$504 = $Int$$next_power_of_two(capacity$505);
  int32_t _bind$506 = capacity$504 - 1;
  int32_t _bind$507 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$504);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1724 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$508 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$504, _tmp$1724
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$509 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2917 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2917)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2917->$0 = _bind$508;
  _block$2917->$1 = 0;
  _block$2917->$2 = capacity$504;
  _block$2917->$3 = _bind$506;
  _block$2917->$4 = _bind$507;
  _block$2917->$5 = _bind$509;
  _block$2917->$6 = -1;
  return _block$2917;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$499
) {
  int32_t capacity$498 = $Int$$next_power_of_two(capacity$499);
  int32_t _bind$500 = capacity$498 - 1;
  int32_t _bind$501 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$498);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1723 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$502 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$498, _tmp$1723
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$503 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$2918 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2918)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2918->$0 = _bind$502;
  _block$2918->$1 = 0;
  _block$2918->$2 = capacity$498;
  _block$2918->$3 = _bind$500;
  _block$2918->$4 = _bind$501;
  _block$2918->$5 = _bind$503;
  _block$2918->$6 = -1;
  return _block$2918;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$493
) {
  int32_t capacity$492 = $Int$$next_power_of_two(capacity$493);
  int32_t _bind$494 = capacity$492 - 1;
  int32_t _bind$495 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$492);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1722 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$496 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$492, _tmp$1722
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$497 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2919 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2919)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2919->$0 = _bind$496;
  _block$2919->$1 = 0;
  _block$2919->$2 = capacity$492;
  _block$2919->$3 = _bind$494;
  _block$2919->$4 = _bind$495;
  _block$2919->$5 = _bind$497;
  _block$2919->$6 = -1;
  return _block$2919;
}

int32_t $Int$$next_power_of_two(int32_t self$491) {
  if (self$491 >= 0) {
    int32_t _tmp$1721;
    int32_t _tmp$1720;
    int32_t _tmp$1719;
    int32_t _tmp$1718;
    if (self$491 <= 1) {
      return 1;
    }
    if (self$491 > 1073741824) {
      return 1073741824;
    }
    _tmp$1721 = self$491 - 1;
    _tmp$1720 = moonbit_clz32(_tmp$1721);
    _tmp$1719 = _tmp$1720 - 1;
    _tmp$1718 = 2147483647 >> (_tmp$1719 & 31);
    return _tmp$1718 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$490) {
  int32_t _tmp$1717 = capacity$490 * 13;
  return _tmp$1717 / 16;
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$488
) {
  if (self$488 == 0) {
    if (self$488) {
      moonbit_decref(self$488);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$489 =
      self$488;
    return _Some$489;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$486
) {
  if (self$486 == 0) {
    if (self$486) {
      moonbit_decref(self$486);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$487 =
      self$486;
    return _Some$487;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$484
) {
  if (self$484 == 0) {
    if (self$484) {
      moonbit_decref(self$484);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$485 =
      self$484;
    return _Some$485;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$482
) {
  if (self$482 == 0) {
    if (self$482) {
      moonbit_decref(self$482);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$483 =
      self$482;
    return _Some$483;
  }
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$481
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1716 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$481);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$1716);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$480
) {
  moonbit_string_t* _field$2575 = self$480->$0;
  moonbit_string_t* buf$1714 = _field$2575;
  int32_t _field$2574 = self$480->$1;
  int32_t _cnt$2802 = Moonbit_object_header(self$480)->rc;
  int32_t len$1715;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$1713;
  if (_cnt$2802 > 1) {
    int32_t _new_cnt$2803;
    moonbit_incref(buf$1714);
    _new_cnt$2803 = _cnt$2802 - 1;
    Moonbit_object_header(self$480)->rc = _new_cnt$2803;
  } else if (_cnt$2802 == 1) {
    moonbit_free(self$480);
  }
  len$1715 = _field$2574;
  _tmp$1713
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$1715, buf$1714
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$1713);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$478
) {
  struct $Ref$3c$Int$3e$* i$477 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$2920;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1702;
  Moonbit_object_header(i$477)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$477->$0 = 0;
  _closure$2920
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$2920)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$2920->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$2920->$0_0 = self$478.$0;
  _closure$2920->$0_1 = self$478.$1;
  _closure$2920->$0_2 = self$478.$2;
  _closure$2920->$1 = i$477;
  _tmp$1702 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$2920;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$1702);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1703
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$1704 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$1703;
  struct $Ref$3c$Int$3e$* _field$2580 = _casted_env$1704->$1;
  struct $Ref$3c$Int$3e$* i$477 = _field$2580;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$2579 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$1704->$0_1, _casted_env$1704->$0_2, _casted_env$1704->$0_0
    };
  int32_t _cnt$2804 = Moonbit_object_header(_casted_env$1704)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$478;
  int32_t val$1705;
  int32_t _tmp$1706;
  if (_cnt$2804 > 1) {
    int32_t _new_cnt$2805;
    moonbit_incref(i$477);
    moonbit_incref(_field$2579.$0);
    _new_cnt$2805 = _cnt$2804 - 1;
    Moonbit_object_header(_casted_env$1704)->rc = _new_cnt$2805;
  } else if (_cnt$2804 == 1) {
    moonbit_free(_casted_env$1704);
  }
  self$478 = _field$2579;
  val$1705 = i$477->$0;
  moonbit_incref(self$478.$0);
  _tmp$1706 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$478);
  if (val$1705 < _tmp$1706) {
    moonbit_string_t* _field$2578 = self$478.$0;
    moonbit_string_t* buf$1709 = _field$2578;
    int32_t _field$2577 = self$478.$1;
    int32_t start$1711 = _field$2577;
    int32_t val$1712 = i$477->$0;
    int32_t _tmp$1710 = start$1711 + val$1712;
    moonbit_string_t _tmp$2576 = (moonbit_string_t)buf$1709[_tmp$1710];
    moonbit_string_t elem$479;
    int32_t val$1708;
    int32_t _tmp$1707;
    moonbit_incref(_tmp$2576);
    moonbit_decref(buf$1709);
    elem$479 = _tmp$2576;
    val$1708 = i$477->$0;
    _tmp$1707 = val$1708 + 1;
    i$477->$0 = _tmp$1707;
    moonbit_decref(i$477);
    return elem$479;
  } else {
    moonbit_decref(self$478.$0);
    moonbit_decref(i$477);
    return 0;
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$476
) {
  return self$476;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$475,
  struct $$moonbitlang$core$builtin$Logger logger$474
) {
  moonbit_string_t _tmp$1701 = $Int$$to_string$inner(self$475, 10);
  logger$474.$0->$method_0(logger$474.$1, _tmp$1701);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$472,
  struct $$3c$String$3e$$3d$$3e$Int* f$473
) {
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _func$471 = self$472;
  return _func$471->code(_func$471, f$473);
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$468,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$470
) {
  int32_t len$1696 = self$468->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1698;
  int32_t _tmp$2583;
  int32_t _tmp$1697;
  int32_t length$469;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2582;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$1699;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2581;
  int32_t _tmp$1700;
  moonbit_incref(self$468);
  _tmp$1698 = $$moonbitlang$core$builtin$Array$$buffer$2(self$468);
  _tmp$2583 = Moonbit_array_length(_tmp$1698);
  moonbit_decref(_tmp$1698);
  _tmp$1697 = _tmp$2583;
  if (len$1696 == _tmp$1697) {
    moonbit_incref(self$468);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$468);
  }
  length$469 = self$468->$1;
  _field$2582 = self$468->$0;
  buf$1699 = _field$2582;
  _old$2581
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$1699[
      length$469
    ];
  if (_old$2581) {
    moonbit_decref(_old$2581);
  }
  buf$1699[length$469] = value$470;
  _tmp$1700 = length$469 + 1;
  self$468->$1 = _tmp$1700;
  moonbit_decref(self$468);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$465,
  struct $$3c$String$2a$Int$3e$* value$467
) {
  int32_t len$1691 = self$465->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$1693;
  int32_t _tmp$2586;
  int32_t _tmp$1692;
  int32_t length$466;
  struct $$3c$String$2a$Int$3e$** _field$2585;
  struct $$3c$String$2a$Int$3e$** buf$1694;
  struct $$3c$String$2a$Int$3e$* _old$2584;
  int32_t _tmp$1695;
  moonbit_incref(self$465);
  _tmp$1693 = $$moonbitlang$core$builtin$Array$$buffer$0(self$465);
  _tmp$2586 = Moonbit_array_length(_tmp$1693);
  moonbit_decref(_tmp$1693);
  _tmp$1692 = _tmp$2586;
  if (len$1691 == _tmp$1692) {
    moonbit_incref(self$465);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$465);
  }
  length$466 = self$465->$1;
  _field$2585 = self$465->$0;
  buf$1694 = _field$2585;
  _old$2584 = (struct $$3c$String$2a$Int$3e$*)buf$1694[length$466];
  if (_old$2584) {
    moonbit_decref(_old$2584);
  }
  buf$1694[length$466] = value$467;
  _tmp$1695 = length$466 + 1;
  self$465->$1 = _tmp$1695;
  moonbit_decref(self$465);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$462,
  moonbit_string_t value$464
) {
  int32_t len$1686 = self$462->$1;
  moonbit_string_t* _tmp$1688;
  int32_t _tmp$2589;
  int32_t _tmp$1687;
  int32_t length$463;
  moonbit_string_t* _field$2588;
  moonbit_string_t* buf$1689;
  moonbit_string_t _old$2587;
  int32_t _tmp$1690;
  moonbit_incref(self$462);
  _tmp$1688 = $$moonbitlang$core$builtin$Array$$buffer$1(self$462);
  _tmp$2589 = Moonbit_array_length(_tmp$1688);
  moonbit_decref(_tmp$1688);
  _tmp$1687 = _tmp$2589;
  if (len$1686 == _tmp$1687) {
    moonbit_incref(self$462);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$462);
  }
  length$463 = self$462->$1;
  _field$2588 = self$462->$0;
  buf$1689 = _field$2588;
  _old$2587 = (moonbit_string_t)buf$1689[length$463];
  moonbit_decref(_old$2587);
  buf$1689[length$463] = value$464;
  _tmp$1690 = length$463 + 1;
  self$462->$1 = _tmp$1690;
  moonbit_decref(self$462);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$460
) {
  int32_t old_cap$459 = self$460->$1;
  int32_t new_cap$461;
  if (old_cap$459 == 0) {
    new_cap$461 = 8;
  } else {
    new_cap$461 = old_cap$459 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$460, new_cap$461);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$457
) {
  int32_t old_cap$456 = self$457->$1;
  int32_t new_cap$458;
  if (old_cap$456 == 0) {
    new_cap$458 = 8;
  } else {
    new_cap$458 = old_cap$456 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$457, new_cap$458);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$454
) {
  int32_t old_cap$453 = self$454->$1;
  int32_t new_cap$455;
  if (old_cap$453 == 0) {
    new_cap$455 = 8;
  } else {
    new_cap$455 = old_cap$453 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$454, new_cap$455);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$450,
  int32_t new_capacity$448
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** new_buf$447 =
    (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_make_ref_array(
      new_capacity$448, 0
    );
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2591 =
    self$450->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$449 =
    _field$2591;
  int32_t old_cap$451 = Moonbit_array_length(old_buf$449);
  int32_t copy_len$452;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$2590;
  if (old_cap$451 < new_capacity$448) {
    copy_len$452 = old_cap$451;
  } else {
    copy_len$452 = new_capacity$448;
  }
  moonbit_incref(old_buf$449);
  moonbit_incref(new_buf$447);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$447, 0, old_buf$449, 0, copy_len$452
  );
  _old$2590 = self$450->$0;
  moonbit_decref(_old$2590);
  self$450->$0 = new_buf$447;
  moonbit_decref(self$450);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$444,
  int32_t new_capacity$442
) {
  struct $$3c$String$2a$Int$3e$** new_buf$441 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_make_ref_array(
      new_capacity$442, 0
    );
  struct $$3c$String$2a$Int$3e$** _field$2593 = self$444->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$443 = _field$2593;
  int32_t old_cap$445 = Moonbit_array_length(old_buf$443);
  int32_t copy_len$446;
  struct $$3c$String$2a$Int$3e$** _old$2592;
  if (old_cap$445 < new_capacity$442) {
    copy_len$446 = old_cap$445;
  } else {
    copy_len$446 = new_capacity$442;
  }
  moonbit_incref(old_buf$443);
  moonbit_incref(new_buf$441);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$441, 0, old_buf$443, 0, copy_len$446
  );
  _old$2592 = self$444->$0;
  moonbit_decref(_old$2592);
  self$444->$0 = new_buf$441;
  moonbit_decref(self$444);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$438,
  int32_t new_capacity$436
) {
  moonbit_string_t* new_buf$435 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$436, (moonbit_string_t)moonbit_string_literal_3.data
    );
  moonbit_string_t* _field$2595 = self$438->$0;
  moonbit_string_t* old_buf$437 = _field$2595;
  int32_t old_cap$439 = Moonbit_array_length(old_buf$437);
  int32_t copy_len$440;
  moonbit_string_t* _old$2594;
  if (old_cap$439 < new_capacity$436) {
    copy_len$440 = old_cap$439;
  } else {
    copy_len$440 = new_capacity$436;
  }
  moonbit_incref(old_buf$437);
  moonbit_incref(new_buf$435);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$435, 0, old_buf$437, 0, copy_len$440
  );
  _old$2594 = self$438->$0;
  moonbit_decref(_old$2594);
  self$438->$0 = new_buf$435;
  moonbit_decref(self$438);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$434
) {
  if (capacity$434 == 0) {
    moonbit_string_t* _tmp$1684 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2921 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2921)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2921->$0 = _tmp$1684;
    _block$2921->$1 = 0;
    return _block$2921;
  } else {
    moonbit_string_t* _tmp$1685 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$434, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2922 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2922)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2922->$0 = _tmp$1685;
    _block$2922->$1 = 0;
    return _block$2922;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$432,
  struct $StringView str$433
) {
  int32_t len$1672 = self$432->$1;
  int32_t _tmp$1674;
  int32_t _tmp$1673;
  int32_t _tmp$1671;
  moonbit_bytes_t _field$2596;
  moonbit_bytes_t data$1675;
  int32_t len$1676;
  moonbit_string_t _tmp$1677;
  int32_t _tmp$1678;
  int32_t _tmp$1679;
  int32_t len$1681;
  int32_t _tmp$1683;
  int32_t _tmp$1682;
  int32_t _tmp$1680;
  moonbit_incref(str$433.$0);
  _tmp$1674 = $StringView$$length(str$433);
  _tmp$1673 = _tmp$1674 * 2;
  _tmp$1671 = len$1672 + _tmp$1673;
  moonbit_incref(self$432);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$432, _tmp$1671
  );
  _field$2596 = self$432->$0;
  data$1675 = _field$2596;
  len$1676 = self$432->$1;
  moonbit_incref(data$1675);
  moonbit_incref(str$433.$0);
  _tmp$1677 = $StringView$$data(str$433);
  moonbit_incref(str$433.$0);
  _tmp$1678 = $StringView$$start_offset(str$433);
  moonbit_incref(str$433.$0);
  _tmp$1679 = $StringView$$length(str$433);
  $FixedArray$$blit_from_string(
    data$1675, len$1676, _tmp$1677, _tmp$1678, _tmp$1679
  );
  len$1681 = self$432->$1;
  _tmp$1683 = $StringView$$length(str$433);
  _tmp$1682 = _tmp$1683 * 2;
  _tmp$1680 = len$1681 + _tmp$1682;
  self$432->$1 = _tmp$1680;
  moonbit_decref(self$432);
  return 0;
}

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$429,
  int32_t i$430,
  int32_t start_offset$431,
  int64_t end_offset$427
) {
  int32_t end_offset$426;
  if (end_offset$427 == 4294967296ll) {
    end_offset$426 = Moonbit_array_length(self$429);
  } else {
    int64_t _Some$428 = end_offset$427;
    end_offset$426 = (int32_t)_Some$428;
  }
  if (i$430 >= 0) {
    return $String$$offset_of_nth_char_forward(
             self$429, i$430, start_offset$431, end_offset$426
           );
  } else {
    int32_t _tmp$1670 = -i$430;
    return $String$$offset_of_nth_char_backward(
             self$429, _tmp$1670, start_offset$431, end_offset$426
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$424,
  int32_t n$422,
  int32_t start_offset$418,
  int32_t end_offset$419
) {
  int32_t _if_result$2923;
  if (start_offset$418 >= 0) {
    _if_result$2923 = start_offset$418 <= end_offset$419;
  } else {
    _if_result$2923 = 0;
  }
  if (_if_result$2923) {
    int32_t utf16_offset$420 = start_offset$418;
    int32_t char_count$421 = 0;
    int32_t _tmp$1668;
    int32_t _if_result$2926;
    while (1) {
      int32_t _tmp$1662 = utf16_offset$420;
      int32_t _if_result$2925;
      if (_tmp$1662 < end_offset$419) {
        int32_t _tmp$1661 = char_count$421;
        _if_result$2925 = _tmp$1661 < n$422;
      } else {
        _if_result$2925 = 0;
      }
      if (_if_result$2925) {
        int32_t _tmp$1666 = utf16_offset$420;
        int32_t c$423 = self$424[_tmp$1666];
        int32_t _tmp$1665;
        if ($Int$$is_leading_surrogate(c$423)) {
          int32_t _tmp$1663 = utf16_offset$420;
          utf16_offset$420 = _tmp$1663 + 2;
        } else {
          int32_t _tmp$1664 = utf16_offset$420;
          utf16_offset$420 = _tmp$1664 + 1;
        }
        _tmp$1665 = char_count$421;
        char_count$421 = _tmp$1665 + 1;
        continue;
      } else {
        moonbit_decref(self$424);
      }
      break;
    }
    _tmp$1668 = char_count$421;
    if (_tmp$1668 < n$422) {
      _if_result$2926 = 1;
    } else {
      int32_t _tmp$1667 = utf16_offset$420;
      _if_result$2926 = _tmp$1667 >= end_offset$419;
    }
    if (_if_result$2926) {
      return 4294967296ll;
    } else {
      int32_t _tmp$1669 = utf16_offset$420;
      return (int64_t)_tmp$1669;
    }
  } else {
    moonbit_decref(self$424);
    return $moonbitlang$core$builtin$abort$3(
             (moonbit_string_t)moonbit_string_literal_31.data,
               (moonbit_string_t)moonbit_string_literal_32.data
           );
  }
}

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$416,
  int32_t n$414,
  int32_t start_offset$413,
  int32_t end_offset$412
) {
  int32_t char_count$410 = 0;
  int32_t utf16_offset$411 = end_offset$412;
  int32_t _tmp$1659;
  int32_t _if_result$2929;
  while (1) {
    int32_t _tmp$1652 = utf16_offset$411;
    int32_t _tmp$1651 = _tmp$1652 - 1;
    int32_t _if_result$2928;
    if (_tmp$1651 >= start_offset$413) {
      int32_t _tmp$1650 = char_count$410;
      _if_result$2928 = _tmp$1650 < n$414;
    } else {
      _if_result$2928 = 0;
    }
    if (_if_result$2928) {
      int32_t _tmp$1657 = utf16_offset$411;
      int32_t _tmp$1656 = _tmp$1657 - 1;
      int32_t c$415 = self$416[_tmp$1656];
      int32_t _tmp$1655;
      if ($Int$$is_trailing_surrogate(c$415)) {
        int32_t _tmp$1653 = utf16_offset$411;
        utf16_offset$411 = _tmp$1653 - 2;
      } else {
        int32_t _tmp$1654 = utf16_offset$411;
        utf16_offset$411 = _tmp$1654 - 1;
      }
      _tmp$1655 = char_count$410;
      char_count$410 = _tmp$1655 + 1;
      continue;
    } else {
      moonbit_decref(self$416);
    }
    break;
  }
  _tmp$1659 = char_count$410;
  if (_tmp$1659 < n$414) {
    _if_result$2929 = 1;
  } else {
    int32_t _tmp$1658 = utf16_offset$411;
    _if_result$2929 = _tmp$1658 < start_offset$413;
  }
  if (_if_result$2929) {
    return 4294967296ll;
  } else {
    int32_t _tmp$1660 = utf16_offset$411;
    return (int64_t)_tmp$1660;
  }
}

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$402,
  int32_t len$405,
  int32_t start_offset$409,
  int64_t end_offset$400
) {
  int32_t end_offset$399;
  int32_t index$403;
  int32_t count$404;
  if (end_offset$400 == 4294967296ll) {
    end_offset$399 = Moonbit_array_length(self$402);
  } else {
    int64_t _Some$401 = end_offset$400;
    end_offset$399 = (int32_t)_Some$401;
  }
  index$403 = start_offset$409;
  count$404 = 0;
  while (1) {
    int32_t _if_result$2931;
    if (index$403 < end_offset$399) {
      _if_result$2931 = count$404 < len$405;
    } else {
      _if_result$2931 = 0;
    }
    if (_if_result$2931) {
      int32_t c1$406 = self$402[index$403];
      int32_t _if_result$2932;
      int32_t _tmp$1648;
      int32_t _tmp$1649;
      if ($Int$$is_leading_surrogate(c1$406)) {
        int32_t _tmp$1644 = index$403 + 1;
        _if_result$2932 = _tmp$1644 < end_offset$399;
      } else {
        _if_result$2932 = 0;
      }
      if (_if_result$2932) {
        int32_t _tmp$1647 = index$403 + 1;
        int32_t c2$407 = self$402[_tmp$1647];
        if ($Int$$is_trailing_surrogate(c2$407)) {
          int32_t _tmp$1645 = index$403 + 2;
          int32_t _tmp$1646 = count$404 + 1;
          index$403 = _tmp$1645;
          count$404 = _tmp$1646;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_33.data,
              (moonbit_string_t)moonbit_string_literal_34.data
          );
        }
      }
      _tmp$1648 = index$403 + 1;
      _tmp$1649 = count$404 + 1;
      index$403 = _tmp$1648;
      count$404 = _tmp$1649;
      continue;
    } else {
      moonbit_decref(self$402);
      return count$404 >= len$405;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$398
) {
  int32_t end$1642 = self$398.$2;
  int32_t _field$2597 = self$398.$1;
  int32_t start$1643;
  moonbit_decref(self$398.$0);
  start$1643 = _field$2597;
  return end$1642 - start$1643;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$397
) {
  int32_t end$1640 = self$397.$2;
  int32_t _field$2598 = self$397.$1;
  int32_t start$1641;
  moonbit_decref(self$397.$0);
  start$1641 = _field$2598;
  return end$1640 - start$1641;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$396
) {
  int32_t end$1638 = self$396.$2;
  int32_t _field$2599 = self$396.$1;
  int32_t start$1639;
  moonbit_decref(self$396.$0);
  start$1639 = _field$2599;
  return end$1638 - start$1639;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$395
) {
  int32_t end$1636 = self$395.$2;
  int32_t _field$2600 = self$395.$1;
  int32_t start$1637;
  moonbit_decref(self$395.$0);
  start$1637 = _field$2600;
  return end$1636 - start$1637;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$394
) {
  int32_t end$1634 = self$394.$2;
  int32_t _field$2601 = self$394.$1;
  int32_t start$1635;
  moonbit_decref(self$394.$0);
  start$1635 = _field$2601;
  return end$1634 - start$1635;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$389
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$2933 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$2933)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2933->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$2933->$0 = self$389;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$2933;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1632,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$387
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$1633 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$1632;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$2602 =
    _casted_env$1633->$0;
  int32_t _cnt$2806 = Moonbit_object_header(_casted_env$1633)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$389;
  if (_cnt$2806 > 1) {
    int32_t _new_cnt$2807;
    moonbit_incref(_field$2602);
    _new_cnt$2807 = _cnt$2806 - 1;
    Moonbit_object_header(_casted_env$1633)->rc = _new_cnt$2807;
  } else if (_cnt$2806 == 1) {
    moonbit_free(_casted_env$1633);
  }
  self$389 = _field$2602;
  while (1) {
    moonbit_string_t _bind$388;
    moonbit_incref(self$389);
    _bind$388 = $$moonbitlang$core$builtin$Iterator$$next$0(self$389);
    if (_bind$388 == 0) {
      moonbit_decref(self$389);
      if (_bind$388) {
        moonbit_decref(_bind$388);
      }
      moonbit_decref(yield_$387);
      return 1;
    } else {
      moonbit_string_t _Some$390 = _bind$388;
      moonbit_string_t _x$391 = _Some$390;
      int32_t _bind$392;
      moonbit_incref(yield_$387);
      _bind$392 = yield_$387->code(yield_$387, _x$391);
      switch (_bind$392) {
        case 1:
          break;
        default: {
          moonbit_decref(self$389);
          moonbit_decref(yield_$387);
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$386
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _func$385 = self$386;
  return _func$385->code(_func$385);
}

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$377,
  struct $$moonbitlang$core$builtin$Logger logger$375
) {
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$376;
  int32_t len$378;
  int32_t i$379;
  int32_t seg$380;
  if (logger$375.$1) {
    moonbit_incref(logger$375.$1);
  }
  logger$375.$0->$method_3(logger$375.$1, 34);
  if (logger$375.$1) {
    moonbit_incref(logger$375.$1);
  }
  moonbit_incref(self$377);
  _env$376
  = (struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$)
    );
  Moonbit_object_header(_env$376)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$, $0_0
    )
    >> 2,
      3,
      0
  );
  _env$376->$0_0 = logger$375.$0;
  _env$376->$0_1 = logger$375.$1;
  _env$376->$1 = self$377;
  len$378 = Moonbit_array_length(self$377);
  i$379 = 0;
  seg$380 = 0;
  $$2a$for$381:;
  while (1) {
    int32_t code$382;
    int32_t c$384;
    struct $$moonbitlang$core$builtin$Logger _bind$1614;
    int32_t _tmp$1615;
    int32_t _tmp$1616;
    int32_t _tmp$1617;
    int32_t _tmp$2938;
    int32_t _tmp$2939;
    if (i$379 >= len$378) {
      moonbit_decref(self$377);
      $moonbitlang$core$builtin$output$flush_segment$7c$3831(
        _env$376, seg$380, i$379
      );
      break;
    }
    code$382 = self$377[i$379];
    switch (code$382) {
      case 34: {
        c$384 = code$382;
        goto $join$383;
        break;
      }
      
      case 92: {
        c$384 = code$382;
        goto $join$383;
        break;
      }
      
      case 10: {
        int32_t _tmp$1618;
        int32_t _tmp$1619;
        moonbit_incref(_env$376);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$376, seg$380, i$379
        );
        if (logger$375.$1) {
          moonbit_incref(logger$375.$1);
        }
        logger$375.$0->$method_0(
          logger$375.$1, (moonbit_string_t)moonbit_string_literal_35.data
        );
        _tmp$1618 = i$379 + 1;
        _tmp$1619 = i$379 + 1;
        i$379 = _tmp$1618;
        seg$380 = _tmp$1619;
        goto $$2a$for$381;
        break;
      }
      
      case 13: {
        int32_t _tmp$1620;
        int32_t _tmp$1621;
        moonbit_incref(_env$376);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$376, seg$380, i$379
        );
        if (logger$375.$1) {
          moonbit_incref(logger$375.$1);
        }
        logger$375.$0->$method_0(
          logger$375.$1, (moonbit_string_t)moonbit_string_literal_36.data
        );
        _tmp$1620 = i$379 + 1;
        _tmp$1621 = i$379 + 1;
        i$379 = _tmp$1620;
        seg$380 = _tmp$1621;
        goto $$2a$for$381;
        break;
      }
      
      case 8: {
        int32_t _tmp$1622;
        int32_t _tmp$1623;
        moonbit_incref(_env$376);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$376, seg$380, i$379
        );
        if (logger$375.$1) {
          moonbit_incref(logger$375.$1);
        }
        logger$375.$0->$method_0(
          logger$375.$1, (moonbit_string_t)moonbit_string_literal_37.data
        );
        _tmp$1622 = i$379 + 1;
        _tmp$1623 = i$379 + 1;
        i$379 = _tmp$1622;
        seg$380 = _tmp$1623;
        goto $$2a$for$381;
        break;
      }
      
      case 9: {
        int32_t _tmp$1624;
        int32_t _tmp$1625;
        moonbit_incref(_env$376);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$376, seg$380, i$379
        );
        if (logger$375.$1) {
          moonbit_incref(logger$375.$1);
        }
        logger$375.$0->$method_0(
          logger$375.$1, (moonbit_string_t)moonbit_string_literal_38.data
        );
        _tmp$1624 = i$379 + 1;
        _tmp$1625 = i$379 + 1;
        i$379 = _tmp$1624;
        seg$380 = _tmp$1625;
        goto $$2a$for$381;
        break;
      }
      default: {
        if (code$382 < 32) {
          int32_t _tmp$1628;
          moonbit_string_t _tmp$1627;
          struct $$moonbitlang$core$builtin$Logger _bind$1626;
          int32_t _tmp$1629;
          int32_t _tmp$1630;
          moonbit_incref(_env$376);
          $moonbitlang$core$builtin$output$flush_segment$7c$3831(
            _env$376, seg$380, i$379
          );
          if (logger$375.$1) {
            moonbit_incref(logger$375.$1);
          }
          logger$375.$0->$method_0(
            logger$375.$1, (moonbit_string_t)moonbit_string_literal_39.data
          );
          _tmp$1628 = code$382 & 0xff;
          _tmp$1627 = $Byte$$to_hex(_tmp$1628);
          if (logger$375.$1) {
            moonbit_incref(logger$375.$1);
          }
          logger$375.$0->$method_0(logger$375.$1, _tmp$1627);
          _bind$1626 = logger$375;
          if (_bind$1626.$1) {
            moonbit_incref(_bind$1626.$1);
          }
          _bind$1626.$0->$method_3(_bind$1626.$1, 125);
          _tmp$1629 = i$379 + 1;
          _tmp$1630 = i$379 + 1;
          i$379 = _tmp$1629;
          seg$380 = _tmp$1630;
          goto $$2a$for$381;
        } else {
          int32_t _tmp$1631 = i$379 + 1;
          int32_t _tmp$2937 = seg$380;
          i$379 = _tmp$1631;
          seg$380 = _tmp$2937;
          goto $$2a$for$381;
        }
        break;
      }
    }
    goto $joinlet$2936;
    $join$383:;
    moonbit_incref(_env$376);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$376, seg$380, i$379
    );
    if (logger$375.$1) {
      moonbit_incref(logger$375.$1);
    }
    logger$375.$0->$method_3(logger$375.$1, 92);
    _bind$1614 = logger$375;
    _tmp$1615 = c$384;
    if (_bind$1614.$1) {
      moonbit_incref(_bind$1614.$1);
    }
    _bind$1614.$0->$method_3(_bind$1614.$1, _tmp$1615);
    _tmp$1616 = i$379 + 1;
    _tmp$1617 = i$379 + 1;
    i$379 = _tmp$1616;
    seg$380 = _tmp$1617;
    continue;
    $joinlet$2936:;
    _tmp$2938 = i$379;
    _tmp$2939 = seg$380;
    i$379 = _tmp$2938;
    seg$380 = _tmp$2939;
    continue;
    break;
  }
  logger$375.$0->$method_3(logger$375.$1, 34);
  return 0;
}

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$371,
  int32_t seg$374,
  int32_t i$373
) {
  moonbit_string_t _field$2604 = _env$371->$1;
  moonbit_string_t self$370 = _field$2604;
  struct $$moonbitlang$core$builtin$Logger _field$2603 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$371->$0_0, _env$371->$0_1
    };
  int32_t _cnt$2808 = Moonbit_object_header(_env$371)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$372;
  if (_cnt$2808 > 1) {
    int32_t _new_cnt$2809;
    moonbit_incref(self$370);
    if (_field$2603.$1) {
      moonbit_incref(_field$2603.$1);
    }
    _new_cnt$2809 = _cnt$2808 - 1;
    Moonbit_object_header(_env$371)->rc = _new_cnt$2809;
  } else if (_cnt$2808 == 1) {
    moonbit_free(_env$371);
  }
  logger$372 = _field$2603;
  if (i$373 > seg$374) {
    int32_t _tmp$1613 = i$373 - seg$374;
    logger$372.$0->$method_1(logger$372.$1, self$370, seg$374, _tmp$1613);
  } else {
    if (logger$372.$1) {
      moonbit_decref(logger$372.$1);
    }
    moonbit_decref(self$370);
  }
  return 0;
}

moonbit_string_t $Byte$$to_hex(int32_t b$369) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$368 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _tmp$1610 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$369, 16);
  int32_t _tmp$1609 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$1610);
  int32_t _tmp$1612;
  int32_t _tmp$1611;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$1608;
  moonbit_incref(_self$368);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$368, _tmp$1609
  );
  _tmp$1612 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$369, 16);
  _tmp$1611
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$1612
  );
  moonbit_incref(_self$368);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$368, _tmp$1611
  );
  _tmp$1608 = _self$368;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$1608);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$367) {
  if (i$367 < 10) {
    int32_t _tmp$1605 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$367, 48);
    return $Byte$$to_char(_tmp$1605);
  } else {
    int32_t _tmp$1607 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$367, 97);
    int32_t _tmp$1606 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$1607, 10);
    return $Byte$$to_char(_tmp$1606);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$365,
  int32_t that$366
) {
  int32_t _tmp$1603 = (int32_t)self$365;
  int32_t _tmp$1604 = (int32_t)that$366;
  int32_t _tmp$1602 = _tmp$1603 - _tmp$1604;
  return _tmp$1602 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$363,
  int32_t that$364
) {
  int32_t _tmp$1600 = (int32_t)self$363;
  int32_t _tmp$1601 = (int32_t)that$364;
  int32_t _tmp$1599 = _tmp$1600 % _tmp$1601;
  return _tmp$1599 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$361,
  int32_t that$362
) {
  int32_t _tmp$1597 = (int32_t)self$361;
  int32_t _tmp$1598 = (int32_t)that$362;
  int32_t _tmp$1596 = _tmp$1597 / _tmp$1598;
  return _tmp$1596 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$359,
  int32_t that$360
) {
  int32_t _tmp$1594 = (int32_t)self$359;
  int32_t _tmp$1595 = (int32_t)that$360;
  int32_t _tmp$1593 = _tmp$1594 + _tmp$1595;
  return _tmp$1593 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$356,
  int32_t start$354,
  int32_t end$355
) {
  int32_t _if_result$2940;
  int32_t len$357;
  int32_t _tmp$1591;
  int32_t _tmp$1592;
  moonbit_bytes_t bytes$358;
  moonbit_bytes_t _tmp$1590;
  if (start$354 == 0) {
    int32_t _tmp$1589 = Moonbit_array_length(str$356);
    _if_result$2940 = end$355 == _tmp$1589;
  } else {
    _if_result$2940 = 0;
  }
  if (_if_result$2940) {
    return str$356;
  }
  len$357 = end$355 - start$354;
  _tmp$1591 = len$357 * 2;
  _tmp$1592 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$358 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1591, _tmp$1592);
  moonbit_incref(bytes$358);
  $FixedArray$$blit_from_string(bytes$358, 0, str$356, start$354, len$357);
  _tmp$1590 = bytes$358;
  return $Bytes$$to_unchecked_string$inner(_tmp$1590, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$353
) {
  return f$353;
}

moonbit_string_t $Int$$to_string$inner(int32_t self$337, int32_t radix$336) {
  int32_t _if_result$2941;
  int32_t is_negative$338;
  uint32_t num$339;
  uint16_t* buffer$340;
  if (radix$336 < 2) {
    _if_result$2941 = 1;
  } else {
    _if_result$2941 = radix$336 > 36;
  }
  if (_if_result$2941) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_40.data,
        (moonbit_string_t)moonbit_string_literal_41.data
    );
  }
  if (self$337 == 0) {
    return (moonbit_string_t)moonbit_string_literal_42.data;
  }
  is_negative$338 = self$337 < 0;
  if (is_negative$338) {
    int32_t _tmp$1588 = -self$337;
    num$339 = *(uint32_t*)&_tmp$1588;
  } else {
    num$339 = *(uint32_t*)&self$337;
  }
  switch (radix$336) {
    case 10: {
      int32_t digit_len$341 = $moonbitlang$core$builtin$dec_count32(num$339);
      int32_t _tmp$1585;
      int32_t total_len$342;
      uint16_t* buffer$343;
      int32_t digit_start$344;
      if (is_negative$338) {
        _tmp$1585 = 1;
      } else {
        _tmp$1585 = 0;
      }
      total_len$342 = digit_len$341 + _tmp$1585;
      buffer$343 = (uint16_t*)moonbit_make_string(total_len$342, 0);
      if (is_negative$338) {
        digit_start$344 = 1;
      } else {
        digit_start$344 = 0;
      }
      moonbit_incref(buffer$343);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$343, num$339, digit_start$344, total_len$342
      );
      buffer$340 = buffer$343;
      break;
    }
    
    case 16: {
      int32_t digit_len$345 = $moonbitlang$core$builtin$hex_count32(num$339);
      int32_t _tmp$1586;
      int32_t total_len$346;
      uint16_t* buffer$347;
      int32_t digit_start$348;
      if (is_negative$338) {
        _tmp$1586 = 1;
      } else {
        _tmp$1586 = 0;
      }
      total_len$346 = digit_len$345 + _tmp$1586;
      buffer$347 = (uint16_t*)moonbit_make_string(total_len$346, 0);
      if (is_negative$338) {
        digit_start$348 = 1;
      } else {
        digit_start$348 = 0;
      }
      moonbit_incref(buffer$347);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$347, num$339, digit_start$348, total_len$346
      );
      buffer$340 = buffer$347;
      break;
    }
    default: {
      int32_t digit_len$349 =
        $moonbitlang$core$builtin$radix_count32(num$339, radix$336);
      int32_t _tmp$1587;
      int32_t total_len$350;
      uint16_t* buffer$351;
      int32_t digit_start$352;
      if (is_negative$338) {
        _tmp$1587 = 1;
      } else {
        _tmp$1587 = 0;
      }
      total_len$350 = digit_len$349 + _tmp$1587;
      buffer$351 = (uint16_t*)moonbit_make_string(total_len$350, 0);
      if (is_negative$338) {
        digit_start$352 = 1;
      } else {
        digit_start$352 = 0;
      }
      moonbit_incref(buffer$351);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$351, num$339, digit_start$352, total_len$350, radix$336
      );
      buffer$340 = buffer$351;
      break;
    }
  }
  if (is_negative$338) {
    buffer$340[0] = 45;
  }
  return buffer$340;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$330,
  int32_t radix$333
) {
  uint32_t num$331;
  uint32_t base$332;
  int32_t count$334;
  if (value$330 == 0u) {
    return 1;
  }
  num$331 = value$330;
  base$332 = *(uint32_t*)&radix$333;
  count$334 = 0;
  while (1) {
    uint32_t _tmp$1582 = num$331;
    if (_tmp$1582 > 0u) {
      int32_t _tmp$1583 = count$334;
      uint32_t _tmp$1584;
      count$334 = _tmp$1583 + 1;
      _tmp$1584 = num$331;
      num$331 = _tmp$1584 / base$332;
      continue;
    }
    break;
  }
  return count$334;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$328) {
  if (value$328 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$329 = moonbit_clz32(value$328);
    int32_t _tmp$1581 = 31 - leading_zeros$329;
    int32_t _tmp$1580 = _tmp$1581 / 4;
    return _tmp$1580 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$327) {
  if (value$327 >= 100000u) {
    if (value$327 >= 10000000u) {
      if (value$327 >= 1000000000u) {
        return 10;
      } else if (value$327 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$327 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$327 >= 1000u) {
    if (value$327 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$327 >= 100u) {
    return 3;
  } else if (value$327 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$317,
  uint32_t num$305,
  int32_t digit_start$308,
  int32_t total_len$307
) {
  uint32_t num$304 = num$305;
  int32_t offset$306 = total_len$307 - digit_start$308;
  uint32_t _tmp$1579;
  int32_t remaining$319;
  int32_t _tmp$1560;
  while (1) {
    uint32_t _tmp$1523 = num$304;
    if (_tmp$1523 >= 10000u) {
      uint32_t _tmp$1546 = num$304;
      uint32_t t$309 = _tmp$1546 / 10000u;
      uint32_t _tmp$1545 = num$304;
      uint32_t _tmp$1544 = _tmp$1545 % 10000u;
      int32_t r$310 = *(int32_t*)&_tmp$1544;
      int32_t d1$311;
      int32_t d2$312;
      int32_t _tmp$1524;
      int32_t _tmp$1543;
      int32_t _tmp$1542;
      int32_t d1_hi$313;
      int32_t _tmp$1541;
      int32_t _tmp$1540;
      int32_t d1_lo$314;
      int32_t _tmp$1539;
      int32_t _tmp$1538;
      int32_t d2_hi$315;
      int32_t _tmp$1537;
      int32_t _tmp$1536;
      int32_t d2_lo$316;
      int32_t _tmp$1526;
      int32_t _tmp$1525;
      int32_t _tmp$1529;
      int32_t _tmp$1528;
      int32_t _tmp$1527;
      int32_t _tmp$1532;
      int32_t _tmp$1531;
      int32_t _tmp$1530;
      int32_t _tmp$1535;
      int32_t _tmp$1534;
      int32_t _tmp$1533;
      num$304 = t$309;
      d1$311 = r$310 / 100;
      d2$312 = r$310 % 100;
      _tmp$1524 = offset$306;
      offset$306 = _tmp$1524 - 4;
      _tmp$1543 = d1$311 / 10;
      _tmp$1542 = 48 + _tmp$1543;
      d1_hi$313 = (uint16_t)_tmp$1542;
      _tmp$1541 = d1$311 % 10;
      _tmp$1540 = 48 + _tmp$1541;
      d1_lo$314 = (uint16_t)_tmp$1540;
      _tmp$1539 = d2$312 / 10;
      _tmp$1538 = 48 + _tmp$1539;
      d2_hi$315 = (uint16_t)_tmp$1538;
      _tmp$1537 = d2$312 % 10;
      _tmp$1536 = 48 + _tmp$1537;
      d2_lo$316 = (uint16_t)_tmp$1536;
      _tmp$1526 = offset$306;
      _tmp$1525 = digit_start$308 + _tmp$1526;
      buffer$317[_tmp$1525] = d1_hi$313;
      _tmp$1529 = offset$306;
      _tmp$1528 = digit_start$308 + _tmp$1529;
      _tmp$1527 = _tmp$1528 + 1;
      buffer$317[_tmp$1527] = d1_lo$314;
      _tmp$1532 = offset$306;
      _tmp$1531 = digit_start$308 + _tmp$1532;
      _tmp$1530 = _tmp$1531 + 2;
      buffer$317[_tmp$1530] = d2_hi$315;
      _tmp$1535 = offset$306;
      _tmp$1534 = digit_start$308 + _tmp$1535;
      _tmp$1533 = _tmp$1534 + 3;
      buffer$317[_tmp$1533] = d2_lo$316;
      continue;
    }
    break;
  }
  _tmp$1579 = num$304;
  remaining$319 = *(int32_t*)&_tmp$1579;
  while (1) {
    int32_t _tmp$1547 = remaining$319;
    if (_tmp$1547 >= 100) {
      int32_t _tmp$1559 = remaining$319;
      int32_t t$320 = _tmp$1559 / 100;
      int32_t _tmp$1558 = remaining$319;
      int32_t d$321 = _tmp$1558 % 100;
      int32_t _tmp$1548;
      int32_t _tmp$1557;
      int32_t _tmp$1556;
      int32_t d_hi$322;
      int32_t _tmp$1555;
      int32_t _tmp$1554;
      int32_t d_lo$323;
      int32_t _tmp$1550;
      int32_t _tmp$1549;
      int32_t _tmp$1553;
      int32_t _tmp$1552;
      int32_t _tmp$1551;
      remaining$319 = t$320;
      _tmp$1548 = offset$306;
      offset$306 = _tmp$1548 - 2;
      _tmp$1557 = d$321 / 10;
      _tmp$1556 = 48 + _tmp$1557;
      d_hi$322 = (uint16_t)_tmp$1556;
      _tmp$1555 = d$321 % 10;
      _tmp$1554 = 48 + _tmp$1555;
      d_lo$323 = (uint16_t)_tmp$1554;
      _tmp$1550 = offset$306;
      _tmp$1549 = digit_start$308 + _tmp$1550;
      buffer$317[_tmp$1549] = d_hi$322;
      _tmp$1553 = offset$306;
      _tmp$1552 = digit_start$308 + _tmp$1553;
      _tmp$1551 = _tmp$1552 + 1;
      buffer$317[_tmp$1551] = d_lo$323;
      continue;
    }
    break;
  }
  _tmp$1560 = remaining$319;
  if (_tmp$1560 >= 10) {
    int32_t _tmp$1561 = offset$306;
    int32_t _tmp$1572;
    int32_t _tmp$1571;
    int32_t _tmp$1570;
    int32_t d_hi$325;
    int32_t _tmp$1569;
    int32_t _tmp$1568;
    int32_t _tmp$1567;
    int32_t d_lo$326;
    int32_t _tmp$1563;
    int32_t _tmp$1562;
    int32_t _tmp$1566;
    int32_t _tmp$1565;
    int32_t _tmp$1564;
    offset$306 = _tmp$1561 - 2;
    _tmp$1572 = remaining$319;
    _tmp$1571 = _tmp$1572 / 10;
    _tmp$1570 = 48 + _tmp$1571;
    d_hi$325 = (uint16_t)_tmp$1570;
    _tmp$1569 = remaining$319;
    _tmp$1568 = _tmp$1569 % 10;
    _tmp$1567 = 48 + _tmp$1568;
    d_lo$326 = (uint16_t)_tmp$1567;
    _tmp$1563 = offset$306;
    _tmp$1562 = digit_start$308 + _tmp$1563;
    buffer$317[_tmp$1562] = d_hi$325;
    _tmp$1566 = offset$306;
    _tmp$1565 = digit_start$308 + _tmp$1566;
    _tmp$1564 = _tmp$1565 + 1;
    buffer$317[_tmp$1564] = d_lo$326;
    moonbit_decref(buffer$317);
  } else {
    int32_t _tmp$1573 = offset$306;
    int32_t _tmp$1578;
    int32_t _tmp$1574;
    int32_t _tmp$1577;
    int32_t _tmp$1576;
    int32_t _tmp$1575;
    offset$306 = _tmp$1573 - 1;
    _tmp$1578 = offset$306;
    _tmp$1574 = digit_start$308 + _tmp$1578;
    _tmp$1577 = remaining$319;
    _tmp$1576 = 48 + _tmp$1577;
    _tmp$1575 = (uint16_t)_tmp$1576;
    buffer$317[_tmp$1574] = _tmp$1575;
    moonbit_decref(buffer$317);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$299,
  uint32_t num$293,
  int32_t digit_start$291,
  int32_t total_len$290,
  int32_t radix$295
) {
  int32_t offset$289 = total_len$290 - digit_start$291;
  uint32_t n$292 = num$293;
  uint32_t base$294 = *(uint32_t*)&radix$295;
  int32_t _tmp$1503 = radix$295 - 1;
  int32_t _tmp$1502 = radix$295 & _tmp$1503;
  if (_tmp$1502 == 0) {
    int32_t shift$296 = moonbit_ctz32(radix$295);
    uint32_t mask$297 = base$294 - 1u;
    while (1) {
      uint32_t _tmp$1504 = n$292;
      if (_tmp$1504 > 0u) {
        int32_t _tmp$1505 = offset$289;
        uint32_t _tmp$1512;
        uint32_t _tmp$1511;
        int32_t digit$298;
        int32_t _tmp$1509;
        int32_t _tmp$1506;
        int32_t _tmp$1508;
        int32_t _tmp$1507;
        uint32_t _tmp$1510;
        offset$289 = _tmp$1505 - 1;
        _tmp$1512 = n$292;
        _tmp$1511 = _tmp$1512 & mask$297;
        digit$298 = *(int32_t*)&_tmp$1511;
        _tmp$1509 = offset$289;
        _tmp$1506 = digit_start$291 + _tmp$1509;
        _tmp$1508
        = ((moonbit_string_t)moonbit_string_literal_43.data)[
          digit$298
        ];
        _tmp$1507 = (uint16_t)_tmp$1508;
        buffer$299[_tmp$1506] = _tmp$1507;
        _tmp$1510 = n$292;
        n$292 = _tmp$1510 >> (shift$296 & 31);
        continue;
      } else {
        moonbit_decref(buffer$299);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1513 = n$292;
      if (_tmp$1513 > 0u) {
        int32_t _tmp$1514 = offset$289;
        uint32_t _tmp$1522;
        uint32_t q$301;
        uint32_t _tmp$1520;
        uint32_t _tmp$1521;
        uint32_t _tmp$1519;
        int32_t digit$302;
        int32_t _tmp$1518;
        int32_t _tmp$1515;
        int32_t _tmp$1517;
        int32_t _tmp$1516;
        offset$289 = _tmp$1514 - 1;
        _tmp$1522 = n$292;
        q$301 = _tmp$1522 / base$294;
        _tmp$1520 = n$292;
        _tmp$1521 = q$301 * base$294;
        _tmp$1519 = _tmp$1520 - _tmp$1521;
        digit$302 = *(int32_t*)&_tmp$1519;
        _tmp$1518 = offset$289;
        _tmp$1515 = digit_start$291 + _tmp$1518;
        _tmp$1517
        = ((moonbit_string_t)moonbit_string_literal_43.data)[
          digit$302
        ];
        _tmp$1516 = (uint16_t)_tmp$1517;
        buffer$299[_tmp$1515] = _tmp$1516;
        n$292 = q$301;
        continue;
      } else {
        moonbit_decref(buffer$299);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$286,
  uint32_t num$282,
  int32_t digit_start$280,
  int32_t total_len$279
) {
  int32_t offset$278 = total_len$279 - digit_start$280;
  uint32_t n$281 = num$282;
  int32_t _tmp$1497;
  while (1) {
    int32_t _tmp$1483 = offset$278;
    if (_tmp$1483 >= 2) {
      int32_t _tmp$1484 = offset$278;
      uint32_t _tmp$1496;
      uint32_t _tmp$1495;
      int32_t byte_val$283;
      int32_t hi$284;
      int32_t lo$285;
      int32_t _tmp$1488;
      int32_t _tmp$1485;
      int32_t _tmp$1487;
      int32_t _tmp$1486;
      int32_t _tmp$1493;
      int32_t _tmp$1492;
      int32_t _tmp$1489;
      int32_t _tmp$1491;
      int32_t _tmp$1490;
      uint32_t _tmp$1494;
      offset$278 = _tmp$1484 - 2;
      _tmp$1496 = n$281;
      _tmp$1495 = _tmp$1496 & 255u;
      byte_val$283 = *(int32_t*)&_tmp$1495;
      hi$284 = byte_val$283 / 16;
      lo$285 = byte_val$283 % 16;
      _tmp$1488 = offset$278;
      _tmp$1485 = digit_start$280 + _tmp$1488;
      _tmp$1487 = ((moonbit_string_t)moonbit_string_literal_43.data)[hi$284];
      _tmp$1486 = (uint16_t)_tmp$1487;
      buffer$286[_tmp$1485] = _tmp$1486;
      _tmp$1493 = offset$278;
      _tmp$1492 = digit_start$280 + _tmp$1493;
      _tmp$1489 = _tmp$1492 + 1;
      _tmp$1491 = ((moonbit_string_t)moonbit_string_literal_43.data)[lo$285];
      _tmp$1490 = (uint16_t)_tmp$1491;
      buffer$286[_tmp$1489] = _tmp$1490;
      _tmp$1494 = n$281;
      n$281 = _tmp$1494 >> 8;
      continue;
    }
    break;
  }
  _tmp$1497 = offset$278;
  if (_tmp$1497 == 1) {
    uint32_t _tmp$1501 = n$281;
    uint32_t _tmp$1500 = _tmp$1501 & 15u;
    int32_t nibble$288 = *(int32_t*)&_tmp$1500;
    int32_t _tmp$1499 =
      ((moonbit_string_t)moonbit_string_literal_43.data)[nibble$288];
    int32_t _tmp$1498 = (uint16_t)_tmp$1499;
    buffer$286[digit_start$280] = _tmp$1498;
    moonbit_decref(buffer$286);
  } else {
    moonbit_decref(buffer$286);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$277
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$276 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1482;
  moonbit_incref(logger$276);
  _tmp$1482
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$276
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$277, _tmp$1482
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$276);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$275
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$274 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1481;
  moonbit_incref(logger$274);
  _tmp$1481
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$274
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$275, _tmp$1481
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$274);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$273
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$272 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1480;
  moonbit_incref(logger$272);
  _tmp$1480
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$272
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$273, _tmp$1480
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$272);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$271
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$270 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1479;
  moonbit_incref(logger$270);
  _tmp$1479
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$270
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$271, _tmp$1479);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$270);
}

int32_t $StringView$$start_offset(struct $StringView self$269) {
  int32_t _field$2605 = self$269.$1;
  moonbit_decref(self$269.$0);
  return _field$2605;
}

moonbit_string_t $StringView$$data(struct $StringView self$268) {
  moonbit_string_t _field$2606 = self$268.$0;
  return _field$2606;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$262,
  moonbit_string_t value$265,
  int32_t start$266,
  int32_t len$267
) {
  void* _try_err$264;
  struct $StringView _tmp$1474;
  int32_t _tmp$1476 = start$266 + len$267;
  int64_t _tmp$1475 = (int64_t)_tmp$1476;
  struct moonbit_result_1 _tmp$2949 =
    $String$$sub$inner(value$265, start$266, _tmp$1475);
  if (_tmp$2949.tag) {
    struct $StringView const _ok$1477 = _tmp$2949.data.ok;
    _tmp$1474 = _ok$1477;
  } else {
    void* const _err$1478 = _tmp$2949.data.err;
    _try_err$264 = _err$1478;
    goto $join$263;
  }
  goto $joinlet$2948;
  $join$263:;
  moonbit_decref(_try_err$264);
  moonbit_panic();
  $joinlet$2948:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$262, _tmp$1474
  );
  return 0;
}

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$255,
  int32_t start$261,
  int64_t end$257
) {
  int32_t len$254 = Moonbit_array_length(self$255);
  int32_t end$256;
  int32_t start$260;
  int32_t _if_result$2950;
  if (end$257 == 4294967296ll) {
    end$256 = len$254;
  } else {
    int64_t _Some$258 = end$257;
    int32_t _end$259 = (int32_t)_Some$258;
    if (_end$259 < 0) {
      end$256 = len$254 + _end$259;
    } else {
      end$256 = _end$259;
    }
  }
  if (start$261 < 0) {
    start$260 = len$254 + start$261;
  } else {
    start$260 = start$261;
  }
  if (start$260 >= 0) {
    if (start$260 <= end$256) {
      _if_result$2950 = end$256 <= len$254;
    } else {
      _if_result$2950 = 0;
    }
  } else {
    _if_result$2950 = 0;
  }
  if (_if_result$2950) {
    int32_t _if_result$2951;
    int32_t _if_result$2953;
    struct $StringView _tmp$1472;
    struct moonbit_result_1 _result$2955;
    if (start$260 < len$254) {
      int32_t _tmp$1468 = self$255[start$260];
      _if_result$2951 = $Int$$is_trailing_surrogate(_tmp$1468);
    } else {
      _if_result$2951 = 0;
    }
    if (_if_result$2951) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1469;
      struct moonbit_result_1 _result$2952;
      moonbit_decref(self$255);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1469
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2952.tag = 0;
      _result$2952.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1469;
      return _result$2952;
    }
    if (end$256 < len$254) {
      int32_t _tmp$1470 = self$255[end$256];
      _if_result$2953 = $Int$$is_trailing_surrogate(_tmp$1470);
    } else {
      _if_result$2953 = 0;
    }
    if (_if_result$2953) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1471;
      struct moonbit_result_1 _result$2954;
      moonbit_decref(self$255);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1471
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2954.tag = 0;
      _result$2954.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1471;
      return _result$2954;
    }
    _tmp$1472 = (struct $StringView){start$260, end$256, self$255};
    _result$2955.tag = 1;
    _result$2955.data.ok = _tmp$1472;
    return _result$2955;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1473;
    struct moonbit_result_1 _result$2956;
    moonbit_decref(self$255);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1473
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$2956.tag = 0;
    _result$2956.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1473;
    return _result$2956;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$253
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$252 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1467;
  moonbit_incref(_self$252);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$252, self$253);
  _tmp$1467 = _self$252;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1467);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$251
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$250 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1466;
  moonbit_incref(_self$250);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$250, self$251);
  _tmp$1466 = _self$250;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1466);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$248
) {
  int32_t seed$247;
  if (seed$opt$248 == 4294967296ll) {
    seed$247 = 0;
  } else {
    int64_t _Some$249 = seed$opt$248;
    seed$247 = (int32_t)_Some$249;
  }
  return $$moonbitlang$core$builtin$Hasher$$new$inner(seed$247);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$246
) {
  uint32_t _tmp$1465 = *(uint32_t*)&seed$246;
  uint32_t _tmp$1464 = _tmp$1465 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$2957 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$2957)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$2957->$0 = _tmp$1464;
  return _block$2957;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$245
) {
  uint32_t _tmp$1463 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$245);
  return *(int32_t*)&_tmp$1463;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$244
) {
  uint32_t _field$2607 = self$244->$0;
  uint32_t acc$243;
  uint32_t _tmp$1452;
  uint32_t _tmp$1454;
  uint32_t _tmp$1453;
  uint32_t _tmp$1455;
  uint32_t _tmp$1456;
  uint32_t _tmp$1458;
  uint32_t _tmp$1457;
  uint32_t _tmp$1459;
  uint32_t _tmp$1460;
  uint32_t _tmp$1462;
  uint32_t _tmp$1461;
  moonbit_decref(self$244);
  acc$243 = _field$2607;
  _tmp$1452 = acc$243;
  _tmp$1454 = acc$243;
  _tmp$1453 = _tmp$1454 >> 15;
  acc$243 = _tmp$1452 ^ _tmp$1453;
  _tmp$1455 = acc$243;
  acc$243 = _tmp$1455 * 2246822519u;
  _tmp$1456 = acc$243;
  _tmp$1458 = acc$243;
  _tmp$1457 = _tmp$1458 >> 13;
  acc$243 = _tmp$1456 ^ _tmp$1457;
  _tmp$1459 = acc$243;
  acc$243 = _tmp$1459 * 3266489917u;
  _tmp$1460 = acc$243;
  _tmp$1462 = acc$243;
  _tmp$1461 = _tmp$1462 >> 16;
  acc$243 = _tmp$1460 ^ _tmp$1461;
  return acc$243;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$242,
  int32_t value$241
) {
  $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(value$241, self$242);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$240,
  moonbit_string_t value$239
) {
  $$moonbitlang$core$builtin$Hash$$String$$hash_combine(value$239, self$240);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$237,
  int32_t value$238
) {
  uint32_t _tmp$1451 = *(uint32_t*)&value$238;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$237, _tmp$1451);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$235,
  uint32_t value$236
) {
  uint32_t acc$1450 = self$235->$0;
  uint32_t _tmp$1449 = acc$1450 + 4u;
  self$235->$0 = _tmp$1449;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$235, value$236);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$233,
  uint32_t input$234
) {
  uint32_t acc$1447 = self$233->$0;
  uint32_t _tmp$1448 = input$234 * 3266489917u;
  uint32_t _tmp$1446 = acc$1447 + _tmp$1448;
  uint32_t _tmp$1445 = $moonbitlang$core$builtin$rotl(_tmp$1446, 17);
  uint32_t _tmp$1444 = _tmp$1445 * 668265263u;
  self$233->$0 = _tmp$1444;
  moonbit_decref(self$233);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$231, int32_t r$232) {
  uint32_t _tmp$1441 = x$231 << (r$232 & 31);
  int32_t _tmp$1443 = 32 - r$232;
  uint32_t _tmp$1442 = x$231 >> (_tmp$1443 & 31);
  return _tmp$1441 | _tmp$1442;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$229,
  moonbit_string_t str$230
) {
  int32_t len$1431 = self$229->$1;
  int32_t _tmp$1433 = Moonbit_array_length(str$230);
  int32_t _tmp$1432 = _tmp$1433 * 2;
  int32_t _tmp$1430 = len$1431 + _tmp$1432;
  moonbit_bytes_t _field$2609;
  moonbit_bytes_t data$1434;
  int32_t len$1435;
  int32_t _tmp$1436;
  int32_t len$1438;
  int32_t _tmp$2608;
  int32_t _tmp$1440;
  int32_t _tmp$1439;
  int32_t _tmp$1437;
  moonbit_incref(self$229);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$229, _tmp$1430
  );
  _field$2609 = self$229->$0;
  data$1434 = _field$2609;
  len$1435 = self$229->$1;
  _tmp$1436 = Moonbit_array_length(str$230);
  moonbit_incref(data$1434);
  moonbit_incref(str$230);
  $FixedArray$$blit_from_string(data$1434, len$1435, str$230, 0, _tmp$1436);
  len$1438 = self$229->$1;
  _tmp$2608 = Moonbit_array_length(str$230);
  moonbit_decref(str$230);
  _tmp$1440 = _tmp$2608;
  _tmp$1439 = _tmp$1440 * 2;
  _tmp$1437 = len$1438 + _tmp$1439;
  self$229->$1 = _tmp$1437;
  moonbit_decref(self$229);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$221,
  int32_t bytes_offset$216,
  moonbit_string_t str$223,
  int32_t str_offset$219,
  int32_t length$217
) {
  int32_t _tmp$1429 = length$217 * 2;
  int32_t _tmp$1428 = bytes_offset$216 + _tmp$1429;
  int32_t e1$215 = _tmp$1428 - 1;
  int32_t _tmp$1427 = str_offset$219 + length$217;
  int32_t e2$218 = _tmp$1427 - 1;
  int32_t len1$220 = Moonbit_array_length(self$221);
  int32_t len2$222 = Moonbit_array_length(str$223);
  int32_t _if_result$2958;
  if (length$217 >= 0) {
    if (bytes_offset$216 >= 0) {
      if (e1$215 < len1$220) {
        if (str_offset$219 >= 0) {
          _if_result$2958 = e2$218 < len2$222;
        } else {
          _if_result$2958 = 0;
        }
      } else {
        _if_result$2958 = 0;
      }
    } else {
      _if_result$2958 = 0;
    }
  } else {
    _if_result$2958 = 0;
  }
  if (_if_result$2958) {
    int32_t end_str_offset$224 = str_offset$219 + length$217;
    int32_t i$225 = str_offset$219;
    int32_t j$226 = bytes_offset$216;
    while (1) {
      if (i$225 < end_str_offset$224) {
        int32_t _tmp$1424 = str$223[i$225];
        uint32_t c$227 = *(uint32_t*)&_tmp$1424;
        uint32_t _tmp$1420 = c$227 & 255u;
        int32_t _tmp$1419 = $UInt$$to_byte(_tmp$1420);
        int32_t _tmp$1421;
        uint32_t _tmp$1423;
        int32_t _tmp$1422;
        int32_t _tmp$1425;
        int32_t _tmp$1426;
        if (j$226 < 0 || j$226 >= Moonbit_array_length(self$221)) {
          moonbit_panic();
        }
        self$221[j$226] = _tmp$1419;
        _tmp$1421 = j$226 + 1;
        _tmp$1423 = c$227 >> 8;
        _tmp$1422 = $UInt$$to_byte(_tmp$1423);
        if (_tmp$1421 < 0 || _tmp$1421 >= Moonbit_array_length(self$221)) {
          moonbit_panic();
        }
        self$221[_tmp$1421] = _tmp$1422;
        _tmp$1425 = i$225 + 1;
        _tmp$1426 = j$226 + 2;
        i$225 = _tmp$1425;
        j$226 = _tmp$1426;
        continue;
      } else {
        moonbit_decref(str$223);
        moonbit_decref(self$221);
      }
      break;
    }
  } else {
    moonbit_decref(str$223);
    moonbit_decref(self$221);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$183
) {
  int32_t _tmp$1392 = Moonbit_array_length(repr$183);
  int64_t _tmp$1391 = (int64_t)_tmp$1392;
  moonbit_incref(repr$183);
  if ($String$$char_length_ge$inner(repr$183, 1, 0, _tmp$1391)) {
    int32_t _tmp$1418 = repr$183[0];
    int32_t _x$184 = _tmp$1418;
    if (_x$184 == 64) {
      int32_t _tmp$1417 = Moonbit_array_length(repr$183);
      int64_t _tmp$1416 = (int64_t)_tmp$1417;
      int64_t _bind$1134;
      int32_t _tmp$1414;
      int32_t _tmp$1415;
      struct $StringView _x$185;
      int32_t _tmp$1413;
      struct $StringView _tmp$1412;
      int64_t _bind$187;
      moonbit_incref(repr$183);
      _bind$1134
      = $String$$offset_of_nth_char$inner(
        repr$183, 1, 0, _tmp$1416
      );
      if (_bind$1134 == 4294967296ll) {
        _tmp$1414 = Moonbit_array_length(repr$183);
      } else {
        int64_t _Some$186 = _bind$1134;
        _tmp$1414 = (int32_t)_Some$186;
      }
      _tmp$1415 = Moonbit_array_length(repr$183);
      _x$185 = (struct $StringView){_tmp$1414, _tmp$1415, repr$183};
      _tmp$1413
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1412
      = (struct $StringView){
        0, _tmp$1413, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$185.$0);
      _bind$187 = $StringView$$find(_x$185, _tmp$1412);
      if (_bind$187 == 4294967296ll) {
        moonbit_decref(_x$185.$0);
        moonbit_panic();
      } else {
        int64_t _Some$188 = _bind$187;
        int32_t _pkg_end$189 = (int32_t)_Some$188;
        int64_t _tmp$1411 = (int64_t)_pkg_end$189;
        struct $StringView pkg$190;
        int32_t _tmp$1410;
        struct $StringView _tmp$1409;
        int64_t _bind$191;
        moonbit_incref(_x$185.$0);
        pkg$190 = $StringView$$view$inner(_x$185, 0, _tmp$1411);
        _tmp$1410
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1409
        = (struct $StringView){
          0, _tmp$1410, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$185.$0);
        _bind$191 = $StringView$$rev_find(_x$185, _tmp$1409);
        if (_bind$191 == 4294967296ll) {
          moonbit_decref(pkg$190.$0);
          moonbit_decref(_x$185.$0);
          moonbit_panic();
        } else {
          int64_t _Some$192 = _bind$191;
          int32_t _start_loc_end$193 = (int32_t)_Some$192;
          int32_t _tmp$1393 = _start_loc_end$193 + 1;
          int32_t _tmp$1394;
          moonbit_incref(_x$185.$0);
          _tmp$1394 = $StringView$$length(_x$185);
          if (_tmp$1393 < _tmp$1394) {
            int32_t _tmp$1408 = _start_loc_end$193 + 1;
            struct $StringView end_loc$194;
            struct $$3c$StringView$2a$StringView$3e$* _bind$195;
            moonbit_incref(_x$185.$0);
            end_loc$194
            = $StringView$$view$inner(
              _x$185, _tmp$1408, 4294967296ll
            );
            _bind$195
            = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
              end_loc$194
            );
            if (_bind$195 == 0) {
              if (_bind$195) {
                moonbit_decref(_bind$195);
              }
              moonbit_decref(pkg$190.$0);
              moonbit_decref(_x$185.$0);
              moonbit_panic();
            } else {
              struct $$3c$StringView$2a$StringView$3e$* _Some$196 = _bind$195;
              struct $$3c$StringView$2a$StringView$3e$* _x$197 = _Some$196;
              struct $StringView _field$2613 =
                (struct $StringView){
                  _x$197->$0_1, _x$197->$0_2, _x$197->$0_0
                };
              struct $StringView _end_line$198 = _field$2613;
              struct $StringView _field$2612 =
                (struct $StringView){
                  _x$197->$1_1, _x$197->$1_2, _x$197->$1_0
                };
              int32_t _cnt$2810 = Moonbit_object_header(_x$197)->rc;
              struct $StringView _end_column$199;
              int64_t _tmp$1407;
              struct $StringView rest$200;
              int32_t _tmp$1406;
              struct $StringView _tmp$1405;
              int64_t _bind$202;
              if (_cnt$2810 > 1) {
                int32_t _new_cnt$2811;
                moonbit_incref(_field$2612.$0);
                moonbit_incref(_end_line$198.$0);
                _new_cnt$2811 = _cnt$2810 - 1;
                Moonbit_object_header(_x$197)->rc = _new_cnt$2811;
              } else if (_cnt$2810 == 1) {
                moonbit_free(_x$197);
              }
              _end_column$199 = _field$2612;
              _tmp$1407 = (int64_t)_start_loc_end$193;
              rest$200 = $StringView$$view$inner(_x$185, 0, _tmp$1407);
              _tmp$1406
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1405
              = (struct $StringView){
                0,
                  _tmp$1406,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$200.$0);
              _bind$202 = $StringView$$rev_find(rest$200, _tmp$1405);
              if (_bind$202 == 4294967296ll) {
                moonbit_decref(rest$200.$0);
                moonbit_decref(_end_column$199.$0);
                moonbit_decref(_end_line$198.$0);
                moonbit_decref(pkg$190.$0);
                goto $join$201;
              } else {
                int64_t _Some$203 = _bind$202;
                int32_t _start_line_end$204 = (int32_t)_Some$203;
                int64_t _tmp$1404 = (int64_t)_start_line_end$204;
                struct $StringView _tmp$1401;
                int32_t _tmp$1403;
                struct $StringView _tmp$1402;
                int64_t _bind$205;
                moonbit_incref(rest$200.$0);
                _tmp$1401 = $StringView$$view$inner(rest$200, 0, _tmp$1404);
                _tmp$1403
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1402
                = (struct $StringView){
                  0,
                    _tmp$1403,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$205 = $StringView$$rev_find(_tmp$1401, _tmp$1402);
                if (_bind$205 == 4294967296ll) {
                  moonbit_decref(rest$200.$0);
                  moonbit_decref(_end_column$199.$0);
                  moonbit_decref(_end_line$198.$0);
                  moonbit_decref(pkg$190.$0);
                  goto $join$201;
                } else {
                  int64_t _Some$206 = _bind$205;
                  int32_t _filename_end$207 = (int32_t)_Some$206;
                  int32_t _tmp$1395 = _filename_end$207 + 1;
                  int32_t _tmp$1396;
                  moonbit_incref(rest$200.$0);
                  _tmp$1396 = $StringView$$length(rest$200);
                  if (_tmp$1395 < _tmp$1396) {
                    int32_t _tmp$1400 = _filename_end$207 + 1;
                    struct $StringView start_loc$208;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$209;
                    moonbit_incref(rest$200.$0);
                    start_loc$208
                    = $StringView$$view$inner(
                      rest$200, _tmp$1400, 4294967296ll
                    );
                    _bind$209
                    = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
                      start_loc$208
                    );
                    if (_bind$209 == 0) {
                      if (_bind$209) {
                        moonbit_decref(_bind$209);
                      }
                      moonbit_decref(rest$200.$0);
                      moonbit_decref(_end_column$199.$0);
                      moonbit_decref(_end_line$198.$0);
                      moonbit_decref(pkg$190.$0);
                      moonbit_panic();
                    } else {
                      struct $$3c$StringView$2a$StringView$3e$* _Some$210 =
                        _bind$209;
                      struct $$3c$StringView$2a$StringView$3e$* _x$211 =
                        _Some$210;
                      struct $StringView _field$2611 =
                        (struct $StringView){
                          _x$211->$0_1, _x$211->$0_2, _x$211->$0_0
                        };
                      struct $StringView _start_line$212 = _field$2611;
                      struct $StringView _field$2610 =
                        (struct $StringView){
                          _x$211->$1_1, _x$211->$1_2, _x$211->$1_0
                        };
                      int32_t _cnt$2812 = Moonbit_object_header(_x$211)->rc;
                      struct $StringView _start_column$213;
                      int32_t _tmp$1397;
                      if (_cnt$2812 > 1) {
                        int32_t _new_cnt$2813;
                        moonbit_incref(_field$2610.$0);
                        moonbit_incref(_start_line$212.$0);
                        _new_cnt$2813 = _cnt$2812 - 1;
                        Moonbit_object_header(_x$211)->rc = _new_cnt$2813;
                      } else if (_cnt$2812 == 1) {
                        moonbit_free(_x$211);
                      }
                      _start_column$213 = _field$2610;
                      _tmp$1397 = _pkg_end$189 + 1;
                      if (_filename_end$207 > _tmp$1397) {
                        int32_t _tmp$1398 = _pkg_end$189 + 1;
                        int64_t _tmp$1399 = (int64_t)_filename_end$207;
                        struct $StringView filename$214 =
                          $StringView$$view$inner(
                            rest$200, _tmp$1398, _tmp$1399
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$2962 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$2962)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$2962->$0_0 = pkg$190.$0;
                        _block$2962->$0_1 = pkg$190.$1;
                        _block$2962->$0_2 = pkg$190.$2;
                        _block$2962->$1_0 = filename$214.$0;
                        _block$2962->$1_1 = filename$214.$1;
                        _block$2962->$1_2 = filename$214.$2;
                        _block$2962->$2_0 = _start_line$212.$0;
                        _block$2962->$2_1 = _start_line$212.$1;
                        _block$2962->$2_2 = _start_line$212.$2;
                        _block$2962->$3_0 = _start_column$213.$0;
                        _block$2962->$3_1 = _start_column$213.$1;
                        _block$2962->$3_2 = _start_column$213.$2;
                        _block$2962->$4_0 = _end_line$198.$0;
                        _block$2962->$4_1 = _end_line$198.$1;
                        _block$2962->$4_2 = _end_line$198.$2;
                        _block$2962->$5_0 = _end_column$199.$0;
                        _block$2962->$5_1 = _end_column$199.$1;
                        _block$2962->$5_2 = _end_column$199.$2;
                        return _block$2962;
                      } else {
                        moonbit_decref(_start_column$213.$0);
                        moonbit_decref(_start_line$212.$0);
                        moonbit_decref(rest$200.$0);
                        moonbit_decref(_end_column$199.$0);
                        moonbit_decref(_end_line$198.$0);
                        moonbit_decref(pkg$190.$0);
                        moonbit_panic();
                      }
                    }
                  } else {
                    moonbit_decref(rest$200.$0);
                    moonbit_decref(_end_column$199.$0);
                    moonbit_decref(_end_line$198.$0);
                    moonbit_decref(pkg$190.$0);
                    moonbit_panic();
                  }
                }
              }
              $join$201:;
              moonbit_panic();
            }
          } else {
            moonbit_decref(pkg$190.$0);
            moonbit_decref(_x$185.$0);
            moonbit_panic();
          }
        }
      }
    } else {
      moonbit_decref(repr$183);
      goto $join$182;
    }
  } else {
    moonbit_decref(repr$183);
    goto $join$182;
  }
  $join$182:;
  moonbit_panic();
}

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$179
) {
  int32_t _tmp$1390 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1389;
  int64_t _bind$178;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1389
  = (struct $StringView){
    0, _tmp$1390, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$179.$0);
  _bind$178 = $StringView$$find(view$179, _tmp$1389);
  if (_bind$178 == 4294967296ll) {
    moonbit_decref(view$179.$0);
    return 0;
  } else {
    int64_t _Some$180 = _bind$178;
    int32_t _i$181 = (int32_t)_Some$180;
    int32_t _if_result$2963;
    if (_i$181 > 0) {
      int32_t _tmp$1382 = _i$181 + 1;
      int32_t _tmp$1383;
      moonbit_incref(view$179.$0);
      _tmp$1383 = $StringView$$length(view$179);
      _if_result$2963 = _tmp$1382 < _tmp$1383;
    } else {
      _if_result$2963 = 0;
    }
    if (_if_result$2963) {
      int64_t _tmp$1388 = (int64_t)_i$181;
      struct $StringView _tmp$1385;
      int32_t _tmp$1387;
      struct $StringView _tmp$1386;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1384;
      moonbit_incref(view$179.$0);
      _tmp$1385 = $StringView$$view$inner(view$179, 0, _tmp$1388);
      _tmp$1387 = _i$181 + 1;
      _tmp$1386 = $StringView$$view$inner(view$179, _tmp$1387, 4294967296ll);
      _tuple$1384
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1384)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1384->$0_0 = _tmp$1385.$0;
      _tuple$1384->$0_1 = _tmp$1385.$1;
      _tuple$1384->$0_2 = _tmp$1385.$2;
      _tuple$1384->$1_0 = _tmp$1386.$0;
      _tuple$1384->$1_1 = _tmp$1386.$1;
      _tuple$1384->$1_2 = _tmp$1386.$2;
      return _tuple$1384;
    } else {
      moonbit_decref(view$179.$0);
      return 0;
    }
  }
}

struct $StringView $StringView$$view$inner(
  struct $StringView self$176,
  int32_t start_offset$177,
  int64_t end_offset$174
) {
  int32_t end_offset$173;
  int32_t _if_result$2964;
  if (end_offset$174 == 4294967296ll) {
    moonbit_incref(self$176.$0);
    end_offset$173 = $StringView$$length(self$176);
  } else {
    int64_t _Some$175 = end_offset$174;
    end_offset$173 = (int32_t)_Some$175;
  }
  if (start_offset$177 >= 0) {
    if (start_offset$177 <= end_offset$173) {
      int32_t _tmp$1376;
      moonbit_incref(self$176.$0);
      _tmp$1376 = $StringView$$length(self$176);
      _if_result$2964 = end_offset$173 <= _tmp$1376;
    } else {
      _if_result$2964 = 0;
    }
  } else {
    _if_result$2964 = 0;
  }
  if (_if_result$2964) {
    moonbit_string_t _field$2615 = self$176.$0;
    moonbit_string_t str$1377 = _field$2615;
    int32_t start$1381 = self$176.$1;
    int32_t _tmp$1378 = start$1381 + start_offset$177;
    int32_t _field$2614 = self$176.$1;
    int32_t start$1380 = _field$2614;
    int32_t _tmp$1379 = start$1380 + end_offset$173;
    return (struct $StringView){_tmp$1378, _tmp$1379, str$1377};
  } else {
    moonbit_decref(self$176.$0);
    return $moonbitlang$core$builtin$abort$2(
             (moonbit_string_t)moonbit_string_literal_44.data,
               (moonbit_string_t)moonbit_string_literal_45.data
           );
  }
}

int64_t $StringView$$rev_find(
  struct $StringView self$172,
  struct $StringView str$171
) {
  int32_t _tmp$1375;
  moonbit_incref(str$171.$0);
  _tmp$1375 = $StringView$$length(str$171);
  if (_tmp$1375 <= 4) {
    return $moonbitlang$core$builtin$brute_force_rev_find(self$172, str$171);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
             self$172, str$171
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$162,
  struct $StringView needle$164
) {
  int32_t haystack_len$161;
  int32_t needle_len$163;
  moonbit_incref(haystack$162.$0);
  haystack_len$161 = $StringView$$length(haystack$162);
  moonbit_incref(needle$164.$0);
  needle_len$163 = $StringView$$length(needle$164);
  if (needle_len$163 > 0) {
    if (haystack_len$161 >= needle_len$163) {
      int32_t needle_first$165;
      int32_t i$166;
      moonbit_incref(needle$164.$0);
      needle_first$165 = $StringView$$unsafe_charcode_at(needle$164, 0);
      i$166 = haystack_len$161 - needle_len$163;
      while (1) {
        int32_t _tmp$1362 = i$166;
        if (_tmp$1362 >= 0) {
          int32_t _tmp$1367;
          while (1) {
            int32_t _tmp$1365 = i$166;
            int32_t _if_result$2967;
            if (_tmp$1365 >= 0) {
              int32_t _tmp$1364 = i$166;
              int32_t _tmp$1363;
              moonbit_incref(haystack$162.$0);
              _tmp$1363
              = $StringView$$unsafe_charcode_at(
                haystack$162, _tmp$1364
              );
              _if_result$2967 = _tmp$1363 != needle_first$165;
            } else {
              _if_result$2967 = 0;
            }
            if (_if_result$2967) {
              int32_t _tmp$1366 = i$166;
              i$166 = _tmp$1366 - 1;
              continue;
            }
            break;
          }
          _tmp$1367 = i$166;
          if (_tmp$1367 >= 0) {
            int32_t j$168 = 1;
            int32_t _tmp$1374;
            while (1) {
              if (j$168 < needle_len$163) {
                int32_t _tmp$1371 = i$166;
                int32_t _tmp$1370 = _tmp$1371 + j$168;
                int32_t _tmp$1368;
                int32_t _tmp$1369;
                int32_t _tmp$1372;
                moonbit_incref(haystack$162.$0);
                _tmp$1368
                = $StringView$$unsafe_charcode_at(
                  haystack$162, _tmp$1370
                );
                moonbit_incref(needle$164.$0);
                _tmp$1369
                = $StringView$$unsafe_charcode_at(
                  needle$164, j$168
                );
                if (_tmp$1368 != _tmp$1369) {
                  break;
                }
                _tmp$1372 = j$168 + 1;
                j$168 = _tmp$1372;
                continue;
              } else {
                int32_t _tmp$1373;
                moonbit_decref(needle$164.$0);
                moonbit_decref(haystack$162.$0);
                _tmp$1373 = i$166;
                return (int64_t)_tmp$1373;
              }
              break;
            }
            _tmp$1374 = i$166;
            i$166 = _tmp$1374 - 1;
          }
          continue;
        } else {
          moonbit_decref(needle$164.$0);
          moonbit_decref(haystack$162.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$164.$0);
      moonbit_decref(haystack$162.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$164.$0);
    moonbit_decref(haystack$162.$0);
    return (int64_t)haystack_len$161;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$151,
  struct $StringView needle$153
) {
  int32_t haystack_len$150;
  int32_t needle_len$152;
  moonbit_incref(haystack$151.$0);
  haystack_len$150 = $StringView$$length(haystack$151);
  moonbit_incref(needle$153.$0);
  needle_len$152 = $StringView$$length(needle$153);
  if (needle_len$152 > 0) {
    if (haystack_len$150 >= needle_len$152) {
      int32_t* skip_table$154 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$152);
      int32_t _tmp$1352 = needle_len$152 - 1;
      int32_t i$155 = _tmp$1352;
      int32_t _tmp$1361;
      int32_t i$157;
      while (1) {
        if (i$155 > 0) {
          int32_t _tmp$1350;
          int32_t _tmp$1349;
          int32_t _tmp$1351;
          moonbit_incref(needle$153.$0);
          _tmp$1350 = $StringView$$unsafe_charcode_at(needle$153, i$155);
          _tmp$1349 = _tmp$1350 & 255;
          if (
            _tmp$1349 < 0
            || _tmp$1349 >= Moonbit_array_length(skip_table$154)
          ) {
            moonbit_panic();
          }
          skip_table$154[_tmp$1349] = i$155;
          _tmp$1351 = i$155 - 1;
          i$155 = _tmp$1351;
          continue;
        }
        break;
      }
      _tmp$1361 = haystack_len$150 - needle_len$152;
      i$157 = _tmp$1361;
      while (1) {
        if (i$157 >= 0) {
          int32_t j$158 = 0;
          int32_t _tmp$1360;
          int32_t _tmp$1359;
          int32_t _tmp$1358;
          int32_t _tmp$1357;
          while (1) {
            if (j$158 < needle_len$152) {
              int32_t _tmp$1355 = i$157 + j$158;
              int32_t _tmp$1353;
              int32_t _tmp$1354;
              int32_t _tmp$1356;
              moonbit_incref(haystack$151.$0);
              _tmp$1353
              = $StringView$$unsafe_charcode_at(
                haystack$151, _tmp$1355
              );
              moonbit_incref(needle$153.$0);
              _tmp$1354 = $StringView$$unsafe_charcode_at(needle$153, j$158);
              if (_tmp$1353 != _tmp$1354) {
                break;
              }
              _tmp$1356 = j$158 + 1;
              j$158 = _tmp$1356;
              continue;
            } else {
              moonbit_decref(skip_table$154);
              moonbit_decref(needle$153.$0);
              moonbit_decref(haystack$151.$0);
              return (int64_t)i$157;
            }
            break;
          }
          moonbit_incref(haystack$151.$0);
          _tmp$1360 = $StringView$$unsafe_charcode_at(haystack$151, i$157);
          _tmp$1359 = _tmp$1360 & 255;
          if (
            _tmp$1359 < 0
            || _tmp$1359 >= Moonbit_array_length(skip_table$154)
          ) {
            moonbit_panic();
          }
          _tmp$1358 = (int32_t)skip_table$154[_tmp$1359];
          _tmp$1357 = i$157 - _tmp$1358;
          i$157 = _tmp$1357;
          continue;
        } else {
          moonbit_decref(skip_table$154);
          moonbit_decref(needle$153.$0);
          moonbit_decref(haystack$151.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$153.$0);
      moonbit_decref(haystack$151.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$153.$0);
    moonbit_decref(haystack$151.$0);
    return (int64_t)haystack_len$150;
  }
}

int64_t $StringView$$find(
  struct $StringView self$149,
  struct $StringView str$148
) {
  int32_t _tmp$1348;
  moonbit_incref(str$148.$0);
  _tmp$1348 = $StringView$$length(str$148);
  if (_tmp$1348 <= 4) {
    return $moonbitlang$core$builtin$brute_force_find(self$149, str$148);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_find(
             self$149, str$148
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$138,
  struct $StringView needle$140
) {
  int32_t haystack_len$137;
  int32_t needle_len$139;
  moonbit_incref(haystack$138.$0);
  haystack_len$137 = $StringView$$length(haystack$138);
  moonbit_incref(needle$140.$0);
  needle_len$139 = $StringView$$length(needle$140);
  if (needle_len$139 > 0) {
    if (haystack_len$137 >= needle_len$139) {
      int32_t needle_first$141;
      int32_t forward_len$142;
      int32_t i$143;
      moonbit_incref(needle$140.$0);
      needle_first$141 = $StringView$$unsafe_charcode_at(needle$140, 0);
      forward_len$142 = haystack_len$137 - needle_len$139;
      i$143 = 0;
      while (1) {
        int32_t _tmp$1335 = i$143;
        if (_tmp$1335 <= forward_len$142) {
          int32_t _tmp$1340;
          while (1) {
            int32_t _tmp$1338 = i$143;
            int32_t _if_result$2974;
            if (_tmp$1338 <= forward_len$142) {
              int32_t _tmp$1337 = i$143;
              int32_t _tmp$1336;
              moonbit_incref(haystack$138.$0);
              _tmp$1336
              = $StringView$$unsafe_charcode_at(
                haystack$138, _tmp$1337
              );
              _if_result$2974 = _tmp$1336 != needle_first$141;
            } else {
              _if_result$2974 = 0;
            }
            if (_if_result$2974) {
              int32_t _tmp$1339 = i$143;
              i$143 = _tmp$1339 + 1;
              continue;
            }
            break;
          }
          _tmp$1340 = i$143;
          if (_tmp$1340 <= forward_len$142) {
            int32_t j$145 = 1;
            int32_t _tmp$1347;
            while (1) {
              if (j$145 < needle_len$139) {
                int32_t _tmp$1344 = i$143;
                int32_t _tmp$1343 = _tmp$1344 + j$145;
                int32_t _tmp$1341;
                int32_t _tmp$1342;
                int32_t _tmp$1345;
                moonbit_incref(haystack$138.$0);
                _tmp$1341
                = $StringView$$unsafe_charcode_at(
                  haystack$138, _tmp$1343
                );
                moonbit_incref(needle$140.$0);
                _tmp$1342
                = $StringView$$unsafe_charcode_at(
                  needle$140, j$145
                );
                if (_tmp$1341 != _tmp$1342) {
                  break;
                }
                _tmp$1345 = j$145 + 1;
                j$145 = _tmp$1345;
                continue;
              } else {
                int32_t _tmp$1346;
                moonbit_decref(needle$140.$0);
                moonbit_decref(haystack$138.$0);
                _tmp$1346 = i$143;
                return (int64_t)_tmp$1346;
              }
              break;
            }
            _tmp$1347 = i$143;
            i$143 = _tmp$1347 + 1;
          }
          continue;
        } else {
          moonbit_decref(needle$140.$0);
          moonbit_decref(haystack$138.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$140.$0);
      moonbit_decref(haystack$138.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$140.$0);
    moonbit_decref(haystack$138.$0);
    return $moonbitlang$core$builtin$brute_force_find$constr$136;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$124,
  struct $StringView needle$126
) {
  int32_t haystack_len$123;
  int32_t needle_len$125;
  moonbit_incref(haystack$124.$0);
  haystack_len$123 = $StringView$$length(haystack$124);
  moonbit_incref(needle$126.$0);
  needle_len$125 = $StringView$$length(needle$126);
  if (needle_len$125 > 0) {
    if (haystack_len$123 >= needle_len$125) {
      int32_t* skip_table$127 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$125);
      int32_t _end4301$128 = needle_len$125 - 1;
      int32_t i$129 = 0;
      int32_t i$131;
      while (1) {
        if (i$129 < _end4301$128) {
          int32_t _tmp$1322;
          int32_t _tmp$1319;
          int32_t _tmp$1321;
          int32_t _tmp$1320;
          int32_t _tmp$1323;
          moonbit_incref(needle$126.$0);
          _tmp$1322 = $StringView$$unsafe_charcode_at(needle$126, i$129);
          _tmp$1319 = _tmp$1322 & 255;
          _tmp$1321 = needle_len$125 - 1;
          _tmp$1320 = _tmp$1321 - i$129;
          if (
            _tmp$1319 < 0
            || _tmp$1319 >= Moonbit_array_length(skip_table$127)
          ) {
            moonbit_panic();
          }
          skip_table$127[_tmp$1319] = _tmp$1320;
          _tmp$1323 = i$129 + 1;
          i$129 = _tmp$1323;
          continue;
        }
        break;
      }
      i$131 = 0;
      while (1) {
        int32_t _tmp$1324 = haystack_len$123 - needle_len$125;
        if (i$131 <= _tmp$1324) {
          int32_t _end4307$132 = needle_len$125 - 1;
          int32_t j$133 = 0;
          int32_t _tmp$1334;
          int32_t _tmp$1333;
          int32_t _tmp$1332;
          int32_t _tmp$1331;
          int32_t _tmp$1330;
          int32_t _tmp$1329;
          while (1) {
            if (j$133 <= _end4307$132) {
              int32_t _tmp$1327 = i$131 + j$133;
              int32_t _tmp$1325;
              int32_t _tmp$1326;
              int32_t _tmp$1328;
              moonbit_incref(haystack$124.$0);
              _tmp$1325
              = $StringView$$unsafe_charcode_at(
                haystack$124, _tmp$1327
              );
              moonbit_incref(needle$126.$0);
              _tmp$1326 = $StringView$$unsafe_charcode_at(needle$126, j$133);
              if (_tmp$1325 != _tmp$1326) {
                break;
              }
              _tmp$1328 = j$133 + 1;
              j$133 = _tmp$1328;
              continue;
            } else {
              moonbit_decref(skip_table$127);
              moonbit_decref(needle$126.$0);
              moonbit_decref(haystack$124.$0);
              return (int64_t)i$131;
            }
            break;
          }
          _tmp$1334 = i$131 + needle_len$125;
          _tmp$1333 = _tmp$1334 - 1;
          moonbit_incref(haystack$124.$0);
          _tmp$1332
          = $StringView$$unsafe_charcode_at(
            haystack$124, _tmp$1333
          );
          _tmp$1331 = _tmp$1332 & 255;
          if (
            _tmp$1331 < 0
            || _tmp$1331 >= Moonbit_array_length(skip_table$127)
          ) {
            moonbit_panic();
          }
          _tmp$1330 = (int32_t)skip_table$127[_tmp$1331];
          _tmp$1329 = i$131 + _tmp$1330;
          i$131 = _tmp$1329;
          continue;
        } else {
          moonbit_decref(skip_table$127);
          moonbit_decref(needle$126.$0);
          moonbit_decref(haystack$124.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$126.$0);
      moonbit_decref(haystack$124.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$126.$0);
    moonbit_decref(haystack$124.$0);
    return $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$122;
  }
}

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$120,
  int32_t index$121
) {
  moonbit_string_t _field$2618 = self$120.$0;
  moonbit_string_t str$1316 = _field$2618;
  int32_t _field$2617 = self$120.$1;
  int32_t start$1318 = _field$2617;
  int32_t _tmp$1317 = start$1318 + index$121;
  int32_t _tmp$2616 = str$1316[_tmp$1317];
  moonbit_decref(str$1316);
  return _tmp$2616;
}

int32_t $StringView$$length(struct $StringView self$119) {
  int32_t end$1314 = self$119.$2;
  int32_t _field$2619 = self$119.$1;
  int32_t start$1315;
  moonbit_decref(self$119.$0);
  start$1315 = _field$2619;
  return end$1314 - start$1315;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$117,
  int32_t index$118
) {
  int32_t len$116 = self$117->$1;
  int32_t _if_result$2979;
  if (index$118 >= 0) {
    _if_result$2979 = index$118 < len$116;
  } else {
    _if_result$2979 = 0;
  }
  if (_if_result$2979) {
    moonbit_string_t* _tmp$1313 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$117);
    moonbit_string_t _tmp$2620;
    if (index$118 < 0 || index$118 >= Moonbit_array_length(_tmp$1313)) {
      moonbit_panic();
    }
    _tmp$2620 = (moonbit_string_t)_tmp$1313[index$118];
    moonbit_incref(_tmp$2620);
    moonbit_decref(_tmp$1313);
    return _tmp$2620;
  } else {
    moonbit_decref(self$117);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$115
) {
  int32_t _field$2621 = self$115->$1;
  moonbit_decref(self$115);
  return _field$2621;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$114
) {
  int32_t _field$2622 = self$114->$1;
  moonbit_decref(self$114);
  return _field$2622;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$113
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2623 =
    self$113->$0;
  int32_t _cnt$2814 = Moonbit_object_header(self$113)->rc;
  if (_cnt$2814 > 1) {
    int32_t _new_cnt$2815;
    moonbit_incref(_field$2623);
    _new_cnt$2815 = _cnt$2814 - 1;
    Moonbit_object_header(self$113)->rc = _new_cnt$2815;
  } else if (_cnt$2814 == 1) {
    moonbit_free(self$113);
  }
  return _field$2623;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$112
) {
  moonbit_string_t* _field$2624 = self$112->$0;
  int32_t _cnt$2816 = Moonbit_object_header(self$112)->rc;
  if (_cnt$2816 > 1) {
    int32_t _new_cnt$2817;
    moonbit_incref(_field$2624);
    _new_cnt$2817 = _cnt$2816 - 1;
    Moonbit_object_header(self$112)->rc = _new_cnt$2817;
  } else if (_cnt$2816 == 1) {
    moonbit_free(self$112);
  }
  return _field$2624;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$111
) {
  struct $$3c$String$2a$Int$3e$** _field$2625 = self$111->$0;
  int32_t _cnt$2818 = Moonbit_object_header(self$111)->rc;
  if (_cnt$2818 > 1) {
    int32_t _new_cnt$2819;
    moonbit_incref(_field$2625);
    _new_cnt$2819 = _cnt$2818 - 1;
    Moonbit_object_header(self$111)->rc = _new_cnt$2819;
  } else if (_cnt$2818 == 1) {
    moonbit_free(self$111);
  }
  return _field$2625;
}

moonbit_string_t $String$$escape(moonbit_string_t self$110) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$109 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1312;
  moonbit_incref(buf$109);
  _tmp$1312
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$109
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$110, _tmp$1312);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$109);
}

int32_t $moonbitlang$core$builtin$op_notequal$1(
  moonbit_string_t x$107,
  moonbit_string_t y$108
) {
  int32_t _tmp$2626 = moonbit_val_array_equal(x$107, y$108);
  int32_t _tmp$1311;
  moonbit_decref(x$107);
  moonbit_decref(y$108);
  _tmp$1311 = _tmp$2626;
  return !_tmp$1311;
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$105, int32_t y$106) {
  int32_t _tmp$1310 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$105, y$106
    );
  return !_tmp$1310;
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
  int32_t len$1305 = self$100->$1;
  int32_t _tmp$1304 = len$1305 + 4;
  moonbit_bytes_t _field$2627;
  moonbit_bytes_t data$1308;
  int32_t len$1309;
  int32_t inc$101;
  int32_t len$1307;
  int32_t _tmp$1306;
  moonbit_incref(self$100);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$100, _tmp$1304
  );
  _field$2627 = self$100->$0;
  data$1308 = _field$2627;
  len$1309 = self$100->$1;
  moonbit_incref(data$1308);
  inc$101 = $FixedArray$$set_utf16le_char(data$1308, len$1309, ch$102);
  len$1307 = self$100->$1;
  _tmp$1306 = len$1307 + inc$101;
  self$100->$1 = _tmp$1306;
  moonbit_decref(self$100);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$95,
  int32_t required$96
) {
  moonbit_bytes_t _field$2631 = self$95->$0;
  moonbit_bytes_t data$1303 = _field$2631;
  int32_t _tmp$2630 = Moonbit_array_length(data$1303);
  int32_t current_len$94 = _tmp$2630;
  int32_t enough_space$97;
  int32_t _tmp$1301;
  int32_t _tmp$1302;
  moonbit_bytes_t new_data$99;
  moonbit_bytes_t _field$2629;
  moonbit_bytes_t data$1299;
  int32_t len$1300;
  moonbit_bytes_t _old$2628;
  if (required$96 <= current_len$94) {
    moonbit_decref(self$95);
    return 0;
  }
  enough_space$97 = current_len$94;
  while (1) {
    int32_t _tmp$1297 = enough_space$97;
    if (_tmp$1297 < required$96) {
      int32_t _tmp$1298 = enough_space$97;
      enough_space$97 = _tmp$1298 * 2;
      continue;
    }
    break;
  }
  _tmp$1301 = enough_space$97;
  _tmp$1302 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$99 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1301, _tmp$1302);
  _field$2629 = self$95->$0;
  data$1299 = _field$2629;
  len$1300 = self$95->$1;
  moonbit_incref(data$1299);
  moonbit_incref(new_data$99);
  $FixedArray$$unsafe_blit$0(new_data$99, 0, data$1299, 0, len$1300);
  _old$2628 = self$95->$0;
  moonbit_decref(_old$2628);
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
    uint32_t _tmp$1280 = code$87 & 255u;
    int32_t _tmp$1279 = $UInt$$to_byte(_tmp$1280);
    int32_t _tmp$1281;
    uint32_t _tmp$1283;
    int32_t _tmp$1282;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1279;
    _tmp$1281 = offset$90 + 1;
    _tmp$1283 = code$87 >> 8;
    _tmp$1282 = $UInt$$to_byte(_tmp$1283);
    if (_tmp$1281 < 0 || _tmp$1281 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1281] = _tmp$1282;
    moonbit_decref(self$89);
    return 2;
  } else if (code$87 < 1114112u) {
    uint32_t hi$91 = code$87 - 65536u;
    uint32_t _tmp$1296 = hi$91 >> 10;
    uint32_t lo$92 = _tmp$1296 | 55296u;
    uint32_t _tmp$1295 = hi$91 & 1023u;
    uint32_t hi$93 = _tmp$1295 | 56320u;
    uint32_t _tmp$1285 = lo$92 & 255u;
    int32_t _tmp$1284 = $UInt$$to_byte(_tmp$1285);
    int32_t _tmp$1286;
    uint32_t _tmp$1288;
    int32_t _tmp$1287;
    int32_t _tmp$1289;
    uint32_t _tmp$1291;
    int32_t _tmp$1290;
    int32_t _tmp$1292;
    uint32_t _tmp$1294;
    int32_t _tmp$1293;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1284;
    _tmp$1286 = offset$90 + 1;
    _tmp$1288 = lo$92 >> 8;
    _tmp$1287 = $UInt$$to_byte(_tmp$1288);
    if (_tmp$1286 < 0 || _tmp$1286 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1286] = _tmp$1287;
    _tmp$1289 = offset$90 + 2;
    _tmp$1291 = hi$93 & 255u;
    _tmp$1290 = $UInt$$to_byte(_tmp$1291);
    if (_tmp$1289 < 0 || _tmp$1289 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1289] = _tmp$1290;
    _tmp$1292 = offset$90 + 3;
    _tmp$1294 = hi$93 >> 8;
    _tmp$1293 = $UInt$$to_byte(_tmp$1294);
    if (_tmp$1292 < 0 || _tmp$1292 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1292] = _tmp$1293;
    moonbit_decref(self$89);
    return 4;
  } else {
    moonbit_decref(self$89);
    return $moonbitlang$core$builtin$abort$1(
             (moonbit_string_t)moonbit_string_literal_46.data,
               (moonbit_string_t)moonbit_string_literal_47.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$86) {
  int32_t _tmp$1278 = *(int32_t*)&self$86;
  return _tmp$1278 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$85) {
  int32_t _tmp$1277 = self$85;
  return *(uint32_t*)&_tmp$1277;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$84
) {
  moonbit_bytes_t _field$2633 = self$84->$0;
  moonbit_bytes_t data$1276 = _field$2633;
  moonbit_bytes_t _tmp$1273;
  int32_t _field$2632;
  int32_t len$1275;
  int64_t _tmp$1274;
  moonbit_incref(data$1276);
  _tmp$1273 = data$1276;
  _field$2632 = self$84->$1;
  moonbit_decref(self$84);
  len$1275 = _field$2632;
  _tmp$1274 = (int64_t)len$1275;
  return $Bytes$$to_unchecked_string$inner(_tmp$1273, 0, _tmp$1274);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$79,
  int32_t offset$83,
  int64_t length$81
) {
  int32_t len$78 = Moonbit_array_length(self$79);
  int32_t length$80;
  int32_t _if_result$2981;
  if (length$81 == 4294967296ll) {
    length$80 = len$78 - offset$83;
  } else {
    int64_t _Some$82 = length$81;
    length$80 = (int32_t)_Some$82;
  }
  if (offset$83 >= 0) {
    if (length$80 >= 0) {
      int32_t _tmp$1272 = offset$83 + length$80;
      _if_result$2981 = _tmp$1272 <= len$78;
    } else {
      _if_result$2981 = 0;
    }
  } else {
    _if_result$2981 = 0;
  }
  if (_if_result$2981) {
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
  struct $$moonbitlang$core$builtin$StringBuilder* _block$2982;
  if (size_hint$76 < 1) {
    initial$75 = 1;
  } else {
    initial$75 = size_hint$76;
  }
  data$77 = (moonbit_bytes_t)moonbit_make_bytes(initial$75, 0);
  _block$2982
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$2982)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$2982->$0 = data$77;
  _block$2982->$1 = 0;
  return _block$2982;
}

int32_t $Byte$$to_char(int32_t self$74) {
  int32_t _tmp$1271 = (int32_t)self$74;
  return _tmp$1271;
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
  int32_t _if_result$2983;
  if (dst$50 == src$51) {
    _if_result$2983 = dst_offset$52 < src_offset$53;
  } else {
    _if_result$2983 = 0;
  }
  if (_if_result$2983) {
    int32_t i$54 = 0;
    while (1) {
      if (i$54 < len$55) {
        int32_t _tmp$1262 = dst_offset$52 + i$54;
        int32_t _tmp$1264 = src_offset$53 + i$54;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2635;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1263;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2634;
        int32_t _tmp$1265;
        if (_tmp$1264 < 0 || _tmp$1264 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2635
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1264
          ];
        _tmp$1263 = _tmp$2635;
        if (_tmp$1262 < 0 || _tmp$1262 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2634
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1262
          ];
        if (_tmp$1263) {
          moonbit_incref(_tmp$1263);
        }
        if (_old$2634) {
          moonbit_decref(_old$2634);
        }
        dst$50[_tmp$1262] = _tmp$1263;
        _tmp$1265 = i$54 + 1;
        i$54 = _tmp$1265;
        continue;
      } else {
        moonbit_decref(src$51);
        moonbit_decref(dst$50);
      }
      break;
    }
  } else {
    int32_t _tmp$1270 = len$55 - 1;
    int32_t i$57 = _tmp$1270;
    while (1) {
      if (i$57 >= 0) {
        int32_t _tmp$1266 = dst_offset$52 + i$57;
        int32_t _tmp$1268 = src_offset$53 + i$57;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2637;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1267;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2636;
        int32_t _tmp$1269;
        if (_tmp$1268 < 0 || _tmp$1268 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2637
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1268
          ];
        _tmp$1267 = _tmp$2637;
        if (_tmp$1266 < 0 || _tmp$1266 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2636
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1266
          ];
        if (_tmp$1267) {
          moonbit_incref(_tmp$1267);
        }
        if (_old$2636) {
          moonbit_decref(_old$2636);
        }
        dst$50[_tmp$1266] = _tmp$1267;
        _tmp$1269 = i$57 - 1;
        i$57 = _tmp$1269;
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
  int32_t _if_result$2986;
  if (dst$41 == src$42) {
    _if_result$2986 = dst_offset$43 < src_offset$44;
  } else {
    _if_result$2986 = 0;
  }
  if (_if_result$2986) {
    int32_t i$45 = 0;
    while (1) {
      if (i$45 < len$46) {
        int32_t _tmp$1253 = dst_offset$43 + i$45;
        int32_t _tmp$1255 = src_offset$44 + i$45;
        struct $$3c$String$2a$Int$3e$* _tmp$2639;
        struct $$3c$String$2a$Int$3e$* _tmp$1254;
        struct $$3c$String$2a$Int$3e$* _old$2638;
        int32_t _tmp$1256;
        if (_tmp$1255 < 0 || _tmp$1255 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2639 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1255];
        _tmp$1254 = _tmp$2639;
        if (_tmp$1253 < 0 || _tmp$1253 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2638 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1253];
        if (_tmp$1254) {
          moonbit_incref(_tmp$1254);
        }
        if (_old$2638) {
          moonbit_decref(_old$2638);
        }
        dst$41[_tmp$1253] = _tmp$1254;
        _tmp$1256 = i$45 + 1;
        i$45 = _tmp$1256;
        continue;
      } else {
        moonbit_decref(src$42);
        moonbit_decref(dst$41);
      }
      break;
    }
  } else {
    int32_t _tmp$1261 = len$46 - 1;
    int32_t i$48 = _tmp$1261;
    while (1) {
      if (i$48 >= 0) {
        int32_t _tmp$1257 = dst_offset$43 + i$48;
        int32_t _tmp$1259 = src_offset$44 + i$48;
        struct $$3c$String$2a$Int$3e$* _tmp$2641;
        struct $$3c$String$2a$Int$3e$* _tmp$1258;
        struct $$3c$String$2a$Int$3e$* _old$2640;
        int32_t _tmp$1260;
        if (_tmp$1259 < 0 || _tmp$1259 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2641 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1259];
        _tmp$1258 = _tmp$2641;
        if (_tmp$1257 < 0 || _tmp$1257 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2640 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1257];
        if (_tmp$1258) {
          moonbit_incref(_tmp$1258);
        }
        if (_old$2640) {
          moonbit_decref(_old$2640);
        }
        dst$41[_tmp$1257] = _tmp$1258;
        _tmp$1260 = i$48 - 1;
        i$48 = _tmp$1260;
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
  int32_t _if_result$2989;
  if (dst$32 == src$33) {
    _if_result$2989 = dst_offset$34 < src_offset$35;
  } else {
    _if_result$2989 = 0;
  }
  if (_if_result$2989) {
    int32_t i$36 = 0;
    while (1) {
      if (i$36 < len$37) {
        int32_t _tmp$1244 = dst_offset$34 + i$36;
        int32_t _tmp$1246 = src_offset$35 + i$36;
        moonbit_string_t _tmp$2643;
        moonbit_string_t _tmp$1245;
        moonbit_string_t _old$2642;
        int32_t _tmp$1247;
        if (_tmp$1246 < 0 || _tmp$1246 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2643 = (moonbit_string_t)src$33[_tmp$1246];
        _tmp$1245 = _tmp$2643;
        if (_tmp$1244 < 0 || _tmp$1244 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2642 = (moonbit_string_t)dst$32[_tmp$1244];
        moonbit_incref(_tmp$1245);
        moonbit_decref(_old$2642);
        dst$32[_tmp$1244] = _tmp$1245;
        _tmp$1247 = i$36 + 1;
        i$36 = _tmp$1247;
        continue;
      } else {
        moonbit_decref(src$33);
        moonbit_decref(dst$32);
      }
      break;
    }
  } else {
    int32_t _tmp$1252 = len$37 - 1;
    int32_t i$39 = _tmp$1252;
    while (1) {
      if (i$39 >= 0) {
        int32_t _tmp$1248 = dst_offset$34 + i$39;
        int32_t _tmp$1250 = src_offset$35 + i$39;
        moonbit_string_t _tmp$2645;
        moonbit_string_t _tmp$1249;
        moonbit_string_t _old$2644;
        int32_t _tmp$1251;
        if (_tmp$1250 < 0 || _tmp$1250 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2645 = (moonbit_string_t)src$33[_tmp$1250];
        _tmp$1249 = _tmp$2645;
        if (_tmp$1248 < 0 || _tmp$1248 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2644 = (moonbit_string_t)dst$32[_tmp$1248];
        moonbit_incref(_tmp$1249);
        moonbit_decref(_old$2644);
        dst$32[_tmp$1248] = _tmp$1249;
        _tmp$1251 = i$39 - 1;
        i$39 = _tmp$1251;
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
  int32_t _if_result$2992;
  if (dst$23 == src$24) {
    _if_result$2992 = dst_offset$25 < src_offset$26;
  } else {
    _if_result$2992 = 0;
  }
  if (_if_result$2992) {
    int32_t i$27 = 0;
    while (1) {
      if (i$27 < len$28) {
        int32_t _tmp$1235 = dst_offset$25 + i$27;
        int32_t _tmp$1237 = src_offset$26 + i$27;
        int32_t _tmp$1236;
        int32_t _tmp$1238;
        if (_tmp$1237 < 0 || _tmp$1237 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1236 = (int32_t)src$24[_tmp$1237];
        if (_tmp$1235 < 0 || _tmp$1235 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1235] = _tmp$1236;
        _tmp$1238 = i$27 + 1;
        i$27 = _tmp$1238;
        continue;
      } else {
        moonbit_decref(src$24);
        moonbit_decref(dst$23);
      }
      break;
    }
  } else {
    int32_t _tmp$1243 = len$28 - 1;
    int32_t i$30 = _tmp$1243;
    while (1) {
      if (i$30 >= 0) {
        int32_t _tmp$1239 = dst_offset$25 + i$30;
        int32_t _tmp$1241 = src_offset$26 + i$30;
        int32_t _tmp$1240;
        int32_t _tmp$1242;
        if (_tmp$1241 < 0 || _tmp$1241 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1240 = (int32_t)src$24[_tmp$1241];
        if (_tmp$1239 < 0 || _tmp$1239 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1239] = _tmp$1240;
        _tmp$1242 = i$30 - 1;
        i$30 = _tmp$1242;
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
  moonbit_string_t _tmp$1234 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$1232 =
    moonbit_add_string(
      _tmp$1234, (moonbit_string_t)moonbit_string_literal_48.data
    );
  moonbit_string_t _tmp$1233 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$1231 = moonbit_add_string(_tmp$1232, _tmp$1233);
  moonbit_string_t _tmp$1230 =
    moonbit_add_string(
      _tmp$1231, (moonbit_string_t)moonbit_string_literal_49.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1230);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$1229 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$1227 =
    moonbit_add_string(
      _tmp$1229, (moonbit_string_t)moonbit_string_literal_48.data
    );
  moonbit_string_t _tmp$1228 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$1226 = moonbit_add_string(_tmp$1227, _tmp$1228);
  moonbit_string_t _tmp$1225 =
    moonbit_add_string(
      _tmp$1226, (moonbit_string_t)moonbit_string_literal_49.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1225);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
) {
  moonbit_string_t _tmp$1224 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$17);
  moonbit_string_t _tmp$1222 =
    moonbit_add_string(
      _tmp$1224, (moonbit_string_t)moonbit_string_literal_48.data
    );
  moonbit_string_t _tmp$1223 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$18);
  moonbit_string_t _tmp$1221 = moonbit_add_string(_tmp$1222, _tmp$1223);
  moonbit_string_t _tmp$1220 =
    moonbit_add_string(
      _tmp$1221, (moonbit_string_t)moonbit_string_literal_49.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1220);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
) {
  moonbit_string_t _tmp$1219 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$15);
  moonbit_string_t _tmp$1217 =
    moonbit_add_string(
      _tmp$1219, (moonbit_string_t)moonbit_string_literal_48.data
    );
  moonbit_string_t _tmp$1218 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$16);
  moonbit_string_t _tmp$1216 = moonbit_add_string(_tmp$1217, _tmp$1218);
  moonbit_string_t _tmp$1215 =
    moonbit_add_string(
      _tmp$1216, (moonbit_string_t)moonbit_string_literal_49.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1215);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$11,
  struct $$moonbitlang$core$builtin$Logger _x_5272$14
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$12 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$11;
  moonbit_string_t _field$2646 = _Failure$12->$0;
  int32_t _cnt$2820 = Moonbit_object_header(_Failure$12)->rc;
  moonbit_string_t _$2a$err_payload_5273$13;
  struct $$moonbitlang$core$builtin$Logger _bind$1214;
  if (_cnt$2820 > 1) {
    int32_t _new_cnt$2821;
    moonbit_incref(_field$2646);
    _new_cnt$2821 = _cnt$2820 - 1;
    Moonbit_object_header(_Failure$12)->rc = _new_cnt$2821;
  } else if (_cnt$2820 == 1) {
    moonbit_free(_Failure$12);
  }
  _$2a$err_payload_5273$13 = _field$2646;
  if (_x_5272$14.$1) {
    moonbit_incref(_x_5272$14.$1);
  }
  _x_5272$14.$0->$method_0(
    _x_5272$14.$1, (moonbit_string_t)moonbit_string_literal_50.data
  );
  if (_x_5272$14.$1) {
    moonbit_incref(_x_5272$14.$1);
  }
  $$moonbitlang$core$builtin$Logger$$write_object$0(
    _x_5272$14, _$2a$err_payload_5273$13
  );
  _bind$1214 = _x_5272$14;
  _bind$1214.$0->$method_0(
    _bind$1214.$1, (moonbit_string_t)moonbit_string_literal_51.data
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
        _x_5286$10.$1, (moonbit_string_t)moonbit_string_literal_52.data
      );
      break;
    }
    default: {
      moonbit_decref(_x_5285$9);
      _x_5286$10.$0->$method_0(
        _x_5286$10.$1, (moonbit_string_t)moonbit_string_literal_53.data
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

moonbit_string_t $Error$to_string(void* _e$1133) {
  switch (Moonbit_object_tag(_e$1133)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1133
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1133
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1133);
      return (moonbit_string_t)moonbit_string_literal_54.data;
      break;
    }
    
    case 5: {
      moonbit_decref(_e$1133);
      return (moonbit_string_t)moonbit_string_literal_55.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
               _e$1133
             );
      break;
    }
    default: {
      moonbit_decref(_e$1133);
      return (moonbit_string_t)moonbit_string_literal_56.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1151,
  int32_t _param$1150
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1149 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1151;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1149, _param$1150
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1148,
  struct $StringView _param$1147
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1146 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1148;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1146, _param$1147
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1145,
  moonbit_string_t _param$1142,
  int32_t _param$1143,
  int32_t _param$1144
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1141 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1145;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1141, _param$1142, _param$1143, _param$1144
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1140,
  moonbit_string_t _param$1139
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1138 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1140;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1138, _param$1139
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$959;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1157;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1156;
  moonbit_string_t* _tmp$1211;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1210;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1209;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1168;
  moonbit_string_t* _tmp$1208;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1207;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1206;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1169;
  moonbit_string_t* _tmp$1205;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1204;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1203;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1170;
  moonbit_string_t* _tmp$1202;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1201;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1200;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1171;
  moonbit_string_t* _tmp$1199;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1198;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1197;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1172;
  moonbit_string_t* _tmp$1196;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1195;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1194;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1173;
  moonbit_string_t* _tmp$1193;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1192;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1191;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1174;
  moonbit_string_t* _tmp$1190;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1189;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1188;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1175;
  moonbit_string_t* _tmp$1187;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1186;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1185;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1176;
  moonbit_string_t* _tmp$1184;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1183;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1182;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1177;
  moonbit_string_t* _tmp$1181;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1180;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1179;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1178;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$956;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1167;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1166;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1165;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1160;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$957;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1164;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1163;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1162;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1161;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$955;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1159;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1158;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$958;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1213;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1212;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3$dyncall$closure.data;
  $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7$dyncall$closure.data;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$122 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$136 = (int64_t)0;
  _bind$959
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1157 = _bind$959;
  _tmp$1156
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1157
  };
  $azimuth$telemetry$working_tests$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1156
  );
  _tmp$1211 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1211[0] = (moonbit_string_t)moonbit_string_literal_57.data;
  _tmp$1210
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1210)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1210->$0 = _tmp$1211;
  _tmp$1210->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0$clo
  );
  _tuple$1209
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1209)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1209->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_0$clo;
  _tuple$1209->$1 = _tmp$1210;
  _tuple$1168
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1168)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1168->$0 = 0;
  _tuple$1168->$1 = _tuple$1209;
  _tmp$1208 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1208[0] = (moonbit_string_t)moonbit_string_literal_58.data;
  _tmp$1207
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1207)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1207->$0 = _tmp$1208;
  _tmp$1207->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1$clo
  );
  _tuple$1206
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1206)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1206->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_1$clo;
  _tuple$1206->$1 = _tmp$1207;
  _tuple$1169
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1169)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1169->$0 = 1;
  _tuple$1169->$1 = _tuple$1206;
  _tmp$1205 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1205[0] = (moonbit_string_t)moonbit_string_literal_59.data;
  _tmp$1204
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1204)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1204->$0 = _tmp$1205;
  _tmp$1204->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2$clo
  );
  _tuple$1203
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1203)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1203->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_2$clo;
  _tuple$1203->$1 = _tmp$1204;
  _tuple$1170
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1170)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1170->$0 = 2;
  _tuple$1170->$1 = _tuple$1203;
  _tmp$1202 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1202[0] = (moonbit_string_t)moonbit_string_literal_60.data;
  _tmp$1201
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1201)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1201->$0 = _tmp$1202;
  _tmp$1201->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3$clo
  );
  _tuple$1200
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1200)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1200->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_3$clo;
  _tuple$1200->$1 = _tmp$1201;
  _tuple$1171
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1171)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1171->$0 = 3;
  _tuple$1171->$1 = _tuple$1200;
  _tmp$1199 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1199[0] = (moonbit_string_t)moonbit_string_literal_61.data;
  _tmp$1198
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1198)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1198->$0 = _tmp$1199;
  _tmp$1198->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4$clo
  );
  _tuple$1197
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1197)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1197->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_4$clo;
  _tuple$1197->$1 = _tmp$1198;
  _tuple$1172
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1172)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1172->$0 = 4;
  _tuple$1172->$1 = _tuple$1197;
  _tmp$1196 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1196[0] = (moonbit_string_t)moonbit_string_literal_62.data;
  _tmp$1195
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1195)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1195->$0 = _tmp$1196;
  _tmp$1195->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5$clo
  );
  _tuple$1194
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1194)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1194->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_5$clo;
  _tuple$1194->$1 = _tmp$1195;
  _tuple$1173
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1173)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1173->$0 = 5;
  _tuple$1173->$1 = _tuple$1194;
  _tmp$1193 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1193[0] = (moonbit_string_t)moonbit_string_literal_63.data;
  _tmp$1192
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1192)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1192->$0 = _tmp$1193;
  _tmp$1192->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6$clo
  );
  _tuple$1191
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1191)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1191->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_6$clo;
  _tuple$1191->$1 = _tmp$1192;
  _tuple$1174
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1174)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1174->$0 = 6;
  _tuple$1174->$1 = _tuple$1191;
  _tmp$1190 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1190[0] = (moonbit_string_t)moonbit_string_literal_64.data;
  _tmp$1189
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1189)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1189->$0 = _tmp$1190;
  _tmp$1189->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7$clo
  );
  _tuple$1188
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1188)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1188->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_7$clo;
  _tuple$1188->$1 = _tmp$1189;
  _tuple$1175
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1175)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1175->$0 = 7;
  _tuple$1175->$1 = _tuple$1188;
  _tmp$1187 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1187[0] = (moonbit_string_t)moonbit_string_literal_65.data;
  _tmp$1186
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1186)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1186->$0 = _tmp$1187;
  _tmp$1186->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8$clo
  );
  _tuple$1185
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1185)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1185->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_8$clo;
  _tuple$1185->$1 = _tmp$1186;
  _tuple$1176
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1176)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1176->$0 = 8;
  _tuple$1176->$1 = _tuple$1185;
  _tmp$1184 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1184[0] = (moonbit_string_t)moonbit_string_literal_66.data;
  _tmp$1183
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1183)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1183->$0 = _tmp$1184;
  _tmp$1183->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9$clo
  );
  _tuple$1182
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1182)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1182->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_9$clo;
  _tuple$1182->$1 = _tmp$1183;
  _tuple$1177
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1177)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1177->$0 = 9;
  _tuple$1177->$1 = _tuple$1182;
  _tmp$1181 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1181[0] = (moonbit_string_t)moonbit_string_literal_67.data;
  _tmp$1180
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1180)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1180->$0 = _tmp$1181;
  _tmp$1180->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10$clo
  );
  _tuple$1179
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1179)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1179->$0
  = $azimuth$telemetry$working_tests$__test_776f726b696e675f74657374732e6d6274_10$clo;
  _tuple$1179->$1 = _tmp$1180;
  _tuple$1178
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1178)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1178->$0 = 10;
  _tuple$1178->$1 = _tuple$1179;
  _bind$956
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      11
    );
  _bind$956[0] = _tuple$1168;
  _bind$956[1] = _tuple$1169;
  _bind$956[2] = _tuple$1170;
  _bind$956[3] = _tuple$1171;
  _bind$956[4] = _tuple$1172;
  _bind$956[5] = _tuple$1173;
  _bind$956[6] = _tuple$1174;
  _bind$956[7] = _tuple$1175;
  _bind$956[8] = _tuple$1176;
  _bind$956[9] = _tuple$1177;
  _bind$956[10] = _tuple$1178;
  _tmp$1167 = _bind$956;
  _tmp$1166
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 11, _tmp$1167
  };
  _tmp$1165 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1166);
  _tuple$1160
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1160)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1160->$0 = (moonbit_string_t)moonbit_string_literal_68.data;
  _tuple$1160->$1 = _tmp$1165;
  _bind$957
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1164 = _bind$957;
  _tmp$1163
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1164
  };
  _tmp$1162 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1163);
  _tuple$1161
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1161)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1161->$0 = (moonbit_string_t)moonbit_string_literal_69.data;
  _tuple$1161->$1 = _tmp$1162;
  _bind$955
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      2
    );
  _bind$955[0] = _tuple$1160;
  _bind$955[1] = _tuple$1161;
  _tmp$1159 = _bind$955;
  _tmp$1158
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 2, _tmp$1159
  };
  $azimuth$telemetry$working_tests$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1158
  );
  _bind$958
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1213 = _bind$958;
  _tmp$1212
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1213
  };
  $azimuth$telemetry$working_tests$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1212
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1155;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1127;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1128;
  int32_t _len$1129;
  int32_t _i$1130;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1155
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1127
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1127)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1127->$0 = _tmp$1155;
  async_tests$1127->$1 = 0;
  _arr$1128
  = $azimuth$telemetry$working_tests$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1128);
  _len$1129 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1128);
  _i$1130 = 0;
  while (1) {
    if (_i$1130 < _len$1129) {
      struct $$3c$String$2a$Int$3e$* arg$1131;
      moonbit_string_t _field$2648;
      moonbit_string_t _tmp$1152;
      int32_t _field$2647;
      int32_t _cnt$2822;
      int32_t _tmp$1153;
      int32_t _tmp$1154;
      moonbit_incref(_arr$1128);
      arg$1131
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1128, _i$1130
      );
      _field$2648 = arg$1131->$0;
      _tmp$1152 = _field$2648;
      _field$2647 = arg$1131->$1;
      _cnt$2822 = Moonbit_object_header(arg$1131)->rc;
      if (_cnt$2822 > 1) {
        int32_t _new_cnt$2823;
        moonbit_incref(_tmp$1152);
        _new_cnt$2823 = _cnt$2822 - 1;
        Moonbit_object_header(arg$1131)->rc = _new_cnt$2823;
      } else if (_cnt$2822 == 1) {
        moonbit_free(arg$1131);
      }
      _tmp$1153 = _field$2647;
      moonbit_incref(async_tests$1127);
      $azimuth$telemetry$working_tests$moonbit_test_driver_internal_do_execute(
        async_tests$1127, _tmp$1152, _tmp$1153
      );
      _tmp$1154 = _i$1130 + 1;
      _i$1130 = _tmp$1154;
      continue;
    } else {
      moonbit_decref(_arr$1128);
    }
    break;
  }
  $azimuth$telemetry$working_tests$moonbit_test_driver_internal_run_async_tests(
    async_tests$1127
  );
  return 0;
}