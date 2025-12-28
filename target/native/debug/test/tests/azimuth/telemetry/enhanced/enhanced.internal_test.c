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

struct $Baggage;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

struct $StringView;

struct $$moonbitlang$core$double$internal$ryu$Umul128;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $SpanContext;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Logger;

struct $Moonbit_Test_Driver_Internal__TestCase;

struct $$3c$$3e$$3d$$3e$Unit;

struct $$3c$String$2a$AttributeValue$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$3c$Int$2a$Int$3e$;

struct $$3c$String$3e$$3d$$3e$Int;

struct $Option$3c$Option$3c$Int$3e$$3e$$Some;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$String$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$3c$StringView$2a$StringView$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$Int$3e$;

struct $$moonbitlang$core$builtin$Array$3c$String$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Resource;

struct $AttributeValue$FloatValue;

struct $Ref$3c$Int$3e$;

struct $AttributeValue$ArrayStringValue;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1;

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $ContextKey$3c$String$3e$;

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap;

struct $AttributeValue$IntValue;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$;

struct $Attributes;

struct $Context;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $AttributeValue$ArrayIntValue;

struct $AttributeValue$BoolValue;

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64;

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$enhanced$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$;

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $Moonbit_Test_Driver_Internal__F$F2;

struct $$moonbitlang$core$builtin$Hasher;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $Option$3c$StringView$3e$$Some;

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Ok;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578;

struct $AttributeValue$StringValue;

struct $$3c$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  
};

struct $Baggage {
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $StringView {
  int32_t $1;
  int32_t $2;
  moonbit_string_t $0;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $$3c$String$2a$AttributeValue$3e$ {
  moonbit_string_t $0;
  void* $1;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$3c$Int$2a$Int$3e$ {
  int32_t $0;
  int32_t $1;
  
};

struct $$3c$String$3e$$3d$$3e$Int {
  int32_t(* code)(struct $$3c$String$3e$$3d$$3e$Int*, moonbit_string_t);
  
};

struct $Option$3c$Option$3c$Int$3e$$3e$$Some {
  int64_t $0;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  moonbit_string_t $1;
  
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

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair {
  uint64_t $0;
  uint64_t $1;
  
};

struct $$moonbitlang$core$builtin$Logger$static_method_table {
  int32_t(* $method_0)(void*, moonbit_string_t);
  int32_t(* $method_1)(void*, moonbit_string_t, int32_t, int32_t);
  int32_t(* $method_2)(void*, struct $StringView);
  int32_t(* $method_3)(void*, int32_t);
  
};

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result {
  uint64_t $0;
  uint64_t $1;
  uint64_t $2;
  
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

struct $Resource {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* $0;
  
};

struct $AttributeValue$FloatValue {
  double $0;
  
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

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  void* $1;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
};

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $1;
  
};

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$ {
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $1;
  
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

struct $Attributes {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* $0;
  
};

struct $Context {
  struct $$3c$String$2a$String$3e$* $0;
  
};

struct $Error$moonbitlang$core$builtin$Failure$Failure {
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$ {
  int32_t $1;
  int32_t* $0;
  
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

struct $AttributeValue$BoolValue {
  int32_t $0;
  
};

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap {
  int32_t(* code)(struct $$3c$String$3e$$3d$$3e$Int*, moonbit_string_t);
  struct $$3c$String$3e$$3d$$3e$Int* $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64 {
  uint64_t $0;
  int32_t $1;
  
};

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$enhanced$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
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

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Ok {
  int32_t $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
};

struct $$moonbitlang$core$builtin$StringBuilder {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578 {
  void* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3665
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3664
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3663
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3662
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3661
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3660
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3659
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3658
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3657
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3656
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1592
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$3630,
  moonbit_string_t s$1570,
  int32_t sep$1571
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1557
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$3539,
  moonbit_bytes_t bytes$1558
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$3532,
  moonbit_string_t s$1552
);

#define $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1515,
  moonbit_string_t filename$1476,
  int32_t index$1477
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3525
);

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3521
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3519
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$3503,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1516,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1517
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$3514,
  int32_t _cont_param$1536
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3511,
  void* _cont_param$1537
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$3505,
  void* _state$1519
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3500,
  void* err$1499
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$3486,
  moonbit_string_t attr$1492
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$3470,
  moonbit_string_t test_name$1479,
  moonbit_string_t file_name$1480,
  moonbit_string_t message$1481,
  int32_t skipped$1482
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1474
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1472,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1473,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1470
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1433,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1446,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1459,
  moonbit_string_t file_filter$1430,
  int32_t index_filter$1431
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4(
  
);

struct $Resource* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$with_attributes(
  struct $Resource* resource$1413,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* attributes$1412
);

struct $Resource* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$new(
  
);

void* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$get_attribute(
  struct $Resource* resource$1406,
  moonbit_string_t key$1410
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3(
  
);

struct $ContextKey$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$ContextKey$$new(
  moonbit_string_t key$1399
);

struct $Context* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$with_value(
  struct $Context* ctx$1398,
  struct $ContextKey$3c$String$3e$* key$1396,
  moonbit_string_t value$1397
);

struct $Context* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$root(
  
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$get(
  struct $Context* ctx$1390,
  struct $ContextKey$3c$String$3e$* key$1395
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2(
  
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$trace_id(
  struct $SpanContext* ctx$1380
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$span_id(
  struct $SpanContext* ctx$1379
);

struct $SpanContext* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$new(
  moonbit_string_t trace_id$1375,
  moonbit_string_t span_id$1376,
  int32_t sampled$1377,
  moonbit_string_t trace_state$1378
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$is_valid(
  struct $SpanContext* ctx$1374
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$is_sampled(
  struct $SpanContext* ctx$1373
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1(
  
);

struct $Baggage* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$set_entry(
  struct $Baggage* baggage$1364,
  moonbit_string_t key$1365,
  moonbit_string_t value$1366
);

struct $Baggage* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$new(
  
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$get_entry(
  struct $Baggage* baggage$1358,
  moonbit_string_t key$1362
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0(
  
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$set(
  struct $Attributes* attrs$1350,
  moonbit_string_t key$1351,
  void* value$1352
);

struct $Attributes* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$new(
  
);

void* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$get(
  struct $Attributes* attrs$1349,
  moonbit_string_t key$1348
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0(
  
);

int32_t $$moonbitlang$core$builtin$Show$$Double$$output(
  double self$1286,
  struct $$moonbitlang$core$builtin$Logger logger$1285
);

moonbit_string_t $Double$$to_string(double self$1284);

moonbit_string_t $moonbitlang$core$double$internal$ryu$ryu_to_string(
  double val$1271
);

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d_small_int(
  uint64_t ieeeMantissa$1265,
  int32_t ieeeExponent$1267
);

moonbit_string_t $moonbitlang$core$double$internal$ryu$to_chars(
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* v$1238,
  int32_t sign$1236
);

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d(
  uint64_t ieeeMantissa$1180,
  uint32_t ieeeExponent$1179
);

int32_t $moonbitlang$core$double$internal$ryu$decimal_length17(
  uint64_t v$1176
);

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computeInvPow5(
  int32_t i$1159
);

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computePow5(
  int32_t i$1141
);

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result $moonbitlang$core$double$internal$ryu$mulShiftAll64(
  uint64_t m$1114,
  struct $$moonbitlang$core$double$internal$ryu$Pow5Pair mul$1111,
  int32_t j$1127,
  int32_t mmShift$1129
);

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf2(
  uint64_t value$1108,
  int32_t p$1109
);

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
  uint64_t value$1106,
  int32_t p$1107
);

int32_t $moonbitlang$core$double$internal$ryu$pow5Factor(uint64_t value$1102);

uint64_t $moonbitlang$core$double$internal$ryu$shiftright128(
  uint64_t lo$1101,
  uint64_t hi$1099,
  int32_t dist$1100
);

struct $$moonbitlang$core$double$internal$ryu$Umul128 $moonbitlang$core$double$internal$ryu$umul128(
  uint64_t a$1089,
  uint64_t b$1092
);

moonbit_string_t $moonbitlang$core$double$internal$ryu$string_from_bytes(
  moonbit_bytes_t bytes$1083,
  int32_t from$1087,
  int32_t to$1085
);

int32_t $moonbitlang$core$double$internal$ryu$log10Pow2(int32_t e$1081);

int32_t $moonbitlang$core$double$internal$ryu$log10Pow5(int32_t e$1080);

moonbit_string_t $moonbitlang$core$double$internal$ryu$copy_special_str(
  int32_t sign$1078,
  int32_t exponent$1079,
  int32_t mantissa$1076
);

int32_t $moonbitlang$core$double$internal$ryu$pow5bits(int32_t e$1075);

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1074
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$1072,
  struct $$moonbitlang$core$builtin$Logger logger$1073
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$1058,
  struct $$moonbitlang$core$builtin$Logger logger$1071
);

uint64_t $Bool$$to_uint64(int32_t self$1056);

int64_t $Bool$$to_int64(int32_t self$1055);

int32_t $Bool$$to_int(int32_t self$1054);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$1053);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$1048,
  moonbit_string_t msg$1050,
  moonbit_string_t loc$1052
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$1043,
  moonbit_string_t msg$1045,
  moonbit_string_t loc$1047
);

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$1042,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1041
);

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$1040,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1039
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$1037,
  moonbit_string_t value$1035
);

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$1031,
  struct $$3c$String$3e$$3d$$3e$Int* f$1033
);

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2720,
  moonbit_string_t k$1032
);

struct $$3c$String$2a$AttributeValue$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$4(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$1029,
  int32_t idx$1030
);

struct $$3c$String$2a$String$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$3(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$1027,
  int32_t idx$1028
);

int32_t $$moonbitlang$core$builtin$Array$$unsafe_get$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$1025,
  int32_t idx$1026
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1023,
  int32_t idx$1024
);

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$1021,
  int32_t idx$1022
);

uint64_t $UInt$$to_uint64(uint32_t self$1020);

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$1016,
  int32_t key$1012
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$1007,
  moonbit_string_t key$1003
);

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$998,
  int32_t key$994
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$989,
  moonbit_string_t key$985
);

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$980,
  int32_t key$976
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$971,
  moonbit_string_t key$967
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$959
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$951
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$943
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$935
);

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$931,
  moonbit_string_t key$932,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$933
);

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$928,
  moonbit_string_t key$929,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$930
);

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$925,
  int32_t key$926,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$927
);

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$922,
  moonbit_string_t key$923,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$924
);

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$912
);

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$901
);

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$890
);

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$879
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$862,
  moonbit_string_t key$871,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$872,
  int32_t hash$870
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$846,
  moonbit_string_t key$855,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$856,
  int32_t hash$854
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$830,
  int32_t key$839,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$840,
  int32_t hash$838
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$814,
  moonbit_string_t key$823,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$824,
  int32_t hash$822
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$808,
  int32_t idx$813,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$812
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$798,
  int32_t idx$803,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$802
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$788,
  int32_t idx$793,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$792
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$778,
  int32_t idx$783,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$782
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$768,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$770,
  int32_t new_idx$769
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$762,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$764,
  int32_t new_idx$763
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$756,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$758,
  int32_t new_idx$757
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$750,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$752,
  int32_t new_idx$751
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$747,
  int32_t idx$749,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$748
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$743,
  int32_t idx$745,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$744
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$739,
  int32_t idx$741,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$740
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$735,
  int32_t idx$737,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$736
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$729
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$723
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$717
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$711
);

int32_t $Int$$next_power_of_two(int32_t self$709);

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$708);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$706
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$704
);

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$702
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$700
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$2(
  int32_t self$694,
  int32_t other$695
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$1(
  moonbit_string_t self$688,
  moonbit_string_t other$689
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  int64_t self$682,
  int64_t other$683
);

uint32_t $ReadOnlyArray$$at$1(uint32_t* self$680, int32_t index$681);

uint64_t $ReadOnlyArray$$at$0(uint64_t* self$678, int32_t index$679);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$677
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$676
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$674
);

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$2368
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$2(
  int32_t self$669,
  struct $$moonbitlang$core$builtin$Logger logger$670
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$1(
  moonbit_string_t self$665,
  struct $$moonbitlang$core$builtin$Logger logger$666
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  int64_t self$661,
  struct $$moonbitlang$core$builtin$Logger logger$662
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$660
);

int32_t $$moonbitlang$core$builtin$Show$$UInt16$$output(
  int32_t self$659,
  struct $$moonbitlang$core$builtin$Logger logger$658
);

int32_t $$moonbitlang$core$builtin$Show$$UInt64$$output(
  uint64_t self$657,
  struct $$moonbitlang$core$builtin$Logger logger$656
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$655,
  struct $$moonbitlang$core$builtin$Logger logger$654
);

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$652,
  struct $$moonbitlang$core$builtin$Logger logger$653
);

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$650,
  struct $$3c$String$3e$$3d$$3e$Int* f$651
);

int32_t $String$$contains(
  moonbit_string_t self$647,
  struct $StringView str$648
);

int32_t $StringView$$contains(
  struct $StringView self$645,
  struct $StringView str$646
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$641,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$643
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$638,
  struct $$3c$String$2a$Int$3e$* value$640
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$635,
  moonbit_string_t value$637
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$633
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$630
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$627
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$623,
  int32_t new_capacity$621
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$617,
  int32_t new_capacity$615
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$611,
  int32_t new_capacity$609
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$607
);

moonbit_string_t $String$$repeat(moonbit_string_t self$601, int32_t n$600);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$598,
  struct $StringView str$599
);

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$595,
  int32_t i$596,
  int32_t start_offset$597,
  int64_t end_offset$593
);

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$590,
  int32_t n$588,
  int32_t start_offset$584,
  int32_t end_offset$585
);

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$582,
  int32_t n$580,
  int32_t start_offset$579,
  int32_t end_offset$578
);

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$568,
  int32_t len$571,
  int32_t start_offset$575,
  int64_t end_offset$566
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$564
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$563
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$562
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$561
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$560
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$555
);

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$2287,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$553
);

moonbit_string_t $$moonbitlang$core$builtin$Iterator$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$552
);

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$543,
  struct $$moonbitlang$core$builtin$Logger logger$541
);

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$537,
  int32_t seg$540,
  int32_t i$539
);

moonbit_string_t $Byte$$to_hex(int32_t b$535);

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$533);

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$531,
  int32_t that$532
);

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$529,
  int32_t that$530
);

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$527,
  int32_t that$528
);

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$525,
  int32_t that$526
);

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$522,
  int32_t start$520,
  int32_t end$521
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$519
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$7(
  int32_t a$513,
  int32_t b$514,
  moonbit_string_t msg$516,
  moonbit_string_t loc$518
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$6(
  moonbit_string_t a$507,
  moonbit_string_t b$508,
  moonbit_string_t msg$510,
  moonbit_string_t loc$512
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$5(
  int32_t a$501,
  int32_t b$502,
  moonbit_string_t msg$504,
  moonbit_string_t loc$506
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  int32_t a$495,
  int32_t b$496,
  moonbit_string_t msg$498,
  moonbit_string_t loc$500
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  moonbit_string_t a$489,
  moonbit_string_t b$490,
  moonbit_string_t msg$492,
  moonbit_string_t loc$494
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  double a$483,
  double b$484,
  moonbit_string_t msg$486,
  moonbit_string_t loc$488
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  int64_t a$477,
  int64_t b$478,
  moonbit_string_t msg$480,
  moonbit_string_t loc$482
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  int32_t a$471,
  int32_t b$472,
  moonbit_string_t msg$474,
  moonbit_string_t loc$476
);

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$470,
  moonbit_string_t loc$469
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$7(int32_t t$468);

moonbit_string_t $moonbitlang$core$builtin$debug_string$6(
  moonbit_string_t t$466
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$5(int32_t t$464);

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(int32_t t$462);

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(
  moonbit_string_t t$460
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(double t$458);

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(int64_t t$456);

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(int32_t t$454);

moonbit_string_t $UInt16$$to_string$inner(
  int32_t self$451,
  int32_t radix$452
);

moonbit_string_t $UInt64$$to_string$inner(
  uint64_t self$443,
  int32_t radix$442
);

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$432,
  uint64_t num$420,
  int32_t digit_start$423,
  int32_t total_len$422
);

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$414,
  uint64_t num$408,
  int32_t digit_start$406,
  int32_t total_len$405,
  int32_t radix$410
);

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$401,
  uint64_t num$397,
  int32_t digit_start$395,
  int32_t total_len$394
);

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$387,
  int32_t radix$390
);

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$385);

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$384);

moonbit_string_t $Int$$to_string$inner(int32_t self$368, int32_t radix$367);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$361,
  int32_t radix$364
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$359);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$358);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$348,
  uint32_t num$336,
  int32_t digit_start$339,
  int32_t total_len$338
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$330,
  uint32_t num$324,
  int32_t digit_start$322,
  int32_t total_len$321,
  int32_t radix$326
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$317,
  uint32_t num$313,
  int32_t digit_start$311,
  int32_t total_len$310
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
  void* self$308
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$306
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  uint64_t self$304
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$302
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$300
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$298
);

int32_t $StringView$$start_offset(struct $StringView self$296);

moonbit_string_t $StringView$$data(struct $StringView self$295);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$289,
  moonbit_string_t value$292,
  int32_t start$293,
  int32_t len$294
);

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$282,
  int32_t start$288,
  int64_t end$284
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$280
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$278
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$275
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$273
);

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$272
);

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$271
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$269,
  int32_t value$268
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$267,
  moonbit_string_t value$266
);

uint64_t $Int$$to_uint64(int32_t self$265);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$263,
  int32_t value$264
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$261,
  uint32_t value$262
);

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$259,
  uint32_t input$260
);

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$257, int32_t r$258);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$255,
  moonbit_string_t str$256
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$247,
  int32_t bytes_offset$242,
  moonbit_string_t str$249,
  int32_t str_offset$245,
  int32_t length$243
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$209
);

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$205
);

struct $StringView $StringView$$view$inner(
  struct $StringView self$202,
  int32_t start_offset$203,
  int64_t end_offset$200
);

int64_t $StringView$$rev_find(
  struct $StringView self$198,
  struct $StringView str$197
);

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$188,
  struct $StringView needle$190
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$177,
  struct $StringView needle$179
);

int64_t $StringView$$find(
  struct $StringView self$175,
  struct $StringView str$174
);

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$164,
  struct $StringView needle$166
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$150,
  struct $StringView needle$152
);

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$146,
  int32_t index$147
);

int32_t $StringView$$length(struct $StringView self$145);

int32_t $$moonbitlang$core$builtin$Array$$at$2(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$143,
  int32_t index$144
);

int32_t $$moonbitlang$core$builtin$Array$$at$1(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$140,
  int32_t index$141
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$137,
  int32_t index$138
);

int32_t $$moonbitlang$core$builtin$Array$$length$5(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$135
);

int32_t $$moonbitlang$core$builtin$Array$$length$4(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$134
);

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$133
);

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$132
);

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$131
);

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$130
);

struct $$3c$String$2a$AttributeValue$3e$** $$moonbitlang$core$builtin$Array$$buffer$6(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$129
);

struct $$3c$String$2a$String$3e$** $$moonbitlang$core$builtin$Array$$buffer$5(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$128
);

int32_t* $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$127
);

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$126
);

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$125
);

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$124
);

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$123
);

moonbit_string_t $String$$escape(moonbit_string_t self$122);

int32_t $moonbitlang$core$builtin$op_notequal$5(int32_t x$119, int32_t y$120);

int32_t $moonbitlang$core$builtin$op_notequal$4(
  moonbit_string_t x$117,
  moonbit_string_t y$118
);

int32_t $moonbitlang$core$builtin$op_notequal$3(int32_t x$115, int32_t y$116);

int32_t $moonbitlang$core$builtin$op_notequal$2(
  moonbit_string_t x$113,
  moonbit_string_t y$114
);

int32_t $moonbitlang$core$builtin$op_notequal$1(int64_t x$111, int64_t y$112);

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$109, int32_t y$110);

int32_t $Int$$is_trailing_surrogate(int32_t self$108);

int32_t $Int$$is_leading_surrogate(int32_t self$107);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$104,
  int32_t ch$106
);

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$99,
  int32_t required$100
);

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default();

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$93,
  int32_t offset$94,
  int32_t value$92
);

int32_t $UInt$$to_byte(uint32_t self$90);

uint32_t $Char$$to_uint(int32_t self$89);

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$88
);

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$83,
  int32_t offset$87,
  int64_t length$85
);

#define $moonbitlang$core$builtin$unsafe_sub_string moonbit_unsafe_bytes_sub_string

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$80
);

int32_t $Byte$$to_char(int32_t self$78);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$73,
  int32_t dst_offset$74,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$75,
  int32_t src_offset$76,
  int32_t len$77
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$68,
  int32_t dst_offset$69,
  struct $$3c$String$2a$Int$3e$** src$70,
  int32_t src_offset$71,
  int32_t len$72
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$63,
  int32_t dst_offset$64,
  moonbit_string_t* src$65,
  int32_t src_offset$66,
  int32_t len$67
);

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$54,
  int32_t dst_offset$56,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$55,
  int32_t src_offset$57,
  int32_t len$59
);

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$45,
  int32_t dst_offset$47,
  struct $$3c$String$2a$Int$3e$** src$46,
  int32_t src_offset$48,
  int32_t len$50
);

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$36,
  int32_t dst_offset$38,
  moonbit_string_t* src$37,
  int32_t src_offset$39,
  int32_t len$41
);

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$27,
  int32_t dst_offset$29,
  moonbit_bytes_t src$28,
  int32_t src_offset$30,
  int32_t len$32
);

int64_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$25,
  moonbit_string_t loc$26
);

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$23,
  moonbit_string_t loc$24
);

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$21,
  moonbit_string_t loc$22
);

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
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

moonbit_string_t $Error$to_string(void* _e$1599);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1617,
  int32_t _param$1616
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1614,
  struct $StringView _param$1613
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1611,
  moonbit_string_t _param$1608,
  int32_t _param$1609,
  int32_t _param$1610
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1606,
  moonbit_string_t _param$1605
);

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_54 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    104, 101, 108, 108, 111, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[1]; 
} const moonbit_string_literal_3 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 0), 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_101 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 48, 56, 58, 51, 45, 49, 48, 56, 58, 51, 
    51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_89 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 97, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_125 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 48, 58, 51, 45, 49, 48, 58, 52, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_46 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    115, 116, 114, 105, 110, 103, 46, 107, 101, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_6 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_100 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    97, 98, 99, 100, 101, 102, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_43 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    52, 57, 58, 51, 45, 52, 57, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[98]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 97), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    49, 48, 52, 58, 49, 54, 45, 49, 48, 52, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_179 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_137 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 51, 55, 58, 51, 45, 51, 55, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_90 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 56, 54, 58, 51, 45, 56, 54, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_78 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 50, 54, 58, 51, 45, 49, 50, 54, 58, 51, 
    57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_159 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_106 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 53, 48, 58, 51, 45, 53, 48, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_145 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    73, 110, 102, 105, 110, 105, 116, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_144 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    78, 97, 78, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[96]; 
} const moonbit_string_literal_49 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 95), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    49, 57, 58, 49, 54, 45, 49, 57, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_150 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    78, 111, 110, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    114, 101, 113, 117, 101, 115, 116, 46, 105, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_141 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_126 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    116, 101, 108, 101, 109, 101, 116, 114, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_63 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 55, 49, 58, 51, 45, 49, 55, 49, 58, 51, 
    51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_79 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 50, 55, 58, 51, 45, 49, 50, 55, 58, 50, 
    57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_45 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    53, 49, 58, 51, 45, 53, 49, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_58 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 54, 52, 58, 51, 45, 49, 54, 52, 58, 50, 
    56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[96]; 
} const moonbit_string_literal_52 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 95), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    51, 49, 58, 49, 51, 45, 51, 49, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[34]; 
} const moonbit_string_literal_182 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 33), 
    101, 110, 104, 97, 110, 99, 101, 100, 32, 98, 97, 115, 105, 99, 32, 
    102, 117, 110, 99, 116, 105, 111, 110, 97, 108, 105, 116, 121, 32, 
    116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_75 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 50, 49, 58, 51, 45, 49, 50, 49, 58, 51, 
    53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_117 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 54, 55, 58, 51, 45, 54, 55, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_32 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    98, 55, 97, 100, 54, 98, 55, 49, 54, 57, 50, 48, 51, 51, 51, 49, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_136 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 51, 54, 58, 51, 45, 51, 54, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_116 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 54, 54, 58, 51, 45, 54, 54, 58, 50, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_92 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 87, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[54]; 
} const moonbit_string_literal_171 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 53), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 118, 105, 101, 119, 46, 109, 98, 116, 58, 49, 
    49, 49, 58, 53, 45, 49, 49, 49, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_114 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 54, 52, 58, 51, 45, 54, 52, 58, 50, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_94 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 57, 50, 58, 51, 45, 57, 50, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_9 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    44, 32, 34, 105, 110, 100, 101, 120, 34, 58, 32, 34, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_194 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    97, 122, 105, 109, 117, 116, 104, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_169 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_152 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 41, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_99 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 48, 52, 58, 51, 45, 49, 48, 52, 58, 51, 
    57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[96]; 
} const moonbit_string_literal_50 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 95), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    50, 53, 58, 49, 51, 45, 50, 53, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_131 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 50, 51, 58, 51, 45, 50, 51, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    115, 101, 114, 118, 105, 99, 101, 46, 110, 97, 109, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_30 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    56, 51, 58, 51, 45, 56, 51, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_28 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    55, 57, 58, 51, 45, 55, 57, 58, 52, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_151 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    83, 111, 109, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[70]; 
} const moonbit_string_literal_123 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 69), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 57, 58, 51, 45, 57, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_65 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    100, 101, 102, 97, 117, 108, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_42 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    114, 101, 113, 52, 53, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_82 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 51, 48, 58, 51, 45, 49, 51, 48, 58, 51, 
    52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_67 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[98]; 
} const moonbit_string_literal_20 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 97), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    49, 48, 53, 58, 49, 51, 45, 49, 48, 53, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_0 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 58, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_167 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 54, 54, 51, 
    58, 53, 45, 54, 54, 51, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_156 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 51, 54, 54, 58, 53, 45, 
    51, 54, 54, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[30]; 
} const moonbit_string_literal_97 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 29), 
    33, 64, 35, 36, 37, 94, 38, 42, 40, 41, 95, 43, 45, 61, 91, 93, 123, 
    125, 124, 59, 39, 58, 34, 44, 46, 47, 60, 62, 63, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_56 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 54, 50, 58, 51, 45, 49, 54, 50, 58, 52, 
    48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_55 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 54, 49, 58, 51, 45, 49, 54, 49, 58, 51, 
    50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    48, 97, 102, 55, 54, 53, 49, 57, 49, 54, 99, 100, 52, 51, 100, 100, 
    56, 52, 52, 56, 101, 98, 50, 49, 49, 99, 56, 48, 51, 49, 57, 99, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_2 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 47, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[26]; 
} const moonbit_string_literal_142 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 25), 
    73, 108, 108, 101, 103, 97, 108, 65, 114, 103, 117, 109, 101, 110, 
    116, 69, 120, 99, 101, 112, 116, 105, 111, 110, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_27 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    116, 101, 115, 116, 95, 118, 97, 108, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_72 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    98, 97, 110, 97, 110, 97, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[117]; 
} const moonbit_string_literal_181 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 116), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 109, 
    117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 47, 
    101, 110, 104, 97, 110, 99, 101, 100, 46, 77, 111, 111, 110, 66, 
    105, 116, 84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 73, 110, 
    116, 101, 114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 114, 46, 
    77, 111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 68, 114, 105, 
    118, 101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 74, 115, 69, 
    114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_80 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 50, 56, 58, 51, 45, 49, 50, 56, 58, 50, 
    57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_34 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    54, 49, 58, 51, 45, 54, 49, 58, 53, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_149 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 102, 97, 108, 115, 101, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[25]; 
} const moonbit_string_literal_189 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 24), 
    98, 97, 103, 103, 97, 103, 101, 32, 98, 97, 115, 105, 99, 32, 111, 
    112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_162 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_160 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_148 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_129 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 54, 58, 51, 45, 49, 54, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_1 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 45, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_81 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 50, 57, 58, 51, 45, 49, 50, 57, 58, 51, 
    51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_71 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    97, 112, 112, 108, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_64 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 55, 56, 58, 51, 45, 49, 55, 56, 58, 50, 
    56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_88 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 56, 50, 58, 51, 45, 56, 50, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_95 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    32, 32, 115, 112, 97, 99, 101, 100, 32, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[98]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 97), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    49, 49, 54, 58, 49, 51, 45, 49, 49, 54, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_127 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 49, 58, 51, 45, 49, 49, 58, 52, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_124 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    97, 122, 105, 109, 117, 116, 104, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_86 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 51, 57, 58, 51, 45, 49, 51, 57, 58, 51, 
    55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_108 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 53, 52, 58, 51, 45, 53, 52, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[24]; 
} const moonbit_string_literal_190 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 23), 
    115, 112, 97, 110, 32, 99, 111, 110, 116, 101, 120, 116, 32, 111, 
    112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    117, 115, 101, 114, 46, 105, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_128 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 53, 58, 51, 45, 49, 53, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[28]; 
} const moonbit_string_literal_188 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 27), 
    97, 116, 116, 114, 105, 98, 117, 116, 101, 115, 32, 98, 97, 115, 
    105, 99, 32, 111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_155 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    73, 110, 118, 97, 108, 105, 100, 32, 115, 116, 97, 114, 116, 32, 
    105, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[79]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 78), 
    123, 34, 112, 97, 99, 107, 97, 103, 101, 34, 58, 32, 34, 97, 122, 
    105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 
    121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 109, 117, 116, 
    104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 47, 101, 110, 
    104, 97, 110, 99, 101, 100, 34, 44, 32, 34, 102, 105, 108, 101, 110, 
    97, 109, 101, 34, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[59]; 
} const moonbit_string_literal_143 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 58), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 100, 111, 117, 98, 108, 101, 47, 105, 110, 116, 
    101, 114, 110, 97, 108, 47, 114, 121, 117, 58, 114, 121, 117, 46, 
    109, 98, 116, 58, 49, 49, 54, 58, 51, 45, 49, 49, 54, 58, 52, 53, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_44 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    53, 48, 58, 51, 45, 53, 48, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[96]; 
} const moonbit_string_literal_51 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 95), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    50, 52, 58, 49, 54, 45, 50, 52, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    109, 105, 115, 115, 105, 110, 103, 46, 107, 101, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_15 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    116, 101, 115, 116, 45, 115, 101, 114, 118, 105, 99, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    54, 56, 58, 51, 45, 54, 56, 58, 53, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_119 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 54, 57, 58, 51, 45, 54, 57, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_164 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    32, 33, 61, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_98 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 48, 48, 58, 51, 45, 49, 48, 48, 58, 52, 
    48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_74 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 50, 48, 58, 51, 45, 49, 50, 48, 58, 51, 
    56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[32]; 
} const moonbit_string_literal_193 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 31), 
    97, 122, 105, 109, 117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 
    101, 100, 95, 116, 101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 
    109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    107, 101, 121, 49, 61, 118, 97, 108, 117, 101, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_102 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 48, 57, 58, 51, 45, 49, 48, 57, 58, 51, 
    51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_4 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_192 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    114, 101, 115, 111, 117, 114, 99, 101, 32, 111, 112, 101, 114, 97, 
    116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_158 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 52, 57, 49, 58, 57, 45, 
    52, 57, 49, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    49, 46, 48, 46, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_133 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 50, 53, 58, 51, 45, 50, 53, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_68 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 56, 52, 58, 51, 45, 49, 56, 52, 58, 52, 
    50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_157 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_84 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    111, 110, 108, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_174 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_166 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_187 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    116, 101, 115, 116, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_165 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    32, 70, 65, 73, 76, 69, 68, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_77 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 50, 53, 58, 51, 45, 49, 50, 53, 58, 51, 
    56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_183 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    101, 110, 104, 97, 110, 99, 101, 100, 32, 110, 117, 109, 101, 114, 
    105, 99, 32, 111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 32, 
    116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[19]; 
} const moonbit_string_literal_191 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 18), 
    99, 111, 110, 116, 101, 120, 116, 32, 111, 112, 101, 114, 97, 116, 
    105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_111 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 53, 57, 58, 51, 45, 53, 57, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_154 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    102, 97, 108, 115, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_122 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    97, 122, 105, 109, 117, 116, 104, 32, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_70 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 57, 57, 58, 51, 45, 49, 57, 57, 58, 51, 
    49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_59 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 54, 53, 58, 51, 45, 49, 54, 53, 58, 51, 
    49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[96]; 
} const moonbit_string_literal_48 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 95), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    50, 48, 58, 49, 51, 45, 50, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_177 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    73, 110, 100, 101, 120, 79, 117, 116, 79, 102, 66, 111, 117, 110, 
    100, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_87 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 52, 54, 58, 51, 45, 49, 52, 54, 58, 50, 
    49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_5 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 32, 98, 
    108, 111, 99, 107, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_168 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_47 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    105, 110, 116, 46, 107, 101, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    117, 115, 101, 114, 49, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_105 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 52, 57, 58, 51, 45, 52, 57, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_91 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    72, 101, 108, 108, 111, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_73 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    99, 104, 101, 114, 114, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_66 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_180 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 73, 110, 115, 
    112, 101, 99, 116, 69, 114, 114, 111, 114, 46, 73, 110, 115, 112, 
    101, 99, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_175 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 10, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_109 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 53, 53, 58, 51, 45, 53, 53, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_93 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_13 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    45, 45, 45, 45, 45, 32, 69, 78, 68, 32, 77, 79, 79, 78, 32, 84, 69, 
    83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_113 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 54, 49, 58, 51, 45, 54, 49, 58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_103 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 52, 55, 58, 51, 45, 52, 55, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[98]; 
} const moonbit_string_literal_22 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 97), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    49, 49, 48, 58, 49, 51, 45, 49, 49, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_85 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 51, 56, 58, 51, 45, 49, 51, 56, 58, 51, 
    56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_176 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    70, 97, 105, 108, 117, 114, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    109, 105, 115, 115, 105, 110, 103, 46, 97, 116, 116, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_10 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    34, 44, 32, 34, 116, 101, 115, 116, 95, 110, 97, 109, 101, 34, 58, 
    32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_140 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 52, 48, 58, 51, 45, 52, 48, 58, 50, 50, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_35 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    54, 50, 58, 51, 45, 54, 50, 58, 53, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_121 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 55, 52, 58, 51, 45, 55, 52, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_76 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 50, 50, 58, 51, 45, 49, 50, 50, 58, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    45, 45, 45, 45, 45, 32, 66, 69, 71, 73, 78, 32, 77, 79, 79, 78, 32, 
    84, 69, 83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[98]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 97), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    49, 48, 57, 58, 49, 54, 45, 49, 48, 57, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_130 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 55, 58, 51, 45, 49, 55, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_115 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 54, 53, 58, 51, 45, 54, 53, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_135 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 51, 53, 58, 51, 45, 51, 53, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_60 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 54, 54, 58, 51, 45, 49, 54, 54, 58, 50, 
    57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_173 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 48, 58, 53, 45, 50, 
    57, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_170 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    73, 110, 118, 97, 108, 105, 100, 32, 105, 110, 100, 101, 120, 32, 
    102, 111, 114, 32, 86, 105, 101, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    57, 49, 58, 51, 45, 57, 49, 58, 52, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[26]; 
} const moonbit_string_literal_186 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 25), 
    101, 110, 104, 97, 110, 99, 101, 100, 32, 111, 112, 116, 105, 111, 
    110, 32, 116, 121, 112, 101, 32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_112 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 54, 48, 58, 51, 45, 54, 48, 58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_147 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 96, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_172 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_61 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 54, 57, 58, 51, 45, 49, 54, 57, 58, 51, 
    50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_163 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    92, 117, 123, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_83 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 51, 52, 58, 51, 45, 49, 51, 52, 58, 51, 
    55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_62 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 55, 48, 58, 51, 45, 49, 55, 48, 58, 51, 
    53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    116, 101, 115, 116, 46, 107, 101, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_110 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 53, 54, 58, 51, 45, 53, 54, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_69 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 57, 48, 58, 51, 45, 49, 57, 48, 58, 51, 
    50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[34]; 
} const moonbit_string_literal_184 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 33), 
    101, 110, 104, 97, 110, 99, 101, 100, 32, 115, 116, 114, 105, 110, 
    103, 32, 109, 97, 110, 105, 112, 117, 108, 97, 116, 105, 111, 110, 
    32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_153 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_36 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    54, 51, 58, 51, 45, 54, 51, 58, 52, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_161 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_138 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 51, 56, 58, 51, 45, 51, 56, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_118 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 54, 56, 58, 51, 45, 54, 56, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_120 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 55, 51, 58, 51, 45, 55, 51, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_96 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 57, 54, 58, 51, 45, 57, 54, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[74]; 
} const moonbit_string_literal_57 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 73), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 49, 54, 51, 58, 51, 45, 49, 54, 51, 58, 51, 
    53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_132 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 50, 52, 58, 51, 45, 50, 52, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_107 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 53, 49, 58, 51, 45, 53, 49, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[95]; 
} const moonbit_string_literal_37 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 94), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    54, 52, 58, 51, 45, 54, 52, 58, 52, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_185 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    101, 110, 104, 97, 110, 99, 101, 100, 32, 97, 114, 114, 97, 121, 
    32, 111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 32, 116, 101, 
    115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    115, 101, 114, 118, 105, 99, 101, 46, 118, 101, 114, 115, 105, 111, 
    110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_178 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    73, 110, 118, 97, 108, 105, 100, 73, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_146 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    48, 46, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_139 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 51, 57, 58, 51, 45, 51, 57, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[98]; 
} const moonbit_string_literal_25 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 97), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    49, 49, 53, 58, 49, 54, 45, 49, 49, 53, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_104 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 52, 56, 58, 51, 45, 52, 56, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[96]; 
} const moonbit_string_literal_53 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 95), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 97, 122, 105, 109, 
    117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 101, 100, 95, 116, 
    101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 109, 98, 116, 58, 
    51, 48, 58, 49, 54, 45, 51, 48, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[72]; 
} const moonbit_string_literal_134 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 71), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 101, 110, 104, 97, 110, 99, 101, 100, 58, 116, 101, 115, 116, 
    46, 109, 98, 116, 58, 51, 50, 58, 51, 45, 51, 50, 58, 50, 52, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3$dyncall
  };

struct { int32_t rc; uint32_t meta; struct $$3c$String$3e$$3d$$3e$Int data; 
} const $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$5
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0$dyncall
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

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$148;

int64_t $moonbitlang$core$builtin$brute_force_find$constr$162;

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
  
} $moonbitlang$core$double$internal$ryu$ryu_to_string$record$1270$object =
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

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$ryu_to_string$record$1270 =
  &$moonbitlang$core$double$internal$ryu$ryu_to_string$record$1270$object.data;

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_with_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_no_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_async_tests;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2$clo;

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3665
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3664
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3663
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3662
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3661
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3660
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3659
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3658
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3657
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3656
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2();
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1592
) {
  moonbit_decref(_tests$1592);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$1551 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1557 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$1564 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1557;
  int32_t moonbit_test_driver_internal_split_mbt_string$1569 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$3655 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$1576 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$1577;
  moonbit_string_t _tmp$3654;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$1578;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$1579;
  int32_t _len$1580;
  int32_t _i$1581;
  Moonbit_object_header(file_and_index$1576)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$1576->$0 = _tmp$3655;
  file_and_index$1576->$1 = 0;
  cli_args$1577
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$1564
  );
  _tmp$3654 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$1577, 1);
  test_args$1578
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$1569, _tmp$3654, 47
  );
  _arr$1579 = test_args$1578;
  moonbit_incref(_arr$1579);
  _len$1580 = $$moonbitlang$core$builtin$Array$$length$1(_arr$1579);
  _i$1581 = 0;
  while (1) {
    if (_i$1581 < _len$1580) {
      moonbit_string_t arg$1582;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$1583;
      moonbit_string_t file$1584;
      moonbit_string_t range$1585;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1586;
      moonbit_string_t _tmp$3652;
      int32_t start$1587;
      moonbit_string_t _tmp$3651;
      int32_t end$1588;
      int32_t i$1589;
      int32_t _tmp$3653;
      moonbit_incref(_arr$1579);
      arg$1582
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$1579, _i$1581
      );
      file_and_range$1583
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1569, arg$1582, 58
      );
      moonbit_incref(file_and_range$1583);
      file$1584
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1583, 0
      );
      range$1585
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1583, 1
      );
      start_and_end$1586
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1569, range$1585, 45
      );
      moonbit_incref(start_and_end$1586);
      _tmp$3652
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1586, 0
      );
      start$1587
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1551, _tmp$3652
      );
      _tmp$3651
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1586, 1
      );
      end$1588
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1551, _tmp$3651
      );
      i$1589 = start$1587;
      while (1) {
        if (i$1589 < end$1588) {
          struct $$3c$String$2a$Int$3e$* _tuple$3649;
          int32_t _tmp$3650;
          moonbit_incref(file$1584);
          _tuple$3649
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$3649)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$3649->$0 = file$1584;
          _tuple$3649->$1 = i$1589;
          moonbit_incref(file_and_index$1576);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$1576, _tuple$3649
          );
          _tmp$3650 = i$1589 + 1;
          i$1589 = _tmp$3650;
          continue;
        } else {
          moonbit_decref(file$1584);
        }
        break;
      }
      _tmp$3653 = _i$1581 + 1;
      _i$1581 = _tmp$3653;
      continue;
    } else {
      moonbit_decref(_arr$1579);
    }
    break;
  }
  return file_and_index$1576;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$3630,
  moonbit_string_t s$1570,
  int32_t sep$1571
) {
  moonbit_string_t* _tmp$3648 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1572 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$1573;
  struct $Ref$3c$Int$3e$* start$1574;
  int32_t val$3643;
  int32_t _tmp$3644;
  Moonbit_object_header(res$1572)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$1572->$0 = _tmp$3648;
  res$1572->$1 = 0;
  i$1573
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1573)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1573->$0 = 0;
  start$1574
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$1574)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$1574->$0 = 0;
  while (1) {
    int32_t val$3631 = i$1573->$0;
    int32_t _tmp$3632 = Moonbit_array_length(s$1570);
    if (val$3631 < _tmp$3632) {
      int32_t val$3635 = i$1573->$0;
      int32_t _tmp$3634;
      int32_t _tmp$3633;
      int32_t val$3642;
      int32_t _tmp$3641;
      if (val$3635 < 0 || val$3635 >= Moonbit_array_length(s$1570)) {
        moonbit_panic();
      }
      _tmp$3634 = s$1570[val$3635];
      _tmp$3633 = _tmp$3634;
      if (_tmp$3633 == sep$1571) {
        int32_t val$3637 = start$1574->$0;
        int32_t val$3638 = i$1573->$0;
        moonbit_string_t _tmp$3636;
        int32_t val$3640;
        int32_t _tmp$3639;
        moonbit_incref(s$1570);
        _tmp$3636 = $String$$unsafe_substring(s$1570, val$3637, val$3638);
        moonbit_incref(res$1572);
        $$moonbitlang$core$builtin$Array$$push$0(res$1572, _tmp$3636);
        val$3640 = i$1573->$0;
        _tmp$3639 = val$3640 + 1;
        start$1574->$0 = _tmp$3639;
      }
      val$3642 = i$1573->$0;
      _tmp$3641 = val$3642 + 1;
      i$1573->$0 = _tmp$3641;
      continue;
    } else {
      moonbit_decref(i$1573);
    }
    break;
  }
  val$3643 = start$1574->$0;
  _tmp$3644 = Moonbit_array_length(s$1570);
  if (val$3643 < _tmp$3644) {
    int32_t _field$3666 = start$1574->$0;
    int32_t val$3646;
    int32_t _tmp$3647;
    moonbit_string_t _tmp$3645;
    moonbit_decref(start$1574);
    val$3646 = _field$3666;
    _tmp$3647 = Moonbit_array_length(s$1570);
    _tmp$3645 = $String$$unsafe_substring(s$1570, val$3646, _tmp$3647);
    moonbit_incref(res$1572);
    $$moonbitlang$core$builtin$Array$$push$0(res$1572, _tmp$3645);
  } else {
    moonbit_decref(start$1574);
    moonbit_decref(s$1570);
  }
  return res$1572;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1557
) {
  moonbit_bytes_t* tmp$1565 =
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$3629 = Moonbit_array_length(tmp$1565);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1566 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$3629);
  int32_t i$1567 = 0;
  while (1) {
    int32_t _tmp$3625 = Moonbit_array_length(tmp$1565);
    if (i$1567 < _tmp$3625) {
      moonbit_bytes_t _tmp$3667;
      moonbit_bytes_t _tmp$3627;
      moonbit_string_t _tmp$3626;
      int32_t _tmp$3628;
      if (i$1567 < 0 || i$1567 >= Moonbit_array_length(tmp$1565)) {
        moonbit_panic();
      }
      _tmp$3667 = (moonbit_bytes_t)tmp$1565[i$1567];
      _tmp$3627 = _tmp$3667;
      moonbit_incref(_tmp$3627);
      _tmp$3626
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1557, _tmp$3627
      );
      moonbit_incref(res$1566);
      $$moonbitlang$core$builtin$Array$$push$0(res$1566, _tmp$3626);
      _tmp$3628 = i$1567 + 1;
      i$1567 = _tmp$3628;
      continue;
    } else {
      moonbit_decref(tmp$1565);
    }
    break;
  }
  return res$1566;
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$3539,
  moonbit_bytes_t bytes$1558
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$1559 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$1560 = Moonbit_array_length(bytes$1558);
  struct $Ref$3c$Int$3e$* i$1561 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1561)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1561->$0 = 0;
  while (1) {
    int32_t val$3540 = i$1561->$0;
    if (val$3540 < len$1560) {
      int32_t val$3624 = i$1561->$0;
      int32_t _tmp$3623;
      int32_t _tmp$3622;
      struct $Ref$3c$Int$3e$* c$1562;
      int32_t val$3541;
      if (val$3624 < 0 || val$3624 >= Moonbit_array_length(bytes$1558)) {
        moonbit_panic();
      }
      _tmp$3623 = bytes$1558[val$3624];
      _tmp$3622 = (int32_t)_tmp$3623;
      c$1562
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$1562)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$1562->$0 = _tmp$3622;
      val$3541 = c$1562->$0;
      if (val$3541 < 128) {
        int32_t _field$3668 = c$1562->$0;
        int32_t val$3543;
        int32_t _tmp$3542;
        int32_t val$3545;
        int32_t _tmp$3544;
        moonbit_decref(c$1562);
        val$3543 = _field$3668;
        _tmp$3542 = val$3543;
        moonbit_incref(res$1559);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$1559, _tmp$3542
        );
        val$3545 = i$1561->$0;
        _tmp$3544 = val$3545 + 1;
        i$1561->$0 = _tmp$3544;
      } else {
        int32_t val$3546 = c$1562->$0;
        if (val$3546 < 224) {
          int32_t val$3548 = i$1561->$0;
          int32_t _tmp$3547 = val$3548 + 1;
          int32_t val$3557;
          int32_t _tmp$3556;
          int32_t _tmp$3550;
          int32_t val$3555;
          int32_t _tmp$3554;
          int32_t _tmp$3553;
          int32_t _tmp$3552;
          int32_t _tmp$3551;
          int32_t _tmp$3549;
          int32_t _field$3669;
          int32_t val$3559;
          int32_t _tmp$3558;
          int32_t val$3561;
          int32_t _tmp$3560;
          if (_tmp$3547 >= len$1560) {
            moonbit_decref(c$1562);
            moonbit_decref(i$1561);
            moonbit_decref(bytes$1558);
            break;
          }
          val$3557 = c$1562->$0;
          _tmp$3556 = val$3557 & 31;
          _tmp$3550 = _tmp$3556 << 6;
          val$3555 = i$1561->$0;
          _tmp$3554 = val$3555 + 1;
          if (_tmp$3554 < 0 || _tmp$3554 >= Moonbit_array_length(bytes$1558)) {
            moonbit_panic();
          }
          _tmp$3553 = bytes$1558[_tmp$3554];
          _tmp$3552 = (int32_t)_tmp$3553;
          _tmp$3551 = _tmp$3552 & 63;
          _tmp$3549 = _tmp$3550 | _tmp$3551;
          c$1562->$0 = _tmp$3549;
          _field$3669 = c$1562->$0;
          moonbit_decref(c$1562);
          val$3559 = _field$3669;
          _tmp$3558 = val$3559;
          moonbit_incref(res$1559);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$1559, _tmp$3558
          );
          val$3561 = i$1561->$0;
          _tmp$3560 = val$3561 + 2;
          i$1561->$0 = _tmp$3560;
        } else {
          int32_t val$3562 = c$1562->$0;
          if (val$3562 < 240) {
            int32_t val$3564 = i$1561->$0;
            int32_t _tmp$3563 = val$3564 + 2;
            int32_t val$3580;
            int32_t _tmp$3579;
            int32_t _tmp$3572;
            int32_t val$3578;
            int32_t _tmp$3577;
            int32_t _tmp$3576;
            int32_t _tmp$3575;
            int32_t _tmp$3574;
            int32_t _tmp$3573;
            int32_t _tmp$3566;
            int32_t val$3571;
            int32_t _tmp$3570;
            int32_t _tmp$3569;
            int32_t _tmp$3568;
            int32_t _tmp$3567;
            int32_t _tmp$3565;
            int32_t _field$3670;
            int32_t val$3582;
            int32_t _tmp$3581;
            int32_t val$3584;
            int32_t _tmp$3583;
            if (_tmp$3563 >= len$1560) {
              moonbit_decref(c$1562);
              moonbit_decref(i$1561);
              moonbit_decref(bytes$1558);
              break;
            }
            val$3580 = c$1562->$0;
            _tmp$3579 = val$3580 & 15;
            _tmp$3572 = _tmp$3579 << 12;
            val$3578 = i$1561->$0;
            _tmp$3577 = val$3578 + 1;
            if (
              _tmp$3577 < 0 || _tmp$3577 >= Moonbit_array_length(bytes$1558)
            ) {
              moonbit_panic();
            }
            _tmp$3576 = bytes$1558[_tmp$3577];
            _tmp$3575 = (int32_t)_tmp$3576;
            _tmp$3574 = _tmp$3575 & 63;
            _tmp$3573 = _tmp$3574 << 6;
            _tmp$3566 = _tmp$3572 | _tmp$3573;
            val$3571 = i$1561->$0;
            _tmp$3570 = val$3571 + 2;
            if (
              _tmp$3570 < 0 || _tmp$3570 >= Moonbit_array_length(bytes$1558)
            ) {
              moonbit_panic();
            }
            _tmp$3569 = bytes$1558[_tmp$3570];
            _tmp$3568 = (int32_t)_tmp$3569;
            _tmp$3567 = _tmp$3568 & 63;
            _tmp$3565 = _tmp$3566 | _tmp$3567;
            c$1562->$0 = _tmp$3565;
            _field$3670 = c$1562->$0;
            moonbit_decref(c$1562);
            val$3582 = _field$3670;
            _tmp$3581 = val$3582;
            moonbit_incref(res$1559);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1559, _tmp$3581
            );
            val$3584 = i$1561->$0;
            _tmp$3583 = val$3584 + 3;
            i$1561->$0 = _tmp$3583;
          } else {
            int32_t val$3586 = i$1561->$0;
            int32_t _tmp$3585 = val$3586 + 3;
            int32_t val$3609;
            int32_t _tmp$3608;
            int32_t _tmp$3601;
            int32_t val$3607;
            int32_t _tmp$3606;
            int32_t _tmp$3605;
            int32_t _tmp$3604;
            int32_t _tmp$3603;
            int32_t _tmp$3602;
            int32_t _tmp$3594;
            int32_t val$3600;
            int32_t _tmp$3599;
            int32_t _tmp$3598;
            int32_t _tmp$3597;
            int32_t _tmp$3596;
            int32_t _tmp$3595;
            int32_t _tmp$3588;
            int32_t val$3593;
            int32_t _tmp$3592;
            int32_t _tmp$3591;
            int32_t _tmp$3590;
            int32_t _tmp$3589;
            int32_t _tmp$3587;
            int32_t val$3611;
            int32_t _tmp$3610;
            int32_t val$3615;
            int32_t _tmp$3614;
            int32_t _tmp$3613;
            int32_t _tmp$3612;
            int32_t _field$3671;
            int32_t val$3619;
            int32_t _tmp$3618;
            int32_t _tmp$3617;
            int32_t _tmp$3616;
            int32_t val$3621;
            int32_t _tmp$3620;
            if (_tmp$3585 >= len$1560) {
              moonbit_decref(c$1562);
              moonbit_decref(i$1561);
              moonbit_decref(bytes$1558);
              break;
            }
            val$3609 = c$1562->$0;
            _tmp$3608 = val$3609 & 7;
            _tmp$3601 = _tmp$3608 << 18;
            val$3607 = i$1561->$0;
            _tmp$3606 = val$3607 + 1;
            if (
              _tmp$3606 < 0 || _tmp$3606 >= Moonbit_array_length(bytes$1558)
            ) {
              moonbit_panic();
            }
            _tmp$3605 = bytes$1558[_tmp$3606];
            _tmp$3604 = (int32_t)_tmp$3605;
            _tmp$3603 = _tmp$3604 & 63;
            _tmp$3602 = _tmp$3603 << 12;
            _tmp$3594 = _tmp$3601 | _tmp$3602;
            val$3600 = i$1561->$0;
            _tmp$3599 = val$3600 + 2;
            if (
              _tmp$3599 < 0 || _tmp$3599 >= Moonbit_array_length(bytes$1558)
            ) {
              moonbit_panic();
            }
            _tmp$3598 = bytes$1558[_tmp$3599];
            _tmp$3597 = (int32_t)_tmp$3598;
            _tmp$3596 = _tmp$3597 & 63;
            _tmp$3595 = _tmp$3596 << 6;
            _tmp$3588 = _tmp$3594 | _tmp$3595;
            val$3593 = i$1561->$0;
            _tmp$3592 = val$3593 + 3;
            if (
              _tmp$3592 < 0 || _tmp$3592 >= Moonbit_array_length(bytes$1558)
            ) {
              moonbit_panic();
            }
            _tmp$3591 = bytes$1558[_tmp$3592];
            _tmp$3590 = (int32_t)_tmp$3591;
            _tmp$3589 = _tmp$3590 & 63;
            _tmp$3587 = _tmp$3588 | _tmp$3589;
            c$1562->$0 = _tmp$3587;
            val$3611 = c$1562->$0;
            _tmp$3610 = val$3611 - 65536;
            c$1562->$0 = _tmp$3610;
            val$3615 = c$1562->$0;
            _tmp$3614 = val$3615 >> 10;
            _tmp$3613 = _tmp$3614 + 55296;
            _tmp$3612 = _tmp$3613;
            moonbit_incref(res$1559);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1559, _tmp$3612
            );
            _field$3671 = c$1562->$0;
            moonbit_decref(c$1562);
            val$3619 = _field$3671;
            _tmp$3618 = val$3619 & 1023;
            _tmp$3617 = _tmp$3618 + 56320;
            _tmp$3616 = _tmp$3617;
            moonbit_incref(res$1559);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1559, _tmp$3616
            );
            val$3621 = i$1561->$0;
            _tmp$3620 = val$3621 + 4;
            i$1561->$0 = _tmp$3620;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$1561);
      moonbit_decref(bytes$1558);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$1559);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$3532,
  moonbit_string_t s$1552
) {
  struct $Ref$3c$Int$3e$* res$1553 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$1554;
  int32_t i$1555;
  int32_t _field$3672;
  Moonbit_object_header(res$1553)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$1553->$0 = 0;
  len$1554 = Moonbit_array_length(s$1552);
  i$1555 = 0;
  while (1) {
    if (i$1555 < len$1554) {
      int32_t val$3537 = res$1553->$0;
      int32_t _tmp$3534 = val$3537 * 10;
      int32_t _tmp$3536;
      int32_t _tmp$3535;
      int32_t _tmp$3533;
      int32_t _tmp$3538;
      if (i$1555 < 0 || i$1555 >= Moonbit_array_length(s$1552)) {
        moonbit_panic();
      }
      _tmp$3536 = s$1552[i$1555];
      _tmp$3535 = _tmp$3536 - 48;
      _tmp$3533 = _tmp$3534 + _tmp$3535;
      res$1553->$0 = _tmp$3533;
      _tmp$3538 = i$1555 + 1;
      i$1555 = _tmp$3538;
      continue;
    } else {
      moonbit_decref(s$1552);
    }
    break;
  }
  _field$3672 = res$1553->$0;
  moonbit_decref(res$1553);
  return _field$3672;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1515,
  moonbit_string_t filename$1476,
  int32_t index$1477
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$1475;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$4171;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1478;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$1487;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$3682;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$3531;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3681;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$1488;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$3680;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$3530;
  moonbit_string_t _field$3679;
  moonbit_string_t file_name$1489;
  moonbit_string_t name$1490;
  int32_t _tmp$3527;
  moonbit_string_t name$1491;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$3484;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$3485;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$4173;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1498;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1514;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1539;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1541;
  void* _field$3676;
  int32_t _cnt$4010;
  void* _bind$1542;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$4177;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$3524;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$4178;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$3517;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$4179;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$3518;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$4180;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3502;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_async_tests
  );
  filtered_test$1475
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_async_tests,
      filename$1476,
      index$1477
  );
  _closure$4171
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$4171)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$4171->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$4171->$0 = index$1477;
  handle_result$1478
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$4171;
  if (filtered_test$1475 == 0) {
    moonbit_decref(async_tests$1515);
    if (filtered_test$1475) {
      moonbit_decref(filtered_test$1475);
    }
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1478,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$1549 =
      filtered_test$1475;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$1550 = _Some$1549;
    item$1487 = _item$1550;
    goto $join$1486;
  }
  goto $joinlet$4172;
  $join$1486:;
  _field$3682 = item$1487->$1;
  meta$3531 = _field$3682;
  _field$3681 = meta$3531->$2;
  attrs$1488 = _field$3681;
  _field$3680 = item$1487->$1;
  meta$3530 = _field$3680;
  _field$3679 = meta$3530->$0;
  file_name$1489 = _field$3679;
  moonbit_incref(attrs$1488);
  moonbit_incref(file_name$1489);
  moonbit_incref(attrs$1488);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$1488)) {
    name$1490 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$1488);
    name$1490 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1488, 0);
  }
  _tmp$3527 = Moonbit_array_length(name$1490);
  if (_tmp$3527 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$3678;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$3529;
    int32_t _field$3677;
    int32_t index$3528;
    moonbit_decref(name$1490);
    _field$3678 = item$1487->$1;
    meta$3529 = _field$3678;
    _field$3677 = meta$3529->$1;
    index$3528 = _field$3677;
    name$1491 = $Int$$to_string$inner(index$3528, 10);
  } else {
    name$1491 = name$1490;
  }
  moonbit_incref(attrs$1488);
  _tmp$3484 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$1488);
  _tmp$3485
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$3484, _tmp$3485)) {
    moonbit_string_t _tmp$3499;
    moonbit_string_t _tmp$3498;
    moonbit_string_t _tmp$3495;
    moonbit_string_t _tmp$3497;
    moonbit_string_t _tmp$3496;
    moonbit_string_t _tmp$3494;
    moonbit_decref(async_tests$1515);
    moonbit_decref(item$1487);
    moonbit_incref(file_name$1489);
    _tmp$3499
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$1489
    );
    _tmp$3498
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$3499
    );
    _tmp$3495
    = moonbit_add_string(
      _tmp$3498, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$3497 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1488, 0);
    _tmp$3496 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$3497);
    _tmp$3494 = moonbit_add_string(_tmp$3495, _tmp$3496);
    $moonbitlang$core$builtin$println$0(_tmp$3494);
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1478,
        name$1491,
        file_name$1489,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$1488);
  }
  moonbit_incref(name$1491);
  moonbit_incref(file_name$1489);
  moonbit_incref(handle_result$1478);
  _closure$4173
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$4173)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$4173->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$4173->$0 = name$1491;
  _closure$4173->$1 = file_name$1489;
  _closure$4173->$2 = handle_result$1478;
  on_err$1498 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$4173;
  _field$3676 = item$1487->$0;
  _cnt$4010 = Moonbit_object_header(item$1487)->rc;
  if (_cnt$4010 > 1) {
    int32_t _new_cnt$4012;
    moonbit_incref(_field$3676);
    _new_cnt$4012 = _cnt$4010 - 1;
    Moonbit_object_header(item$1487)->rc = _new_cnt$4012;
  } else if (_cnt$4010 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$4011 = item$1487->$1;
    moonbit_decref(_field$4011);
    moonbit_free(item$1487);
  }
  _bind$1542 = _field$3676;
  switch (Moonbit_object_tag(_bind$1542)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$1543;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3673;
      int32_t _cnt$4013;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1544;
      moonbit_decref(async_tests$1515);
      _F0$1543 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$1542;
      _field$3673 = _F0$1543->$0;
      _cnt$4013 = Moonbit_object_header(_F0$1543)->rc;
      if (_cnt$4013 > 1) {
        int32_t _new_cnt$4014;
        moonbit_incref(_field$3673);
        _new_cnt$4014 = _cnt$4013 - 1;
        Moonbit_object_header(_F0$1543)->rc = _new_cnt$4014;
      } else if (_cnt$4013 == 1) {
        moonbit_free(_F0$1543);
      }
      _f$1544 = _field$3673;
      f$1541 = _f$1544;
      goto $join$1540;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$1545;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3674;
      int32_t _cnt$4015;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1546;
      moonbit_decref(async_tests$1515);
      _F1$1545 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$1542;
      _field$3674 = _F1$1545->$0;
      _cnt$4015 = Moonbit_object_header(_F1$1545)->rc;
      if (_cnt$4015 > 1) {
        int32_t _new_cnt$4016;
        moonbit_incref(_field$3674);
        _new_cnt$4016 = _cnt$4015 - 1;
        Moonbit_object_header(_F1$1545)->rc = _new_cnt$4016;
      } else if (_cnt$4015 == 1) {
        moonbit_free(_F1$1545);
      }
      _f$1546 = _field$3674;
      f$1539 = _f$1546;
      goto $join$1538;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$1547 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$1542;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3675 =
        _F2$1547->$0;
      int32_t _cnt$4017 = Moonbit_object_header(_F2$1547)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$1548;
      if (_cnt$4017 > 1) {
        int32_t _new_cnt$4018;
        moonbit_incref(_field$3675);
        _new_cnt$4018 = _cnt$4017 - 1;
        Moonbit_object_header(_F2$1547)->rc = _new_cnt$4018;
      } else if (_cnt$4017 == 1) {
        moonbit_free(_F2$1547);
      }
      _f$1548 = _field$3675;
      f$1514 = _f$1548;
      goto $join$1513;
      break;
    }
  }
  goto $joinlet$4176;
  $join$1540:;
  _closure$4177
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$4177)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$4177->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$4177->$0 = name$1491;
  _closure$4177->$1 = file_name$1489;
  _closure$4177->$2 = handle_result$1478;
  _tmp$3524 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$4177;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_catch_error(
    f$1541, _tmp$3524, on_err$1498
  );
  $joinlet$4176:;
  goto $joinlet$4175;
  $join$1538:;
  moonbit_incref(name$1491);
  _closure$4178
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$4178)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$4178->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$4178->$0 = f$1539;
  _closure$4178->$1 = name$1491;
  _tmp$3517
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$4178;
  _closure$4179
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$4179)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$4179->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$4179->$0 = name$1491;
  _closure$4179->$1 = file_name$1489;
  _closure$4179->$2 = handle_result$1478;
  _tmp$3518 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$4179;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_catch_error(
    _tmp$3517, _tmp$3518, on_err$1498
  );
  $joinlet$4175:;
  goto $joinlet$4174;
  $join$1513:;
  _closure$4180
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$4180)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$4180->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$4180->$0 = f$1514;
  _closure$4180->$1 = on_err$1498;
  _closure$4180->$2 = name$1491;
  _closure$4180->$3 = file_name$1489;
  _closure$4180->$4 = handle_result$1478;
  _tmp$3502
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$4180;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$1515, _tmp$3502);
  $joinlet$4174:;
  $joinlet$4172:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3525
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$3526 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$3525;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3685 =
    _casted_env$3526->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1478 =
    _field$3685;
  moonbit_string_t _field$3684 = _casted_env$3526->$1;
  moonbit_string_t file_name$1489 = _field$3684;
  moonbit_string_t _field$3683 = _casted_env$3526->$0;
  int32_t _cnt$4019 = Moonbit_object_header(_casted_env$3526)->rc;
  moonbit_string_t name$1491;
  if (_cnt$4019 > 1) {
    int32_t _new_cnt$4020;
    moonbit_incref(handle_result$1478);
    moonbit_incref(file_name$1489);
    moonbit_incref(_field$3683);
    _new_cnt$4020 = _cnt$4019 - 1;
    Moonbit_object_header(_casted_env$3526)->rc = _new_cnt$4020;
  } else if (_cnt$4019 == 1) {
    moonbit_free(_casted_env$3526);
  }
  name$1491 = _field$3683;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1478,
      name$1491,
      file_name$1489,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3521
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$3522 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$3521;
  moonbit_string_t _field$3687 = _casted_env$3522->$1;
  moonbit_string_t name$1491 = _field$3687;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3686 =
    _casted_env$3522->$0;
  int32_t _cnt$4021 = Moonbit_object_header(_casted_env$3522)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1539;
  int32_t _tmp$3523;
  if (_cnt$4021 > 1) {
    int32_t _new_cnt$4022;
    moonbit_incref(name$1491);
    moonbit_incref(_field$3686);
    _new_cnt$4022 = _cnt$4021 - 1;
    Moonbit_object_header(_casted_env$3522)->rc = _new_cnt$4022;
  } else if (_cnt$4021 == 1) {
    moonbit_free(_casted_env$3522);
  }
  f$1539 = _field$3686;
  _tmp$3523
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_new_test_arg(
    name$1491
  );
  return f$1539->code(f$1539, _tmp$3523);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3519
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$3520 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$3519;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3690 =
    _casted_env$3520->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1478 =
    _field$3690;
  moonbit_string_t _field$3689 = _casted_env$3520->$1;
  moonbit_string_t file_name$1489 = _field$3689;
  moonbit_string_t _field$3688 = _casted_env$3520->$0;
  int32_t _cnt$4023 = Moonbit_object_header(_casted_env$3520)->rc;
  moonbit_string_t name$1491;
  if (_cnt$4023 > 1) {
    int32_t _new_cnt$4024;
    moonbit_incref(handle_result$1478);
    moonbit_incref(file_name$1489);
    moonbit_incref(_field$3688);
    _new_cnt$4024 = _cnt$4023 - 1;
    Moonbit_object_header(_casted_env$3520)->rc = _new_cnt$4024;
  } else if (_cnt$4023 == 1) {
    moonbit_free(_casted_env$3520);
  }
  name$1491 = _field$3688;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1478,
      name$1491,
      file_name$1489,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$3503,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1516,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1517
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$3504 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$3503;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3695 =
    _casted_env$3504->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1478 =
    _field$3695;
  moonbit_string_t _field$3694 = _casted_env$3504->$3;
  moonbit_string_t file_name$1489 = _field$3694;
  moonbit_string_t _field$3693 = _casted_env$3504->$2;
  moonbit_string_t name$1491 = _field$3693;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$3692 = _casted_env$3504->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1498 = _field$3692;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3691 =
    _casted_env$3504->$0;
  int32_t _cnt$4025 = Moonbit_object_header(_casted_env$3504)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1514;
  int32_t _async_driver$1518;
  int32_t _tmp$3508;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$4181;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$3509;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$4182;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$3510;
  if (_cnt$4025 > 1) {
    int32_t _new_cnt$4026;
    moonbit_incref(handle_result$1478);
    moonbit_incref(file_name$1489);
    moonbit_incref(name$1491);
    moonbit_incref(on_err$1498);
    moonbit_incref(_field$3691);
    _new_cnt$4026 = _cnt$4025 - 1;
    Moonbit_object_header(_casted_env$3504)->rc = _new_cnt$4026;
  } else if (_cnt$4025 == 1) {
    moonbit_free(_casted_env$3504);
  }
  f$1514 = _field$3691;
  _async_driver$1518 = 0;
  moonbit_incref(name$1491);
  _tmp$3508
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_new_test_arg(
    name$1491
  );
  moonbit_incref(_cont$1516);
  _closure$4181
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$4181)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$4181->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$4181->$0 = _async_driver$1518;
  _closure$4181->$1 = _cont$1516;
  _closure$4181->$2 = name$1491;
  _closure$4181->$3 = file_name$1489;
  _closure$4181->$4 = handle_result$1478;
  _tmp$3509 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$4181;
  _closure$4182
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$4182)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$4182->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$4182->$0 = _async_driver$1518;
  _closure$4182->$1 = _err_cont$1517;
  _closure$4182->$2 = _cont$1516;
  _closure$4182->$3 = on_err$1498;
  _tmp$3510 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$4182;
  f$1514->code(f$1514, _tmp$3508, _tmp$3509, _tmp$3510);
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$3514,
  int32_t _cont_param$1536
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$3515 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$3514;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3700 =
    _casted_env$3515->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1478 =
    _field$3700;
  moonbit_string_t _field$3699 = _casted_env$3515->$3;
  moonbit_string_t file_name$1489 = _field$3699;
  moonbit_string_t _field$3698 = _casted_env$3515->$2;
  moonbit_string_t name$1491 = _field$3698;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3697 = _casted_env$3515->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1516 = _field$3697;
  int32_t _field$3696 = _casted_env$3515->$0;
  int32_t _cnt$4027 = Moonbit_object_header(_casted_env$3515)->rc;
  int32_t _async_driver$1518;
  void* State_1$3516;
  if (_cnt$4027 > 1) {
    int32_t _new_cnt$4028;
    moonbit_incref(handle_result$1478);
    moonbit_incref(file_name$1489);
    moonbit_incref(name$1491);
    moonbit_incref(_cont$1516);
    _new_cnt$4028 = _cnt$4027 - 1;
    Moonbit_object_header(_casted_env$3515)->rc = _new_cnt$4028;
  } else if (_cnt$4027 == 1) {
    moonbit_free(_casted_env$3515);
  }
  _async_driver$1518 = _field$3696;
  State_1$3516
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1
      )
    );
  Moonbit_object_header(State_1$3516)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1*)State_1$3516)->$0
  = _cont_param$1536;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1*)State_1$3516)->$1
  = handle_result$1478;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1*)State_1$3516)->$2
  = file_name$1489;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1*)State_1$3516)->$3
  = name$1491;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1*)State_1$3516)->$4
  = _cont$1516;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1518, State_1$3516
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3511,
  void* _cont_param$1537
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$3512 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$3511;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$3704 = _casted_env$3512->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1498 = _field$3704;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3703 = _casted_env$3512->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1516 = _field$3703;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$3702 = _casted_env$3512->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1517 = _field$3702;
  int32_t _field$3701 = _casted_env$3512->$0;
  int32_t _cnt$4029 = Moonbit_object_header(_casted_env$3512)->rc;
  int32_t _async_driver$1518;
  void* _try$578$3513;
  if (_cnt$4029 > 1) {
    int32_t _new_cnt$4030;
    moonbit_incref(on_err$1498);
    moonbit_incref(_cont$1516);
    moonbit_incref(_err_cont$1517);
    _new_cnt$4030 = _cnt$4029 - 1;
    Moonbit_object_header(_casted_env$3512)->rc = _new_cnt$4030;
  } else if (_cnt$4029 == 1) {
    moonbit_free(_casted_env$3512);
  }
  _async_driver$1518 = _field$3701;
  _try$578$3513
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578
      )
    );
  Moonbit_object_header(_try$578$3513)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578*)_try$578$3513)->$0
  = _cont_param$1537;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578*)_try$578$3513)->$1
  = on_err$1498;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578*)_try$578$3513)->$2
  = _cont$1516;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578*)_try$578$3513)->$3
  = _err_cont$1517;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1518, _try$578$3513
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$3505,
  void* _state$1519
) {
  switch (Moonbit_object_tag(_state$1519)) {
    case 0: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578* _$2a$try$578$1520 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$$2a$try$578*)_state$1519;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$3708 = _$2a$try$578$1520->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1521 = _field$3708;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3707 = _$2a$try$578$1520->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1522 = _field$3707;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$3706 = _$2a$try$578$1520->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1523 = _field$3706;
      void* _field$3705 = _$2a$try$578$1520->$0;
      int32_t _cnt$4031 = Moonbit_object_header(_$2a$try$578$1520)->rc;
      void* _try_err$1524;
      void* err$1526;
      void* err$1528;
      int32_t _tmp$3507;
      if (_cnt$4031 > 1) {
        int32_t _new_cnt$4032;
        moonbit_incref(_err_cont$1521);
        moonbit_incref(_cont$1522);
        moonbit_incref(on_err$1523);
        moonbit_incref(_field$3705);
        _new_cnt$4032 = _cnt$4031 - 1;
        Moonbit_object_header(_$2a$try$578$1520)->rc = _new_cnt$4032;
      } else if (_cnt$4031 == 1) {
        moonbit_free(_$2a$try$578$1520);
      }
      _try_err$1524 = _field$3705;
      if (
        $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$1523);
        moonbit_decref(_cont$1522);
        err$1528 = _try_err$1524;
        goto $join$1527;
      } else {
        moonbit_decref(_err_cont$1521);
        err$1526 = _try_err$1524;
        goto $join$1525;
      }
      goto $joinlet$4184;
      $join$1527:;
      return _err_cont$1521->code(_err_cont$1521, err$1528);
      $joinlet$4184:;
      goto $joinlet$4183;
      $join$1525:;
      _tmp$3507 = on_err$1523->code(on_err$1523, err$1526);
      _cont$1522->code(_cont$1522, _tmp$3507);
      $joinlet$4183:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1* _State_1$1529 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$594$on_err$68$$2a$arm$586$lambda$612$State$State_1*)_state$1519;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3712 = _State_1$1529->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1530 = _field$3712;
      moonbit_string_t _field$3711 = _State_1$1529->$3;
      moonbit_string_t name$1531 = _field$3711;
      moonbit_string_t _field$3710 = _State_1$1529->$2;
      moonbit_string_t file_name$1532 = _field$3710;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3709 =
        _State_1$1529->$1;
      int32_t _cnt$4033 = Moonbit_object_header(_State_1$1529)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1533;
      int32_t _tmp$3506;
      if (_cnt$4033 > 1) {
        int32_t _new_cnt$4034;
        moonbit_incref(_cont$1530);
        moonbit_incref(name$1531);
        moonbit_incref(file_name$1532);
        moonbit_incref(_field$3709);
        _new_cnt$4034 = _cnt$4033 - 1;
        Moonbit_object_header(_State_1$1529)->rc = _new_cnt$4034;
      } else if (_cnt$4033 == 1) {
        moonbit_free(_State_1$1529);
      }
      handle_result$1533 = _field$3709;
      _tmp$3506
      = handle_result$1533->code(
        handle_result$1533,
          name$1531,
          file_name$1532,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$1530->code(_cont$1530, _tmp$3506);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3500,
  void* err$1499
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$3501 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$3500;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3719 =
    _casted_env$3501->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1478 =
    _field$3719;
  moonbit_string_t _field$3718 = _casted_env$3501->$1;
  moonbit_string_t file_name$1489 = _field$3718;
  moonbit_string_t _field$3717 = _casted_env$3501->$0;
  int32_t _cnt$4035 = Moonbit_object_header(_casted_env$3501)->rc;
  moonbit_string_t name$1491;
  void* e$1501;
  moonbit_string_t e$1504;
  moonbit_string_t message$1502;
  if (_cnt$4035 > 1) {
    int32_t _new_cnt$4036;
    moonbit_incref(handle_result$1478);
    moonbit_incref(file_name$1489);
    moonbit_incref(_field$3717);
    _new_cnt$4036 = _cnt$4035 - 1;
    Moonbit_object_header(_casted_env$3501)->rc = _new_cnt$4036;
  } else if (_cnt$4035 == 1) {
    moonbit_free(_casted_env$3501);
  }
  name$1491 = _field$3717;
  switch (Moonbit_object_tag(err$1499)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$1505 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$1499;
      moonbit_string_t _field$3713 = _Failure$1505->$0;
      int32_t _cnt$4037 = Moonbit_object_header(_Failure$1505)->rc;
      moonbit_string_t _e$1506;
      if (_cnt$4037 > 1) {
        int32_t _new_cnt$4038;
        moonbit_incref(_field$3713);
        _new_cnt$4038 = _cnt$4037 - 1;
        Moonbit_object_header(_Failure$1505)->rc = _new_cnt$4038;
      } else if (_cnt$4037 == 1) {
        moonbit_free(_Failure$1505);
      }
      _e$1506 = _field$3713;
      e$1504 = _e$1506;
      goto $join$1503;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$1507 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$1499;
      moonbit_string_t _field$3714 = _InspectError$1507->$0;
      int32_t _cnt$4039 = Moonbit_object_header(_InspectError$1507)->rc;
      moonbit_string_t _e$1508;
      if (_cnt$4039 > 1) {
        int32_t _new_cnt$4040;
        moonbit_incref(_field$3714);
        _new_cnt$4040 = _cnt$4039 - 1;
        Moonbit_object_header(_InspectError$1507)->rc = _new_cnt$4040;
      } else if (_cnt$4039 == 1) {
        moonbit_free(_InspectError$1507);
      }
      _e$1508 = _field$3714;
      e$1504 = _e$1508;
      goto $join$1503;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$1509 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$1499;
      moonbit_string_t _field$3715 = _SnapshotError$1509->$0;
      int32_t _cnt$4041 = Moonbit_object_header(_SnapshotError$1509)->rc;
      moonbit_string_t _e$1510;
      if (_cnt$4041 > 1) {
        int32_t _new_cnt$4042;
        moonbit_incref(_field$3715);
        _new_cnt$4042 = _cnt$4041 - 1;
        Moonbit_object_header(_SnapshotError$1509)->rc = _new_cnt$4042;
      } else if (_cnt$4041 == 1) {
        moonbit_free(_SnapshotError$1509);
      }
      _e$1510 = _field$3715;
      e$1504 = _e$1510;
      goto $join$1503;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$tests$azimuth$telemetry$enhanced$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$1511 =
        (struct $Error$azimuth$telemetry$tests$azimuth$telemetry$enhanced$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$1499;
      moonbit_string_t _field$3716 =
        _MoonBitTestDriverInternalJsError$1511->$0;
      int32_t _cnt$4043 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1511)->rc;
      moonbit_string_t _e$1512;
      if (_cnt$4043 > 1) {
        int32_t _new_cnt$4044;
        moonbit_incref(_field$3716);
        _new_cnt$4044 = _cnt$4043 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1511)->rc
        = _new_cnt$4044;
      } else if (_cnt$4043 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$1511);
      }
      _e$1512 = _field$3716;
      e$1504 = _e$1512;
      goto $join$1503;
      break;
    }
    default: {
      e$1501 = err$1499;
      goto $join$1500;
      break;
    }
  }
  goto $joinlet$4186;
  $join$1503:;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1478, name$1491, file_name$1489, e$1504, 0
  );
  $joinlet$4186:;
  goto $joinlet$4185;
  $join$1500:;
  message$1502 = $Error$to_string(e$1501);
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1478, name$1491, file_name$1489, message$1502, 0
  );
  $joinlet$4185:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$3486,
  moonbit_string_t attr$1492
) {
  int32_t _tmp$3488;
  int64_t _tmp$3487;
  moonbit_decref(_env$3486);
  _tmp$3488 = Moonbit_array_length(attr$1492);
  _tmp$3487 = (int64_t)_tmp$3488;
  moonbit_incref(attr$1492);
  if ($String$$char_length_ge$inner(attr$1492, 5, 0, _tmp$3487)) {
    int32_t _tmp$3493 = attr$1492[0];
    int32_t _x$1493 = _tmp$3493;
    if (_x$1493 == 112) {
      int32_t _tmp$3492 = attr$1492[1];
      int32_t _x$1494 = _tmp$3492;
      if (_x$1494 == 97) {
        int32_t _tmp$3491 = attr$1492[2];
        int32_t _x$1495 = _tmp$3491;
        if (_x$1495 == 110) {
          int32_t _tmp$3490 = attr$1492[3];
          int32_t _x$1496 = _tmp$3490;
          if (_x$1496 == 105) {
            int32_t _tmp$3720 = attr$1492[4];
            int32_t _tmp$3489;
            int32_t _x$1497;
            moonbit_decref(attr$1492);
            _tmp$3489 = _tmp$3720;
            _x$1497 = _tmp$3489;
            return _x$1497 == 99 || 0;
          } else {
            moonbit_decref(attr$1492);
            return 0;
          }
        } else {
          moonbit_decref(attr$1492);
          return 0;
        }
      } else {
        moonbit_decref(attr$1492);
        return 0;
      }
    } else {
      moonbit_decref(attr$1492);
      return 0;
    }
  } else {
    moonbit_decref(attr$1492);
    return 0;
  }
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$3470,
  moonbit_string_t test_name$1479,
  moonbit_string_t file_name$1480,
  moonbit_string_t message$1481,
  int32_t skipped$1482
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$3471 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$3470;
  int32_t _field$3721 = _casted_env$3471->$0;
  int32_t index$1477;
  int32_t _if_result$4187;
  moonbit_string_t file_name$1483;
  moonbit_string_t test_name$1484;
  moonbit_string_t message$1485;
  moonbit_string_t _tmp$3483;
  moonbit_string_t _tmp$3482;
  moonbit_string_t _tmp$3480;
  moonbit_string_t _tmp$3481;
  moonbit_string_t _tmp$3479;
  moonbit_string_t _tmp$3477;
  moonbit_string_t _tmp$3478;
  moonbit_string_t _tmp$3476;
  moonbit_string_t _tmp$3474;
  moonbit_string_t _tmp$3475;
  moonbit_string_t _tmp$3473;
  moonbit_string_t _tmp$3472;
  moonbit_decref(_casted_env$3471);
  index$1477 = _field$3721;
  if (!skipped$1482) {
    _if_result$4187 = 1;
  } else {
    _if_result$4187 = 0;
  }
  if (_if_result$4187) {
    
  }
  file_name$1483 = $String$$escape(file_name$1480);
  test_name$1484 = $String$$escape(test_name$1479);
  message$1485 = $String$$escape(message$1481);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$3483
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$1483
  );
  _tmp$3482
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$3483
  );
  _tmp$3480
  = moonbit_add_string(
    _tmp$3482, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$3481
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$1477
  );
  _tmp$3479 = moonbit_add_string(_tmp$3480, _tmp$3481);
  _tmp$3477
  = moonbit_add_string(
    _tmp$3479, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$3478
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$1484
  );
  _tmp$3476 = moonbit_add_string(_tmp$3477, _tmp$3478);
  _tmp$3474
  = moonbit_add_string(
    _tmp$3476, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$3475
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    message$1485
  );
  _tmp$3473 = moonbit_add_string(_tmp$3474, _tmp$3475);
  _tmp$3472
  = moonbit_add_string(
    _tmp$3473, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$3472);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1474
) {
  moonbit_decref(_discard_$1474);
  return 42;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1472,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1473,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1470
) {
  void* _try_err$1468;
  struct moonbit_result_0 _tmp$4189 = f$1472->code(f$1472);
  void* err$1469;
  if (_tmp$4189.tag) {
    int32_t const _ok$3468 = _tmp$4189.data.ok;
    moonbit_decref(on_err$1470);
  } else {
    void* const _err$3469 = _tmp$4189.data.err;
    moonbit_decref(on_ok$1473);
    _try_err$1468 = _err$3469;
    goto $join$1467;
  }
  on_ok$1473->code(on_ok$1473);
  goto $joinlet$4188;
  $join$1467:;
  err$1469 = _try_err$1468;
  on_err$1470->code(on_err$1470, err$1469);
  $joinlet$4188:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1433,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1446,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1459,
  moonbit_string_t file_filter$1430,
  int32_t index_filter$1431
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1427;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1428;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1432;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3727;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$3459;
  void* F0$3456;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3726;
  int32_t _cnt$4045;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$3458;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$3457;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1429;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1442;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1443;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1445;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3725;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$3463;
  void* F1$3460;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3724;
  int32_t _cnt$4048;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$3462;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$3461;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1444;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1455;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1456;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1458;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3723;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3467;
  void* F2$3464;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3722;
  int32_t _cnt$4051;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$3466;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$3465;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1457;
  moonbit_incref(file_filter$1430);
  _bind$1432
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$1433, file_filter$1430
  );
  if (_bind$1432 == 0) {
    if (_bind$1432) {
      moonbit_decref(_bind$1432);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1434 =
      _bind$1432;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1435 =
      _Some$1434;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1437;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1438;
    moonbit_incref(_index_func_map$1435);
    _bind$1438
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$1435, index_filter$1431
    );
    if (_bind$1438 == 0) {
      if (_bind$1438) {
        moonbit_decref(_bind$1438);
      }
      moonbit_decref(_index_func_map$1435);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1439;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1440;
      moonbit_decref(async_tests$1459);
      moonbit_decref(with_args_tests$1446);
      _Some$1439 = _bind$1438;
      _func_attrs_tuple$1440 = _Some$1439;
      func_attrs_tuple$1437 = _func_attrs_tuple$1440;
      goto $join$1436;
    }
    goto $joinlet$4191;
    $join$1436:;
    index_func_map$1427 = _index_func_map$1435;
    func_attrs_tuple$1428 = func_attrs_tuple$1437;
    goto $join$1426;
    $joinlet$4191:;
  }
  goto $joinlet$4190;
  $join$1426:;
  moonbit_decref(index_func_map$1427);
  _field$3727 = func_attrs_tuple$1428->$0;
  _tmp$3459 = _field$3727;
  moonbit_incref(_tmp$3459);
  F0$3456
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$3456)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$3456)->$0 = _tmp$3459;
  _field$3726 = func_attrs_tuple$1428->$1;
  _cnt$4045 = Moonbit_object_header(func_attrs_tuple$1428)->rc;
  if (_cnt$4045 > 1) {
    int32_t _new_cnt$4047;
    moonbit_incref(_field$3726);
    _new_cnt$4047 = _cnt$4045 - 1;
    Moonbit_object_header(func_attrs_tuple$1428)->rc = _new_cnt$4047;
  } else if (_cnt$4045 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$4046 =
      func_attrs_tuple$1428->$0;
    moonbit_decref(_field$4046);
    moonbit_free(func_attrs_tuple$1428);
  }
  _tmp$3458 = _field$3726;
  _tmp$3457
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$3457)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$3457->$0 = file_filter$1430;
  _tmp$3457->$1 = index_filter$1431;
  _tmp$3457->$2 = _tmp$3458;
  k$1429
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1429)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1429->$0 = F0$3456;
  k$1429->$1 = _tmp$3457;
  return k$1429;
  $joinlet$4190:;
  moonbit_incref(file_filter$1430);
  _bind$1445
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$1446, file_filter$1430
  );
  if (_bind$1445 == 0) {
    if (_bind$1445) {
      moonbit_decref(_bind$1445);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1447 =
      _bind$1445;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1448 =
      _Some$1447;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1450;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1451;
    moonbit_incref(_index_func_map$1448);
    _bind$1451
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$1448, index_filter$1431
    );
    if (_bind$1451 == 0) {
      if (_bind$1451) {
        moonbit_decref(_bind$1451);
      }
      moonbit_decref(_index_func_map$1448);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1452;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1453;
      moonbit_decref(async_tests$1459);
      _Some$1452 = _bind$1451;
      _func_attrs_tuple$1453 = _Some$1452;
      func_attrs_tuple$1450 = _func_attrs_tuple$1453;
      goto $join$1449;
    }
    goto $joinlet$4193;
    $join$1449:;
    index_func_map$1442 = _index_func_map$1448;
    func_attrs_tuple$1443 = func_attrs_tuple$1450;
    goto $join$1441;
    $joinlet$4193:;
  }
  goto $joinlet$4192;
  $join$1441:;
  moonbit_decref(index_func_map$1442);
  _field$3725 = func_attrs_tuple$1443->$0;
  _tmp$3463 = _field$3725;
  moonbit_incref(_tmp$3463);
  F1$3460
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$3460)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$3460)->$0 = _tmp$3463;
  _field$3724 = func_attrs_tuple$1443->$1;
  _cnt$4048 = Moonbit_object_header(func_attrs_tuple$1443)->rc;
  if (_cnt$4048 > 1) {
    int32_t _new_cnt$4050;
    moonbit_incref(_field$3724);
    _new_cnt$4050 = _cnt$4048 - 1;
    Moonbit_object_header(func_attrs_tuple$1443)->rc = _new_cnt$4050;
  } else if (_cnt$4048 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$4049 =
      func_attrs_tuple$1443->$0;
    moonbit_decref(_field$4049);
    moonbit_free(func_attrs_tuple$1443);
  }
  _tmp$3462 = _field$3724;
  _tmp$3461
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$3461)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$3461->$0 = file_filter$1430;
  _tmp$3461->$1 = index_filter$1431;
  _tmp$3461->$2 = _tmp$3462;
  k$1444
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1444)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1444->$0 = F1$3460;
  k$1444->$1 = _tmp$3461;
  return k$1444;
  $joinlet$4192:;
  moonbit_incref(file_filter$1430);
  _bind$1458
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$1459, file_filter$1430
  );
  if (_bind$1458 == 0) {
    if (_bind$1458) {
      moonbit_decref(_bind$1458);
    }
    moonbit_decref(file_filter$1430);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1460 =
      _bind$1458;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1461 =
      _Some$1460;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1463;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1464;
    moonbit_incref(_index_func_map$1461);
    _bind$1464
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$1461, index_filter$1431
    );
    if (_bind$1464 == 0) {
      if (_bind$1464) {
        moonbit_decref(_bind$1464);
      }
      moonbit_decref(_index_func_map$1461);
      moonbit_decref(file_filter$1430);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1465 =
        _bind$1464;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1466 =
        _Some$1465;
      func_attrs_tuple$1463 = _func_attrs_tuple$1466;
      goto $join$1462;
    }
    goto $joinlet$4195;
    $join$1462:;
    index_func_map$1455 = _index_func_map$1461;
    func_attrs_tuple$1456 = func_attrs_tuple$1463;
    goto $join$1454;
    $joinlet$4195:;
  }
  goto $joinlet$4194;
  $join$1454:;
  moonbit_decref(index_func_map$1455);
  _field$3723 = func_attrs_tuple$1456->$0;
  _tmp$3467 = _field$3723;
  moonbit_incref(_tmp$3467);
  F2$3464
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$3464)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$3464)->$0 = _tmp$3467;
  _field$3722 = func_attrs_tuple$1456->$1;
  _cnt$4051 = Moonbit_object_header(func_attrs_tuple$1456)->rc;
  if (_cnt$4051 > 1) {
    int32_t _new_cnt$4053;
    moonbit_incref(_field$3722);
    _new_cnt$4053 = _cnt$4051 - 1;
    Moonbit_object_header(func_attrs_tuple$1456)->rc = _new_cnt$4053;
  } else if (_cnt$4051 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$4052 =
      func_attrs_tuple$1456->$0;
    moonbit_decref(_field$4052);
    moonbit_free(func_attrs_tuple$1456);
  }
  _tmp$3466 = _field$3722;
  _tmp$3465
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$3465)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$3465->$0 = file_filter$1430;
  _tmp$3465->$1 = index_filter$1431;
  _tmp$3465->$2 = _tmp$3466;
  k$1457
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1457)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1457->$0 = F2$3464;
  k$1457->$1 = _tmp$3465;
  return k$1457;
  $joinlet$4194:;
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4(
  
) {
  struct $Resource* resource$1414 =
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$new();
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* _field$3731 =
    resource$1414->$0;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* attributes$3434 =
    _field$3731;
  int32_t _tmp$3432;
  moonbit_string_t _tmp$3433;
  struct moonbit_result_0 _tmp$4196;
  void* StringValue$3455;
  struct $$3c$String$2a$AttributeValue$3e$* _tuple$3452;
  void* StringValue$3454;
  struct $$3c$String$2a$AttributeValue$3e$* _tuple$3453;
  struct $$3c$String$2a$AttributeValue$3e$** _tmp$3451;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* attrs$1415;
  struct $Resource* resource_with_attrs$1416;
  void* service_name$1417;
  void* service_version$1418;
  void* missing_attr$1419;
  int32_t _tmp$3730;
  int32_t _tmp$3729;
  int32_t _tmp$3728;
  moonbit_incref(attributes$3434);
  _tmp$3432 = $$moonbitlang$core$builtin$Array$$length$4(attributes$3434);
  _tmp$3433 = 0;
  _tmp$4196
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3432, 0, _tmp$3433, (moonbit_string_t)moonbit_string_literal_14.data
  );
  if (_tmp$4196.tag) {
    int32_t const _ok$3435 = _tmp$4196.data.ok;
  } else {
    void* const _err$3436 = _tmp$4196.data.err;
    struct moonbit_result_0 _result$4197;
    moonbit_decref(resource$1414);
    _result$4197.tag = 0;
    _result$4197.data.err = _err$3436;
    return _result$4197;
  }
  StringValue$3455
  = (void*)moonbit_malloc(sizeof(struct $AttributeValue$StringValue));
  Moonbit_object_header(StringValue$3455)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $AttributeValue$StringValue, $0) >> 2, 1, 0
  );
  ((struct $AttributeValue$StringValue*)StringValue$3455)->$0
  = (moonbit_string_t)moonbit_string_literal_15.data;
  _tuple$3452
  = (struct $$3c$String$2a$AttributeValue$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$AttributeValue$3e$)
    );
  Moonbit_object_header(_tuple$3452)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$AttributeValue$3e$, $0) >> 2, 2, 0
  );
  _tuple$3452->$0 = (moonbit_string_t)moonbit_string_literal_16.data;
  _tuple$3452->$1 = StringValue$3455;
  StringValue$3454
  = (void*)moonbit_malloc(sizeof(struct $AttributeValue$StringValue));
  Moonbit_object_header(StringValue$3454)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $AttributeValue$StringValue, $0) >> 2, 1, 0
  );
  ((struct $AttributeValue$StringValue*)StringValue$3454)->$0
  = (moonbit_string_t)moonbit_string_literal_17.data;
  _tuple$3453
  = (struct $$3c$String$2a$AttributeValue$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$AttributeValue$3e$)
    );
  Moonbit_object_header(_tuple$3453)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$AttributeValue$3e$, $0) >> 2, 2, 0
  );
  _tuple$3453->$0 = (moonbit_string_t)moonbit_string_literal_18.data;
  _tuple$3453->$1 = StringValue$3454;
  _tmp$3451
  = (struct $$3c$String$2a$AttributeValue$3e$**)moonbit_make_ref_array_raw(2);
  _tmp$3451[0] = _tuple$3452;
  _tmp$3451[1] = _tuple$3453;
  attrs$1415
  = (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$
      )
    );
  Moonbit_object_header(attrs$1415)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  attrs$1415->$0 = _tmp$3451;
  attrs$1415->$1 = 2;
  resource_with_attrs$1416
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$with_attributes(
    resource$1414, attrs$1415
  );
  moonbit_incref(resource_with_attrs$1416);
  service_name$1417
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$get_attribute(
    resource_with_attrs$1416,
      (moonbit_string_t)moonbit_string_literal_16.data
  );
  moonbit_incref(resource_with_attrs$1416);
  service_version$1418
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$get_attribute(
    resource_with_attrs$1416,
      (moonbit_string_t)moonbit_string_literal_18.data
  );
  missing_attr$1419
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$get_attribute(
    resource_with_attrs$1416,
      (moonbit_string_t)moonbit_string_literal_19.data
  );
  _tmp$3730 = service_name$1417 == 0;
  if (service_name$1417) {
    moonbit_decref(service_name$1417);
  }
  if (_tmp$3730) {
    moonbit_string_t _tmp$3437 = 0;
    struct moonbit_result_0 _tmp$4198 =
      $moonbitlang$core$builtin$assert_false(
        1, _tmp$3437, (moonbit_string_t)moonbit_string_literal_20.data
      );
    if (_tmp$4198.tag) {
      int32_t const _ok$3438 = _tmp$4198.data.ok;
    } else {
      void* const _err$3439 = _tmp$4198.data.err;
      struct moonbit_result_0 _result$4199;
      if (missing_attr$1419) {
        moonbit_decref(missing_attr$1419);
      }
      if (service_version$1418) {
        moonbit_decref(service_version$1418);
      }
      _result$4199.tag = 0;
      _result$4199.data.err = _err$3439;
      return _result$4199;
    }
  } else {
    moonbit_string_t _tmp$3440 = 0;
    struct moonbit_result_0 _tmp$4200 =
      $moonbitlang$core$builtin$assert_true(
        1, _tmp$3440, (moonbit_string_t)moonbit_string_literal_21.data
      );
    if (_tmp$4200.tag) {
      int32_t const _ok$3441 = _tmp$4200.data.ok;
    } else {
      void* const _err$3442 = _tmp$4200.data.err;
      struct moonbit_result_0 _result$4201;
      if (missing_attr$1419) {
        moonbit_decref(missing_attr$1419);
      }
      if (service_version$1418) {
        moonbit_decref(service_version$1418);
      }
      _result$4201.tag = 0;
      _result$4201.data.err = _err$3442;
      return _result$4201;
    }
  }
  _tmp$3729 = service_version$1418 == 0;
  if (service_version$1418) {
    moonbit_decref(service_version$1418);
  }
  if (_tmp$3729) {
    moonbit_string_t _tmp$3443 = 0;
    struct moonbit_result_0 _tmp$4202 =
      $moonbitlang$core$builtin$assert_false(
        1, _tmp$3443, (moonbit_string_t)moonbit_string_literal_22.data
      );
    if (_tmp$4202.tag) {
      int32_t const _ok$3444 = _tmp$4202.data.ok;
    } else {
      void* const _err$3445 = _tmp$4202.data.err;
      struct moonbit_result_0 _result$4203;
      if (missing_attr$1419) {
        moonbit_decref(missing_attr$1419);
      }
      _result$4203.tag = 0;
      _result$4203.data.err = _err$3445;
      return _result$4203;
    }
  } else {
    moonbit_string_t _tmp$3446 = 0;
    struct moonbit_result_0 _tmp$4204 =
      $moonbitlang$core$builtin$assert_true(
        1, _tmp$3446, (moonbit_string_t)moonbit_string_literal_23.data
      );
    if (_tmp$4204.tag) {
      int32_t const _ok$3447 = _tmp$4204.data.ok;
    } else {
      void* const _err$3448 = _tmp$4204.data.err;
      struct moonbit_result_0 _result$4205;
      if (missing_attr$1419) {
        moonbit_decref(missing_attr$1419);
      }
      _result$4205.tag = 0;
      _result$4205.data.err = _err$3448;
      return _result$4205;
    }
  }
  _tmp$3728 = missing_attr$1419 == 0;
  if (missing_attr$1419) {
    moonbit_decref(missing_attr$1419);
  }
  if (_tmp$3728) {
    moonbit_string_t _tmp$3449 = 0;
    return $moonbitlang$core$builtin$assert_true(
             1, _tmp$3449, (moonbit_string_t)moonbit_string_literal_24.data
           );
  } else {
    moonbit_string_t _tmp$3450 = 0;
    return $moonbitlang$core$builtin$assert_false(
             1, _tmp$3450, (moonbit_string_t)moonbit_string_literal_25.data
           );
  }
}

struct $Resource* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$with_attributes(
  struct $Resource* resource$1413,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* attributes$1412
) {
  struct $Resource* _block$4206;
  moonbit_decref(resource$1413);
  _block$4206 = (struct $Resource*)moonbit_malloc(sizeof(struct $Resource));
  Moonbit_object_header(_block$4206)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Resource, $0) >> 2, 1, 0
  );
  _block$4206->$0 = attributes$1412;
  return _block$4206;
}

struct $Resource* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$new(
  
) {
  struct $$3c$String$2a$AttributeValue$3e$** _tmp$3431 =
    (struct $$3c$String$2a$AttributeValue$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* _tmp$3430 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$
      )
    );
  struct $Resource* _block$4207;
  Moonbit_object_header(_tmp$3430)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  _tmp$3430->$0 = _tmp$3431;
  _tmp$3430->$1 = 0;
  _block$4207 = (struct $Resource*)moonbit_malloc(sizeof(struct $Resource));
  Moonbit_object_header(_block$4207)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Resource, $0) >> 2, 1, 0
  );
  _block$4207->$0 = _tmp$3430;
  return _block$4207;
}

void* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Resource$$get_attribute(
  struct $Resource* resource$1406,
  moonbit_string_t key$1410
) {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* _field$3735 =
    resource$1406->$0;
  int32_t _cnt$4054 = Moonbit_object_header(resource$1406)->rc;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* _arr$1405;
  int32_t _len$1407;
  int32_t _i$1408;
  if (_cnt$4054 > 1) {
    int32_t _new_cnt$4055;
    moonbit_incref(_field$3735);
    _new_cnt$4055 = _cnt$4054 - 1;
    Moonbit_object_header(resource$1406)->rc = _new_cnt$4055;
  } else if (_cnt$4054 == 1) {
    moonbit_free(resource$1406);
  }
  _arr$1405 = _field$3735;
  moonbit_incref(_arr$1405);
  _len$1407 = $$moonbitlang$core$builtin$Array$$length$4(_arr$1405);
  _i$1408 = 0;
  while (1) {
    if (_i$1408 < _len$1407) {
      struct $$3c$String$2a$AttributeValue$3e$* attr$1409;
      moonbit_string_t _field$3734;
      moonbit_string_t _tmp$3427;
      int32_t _tmp$3733;
      int32_t _tmp$3429;
      moonbit_incref(_arr$1405);
      attr$1409
      = $$moonbitlang$core$builtin$Array$$unsafe_get$4(
        _arr$1405, _i$1408
      );
      _field$3734 = attr$1409->$0;
      _tmp$3427 = _field$3734;
      _tmp$3733 = moonbit_val_array_equal(_tmp$3427, key$1410);
      if (_tmp$3733) {
        void* _field$3732;
        void* _tmp$3428;
        moonbit_decref(key$1410);
        moonbit_decref(_arr$1405);
        _field$3732 = attr$1409->$1;
        moonbit_incref(_field$3732);
        moonbit_decref(attr$1409);
        _tmp$3428 = _field$3732;
        return _tmp$3428;
      } else {
        moonbit_decref(attr$1409);
      }
      _tmp$3429 = _i$1408 + 1;
      _i$1408 = _tmp$3429;
      continue;
    } else {
      moonbit_decref(key$1410);
      moonbit_decref(_arr$1405);
      return 0;
    }
    break;
  }
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3(
  
) {
  struct $Context* ctx$1400 =
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$root();
  struct $ContextKey$3c$String$3e$* key$1401 =
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$ContextKey$$new(
      (moonbit_string_t)moonbit_string_literal_26.data
    );
  struct $Context* ctx_with_value$1402;
  moonbit_string_t retrieved_value$1403;
  moonbit_string_t _tmp$3420;
  moonbit_string_t _tmp$3421;
  struct moonbit_result_0 _tmp$4209;
  struct $ContextKey$3c$String$3e$* _tmp$3426;
  moonbit_string_t missing_value$1404;
  moonbit_string_t _tmp$3424;
  moonbit_string_t _tmp$3425;
  moonbit_incref(key$1401);
  moonbit_incref(ctx$1400);
  ctx_with_value$1402
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$with_value(
    ctx$1400, key$1401, (moonbit_string_t)moonbit_string_literal_27.data
  );
  retrieved_value$1403
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$get(
    ctx_with_value$1402, key$1401
  );
  _tmp$3420 = (moonbit_string_t)moonbit_string_literal_27.data;
  _tmp$3421 = 0;
  _tmp$4209
  = $moonbitlang$core$builtin$assert_eq$6(
    retrieved_value$1403,
      _tmp$3420,
      _tmp$3421,
      (moonbit_string_t)moonbit_string_literal_28.data
  );
  if (_tmp$4209.tag) {
    int32_t const _ok$3422 = _tmp$4209.data.ok;
  } else {
    void* const _err$3423 = _tmp$4209.data.err;
    struct moonbit_result_0 _result$4210;
    moonbit_decref(ctx$1400);
    _result$4210.tag = 0;
    _result$4210.data.err = _err$3423;
    return _result$4210;
  }
  _tmp$3426
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$ContextKey$$new(
    (moonbit_string_t)moonbit_string_literal_29.data
  );
  missing_value$1404
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$get(
    ctx$1400, _tmp$3426
  );
  _tmp$3424 = 0;
  _tmp$3425 = 0;
  return $moonbitlang$core$builtin$assert_eq$6(
           missing_value$1404,
             _tmp$3424,
             _tmp$3425,
             (moonbit_string_t)moonbit_string_literal_30.data
         );
}

struct $ContextKey$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$ContextKey$$new(
  moonbit_string_t key$1399
) {
  struct $ContextKey$3c$String$3e$* _block$4211 =
    (struct $ContextKey$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $ContextKey$3c$String$3e$)
    );
  Moonbit_object_header(_block$4211)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $ContextKey$3c$String$3e$, $0) >> 2, 1, 0
  );
  _block$4211->$0 = key$1399;
  return _block$4211;
}

struct $Context* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$with_value(
  struct $Context* ctx$1398,
  struct $ContextKey$3c$String$3e$* key$1396,
  moonbit_string_t value$1397
) {
  moonbit_string_t _field$3736;
  int32_t _cnt$4056;
  moonbit_string_t key$3419;
  struct $$3c$String$2a$String$3e$* _tuple$3418;
  struct $$3c$String$2a$String$3e$* _tmp$3417;
  struct $Context* _block$4212;
  moonbit_decref(ctx$1398);
  _field$3736 = key$1396->$0;
  _cnt$4056 = Moonbit_object_header(key$1396)->rc;
  if (_cnt$4056 > 1) {
    int32_t _new_cnt$4057;
    moonbit_incref(_field$3736);
    _new_cnt$4057 = _cnt$4056 - 1;
    Moonbit_object_header(key$1396)->rc = _new_cnt$4057;
  } else if (_cnt$4056 == 1) {
    moonbit_free(key$1396);
  }
  key$3419 = _field$3736;
  _tuple$3418
  = (struct $$3c$String$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$String$3e$)
    );
  Moonbit_object_header(_tuple$3418)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$String$3e$, $0) >> 2, 2, 0
  );
  _tuple$3418->$0 = key$3419;
  _tuple$3418->$1 = value$1397;
  _tmp$3417 = _tuple$3418;
  _block$4212 = (struct $Context*)moonbit_malloc(sizeof(struct $Context));
  Moonbit_object_header(_block$4212)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Context, $0) >> 2, 1, 0
  );
  _block$4212->$0 = _tmp$3417;
  return _block$4212;
}

struct $Context* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$root(
  
) {
  struct $$3c$String$2a$String$3e$* _tmp$3416 = 0;
  struct $Context* _block$4213 =
    (struct $Context*)moonbit_malloc(sizeof(struct $Context));
  Moonbit_object_header(_block$4213)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Context, $0) >> 2, 1, 0
  );
  _block$4213->$0 = _tmp$3416;
  return _block$4213;
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Context$$get(
  struct $Context* ctx$1390,
  struct $ContextKey$3c$String$3e$* key$1395
) {
  moonbit_string_t k$1387;
  moonbit_string_t v$1388;
  struct $$3c$String$2a$String$3e$* _field$3741 = ctx$1390->$0;
  int32_t _cnt$4058 = Moonbit_object_header(ctx$1390)->rc;
  struct $$3c$String$2a$String$3e$* _bind$1389;
  if (_cnt$4058 > 1) {
    int32_t _new_cnt$4059;
    if (_field$3741) {
      moonbit_incref(_field$3741);
    }
    _new_cnt$4059 = _cnt$4058 - 1;
    Moonbit_object_header(ctx$1390)->rc = _new_cnt$4059;
  } else if (_cnt$4058 == 1) {
    moonbit_free(ctx$1390);
  }
  _bind$1389 = _field$3741;
  if (_bind$1389 == 0) {
    moonbit_decref(key$1395);
    if (_bind$1389) {
      moonbit_decref(_bind$1389);
    }
    goto $join$1385;
  } else {
    struct $$3c$String$2a$String$3e$* _Some$1391 = _bind$1389;
    struct $$3c$String$2a$String$3e$* _x$1392 = _Some$1391;
    moonbit_string_t _field$3740 = _x$1392->$0;
    moonbit_string_t _k$1393 = _field$3740;
    moonbit_string_t _field$3739 = _x$1392->$1;
    int32_t _cnt$4060 = Moonbit_object_header(_x$1392)->rc;
    moonbit_string_t _v$1394;
    moonbit_string_t _field$3738;
    int32_t _cnt$4062;
    moonbit_string_t key$3415;
    int32_t _tmp$3737;
    if (_cnt$4060 > 1) {
      int32_t _new_cnt$4061;
      moonbit_incref(_field$3739);
      moonbit_incref(_k$1393);
      _new_cnt$4061 = _cnt$4060 - 1;
      Moonbit_object_header(_x$1392)->rc = _new_cnt$4061;
    } else if (_cnt$4060 == 1) {
      moonbit_free(_x$1392);
    }
    _v$1394 = _field$3739;
    _field$3738 = key$1395->$0;
    _cnt$4062 = Moonbit_object_header(key$1395)->rc;
    if (_cnt$4062 > 1) {
      int32_t _new_cnt$4063;
      moonbit_incref(_field$3738);
      _new_cnt$4063 = _cnt$4062 - 1;
      Moonbit_object_header(key$1395)->rc = _new_cnt$4063;
    } else if (_cnt$4062 == 1) {
      moonbit_free(key$1395);
    }
    key$3415 = _field$3738;
    _tmp$3737 = moonbit_val_array_equal(_k$1393, key$3415);
    moonbit_decref(key$3415);
    if (_tmp$3737) {
      k$1387 = _k$1393;
      v$1388 = _v$1394;
      goto $join$1386;
    } else {
      moonbit_decref(_v$1394);
      moonbit_decref(_k$1393);
      goto $join$1385;
    }
  }
  $join$1386:;
  moonbit_decref(k$1387);
  return v$1388;
  $join$1385:;
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2(
  
) {
  moonbit_string_t trace_id$1381 =
    (moonbit_string_t)moonbit_string_literal_31.data;
  moonbit_string_t span_id$1382 =
    (moonbit_string_t)moonbit_string_literal_32.data;
  struct $SpanContext* span_ctx$1383;
  moonbit_string_t _tmp$3397;
  moonbit_string_t _tmp$3398;
  struct moonbit_result_0 _tmp$4216;
  moonbit_string_t _tmp$3401;
  moonbit_string_t _tmp$3402;
  struct moonbit_result_0 _tmp$4218;
  int32_t _tmp$3405;
  moonbit_string_t _tmp$3406;
  struct moonbit_result_0 _tmp$4220;
  int32_t _tmp$3409;
  moonbit_string_t _tmp$3410;
  struct moonbit_result_0 _tmp$4222;
  struct $SpanContext* invalid_ctx$1384;
  int32_t _tmp$3413;
  moonbit_string_t _tmp$3414;
  moonbit_incref(span_id$1382);
  moonbit_incref(trace_id$1381);
  span_ctx$1383
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$new(
    trace_id$1381,
      span_id$1382,
      1,
      (moonbit_string_t)moonbit_string_literal_33.data
  );
  moonbit_incref(span_ctx$1383);
  _tmp$3397
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$trace_id(
    span_ctx$1383
  );
  _tmp$3398 = 0;
  _tmp$4216
  = $moonbitlang$core$builtin$assert_eq$3(
    _tmp$3397,
      trace_id$1381,
      _tmp$3398,
      (moonbit_string_t)moonbit_string_literal_34.data
  );
  if (_tmp$4216.tag) {
    int32_t const _ok$3399 = _tmp$4216.data.ok;
  } else {
    void* const _err$3400 = _tmp$4216.data.err;
    struct moonbit_result_0 _result$4217;
    moonbit_decref(span_ctx$1383);
    moonbit_decref(span_id$1382);
    _result$4217.tag = 0;
    _result$4217.data.err = _err$3400;
    return _result$4217;
  }
  moonbit_incref(span_ctx$1383);
  _tmp$3401
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$span_id(
    span_ctx$1383
  );
  _tmp$3402 = 0;
  _tmp$4218
  = $moonbitlang$core$builtin$assert_eq$3(
    _tmp$3401,
      span_id$1382,
      _tmp$3402,
      (moonbit_string_t)moonbit_string_literal_35.data
  );
  if (_tmp$4218.tag) {
    int32_t const _ok$3403 = _tmp$4218.data.ok;
  } else {
    void* const _err$3404 = _tmp$4218.data.err;
    struct moonbit_result_0 _result$4219;
    moonbit_decref(span_ctx$1383);
    _result$4219.tag = 0;
    _result$4219.data.err = _err$3404;
    return _result$4219;
  }
  moonbit_incref(span_ctx$1383);
  _tmp$3405
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$is_sampled(
    span_ctx$1383
  );
  _tmp$3406 = 0;
  _tmp$4220
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3405, _tmp$3406, (moonbit_string_t)moonbit_string_literal_36.data
  );
  if (_tmp$4220.tag) {
    int32_t const _ok$3407 = _tmp$4220.data.ok;
  } else {
    void* const _err$3408 = _tmp$4220.data.err;
    struct moonbit_result_0 _result$4221;
    moonbit_decref(span_ctx$1383);
    _result$4221.tag = 0;
    _result$4221.data.err = _err$3408;
    return _result$4221;
  }
  _tmp$3409
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$is_valid(
    span_ctx$1383
  );
  _tmp$3410 = 0;
  _tmp$4222
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3409, _tmp$3410, (moonbit_string_t)moonbit_string_literal_37.data
  );
  if (_tmp$4222.tag) {
    int32_t const _ok$3411 = _tmp$4222.data.ok;
  } else {
    void* const _err$3412 = _tmp$4222.data.err;
    struct moonbit_result_0 _result$4223;
    _result$4223.tag = 0;
    _result$4223.data.err = _err$3412;
    return _result$4223;
  }
  invalid_ctx$1384
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$new(
    (moonbit_string_t)moonbit_string_literal_3.data,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0,
      (moonbit_string_t)moonbit_string_literal_3.data
  );
  _tmp$3413
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$is_valid(
    invalid_ctx$1384
  );
  _tmp$3414 = 0;
  return $moonbitlang$core$builtin$assert_false(
           _tmp$3413,
             _tmp$3414,
             (moonbit_string_t)moonbit_string_literal_38.data
         );
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$trace_id(
  struct $SpanContext* ctx$1380
) {
  moonbit_string_t _field$3742 = ctx$1380->$0;
  int32_t _cnt$4064 = Moonbit_object_header(ctx$1380)->rc;
  if (_cnt$4064 > 1) {
    int32_t _new_cnt$4067;
    moonbit_incref(_field$3742);
    _new_cnt$4067 = _cnt$4064 - 1;
    Moonbit_object_header(ctx$1380)->rc = _new_cnt$4067;
  } else if (_cnt$4064 == 1) {
    moonbit_string_t _field$4066 = ctx$1380->$3;
    moonbit_string_t _field$4065;
    moonbit_decref(_field$4066);
    _field$4065 = ctx$1380->$1;
    moonbit_decref(_field$4065);
    moonbit_free(ctx$1380);
  }
  return _field$3742;
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$span_id(
  struct $SpanContext* ctx$1379
) {
  moonbit_string_t _field$3743 = ctx$1379->$1;
  int32_t _cnt$4068 = Moonbit_object_header(ctx$1379)->rc;
  if (_cnt$4068 > 1) {
    int32_t _new_cnt$4071;
    moonbit_incref(_field$3743);
    _new_cnt$4071 = _cnt$4068 - 1;
    Moonbit_object_header(ctx$1379)->rc = _new_cnt$4071;
  } else if (_cnt$4068 == 1) {
    moonbit_string_t _field$4070 = ctx$1379->$3;
    moonbit_string_t _field$4069;
    moonbit_decref(_field$4070);
    _field$4069 = ctx$1379->$0;
    moonbit_decref(_field$4069);
    moonbit_free(ctx$1379);
  }
  return _field$3743;
}

struct $SpanContext* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$new(
  moonbit_string_t trace_id$1375,
  moonbit_string_t span_id$1376,
  int32_t sampled$1377,
  moonbit_string_t trace_state$1378
) {
  struct $SpanContext* _block$4224 =
    (struct $SpanContext*)moonbit_malloc(sizeof(struct $SpanContext));
  Moonbit_object_header(_block$4224)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $SpanContext, $0) >> 2, 3, 0
  );
  _block$4224->$0 = trace_id$1375;
  _block$4224->$1 = span_id$1376;
  _block$4224->$2 = sampled$1377;
  _block$4224->$3 = trace_state$1378;
  return _block$4224;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$is_valid(
  struct $SpanContext* ctx$1374
) {
  moonbit_string_t _field$3745 = ctx$1374->$0;
  moonbit_string_t trace_id$3396 = _field$3745;
  moonbit_incref(trace_id$3396);
  if (
    $moonbitlang$core$builtin$op_notequal$4(
      trace_id$3396, (moonbit_string_t)moonbit_string_literal_3.data
    )
  ) {
    moonbit_string_t _field$3744 = ctx$1374->$1;
    int32_t _cnt$4072 = Moonbit_object_header(ctx$1374)->rc;
    moonbit_string_t span_id$3395;
    if (_cnt$4072 > 1) {
      int32_t _new_cnt$4075;
      moonbit_incref(_field$3744);
      _new_cnt$4075 = _cnt$4072 - 1;
      Moonbit_object_header(ctx$1374)->rc = _new_cnt$4075;
    } else if (_cnt$4072 == 1) {
      moonbit_string_t _field$4074 = ctx$1374->$3;
      moonbit_string_t _field$4073;
      moonbit_decref(_field$4074);
      _field$4073 = ctx$1374->$0;
      moonbit_decref(_field$4073);
      moonbit_free(ctx$1374);
    }
    span_id$3395 = _field$3744;
    return $moonbitlang$core$builtin$op_notequal$4(
             span_id$3395, (moonbit_string_t)moonbit_string_literal_3.data
           );
  } else {
    moonbit_decref(ctx$1374);
    return 0;
  }
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$SpanContext$$is_sampled(
  struct $SpanContext* ctx$1373
) {
  int32_t _field$3746 = ctx$1373->$2;
  moonbit_decref(ctx$1373);
  return _field$3746;
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1(
  
) {
  struct $Baggage* baggage$1367 =
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$new();
  struct $Baggage* updated_baggage$1368 =
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$set_entry(
      baggage$1367,
        (moonbit_string_t)moonbit_string_literal_39.data,
        (moonbit_string_t)moonbit_string_literal_40.data
    );
  struct $Baggage* final_baggage$1369 =
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$set_entry(
      updated_baggage$1368,
        (moonbit_string_t)moonbit_string_literal_41.data,
        (moonbit_string_t)moonbit_string_literal_42.data
    );
  moonbit_string_t user_id$1370;
  moonbit_string_t request_id$1371;
  moonbit_string_t missing_entry$1372;
  moonbit_string_t _tmp$3385;
  moonbit_string_t _tmp$3386;
  struct moonbit_result_0 _tmp$4225;
  moonbit_string_t _tmp$3389;
  moonbit_string_t _tmp$3390;
  struct moonbit_result_0 _tmp$4227;
  moonbit_string_t _tmp$3393;
  moonbit_string_t _tmp$3394;
  moonbit_incref(final_baggage$1369);
  user_id$1370
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$get_entry(
    final_baggage$1369, (moonbit_string_t)moonbit_string_literal_39.data
  );
  moonbit_incref(final_baggage$1369);
  request_id$1371
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$get_entry(
    final_baggage$1369, (moonbit_string_t)moonbit_string_literal_41.data
  );
  missing_entry$1372
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$get_entry(
    final_baggage$1369, (moonbit_string_t)moonbit_string_literal_29.data
  );
  _tmp$3385 = 0;
  _tmp$3386 = 0;
  _tmp$4225
  = $moonbitlang$core$builtin$assert_eq$6(
    user_id$1370,
      _tmp$3385,
      _tmp$3386,
      (moonbit_string_t)moonbit_string_literal_43.data
  );
  if (_tmp$4225.tag) {
    int32_t const _ok$3387 = _tmp$4225.data.ok;
  } else {
    void* const _err$3388 = _tmp$4225.data.err;
    struct moonbit_result_0 _result$4226;
    if (missing_entry$1372) {
      moonbit_decref(missing_entry$1372);
    }
    if (request_id$1371) {
      moonbit_decref(request_id$1371);
    }
    _result$4226.tag = 0;
    _result$4226.data.err = _err$3388;
    return _result$4226;
  }
  _tmp$3389 = 0;
  _tmp$3390 = 0;
  _tmp$4227
  = $moonbitlang$core$builtin$assert_eq$6(
    request_id$1371,
      _tmp$3389,
      _tmp$3390,
      (moonbit_string_t)moonbit_string_literal_44.data
  );
  if (_tmp$4227.tag) {
    int32_t const _ok$3391 = _tmp$4227.data.ok;
  } else {
    void* const _err$3392 = _tmp$4227.data.err;
    struct moonbit_result_0 _result$4228;
    if (missing_entry$1372) {
      moonbit_decref(missing_entry$1372);
    }
    _result$4228.tag = 0;
    _result$4228.data.err = _err$3392;
    return _result$4228;
  }
  _tmp$3393 = 0;
  _tmp$3394 = 0;
  return $moonbitlang$core$builtin$assert_eq$6(
           missing_entry$1372,
             _tmp$3393,
             _tmp$3394,
             (moonbit_string_t)moonbit_string_literal_45.data
         );
}

struct $Baggage* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$set_entry(
  struct $Baggage* baggage$1364,
  moonbit_string_t key$1365,
  moonbit_string_t value$1366
) {
  moonbit_decref(value$1366);
  moonbit_decref(key$1365);
  return baggage$1364;
}

struct $Baggage* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$new(
  
) {
  struct $$3c$String$2a$String$3e$** _tmp$3384 =
    (struct $$3c$String$2a$String$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _tmp$3383 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$
      )
    );
  struct $Baggage* _block$4229;
  Moonbit_object_header(_tmp$3383)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  _tmp$3383->$0 = _tmp$3384;
  _tmp$3383->$1 = 0;
  _block$4229 = (struct $Baggage*)moonbit_malloc(sizeof(struct $Baggage));
  Moonbit_object_header(_block$4229)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Baggage, $0) >> 2, 1, 0
  );
  _block$4229->$0 = _tmp$3383;
  return _block$4229;
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Baggage$$get_entry(
  struct $Baggage* baggage$1358,
  moonbit_string_t key$1362
) {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$3750 =
    baggage$1358->$0;
  int32_t _cnt$4076 = Moonbit_object_header(baggage$1358)->rc;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _arr$1357;
  int32_t _len$1359;
  int32_t _i$1360;
  if (_cnt$4076 > 1) {
    int32_t _new_cnt$4077;
    moonbit_incref(_field$3750);
    _new_cnt$4077 = _cnt$4076 - 1;
    Moonbit_object_header(baggage$1358)->rc = _new_cnt$4077;
  } else if (_cnt$4076 == 1) {
    moonbit_free(baggage$1358);
  }
  _arr$1357 = _field$3750;
  moonbit_incref(_arr$1357);
  _len$1359 = $$moonbitlang$core$builtin$Array$$length$5(_arr$1357);
  _i$1360 = 0;
  while (1) {
    if (_i$1360 < _len$1359) {
      struct $$3c$String$2a$String$3e$* entry$1361;
      moonbit_string_t _field$3749;
      moonbit_string_t _tmp$3380;
      int32_t _tmp$3748;
      int32_t _tmp$3382;
      moonbit_incref(_arr$1357);
      entry$1361
      = $$moonbitlang$core$builtin$Array$$unsafe_get$3(
        _arr$1357, _i$1360
      );
      _field$3749 = entry$1361->$0;
      _tmp$3380 = _field$3749;
      _tmp$3748 = moonbit_val_array_equal(_tmp$3380, key$1362);
      if (_tmp$3748) {
        moonbit_string_t _field$3747;
        moonbit_string_t _tmp$3381;
        moonbit_decref(key$1362);
        moonbit_decref(_arr$1357);
        _field$3747 = entry$1361->$1;
        moonbit_incref(_field$3747);
        moonbit_decref(entry$1361);
        _tmp$3381 = _field$3747;
        return _tmp$3381;
      } else {
        moonbit_decref(entry$1361);
      }
      _tmp$3382 = _i$1360 + 1;
      _i$1360 = _tmp$3382;
      continue;
    } else {
      moonbit_decref(key$1362);
      moonbit_decref(_arr$1357);
      return 0;
    }
    break;
  }
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0(
  
) {
  struct $Attributes* attrs$1353 =
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$new();
  void* StringValue$3364 =
    (void*)moonbit_malloc(sizeof(struct $AttributeValue$StringValue));
  void* IntValue$3365;
  void* string_value$1354;
  void* int_value$1355;
  void* missing_value$1356;
  int32_t _tmp$3753;
  int32_t _tmp$3752;
  int32_t _tmp$3751;
  Moonbit_object_header(StringValue$3364)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $AttributeValue$StringValue, $0) >> 2, 1, 0
  );
  ((struct $AttributeValue$StringValue*)StringValue$3364)->$0
  = (moonbit_string_t)moonbit_string_literal_27.data;
  moonbit_incref(attrs$1353);
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$set(
    attrs$1353,
      (moonbit_string_t)moonbit_string_literal_46.data,
      StringValue$3364
  );
  IntValue$3365
  = (void*)moonbit_malloc(sizeof(struct $AttributeValue$IntValue));
  Moonbit_object_header(IntValue$3365)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $AttributeValue$IntValue) >> 2, 0, 1
  );
  ((struct $AttributeValue$IntValue*)IntValue$3365)->$0 = 42;
  moonbit_incref(attrs$1353);
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$set(
    attrs$1353,
      (moonbit_string_t)moonbit_string_literal_47.data,
      IntValue$3365
  );
  moonbit_incref(attrs$1353);
  string_value$1354
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$get(
    attrs$1353, (moonbit_string_t)moonbit_string_literal_46.data
  );
  moonbit_incref(attrs$1353);
  int_value$1355
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$get(
    attrs$1353, (moonbit_string_t)moonbit_string_literal_47.data
  );
  missing_value$1356
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$get(
    attrs$1353, (moonbit_string_t)moonbit_string_literal_29.data
  );
  _tmp$3753 = string_value$1354 == 0;
  if (string_value$1354) {
    moonbit_decref(string_value$1354);
  }
  if (_tmp$3753) {
    moonbit_string_t _tmp$3366 = 0;
    struct moonbit_result_0 _tmp$4231 =
      $moonbitlang$core$builtin$assert_false(
        1, _tmp$3366, (moonbit_string_t)moonbit_string_literal_48.data
      );
    if (_tmp$4231.tag) {
      int32_t const _ok$3367 = _tmp$4231.data.ok;
    } else {
      void* const _err$3368 = _tmp$4231.data.err;
      struct moonbit_result_0 _result$4232;
      if (missing_value$1356) {
        moonbit_decref(missing_value$1356);
      }
      if (int_value$1355) {
        moonbit_decref(int_value$1355);
      }
      _result$4232.tag = 0;
      _result$4232.data.err = _err$3368;
      return _result$4232;
    }
  } else {
    moonbit_string_t _tmp$3369 = 0;
    struct moonbit_result_0 _tmp$4233 =
      $moonbitlang$core$builtin$assert_true(
        1, _tmp$3369, (moonbit_string_t)moonbit_string_literal_49.data
      );
    if (_tmp$4233.tag) {
      int32_t const _ok$3370 = _tmp$4233.data.ok;
    } else {
      void* const _err$3371 = _tmp$4233.data.err;
      struct moonbit_result_0 _result$4234;
      if (missing_value$1356) {
        moonbit_decref(missing_value$1356);
      }
      if (int_value$1355) {
        moonbit_decref(int_value$1355);
      }
      _result$4234.tag = 0;
      _result$4234.data.err = _err$3371;
      return _result$4234;
    }
  }
  _tmp$3752 = int_value$1355 == 0;
  if (int_value$1355) {
    moonbit_decref(int_value$1355);
  }
  if (_tmp$3752) {
    moonbit_string_t _tmp$3372 = 0;
    struct moonbit_result_0 _tmp$4235 =
      $moonbitlang$core$builtin$assert_false(
        1, _tmp$3372, (moonbit_string_t)moonbit_string_literal_50.data
      );
    if (_tmp$4235.tag) {
      int32_t const _ok$3373 = _tmp$4235.data.ok;
    } else {
      void* const _err$3374 = _tmp$4235.data.err;
      struct moonbit_result_0 _result$4236;
      if (missing_value$1356) {
        moonbit_decref(missing_value$1356);
      }
      _result$4236.tag = 0;
      _result$4236.data.err = _err$3374;
      return _result$4236;
    }
  } else {
    moonbit_string_t _tmp$3375 = 0;
    struct moonbit_result_0 _tmp$4237 =
      $moonbitlang$core$builtin$assert_true(
        1, _tmp$3375, (moonbit_string_t)moonbit_string_literal_51.data
      );
    if (_tmp$4237.tag) {
      int32_t const _ok$3376 = _tmp$4237.data.ok;
    } else {
      void* const _err$3377 = _tmp$4237.data.err;
      struct moonbit_result_0 _result$4238;
      if (missing_value$1356) {
        moonbit_decref(missing_value$1356);
      }
      _result$4238.tag = 0;
      _result$4238.data.err = _err$3377;
      return _result$4238;
    }
  }
  _tmp$3751 = missing_value$1356 == 0;
  if (missing_value$1356) {
    moonbit_decref(missing_value$1356);
  }
  if (_tmp$3751) {
    moonbit_string_t _tmp$3378 = 0;
    return $moonbitlang$core$builtin$assert_true(
             1, _tmp$3378, (moonbit_string_t)moonbit_string_literal_52.data
           );
  } else {
    moonbit_string_t _tmp$3379 = 0;
    return $moonbitlang$core$builtin$assert_false(
             1, _tmp$3379, (moonbit_string_t)moonbit_string_literal_53.data
           );
  }
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$set(
  struct $Attributes* attrs$1350,
  moonbit_string_t key$1351,
  void* value$1352
) {
  moonbit_decref(value$1352);
  moonbit_decref(key$1351);
  moonbit_decref(attrs$1350);
  return 0;
}

struct $Attributes* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$new(
  
) {
  struct $$3c$String$2a$AttributeValue$3e$** _tmp$3363 =
    (struct $$3c$String$2a$AttributeValue$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* _tmp$3362 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$
      )
    );
  struct $Attributes* _block$4239;
  Moonbit_object_header(_tmp$3362)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  _tmp$3362->$0 = _tmp$3363;
  _tmp$3362->$1 = 0;
  _block$4239
  = (struct $Attributes*)moonbit_malloc(sizeof(struct $Attributes));
  Moonbit_object_header(_block$4239)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Attributes, $0) >> 2, 1, 0
  );
  _block$4239->$0 = _tmp$3362;
  return _block$4239;
}

void* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced$Attributes$$get(
  struct $Attributes* attrs$1349,
  moonbit_string_t key$1348
) {
  moonbit_decref(attrs$1349);
  if (
    moonbit_val_array_equal(
      key$1348, (moonbit_string_t)moonbit_string_literal_46.data
    )
  ) {
    void* StringValue$3360;
    moonbit_decref(key$1348);
    StringValue$3360
    = (void*)moonbit_malloc(sizeof(struct $AttributeValue$StringValue));
    Moonbit_object_header(StringValue$3360)->meta
    = Moonbit_make_regular_object_header(
      offsetof(struct $AttributeValue$StringValue, $0) >> 2, 1, 0
    );
    ((struct $AttributeValue$StringValue*)StringValue$3360)->$0
    = (moonbit_string_t)moonbit_string_literal_27.data;
    return StringValue$3360;
  } else {
    int32_t _tmp$3754 =
      moonbit_val_array_equal(
        key$1348, (moonbit_string_t)moonbit_string_literal_47.data
      );
    moonbit_decref(key$1348);
    if (_tmp$3754) {
      void* IntValue$3361 =
        (void*)moonbit_malloc(sizeof(struct $AttributeValue$IntValue));
      Moonbit_object_header(IntValue$3361)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $AttributeValue$IntValue) >> 2, 0, 1
      );
      ((struct $AttributeValue$IntValue*)IntValue$3361)->$0 = 42;
      return IntValue$3361;
    } else {
      return 0;
    }
  }
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4(
  
) {
  int64_t some_int$1319 = (int64_t)42;
  moonbit_string_t some_string$1320 =
    (moonbit_string_t)moonbit_string_literal_54.data;
  int32_t some_bool$1321 = 1;
  int64_t none_int$1322 = 4294967296ll;
  moonbit_string_t none_string$1323 = 0;
  int32_t none_bool$1324 = -1;
  int64_t _tmp$3314 = (int64_t)42;
  moonbit_string_t _tmp$3315 = 0;
  struct moonbit_result_0 _tmp$4240 =
    $moonbitlang$core$builtin$assert_eq$1(
      some_int$1319,
        _tmp$3314,
        _tmp$3315,
        (moonbit_string_t)moonbit_string_literal_55.data
    );
  moonbit_string_t _tmp$3318;
  moonbit_string_t _tmp$3319;
  struct moonbit_result_0 _tmp$4242;
  int32_t _tmp$3322;
  moonbit_string_t _tmp$3323;
  struct moonbit_result_0 _tmp$4244;
  moonbit_string_t _tmp$3326;
  struct moonbit_result_0 _tmp$4246;
  moonbit_string_t _tmp$3329;
  moonbit_string_t _tmp$3330;
  struct moonbit_result_0 _tmp$4248;
  moonbit_string_t _tmp$3333;
  struct moonbit_result_0 _tmp$4250;
  int32_t _tmp$3336;
  moonbit_string_t _tmp$3337;
  struct moonbit_result_0 _tmp$4252;
  moonbit_string_t _tmp$3342;
  int32_t _tmp$3340;
  moonbit_string_t _tmp$3341;
  struct moonbit_result_0 _tmp$4254;
  int32_t _tmp$3345;
  moonbit_string_t _tmp$3346;
  struct moonbit_result_0 _tmp$4256;
  int32_t x$1327;
  int32_t int_result$1325;
  moonbit_string_t _tmp$3349;
  struct moonbit_result_0 _tmp$4259;
  moonbit_string_t s$1332;
  moonbit_string_t string_result$1330;
  moonbit_string_t _tmp$3352;
  struct moonbit_result_0 _tmp$4262;
  int32_t b$1337;
  int32_t bool_result$1335;
  moonbit_string_t _tmp$3355;
  struct moonbit_result_0 _tmp$4265;
  int64_t _tmp$3359;
  void* nested_option$1340;
  int32_t x$1343;
  int32_t nested_result$1341;
  moonbit_string_t _tmp$3358;
  if (_tmp$4240.tag) {
    int32_t const _ok$3316 = _tmp$4240.data.ok;
  } else {
    void* const _err$3317 = _tmp$4240.data.err;
    struct moonbit_result_0 _result$4241;
    if (none_string$1323) {
      moonbit_decref(none_string$1323);
    }
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4241.tag = 0;
    _result$4241.data.err = _err$3317;
    return _result$4241;
  }
  _tmp$3318 = (moonbit_string_t)moonbit_string_literal_54.data;
  _tmp$3319 = 0;
  if (some_string$1320) {
    moonbit_incref(some_string$1320);
  }
  _tmp$4242
  = $moonbitlang$core$builtin$assert_eq$6(
    some_string$1320,
      _tmp$3318,
      _tmp$3319,
      (moonbit_string_t)moonbit_string_literal_56.data
  );
  if (_tmp$4242.tag) {
    int32_t const _ok$3320 = _tmp$4242.data.ok;
  } else {
    void* const _err$3321 = _tmp$4242.data.err;
    struct moonbit_result_0 _result$4243;
    if (none_string$1323) {
      moonbit_decref(none_string$1323);
    }
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4243.tag = 0;
    _result$4243.data.err = _err$3321;
    return _result$4243;
  }
  _tmp$3322 = 1;
  _tmp$3323 = 0;
  _tmp$4244
  = $moonbitlang$core$builtin$assert_eq$7(
    some_bool$1321,
      _tmp$3322,
      _tmp$3323,
      (moonbit_string_t)moonbit_string_literal_57.data
  );
  if (_tmp$4244.tag) {
    int32_t const _ok$3324 = _tmp$4244.data.ok;
  } else {
    void* const _err$3325 = _tmp$4244.data.err;
    struct moonbit_result_0 _result$4245;
    if (none_string$1323) {
      moonbit_decref(none_string$1323);
    }
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4245.tag = 0;
    _result$4245.data.err = _err$3325;
    return _result$4245;
  }
  _tmp$3326 = 0;
  _tmp$4246
  = $moonbitlang$core$builtin$assert_eq$1(
    none_int$1322,
      4294967296ll,
      _tmp$3326,
      (moonbit_string_t)moonbit_string_literal_58.data
  );
  if (_tmp$4246.tag) {
    int32_t const _ok$3327 = _tmp$4246.data.ok;
  } else {
    void* const _err$3328 = _tmp$4246.data.err;
    struct moonbit_result_0 _result$4247;
    if (none_string$1323) {
      moonbit_decref(none_string$1323);
    }
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4247.tag = 0;
    _result$4247.data.err = _err$3328;
    return _result$4247;
  }
  _tmp$3329 = 0;
  _tmp$3330 = 0;
  _tmp$4248
  = $moonbitlang$core$builtin$assert_eq$6(
    none_string$1323,
      _tmp$3329,
      _tmp$3330,
      (moonbit_string_t)moonbit_string_literal_59.data
  );
  if (_tmp$4248.tag) {
    int32_t const _ok$3331 = _tmp$4248.data.ok;
  } else {
    void* const _err$3332 = _tmp$4248.data.err;
    struct moonbit_result_0 _result$4249;
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4249.tag = 0;
    _result$4249.data.err = _err$3332;
    return _result$4249;
  }
  _tmp$3333 = 0;
  _tmp$4250
  = $moonbitlang$core$builtin$assert_eq$7(
    none_bool$1324,
      -1,
      _tmp$3333,
      (moonbit_string_t)moonbit_string_literal_60.data
  );
  if (_tmp$4250.tag) {
    int32_t const _ok$3334 = _tmp$4250.data.ok;
  } else {
    void* const _err$3335 = _tmp$4250.data.err;
    struct moonbit_result_0 _result$4251;
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4251.tag = 0;
    _result$4251.data.err = _err$3335;
    return _result$4251;
  }
  _tmp$3336
  = $moonbitlang$core$builtin$op_notequal$1(
    some_int$1319, 4294967296ll
  );
  _tmp$3337 = 0;
  _tmp$4252
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3336, _tmp$3337, (moonbit_string_t)moonbit_string_literal_61.data
  );
  if (_tmp$4252.tag) {
    int32_t const _ok$3338 = _tmp$4252.data.ok;
  } else {
    void* const _err$3339 = _tmp$4252.data.err;
    struct moonbit_result_0 _result$4253;
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4253.tag = 0;
    _result$4253.data.err = _err$3339;
    return _result$4253;
  }
  _tmp$3342 = 0;
  if (some_string$1320) {
    moonbit_incref(some_string$1320);
  }
  _tmp$3340
  = $moonbitlang$core$builtin$op_notequal$2(
    some_string$1320, _tmp$3342
  );
  _tmp$3341 = 0;
  _tmp$4254
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3340, _tmp$3341, (moonbit_string_t)moonbit_string_literal_62.data
  );
  if (_tmp$4254.tag) {
    int32_t const _ok$3343 = _tmp$4254.data.ok;
  } else {
    void* const _err$3344 = _tmp$4254.data.err;
    struct moonbit_result_0 _result$4255;
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4255.tag = 0;
    _result$4255.data.err = _err$3344;
    return _result$4255;
  }
  _tmp$3345 = $moonbitlang$core$builtin$op_notequal$3(some_bool$1321, -1);
  _tmp$3346 = 0;
  _tmp$4256
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3345, _tmp$3346, (moonbit_string_t)moonbit_string_literal_63.data
  );
  if (_tmp$4256.tag) {
    int32_t const _ok$3347 = _tmp$4256.data.ok;
  } else {
    void* const _err$3348 = _tmp$4256.data.err;
    struct moonbit_result_0 _result$4257;
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4257.tag = 0;
    _result$4257.data.err = _err$3348;
    return _result$4257;
  }
  if (some_int$1319 == 4294967296ll) {
    int_result$1325 = 0;
  } else {
    int64_t _Some$1328 = some_int$1319;
    int32_t _x$1329 = (int32_t)_Some$1328;
    x$1327 = _x$1329;
    goto $join$1326;
  }
  goto $joinlet$4258;
  $join$1326:;
  int_result$1325 = x$1327 + 10;
  $joinlet$4258:;
  _tmp$3349 = 0;
  _tmp$4259
  = $moonbitlang$core$builtin$assert_eq$0(
    int_result$1325,
      52,
      _tmp$3349,
      (moonbit_string_t)moonbit_string_literal_64.data
  );
  if (_tmp$4259.tag) {
    int32_t const _ok$3350 = _tmp$4259.data.ok;
  } else {
    void* const _err$3351 = _tmp$4259.data.err;
    struct moonbit_result_0 _result$4260;
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    _result$4260.tag = 0;
    _result$4260.data.err = _err$3351;
    return _result$4260;
  }
  if (some_string$1320 == 0) {
    if (some_string$1320) {
      moonbit_decref(some_string$1320);
    }
    string_result$1330 = (moonbit_string_t)moonbit_string_literal_65.data;
  } else {
    moonbit_string_t _Some$1333 = some_string$1320;
    moonbit_string_t _s$1334 = _Some$1333;
    s$1332 = _s$1334;
    goto $join$1331;
  }
  goto $joinlet$4261;
  $join$1331:;
  string_result$1330
  = moonbit_add_string(
    s$1332, (moonbit_string_t)moonbit_string_literal_66.data
  );
  $joinlet$4261:;
  _tmp$3352 = 0;
  _tmp$4262
  = $moonbitlang$core$builtin$assert_eq$3(
    string_result$1330,
      (moonbit_string_t)moonbit_string_literal_67.data,
      _tmp$3352,
      (moonbit_string_t)moonbit_string_literal_68.data
  );
  if (_tmp$4262.tag) {
    int32_t const _ok$3353 = _tmp$4262.data.ok;
  } else {
    void* const _err$3354 = _tmp$4262.data.err;
    struct moonbit_result_0 _result$4263;
    _result$4263.tag = 0;
    _result$4263.data.err = _err$3354;
    return _result$4263;
  }
  if (some_bool$1321 == -1) {
    bool_result$1335 = 0;
  } else {
    int32_t _Some$1338 = some_bool$1321;
    int32_t _b$1339 = _Some$1338;
    b$1337 = _b$1339;
    goto $join$1336;
  }
  goto $joinlet$4264;
  $join$1336:;
  bool_result$1335 = !b$1337;
  $joinlet$4264:;
  _tmp$3355 = 0;
  _tmp$4265
  = $moonbitlang$core$builtin$assert_eq$5(
    bool_result$1335,
      0,
      _tmp$3355,
      (moonbit_string_t)moonbit_string_literal_69.data
  );
  if (_tmp$4265.tag) {
    int32_t const _ok$3356 = _tmp$4265.data.ok;
  } else {
    void* const _err$3357 = _tmp$4265.data.err;
    struct moonbit_result_0 _result$4266;
    _result$4266.tag = 0;
    _result$4266.data.err = _err$3357;
    return _result$4266;
  }
  _tmp$3359 = (int64_t)42;
  nested_option$1340
  = (void*)moonbit_malloc(
      sizeof(struct $Option$3c$Option$3c$Int$3e$$3e$$Some)
    );
  Moonbit_object_header(nested_option$1340)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Option$3c$Option$3c$Int$3e$$3e$$Some) >> 2, 0, 1
  );
  ((struct $Option$3c$Option$3c$Int$3e$$3e$$Some*)nested_option$1340)->$0
  = _tmp$3359;
  switch (Moonbit_object_tag(nested_option$1340)) {
    case 1: {
      struct $Option$3c$Option$3c$Int$3e$$3e$$Some* _Some$1344 =
        (struct $Option$3c$Option$3c$Int$3e$$3e$$Some*)nested_option$1340;
      int64_t _field$3755 = _Some$1344->$0;
      int64_t _x$1345;
      moonbit_decref(_Some$1344);
      _x$1345 = _field$3755;
      if (_x$1345 == 4294967296ll) {
        nested_result$1341 = 0;
      } else {
        int64_t _Some$1346 = _x$1345;
        int32_t _x$1347 = (int32_t)_Some$1346;
        x$1343 = _x$1347;
        goto $join$1342;
      }
      break;
    }
    default: {
      moonbit_decref(nested_option$1340);
      nested_result$1341 = -1;
      break;
    }
  }
  goto $joinlet$4267;
  $join$1342:;
  nested_result$1341 = x$1343 * 2;
  $joinlet$4267:;
  _tmp$3358 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           nested_result$1341,
             84,
             _tmp$3358,
             (moonbit_string_t)moonbit_string_literal_70.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3(
  
) {
  moonbit_string_t* _tmp$3313 =
    (moonbit_string_t*)moonbit_make_ref_array_raw(3);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* string_array$1308;
  int32_t* _tmp$3312;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* int_array$1309;
  int32_t* _tmp$3311;
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* bool_array$1310;
  int32_t _tmp$3256;
  moonbit_string_t _tmp$3257;
  struct moonbit_result_0 _tmp$4268;
  int32_t _tmp$3260;
  moonbit_string_t _tmp$3261;
  struct moonbit_result_0 _tmp$4270;
  int32_t _tmp$3264;
  moonbit_string_t _tmp$3265;
  struct moonbit_result_0 _tmp$4272;
  moonbit_string_t _tmp$3268;
  moonbit_string_t _tmp$3269;
  struct moonbit_result_0 _tmp$4274;
  moonbit_string_t _tmp$3272;
  moonbit_string_t _tmp$3273;
  struct moonbit_result_0 _tmp$4276;
  int32_t _tmp$3276;
  moonbit_string_t _tmp$3277;
  struct moonbit_result_0 _tmp$4278;
  int32_t _tmp$3280;
  moonbit_string_t _tmp$3281;
  struct moonbit_result_0 _tmp$4280;
  int32_t _tmp$3284;
  moonbit_string_t _tmp$3285;
  struct moonbit_result_0 _tmp$4282;
  int32_t _tmp$3288;
  moonbit_string_t _tmp$3289;
  struct moonbit_result_0 _tmp$4284;
  moonbit_string_t* _tmp$3310;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* empty_array$1311;
  int32_t _tmp$3292;
  moonbit_string_t _tmp$3293;
  struct moonbit_result_0 _tmp$4286;
  moonbit_string_t* _tmp$3309;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* single_array$1312;
  int32_t _tmp$3296;
  moonbit_string_t _tmp$3297;
  struct moonbit_result_0 _tmp$4288;
  moonbit_string_t _tmp$3300;
  moonbit_string_t _tmp$3301;
  struct moonbit_result_0 _tmp$4290;
  struct $Ref$3c$Int$3e$* sum$1313;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _arr$1314;
  int32_t _len$1315;
  int32_t _i$1316;
  int32_t _field$3756;
  int32_t val$3307;
  moonbit_string_t _tmp$3308;
  _tmp$3313[0] = (moonbit_string_t)moonbit_string_literal_71.data;
  _tmp$3313[1] = (moonbit_string_t)moonbit_string_literal_72.data;
  _tmp$3313[2] = (moonbit_string_t)moonbit_string_literal_73.data;
  string_array$1308
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(string_array$1308)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  string_array$1308->$0 = _tmp$3313;
  string_array$1308->$1 = 3;
  _tmp$3312 = (int32_t*)moonbit_make_int32_array_raw(5);
  _tmp$3312[0] = 1;
  _tmp$3312[1] = 2;
  _tmp$3312[2] = 3;
  _tmp$3312[3] = 4;
  _tmp$3312[4] = 5;
  int_array$1309
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(int_array$1309)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  int_array$1309->$0 = _tmp$3312;
  int_array$1309->$1 = 5;
  _tmp$3311 = (int32_t*)moonbit_make_int32_array_raw(3);
  _tmp$3311[0] = 1;
  _tmp$3311[1] = 0;
  _tmp$3311[2] = 1;
  bool_array$1310
  = (struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$)
    );
  Moonbit_object_header(bool_array$1310)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$, $0) >> 2,
      1,
      0
  );
  bool_array$1310->$0 = _tmp$3311;
  bool_array$1310->$1 = 3;
  moonbit_incref(string_array$1308);
  _tmp$3256 = $$moonbitlang$core$builtin$Array$$length$1(string_array$1308);
  _tmp$3257 = 0;
  _tmp$4268
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3256, 3, _tmp$3257, (moonbit_string_t)moonbit_string_literal_74.data
  );
  if (_tmp$4268.tag) {
    int32_t const _ok$3258 = _tmp$4268.data.ok;
  } else {
    void* const _err$3259 = _tmp$4268.data.err;
    struct moonbit_result_0 _result$4269;
    moonbit_decref(bool_array$1310);
    moonbit_decref(int_array$1309);
    moonbit_decref(string_array$1308);
    _result$4269.tag = 0;
    _result$4269.data.err = _err$3259;
    return _result$4269;
  }
  moonbit_incref(int_array$1309);
  _tmp$3260 = $$moonbitlang$core$builtin$Array$$length$2(int_array$1309);
  _tmp$3261 = 0;
  _tmp$4270
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3260, 5, _tmp$3261, (moonbit_string_t)moonbit_string_literal_75.data
  );
  if (_tmp$4270.tag) {
    int32_t const _ok$3262 = _tmp$4270.data.ok;
  } else {
    void* const _err$3263 = _tmp$4270.data.err;
    struct moonbit_result_0 _result$4271;
    moonbit_decref(bool_array$1310);
    moonbit_decref(int_array$1309);
    moonbit_decref(string_array$1308);
    _result$4271.tag = 0;
    _result$4271.data.err = _err$3263;
    return _result$4271;
  }
  moonbit_incref(bool_array$1310);
  _tmp$3264 = $$moonbitlang$core$builtin$Array$$length$3(bool_array$1310);
  _tmp$3265 = 0;
  _tmp$4272
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3264, 3, _tmp$3265, (moonbit_string_t)moonbit_string_literal_76.data
  );
  if (_tmp$4272.tag) {
    int32_t const _ok$3266 = _tmp$4272.data.ok;
  } else {
    void* const _err$3267 = _tmp$4272.data.err;
    struct moonbit_result_0 _result$4273;
    moonbit_decref(bool_array$1310);
    moonbit_decref(int_array$1309);
    moonbit_decref(string_array$1308);
    _result$4273.tag = 0;
    _result$4273.data.err = _err$3267;
    return _result$4273;
  }
  moonbit_incref(string_array$1308);
  _tmp$3268 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1308, 0);
  _tmp$3269 = 0;
  _tmp$4274
  = $moonbitlang$core$builtin$assert_eq$3(
    _tmp$3268,
      (moonbit_string_t)moonbit_string_literal_71.data,
      _tmp$3269,
      (moonbit_string_t)moonbit_string_literal_77.data
  );
  if (_tmp$4274.tag) {
    int32_t const _ok$3270 = _tmp$4274.data.ok;
  } else {
    void* const _err$3271 = _tmp$4274.data.err;
    struct moonbit_result_0 _result$4275;
    moonbit_decref(bool_array$1310);
    moonbit_decref(int_array$1309);
    moonbit_decref(string_array$1308);
    _result$4275.tag = 0;
    _result$4275.data.err = _err$3271;
    return _result$4275;
  }
  _tmp$3272 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1308, 2);
  _tmp$3273 = 0;
  _tmp$4276
  = $moonbitlang$core$builtin$assert_eq$3(
    _tmp$3272,
      (moonbit_string_t)moonbit_string_literal_73.data,
      _tmp$3273,
      (moonbit_string_t)moonbit_string_literal_78.data
  );
  if (_tmp$4276.tag) {
    int32_t const _ok$3274 = _tmp$4276.data.ok;
  } else {
    void* const _err$3275 = _tmp$4276.data.err;
    struct moonbit_result_0 _result$4277;
    moonbit_decref(bool_array$1310);
    moonbit_decref(int_array$1309);
    _result$4277.tag = 0;
    _result$4277.data.err = _err$3275;
    return _result$4277;
  }
  moonbit_incref(int_array$1309);
  _tmp$3276 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1309, 0);
  _tmp$3277 = 0;
  _tmp$4278
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3276, 1, _tmp$3277, (moonbit_string_t)moonbit_string_literal_79.data
  );
  if (_tmp$4278.tag) {
    int32_t const _ok$3278 = _tmp$4278.data.ok;
  } else {
    void* const _err$3279 = _tmp$4278.data.err;
    struct moonbit_result_0 _result$4279;
    moonbit_decref(bool_array$1310);
    moonbit_decref(int_array$1309);
    _result$4279.tag = 0;
    _result$4279.data.err = _err$3279;
    return _result$4279;
  }
  moonbit_incref(int_array$1309);
  _tmp$3280 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1309, 4);
  _tmp$3281 = 0;
  _tmp$4280
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3280, 5, _tmp$3281, (moonbit_string_t)moonbit_string_literal_80.data
  );
  if (_tmp$4280.tag) {
    int32_t const _ok$3282 = _tmp$4280.data.ok;
  } else {
    void* const _err$3283 = _tmp$4280.data.err;
    struct moonbit_result_0 _result$4281;
    moonbit_decref(bool_array$1310);
    moonbit_decref(int_array$1309);
    _result$4281.tag = 0;
    _result$4281.data.err = _err$3283;
    return _result$4281;
  }
  moonbit_incref(bool_array$1310);
  _tmp$3284 = $$moonbitlang$core$builtin$Array$$at$2(bool_array$1310, 0);
  _tmp$3285 = 0;
  _tmp$4282
  = $moonbitlang$core$builtin$assert_eq$5(
    _tmp$3284, 1, _tmp$3285, (moonbit_string_t)moonbit_string_literal_81.data
  );
  if (_tmp$4282.tag) {
    int32_t const _ok$3286 = _tmp$4282.data.ok;
  } else {
    void* const _err$3287 = _tmp$4282.data.err;
    struct moonbit_result_0 _result$4283;
    moonbit_decref(bool_array$1310);
    moonbit_decref(int_array$1309);
    _result$4283.tag = 0;
    _result$4283.data.err = _err$3287;
    return _result$4283;
  }
  _tmp$3288 = $$moonbitlang$core$builtin$Array$$at$2(bool_array$1310, 1);
  _tmp$3289 = 0;
  _tmp$4284
  = $moonbitlang$core$builtin$assert_eq$5(
    _tmp$3288, 0, _tmp$3289, (moonbit_string_t)moonbit_string_literal_82.data
  );
  if (_tmp$4284.tag) {
    int32_t const _ok$3290 = _tmp$4284.data.ok;
  } else {
    void* const _err$3291 = _tmp$4284.data.err;
    struct moonbit_result_0 _result$4285;
    moonbit_decref(int_array$1309);
    _result$4285.tag = 0;
    _result$4285.data.err = _err$3291;
    return _result$4285;
  }
  _tmp$3310 = (moonbit_string_t*)moonbit_empty_ref_array;
  empty_array$1311
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(empty_array$1311)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  empty_array$1311->$0 = _tmp$3310;
  empty_array$1311->$1 = 0;
  _tmp$3292 = $$moonbitlang$core$builtin$Array$$length$1(empty_array$1311);
  _tmp$3293 = 0;
  _tmp$4286
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3292, 0, _tmp$3293, (moonbit_string_t)moonbit_string_literal_83.data
  );
  if (_tmp$4286.tag) {
    int32_t const _ok$3294 = _tmp$4286.data.ok;
  } else {
    void* const _err$3295 = _tmp$4286.data.err;
    struct moonbit_result_0 _result$4287;
    moonbit_decref(int_array$1309);
    _result$4287.tag = 0;
    _result$4287.data.err = _err$3295;
    return _result$4287;
  }
  _tmp$3309 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$3309[0] = (moonbit_string_t)moonbit_string_literal_84.data;
  single_array$1312
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(single_array$1312)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  single_array$1312->$0 = _tmp$3309;
  single_array$1312->$1 = 1;
  moonbit_incref(single_array$1312);
  _tmp$3296 = $$moonbitlang$core$builtin$Array$$length$1(single_array$1312);
  _tmp$3297 = 0;
  _tmp$4288
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3296, 1, _tmp$3297, (moonbit_string_t)moonbit_string_literal_85.data
  );
  if (_tmp$4288.tag) {
    int32_t const _ok$3298 = _tmp$4288.data.ok;
  } else {
    void* const _err$3299 = _tmp$4288.data.err;
    struct moonbit_result_0 _result$4289;
    moonbit_decref(single_array$1312);
    moonbit_decref(int_array$1309);
    _result$4289.tag = 0;
    _result$4289.data.err = _err$3299;
    return _result$4289;
  }
  _tmp$3300 = $$moonbitlang$core$builtin$Array$$at$0(single_array$1312, 0);
  _tmp$3301 = 0;
  _tmp$4290
  = $moonbitlang$core$builtin$assert_eq$3(
    _tmp$3300,
      (moonbit_string_t)moonbit_string_literal_84.data,
      _tmp$3301,
      (moonbit_string_t)moonbit_string_literal_86.data
  );
  if (_tmp$4290.tag) {
    int32_t const _ok$3302 = _tmp$4290.data.ok;
  } else {
    void* const _err$3303 = _tmp$4290.data.err;
    struct moonbit_result_0 _result$4291;
    moonbit_decref(int_array$1309);
    _result$4291.tag = 0;
    _result$4291.data.err = _err$3303;
    return _result$4291;
  }
  sum$1313
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(sum$1313)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  sum$1313->$0 = 0;
  _arr$1314 = int_array$1309;
  moonbit_incref(_arr$1314);
  _len$1315 = $$moonbitlang$core$builtin$Array$$length$2(_arr$1314);
  _i$1316 = 0;
  while (1) {
    if (_i$1316 < _len$1315) {
      int32_t num$1317;
      int32_t val$3305;
      int32_t _tmp$3304;
      int32_t _tmp$3306;
      moonbit_incref(_arr$1314);
      num$1317
      = $$moonbitlang$core$builtin$Array$$unsafe_get$2(
        _arr$1314, _i$1316
      );
      val$3305 = sum$1313->$0;
      _tmp$3304 = val$3305 + num$1317;
      sum$1313->$0 = _tmp$3304;
      _tmp$3306 = _i$1316 + 1;
      _i$1316 = _tmp$3306;
      continue;
    } else {
      moonbit_decref(_arr$1314);
    }
    break;
  }
  _field$3756 = sum$1313->$0;
  moonbit_decref(sum$1313);
  val$3307 = _field$3756;
  _tmp$3308 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           val$3307,
             15,
             _tmp$3308,
             (moonbit_string_t)moonbit_string_literal_87.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2(
  
) {
  moonbit_string_t empty_string$1299 =
    (moonbit_string_t)moonbit_string_literal_3.data;
  int32_t _tmp$3762 = Moonbit_array_length(empty_string$1299);
  int32_t _tmp$3227;
  moonbit_string_t _tmp$3228;
  struct moonbit_result_0 _tmp$4293;
  moonbit_string_t single_char$1300;
  int32_t _tmp$3761;
  int32_t _tmp$3231;
  moonbit_string_t _tmp$3232;
  struct moonbit_result_0 _tmp$4295;
  moonbit_string_t str1$1301;
  moonbit_string_t str2$1302;
  moonbit_string_t combined$1303;
  moonbit_string_t _tmp$3235;
  struct moonbit_result_0 _tmp$4297;
  moonbit_string_t spaced_string$1304;
  int32_t _tmp$3760;
  int32_t _tmp$3238;
  moonbit_string_t _tmp$3239;
  struct moonbit_result_0 _tmp$4299;
  moonbit_string_t special_chars$1305;
  int32_t _tmp$3759;
  int32_t _tmp$3242;
  moonbit_string_t _tmp$3243;
  struct moonbit_result_0 _tmp$4301;
  moonbit_string_t long_string$1306;
  int32_t _tmp$3758;
  int32_t _tmp$3246;
  moonbit_string_t _tmp$3247;
  struct moonbit_result_0 _tmp$4303;
  moonbit_string_t test_string$1307;
  int32_t _tmp$3250;
  moonbit_string_t _tmp$3251;
  struct moonbit_result_0 _tmp$4305;
  int32_t _tmp$3757;
  int32_t _tmp$3254;
  moonbit_string_t _tmp$3255;
  moonbit_decref(empty_string$1299);
  _tmp$3227 = _tmp$3762;
  _tmp$3228 = 0;
  _tmp$4293
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3227, 0, _tmp$3228, (moonbit_string_t)moonbit_string_literal_88.data
  );
  if (_tmp$4293.tag) {
    int32_t const _ok$3229 = _tmp$4293.data.ok;
  } else {
    void* const _err$3230 = _tmp$4293.data.err;
    struct moonbit_result_0 _result$4294;
    _result$4294.tag = 0;
    _result$4294.data.err = _err$3230;
    return _result$4294;
  }
  single_char$1300 = (moonbit_string_t)moonbit_string_literal_89.data;
  _tmp$3761 = Moonbit_array_length(single_char$1300);
  moonbit_decref(single_char$1300);
  _tmp$3231 = _tmp$3761;
  _tmp$3232 = 0;
  _tmp$4295
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3231, 1, _tmp$3232, (moonbit_string_t)moonbit_string_literal_90.data
  );
  if (_tmp$4295.tag) {
    int32_t const _ok$3233 = _tmp$4295.data.ok;
  } else {
    void* const _err$3234 = _tmp$4295.data.err;
    struct moonbit_result_0 _result$4296;
    _result$4296.tag = 0;
    _result$4296.data.err = _err$3234;
    return _result$4296;
  }
  str1$1301 = (moonbit_string_t)moonbit_string_literal_91.data;
  str2$1302 = (moonbit_string_t)moonbit_string_literal_92.data;
  combined$1303 = moonbit_add_string(str1$1301, str2$1302);
  _tmp$3235 = 0;
  _tmp$4297
  = $moonbitlang$core$builtin$assert_eq$3(
    combined$1303,
      (moonbit_string_t)moonbit_string_literal_93.data,
      _tmp$3235,
      (moonbit_string_t)moonbit_string_literal_94.data
  );
  if (_tmp$4297.tag) {
    int32_t const _ok$3236 = _tmp$4297.data.ok;
  } else {
    void* const _err$3237 = _tmp$4297.data.err;
    struct moonbit_result_0 _result$4298;
    _result$4298.tag = 0;
    _result$4298.data.err = _err$3237;
    return _result$4298;
  }
  spaced_string$1304 = (moonbit_string_t)moonbit_string_literal_95.data;
  _tmp$3760 = Moonbit_array_length(spaced_string$1304);
  moonbit_decref(spaced_string$1304);
  _tmp$3238 = _tmp$3760;
  _tmp$3239 = 0;
  _tmp$4299
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3238,
      10,
      _tmp$3239,
      (moonbit_string_t)moonbit_string_literal_96.data
  );
  if (_tmp$4299.tag) {
    int32_t const _ok$3240 = _tmp$4299.data.ok;
  } else {
    void* const _err$3241 = _tmp$4299.data.err;
    struct moonbit_result_0 _result$4300;
    _result$4300.tag = 0;
    _result$4300.data.err = _err$3241;
    return _result$4300;
  }
  special_chars$1305 = (moonbit_string_t)moonbit_string_literal_97.data;
  _tmp$3759 = Moonbit_array_length(special_chars$1305);
  moonbit_decref(special_chars$1305);
  _tmp$3242 = _tmp$3759;
  _tmp$3243 = 0;
  _tmp$4301
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3242,
      29,
      _tmp$3243,
      (moonbit_string_t)moonbit_string_literal_98.data
  );
  if (_tmp$4301.tag) {
    int32_t const _ok$3244 = _tmp$4301.data.ok;
  } else {
    void* const _err$3245 = _tmp$4301.data.err;
    struct moonbit_result_0 _result$4302;
    _result$4302.tag = 0;
    _result$4302.data.err = _err$3245;
    return _result$4302;
  }
  long_string$1306
  = $String$$repeat(
    (moonbit_string_t)moonbit_string_literal_89.data, 100
  );
  _tmp$3758 = Moonbit_array_length(long_string$1306);
  moonbit_decref(long_string$1306);
  _tmp$3246 = _tmp$3758;
  _tmp$3247 = 0;
  _tmp$4303
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3246,
      100,
      _tmp$3247,
      (moonbit_string_t)moonbit_string_literal_99.data
  );
  if (_tmp$4303.tag) {
    int32_t const _ok$3248 = _tmp$4303.data.ok;
  } else {
    void* const _err$3249 = _tmp$4303.data.err;
    struct moonbit_result_0 _result$4304;
    _result$4304.tag = 0;
    _result$4304.data.err = _err$3249;
    return _result$4304;
  }
  test_string$1307 = (moonbit_string_t)moonbit_string_literal_100.data;
  if (0 < 0 || 0 >= Moonbit_array_length(test_string$1307)) {
    moonbit_panic();
  }
  _tmp$3250 = test_string$1307[0];
  _tmp$3251 = 0;
  _tmp$4305
  = $moonbitlang$core$builtin$assert_eq$4(
    _tmp$3250,
      97,
      _tmp$3251,
      (moonbit_string_t)moonbit_string_literal_101.data
  );
  if (_tmp$4305.tag) {
    int32_t const _ok$3252 = _tmp$4305.data.ok;
  } else {
    void* const _err$3253 = _tmp$4305.data.err;
    struct moonbit_result_0 _result$4306;
    moonbit_decref(test_string$1307);
    _result$4306.tag = 0;
    _result$4306.data.err = _err$3253;
    return _result$4306;
  }
  if (5 < 0 || 5 >= Moonbit_array_length(test_string$1307)) {
    moonbit_panic();
  }
  _tmp$3757 = test_string$1307[5];
  moonbit_decref(test_string$1307);
  _tmp$3254 = _tmp$3757;
  _tmp$3255 = 0;
  return $moonbitlang$core$builtin$assert_eq$4(
           _tmp$3254,
             102,
             _tmp$3255,
             (moonbit_string_t)moonbit_string_literal_102.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1(
  
) {
  moonbit_string_t _tmp$3170 = 0;
  struct moonbit_result_0 _tmp$4307 =
    $moonbitlang$core$builtin$assert_eq$0(
      15, 15, _tmp$3170, (moonbit_string_t)moonbit_string_literal_103.data
    );
  moonbit_string_t _tmp$3173;
  struct moonbit_result_0 _tmp$4309;
  moonbit_string_t _tmp$3176;
  struct moonbit_result_0 _tmp$4311;
  moonbit_string_t _tmp$3179;
  struct moonbit_result_0 _tmp$4313;
  moonbit_string_t _tmp$3182;
  struct moonbit_result_0 _tmp$4315;
  moonbit_string_t _tmp$3185;
  struct moonbit_result_0 _tmp$4317;
  moonbit_string_t _tmp$3188;
  struct moonbit_result_0 _tmp$4319;
  moonbit_string_t _tmp$3191;
  struct moonbit_result_0 _tmp$4321;
  moonbit_string_t _tmp$3194;
  struct moonbit_result_0 _tmp$4323;
  moonbit_string_t _tmp$3197;
  struct moonbit_result_0 _tmp$4325;
  moonbit_string_t _tmp$3200;
  struct moonbit_result_0 _tmp$4327;
  moonbit_string_t _tmp$3203;
  struct moonbit_result_0 _tmp$4329;
  moonbit_string_t _tmp$3206;
  struct moonbit_result_0 _tmp$4331;
  moonbit_string_t _tmp$3209;
  struct moonbit_result_0 _tmp$4333;
  moonbit_string_t _tmp$3212;
  struct moonbit_result_0 _tmp$4335;
  moonbit_string_t _tmp$3215;
  struct moonbit_result_0 _tmp$4337;
  moonbit_string_t _tmp$3218;
  struct moonbit_result_0 _tmp$4339;
  int32_t large_num$1298;
  int32_t _tmp$3221;
  moonbit_string_t _tmp$3222;
  struct moonbit_result_0 _tmp$4341;
  int32_t _tmp$3225;
  moonbit_string_t _tmp$3226;
  if (_tmp$4307.tag) {
    int32_t const _ok$3171 = _tmp$4307.data.ok;
  } else {
    void* const _err$3172 = _tmp$4307.data.err;
    struct moonbit_result_0 _result$4308;
    _result$4308.tag = 0;
    _result$4308.data.err = _err$3172;
    return _result$4308;
  }
  _tmp$3173 = 0;
  _tmp$4309
  = $moonbitlang$core$builtin$assert_eq$0(
    5, 5, _tmp$3173, (moonbit_string_t)moonbit_string_literal_104.data
  );
  if (_tmp$4309.tag) {
    int32_t const _ok$3174 = _tmp$4309.data.ok;
  } else {
    void* const _err$3175 = _tmp$4309.data.err;
    struct moonbit_result_0 _result$4310;
    _result$4310.tag = 0;
    _result$4310.data.err = _err$3175;
    return _result$4310;
  }
  _tmp$3176 = 0;
  _tmp$4311
  = $moonbitlang$core$builtin$assert_eq$0(
    50, 50, _tmp$3176, (moonbit_string_t)moonbit_string_literal_105.data
  );
  if (_tmp$4311.tag) {
    int32_t const _ok$3177 = _tmp$4311.data.ok;
  } else {
    void* const _err$3178 = _tmp$4311.data.err;
    struct moonbit_result_0 _result$4312;
    _result$4312.tag = 0;
    _result$4312.data.err = _err$3178;
    return _result$4312;
  }
  _tmp$3179 = 0;
  _tmp$4313
  = $moonbitlang$core$builtin$assert_eq$0(
    2, 2, _tmp$3179, (moonbit_string_t)moonbit_string_literal_106.data
  );
  if (_tmp$4313.tag) {
    int32_t const _ok$3180 = _tmp$4313.data.ok;
  } else {
    void* const _err$3181 = _tmp$4313.data.err;
    struct moonbit_result_0 _result$4314;
    _result$4314.tag = 0;
    _result$4314.data.err = _err$3181;
    return _result$4314;
  }
  _tmp$3182 = 0;
  _tmp$4315
  = $moonbitlang$core$builtin$assert_eq$0(
    1, 1, _tmp$3182, (moonbit_string_t)moonbit_string_literal_107.data
  );
  if (_tmp$4315.tag) {
    int32_t const _ok$3183 = _tmp$4315.data.ok;
  } else {
    void* const _err$3184 = _tmp$4315.data.err;
    struct moonbit_result_0 _result$4316;
    _result$4316.tag = 0;
    _result$4316.data.err = _err$3184;
    return _result$4316;
  }
  _tmp$3185 = 0;
  _tmp$4317
  = $moonbitlang$core$builtin$assert_eq$2(
    0x1.8p+2,
      0x1.8p+2,
      _tmp$3185,
      (moonbit_string_t)moonbit_string_literal_108.data
  );
  if (_tmp$4317.tag) {
    int32_t const _ok$3186 = _tmp$4317.data.ok;
  } else {
    void* const _err$3187 = _tmp$4317.data.err;
    struct moonbit_result_0 _result$4318;
    _result$4318.tag = 0;
    _result$4318.data.err = _err$3187;
    return _result$4318;
  }
  _tmp$3188 = 0;
  _tmp$4319
  = $moonbitlang$core$builtin$assert_eq$2(
    0x1.91eb851eb851fp+2,
      0x1.91eb851eb851fp+2,
      _tmp$3188,
      (moonbit_string_t)moonbit_string_literal_109.data
  );
  if (_tmp$4319.tag) {
    int32_t const _ok$3189 = _tmp$4319.data.ok;
  } else {
    void* const _err$3190 = _tmp$4319.data.err;
    struct moonbit_result_0 _result$4320;
    _result$4320.tag = 0;
    _result$4320.data.err = _err$3190;
    return _result$4320;
  }
  _tmp$3191 = 0;
  _tmp$4321
  = $moonbitlang$core$builtin$assert_eq$2(
    0x1.4p+1,
      0x1.4p+1,
      _tmp$3191,
      (moonbit_string_t)moonbit_string_literal_110.data
  );
  if (_tmp$4321.tag) {
    int32_t const _ok$3192 = _tmp$4321.data.ok;
  } else {
    void* const _err$3193 = _tmp$4321.data.err;
    struct moonbit_result_0 _result$4322;
    _result$4322.tag = 0;
    _result$4322.data.err = _err$3193;
    return _result$4322;
  }
  _tmp$3194 = 0;
  _tmp$4323
  = $moonbitlang$core$builtin$assert_eq$0(
    5, 5, _tmp$3194, (moonbit_string_t)moonbit_string_literal_111.data
  );
  if (_tmp$4323.tag) {
    int32_t const _ok$3195 = _tmp$4323.data.ok;
  } else {
    void* const _err$3196 = _tmp$4323.data.err;
    struct moonbit_result_0 _result$4324;
    _result$4324.tag = 0;
    _result$4324.data.err = _err$3196;
    return _result$4324;
  }
  _tmp$3197 = 0;
  _tmp$4325
  = $moonbitlang$core$builtin$assert_eq$0(
    -10, -10, _tmp$3197, (moonbit_string_t)moonbit_string_literal_112.data
  );
  if (_tmp$4325.tag) {
    int32_t const _ok$3198 = _tmp$4325.data.ok;
  } else {
    void* const _err$3199 = _tmp$4325.data.err;
    struct moonbit_result_0 _result$4326;
    _result$4326.tag = 0;
    _result$4326.data.err = _err$3199;
    return _result$4326;
  }
  _tmp$3200 = 0;
  _tmp$4327
  = $moonbitlang$core$builtin$assert_eq$0(
    -5, -5, _tmp$3200, (moonbit_string_t)moonbit_string_literal_113.data
  );
  if (_tmp$4327.tag) {
    int32_t const _ok$3201 = _tmp$4327.data.ok;
  } else {
    void* const _err$3202 = _tmp$4327.data.err;
    struct moonbit_result_0 _result$4328;
    _result$4328.tag = 0;
    _result$4328.data.err = _err$3202;
    return _result$4328;
  }
  _tmp$3203 = 0;
  _tmp$4329
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3203, (moonbit_string_t)moonbit_string_literal_114.data
  );
  if (_tmp$4329.tag) {
    int32_t const _ok$3204 = _tmp$4329.data.ok;
  } else {
    void* const _err$3205 = _tmp$4329.data.err;
    struct moonbit_result_0 _result$4330;
    _result$4330.tag = 0;
    _result$4330.data.err = _err$3205;
    return _result$4330;
  }
  _tmp$3206 = 0;
  _tmp$4331
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3206, (moonbit_string_t)moonbit_string_literal_115.data
  );
  if (_tmp$4331.tag) {
    int32_t const _ok$3207 = _tmp$4331.data.ok;
  } else {
    void* const _err$3208 = _tmp$4331.data.err;
    struct moonbit_result_0 _result$4332;
    _result$4332.tag = 0;
    _result$4332.data.err = _err$3208;
    return _result$4332;
  }
  _tmp$3209 = 0;
  _tmp$4333
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3209, (moonbit_string_t)moonbit_string_literal_116.data
  );
  if (_tmp$4333.tag) {
    int32_t const _ok$3210 = _tmp$4333.data.ok;
  } else {
    void* const _err$3211 = _tmp$4333.data.err;
    struct moonbit_result_0 _result$4334;
    _result$4334.tag = 0;
    _result$4334.data.err = _err$3211;
    return _result$4334;
  }
  _tmp$3212 = 0;
  _tmp$4335
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3212, (moonbit_string_t)moonbit_string_literal_117.data
  );
  if (_tmp$4335.tag) {
    int32_t const _ok$3213 = _tmp$4335.data.ok;
  } else {
    void* const _err$3214 = _tmp$4335.data.err;
    struct moonbit_result_0 _result$4336;
    _result$4336.tag = 0;
    _result$4336.data.err = _err$3214;
    return _result$4336;
  }
  _tmp$3215 = 0;
  _tmp$4337
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3215, (moonbit_string_t)moonbit_string_literal_118.data
  );
  if (_tmp$4337.tag) {
    int32_t const _ok$3216 = _tmp$4337.data.ok;
  } else {
    void* const _err$3217 = _tmp$4337.data.err;
    struct moonbit_result_0 _result$4338;
    _result$4338.tag = 0;
    _result$4338.data.err = _err$3217;
    return _result$4338;
  }
  _tmp$3218 = 0;
  _tmp$4339
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3218, (moonbit_string_t)moonbit_string_literal_119.data
  );
  if (_tmp$4339.tag) {
    int32_t const _ok$3219 = _tmp$4339.data.ok;
  } else {
    void* const _err$3220 = _tmp$4339.data.err;
    struct moonbit_result_0 _result$4340;
    _result$4340.tag = 0;
    _result$4340.data.err = _err$3220;
    return _result$4340;
  }
  large_num$1298 = 1000000;
  _tmp$3221 = large_num$1298 + large_num$1298;
  _tmp$3222 = 0;
  _tmp$4341
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3221,
      2000000,
      _tmp$3222,
      (moonbit_string_t)moonbit_string_literal_120.data
  );
  if (_tmp$4341.tag) {
    int32_t const _ok$3223 = _tmp$4341.data.ok;
  } else {
    void* const _err$3224 = _tmp$4341.data.err;
    struct moonbit_result_0 _result$4342;
    _result$4342.tag = 0;
    _result$4342.data.err = _err$3224;
    return _result$4342;
  }
  _tmp$3225 = large_num$1298 * 2;
  _tmp$3226 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           _tmp$3225,
             2000000,
             _tmp$3226,
             (moonbit_string_t)moonbit_string_literal_121.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0(
  
) {
  moonbit_string_t test_string$1287 =
    (moonbit_string_t)moonbit_string_literal_122.data;
  int32_t _tmp$3111 = Moonbit_array_length(test_string$1287);
  moonbit_string_t _tmp$3112 = 0;
  struct moonbit_result_0 _tmp$4343 =
    $moonbitlang$core$builtin$assert_eq$0(
      _tmp$3111,
        22,
        _tmp$3112,
        (moonbit_string_t)moonbit_string_literal_123.data
    );
  moonbit_string_t _bind$1288;
  int32_t _tmp$3118;
  struct $StringView _tmp$3117;
  int32_t _tmp$3115;
  moonbit_string_t _tmp$3116;
  struct moonbit_result_0 _tmp$4345;
  moonbit_string_t _bind$1289;
  int32_t _tmp$3124;
  struct $StringView _tmp$3123;
  int32_t _tmp$3121;
  moonbit_string_t _tmp$3122;
  struct moonbit_result_0 _tmp$4347;
  int32_t* _tmp$3169;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* test_array$1290;
  int32_t _tmp$3127;
  moonbit_string_t _tmp$3128;
  struct moonbit_result_0 _tmp$4349;
  int32_t _tmp$3131;
  moonbit_string_t _tmp$3132;
  struct moonbit_result_0 _tmp$4351;
  int32_t _tmp$3135;
  moonbit_string_t _tmp$3136;
  struct moonbit_result_0 _tmp$4353;
  int64_t some_value$1291;
  int64_t none_value$1292;
  int64_t _tmp$3139;
  moonbit_string_t _tmp$3140;
  struct moonbit_result_0 _tmp$4355;
  moonbit_string_t _tmp$3143;
  struct moonbit_result_0 _tmp$4357;
  int32_t _tmp$3146;
  moonbit_string_t _tmp$3147;
  struct moonbit_result_0 _tmp$4359;
  int32_t x$1295;
  int32_t result$1293;
  moonbit_string_t _tmp$3150;
  struct moonbit_result_0 _tmp$4362;
  moonbit_string_t _tmp$3153;
  struct moonbit_result_0 _tmp$4364;
  moonbit_string_t _tmp$3156;
  struct moonbit_result_0 _tmp$4366;
  moonbit_string_t _tmp$3159;
  struct moonbit_result_0 _tmp$4368;
  moonbit_string_t _tmp$3162;
  struct moonbit_result_0 _tmp$4370;
  moonbit_string_t _tmp$3165;
  struct moonbit_result_0 _tmp$4372;
  moonbit_string_t _tmp$3168;
  if (_tmp$4343.tag) {
    int32_t const _ok$3113 = _tmp$4343.data.ok;
  } else {
    void* const _err$3114 = _tmp$4343.data.err;
    struct moonbit_result_0 _result$4344;
    moonbit_decref(test_string$1287);
    _result$4344.tag = 0;
    _result$4344.data.err = _err$3114;
    return _result$4344;
  }
  _bind$1288 = (moonbit_string_t)moonbit_string_literal_124.data;
  _tmp$3118 = Moonbit_array_length(_bind$1288);
  _tmp$3117 = (struct $StringView){0, _tmp$3118, _bind$1288};
  moonbit_incref(test_string$1287);
  _tmp$3115 = $String$$contains(test_string$1287, _tmp$3117);
  _tmp$3116 = 0;
  _tmp$4345
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3115, _tmp$3116, (moonbit_string_t)moonbit_string_literal_125.data
  );
  if (_tmp$4345.tag) {
    int32_t const _ok$3119 = _tmp$4345.data.ok;
  } else {
    void* const _err$3120 = _tmp$4345.data.err;
    struct moonbit_result_0 _result$4346;
    moonbit_decref(test_string$1287);
    _result$4346.tag = 0;
    _result$4346.data.err = _err$3120;
    return _result$4346;
  }
  _bind$1289 = (moonbit_string_t)moonbit_string_literal_126.data;
  _tmp$3124 = Moonbit_array_length(_bind$1289);
  _tmp$3123 = (struct $StringView){0, _tmp$3124, _bind$1289};
  _tmp$3121 = $String$$contains(test_string$1287, _tmp$3123);
  _tmp$3122 = 0;
  _tmp$4347
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3121, _tmp$3122, (moonbit_string_t)moonbit_string_literal_127.data
  );
  if (_tmp$4347.tag) {
    int32_t const _ok$3125 = _tmp$4347.data.ok;
  } else {
    void* const _err$3126 = _tmp$4347.data.err;
    struct moonbit_result_0 _result$4348;
    _result$4348.tag = 0;
    _result$4348.data.err = _err$3126;
    return _result$4348;
  }
  _tmp$3169 = (int32_t*)moonbit_make_int32_array_raw(10);
  _tmp$3169[0] = 1;
  _tmp$3169[1] = 2;
  _tmp$3169[2] = 3;
  _tmp$3169[3] = 4;
  _tmp$3169[4] = 5;
  _tmp$3169[5] = 6;
  _tmp$3169[6] = 7;
  _tmp$3169[7] = 8;
  _tmp$3169[8] = 9;
  _tmp$3169[9] = 10;
  test_array$1290
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(test_array$1290)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  test_array$1290->$0 = _tmp$3169;
  test_array$1290->$1 = 10;
  moonbit_incref(test_array$1290);
  _tmp$3127 = $$moonbitlang$core$builtin$Array$$length$2(test_array$1290);
  _tmp$3128 = 0;
  _tmp$4349
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3127,
      10,
      _tmp$3128,
      (moonbit_string_t)moonbit_string_literal_128.data
  );
  if (_tmp$4349.tag) {
    int32_t const _ok$3129 = _tmp$4349.data.ok;
  } else {
    void* const _err$3130 = _tmp$4349.data.err;
    struct moonbit_result_0 _result$4350;
    moonbit_decref(test_array$1290);
    _result$4350.tag = 0;
    _result$4350.data.err = _err$3130;
    return _result$4350;
  }
  moonbit_incref(test_array$1290);
  _tmp$3131 = $$moonbitlang$core$builtin$Array$$at$1(test_array$1290, 0);
  _tmp$3132 = 0;
  _tmp$4351
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3131,
      1,
      _tmp$3132,
      (moonbit_string_t)moonbit_string_literal_129.data
  );
  if (_tmp$4351.tag) {
    int32_t const _ok$3133 = _tmp$4351.data.ok;
  } else {
    void* const _err$3134 = _tmp$4351.data.err;
    struct moonbit_result_0 _result$4352;
    moonbit_decref(test_array$1290);
    _result$4352.tag = 0;
    _result$4352.data.err = _err$3134;
    return _result$4352;
  }
  _tmp$3135 = $$moonbitlang$core$builtin$Array$$at$1(test_array$1290, 9);
  _tmp$3136 = 0;
  _tmp$4353
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3135,
      10,
      _tmp$3136,
      (moonbit_string_t)moonbit_string_literal_130.data
  );
  if (_tmp$4353.tag) {
    int32_t const _ok$3137 = _tmp$4353.data.ok;
  } else {
    void* const _err$3138 = _tmp$4353.data.err;
    struct moonbit_result_0 _result$4354;
    _result$4354.tag = 0;
    _result$4354.data.err = _err$3138;
    return _result$4354;
  }
  some_value$1291 = (int64_t)42;
  none_value$1292 = 4294967296ll;
  _tmp$3139 = (int64_t)42;
  _tmp$3140 = 0;
  _tmp$4355
  = $moonbitlang$core$builtin$assert_eq$1(
    some_value$1291,
      _tmp$3139,
      _tmp$3140,
      (moonbit_string_t)moonbit_string_literal_131.data
  );
  if (_tmp$4355.tag) {
    int32_t const _ok$3141 = _tmp$4355.data.ok;
  } else {
    void* const _err$3142 = _tmp$4355.data.err;
    struct moonbit_result_0 _result$4356;
    _result$4356.tag = 0;
    _result$4356.data.err = _err$3142;
    return _result$4356;
  }
  _tmp$3143 = 0;
  _tmp$4357
  = $moonbitlang$core$builtin$assert_eq$1(
    none_value$1292,
      4294967296ll,
      _tmp$3143,
      (moonbit_string_t)moonbit_string_literal_132.data
  );
  if (_tmp$4357.tag) {
    int32_t const _ok$3144 = _tmp$4357.data.ok;
  } else {
    void* const _err$3145 = _tmp$4357.data.err;
    struct moonbit_result_0 _result$4358;
    _result$4358.tag = 0;
    _result$4358.data.err = _err$3145;
    return _result$4358;
  }
  _tmp$3146
  = $moonbitlang$core$builtin$op_notequal$1(
    some_value$1291, 4294967296ll
  );
  _tmp$3147 = 0;
  _tmp$4359
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3146, _tmp$3147, (moonbit_string_t)moonbit_string_literal_133.data
  );
  if (_tmp$4359.tag) {
    int32_t const _ok$3148 = _tmp$4359.data.ok;
  } else {
    void* const _err$3149 = _tmp$4359.data.err;
    struct moonbit_result_0 _result$4360;
    _result$4360.tag = 0;
    _result$4360.data.err = _err$3149;
    return _result$4360;
  }
  if (some_value$1291 == 4294967296ll) {
    result$1293 = 0;
  } else {
    int64_t _Some$1296 = some_value$1291;
    int32_t _x$1297 = (int32_t)_Some$1296;
    x$1295 = _x$1297;
    goto $join$1294;
  }
  goto $joinlet$4361;
  $join$1294:;
  result$1293 = x$1295 * 2;
  $joinlet$4361:;
  _tmp$3150 = 0;
  _tmp$4362
  = $moonbitlang$core$builtin$assert_eq$0(
    result$1293,
      84,
      _tmp$3150,
      (moonbit_string_t)moonbit_string_literal_134.data
  );
  if (_tmp$4362.tag) {
    int32_t const _ok$3151 = _tmp$4362.data.ok;
  } else {
    void* const _err$3152 = _tmp$4362.data.err;
    struct moonbit_result_0 _result$4363;
    _result$4363.tag = 0;
    _result$4363.data.err = _err$3152;
    return _result$4363;
  }
  _tmp$3153 = 0;
  _tmp$4364
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3153, (moonbit_string_t)moonbit_string_literal_135.data
  );
  if (_tmp$4364.tag) {
    int32_t const _ok$3154 = _tmp$4364.data.ok;
  } else {
    void* const _err$3155 = _tmp$4364.data.err;
    struct moonbit_result_0 _result$4365;
    _result$4365.tag = 0;
    _result$4365.data.err = _err$3155;
    return _result$4365;
  }
  _tmp$3156 = 0;
  _tmp$4366
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3156, (moonbit_string_t)moonbit_string_literal_136.data
  );
  if (_tmp$4366.tag) {
    int32_t const _ok$3157 = _tmp$4366.data.ok;
  } else {
    void* const _err$3158 = _tmp$4366.data.err;
    struct moonbit_result_0 _result$4367;
    _result$4367.tag = 0;
    _result$4367.data.err = _err$3158;
    return _result$4367;
  }
  _tmp$3159 = 0;
  _tmp$4368
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3159, (moonbit_string_t)moonbit_string_literal_137.data
  );
  if (_tmp$4368.tag) {
    int32_t const _ok$3160 = _tmp$4368.data.ok;
  } else {
    void* const _err$3161 = _tmp$4368.data.err;
    struct moonbit_result_0 _result$4369;
    _result$4369.tag = 0;
    _result$4369.data.err = _err$3161;
    return _result$4369;
  }
  _tmp$3162 = 0;
  _tmp$4370
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3162, (moonbit_string_t)moonbit_string_literal_138.data
  );
  if (_tmp$4370.tag) {
    int32_t const _ok$3163 = _tmp$4370.data.ok;
  } else {
    void* const _err$3164 = _tmp$4370.data.err;
    struct moonbit_result_0 _result$4371;
    _result$4371.tag = 0;
    _result$4371.data.err = _err$3164;
    return _result$4371;
  }
  _tmp$3165 = 0;
  _tmp$4372
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3165, (moonbit_string_t)moonbit_string_literal_139.data
  );
  if (_tmp$4372.tag) {
    int32_t const _ok$3166 = _tmp$4372.data.ok;
  } else {
    void* const _err$3167 = _tmp$4372.data.err;
    struct moonbit_result_0 _result$4373;
    _result$4373.tag = 0;
    _result$4373.data.err = _err$3167;
    return _result$4373;
  }
  _tmp$3168 = 0;
  return $moonbitlang$core$builtin$assert_false(
           0, _tmp$3168, (moonbit_string_t)moonbit_string_literal_140.data
         );
}

int32_t $$moonbitlang$core$builtin$Show$$Double$$output(
  double self$1286,
  struct $$moonbitlang$core$builtin$Logger logger$1285
) {
  moonbit_string_t _tmp$3110 = $Double$$to_string(self$1286);
  logger$1285.$0->$method_0(logger$1285.$1, _tmp$3110);
  return 0;
}

moonbit_string_t $Double$$to_string(double self$1284) {
  return $moonbitlang$core$double$internal$ryu$ryu_to_string(self$1284);
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$ryu_to_string(
  double val$1271
) {
  uint64_t bits$1272;
  uint64_t _tmp$3109;
  uint64_t _tmp$3108;
  int32_t ieeeSign$1273;
  uint64_t ieeeMantissa$1274;
  uint64_t _tmp$3107;
  uint64_t _tmp$3106;
  int32_t ieeeExponent$1275;
  int32_t _if_result$4374;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* v$1276;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* small$1277;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3105;
  if (val$1271 == 0x0p+0) {
    return (moonbit_string_t)moonbit_string_literal_141.data;
  }
  bits$1272 = *(int64_t*)&val$1271;
  _tmp$3109 = bits$1272 >> 63;
  _tmp$3108 = _tmp$3109 & 1ull;
  ieeeSign$1273 = _tmp$3108 != 0ull;
  ieeeMantissa$1274 = bits$1272 & 4503599627370495ull;
  _tmp$3107 = bits$1272 >> 52;
  _tmp$3106 = _tmp$3107 & 2047ull;
  ieeeExponent$1275 = (int32_t)_tmp$3106;
  if (ieeeExponent$1275 == 2047) {
    _if_result$4374 = 1;
  } else if (ieeeExponent$1275 == 0) {
    _if_result$4374 = ieeeMantissa$1274 == 0ull;
  } else {
    _if_result$4374 = 0;
  }
  if (_if_result$4374) {
    int32_t _tmp$3094 = ieeeExponent$1275 != 0;
    int32_t _tmp$3095 = ieeeMantissa$1274 != 0ull;
    return $moonbitlang$core$double$internal$ryu$copy_special_str(
             ieeeSign$1273, _tmp$3094, _tmp$3095
           );
  }
  v$1276 = $moonbitlang$core$double$internal$ryu$ryu_to_string$record$1270;
  small$1277
  = $moonbitlang$core$double$internal$ryu$d2d_small_int(
    ieeeMantissa$1274, ieeeExponent$1275
  );
  if (small$1277 == 0) {
    uint32_t _tmp$3096;
    if (small$1277) {
      moonbit_decref(small$1277);
    }
    _tmp$3096 = *(uint32_t*)&ieeeExponent$1275;
    v$1276
    = $moonbitlang$core$double$internal$ryu$d2d(
      ieeeMantissa$1274, _tmp$3096
    );
  } else {
    struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _Some$1278 =
      small$1277;
    struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _f$1279 =
      _Some$1278;
    struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* x$1280 =
      _f$1279;
    while (1) {
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3104 =
        x$1280;
      uint64_t _field$3765 = _tmp$3104->$0;
      uint64_t mantissa$3103 = _field$3765;
      uint64_t q$1281 = mantissa$3103 / 10ull;
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3102 =
        x$1280;
      uint64_t _field$3764 = _tmp$3102->$0;
      uint64_t mantissa$3100 = _field$3764;
      uint64_t _tmp$3101 = 10ull * q$1281;
      uint64_t r$1282 = mantissa$3100 - _tmp$3101;
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3099;
      int32_t _field$3763;
      int32_t exponent$3098;
      int32_t _tmp$3097;
      if (r$1282 != 0ull) {
        break;
      }
      _tmp$3099 = x$1280;
      _field$3763 = _tmp$3099->$1;
      moonbit_decref(_tmp$3099);
      exponent$3098 = _field$3763;
      _tmp$3097 = exponent$3098 + 1;
      x$1280
      = (struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64*)moonbit_malloc(
          sizeof(
            struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
          )
        );
      Moonbit_object_header(x$1280)->meta
      = Moonbit_make_regular_object_header(
        sizeof(
          struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
        )
        >> 2,
          0,
          0
      );
      x$1280->$0 = q$1281;
      x$1280->$1 = _tmp$3097;
      continue;
      break;
    }
    v$1276 = x$1280;
  }
  _tmp$3105 = v$1276;
  return $moonbitlang$core$double$internal$ryu$to_chars(
           _tmp$3105, ieeeSign$1273
         );
}

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d_small_int(
  uint64_t ieeeMantissa$1265,
  int32_t ieeeExponent$1267
) {
  uint64_t m2$1264 = 4503599627370496ull | ieeeMantissa$1265;
  int32_t _tmp$3093 = ieeeExponent$1267 - 1023;
  int32_t e2$1266 = _tmp$3093 - 52;
  int32_t _tmp$3092;
  uint64_t _tmp$3091;
  uint64_t mask$1268;
  uint64_t fraction$1269;
  int32_t _tmp$3090;
  uint64_t _tmp$3089;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3088;
  if (e2$1266 > 0) {
    return 0;
  }
  if (e2$1266 < -52) {
    return 0;
  }
  _tmp$3092 = -e2$1266;
  _tmp$3091 = 1ull << (_tmp$3092 & 63);
  mask$1268 = _tmp$3091 - 1ull;
  fraction$1269 = m2$1264 & mask$1268;
  if (fraction$1269 != 0ull) {
    return 0;
  }
  _tmp$3090 = -e2$1266;
  _tmp$3089 = m2$1264 >> (_tmp$3090 & 63);
  _tmp$3088
  = (struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64)
    );
  Moonbit_object_header(_tmp$3088)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
    )
    >> 2,
      0,
      0
  );
  _tmp$3088->$0 = _tmp$3089;
  _tmp$3088->$1 = 0;
  return _tmp$3088;
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$to_chars(
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* v$1238,
  int32_t sign$1236
) {
  int32_t _tmp$3087 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  moonbit_bytes_t result$1234 =
    (moonbit_bytes_t)moonbit_make_bytes(25, _tmp$3087);
  int32_t index$1235 = 0;
  uint64_t output$1237;
  uint64_t _tmp$3086;
  int32_t olength$1239;
  int32_t _field$3766;
  int32_t exponent$3085;
  int32_t _tmp$3084;
  int32_t exp$1240;
  int32_t _tmp$3083;
  int32_t _tmp$3081;
  int32_t scientificNotation$1241;
  if (sign$1236) {
    int32_t _tmp$2956 = index$1235;
    int32_t _tmp$2957;
    if (_tmp$2956 < 0 || _tmp$2956 >= Moonbit_array_length(result$1234)) {
      moonbit_panic();
    }
    result$1234[_tmp$2956] = 45;
    _tmp$2957 = index$1235;
    index$1235 = _tmp$2957 + 1;
  }
  output$1237 = v$1238->$0;
  _tmp$3086 = output$1237;
  olength$1239
  = $moonbitlang$core$double$internal$ryu$decimal_length17(
    _tmp$3086
  );
  _field$3766 = v$1238->$1;
  moonbit_decref(v$1238);
  exponent$3085 = _field$3766;
  _tmp$3084 = exponent$3085 + olength$1239;
  exp$1240 = _tmp$3084 - 1;
  _tmp$3083 = exp$1240;
  if (_tmp$3083 >= -6) {
    int32_t _tmp$3082 = exp$1240;
    _tmp$3081 = _tmp$3082 < 21;
  } else {
    _tmp$3081 = 0;
  }
  scientificNotation$1241 = !_tmp$3081;
  if (scientificNotation$1241) {
    int32_t _end41$1242 = olength$1239 - 1;
    int32_t i$1243 = 0;
    int32_t _tmp$2967;
    uint64_t _tmp$2972;
    int32_t _tmp$2971;
    int32_t _tmp$2970;
    int32_t _tmp$2969;
    int32_t _tmp$2968;
    int32_t _tmp$2976;
    int32_t _tmp$2977;
    int32_t _tmp$2978;
    int32_t _tmp$2979;
    int32_t _tmp$2980;
    int32_t _tmp$2986;
    int32_t _tmp$3019;
    while (1) {
      if (i$1243 < _end41$1242) {
        uint64_t _tmp$2965 = output$1237;
        uint64_t c$1244 = _tmp$2965 % 10ull;
        uint64_t _tmp$2958 = output$1237;
        int32_t _tmp$2964;
        int32_t _tmp$2963;
        int32_t _tmp$2959;
        int32_t _tmp$2962;
        int32_t _tmp$2961;
        int32_t _tmp$2960;
        int32_t _tmp$2966;
        output$1237 = _tmp$2958 / 10ull;
        _tmp$2964 = index$1235;
        _tmp$2963 = _tmp$2964 + olength$1239;
        _tmp$2959 = _tmp$2963 - i$1243;
        _tmp$2962 = (int32_t)c$1244;
        _tmp$2961 = 48 + _tmp$2962;
        _tmp$2960 = _tmp$2961 & 0xff;
        if (_tmp$2959 < 0 || _tmp$2959 >= Moonbit_array_length(result$1234)) {
          moonbit_panic();
        }
        result$1234[_tmp$2959] = _tmp$2960;
        _tmp$2966 = i$1243 + 1;
        i$1243 = _tmp$2966;
        continue;
      }
      break;
    }
    _tmp$2967 = index$1235;
    _tmp$2972 = output$1237;
    _tmp$2971 = (int32_t)_tmp$2972;
    _tmp$2970 = _tmp$2971 % 10;
    _tmp$2969 = 48 + _tmp$2970;
    _tmp$2968 = _tmp$2969 & 0xff;
    if (_tmp$2967 < 0 || _tmp$2967 >= Moonbit_array_length(result$1234)) {
      moonbit_panic();
    }
    result$1234[_tmp$2967] = _tmp$2968;
    if (olength$1239 > 1) {
      int32_t _tmp$2974 = index$1235;
      int32_t _tmp$2973 = _tmp$2974 + 1;
      if (_tmp$2973 < 0 || _tmp$2973 >= Moonbit_array_length(result$1234)) {
        moonbit_panic();
      }
      result$1234[_tmp$2973] = 46;
    } else {
      int32_t _tmp$2975 = index$1235;
      index$1235 = _tmp$2975 - 1;
    }
    _tmp$2976 = index$1235;
    _tmp$2977 = olength$1239 + 1;
    index$1235 = _tmp$2976 + _tmp$2977;
    _tmp$2978 = index$1235;
    if (_tmp$2978 < 0 || _tmp$2978 >= Moonbit_array_length(result$1234)) {
      moonbit_panic();
    }
    result$1234[_tmp$2978] = 101;
    _tmp$2979 = index$1235;
    index$1235 = _tmp$2979 + 1;
    _tmp$2980 = exp$1240;
    if (_tmp$2980 < 0) {
      int32_t _tmp$2981 = index$1235;
      int32_t _tmp$2982;
      int32_t _tmp$2983;
      if (_tmp$2981 < 0 || _tmp$2981 >= Moonbit_array_length(result$1234)) {
        moonbit_panic();
      }
      result$1234[_tmp$2981] = 45;
      _tmp$2982 = index$1235;
      index$1235 = _tmp$2982 + 1;
      _tmp$2983 = exp$1240;
      exp$1240 = -_tmp$2983;
    } else {
      int32_t _tmp$2984 = index$1235;
      int32_t _tmp$2985;
      if (_tmp$2984 < 0 || _tmp$2984 >= Moonbit_array_length(result$1234)) {
        moonbit_panic();
      }
      result$1234[_tmp$2984] = 43;
      _tmp$2985 = index$1235;
      index$1235 = _tmp$2985 + 1;
    }
    _tmp$2986 = exp$1240;
    if (_tmp$2986 >= 100) {
      int32_t _tmp$3002 = exp$1240;
      int32_t a$1246 = _tmp$3002 / 100;
      int32_t _tmp$3001 = exp$1240;
      int32_t _tmp$3000 = _tmp$3001 / 10;
      int32_t b$1247 = _tmp$3000 % 10;
      int32_t _tmp$2999 = exp$1240;
      int32_t c$1248 = _tmp$2999 % 10;
      int32_t _tmp$2987 = index$1235;
      int32_t _tmp$2989 = 48 + a$1246;
      int32_t _tmp$2988 = _tmp$2989 & 0xff;
      int32_t _tmp$2993;
      int32_t _tmp$2990;
      int32_t _tmp$2992;
      int32_t _tmp$2991;
      int32_t _tmp$2997;
      int32_t _tmp$2994;
      int32_t _tmp$2996;
      int32_t _tmp$2995;
      int32_t _tmp$2998;
      if (_tmp$2987 < 0 || _tmp$2987 >= Moonbit_array_length(result$1234)) {
        moonbit_panic();
      }
      result$1234[_tmp$2987] = _tmp$2988;
      _tmp$2993 = index$1235;
      _tmp$2990 = _tmp$2993 + 1;
      _tmp$2992 = 48 + b$1247;
      _tmp$2991 = _tmp$2992 & 0xff;
      if (_tmp$2990 < 0 || _tmp$2990 >= Moonbit_array_length(result$1234)) {
        moonbit_panic();
      }
      result$1234[_tmp$2990] = _tmp$2991;
      _tmp$2997 = index$1235;
      _tmp$2994 = _tmp$2997 + 2;
      _tmp$2996 = 48 + c$1248;
      _tmp$2995 = _tmp$2996 & 0xff;
      if (_tmp$2994 < 0 || _tmp$2994 >= Moonbit_array_length(result$1234)) {
        moonbit_panic();
      }
      result$1234[_tmp$2994] = _tmp$2995;
      _tmp$2998 = index$1235;
      index$1235 = _tmp$2998 + 3;
    } else {
      int32_t _tmp$3003 = exp$1240;
      if (_tmp$3003 >= 10) {
        int32_t _tmp$3013 = exp$1240;
        int32_t a$1249 = _tmp$3013 / 10;
        int32_t _tmp$3012 = exp$1240;
        int32_t b$1250 = _tmp$3012 % 10;
        int32_t _tmp$3004 = index$1235;
        int32_t _tmp$3006 = 48 + a$1249;
        int32_t _tmp$3005 = _tmp$3006 & 0xff;
        int32_t _tmp$3010;
        int32_t _tmp$3007;
        int32_t _tmp$3009;
        int32_t _tmp$3008;
        int32_t _tmp$3011;
        if (_tmp$3004 < 0 || _tmp$3004 >= Moonbit_array_length(result$1234)) {
          moonbit_panic();
        }
        result$1234[_tmp$3004] = _tmp$3005;
        _tmp$3010 = index$1235;
        _tmp$3007 = _tmp$3010 + 1;
        _tmp$3009 = 48 + b$1250;
        _tmp$3008 = _tmp$3009 & 0xff;
        if (_tmp$3007 < 0 || _tmp$3007 >= Moonbit_array_length(result$1234)) {
          moonbit_panic();
        }
        result$1234[_tmp$3007] = _tmp$3008;
        _tmp$3011 = index$1235;
        index$1235 = _tmp$3011 + 2;
      } else {
        int32_t _tmp$3014 = index$1235;
        int32_t _tmp$3017 = exp$1240;
        int32_t _tmp$3016 = 48 + _tmp$3017;
        int32_t _tmp$3015 = _tmp$3016 & 0xff;
        int32_t _tmp$3018;
        if (_tmp$3014 < 0 || _tmp$3014 >= Moonbit_array_length(result$1234)) {
          moonbit_panic();
        }
        result$1234[_tmp$3014] = _tmp$3015;
        _tmp$3018 = index$1235;
        index$1235 = _tmp$3018 + 1;
      }
    }
    _tmp$3019 = index$1235;
    return $moonbitlang$core$double$internal$ryu$string_from_bytes(
             result$1234, 0, _tmp$3019
           );
  } else {
    int32_t _tmp$3020 = exp$1240;
    int32_t _tmp$3080;
    if (_tmp$3020 < 0) {
      int32_t _tmp$3021 = index$1235;
      int32_t _tmp$3022;
      int32_t _tmp$3023;
      int32_t _tmp$3024;
      int32_t i$1251;
      int32_t current$1253;
      int32_t i$1254;
      if (_tmp$3021 < 0 || _tmp$3021 >= Moonbit_array_length(result$1234)) {
        moonbit_panic();
      }
      result$1234[_tmp$3021] = 48;
      _tmp$3022 = index$1235;
      index$1235 = _tmp$3022 + 1;
      _tmp$3023 = index$1235;
      if (_tmp$3023 < 0 || _tmp$3023 >= Moonbit_array_length(result$1234)) {
        moonbit_panic();
      }
      result$1234[_tmp$3023] = 46;
      _tmp$3024 = index$1235;
      index$1235 = _tmp$3024 + 1;
      i$1251 = -1;
      while (1) {
        int32_t _tmp$3025 = exp$1240;
        if (i$1251 > _tmp$3025) {
          int32_t _tmp$3026 = index$1235;
          int32_t _tmp$3027;
          int32_t _tmp$3028;
          if (
            _tmp$3026 < 0 || _tmp$3026 >= Moonbit_array_length(result$1234)
          ) {
            moonbit_panic();
          }
          result$1234[_tmp$3026] = 48;
          _tmp$3027 = index$1235;
          index$1235 = _tmp$3027 + 1;
          _tmp$3028 = i$1251 - 1;
          i$1251 = _tmp$3028;
          continue;
        }
        break;
      }
      current$1253 = index$1235;
      i$1254 = 0;
      while (1) {
        if (i$1254 < olength$1239) {
          int32_t _tmp$3036 = current$1253 + olength$1239;
          int32_t _tmp$3035 = _tmp$3036 - i$1254;
          int32_t _tmp$3029 = _tmp$3035 - 1;
          uint64_t _tmp$3034 = output$1237;
          uint64_t _tmp$3033 = _tmp$3034 % 10ull;
          int32_t _tmp$3032 = (int32_t)_tmp$3033;
          int32_t _tmp$3031 = 48 + _tmp$3032;
          int32_t _tmp$3030 = _tmp$3031 & 0xff;
          uint64_t _tmp$3037;
          int32_t _tmp$3038;
          int32_t _tmp$3039;
          if (
            _tmp$3029 < 0 || _tmp$3029 >= Moonbit_array_length(result$1234)
          ) {
            moonbit_panic();
          }
          result$1234[_tmp$3029] = _tmp$3030;
          _tmp$3037 = output$1237;
          output$1237 = _tmp$3037 / 10ull;
          _tmp$3038 = index$1235;
          index$1235 = _tmp$3038 + 1;
          _tmp$3039 = i$1254 + 1;
          i$1254 = _tmp$3039;
          continue;
        }
        break;
      }
    } else {
      int32_t _tmp$3041 = exp$1240;
      int32_t _tmp$3040 = _tmp$3041 + 1;
      if (_tmp$3040 >= olength$1239) {
        int32_t i$1256 = 0;
        int32_t _tmp$3053;
        int32_t _tmp$3057;
        int32_t _end64$1258;
        int32_t i$1259;
        while (1) {
          if (i$1256 < olength$1239) {
            int32_t _tmp$3050 = index$1235;
            int32_t _tmp$3049 = _tmp$3050 + olength$1239;
            int32_t _tmp$3048 = _tmp$3049 - i$1256;
            int32_t _tmp$3042 = _tmp$3048 - 1;
            uint64_t _tmp$3047 = output$1237;
            uint64_t _tmp$3046 = _tmp$3047 % 10ull;
            int32_t _tmp$3045 = (int32_t)_tmp$3046;
            int32_t _tmp$3044 = 48 + _tmp$3045;
            int32_t _tmp$3043 = _tmp$3044 & 0xff;
            uint64_t _tmp$3051;
            int32_t _tmp$3052;
            if (
              _tmp$3042 < 0 || _tmp$3042 >= Moonbit_array_length(result$1234)
            ) {
              moonbit_panic();
            }
            result$1234[_tmp$3042] = _tmp$3043;
            _tmp$3051 = output$1237;
            output$1237 = _tmp$3051 / 10ull;
            _tmp$3052 = i$1256 + 1;
            i$1256 = _tmp$3052;
            continue;
          }
          break;
        }
        _tmp$3053 = index$1235;
        index$1235 = _tmp$3053 + olength$1239;
        _tmp$3057 = exp$1240;
        _end64$1258 = _tmp$3057 + 1;
        i$1259 = olength$1239;
        while (1) {
          if (i$1259 < _end64$1258) {
            int32_t _tmp$3054 = index$1235;
            int32_t _tmp$3055;
            int32_t _tmp$3056;
            if (
              _tmp$3054 < 0 || _tmp$3054 >= Moonbit_array_length(result$1234)
            ) {
              moonbit_panic();
            }
            result$1234[_tmp$3054] = 48;
            _tmp$3055 = index$1235;
            index$1235 = _tmp$3055 + 1;
            _tmp$3056 = i$1259 + 1;
            i$1259 = _tmp$3056;
            continue;
          }
          break;
        }
      } else {
        int32_t _tmp$3079 = index$1235;
        int32_t current$1261 = _tmp$3079 + 1;
        int32_t i$1262 = 0;
        int32_t _tmp$3077;
        int32_t _tmp$3078;
        while (1) {
          if (i$1262 < olength$1239) {
            int32_t _tmp$3060 = olength$1239 - i$1262;
            int32_t _tmp$3058 = _tmp$3060 - 1;
            int32_t _tmp$3059 = exp$1240;
            int32_t _tmp$3074;
            int32_t _tmp$3073;
            int32_t _tmp$3072;
            int32_t _tmp$3066;
            uint64_t _tmp$3071;
            uint64_t _tmp$3070;
            int32_t _tmp$3069;
            int32_t _tmp$3068;
            int32_t _tmp$3067;
            uint64_t _tmp$3075;
            int32_t _tmp$3076;
            if (_tmp$3058 == _tmp$3059) {
              int32_t _tmp$3064 = current$1261;
              int32_t _tmp$3063 = _tmp$3064 + olength$1239;
              int32_t _tmp$3062 = _tmp$3063 - i$1262;
              int32_t _tmp$3061 = _tmp$3062 - 1;
              int32_t _tmp$3065;
              if (
                _tmp$3061 < 0
                || _tmp$3061 >= Moonbit_array_length(result$1234)
              ) {
                moonbit_panic();
              }
              result$1234[_tmp$3061] = 46;
              _tmp$3065 = current$1261;
              current$1261 = _tmp$3065 - 1;
            }
            _tmp$3074 = current$1261;
            _tmp$3073 = _tmp$3074 + olength$1239;
            _tmp$3072 = _tmp$3073 - i$1262;
            _tmp$3066 = _tmp$3072 - 1;
            _tmp$3071 = output$1237;
            _tmp$3070 = _tmp$3071 % 10ull;
            _tmp$3069 = (int32_t)_tmp$3070;
            _tmp$3068 = 48 + _tmp$3069;
            _tmp$3067 = _tmp$3068 & 0xff;
            if (
              _tmp$3066 < 0 || _tmp$3066 >= Moonbit_array_length(result$1234)
            ) {
              moonbit_panic();
            }
            result$1234[_tmp$3066] = _tmp$3067;
            _tmp$3075 = output$1237;
            output$1237 = _tmp$3075 / 10ull;
            _tmp$3076 = i$1262 + 1;
            i$1262 = _tmp$3076;
            continue;
          }
          break;
        }
        _tmp$3077 = index$1235;
        _tmp$3078 = olength$1239 + 1;
        index$1235 = _tmp$3077 + _tmp$3078;
      }
    }
    _tmp$3080 = index$1235;
    return $moonbitlang$core$double$internal$ryu$string_from_bytes(
             result$1234, 0, _tmp$3080
           );
  }
}

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d(
  uint64_t ieeeMantissa$1180,
  uint32_t ieeeExponent$1179
) {
  int32_t e2$1177 = 0;
  uint64_t m2$1178 = 0ull;
  uint64_t _tmp$2955;
  uint64_t _tmp$2954;
  int32_t even$1181;
  uint64_t _tmp$2953;
  uint64_t mv$1182;
  int32_t mmShift$1183;
  uint64_t vr$1184;
  uint64_t vp$1185;
  uint64_t vm$1186;
  int32_t e10$1187;
  int32_t vmIsTrailingZeros$1188;
  int32_t vrIsTrailingZeros$1189;
  int32_t _tmp$2855;
  int32_t removed$1208;
  int32_t lastRemovedDigit$1209;
  uint64_t output$1210;
  int32_t _tmp$2951;
  int32_t _tmp$2952;
  int32_t exp$1233;
  uint64_t _tmp$2950;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _block$4387;
  if (ieeeExponent$1179 == 0u) {
    e2$1177 = -1076;
    m2$1178 = ieeeMantissa$1180;
  } else {
    int32_t _tmp$2854 = *(int32_t*)&ieeeExponent$1179;
    int32_t _tmp$2853 = _tmp$2854 - 1023;
    int32_t _tmp$2852 = _tmp$2853 - 52;
    e2$1177 = _tmp$2852 - 2;
    m2$1178 = 4503599627370496ull | ieeeMantissa$1180;
  }
  _tmp$2955 = m2$1178;
  _tmp$2954 = _tmp$2955 & 1ull;
  even$1181 = _tmp$2954 == 0ull;
  _tmp$2953 = m2$1178;
  mv$1182 = 4ull * _tmp$2953;
  if (ieeeMantissa$1180 != 0ull) {
    mmShift$1183 = 1;
  } else {
    mmShift$1183 = ieeeExponent$1179 <= 1u;
  }
  vr$1184 = 0ull;
  vp$1185 = 0ull;
  vm$1186 = 0ull;
  e10$1187 = 0;
  vmIsTrailingZeros$1188 = 0;
  vrIsTrailingZeros$1189 = 0;
  _tmp$2855 = e2$1177;
  if (_tmp$2855 >= 0) {
    int32_t _tmp$2877 = e2$1177;
    int32_t _tmp$2873 =
      $moonbitlang$core$double$internal$ryu$log10Pow2(_tmp$2877);
    int32_t _tmp$2876 = e2$1177;
    int32_t _tmp$2875 = _tmp$2876 > 3;
    int32_t _tmp$2874 = $Bool$$to_int(_tmp$2875);
    int32_t q$1190 = _tmp$2873 - _tmp$2874;
    int32_t _tmp$2872;
    int32_t _tmp$2871;
    int32_t k$1191;
    int32_t _tmp$2870;
    int32_t _tmp$2869;
    int32_t _tmp$2868;
    int32_t i$1192;
    struct $$moonbitlang$core$double$internal$ryu$Pow5Pair pow5$1193;
    uint64_t _tmp$2867;
    struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result _bind$1194;
    uint64_t _vrOut$1195;
    uint64_t _vpOut$1196;
    uint64_t _vmOut$1197;
    e10$1187 = q$1190;
    _tmp$2872 = $moonbitlang$core$double$internal$ryu$pow5bits(q$1190);
    _tmp$2871 = 125 + _tmp$2872;
    k$1191 = _tmp$2871 - 1;
    _tmp$2870 = e2$1177;
    _tmp$2869 = -_tmp$2870;
    _tmp$2868 = _tmp$2869 + q$1190;
    i$1192 = _tmp$2868 + k$1191;
    pow5$1193
    = $moonbitlang$core$double$internal$ryu$double_computeInvPow5(
      q$1190
    );
    _tmp$2867 = m2$1178;
    _bind$1194
    = $moonbitlang$core$double$internal$ryu$mulShiftAll64(
      _tmp$2867, pow5$1193, i$1192, mmShift$1183
    );
    _vrOut$1195 = _bind$1194.$0;
    _vpOut$1196 = _bind$1194.$1;
    _vmOut$1197 = _bind$1194.$2;
    vr$1184 = _vrOut$1195;
    vp$1185 = _vpOut$1196;
    vm$1186 = _vmOut$1197;
    if (q$1190 <= 21) {
      int32_t _tmp$2863 = (int32_t)mv$1182;
      uint64_t _tmp$2866 = mv$1182 / 5ull;
      int32_t _tmp$2865 = (int32_t)_tmp$2866;
      int32_t _tmp$2864 = 5 * _tmp$2865;
      int32_t mvMod5$1198 = _tmp$2863 - _tmp$2864;
      if (mvMod5$1198 == 0) {
        vrIsTrailingZeros$1189
        = $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
          mv$1182, q$1190
        );
      } else if (even$1181) {
        uint64_t _tmp$2857 = mv$1182 - 1ull;
        uint64_t _tmp$2858 = $Bool$$to_uint64(mmShift$1183);
        uint64_t _tmp$2856 = _tmp$2857 - _tmp$2858;
        vmIsTrailingZeros$1188
        = $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
          _tmp$2856, q$1190
        );
      } else {
        uint64_t _tmp$2859 = vp$1185;
        uint64_t _tmp$2862 = mv$1182 + 2ull;
        int32_t _tmp$2861 =
          $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
            _tmp$2862, q$1190
          );
        uint64_t _tmp$2860 = $Bool$$to_uint64(_tmp$2861);
        vp$1185 = _tmp$2859 - _tmp$2860;
      }
    }
  } else {
    int32_t _tmp$2891 = e2$1177;
    int32_t _tmp$2890 = -_tmp$2891;
    int32_t _tmp$2885 =
      $moonbitlang$core$double$internal$ryu$log10Pow5(_tmp$2890);
    int32_t _tmp$2889 = e2$1177;
    int32_t _tmp$2888 = -_tmp$2889;
    int32_t _tmp$2887 = _tmp$2888 > 1;
    int32_t _tmp$2886 = $Bool$$to_int(_tmp$2887);
    int32_t q$1199 = _tmp$2885 - _tmp$2886;
    int32_t _tmp$2878 = e2$1177;
    int32_t _tmp$2884;
    int32_t _tmp$2883;
    int32_t i$1200;
    int32_t _tmp$2882;
    int32_t k$1201;
    int32_t j$1202;
    struct $$moonbitlang$core$double$internal$ryu$Pow5Pair pow5$1203;
    uint64_t _tmp$2881;
    struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result _bind$1204;
    uint64_t _vrOut$1205;
    uint64_t _vpOut$1206;
    uint64_t _vmOut$1207;
    e10$1187 = q$1199 + _tmp$2878;
    _tmp$2884 = e2$1177;
    _tmp$2883 = -_tmp$2884;
    i$1200 = _tmp$2883 - q$1199;
    _tmp$2882 = $moonbitlang$core$double$internal$ryu$pow5bits(i$1200);
    k$1201 = _tmp$2882 - 125;
    j$1202 = q$1199 - k$1201;
    pow5$1203
    = $moonbitlang$core$double$internal$ryu$double_computePow5(
      i$1200
    );
    _tmp$2881 = m2$1178;
    _bind$1204
    = $moonbitlang$core$double$internal$ryu$mulShiftAll64(
      _tmp$2881, pow5$1203, j$1202, mmShift$1183
    );
    _vrOut$1205 = _bind$1204.$0;
    _vpOut$1206 = _bind$1204.$1;
    _vmOut$1207 = _bind$1204.$2;
    vr$1184 = _vrOut$1205;
    vp$1185 = _vpOut$1206;
    vm$1186 = _vmOut$1207;
    if (q$1199 <= 1) {
      vrIsTrailingZeros$1189 = 1;
      if (even$1181) {
        int32_t _tmp$2879 = $Bool$$to_int(mmShift$1183);
        vmIsTrailingZeros$1188 = _tmp$2879 == 1;
      } else {
        uint64_t _tmp$2880 = vp$1185;
        vp$1185 = _tmp$2880 - 1ull;
      }
    } else if (q$1199 < 63) {
      vrIsTrailingZeros$1189
      = $moonbitlang$core$double$internal$ryu$multipleOfPowerOf2(
        mv$1182, q$1199
      );
    }
  }
  removed$1208 = 0;
  lastRemovedDigit$1209 = 0;
  output$1210 = 0ull;
  if (vmIsTrailingZeros$1188 || vrIsTrailingZeros$1189) {
    int32_t _if_result$4384;
    uint64_t _tmp$2921;
    uint64_t _tmp$2927;
    uint64_t _tmp$2928;
    int32_t _if_result$4385;
    int32_t _tmp$2924;
    int64_t _tmp$2923;
    uint64_t _tmp$2922;
    while (1) {
      uint64_t _tmp$2904 = vp$1185;
      uint64_t vpDiv10$1211 = _tmp$2904 / 10ull;
      uint64_t _tmp$2903 = vm$1186;
      uint64_t vmDiv10$1212 = _tmp$2903 / 10ull;
      uint64_t _tmp$2902;
      int32_t _tmp$2899;
      int32_t _tmp$2901;
      int32_t _tmp$2900;
      int32_t vmMod10$1214;
      uint64_t _tmp$2898;
      uint64_t vrDiv10$1215;
      uint64_t _tmp$2897;
      int32_t _tmp$2894;
      int32_t _tmp$2896;
      int32_t _tmp$2895;
      int32_t vrMod10$1216;
      int32_t _tmp$2893;
      if (vpDiv10$1211 <= vmDiv10$1212) {
        break;
      }
      _tmp$2902 = vm$1186;
      _tmp$2899 = (int32_t)_tmp$2902;
      _tmp$2901 = (int32_t)vmDiv10$1212;
      _tmp$2900 = 10 * _tmp$2901;
      vmMod10$1214 = _tmp$2899 - _tmp$2900;
      _tmp$2898 = vr$1184;
      vrDiv10$1215 = _tmp$2898 / 10ull;
      _tmp$2897 = vr$1184;
      _tmp$2894 = (int32_t)_tmp$2897;
      _tmp$2896 = (int32_t)vrDiv10$1215;
      _tmp$2895 = 10 * _tmp$2896;
      vrMod10$1216 = _tmp$2894 - _tmp$2895;
      if (vmIsTrailingZeros$1188) {
        vmIsTrailingZeros$1188 = vmMod10$1214 == 0;
      } else {
        vmIsTrailingZeros$1188 = 0;
      }
      if (vrIsTrailingZeros$1189) {
        int32_t _tmp$2892 = lastRemovedDigit$1209;
        vrIsTrailingZeros$1189 = _tmp$2892 == 0;
      } else {
        vrIsTrailingZeros$1189 = 0;
      }
      lastRemovedDigit$1209 = vrMod10$1216;
      vr$1184 = vrDiv10$1215;
      vp$1185 = vpDiv10$1211;
      vm$1186 = vmDiv10$1212;
      _tmp$2893 = removed$1208;
      removed$1208 = _tmp$2893 + 1;
      continue;
      break;
    }
    if (vmIsTrailingZeros$1188) {
      while (1) {
        uint64_t _tmp$2917 = vm$1186;
        uint64_t vmDiv10$1217 = _tmp$2917 / 10ull;
        uint64_t _tmp$2916 = vm$1186;
        int32_t _tmp$2913 = (int32_t)_tmp$2916;
        int32_t _tmp$2915 = (int32_t)vmDiv10$1217;
        int32_t _tmp$2914 = 10 * _tmp$2915;
        int32_t vmMod10$1218 = _tmp$2913 - _tmp$2914;
        uint64_t _tmp$2912;
        uint64_t vpDiv10$1220;
        uint64_t _tmp$2911;
        uint64_t vrDiv10$1221;
        uint64_t _tmp$2910;
        int32_t _tmp$2907;
        int32_t _tmp$2909;
        int32_t _tmp$2908;
        int32_t vrMod10$1222;
        int32_t _tmp$2906;
        if (vmMod10$1218 != 0) {
          break;
        }
        _tmp$2912 = vp$1185;
        vpDiv10$1220 = _tmp$2912 / 10ull;
        _tmp$2911 = vr$1184;
        vrDiv10$1221 = _tmp$2911 / 10ull;
        _tmp$2910 = vr$1184;
        _tmp$2907 = (int32_t)_tmp$2910;
        _tmp$2909 = (int32_t)vrDiv10$1221;
        _tmp$2908 = 10 * _tmp$2909;
        vrMod10$1222 = _tmp$2907 - _tmp$2908;
        if (vrIsTrailingZeros$1189) {
          int32_t _tmp$2905 = lastRemovedDigit$1209;
          vrIsTrailingZeros$1189 = _tmp$2905 == 0;
        } else {
          vrIsTrailingZeros$1189 = 0;
        }
        lastRemovedDigit$1209 = vrMod10$1222;
        vr$1184 = vrDiv10$1221;
        vp$1185 = vpDiv10$1220;
        vm$1186 = vmDiv10$1217;
        _tmp$2906 = removed$1208;
        removed$1208 = _tmp$2906 + 1;
        continue;
        break;
      }
    }
    if (vrIsTrailingZeros$1189) {
      int32_t _tmp$2920 = lastRemovedDigit$1209;
      if (_tmp$2920 == 5) {
        uint64_t _tmp$2919 = vr$1184;
        uint64_t _tmp$2918 = _tmp$2919 % 2ull;
        _if_result$4384 = _tmp$2918 == 0ull;
      } else {
        _if_result$4384 = 0;
      }
    } else {
      _if_result$4384 = 0;
    }
    if (_if_result$4384) {
      lastRemovedDigit$1209 = 4;
    }
    _tmp$2921 = vr$1184;
    _tmp$2927 = vr$1184;
    _tmp$2928 = vm$1186;
    if (_tmp$2927 == _tmp$2928) {
      if (!even$1181) {
        _if_result$4385 = 1;
      } else {
        int32_t _tmp$2926 = vmIsTrailingZeros$1188;
        _if_result$4385 = !_tmp$2926;
      }
    } else {
      _if_result$4385 = 0;
    }
    if (_if_result$4385) {
      _tmp$2924 = 1;
    } else {
      int32_t _tmp$2925 = lastRemovedDigit$1209;
      _tmp$2924 = _tmp$2925 >= 5;
    }
    _tmp$2923 = $Bool$$to_int64(_tmp$2924);
    _tmp$2922 = *(uint64_t*)&_tmp$2923;
    output$1210 = _tmp$2921 + _tmp$2922;
  } else {
    int32_t roundUp$1223 = 0;
    uint64_t _tmp$2949 = vp$1185;
    uint64_t vpDiv100$1224 = _tmp$2949 / 100ull;
    uint64_t _tmp$2948 = vm$1186;
    uint64_t vmDiv100$1225 = _tmp$2948 / 100ull;
    uint64_t _tmp$2943;
    uint64_t _tmp$2946;
    uint64_t _tmp$2947;
    int32_t _tmp$2945;
    uint64_t _tmp$2944;
    if (vpDiv100$1224 > vmDiv100$1225) {
      uint64_t _tmp$2934 = vr$1184;
      uint64_t vrDiv100$1226 = _tmp$2934 / 100ull;
      uint64_t _tmp$2933 = vr$1184;
      int32_t _tmp$2930 = (int32_t)_tmp$2933;
      int32_t _tmp$2932 = (int32_t)vrDiv100$1226;
      int32_t _tmp$2931 = 100 * _tmp$2932;
      int32_t vrMod100$1227 = _tmp$2930 - _tmp$2931;
      int32_t _tmp$2929;
      roundUp$1223 = vrMod100$1227 >= 50;
      vr$1184 = vrDiv100$1226;
      vp$1185 = vpDiv100$1224;
      vm$1186 = vmDiv100$1225;
      _tmp$2929 = removed$1208;
      removed$1208 = _tmp$2929 + 2;
    }
    while (1) {
      uint64_t _tmp$2942 = vp$1185;
      uint64_t vpDiv10$1228 = _tmp$2942 / 10ull;
      uint64_t _tmp$2941 = vm$1186;
      uint64_t vmDiv10$1229 = _tmp$2941 / 10ull;
      uint64_t _tmp$2940;
      uint64_t vrDiv10$1231;
      uint64_t _tmp$2939;
      int32_t _tmp$2936;
      int32_t _tmp$2938;
      int32_t _tmp$2937;
      int32_t vrMod10$1232;
      int32_t _tmp$2935;
      if (vpDiv10$1228 <= vmDiv10$1229) {
        break;
      }
      _tmp$2940 = vr$1184;
      vrDiv10$1231 = _tmp$2940 / 10ull;
      _tmp$2939 = vr$1184;
      _tmp$2936 = (int32_t)_tmp$2939;
      _tmp$2938 = (int32_t)vrDiv10$1231;
      _tmp$2937 = 10 * _tmp$2938;
      vrMod10$1232 = _tmp$2936 - _tmp$2937;
      roundUp$1223 = vrMod10$1232 >= 5;
      vr$1184 = vrDiv10$1231;
      vp$1185 = vpDiv10$1228;
      vm$1186 = vmDiv10$1229;
      _tmp$2935 = removed$1208;
      removed$1208 = _tmp$2935 + 1;
      continue;
      break;
    }
    _tmp$2943 = vr$1184;
    _tmp$2946 = vr$1184;
    _tmp$2947 = vm$1186;
    _tmp$2945 = _tmp$2946 == _tmp$2947 || roundUp$1223;
    _tmp$2944 = $Bool$$to_uint64(_tmp$2945);
    output$1210 = _tmp$2943 + _tmp$2944;
  }
  _tmp$2951 = e10$1187;
  _tmp$2952 = removed$1208;
  exp$1233 = _tmp$2951 + _tmp$2952;
  _tmp$2950 = output$1210;
  _block$4387
  = (struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64)
    );
  Moonbit_object_header(_block$4387)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
    )
    >> 2,
      0,
      0
  );
  _block$4387->$0 = _tmp$2950;
  _block$4387->$1 = exp$1233;
  return _block$4387;
}

int32_t $moonbitlang$core$double$internal$ryu$decimal_length17(
  uint64_t v$1176
) {
  if (v$1176 >= 10000000000000000ull) {
    return 17;
  }
  if (v$1176 >= 1000000000000000ull) {
    return 16;
  }
  if (v$1176 >= 100000000000000ull) {
    return 15;
  }
  if (v$1176 >= 10000000000000ull) {
    return 14;
  }
  if (v$1176 >= 1000000000000ull) {
    return 13;
  }
  if (v$1176 >= 100000000000ull) {
    return 12;
  }
  if (v$1176 >= 10000000000ull) {
    return 11;
  }
  if (v$1176 >= 1000000000ull) {
    return 10;
  }
  if (v$1176 >= 100000000ull) {
    return 9;
  }
  if (v$1176 >= 10000000ull) {
    return 8;
  }
  if (v$1176 >= 1000000ull) {
    return 7;
  }
  if (v$1176 >= 100000ull) {
    return 6;
  }
  if (v$1176 >= 10000ull) {
    return 5;
  }
  if (v$1176 >= 1000ull) {
    return 4;
  }
  if (v$1176 >= 100ull) {
    return 3;
  }
  if (v$1176 >= 10ull) {
    return 2;
  }
  return 1;
}

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computeInvPow5(
  int32_t i$1159
) {
  int32_t _tmp$2851 = i$1159 + 26;
  int32_t _tmp$2850 = _tmp$2851 - 1;
  int32_t base$1158 = _tmp$2850 / 26;
  int32_t base2$1160 = base$1158 * 26;
  int32_t offset$1161 = base2$1160 - i$1159;
  int32_t _tmp$2849 = base$1158 * 2;
  uint64_t mul0$1162;
  int32_t _tmp$2848;
  int32_t _tmp$2847;
  uint64_t mul1$1163;
  uint64_t m$1164;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1165;
  uint64_t _low1$1166;
  uint64_t _high1$1167;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1168;
  uint64_t _low0$1169;
  uint64_t _high0$1170;
  uint64_t sum$1171;
  uint64_t high1$1172;
  int32_t _tmp$2845;
  int32_t _tmp$2846;
  int32_t delta$1173;
  uint64_t _tmp$2844;
  uint64_t _tmp$2836;
  int32_t _tmp$2843;
  uint32_t _tmp$2840;
  int32_t _tmp$2842;
  int32_t _tmp$2841;
  uint32_t _tmp$2839;
  uint32_t _tmp$2838;
  uint64_t _tmp$2837;
  uint64_t a$1174;
  uint64_t _tmp$2835;
  uint64_t b$1175;
  moonbit_incref(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2
  );
  mul0$1162
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2, _tmp$2849
  );
  _tmp$2848 = base$1158 * 2;
  _tmp$2847 = _tmp$2848 + 1;
  moonbit_incref(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2
  );
  mul1$1163
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2, _tmp$2847
  );
  if (offset$1161 == 0) {
    return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
             mul0$1162, mul1$1163
           };
  }
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE);
  m$1164
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE, offset$1161
  );
  _bind$1165
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1164, mul1$1163
  );
  _low1$1166 = _bind$1165.$0;
  _high1$1167 = _bind$1165.$1;
  _bind$1168
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1164, mul0$1162
  );
  _low0$1169 = _bind$1168.$0;
  _high0$1170 = _bind$1168.$1;
  sum$1171 = _high0$1170 + _low1$1166;
  high1$1172 = _high1$1167;
  if (sum$1171 < _high0$1170) {
    uint64_t _tmp$2834 = high1$1172;
    high1$1172 = _tmp$2834 + 1ull;
  }
  _tmp$2845 = $moonbitlang$core$double$internal$ryu$pow5bits(base2$1160);
  _tmp$2846 = $moonbitlang$core$double$internal$ryu$pow5bits(i$1159);
  delta$1173 = _tmp$2845 - _tmp$2846;
  _tmp$2844
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    _low0$1169, sum$1171, delta$1173
  );
  _tmp$2836 = _tmp$2844 + 1ull;
  _tmp$2843 = i$1159 / 16;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gPOW5_INV_OFFSETS);
  _tmp$2840
  = $ReadOnlyArray$$at$1(
    $moonbitlang$core$double$internal$ryu$gPOW5_INV_OFFSETS, _tmp$2843
  );
  _tmp$2842 = i$1159 % 16;
  _tmp$2841 = _tmp$2842 << 1;
  _tmp$2839 = _tmp$2840 >> (_tmp$2841 & 31);
  _tmp$2838 = _tmp$2839 & 3u;
  _tmp$2837 = $UInt$$to_uint64(_tmp$2838);
  a$1174 = _tmp$2836 + _tmp$2837;
  _tmp$2835 = high1$1172;
  b$1175
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    sum$1171, _tmp$2835, delta$1173
  );
  return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
           a$1174, b$1175
         };
}

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computePow5(
  int32_t i$1141
) {
  int32_t base$1140 = i$1141 / 26;
  int32_t base2$1142 = base$1140 * 26;
  int32_t offset$1143 = i$1141 - base2$1142;
  int32_t _tmp$2833 = base$1140 * 2;
  uint64_t mul0$1144;
  int32_t _tmp$2832;
  int32_t _tmp$2831;
  uint64_t mul1$1145;
  uint64_t m$1146;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1147;
  uint64_t _low1$1148;
  uint64_t _high1$1149;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1150;
  uint64_t _low0$1151;
  uint64_t _high0$1152;
  uint64_t sum$1153;
  uint64_t high1$1154;
  int32_t _tmp$2829;
  int32_t _tmp$2830;
  int32_t delta$1155;
  uint64_t _tmp$2821;
  int32_t _tmp$2828;
  uint32_t _tmp$2825;
  int32_t _tmp$2827;
  int32_t _tmp$2826;
  uint32_t _tmp$2824;
  uint32_t _tmp$2823;
  uint64_t _tmp$2822;
  uint64_t a$1156;
  uint64_t _tmp$2820;
  uint64_t b$1157;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2);
  mul0$1144
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2, _tmp$2833
  );
  _tmp$2832 = base$1140 * 2;
  _tmp$2831 = _tmp$2832 + 1;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2);
  mul1$1145
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2, _tmp$2831
  );
  if (offset$1143 == 0) {
    return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
             mul0$1144, mul1$1145
           };
  }
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE);
  m$1146
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE, offset$1143
  );
  _bind$1147
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1146, mul1$1145
  );
  _low1$1148 = _bind$1147.$0;
  _high1$1149 = _bind$1147.$1;
  _bind$1150
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1146, mul0$1144
  );
  _low0$1151 = _bind$1150.$0;
  _high0$1152 = _bind$1150.$1;
  sum$1153 = _high0$1152 + _low1$1148;
  high1$1154 = _high1$1149;
  if (sum$1153 < _high0$1152) {
    uint64_t _tmp$2819 = high1$1154;
    high1$1154 = _tmp$2819 + 1ull;
  }
  _tmp$2829 = $moonbitlang$core$double$internal$ryu$pow5bits(i$1141);
  _tmp$2830 = $moonbitlang$core$double$internal$ryu$pow5bits(base2$1142);
  delta$1155 = _tmp$2829 - _tmp$2830;
  _tmp$2821
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    _low0$1151, sum$1153, delta$1155
  );
  _tmp$2828 = i$1141 / 16;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gPOW5_OFFSETS);
  _tmp$2825
  = $ReadOnlyArray$$at$1(
    $moonbitlang$core$double$internal$ryu$gPOW5_OFFSETS, _tmp$2828
  );
  _tmp$2827 = i$1141 % 16;
  _tmp$2826 = _tmp$2827 << 1;
  _tmp$2824 = _tmp$2825 >> (_tmp$2826 & 31);
  _tmp$2823 = _tmp$2824 & 3u;
  _tmp$2822 = $UInt$$to_uint64(_tmp$2823);
  a$1156 = _tmp$2821 + _tmp$2822;
  _tmp$2820 = high1$1154;
  b$1157
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    sum$1153, _tmp$2820, delta$1155
  );
  return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
           a$1156, b$1157
         };
}

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result $moonbitlang$core$double$internal$ryu$mulShiftAll64(
  uint64_t m$1114,
  struct $$moonbitlang$core$double$internal$ryu$Pow5Pair mul$1111,
  int32_t j$1127,
  int32_t mmShift$1129
) {
  uint64_t _mul0$1110 = mul$1111.$0;
  uint64_t _mul1$1112 = mul$1111.$1;
  uint64_t m$1113 = m$1114 << 1;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1115 =
    $moonbitlang$core$double$internal$ryu$umul128(m$1113, _mul0$1110);
  uint64_t _lo$1116 = _bind$1115.$0;
  uint64_t _tmp$1117 = _bind$1115.$1;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1118 =
    $moonbitlang$core$double$internal$ryu$umul128(m$1113, _mul1$1112);
  uint64_t _lo2$1119 = _bind$1118.$0;
  uint64_t _hi2$1120 = _bind$1118.$1;
  uint64_t mid$1121 = _tmp$1117 + _lo2$1119;
  uint64_t _tmp$2818;
  uint64_t hi$1122;
  uint64_t lo2$1123;
  uint64_t _tmp$2816;
  uint64_t _tmp$2817;
  uint64_t mid2$1124;
  uint64_t _tmp$2815;
  uint64_t hi2$1125;
  int32_t _tmp$2814;
  int32_t _tmp$2813;
  uint64_t vp$1126;
  uint64_t vm$1128;
  int32_t _tmp$2812;
  int32_t _tmp$2811;
  uint64_t vr$1139;
  uint64_t _tmp$2810;
  if (mid$1121 < _tmp$1117) {
    _tmp$2818 = 1ull;
  } else {
    _tmp$2818 = 0ull;
  }
  hi$1122 = _hi2$1120 + _tmp$2818;
  lo2$1123 = _lo$1116 + _mul0$1110;
  _tmp$2816 = mid$1121 + _mul1$1112;
  if (lo2$1123 < _lo$1116) {
    _tmp$2817 = 1ull;
  } else {
    _tmp$2817 = 0ull;
  }
  mid2$1124 = _tmp$2816 + _tmp$2817;
  if (mid2$1124 < mid$1121) {
    _tmp$2815 = 1ull;
  } else {
    _tmp$2815 = 0ull;
  }
  hi2$1125 = hi$1122 + _tmp$2815;
  _tmp$2814 = j$1127 - 64;
  _tmp$2813 = _tmp$2814 - 1;
  vp$1126
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    mid2$1124, hi2$1125, _tmp$2813
  );
  vm$1128 = 0ull;
  if (mmShift$1129) {
    uint64_t lo3$1130 = _lo$1116 - _mul0$1110;
    uint64_t _tmp$2800 = mid$1121 - _mul1$1112;
    uint64_t _tmp$2801;
    uint64_t mid3$1131;
    uint64_t _tmp$2799;
    uint64_t hi3$1132;
    int32_t _tmp$2798;
    int32_t _tmp$2797;
    if (_lo$1116 < lo3$1130) {
      _tmp$2801 = 1ull;
    } else {
      _tmp$2801 = 0ull;
    }
    mid3$1131 = _tmp$2800 - _tmp$2801;
    if (mid$1121 < mid3$1131) {
      _tmp$2799 = 1ull;
    } else {
      _tmp$2799 = 0ull;
    }
    hi3$1132 = hi$1122 - _tmp$2799;
    _tmp$2798 = j$1127 - 64;
    _tmp$2797 = _tmp$2798 - 1;
    vm$1128
    = $moonbitlang$core$double$internal$ryu$shiftright128(
      mid3$1131, hi3$1132, _tmp$2797
    );
  } else {
    uint64_t lo3$1133 = _lo$1116 + _lo$1116;
    uint64_t _tmp$2808 = mid$1121 + mid$1121;
    uint64_t _tmp$2809;
    uint64_t mid3$1134;
    uint64_t _tmp$2806;
    uint64_t _tmp$2807;
    uint64_t hi3$1135;
    uint64_t lo4$1136;
    uint64_t _tmp$2804;
    uint64_t _tmp$2805;
    uint64_t mid4$1137;
    uint64_t _tmp$2803;
    uint64_t hi4$1138;
    int32_t _tmp$2802;
    if (lo3$1133 < _lo$1116) {
      _tmp$2809 = 1ull;
    } else {
      _tmp$2809 = 0ull;
    }
    mid3$1134 = _tmp$2808 + _tmp$2809;
    _tmp$2806 = hi$1122 + hi$1122;
    if (mid3$1134 < mid$1121) {
      _tmp$2807 = 1ull;
    } else {
      _tmp$2807 = 0ull;
    }
    hi3$1135 = _tmp$2806 + _tmp$2807;
    lo4$1136 = lo3$1133 - _mul0$1110;
    _tmp$2804 = mid3$1134 - _mul1$1112;
    if (lo3$1133 < lo4$1136) {
      _tmp$2805 = 1ull;
    } else {
      _tmp$2805 = 0ull;
    }
    mid4$1137 = _tmp$2804 - _tmp$2805;
    if (mid3$1134 < mid4$1137) {
      _tmp$2803 = 1ull;
    } else {
      _tmp$2803 = 0ull;
    }
    hi4$1138 = hi3$1135 - _tmp$2803;
    _tmp$2802 = j$1127 - 64;
    vm$1128
    = $moonbitlang$core$double$internal$ryu$shiftright128(
      mid4$1137, hi4$1138, _tmp$2802
    );
  }
  _tmp$2812 = j$1127 - 64;
  _tmp$2811 = _tmp$2812 - 1;
  vr$1139
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    mid$1121, hi$1122, _tmp$2811
  );
  _tmp$2810 = vm$1128;
  return (struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result){
           vr$1139, vp$1126, _tmp$2810
         };
}

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf2(
  uint64_t value$1108,
  int32_t p$1109
) {
  uint64_t _tmp$2796 = 1ull << (p$1109 & 63);
  uint64_t _tmp$2795 = _tmp$2796 - 1ull;
  uint64_t _tmp$2794 = value$1108 & _tmp$2795;
  return _tmp$2794 == 0ull;
}

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
  uint64_t value$1106,
  int32_t p$1107
) {
  int32_t _tmp$2793 =
    $moonbitlang$core$double$internal$ryu$pow5Factor(value$1106);
  return _tmp$2793 >= p$1107;
}

int32_t $moonbitlang$core$double$internal$ryu$pow5Factor(uint64_t value$1102) {
  uint64_t _tmp$2781 = value$1102 % 5ull;
  uint64_t _tmp$2782;
  uint64_t _tmp$2783;
  uint64_t _tmp$2784;
  int32_t count$1103;
  uint64_t value$1104;
  uint64_t _tmp$2792;
  moonbit_string_t _tmp$2791;
  moonbit_string_t _tmp$2790;
  if (_tmp$2781 != 0ull) {
    return 0;
  }
  _tmp$2782 = value$1102 % 25ull;
  if (_tmp$2782 != 0ull) {
    return 1;
  }
  _tmp$2783 = value$1102 % 125ull;
  if (_tmp$2783 != 0ull) {
    return 2;
  }
  _tmp$2784 = value$1102 % 625ull;
  if (_tmp$2784 != 0ull) {
    return 3;
  }
  count$1103 = 4;
  value$1104 = value$1102 / 625ull;
  while (1) {
    uint64_t _tmp$2785 = value$1104;
    if (_tmp$2785 > 0ull) {
      uint64_t _tmp$2787 = value$1104;
      uint64_t _tmp$2786 = _tmp$2787 % 5ull;
      uint64_t _tmp$2788;
      int32_t _tmp$2789;
      if (_tmp$2786 != 0ull) {
        return count$1103;
      }
      _tmp$2788 = value$1104;
      value$1104 = _tmp$2788 / 5ull;
      _tmp$2789 = count$1103;
      count$1103 = _tmp$2789 + 1;
      continue;
    }
    break;
  }
  _tmp$2792 = value$1104;
  _tmp$2791
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
    _tmp$2792
  );
  _tmp$2790
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_142.data, _tmp$2791
  );
  return $moonbitlang$core$builtin$abort$1(
           _tmp$2790, (moonbit_string_t)moonbit_string_literal_143.data
         );
}

uint64_t $moonbitlang$core$double$internal$ryu$shiftright128(
  uint64_t lo$1101,
  uint64_t hi$1099,
  int32_t dist$1100
) {
  int32_t _tmp$2780 = 64 - dist$1100;
  uint64_t _tmp$2778 = hi$1099 << (_tmp$2780 & 63);
  uint64_t _tmp$2779 = lo$1101 >> (dist$1100 & 63);
  return _tmp$2778 | _tmp$2779;
}

struct $$moonbitlang$core$double$internal$ryu$Umul128 $moonbitlang$core$double$internal$ryu$umul128(
  uint64_t a$1089,
  uint64_t b$1092
) {
  uint64_t aLo$1088 = a$1089 & 4294967295ull;
  uint64_t aHi$1090 = a$1089 >> 32;
  uint64_t bLo$1091 = b$1092 & 4294967295ull;
  uint64_t bHi$1093 = b$1092 >> 32;
  uint64_t x$1094 = aLo$1088 * bLo$1091;
  uint64_t _tmp$2776 = aHi$1090 * bLo$1091;
  uint64_t _tmp$2777 = x$1094 >> 32;
  uint64_t y$1095 = _tmp$2776 + _tmp$2777;
  uint64_t _tmp$2774 = aLo$1088 * bHi$1093;
  uint64_t _tmp$2775 = y$1095 & 4294967295ull;
  uint64_t z$1096 = _tmp$2774 + _tmp$2775;
  uint64_t _tmp$2772 = aHi$1090 * bHi$1093;
  uint64_t _tmp$2773 = y$1095 >> 32;
  uint64_t _tmp$2770 = _tmp$2772 + _tmp$2773;
  uint64_t _tmp$2771 = z$1096 >> 32;
  uint64_t w$1097 = _tmp$2770 + _tmp$2771;
  uint64_t lo$1098 = a$1089 * b$1092;
  return (struct $$moonbitlang$core$double$internal$ryu$Umul128){
           lo$1098, w$1097
         };
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$string_from_bytes(
  moonbit_bytes_t bytes$1083,
  int32_t from$1087,
  int32_t to$1085
) {
  int32_t _tmp$2769 = Moonbit_array_length(bytes$1083);
  struct $$moonbitlang$core$builtin$StringBuilder* buf$1082 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(_tmp$2769);
  int32_t i$1084 = from$1087;
  while (1) {
    if (i$1084 < to$1085) {
      int32_t _tmp$2767;
      int32_t _tmp$2766;
      int32_t _tmp$2768;
      if (i$1084 < 0 || i$1084 >= Moonbit_array_length(bytes$1083)) {
        moonbit_panic();
      }
      _tmp$2767 = (int32_t)bytes$1083[i$1084];
      _tmp$2766 = $Byte$$to_char(_tmp$2767);
      moonbit_incref(buf$1082);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
        buf$1082, _tmp$2766
      );
      _tmp$2768 = i$1084 + 1;
      i$1084 = _tmp$2768;
      continue;
    } else {
      moonbit_decref(bytes$1083);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$1082);
}

int32_t $moonbitlang$core$double$internal$ryu$log10Pow2(int32_t e$1081) {
  int32_t _tmp$2765 = e$1081 * 78913;
  uint32_t _tmp$2764 = *(uint32_t*)&_tmp$2765;
  uint32_t _tmp$2763 = _tmp$2764 >> 18;
  return *(int32_t*)&_tmp$2763;
}

int32_t $moonbitlang$core$double$internal$ryu$log10Pow5(int32_t e$1080) {
  int32_t _tmp$2762 = e$1080 * 732923;
  uint32_t _tmp$2761 = *(uint32_t*)&_tmp$2762;
  uint32_t _tmp$2760 = _tmp$2761 >> 20;
  return *(int32_t*)&_tmp$2760;
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$copy_special_str(
  int32_t sign$1078,
  int32_t exponent$1079,
  int32_t mantissa$1076
) {
  moonbit_string_t s$1077;
  if (mantissa$1076) {
    return (moonbit_string_t)moonbit_string_literal_144.data;
  }
  if (sign$1078) {
    s$1077 = (moonbit_string_t)moonbit_string_literal_1.data;
  } else {
    s$1077 = (moonbit_string_t)moonbit_string_literal_3.data;
  }
  if (exponent$1079) {
    return moonbit_add_string(
             s$1077, (moonbit_string_t)moonbit_string_literal_145.data
           );
  }
  return moonbit_add_string(
           s$1077, (moonbit_string_t)moonbit_string_literal_146.data
         );
}

int32_t $moonbitlang$core$double$internal$ryu$pow5bits(int32_t e$1075) {
  int32_t _tmp$2759 = e$1075 * 1217359;
  uint32_t _tmp$2758 = *(uint32_t*)&_tmp$2759;
  uint32_t _tmp$2757 = _tmp$2758 >> 19;
  int32_t _tmp$2756 = *(int32_t*)&_tmp$2757;
  return _tmp$2756 + 1;
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1074
) {
  int32_t _field$3767 = self$1074->$1;
  int32_t len$2755;
  moonbit_decref(self$1074);
  len$2755 = _field$3767;
  return len$2755 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$1072,
  struct $$moonbitlang$core$builtin$Logger logger$1073
) {
  moonbit_string_t _tmp$2754 = self$1072;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$2753 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$2754);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$2753, logger$1073
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$1058,
  struct $$moonbitlang$core$builtin$Logger logger$1071
) {
  struct $StringView _field$3776 =
    (struct $StringView){self$1058->$0_1, self$1058->$0_2, self$1058->$0_0};
  struct $StringView pkg$1057 = _field$3776;
  int32_t _tmp$2752 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$2751;
  int64_t _bind$1059;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$1060;
  struct $StringView _field$3775;
  struct $StringView _module_name$1067;
  void* _field$3774;
  int32_t _cnt$4078;
  void* _package_name$1068;
  struct $StringView _field$3772;
  struct $StringView filename$2734;
  struct $StringView _field$3771;
  struct $StringView start_line$2735;
  struct $StringView _field$3770;
  struct $StringView start_column$2736;
  struct $StringView _field$3769;
  struct $StringView end_line$2737;
  struct $StringView _field$3768;
  int32_t _cnt$4082;
  struct $StringView end_column$2738;
  struct $$moonbitlang$core$builtin$Logger _bind$2733;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$2751
  = (struct $StringView){
    0, _tmp$2752, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$1057.$0);
  moonbit_incref(pkg$1057.$0);
  _bind$1059 = $StringView$$find(pkg$1057, _tmp$2751);
  if (_bind$1059 == 4294967296ll) {
    void* None$2739 =
      (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
    _bind$1060
    = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
        sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
      );
    Moonbit_object_header(_bind$1060)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
      )
      >> 2,
        2,
        0
    );
    _bind$1060->$0_0 = pkg$1057.$0;
    _bind$1060->$0_1 = pkg$1057.$1;
    _bind$1060->$0_2 = pkg$1057.$2;
    _bind$1060->$1 = None$2739;
  } else {
    int64_t _Some$1061 = _bind$1059;
    int32_t _first_slash$1062 = (int32_t)_Some$1061;
    int32_t _tmp$2750 = _first_slash$1062 + 1;
    struct $StringView _tmp$2747;
    int32_t _tmp$2749;
    struct $StringView _tmp$2748;
    int64_t _bind$1063;
    moonbit_incref(pkg$1057.$0);
    _tmp$2747 = $StringView$$view$inner(pkg$1057, _tmp$2750, 4294967296ll);
    _tmp$2749
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$2748
    = (struct $StringView){
      0, _tmp$2749, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$1063 = $StringView$$find(_tmp$2747, _tmp$2748);
    if (_bind$1063 == 4294967296ll) {
      void* None$2740 =
        (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$1060
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$1060)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$1060->$0_0 = pkg$1057.$0;
      _bind$1060->$0_1 = pkg$1057.$1;
      _bind$1060->$0_2 = pkg$1057.$2;
      _bind$1060->$1 = None$2740;
    } else {
      int64_t _Some$1064 = _bind$1063;
      int32_t _second_slash$1065 = (int32_t)_Some$1064;
      int32_t _tmp$2746 = _first_slash$1062 + 1;
      int32_t module_name_end$1066 = _tmp$2746 + _second_slash$1065;
      int64_t _tmp$2745 = (int64_t)module_name_end$1066;
      struct $StringView _tmp$2741;
      int32_t _tmp$2744;
      struct $StringView _tmp$2743;
      void* Some$2742;
      moonbit_incref(pkg$1057.$0);
      _tmp$2741 = $StringView$$view$inner(pkg$1057, 0, _tmp$2745);
      _tmp$2744 = module_name_end$1066 + 1;
      _tmp$2743 = $StringView$$view$inner(pkg$1057, _tmp$2744, 4294967296ll);
      Some$2742
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$2742)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$2742)->$0_0
      = _tmp$2743.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2742)->$0_1
      = _tmp$2743.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2742)->$0_2
      = _tmp$2743.$2;
      _bind$1060
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$1060)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$1060->$0_0 = _tmp$2741.$0;
      _bind$1060->$0_1 = _tmp$2741.$1;
      _bind$1060->$0_2 = _tmp$2741.$2;
      _bind$1060->$1 = Some$2742;
    }
  }
  _field$3775
  = (struct $StringView){
    _bind$1060->$0_1, _bind$1060->$0_2, _bind$1060->$0_0
  };
  _module_name$1067 = _field$3775;
  _field$3774 = _bind$1060->$1;
  _cnt$4078 = Moonbit_object_header(_bind$1060)->rc;
  if (_cnt$4078 > 1) {
    int32_t _new_cnt$4079;
    moonbit_incref(_field$3774);
    moonbit_incref(_module_name$1067.$0);
    _new_cnt$4079 = _cnt$4078 - 1;
    Moonbit_object_header(_bind$1060)->rc = _new_cnt$4079;
  } else if (_cnt$4078 == 1) {
    moonbit_free(_bind$1060);
  }
  _package_name$1068 = _field$3774;
  switch (Moonbit_object_tag(_package_name$1068)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$1069 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$1068;
      struct $StringView _field$3773 =
        (struct $StringView){
          _Some$1069->$0_1, _Some$1069->$0_2, _Some$1069->$0_0
        };
      int32_t _cnt$4080 = Moonbit_object_header(_Some$1069)->rc;
      struct $StringView _pkg_name$1070;
      struct $$moonbitlang$core$builtin$Logger _bind$2732;
      if (_cnt$4080 > 1) {
        int32_t _new_cnt$4081;
        moonbit_incref(_field$3773.$0);
        _new_cnt$4081 = _cnt$4080 - 1;
        Moonbit_object_header(_Some$1069)->rc = _new_cnt$4081;
      } else if (_cnt$4080 == 1) {
        moonbit_free(_Some$1069);
      }
      _pkg_name$1070 = _field$3773;
      if (logger$1071.$1) {
        moonbit_incref(logger$1071.$1);
      }
      logger$1071.$0->$method_2(logger$1071.$1, _pkg_name$1070);
      _bind$2732 = logger$1071;
      if (_bind$2732.$1) {
        moonbit_incref(_bind$2732.$1);
      }
      _bind$2732.$0->$method_3(_bind$2732.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$1068);
      break;
    }
  }
  _field$3772
  = (struct $StringView){
    self$1058->$1_1, self$1058->$1_2, self$1058->$1_0
  };
  filename$2734 = _field$3772;
  moonbit_incref(filename$2734.$0);
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_2(logger$1071.$1, filename$2734);
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_3(logger$1071.$1, 58);
  _field$3771
  = (struct $StringView){
    self$1058->$2_1, self$1058->$2_2, self$1058->$2_0
  };
  start_line$2735 = _field$3771;
  moonbit_incref(start_line$2735.$0);
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_2(logger$1071.$1, start_line$2735);
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_3(logger$1071.$1, 58);
  _field$3770
  = (struct $StringView){
    self$1058->$3_1, self$1058->$3_2, self$1058->$3_0
  };
  start_column$2736 = _field$3770;
  moonbit_incref(start_column$2736.$0);
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_2(logger$1071.$1, start_column$2736);
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_3(logger$1071.$1, 45);
  _field$3769
  = (struct $StringView){
    self$1058->$4_1, self$1058->$4_2, self$1058->$4_0
  };
  end_line$2737 = _field$3769;
  moonbit_incref(end_line$2737.$0);
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_2(logger$1071.$1, end_line$2737);
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_3(logger$1071.$1, 58);
  _field$3768
  = (struct $StringView){
    self$1058->$5_1, self$1058->$5_2, self$1058->$5_0
  };
  _cnt$4082 = Moonbit_object_header(self$1058)->rc;
  if (_cnt$4082 > 1) {
    int32_t _new_cnt$4088;
    moonbit_incref(_field$3768.$0);
    _new_cnt$4088 = _cnt$4082 - 1;
    Moonbit_object_header(self$1058)->rc = _new_cnt$4088;
  } else if (_cnt$4082 == 1) {
    struct $StringView _field$4087 =
      (struct $StringView){self$1058->$4_1, self$1058->$4_2, self$1058->$4_0};
    struct $StringView _field$4086;
    struct $StringView _field$4085;
    struct $StringView _field$4084;
    struct $StringView _field$4083;
    moonbit_decref(_field$4087.$0);
    _field$4086
    = (struct $StringView){
      self$1058->$3_1, self$1058->$3_2, self$1058->$3_0
    };
    moonbit_decref(_field$4086.$0);
    _field$4085
    = (struct $StringView){
      self$1058->$2_1, self$1058->$2_2, self$1058->$2_0
    };
    moonbit_decref(_field$4085.$0);
    _field$4084
    = (struct $StringView){
      self$1058->$1_1, self$1058->$1_2, self$1058->$1_0
    };
    moonbit_decref(_field$4084.$0);
    _field$4083
    = (struct $StringView){
      self$1058->$0_1, self$1058->$0_2, self$1058->$0_0
    };
    moonbit_decref(_field$4083.$0);
    moonbit_free(self$1058);
  }
  end_column$2738 = _field$3768;
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_2(logger$1071.$1, end_column$2738);
  if (logger$1071.$1) {
    moonbit_incref(logger$1071.$1);
  }
  logger$1071.$0->$method_3(logger$1071.$1, 64);
  _bind$2733 = logger$1071;
  _bind$2733.$0->$method_2(_bind$2733.$1, _module_name$1067);
  return 0;
}

uint64_t $Bool$$to_uint64(int32_t self$1056) {
  if (self$1056) {
    return 1ull;
  } else {
    return 0ull;
  }
}

int64_t $Bool$$to_int64(int32_t self$1055) {
  if (self$1055) {
    return 1ll;
  } else {
    return 0ll;
  }
}

int32_t $Bool$$to_int(int32_t self$1054) {
  if (self$1054) {
    return 1;
  } else {
    return 0;
  }
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$1053) {
  moonbit_string_t _tmp$2731 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$1053);
  moonbit_println(_tmp$2731);
  moonbit_decref(_tmp$2731);
  return 0;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$1048,
  moonbit_string_t msg$1050,
  moonbit_string_t loc$1052
) {
  if (!x$1048) {
    moonbit_string_t fail_msg$1049;
    if (msg$1050 == 0) {
      moonbit_string_t _tmp$2729;
      moonbit_string_t _tmp$2728;
      if (msg$1050) {
        moonbit_decref(msg$1050);
      }
      _tmp$2729
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$1048
      );
      _tmp$2728
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2729
      );
      fail_msg$1049
      = moonbit_add_string(
        _tmp$2728, (moonbit_string_t)moonbit_string_literal_148.data
      );
    } else {
      moonbit_string_t _Some$1051 = msg$1050;
      fail_msg$1049 = _Some$1051;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$1049, loc$1052);
  } else {
    int32_t _tmp$2730;
    struct moonbit_result_0 _result$4390;
    moonbit_decref(loc$1052);
    if (msg$1050) {
      moonbit_decref(msg$1050);
    }
    _tmp$2730 = 0;
    _result$4390.tag = 1;
    _result$4390.data.ok = _tmp$2730;
    return _result$4390;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$1043,
  moonbit_string_t msg$1045,
  moonbit_string_t loc$1047
) {
  if (x$1043) {
    moonbit_string_t fail_msg$1044;
    if (msg$1045 == 0) {
      moonbit_string_t _tmp$2726;
      moonbit_string_t _tmp$2725;
      if (msg$1045) {
        moonbit_decref(msg$1045);
      }
      _tmp$2726
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$1043
      );
      _tmp$2725
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2726
      );
      fail_msg$1044
      = moonbit_add_string(
        _tmp$2725, (moonbit_string_t)moonbit_string_literal_149.data
      );
    } else {
      moonbit_string_t _Some$1046 = msg$1045;
      fail_msg$1044 = _Some$1046;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$1044, loc$1047);
  } else {
    int32_t _tmp$2727;
    struct moonbit_result_0 _result$4391;
    moonbit_decref(loc$1047);
    if (msg$1045) {
      moonbit_decref(msg$1045);
    }
    _tmp$2727 = 0;
    _result$4391.tag = 1;
    _result$4391.data.ok = _tmp$2727;
    return _result$4391;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$1042,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1041
) {
  $$moonbitlang$core$builtin$Hasher$$combine_int(hasher$1041, self$1042);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$1040,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1039
) {
  $$moonbitlang$core$builtin$Hasher$$combine_string(hasher$1039, self$1040);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$1037,
  moonbit_string_t value$1035
) {
  int32_t _end2448$1034 = Moonbit_array_length(value$1035);
  int32_t i$1036 = 0;
  while (1) {
    if (i$1036 < _end2448$1034) {
      int32_t _tmp$2723 = value$1035[i$1036];
      uint32_t _tmp$2722 = *(uint32_t*)&_tmp$2723;
      int32_t _tmp$2724;
      moonbit_incref(self$1037);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$1037, _tmp$2722);
      _tmp$2724 = i$1036 + 1;
      i$1036 = _tmp$2724;
      continue;
    } else {
      moonbit_decref(self$1037);
      moonbit_decref(value$1035);
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$1031,
  struct $$3c$String$3e$$3d$$3e$Int* f$1033
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$4393 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$2719;
  int32_t _tmp$2718;
  Moonbit_object_header(_closure$4393)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$4393->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$4393->$0 = f$1033;
  _tmp$2719 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$4393;
  _tmp$2718 = $$moonbitlang$core$builtin$Iter$$run$0(self$1031, _tmp$2719);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$2718, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2720,
  moonbit_string_t k$1032
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$2721 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$2720;
  struct $$3c$String$3e$$3d$$3e$Int* _field$3777 = _casted_env$2721->$0;
  int32_t _cnt$4089 = Moonbit_object_header(_casted_env$2721)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$1033;
  if (_cnt$4089 > 1) {
    int32_t _new_cnt$4090;
    moonbit_incref(_field$3777);
    _new_cnt$4090 = _cnt$4089 - 1;
    Moonbit_object_header(_casted_env$2721)->rc = _new_cnt$4090;
  } else if (_cnt$4089 == 1) {
    moonbit_free(_casted_env$2721);
  }
  f$1033 = _field$3777;
  if (f$1033->code(f$1033, k$1032)) {
    return 0;
  } else {
    return 1;
  }
}

struct $$3c$String$2a$AttributeValue$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$4(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$1029,
  int32_t idx$1030
) {
  struct $$3c$String$2a$AttributeValue$3e$** _tmp$2717 =
    $$moonbitlang$core$builtin$Array$$buffer$6(self$1029);
  struct $$3c$String$2a$AttributeValue$3e$* _tmp$3778;
  if (idx$1030 < 0 || idx$1030 >= Moonbit_array_length(_tmp$2717)) {
    moonbit_panic();
  }
  _tmp$3778 = (struct $$3c$String$2a$AttributeValue$3e$*)_tmp$2717[idx$1030];
  if (_tmp$3778) {
    moonbit_incref(_tmp$3778);
  }
  moonbit_decref(_tmp$2717);
  return _tmp$3778;
}

struct $$3c$String$2a$String$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$3(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$1027,
  int32_t idx$1028
) {
  struct $$3c$String$2a$String$3e$** _tmp$2716 =
    $$moonbitlang$core$builtin$Array$$buffer$5(self$1027);
  struct $$3c$String$2a$String$3e$* _tmp$3779;
  if (idx$1028 < 0 || idx$1028 >= Moonbit_array_length(_tmp$2716)) {
    moonbit_panic();
  }
  _tmp$3779 = (struct $$3c$String$2a$String$3e$*)_tmp$2716[idx$1028];
  if (_tmp$3779) {
    moonbit_incref(_tmp$3779);
  }
  moonbit_decref(_tmp$2716);
  return _tmp$3779;
}

int32_t $$moonbitlang$core$builtin$Array$$unsafe_get$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$1025,
  int32_t idx$1026
) {
  int32_t* _tmp$2715 = $$moonbitlang$core$builtin$Array$$buffer$3(self$1025);
  int32_t _tmp$3780;
  if (idx$1026 < 0 || idx$1026 >= Moonbit_array_length(_tmp$2715)) {
    moonbit_panic();
  }
  _tmp$3780 = (int32_t)_tmp$2715[idx$1026];
  moonbit_decref(_tmp$2715);
  return _tmp$3780;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1023,
  int32_t idx$1024
) {
  moonbit_string_t* _tmp$2714 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$1023);
  moonbit_string_t _tmp$3781;
  if (idx$1024 < 0 || idx$1024 >= Moonbit_array_length(_tmp$2714)) {
    moonbit_panic();
  }
  _tmp$3781 = (moonbit_string_t)_tmp$2714[idx$1024];
  moonbit_incref(_tmp$3781);
  moonbit_decref(_tmp$2714);
  return _tmp$3781;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$1021,
  int32_t idx$1022
) {
  struct $$3c$String$2a$Int$3e$** _tmp$2713 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$1021);
  struct $$3c$String$2a$Int$3e$* _tmp$3782;
  if (idx$1022 < 0 || idx$1022 >= Moonbit_array_length(_tmp$2713)) {
    moonbit_panic();
  }
  _tmp$3782 = (struct $$3c$String$2a$Int$3e$*)_tmp$2713[idx$1022];
  if (_tmp$3782) {
    moonbit_incref(_tmp$3782);
  }
  moonbit_decref(_tmp$2713);
  return _tmp$3782;
}

uint64_t $UInt$$to_uint64(uint32_t self$1020) {
  return (uint64_t)self$1020;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$1016,
  int32_t key$1012
) {
  int32_t hash$1011 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$1012);
  int32_t capacity_mask$2712 = self$1016->$3;
  int32_t _tmp$2711 = hash$1011 & capacity_mask$2712;
  int32_t i$1013 = 0;
  int32_t idx$1014 = _tmp$2711;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3786 =
      self$1016->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2710 =
      _field$3786;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3785;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1015;
    if (idx$1014 < 0 || idx$1014 >= Moonbit_array_length(entries$2710)) {
      moonbit_panic();
    }
    _tmp$3785
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2710[
        idx$1014
      ];
    _bind$1015 = _tmp$3785;
    if (_bind$1015 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2699;
      if (_bind$1015) {
        moonbit_incref(_bind$1015);
      }
      moonbit_decref(self$1016);
      if (_bind$1015) {
        moonbit_decref(_bind$1015);
      }
      _tmp$2699 = 0;
      return _tmp$2699;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1017 =
        _bind$1015;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$1018 =
        _Some$1017;
      int32_t hash$2701 = _entry$1018->$3;
      int32_t _if_result$4395;
      int32_t _field$3783;
      int32_t psl$2704;
      int32_t _tmp$2706;
      int32_t _tmp$2708;
      int32_t capacity_mask$2709;
      int32_t _tmp$2707;
      if (hash$2701 == hash$1011) {
        int32_t key$2700 = _entry$1018->$4;
        _if_result$4395 = key$2700 == key$1012;
      } else {
        _if_result$4395 = 0;
      }
      if (_if_result$4395) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3784;
        int32_t _cnt$4091;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2703;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2702;
        moonbit_incref(_entry$1018);
        moonbit_decref(self$1016);
        _field$3784 = _entry$1018->$5;
        _cnt$4091 = Moonbit_object_header(_entry$1018)->rc;
        if (_cnt$4091 > 1) {
          int32_t _new_cnt$4093;
          moonbit_incref(_field$3784);
          _new_cnt$4093 = _cnt$4091 - 1;
          Moonbit_object_header(_entry$1018)->rc = _new_cnt$4093;
        } else if (_cnt$4091 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4092 =
            _entry$1018->$1;
          if (_field$4092) {
            moonbit_decref(_field$4092);
          }
          moonbit_free(_entry$1018);
        }
        value$2703 = _field$3784;
        _tmp$2702 = value$2703;
        return _tmp$2702;
      } else {
        moonbit_incref(_entry$1018);
      }
      _field$3783 = _entry$1018->$2;
      moonbit_decref(_entry$1018);
      psl$2704 = _field$3783;
      if (i$1013 > psl$2704) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2705;
        moonbit_decref(self$1016);
        _tmp$2705 = 0;
        return _tmp$2705;
      }
      _tmp$2706 = i$1013 + 1;
      _tmp$2708 = idx$1014 + 1;
      capacity_mask$2709 = self$1016->$3;
      _tmp$2707 = _tmp$2708 & capacity_mask$2709;
      i$1013 = _tmp$2706;
      idx$1014 = _tmp$2707;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$1007,
  moonbit_string_t key$1003
) {
  int32_t hash$1002;
  int32_t capacity_mask$2698;
  int32_t _tmp$2697;
  int32_t i$1004;
  int32_t idx$1005;
  moonbit_incref(key$1003);
  hash$1002
  = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
    key$1003
  );
  capacity_mask$2698 = self$1007->$3;
  _tmp$2697 = hash$1002 & capacity_mask$2698;
  i$1004 = 0;
  idx$1005 = _tmp$2697;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3792 =
      self$1007->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2696 =
      _field$3792;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3791;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$1006;
    if (idx$1005 < 0 || idx$1005 >= Moonbit_array_length(entries$2696)) {
      moonbit_panic();
    }
    _tmp$3791
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2696[
        idx$1005
      ];
    _bind$1006 = _tmp$3791;
    if (_bind$1006 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2685;
      if (_bind$1006) {
        moonbit_incref(_bind$1006);
      }
      moonbit_decref(self$1007);
      if (_bind$1006) {
        moonbit_decref(_bind$1006);
      }
      moonbit_decref(key$1003);
      _tmp$2685 = 0;
      return _tmp$2685;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$1008 =
        _bind$1006;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$1009 =
        _Some$1008;
      int32_t hash$2687 = _entry$1009->$3;
      int32_t _if_result$4397;
      int32_t _field$3787;
      int32_t psl$2690;
      int32_t _tmp$2692;
      int32_t _tmp$2694;
      int32_t capacity_mask$2695;
      int32_t _tmp$2693;
      if (hash$2687 == hash$1002) {
        moonbit_string_t _field$3790 = _entry$1009->$4;
        moonbit_string_t key$2686 = _field$3790;
        int32_t _tmp$3789 = moonbit_val_array_equal(key$2686, key$1003);
        _if_result$4397 = _tmp$3789;
      } else {
        _if_result$4397 = 0;
      }
      if (_if_result$4397) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3788;
        int32_t _cnt$4094;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2689;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2688;
        moonbit_incref(_entry$1009);
        moonbit_decref(self$1007);
        moonbit_decref(key$1003);
        _field$3788 = _entry$1009->$5;
        _cnt$4094 = Moonbit_object_header(_entry$1009)->rc;
        if (_cnt$4094 > 1) {
          int32_t _new_cnt$4097;
          moonbit_incref(_field$3788);
          _new_cnt$4097 = _cnt$4094 - 1;
          Moonbit_object_header(_entry$1009)->rc = _new_cnt$4097;
        } else if (_cnt$4094 == 1) {
          moonbit_string_t _field$4096 = _entry$1009->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4095;
          moonbit_decref(_field$4096);
          _field$4095 = _entry$1009->$1;
          if (_field$4095) {
            moonbit_decref(_field$4095);
          }
          moonbit_free(_entry$1009);
        }
        value$2689 = _field$3788;
        _tmp$2688 = value$2689;
        return _tmp$2688;
      } else {
        moonbit_incref(_entry$1009);
      }
      _field$3787 = _entry$1009->$2;
      moonbit_decref(_entry$1009);
      psl$2690 = _field$3787;
      if (i$1004 > psl$2690) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2691;
        moonbit_decref(self$1007);
        moonbit_decref(key$1003);
        _tmp$2691 = 0;
        return _tmp$2691;
      }
      _tmp$2692 = i$1004 + 1;
      _tmp$2694 = idx$1005 + 1;
      capacity_mask$2695 = self$1007->$3;
      _tmp$2693 = _tmp$2694 & capacity_mask$2695;
      i$1004 = _tmp$2692;
      idx$1005 = _tmp$2693;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$998,
  int32_t key$994
) {
  int32_t hash$993 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$994);
  int32_t capacity_mask$2684 = self$998->$3;
  int32_t _tmp$2683 = hash$993 & capacity_mask$2684;
  int32_t i$995 = 0;
  int32_t idx$996 = _tmp$2683;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3796 =
      self$998->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2682 =
      _field$3796;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3795;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$997;
    if (idx$996 < 0 || idx$996 >= Moonbit_array_length(entries$2682)) {
      moonbit_panic();
    }
    _tmp$3795
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2682[
        idx$996
      ];
    _bind$997 = _tmp$3795;
    if (_bind$997 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2671;
      if (_bind$997) {
        moonbit_incref(_bind$997);
      }
      moonbit_decref(self$998);
      if (_bind$997) {
        moonbit_decref(_bind$997);
      }
      _tmp$2671 = 0;
      return _tmp$2671;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$999 =
        _bind$997;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$1000 =
        _Some$999;
      int32_t hash$2673 = _entry$1000->$3;
      int32_t _if_result$4399;
      int32_t _field$3793;
      int32_t psl$2676;
      int32_t _tmp$2678;
      int32_t _tmp$2680;
      int32_t capacity_mask$2681;
      int32_t _tmp$2679;
      if (hash$2673 == hash$993) {
        int32_t key$2672 = _entry$1000->$4;
        _if_result$4399 = key$2672 == key$994;
      } else {
        _if_result$4399 = 0;
      }
      if (_if_result$4399) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3794;
        int32_t _cnt$4098;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2675;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2674;
        moonbit_incref(_entry$1000);
        moonbit_decref(self$998);
        _field$3794 = _entry$1000->$5;
        _cnt$4098 = Moonbit_object_header(_entry$1000)->rc;
        if (_cnt$4098 > 1) {
          int32_t _new_cnt$4100;
          moonbit_incref(_field$3794);
          _new_cnt$4100 = _cnt$4098 - 1;
          Moonbit_object_header(_entry$1000)->rc = _new_cnt$4100;
        } else if (_cnt$4098 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4099 =
            _entry$1000->$1;
          if (_field$4099) {
            moonbit_decref(_field$4099);
          }
          moonbit_free(_entry$1000);
        }
        value$2675 = _field$3794;
        _tmp$2674 = value$2675;
        return _tmp$2674;
      } else {
        moonbit_incref(_entry$1000);
      }
      _field$3793 = _entry$1000->$2;
      moonbit_decref(_entry$1000);
      psl$2676 = _field$3793;
      if (i$995 > psl$2676) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2677;
        moonbit_decref(self$998);
        _tmp$2677 = 0;
        return _tmp$2677;
      }
      _tmp$2678 = i$995 + 1;
      _tmp$2680 = idx$996 + 1;
      capacity_mask$2681 = self$998->$3;
      _tmp$2679 = _tmp$2680 & capacity_mask$2681;
      i$995 = _tmp$2678;
      idx$996 = _tmp$2679;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$989,
  moonbit_string_t key$985
) {
  int32_t hash$984;
  int32_t capacity_mask$2670;
  int32_t _tmp$2669;
  int32_t i$986;
  int32_t idx$987;
  moonbit_incref(key$985);
  hash$984 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$985);
  capacity_mask$2670 = self$989->$3;
  _tmp$2669 = hash$984 & capacity_mask$2670;
  i$986 = 0;
  idx$987 = _tmp$2669;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3802 =
      self$989->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2668 =
      _field$3802;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3801;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$988;
    if (idx$987 < 0 || idx$987 >= Moonbit_array_length(entries$2668)) {
      moonbit_panic();
    }
    _tmp$3801
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2668[
        idx$987
      ];
    _bind$988 = _tmp$3801;
    if (_bind$988 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2657;
      if (_bind$988) {
        moonbit_incref(_bind$988);
      }
      moonbit_decref(self$989);
      if (_bind$988) {
        moonbit_decref(_bind$988);
      }
      moonbit_decref(key$985);
      _tmp$2657 = 0;
      return _tmp$2657;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$990 =
        _bind$988;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$991 =
        _Some$990;
      int32_t hash$2659 = _entry$991->$3;
      int32_t _if_result$4401;
      int32_t _field$3797;
      int32_t psl$2662;
      int32_t _tmp$2664;
      int32_t _tmp$2666;
      int32_t capacity_mask$2667;
      int32_t _tmp$2665;
      if (hash$2659 == hash$984) {
        moonbit_string_t _field$3800 = _entry$991->$4;
        moonbit_string_t key$2658 = _field$3800;
        int32_t _tmp$3799 = moonbit_val_array_equal(key$2658, key$985);
        _if_result$4401 = _tmp$3799;
      } else {
        _if_result$4401 = 0;
      }
      if (_if_result$4401) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3798;
        int32_t _cnt$4101;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2661;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2660;
        moonbit_incref(_entry$991);
        moonbit_decref(self$989);
        moonbit_decref(key$985);
        _field$3798 = _entry$991->$5;
        _cnt$4101 = Moonbit_object_header(_entry$991)->rc;
        if (_cnt$4101 > 1) {
          int32_t _new_cnt$4104;
          moonbit_incref(_field$3798);
          _new_cnt$4104 = _cnt$4101 - 1;
          Moonbit_object_header(_entry$991)->rc = _new_cnt$4104;
        } else if (_cnt$4101 == 1) {
          moonbit_string_t _field$4103 = _entry$991->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4102;
          moonbit_decref(_field$4103);
          _field$4102 = _entry$991->$1;
          if (_field$4102) {
            moonbit_decref(_field$4102);
          }
          moonbit_free(_entry$991);
        }
        value$2661 = _field$3798;
        _tmp$2660 = value$2661;
        return _tmp$2660;
      } else {
        moonbit_incref(_entry$991);
      }
      _field$3797 = _entry$991->$2;
      moonbit_decref(_entry$991);
      psl$2662 = _field$3797;
      if (i$986 > psl$2662) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2663;
        moonbit_decref(self$989);
        moonbit_decref(key$985);
        _tmp$2663 = 0;
        return _tmp$2663;
      }
      _tmp$2664 = i$986 + 1;
      _tmp$2666 = idx$987 + 1;
      capacity_mask$2667 = self$989->$3;
      _tmp$2665 = _tmp$2666 & capacity_mask$2667;
      i$986 = _tmp$2664;
      idx$987 = _tmp$2665;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$980,
  int32_t key$976
) {
  int32_t hash$975 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$976);
  int32_t capacity_mask$2656 = self$980->$3;
  int32_t _tmp$2655 = hash$975 & capacity_mask$2656;
  int32_t i$977 = 0;
  int32_t idx$978 = _tmp$2655;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3806 =
      self$980->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2654 =
      _field$3806;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3805;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$979;
    if (idx$978 < 0 || idx$978 >= Moonbit_array_length(entries$2654)) {
      moonbit_panic();
    }
    _tmp$3805
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2654[
        idx$978
      ];
    _bind$979 = _tmp$3805;
    if (_bind$979 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2643;
      if (_bind$979) {
        moonbit_incref(_bind$979);
      }
      moonbit_decref(self$980);
      if (_bind$979) {
        moonbit_decref(_bind$979);
      }
      _tmp$2643 = 0;
      return _tmp$2643;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$981 =
        _bind$979;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$982 =
        _Some$981;
      int32_t hash$2645 = _entry$982->$3;
      int32_t _if_result$4403;
      int32_t _field$3803;
      int32_t psl$2648;
      int32_t _tmp$2650;
      int32_t _tmp$2652;
      int32_t capacity_mask$2653;
      int32_t _tmp$2651;
      if (hash$2645 == hash$975) {
        int32_t key$2644 = _entry$982->$4;
        _if_result$4403 = key$2644 == key$976;
      } else {
        _if_result$4403 = 0;
      }
      if (_if_result$4403) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3804;
        int32_t _cnt$4105;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2647;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2646;
        moonbit_incref(_entry$982);
        moonbit_decref(self$980);
        _field$3804 = _entry$982->$5;
        _cnt$4105 = Moonbit_object_header(_entry$982)->rc;
        if (_cnt$4105 > 1) {
          int32_t _new_cnt$4107;
          moonbit_incref(_field$3804);
          _new_cnt$4107 = _cnt$4105 - 1;
          Moonbit_object_header(_entry$982)->rc = _new_cnt$4107;
        } else if (_cnt$4105 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4106 =
            _entry$982->$1;
          if (_field$4106) {
            moonbit_decref(_field$4106);
          }
          moonbit_free(_entry$982);
        }
        value$2647 = _field$3804;
        _tmp$2646 = value$2647;
        return _tmp$2646;
      } else {
        moonbit_incref(_entry$982);
      }
      _field$3803 = _entry$982->$2;
      moonbit_decref(_entry$982);
      psl$2648 = _field$3803;
      if (i$977 > psl$2648) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2649;
        moonbit_decref(self$980);
        _tmp$2649 = 0;
        return _tmp$2649;
      }
      _tmp$2650 = i$977 + 1;
      _tmp$2652 = idx$978 + 1;
      capacity_mask$2653 = self$980->$3;
      _tmp$2651 = _tmp$2652 & capacity_mask$2653;
      i$977 = _tmp$2650;
      idx$978 = _tmp$2651;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$971,
  moonbit_string_t key$967
) {
  int32_t hash$966;
  int32_t capacity_mask$2642;
  int32_t _tmp$2641;
  int32_t i$968;
  int32_t idx$969;
  moonbit_incref(key$967);
  hash$966 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$967);
  capacity_mask$2642 = self$971->$3;
  _tmp$2641 = hash$966 & capacity_mask$2642;
  i$968 = 0;
  idx$969 = _tmp$2641;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3812 =
      self$971->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2640 =
      _field$3812;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3811;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$970;
    if (idx$969 < 0 || idx$969 >= Moonbit_array_length(entries$2640)) {
      moonbit_panic();
    }
    _tmp$3811
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2640[
        idx$969
      ];
    _bind$970 = _tmp$3811;
    if (_bind$970 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2629;
      if (_bind$970) {
        moonbit_incref(_bind$970);
      }
      moonbit_decref(self$971);
      if (_bind$970) {
        moonbit_decref(_bind$970);
      }
      moonbit_decref(key$967);
      _tmp$2629 = 0;
      return _tmp$2629;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$972 =
        _bind$970;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$973 =
        _Some$972;
      int32_t hash$2631 = _entry$973->$3;
      int32_t _if_result$4405;
      int32_t _field$3807;
      int32_t psl$2634;
      int32_t _tmp$2636;
      int32_t _tmp$2638;
      int32_t capacity_mask$2639;
      int32_t _tmp$2637;
      if (hash$2631 == hash$966) {
        moonbit_string_t _field$3810 = _entry$973->$4;
        moonbit_string_t key$2630 = _field$3810;
        int32_t _tmp$3809 = moonbit_val_array_equal(key$2630, key$967);
        _if_result$4405 = _tmp$3809;
      } else {
        _if_result$4405 = 0;
      }
      if (_if_result$4405) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3808;
        int32_t _cnt$4108;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2633;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2632;
        moonbit_incref(_entry$973);
        moonbit_decref(self$971);
        moonbit_decref(key$967);
        _field$3808 = _entry$973->$5;
        _cnt$4108 = Moonbit_object_header(_entry$973)->rc;
        if (_cnt$4108 > 1) {
          int32_t _new_cnt$4111;
          moonbit_incref(_field$3808);
          _new_cnt$4111 = _cnt$4108 - 1;
          Moonbit_object_header(_entry$973)->rc = _new_cnt$4111;
        } else if (_cnt$4108 == 1) {
          moonbit_string_t _field$4110 = _entry$973->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4109;
          moonbit_decref(_field$4110);
          _field$4109 = _entry$973->$1;
          if (_field$4109) {
            moonbit_decref(_field$4109);
          }
          moonbit_free(_entry$973);
        }
        value$2633 = _field$3808;
        _tmp$2632 = value$2633;
        return _tmp$2632;
      } else {
        moonbit_incref(_entry$973);
      }
      _field$3807 = _entry$973->$2;
      moonbit_decref(_entry$973);
      psl$2634 = _field$3807;
      if (i$968 > psl$2634) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2635;
        moonbit_decref(self$971);
        moonbit_decref(key$967);
        _tmp$2635 = 0;
        return _tmp$2635;
      }
      _tmp$2636 = i$968 + 1;
      _tmp$2638 = idx$969 + 1;
      capacity_mask$2639 = self$971->$3;
      _tmp$2637 = _tmp$2638 & capacity_mask$2639;
      i$968 = _tmp$2636;
      idx$969 = _tmp$2637;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$959
) {
  int32_t length$958;
  int32_t capacity$960;
  int32_t _tmp$2620;
  int32_t _tmp$2619;
  int32_t _tmp$2628;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$961;
  int32_t _len$962;
  int32_t _i$963;
  moonbit_incref(arr$959.$0);
  length$958 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$959);
  capacity$960 = $Int$$next_power_of_two(length$958);
  _tmp$2620 = capacity$960;
  _tmp$2619 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2620);
  if (length$958 > _tmp$2619) {
    int32_t _tmp$2621 = capacity$960;
    capacity$960 = _tmp$2621 * 2;
  }
  _tmp$2628 = capacity$960;
  m$961 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$2628);
  moonbit_incref(arr$959.$0);
  _len$962 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$959);
  _i$963 = 0;
  while (1) {
    if (_i$963 < _len$962) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3816 =
        arr$959.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2624 =
        _field$3816;
      int32_t start$2626 = arr$959.$1;
      int32_t _tmp$2625 = start$2626 + _i$963;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3815 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2624[
          _tmp$2625
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$964 =
        _tmp$3815;
      moonbit_string_t _field$3814 = e$964->$0;
      moonbit_string_t _tmp$2622 = _field$3814;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3813 =
        e$964->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2623 =
        _field$3813;
      int32_t _tmp$2627;
      moonbit_incref(_tmp$2623);
      moonbit_incref(_tmp$2622);
      moonbit_incref(m$961);
      $$moonbitlang$core$builtin$Map$$set$3(m$961, _tmp$2622, _tmp$2623);
      _tmp$2627 = _i$963 + 1;
      _i$963 = _tmp$2627;
      continue;
    } else {
      moonbit_decref(arr$959.$0);
    }
    break;
  }
  return m$961;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$951
) {
  int32_t length$950;
  int32_t capacity$952;
  int32_t _tmp$2610;
  int32_t _tmp$2609;
  int32_t _tmp$2618;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$953;
  int32_t _len$954;
  int32_t _i$955;
  moonbit_incref(arr$951.$0);
  length$950 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$951);
  capacity$952 = $Int$$next_power_of_two(length$950);
  _tmp$2610 = capacity$952;
  _tmp$2609 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2610);
  if (length$950 > _tmp$2609) {
    int32_t _tmp$2611 = capacity$952;
    capacity$952 = _tmp$2611 * 2;
  }
  _tmp$2618 = capacity$952;
  m$953 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$2618);
  moonbit_incref(arr$951.$0);
  _len$954 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$951);
  _i$955 = 0;
  while (1) {
    if (_i$955 < _len$954) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3820 =
        arr$951.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2614 =
        _field$3820;
      int32_t start$2616 = arr$951.$1;
      int32_t _tmp$2615 = start$2616 + _i$955;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3819 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2614[
          _tmp$2615
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$956 =
        _tmp$3819;
      moonbit_string_t _field$3818 = e$956->$0;
      moonbit_string_t _tmp$2612 = _field$3818;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3817 =
        e$956->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2613 =
        _field$3817;
      int32_t _tmp$2617;
      moonbit_incref(_tmp$2613);
      moonbit_incref(_tmp$2612);
      moonbit_incref(m$953);
      $$moonbitlang$core$builtin$Map$$set$2(m$953, _tmp$2612, _tmp$2613);
      _tmp$2617 = _i$955 + 1;
      _i$955 = _tmp$2617;
      continue;
    } else {
      moonbit_decref(arr$951.$0);
    }
    break;
  }
  return m$953;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$943
) {
  int32_t length$942;
  int32_t capacity$944;
  int32_t _tmp$2600;
  int32_t _tmp$2599;
  int32_t _tmp$2608;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$945;
  int32_t _len$946;
  int32_t _i$947;
  moonbit_incref(arr$943.$0);
  length$942 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$943);
  capacity$944 = $Int$$next_power_of_two(length$942);
  _tmp$2600 = capacity$944;
  _tmp$2599 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2600);
  if (length$942 > _tmp$2599) {
    int32_t _tmp$2601 = capacity$944;
    capacity$944 = _tmp$2601 * 2;
  }
  _tmp$2608 = capacity$944;
  m$945 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$2608);
  moonbit_incref(arr$943.$0);
  _len$946 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$943);
  _i$947 = 0;
  while (1) {
    if (_i$947 < _len$946) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3823 =
        arr$943.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$2604 =
        _field$3823;
      int32_t start$2606 = arr$943.$1;
      int32_t _tmp$2605 = start$2606 + _i$947;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3822 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$2604[
          _tmp$2605
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$948 =
        _tmp$3822;
      int32_t _tmp$2602 = e$948->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3821 =
        e$948->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2603 =
        _field$3821;
      int32_t _tmp$2607;
      moonbit_incref(_tmp$2603);
      moonbit_incref(m$945);
      $$moonbitlang$core$builtin$Map$$set$1(m$945, _tmp$2602, _tmp$2603);
      _tmp$2607 = _i$947 + 1;
      _i$947 = _tmp$2607;
      continue;
    } else {
      moonbit_decref(arr$943.$0);
    }
    break;
  }
  return m$945;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$935
) {
  int32_t length$934;
  int32_t capacity$936;
  int32_t _tmp$2590;
  int32_t _tmp$2589;
  int32_t _tmp$2598;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$937;
  int32_t _len$938;
  int32_t _i$939;
  moonbit_incref(arr$935.$0);
  length$934 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$935);
  capacity$936 = $Int$$next_power_of_two(length$934);
  _tmp$2590 = capacity$936;
  _tmp$2589 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2590);
  if (length$934 > _tmp$2589) {
    int32_t _tmp$2591 = capacity$936;
    capacity$936 = _tmp$2591 * 2;
  }
  _tmp$2598 = capacity$936;
  m$937 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$2598);
  moonbit_incref(arr$935.$0);
  _len$938 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$935);
  _i$939 = 0;
  while (1) {
    if (_i$939 < _len$938) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3827 =
        arr$935.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2594 =
        _field$3827;
      int32_t start$2596 = arr$935.$1;
      int32_t _tmp$2595 = start$2596 + _i$939;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3826 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2594[
          _tmp$2595
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$940 =
        _tmp$3826;
      moonbit_string_t _field$3825 = e$940->$0;
      moonbit_string_t _tmp$2592 = _field$3825;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3824 =
        e$940->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2593 =
        _field$3824;
      int32_t _tmp$2597;
      moonbit_incref(_tmp$2593);
      moonbit_incref(_tmp$2592);
      moonbit_incref(m$937);
      $$moonbitlang$core$builtin$Map$$set$0(m$937, _tmp$2592, _tmp$2593);
      _tmp$2597 = _i$939 + 1;
      _i$939 = _tmp$2597;
      continue;
    } else {
      moonbit_decref(arr$935.$0);
    }
    break;
  }
  return m$937;
}

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$931,
  moonbit_string_t key$932,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$933
) {
  int32_t _tmp$2588;
  moonbit_incref(key$932);
  _tmp$2588 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$932);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$931, key$932, value$933, _tmp$2588
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$928,
  moonbit_string_t key$929,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$930
) {
  int32_t _tmp$2587;
  moonbit_incref(key$929);
  _tmp$2587 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$929);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$928, key$929, value$930, _tmp$2587
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$925,
  int32_t key$926,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$927
) {
  int32_t _tmp$2586 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$926);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$925, key$926, value$927, _tmp$2586
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$922,
  moonbit_string_t key$923,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$924
) {
  int32_t _tmp$2585;
  moonbit_incref(key$923);
  _tmp$2585 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$923);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$922, key$923, value$924, _tmp$2585
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$912
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3834 =
    self$912->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$911 =
    _field$3834;
  int32_t capacity$2584 = self$912->$2;
  int32_t new_capacity$913 = capacity$2584 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2579 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2578 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$913, _tmp$2579
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$3833 =
    self$912->$0;
  int32_t _tmp$2580;
  int32_t capacity$2582;
  int32_t _tmp$2581;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2583;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3832;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$914;
  if (old_head$911) {
    moonbit_incref(old_head$911);
  }
  moonbit_decref(_old$3833);
  self$912->$0 = _tmp$2578;
  self$912->$2 = new_capacity$913;
  _tmp$2580 = new_capacity$913 - 1;
  self$912->$3 = _tmp$2580;
  capacity$2582 = self$912->$2;
  _tmp$2581 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2582);
  self$912->$4 = _tmp$2581;
  self$912->$1 = 0;
  _tmp$2583 = 0;
  _old$3832 = self$912->$5;
  if (_old$3832) {
    moonbit_decref(_old$3832);
  }
  self$912->$5 = _tmp$2583;
  self$912->$6 = -1;
  _param$914 = old_head$911;
  while (1) {
    if (_param$914 == 0) {
      if (_param$914) {
        moonbit_decref(_param$914);
      }
      moonbit_decref(self$912);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$915 =
        _param$914;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$916 =
        _Some$915;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3831 =
        _x$916->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$917 =
        _field$3831;
      moonbit_string_t _field$3830 = _x$916->$4;
      moonbit_string_t _key$918 = _field$3830;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3829 =
        _x$916->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$919 =
        _field$3829;
      int32_t _field$3828 = _x$916->$3;
      int32_t _cnt$4112 = Moonbit_object_header(_x$916)->rc;
      int32_t _hash$920;
      if (_cnt$4112 > 1) {
        int32_t _new_cnt$4113;
        moonbit_incref(_value$919);
        moonbit_incref(_key$918);
        if (_next$917) {
          moonbit_incref(_next$917);
        }
        _new_cnt$4113 = _cnt$4112 - 1;
        Moonbit_object_header(_x$916)->rc = _new_cnt$4113;
      } else if (_cnt$4112 == 1) {
        moonbit_free(_x$916);
      }
      _hash$920 = _field$3828;
      moonbit_incref(self$912);
      $$moonbitlang$core$builtin$Map$$set_with_hash$3(
        self$912, _key$918, _value$919, _hash$920
      );
      _param$914 = _next$917;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$901
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3841 =
    self$901->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$900 =
    _field$3841;
  int32_t capacity$2577 = self$901->$2;
  int32_t new_capacity$902 = capacity$2577 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2572 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2571 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$902, _tmp$2572
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$3840 =
    self$901->$0;
  int32_t _tmp$2573;
  int32_t capacity$2575;
  int32_t _tmp$2574;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2576;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3839;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$903;
  if (old_head$900) {
    moonbit_incref(old_head$900);
  }
  moonbit_decref(_old$3840);
  self$901->$0 = _tmp$2571;
  self$901->$2 = new_capacity$902;
  _tmp$2573 = new_capacity$902 - 1;
  self$901->$3 = _tmp$2573;
  capacity$2575 = self$901->$2;
  _tmp$2574 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2575);
  self$901->$4 = _tmp$2574;
  self$901->$1 = 0;
  _tmp$2576 = 0;
  _old$3839 = self$901->$5;
  if (_old$3839) {
    moonbit_decref(_old$3839);
  }
  self$901->$5 = _tmp$2576;
  self$901->$6 = -1;
  _param$903 = old_head$900;
  while (1) {
    if (_param$903 == 0) {
      if (_param$903) {
        moonbit_decref(_param$903);
      }
      moonbit_decref(self$901);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$904 =
        _param$903;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$905 =
        _Some$904;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3838 =
        _x$905->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$906 =
        _field$3838;
      moonbit_string_t _field$3837 = _x$905->$4;
      moonbit_string_t _key$907 = _field$3837;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3836 =
        _x$905->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$908 =
        _field$3836;
      int32_t _field$3835 = _x$905->$3;
      int32_t _cnt$4114 = Moonbit_object_header(_x$905)->rc;
      int32_t _hash$909;
      if (_cnt$4114 > 1) {
        int32_t _new_cnt$4115;
        moonbit_incref(_value$908);
        moonbit_incref(_key$907);
        if (_next$906) {
          moonbit_incref(_next$906);
        }
        _new_cnt$4115 = _cnt$4114 - 1;
        Moonbit_object_header(_x$905)->rc = _new_cnt$4115;
      } else if (_cnt$4114 == 1) {
        moonbit_free(_x$905);
      }
      _hash$909 = _field$3835;
      moonbit_incref(self$901);
      $$moonbitlang$core$builtin$Map$$set_with_hash$2(
        self$901, _key$907, _value$908, _hash$909
      );
      _param$903 = _next$906;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$890
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3847 =
    self$890->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$889 =
    _field$3847;
  int32_t capacity$2570 = self$890->$2;
  int32_t new_capacity$891 = capacity$2570 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2565 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$2564 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$891, _tmp$2565
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$3846 =
    self$890->$0;
  int32_t _tmp$2566;
  int32_t capacity$2568;
  int32_t _tmp$2567;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2569;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3845;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$892;
  if (old_head$889) {
    moonbit_incref(old_head$889);
  }
  moonbit_decref(_old$3846);
  self$890->$0 = _tmp$2564;
  self$890->$2 = new_capacity$891;
  _tmp$2566 = new_capacity$891 - 1;
  self$890->$3 = _tmp$2566;
  capacity$2568 = self$890->$2;
  _tmp$2567 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2568);
  self$890->$4 = _tmp$2567;
  self$890->$1 = 0;
  _tmp$2569 = 0;
  _old$3845 = self$890->$5;
  if (_old$3845) {
    moonbit_decref(_old$3845);
  }
  self$890->$5 = _tmp$2569;
  self$890->$6 = -1;
  _param$892 = old_head$889;
  while (1) {
    if (_param$892 == 0) {
      if (_param$892) {
        moonbit_decref(_param$892);
      }
      moonbit_decref(self$890);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$893 =
        _param$892;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _x$894 =
        _Some$893;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3844 =
        _x$894->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$895 =
        _field$3844;
      int32_t _key$896 = _x$894->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3843 =
        _x$894->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$897 =
        _field$3843;
      int32_t _field$3842 = _x$894->$3;
      int32_t _cnt$4116 = Moonbit_object_header(_x$894)->rc;
      int32_t _hash$898;
      if (_cnt$4116 > 1) {
        int32_t _new_cnt$4117;
        moonbit_incref(_value$897);
        if (_next$895) {
          moonbit_incref(_next$895);
        }
        _new_cnt$4117 = _cnt$4116 - 1;
        Moonbit_object_header(_x$894)->rc = _new_cnt$4117;
      } else if (_cnt$4116 == 1) {
        moonbit_free(_x$894);
      }
      _hash$898 = _field$3842;
      moonbit_incref(self$890);
      $$moonbitlang$core$builtin$Map$$set_with_hash$1(
        self$890, _key$896, _value$897, _hash$898
      );
      _param$892 = _next$895;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$879
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3854 =
    self$879->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$878 =
    _field$3854;
  int32_t capacity$2563 = self$879->$2;
  int32_t new_capacity$880 = capacity$2563 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2558 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2557 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$880, _tmp$2558
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$3853 =
    self$879->$0;
  int32_t _tmp$2559;
  int32_t capacity$2561;
  int32_t _tmp$2560;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2562;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3852;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$881;
  if (old_head$878) {
    moonbit_incref(old_head$878);
  }
  moonbit_decref(_old$3853);
  self$879->$0 = _tmp$2557;
  self$879->$2 = new_capacity$880;
  _tmp$2559 = new_capacity$880 - 1;
  self$879->$3 = _tmp$2559;
  capacity$2561 = self$879->$2;
  _tmp$2560 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2561);
  self$879->$4 = _tmp$2560;
  self$879->$1 = 0;
  _tmp$2562 = 0;
  _old$3852 = self$879->$5;
  if (_old$3852) {
    moonbit_decref(_old$3852);
  }
  self$879->$5 = _tmp$2562;
  self$879->$6 = -1;
  _param$881 = old_head$878;
  while (1) {
    if (_param$881 == 0) {
      if (_param$881) {
        moonbit_decref(_param$881);
      }
      moonbit_decref(self$879);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$882 =
        _param$881;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$883 =
        _Some$882;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3851 =
        _x$883->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$884 =
        _field$3851;
      moonbit_string_t _field$3850 = _x$883->$4;
      moonbit_string_t _key$885 = _field$3850;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3849 =
        _x$883->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$886 =
        _field$3849;
      int32_t _field$3848 = _x$883->$3;
      int32_t _cnt$4118 = Moonbit_object_header(_x$883)->rc;
      int32_t _hash$887;
      if (_cnt$4118 > 1) {
        int32_t _new_cnt$4119;
        moonbit_incref(_value$886);
        moonbit_incref(_key$885);
        if (_next$884) {
          moonbit_incref(_next$884);
        }
        _new_cnt$4119 = _cnt$4118 - 1;
        Moonbit_object_header(_x$883)->rc = _new_cnt$4119;
      } else if (_cnt$4118 == 1) {
        moonbit_free(_x$883);
      }
      _hash$887 = _field$3848;
      moonbit_incref(self$879);
      $$moonbitlang$core$builtin$Map$$set_with_hash$0(
        self$879, _key$885, _value$886, _hash$887
      );
      _param$881 = _next$884;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$862,
  moonbit_string_t key$871,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$872,
  int32_t hash$870
) {
  int32_t size$2543 = self$862->$1;
  int32_t grow_at$2544 = self$862->$4;
  int32_t capacity_mask$2556;
  int32_t _tmp$2555;
  struct $$3c$Int$2a$Int$3e$* _bind$863;
  int32_t psl$864;
  int32_t idx$865;
  int32_t _idx$873;
  int32_t _field$3855;
  int32_t _psl$874;
  int32_t _bind$875;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$876;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$877;
  if (size$2543 >= grow_at$2544) {
    moonbit_incref(self$862);
    $$moonbitlang$core$builtin$Map$$grow$3(self$862);
  }
  capacity_mask$2556 = self$862->$3;
  _tmp$2555 = hash$870 & capacity_mask$2556;
  psl$864 = 0;
  idx$865 = _tmp$2555;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3860 =
      self$862->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2554 =
      _field$3860;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3859;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$866;
    if (idx$865 < 0 || idx$865 >= Moonbit_array_length(entries$2554)) {
      moonbit_panic();
    }
    _tmp$3859
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2554[
        idx$865
      ];
    _bind$866 = _tmp$3859;
    if (_bind$866 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2545 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2545)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2545->$0 = idx$865;
      _tuple$2545->$1 = psl$864;
      _bind$863 = _tuple$2545;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$868 =
        _bind$866;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$869 =
        _Some$868;
      int32_t hash$2547 = _curr_entry$869->$3;
      int32_t _if_result$4415;
      int32_t psl$2548;
      int32_t _tmp$2550;
      int32_t _tmp$2552;
      int32_t capacity_mask$2553;
      int32_t _tmp$2551;
      if (hash$2547 == hash$870) {
        moonbit_string_t _field$3858 = _curr_entry$869->$4;
        moonbit_string_t key$2546 = _field$3858;
        int32_t _tmp$3857 = moonbit_val_array_equal(key$2546, key$871);
        _if_result$4415 = _tmp$3857;
      } else {
        _if_result$4415 = 0;
      }
      if (_if_result$4415) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3856;
        moonbit_incref(_curr_entry$869);
        moonbit_decref(key$871);
        moonbit_decref(self$862);
        _old$3856 = _curr_entry$869->$5;
        moonbit_decref(_old$3856);
        _curr_entry$869->$5 = value$872;
        moonbit_decref(_curr_entry$869);
        return 0;
      } else {
        moonbit_incref(_curr_entry$869);
      }
      psl$2548 = _curr_entry$869->$2;
      if (psl$864 > psl$2548) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2549;
        moonbit_incref(self$862);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$862, idx$865, _curr_entry$869
        );
        _tuple$2549
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2549)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2549->$0 = idx$865;
        _tuple$2549->$1 = psl$864;
        _bind$863 = _tuple$2549;
        break;
      } else {
        moonbit_decref(_curr_entry$869);
      }
      _tmp$2550 = psl$864 + 1;
      _tmp$2552 = idx$865 + 1;
      capacity_mask$2553 = self$862->$3;
      _tmp$2551 = _tmp$2552 & capacity_mask$2553;
      psl$864 = _tmp$2550;
      idx$865 = _tmp$2551;
      continue;
    }
    break;
  }
  _idx$873 = _bind$863->$0;
  _field$3855 = _bind$863->$1;
  moonbit_decref(_bind$863);
  _psl$874 = _field$3855;
  _bind$875 = self$862->$6;
  _bind$876 = 0;
  entry$877
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$877)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$877->$0 = _bind$875;
  entry$877->$1 = _bind$876;
  entry$877->$2 = _psl$874;
  entry$877->$3 = hash$870;
  entry$877->$4 = key$871;
  entry$877->$5 = value$872;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
    self$862, _idx$873, entry$877
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$846,
  moonbit_string_t key$855,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$856,
  int32_t hash$854
) {
  int32_t size$2529 = self$846->$1;
  int32_t grow_at$2530 = self$846->$4;
  int32_t capacity_mask$2542;
  int32_t _tmp$2541;
  struct $$3c$Int$2a$Int$3e$* _bind$847;
  int32_t psl$848;
  int32_t idx$849;
  int32_t _idx$857;
  int32_t _field$3861;
  int32_t _psl$858;
  int32_t _bind$859;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$860;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$861;
  if (size$2529 >= grow_at$2530) {
    moonbit_incref(self$846);
    $$moonbitlang$core$builtin$Map$$grow$2(self$846);
  }
  capacity_mask$2542 = self$846->$3;
  _tmp$2541 = hash$854 & capacity_mask$2542;
  psl$848 = 0;
  idx$849 = _tmp$2541;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3866 =
      self$846->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2540 =
      _field$3866;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3865;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$850;
    if (idx$849 < 0 || idx$849 >= Moonbit_array_length(entries$2540)) {
      moonbit_panic();
    }
    _tmp$3865
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2540[
        idx$849
      ];
    _bind$850 = _tmp$3865;
    if (_bind$850 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2531 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2531)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2531->$0 = idx$849;
      _tuple$2531->$1 = psl$848;
      _bind$847 = _tuple$2531;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$852 =
        _bind$850;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$853 =
        _Some$852;
      int32_t hash$2533 = _curr_entry$853->$3;
      int32_t _if_result$4417;
      int32_t psl$2534;
      int32_t _tmp$2536;
      int32_t _tmp$2538;
      int32_t capacity_mask$2539;
      int32_t _tmp$2537;
      if (hash$2533 == hash$854) {
        moonbit_string_t _field$3864 = _curr_entry$853->$4;
        moonbit_string_t key$2532 = _field$3864;
        int32_t _tmp$3863 = moonbit_val_array_equal(key$2532, key$855);
        _if_result$4417 = _tmp$3863;
      } else {
        _if_result$4417 = 0;
      }
      if (_if_result$4417) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3862;
        moonbit_incref(_curr_entry$853);
        moonbit_decref(key$855);
        moonbit_decref(self$846);
        _old$3862 = _curr_entry$853->$5;
        moonbit_decref(_old$3862);
        _curr_entry$853->$5 = value$856;
        moonbit_decref(_curr_entry$853);
        return 0;
      } else {
        moonbit_incref(_curr_entry$853);
      }
      psl$2534 = _curr_entry$853->$2;
      if (psl$848 > psl$2534) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2535;
        moonbit_incref(self$846);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$846, idx$849, _curr_entry$853
        );
        _tuple$2535
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2535)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2535->$0 = idx$849;
        _tuple$2535->$1 = psl$848;
        _bind$847 = _tuple$2535;
        break;
      } else {
        moonbit_decref(_curr_entry$853);
      }
      _tmp$2536 = psl$848 + 1;
      _tmp$2538 = idx$849 + 1;
      capacity_mask$2539 = self$846->$3;
      _tmp$2537 = _tmp$2538 & capacity_mask$2539;
      psl$848 = _tmp$2536;
      idx$849 = _tmp$2537;
      continue;
    }
    break;
  }
  _idx$857 = _bind$847->$0;
  _field$3861 = _bind$847->$1;
  moonbit_decref(_bind$847);
  _psl$858 = _field$3861;
  _bind$859 = self$846->$6;
  _bind$860 = 0;
  entry$861
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$861)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$861->$0 = _bind$859;
  entry$861->$1 = _bind$860;
  entry$861->$2 = _psl$858;
  entry$861->$3 = hash$854;
  entry$861->$4 = key$855;
  entry$861->$5 = value$856;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
    self$846, _idx$857, entry$861
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$830,
  int32_t key$839,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$840,
  int32_t hash$838
) {
  int32_t size$2515 = self$830->$1;
  int32_t grow_at$2516 = self$830->$4;
  int32_t capacity_mask$2528;
  int32_t _tmp$2527;
  struct $$3c$Int$2a$Int$3e$* _bind$831;
  int32_t psl$832;
  int32_t idx$833;
  int32_t _idx$841;
  int32_t _field$3867;
  int32_t _psl$842;
  int32_t _bind$843;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$844;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$845;
  if (size$2515 >= grow_at$2516) {
    moonbit_incref(self$830);
    $$moonbitlang$core$builtin$Map$$grow$1(self$830);
  }
  capacity_mask$2528 = self$830->$3;
  _tmp$2527 = hash$838 & capacity_mask$2528;
  psl$832 = 0;
  idx$833 = _tmp$2527;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3870 =
      self$830->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2526 =
      _field$3870;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3869;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$834;
    if (idx$833 < 0 || idx$833 >= Moonbit_array_length(entries$2526)) {
      moonbit_panic();
    }
    _tmp$3869
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2526[
        idx$833
      ];
    _bind$834 = _tmp$3869;
    if (_bind$834 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2517 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2517)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2517->$0 = idx$833;
      _tuple$2517->$1 = psl$832;
      _bind$831 = _tuple$2517;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$836 =
        _bind$834;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$837 =
        _Some$836;
      int32_t hash$2519 = _curr_entry$837->$3;
      int32_t _if_result$4419;
      int32_t psl$2520;
      int32_t _tmp$2522;
      int32_t _tmp$2524;
      int32_t capacity_mask$2525;
      int32_t _tmp$2523;
      if (hash$2519 == hash$838) {
        int32_t key$2518 = _curr_entry$837->$4;
        _if_result$4419 = key$2518 == key$839;
      } else {
        _if_result$4419 = 0;
      }
      if (_if_result$4419) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$3868;
        moonbit_incref(_curr_entry$837);
        moonbit_decref(self$830);
        _old$3868 = _curr_entry$837->$5;
        moonbit_decref(_old$3868);
        _curr_entry$837->$5 = value$840;
        moonbit_decref(_curr_entry$837);
        return 0;
      } else {
        moonbit_incref(_curr_entry$837);
      }
      psl$2520 = _curr_entry$837->$2;
      if (psl$832 > psl$2520) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2521;
        moonbit_incref(self$830);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$830, idx$833, _curr_entry$837
        );
        _tuple$2521
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2521)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2521->$0 = idx$833;
        _tuple$2521->$1 = psl$832;
        _bind$831 = _tuple$2521;
        break;
      } else {
        moonbit_decref(_curr_entry$837);
      }
      _tmp$2522 = psl$832 + 1;
      _tmp$2524 = idx$833 + 1;
      capacity_mask$2525 = self$830->$3;
      _tmp$2523 = _tmp$2524 & capacity_mask$2525;
      psl$832 = _tmp$2522;
      idx$833 = _tmp$2523;
      continue;
    }
    break;
  }
  _idx$841 = _bind$831->$0;
  _field$3867 = _bind$831->$1;
  moonbit_decref(_bind$831);
  _psl$842 = _field$3867;
  _bind$843 = self$830->$6;
  _bind$844 = 0;
  entry$845
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$845)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      2,
      0
  );
  entry$845->$0 = _bind$843;
  entry$845->$1 = _bind$844;
  entry$845->$2 = _psl$842;
  entry$845->$3 = hash$838;
  entry$845->$4 = key$839;
  entry$845->$5 = value$840;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
    self$830, _idx$841, entry$845
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$814,
  moonbit_string_t key$823,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$824,
  int32_t hash$822
) {
  int32_t size$2501 = self$814->$1;
  int32_t grow_at$2502 = self$814->$4;
  int32_t capacity_mask$2514;
  int32_t _tmp$2513;
  struct $$3c$Int$2a$Int$3e$* _bind$815;
  int32_t psl$816;
  int32_t idx$817;
  int32_t _idx$825;
  int32_t _field$3871;
  int32_t _psl$826;
  int32_t _bind$827;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$828;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$829;
  if (size$2501 >= grow_at$2502) {
    moonbit_incref(self$814);
    $$moonbitlang$core$builtin$Map$$grow$0(self$814);
  }
  capacity_mask$2514 = self$814->$3;
  _tmp$2513 = hash$822 & capacity_mask$2514;
  psl$816 = 0;
  idx$817 = _tmp$2513;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3876 =
      self$814->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2512 =
      _field$3876;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3875;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$818;
    if (idx$817 < 0 || idx$817 >= Moonbit_array_length(entries$2512)) {
      moonbit_panic();
    }
    _tmp$3875
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2512[
        idx$817
      ];
    _bind$818 = _tmp$3875;
    if (_bind$818 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2503 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2503)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2503->$0 = idx$817;
      _tuple$2503->$1 = psl$816;
      _bind$815 = _tuple$2503;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$820 =
        _bind$818;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$821 =
        _Some$820;
      int32_t hash$2505 = _curr_entry$821->$3;
      int32_t _if_result$4421;
      int32_t psl$2506;
      int32_t _tmp$2508;
      int32_t _tmp$2510;
      int32_t capacity_mask$2511;
      int32_t _tmp$2509;
      if (hash$2505 == hash$822) {
        moonbit_string_t _field$3874 = _curr_entry$821->$4;
        moonbit_string_t key$2504 = _field$3874;
        int32_t _tmp$3873 = moonbit_val_array_equal(key$2504, key$823);
        _if_result$4421 = _tmp$3873;
      } else {
        _if_result$4421 = 0;
      }
      if (_if_result$4421) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3872;
        moonbit_incref(_curr_entry$821);
        moonbit_decref(key$823);
        moonbit_decref(self$814);
        _old$3872 = _curr_entry$821->$5;
        moonbit_decref(_old$3872);
        _curr_entry$821->$5 = value$824;
        moonbit_decref(_curr_entry$821);
        return 0;
      } else {
        moonbit_incref(_curr_entry$821);
      }
      psl$2506 = _curr_entry$821->$2;
      if (psl$816 > psl$2506) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2507;
        moonbit_incref(self$814);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$814, idx$817, _curr_entry$821
        );
        _tuple$2507
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2507)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2507->$0 = idx$817;
        _tuple$2507->$1 = psl$816;
        _bind$815 = _tuple$2507;
        break;
      } else {
        moonbit_decref(_curr_entry$821);
      }
      _tmp$2508 = psl$816 + 1;
      _tmp$2510 = idx$817 + 1;
      capacity_mask$2511 = self$814->$3;
      _tmp$2509 = _tmp$2510 & capacity_mask$2511;
      psl$816 = _tmp$2508;
      idx$817 = _tmp$2509;
      continue;
    }
    break;
  }
  _idx$825 = _bind$815->$0;
  _field$3871 = _bind$815->$1;
  moonbit_decref(_bind$815);
  _psl$826 = _field$3871;
  _bind$827 = self$814->$6;
  _bind$828 = 0;
  entry$829
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$829)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$829->$0 = _bind$827;
  entry$829->$1 = _bind$828;
  entry$829->$2 = _psl$826;
  entry$829->$3 = hash$822;
  entry$829->$4 = key$823;
  entry$829->$5 = value$824;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
    self$814, _idx$825, entry$829
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$808,
  int32_t idx$813,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$812
) {
  int32_t psl$2500 = entry$812->$2;
  int32_t _tmp$2496 = psl$2500 + 1;
  int32_t _tmp$2498 = idx$813 + 1;
  int32_t capacity_mask$2499 = self$808->$3;
  int32_t _tmp$2497 = _tmp$2498 & capacity_mask$2499;
  int32_t psl$804 = _tmp$2496;
  int32_t idx$805 = _tmp$2497;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$806 =
    entry$812;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3878 =
      self$808->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2495 =
      _field$3878;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3877;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$807;
    if (idx$805 < 0 || idx$805 >= Moonbit_array_length(entries$2495)) {
      moonbit_panic();
    }
    _tmp$3877
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2495[
        idx$805
      ];
    _bind$807 = _tmp$3877;
    if (_bind$807 == 0) {
      entry$806->$2 = psl$804;
      $$moonbitlang$core$builtin$Map$$set_entry$3(
        self$808, entry$806, idx$805
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$810 =
        _bind$807;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$811 =
        _Some$810;
      int32_t psl$2485 = _curr_entry$811->$2;
      if (psl$804 > psl$2485) {
        int32_t psl$2490;
        int32_t _tmp$2486;
        int32_t _tmp$2488;
        int32_t capacity_mask$2489;
        int32_t _tmp$2487;
        entry$806->$2 = psl$804;
        moonbit_incref(_curr_entry$811);
        moonbit_incref(self$808);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$808, entry$806, idx$805
        );
        psl$2490 = _curr_entry$811->$2;
        _tmp$2486 = psl$2490 + 1;
        _tmp$2488 = idx$805 + 1;
        capacity_mask$2489 = self$808->$3;
        _tmp$2487 = _tmp$2488 & capacity_mask$2489;
        psl$804 = _tmp$2486;
        idx$805 = _tmp$2487;
        entry$806 = _curr_entry$811;
        continue;
      } else {
        int32_t _tmp$2491 = psl$804 + 1;
        int32_t _tmp$2493 = idx$805 + 1;
        int32_t capacity_mask$2494 = self$808->$3;
        int32_t _tmp$2492 = _tmp$2493 & capacity_mask$2494;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4423 =
          entry$806;
        psl$804 = _tmp$2491;
        idx$805 = _tmp$2492;
        entry$806 = _tmp$4423;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$798,
  int32_t idx$803,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$802
) {
  int32_t psl$2484 = entry$802->$2;
  int32_t _tmp$2480 = psl$2484 + 1;
  int32_t _tmp$2482 = idx$803 + 1;
  int32_t capacity_mask$2483 = self$798->$3;
  int32_t _tmp$2481 = _tmp$2482 & capacity_mask$2483;
  int32_t psl$794 = _tmp$2480;
  int32_t idx$795 = _tmp$2481;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$796 =
    entry$802;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3880 =
      self$798->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2479 =
      _field$3880;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3879;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$797;
    if (idx$795 < 0 || idx$795 >= Moonbit_array_length(entries$2479)) {
      moonbit_panic();
    }
    _tmp$3879
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2479[
        idx$795
      ];
    _bind$797 = _tmp$3879;
    if (_bind$797 == 0) {
      entry$796->$2 = psl$794;
      $$moonbitlang$core$builtin$Map$$set_entry$2(
        self$798, entry$796, idx$795
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$800 =
        _bind$797;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$801 =
        _Some$800;
      int32_t psl$2469 = _curr_entry$801->$2;
      if (psl$794 > psl$2469) {
        int32_t psl$2474;
        int32_t _tmp$2470;
        int32_t _tmp$2472;
        int32_t capacity_mask$2473;
        int32_t _tmp$2471;
        entry$796->$2 = psl$794;
        moonbit_incref(_curr_entry$801);
        moonbit_incref(self$798);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$798, entry$796, idx$795
        );
        psl$2474 = _curr_entry$801->$2;
        _tmp$2470 = psl$2474 + 1;
        _tmp$2472 = idx$795 + 1;
        capacity_mask$2473 = self$798->$3;
        _tmp$2471 = _tmp$2472 & capacity_mask$2473;
        psl$794 = _tmp$2470;
        idx$795 = _tmp$2471;
        entry$796 = _curr_entry$801;
        continue;
      } else {
        int32_t _tmp$2475 = psl$794 + 1;
        int32_t _tmp$2477 = idx$795 + 1;
        int32_t capacity_mask$2478 = self$798->$3;
        int32_t _tmp$2476 = _tmp$2477 & capacity_mask$2478;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4425 =
          entry$796;
        psl$794 = _tmp$2475;
        idx$795 = _tmp$2476;
        entry$796 = _tmp$4425;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$788,
  int32_t idx$793,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$792
) {
  int32_t psl$2468 = entry$792->$2;
  int32_t _tmp$2464 = psl$2468 + 1;
  int32_t _tmp$2466 = idx$793 + 1;
  int32_t capacity_mask$2467 = self$788->$3;
  int32_t _tmp$2465 = _tmp$2466 & capacity_mask$2467;
  int32_t psl$784 = _tmp$2464;
  int32_t idx$785 = _tmp$2465;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$786 =
    entry$792;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3882 =
      self$788->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2463 =
      _field$3882;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3881;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$787;
    if (idx$785 < 0 || idx$785 >= Moonbit_array_length(entries$2463)) {
      moonbit_panic();
    }
    _tmp$3881
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2463[
        idx$785
      ];
    _bind$787 = _tmp$3881;
    if (_bind$787 == 0) {
      entry$786->$2 = psl$784;
      $$moonbitlang$core$builtin$Map$$set_entry$1(
        self$788, entry$786, idx$785
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$790 =
        _bind$787;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$791 =
        _Some$790;
      int32_t psl$2453 = _curr_entry$791->$2;
      if (psl$784 > psl$2453) {
        int32_t psl$2458;
        int32_t _tmp$2454;
        int32_t _tmp$2456;
        int32_t capacity_mask$2457;
        int32_t _tmp$2455;
        entry$786->$2 = psl$784;
        moonbit_incref(_curr_entry$791);
        moonbit_incref(self$788);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$788, entry$786, idx$785
        );
        psl$2458 = _curr_entry$791->$2;
        _tmp$2454 = psl$2458 + 1;
        _tmp$2456 = idx$785 + 1;
        capacity_mask$2457 = self$788->$3;
        _tmp$2455 = _tmp$2456 & capacity_mask$2457;
        psl$784 = _tmp$2454;
        idx$785 = _tmp$2455;
        entry$786 = _curr_entry$791;
        continue;
      } else {
        int32_t _tmp$2459 = psl$784 + 1;
        int32_t _tmp$2461 = idx$785 + 1;
        int32_t capacity_mask$2462 = self$788->$3;
        int32_t _tmp$2460 = _tmp$2461 & capacity_mask$2462;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4427 =
          entry$786;
        psl$784 = _tmp$2459;
        idx$785 = _tmp$2460;
        entry$786 = _tmp$4427;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$778,
  int32_t idx$783,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$782
) {
  int32_t psl$2452 = entry$782->$2;
  int32_t _tmp$2448 = psl$2452 + 1;
  int32_t _tmp$2450 = idx$783 + 1;
  int32_t capacity_mask$2451 = self$778->$3;
  int32_t _tmp$2449 = _tmp$2450 & capacity_mask$2451;
  int32_t psl$774 = _tmp$2448;
  int32_t idx$775 = _tmp$2449;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$776 =
    entry$782;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3884 =
      self$778->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2447 =
      _field$3884;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3883;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$777;
    if (idx$775 < 0 || idx$775 >= Moonbit_array_length(entries$2447)) {
      moonbit_panic();
    }
    _tmp$3883
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2447[
        idx$775
      ];
    _bind$777 = _tmp$3883;
    if (_bind$777 == 0) {
      entry$776->$2 = psl$774;
      $$moonbitlang$core$builtin$Map$$set_entry$0(
        self$778, entry$776, idx$775
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$780 =
        _bind$777;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$781 =
        _Some$780;
      int32_t psl$2437 = _curr_entry$781->$2;
      if (psl$774 > psl$2437) {
        int32_t psl$2442;
        int32_t _tmp$2438;
        int32_t _tmp$2440;
        int32_t capacity_mask$2441;
        int32_t _tmp$2439;
        entry$776->$2 = psl$774;
        moonbit_incref(_curr_entry$781);
        moonbit_incref(self$778);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$778, entry$776, idx$775
        );
        psl$2442 = _curr_entry$781->$2;
        _tmp$2438 = psl$2442 + 1;
        _tmp$2440 = idx$775 + 1;
        capacity_mask$2441 = self$778->$3;
        _tmp$2439 = _tmp$2440 & capacity_mask$2441;
        psl$774 = _tmp$2438;
        idx$775 = _tmp$2439;
        entry$776 = _curr_entry$781;
        continue;
      } else {
        int32_t _tmp$2443 = psl$774 + 1;
        int32_t _tmp$2445 = idx$775 + 1;
        int32_t capacity_mask$2446 = self$778->$3;
        int32_t _tmp$2444 = _tmp$2445 & capacity_mask$2446;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4429 =
          entry$776;
        psl$774 = _tmp$2443;
        idx$775 = _tmp$2444;
        entry$776 = _tmp$4429;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$768,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$770,
  int32_t new_idx$769
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3887 =
    self$768->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2435 =
    _field$3887;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2436;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3886;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3885;
  int32_t _cnt$4120;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$771;
  moonbit_incref(entry$770);
  _tmp$2436 = entry$770;
  if (new_idx$769 < 0 || new_idx$769 >= Moonbit_array_length(entries$2435)) {
    moonbit_panic();
  }
  _old$3886
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2435[
      new_idx$769
    ];
  if (_old$3886) {
    moonbit_decref(_old$3886);
  }
  entries$2435[new_idx$769] = _tmp$2436;
  _field$3885 = entry$770->$1;
  _cnt$4120 = Moonbit_object_header(entry$770)->rc;
  if (_cnt$4120 > 1) {
    int32_t _new_cnt$4123;
    if (_field$3885) {
      moonbit_incref(_field$3885);
    }
    _new_cnt$4123 = _cnt$4120 - 1;
    Moonbit_object_header(entry$770)->rc = _new_cnt$4123;
  } else if (_cnt$4120 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4122 =
      entry$770->$5;
    moonbit_string_t _field$4121;
    moonbit_decref(_field$4122);
    _field$4121 = entry$770->$4;
    moonbit_decref(_field$4121);
    moonbit_free(entry$770);
  }
  _bind$771 = _field$3885;
  if (_bind$771 == 0) {
    if (_bind$771) {
      moonbit_decref(_bind$771);
    }
    self$768->$6 = new_idx$769;
    moonbit_decref(self$768);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$772;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$773;
    moonbit_decref(self$768);
    _Some$772 = _bind$771;
    _next$773 = _Some$772;
    _next$773->$0 = new_idx$769;
    moonbit_decref(_next$773);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$762,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$764,
  int32_t new_idx$763
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3890 =
    self$762->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2433 =
    _field$3890;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2434;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3889;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3888;
  int32_t _cnt$4124;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$765;
  moonbit_incref(entry$764);
  _tmp$2434 = entry$764;
  if (new_idx$763 < 0 || new_idx$763 >= Moonbit_array_length(entries$2433)) {
    moonbit_panic();
  }
  _old$3889
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2433[
      new_idx$763
    ];
  if (_old$3889) {
    moonbit_decref(_old$3889);
  }
  entries$2433[new_idx$763] = _tmp$2434;
  _field$3888 = entry$764->$1;
  _cnt$4124 = Moonbit_object_header(entry$764)->rc;
  if (_cnt$4124 > 1) {
    int32_t _new_cnt$4127;
    if (_field$3888) {
      moonbit_incref(_field$3888);
    }
    _new_cnt$4127 = _cnt$4124 - 1;
    Moonbit_object_header(entry$764)->rc = _new_cnt$4127;
  } else if (_cnt$4124 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4126 =
      entry$764->$5;
    moonbit_string_t _field$4125;
    moonbit_decref(_field$4126);
    _field$4125 = entry$764->$4;
    moonbit_decref(_field$4125);
    moonbit_free(entry$764);
  }
  _bind$765 = _field$3888;
  if (_bind$765 == 0) {
    if (_bind$765) {
      moonbit_decref(_bind$765);
    }
    self$762->$6 = new_idx$763;
    moonbit_decref(self$762);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$766;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$767;
    moonbit_decref(self$762);
    _Some$766 = _bind$765;
    _next$767 = _Some$766;
    _next$767->$0 = new_idx$763;
    moonbit_decref(_next$767);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$756,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$758,
  int32_t new_idx$757
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3893 =
    self$756->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2431 =
    _field$3893;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2432;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3892;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3891;
  int32_t _cnt$4128;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$759;
  moonbit_incref(entry$758);
  _tmp$2432 = entry$758;
  if (new_idx$757 < 0 || new_idx$757 >= Moonbit_array_length(entries$2431)) {
    moonbit_panic();
  }
  _old$3892
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2431[
      new_idx$757
    ];
  if (_old$3892) {
    moonbit_decref(_old$3892);
  }
  entries$2431[new_idx$757] = _tmp$2432;
  _field$3891 = entry$758->$1;
  _cnt$4128 = Moonbit_object_header(entry$758)->rc;
  if (_cnt$4128 > 1) {
    int32_t _new_cnt$4130;
    if (_field$3891) {
      moonbit_incref(_field$3891);
    }
    _new_cnt$4130 = _cnt$4128 - 1;
    Moonbit_object_header(entry$758)->rc = _new_cnt$4130;
  } else if (_cnt$4128 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$4129 =
      entry$758->$5;
    moonbit_decref(_field$4129);
    moonbit_free(entry$758);
  }
  _bind$759 = _field$3891;
  if (_bind$759 == 0) {
    if (_bind$759) {
      moonbit_decref(_bind$759);
    }
    self$756->$6 = new_idx$757;
    moonbit_decref(self$756);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$760;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$761;
    moonbit_decref(self$756);
    _Some$760 = _bind$759;
    _next$761 = _Some$760;
    _next$761->$0 = new_idx$757;
    moonbit_decref(_next$761);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$750,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$752,
  int32_t new_idx$751
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3896 =
    self$750->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2429 =
    _field$3896;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2430;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3895;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3894;
  int32_t _cnt$4131;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$753;
  moonbit_incref(entry$752);
  _tmp$2430 = entry$752;
  if (new_idx$751 < 0 || new_idx$751 >= Moonbit_array_length(entries$2429)) {
    moonbit_panic();
  }
  _old$3895
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2429[
      new_idx$751
    ];
  if (_old$3895) {
    moonbit_decref(_old$3895);
  }
  entries$2429[new_idx$751] = _tmp$2430;
  _field$3894 = entry$752->$1;
  _cnt$4131 = Moonbit_object_header(entry$752)->rc;
  if (_cnt$4131 > 1) {
    int32_t _new_cnt$4134;
    if (_field$3894) {
      moonbit_incref(_field$3894);
    }
    _new_cnt$4134 = _cnt$4131 - 1;
    Moonbit_object_header(entry$752)->rc = _new_cnt$4134;
  } else if (_cnt$4131 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4133 =
      entry$752->$5;
    moonbit_string_t _field$4132;
    moonbit_decref(_field$4133);
    _field$4132 = entry$752->$4;
    moonbit_decref(_field$4132);
    moonbit_free(entry$752);
  }
  _bind$753 = _field$3894;
  if (_bind$753 == 0) {
    if (_bind$753) {
      moonbit_decref(_bind$753);
    }
    self$750->$6 = new_idx$751;
    moonbit_decref(self$750);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$754;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$755;
    moonbit_decref(self$750);
    _Some$754 = _bind$753;
    _next$755 = _Some$754;
    _next$755->$0 = new_idx$751;
    moonbit_decref(_next$755);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$747,
  int32_t idx$749,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$748
) {
  int32_t _bind$746 = self$747->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3898;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2425;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2426;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3897;
  int32_t size$2428;
  int32_t _tmp$2427;
  switch (_bind$746) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2420;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3899;
      moonbit_incref(entry$748);
      _tmp$2420 = entry$748;
      _old$3899 = self$747->$5;
      if (_old$3899) {
        moonbit_decref(_old$3899);
      }
      self$747->$5 = _tmp$2420;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3902 =
        self$747->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2424 =
        _field$3902;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3901;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2423;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2421;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2422;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3900;
      if (_bind$746 < 0 || _bind$746 >= Moonbit_array_length(entries$2424)) {
        moonbit_panic();
      }
      _tmp$3901
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2424[
          _bind$746
        ];
      _tmp$2423 = _tmp$3901;
      if (_tmp$2423) {
        moonbit_incref(_tmp$2423);
      }
      _tmp$2421 = $Option$$unwrap$3(_tmp$2423);
      moonbit_incref(entry$748);
      _tmp$2422 = entry$748;
      _old$3900 = _tmp$2421->$1;
      if (_old$3900) {
        moonbit_decref(_old$3900);
      }
      _tmp$2421->$1 = _tmp$2422;
      moonbit_decref(_tmp$2421);
      break;
    }
  }
  self$747->$6 = idx$749;
  _field$3898 = self$747->$0;
  entries$2425 = _field$3898;
  _tmp$2426 = entry$748;
  if (idx$749 < 0 || idx$749 >= Moonbit_array_length(entries$2425)) {
    moonbit_panic();
  }
  _old$3897
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2425[
      idx$749
    ];
  if (_old$3897) {
    moonbit_decref(_old$3897);
  }
  entries$2425[idx$749] = _tmp$2426;
  size$2428 = self$747->$1;
  _tmp$2427 = size$2428 + 1;
  self$747->$1 = _tmp$2427;
  moonbit_decref(self$747);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$743,
  int32_t idx$745,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$744
) {
  int32_t _bind$742 = self$743->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3904;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2416;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2417;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3903;
  int32_t size$2419;
  int32_t _tmp$2418;
  switch (_bind$742) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2411;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3905;
      moonbit_incref(entry$744);
      _tmp$2411 = entry$744;
      _old$3905 = self$743->$5;
      if (_old$3905) {
        moonbit_decref(_old$3905);
      }
      self$743->$5 = _tmp$2411;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3908 =
        self$743->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2415 =
        _field$3908;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3907;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2414;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2412;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2413;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3906;
      if (_bind$742 < 0 || _bind$742 >= Moonbit_array_length(entries$2415)) {
        moonbit_panic();
      }
      _tmp$3907
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2415[
          _bind$742
        ];
      _tmp$2414 = _tmp$3907;
      if (_tmp$2414) {
        moonbit_incref(_tmp$2414);
      }
      _tmp$2412 = $Option$$unwrap$2(_tmp$2414);
      moonbit_incref(entry$744);
      _tmp$2413 = entry$744;
      _old$3906 = _tmp$2412->$1;
      if (_old$3906) {
        moonbit_decref(_old$3906);
      }
      _tmp$2412->$1 = _tmp$2413;
      moonbit_decref(_tmp$2412);
      break;
    }
  }
  self$743->$6 = idx$745;
  _field$3904 = self$743->$0;
  entries$2416 = _field$3904;
  _tmp$2417 = entry$744;
  if (idx$745 < 0 || idx$745 >= Moonbit_array_length(entries$2416)) {
    moonbit_panic();
  }
  _old$3903
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2416[
      idx$745
    ];
  if (_old$3903) {
    moonbit_decref(_old$3903);
  }
  entries$2416[idx$745] = _tmp$2417;
  size$2419 = self$743->$1;
  _tmp$2418 = size$2419 + 1;
  self$743->$1 = _tmp$2418;
  moonbit_decref(self$743);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$739,
  int32_t idx$741,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$740
) {
  int32_t _bind$738 = self$739->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3910;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2407;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2408;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3909;
  int32_t size$2410;
  int32_t _tmp$2409;
  switch (_bind$738) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2402;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3911;
      moonbit_incref(entry$740);
      _tmp$2402 = entry$740;
      _old$3911 = self$739->$5;
      if (_old$3911) {
        moonbit_decref(_old$3911);
      }
      self$739->$5 = _tmp$2402;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3914 =
        self$739->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2406 =
        _field$3914;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3913;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2405;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2403;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2404;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3912;
      if (_bind$738 < 0 || _bind$738 >= Moonbit_array_length(entries$2406)) {
        moonbit_panic();
      }
      _tmp$3913
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2406[
          _bind$738
        ];
      _tmp$2405 = _tmp$3913;
      if (_tmp$2405) {
        moonbit_incref(_tmp$2405);
      }
      _tmp$2403 = $Option$$unwrap$1(_tmp$2405);
      moonbit_incref(entry$740);
      _tmp$2404 = entry$740;
      _old$3912 = _tmp$2403->$1;
      if (_old$3912) {
        moonbit_decref(_old$3912);
      }
      _tmp$2403->$1 = _tmp$2404;
      moonbit_decref(_tmp$2403);
      break;
    }
  }
  self$739->$6 = idx$741;
  _field$3910 = self$739->$0;
  entries$2407 = _field$3910;
  _tmp$2408 = entry$740;
  if (idx$741 < 0 || idx$741 >= Moonbit_array_length(entries$2407)) {
    moonbit_panic();
  }
  _old$3909
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2407[
      idx$741
    ];
  if (_old$3909) {
    moonbit_decref(_old$3909);
  }
  entries$2407[idx$741] = _tmp$2408;
  size$2410 = self$739->$1;
  _tmp$2409 = size$2410 + 1;
  self$739->$1 = _tmp$2409;
  moonbit_decref(self$739);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$735,
  int32_t idx$737,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$736
) {
  int32_t _bind$734 = self$735->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3916;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2398;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2399;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3915;
  int32_t size$2401;
  int32_t _tmp$2400;
  switch (_bind$734) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2393;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3917;
      moonbit_incref(entry$736);
      _tmp$2393 = entry$736;
      _old$3917 = self$735->$5;
      if (_old$3917) {
        moonbit_decref(_old$3917);
      }
      self$735->$5 = _tmp$2393;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3920 =
        self$735->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2397 =
        _field$3920;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3919;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2396;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2394;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2395;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3918;
      if (_bind$734 < 0 || _bind$734 >= Moonbit_array_length(entries$2397)) {
        moonbit_panic();
      }
      _tmp$3919
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2397[
          _bind$734
        ];
      _tmp$2396 = _tmp$3919;
      if (_tmp$2396) {
        moonbit_incref(_tmp$2396);
      }
      _tmp$2394 = $Option$$unwrap$0(_tmp$2396);
      moonbit_incref(entry$736);
      _tmp$2395 = entry$736;
      _old$3918 = _tmp$2394->$1;
      if (_old$3918) {
        moonbit_decref(_old$3918);
      }
      _tmp$2394->$1 = _tmp$2395;
      moonbit_decref(_tmp$2394);
      break;
    }
  }
  self$735->$6 = idx$737;
  _field$3916 = self$735->$0;
  entries$2398 = _field$3916;
  _tmp$2399 = entry$736;
  if (idx$737 < 0 || idx$737 >= Moonbit_array_length(entries$2398)) {
    moonbit_panic();
  }
  _old$3915
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2398[
      idx$737
    ];
  if (_old$3915) {
    moonbit_decref(_old$3915);
  }
  entries$2398[idx$737] = _tmp$2399;
  size$2401 = self$735->$1;
  _tmp$2400 = size$2401 + 1;
  self$735->$1 = _tmp$2400;
  moonbit_decref(self$735);
  return 0;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$729
) {
  int32_t capacity$728 = $Int$$next_power_of_two(capacity$729);
  int32_t _bind$730 = capacity$728 - 1;
  int32_t _bind$731 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$728);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2392 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$732 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$728, _tmp$2392
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$733 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$4430 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4430)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4430->$0 = _bind$732;
  _block$4430->$1 = 0;
  _block$4430->$2 = capacity$728;
  _block$4430->$3 = _bind$730;
  _block$4430->$4 = _bind$731;
  _block$4430->$5 = _bind$733;
  _block$4430->$6 = -1;
  return _block$4430;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$723
) {
  int32_t capacity$722 = $Int$$next_power_of_two(capacity$723);
  int32_t _bind$724 = capacity$722 - 1;
  int32_t _bind$725 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$722);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2391 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$726 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$722, _tmp$2391
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$727 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$4431 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4431)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4431->$0 = _bind$726;
  _block$4431->$1 = 0;
  _block$4431->$2 = capacity$722;
  _block$4431->$3 = _bind$724;
  _block$4431->$4 = _bind$725;
  _block$4431->$5 = _bind$727;
  _block$4431->$6 = -1;
  return _block$4431;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$717
) {
  int32_t capacity$716 = $Int$$next_power_of_two(capacity$717);
  int32_t _bind$718 = capacity$716 - 1;
  int32_t _bind$719 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$716);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2390 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$720 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$716, _tmp$2390
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$721 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$4432 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4432)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4432->$0 = _bind$720;
  _block$4432->$1 = 0;
  _block$4432->$2 = capacity$716;
  _block$4432->$3 = _bind$718;
  _block$4432->$4 = _bind$719;
  _block$4432->$5 = _bind$721;
  _block$4432->$6 = -1;
  return _block$4432;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$711
) {
  int32_t capacity$710 = $Int$$next_power_of_two(capacity$711);
  int32_t _bind$712 = capacity$710 - 1;
  int32_t _bind$713 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$710);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2389 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$714 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$710, _tmp$2389
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$715 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$4433 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4433)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4433->$0 = _bind$714;
  _block$4433->$1 = 0;
  _block$4433->$2 = capacity$710;
  _block$4433->$3 = _bind$712;
  _block$4433->$4 = _bind$713;
  _block$4433->$5 = _bind$715;
  _block$4433->$6 = -1;
  return _block$4433;
}

int32_t $Int$$next_power_of_two(int32_t self$709) {
  if (self$709 >= 0) {
    int32_t _tmp$2388;
    int32_t _tmp$2387;
    int32_t _tmp$2386;
    int32_t _tmp$2385;
    if (self$709 <= 1) {
      return 1;
    }
    if (self$709 > 1073741824) {
      return 1073741824;
    }
    _tmp$2388 = self$709 - 1;
    _tmp$2387 = moonbit_clz32(_tmp$2388);
    _tmp$2386 = _tmp$2387 - 1;
    _tmp$2385 = 2147483647 >> (_tmp$2386 & 31);
    return _tmp$2385 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$708) {
  int32_t _tmp$2384 = capacity$708 * 13;
  return _tmp$2384 / 16;
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$706
) {
  if (self$706 == 0) {
    if (self$706) {
      moonbit_decref(self$706);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$707 =
      self$706;
    return _Some$707;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$704
) {
  if (self$704 == 0) {
    if (self$704) {
      moonbit_decref(self$704);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$705 =
      self$704;
    return _Some$705;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$702
) {
  if (self$702 == 0) {
    if (self$702) {
      moonbit_decref(self$702);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$703 =
      self$702;
    return _Some$703;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$700
) {
  if (self$700 == 0) {
    if (self$700) {
      moonbit_decref(self$700);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$701 =
      self$700;
    return _Some$701;
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$2(
  int32_t self$694,
  int32_t other$695
) {
  if (self$694 == -1) {
    return other$695 == -1;
  } else {
    int32_t _Some$696 = self$694;
    int32_t _x$697 = _Some$696;
    if (other$695 == -1) {
      return 0;
    } else {
      int32_t _Some$698 = other$695;
      int32_t _y$699 = _Some$698;
      return _x$697 == _y$699;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$1(
  moonbit_string_t self$688,
  moonbit_string_t other$689
) {
  if (self$688 == 0) {
    int32_t _tmp$3921;
    if (self$688) {
      moonbit_decref(self$688);
    }
    _tmp$3921 = other$689 == 0;
    if (other$689) {
      moonbit_decref(other$689);
    }
    return _tmp$3921;
  } else {
    moonbit_string_t _Some$690 = self$688;
    moonbit_string_t _x$691 = _Some$690;
    if (other$689 == 0) {
      moonbit_decref(_x$691);
      if (other$689) {
        moonbit_decref(other$689);
      }
      return 0;
    } else {
      moonbit_string_t _Some$692 = other$689;
      moonbit_string_t _y$693 = _Some$692;
      int32_t _tmp$3922 = moonbit_val_array_equal(_x$691, _y$693);
      moonbit_decref(_x$691);
      moonbit_decref(_y$693);
      return _tmp$3922;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  int64_t self$682,
  int64_t other$683
) {
  if (self$682 == 4294967296ll) {
    return other$683 == 4294967296ll;
  } else {
    int64_t _Some$684 = self$682;
    int32_t _x$685 = (int32_t)_Some$684;
    if (other$683 == 4294967296ll) {
      return 0;
    } else {
      int64_t _Some$686 = other$683;
      int32_t _y$687 = (int32_t)_Some$686;
      return _x$685 == _y$687;
    }
  }
}

uint32_t $ReadOnlyArray$$at$1(uint32_t* self$680, int32_t index$681) {
  uint32_t* _tmp$2383 = self$680;
  uint32_t _tmp$3923;
  if (index$681 < 0 || index$681 >= Moonbit_array_length(_tmp$2383)) {
    moonbit_panic();
  }
  _tmp$3923 = (uint32_t)_tmp$2383[index$681];
  moonbit_decref(_tmp$2383);
  return _tmp$3923;
}

uint64_t $ReadOnlyArray$$at$0(uint64_t* self$678, int32_t index$679) {
  uint64_t* _tmp$2382 = self$678;
  uint64_t _tmp$3924;
  if (index$679 < 0 || index$679 >= Moonbit_array_length(_tmp$2382)) {
    moonbit_panic();
  }
  _tmp$3924 = (uint64_t)_tmp$2382[index$679];
  moonbit_decref(_tmp$2382);
  return _tmp$3924;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$677
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$2381 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$677);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$2381);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$676
) {
  moonbit_string_t* _field$3926 = self$676->$0;
  moonbit_string_t* buf$2379 = _field$3926;
  int32_t _field$3925 = self$676->$1;
  int32_t _cnt$4135 = Moonbit_object_header(self$676)->rc;
  int32_t len$2380;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$2378;
  if (_cnt$4135 > 1) {
    int32_t _new_cnt$4136;
    moonbit_incref(buf$2379);
    _new_cnt$4136 = _cnt$4135 - 1;
    Moonbit_object_header(self$676)->rc = _new_cnt$4136;
  } else if (_cnt$4135 == 1) {
    moonbit_free(self$676);
  }
  len$2380 = _field$3925;
  _tmp$2378
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$2380, buf$2379
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$2378);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$674
) {
  struct $Ref$3c$Int$3e$* i$673 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$4434;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$2367;
  Moonbit_object_header(i$673)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$673->$0 = 0;
  _closure$4434
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$4434)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$4434->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$4434->$0_0 = self$674.$0;
  _closure$4434->$0_1 = self$674.$1;
  _closure$4434->$0_2 = self$674.$2;
  _closure$4434->$1 = i$673;
  _tmp$2367 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$4434;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$2367);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$2368
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$2369 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$2368;
  struct $Ref$3c$Int$3e$* _field$3931 = _casted_env$2369->$1;
  struct $Ref$3c$Int$3e$* i$673 = _field$3931;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$3930 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$2369->$0_1, _casted_env$2369->$0_2, _casted_env$2369->$0_0
    };
  int32_t _cnt$4137 = Moonbit_object_header(_casted_env$2369)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$674;
  int32_t val$2370;
  int32_t _tmp$2371;
  if (_cnt$4137 > 1) {
    int32_t _new_cnt$4138;
    moonbit_incref(i$673);
    moonbit_incref(_field$3930.$0);
    _new_cnt$4138 = _cnt$4137 - 1;
    Moonbit_object_header(_casted_env$2369)->rc = _new_cnt$4138;
  } else if (_cnt$4137 == 1) {
    moonbit_free(_casted_env$2369);
  }
  self$674 = _field$3930;
  val$2370 = i$673->$0;
  moonbit_incref(self$674.$0);
  _tmp$2371 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$674);
  if (val$2370 < _tmp$2371) {
    moonbit_string_t* _field$3929 = self$674.$0;
    moonbit_string_t* buf$2374 = _field$3929;
    int32_t _field$3928 = self$674.$1;
    int32_t start$2376 = _field$3928;
    int32_t val$2377 = i$673->$0;
    int32_t _tmp$2375 = start$2376 + val$2377;
    moonbit_string_t _tmp$3927 = (moonbit_string_t)buf$2374[_tmp$2375];
    moonbit_string_t elem$675;
    int32_t val$2373;
    int32_t _tmp$2372;
    moonbit_incref(_tmp$3927);
    moonbit_decref(buf$2374);
    elem$675 = _tmp$3927;
    val$2373 = i$673->$0;
    _tmp$2372 = val$2373 + 1;
    i$673->$0 = _tmp$2372;
    moonbit_decref(i$673);
    return elem$675;
  } else {
    moonbit_decref(self$674.$0);
    moonbit_decref(i$673);
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$2(
  int32_t self$669,
  struct $$moonbitlang$core$builtin$Logger logger$670
) {
  if (self$669 == -1) {
    logger$670.$0->$method_0(
      logger$670.$1, (moonbit_string_t)moonbit_string_literal_150.data
    );
  } else {
    int32_t _Some$671 = self$669;
    int32_t _arg$672 = _Some$671;
    struct $$moonbitlang$core$builtin$Logger _bind$2366;
    if (logger$670.$1) {
      moonbit_incref(logger$670.$1);
    }
    logger$670.$0->$method_0(
      logger$670.$1, (moonbit_string_t)moonbit_string_literal_151.data
    );
    if (logger$670.$1) {
      moonbit_incref(logger$670.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$2(logger$670, _arg$672);
    _bind$2366 = logger$670;
    _bind$2366.$0->$method_0(
      _bind$2366.$1, (moonbit_string_t)moonbit_string_literal_152.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$1(
  moonbit_string_t self$665,
  struct $$moonbitlang$core$builtin$Logger logger$666
) {
  if (self$665 == 0) {
    if (self$665) {
      moonbit_decref(self$665);
    }
    logger$666.$0->$method_0(
      logger$666.$1, (moonbit_string_t)moonbit_string_literal_150.data
    );
  } else {
    moonbit_string_t _Some$667 = self$665;
    moonbit_string_t _arg$668 = _Some$667;
    struct $$moonbitlang$core$builtin$Logger _bind$2365;
    if (logger$666.$1) {
      moonbit_incref(logger$666.$1);
    }
    logger$666.$0->$method_0(
      logger$666.$1, (moonbit_string_t)moonbit_string_literal_151.data
    );
    if (logger$666.$1) {
      moonbit_incref(logger$666.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$1(logger$666, _arg$668);
    _bind$2365 = logger$666;
    _bind$2365.$0->$method_0(
      _bind$2365.$1, (moonbit_string_t)moonbit_string_literal_152.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  int64_t self$661,
  struct $$moonbitlang$core$builtin$Logger logger$662
) {
  if (self$661 == 4294967296ll) {
    logger$662.$0->$method_0(
      logger$662.$1, (moonbit_string_t)moonbit_string_literal_150.data
    );
  } else {
    int64_t _Some$663 = self$661;
    int32_t _arg$664 = (int32_t)_Some$663;
    struct $$moonbitlang$core$builtin$Logger _bind$2364;
    if (logger$662.$1) {
      moonbit_incref(logger$662.$1);
    }
    logger$662.$0->$method_0(
      logger$662.$1, (moonbit_string_t)moonbit_string_literal_151.data
    );
    if (logger$662.$1) {
      moonbit_incref(logger$662.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$0(logger$662, _arg$664);
    _bind$2364 = logger$662;
    _bind$2364.$0->$method_0(
      _bind$2364.$1, (moonbit_string_t)moonbit_string_literal_152.data
    );
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$660
) {
  return self$660;
}

int32_t $$moonbitlang$core$builtin$Show$$UInt16$$output(
  int32_t self$659,
  struct $$moonbitlang$core$builtin$Logger logger$658
) {
  moonbit_string_t _tmp$2363 = $UInt16$$to_string$inner(self$659, 10);
  logger$658.$0->$method_0(logger$658.$1, _tmp$2363);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$UInt64$$output(
  uint64_t self$657,
  struct $$moonbitlang$core$builtin$Logger logger$656
) {
  moonbit_string_t _tmp$2362 = $UInt64$$to_string$inner(self$657, 10);
  logger$656.$0->$method_0(logger$656.$1, _tmp$2362);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$655,
  struct $$moonbitlang$core$builtin$Logger logger$654
) {
  moonbit_string_t _tmp$2361 = $Int$$to_string$inner(self$655, 10);
  logger$654.$0->$method_0(logger$654.$1, _tmp$2361);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$652,
  struct $$moonbitlang$core$builtin$Logger logger$653
) {
  if (self$652) {
    logger$653.$0->$method_0(
      logger$653.$1, (moonbit_string_t)moonbit_string_literal_153.data
    );
  } else {
    logger$653.$0->$method_0(
      logger$653.$1, (moonbit_string_t)moonbit_string_literal_154.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$650,
  struct $$3c$String$3e$$3d$$3e$Int* f$651
) {
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _func$649 = self$650;
  return _func$649->code(_func$649, f$651);
}

int32_t $String$$contains(
  moonbit_string_t self$647,
  struct $StringView str$648
) {
  int32_t _tmp$2360 = Moonbit_array_length(self$647);
  struct $StringView _tmp$2359 = (struct $StringView){0, _tmp$2360, self$647};
  return $StringView$$contains(_tmp$2359, str$648);
}

int32_t $StringView$$contains(
  struct $StringView self$645,
  struct $StringView str$646
) {
  int64_t _bind$644 = $StringView$$find(self$645, str$646);
  int32_t _tmp$2358 = _bind$644 == 4294967296ll;
  return !_tmp$2358;
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$641,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$643
) {
  int32_t len$2353 = self$641->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$2355;
  int32_t _tmp$3934;
  int32_t _tmp$2354;
  int32_t length$642;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$3933;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$2356;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$3932;
  int32_t _tmp$2357;
  moonbit_incref(self$641);
  _tmp$2355 = $$moonbitlang$core$builtin$Array$$buffer$2(self$641);
  _tmp$3934 = Moonbit_array_length(_tmp$2355);
  moonbit_decref(_tmp$2355);
  _tmp$2354 = _tmp$3934;
  if (len$2353 == _tmp$2354) {
    moonbit_incref(self$641);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$641);
  }
  length$642 = self$641->$1;
  _field$3933 = self$641->$0;
  buf$2356 = _field$3933;
  _old$3932
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$2356[
      length$642
    ];
  if (_old$3932) {
    moonbit_decref(_old$3932);
  }
  buf$2356[length$642] = value$643;
  _tmp$2357 = length$642 + 1;
  self$641->$1 = _tmp$2357;
  moonbit_decref(self$641);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$638,
  struct $$3c$String$2a$Int$3e$* value$640
) {
  int32_t len$2348 = self$638->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$2350;
  int32_t _tmp$3937;
  int32_t _tmp$2349;
  int32_t length$639;
  struct $$3c$String$2a$Int$3e$** _field$3936;
  struct $$3c$String$2a$Int$3e$** buf$2351;
  struct $$3c$String$2a$Int$3e$* _old$3935;
  int32_t _tmp$2352;
  moonbit_incref(self$638);
  _tmp$2350 = $$moonbitlang$core$builtin$Array$$buffer$0(self$638);
  _tmp$3937 = Moonbit_array_length(_tmp$2350);
  moonbit_decref(_tmp$2350);
  _tmp$2349 = _tmp$3937;
  if (len$2348 == _tmp$2349) {
    moonbit_incref(self$638);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$638);
  }
  length$639 = self$638->$1;
  _field$3936 = self$638->$0;
  buf$2351 = _field$3936;
  _old$3935 = (struct $$3c$String$2a$Int$3e$*)buf$2351[length$639];
  if (_old$3935) {
    moonbit_decref(_old$3935);
  }
  buf$2351[length$639] = value$640;
  _tmp$2352 = length$639 + 1;
  self$638->$1 = _tmp$2352;
  moonbit_decref(self$638);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$635,
  moonbit_string_t value$637
) {
  int32_t len$2343 = self$635->$1;
  moonbit_string_t* _tmp$2345;
  int32_t _tmp$3940;
  int32_t _tmp$2344;
  int32_t length$636;
  moonbit_string_t* _field$3939;
  moonbit_string_t* buf$2346;
  moonbit_string_t _old$3938;
  int32_t _tmp$2347;
  moonbit_incref(self$635);
  _tmp$2345 = $$moonbitlang$core$builtin$Array$$buffer$1(self$635);
  _tmp$3940 = Moonbit_array_length(_tmp$2345);
  moonbit_decref(_tmp$2345);
  _tmp$2344 = _tmp$3940;
  if (len$2343 == _tmp$2344) {
    moonbit_incref(self$635);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$635);
  }
  length$636 = self$635->$1;
  _field$3939 = self$635->$0;
  buf$2346 = _field$3939;
  _old$3938 = (moonbit_string_t)buf$2346[length$636];
  moonbit_decref(_old$3938);
  buf$2346[length$636] = value$637;
  _tmp$2347 = length$636 + 1;
  self$635->$1 = _tmp$2347;
  moonbit_decref(self$635);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$633
) {
  int32_t old_cap$632 = self$633->$1;
  int32_t new_cap$634;
  if (old_cap$632 == 0) {
    new_cap$634 = 8;
  } else {
    new_cap$634 = old_cap$632 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$633, new_cap$634);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$630
) {
  int32_t old_cap$629 = self$630->$1;
  int32_t new_cap$631;
  if (old_cap$629 == 0) {
    new_cap$631 = 8;
  } else {
    new_cap$631 = old_cap$629 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$630, new_cap$631);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$627
) {
  int32_t old_cap$626 = self$627->$1;
  int32_t new_cap$628;
  if (old_cap$626 == 0) {
    new_cap$628 = 8;
  } else {
    new_cap$628 = old_cap$626 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$627, new_cap$628);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$623,
  int32_t new_capacity$621
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** new_buf$620 =
    (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_make_ref_array(
      new_capacity$621, 0
    );
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$3942 =
    self$623->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$622 =
    _field$3942;
  int32_t old_cap$624 = Moonbit_array_length(old_buf$622);
  int32_t copy_len$625;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$3941;
  if (old_cap$624 < new_capacity$621) {
    copy_len$625 = old_cap$624;
  } else {
    copy_len$625 = new_capacity$621;
  }
  moonbit_incref(old_buf$622);
  moonbit_incref(new_buf$620);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$620, 0, old_buf$622, 0, copy_len$625
  );
  _old$3941 = self$623->$0;
  moonbit_decref(_old$3941);
  self$623->$0 = new_buf$620;
  moonbit_decref(self$623);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$617,
  int32_t new_capacity$615
) {
  struct $$3c$String$2a$Int$3e$** new_buf$614 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_make_ref_array(
      new_capacity$615, 0
    );
  struct $$3c$String$2a$Int$3e$** _field$3944 = self$617->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$616 = _field$3944;
  int32_t old_cap$618 = Moonbit_array_length(old_buf$616);
  int32_t copy_len$619;
  struct $$3c$String$2a$Int$3e$** _old$3943;
  if (old_cap$618 < new_capacity$615) {
    copy_len$619 = old_cap$618;
  } else {
    copy_len$619 = new_capacity$615;
  }
  moonbit_incref(old_buf$616);
  moonbit_incref(new_buf$614);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$614, 0, old_buf$616, 0, copy_len$619
  );
  _old$3943 = self$617->$0;
  moonbit_decref(_old$3943);
  self$617->$0 = new_buf$614;
  moonbit_decref(self$617);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$611,
  int32_t new_capacity$609
) {
  moonbit_string_t* new_buf$608 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$609, (moonbit_string_t)moonbit_string_literal_3.data
    );
  moonbit_string_t* _field$3946 = self$611->$0;
  moonbit_string_t* old_buf$610 = _field$3946;
  int32_t old_cap$612 = Moonbit_array_length(old_buf$610);
  int32_t copy_len$613;
  moonbit_string_t* _old$3945;
  if (old_cap$612 < new_capacity$609) {
    copy_len$613 = old_cap$612;
  } else {
    copy_len$613 = new_capacity$609;
  }
  moonbit_incref(old_buf$610);
  moonbit_incref(new_buf$608);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$608, 0, old_buf$610, 0, copy_len$613
  );
  _old$3945 = self$611->$0;
  moonbit_decref(_old$3945);
  self$611->$0 = new_buf$608;
  moonbit_decref(self$611);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$607
) {
  if (capacity$607 == 0) {
    moonbit_string_t* _tmp$2341 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$4435 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$4435)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$4435->$0 = _tmp$2341;
    _block$4435->$1 = 0;
    return _block$4435;
  } else {
    moonbit_string_t* _tmp$2342 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$607, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$4436 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$4436)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$4436->$0 = _tmp$2342;
    _block$4436->$1 = 0;
    return _block$4436;
  }
}

moonbit_string_t $String$$repeat(moonbit_string_t self$601, int32_t n$600) {
  if (n$600 <= 0) {
    moonbit_decref(self$601);
    return (moonbit_string_t)moonbit_string_literal_3.data;
  } else if (n$600 == 1) {
    return self$601;
  } else {
    int32_t len$602 = Moonbit_array_length(self$601);
    int32_t _tmp$2340 = len$602 * n$600;
    struct $$moonbitlang$core$builtin$StringBuilder* buf$603 =
      $$moonbitlang$core$builtin$StringBuilder$$new$inner(_tmp$2340);
    moonbit_string_t str$604 = self$601;
    int32_t _$605 = 0;
    while (1) {
      if (_$605 < n$600) {
        int32_t _tmp$2339;
        moonbit_incref(str$604);
        moonbit_incref(buf$603);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$603, str$604
        );
        _tmp$2339 = _$605 + 1;
        _$605 = _tmp$2339;
        continue;
      } else {
        moonbit_decref(str$604);
      }
      break;
    }
    return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$603);
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$598,
  struct $StringView str$599
) {
  int32_t len$2327 = self$598->$1;
  int32_t _tmp$2329;
  int32_t _tmp$2328;
  int32_t _tmp$2326;
  moonbit_bytes_t _field$3947;
  moonbit_bytes_t data$2330;
  int32_t len$2331;
  moonbit_string_t _tmp$2332;
  int32_t _tmp$2333;
  int32_t _tmp$2334;
  int32_t len$2336;
  int32_t _tmp$2338;
  int32_t _tmp$2337;
  int32_t _tmp$2335;
  moonbit_incref(str$599.$0);
  _tmp$2329 = $StringView$$length(str$599);
  _tmp$2328 = _tmp$2329 * 2;
  _tmp$2326 = len$2327 + _tmp$2328;
  moonbit_incref(self$598);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$598, _tmp$2326
  );
  _field$3947 = self$598->$0;
  data$2330 = _field$3947;
  len$2331 = self$598->$1;
  moonbit_incref(data$2330);
  moonbit_incref(str$599.$0);
  _tmp$2332 = $StringView$$data(str$599);
  moonbit_incref(str$599.$0);
  _tmp$2333 = $StringView$$start_offset(str$599);
  moonbit_incref(str$599.$0);
  _tmp$2334 = $StringView$$length(str$599);
  $FixedArray$$blit_from_string(
    data$2330, len$2331, _tmp$2332, _tmp$2333, _tmp$2334
  );
  len$2336 = self$598->$1;
  _tmp$2338 = $StringView$$length(str$599);
  _tmp$2337 = _tmp$2338 * 2;
  _tmp$2335 = len$2336 + _tmp$2337;
  self$598->$1 = _tmp$2335;
  moonbit_decref(self$598);
  return 0;
}

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$595,
  int32_t i$596,
  int32_t start_offset$597,
  int64_t end_offset$593
) {
  int32_t end_offset$592;
  if (end_offset$593 == 4294967296ll) {
    end_offset$592 = Moonbit_array_length(self$595);
  } else {
    int64_t _Some$594 = end_offset$593;
    end_offset$592 = (int32_t)_Some$594;
  }
  if (i$596 >= 0) {
    return $String$$offset_of_nth_char_forward(
             self$595, i$596, start_offset$597, end_offset$592
           );
  } else {
    int32_t _tmp$2325 = -i$596;
    return $String$$offset_of_nth_char_backward(
             self$595, _tmp$2325, start_offset$597, end_offset$592
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$590,
  int32_t n$588,
  int32_t start_offset$584,
  int32_t end_offset$585
) {
  int32_t _if_result$4438;
  if (start_offset$584 >= 0) {
    _if_result$4438 = start_offset$584 <= end_offset$585;
  } else {
    _if_result$4438 = 0;
  }
  if (_if_result$4438) {
    int32_t utf16_offset$586 = start_offset$584;
    int32_t char_count$587 = 0;
    int32_t _tmp$2323;
    int32_t _if_result$4441;
    while (1) {
      int32_t _tmp$2317 = utf16_offset$586;
      int32_t _if_result$4440;
      if (_tmp$2317 < end_offset$585) {
        int32_t _tmp$2316 = char_count$587;
        _if_result$4440 = _tmp$2316 < n$588;
      } else {
        _if_result$4440 = 0;
      }
      if (_if_result$4440) {
        int32_t _tmp$2321 = utf16_offset$586;
        int32_t c$589 = self$590[_tmp$2321];
        int32_t _tmp$2320;
        if ($Int$$is_leading_surrogate(c$589)) {
          int32_t _tmp$2318 = utf16_offset$586;
          utf16_offset$586 = _tmp$2318 + 2;
        } else {
          int32_t _tmp$2319 = utf16_offset$586;
          utf16_offset$586 = _tmp$2319 + 1;
        }
        _tmp$2320 = char_count$587;
        char_count$587 = _tmp$2320 + 1;
        continue;
      } else {
        moonbit_decref(self$590);
      }
      break;
    }
    _tmp$2323 = char_count$587;
    if (_tmp$2323 < n$588) {
      _if_result$4441 = 1;
    } else {
      int32_t _tmp$2322 = utf16_offset$586;
      _if_result$4441 = _tmp$2322 >= end_offset$585;
    }
    if (_if_result$4441) {
      return 4294967296ll;
    } else {
      int32_t _tmp$2324 = utf16_offset$586;
      return (int64_t)_tmp$2324;
    }
  } else {
    moonbit_decref(self$590);
    return $moonbitlang$core$builtin$abort$3(
             (moonbit_string_t)moonbit_string_literal_155.data,
               (moonbit_string_t)moonbit_string_literal_156.data
           );
  }
}

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$582,
  int32_t n$580,
  int32_t start_offset$579,
  int32_t end_offset$578
) {
  int32_t char_count$576 = 0;
  int32_t utf16_offset$577 = end_offset$578;
  int32_t _tmp$2314;
  int32_t _if_result$4444;
  while (1) {
    int32_t _tmp$2307 = utf16_offset$577;
    int32_t _tmp$2306 = _tmp$2307 - 1;
    int32_t _if_result$4443;
    if (_tmp$2306 >= start_offset$579) {
      int32_t _tmp$2305 = char_count$576;
      _if_result$4443 = _tmp$2305 < n$580;
    } else {
      _if_result$4443 = 0;
    }
    if (_if_result$4443) {
      int32_t _tmp$2312 = utf16_offset$577;
      int32_t _tmp$2311 = _tmp$2312 - 1;
      int32_t c$581 = self$582[_tmp$2311];
      int32_t _tmp$2310;
      if ($Int$$is_trailing_surrogate(c$581)) {
        int32_t _tmp$2308 = utf16_offset$577;
        utf16_offset$577 = _tmp$2308 - 2;
      } else {
        int32_t _tmp$2309 = utf16_offset$577;
        utf16_offset$577 = _tmp$2309 - 1;
      }
      _tmp$2310 = char_count$576;
      char_count$576 = _tmp$2310 + 1;
      continue;
    } else {
      moonbit_decref(self$582);
    }
    break;
  }
  _tmp$2314 = char_count$576;
  if (_tmp$2314 < n$580) {
    _if_result$4444 = 1;
  } else {
    int32_t _tmp$2313 = utf16_offset$577;
    _if_result$4444 = _tmp$2313 < start_offset$579;
  }
  if (_if_result$4444) {
    return 4294967296ll;
  } else {
    int32_t _tmp$2315 = utf16_offset$577;
    return (int64_t)_tmp$2315;
  }
}

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$568,
  int32_t len$571,
  int32_t start_offset$575,
  int64_t end_offset$566
) {
  int32_t end_offset$565;
  int32_t index$569;
  int32_t count$570;
  if (end_offset$566 == 4294967296ll) {
    end_offset$565 = Moonbit_array_length(self$568);
  } else {
    int64_t _Some$567 = end_offset$566;
    end_offset$565 = (int32_t)_Some$567;
  }
  index$569 = start_offset$575;
  count$570 = 0;
  while (1) {
    int32_t _if_result$4446;
    if (index$569 < end_offset$565) {
      _if_result$4446 = count$570 < len$571;
    } else {
      _if_result$4446 = 0;
    }
    if (_if_result$4446) {
      int32_t c1$572 = self$568[index$569];
      int32_t _if_result$4447;
      int32_t _tmp$2303;
      int32_t _tmp$2304;
      if ($Int$$is_leading_surrogate(c1$572)) {
        int32_t _tmp$2299 = index$569 + 1;
        _if_result$4447 = _tmp$2299 < end_offset$565;
      } else {
        _if_result$4447 = 0;
      }
      if (_if_result$4447) {
        int32_t _tmp$2302 = index$569 + 1;
        int32_t c2$573 = self$568[_tmp$2302];
        if ($Int$$is_trailing_surrogate(c2$573)) {
          int32_t _tmp$2300 = index$569 + 2;
          int32_t _tmp$2301 = count$570 + 1;
          index$569 = _tmp$2300;
          count$570 = _tmp$2301;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_157.data,
              (moonbit_string_t)moonbit_string_literal_158.data
          );
        }
      }
      _tmp$2303 = index$569 + 1;
      _tmp$2304 = count$570 + 1;
      index$569 = _tmp$2303;
      count$570 = _tmp$2304;
      continue;
    } else {
      moonbit_decref(self$568);
      return count$570 >= len$571;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$564
) {
  int32_t end$2297 = self$564.$2;
  int32_t _field$3948 = self$564.$1;
  int32_t start$2298;
  moonbit_decref(self$564.$0);
  start$2298 = _field$3948;
  return end$2297 - start$2298;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$563
) {
  int32_t end$2295 = self$563.$2;
  int32_t _field$3949 = self$563.$1;
  int32_t start$2296;
  moonbit_decref(self$563.$0);
  start$2296 = _field$3949;
  return end$2295 - start$2296;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$562
) {
  int32_t end$2293 = self$562.$2;
  int32_t _field$3950 = self$562.$1;
  int32_t start$2294;
  moonbit_decref(self$562.$0);
  start$2294 = _field$3950;
  return end$2293 - start$2294;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$561
) {
  int32_t end$2291 = self$561.$2;
  int32_t _field$3951 = self$561.$1;
  int32_t start$2292;
  moonbit_decref(self$561.$0);
  start$2292 = _field$3951;
  return end$2291 - start$2292;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$560
) {
  int32_t end$2289 = self$560.$2;
  int32_t _field$3952 = self$560.$1;
  int32_t start$2290;
  moonbit_decref(self$560.$0);
  start$2290 = _field$3952;
  return end$2289 - start$2290;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$555
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$4448 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$4448)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$4448->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$4448->$0 = self$555;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$4448;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$2287,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$553
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$2288 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$2287;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$3953 =
    _casted_env$2288->$0;
  int32_t _cnt$4139 = Moonbit_object_header(_casted_env$2288)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$555;
  if (_cnt$4139 > 1) {
    int32_t _new_cnt$4140;
    moonbit_incref(_field$3953);
    _new_cnt$4140 = _cnt$4139 - 1;
    Moonbit_object_header(_casted_env$2288)->rc = _new_cnt$4140;
  } else if (_cnt$4139 == 1) {
    moonbit_free(_casted_env$2288);
  }
  self$555 = _field$3953;
  while (1) {
    moonbit_string_t _bind$554;
    moonbit_incref(self$555);
    _bind$554 = $$moonbitlang$core$builtin$Iterator$$next$0(self$555);
    if (_bind$554 == 0) {
      moonbit_decref(self$555);
      if (_bind$554) {
        moonbit_decref(_bind$554);
      }
      moonbit_decref(yield_$553);
      return 1;
    } else {
      moonbit_string_t _Some$556 = _bind$554;
      moonbit_string_t _x$557 = _Some$556;
      int32_t _bind$558;
      moonbit_incref(yield_$553);
      _bind$558 = yield_$553->code(yield_$553, _x$557);
      switch (_bind$558) {
        case 1:
          break;
        default: {
          moonbit_decref(self$555);
          moonbit_decref(yield_$553);
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$552
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _func$551 = self$552;
  return _func$551->code(_func$551);
}

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$543,
  struct $$moonbitlang$core$builtin$Logger logger$541
) {
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$542;
  int32_t len$544;
  int32_t i$545;
  int32_t seg$546;
  if (logger$541.$1) {
    moonbit_incref(logger$541.$1);
  }
  logger$541.$0->$method_3(logger$541.$1, 34);
  if (logger$541.$1) {
    moonbit_incref(logger$541.$1);
  }
  moonbit_incref(self$543);
  _env$542
  = (struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$)
    );
  Moonbit_object_header(_env$542)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$, $0_0
    )
    >> 2,
      3,
      0
  );
  _env$542->$0_0 = logger$541.$0;
  _env$542->$0_1 = logger$541.$1;
  _env$542->$1 = self$543;
  len$544 = Moonbit_array_length(self$543);
  i$545 = 0;
  seg$546 = 0;
  $$2a$for$547:;
  while (1) {
    int32_t code$548;
    int32_t c$550;
    struct $$moonbitlang$core$builtin$Logger _bind$2269;
    int32_t _tmp$2270;
    int32_t _tmp$2271;
    int32_t _tmp$2272;
    int32_t _tmp$4453;
    int32_t _tmp$4454;
    if (i$545 >= len$544) {
      moonbit_decref(self$543);
      $moonbitlang$core$builtin$output$flush_segment$7c$3831(
        _env$542, seg$546, i$545
      );
      break;
    }
    code$548 = self$543[i$545];
    switch (code$548) {
      case 34: {
        c$550 = code$548;
        goto $join$549;
        break;
      }
      
      case 92: {
        c$550 = code$548;
        goto $join$549;
        break;
      }
      
      case 10: {
        int32_t _tmp$2273;
        int32_t _tmp$2274;
        moonbit_incref(_env$542);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$542, seg$546, i$545
        );
        if (logger$541.$1) {
          moonbit_incref(logger$541.$1);
        }
        logger$541.$0->$method_0(
          logger$541.$1, (moonbit_string_t)moonbit_string_literal_159.data
        );
        _tmp$2273 = i$545 + 1;
        _tmp$2274 = i$545 + 1;
        i$545 = _tmp$2273;
        seg$546 = _tmp$2274;
        goto $$2a$for$547;
        break;
      }
      
      case 13: {
        int32_t _tmp$2275;
        int32_t _tmp$2276;
        moonbit_incref(_env$542);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$542, seg$546, i$545
        );
        if (logger$541.$1) {
          moonbit_incref(logger$541.$1);
        }
        logger$541.$0->$method_0(
          logger$541.$1, (moonbit_string_t)moonbit_string_literal_160.data
        );
        _tmp$2275 = i$545 + 1;
        _tmp$2276 = i$545 + 1;
        i$545 = _tmp$2275;
        seg$546 = _tmp$2276;
        goto $$2a$for$547;
        break;
      }
      
      case 8: {
        int32_t _tmp$2277;
        int32_t _tmp$2278;
        moonbit_incref(_env$542);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$542, seg$546, i$545
        );
        if (logger$541.$1) {
          moonbit_incref(logger$541.$1);
        }
        logger$541.$0->$method_0(
          logger$541.$1, (moonbit_string_t)moonbit_string_literal_161.data
        );
        _tmp$2277 = i$545 + 1;
        _tmp$2278 = i$545 + 1;
        i$545 = _tmp$2277;
        seg$546 = _tmp$2278;
        goto $$2a$for$547;
        break;
      }
      
      case 9: {
        int32_t _tmp$2279;
        int32_t _tmp$2280;
        moonbit_incref(_env$542);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$542, seg$546, i$545
        );
        if (logger$541.$1) {
          moonbit_incref(logger$541.$1);
        }
        logger$541.$0->$method_0(
          logger$541.$1, (moonbit_string_t)moonbit_string_literal_162.data
        );
        _tmp$2279 = i$545 + 1;
        _tmp$2280 = i$545 + 1;
        i$545 = _tmp$2279;
        seg$546 = _tmp$2280;
        goto $$2a$for$547;
        break;
      }
      default: {
        if (code$548 < 32) {
          int32_t _tmp$2283;
          moonbit_string_t _tmp$2282;
          struct $$moonbitlang$core$builtin$Logger _bind$2281;
          int32_t _tmp$2284;
          int32_t _tmp$2285;
          moonbit_incref(_env$542);
          $moonbitlang$core$builtin$output$flush_segment$7c$3831(
            _env$542, seg$546, i$545
          );
          if (logger$541.$1) {
            moonbit_incref(logger$541.$1);
          }
          logger$541.$0->$method_0(
            logger$541.$1, (moonbit_string_t)moonbit_string_literal_163.data
          );
          _tmp$2283 = code$548 & 0xff;
          _tmp$2282 = $Byte$$to_hex(_tmp$2283);
          if (logger$541.$1) {
            moonbit_incref(logger$541.$1);
          }
          logger$541.$0->$method_0(logger$541.$1, _tmp$2282);
          _bind$2281 = logger$541;
          if (_bind$2281.$1) {
            moonbit_incref(_bind$2281.$1);
          }
          _bind$2281.$0->$method_3(_bind$2281.$1, 125);
          _tmp$2284 = i$545 + 1;
          _tmp$2285 = i$545 + 1;
          i$545 = _tmp$2284;
          seg$546 = _tmp$2285;
          goto $$2a$for$547;
        } else {
          int32_t _tmp$2286 = i$545 + 1;
          int32_t _tmp$4452 = seg$546;
          i$545 = _tmp$2286;
          seg$546 = _tmp$4452;
          goto $$2a$for$547;
        }
        break;
      }
    }
    goto $joinlet$4451;
    $join$549:;
    moonbit_incref(_env$542);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$542, seg$546, i$545
    );
    if (logger$541.$1) {
      moonbit_incref(logger$541.$1);
    }
    logger$541.$0->$method_3(logger$541.$1, 92);
    _bind$2269 = logger$541;
    _tmp$2270 = c$550;
    if (_bind$2269.$1) {
      moonbit_incref(_bind$2269.$1);
    }
    _bind$2269.$0->$method_3(_bind$2269.$1, _tmp$2270);
    _tmp$2271 = i$545 + 1;
    _tmp$2272 = i$545 + 1;
    i$545 = _tmp$2271;
    seg$546 = _tmp$2272;
    continue;
    $joinlet$4451:;
    _tmp$4453 = i$545;
    _tmp$4454 = seg$546;
    i$545 = _tmp$4453;
    seg$546 = _tmp$4454;
    continue;
    break;
  }
  logger$541.$0->$method_3(logger$541.$1, 34);
  return 0;
}

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$537,
  int32_t seg$540,
  int32_t i$539
) {
  moonbit_string_t _field$3955 = _env$537->$1;
  moonbit_string_t self$536 = _field$3955;
  struct $$moonbitlang$core$builtin$Logger _field$3954 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$537->$0_0, _env$537->$0_1
    };
  int32_t _cnt$4141 = Moonbit_object_header(_env$537)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$538;
  if (_cnt$4141 > 1) {
    int32_t _new_cnt$4142;
    moonbit_incref(self$536);
    if (_field$3954.$1) {
      moonbit_incref(_field$3954.$1);
    }
    _new_cnt$4142 = _cnt$4141 - 1;
    Moonbit_object_header(_env$537)->rc = _new_cnt$4142;
  } else if (_cnt$4141 == 1) {
    moonbit_free(_env$537);
  }
  logger$538 = _field$3954;
  if (i$539 > seg$540) {
    int32_t _tmp$2268 = i$539 - seg$540;
    logger$538.$0->$method_1(logger$538.$1, self$536, seg$540, _tmp$2268);
  } else {
    if (logger$538.$1) {
      moonbit_decref(logger$538.$1);
    }
    moonbit_decref(self$536);
  }
  return 0;
}

moonbit_string_t $Byte$$to_hex(int32_t b$535) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$534 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _tmp$2265 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$535, 16);
  int32_t _tmp$2264 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$2265);
  int32_t _tmp$2267;
  int32_t _tmp$2266;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$2263;
  moonbit_incref(_self$534);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$534, _tmp$2264
  );
  _tmp$2267 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$535, 16);
  _tmp$2266
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$2267
  );
  moonbit_incref(_self$534);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$534, _tmp$2266
  );
  _tmp$2263 = _self$534;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$2263);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$533) {
  if (i$533 < 10) {
    int32_t _tmp$2260 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$533, 48);
    return $Byte$$to_char(_tmp$2260);
  } else {
    int32_t _tmp$2262 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$533, 97);
    int32_t _tmp$2261 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$2262, 10);
    return $Byte$$to_char(_tmp$2261);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$531,
  int32_t that$532
) {
  int32_t _tmp$2258 = (int32_t)self$531;
  int32_t _tmp$2259 = (int32_t)that$532;
  int32_t _tmp$2257 = _tmp$2258 - _tmp$2259;
  return _tmp$2257 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$529,
  int32_t that$530
) {
  int32_t _tmp$2255 = (int32_t)self$529;
  int32_t _tmp$2256 = (int32_t)that$530;
  int32_t _tmp$2254 = _tmp$2255 % _tmp$2256;
  return _tmp$2254 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$527,
  int32_t that$528
) {
  int32_t _tmp$2252 = (int32_t)self$527;
  int32_t _tmp$2253 = (int32_t)that$528;
  int32_t _tmp$2251 = _tmp$2252 / _tmp$2253;
  return _tmp$2251 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$525,
  int32_t that$526
) {
  int32_t _tmp$2249 = (int32_t)self$525;
  int32_t _tmp$2250 = (int32_t)that$526;
  int32_t _tmp$2248 = _tmp$2249 + _tmp$2250;
  return _tmp$2248 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$522,
  int32_t start$520,
  int32_t end$521
) {
  int32_t _if_result$4455;
  int32_t len$523;
  int32_t _tmp$2246;
  int32_t _tmp$2247;
  moonbit_bytes_t bytes$524;
  moonbit_bytes_t _tmp$2245;
  if (start$520 == 0) {
    int32_t _tmp$2244 = Moonbit_array_length(str$522);
    _if_result$4455 = end$521 == _tmp$2244;
  } else {
    _if_result$4455 = 0;
  }
  if (_if_result$4455) {
    return str$522;
  }
  len$523 = end$521 - start$520;
  _tmp$2246 = len$523 * 2;
  _tmp$2247 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$524 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$2246, _tmp$2247);
  moonbit_incref(bytes$524);
  $FixedArray$$blit_from_string(bytes$524, 0, str$522, start$520, len$523);
  _tmp$2245 = bytes$524;
  return $Bytes$$to_unchecked_string$inner(_tmp$2245, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$519
) {
  return f$519;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$7(
  int32_t a$513,
  int32_t b$514,
  moonbit_string_t msg$516,
  moonbit_string_t loc$518
) {
  if ($moonbitlang$core$builtin$op_notequal$3(a$513, b$514)) {
    moonbit_string_t fail_msg$515;
    if (msg$516 == 0) {
      moonbit_string_t _tmp$2242;
      moonbit_string_t _tmp$2241;
      moonbit_string_t _tmp$2240;
      moonbit_string_t _tmp$2237;
      moonbit_string_t _tmp$2239;
      moonbit_string_t _tmp$2238;
      moonbit_string_t _tmp$2236;
      if (msg$516) {
        moonbit_decref(msg$516);
      }
      _tmp$2242 = $moonbitlang$core$builtin$debug_string$7(a$513);
      _tmp$2241
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2242
      );
      _tmp$2240
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2241
      );
      _tmp$2237
      = moonbit_add_string(
        _tmp$2240, (moonbit_string_t)moonbit_string_literal_164.data
      );
      _tmp$2239 = $moonbitlang$core$builtin$debug_string$7(b$514);
      _tmp$2238
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2239
      );
      _tmp$2236 = moonbit_add_string(_tmp$2237, _tmp$2238);
      fail_msg$515
      = moonbit_add_string(
        _tmp$2236, (moonbit_string_t)moonbit_string_literal_147.data
      );
    } else {
      moonbit_string_t _Some$517 = msg$516;
      fail_msg$515 = _Some$517;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$515, loc$518);
  } else {
    int32_t _tmp$2243;
    struct moonbit_result_0 _result$4456;
    moonbit_decref(loc$518);
    if (msg$516) {
      moonbit_decref(msg$516);
    }
    _tmp$2243 = 0;
    _result$4456.tag = 1;
    _result$4456.data.ok = _tmp$2243;
    return _result$4456;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$6(
  moonbit_string_t a$507,
  moonbit_string_t b$508,
  moonbit_string_t msg$510,
  moonbit_string_t loc$512
) {
  if (b$508) {
    moonbit_incref(b$508);
  }
  if (a$507) {
    moonbit_incref(a$507);
  }
  if ($moonbitlang$core$builtin$op_notequal$2(a$507, b$508)) {
    moonbit_string_t fail_msg$509;
    if (msg$510 == 0) {
      moonbit_string_t _tmp$2234;
      moonbit_string_t _tmp$2233;
      moonbit_string_t _tmp$2232;
      moonbit_string_t _tmp$2229;
      moonbit_string_t _tmp$2231;
      moonbit_string_t _tmp$2230;
      moonbit_string_t _tmp$2228;
      if (msg$510) {
        moonbit_decref(msg$510);
      }
      _tmp$2234 = $moonbitlang$core$builtin$debug_string$6(a$507);
      _tmp$2233
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2234
      );
      _tmp$2232
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2233
      );
      _tmp$2229
      = moonbit_add_string(
        _tmp$2232, (moonbit_string_t)moonbit_string_literal_164.data
      );
      _tmp$2231 = $moonbitlang$core$builtin$debug_string$6(b$508);
      _tmp$2230
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2231
      );
      _tmp$2228 = moonbit_add_string(_tmp$2229, _tmp$2230);
      fail_msg$509
      = moonbit_add_string(
        _tmp$2228, (moonbit_string_t)moonbit_string_literal_147.data
      );
    } else {
      moonbit_string_t _Some$511;
      if (b$508) {
        moonbit_decref(b$508);
      }
      if (a$507) {
        moonbit_decref(a$507);
      }
      _Some$511 = msg$510;
      fail_msg$509 = _Some$511;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$509, loc$512);
  } else {
    int32_t _tmp$2235;
    struct moonbit_result_0 _result$4457;
    moonbit_decref(loc$512);
    if (msg$510) {
      moonbit_decref(msg$510);
    }
    if (b$508) {
      moonbit_decref(b$508);
    }
    if (a$507) {
      moonbit_decref(a$507);
    }
    _tmp$2235 = 0;
    _result$4457.tag = 1;
    _result$4457.data.ok = _tmp$2235;
    return _result$4457;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$5(
  int32_t a$501,
  int32_t b$502,
  moonbit_string_t msg$504,
  moonbit_string_t loc$506
) {
  if ($moonbitlang$core$builtin$op_notequal$5(a$501, b$502)) {
    moonbit_string_t fail_msg$503;
    if (msg$504 == 0) {
      moonbit_string_t _tmp$2226;
      moonbit_string_t _tmp$2225;
      moonbit_string_t _tmp$2224;
      moonbit_string_t _tmp$2221;
      moonbit_string_t _tmp$2223;
      moonbit_string_t _tmp$2222;
      moonbit_string_t _tmp$2220;
      if (msg$504) {
        moonbit_decref(msg$504);
      }
      _tmp$2226 = $moonbitlang$core$builtin$debug_string$5(a$501);
      _tmp$2225
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2226
      );
      _tmp$2224
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2225
      );
      _tmp$2221
      = moonbit_add_string(
        _tmp$2224, (moonbit_string_t)moonbit_string_literal_164.data
      );
      _tmp$2223 = $moonbitlang$core$builtin$debug_string$5(b$502);
      _tmp$2222
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2223
      );
      _tmp$2220 = moonbit_add_string(_tmp$2221, _tmp$2222);
      fail_msg$503
      = moonbit_add_string(
        _tmp$2220, (moonbit_string_t)moonbit_string_literal_147.data
      );
    } else {
      moonbit_string_t _Some$505 = msg$504;
      fail_msg$503 = _Some$505;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$503, loc$506);
  } else {
    int32_t _tmp$2227;
    struct moonbit_result_0 _result$4458;
    moonbit_decref(loc$506);
    if (msg$504) {
      moonbit_decref(msg$504);
    }
    _tmp$2227 = 0;
    _result$4458.tag = 1;
    _result$4458.data.ok = _tmp$2227;
    return _result$4458;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  int32_t a$495,
  int32_t b$496,
  moonbit_string_t msg$498,
  moonbit_string_t loc$500
) {
  if (a$495 != b$496) {
    moonbit_string_t fail_msg$497;
    if (msg$498 == 0) {
      moonbit_string_t _tmp$2218;
      moonbit_string_t _tmp$2217;
      moonbit_string_t _tmp$2216;
      moonbit_string_t _tmp$2213;
      moonbit_string_t _tmp$2215;
      moonbit_string_t _tmp$2214;
      moonbit_string_t _tmp$2212;
      if (msg$498) {
        moonbit_decref(msg$498);
      }
      _tmp$2218 = $moonbitlang$core$builtin$debug_string$4(a$495);
      _tmp$2217
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2218
      );
      _tmp$2216
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2217
      );
      _tmp$2213
      = moonbit_add_string(
        _tmp$2216, (moonbit_string_t)moonbit_string_literal_164.data
      );
      _tmp$2215 = $moonbitlang$core$builtin$debug_string$4(b$496);
      _tmp$2214
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2215
      );
      _tmp$2212 = moonbit_add_string(_tmp$2213, _tmp$2214);
      fail_msg$497
      = moonbit_add_string(
        _tmp$2212, (moonbit_string_t)moonbit_string_literal_147.data
      );
    } else {
      moonbit_string_t _Some$499 = msg$498;
      fail_msg$497 = _Some$499;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$497, loc$500);
  } else {
    int32_t _tmp$2219;
    struct moonbit_result_0 _result$4459;
    moonbit_decref(loc$500);
    if (msg$498) {
      moonbit_decref(msg$498);
    }
    _tmp$2219 = 0;
    _result$4459.tag = 1;
    _result$4459.data.ok = _tmp$2219;
    return _result$4459;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  moonbit_string_t a$489,
  moonbit_string_t b$490,
  moonbit_string_t msg$492,
  moonbit_string_t loc$494
) {
  moonbit_incref(b$490);
  moonbit_incref(a$489);
  if ($moonbitlang$core$builtin$op_notequal$4(a$489, b$490)) {
    moonbit_string_t fail_msg$491;
    if (msg$492 == 0) {
      moonbit_string_t _tmp$2210;
      moonbit_string_t _tmp$2209;
      moonbit_string_t _tmp$2208;
      moonbit_string_t _tmp$2205;
      moonbit_string_t _tmp$2207;
      moonbit_string_t _tmp$2206;
      moonbit_string_t _tmp$2204;
      if (msg$492) {
        moonbit_decref(msg$492);
      }
      _tmp$2210 = $moonbitlang$core$builtin$debug_string$3(a$489);
      _tmp$2209
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2210
      );
      _tmp$2208
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2209
      );
      _tmp$2205
      = moonbit_add_string(
        _tmp$2208, (moonbit_string_t)moonbit_string_literal_164.data
      );
      _tmp$2207 = $moonbitlang$core$builtin$debug_string$3(b$490);
      _tmp$2206
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2207
      );
      _tmp$2204 = moonbit_add_string(_tmp$2205, _tmp$2206);
      fail_msg$491
      = moonbit_add_string(
        _tmp$2204, (moonbit_string_t)moonbit_string_literal_147.data
      );
    } else {
      moonbit_string_t _Some$493;
      moonbit_decref(b$490);
      moonbit_decref(a$489);
      _Some$493 = msg$492;
      fail_msg$491 = _Some$493;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$491, loc$494);
  } else {
    int32_t _tmp$2211;
    struct moonbit_result_0 _result$4460;
    moonbit_decref(loc$494);
    if (msg$492) {
      moonbit_decref(msg$492);
    }
    moonbit_decref(b$490);
    moonbit_decref(a$489);
    _tmp$2211 = 0;
    _result$4460.tag = 1;
    _result$4460.data.ok = _tmp$2211;
    return _result$4460;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  double a$483,
  double b$484,
  moonbit_string_t msg$486,
  moonbit_string_t loc$488
) {
  if (a$483 != b$484) {
    moonbit_string_t fail_msg$485;
    if (msg$486 == 0) {
      moonbit_string_t _tmp$2202;
      moonbit_string_t _tmp$2201;
      moonbit_string_t _tmp$2200;
      moonbit_string_t _tmp$2197;
      moonbit_string_t _tmp$2199;
      moonbit_string_t _tmp$2198;
      moonbit_string_t _tmp$2196;
      if (msg$486) {
        moonbit_decref(msg$486);
      }
      _tmp$2202 = $moonbitlang$core$builtin$debug_string$2(a$483);
      _tmp$2201
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2202
      );
      _tmp$2200
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2201
      );
      _tmp$2197
      = moonbit_add_string(
        _tmp$2200, (moonbit_string_t)moonbit_string_literal_164.data
      );
      _tmp$2199 = $moonbitlang$core$builtin$debug_string$2(b$484);
      _tmp$2198
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2199
      );
      _tmp$2196 = moonbit_add_string(_tmp$2197, _tmp$2198);
      fail_msg$485
      = moonbit_add_string(
        _tmp$2196, (moonbit_string_t)moonbit_string_literal_147.data
      );
    } else {
      moonbit_string_t _Some$487 = msg$486;
      fail_msg$485 = _Some$487;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$485, loc$488);
  } else {
    int32_t _tmp$2203;
    struct moonbit_result_0 _result$4461;
    moonbit_decref(loc$488);
    if (msg$486) {
      moonbit_decref(msg$486);
    }
    _tmp$2203 = 0;
    _result$4461.tag = 1;
    _result$4461.data.ok = _tmp$2203;
    return _result$4461;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  int64_t a$477,
  int64_t b$478,
  moonbit_string_t msg$480,
  moonbit_string_t loc$482
) {
  if ($moonbitlang$core$builtin$op_notequal$1(a$477, b$478)) {
    moonbit_string_t fail_msg$479;
    if (msg$480 == 0) {
      moonbit_string_t _tmp$2194;
      moonbit_string_t _tmp$2193;
      moonbit_string_t _tmp$2192;
      moonbit_string_t _tmp$2189;
      moonbit_string_t _tmp$2191;
      moonbit_string_t _tmp$2190;
      moonbit_string_t _tmp$2188;
      if (msg$480) {
        moonbit_decref(msg$480);
      }
      _tmp$2194 = $moonbitlang$core$builtin$debug_string$1(a$477);
      _tmp$2193
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2194
      );
      _tmp$2192
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2193
      );
      _tmp$2189
      = moonbit_add_string(
        _tmp$2192, (moonbit_string_t)moonbit_string_literal_164.data
      );
      _tmp$2191 = $moonbitlang$core$builtin$debug_string$1(b$478);
      _tmp$2190
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2191
      );
      _tmp$2188 = moonbit_add_string(_tmp$2189, _tmp$2190);
      fail_msg$479
      = moonbit_add_string(
        _tmp$2188, (moonbit_string_t)moonbit_string_literal_147.data
      );
    } else {
      moonbit_string_t _Some$481 = msg$480;
      fail_msg$479 = _Some$481;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$479, loc$482);
  } else {
    int32_t _tmp$2195;
    struct moonbit_result_0 _result$4462;
    moonbit_decref(loc$482);
    if (msg$480) {
      moonbit_decref(msg$480);
    }
    _tmp$2195 = 0;
    _result$4462.tag = 1;
    _result$4462.data.ok = _tmp$2195;
    return _result$4462;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  int32_t a$471,
  int32_t b$472,
  moonbit_string_t msg$474,
  moonbit_string_t loc$476
) {
  if (a$471 != b$472) {
    moonbit_string_t fail_msg$473;
    if (msg$474 == 0) {
      moonbit_string_t _tmp$2186;
      moonbit_string_t _tmp$2185;
      moonbit_string_t _tmp$2184;
      moonbit_string_t _tmp$2181;
      moonbit_string_t _tmp$2183;
      moonbit_string_t _tmp$2182;
      moonbit_string_t _tmp$2180;
      if (msg$474) {
        moonbit_decref(msg$474);
      }
      _tmp$2186 = $moonbitlang$core$builtin$debug_string$0(a$471);
      _tmp$2185
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2186
      );
      _tmp$2184
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_147.data, _tmp$2185
      );
      _tmp$2181
      = moonbit_add_string(
        _tmp$2184, (moonbit_string_t)moonbit_string_literal_164.data
      );
      _tmp$2183 = $moonbitlang$core$builtin$debug_string$0(b$472);
      _tmp$2182
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2183
      );
      _tmp$2180 = moonbit_add_string(_tmp$2181, _tmp$2182);
      fail_msg$473
      = moonbit_add_string(
        _tmp$2180, (moonbit_string_t)moonbit_string_literal_147.data
      );
    } else {
      moonbit_string_t _Some$475 = msg$474;
      fail_msg$473 = _Some$475;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$473, loc$476);
  } else {
    int32_t _tmp$2187;
    struct moonbit_result_0 _result$4463;
    moonbit_decref(loc$476);
    if (msg$474) {
      moonbit_decref(msg$474);
    }
    _tmp$2187 = 0;
    _result$4463.tag = 1;
    _result$4463.data.ok = _tmp$2187;
    return _result$4463;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$470,
  moonbit_string_t loc$469
) {
  moonbit_string_t _tmp$2179 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$469);
  moonbit_string_t _tmp$2177 =
    moonbit_add_string(
      _tmp$2179, (moonbit_string_t)moonbit_string_literal_165.data
    );
  moonbit_string_t _tmp$2178 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(msg$470);
  moonbit_string_t _tmp$2176 = moonbit_add_string(_tmp$2177, _tmp$2178);
  void* moonbitlang$core$builtin$Failure$Failure$2175 =
    (void*)moonbit_malloc(
      sizeof(struct $Error$moonbitlang$core$builtin$Failure$Failure)
    );
  struct moonbit_result_0 _result$4464;
  Moonbit_object_header(moonbitlang$core$builtin$Failure$Failure$2175)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Error$moonbitlang$core$builtin$Failure$Failure, $0) >> 2,
      1,
      2
  );
  ((struct $Error$moonbitlang$core$builtin$Failure$Failure*)moonbitlang$core$builtin$Failure$Failure$2175)->$0
  = _tmp$2176;
  _result$4464.tag = 0;
  _result$4464.data.err = moonbitlang$core$builtin$Failure$Failure$2175;
  return _result$4464;
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$7(int32_t t$468) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$467 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2174;
  moonbit_incref(buf$467);
  _tmp$2174
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$467
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$2(t$468, _tmp$2174);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$467);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$6(
  moonbit_string_t t$466
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$465 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2173;
  moonbit_incref(buf$465);
  _tmp$2173
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$465
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$1(t$466, _tmp$2173);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$465);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$5(int32_t t$464) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$463 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2172;
  moonbit_incref(buf$463);
  _tmp$2172
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$463
  };
  $$moonbitlang$core$builtin$Show$$Bool$$output(t$464, _tmp$2172);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$463);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(int32_t t$462) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$461 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2171;
  moonbit_incref(buf$461);
  _tmp$2171
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$461
  };
  $$moonbitlang$core$builtin$Show$$UInt16$$output(t$462, _tmp$2171);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$461);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(
  moonbit_string_t t$460
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$459 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2170;
  moonbit_incref(buf$459);
  _tmp$2170
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$459
  };
  $$moonbitlang$core$builtin$Show$$String$$output(t$460, _tmp$2170);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$459);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(double t$458) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$457 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2169;
  moonbit_incref(buf$457);
  _tmp$2169
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$457
  };
  $$moonbitlang$core$builtin$Show$$Double$$output(t$458, _tmp$2169);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$457);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(int64_t t$456) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$455 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2168;
  moonbit_incref(buf$455);
  _tmp$2168
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$455
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$0(t$456, _tmp$2168);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$455);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(int32_t t$454) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$453 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2167;
  moonbit_incref(buf$453);
  _tmp$2167
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$453
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(t$454, _tmp$2167);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$453);
}

moonbit_string_t $UInt16$$to_string$inner(
  int32_t self$451,
  int32_t radix$452
) {
  int32_t _tmp$2166 = (int32_t)self$451;
  return $Int$$to_string$inner(_tmp$2166, radix$452);
}

moonbit_string_t $UInt64$$to_string$inner(
  uint64_t self$443,
  int32_t radix$442
) {
  int32_t _if_result$4465;
  uint16_t* buffer$444;
  if (radix$442 < 2) {
    _if_result$4465 = 1;
  } else {
    _if_result$4465 = radix$442 > 36;
  }
  if (_if_result$4465) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_166.data,
        (moonbit_string_t)moonbit_string_literal_167.data
    );
  }
  if (self$443 == 0ull) {
    return (moonbit_string_t)moonbit_string_literal_141.data;
  }
  switch (radix$442) {
    case 10: {
      int32_t len$445 = $moonbitlang$core$builtin$dec_count64(self$443);
      uint16_t* buffer$446 = (uint16_t*)moonbit_make_string(len$445, 0);
      moonbit_incref(buffer$446);
      $moonbitlang$core$builtin$int64_to_string_dec(
        buffer$446, self$443, 0, len$445
      );
      buffer$444 = buffer$446;
      break;
    }
    
    case 16: {
      int32_t len$447 = $moonbitlang$core$builtin$hex_count64(self$443);
      uint16_t* buffer$448 = (uint16_t*)moonbit_make_string(len$447, 0);
      moonbit_incref(buffer$448);
      $moonbitlang$core$builtin$int64_to_string_hex(
        buffer$448, self$443, 0, len$447
      );
      buffer$444 = buffer$448;
      break;
    }
    default: {
      int32_t len$449 =
        $moonbitlang$core$builtin$radix_count64(self$443, radix$442);
      uint16_t* buffer$450 = (uint16_t*)moonbit_make_string(len$449, 0);
      moonbit_incref(buffer$450);
      $moonbitlang$core$builtin$int64_to_string_generic(
        buffer$450, self$443, 0, len$449, radix$442
      );
      buffer$444 = buffer$450;
      break;
    }
  }
  return buffer$444;
}

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$432,
  uint64_t num$420,
  int32_t digit_start$423,
  int32_t total_len$422
) {
  uint64_t num$419 = num$420;
  int32_t offset$421 = total_len$422 - digit_start$423;
  uint64_t _tmp$2165;
  int32_t remaining$434;
  int32_t _tmp$2146;
  while (1) {
    uint64_t _tmp$2109 = num$419;
    if (_tmp$2109 >= 10000ull) {
      uint64_t _tmp$2132 = num$419;
      uint64_t t$424 = _tmp$2132 / 10000ull;
      uint64_t _tmp$2131 = num$419;
      uint64_t _tmp$2130 = _tmp$2131 % 10000ull;
      int32_t r$425 = (int32_t)_tmp$2130;
      int32_t d1$426;
      int32_t d2$427;
      int32_t _tmp$2110;
      int32_t _tmp$2129;
      int32_t _tmp$2128;
      int32_t d1_hi$428;
      int32_t _tmp$2127;
      int32_t _tmp$2126;
      int32_t d1_lo$429;
      int32_t _tmp$2125;
      int32_t _tmp$2124;
      int32_t d2_hi$430;
      int32_t _tmp$2123;
      int32_t _tmp$2122;
      int32_t d2_lo$431;
      int32_t _tmp$2112;
      int32_t _tmp$2111;
      int32_t _tmp$2115;
      int32_t _tmp$2114;
      int32_t _tmp$2113;
      int32_t _tmp$2118;
      int32_t _tmp$2117;
      int32_t _tmp$2116;
      int32_t _tmp$2121;
      int32_t _tmp$2120;
      int32_t _tmp$2119;
      num$419 = t$424;
      d1$426 = r$425 / 100;
      d2$427 = r$425 % 100;
      _tmp$2110 = offset$421;
      offset$421 = _tmp$2110 - 4;
      _tmp$2129 = d1$426 / 10;
      _tmp$2128 = 48 + _tmp$2129;
      d1_hi$428 = (uint16_t)_tmp$2128;
      _tmp$2127 = d1$426 % 10;
      _tmp$2126 = 48 + _tmp$2127;
      d1_lo$429 = (uint16_t)_tmp$2126;
      _tmp$2125 = d2$427 / 10;
      _tmp$2124 = 48 + _tmp$2125;
      d2_hi$430 = (uint16_t)_tmp$2124;
      _tmp$2123 = d2$427 % 10;
      _tmp$2122 = 48 + _tmp$2123;
      d2_lo$431 = (uint16_t)_tmp$2122;
      _tmp$2112 = offset$421;
      _tmp$2111 = digit_start$423 + _tmp$2112;
      buffer$432[_tmp$2111] = d1_hi$428;
      _tmp$2115 = offset$421;
      _tmp$2114 = digit_start$423 + _tmp$2115;
      _tmp$2113 = _tmp$2114 + 1;
      buffer$432[_tmp$2113] = d1_lo$429;
      _tmp$2118 = offset$421;
      _tmp$2117 = digit_start$423 + _tmp$2118;
      _tmp$2116 = _tmp$2117 + 2;
      buffer$432[_tmp$2116] = d2_hi$430;
      _tmp$2121 = offset$421;
      _tmp$2120 = digit_start$423 + _tmp$2121;
      _tmp$2119 = _tmp$2120 + 3;
      buffer$432[_tmp$2119] = d2_lo$431;
      continue;
    }
    break;
  }
  _tmp$2165 = num$419;
  remaining$434 = (int32_t)_tmp$2165;
  while (1) {
    int32_t _tmp$2133 = remaining$434;
    if (_tmp$2133 >= 100) {
      int32_t _tmp$2145 = remaining$434;
      int32_t t$435 = _tmp$2145 / 100;
      int32_t _tmp$2144 = remaining$434;
      int32_t d$436 = _tmp$2144 % 100;
      int32_t _tmp$2134;
      int32_t _tmp$2143;
      int32_t _tmp$2142;
      int32_t d_hi$437;
      int32_t _tmp$2141;
      int32_t _tmp$2140;
      int32_t d_lo$438;
      int32_t _tmp$2136;
      int32_t _tmp$2135;
      int32_t _tmp$2139;
      int32_t _tmp$2138;
      int32_t _tmp$2137;
      remaining$434 = t$435;
      _tmp$2134 = offset$421;
      offset$421 = _tmp$2134 - 2;
      _tmp$2143 = d$436 / 10;
      _tmp$2142 = 48 + _tmp$2143;
      d_hi$437 = (uint16_t)_tmp$2142;
      _tmp$2141 = d$436 % 10;
      _tmp$2140 = 48 + _tmp$2141;
      d_lo$438 = (uint16_t)_tmp$2140;
      _tmp$2136 = offset$421;
      _tmp$2135 = digit_start$423 + _tmp$2136;
      buffer$432[_tmp$2135] = d_hi$437;
      _tmp$2139 = offset$421;
      _tmp$2138 = digit_start$423 + _tmp$2139;
      _tmp$2137 = _tmp$2138 + 1;
      buffer$432[_tmp$2137] = d_lo$438;
      continue;
    }
    break;
  }
  _tmp$2146 = remaining$434;
  if (_tmp$2146 >= 10) {
    int32_t _tmp$2147 = offset$421;
    int32_t _tmp$2158;
    int32_t _tmp$2157;
    int32_t _tmp$2156;
    int32_t d_hi$440;
    int32_t _tmp$2155;
    int32_t _tmp$2154;
    int32_t _tmp$2153;
    int32_t d_lo$441;
    int32_t _tmp$2149;
    int32_t _tmp$2148;
    int32_t _tmp$2152;
    int32_t _tmp$2151;
    int32_t _tmp$2150;
    offset$421 = _tmp$2147 - 2;
    _tmp$2158 = remaining$434;
    _tmp$2157 = _tmp$2158 / 10;
    _tmp$2156 = 48 + _tmp$2157;
    d_hi$440 = (uint16_t)_tmp$2156;
    _tmp$2155 = remaining$434;
    _tmp$2154 = _tmp$2155 % 10;
    _tmp$2153 = 48 + _tmp$2154;
    d_lo$441 = (uint16_t)_tmp$2153;
    _tmp$2149 = offset$421;
    _tmp$2148 = digit_start$423 + _tmp$2149;
    buffer$432[_tmp$2148] = d_hi$440;
    _tmp$2152 = offset$421;
    _tmp$2151 = digit_start$423 + _tmp$2152;
    _tmp$2150 = _tmp$2151 + 1;
    buffer$432[_tmp$2150] = d_lo$441;
    moonbit_decref(buffer$432);
  } else {
    int32_t _tmp$2159 = offset$421;
    int32_t _tmp$2164;
    int32_t _tmp$2160;
    int32_t _tmp$2163;
    int32_t _tmp$2162;
    int32_t _tmp$2161;
    offset$421 = _tmp$2159 - 1;
    _tmp$2164 = offset$421;
    _tmp$2160 = digit_start$423 + _tmp$2164;
    _tmp$2163 = remaining$434;
    _tmp$2162 = 48 + _tmp$2163;
    _tmp$2161 = (uint16_t)_tmp$2162;
    buffer$432[_tmp$2160] = _tmp$2161;
    moonbit_decref(buffer$432);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$414,
  uint64_t num$408,
  int32_t digit_start$406,
  int32_t total_len$405,
  int32_t radix$410
) {
  int32_t offset$404 = total_len$405 - digit_start$406;
  uint64_t n$407 = num$408;
  uint64_t base$409 = $Int$$to_uint64(radix$410);
  int32_t _tmp$2089 = radix$410 - 1;
  int32_t _tmp$2088 = radix$410 & _tmp$2089;
  if (_tmp$2088 == 0) {
    int32_t shift$411 = moonbit_ctz32(radix$410);
    uint64_t mask$412 = base$409 - 1ull;
    while (1) {
      uint64_t _tmp$2090 = n$407;
      if (_tmp$2090 > 0ull) {
        int32_t _tmp$2091 = offset$404;
        uint64_t _tmp$2098;
        uint64_t _tmp$2097;
        int32_t digit$413;
        int32_t _tmp$2095;
        int32_t _tmp$2092;
        int32_t _tmp$2094;
        int32_t _tmp$2093;
        uint64_t _tmp$2096;
        offset$404 = _tmp$2091 - 1;
        _tmp$2098 = n$407;
        _tmp$2097 = _tmp$2098 & mask$412;
        digit$413 = (int32_t)_tmp$2097;
        _tmp$2095 = offset$404;
        _tmp$2092 = digit_start$406 + _tmp$2095;
        _tmp$2094
        = ((moonbit_string_t)moonbit_string_literal_168.data)[
          digit$413
        ];
        _tmp$2093 = (uint16_t)_tmp$2094;
        buffer$414[_tmp$2092] = _tmp$2093;
        _tmp$2096 = n$407;
        n$407 = _tmp$2096 >> (shift$411 & 63);
        continue;
      } else {
        moonbit_decref(buffer$414);
      }
      break;
    }
  } else {
    while (1) {
      uint64_t _tmp$2099 = n$407;
      if (_tmp$2099 > 0ull) {
        int32_t _tmp$2100 = offset$404;
        uint64_t _tmp$2108;
        uint64_t q$416;
        uint64_t _tmp$2106;
        uint64_t _tmp$2107;
        uint64_t _tmp$2105;
        int32_t digit$417;
        int32_t _tmp$2104;
        int32_t _tmp$2101;
        int32_t _tmp$2103;
        int32_t _tmp$2102;
        offset$404 = _tmp$2100 - 1;
        _tmp$2108 = n$407;
        q$416 = _tmp$2108 / base$409;
        _tmp$2106 = n$407;
        _tmp$2107 = q$416 * base$409;
        _tmp$2105 = _tmp$2106 - _tmp$2107;
        digit$417 = (int32_t)_tmp$2105;
        _tmp$2104 = offset$404;
        _tmp$2101 = digit_start$406 + _tmp$2104;
        _tmp$2103
        = ((moonbit_string_t)moonbit_string_literal_168.data)[
          digit$417
        ];
        _tmp$2102 = (uint16_t)_tmp$2103;
        buffer$414[_tmp$2101] = _tmp$2102;
        n$407 = q$416;
        continue;
      } else {
        moonbit_decref(buffer$414);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$401,
  uint64_t num$397,
  int32_t digit_start$395,
  int32_t total_len$394
) {
  int32_t offset$393 = total_len$394 - digit_start$395;
  uint64_t n$396 = num$397;
  int32_t _tmp$2083;
  while (1) {
    int32_t _tmp$2069 = offset$393;
    if (_tmp$2069 >= 2) {
      int32_t _tmp$2070 = offset$393;
      uint64_t _tmp$2082;
      uint64_t _tmp$2081;
      int32_t byte_val$398;
      int32_t hi$399;
      int32_t lo$400;
      int32_t _tmp$2074;
      int32_t _tmp$2071;
      int32_t _tmp$2073;
      int32_t _tmp$2072;
      int32_t _tmp$2079;
      int32_t _tmp$2078;
      int32_t _tmp$2075;
      int32_t _tmp$2077;
      int32_t _tmp$2076;
      uint64_t _tmp$2080;
      offset$393 = _tmp$2070 - 2;
      _tmp$2082 = n$396;
      _tmp$2081 = _tmp$2082 & 255ull;
      byte_val$398 = (int32_t)_tmp$2081;
      hi$399 = byte_val$398 / 16;
      lo$400 = byte_val$398 % 16;
      _tmp$2074 = offset$393;
      _tmp$2071 = digit_start$395 + _tmp$2074;
      _tmp$2073 = ((moonbit_string_t)moonbit_string_literal_168.data)[hi$399];
      _tmp$2072 = (uint16_t)_tmp$2073;
      buffer$401[_tmp$2071] = _tmp$2072;
      _tmp$2079 = offset$393;
      _tmp$2078 = digit_start$395 + _tmp$2079;
      _tmp$2075 = _tmp$2078 + 1;
      _tmp$2077 = ((moonbit_string_t)moonbit_string_literal_168.data)[lo$400];
      _tmp$2076 = (uint16_t)_tmp$2077;
      buffer$401[_tmp$2075] = _tmp$2076;
      _tmp$2080 = n$396;
      n$396 = _tmp$2080 >> 8;
      continue;
    }
    break;
  }
  _tmp$2083 = offset$393;
  if (_tmp$2083 == 1) {
    uint64_t _tmp$2087 = n$396;
    uint64_t _tmp$2086 = _tmp$2087 & 15ull;
    int32_t nibble$403 = (int32_t)_tmp$2086;
    int32_t _tmp$2085 =
      ((moonbit_string_t)moonbit_string_literal_168.data)[nibble$403];
    int32_t _tmp$2084 = (uint16_t)_tmp$2085;
    buffer$401[digit_start$395] = _tmp$2084;
    moonbit_decref(buffer$401);
  } else {
    moonbit_decref(buffer$401);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$387,
  int32_t radix$390
) {
  uint64_t num$388;
  uint64_t base$389;
  int32_t count$391;
  if (value$387 == 0ull) {
    return 1;
  }
  num$388 = value$387;
  base$389 = $Int$$to_uint64(radix$390);
  count$391 = 0;
  while (1) {
    uint64_t _tmp$2066 = num$388;
    if (_tmp$2066 > 0ull) {
      int32_t _tmp$2067 = count$391;
      uint64_t _tmp$2068;
      count$391 = _tmp$2067 + 1;
      _tmp$2068 = num$388;
      num$388 = _tmp$2068 / base$389;
      continue;
    }
    break;
  }
  return count$391;
}

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$385) {
  if (value$385 == 0ull) {
    return 1;
  } else {
    int32_t leading_zeros$386 = moonbit_clz64(value$385);
    int32_t _tmp$2065 = 63 - leading_zeros$386;
    int32_t _tmp$2064 = _tmp$2065 / 4;
    return _tmp$2064 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$384) {
  if (value$384 >= 10000000000ull) {
    if (value$384 >= 100000000000000ull) {
      if (value$384 >= 10000000000000000ull) {
        if (value$384 >= 1000000000000000000ull) {
          if (value$384 >= 10000000000000000000ull) {
            return 20;
          } else {
            return 19;
          }
        } else if (value$384 >= 100000000000000000ull) {
          return 18;
        } else {
          return 17;
        }
      } else if (value$384 >= 1000000000000000ull) {
        return 16;
      } else {
        return 15;
      }
    } else if (value$384 >= 1000000000000ull) {
      if (value$384 >= 10000000000000ull) {
        return 14;
      } else {
        return 13;
      }
    } else if (value$384 >= 100000000000ull) {
      return 12;
    } else {
      return 11;
    }
  } else if (value$384 >= 100000ull) {
    if (value$384 >= 10000000ull) {
      if (value$384 >= 1000000000ull) {
        return 10;
      } else if (value$384 >= 100000000ull) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$384 >= 1000000ull) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$384 >= 1000ull) {
    if (value$384 >= 10000ull) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$384 >= 100ull) {
    return 3;
  } else if (value$384 >= 10ull) {
    return 2;
  } else {
    return 1;
  }
}

moonbit_string_t $Int$$to_string$inner(int32_t self$368, int32_t radix$367) {
  int32_t _if_result$4472;
  int32_t is_negative$369;
  uint32_t num$370;
  uint16_t* buffer$371;
  if (radix$367 < 2) {
    _if_result$4472 = 1;
  } else {
    _if_result$4472 = radix$367 > 36;
  }
  if (_if_result$4472) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_166.data,
        (moonbit_string_t)moonbit_string_literal_169.data
    );
  }
  if (self$368 == 0) {
    return (moonbit_string_t)moonbit_string_literal_141.data;
  }
  is_negative$369 = self$368 < 0;
  if (is_negative$369) {
    int32_t _tmp$2063 = -self$368;
    num$370 = *(uint32_t*)&_tmp$2063;
  } else {
    num$370 = *(uint32_t*)&self$368;
  }
  switch (radix$367) {
    case 10: {
      int32_t digit_len$372 = $moonbitlang$core$builtin$dec_count32(num$370);
      int32_t _tmp$2060;
      int32_t total_len$373;
      uint16_t* buffer$374;
      int32_t digit_start$375;
      if (is_negative$369) {
        _tmp$2060 = 1;
      } else {
        _tmp$2060 = 0;
      }
      total_len$373 = digit_len$372 + _tmp$2060;
      buffer$374 = (uint16_t*)moonbit_make_string(total_len$373, 0);
      if (is_negative$369) {
        digit_start$375 = 1;
      } else {
        digit_start$375 = 0;
      }
      moonbit_incref(buffer$374);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$374, num$370, digit_start$375, total_len$373
      );
      buffer$371 = buffer$374;
      break;
    }
    
    case 16: {
      int32_t digit_len$376 = $moonbitlang$core$builtin$hex_count32(num$370);
      int32_t _tmp$2061;
      int32_t total_len$377;
      uint16_t* buffer$378;
      int32_t digit_start$379;
      if (is_negative$369) {
        _tmp$2061 = 1;
      } else {
        _tmp$2061 = 0;
      }
      total_len$377 = digit_len$376 + _tmp$2061;
      buffer$378 = (uint16_t*)moonbit_make_string(total_len$377, 0);
      if (is_negative$369) {
        digit_start$379 = 1;
      } else {
        digit_start$379 = 0;
      }
      moonbit_incref(buffer$378);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$378, num$370, digit_start$379, total_len$377
      );
      buffer$371 = buffer$378;
      break;
    }
    default: {
      int32_t digit_len$380 =
        $moonbitlang$core$builtin$radix_count32(num$370, radix$367);
      int32_t _tmp$2062;
      int32_t total_len$381;
      uint16_t* buffer$382;
      int32_t digit_start$383;
      if (is_negative$369) {
        _tmp$2062 = 1;
      } else {
        _tmp$2062 = 0;
      }
      total_len$381 = digit_len$380 + _tmp$2062;
      buffer$382 = (uint16_t*)moonbit_make_string(total_len$381, 0);
      if (is_negative$369) {
        digit_start$383 = 1;
      } else {
        digit_start$383 = 0;
      }
      moonbit_incref(buffer$382);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$382, num$370, digit_start$383, total_len$381, radix$367
      );
      buffer$371 = buffer$382;
      break;
    }
  }
  if (is_negative$369) {
    buffer$371[0] = 45;
  }
  return buffer$371;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$361,
  int32_t radix$364
) {
  uint32_t num$362;
  uint32_t base$363;
  int32_t count$365;
  if (value$361 == 0u) {
    return 1;
  }
  num$362 = value$361;
  base$363 = *(uint32_t*)&radix$364;
  count$365 = 0;
  while (1) {
    uint32_t _tmp$2057 = num$362;
    if (_tmp$2057 > 0u) {
      int32_t _tmp$2058 = count$365;
      uint32_t _tmp$2059;
      count$365 = _tmp$2058 + 1;
      _tmp$2059 = num$362;
      num$362 = _tmp$2059 / base$363;
      continue;
    }
    break;
  }
  return count$365;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$359) {
  if (value$359 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$360 = moonbit_clz32(value$359);
    int32_t _tmp$2056 = 31 - leading_zeros$360;
    int32_t _tmp$2055 = _tmp$2056 / 4;
    return _tmp$2055 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$358) {
  if (value$358 >= 100000u) {
    if (value$358 >= 10000000u) {
      if (value$358 >= 1000000000u) {
        return 10;
      } else if (value$358 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$358 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$358 >= 1000u) {
    if (value$358 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$358 >= 100u) {
    return 3;
  } else if (value$358 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$348,
  uint32_t num$336,
  int32_t digit_start$339,
  int32_t total_len$338
) {
  uint32_t num$335 = num$336;
  int32_t offset$337 = total_len$338 - digit_start$339;
  uint32_t _tmp$2054;
  int32_t remaining$350;
  int32_t _tmp$2035;
  while (1) {
    uint32_t _tmp$1998 = num$335;
    if (_tmp$1998 >= 10000u) {
      uint32_t _tmp$2021 = num$335;
      uint32_t t$340 = _tmp$2021 / 10000u;
      uint32_t _tmp$2020 = num$335;
      uint32_t _tmp$2019 = _tmp$2020 % 10000u;
      int32_t r$341 = *(int32_t*)&_tmp$2019;
      int32_t d1$342;
      int32_t d2$343;
      int32_t _tmp$1999;
      int32_t _tmp$2018;
      int32_t _tmp$2017;
      int32_t d1_hi$344;
      int32_t _tmp$2016;
      int32_t _tmp$2015;
      int32_t d1_lo$345;
      int32_t _tmp$2014;
      int32_t _tmp$2013;
      int32_t d2_hi$346;
      int32_t _tmp$2012;
      int32_t _tmp$2011;
      int32_t d2_lo$347;
      int32_t _tmp$2001;
      int32_t _tmp$2000;
      int32_t _tmp$2004;
      int32_t _tmp$2003;
      int32_t _tmp$2002;
      int32_t _tmp$2007;
      int32_t _tmp$2006;
      int32_t _tmp$2005;
      int32_t _tmp$2010;
      int32_t _tmp$2009;
      int32_t _tmp$2008;
      num$335 = t$340;
      d1$342 = r$341 / 100;
      d2$343 = r$341 % 100;
      _tmp$1999 = offset$337;
      offset$337 = _tmp$1999 - 4;
      _tmp$2018 = d1$342 / 10;
      _tmp$2017 = 48 + _tmp$2018;
      d1_hi$344 = (uint16_t)_tmp$2017;
      _tmp$2016 = d1$342 % 10;
      _tmp$2015 = 48 + _tmp$2016;
      d1_lo$345 = (uint16_t)_tmp$2015;
      _tmp$2014 = d2$343 / 10;
      _tmp$2013 = 48 + _tmp$2014;
      d2_hi$346 = (uint16_t)_tmp$2013;
      _tmp$2012 = d2$343 % 10;
      _tmp$2011 = 48 + _tmp$2012;
      d2_lo$347 = (uint16_t)_tmp$2011;
      _tmp$2001 = offset$337;
      _tmp$2000 = digit_start$339 + _tmp$2001;
      buffer$348[_tmp$2000] = d1_hi$344;
      _tmp$2004 = offset$337;
      _tmp$2003 = digit_start$339 + _tmp$2004;
      _tmp$2002 = _tmp$2003 + 1;
      buffer$348[_tmp$2002] = d1_lo$345;
      _tmp$2007 = offset$337;
      _tmp$2006 = digit_start$339 + _tmp$2007;
      _tmp$2005 = _tmp$2006 + 2;
      buffer$348[_tmp$2005] = d2_hi$346;
      _tmp$2010 = offset$337;
      _tmp$2009 = digit_start$339 + _tmp$2010;
      _tmp$2008 = _tmp$2009 + 3;
      buffer$348[_tmp$2008] = d2_lo$347;
      continue;
    }
    break;
  }
  _tmp$2054 = num$335;
  remaining$350 = *(int32_t*)&_tmp$2054;
  while (1) {
    int32_t _tmp$2022 = remaining$350;
    if (_tmp$2022 >= 100) {
      int32_t _tmp$2034 = remaining$350;
      int32_t t$351 = _tmp$2034 / 100;
      int32_t _tmp$2033 = remaining$350;
      int32_t d$352 = _tmp$2033 % 100;
      int32_t _tmp$2023;
      int32_t _tmp$2032;
      int32_t _tmp$2031;
      int32_t d_hi$353;
      int32_t _tmp$2030;
      int32_t _tmp$2029;
      int32_t d_lo$354;
      int32_t _tmp$2025;
      int32_t _tmp$2024;
      int32_t _tmp$2028;
      int32_t _tmp$2027;
      int32_t _tmp$2026;
      remaining$350 = t$351;
      _tmp$2023 = offset$337;
      offset$337 = _tmp$2023 - 2;
      _tmp$2032 = d$352 / 10;
      _tmp$2031 = 48 + _tmp$2032;
      d_hi$353 = (uint16_t)_tmp$2031;
      _tmp$2030 = d$352 % 10;
      _tmp$2029 = 48 + _tmp$2030;
      d_lo$354 = (uint16_t)_tmp$2029;
      _tmp$2025 = offset$337;
      _tmp$2024 = digit_start$339 + _tmp$2025;
      buffer$348[_tmp$2024] = d_hi$353;
      _tmp$2028 = offset$337;
      _tmp$2027 = digit_start$339 + _tmp$2028;
      _tmp$2026 = _tmp$2027 + 1;
      buffer$348[_tmp$2026] = d_lo$354;
      continue;
    }
    break;
  }
  _tmp$2035 = remaining$350;
  if (_tmp$2035 >= 10) {
    int32_t _tmp$2036 = offset$337;
    int32_t _tmp$2047;
    int32_t _tmp$2046;
    int32_t _tmp$2045;
    int32_t d_hi$356;
    int32_t _tmp$2044;
    int32_t _tmp$2043;
    int32_t _tmp$2042;
    int32_t d_lo$357;
    int32_t _tmp$2038;
    int32_t _tmp$2037;
    int32_t _tmp$2041;
    int32_t _tmp$2040;
    int32_t _tmp$2039;
    offset$337 = _tmp$2036 - 2;
    _tmp$2047 = remaining$350;
    _tmp$2046 = _tmp$2047 / 10;
    _tmp$2045 = 48 + _tmp$2046;
    d_hi$356 = (uint16_t)_tmp$2045;
    _tmp$2044 = remaining$350;
    _tmp$2043 = _tmp$2044 % 10;
    _tmp$2042 = 48 + _tmp$2043;
    d_lo$357 = (uint16_t)_tmp$2042;
    _tmp$2038 = offset$337;
    _tmp$2037 = digit_start$339 + _tmp$2038;
    buffer$348[_tmp$2037] = d_hi$356;
    _tmp$2041 = offset$337;
    _tmp$2040 = digit_start$339 + _tmp$2041;
    _tmp$2039 = _tmp$2040 + 1;
    buffer$348[_tmp$2039] = d_lo$357;
    moonbit_decref(buffer$348);
  } else {
    int32_t _tmp$2048 = offset$337;
    int32_t _tmp$2053;
    int32_t _tmp$2049;
    int32_t _tmp$2052;
    int32_t _tmp$2051;
    int32_t _tmp$2050;
    offset$337 = _tmp$2048 - 1;
    _tmp$2053 = offset$337;
    _tmp$2049 = digit_start$339 + _tmp$2053;
    _tmp$2052 = remaining$350;
    _tmp$2051 = 48 + _tmp$2052;
    _tmp$2050 = (uint16_t)_tmp$2051;
    buffer$348[_tmp$2049] = _tmp$2050;
    moonbit_decref(buffer$348);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$330,
  uint32_t num$324,
  int32_t digit_start$322,
  int32_t total_len$321,
  int32_t radix$326
) {
  int32_t offset$320 = total_len$321 - digit_start$322;
  uint32_t n$323 = num$324;
  uint32_t base$325 = *(uint32_t*)&radix$326;
  int32_t _tmp$1978 = radix$326 - 1;
  int32_t _tmp$1977 = radix$326 & _tmp$1978;
  if (_tmp$1977 == 0) {
    int32_t shift$327 = moonbit_ctz32(radix$326);
    uint32_t mask$328 = base$325 - 1u;
    while (1) {
      uint32_t _tmp$1979 = n$323;
      if (_tmp$1979 > 0u) {
        int32_t _tmp$1980 = offset$320;
        uint32_t _tmp$1987;
        uint32_t _tmp$1986;
        int32_t digit$329;
        int32_t _tmp$1984;
        int32_t _tmp$1981;
        int32_t _tmp$1983;
        int32_t _tmp$1982;
        uint32_t _tmp$1985;
        offset$320 = _tmp$1980 - 1;
        _tmp$1987 = n$323;
        _tmp$1986 = _tmp$1987 & mask$328;
        digit$329 = *(int32_t*)&_tmp$1986;
        _tmp$1984 = offset$320;
        _tmp$1981 = digit_start$322 + _tmp$1984;
        _tmp$1983
        = ((moonbit_string_t)moonbit_string_literal_168.data)[
          digit$329
        ];
        _tmp$1982 = (uint16_t)_tmp$1983;
        buffer$330[_tmp$1981] = _tmp$1982;
        _tmp$1985 = n$323;
        n$323 = _tmp$1985 >> (shift$327 & 31);
        continue;
      } else {
        moonbit_decref(buffer$330);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1988 = n$323;
      if (_tmp$1988 > 0u) {
        int32_t _tmp$1989 = offset$320;
        uint32_t _tmp$1997;
        uint32_t q$332;
        uint32_t _tmp$1995;
        uint32_t _tmp$1996;
        uint32_t _tmp$1994;
        int32_t digit$333;
        int32_t _tmp$1993;
        int32_t _tmp$1990;
        int32_t _tmp$1992;
        int32_t _tmp$1991;
        offset$320 = _tmp$1989 - 1;
        _tmp$1997 = n$323;
        q$332 = _tmp$1997 / base$325;
        _tmp$1995 = n$323;
        _tmp$1996 = q$332 * base$325;
        _tmp$1994 = _tmp$1995 - _tmp$1996;
        digit$333 = *(int32_t*)&_tmp$1994;
        _tmp$1993 = offset$320;
        _tmp$1990 = digit_start$322 + _tmp$1993;
        _tmp$1992
        = ((moonbit_string_t)moonbit_string_literal_168.data)[
          digit$333
        ];
        _tmp$1991 = (uint16_t)_tmp$1992;
        buffer$330[_tmp$1990] = _tmp$1991;
        n$323 = q$332;
        continue;
      } else {
        moonbit_decref(buffer$330);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$317,
  uint32_t num$313,
  int32_t digit_start$311,
  int32_t total_len$310
) {
  int32_t offset$309 = total_len$310 - digit_start$311;
  uint32_t n$312 = num$313;
  int32_t _tmp$1972;
  while (1) {
    int32_t _tmp$1958 = offset$309;
    if (_tmp$1958 >= 2) {
      int32_t _tmp$1959 = offset$309;
      uint32_t _tmp$1971;
      uint32_t _tmp$1970;
      int32_t byte_val$314;
      int32_t hi$315;
      int32_t lo$316;
      int32_t _tmp$1963;
      int32_t _tmp$1960;
      int32_t _tmp$1962;
      int32_t _tmp$1961;
      int32_t _tmp$1968;
      int32_t _tmp$1967;
      int32_t _tmp$1964;
      int32_t _tmp$1966;
      int32_t _tmp$1965;
      uint32_t _tmp$1969;
      offset$309 = _tmp$1959 - 2;
      _tmp$1971 = n$312;
      _tmp$1970 = _tmp$1971 & 255u;
      byte_val$314 = *(int32_t*)&_tmp$1970;
      hi$315 = byte_val$314 / 16;
      lo$316 = byte_val$314 % 16;
      _tmp$1963 = offset$309;
      _tmp$1960 = digit_start$311 + _tmp$1963;
      _tmp$1962 = ((moonbit_string_t)moonbit_string_literal_168.data)[hi$315];
      _tmp$1961 = (uint16_t)_tmp$1962;
      buffer$317[_tmp$1960] = _tmp$1961;
      _tmp$1968 = offset$309;
      _tmp$1967 = digit_start$311 + _tmp$1968;
      _tmp$1964 = _tmp$1967 + 1;
      _tmp$1966 = ((moonbit_string_t)moonbit_string_literal_168.data)[lo$316];
      _tmp$1965 = (uint16_t)_tmp$1966;
      buffer$317[_tmp$1964] = _tmp$1965;
      _tmp$1969 = n$312;
      n$312 = _tmp$1969 >> 8;
      continue;
    }
    break;
  }
  _tmp$1972 = offset$309;
  if (_tmp$1972 == 1) {
    uint32_t _tmp$1976 = n$312;
    uint32_t _tmp$1975 = _tmp$1976 & 15u;
    int32_t nibble$319 = *(int32_t*)&_tmp$1975;
    int32_t _tmp$1974 =
      ((moonbit_string_t)moonbit_string_literal_168.data)[nibble$319];
    int32_t _tmp$1973 = (uint16_t)_tmp$1974;
    buffer$317[digit_start$311] = _tmp$1973;
    moonbit_decref(buffer$317);
  } else {
    moonbit_decref(buffer$317);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
  void* self$308
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$307 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1957;
  moonbit_incref(logger$307);
  _tmp$1957
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$307
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$308, _tmp$1957
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$307);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$306
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$305 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1956;
  moonbit_incref(logger$305);
  _tmp$1956
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$305
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$306, _tmp$1956
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$305);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  uint64_t self$304
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$303 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1955;
  moonbit_incref(logger$303);
  _tmp$1955
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$303
  };
  $$moonbitlang$core$builtin$Show$$UInt64$$output(self$304, _tmp$1955);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$303);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$302
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$301 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1954;
  moonbit_incref(logger$301);
  _tmp$1954
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$301
  };
  $$moonbitlang$core$builtin$Show$$Bool$$output(self$302, _tmp$1954);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$301);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$300
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$299 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1953;
  moonbit_incref(logger$299);
  _tmp$1953
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$299
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$300, _tmp$1953
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$299);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$298
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$297 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1952;
  moonbit_incref(logger$297);
  _tmp$1952
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$297
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$298, _tmp$1952);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$297);
}

int32_t $StringView$$start_offset(struct $StringView self$296) {
  int32_t _field$3956 = self$296.$1;
  moonbit_decref(self$296.$0);
  return _field$3956;
}

moonbit_string_t $StringView$$data(struct $StringView self$295) {
  moonbit_string_t _field$3957 = self$295.$0;
  return _field$3957;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$289,
  moonbit_string_t value$292,
  int32_t start$293,
  int32_t len$294
) {
  void* _try_err$291;
  struct $StringView _tmp$1947;
  int32_t _tmp$1949 = start$293 + len$294;
  int64_t _tmp$1948 = (int64_t)_tmp$1949;
  struct moonbit_result_1 _tmp$4480 =
    $String$$sub$inner(value$292, start$293, _tmp$1948);
  if (_tmp$4480.tag) {
    struct $StringView const _ok$1950 = _tmp$4480.data.ok;
    _tmp$1947 = _ok$1950;
  } else {
    void* const _err$1951 = _tmp$4480.data.err;
    _try_err$291 = _err$1951;
    goto $join$290;
  }
  goto $joinlet$4479;
  $join$290:;
  moonbit_decref(_try_err$291);
  moonbit_panic();
  $joinlet$4479:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$289, _tmp$1947
  );
  return 0;
}

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$282,
  int32_t start$288,
  int64_t end$284
) {
  int32_t len$281 = Moonbit_array_length(self$282);
  int32_t end$283;
  int32_t start$287;
  int32_t _if_result$4481;
  if (end$284 == 4294967296ll) {
    end$283 = len$281;
  } else {
    int64_t _Some$285 = end$284;
    int32_t _end$286 = (int32_t)_Some$285;
    if (_end$286 < 0) {
      end$283 = len$281 + _end$286;
    } else {
      end$283 = _end$286;
    }
  }
  if (start$288 < 0) {
    start$287 = len$281 + start$288;
  } else {
    start$287 = start$288;
  }
  if (start$287 >= 0) {
    if (start$287 <= end$283) {
      _if_result$4481 = end$283 <= len$281;
    } else {
      _if_result$4481 = 0;
    }
  } else {
    _if_result$4481 = 0;
  }
  if (_if_result$4481) {
    int32_t _if_result$4482;
    int32_t _if_result$4484;
    struct $StringView _tmp$1945;
    struct moonbit_result_1 _result$4486;
    if (start$287 < len$281) {
      int32_t _tmp$1941 = self$282[start$287];
      _if_result$4482 = $Int$$is_trailing_surrogate(_tmp$1941);
    } else {
      _if_result$4482 = 0;
    }
    if (_if_result$4482) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1942;
      struct moonbit_result_1 _result$4483;
      moonbit_decref(self$282);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1942
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$4483.tag = 0;
      _result$4483.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1942;
      return _result$4483;
    }
    if (end$283 < len$281) {
      int32_t _tmp$1943 = self$282[end$283];
      _if_result$4484 = $Int$$is_trailing_surrogate(_tmp$1943);
    } else {
      _if_result$4484 = 0;
    }
    if (_if_result$4484) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1944;
      struct moonbit_result_1 _result$4485;
      moonbit_decref(self$282);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1944
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$4485.tag = 0;
      _result$4485.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1944;
      return _result$4485;
    }
    _tmp$1945 = (struct $StringView){start$287, end$283, self$282};
    _result$4486.tag = 1;
    _result$4486.data.ok = _tmp$1945;
    return _result$4486;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1946;
    struct moonbit_result_1 _result$4487;
    moonbit_decref(self$282);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1946
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$4487.tag = 0;
    _result$4487.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1946;
    return _result$4487;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$280
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$279 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1940;
  moonbit_incref(_self$279);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$279, self$280);
  _tmp$1940 = _self$279;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1940);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$278
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$277 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1939;
  moonbit_incref(_self$277);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$277, self$278);
  _tmp$1939 = _self$277;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1939);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$275
) {
  int32_t seed$274;
  if (seed$opt$275 == 4294967296ll) {
    seed$274 = 0;
  } else {
    int64_t _Some$276 = seed$opt$275;
    seed$274 = (int32_t)_Some$276;
  }
  return $$moonbitlang$core$builtin$Hasher$$new$inner(seed$274);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$273
) {
  uint32_t _tmp$1938 = *(uint32_t*)&seed$273;
  uint32_t _tmp$1937 = _tmp$1938 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$4488 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$4488)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$4488->$0 = _tmp$1937;
  return _block$4488;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$272
) {
  uint32_t _tmp$1936 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$272);
  return *(int32_t*)&_tmp$1936;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$271
) {
  uint32_t _field$3958 = self$271->$0;
  uint32_t acc$270;
  uint32_t _tmp$1925;
  uint32_t _tmp$1927;
  uint32_t _tmp$1926;
  uint32_t _tmp$1928;
  uint32_t _tmp$1929;
  uint32_t _tmp$1931;
  uint32_t _tmp$1930;
  uint32_t _tmp$1932;
  uint32_t _tmp$1933;
  uint32_t _tmp$1935;
  uint32_t _tmp$1934;
  moonbit_decref(self$271);
  acc$270 = _field$3958;
  _tmp$1925 = acc$270;
  _tmp$1927 = acc$270;
  _tmp$1926 = _tmp$1927 >> 15;
  acc$270 = _tmp$1925 ^ _tmp$1926;
  _tmp$1928 = acc$270;
  acc$270 = _tmp$1928 * 2246822519u;
  _tmp$1929 = acc$270;
  _tmp$1931 = acc$270;
  _tmp$1930 = _tmp$1931 >> 13;
  acc$270 = _tmp$1929 ^ _tmp$1930;
  _tmp$1932 = acc$270;
  acc$270 = _tmp$1932 * 3266489917u;
  _tmp$1933 = acc$270;
  _tmp$1935 = acc$270;
  _tmp$1934 = _tmp$1935 >> 16;
  acc$270 = _tmp$1933 ^ _tmp$1934;
  return acc$270;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$269,
  int32_t value$268
) {
  $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(value$268, self$269);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$267,
  moonbit_string_t value$266
) {
  $$moonbitlang$core$builtin$Hash$$String$$hash_combine(value$266, self$267);
  return 0;
}

uint64_t $Int$$to_uint64(int32_t self$265) {
  int64_t _tmp$1924 = (int64_t)self$265;
  return *(uint64_t*)&_tmp$1924;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$263,
  int32_t value$264
) {
  uint32_t _tmp$1923 = *(uint32_t*)&value$264;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$263, _tmp$1923);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$261,
  uint32_t value$262
) {
  uint32_t acc$1922 = self$261->$0;
  uint32_t _tmp$1921 = acc$1922 + 4u;
  self$261->$0 = _tmp$1921;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$261, value$262);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$259,
  uint32_t input$260
) {
  uint32_t acc$1919 = self$259->$0;
  uint32_t _tmp$1920 = input$260 * 3266489917u;
  uint32_t _tmp$1918 = acc$1919 + _tmp$1920;
  uint32_t _tmp$1917 = $moonbitlang$core$builtin$rotl(_tmp$1918, 17);
  uint32_t _tmp$1916 = _tmp$1917 * 668265263u;
  self$259->$0 = _tmp$1916;
  moonbit_decref(self$259);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$257, int32_t r$258) {
  uint32_t _tmp$1913 = x$257 << (r$258 & 31);
  int32_t _tmp$1915 = 32 - r$258;
  uint32_t _tmp$1914 = x$257 >> (_tmp$1915 & 31);
  return _tmp$1913 | _tmp$1914;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$255,
  moonbit_string_t str$256
) {
  int32_t len$1903 = self$255->$1;
  int32_t _tmp$1905 = Moonbit_array_length(str$256);
  int32_t _tmp$1904 = _tmp$1905 * 2;
  int32_t _tmp$1902 = len$1903 + _tmp$1904;
  moonbit_bytes_t _field$3960;
  moonbit_bytes_t data$1906;
  int32_t len$1907;
  int32_t _tmp$1908;
  int32_t len$1910;
  int32_t _tmp$3959;
  int32_t _tmp$1912;
  int32_t _tmp$1911;
  int32_t _tmp$1909;
  moonbit_incref(self$255);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$255, _tmp$1902
  );
  _field$3960 = self$255->$0;
  data$1906 = _field$3960;
  len$1907 = self$255->$1;
  _tmp$1908 = Moonbit_array_length(str$256);
  moonbit_incref(data$1906);
  moonbit_incref(str$256);
  $FixedArray$$blit_from_string(data$1906, len$1907, str$256, 0, _tmp$1908);
  len$1910 = self$255->$1;
  _tmp$3959 = Moonbit_array_length(str$256);
  moonbit_decref(str$256);
  _tmp$1912 = _tmp$3959;
  _tmp$1911 = _tmp$1912 * 2;
  _tmp$1909 = len$1910 + _tmp$1911;
  self$255->$1 = _tmp$1909;
  moonbit_decref(self$255);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$247,
  int32_t bytes_offset$242,
  moonbit_string_t str$249,
  int32_t str_offset$245,
  int32_t length$243
) {
  int32_t _tmp$1901 = length$243 * 2;
  int32_t _tmp$1900 = bytes_offset$242 + _tmp$1901;
  int32_t e1$241 = _tmp$1900 - 1;
  int32_t _tmp$1899 = str_offset$245 + length$243;
  int32_t e2$244 = _tmp$1899 - 1;
  int32_t len1$246 = Moonbit_array_length(self$247);
  int32_t len2$248 = Moonbit_array_length(str$249);
  int32_t _if_result$4489;
  if (length$243 >= 0) {
    if (bytes_offset$242 >= 0) {
      if (e1$241 < len1$246) {
        if (str_offset$245 >= 0) {
          _if_result$4489 = e2$244 < len2$248;
        } else {
          _if_result$4489 = 0;
        }
      } else {
        _if_result$4489 = 0;
      }
    } else {
      _if_result$4489 = 0;
    }
  } else {
    _if_result$4489 = 0;
  }
  if (_if_result$4489) {
    int32_t end_str_offset$250 = str_offset$245 + length$243;
    int32_t i$251 = str_offset$245;
    int32_t j$252 = bytes_offset$242;
    while (1) {
      if (i$251 < end_str_offset$250) {
        int32_t _tmp$1896 = str$249[i$251];
        uint32_t c$253 = *(uint32_t*)&_tmp$1896;
        uint32_t _tmp$1892 = c$253 & 255u;
        int32_t _tmp$1891 = $UInt$$to_byte(_tmp$1892);
        int32_t _tmp$1893;
        uint32_t _tmp$1895;
        int32_t _tmp$1894;
        int32_t _tmp$1897;
        int32_t _tmp$1898;
        if (j$252 < 0 || j$252 >= Moonbit_array_length(self$247)) {
          moonbit_panic();
        }
        self$247[j$252] = _tmp$1891;
        _tmp$1893 = j$252 + 1;
        _tmp$1895 = c$253 >> 8;
        _tmp$1894 = $UInt$$to_byte(_tmp$1895);
        if (_tmp$1893 < 0 || _tmp$1893 >= Moonbit_array_length(self$247)) {
          moonbit_panic();
        }
        self$247[_tmp$1893] = _tmp$1894;
        _tmp$1897 = i$251 + 1;
        _tmp$1898 = j$252 + 2;
        i$251 = _tmp$1897;
        j$252 = _tmp$1898;
        continue;
      } else {
        moonbit_decref(str$249);
        moonbit_decref(self$247);
      }
      break;
    }
  } else {
    moonbit_decref(str$249);
    moonbit_decref(self$247);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$209
) {
  int32_t _tmp$1864 = Moonbit_array_length(repr$209);
  int64_t _tmp$1863 = (int64_t)_tmp$1864;
  moonbit_incref(repr$209);
  if ($String$$char_length_ge$inner(repr$209, 1, 0, _tmp$1863)) {
    int32_t _tmp$1890 = repr$209[0];
    int32_t _x$210 = _tmp$1890;
    if (_x$210 == 64) {
      int32_t _tmp$1889 = Moonbit_array_length(repr$209);
      int64_t _tmp$1888 = (int64_t)_tmp$1889;
      int64_t _bind$1600;
      int32_t _tmp$1886;
      int32_t _tmp$1887;
      struct $StringView _x$211;
      int32_t _tmp$1885;
      struct $StringView _tmp$1884;
      int64_t _bind$213;
      moonbit_incref(repr$209);
      _bind$1600
      = $String$$offset_of_nth_char$inner(
        repr$209, 1, 0, _tmp$1888
      );
      if (_bind$1600 == 4294967296ll) {
        _tmp$1886 = Moonbit_array_length(repr$209);
      } else {
        int64_t _Some$212 = _bind$1600;
        _tmp$1886 = (int32_t)_Some$212;
      }
      _tmp$1887 = Moonbit_array_length(repr$209);
      _x$211 = (struct $StringView){_tmp$1886, _tmp$1887, repr$209};
      _tmp$1885
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1884
      = (struct $StringView){
        0, _tmp$1885, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$211.$0);
      _bind$213 = $StringView$$find(_x$211, _tmp$1884);
      if (_bind$213 == 4294967296ll) {
        moonbit_decref(_x$211.$0);
        moonbit_panic();
      } else {
        int64_t _Some$214 = _bind$213;
        int32_t _pkg_end$215 = (int32_t)_Some$214;
        int64_t _tmp$1883 = (int64_t)_pkg_end$215;
        struct $StringView pkg$216;
        int32_t _tmp$1882;
        struct $StringView _tmp$1881;
        int64_t _bind$217;
        moonbit_incref(_x$211.$0);
        pkg$216 = $StringView$$view$inner(_x$211, 0, _tmp$1883);
        _tmp$1882
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1881
        = (struct $StringView){
          0, _tmp$1882, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$211.$0);
        _bind$217 = $StringView$$rev_find(_x$211, _tmp$1881);
        if (_bind$217 == 4294967296ll) {
          moonbit_decref(pkg$216.$0);
          moonbit_decref(_x$211.$0);
          moonbit_panic();
        } else {
          int64_t _Some$218 = _bind$217;
          int32_t _start_loc_end$219 = (int32_t)_Some$218;
          int32_t _tmp$1865 = _start_loc_end$219 + 1;
          int32_t _tmp$1866;
          moonbit_incref(_x$211.$0);
          _tmp$1866 = $StringView$$length(_x$211);
          if (_tmp$1865 < _tmp$1866) {
            int32_t _tmp$1880 = _start_loc_end$219 + 1;
            struct $StringView end_loc$220;
            struct $$3c$StringView$2a$StringView$3e$* _bind$221;
            moonbit_incref(_x$211.$0);
            end_loc$220
            = $StringView$$view$inner(
              _x$211, _tmp$1880, 4294967296ll
            );
            _bind$221
            = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
              end_loc$220
            );
            if (_bind$221 == 0) {
              if (_bind$221) {
                moonbit_decref(_bind$221);
              }
              moonbit_decref(pkg$216.$0);
              moonbit_decref(_x$211.$0);
              moonbit_panic();
            } else {
              struct $$3c$StringView$2a$StringView$3e$* _Some$222 = _bind$221;
              struct $$3c$StringView$2a$StringView$3e$* _x$223 = _Some$222;
              struct $StringView _field$3964 =
                (struct $StringView){
                  _x$223->$0_1, _x$223->$0_2, _x$223->$0_0
                };
              struct $StringView _end_line$224 = _field$3964;
              struct $StringView _field$3963 =
                (struct $StringView){
                  _x$223->$1_1, _x$223->$1_2, _x$223->$1_0
                };
              int32_t _cnt$4143 = Moonbit_object_header(_x$223)->rc;
              struct $StringView _end_column$225;
              int64_t _tmp$1879;
              struct $StringView rest$226;
              int32_t _tmp$1878;
              struct $StringView _tmp$1877;
              int64_t _bind$228;
              if (_cnt$4143 > 1) {
                int32_t _new_cnt$4144;
                moonbit_incref(_field$3963.$0);
                moonbit_incref(_end_line$224.$0);
                _new_cnt$4144 = _cnt$4143 - 1;
                Moonbit_object_header(_x$223)->rc = _new_cnt$4144;
              } else if (_cnt$4143 == 1) {
                moonbit_free(_x$223);
              }
              _end_column$225 = _field$3963;
              _tmp$1879 = (int64_t)_start_loc_end$219;
              rest$226 = $StringView$$view$inner(_x$211, 0, _tmp$1879);
              _tmp$1878
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1877
              = (struct $StringView){
                0,
                  _tmp$1878,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$226.$0);
              _bind$228 = $StringView$$rev_find(rest$226, _tmp$1877);
              if (_bind$228 == 4294967296ll) {
                moonbit_decref(rest$226.$0);
                moonbit_decref(_end_column$225.$0);
                moonbit_decref(_end_line$224.$0);
                moonbit_decref(pkg$216.$0);
                goto $join$227;
              } else {
                int64_t _Some$229 = _bind$228;
                int32_t _start_line_end$230 = (int32_t)_Some$229;
                int64_t _tmp$1876 = (int64_t)_start_line_end$230;
                struct $StringView _tmp$1873;
                int32_t _tmp$1875;
                struct $StringView _tmp$1874;
                int64_t _bind$231;
                moonbit_incref(rest$226.$0);
                _tmp$1873 = $StringView$$view$inner(rest$226, 0, _tmp$1876);
                _tmp$1875
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1874
                = (struct $StringView){
                  0,
                    _tmp$1875,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$231 = $StringView$$rev_find(_tmp$1873, _tmp$1874);
                if (_bind$231 == 4294967296ll) {
                  moonbit_decref(rest$226.$0);
                  moonbit_decref(_end_column$225.$0);
                  moonbit_decref(_end_line$224.$0);
                  moonbit_decref(pkg$216.$0);
                  goto $join$227;
                } else {
                  int64_t _Some$232 = _bind$231;
                  int32_t _filename_end$233 = (int32_t)_Some$232;
                  int32_t _tmp$1867 = _filename_end$233 + 1;
                  int32_t _tmp$1868;
                  moonbit_incref(rest$226.$0);
                  _tmp$1868 = $StringView$$length(rest$226);
                  if (_tmp$1867 < _tmp$1868) {
                    int32_t _tmp$1872 = _filename_end$233 + 1;
                    struct $StringView start_loc$234;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$235;
                    moonbit_incref(rest$226.$0);
                    start_loc$234
                    = $StringView$$view$inner(
                      rest$226, _tmp$1872, 4294967296ll
                    );
                    _bind$235
                    = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
                      start_loc$234
                    );
                    if (_bind$235 == 0) {
                      if (_bind$235) {
                        moonbit_decref(_bind$235);
                      }
                      moonbit_decref(rest$226.$0);
                      moonbit_decref(_end_column$225.$0);
                      moonbit_decref(_end_line$224.$0);
                      moonbit_decref(pkg$216.$0);
                      moonbit_panic();
                    } else {
                      struct $$3c$StringView$2a$StringView$3e$* _Some$236 =
                        _bind$235;
                      struct $$3c$StringView$2a$StringView$3e$* _x$237 =
                        _Some$236;
                      struct $StringView _field$3962 =
                        (struct $StringView){
                          _x$237->$0_1, _x$237->$0_2, _x$237->$0_0
                        };
                      struct $StringView _start_line$238 = _field$3962;
                      struct $StringView _field$3961 =
                        (struct $StringView){
                          _x$237->$1_1, _x$237->$1_2, _x$237->$1_0
                        };
                      int32_t _cnt$4145 = Moonbit_object_header(_x$237)->rc;
                      struct $StringView _start_column$239;
                      int32_t _tmp$1869;
                      if (_cnt$4145 > 1) {
                        int32_t _new_cnt$4146;
                        moonbit_incref(_field$3961.$0);
                        moonbit_incref(_start_line$238.$0);
                        _new_cnt$4146 = _cnt$4145 - 1;
                        Moonbit_object_header(_x$237)->rc = _new_cnt$4146;
                      } else if (_cnt$4145 == 1) {
                        moonbit_free(_x$237);
                      }
                      _start_column$239 = _field$3961;
                      _tmp$1869 = _pkg_end$215 + 1;
                      if (_filename_end$233 > _tmp$1869) {
                        int32_t _tmp$1870 = _pkg_end$215 + 1;
                        int64_t _tmp$1871 = (int64_t)_filename_end$233;
                        struct $StringView filename$240 =
                          $StringView$$view$inner(
                            rest$226, _tmp$1870, _tmp$1871
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$4493 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$4493)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$4493->$0_0 = pkg$216.$0;
                        _block$4493->$0_1 = pkg$216.$1;
                        _block$4493->$0_2 = pkg$216.$2;
                        _block$4493->$1_0 = filename$240.$0;
                        _block$4493->$1_1 = filename$240.$1;
                        _block$4493->$1_2 = filename$240.$2;
                        _block$4493->$2_0 = _start_line$238.$0;
                        _block$4493->$2_1 = _start_line$238.$1;
                        _block$4493->$2_2 = _start_line$238.$2;
                        _block$4493->$3_0 = _start_column$239.$0;
                        _block$4493->$3_1 = _start_column$239.$1;
                        _block$4493->$3_2 = _start_column$239.$2;
                        _block$4493->$4_0 = _end_line$224.$0;
                        _block$4493->$4_1 = _end_line$224.$1;
                        _block$4493->$4_2 = _end_line$224.$2;
                        _block$4493->$5_0 = _end_column$225.$0;
                        _block$4493->$5_1 = _end_column$225.$1;
                        _block$4493->$5_2 = _end_column$225.$2;
                        return _block$4493;
                      } else {
                        moonbit_decref(_start_column$239.$0);
                        moonbit_decref(_start_line$238.$0);
                        moonbit_decref(rest$226.$0);
                        moonbit_decref(_end_column$225.$0);
                        moonbit_decref(_end_line$224.$0);
                        moonbit_decref(pkg$216.$0);
                        moonbit_panic();
                      }
                    }
                  } else {
                    moonbit_decref(rest$226.$0);
                    moonbit_decref(_end_column$225.$0);
                    moonbit_decref(_end_line$224.$0);
                    moonbit_decref(pkg$216.$0);
                    moonbit_panic();
                  }
                }
              }
              $join$227:;
              moonbit_panic();
            }
          } else {
            moonbit_decref(pkg$216.$0);
            moonbit_decref(_x$211.$0);
            moonbit_panic();
          }
        }
      }
    } else {
      moonbit_decref(repr$209);
      goto $join$208;
    }
  } else {
    moonbit_decref(repr$209);
    goto $join$208;
  }
  $join$208:;
  moonbit_panic();
}

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$205
) {
  int32_t _tmp$1862 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1861;
  int64_t _bind$204;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1861
  = (struct $StringView){
    0, _tmp$1862, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$205.$0);
  _bind$204 = $StringView$$find(view$205, _tmp$1861);
  if (_bind$204 == 4294967296ll) {
    moonbit_decref(view$205.$0);
    return 0;
  } else {
    int64_t _Some$206 = _bind$204;
    int32_t _i$207 = (int32_t)_Some$206;
    int32_t _if_result$4494;
    if (_i$207 > 0) {
      int32_t _tmp$1854 = _i$207 + 1;
      int32_t _tmp$1855;
      moonbit_incref(view$205.$0);
      _tmp$1855 = $StringView$$length(view$205);
      _if_result$4494 = _tmp$1854 < _tmp$1855;
    } else {
      _if_result$4494 = 0;
    }
    if (_if_result$4494) {
      int64_t _tmp$1860 = (int64_t)_i$207;
      struct $StringView _tmp$1857;
      int32_t _tmp$1859;
      struct $StringView _tmp$1858;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1856;
      moonbit_incref(view$205.$0);
      _tmp$1857 = $StringView$$view$inner(view$205, 0, _tmp$1860);
      _tmp$1859 = _i$207 + 1;
      _tmp$1858 = $StringView$$view$inner(view$205, _tmp$1859, 4294967296ll);
      _tuple$1856
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1856)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1856->$0_0 = _tmp$1857.$0;
      _tuple$1856->$0_1 = _tmp$1857.$1;
      _tuple$1856->$0_2 = _tmp$1857.$2;
      _tuple$1856->$1_0 = _tmp$1858.$0;
      _tuple$1856->$1_1 = _tmp$1858.$1;
      _tuple$1856->$1_2 = _tmp$1858.$2;
      return _tuple$1856;
    } else {
      moonbit_decref(view$205.$0);
      return 0;
    }
  }
}

struct $StringView $StringView$$view$inner(
  struct $StringView self$202,
  int32_t start_offset$203,
  int64_t end_offset$200
) {
  int32_t end_offset$199;
  int32_t _if_result$4495;
  if (end_offset$200 == 4294967296ll) {
    moonbit_incref(self$202.$0);
    end_offset$199 = $StringView$$length(self$202);
  } else {
    int64_t _Some$201 = end_offset$200;
    end_offset$199 = (int32_t)_Some$201;
  }
  if (start_offset$203 >= 0) {
    if (start_offset$203 <= end_offset$199) {
      int32_t _tmp$1848;
      moonbit_incref(self$202.$0);
      _tmp$1848 = $StringView$$length(self$202);
      _if_result$4495 = end_offset$199 <= _tmp$1848;
    } else {
      _if_result$4495 = 0;
    }
  } else {
    _if_result$4495 = 0;
  }
  if (_if_result$4495) {
    moonbit_string_t _field$3966 = self$202.$0;
    moonbit_string_t str$1849 = _field$3966;
    int32_t start$1853 = self$202.$1;
    int32_t _tmp$1850 = start$1853 + start_offset$203;
    int32_t _field$3965 = self$202.$1;
    int32_t start$1852 = _field$3965;
    int32_t _tmp$1851 = start$1852 + end_offset$199;
    return (struct $StringView){_tmp$1850, _tmp$1851, str$1849};
  } else {
    moonbit_decref(self$202.$0);
    return $moonbitlang$core$builtin$abort$2(
             (moonbit_string_t)moonbit_string_literal_170.data,
               (moonbit_string_t)moonbit_string_literal_171.data
           );
  }
}

int64_t $StringView$$rev_find(
  struct $StringView self$198,
  struct $StringView str$197
) {
  int32_t _tmp$1847;
  moonbit_incref(str$197.$0);
  _tmp$1847 = $StringView$$length(str$197);
  if (_tmp$1847 <= 4) {
    return $moonbitlang$core$builtin$brute_force_rev_find(self$198, str$197);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
             self$198, str$197
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$188,
  struct $StringView needle$190
) {
  int32_t haystack_len$187;
  int32_t needle_len$189;
  moonbit_incref(haystack$188.$0);
  haystack_len$187 = $StringView$$length(haystack$188);
  moonbit_incref(needle$190.$0);
  needle_len$189 = $StringView$$length(needle$190);
  if (needle_len$189 > 0) {
    if (haystack_len$187 >= needle_len$189) {
      int32_t needle_first$191;
      int32_t i$192;
      moonbit_incref(needle$190.$0);
      needle_first$191 = $StringView$$unsafe_charcode_at(needle$190, 0);
      i$192 = haystack_len$187 - needle_len$189;
      while (1) {
        int32_t _tmp$1834 = i$192;
        if (_tmp$1834 >= 0) {
          int32_t _tmp$1839;
          while (1) {
            int32_t _tmp$1837 = i$192;
            int32_t _if_result$4498;
            if (_tmp$1837 >= 0) {
              int32_t _tmp$1836 = i$192;
              int32_t _tmp$1835;
              moonbit_incref(haystack$188.$0);
              _tmp$1835
              = $StringView$$unsafe_charcode_at(
                haystack$188, _tmp$1836
              );
              _if_result$4498 = _tmp$1835 != needle_first$191;
            } else {
              _if_result$4498 = 0;
            }
            if (_if_result$4498) {
              int32_t _tmp$1838 = i$192;
              i$192 = _tmp$1838 - 1;
              continue;
            }
            break;
          }
          _tmp$1839 = i$192;
          if (_tmp$1839 >= 0) {
            int32_t j$194 = 1;
            int32_t _tmp$1846;
            while (1) {
              if (j$194 < needle_len$189) {
                int32_t _tmp$1843 = i$192;
                int32_t _tmp$1842 = _tmp$1843 + j$194;
                int32_t _tmp$1840;
                int32_t _tmp$1841;
                int32_t _tmp$1844;
                moonbit_incref(haystack$188.$0);
                _tmp$1840
                = $StringView$$unsafe_charcode_at(
                  haystack$188, _tmp$1842
                );
                moonbit_incref(needle$190.$0);
                _tmp$1841
                = $StringView$$unsafe_charcode_at(
                  needle$190, j$194
                );
                if (_tmp$1840 != _tmp$1841) {
                  break;
                }
                _tmp$1844 = j$194 + 1;
                j$194 = _tmp$1844;
                continue;
              } else {
                int32_t _tmp$1845;
                moonbit_decref(needle$190.$0);
                moonbit_decref(haystack$188.$0);
                _tmp$1845 = i$192;
                return (int64_t)_tmp$1845;
              }
              break;
            }
            _tmp$1846 = i$192;
            i$192 = _tmp$1846 - 1;
          }
          continue;
        } else {
          moonbit_decref(needle$190.$0);
          moonbit_decref(haystack$188.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$190.$0);
      moonbit_decref(haystack$188.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$190.$0);
    moonbit_decref(haystack$188.$0);
    return (int64_t)haystack_len$187;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$177,
  struct $StringView needle$179
) {
  int32_t haystack_len$176;
  int32_t needle_len$178;
  moonbit_incref(haystack$177.$0);
  haystack_len$176 = $StringView$$length(haystack$177);
  moonbit_incref(needle$179.$0);
  needle_len$178 = $StringView$$length(needle$179);
  if (needle_len$178 > 0) {
    if (haystack_len$176 >= needle_len$178) {
      int32_t* skip_table$180 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$178);
      int32_t _tmp$1824 = needle_len$178 - 1;
      int32_t i$181 = _tmp$1824;
      int32_t _tmp$1833;
      int32_t i$183;
      while (1) {
        if (i$181 > 0) {
          int32_t _tmp$1822;
          int32_t _tmp$1821;
          int32_t _tmp$1823;
          moonbit_incref(needle$179.$0);
          _tmp$1822 = $StringView$$unsafe_charcode_at(needle$179, i$181);
          _tmp$1821 = _tmp$1822 & 255;
          if (
            _tmp$1821 < 0
            || _tmp$1821 >= Moonbit_array_length(skip_table$180)
          ) {
            moonbit_panic();
          }
          skip_table$180[_tmp$1821] = i$181;
          _tmp$1823 = i$181 - 1;
          i$181 = _tmp$1823;
          continue;
        }
        break;
      }
      _tmp$1833 = haystack_len$176 - needle_len$178;
      i$183 = _tmp$1833;
      while (1) {
        if (i$183 >= 0) {
          int32_t j$184 = 0;
          int32_t _tmp$1832;
          int32_t _tmp$1831;
          int32_t _tmp$1830;
          int32_t _tmp$1829;
          while (1) {
            if (j$184 < needle_len$178) {
              int32_t _tmp$1827 = i$183 + j$184;
              int32_t _tmp$1825;
              int32_t _tmp$1826;
              int32_t _tmp$1828;
              moonbit_incref(haystack$177.$0);
              _tmp$1825
              = $StringView$$unsafe_charcode_at(
                haystack$177, _tmp$1827
              );
              moonbit_incref(needle$179.$0);
              _tmp$1826 = $StringView$$unsafe_charcode_at(needle$179, j$184);
              if (_tmp$1825 != _tmp$1826) {
                break;
              }
              _tmp$1828 = j$184 + 1;
              j$184 = _tmp$1828;
              continue;
            } else {
              moonbit_decref(skip_table$180);
              moonbit_decref(needle$179.$0);
              moonbit_decref(haystack$177.$0);
              return (int64_t)i$183;
            }
            break;
          }
          moonbit_incref(haystack$177.$0);
          _tmp$1832 = $StringView$$unsafe_charcode_at(haystack$177, i$183);
          _tmp$1831 = _tmp$1832 & 255;
          if (
            _tmp$1831 < 0
            || _tmp$1831 >= Moonbit_array_length(skip_table$180)
          ) {
            moonbit_panic();
          }
          _tmp$1830 = (int32_t)skip_table$180[_tmp$1831];
          _tmp$1829 = i$183 - _tmp$1830;
          i$183 = _tmp$1829;
          continue;
        } else {
          moonbit_decref(skip_table$180);
          moonbit_decref(needle$179.$0);
          moonbit_decref(haystack$177.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$179.$0);
      moonbit_decref(haystack$177.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$179.$0);
    moonbit_decref(haystack$177.$0);
    return (int64_t)haystack_len$176;
  }
}

int64_t $StringView$$find(
  struct $StringView self$175,
  struct $StringView str$174
) {
  int32_t _tmp$1820;
  moonbit_incref(str$174.$0);
  _tmp$1820 = $StringView$$length(str$174);
  if (_tmp$1820 <= 4) {
    return $moonbitlang$core$builtin$brute_force_find(self$175, str$174);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_find(
             self$175, str$174
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$164,
  struct $StringView needle$166
) {
  int32_t haystack_len$163;
  int32_t needle_len$165;
  moonbit_incref(haystack$164.$0);
  haystack_len$163 = $StringView$$length(haystack$164);
  moonbit_incref(needle$166.$0);
  needle_len$165 = $StringView$$length(needle$166);
  if (needle_len$165 > 0) {
    if (haystack_len$163 >= needle_len$165) {
      int32_t needle_first$167;
      int32_t forward_len$168;
      int32_t i$169;
      moonbit_incref(needle$166.$0);
      needle_first$167 = $StringView$$unsafe_charcode_at(needle$166, 0);
      forward_len$168 = haystack_len$163 - needle_len$165;
      i$169 = 0;
      while (1) {
        int32_t _tmp$1807 = i$169;
        if (_tmp$1807 <= forward_len$168) {
          int32_t _tmp$1812;
          while (1) {
            int32_t _tmp$1810 = i$169;
            int32_t _if_result$4505;
            if (_tmp$1810 <= forward_len$168) {
              int32_t _tmp$1809 = i$169;
              int32_t _tmp$1808;
              moonbit_incref(haystack$164.$0);
              _tmp$1808
              = $StringView$$unsafe_charcode_at(
                haystack$164, _tmp$1809
              );
              _if_result$4505 = _tmp$1808 != needle_first$167;
            } else {
              _if_result$4505 = 0;
            }
            if (_if_result$4505) {
              int32_t _tmp$1811 = i$169;
              i$169 = _tmp$1811 + 1;
              continue;
            }
            break;
          }
          _tmp$1812 = i$169;
          if (_tmp$1812 <= forward_len$168) {
            int32_t j$171 = 1;
            int32_t _tmp$1819;
            while (1) {
              if (j$171 < needle_len$165) {
                int32_t _tmp$1816 = i$169;
                int32_t _tmp$1815 = _tmp$1816 + j$171;
                int32_t _tmp$1813;
                int32_t _tmp$1814;
                int32_t _tmp$1817;
                moonbit_incref(haystack$164.$0);
                _tmp$1813
                = $StringView$$unsafe_charcode_at(
                  haystack$164, _tmp$1815
                );
                moonbit_incref(needle$166.$0);
                _tmp$1814
                = $StringView$$unsafe_charcode_at(
                  needle$166, j$171
                );
                if (_tmp$1813 != _tmp$1814) {
                  break;
                }
                _tmp$1817 = j$171 + 1;
                j$171 = _tmp$1817;
                continue;
              } else {
                int32_t _tmp$1818;
                moonbit_decref(needle$166.$0);
                moonbit_decref(haystack$164.$0);
                _tmp$1818 = i$169;
                return (int64_t)_tmp$1818;
              }
              break;
            }
            _tmp$1819 = i$169;
            i$169 = _tmp$1819 + 1;
          }
          continue;
        } else {
          moonbit_decref(needle$166.$0);
          moonbit_decref(haystack$164.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$166.$0);
      moonbit_decref(haystack$164.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$166.$0);
    moonbit_decref(haystack$164.$0);
    return $moonbitlang$core$builtin$brute_force_find$constr$162;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$150,
  struct $StringView needle$152
) {
  int32_t haystack_len$149;
  int32_t needle_len$151;
  moonbit_incref(haystack$150.$0);
  haystack_len$149 = $StringView$$length(haystack$150);
  moonbit_incref(needle$152.$0);
  needle_len$151 = $StringView$$length(needle$152);
  if (needle_len$151 > 0) {
    if (haystack_len$149 >= needle_len$151) {
      int32_t* skip_table$153 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$151);
      int32_t _end4301$154 = needle_len$151 - 1;
      int32_t i$155 = 0;
      int32_t i$157;
      while (1) {
        if (i$155 < _end4301$154) {
          int32_t _tmp$1794;
          int32_t _tmp$1791;
          int32_t _tmp$1793;
          int32_t _tmp$1792;
          int32_t _tmp$1795;
          moonbit_incref(needle$152.$0);
          _tmp$1794 = $StringView$$unsafe_charcode_at(needle$152, i$155);
          _tmp$1791 = _tmp$1794 & 255;
          _tmp$1793 = needle_len$151 - 1;
          _tmp$1792 = _tmp$1793 - i$155;
          if (
            _tmp$1791 < 0
            || _tmp$1791 >= Moonbit_array_length(skip_table$153)
          ) {
            moonbit_panic();
          }
          skip_table$153[_tmp$1791] = _tmp$1792;
          _tmp$1795 = i$155 + 1;
          i$155 = _tmp$1795;
          continue;
        }
        break;
      }
      i$157 = 0;
      while (1) {
        int32_t _tmp$1796 = haystack_len$149 - needle_len$151;
        if (i$157 <= _tmp$1796) {
          int32_t _end4307$158 = needle_len$151 - 1;
          int32_t j$159 = 0;
          int32_t _tmp$1806;
          int32_t _tmp$1805;
          int32_t _tmp$1804;
          int32_t _tmp$1803;
          int32_t _tmp$1802;
          int32_t _tmp$1801;
          while (1) {
            if (j$159 <= _end4307$158) {
              int32_t _tmp$1799 = i$157 + j$159;
              int32_t _tmp$1797;
              int32_t _tmp$1798;
              int32_t _tmp$1800;
              moonbit_incref(haystack$150.$0);
              _tmp$1797
              = $StringView$$unsafe_charcode_at(
                haystack$150, _tmp$1799
              );
              moonbit_incref(needle$152.$0);
              _tmp$1798 = $StringView$$unsafe_charcode_at(needle$152, j$159);
              if (_tmp$1797 != _tmp$1798) {
                break;
              }
              _tmp$1800 = j$159 + 1;
              j$159 = _tmp$1800;
              continue;
            } else {
              moonbit_decref(skip_table$153);
              moonbit_decref(needle$152.$0);
              moonbit_decref(haystack$150.$0);
              return (int64_t)i$157;
            }
            break;
          }
          _tmp$1806 = i$157 + needle_len$151;
          _tmp$1805 = _tmp$1806 - 1;
          moonbit_incref(haystack$150.$0);
          _tmp$1804
          = $StringView$$unsafe_charcode_at(
            haystack$150, _tmp$1805
          );
          _tmp$1803 = _tmp$1804 & 255;
          if (
            _tmp$1803 < 0
            || _tmp$1803 >= Moonbit_array_length(skip_table$153)
          ) {
            moonbit_panic();
          }
          _tmp$1802 = (int32_t)skip_table$153[_tmp$1803];
          _tmp$1801 = i$157 + _tmp$1802;
          i$157 = _tmp$1801;
          continue;
        } else {
          moonbit_decref(skip_table$153);
          moonbit_decref(needle$152.$0);
          moonbit_decref(haystack$150.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$152.$0);
      moonbit_decref(haystack$150.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$152.$0);
    moonbit_decref(haystack$150.$0);
    return $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$148;
  }
}

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$146,
  int32_t index$147
) {
  moonbit_string_t _field$3969 = self$146.$0;
  moonbit_string_t str$1788 = _field$3969;
  int32_t _field$3968 = self$146.$1;
  int32_t start$1790 = _field$3968;
  int32_t _tmp$1789 = start$1790 + index$147;
  int32_t _tmp$3967 = str$1788[_tmp$1789];
  moonbit_decref(str$1788);
  return _tmp$3967;
}

int32_t $StringView$$length(struct $StringView self$145) {
  int32_t end$1786 = self$145.$2;
  int32_t _field$3970 = self$145.$1;
  int32_t start$1787;
  moonbit_decref(self$145.$0);
  start$1787 = _field$3970;
  return end$1786 - start$1787;
}

int32_t $$moonbitlang$core$builtin$Array$$at$2(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$143,
  int32_t index$144
) {
  int32_t len$142 = self$143->$1;
  int32_t _if_result$4510;
  if (index$144 >= 0) {
    _if_result$4510 = index$144 < len$142;
  } else {
    _if_result$4510 = 0;
  }
  if (_if_result$4510) {
    int32_t* _tmp$1785 = $$moonbitlang$core$builtin$Array$$buffer$4(self$143);
    int32_t _tmp$3971;
    if (index$144 < 0 || index$144 >= Moonbit_array_length(_tmp$1785)) {
      moonbit_panic();
    }
    _tmp$3971 = (int32_t)_tmp$1785[index$144];
    moonbit_decref(_tmp$1785);
    return _tmp$3971;
  } else {
    moonbit_decref(self$143);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$at$1(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$140,
  int32_t index$141
) {
  int32_t len$139 = self$140->$1;
  int32_t _if_result$4511;
  if (index$141 >= 0) {
    _if_result$4511 = index$141 < len$139;
  } else {
    _if_result$4511 = 0;
  }
  if (_if_result$4511) {
    int32_t* _tmp$1784 = $$moonbitlang$core$builtin$Array$$buffer$3(self$140);
    int32_t _tmp$3972;
    if (index$141 < 0 || index$141 >= Moonbit_array_length(_tmp$1784)) {
      moonbit_panic();
    }
    _tmp$3972 = (int32_t)_tmp$1784[index$141];
    moonbit_decref(_tmp$1784);
    return _tmp$3972;
  } else {
    moonbit_decref(self$140);
    moonbit_panic();
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$137,
  int32_t index$138
) {
  int32_t len$136 = self$137->$1;
  int32_t _if_result$4512;
  if (index$138 >= 0) {
    _if_result$4512 = index$138 < len$136;
  } else {
    _if_result$4512 = 0;
  }
  if (_if_result$4512) {
    moonbit_string_t* _tmp$1783 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$137);
    moonbit_string_t _tmp$3973;
    if (index$138 < 0 || index$138 >= Moonbit_array_length(_tmp$1783)) {
      moonbit_panic();
    }
    _tmp$3973 = (moonbit_string_t)_tmp$1783[index$138];
    moonbit_incref(_tmp$3973);
    moonbit_decref(_tmp$1783);
    return _tmp$3973;
  } else {
    moonbit_decref(self$137);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$5(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$135
) {
  int32_t _field$3974 = self$135->$1;
  moonbit_decref(self$135);
  return _field$3974;
}

int32_t $$moonbitlang$core$builtin$Array$$length$4(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$134
) {
  int32_t _field$3975 = self$134->$1;
  moonbit_decref(self$134);
  return _field$3975;
}

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$133
) {
  int32_t _field$3976 = self$133->$1;
  moonbit_decref(self$133);
  return _field$3976;
}

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$132
) {
  int32_t _field$3977 = self$132->$1;
  moonbit_decref(self$132);
  return _field$3977;
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$131
) {
  int32_t _field$3978 = self$131->$1;
  moonbit_decref(self$131);
  return _field$3978;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$130
) {
  int32_t _field$3979 = self$130->$1;
  moonbit_decref(self$130);
  return _field$3979;
}

struct $$3c$String$2a$AttributeValue$3e$** $$moonbitlang$core$builtin$Array$$buffer$6(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$129
) {
  struct $$3c$String$2a$AttributeValue$3e$** _field$3980 = self$129->$0;
  int32_t _cnt$4147 = Moonbit_object_header(self$129)->rc;
  if (_cnt$4147 > 1) {
    int32_t _new_cnt$4148;
    moonbit_incref(_field$3980);
    _new_cnt$4148 = _cnt$4147 - 1;
    Moonbit_object_header(self$129)->rc = _new_cnt$4148;
  } else if (_cnt$4147 == 1) {
    moonbit_free(self$129);
  }
  return _field$3980;
}

struct $$3c$String$2a$String$3e$** $$moonbitlang$core$builtin$Array$$buffer$5(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$128
) {
  struct $$3c$String$2a$String$3e$** _field$3981 = self$128->$0;
  int32_t _cnt$4149 = Moonbit_object_header(self$128)->rc;
  if (_cnt$4149 > 1) {
    int32_t _new_cnt$4150;
    moonbit_incref(_field$3981);
    _new_cnt$4150 = _cnt$4149 - 1;
    Moonbit_object_header(self$128)->rc = _new_cnt$4150;
  } else if (_cnt$4149 == 1) {
    moonbit_free(self$128);
  }
  return _field$3981;
}

int32_t* $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$127
) {
  int32_t* _field$3982 = self$127->$0;
  int32_t _cnt$4151 = Moonbit_object_header(self$127)->rc;
  if (_cnt$4151 > 1) {
    int32_t _new_cnt$4152;
    moonbit_incref(_field$3982);
    _new_cnt$4152 = _cnt$4151 - 1;
    Moonbit_object_header(self$127)->rc = _new_cnt$4152;
  } else if (_cnt$4151 == 1) {
    moonbit_free(self$127);
  }
  return _field$3982;
}

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$126
) {
  int32_t* _field$3983 = self$126->$0;
  int32_t _cnt$4153 = Moonbit_object_header(self$126)->rc;
  if (_cnt$4153 > 1) {
    int32_t _new_cnt$4154;
    moonbit_incref(_field$3983);
    _new_cnt$4154 = _cnt$4153 - 1;
    Moonbit_object_header(self$126)->rc = _new_cnt$4154;
  } else if (_cnt$4153 == 1) {
    moonbit_free(self$126);
  }
  return _field$3983;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$125
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$3984 =
    self$125->$0;
  int32_t _cnt$4155 = Moonbit_object_header(self$125)->rc;
  if (_cnt$4155 > 1) {
    int32_t _new_cnt$4156;
    moonbit_incref(_field$3984);
    _new_cnt$4156 = _cnt$4155 - 1;
    Moonbit_object_header(self$125)->rc = _new_cnt$4156;
  } else if (_cnt$4155 == 1) {
    moonbit_free(self$125);
  }
  return _field$3984;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$124
) {
  moonbit_string_t* _field$3985 = self$124->$0;
  int32_t _cnt$4157 = Moonbit_object_header(self$124)->rc;
  if (_cnt$4157 > 1) {
    int32_t _new_cnt$4158;
    moonbit_incref(_field$3985);
    _new_cnt$4158 = _cnt$4157 - 1;
    Moonbit_object_header(self$124)->rc = _new_cnt$4158;
  } else if (_cnt$4157 == 1) {
    moonbit_free(self$124);
  }
  return _field$3985;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$123
) {
  struct $$3c$String$2a$Int$3e$** _field$3986 = self$123->$0;
  int32_t _cnt$4159 = Moonbit_object_header(self$123)->rc;
  if (_cnt$4159 > 1) {
    int32_t _new_cnt$4160;
    moonbit_incref(_field$3986);
    _new_cnt$4160 = _cnt$4159 - 1;
    Moonbit_object_header(self$123)->rc = _new_cnt$4160;
  } else if (_cnt$4159 == 1) {
    moonbit_free(self$123);
  }
  return _field$3986;
}

moonbit_string_t $String$$escape(moonbit_string_t self$122) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$121 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1782;
  moonbit_incref(buf$121);
  _tmp$1782
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$121
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$122, _tmp$1782);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$121);
}

int32_t $moonbitlang$core$builtin$op_notequal$5(int32_t x$119, int32_t y$120) {
  int32_t _tmp$1781 = x$119 == y$120;
  return !_tmp$1781;
}

int32_t $moonbitlang$core$builtin$op_notequal$4(
  moonbit_string_t x$117,
  moonbit_string_t y$118
) {
  int32_t _tmp$3987 = moonbit_val_array_equal(x$117, y$118);
  int32_t _tmp$1780;
  moonbit_decref(x$117);
  moonbit_decref(y$118);
  _tmp$1780 = _tmp$3987;
  return !_tmp$1780;
}

int32_t $moonbitlang$core$builtin$op_notequal$3(int32_t x$115, int32_t y$116) {
  int32_t _tmp$1779 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$2(x$115, y$116);
  return !_tmp$1779;
}

int32_t $moonbitlang$core$builtin$op_notequal$2(
  moonbit_string_t x$113,
  moonbit_string_t y$114
) {
  int32_t _tmp$1778 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$1(x$113, y$114);
  return !_tmp$1778;
}

int32_t $moonbitlang$core$builtin$op_notequal$1(int64_t x$111, int64_t y$112) {
  int32_t _tmp$1777 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$0(x$111, y$112);
  return !_tmp$1777;
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$109, int32_t y$110) {
  int32_t _tmp$1776 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$109, y$110
    );
  return !_tmp$1776;
}

int32_t $Int$$is_trailing_surrogate(int32_t self$108) {
  if (56320 <= self$108) {
    return self$108 <= 57343;
  } else {
    return 0;
  }
}

int32_t $Int$$is_leading_surrogate(int32_t self$107) {
  if (55296 <= self$107) {
    return self$107 <= 56319;
  } else {
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$104,
  int32_t ch$106
) {
  int32_t len$1771 = self$104->$1;
  int32_t _tmp$1770 = len$1771 + 4;
  moonbit_bytes_t _field$3988;
  moonbit_bytes_t data$1774;
  int32_t len$1775;
  int32_t inc$105;
  int32_t len$1773;
  int32_t _tmp$1772;
  moonbit_incref(self$104);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$104, _tmp$1770
  );
  _field$3988 = self$104->$0;
  data$1774 = _field$3988;
  len$1775 = self$104->$1;
  moonbit_incref(data$1774);
  inc$105 = $FixedArray$$set_utf16le_char(data$1774, len$1775, ch$106);
  len$1773 = self$104->$1;
  _tmp$1772 = len$1773 + inc$105;
  self$104->$1 = _tmp$1772;
  moonbit_decref(self$104);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$99,
  int32_t required$100
) {
  moonbit_bytes_t _field$3992 = self$99->$0;
  moonbit_bytes_t data$1769 = _field$3992;
  int32_t _tmp$3991 = Moonbit_array_length(data$1769);
  int32_t current_len$98 = _tmp$3991;
  int32_t enough_space$101;
  int32_t _tmp$1767;
  int32_t _tmp$1768;
  moonbit_bytes_t new_data$103;
  moonbit_bytes_t _field$3990;
  moonbit_bytes_t data$1765;
  int32_t len$1766;
  moonbit_bytes_t _old$3989;
  if (required$100 <= current_len$98) {
    moonbit_decref(self$99);
    return 0;
  }
  enough_space$101 = current_len$98;
  while (1) {
    int32_t _tmp$1763 = enough_space$101;
    if (_tmp$1763 < required$100) {
      int32_t _tmp$1764 = enough_space$101;
      enough_space$101 = _tmp$1764 * 2;
      continue;
    }
    break;
  }
  _tmp$1767 = enough_space$101;
  _tmp$1768 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$103 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1767, _tmp$1768);
  _field$3990 = self$99->$0;
  data$1765 = _field$3990;
  len$1766 = self$99->$1;
  moonbit_incref(data$1765);
  moonbit_incref(new_data$103);
  $FixedArray$$unsafe_blit$0(new_data$103, 0, data$1765, 0, len$1766);
  _old$3989 = self$99->$0;
  moonbit_decref(_old$3989);
  self$99->$0 = new_data$103;
  moonbit_decref(self$99);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default() {
  return 0;
}

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$93,
  int32_t offset$94,
  int32_t value$92
) {
  uint32_t code$91 = $Char$$to_uint(value$92);
  if (code$91 < 65536u) {
    uint32_t _tmp$1746 = code$91 & 255u;
    int32_t _tmp$1745 = $UInt$$to_byte(_tmp$1746);
    int32_t _tmp$1747;
    uint32_t _tmp$1749;
    int32_t _tmp$1748;
    if (offset$94 < 0 || offset$94 >= Moonbit_array_length(self$93)) {
      moonbit_panic();
    }
    self$93[offset$94] = _tmp$1745;
    _tmp$1747 = offset$94 + 1;
    _tmp$1749 = code$91 >> 8;
    _tmp$1748 = $UInt$$to_byte(_tmp$1749);
    if (_tmp$1747 < 0 || _tmp$1747 >= Moonbit_array_length(self$93)) {
      moonbit_panic();
    }
    self$93[_tmp$1747] = _tmp$1748;
    moonbit_decref(self$93);
    return 2;
  } else if (code$91 < 1114112u) {
    uint32_t hi$95 = code$91 - 65536u;
    uint32_t _tmp$1762 = hi$95 >> 10;
    uint32_t lo$96 = _tmp$1762 | 55296u;
    uint32_t _tmp$1761 = hi$95 & 1023u;
    uint32_t hi$97 = _tmp$1761 | 56320u;
    uint32_t _tmp$1751 = lo$96 & 255u;
    int32_t _tmp$1750 = $UInt$$to_byte(_tmp$1751);
    int32_t _tmp$1752;
    uint32_t _tmp$1754;
    int32_t _tmp$1753;
    int32_t _tmp$1755;
    uint32_t _tmp$1757;
    int32_t _tmp$1756;
    int32_t _tmp$1758;
    uint32_t _tmp$1760;
    int32_t _tmp$1759;
    if (offset$94 < 0 || offset$94 >= Moonbit_array_length(self$93)) {
      moonbit_panic();
    }
    self$93[offset$94] = _tmp$1750;
    _tmp$1752 = offset$94 + 1;
    _tmp$1754 = lo$96 >> 8;
    _tmp$1753 = $UInt$$to_byte(_tmp$1754);
    if (_tmp$1752 < 0 || _tmp$1752 >= Moonbit_array_length(self$93)) {
      moonbit_panic();
    }
    self$93[_tmp$1752] = _tmp$1753;
    _tmp$1755 = offset$94 + 2;
    _tmp$1757 = hi$97 & 255u;
    _tmp$1756 = $UInt$$to_byte(_tmp$1757);
    if (_tmp$1755 < 0 || _tmp$1755 >= Moonbit_array_length(self$93)) {
      moonbit_panic();
    }
    self$93[_tmp$1755] = _tmp$1756;
    _tmp$1758 = offset$94 + 3;
    _tmp$1760 = hi$97 >> 8;
    _tmp$1759 = $UInt$$to_byte(_tmp$1760);
    if (_tmp$1758 < 0 || _tmp$1758 >= Moonbit_array_length(self$93)) {
      moonbit_panic();
    }
    self$93[_tmp$1758] = _tmp$1759;
    moonbit_decref(self$93);
    return 4;
  } else {
    moonbit_decref(self$93);
    return $moonbitlang$core$builtin$abort$1(
             (moonbit_string_t)moonbit_string_literal_172.data,
               (moonbit_string_t)moonbit_string_literal_173.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$90) {
  int32_t _tmp$1744 = *(int32_t*)&self$90;
  return _tmp$1744 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$89) {
  int32_t _tmp$1743 = self$89;
  return *(uint32_t*)&_tmp$1743;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$88
) {
  moonbit_bytes_t _field$3994 = self$88->$0;
  moonbit_bytes_t data$1742 = _field$3994;
  moonbit_bytes_t _tmp$1739;
  int32_t _field$3993;
  int32_t len$1741;
  int64_t _tmp$1740;
  moonbit_incref(data$1742);
  _tmp$1739 = data$1742;
  _field$3993 = self$88->$1;
  moonbit_decref(self$88);
  len$1741 = _field$3993;
  _tmp$1740 = (int64_t)len$1741;
  return $Bytes$$to_unchecked_string$inner(_tmp$1739, 0, _tmp$1740);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$83,
  int32_t offset$87,
  int64_t length$85
) {
  int32_t len$82 = Moonbit_array_length(self$83);
  int32_t length$84;
  int32_t _if_result$4514;
  if (length$85 == 4294967296ll) {
    length$84 = len$82 - offset$87;
  } else {
    int64_t _Some$86 = length$85;
    length$84 = (int32_t)_Some$86;
  }
  if (offset$87 >= 0) {
    if (length$84 >= 0) {
      int32_t _tmp$1738 = offset$87 + length$84;
      _if_result$4514 = _tmp$1738 <= len$82;
    } else {
      _if_result$4514 = 0;
    }
  } else {
    _if_result$4514 = 0;
  }
  if (_if_result$4514) {
    return $moonbitlang$core$builtin$unsafe_sub_string(
             self$83, offset$87, length$84
           );
  } else {
    moonbit_decref(self$83);
    moonbit_panic();
  }
}

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$80
) {
  int32_t initial$79;
  moonbit_bytes_t data$81;
  struct $$moonbitlang$core$builtin$StringBuilder* _block$4515;
  if (size_hint$80 < 1) {
    initial$79 = 1;
  } else {
    initial$79 = size_hint$80;
  }
  data$81 = (moonbit_bytes_t)moonbit_make_bytes(initial$79, 0);
  _block$4515
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$4515)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$4515->$0 = data$81;
  _block$4515->$1 = 0;
  return _block$4515;
}

int32_t $Byte$$to_char(int32_t self$78) {
  int32_t _tmp$1737 = (int32_t)self$78;
  return _tmp$1737;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$73,
  int32_t dst_offset$74,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$75,
  int32_t src_offset$76,
  int32_t len$77
) {
  $FixedArray$$unsafe_blit$3(
    dst$73, dst_offset$74, src$75, src_offset$76, len$77
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$68,
  int32_t dst_offset$69,
  struct $$3c$String$2a$Int$3e$** src$70,
  int32_t src_offset$71,
  int32_t len$72
) {
  $FixedArray$$unsafe_blit$2(
    dst$68, dst_offset$69, src$70, src_offset$71, len$72
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$63,
  int32_t dst_offset$64,
  moonbit_string_t* src$65,
  int32_t src_offset$66,
  int32_t len$67
) {
  $FixedArray$$unsafe_blit$1(
    dst$63, dst_offset$64, src$65, src_offset$66, len$67
  );
  return 0;
}

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$54,
  int32_t dst_offset$56,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$55,
  int32_t src_offset$57,
  int32_t len$59
) {
  int32_t _if_result$4516;
  if (dst$54 == src$55) {
    _if_result$4516 = dst_offset$56 < src_offset$57;
  } else {
    _if_result$4516 = 0;
  }
  if (_if_result$4516) {
    int32_t i$58 = 0;
    while (1) {
      if (i$58 < len$59) {
        int32_t _tmp$1728 = dst_offset$56 + i$58;
        int32_t _tmp$1730 = src_offset$57 + i$58;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3996;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1729;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$3995;
        int32_t _tmp$1731;
        if (_tmp$1730 < 0 || _tmp$1730 >= Moonbit_array_length(src$55)) {
          moonbit_panic();
        }
        _tmp$3996
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$55[
            _tmp$1730
          ];
        _tmp$1729 = _tmp$3996;
        if (_tmp$1728 < 0 || _tmp$1728 >= Moonbit_array_length(dst$54)) {
          moonbit_panic();
        }
        _old$3995
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$54[
            _tmp$1728
          ];
        if (_tmp$1729) {
          moonbit_incref(_tmp$1729);
        }
        if (_old$3995) {
          moonbit_decref(_old$3995);
        }
        dst$54[_tmp$1728] = _tmp$1729;
        _tmp$1731 = i$58 + 1;
        i$58 = _tmp$1731;
        continue;
      } else {
        moonbit_decref(src$55);
        moonbit_decref(dst$54);
      }
      break;
    }
  } else {
    int32_t _tmp$1736 = len$59 - 1;
    int32_t i$61 = _tmp$1736;
    while (1) {
      if (i$61 >= 0) {
        int32_t _tmp$1732 = dst_offset$56 + i$61;
        int32_t _tmp$1734 = src_offset$57 + i$61;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3998;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1733;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$3997;
        int32_t _tmp$1735;
        if (_tmp$1734 < 0 || _tmp$1734 >= Moonbit_array_length(src$55)) {
          moonbit_panic();
        }
        _tmp$3998
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$55[
            _tmp$1734
          ];
        _tmp$1733 = _tmp$3998;
        if (_tmp$1732 < 0 || _tmp$1732 >= Moonbit_array_length(dst$54)) {
          moonbit_panic();
        }
        _old$3997
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$54[
            _tmp$1732
          ];
        if (_tmp$1733) {
          moonbit_incref(_tmp$1733);
        }
        if (_old$3997) {
          moonbit_decref(_old$3997);
        }
        dst$54[_tmp$1732] = _tmp$1733;
        _tmp$1735 = i$61 - 1;
        i$61 = _tmp$1735;
        continue;
      } else {
        moonbit_decref(src$55);
        moonbit_decref(dst$54);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$2(
  struct $$3c$String$2a$Int$3e$** dst$45,
  int32_t dst_offset$47,
  struct $$3c$String$2a$Int$3e$** src$46,
  int32_t src_offset$48,
  int32_t len$50
) {
  int32_t _if_result$4519;
  if (dst$45 == src$46) {
    _if_result$4519 = dst_offset$47 < src_offset$48;
  } else {
    _if_result$4519 = 0;
  }
  if (_if_result$4519) {
    int32_t i$49 = 0;
    while (1) {
      if (i$49 < len$50) {
        int32_t _tmp$1719 = dst_offset$47 + i$49;
        int32_t _tmp$1721 = src_offset$48 + i$49;
        struct $$3c$String$2a$Int$3e$* _tmp$4000;
        struct $$3c$String$2a$Int$3e$* _tmp$1720;
        struct $$3c$String$2a$Int$3e$* _old$3999;
        int32_t _tmp$1722;
        if (_tmp$1721 < 0 || _tmp$1721 >= Moonbit_array_length(src$46)) {
          moonbit_panic();
        }
        _tmp$4000 = (struct $$3c$String$2a$Int$3e$*)src$46[_tmp$1721];
        _tmp$1720 = _tmp$4000;
        if (_tmp$1719 < 0 || _tmp$1719 >= Moonbit_array_length(dst$45)) {
          moonbit_panic();
        }
        _old$3999 = (struct $$3c$String$2a$Int$3e$*)dst$45[_tmp$1719];
        if (_tmp$1720) {
          moonbit_incref(_tmp$1720);
        }
        if (_old$3999) {
          moonbit_decref(_old$3999);
        }
        dst$45[_tmp$1719] = _tmp$1720;
        _tmp$1722 = i$49 + 1;
        i$49 = _tmp$1722;
        continue;
      } else {
        moonbit_decref(src$46);
        moonbit_decref(dst$45);
      }
      break;
    }
  } else {
    int32_t _tmp$1727 = len$50 - 1;
    int32_t i$52 = _tmp$1727;
    while (1) {
      if (i$52 >= 0) {
        int32_t _tmp$1723 = dst_offset$47 + i$52;
        int32_t _tmp$1725 = src_offset$48 + i$52;
        struct $$3c$String$2a$Int$3e$* _tmp$4002;
        struct $$3c$String$2a$Int$3e$* _tmp$1724;
        struct $$3c$String$2a$Int$3e$* _old$4001;
        int32_t _tmp$1726;
        if (_tmp$1725 < 0 || _tmp$1725 >= Moonbit_array_length(src$46)) {
          moonbit_panic();
        }
        _tmp$4002 = (struct $$3c$String$2a$Int$3e$*)src$46[_tmp$1725];
        _tmp$1724 = _tmp$4002;
        if (_tmp$1723 < 0 || _tmp$1723 >= Moonbit_array_length(dst$45)) {
          moonbit_panic();
        }
        _old$4001 = (struct $$3c$String$2a$Int$3e$*)dst$45[_tmp$1723];
        if (_tmp$1724) {
          moonbit_incref(_tmp$1724);
        }
        if (_old$4001) {
          moonbit_decref(_old$4001);
        }
        dst$45[_tmp$1723] = _tmp$1724;
        _tmp$1726 = i$52 - 1;
        i$52 = _tmp$1726;
        continue;
      } else {
        moonbit_decref(src$46);
        moonbit_decref(dst$45);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$1(
  moonbit_string_t* dst$36,
  int32_t dst_offset$38,
  moonbit_string_t* src$37,
  int32_t src_offset$39,
  int32_t len$41
) {
  int32_t _if_result$4522;
  if (dst$36 == src$37) {
    _if_result$4522 = dst_offset$38 < src_offset$39;
  } else {
    _if_result$4522 = 0;
  }
  if (_if_result$4522) {
    int32_t i$40 = 0;
    while (1) {
      if (i$40 < len$41) {
        int32_t _tmp$1710 = dst_offset$38 + i$40;
        int32_t _tmp$1712 = src_offset$39 + i$40;
        moonbit_string_t _tmp$4004;
        moonbit_string_t _tmp$1711;
        moonbit_string_t _old$4003;
        int32_t _tmp$1713;
        if (_tmp$1712 < 0 || _tmp$1712 >= Moonbit_array_length(src$37)) {
          moonbit_panic();
        }
        _tmp$4004 = (moonbit_string_t)src$37[_tmp$1712];
        _tmp$1711 = _tmp$4004;
        if (_tmp$1710 < 0 || _tmp$1710 >= Moonbit_array_length(dst$36)) {
          moonbit_panic();
        }
        _old$4003 = (moonbit_string_t)dst$36[_tmp$1710];
        moonbit_incref(_tmp$1711);
        moonbit_decref(_old$4003);
        dst$36[_tmp$1710] = _tmp$1711;
        _tmp$1713 = i$40 + 1;
        i$40 = _tmp$1713;
        continue;
      } else {
        moonbit_decref(src$37);
        moonbit_decref(dst$36);
      }
      break;
    }
  } else {
    int32_t _tmp$1718 = len$41 - 1;
    int32_t i$43 = _tmp$1718;
    while (1) {
      if (i$43 >= 0) {
        int32_t _tmp$1714 = dst_offset$38 + i$43;
        int32_t _tmp$1716 = src_offset$39 + i$43;
        moonbit_string_t _tmp$4006;
        moonbit_string_t _tmp$1715;
        moonbit_string_t _old$4005;
        int32_t _tmp$1717;
        if (_tmp$1716 < 0 || _tmp$1716 >= Moonbit_array_length(src$37)) {
          moonbit_panic();
        }
        _tmp$4006 = (moonbit_string_t)src$37[_tmp$1716];
        _tmp$1715 = _tmp$4006;
        if (_tmp$1714 < 0 || _tmp$1714 >= Moonbit_array_length(dst$36)) {
          moonbit_panic();
        }
        _old$4005 = (moonbit_string_t)dst$36[_tmp$1714];
        moonbit_incref(_tmp$1715);
        moonbit_decref(_old$4005);
        dst$36[_tmp$1714] = _tmp$1715;
        _tmp$1717 = i$43 - 1;
        i$43 = _tmp$1717;
        continue;
      } else {
        moonbit_decref(src$37);
        moonbit_decref(dst$36);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$0(
  moonbit_bytes_t dst$27,
  int32_t dst_offset$29,
  moonbit_bytes_t src$28,
  int32_t src_offset$30,
  int32_t len$32
) {
  int32_t _if_result$4525;
  if (dst$27 == src$28) {
    _if_result$4525 = dst_offset$29 < src_offset$30;
  } else {
    _if_result$4525 = 0;
  }
  if (_if_result$4525) {
    int32_t i$31 = 0;
    while (1) {
      if (i$31 < len$32) {
        int32_t _tmp$1701 = dst_offset$29 + i$31;
        int32_t _tmp$1703 = src_offset$30 + i$31;
        int32_t _tmp$1702;
        int32_t _tmp$1704;
        if (_tmp$1703 < 0 || _tmp$1703 >= Moonbit_array_length(src$28)) {
          moonbit_panic();
        }
        _tmp$1702 = (int32_t)src$28[_tmp$1703];
        if (_tmp$1701 < 0 || _tmp$1701 >= Moonbit_array_length(dst$27)) {
          moonbit_panic();
        }
        dst$27[_tmp$1701] = _tmp$1702;
        _tmp$1704 = i$31 + 1;
        i$31 = _tmp$1704;
        continue;
      } else {
        moonbit_decref(src$28);
        moonbit_decref(dst$27);
      }
      break;
    }
  } else {
    int32_t _tmp$1709 = len$32 - 1;
    int32_t i$34 = _tmp$1709;
    while (1) {
      if (i$34 >= 0) {
        int32_t _tmp$1705 = dst_offset$29 + i$34;
        int32_t _tmp$1707 = src_offset$30 + i$34;
        int32_t _tmp$1706;
        int32_t _tmp$1708;
        if (_tmp$1707 < 0 || _tmp$1707 >= Moonbit_array_length(src$28)) {
          moonbit_panic();
        }
        _tmp$1706 = (int32_t)src$28[_tmp$1707];
        if (_tmp$1705 < 0 || _tmp$1705 >= Moonbit_array_length(dst$27)) {
          moonbit_panic();
        }
        dst$27[_tmp$1705] = _tmp$1706;
        _tmp$1708 = i$34 - 1;
        i$34 = _tmp$1708;
        continue;
      } else {
        moonbit_decref(src$28);
        moonbit_decref(dst$27);
      }
      break;
    }
  }
  return 0;
}

int64_t $moonbitlang$core$builtin$abort$3(
  moonbit_string_t string$25,
  moonbit_string_t loc$26
) {
  moonbit_string_t _tmp$1700 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$25);
  moonbit_string_t _tmp$1698 =
    moonbit_add_string(
      _tmp$1700, (moonbit_string_t)moonbit_string_literal_174.data
    );
  moonbit_string_t _tmp$1699 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$26);
  moonbit_string_t _tmp$1697 = moonbit_add_string(_tmp$1698, _tmp$1699);
  moonbit_string_t _tmp$1696 =
    moonbit_add_string(
      _tmp$1697, (moonbit_string_t)moonbit_string_literal_175.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1696);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$23,
  moonbit_string_t loc$24
) {
  moonbit_string_t _tmp$1695 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$23);
  moonbit_string_t _tmp$1693 =
    moonbit_add_string(
      _tmp$1695, (moonbit_string_t)moonbit_string_literal_174.data
    );
  moonbit_string_t _tmp$1694 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$24);
  moonbit_string_t _tmp$1692 = moonbit_add_string(_tmp$1693, _tmp$1694);
  moonbit_string_t _tmp$1691 =
    moonbit_add_string(
      _tmp$1692, (moonbit_string_t)moonbit_string_literal_175.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1691);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$21,
  moonbit_string_t loc$22
) {
  moonbit_string_t _tmp$1690 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$1688 =
    moonbit_add_string(
      _tmp$1690, (moonbit_string_t)moonbit_string_literal_174.data
    );
  moonbit_string_t _tmp$1689 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$1687 = moonbit_add_string(_tmp$1688, _tmp$1689);
  moonbit_string_t _tmp$1686 =
    moonbit_add_string(
      _tmp$1687, (moonbit_string_t)moonbit_string_literal_175.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1686);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$1685 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$1683 =
    moonbit_add_string(
      _tmp$1685, (moonbit_string_t)moonbit_string_literal_174.data
    );
  moonbit_string_t _tmp$1684 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$1682 = moonbit_add_string(_tmp$1683, _tmp$1684);
  moonbit_string_t _tmp$1681 =
    moonbit_add_string(
      _tmp$1682, (moonbit_string_t)moonbit_string_literal_175.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1681);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$15,
  struct $$moonbitlang$core$builtin$Logger _x_5272$18
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$16 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$15;
  moonbit_string_t _field$4007 = _Failure$16->$0;
  int32_t _cnt$4161 = Moonbit_object_header(_Failure$16)->rc;
  moonbit_string_t _$2a$err_payload_5273$17;
  struct $$moonbitlang$core$builtin$Logger _bind$1680;
  if (_cnt$4161 > 1) {
    int32_t _new_cnt$4162;
    moonbit_incref(_field$4007);
    _new_cnt$4162 = _cnt$4161 - 1;
    Moonbit_object_header(_Failure$16)->rc = _new_cnt$4162;
  } else if (_cnt$4161 == 1) {
    moonbit_free(_Failure$16);
  }
  _$2a$err_payload_5273$17 = _field$4007;
  if (_x_5272$18.$1) {
    moonbit_incref(_x_5272$18.$1);
  }
  _x_5272$18.$0->$method_0(
    _x_5272$18.$1, (moonbit_string_t)moonbit_string_literal_176.data
  );
  if (_x_5272$18.$1) {
    moonbit_incref(_x_5272$18.$1);
  }
  $$moonbitlang$core$builtin$Logger$$write_object$1(
    _x_5272$18, _$2a$err_payload_5273$17
  );
  _bind$1680 = _x_5272$18;
  _bind$1680.$0->$method_0(
    _bind$1680.$1, (moonbit_string_t)moonbit_string_literal_152.data
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
        _x_5286$14.$1, (moonbit_string_t)moonbit_string_literal_177.data
      );
      break;
    }
    default: {
      moonbit_decref(_x_5285$13);
      _x_5286$14.$0->$method_0(
        _x_5286$14.$1, (moonbit_string_t)moonbit_string_literal_178.data
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
  int32_t obj$9
) {
  $$moonbitlang$core$builtin$Show$$Bool$$output(obj$9, self$10);
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

moonbit_string_t $Error$to_string(void* _e$1599) {
  switch (Moonbit_object_tag(_e$1599)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
               _e$1599
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
               _e$1599
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1599);
      return (moonbit_string_t)moonbit_string_literal_179.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
               _e$1599
             );
      break;
    }
    
    case 3: {
      moonbit_decref(_e$1599);
      return (moonbit_string_t)moonbit_string_literal_180.data;
      break;
    }
    default: {
      moonbit_decref(_e$1599);
      return (moonbit_string_t)moonbit_string_literal_181.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1617,
  int32_t _param$1616
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1615 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1617;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1615, _param$1616
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1614,
  struct $StringView _param$1613
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1612 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1614;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1612, _param$1613
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1611,
  moonbit_string_t _param$1608,
  int32_t _param$1609,
  int32_t _param$1610
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1607 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1611;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1607, _param$1608, _param$1609, _param$1610
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1606,
  moonbit_string_t _param$1605
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1604 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1606;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1604, _param$1605
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1424;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1623;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1622;
  moonbit_string_t* _tmp$1677;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1676;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1675;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1658;
  moonbit_string_t* _tmp$1674;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1673;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1672;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1659;
  moonbit_string_t* _tmp$1671;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1670;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1669;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1660;
  moonbit_string_t* _tmp$1668;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1667;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1666;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1661;
  moonbit_string_t* _tmp$1665;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1664;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1663;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1662;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$1421;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1657;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1656;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1655;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1626;
  moonbit_string_t* _tmp$1654;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1653;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1652;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1635;
  moonbit_string_t* _tmp$1651;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1650;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1649;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1636;
  moonbit_string_t* _tmp$1648;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1647;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1646;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1637;
  moonbit_string_t* _tmp$1645;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1644;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1643;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1638;
  moonbit_string_t* _tmp$1642;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1641;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1640;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1639;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$1422;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1634;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1633;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1632;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1627;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$1423;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1631;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1630;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1629;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1628;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1420;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1625;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1624;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1425;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1679;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1678;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2$dyncall$closure.data;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$148 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$162 = (int64_t)0;
  _bind$1424
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1623 = _bind$1424;
  _tmp$1622
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1623
  };
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1622
  );
  _tmp$1677 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1677[0] = (moonbit_string_t)moonbit_string_literal_182.data;
  _tmp$1676
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1676)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1676->$0 = _tmp$1677;
  _tmp$1676->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0$clo
  );
  _tuple$1675
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1675)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1675->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_0$clo;
  _tuple$1675->$1 = _tmp$1676;
  _tuple$1658
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1658)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1658->$0 = 0;
  _tuple$1658->$1 = _tuple$1675;
  _tmp$1674 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1674[0] = (moonbit_string_t)moonbit_string_literal_183.data;
  _tmp$1673
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1673)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1673->$0 = _tmp$1674;
  _tmp$1673->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1$clo
  );
  _tuple$1672
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1672)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1672->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_1$clo;
  _tuple$1672->$1 = _tmp$1673;
  _tuple$1659
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1659)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1659->$0 = 1;
  _tuple$1659->$1 = _tuple$1672;
  _tmp$1671 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1671[0] = (moonbit_string_t)moonbit_string_literal_184.data;
  _tmp$1670
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1670)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1670->$0 = _tmp$1671;
  _tmp$1670->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2$clo
  );
  _tuple$1669
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1669)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1669->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_2$clo;
  _tuple$1669->$1 = _tmp$1670;
  _tuple$1660
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1660)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1660->$0 = 2;
  _tuple$1660->$1 = _tuple$1669;
  _tmp$1668 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1668[0] = (moonbit_string_t)moonbit_string_literal_185.data;
  _tmp$1667
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1667)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1667->$0 = _tmp$1668;
  _tmp$1667->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3$clo
  );
  _tuple$1666
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1666)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1666->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_3$clo;
  _tuple$1666->$1 = _tmp$1667;
  _tuple$1661
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1661)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1661->$0 = 3;
  _tuple$1661->$1 = _tuple$1666;
  _tmp$1665 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1665[0] = (moonbit_string_t)moonbit_string_literal_186.data;
  _tmp$1664
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1664)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1664->$0 = _tmp$1665;
  _tmp$1664->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4$clo
  );
  _tuple$1663
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1663)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1663->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_746573742e6d6274_4$clo;
  _tuple$1663->$1 = _tmp$1664;
  _tuple$1662
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1662)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1662->$0 = 4;
  _tuple$1662->$1 = _tuple$1663;
  _bind$1421
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      5
    );
  _bind$1421[0] = _tuple$1658;
  _bind$1421[1] = _tuple$1659;
  _bind$1421[2] = _tuple$1660;
  _bind$1421[3] = _tuple$1661;
  _bind$1421[4] = _tuple$1662;
  _tmp$1657 = _bind$1421;
  _tmp$1656
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 5, _tmp$1657
  };
  _tmp$1655 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1656);
  _tuple$1626
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1626)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1626->$0 = (moonbit_string_t)moonbit_string_literal_187.data;
  _tuple$1626->$1 = _tmp$1655;
  _tmp$1654 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1654[0] = (moonbit_string_t)moonbit_string_literal_188.data;
  _tmp$1653
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1653)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1653->$0 = _tmp$1654;
  _tmp$1653->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0$clo
  );
  _tuple$1652
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1652)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1652->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_0$clo;
  _tuple$1652->$1 = _tmp$1653;
  _tuple$1635
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1635)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1635->$0 = 0;
  _tuple$1635->$1 = _tuple$1652;
  _tmp$1651 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1651[0] = (moonbit_string_t)moonbit_string_literal_189.data;
  _tmp$1650
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1650)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1650->$0 = _tmp$1651;
  _tmp$1650->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1$clo
  );
  _tuple$1649
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1649)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1649->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_1$clo;
  _tuple$1649->$1 = _tmp$1650;
  _tuple$1636
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1636)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1636->$0 = 1;
  _tuple$1636->$1 = _tuple$1649;
  _tmp$1648 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1648[0] = (moonbit_string_t)moonbit_string_literal_190.data;
  _tmp$1647
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1647)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1647->$0 = _tmp$1648;
  _tmp$1647->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2$clo
  );
  _tuple$1646
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1646)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1646->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_2$clo;
  _tuple$1646->$1 = _tmp$1647;
  _tuple$1637
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1637)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1637->$0 = 2;
  _tuple$1637->$1 = _tuple$1646;
  _tmp$1645 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1645[0] = (moonbit_string_t)moonbit_string_literal_191.data;
  _tmp$1644
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1644)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1644->$0 = _tmp$1645;
  _tmp$1644->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3$clo
  );
  _tuple$1643
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1643)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1643->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_3$clo;
  _tuple$1643->$1 = _tmp$1644;
  _tuple$1638
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1638)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1638->$0 = 3;
  _tuple$1638->$1 = _tuple$1643;
  _tmp$1642 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1642[0] = (moonbit_string_t)moonbit_string_literal_192.data;
  _tmp$1641
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1641)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1641->$0 = _tmp$1642;
  _tmp$1641->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4$clo
  );
  _tuple$1640
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1640)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1640->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$__test_617a696d7574685f656e68616e6365645f746573745f73756974652e6d6274_4$clo;
  _tuple$1640->$1 = _tmp$1641;
  _tuple$1639
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1639)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1639->$0 = 4;
  _tuple$1639->$1 = _tuple$1640;
  _bind$1422
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      5
    );
  _bind$1422[0] = _tuple$1635;
  _bind$1422[1] = _tuple$1636;
  _bind$1422[2] = _tuple$1637;
  _bind$1422[3] = _tuple$1638;
  _bind$1422[4] = _tuple$1639;
  _tmp$1634 = _bind$1422;
  _tmp$1633
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 5, _tmp$1634
  };
  _tmp$1632 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1633);
  _tuple$1627
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1627)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1627->$0 = (moonbit_string_t)moonbit_string_literal_193.data;
  _tuple$1627->$1 = _tmp$1632;
  _bind$1423
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1631 = _bind$1423;
  _tmp$1630
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1631
  };
  _tmp$1629 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1630);
  _tuple$1628
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1628)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1628->$0 = (moonbit_string_t)moonbit_string_literal_194.data;
  _tuple$1628->$1 = _tmp$1629;
  _bind$1420
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      3
    );
  _bind$1420[0] = _tuple$1626;
  _bind$1420[1] = _tuple$1627;
  _bind$1420[2] = _tuple$1628;
  _tmp$1625 = _bind$1420;
  _tmp$1624
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 3, _tmp$1625
  };
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1624
  );
  _bind$1425
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1679 = _bind$1425;
  _tmp$1678
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1679
  };
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1678
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1621;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1593;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1594;
  int32_t _len$1595;
  int32_t _i$1596;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1621
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1593
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1593)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1593->$0 = _tmp$1621;
  async_tests$1593->$1 = 0;
  _arr$1594
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1594);
  _len$1595 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1594);
  _i$1596 = 0;
  while (1) {
    if (_i$1596 < _len$1595) {
      struct $$3c$String$2a$Int$3e$* arg$1597;
      moonbit_string_t _field$4009;
      moonbit_string_t _tmp$1618;
      int32_t _field$4008;
      int32_t _cnt$4163;
      int32_t _tmp$1619;
      int32_t _tmp$1620;
      moonbit_incref(_arr$1594);
      arg$1597
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1594, _i$1596
      );
      _field$4009 = arg$1597->$0;
      _tmp$1618 = _field$4009;
      _field$4008 = arg$1597->$1;
      _cnt$4163 = Moonbit_object_header(arg$1597)->rc;
      if (_cnt$4163 > 1) {
        int32_t _new_cnt$4164;
        moonbit_incref(_tmp$1618);
        _new_cnt$4164 = _cnt$4163 - 1;
        Moonbit_object_header(arg$1597)->rc = _new_cnt$4164;
      } else if (_cnt$4163 == 1) {
        moonbit_free(arg$1597);
      }
      _tmp$1619 = _field$4008;
      moonbit_incref(async_tests$1593);
      $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_do_execute(
        async_tests$1593, _tmp$1618, _tmp$1619
      );
      _tmp$1620 = _i$1596 + 1;
      _i$1596 = _tmp$1620;
      continue;
    } else {
      moonbit_decref(_arr$1594);
    }
    break;
  }
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced$moonbit_test_driver_internal_run_async_tests(
    async_tests$1593
  );
  return 0;
}