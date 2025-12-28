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

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

struct $Baggage;

struct $TextMapCarrier;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $StringView;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $SpanContext;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

struct $UpDownCounter;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $HttpResponse;

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $Error$azimuth$telemetry$simple_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

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

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$3c$StringView$2a$StringView$3e$;

struct $LogRecord;

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

struct $InstrumentationScope;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$Int$3e$;

struct $$moonbitlang$core$builtin$Array$3c$String$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $Resource;

struct $AttributeValue$FloatValue;

struct $Option$3c$Option$3c$String$3e$$3e$$Some;

struct $Ref$3c$Int$3e$;

struct $AttributeValue$ArrayStringValue;

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $Option$3c$Int64$3e$$Some;

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $ContextKey$3c$String$3e$;

struct $HttpRequest;

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap;

struct $AttributeValue$IntValue;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$;

struct $Attributes;

struct $Counter;

struct $Context;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $AttributeValue$ArrayIntValue;

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

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Ok;

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $AttributeValue$StringValue;

struct $Gauge;

struct $Histogram;

struct $$3c$Unit$3e$$3d$$3e$Unit {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  
};

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $Baggage {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* $0;
  
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

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
};

struct $UpDownCounter {
  moonbit_string_t $0;
  moonbit_string_t $1;
  moonbit_string_t $2;
  
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

struct $Error$azimuth$telemetry$simple_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
  moonbit_string_t $0;
  
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

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
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

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
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

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $Resource {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* $0;
  
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

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
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

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572 {
  void* $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
};

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$ {
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t $0_0;
  void* $1;
  
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

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  moonbit_string_t $1;
  
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

struct $Attributes {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* $0;
  
};

struct $Counter {
  moonbit_string_t $0;
  moonbit_string_t $1;
  moonbit_string_t $2;
  
};

struct $Context {
  struct $$3c$String$2a$String$3e$* $0;
  
};

struct $Error$moonbitlang$core$builtin$Failure$Failure {
  moonbit_string_t $0;
  
};

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Ok {
  int32_t $0;
  
};

struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$moonbitlang$core$builtin$StringBuilder {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $AttributeValue$StringValue {
  moonbit_string_t $0;
  
};

struct $Gauge {
  moonbit_string_t $0;
  moonbit_string_t $1;
  moonbit_string_t $2;
  
};

struct $Histogram {
  moonbit_string_t $0;
  moonbit_string_t $1;
  moonbit_string_t $2;
  
};

struct moonbit_result_0 {
  int tag;
  union { int32_t ok; void* err;  } data;
  
};

struct moonbit_result_1 {
  int tag;
  union { struct $StringView ok; void* err;  } data;
  
};

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2959
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2958
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2957
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2956
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2955
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2954
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2953
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2952
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2951
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2950
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2949
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2948
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2947
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2946
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2945
);

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1374
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2919,
  moonbit_string_t s$1352,
  int32_t sep$1353
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1339
);

moonbit_string_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2828,
  moonbit_bytes_t bytes$1340
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$2821,
  moonbit_string_t s$1334
);

#define $azimuth$telemetry$simple_test$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1297,
  moonbit_string_t filename$1258,
  int32_t index$1259
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2814
);

struct moonbit_result_0 $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2810
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2808
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2792,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1298,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1299
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2803,
  int32_t _cont_param$1318
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2800,
  void* _cont_param$1319
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$2794,
  void* _state$1301
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2789,
  void* err$1281
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2775,
  moonbit_string_t attr$1274
);

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$2759,
  moonbit_string_t test_name$1261,
  moonbit_string_t file_name$1262,
  moonbit_string_t message$1263,
  int32_t skipped$1264
);

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1256
);

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1254,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1255,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1252
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$simple_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1215,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1228,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1241,
  moonbit_string_t file_filter$1212,
  int32_t index_filter$1213
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9(
  
);

int32_t $$azimuth$telemetry$simple_test$Random$$system();

uint64_t $$azimuth$telemetry$simple_test$Random$$next_u64(
  int32_t random$1195
);

int32_t $$azimuth$telemetry$simple_test$Clock$$system();

int64_t $$azimuth$telemetry$simple_test$Clock$$now_unix_nanos(
  int32_t clock$1194
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8(
  
);

int32_t $$azimuth$telemetry$simple_test$HttpResponse$$status_code(
  struct $HttpResponse* response$1189
);

struct $HttpResponse* $$azimuth$telemetry$simple_test$HttpResponse$$new(
  int32_t status_code$1187,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$1188,
  void* body$opt$1185
);

struct $HttpResponse* $$azimuth$telemetry$simple_test$HttpResponse$$new$inner(
  int32_t status_code$1181,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$1182,
  moonbit_string_t body$1183
);

moonbit_string_t $$azimuth$telemetry$simple_test$HttpResponse$$body(
  struct $HttpResponse* response$1180
);

moonbit_string_t $$azimuth$telemetry$simple_test$HttpRequest$$url(
  struct $HttpRequest* request$1179
);

struct $HttpRequest* $$azimuth$telemetry$simple_test$HttpRequest$$new(
  moonbit_string_t http_method$1176,
  moonbit_string_t url$1177,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$1178,
  void* body$opt$1174
);

struct $HttpRequest* $$azimuth$telemetry$simple_test$HttpRequest$$new$inner(
  moonbit_string_t http_method$1169,
  moonbit_string_t url$1170,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$1171,
  moonbit_string_t body$1172
);

moonbit_string_t $$azimuth$telemetry$simple_test$HttpRequest$$http_method(
  struct $HttpRequest* request$1168
);

moonbit_string_t $$azimuth$telemetry$simple_test$HttpRequest$$body(
  struct $HttpRequest* request$1167
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7(
  
);

int32_t $$azimuth$telemetry$simple_test$TextMapCarrier$$set(
  struct $TextMapCarrier* carrier$1160,
  moonbit_string_t key$1161,
  moonbit_string_t value$1162
);

struct $TextMapCarrier* $$azimuth$telemetry$simple_test$TextMapCarrier$$new();

moonbit_string_t $$azimuth$telemetry$simple_test$TextMapCarrier$$get(
  struct $TextMapCarrier* carrier$1159,
  moonbit_string_t key$1158
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6(
  
);

struct $Baggage* $$azimuth$telemetry$simple_test$Baggage$$set_entry(
  struct $Baggage* baggage$1152,
  moonbit_string_t key$1153,
  moonbit_string_t value$1154
);

struct $Baggage* $$azimuth$telemetry$simple_test$Baggage$$new();

moonbit_string_t $$azimuth$telemetry$simple_test$Baggage$$get_entry(
  struct $Baggage* baggage$1146,
  moonbit_string_t key$1150
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5(
  
);

int32_t $$azimuth$telemetry$simple_test$LogRecord$$severity_number(
  struct $LogRecord* record$1132
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4(
  
);

struct $Resource* $$azimuth$telemetry$simple_test$Resource$$with_attributes(
  struct $Resource* resource$1112,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* attributes$1111
);

struct $Resource* $$azimuth$telemetry$simple_test$Resource$$new();

void* $$azimuth$telemetry$simple_test$Resource$$get_attribute(
  struct $Resource* resource$1105,
  moonbit_string_t key$1109
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3(
  
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2(
  
);

struct $UpDownCounter* $$azimuth$telemetry$simple_test$Meter$$create_updown_counter(
  struct $Meter* meter$1086,
  moonbit_string_t name$1087,
  void* description$opt$1081,
  void* unit$opt$1084
);

struct $UpDownCounter* $$azimuth$telemetry$simple_test$Meter$$create_updown_counter$inner(
  struct $Meter* meter$1079,
  moonbit_string_t name$1076,
  moonbit_string_t description$1077,
  moonbit_string_t unit$1078
);

struct $Histogram* $$azimuth$telemetry$simple_test$Meter$$create_histogram(
  struct $Meter* meter$1074,
  moonbit_string_t name$1075,
  void* description$opt$1069,
  void* unit$opt$1072
);

struct $Histogram* $$azimuth$telemetry$simple_test$Meter$$create_histogram$inner(
  struct $Meter* meter$1067,
  moonbit_string_t name$1064,
  moonbit_string_t description$1065,
  moonbit_string_t unit$1066
);

struct $Gauge* $$azimuth$telemetry$simple_test$Meter$$create_gauge(
  struct $Meter* meter$1062,
  moonbit_string_t name$1063,
  void* description$opt$1057,
  void* unit$opt$1060
);

struct $Gauge* $$azimuth$telemetry$simple_test$Meter$$create_gauge$inner(
  struct $Meter* meter$1055,
  moonbit_string_t name$1052,
  moonbit_string_t description$1053,
  moonbit_string_t unit$1054
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1(
  
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0(
  
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4(
  
);

struct $LogRecord* $$azimuth$telemetry$simple_test$LogRecord$$new(
  int32_t severity$1039,
  moonbit_string_t body$1040
);

moonbit_string_t $$azimuth$telemetry$simple_test$LogRecord$$body(
  struct $LogRecord* record$1038
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3(
  
);

struct $Meter* $$azimuth$telemetry$simple_test$MeterProvider$$get_meter(
  int32_t provider$1034,
  moonbit_string_t name$1033
);

int32_t $$azimuth$telemetry$simple_test$MeterProvider$$default();

struct $Counter* $$azimuth$telemetry$simple_test$Meter$$create_counter(
  struct $Meter* meter$1031,
  moonbit_string_t name$1030
);

int32_t $$azimuth$telemetry$simple_test$Counter$$add(
  struct $Counter* counter$1028,
  double value$1029,
  void* attributes$opt$1026
);

int32_t $$azimuth$telemetry$simple_test$Counter$$add$inner(
  struct $Counter* counter$1022,
  double value$1023,
  struct $Attributes* attributes$1024
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2(
  
);

struct $ContextKey$3c$String$3e$* $$azimuth$telemetry$simple_test$ContextKey$$new(
  moonbit_string_t key$1017
);

struct $Context* $$azimuth$telemetry$simple_test$Context$$with_value(
  struct $Context* ctx$1016,
  struct $ContextKey$3c$String$3e$* key$1014,
  moonbit_string_t value$1015
);

struct $Context* $$azimuth$telemetry$simple_test$Context$$root();

moonbit_string_t $$azimuth$telemetry$simple_test$Context$$get(
  struct $Context* ctx$1008,
  struct $ContextKey$3c$String$3e$* key$1013
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1(
  
);

moonbit_string_t $$azimuth$telemetry$simple_test$SpanContext$$trace_id(
  struct $SpanContext* ctx$999
);

moonbit_string_t $$azimuth$telemetry$simple_test$SpanContext$$span_id(
  struct $SpanContext* ctx$998
);

struct $SpanContext* $$azimuth$telemetry$simple_test$SpanContext$$new(
  moonbit_string_t trace_id$994,
  moonbit_string_t span_id$995,
  int32_t sampled$996,
  moonbit_string_t trace_state$997
);

int32_t $$azimuth$telemetry$simple_test$SpanContext$$is_valid(
  struct $SpanContext* ctx$993
);

int32_t $$azimuth$telemetry$simple_test$SpanContext$$is_sampled(
  struct $SpanContext* ctx$992
);

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0(
  
);

int32_t $$azimuth$telemetry$simple_test$Attributes$$set(
  struct $Attributes* attrs$987,
  moonbit_string_t key$988,
  void* value$989
);

struct $Attributes* $$azimuth$telemetry$simple_test$Attributes$$new();

void* $$azimuth$telemetry$simple_test$Attributes$$get(
  struct $Attributes* attrs$986,
  moonbit_string_t key$985
);

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$984
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$982,
  struct $$moonbitlang$core$builtin$Logger logger$983
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$968,
  struct $$moonbitlang$core$builtin$Logger logger$981
);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$966);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$961,
  moonbit_string_t msg$963,
  moonbit_string_t loc$965
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$956,
  moonbit_string_t msg$958,
  moonbit_string_t loc$960
);

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$955,
  struct $$moonbitlang$core$builtin$Hasher* hasher$954
);

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$953,
  struct $$moonbitlang$core$builtin$Hasher* hasher$952
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$950,
  moonbit_string_t value$948
);

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$944,
  struct $$3c$String$3e$$3d$$3e$Int* f$946
);

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2478,
  moonbit_string_t k$945
);

struct $$3c$String$2a$String$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$3(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$942,
  int32_t idx$943
);

struct $$3c$String$2a$AttributeValue$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$2(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$940,
  int32_t idx$941
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$938,
  int32_t idx$939
);

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$936,
  int32_t idx$937
);

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$932,
  int32_t key$928
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$923,
  moonbit_string_t key$919
);

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$914,
  int32_t key$910
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$905,
  moonbit_string_t key$901
);

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$896,
  int32_t key$892
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$887,
  moonbit_string_t key$883
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$875
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$867
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$859
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$851
);

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$847,
  moonbit_string_t key$848,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$849
);

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$844,
  moonbit_string_t key$845,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$846
);

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$841,
  int32_t key$842,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$843
);

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$838,
  moonbit_string_t key$839,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$840
);

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$828
);

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$817
);

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$806
);

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$795
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$778,
  moonbit_string_t key$787,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$788,
  int32_t hash$786
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$762,
  moonbit_string_t key$771,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$772,
  int32_t hash$770
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$746,
  int32_t key$755,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$756,
  int32_t hash$754
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$730,
  moonbit_string_t key$739,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$740,
  int32_t hash$738
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$724,
  int32_t idx$729,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$728
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$714,
  int32_t idx$719,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$718
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$704,
  int32_t idx$709,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$708
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$694,
  int32_t idx$699,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$698
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$684,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$686,
  int32_t new_idx$685
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$678,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$680,
  int32_t new_idx$679
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$672,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$674,
  int32_t new_idx$673
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$666,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$668,
  int32_t new_idx$667
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$663,
  int32_t idx$665,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$664
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$659,
  int32_t idx$661,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$660
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$655,
  int32_t idx$657,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$656
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$651,
  int32_t idx$653,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$652
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$645
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$639
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$633
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$627
);

int32_t $Int$$next_power_of_two(int32_t self$625);

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$624);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$622
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$620
);

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$618
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$616
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  moonbit_string_t self$610,
  moonbit_string_t other$611
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$609
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$608
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$606
);

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$2129
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  moonbit_string_t self$601,
  struct $$moonbitlang$core$builtin$Logger logger$602
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$600
);

int32_t $$moonbitlang$core$builtin$Show$$UInt64$$output(
  uint64_t self$599,
  struct $$moonbitlang$core$builtin$Logger logger$598
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$597,
  struct $$moonbitlang$core$builtin$Logger logger$596
);

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$594,
  struct $$moonbitlang$core$builtin$Logger logger$595
);

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$592,
  struct $$3c$String$3e$$3d$$3e$Int* f$593
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$588,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$590
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$585,
  struct $$3c$String$2a$Int$3e$* value$587
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$582,
  moonbit_string_t value$584
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$580
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$577
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$574
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$570,
  int32_t new_capacity$568
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$564,
  int32_t new_capacity$562
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$558,
  int32_t new_capacity$556
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$554
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$552,
  struct $StringView str$553
);

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$549,
  int32_t i$550,
  int32_t start_offset$551,
  int64_t end_offset$547
);

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$544,
  int32_t n$542,
  int32_t start_offset$538,
  int32_t end_offset$539
);

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$536,
  int32_t n$534,
  int32_t start_offset$533,
  int32_t end_offset$532
);

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$522,
  int32_t len$525,
  int32_t start_offset$529,
  int64_t end_offset$520
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$518
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$517
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$516
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$515
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$514
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$509
);

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$2056,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$507
);

moonbit_string_t $$moonbitlang$core$builtin$Iterator$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$506
);

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$497,
  struct $$moonbitlang$core$builtin$Logger logger$495
);

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$491,
  int32_t seg$494,
  int32_t i$493
);

moonbit_string_t $Byte$$to_hex(int32_t b$489);

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$487);

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$485,
  int32_t that$486
);

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$483,
  int32_t that$484
);

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$481,
  int32_t that$482
);

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$479,
  int32_t that$480
);

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$476,
  int32_t start$474,
  int32_t end$475
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$473
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  uint64_t a$467,
  uint64_t b$468,
  moonbit_string_t msg$470,
  moonbit_string_t loc$472
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  int32_t a$461,
  int32_t b$462,
  moonbit_string_t msg$464,
  moonbit_string_t loc$466
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  int32_t a$455,
  int32_t b$456,
  moonbit_string_t msg$458,
  moonbit_string_t loc$460
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  moonbit_string_t a$449,
  moonbit_string_t b$450,
  moonbit_string_t msg$452,
  moonbit_string_t loc$454
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  moonbit_string_t a$443,
  moonbit_string_t b$444,
  moonbit_string_t msg$446,
  moonbit_string_t loc$448
);

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$442,
  moonbit_string_t loc$441
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(uint64_t t$440);

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(int32_t t$438);

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(int32_t t$436);

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(
  moonbit_string_t t$434
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(
  moonbit_string_t t$432
);

moonbit_string_t $UInt64$$to_string$inner(
  uint64_t self$423,
  int32_t radix$422
);

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$412,
  uint64_t num$400,
  int32_t digit_start$403,
  int32_t total_len$402
);

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$394,
  uint64_t num$388,
  int32_t digit_start$386,
  int32_t total_len$385,
  int32_t radix$390
);

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$381,
  uint64_t num$377,
  int32_t digit_start$375,
  int32_t total_len$374
);

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$367,
  int32_t radix$370
);

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$365);

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$364);

moonbit_string_t $Int$$to_string$inner(int32_t self$348, int32_t radix$347);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$341,
  int32_t radix$344
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$339);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$338);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$328,
  uint32_t num$316,
  int32_t digit_start$319,
  int32_t total_len$318
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$310,
  uint32_t num$304,
  int32_t digit_start$302,
  int32_t total_len$301,
  int32_t radix$306
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$297,
  uint32_t num$293,
  int32_t digit_start$291,
  int32_t total_len$290
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$288
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$286
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$284
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$282
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$280
);

int32_t $StringView$$start_offset(struct $StringView self$278);

moonbit_string_t $StringView$$data(struct $StringView self$277);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$271,
  moonbit_string_t value$274,
  int32_t start$275,
  int32_t len$276
);

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$264,
  int32_t start$270,
  int64_t end$266
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$262
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$260
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$257
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$255
);

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$254
);

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$253
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$251,
  int32_t value$250
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$249,
  moonbit_string_t value$248
);

uint64_t $Int$$to_uint64(int32_t self$247);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$245,
  int32_t value$246
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$243,
  uint32_t value$244
);

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$241,
  uint32_t input$242
);

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$239, int32_t r$240);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$237,
  moonbit_string_t str$238
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$229,
  int32_t bytes_offset$224,
  moonbit_string_t str$231,
  int32_t str_offset$227,
  int32_t length$225
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$191
);

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$187
);

struct $StringView $StringView$$view$inner(
  struct $StringView self$184,
  int32_t start_offset$185,
  int64_t end_offset$182
);

int64_t $StringView$$rev_find(
  struct $StringView self$180,
  struct $StringView str$179
);

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$170,
  struct $StringView needle$172
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$159,
  struct $StringView needle$161
);

int64_t $StringView$$find(
  struct $StringView self$157,
  struct $StringView str$156
);

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$146,
  struct $StringView needle$148
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$132,
  struct $StringView needle$134
);

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$128,
  int32_t index$129
);

int32_t $StringView$$length(struct $StringView self$127);

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$125,
  int32_t index$126
);

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$123
);

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$122
);

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$121
);

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$120
);

struct $$3c$String$2a$String$3e$** $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$119
);

struct $$3c$String$2a$AttributeValue$3e$** $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$118
);

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$117
);

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$116
);

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$115
);

moonbit_string_t $String$$escape(moonbit_string_t self$114);

int32_t $moonbitlang$core$builtin$op_notequal$3(int32_t x$111, int32_t y$112);

int32_t $moonbitlang$core$builtin$op_notequal$2(
  moonbit_string_t x$109,
  moonbit_string_t y$110
);

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

moonbit_string_t $Error$to_string(void* _e$1381);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1399,
  int32_t _param$1398
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1396,
  struct $StringView _param$1395
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1393,
  moonbit_string_t _param$1390,
  int32_t _param$1391,
  int32_t _param$1392
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1388,
  moonbit_string_t _param$1387
);

struct { int32_t rc; uint32_t meta; uint16_t const data[56]; 
} const moonbit_string_literal_34 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 55), 
    48, 48, 45, 48, 97, 102, 55, 54, 53, 49, 57, 49, 54, 99, 100, 52, 
    51, 100, 100, 56, 52, 52, 56, 101, 98, 50, 49, 49, 99, 56, 48, 51, 
    49, 57, 99, 45, 98, 55, 97, 100, 54, 98, 55, 49, 54, 57, 50, 48, 
    51, 51, 51, 49, 45, 48, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[29]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 28), 
    104, 116, 116, 112, 115, 58, 47, 47, 97, 112, 105, 46, 101, 120, 
    97, 109, 112, 108, 101, 46, 99, 111, 109, 47, 100, 97, 116, 97, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[1]; 
} const moonbit_string_literal_3 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 0), 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_79 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 54, 54, 58, 
    49, 48, 45, 54, 54, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_73 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_60 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 51, 48, 
    58, 49, 48, 45, 49, 51, 48, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_82 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    99, 97, 115, 99, 97, 100, 101, 95, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_156 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_122 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 
    98, 105, 116, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 
    49, 49, 58, 51, 45, 49, 49, 58, 50, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_135 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_51 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 48, 57, 
    58, 49, 52, 45, 49, 48, 57, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_70 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 56, 57, 58, 
    49, 48, 45, 56, 57, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_54 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 49, 53, 
    58, 49, 48, 45, 49, 49, 53, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[24]; 
} const moonbit_string_literal_171 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 23), 
    98, 97, 103, 103, 97, 103, 101, 32, 101, 110, 116, 114, 121, 32, 
    108, 105, 102, 101, 99, 121, 99, 108, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 57, 55, 
    58, 51, 45, 49, 57, 55, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_126 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    78, 111, 110, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_144 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_62 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 51, 53, 
    58, 49, 48, 45, 49, 51, 53, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[28]; 
} const moonbit_string_literal_172 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 27), 
    116, 101, 120, 116, 32, 109, 97, 112, 32, 99, 97, 114, 114, 105, 
    101, 114, 32, 111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_115 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 
    98, 105, 116, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 
    51, 49, 58, 51, 45, 51, 49, 58, 52, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[65]; 
} const moonbit_string_literal_107 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 64), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 51, 58, 
    51, 45, 49, 51, 58, 53, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    98, 97, 103, 103, 97, 103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_27 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 55, 57, 
    58, 51, 45, 49, 55, 57, 58, 53, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_117 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    98, 55, 97, 100, 54, 98, 55, 49, 54, 57, 50, 48, 51, 51, 51, 49, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_35 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 54, 51, 
    58, 51, 45, 49, 54, 51, 58, 57, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_108 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    84, 101, 115, 116, 32, 108, 111, 103, 32, 109, 101, 115, 115, 97, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[54]; 
} const moonbit_string_literal_148 =
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
} const moonbit_string_literal_176 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    97, 122, 105, 109, 117, 116, 104, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_146 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_128 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 41, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 57, 54, 
    58, 51, 45, 49, 57, 54, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_64 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    115, 101, 114, 118, 105, 99, 101, 46, 110, 97, 109, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    49, 50, 51, 52, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_127 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    83, 111, 109, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[96]; 
} const moonbit_string_literal_158 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 95), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 115, 
    116, 46, 77, 111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 68, 
    114, 105, 118, 101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 74, 
    115, 69, 114, 114, 111, 114, 46, 77, 111, 111, 110, 66, 105, 116, 
    84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 73, 110, 116, 101, 
    114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_81 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 55, 49, 58, 
    49, 48, 45, 55, 49, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_0 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 58, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_143 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 54, 54, 51, 
    58, 53, 45, 54, 54, 51, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_132 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 51, 54, 54, 58, 53, 45, 
    51, 54, 54, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_116 =
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
} const moonbit_string_literal_114 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    116, 101, 115, 116, 95, 118, 97, 108, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_30 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    48, 48, 45, 116, 114, 97, 99, 101, 45, 115, 112, 97, 110, 45, 102, 
    108, 97, 103, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_110 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    116, 101, 115, 116, 45, 109, 101, 116, 101, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_125 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 102, 97, 108, 115, 101, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_48 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    87, 97, 114, 110, 105, 110, 103, 32, 109, 101, 115, 115, 97, 103, 
    101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_138 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_136 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_124 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_95 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    118, 97, 108, 117, 101, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[65]; 
} const moonbit_string_literal_87 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 64), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 52, 51, 58, 
    51, 45, 52, 51, 58, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_1 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 45, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_57 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 50, 52, 
    58, 49, 51, 45, 49, 50, 52, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_91 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    107, 101, 121, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_42 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 52, 56, 
    58, 51, 45, 49, 52, 56, 58, 54, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_77 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 54, 49, 58, 
    49, 48, 45, 54, 49, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_45 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    84, 114, 97, 99, 101, 32, 109, 101, 115, 115, 97, 103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_109 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 
    98, 105, 116, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 
    53, 50, 58, 51, 45, 53, 50, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_71 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 57, 51, 58, 
    51, 55, 45, 57, 51, 58, 55, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_22 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    80, 79, 83, 84, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[63]; 
} const moonbit_string_literal_105 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 62), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 57, 58, 51, 
    45, 57, 58, 53, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_56 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 50, 48, 
    58, 49, 48, 45, 49, 50, 48, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_43 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    110, 111, 110, 101, 120, 105, 115, 116, 101, 110, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    116, 114, 97, 99, 101, 112, 97, 114, 101, 110, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_173 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    104, 116, 116, 112, 32, 99, 108, 105, 101, 110, 116, 32, 114, 101, 
    113, 117, 101, 115, 116, 32, 114, 101, 115, 112, 111, 110, 115, 101, 
    32, 99, 121, 99, 108, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_52 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 49, 48, 
    58, 49, 48, 45, 49, 49, 48, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_37 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    117, 115, 101, 114, 46, 105, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[25]; 
} const moonbit_string_literal_161 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 24), 
    99, 111, 110, 116, 101, 120, 116, 32, 118, 97, 108, 117, 101, 32, 
    111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[28]; 
} const moonbit_string_literal_159 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 27), 
    97, 116, 116, 114, 105, 98, 117, 116, 101, 115, 32, 98, 97, 115, 
    105, 99, 32, 111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[65]; 
} const moonbit_string_literal_106 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 64), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 48, 58, 
    51, 45, 49, 48, 58, 53, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_131 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    73, 110, 118, 97, 108, 105, 100, 32, 115, 116, 97, 114, 116, 32, 
    105, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_92 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    107, 101, 121, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_36 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 54, 52, 
    58, 51, 45, 49, 54, 52, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_72 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 57, 52, 58, 
    49, 48, 45, 57, 52, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_25 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 55, 51, 
    58, 51, 45, 49, 55, 51, 58, 55, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_76 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 54, 48, 58, 
    50, 57, 45, 54, 48, 58, 52, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_75 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    109, 105, 115, 115, 105, 110, 103, 46, 107, 101, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_59 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 50, 57, 
    58, 49, 52, 45, 49, 50, 57, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    66, 101, 97, 114, 101, 114, 32, 116, 111, 107, 101, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_15 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 57, 53, 
    58, 51, 45, 49, 57, 53, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[63]; 
} const moonbit_string_literal_104 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 62), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 56, 58, 51, 
    45, 56, 58, 53, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[32]; 
} const moonbit_string_literal_170 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 31), 
    108, 111, 103, 32, 114, 101, 99, 111, 114, 100, 32, 115, 101, 118, 
    101, 114, 105, 116, 121, 32, 112, 114, 111, 103, 114, 101, 115, 115, 
    105, 111, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 55, 50, 
    58, 51, 45, 49, 55, 50, 58, 53, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_140 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    32, 33, 61, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_65 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    112, 114, 111, 100, 117, 99, 116, 105, 111, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_166 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    99, 111, 110, 116, 101, 120, 116, 32, 118, 97, 108, 117, 101, 32, 
    99, 104, 97, 105, 110, 32, 111, 112, 101, 114, 97, 116, 105, 111, 
    110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[25]; 
} const moonbit_string_literal_164 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 24), 
    115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 98, 105, 116, 
    95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_4 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_134 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 52, 57, 49, 58, 57, 45, 
    52, 57, 49, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[65]; 
} const moonbit_string_literal_97 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 64), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 50, 56, 58, 
    51, 45, 50, 56, 58, 53, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_47 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    73, 110, 102, 111, 32, 109, 101, 115, 115, 97, 103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_133 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_151 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_102 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    115, 116, 97, 116, 101, 61, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_84 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    116, 101, 115, 116, 46, 104, 105, 115, 116, 111, 103, 114, 97, 109, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_163 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    108, 111, 103, 32, 114, 101, 99, 111, 114, 100, 32, 99, 114, 101, 
    97, 116, 105, 111, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_142 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_141 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    32, 70, 65, 73, 76, 69, 68, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 55, 52, 
    58, 51, 45, 49, 55, 52, 58, 52, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_130 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    102, 97, 108, 115, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[65]; 
} const moonbit_string_literal_98 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 64), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 50, 57, 58, 
    51, 45, 50, 57, 58, 53, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_154 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    73, 110, 100, 101, 120, 79, 117, 116, 79, 102, 66, 111, 117, 110, 
    100, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_121 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 
    98, 105, 116, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 
    50, 50, 58, 51, 45, 50, 50, 58, 52, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_5 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 32, 98, 
    108, 111, 99, 107, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_86 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    116, 101, 115, 116, 46, 103, 97, 117, 103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_49 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    69, 114, 114, 111, 114, 32, 109, 101, 115, 115, 97, 103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_46 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    68, 101, 98, 117, 103, 32, 109, 101, 115, 115, 97, 103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[65]; 
} const moonbit_string_literal_88 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 64), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 52, 52, 58, 
    51, 45, 52, 52, 58, 52, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_145 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_74 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    105, 110, 116, 46, 107, 101, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_44 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 52, 57, 
    58, 51, 45, 49, 52, 57, 58, 54, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_157 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 73, 110, 115, 
    112, 101, 99, 116, 69, 114, 114, 111, 114, 46, 73, 110, 115, 112, 
    101, 99, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_152 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 10, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_13 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    45, 45, 45, 45, 45, 32, 69, 78, 68, 32, 77, 79, 79, 78, 32, 84, 69, 
    83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_93 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    107, 101, 121, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_53 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 49, 52, 
    58, 49, 52, 45, 49, 49, 52, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_61 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 51, 52, 
    58, 49, 52, 45, 49, 51, 52, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_111 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 
    98, 105, 116, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 
    51, 57, 58, 51, 45, 51, 57, 58, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_50 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    70, 97, 116, 97, 108, 32, 109, 101, 115, 115, 97, 103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[27]; 
} const moonbit_string_literal_162 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 26), 
    109, 101, 116, 114, 105, 99, 115, 32, 99, 111, 117, 110, 116, 101, 
    114, 32, 111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    97, 98, 99, 100, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[11]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 10), 
    115, 101, 115, 115, 105, 111, 110, 46, 105, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[24]; 
} const moonbit_string_literal_32 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 23), 
    107, 101, 121, 49, 61, 118, 97, 108, 117, 101, 49, 44, 107, 101, 
    121, 50, 61, 118, 97, 108, 117, 101, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_153 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    109, 105, 115, 115, 105, 110, 103, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_120 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 
    98, 105, 116, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 
    50, 49, 58, 51, 45, 50, 49, 58, 52, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_67 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    50, 46, 48, 46, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_174 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    99, 108, 111, 99, 107, 32, 114, 97, 110, 100, 111, 109, 32, 116, 
    101, 109, 112, 111, 114, 97, 108, 32, 115, 101, 113, 117, 101, 110, 
    99, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 52, 55, 
    58, 51, 45, 49, 52, 55, 58, 54, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    45, 45, 45, 45, 45, 32, 66, 69, 71, 73, 78, 32, 77, 79, 79, 78, 32, 
    84, 69, 83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_100 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    116, 114, 97, 99, 101, 49, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_69 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 56, 56, 58, 
    51, 50, 45, 56, 56, 58, 55, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_112 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 
    98, 105, 116, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 
    52, 52, 58, 51, 45, 52, 52, 58, 50, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_94 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    118, 97, 108, 117, 101, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_85 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    116, 101, 115, 116, 46, 117, 112, 100, 111, 119, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    97, 112, 112, 108, 105, 99, 97, 116, 105, 111, 110, 47, 106, 115, 
    111, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_83 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    116, 101, 115, 116, 46, 99, 111, 117, 110, 116, 101, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_150 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 48, 58, 53, 45, 50, 
    57, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[63]; 
} const moonbit_string_literal_103 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 62), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 55, 58, 51, 
    45, 55, 58, 53, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_101 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    115, 112, 97, 110, 52, 53, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_147 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    73, 110, 118, 97, 108, 105, 100, 32, 105, 110, 100, 101, 120, 32, 
    102, 111, 114, 32, 86, 105, 101, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[65]; 
} const moonbit_string_literal_90 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 64), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 52, 54, 58, 
    51, 45, 52, 54, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_28 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 56, 48, 
    58, 51, 45, 49, 56, 48, 58, 52, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_78 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 54, 53, 58, 
    50, 54, 45, 54, 53, 58, 52, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[36]; 
} const moonbit_string_literal_167 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 35), 
    109, 101, 116, 114, 105, 99, 115, 32, 105, 110, 115, 116, 114, 117, 
    109, 101, 110, 116, 32, 99, 114, 101, 97, 116, 105, 111, 110, 32, 
    99, 97, 115, 99, 97, 100, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[29]; 
} const moonbit_string_literal_169 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 28), 
    114, 101, 115, 111, 117, 114, 99, 101, 32, 97, 116, 116, 114, 105, 
    98, 117, 116, 101, 32, 104, 105, 101, 114, 97, 114, 99, 104, 121, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_118 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 
    98, 105, 116, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 
    49, 57, 58, 51, 45, 49, 57, 58, 53, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_20 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    65, 117, 116, 104, 111, 114, 105, 122, 97, 116, 105, 111, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_123 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 96, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_149 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_139 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    92, 117, 123, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_63 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    104, 105, 101, 114, 97, 114, 99, 104, 105, 99, 97, 108, 45, 115, 
    101, 114, 118, 105, 99, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_113 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    116, 101, 115, 116, 46, 107, 101, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_55 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 49, 57, 
    58, 49, 51, 45, 49, 49, 57, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_175 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 121, 95, 116, 
    101, 115, 116, 115, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_129 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_137 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[65]; 
} const moonbit_string_literal_89 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 64), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 52, 53, 58, 
    51, 45, 52, 53, 58, 52, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[33]; 
} const moonbit_string_literal_165 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 32), 
    115, 112, 97, 110, 32, 99, 111, 110, 116, 101, 120, 116, 32, 97, 
    100, 118, 97, 110, 99, 101, 100, 32, 118, 97, 108, 105, 100, 97, 
    116, 105, 111, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[66]; 
} const moonbit_string_literal_80 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 65), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 55, 48, 58, 
    49, 51, 45, 55, 48, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[58]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 57), 
    123, 34, 112, 97, 99, 107, 97, 103, 101, 34, 58, 32, 34, 97, 122, 
    105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 
    121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 115, 116, 34, 
    44, 32, 34, 102, 105, 108, 101, 110, 97, 109, 101, 34, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[65]; 
} const moonbit_string_literal_99 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 64), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 51, 48, 58, 
    51, 45, 51, 48, 58, 53, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[30]; 
} const moonbit_string_literal_168 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 29), 
    97, 116, 116, 114, 105, 98, 117, 116, 101, 32, 118, 97, 108, 117, 
    101, 32, 116, 121, 112, 101, 32, 115, 112, 101, 99, 116, 114, 117, 
    109, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[68]; 
} const moonbit_string_literal_58 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 67), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 50, 53, 
    58, 49, 48, 45, 49, 50, 53, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[24]; 
} const moonbit_string_literal_160 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 23), 
    115, 112, 97, 110, 32, 99, 111, 110, 116, 101, 120, 116, 32, 118, 
    97, 108, 105, 100, 97, 116, 105, 111, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_66 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    115, 101, 114, 118, 105, 99, 101, 46, 110, 97, 109, 101, 115, 112, 
    97, 99, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_119 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 115, 105, 109, 112, 108, 101, 95, 109, 111, 111, 110, 
    98, 105, 116, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 
    50, 48, 58, 51, 45, 50, 48, 58, 53, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[16]; 
} const moonbit_string_literal_68 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 15), 
    115, 101, 114, 118, 105, 99, 101, 46, 118, 101, 114, 115, 105, 111, 
    110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_155 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    73, 110, 118, 97, 108, 105, 100, 73, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_96 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    118, 97, 108, 117, 101, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    67, 111, 110, 116, 101, 110, 116, 45, 84, 121, 112, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[67]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 66), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 115, 105, 109, 112, 108, 101, 95, 116, 101, 
    115, 116, 58, 104, 105, 103, 104, 95, 113, 117, 97, 108, 105, 116, 
    121, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 58, 49, 57, 52, 
    58, 51, 45, 49, 57, 52, 58, 51, 49, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0$dyncall
  };

struct { int32_t rc; uint32_t meta; struct $$3c$String$3e$$3d$$3e$Int data; 
} const $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$5
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4$dyncall
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

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$130;

int64_t $moonbitlang$core$builtin$brute_force_find$constr$144;

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$simple_test$moonbit_test_driver_internal_async_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$simple_test$moonbit_test_driver_internal_no_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$simple_test$moonbit_test_driver_internal_with_args_tests;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1$clo;

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2959
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2958
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2957
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2956
) {
  return $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2955
) {
  return $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2954
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2953
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2952
) {
  return $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2951
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2950
) {
  return $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2949
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2948
) {
  return $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2947
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2946
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7();
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2945
) {
  return $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2();
}

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1374
) {
  moonbit_decref(_tests$1374);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$1333 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1339 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$1346 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1339;
  int32_t moonbit_test_driver_internal_split_mbt_string$1351 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$2944 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$1358 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$1359;
  moonbit_string_t _tmp$2943;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$1360;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$1361;
  int32_t _len$1362;
  int32_t _i$1363;
  Moonbit_object_header(file_and_index$1358)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$1358->$0 = _tmp$2944;
  file_and_index$1358->$1 = 0;
  cli_args$1359
  = $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$1346
  );
  _tmp$2943 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$1359, 1);
  test_args$1360
  = $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$1351, _tmp$2943, 47
  );
  _arr$1361 = test_args$1360;
  moonbit_incref(_arr$1361);
  _len$1362 = $$moonbitlang$core$builtin$Array$$length$1(_arr$1361);
  _i$1363 = 0;
  while (1) {
    if (_i$1363 < _len$1362) {
      moonbit_string_t arg$1364;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$1365;
      moonbit_string_t file$1366;
      moonbit_string_t range$1367;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1368;
      moonbit_string_t _tmp$2941;
      int32_t start$1369;
      moonbit_string_t _tmp$2940;
      int32_t end$1370;
      int32_t i$1371;
      int32_t _tmp$2942;
      moonbit_incref(_arr$1361);
      arg$1364
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$1361, _i$1363
      );
      file_and_range$1365
      = $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1351, arg$1364, 58
      );
      moonbit_incref(file_and_range$1365);
      file$1366
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1365, 0
      );
      range$1367
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1365, 1
      );
      start_and_end$1368
      = $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1351, range$1367, 45
      );
      moonbit_incref(start_and_end$1368);
      _tmp$2941
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1368, 0
      );
      start$1369
      = $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1333, _tmp$2941
      );
      _tmp$2940
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1368, 1
      );
      end$1370
      = $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1333, _tmp$2940
      );
      i$1371 = start$1369;
      while (1) {
        if (i$1371 < end$1370) {
          struct $$3c$String$2a$Int$3e$* _tuple$2938;
          int32_t _tmp$2939;
          moonbit_incref(file$1366);
          _tuple$2938
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$2938)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$2938->$0 = file$1366;
          _tuple$2938->$1 = i$1371;
          moonbit_incref(file_and_index$1358);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$1358, _tuple$2938
          );
          _tmp$2939 = i$1371 + 1;
          i$1371 = _tmp$2939;
          continue;
        } else {
          moonbit_decref(file$1366);
        }
        break;
      }
      _tmp$2942 = _i$1363 + 1;
      _i$1363 = _tmp$2942;
      continue;
    } else {
      moonbit_decref(_arr$1361);
    }
    break;
  }
  return file_and_index$1358;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2919,
  moonbit_string_t s$1352,
  int32_t sep$1353
) {
  moonbit_string_t* _tmp$2937 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1354 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$1355;
  struct $Ref$3c$Int$3e$* start$1356;
  int32_t val$2932;
  int32_t _tmp$2933;
  Moonbit_object_header(res$1354)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$1354->$0 = _tmp$2937;
  res$1354->$1 = 0;
  i$1355
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1355)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1355->$0 = 0;
  start$1356
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$1356)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$1356->$0 = 0;
  while (1) {
    int32_t val$2920 = i$1355->$0;
    int32_t _tmp$2921 = Moonbit_array_length(s$1352);
    if (val$2920 < _tmp$2921) {
      int32_t val$2924 = i$1355->$0;
      int32_t _tmp$2923;
      int32_t _tmp$2922;
      int32_t val$2931;
      int32_t _tmp$2930;
      if (val$2924 < 0 || val$2924 >= Moonbit_array_length(s$1352)) {
        moonbit_panic();
      }
      _tmp$2923 = s$1352[val$2924];
      _tmp$2922 = _tmp$2923;
      if (_tmp$2922 == sep$1353) {
        int32_t val$2926 = start$1356->$0;
        int32_t val$2927 = i$1355->$0;
        moonbit_string_t _tmp$2925;
        int32_t val$2929;
        int32_t _tmp$2928;
        moonbit_incref(s$1352);
        _tmp$2925 = $String$$unsafe_substring(s$1352, val$2926, val$2927);
        moonbit_incref(res$1354);
        $$moonbitlang$core$builtin$Array$$push$0(res$1354, _tmp$2925);
        val$2929 = i$1355->$0;
        _tmp$2928 = val$2929 + 1;
        start$1356->$0 = _tmp$2928;
      }
      val$2931 = i$1355->$0;
      _tmp$2930 = val$2931 + 1;
      i$1355->$0 = _tmp$2930;
      continue;
    } else {
      moonbit_decref(i$1355);
    }
    break;
  }
  val$2932 = start$1356->$0;
  _tmp$2933 = Moonbit_array_length(s$1352);
  if (val$2932 < _tmp$2933) {
    int32_t _field$2960 = start$1356->$0;
    int32_t val$2935;
    int32_t _tmp$2936;
    moonbit_string_t _tmp$2934;
    moonbit_decref(start$1356);
    val$2935 = _field$2960;
    _tmp$2936 = Moonbit_array_length(s$1352);
    _tmp$2934 = $String$$unsafe_substring(s$1352, val$2935, _tmp$2936);
    moonbit_incref(res$1354);
    $$moonbitlang$core$builtin$Array$$push$0(res$1354, _tmp$2934);
  } else {
    moonbit_decref(start$1356);
    moonbit_decref(s$1352);
  }
  return res$1354;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1339
) {
  moonbit_bytes_t* tmp$1347 =
    $azimuth$telemetry$simple_test$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$2918 = Moonbit_array_length(tmp$1347);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1348 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$2918);
  int32_t i$1349 = 0;
  while (1) {
    int32_t _tmp$2914 = Moonbit_array_length(tmp$1347);
    if (i$1349 < _tmp$2914) {
      moonbit_bytes_t _tmp$2961;
      moonbit_bytes_t _tmp$2916;
      moonbit_string_t _tmp$2915;
      int32_t _tmp$2917;
      if (i$1349 < 0 || i$1349 >= Moonbit_array_length(tmp$1347)) {
        moonbit_panic();
      }
      _tmp$2961 = (moonbit_bytes_t)tmp$1347[i$1349];
      _tmp$2916 = _tmp$2961;
      moonbit_incref(_tmp$2916);
      _tmp$2915
      = $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1339, _tmp$2916
      );
      moonbit_incref(res$1348);
      $$moonbitlang$core$builtin$Array$$push$0(res$1348, _tmp$2915);
      _tmp$2917 = i$1349 + 1;
      i$1349 = _tmp$2917;
      continue;
    } else {
      moonbit_decref(tmp$1347);
    }
    break;
  }
  return res$1348;
}

moonbit_string_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2828,
  moonbit_bytes_t bytes$1340
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$1341 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$1342 = Moonbit_array_length(bytes$1340);
  struct $Ref$3c$Int$3e$* i$1343 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1343)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1343->$0 = 0;
  while (1) {
    int32_t val$2829 = i$1343->$0;
    if (val$2829 < len$1342) {
      int32_t val$2913 = i$1343->$0;
      int32_t _tmp$2912;
      int32_t _tmp$2911;
      struct $Ref$3c$Int$3e$* c$1344;
      int32_t val$2830;
      if (val$2913 < 0 || val$2913 >= Moonbit_array_length(bytes$1340)) {
        moonbit_panic();
      }
      _tmp$2912 = bytes$1340[val$2913];
      _tmp$2911 = (int32_t)_tmp$2912;
      c$1344
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$1344)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$1344->$0 = _tmp$2911;
      val$2830 = c$1344->$0;
      if (val$2830 < 128) {
        int32_t _field$2962 = c$1344->$0;
        int32_t val$2832;
        int32_t _tmp$2831;
        int32_t val$2834;
        int32_t _tmp$2833;
        moonbit_decref(c$1344);
        val$2832 = _field$2962;
        _tmp$2831 = val$2832;
        moonbit_incref(res$1341);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$1341, _tmp$2831
        );
        val$2834 = i$1343->$0;
        _tmp$2833 = val$2834 + 1;
        i$1343->$0 = _tmp$2833;
      } else {
        int32_t val$2835 = c$1344->$0;
        if (val$2835 < 224) {
          int32_t val$2837 = i$1343->$0;
          int32_t _tmp$2836 = val$2837 + 1;
          int32_t val$2846;
          int32_t _tmp$2845;
          int32_t _tmp$2839;
          int32_t val$2844;
          int32_t _tmp$2843;
          int32_t _tmp$2842;
          int32_t _tmp$2841;
          int32_t _tmp$2840;
          int32_t _tmp$2838;
          int32_t _field$2963;
          int32_t val$2848;
          int32_t _tmp$2847;
          int32_t val$2850;
          int32_t _tmp$2849;
          if (_tmp$2836 >= len$1342) {
            moonbit_decref(c$1344);
            moonbit_decref(i$1343);
            moonbit_decref(bytes$1340);
            break;
          }
          val$2846 = c$1344->$0;
          _tmp$2845 = val$2846 & 31;
          _tmp$2839 = _tmp$2845 << 6;
          val$2844 = i$1343->$0;
          _tmp$2843 = val$2844 + 1;
          if (_tmp$2843 < 0 || _tmp$2843 >= Moonbit_array_length(bytes$1340)) {
            moonbit_panic();
          }
          _tmp$2842 = bytes$1340[_tmp$2843];
          _tmp$2841 = (int32_t)_tmp$2842;
          _tmp$2840 = _tmp$2841 & 63;
          _tmp$2838 = _tmp$2839 | _tmp$2840;
          c$1344->$0 = _tmp$2838;
          _field$2963 = c$1344->$0;
          moonbit_decref(c$1344);
          val$2848 = _field$2963;
          _tmp$2847 = val$2848;
          moonbit_incref(res$1341);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$1341, _tmp$2847
          );
          val$2850 = i$1343->$0;
          _tmp$2849 = val$2850 + 2;
          i$1343->$0 = _tmp$2849;
        } else {
          int32_t val$2851 = c$1344->$0;
          if (val$2851 < 240) {
            int32_t val$2853 = i$1343->$0;
            int32_t _tmp$2852 = val$2853 + 2;
            int32_t val$2869;
            int32_t _tmp$2868;
            int32_t _tmp$2861;
            int32_t val$2867;
            int32_t _tmp$2866;
            int32_t _tmp$2865;
            int32_t _tmp$2864;
            int32_t _tmp$2863;
            int32_t _tmp$2862;
            int32_t _tmp$2855;
            int32_t val$2860;
            int32_t _tmp$2859;
            int32_t _tmp$2858;
            int32_t _tmp$2857;
            int32_t _tmp$2856;
            int32_t _tmp$2854;
            int32_t _field$2964;
            int32_t val$2871;
            int32_t _tmp$2870;
            int32_t val$2873;
            int32_t _tmp$2872;
            if (_tmp$2852 >= len$1342) {
              moonbit_decref(c$1344);
              moonbit_decref(i$1343);
              moonbit_decref(bytes$1340);
              break;
            }
            val$2869 = c$1344->$0;
            _tmp$2868 = val$2869 & 15;
            _tmp$2861 = _tmp$2868 << 12;
            val$2867 = i$1343->$0;
            _tmp$2866 = val$2867 + 1;
            if (
              _tmp$2866 < 0 || _tmp$2866 >= Moonbit_array_length(bytes$1340)
            ) {
              moonbit_panic();
            }
            _tmp$2865 = bytes$1340[_tmp$2866];
            _tmp$2864 = (int32_t)_tmp$2865;
            _tmp$2863 = _tmp$2864 & 63;
            _tmp$2862 = _tmp$2863 << 6;
            _tmp$2855 = _tmp$2861 | _tmp$2862;
            val$2860 = i$1343->$0;
            _tmp$2859 = val$2860 + 2;
            if (
              _tmp$2859 < 0 || _tmp$2859 >= Moonbit_array_length(bytes$1340)
            ) {
              moonbit_panic();
            }
            _tmp$2858 = bytes$1340[_tmp$2859];
            _tmp$2857 = (int32_t)_tmp$2858;
            _tmp$2856 = _tmp$2857 & 63;
            _tmp$2854 = _tmp$2855 | _tmp$2856;
            c$1344->$0 = _tmp$2854;
            _field$2964 = c$1344->$0;
            moonbit_decref(c$1344);
            val$2871 = _field$2964;
            _tmp$2870 = val$2871;
            moonbit_incref(res$1341);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1341, _tmp$2870
            );
            val$2873 = i$1343->$0;
            _tmp$2872 = val$2873 + 3;
            i$1343->$0 = _tmp$2872;
          } else {
            int32_t val$2875 = i$1343->$0;
            int32_t _tmp$2874 = val$2875 + 3;
            int32_t val$2898;
            int32_t _tmp$2897;
            int32_t _tmp$2890;
            int32_t val$2896;
            int32_t _tmp$2895;
            int32_t _tmp$2894;
            int32_t _tmp$2893;
            int32_t _tmp$2892;
            int32_t _tmp$2891;
            int32_t _tmp$2883;
            int32_t val$2889;
            int32_t _tmp$2888;
            int32_t _tmp$2887;
            int32_t _tmp$2886;
            int32_t _tmp$2885;
            int32_t _tmp$2884;
            int32_t _tmp$2877;
            int32_t val$2882;
            int32_t _tmp$2881;
            int32_t _tmp$2880;
            int32_t _tmp$2879;
            int32_t _tmp$2878;
            int32_t _tmp$2876;
            int32_t val$2900;
            int32_t _tmp$2899;
            int32_t val$2904;
            int32_t _tmp$2903;
            int32_t _tmp$2902;
            int32_t _tmp$2901;
            int32_t _field$2965;
            int32_t val$2908;
            int32_t _tmp$2907;
            int32_t _tmp$2906;
            int32_t _tmp$2905;
            int32_t val$2910;
            int32_t _tmp$2909;
            if (_tmp$2874 >= len$1342) {
              moonbit_decref(c$1344);
              moonbit_decref(i$1343);
              moonbit_decref(bytes$1340);
              break;
            }
            val$2898 = c$1344->$0;
            _tmp$2897 = val$2898 & 7;
            _tmp$2890 = _tmp$2897 << 18;
            val$2896 = i$1343->$0;
            _tmp$2895 = val$2896 + 1;
            if (
              _tmp$2895 < 0 || _tmp$2895 >= Moonbit_array_length(bytes$1340)
            ) {
              moonbit_panic();
            }
            _tmp$2894 = bytes$1340[_tmp$2895];
            _tmp$2893 = (int32_t)_tmp$2894;
            _tmp$2892 = _tmp$2893 & 63;
            _tmp$2891 = _tmp$2892 << 12;
            _tmp$2883 = _tmp$2890 | _tmp$2891;
            val$2889 = i$1343->$0;
            _tmp$2888 = val$2889 + 2;
            if (
              _tmp$2888 < 0 || _tmp$2888 >= Moonbit_array_length(bytes$1340)
            ) {
              moonbit_panic();
            }
            _tmp$2887 = bytes$1340[_tmp$2888];
            _tmp$2886 = (int32_t)_tmp$2887;
            _tmp$2885 = _tmp$2886 & 63;
            _tmp$2884 = _tmp$2885 << 6;
            _tmp$2877 = _tmp$2883 | _tmp$2884;
            val$2882 = i$1343->$0;
            _tmp$2881 = val$2882 + 3;
            if (
              _tmp$2881 < 0 || _tmp$2881 >= Moonbit_array_length(bytes$1340)
            ) {
              moonbit_panic();
            }
            _tmp$2880 = bytes$1340[_tmp$2881];
            _tmp$2879 = (int32_t)_tmp$2880;
            _tmp$2878 = _tmp$2879 & 63;
            _tmp$2876 = _tmp$2877 | _tmp$2878;
            c$1344->$0 = _tmp$2876;
            val$2900 = c$1344->$0;
            _tmp$2899 = val$2900 - 65536;
            c$1344->$0 = _tmp$2899;
            val$2904 = c$1344->$0;
            _tmp$2903 = val$2904 >> 10;
            _tmp$2902 = _tmp$2903 + 55296;
            _tmp$2901 = _tmp$2902;
            moonbit_incref(res$1341);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1341, _tmp$2901
            );
            _field$2965 = c$1344->$0;
            moonbit_decref(c$1344);
            val$2908 = _field$2965;
            _tmp$2907 = val$2908 & 1023;
            _tmp$2906 = _tmp$2907 + 56320;
            _tmp$2905 = _tmp$2906;
            moonbit_incref(res$1341);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1341, _tmp$2905
            );
            val$2910 = i$1343->$0;
            _tmp$2909 = val$2910 + 4;
            i$1343->$0 = _tmp$2909;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$1343);
      moonbit_decref(bytes$1340);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$1341);
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$2821,
  moonbit_string_t s$1334
) {
  struct $Ref$3c$Int$3e$* res$1335 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$1336;
  int32_t i$1337;
  int32_t _field$2966;
  Moonbit_object_header(res$1335)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$1335->$0 = 0;
  len$1336 = Moonbit_array_length(s$1334);
  i$1337 = 0;
  while (1) {
    if (i$1337 < len$1336) {
      int32_t val$2826 = res$1335->$0;
      int32_t _tmp$2823 = val$2826 * 10;
      int32_t _tmp$2825;
      int32_t _tmp$2824;
      int32_t _tmp$2822;
      int32_t _tmp$2827;
      if (i$1337 < 0 || i$1337 >= Moonbit_array_length(s$1334)) {
        moonbit_panic();
      }
      _tmp$2825 = s$1334[i$1337];
      _tmp$2824 = _tmp$2825 - 48;
      _tmp$2822 = _tmp$2823 + _tmp$2824;
      res$1335->$0 = _tmp$2822;
      _tmp$2827 = i$1337 + 1;
      i$1337 = _tmp$2827;
      continue;
    } else {
      moonbit_decref(s$1334);
    }
    break;
  }
  _field$2966 = res$1335->$0;
  moonbit_decref(res$1335);
  return _field$2966;
}

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1297,
  moonbit_string_t filename$1258,
  int32_t index$1259
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$1257;
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$3524;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1260;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$1269;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2976;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$2820;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2975;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$1270;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2974;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$2819;
  moonbit_string_t _field$2973;
  moonbit_string_t file_name$1271;
  moonbit_string_t name$1272;
  int32_t _tmp$2816;
  moonbit_string_t name$1273;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$2773;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$2774;
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$3526;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1280;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1296;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1321;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1323;
  void* _field$2970;
  int32_t _cnt$3303;
  void* _bind$1324;
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$3530;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$2813;
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$3531;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2806;
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$3532;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$2807;
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$3533;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2791;
  moonbit_incref(
    $azimuth$telemetry$simple_test$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$simple_test$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$simple_test$moonbit_test_driver_internal_async_tests
  );
  filtered_test$1257
  = $azimuth$telemetry$simple_test$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$simple_test$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$simple_test$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$simple_test$moonbit_test_driver_internal_async_tests,
      filename$1258,
      index$1259
  );
  _closure$3524
  = (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$3524)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$3524->code
  = &$$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$3524->$0 = index$1259;
  handle_result$1260
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$3524;
  if (filtered_test$1257 == 0) {
    moonbit_decref(async_tests$1297);
    if (filtered_test$1257) {
      moonbit_decref(filtered_test$1257);
    }
    $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1260,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$1331 =
      filtered_test$1257;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$1332 = _Some$1331;
    item$1269 = _item$1332;
    goto $join$1268;
  }
  goto $joinlet$3525;
  $join$1268:;
  _field$2976 = item$1269->$1;
  meta$2820 = _field$2976;
  _field$2975 = meta$2820->$2;
  attrs$1270 = _field$2975;
  _field$2974 = item$1269->$1;
  meta$2819 = _field$2974;
  _field$2973 = meta$2819->$0;
  file_name$1271 = _field$2973;
  moonbit_incref(attrs$1270);
  moonbit_incref(file_name$1271);
  moonbit_incref(attrs$1270);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$1270)) {
    name$1272 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$1270);
    name$1272 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1270, 0);
  }
  _tmp$2816 = Moonbit_array_length(name$1272);
  if (_tmp$2816 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2972;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$2818;
    int32_t _field$2971;
    int32_t index$2817;
    moonbit_decref(name$1272);
    _field$2972 = item$1269->$1;
    meta$2818 = _field$2972;
    _field$2971 = meta$2818->$1;
    index$2817 = _field$2971;
    name$1273 = $Int$$to_string$inner(index$2817, 10);
  } else {
    name$1273 = name$1272;
  }
  moonbit_incref(attrs$1270);
  _tmp$2773 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$1270);
  _tmp$2774
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$2773, _tmp$2774)) {
    moonbit_string_t _tmp$2788;
    moonbit_string_t _tmp$2787;
    moonbit_string_t _tmp$2784;
    moonbit_string_t _tmp$2786;
    moonbit_string_t _tmp$2785;
    moonbit_string_t _tmp$2783;
    moonbit_decref(async_tests$1297);
    moonbit_decref(item$1269);
    moonbit_incref(file_name$1271);
    _tmp$2788
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$1271
    );
    _tmp$2787
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$2788
    );
    _tmp$2784
    = moonbit_add_string(
      _tmp$2787, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$2786 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1270, 0);
    _tmp$2785 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$2786);
    _tmp$2783 = moonbit_add_string(_tmp$2784, _tmp$2785);
    $moonbitlang$core$builtin$println$0(_tmp$2783);
    $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1260,
        name$1273,
        file_name$1271,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$1270);
  }
  moonbit_incref(name$1273);
  moonbit_incref(file_name$1271);
  moonbit_incref(handle_result$1260);
  _closure$3526
  = (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$3526)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$3526->code
  = &$$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$3526->$0 = name$1273;
  _closure$3526->$1 = file_name$1271;
  _closure$3526->$2 = handle_result$1260;
  on_err$1280 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$3526;
  _field$2970 = item$1269->$0;
  _cnt$3303 = Moonbit_object_header(item$1269)->rc;
  if (_cnt$3303 > 1) {
    int32_t _new_cnt$3305;
    moonbit_incref(_field$2970);
    _new_cnt$3305 = _cnt$3303 - 1;
    Moonbit_object_header(item$1269)->rc = _new_cnt$3305;
  } else if (_cnt$3303 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$3304 = item$1269->$1;
    moonbit_decref(_field$3304);
    moonbit_free(item$1269);
  }
  _bind$1324 = _field$2970;
  switch (Moonbit_object_tag(_bind$1324)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$1325;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2967;
      int32_t _cnt$3306;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1326;
      moonbit_decref(async_tests$1297);
      _F0$1325 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$1324;
      _field$2967 = _F0$1325->$0;
      _cnt$3306 = Moonbit_object_header(_F0$1325)->rc;
      if (_cnt$3306 > 1) {
        int32_t _new_cnt$3307;
        moonbit_incref(_field$2967);
        _new_cnt$3307 = _cnt$3306 - 1;
        Moonbit_object_header(_F0$1325)->rc = _new_cnt$3307;
      } else if (_cnt$3306 == 1) {
        moonbit_free(_F0$1325);
      }
      _f$1326 = _field$2967;
      f$1323 = _f$1326;
      goto $join$1322;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$1327;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2968;
      int32_t _cnt$3308;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1328;
      moonbit_decref(async_tests$1297);
      _F1$1327 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$1324;
      _field$2968 = _F1$1327->$0;
      _cnt$3308 = Moonbit_object_header(_F1$1327)->rc;
      if (_cnt$3308 > 1) {
        int32_t _new_cnt$3309;
        moonbit_incref(_field$2968);
        _new_cnt$3309 = _cnt$3308 - 1;
        Moonbit_object_header(_F1$1327)->rc = _new_cnt$3309;
      } else if (_cnt$3308 == 1) {
        moonbit_free(_F1$1327);
      }
      _f$1328 = _field$2968;
      f$1321 = _f$1328;
      goto $join$1320;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$1329 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$1324;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2969 =
        _F2$1329->$0;
      int32_t _cnt$3310 = Moonbit_object_header(_F2$1329)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$1330;
      if (_cnt$3310 > 1) {
        int32_t _new_cnt$3311;
        moonbit_incref(_field$2969);
        _new_cnt$3311 = _cnt$3310 - 1;
        Moonbit_object_header(_F2$1329)->rc = _new_cnt$3311;
      } else if (_cnt$3310 == 1) {
        moonbit_free(_F2$1329);
      }
      _f$1330 = _field$2969;
      f$1296 = _f$1330;
      goto $join$1295;
      break;
    }
  }
  goto $joinlet$3529;
  $join$1322:;
  _closure$3530
  = (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$3530)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$3530->code
  = &$$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$3530->$0 = name$1273;
  _closure$3530->$1 = file_name$1271;
  _closure$3530->$2 = handle_result$1260;
  _tmp$2813 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$3530;
  $azimuth$telemetry$simple_test$moonbit_test_driver_internal_catch_error(
    f$1323, _tmp$2813, on_err$1280
  );
  $joinlet$3529:;
  goto $joinlet$3528;
  $join$1320:;
  moonbit_incref(name$1273);
  _closure$3531
  = (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$3531)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$3531->code
  = &$$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$3531->$0 = f$1321;
  _closure$3531->$1 = name$1273;
  _tmp$2806
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$3531;
  _closure$3532
  = (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$3532)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$3532->code
  = &$$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$3532->$0 = name$1273;
  _closure$3532->$1 = file_name$1271;
  _closure$3532->$2 = handle_result$1260;
  _tmp$2807 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$3532;
  $azimuth$telemetry$simple_test$moonbit_test_driver_internal_catch_error(
    _tmp$2806, _tmp$2807, on_err$1280
  );
  $joinlet$3528:;
  goto $joinlet$3527;
  $join$1295:;
  _closure$3533
  = (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$3533)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$3533->code
  = &$$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$3533->$0 = f$1296;
  _closure$3533->$1 = on_err$1280;
  _closure$3533->$2 = name$1273;
  _closure$3533->$3 = file_name$1271;
  _closure$3533->$4 = handle_result$1260;
  _tmp$2791
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$3533;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$1297, _tmp$2791);
  $joinlet$3527:;
  $joinlet$3525:;
  return 0;
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2814
) {
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$2815 =
    (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$2814;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2979 =
    _casted_env$2815->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1260 =
    _field$2979;
  moonbit_string_t _field$2978 = _casted_env$2815->$1;
  moonbit_string_t file_name$1271 = _field$2978;
  moonbit_string_t _field$2977 = _casted_env$2815->$0;
  int32_t _cnt$3312 = Moonbit_object_header(_casted_env$2815)->rc;
  moonbit_string_t name$1273;
  if (_cnt$3312 > 1) {
    int32_t _new_cnt$3313;
    moonbit_incref(handle_result$1260);
    moonbit_incref(file_name$1271);
    moonbit_incref(_field$2977);
    _new_cnt$3313 = _cnt$3312 - 1;
    Moonbit_object_header(_casted_env$2815)->rc = _new_cnt$3313;
  } else if (_cnt$3312 == 1) {
    moonbit_free(_casted_env$2815);
  }
  name$1273 = _field$2977;
  $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1260,
      name$1273,
      file_name$1271,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$2810
) {
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$2811 =
    (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$2810;
  moonbit_string_t _field$2981 = _casted_env$2811->$1;
  moonbit_string_t name$1273 = _field$2981;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2980 =
    _casted_env$2811->$0;
  int32_t _cnt$3314 = Moonbit_object_header(_casted_env$2811)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1321;
  int32_t _tmp$2812;
  if (_cnt$3314 > 1) {
    int32_t _new_cnt$3315;
    moonbit_incref(name$1273);
    moonbit_incref(_field$2980);
    _new_cnt$3315 = _cnt$3314 - 1;
    Moonbit_object_header(_casted_env$2811)->rc = _new_cnt$3315;
  } else if (_cnt$3314 == 1) {
    moonbit_free(_casted_env$2811);
  }
  f$1321 = _field$2980;
  _tmp$2812
  = $azimuth$telemetry$simple_test$moonbit_test_driver_internal_new_test_arg(
    name$1273
  );
  return f$1321->code(f$1321, _tmp$2812);
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$2808
) {
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$2809 =
    (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$2808;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2984 =
    _casted_env$2809->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1260 =
    _field$2984;
  moonbit_string_t _field$2983 = _casted_env$2809->$1;
  moonbit_string_t file_name$1271 = _field$2983;
  moonbit_string_t _field$2982 = _casted_env$2809->$0;
  int32_t _cnt$3316 = Moonbit_object_header(_casted_env$2809)->rc;
  moonbit_string_t name$1273;
  if (_cnt$3316 > 1) {
    int32_t _new_cnt$3317;
    moonbit_incref(handle_result$1260);
    moonbit_incref(file_name$1271);
    moonbit_incref(_field$2982);
    _new_cnt$3317 = _cnt$3316 - 1;
    Moonbit_object_header(_casted_env$2809)->rc = _new_cnt$3317;
  } else if (_cnt$3316 == 1) {
    moonbit_free(_casted_env$2809);
  }
  name$1273 = _field$2982;
  $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1260,
      name$1273,
      file_name$1271,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$2792,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1298,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1299
) {
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$2793 =
    (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$2792;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2989 =
    _casted_env$2793->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1260 =
    _field$2989;
  moonbit_string_t _field$2988 = _casted_env$2793->$3;
  moonbit_string_t file_name$1271 = _field$2988;
  moonbit_string_t _field$2987 = _casted_env$2793->$2;
  moonbit_string_t name$1273 = _field$2987;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2986 = _casted_env$2793->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1280 = _field$2986;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2985 =
    _casted_env$2793->$0;
  int32_t _cnt$3318 = Moonbit_object_header(_casted_env$2793)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1296;
  int32_t _async_driver$1300;
  int32_t _tmp$2797;
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$3534;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$2798;
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$3535;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$2799;
  if (_cnt$3318 > 1) {
    int32_t _new_cnt$3319;
    moonbit_incref(handle_result$1260);
    moonbit_incref(file_name$1271);
    moonbit_incref(name$1273);
    moonbit_incref(on_err$1280);
    moonbit_incref(_field$2985);
    _new_cnt$3319 = _cnt$3318 - 1;
    Moonbit_object_header(_casted_env$2793)->rc = _new_cnt$3319;
  } else if (_cnt$3318 == 1) {
    moonbit_free(_casted_env$2793);
  }
  f$1296 = _field$2985;
  _async_driver$1300 = 0;
  moonbit_incref(name$1273);
  _tmp$2797
  = $azimuth$telemetry$simple_test$moonbit_test_driver_internal_new_test_arg(
    name$1273
  );
  moonbit_incref(_cont$1298);
  _closure$3534
  = (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$3534)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$3534->code
  = &$$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$3534->$0 = _async_driver$1300;
  _closure$3534->$1 = _cont$1298;
  _closure$3534->$2 = name$1273;
  _closure$3534->$3 = file_name$1271;
  _closure$3534->$4 = handle_result$1260;
  _tmp$2798 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$3534;
  _closure$3535
  = (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$3535)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$3535->code
  = &$$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$3535->$0 = _async_driver$1300;
  _closure$3535->$1 = _err_cont$1299;
  _closure$3535->$2 = _cont$1298;
  _closure$3535->$3 = on_err$1280;
  _tmp$2799 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$3535;
  f$1296->code(f$1296, _tmp$2797, _tmp$2798, _tmp$2799);
  return 0;
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$2803,
  int32_t _cont_param$1318
) {
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$2804 =
    (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$2803;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2994 =
    _casted_env$2804->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1260 =
    _field$2994;
  moonbit_string_t _field$2993 = _casted_env$2804->$3;
  moonbit_string_t file_name$1271 = _field$2993;
  moonbit_string_t _field$2992 = _casted_env$2804->$2;
  moonbit_string_t name$1273 = _field$2992;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2991 = _casted_env$2804->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1298 = _field$2991;
  int32_t _field$2990 = _casted_env$2804->$0;
  int32_t _cnt$3320 = Moonbit_object_header(_casted_env$2804)->rc;
  int32_t _async_driver$1300;
  void* State_1$2805;
  if (_cnt$3320 > 1) {
    int32_t _new_cnt$3321;
    moonbit_incref(handle_result$1260);
    moonbit_incref(file_name$1271);
    moonbit_incref(name$1273);
    moonbit_incref(_cont$1298);
    _new_cnt$3321 = _cnt$3320 - 1;
    Moonbit_object_header(_casted_env$2804)->rc = _new_cnt$3321;
  } else if (_cnt$3320 == 1) {
    moonbit_free(_casted_env$2804);
  }
  _async_driver$1300 = _field$2990;
  State_1$2805
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1
      )
    );
  Moonbit_object_header(State_1$2805)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1*)State_1$2805)->$0
  = _cont_param$1318;
  ((struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1*)State_1$2805)->$1
  = handle_result$1260;
  ((struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1*)State_1$2805)->$2
  = file_name$1271;
  ((struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1*)State_1$2805)->$3
  = name$1273;
  ((struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1*)State_1$2805)->$4
  = _cont$1298;
  $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1300, State_1$2805
  );
  return 0;
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2800,
  void* _cont_param$1319
) {
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$2801 =
    (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$2800;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2998 = _casted_env$2801->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1280 = _field$2998;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2997 = _casted_env$2801->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1298 = _field$2997;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2996 = _casted_env$2801->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1299 = _field$2996;
  int32_t _field$2995 = _casted_env$2801->$0;
  int32_t _cnt$3322 = Moonbit_object_header(_casted_env$2801)->rc;
  int32_t _async_driver$1300;
  void* _try$572$2802;
  if (_cnt$3322 > 1) {
    int32_t _new_cnt$3323;
    moonbit_incref(on_err$1280);
    moonbit_incref(_cont$1298);
    moonbit_incref(_err_cont$1299);
    _new_cnt$3323 = _cnt$3322 - 1;
    Moonbit_object_header(_casted_env$2801)->rc = _new_cnt$3323;
  } else if (_cnt$3322 == 1) {
    moonbit_free(_casted_env$2801);
  }
  _async_driver$1300 = _field$2995;
  _try$572$2802
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572
      )
    );
  Moonbit_object_header(_try$572$2802)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572*)_try$572$2802)->$0
  = _cont_param$1319;
  ((struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572*)_try$572$2802)->$1
  = on_err$1280;
  ((struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572*)_try$572$2802)->$2
  = _cont$1298;
  ((struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572*)_try$572$2802)->$3
  = _err_cont$1299;
  $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1300, _try$572$2802
  );
  return 0;
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$2794,
  void* _state$1301
) {
  switch (Moonbit_object_tag(_state$1301)) {
    case 0: {
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572* _$2a$try$572$1302 =
        (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$$2a$try$572*)_state$1301;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$3002 = _$2a$try$572$1302->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1303 = _field$3002;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3001 = _$2a$try$572$1302->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1304 = _field$3001;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$3000 = _$2a$try$572$1302->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1305 = _field$3000;
      void* _field$2999 = _$2a$try$572$1302->$0;
      int32_t _cnt$3324 = Moonbit_object_header(_$2a$try$572$1302)->rc;
      void* _try_err$1306;
      void* err$1308;
      void* err$1310;
      int32_t _tmp$2796;
      if (_cnt$3324 > 1) {
        int32_t _new_cnt$3325;
        moonbit_incref(_err_cont$1303);
        moonbit_incref(_cont$1304);
        moonbit_incref(on_err$1305);
        moonbit_incref(_field$2999);
        _new_cnt$3325 = _cnt$3324 - 1;
        Moonbit_object_header(_$2a$try$572$1302)->rc = _new_cnt$3325;
      } else if (_cnt$3324 == 1) {
        moonbit_free(_$2a$try$572$1302);
      }
      _try_err$1306 = _field$2999;
      if (
        $azimuth$telemetry$simple_test$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$1305);
        moonbit_decref(_cont$1304);
        err$1310 = _try_err$1306;
        goto $join$1309;
      } else {
        moonbit_decref(_err_cont$1303);
        err$1308 = _try_err$1306;
        goto $join$1307;
      }
      goto $joinlet$3537;
      $join$1309:;
      return _err_cont$1303->code(_err_cont$1303, err$1310);
      $joinlet$3537:;
      goto $joinlet$3536;
      $join$1307:;
      _tmp$2796 = on_err$1305->code(on_err$1305, err$1308);
      _cont$1304->code(_cont$1304, _tmp$2796);
      $joinlet$3536:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1* _State_1$1311 =
        (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$588$on_err$68$$2a$arm$580$lambda$606$State$State_1*)_state$1301;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$3006 = _State_1$1311->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1312 = _field$3006;
      moonbit_string_t _field$3005 = _State_1$1311->$3;
      moonbit_string_t name$1313 = _field$3005;
      moonbit_string_t _field$3004 = _State_1$1311->$2;
      moonbit_string_t file_name$1314 = _field$3004;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3003 =
        _State_1$1311->$1;
      int32_t _cnt$3326 = Moonbit_object_header(_State_1$1311)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1315;
      int32_t _tmp$2795;
      if (_cnt$3326 > 1) {
        int32_t _new_cnt$3327;
        moonbit_incref(_cont$1312);
        moonbit_incref(name$1313);
        moonbit_incref(file_name$1314);
        moonbit_incref(_field$3003);
        _new_cnt$3327 = _cnt$3326 - 1;
        Moonbit_object_header(_State_1$1311)->rc = _new_cnt$3327;
      } else if (_cnt$3326 == 1) {
        moonbit_free(_State_1$1311);
      }
      handle_result$1315 = _field$3003;
      _tmp$2795
      = handle_result$1315->code(
        handle_result$1315,
          name$1313,
          file_name$1314,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$1312->code(_cont$1312, _tmp$2795);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$2789,
  void* err$1281
) {
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$2790 =
    (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$2789;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$3013 =
    _casted_env$2790->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1260 =
    _field$3013;
  moonbit_string_t _field$3012 = _casted_env$2790->$1;
  moonbit_string_t file_name$1271 = _field$3012;
  moonbit_string_t _field$3011 = _casted_env$2790->$0;
  int32_t _cnt$3328 = Moonbit_object_header(_casted_env$2790)->rc;
  moonbit_string_t name$1273;
  void* e$1283;
  moonbit_string_t e$1286;
  moonbit_string_t message$1284;
  if (_cnt$3328 > 1) {
    int32_t _new_cnt$3329;
    moonbit_incref(handle_result$1260);
    moonbit_incref(file_name$1271);
    moonbit_incref(_field$3011);
    _new_cnt$3329 = _cnt$3328 - 1;
    Moonbit_object_header(_casted_env$2790)->rc = _new_cnt$3329;
  } else if (_cnt$3328 == 1) {
    moonbit_free(_casted_env$2790);
  }
  name$1273 = _field$3011;
  switch (Moonbit_object_tag(err$1281)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$1287 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$1281;
      moonbit_string_t _field$3007 = _Failure$1287->$0;
      int32_t _cnt$3330 = Moonbit_object_header(_Failure$1287)->rc;
      moonbit_string_t _e$1288;
      if (_cnt$3330 > 1) {
        int32_t _new_cnt$3331;
        moonbit_incref(_field$3007);
        _new_cnt$3331 = _cnt$3330 - 1;
        Moonbit_object_header(_Failure$1287)->rc = _new_cnt$3331;
      } else if (_cnt$3330 == 1) {
        moonbit_free(_Failure$1287);
      }
      _e$1288 = _field$3007;
      e$1286 = _e$1288;
      goto $join$1285;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$1289 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$1281;
      moonbit_string_t _field$3008 = _InspectError$1289->$0;
      int32_t _cnt$3332 = Moonbit_object_header(_InspectError$1289)->rc;
      moonbit_string_t _e$1290;
      if (_cnt$3332 > 1) {
        int32_t _new_cnt$3333;
        moonbit_incref(_field$3008);
        _new_cnt$3333 = _cnt$3332 - 1;
        Moonbit_object_header(_InspectError$1289)->rc = _new_cnt$3333;
      } else if (_cnt$3332 == 1) {
        moonbit_free(_InspectError$1289);
      }
      _e$1290 = _field$3008;
      e$1286 = _e$1290;
      goto $join$1285;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$1291 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$1281;
      moonbit_string_t _field$3009 = _SnapshotError$1291->$0;
      int32_t _cnt$3334 = Moonbit_object_header(_SnapshotError$1291)->rc;
      moonbit_string_t _e$1292;
      if (_cnt$3334 > 1) {
        int32_t _new_cnt$3335;
        moonbit_incref(_field$3009);
        _new_cnt$3335 = _cnt$3334 - 1;
        Moonbit_object_header(_SnapshotError$1291)->rc = _new_cnt$3335;
      } else if (_cnt$3334 == 1) {
        moonbit_free(_SnapshotError$1291);
      }
      _e$1292 = _field$3009;
      e$1286 = _e$1292;
      goto $join$1285;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$simple_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$1293 =
        (struct $Error$azimuth$telemetry$simple_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$1281;
      moonbit_string_t _field$3010 =
        _MoonBitTestDriverInternalJsError$1293->$0;
      int32_t _cnt$3336 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1293)->rc;
      moonbit_string_t _e$1294;
      if (_cnt$3336 > 1) {
        int32_t _new_cnt$3337;
        moonbit_incref(_field$3010);
        _new_cnt$3337 = _cnt$3336 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1293)->rc
        = _new_cnt$3337;
      } else if (_cnt$3336 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$1293);
      }
      _e$1294 = _field$3010;
      e$1286 = _e$1294;
      goto $join$1285;
      break;
    }
    default: {
      e$1283 = err$1281;
      goto $join$1282;
      break;
    }
  }
  goto $joinlet$3539;
  $join$1285:;
  $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1260, name$1273, file_name$1271, e$1286, 0
  );
  $joinlet$3539:;
  goto $joinlet$3538;
  $join$1282:;
  message$1284 = $Error$to_string(e$1283);
  $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1260, name$1273, file_name$1271, message$1284, 0
  );
  $joinlet$3538:;
  return 0;
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2775,
  moonbit_string_t attr$1274
) {
  int32_t _tmp$2777;
  int64_t _tmp$2776;
  moonbit_decref(_env$2775);
  _tmp$2777 = Moonbit_array_length(attr$1274);
  _tmp$2776 = (int64_t)_tmp$2777;
  moonbit_incref(attr$1274);
  if ($String$$char_length_ge$inner(attr$1274, 5, 0, _tmp$2776)) {
    int32_t _tmp$2782 = attr$1274[0];
    int32_t _x$1275 = _tmp$2782;
    if (_x$1275 == 112) {
      int32_t _tmp$2781 = attr$1274[1];
      int32_t _x$1276 = _tmp$2781;
      if (_x$1276 == 97) {
        int32_t _tmp$2780 = attr$1274[2];
        int32_t _x$1277 = _tmp$2780;
        if (_x$1277 == 110) {
          int32_t _tmp$2779 = attr$1274[3];
          int32_t _x$1278 = _tmp$2779;
          if (_x$1278 == 105) {
            int32_t _tmp$3014 = attr$1274[4];
            int32_t _tmp$2778;
            int32_t _x$1279;
            moonbit_decref(attr$1274);
            _tmp$2778 = _tmp$3014;
            _x$1279 = _tmp$2778;
            return _x$1279 == 99 || 0;
          } else {
            moonbit_decref(attr$1274);
            return 0;
          }
        } else {
          moonbit_decref(attr$1274);
          return 0;
        }
      } else {
        moonbit_decref(attr$1274);
        return 0;
      }
    } else {
      moonbit_decref(attr$1274);
      return 0;
    }
  } else {
    moonbit_decref(attr$1274);
    return 0;
  }
}

int32_t $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$2759,
  moonbit_string_t test_name$1261,
  moonbit_string_t file_name$1262,
  moonbit_string_t message$1263,
  int32_t skipped$1264
) {
  struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$2760 =
    (struct $$azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$2759;
  int32_t _field$3015 = _casted_env$2760->$0;
  int32_t index$1259;
  int32_t _if_result$3540;
  moonbit_string_t file_name$1265;
  moonbit_string_t test_name$1266;
  moonbit_string_t message$1267;
  moonbit_string_t _tmp$2772;
  moonbit_string_t _tmp$2771;
  moonbit_string_t _tmp$2769;
  moonbit_string_t _tmp$2770;
  moonbit_string_t _tmp$2768;
  moonbit_string_t _tmp$2766;
  moonbit_string_t _tmp$2767;
  moonbit_string_t _tmp$2765;
  moonbit_string_t _tmp$2763;
  moonbit_string_t _tmp$2764;
  moonbit_string_t _tmp$2762;
  moonbit_string_t _tmp$2761;
  moonbit_decref(_casted_env$2760);
  index$1259 = _field$3015;
  if (!skipped$1264) {
    _if_result$3540 = 1;
  } else {
    _if_result$3540 = 0;
  }
  if (_if_result$3540) {
    
  }
  file_name$1265 = $String$$escape(file_name$1262);
  test_name$1266 = $String$$escape(test_name$1261);
  message$1267 = $String$$escape(message$1263);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$2772
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$1265
  );
  _tmp$2771
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$2772
  );
  _tmp$2769
  = moonbit_add_string(
    _tmp$2771, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$2770
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$1259
  );
  _tmp$2768 = moonbit_add_string(_tmp$2769, _tmp$2770);
  _tmp$2766
  = moonbit_add_string(
    _tmp$2768, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$2767
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$1266
  );
  _tmp$2765 = moonbit_add_string(_tmp$2766, _tmp$2767);
  _tmp$2763
  = moonbit_add_string(
    _tmp$2765, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$2764
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    message$1267
  );
  _tmp$2762 = moonbit_add_string(_tmp$2763, _tmp$2764);
  _tmp$2761
  = moonbit_add_string(
    _tmp$2762, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$2761);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1256
) {
  moonbit_decref(_discard_$1256);
  return 42;
}

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$simple_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1254,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1255,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1252
) {
  void* _try_err$1250;
  struct moonbit_result_0 _tmp$3542 = f$1254->code(f$1254);
  void* err$1251;
  if (_tmp$3542.tag) {
    int32_t const _ok$2757 = _tmp$3542.data.ok;
    moonbit_decref(on_err$1252);
  } else {
    void* const _err$2758 = _tmp$3542.data.err;
    moonbit_decref(on_ok$1255);
    _try_err$1250 = _err$2758;
    goto $join$1249;
  }
  on_ok$1255->code(on_ok$1255);
  goto $joinlet$3541;
  $join$1249:;
  err$1251 = _try_err$1250;
  on_err$1252->code(on_err$1252, err$1251);
  $joinlet$3541:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$simple_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1215,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1228,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1241,
  moonbit_string_t file_filter$1212,
  int32_t index_filter$1213
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1209;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1210;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1214;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3021;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2748;
  void* F0$2745;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3020;
  int32_t _cnt$3338;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$2747;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$2746;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1211;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1224;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1225;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1227;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3019;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$2752;
  void* F1$2749;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3018;
  int32_t _cnt$3341;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$2751;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$2750;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1226;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1237;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1238;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1240;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3017;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2756;
  void* F2$2753;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$3016;
  int32_t _cnt$3344;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$2755;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$2754;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1239;
  moonbit_incref(file_filter$1212);
  _bind$1214
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$1215, file_filter$1212
  );
  if (_bind$1214 == 0) {
    if (_bind$1214) {
      moonbit_decref(_bind$1214);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1216 =
      _bind$1214;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1217 =
      _Some$1216;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1219;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1220;
    moonbit_incref(_index_func_map$1217);
    _bind$1220
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$1217, index_filter$1213
    );
    if (_bind$1220 == 0) {
      if (_bind$1220) {
        moonbit_decref(_bind$1220);
      }
      moonbit_decref(_index_func_map$1217);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1221;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1222;
      moonbit_decref(async_tests$1241);
      moonbit_decref(with_args_tests$1228);
      _Some$1221 = _bind$1220;
      _func_attrs_tuple$1222 = _Some$1221;
      func_attrs_tuple$1219 = _func_attrs_tuple$1222;
      goto $join$1218;
    }
    goto $joinlet$3544;
    $join$1218:;
    index_func_map$1209 = _index_func_map$1217;
    func_attrs_tuple$1210 = func_attrs_tuple$1219;
    goto $join$1208;
    $joinlet$3544:;
  }
  goto $joinlet$3543;
  $join$1208:;
  moonbit_decref(index_func_map$1209);
  _field$3021 = func_attrs_tuple$1210->$0;
  _tmp$2748 = _field$3021;
  moonbit_incref(_tmp$2748);
  F0$2745
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$2745)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$2745)->$0 = _tmp$2748;
  _field$3020 = func_attrs_tuple$1210->$1;
  _cnt$3338 = Moonbit_object_header(func_attrs_tuple$1210)->rc;
  if (_cnt$3338 > 1) {
    int32_t _new_cnt$3340;
    moonbit_incref(_field$3020);
    _new_cnt$3340 = _cnt$3338 - 1;
    Moonbit_object_header(func_attrs_tuple$1210)->rc = _new_cnt$3340;
  } else if (_cnt$3338 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3339 =
      func_attrs_tuple$1210->$0;
    moonbit_decref(_field$3339);
    moonbit_free(func_attrs_tuple$1210);
  }
  _tmp$2747 = _field$3020;
  _tmp$2746
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$2746)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$2746->$0 = file_filter$1212;
  _tmp$2746->$1 = index_filter$1213;
  _tmp$2746->$2 = _tmp$2747;
  k$1211
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1211)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1211->$0 = F0$2745;
  k$1211->$1 = _tmp$2746;
  return k$1211;
  $joinlet$3543:;
  moonbit_incref(file_filter$1212);
  _bind$1227
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$1228, file_filter$1212
  );
  if (_bind$1227 == 0) {
    if (_bind$1227) {
      moonbit_decref(_bind$1227);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1229 =
      _bind$1227;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1230 =
      _Some$1229;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1232;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1233;
    moonbit_incref(_index_func_map$1230);
    _bind$1233
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$1230, index_filter$1213
    );
    if (_bind$1233 == 0) {
      if (_bind$1233) {
        moonbit_decref(_bind$1233);
      }
      moonbit_decref(_index_func_map$1230);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1234;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1235;
      moonbit_decref(async_tests$1241);
      _Some$1234 = _bind$1233;
      _func_attrs_tuple$1235 = _Some$1234;
      func_attrs_tuple$1232 = _func_attrs_tuple$1235;
      goto $join$1231;
    }
    goto $joinlet$3546;
    $join$1231:;
    index_func_map$1224 = _index_func_map$1230;
    func_attrs_tuple$1225 = func_attrs_tuple$1232;
    goto $join$1223;
    $joinlet$3546:;
  }
  goto $joinlet$3545;
  $join$1223:;
  moonbit_decref(index_func_map$1224);
  _field$3019 = func_attrs_tuple$1225->$0;
  _tmp$2752 = _field$3019;
  moonbit_incref(_tmp$2752);
  F1$2749
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$2749)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$2749)->$0 = _tmp$2752;
  _field$3018 = func_attrs_tuple$1225->$1;
  _cnt$3341 = Moonbit_object_header(func_attrs_tuple$1225)->rc;
  if (_cnt$3341 > 1) {
    int32_t _new_cnt$3343;
    moonbit_incref(_field$3018);
    _new_cnt$3343 = _cnt$3341 - 1;
    Moonbit_object_header(func_attrs_tuple$1225)->rc = _new_cnt$3343;
  } else if (_cnt$3341 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$3342 =
      func_attrs_tuple$1225->$0;
    moonbit_decref(_field$3342);
    moonbit_free(func_attrs_tuple$1225);
  }
  _tmp$2751 = _field$3018;
  _tmp$2750
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$2750)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$2750->$0 = file_filter$1212;
  _tmp$2750->$1 = index_filter$1213;
  _tmp$2750->$2 = _tmp$2751;
  k$1226
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1226)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1226->$0 = F1$2749;
  k$1226->$1 = _tmp$2750;
  return k$1226;
  $joinlet$3545:;
  moonbit_incref(file_filter$1212);
  _bind$1240
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$1241, file_filter$1212
  );
  if (_bind$1240 == 0) {
    if (_bind$1240) {
      moonbit_decref(_bind$1240);
    }
    moonbit_decref(file_filter$1212);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1242 =
      _bind$1240;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1243 =
      _Some$1242;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1245;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1246;
    moonbit_incref(_index_func_map$1243);
    _bind$1246
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$1243, index_filter$1213
    );
    if (_bind$1246 == 0) {
      if (_bind$1246) {
        moonbit_decref(_bind$1246);
      }
      moonbit_decref(_index_func_map$1243);
      moonbit_decref(file_filter$1212);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1247 =
        _bind$1246;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1248 =
        _Some$1247;
      func_attrs_tuple$1245 = _func_attrs_tuple$1248;
      goto $join$1244;
    }
    goto $joinlet$3548;
    $join$1244:;
    index_func_map$1237 = _index_func_map$1243;
    func_attrs_tuple$1238 = func_attrs_tuple$1245;
    goto $join$1236;
    $joinlet$3548:;
  }
  goto $joinlet$3547;
  $join$1236:;
  moonbit_decref(index_func_map$1237);
  _field$3017 = func_attrs_tuple$1238->$0;
  _tmp$2756 = _field$3017;
  moonbit_incref(_tmp$2756);
  F2$2753
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$2753)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$2753)->$0 = _tmp$2756;
  _field$3016 = func_attrs_tuple$1238->$1;
  _cnt$3344 = Moonbit_object_header(func_attrs_tuple$1238)->rc;
  if (_cnt$3344 > 1) {
    int32_t _new_cnt$3346;
    moonbit_incref(_field$3016);
    _new_cnt$3346 = _cnt$3344 - 1;
    Moonbit_object_header(func_attrs_tuple$1238)->rc = _new_cnt$3346;
  } else if (_cnt$3344 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$3345 =
      func_attrs_tuple$1238->$0;
    moonbit_decref(_field$3345);
    moonbit_free(func_attrs_tuple$1238);
  }
  _tmp$2755 = _field$3016;
  _tmp$2754
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$2754)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$2754->$0 = file_filter$1212;
  _tmp$2754->$1 = index_filter$1213;
  _tmp$2754->$2 = _tmp$2755;
  k$1239
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1239)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1239->$0 = F2$2753;
  k$1239->$1 = _tmp$2754;
  return k$1239;
  $joinlet$3547:;
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9(
  
) {
  int32_t clock$1196 = $$azimuth$telemetry$simple_test$Clock$$system();
  int32_t random$1197 = $$azimuth$telemetry$simple_test$Random$$system();
  int64_t timestamp1$1198 =
    $$azimuth$telemetry$simple_test$Clock$$now_unix_nanos(clock$1196);
  uint64_t random1$1199 =
    $$azimuth$telemetry$simple_test$Random$$next_u64(random$1197);
  int64_t timestamp2$1200 =
    $$azimuth$telemetry$simple_test$Clock$$now_unix_nanos(clock$1196);
  uint64_t random2$1201 =
    $$azimuth$telemetry$simple_test$Random$$next_u64(random$1197);
  int32_t _tmp$2733 = timestamp1$1198 > 0ll;
  moonbit_string_t _tmp$2734 = 0;
  struct moonbit_result_0 _tmp$3549 =
    $moonbitlang$core$builtin$assert_true(
      _tmp$2733, _tmp$2734, (moonbit_string_t)moonbit_string_literal_14.data
    );
  int32_t _tmp$2737;
  moonbit_string_t _tmp$2738;
  struct moonbit_result_0 _tmp$3551;
  moonbit_string_t _tmp$2741;
  struct moonbit_result_0 _tmp$3553;
  moonbit_string_t _tmp$2744;
  if (_tmp$3549.tag) {
    int32_t const _ok$2735 = _tmp$3549.data.ok;
  } else {
    void* const _err$2736 = _tmp$3549.data.err;
    struct moonbit_result_0 _result$3550;
    _result$3550.tag = 0;
    _result$3550.data.err = _err$2736;
    return _result$3550;
  }
  _tmp$2737 = timestamp2$1200 >= timestamp1$1198;
  _tmp$2738 = 0;
  _tmp$3551
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2737, _tmp$2738, (moonbit_string_t)moonbit_string_literal_15.data
  );
  if (_tmp$3551.tag) {
    int32_t const _ok$2739 = _tmp$3551.data.ok;
  } else {
    void* const _err$2740 = _tmp$3551.data.err;
    struct moonbit_result_0 _result$3552;
    _result$3552.tag = 0;
    _result$3552.data.err = _err$2740;
    return _result$3552;
  }
  _tmp$2741 = 0;
  _tmp$3553
  = $moonbitlang$core$builtin$assert_eq$4(
    random1$1199,
      12345ull,
      _tmp$2741,
      (moonbit_string_t)moonbit_string_literal_16.data
  );
  if (_tmp$3553.tag) {
    int32_t const _ok$2742 = _tmp$3553.data.ok;
  } else {
    void* const _err$2743 = _tmp$3553.data.err;
    struct moonbit_result_0 _result$3554;
    _result$3554.tag = 0;
    _result$3554.data.err = _err$2743;
    return _result$3554;
  }
  _tmp$2744 = 0;
  return $moonbitlang$core$builtin$assert_eq$4(
           random2$1201,
             12345ull,
             _tmp$2744,
             (moonbit_string_t)moonbit_string_literal_17.data
         );
}

int32_t $$azimuth$telemetry$simple_test$Random$$system() {
  return 0;
}

uint64_t $$azimuth$telemetry$simple_test$Random$$next_u64(
  int32_t random$1195
) {
  return 12345ull;
}

int32_t $$azimuth$telemetry$simple_test$Clock$$system() {
  return 0;
}

int64_t $$azimuth$telemetry$simple_test$Clock$$now_unix_nanos(
  int32_t clock$1194
) {
  return 1735689600000000000ll;
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8(
  
) {
  struct $$3c$String$2a$String$3e$* _tuple$2731 =
    (struct $$3c$String$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$String$3e$)
    );
  struct $$3c$String$2a$String$3e$* _tuple$2732;
  struct $$3c$String$2a$String$3e$** _tmp$2730;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$1190;
  void* None$2729;
  struct $HttpRequest* request$1191;
  moonbit_string_t _tmp$2706;
  moonbit_string_t _tmp$2707;
  struct moonbit_result_0 _tmp$3555;
  moonbit_string_t _tmp$2710;
  moonbit_string_t _tmp$2711;
  struct moonbit_result_0 _tmp$3557;
  moonbit_string_t _tmp$2714;
  moonbit_string_t _tmp$2715;
  moonbit_string_t _tmp$2716;
  struct moonbit_result_0 _tmp$3559;
  struct $$3c$String$2a$String$3e$* _tuple$2728;
  struct $$3c$String$2a$String$3e$** _tmp$2727;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* response_headers$1192;
  void* None$2726;
  struct $HttpResponse* response$1193;
  int32_t _tmp$2719;
  moonbit_string_t _tmp$2720;
  struct moonbit_result_0 _tmp$3561;
  moonbit_string_t _tmp$2723;
  moonbit_string_t _tmp$2724;
  moonbit_string_t _tmp$2725;
  Moonbit_object_header(_tuple$2731)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$String$3e$, $0) >> 2, 2, 0
  );
  _tuple$2731->$0 = (moonbit_string_t)moonbit_string_literal_18.data;
  _tuple$2731->$1 = (moonbit_string_t)moonbit_string_literal_19.data;
  _tuple$2732
  = (struct $$3c$String$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$String$3e$)
    );
  Moonbit_object_header(_tuple$2732)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$String$3e$, $0) >> 2, 2, 0
  );
  _tuple$2732->$0 = (moonbit_string_t)moonbit_string_literal_20.data;
  _tuple$2732->$1 = (moonbit_string_t)moonbit_string_literal_21.data;
  _tmp$2730
  = (struct $$3c$String$2a$String$3e$**)moonbit_make_ref_array_raw(2);
  _tmp$2730[0] = _tuple$2731;
  _tmp$2730[1] = _tuple$2732;
  headers$1190
  = (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$
      )
    );
  Moonbit_object_header(headers$1190)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  headers$1190->$0 = _tmp$2730;
  headers$1190->$1 = 2;
  None$2729 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  request$1191
  = $$azimuth$telemetry$simple_test$HttpRequest$$new(
    (moonbit_string_t)moonbit_string_literal_22.data,
      (moonbit_string_t)moonbit_string_literal_23.data,
      headers$1190,
      None$2729
  );
  moonbit_incref(request$1191);
  _tmp$2706
  = $$azimuth$telemetry$simple_test$HttpRequest$$http_method(
    request$1191
  );
  _tmp$2707 = 0;
  _tmp$3555
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2706,
      (moonbit_string_t)moonbit_string_literal_22.data,
      _tmp$2707,
      (moonbit_string_t)moonbit_string_literal_24.data
  );
  if (_tmp$3555.tag) {
    int32_t const _ok$2708 = _tmp$3555.data.ok;
  } else {
    void* const _err$2709 = _tmp$3555.data.err;
    struct moonbit_result_0 _result$3556;
    moonbit_decref(request$1191);
    _result$3556.tag = 0;
    _result$3556.data.err = _err$2709;
    return _result$3556;
  }
  moonbit_incref(request$1191);
  _tmp$2710 = $$azimuth$telemetry$simple_test$HttpRequest$$url(request$1191);
  _tmp$2711 = 0;
  _tmp$3557
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2710,
      (moonbit_string_t)moonbit_string_literal_23.data,
      _tmp$2711,
      (moonbit_string_t)moonbit_string_literal_25.data
  );
  if (_tmp$3557.tag) {
    int32_t const _ok$2712 = _tmp$3557.data.ok;
  } else {
    void* const _err$2713 = _tmp$3557.data.err;
    struct moonbit_result_0 _result$3558;
    moonbit_decref(request$1191);
    _result$3558.tag = 0;
    _result$3558.data.err = _err$2713;
    return _result$3558;
  }
  _tmp$2714 = $$azimuth$telemetry$simple_test$HttpRequest$$body(request$1191);
  _tmp$2715 = 0;
  _tmp$2716 = 0;
  _tmp$3559
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$2714,
      _tmp$2715,
      _tmp$2716,
      (moonbit_string_t)moonbit_string_literal_26.data
  );
  if (_tmp$3559.tag) {
    int32_t const _ok$2717 = _tmp$3559.data.ok;
  } else {
    void* const _err$2718 = _tmp$3559.data.err;
    struct moonbit_result_0 _result$3560;
    _result$3560.tag = 0;
    _result$3560.data.err = _err$2718;
    return _result$3560;
  }
  _tuple$2728
  = (struct $$3c$String$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$String$3e$)
    );
  Moonbit_object_header(_tuple$2728)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$String$3e$, $0) >> 2, 2, 0
  );
  _tuple$2728->$0 = (moonbit_string_t)moonbit_string_literal_18.data;
  _tuple$2728->$1 = (moonbit_string_t)moonbit_string_literal_19.data;
  _tmp$2727
  = (struct $$3c$String$2a$String$3e$**)moonbit_make_ref_array_raw(1);
  _tmp$2727[0] = _tuple$2728;
  response_headers$1192
  = (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$
      )
    );
  Moonbit_object_header(response_headers$1192)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  response_headers$1192->$0 = _tmp$2727;
  response_headers$1192->$1 = 1;
  None$2726 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  response$1193
  = $$azimuth$telemetry$simple_test$HttpResponse$$new(
    200, response_headers$1192, None$2726
  );
  moonbit_incref(response$1193);
  _tmp$2719
  = $$azimuth$telemetry$simple_test$HttpResponse$$status_code(
    response$1193
  );
  _tmp$2720 = 0;
  _tmp$3561
  = $moonbitlang$core$builtin$assert_eq$3(
    _tmp$2719,
      200,
      _tmp$2720,
      (moonbit_string_t)moonbit_string_literal_27.data
  );
  if (_tmp$3561.tag) {
    int32_t const _ok$2721 = _tmp$3561.data.ok;
  } else {
    void* const _err$2722 = _tmp$3561.data.err;
    struct moonbit_result_0 _result$3562;
    moonbit_decref(response$1193);
    _result$3562.tag = 0;
    _result$3562.data.err = _err$2722;
    return _result$3562;
  }
  _tmp$2723
  = $$azimuth$telemetry$simple_test$HttpResponse$$body(
    response$1193
  );
  _tmp$2724 = 0;
  _tmp$2725 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           _tmp$2723,
             _tmp$2724,
             _tmp$2725,
             (moonbit_string_t)moonbit_string_literal_28.data
         );
}

int32_t $$azimuth$telemetry$simple_test$HttpResponse$$status_code(
  struct $HttpResponse* response$1189
) {
  int32_t _field$3022 = response$1189->$0;
  moonbit_decref(response$1189);
  return _field$3022;
}

struct $HttpResponse* $$azimuth$telemetry$simple_test$HttpResponse$$new(
  int32_t status_code$1187,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$1188,
  void* body$opt$1185
) {
  moonbit_string_t body$1184;
  switch (Moonbit_object_tag(body$opt$1185)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$1186 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)body$opt$1185;
      moonbit_string_t _field$3023 = _Some$1186->$0;
      int32_t _cnt$3347 = Moonbit_object_header(_Some$1186)->rc;
      if (_cnt$3347 > 1) {
        int32_t _new_cnt$3348;
        if (_field$3023) {
          moonbit_incref(_field$3023);
        }
        _new_cnt$3348 = _cnt$3347 - 1;
        Moonbit_object_header(_Some$1186)->rc = _new_cnt$3348;
      } else if (_cnt$3347 == 1) {
        moonbit_free(_Some$1186);
      }
      body$1184 = _field$3023;
      break;
    }
    default: {
      moonbit_decref(body$opt$1185);
      body$1184 = 0;
      break;
    }
  }
  return $$azimuth$telemetry$simple_test$HttpResponse$$new$inner(
           status_code$1187, headers$1188, body$1184
         );
}

struct $HttpResponse* $$azimuth$telemetry$simple_test$HttpResponse$$new$inner(
  int32_t status_code$1181,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$1182,
  moonbit_string_t body$1183
) {
  struct $HttpResponse* _block$3563 =
    (struct $HttpResponse*)moonbit_malloc(sizeof(struct $HttpResponse));
  Moonbit_object_header(_block$3563)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $HttpResponse, $1) >> 2, 2, 0
  );
  _block$3563->$0 = status_code$1181;
  _block$3563->$1 = headers$1182;
  _block$3563->$2 = body$1183;
  return _block$3563;
}

moonbit_string_t $$azimuth$telemetry$simple_test$HttpResponse$$body(
  struct $HttpResponse* response$1180
) {
  moonbit_string_t _field$3024 = response$1180->$2;
  int32_t _cnt$3349 = Moonbit_object_header(response$1180)->rc;
  if (_cnt$3349 > 1) {
    int32_t _new_cnt$3351;
    if (_field$3024) {
      moonbit_incref(_field$3024);
    }
    _new_cnt$3351 = _cnt$3349 - 1;
    Moonbit_object_header(response$1180)->rc = _new_cnt$3351;
  } else if (_cnt$3349 == 1) {
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$3350 =
      response$1180->$1;
    moonbit_decref(_field$3350);
    moonbit_free(response$1180);
  }
  return _field$3024;
}

moonbit_string_t $$azimuth$telemetry$simple_test$HttpRequest$$url(
  struct $HttpRequest* request$1179
) {
  moonbit_string_t _field$3025 = request$1179->$1;
  int32_t _cnt$3352 = Moonbit_object_header(request$1179)->rc;
  if (_cnt$3352 > 1) {
    int32_t _new_cnt$3356;
    moonbit_incref(_field$3025);
    _new_cnt$3356 = _cnt$3352 - 1;
    Moonbit_object_header(request$1179)->rc = _new_cnt$3356;
  } else if (_cnt$3352 == 1) {
    moonbit_string_t _field$3355 = request$1179->$3;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$3354;
    moonbit_string_t _field$3353;
    if (_field$3355) {
      moonbit_decref(_field$3355);
    }
    _field$3354 = request$1179->$2;
    moonbit_decref(_field$3354);
    _field$3353 = request$1179->$0;
    moonbit_decref(_field$3353);
    moonbit_free(request$1179);
  }
  return _field$3025;
}

struct $HttpRequest* $$azimuth$telemetry$simple_test$HttpRequest$$new(
  moonbit_string_t http_method$1176,
  moonbit_string_t url$1177,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$1178,
  void* body$opt$1174
) {
  moonbit_string_t body$1173;
  switch (Moonbit_object_tag(body$opt$1174)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$1175 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)body$opt$1174;
      moonbit_string_t _field$3026 = _Some$1175->$0;
      int32_t _cnt$3357 = Moonbit_object_header(_Some$1175)->rc;
      if (_cnt$3357 > 1) {
        int32_t _new_cnt$3358;
        if (_field$3026) {
          moonbit_incref(_field$3026);
        }
        _new_cnt$3358 = _cnt$3357 - 1;
        Moonbit_object_header(_Some$1175)->rc = _new_cnt$3358;
      } else if (_cnt$3357 == 1) {
        moonbit_free(_Some$1175);
      }
      body$1173 = _field$3026;
      break;
    }
    default: {
      moonbit_decref(body$opt$1174);
      body$1173 = 0;
      break;
    }
  }
  return $$azimuth$telemetry$simple_test$HttpRequest$$new$inner(
           http_method$1176, url$1177, headers$1178, body$1173
         );
}

struct $HttpRequest* $$azimuth$telemetry$simple_test$HttpRequest$$new$inner(
  moonbit_string_t http_method$1169,
  moonbit_string_t url$1170,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* headers$1171,
  moonbit_string_t body$1172
) {
  struct $HttpRequest* _block$3564 =
    (struct $HttpRequest*)moonbit_malloc(sizeof(struct $HttpRequest));
  Moonbit_object_header(_block$3564)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $HttpRequest, $0) >> 2, 4, 0
  );
  _block$3564->$0 = http_method$1169;
  _block$3564->$1 = url$1170;
  _block$3564->$2 = headers$1171;
  _block$3564->$3 = body$1172;
  return _block$3564;
}

moonbit_string_t $$azimuth$telemetry$simple_test$HttpRequest$$http_method(
  struct $HttpRequest* request$1168
) {
  moonbit_string_t _field$3027 = request$1168->$0;
  int32_t _cnt$3359 = Moonbit_object_header(request$1168)->rc;
  if (_cnt$3359 > 1) {
    int32_t _new_cnt$3363;
    moonbit_incref(_field$3027);
    _new_cnt$3363 = _cnt$3359 - 1;
    Moonbit_object_header(request$1168)->rc = _new_cnt$3363;
  } else if (_cnt$3359 == 1) {
    moonbit_string_t _field$3362 = request$1168->$3;
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$3361;
    moonbit_string_t _field$3360;
    if (_field$3362) {
      moonbit_decref(_field$3362);
    }
    _field$3361 = request$1168->$2;
    moonbit_decref(_field$3361);
    _field$3360 = request$1168->$1;
    moonbit_decref(_field$3360);
    moonbit_free(request$1168);
  }
  return _field$3027;
}

moonbit_string_t $$azimuth$telemetry$simple_test$HttpRequest$$body(
  struct $HttpRequest* request$1167
) {
  moonbit_string_t _field$3028 = request$1167->$3;
  int32_t _cnt$3364 = Moonbit_object_header(request$1167)->rc;
  if (_cnt$3364 > 1) {
    int32_t _new_cnt$3368;
    if (_field$3028) {
      moonbit_incref(_field$3028);
    }
    _new_cnt$3368 = _cnt$3364 - 1;
    Moonbit_object_header(request$1167)->rc = _new_cnt$3368;
  } else if (_cnt$3364 == 1) {
    struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$3367 =
      request$1167->$2;
    moonbit_string_t _field$3366;
    moonbit_string_t _field$3365;
    moonbit_decref(_field$3367);
    _field$3366 = request$1167->$1;
    moonbit_decref(_field$3366);
    _field$3365 = request$1167->$0;
    moonbit_decref(_field$3365);
    moonbit_free(request$1167);
  }
  return _field$3028;
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7(
  
) {
  struct $TextMapCarrier* carrier$1163 =
    $$azimuth$telemetry$simple_test$TextMapCarrier$$new();
  moonbit_string_t traceparent$1164;
  moonbit_string_t _tmp$3029;
  moonbit_string_t missing$1166;
  moonbit_string_t _tmp$2700;
  moonbit_string_t _tmp$2701;
  struct moonbit_result_0 _tmp$3565;
  moonbit_string_t _tmp$2704;
  moonbit_string_t _tmp$2705;
  moonbit_incref(carrier$1163);
  $$azimuth$telemetry$simple_test$TextMapCarrier$$set(
    carrier$1163,
      (moonbit_string_t)moonbit_string_literal_29.data,
      (moonbit_string_t)moonbit_string_literal_30.data
  );
  moonbit_incref(carrier$1163);
  $$azimuth$telemetry$simple_test$TextMapCarrier$$set(
    carrier$1163,
      (moonbit_string_t)moonbit_string_literal_31.data,
      (moonbit_string_t)moonbit_string_literal_32.data
  );
  moonbit_incref(carrier$1163);
  traceparent$1164
  = $$azimuth$telemetry$simple_test$TextMapCarrier$$get(
    carrier$1163, (moonbit_string_t)moonbit_string_literal_29.data
  );
  moonbit_incref(carrier$1163);
  _tmp$3029
  = $$azimuth$telemetry$simple_test$TextMapCarrier$$get(
    carrier$1163, (moonbit_string_t)moonbit_string_literal_31.data
  );
  if (_tmp$3029) {
    moonbit_decref(_tmp$3029);
  }
  missing$1166
  = $$azimuth$telemetry$simple_test$TextMapCarrier$$get(
    carrier$1163, (moonbit_string_t)moonbit_string_literal_33.data
  );
  _tmp$2700 = (moonbit_string_t)moonbit_string_literal_34.data;
  _tmp$2701 = 0;
  _tmp$3565
  = $moonbitlang$core$builtin$assert_eq$1(
    traceparent$1164,
      _tmp$2700,
      _tmp$2701,
      (moonbit_string_t)moonbit_string_literal_35.data
  );
  if (_tmp$3565.tag) {
    int32_t const _ok$2702 = _tmp$3565.data.ok;
  } else {
    void* const _err$2703 = _tmp$3565.data.err;
    struct moonbit_result_0 _result$3566;
    if (missing$1166) {
      moonbit_decref(missing$1166);
    }
    _result$3566.tag = 0;
    _result$3566.data.err = _err$2703;
    return _result$3566;
  }
  _tmp$2704 = 0;
  _tmp$2705 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           missing$1166,
             _tmp$2704,
             _tmp$2705,
             (moonbit_string_t)moonbit_string_literal_36.data
         );
}

int32_t $$azimuth$telemetry$simple_test$TextMapCarrier$$set(
  struct $TextMapCarrier* carrier$1160,
  moonbit_string_t key$1161,
  moonbit_string_t value$1162
) {
  moonbit_decref(value$1162);
  moonbit_decref(key$1161);
  moonbit_decref(carrier$1160);
  return 0;
}

struct $TextMapCarrier* $$azimuth$telemetry$simple_test$TextMapCarrier$$new() {
  struct $$3c$String$2a$String$3e$** _tmp$2699 =
    (struct $$3c$String$2a$String$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _tmp$2698 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$
      )
    );
  struct $TextMapCarrier* _block$3567;
  Moonbit_object_header(_tmp$2698)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  _tmp$2698->$0 = _tmp$2699;
  _tmp$2698->$1 = 0;
  _block$3567
  = (struct $TextMapCarrier*)moonbit_malloc(sizeof(struct $TextMapCarrier));
  Moonbit_object_header(_block$3567)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $TextMapCarrier, $0) >> 2, 1, 0
  );
  _block$3567->$0 = _tmp$2698;
  return _block$3567;
}

moonbit_string_t $$azimuth$telemetry$simple_test$TextMapCarrier$$get(
  struct $TextMapCarrier* carrier$1159,
  moonbit_string_t key$1158
) {
  int32_t _tmp$3030;
  moonbit_decref(carrier$1159);
  _tmp$3030
  = moonbit_val_array_equal(
    key$1158, (moonbit_string_t)moonbit_string_literal_29.data
  );
  moonbit_decref(key$1158);
  if (_tmp$3030) {
    return (moonbit_string_t)moonbit_string_literal_34.data;
  } else {
    return 0;
  }
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6(
  
) {
  struct $Baggage* baggage$1155 =
    $$azimuth$telemetry$simple_test$Baggage$$new();
  struct $Baggage* with_entry1$1156 =
    $$azimuth$telemetry$simple_test$Baggage$$set_entry(
      baggage$1155,
        (moonbit_string_t)moonbit_string_literal_37.data,
        (moonbit_string_t)moonbit_string_literal_38.data
    );
  struct $Baggage* with_entry2$1157 =
    $$azimuth$telemetry$simple_test$Baggage$$set_entry(
      with_entry1$1156,
        (moonbit_string_t)moonbit_string_literal_39.data,
        (moonbit_string_t)moonbit_string_literal_40.data
    );
  moonbit_string_t _tmp$2685;
  moonbit_string_t _tmp$2686;
  moonbit_string_t _tmp$2687;
  struct moonbit_result_0 _tmp$3568;
  moonbit_string_t _tmp$2690;
  moonbit_string_t _tmp$2691;
  moonbit_string_t _tmp$2692;
  struct moonbit_result_0 _tmp$3570;
  moonbit_string_t _tmp$2695;
  moonbit_string_t _tmp$2696;
  moonbit_string_t _tmp$2697;
  moonbit_incref(with_entry2$1157);
  _tmp$2685
  = $$azimuth$telemetry$simple_test$Baggage$$get_entry(
    with_entry2$1157, (moonbit_string_t)moonbit_string_literal_37.data
  );
  _tmp$2686 = 0;
  _tmp$2687 = 0;
  _tmp$3568
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$2685,
      _tmp$2686,
      _tmp$2687,
      (moonbit_string_t)moonbit_string_literal_41.data
  );
  if (_tmp$3568.tag) {
    int32_t const _ok$2688 = _tmp$3568.data.ok;
  } else {
    void* const _err$2689 = _tmp$3568.data.err;
    struct moonbit_result_0 _result$3569;
    moonbit_decref(with_entry2$1157);
    _result$3569.tag = 0;
    _result$3569.data.err = _err$2689;
    return _result$3569;
  }
  moonbit_incref(with_entry2$1157);
  _tmp$2690
  = $$azimuth$telemetry$simple_test$Baggage$$get_entry(
    with_entry2$1157, (moonbit_string_t)moonbit_string_literal_39.data
  );
  _tmp$2691 = 0;
  _tmp$2692 = 0;
  _tmp$3570
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$2690,
      _tmp$2691,
      _tmp$2692,
      (moonbit_string_t)moonbit_string_literal_42.data
  );
  if (_tmp$3570.tag) {
    int32_t const _ok$2693 = _tmp$3570.data.ok;
  } else {
    void* const _err$2694 = _tmp$3570.data.err;
    struct moonbit_result_0 _result$3571;
    moonbit_decref(with_entry2$1157);
    _result$3571.tag = 0;
    _result$3571.data.err = _err$2694;
    return _result$3571;
  }
  _tmp$2695
  = $$azimuth$telemetry$simple_test$Baggage$$get_entry(
    with_entry2$1157, (moonbit_string_t)moonbit_string_literal_43.data
  );
  _tmp$2696 = 0;
  _tmp$2697 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           _tmp$2695,
             _tmp$2696,
             _tmp$2697,
             (moonbit_string_t)moonbit_string_literal_44.data
         );
}

struct $Baggage* $$azimuth$telemetry$simple_test$Baggage$$set_entry(
  struct $Baggage* baggage$1152,
  moonbit_string_t key$1153,
  moonbit_string_t value$1154
) {
  moonbit_decref(value$1154);
  moonbit_decref(key$1153);
  return baggage$1152;
}

struct $Baggage* $$azimuth$telemetry$simple_test$Baggage$$new() {
  struct $$3c$String$2a$String$3e$** _tmp$2684 =
    (struct $$3c$String$2a$String$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _tmp$2683 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$
      )
    );
  struct $Baggage* _block$3572;
  Moonbit_object_header(_tmp$2683)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  _tmp$2683->$0 = _tmp$2684;
  _tmp$2683->$1 = 0;
  _block$3572 = (struct $Baggage*)moonbit_malloc(sizeof(struct $Baggage));
  Moonbit_object_header(_block$3572)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Baggage, $0) >> 2, 1, 0
  );
  _block$3572->$0 = _tmp$2683;
  return _block$3572;
}

moonbit_string_t $$azimuth$telemetry$simple_test$Baggage$$get_entry(
  struct $Baggage* baggage$1146,
  moonbit_string_t key$1150
) {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _field$3034 =
    baggage$1146->$0;
  int32_t _cnt$3369 = Moonbit_object_header(baggage$1146)->rc;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* _arr$1145;
  int32_t _len$1147;
  int32_t _i$1148;
  if (_cnt$3369 > 1) {
    int32_t _new_cnt$3370;
    moonbit_incref(_field$3034);
    _new_cnt$3370 = _cnt$3369 - 1;
    Moonbit_object_header(baggage$1146)->rc = _new_cnt$3370;
  } else if (_cnt$3369 == 1) {
    moonbit_free(baggage$1146);
  }
  _arr$1145 = _field$3034;
  moonbit_incref(_arr$1145);
  _len$1147 = $$moonbitlang$core$builtin$Array$$length$3(_arr$1145);
  _i$1148 = 0;
  while (1) {
    if (_i$1148 < _len$1147) {
      struct $$3c$String$2a$String$3e$* entry$1149;
      moonbit_string_t _field$3033;
      moonbit_string_t _tmp$2680;
      int32_t _tmp$3032;
      int32_t _tmp$2682;
      moonbit_incref(_arr$1145);
      entry$1149
      = $$moonbitlang$core$builtin$Array$$unsafe_get$3(
        _arr$1145, _i$1148
      );
      _field$3033 = entry$1149->$0;
      _tmp$2680 = _field$3033;
      _tmp$3032 = moonbit_val_array_equal(_tmp$2680, key$1150);
      if (_tmp$3032) {
        moonbit_string_t _field$3031;
        moonbit_string_t _tmp$2681;
        moonbit_decref(key$1150);
        moonbit_decref(_arr$1145);
        _field$3031 = entry$1149->$1;
        moonbit_incref(_field$3031);
        moonbit_decref(entry$1149);
        _tmp$2681 = _field$3031;
        return _tmp$2681;
      } else {
        moonbit_decref(entry$1149);
      }
      _tmp$2682 = _i$1148 + 1;
      _i$1148 = _tmp$2682;
      continue;
    } else {
      moonbit_decref(key$1150);
      moonbit_decref(_arr$1145);
      return 0;
    }
    break;
  }
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5(
  
) {
  struct $LogRecord* trace_log$1133 =
    $$azimuth$telemetry$simple_test$LogRecord$$new(
      0, (moonbit_string_t)moonbit_string_literal_45.data
    );
  struct $LogRecord* debug_log$1134 =
    $$azimuth$telemetry$simple_test$LogRecord$$new(
      1, (moonbit_string_t)moonbit_string_literal_46.data
    );
  struct $LogRecord* info_log$1135 =
    $$azimuth$telemetry$simple_test$LogRecord$$new(
      2, (moonbit_string_t)moonbit_string_literal_47.data
    );
  struct $LogRecord* warn_log$1136 =
    $$azimuth$telemetry$simple_test$LogRecord$$new(
      3, (moonbit_string_t)moonbit_string_literal_48.data
    );
  struct $LogRecord* error_log$1137 =
    $$azimuth$telemetry$simple_test$LogRecord$$new(
      4, (moonbit_string_t)moonbit_string_literal_49.data
    );
  struct $LogRecord* fatal_log$1138 =
    $$azimuth$telemetry$simple_test$LogRecord$$new(
      5, (moonbit_string_t)moonbit_string_literal_50.data
    );
  int32_t _bind$1139 =
    $$azimuth$telemetry$simple_test$LogRecord$$severity_number(
      trace_log$1133
    );
  int32_t _bind$1140;
  int32_t _bind$1141;
  int32_t _bind$1142;
  int32_t _bind$1143;
  int32_t _bind$1144;
  switch (_bind$1139) {
    case 0: {
      moonbit_string_t _tmp$2648 = 0;
      struct moonbit_result_0 _tmp$3574 =
        $moonbitlang$core$builtin$assert_true(
          1, _tmp$2648, (moonbit_string_t)moonbit_string_literal_51.data
        );
      if (_tmp$3574.tag) {
        int32_t const _ok$2649 = _tmp$3574.data.ok;
      } else {
        void* const _err$2650 = _tmp$3574.data.err;
        struct moonbit_result_0 _result$3575;
        moonbit_decref(fatal_log$1138);
        moonbit_decref(error_log$1137);
        moonbit_decref(warn_log$1136);
        moonbit_decref(info_log$1135);
        moonbit_decref(debug_log$1134);
        _result$3575.tag = 0;
        _result$3575.data.err = _err$2650;
        return _result$3575;
      }
      break;
    }
    default: {
      moonbit_string_t _tmp$2651 = 0;
      struct moonbit_result_0 _tmp$3576 =
        $moonbitlang$core$builtin$assert_false(
          1, _tmp$2651, (moonbit_string_t)moonbit_string_literal_52.data
        );
      if (_tmp$3576.tag) {
        int32_t const _ok$2652 = _tmp$3576.data.ok;
      } else {
        void* const _err$2653 = _tmp$3576.data.err;
        struct moonbit_result_0 _result$3577;
        moonbit_decref(fatal_log$1138);
        moonbit_decref(error_log$1137);
        moonbit_decref(warn_log$1136);
        moonbit_decref(info_log$1135);
        moonbit_decref(debug_log$1134);
        _result$3577.tag = 0;
        _result$3577.data.err = _err$2653;
        return _result$3577;
      }
      break;
    }
  }
  _bind$1140
  = $$azimuth$telemetry$simple_test$LogRecord$$severity_number(
    debug_log$1134
  );
  switch (_bind$1140) {
    case 1: {
      moonbit_string_t _tmp$2654 = 0;
      struct moonbit_result_0 _tmp$3578 =
        $moonbitlang$core$builtin$assert_true(
          1, _tmp$2654, (moonbit_string_t)moonbit_string_literal_53.data
        );
      if (_tmp$3578.tag) {
        int32_t const _ok$2655 = _tmp$3578.data.ok;
      } else {
        void* const _err$2656 = _tmp$3578.data.err;
        struct moonbit_result_0 _result$3579;
        moonbit_decref(fatal_log$1138);
        moonbit_decref(error_log$1137);
        moonbit_decref(warn_log$1136);
        moonbit_decref(info_log$1135);
        _result$3579.tag = 0;
        _result$3579.data.err = _err$2656;
        return _result$3579;
      }
      break;
    }
    default: {
      moonbit_string_t _tmp$2657 = 0;
      struct moonbit_result_0 _tmp$3580 =
        $moonbitlang$core$builtin$assert_false(
          1, _tmp$2657, (moonbit_string_t)moonbit_string_literal_54.data
        );
      if (_tmp$3580.tag) {
        int32_t const _ok$2658 = _tmp$3580.data.ok;
      } else {
        void* const _err$2659 = _tmp$3580.data.err;
        struct moonbit_result_0 _result$3581;
        moonbit_decref(fatal_log$1138);
        moonbit_decref(error_log$1137);
        moonbit_decref(warn_log$1136);
        moonbit_decref(info_log$1135);
        _result$3581.tag = 0;
        _result$3581.data.err = _err$2659;
        return _result$3581;
      }
      break;
    }
  }
  _bind$1141
  = $$azimuth$telemetry$simple_test$LogRecord$$severity_number(
    info_log$1135
  );
  switch (_bind$1141) {
    case 2: {
      moonbit_string_t _tmp$2660 = 0;
      struct moonbit_result_0 _tmp$3582 =
        $moonbitlang$core$builtin$assert_true(
          1, _tmp$2660, (moonbit_string_t)moonbit_string_literal_55.data
        );
      if (_tmp$3582.tag) {
        int32_t const _ok$2661 = _tmp$3582.data.ok;
      } else {
        void* const _err$2662 = _tmp$3582.data.err;
        struct moonbit_result_0 _result$3583;
        moonbit_decref(fatal_log$1138);
        moonbit_decref(error_log$1137);
        moonbit_decref(warn_log$1136);
        _result$3583.tag = 0;
        _result$3583.data.err = _err$2662;
        return _result$3583;
      }
      break;
    }
    default: {
      moonbit_string_t _tmp$2663 = 0;
      struct moonbit_result_0 _tmp$3584 =
        $moonbitlang$core$builtin$assert_false(
          1, _tmp$2663, (moonbit_string_t)moonbit_string_literal_56.data
        );
      if (_tmp$3584.tag) {
        int32_t const _ok$2664 = _tmp$3584.data.ok;
      } else {
        void* const _err$2665 = _tmp$3584.data.err;
        struct moonbit_result_0 _result$3585;
        moonbit_decref(fatal_log$1138);
        moonbit_decref(error_log$1137);
        moonbit_decref(warn_log$1136);
        _result$3585.tag = 0;
        _result$3585.data.err = _err$2665;
        return _result$3585;
      }
      break;
    }
  }
  _bind$1142
  = $$azimuth$telemetry$simple_test$LogRecord$$severity_number(
    warn_log$1136
  );
  switch (_bind$1142) {
    case 3: {
      moonbit_string_t _tmp$2666 = 0;
      struct moonbit_result_0 _tmp$3586 =
        $moonbitlang$core$builtin$assert_true(
          1, _tmp$2666, (moonbit_string_t)moonbit_string_literal_57.data
        );
      if (_tmp$3586.tag) {
        int32_t const _ok$2667 = _tmp$3586.data.ok;
      } else {
        void* const _err$2668 = _tmp$3586.data.err;
        struct moonbit_result_0 _result$3587;
        moonbit_decref(fatal_log$1138);
        moonbit_decref(error_log$1137);
        _result$3587.tag = 0;
        _result$3587.data.err = _err$2668;
        return _result$3587;
      }
      break;
    }
    default: {
      moonbit_string_t _tmp$2669 = 0;
      struct moonbit_result_0 _tmp$3588 =
        $moonbitlang$core$builtin$assert_false(
          1, _tmp$2669, (moonbit_string_t)moonbit_string_literal_58.data
        );
      if (_tmp$3588.tag) {
        int32_t const _ok$2670 = _tmp$3588.data.ok;
      } else {
        void* const _err$2671 = _tmp$3588.data.err;
        struct moonbit_result_0 _result$3589;
        moonbit_decref(fatal_log$1138);
        moonbit_decref(error_log$1137);
        _result$3589.tag = 0;
        _result$3589.data.err = _err$2671;
        return _result$3589;
      }
      break;
    }
  }
  _bind$1143
  = $$azimuth$telemetry$simple_test$LogRecord$$severity_number(
    error_log$1137
  );
  switch (_bind$1143) {
    case 4: {
      moonbit_string_t _tmp$2672 = 0;
      struct moonbit_result_0 _tmp$3590 =
        $moonbitlang$core$builtin$assert_true(
          1, _tmp$2672, (moonbit_string_t)moonbit_string_literal_59.data
        );
      if (_tmp$3590.tag) {
        int32_t const _ok$2673 = _tmp$3590.data.ok;
      } else {
        void* const _err$2674 = _tmp$3590.data.err;
        struct moonbit_result_0 _result$3591;
        moonbit_decref(fatal_log$1138);
        _result$3591.tag = 0;
        _result$3591.data.err = _err$2674;
        return _result$3591;
      }
      break;
    }
    default: {
      moonbit_string_t _tmp$2675 = 0;
      struct moonbit_result_0 _tmp$3592 =
        $moonbitlang$core$builtin$assert_false(
          1, _tmp$2675, (moonbit_string_t)moonbit_string_literal_60.data
        );
      if (_tmp$3592.tag) {
        int32_t const _ok$2676 = _tmp$3592.data.ok;
      } else {
        void* const _err$2677 = _tmp$3592.data.err;
        struct moonbit_result_0 _result$3593;
        moonbit_decref(fatal_log$1138);
        _result$3593.tag = 0;
        _result$3593.data.err = _err$2677;
        return _result$3593;
      }
      break;
    }
  }
  _bind$1144
  = $$azimuth$telemetry$simple_test$LogRecord$$severity_number(
    fatal_log$1138
  );
  switch (_bind$1144) {
    case 5: {
      moonbit_string_t _tmp$2678 = 0;
      return $moonbitlang$core$builtin$assert_true(
               1, _tmp$2678, (moonbit_string_t)moonbit_string_literal_61.data
             );
      break;
    }
    default: {
      moonbit_string_t _tmp$2679 = 0;
      return $moonbitlang$core$builtin$assert_false(
               1, _tmp$2679, (moonbit_string_t)moonbit_string_literal_62.data
             );
      break;
    }
  }
}

int32_t $$azimuth$telemetry$simple_test$LogRecord$$severity_number(
  struct $LogRecord* record$1132
) {
  int32_t _field$3035 = record$1132->$0;
  moonbit_decref(record$1132);
  return _field$3035;
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4(
  
) {
  struct $Resource* resource$1113 =
    $$azimuth$telemetry$simple_test$Resource$$new();
  void* StringValue$2647 =
    (void*)moonbit_malloc(sizeof(struct $AttributeValue$StringValue));
  struct $$3c$String$2a$AttributeValue$3e$* _tuple$2642;
  void* StringValue$2646;
  struct $$3c$String$2a$AttributeValue$3e$* _tuple$2643;
  void* StringValue$2645;
  struct $$3c$String$2a$AttributeValue$3e$* _tuple$2644;
  struct $$3c$String$2a$AttributeValue$3e$** _tmp$2641;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* base_attrs$1114;
  struct $Resource* resource_with_base$1115;
  moonbit_string_t name$1118;
  void* _bind$1119;
  moonbit_string_t _tmp$2636;
  struct moonbit_result_0 _tmp$3596;
  moonbit_string_t _tmp$2633;
  struct moonbit_result_0 _tmp$3598;
  moonbit_string_t namespace$1126;
  void* _bind$1127;
  moonbit_string_t _tmp$2640;
  moonbit_string_t _tmp$2639;
  Moonbit_object_header(StringValue$2647)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $AttributeValue$StringValue, $0) >> 2, 1, 0
  );
  ((struct $AttributeValue$StringValue*)StringValue$2647)->$0
  = (moonbit_string_t)moonbit_string_literal_63.data;
  _tuple$2642
  = (struct $$3c$String$2a$AttributeValue$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$AttributeValue$3e$)
    );
  Moonbit_object_header(_tuple$2642)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$AttributeValue$3e$, $0) >> 2, 2, 0
  );
  _tuple$2642->$0 = (moonbit_string_t)moonbit_string_literal_64.data;
  _tuple$2642->$1 = StringValue$2647;
  StringValue$2646
  = (void*)moonbit_malloc(sizeof(struct $AttributeValue$StringValue));
  Moonbit_object_header(StringValue$2646)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $AttributeValue$StringValue, $0) >> 2, 1, 0
  );
  ((struct $AttributeValue$StringValue*)StringValue$2646)->$0
  = (moonbit_string_t)moonbit_string_literal_65.data;
  _tuple$2643
  = (struct $$3c$String$2a$AttributeValue$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$AttributeValue$3e$)
    );
  Moonbit_object_header(_tuple$2643)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$AttributeValue$3e$, $0) >> 2, 2, 0
  );
  _tuple$2643->$0 = (moonbit_string_t)moonbit_string_literal_66.data;
  _tuple$2643->$1 = StringValue$2646;
  StringValue$2645
  = (void*)moonbit_malloc(sizeof(struct $AttributeValue$StringValue));
  Moonbit_object_header(StringValue$2645)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $AttributeValue$StringValue, $0) >> 2, 1, 0
  );
  ((struct $AttributeValue$StringValue*)StringValue$2645)->$0
  = (moonbit_string_t)moonbit_string_literal_67.data;
  _tuple$2644
  = (struct $$3c$String$2a$AttributeValue$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$AttributeValue$3e$)
    );
  Moonbit_object_header(_tuple$2644)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$AttributeValue$3e$, $0) >> 2, 2, 0
  );
  _tuple$2644->$0 = (moonbit_string_t)moonbit_string_literal_68.data;
  _tuple$2644->$1 = StringValue$2645;
  _tmp$2641
  = (struct $$3c$String$2a$AttributeValue$3e$**)moonbit_make_ref_array_raw(3);
  _tmp$2641[0] = _tuple$2642;
  _tmp$2641[1] = _tuple$2643;
  _tmp$2641[2] = _tuple$2644;
  base_attrs$1114
  = (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$
      )
    );
  Moonbit_object_header(base_attrs$1114)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  base_attrs$1114->$0 = _tmp$2641;
  base_attrs$1114->$1 = 3;
  resource_with_base$1115
  = $$azimuth$telemetry$simple_test$Resource$$with_attributes(
    resource$1113, base_attrs$1114
  );
  moonbit_incref(resource_with_base$1115);
  _bind$1119
  = $$azimuth$telemetry$simple_test$Resource$$get_attribute(
    resource_with_base$1115, (moonbit_string_t)moonbit_string_literal_64.data
  );
  if (_bind$1119 == 0) {
    if (_bind$1119) {
      moonbit_decref(_bind$1119);
    }
    goto $join$1116;
  } else {
    void* _Some$1120 = _bind$1119;
    void* _x$1121 = _Some$1120;
    switch (Moonbit_object_tag(_x$1121)) {
      case 0: {
        struct $AttributeValue$StringValue* _StringValue$1122 =
          (struct $AttributeValue$StringValue*)_x$1121;
        moonbit_string_t _field$3037 = _StringValue$1122->$0;
        int32_t _cnt$3371 = Moonbit_object_header(_StringValue$1122)->rc;
        moonbit_string_t _name$1123;
        if (_cnt$3371 > 1) {
          int32_t _new_cnt$3372;
          moonbit_incref(_field$3037);
          _new_cnt$3372 = _cnt$3371 - 1;
          Moonbit_object_header(_StringValue$1122)->rc = _new_cnt$3372;
        } else if (_cnt$3371 == 1) {
          moonbit_free(_StringValue$1122);
        }
        _name$1123 = _field$3037;
        name$1118 = _name$1123;
        goto $join$1117;
        break;
      }
      default: {
        moonbit_decref(_x$1121);
        goto $join$1116;
        break;
      }
    }
  }
  goto $joinlet$3595;
  $join$1117:;
  _tmp$2636 = 0;
  _tmp$3596
  = $moonbitlang$core$builtin$assert_eq$0(
    name$1118,
      (moonbit_string_t)moonbit_string_literal_63.data,
      _tmp$2636,
      (moonbit_string_t)moonbit_string_literal_69.data
  );
  if (_tmp$3596.tag) {
    int32_t const _ok$2637 = _tmp$3596.data.ok;
  } else {
    void* const _err$2638 = _tmp$3596.data.err;
    struct moonbit_result_0 _result$3597;
    moonbit_decref(resource_with_base$1115);
    _result$3597.tag = 0;
    _result$3597.data.err = _err$2638;
    return _result$3597;
  }
  $joinlet$3595:;
  goto $joinlet$3594;
  $join$1116:;
  _tmp$2633 = 0;
  _tmp$3598
  = $moonbitlang$core$builtin$assert_false(
    1, _tmp$2633, (moonbit_string_t)moonbit_string_literal_70.data
  );
  if (_tmp$3598.tag) {
    int32_t const _ok$2634 = _tmp$3598.data.ok;
  } else {
    void* const _err$2635 = _tmp$3598.data.err;
    struct moonbit_result_0 _result$3599;
    moonbit_decref(resource_with_base$1115);
    _result$3599.tag = 0;
    _result$3599.data.err = _err$2635;
    return _result$3599;
  }
  $joinlet$3594:;
  _bind$1127
  = $$azimuth$telemetry$simple_test$Resource$$get_attribute(
    resource_with_base$1115, (moonbit_string_t)moonbit_string_literal_66.data
  );
  if (_bind$1127 == 0) {
    if (_bind$1127) {
      moonbit_decref(_bind$1127);
    }
    goto $join$1124;
  } else {
    void* _Some$1128 = _bind$1127;
    void* _x$1129 = _Some$1128;
    switch (Moonbit_object_tag(_x$1129)) {
      case 0: {
        struct $AttributeValue$StringValue* _StringValue$1130 =
          (struct $AttributeValue$StringValue*)_x$1129;
        moonbit_string_t _field$3036 = _StringValue$1130->$0;
        int32_t _cnt$3373 = Moonbit_object_header(_StringValue$1130)->rc;
        moonbit_string_t _namespace$1131;
        if (_cnt$3373 > 1) {
          int32_t _new_cnt$3374;
          moonbit_incref(_field$3036);
          _new_cnt$3374 = _cnt$3373 - 1;
          Moonbit_object_header(_StringValue$1130)->rc = _new_cnt$3374;
        } else if (_cnt$3373 == 1) {
          moonbit_free(_StringValue$1130);
        }
        _namespace$1131 = _field$3036;
        namespace$1126 = _namespace$1131;
        goto $join$1125;
        break;
      }
      default: {
        moonbit_decref(_x$1129);
        goto $join$1124;
        break;
      }
    }
  }
  $join$1125:;
  _tmp$2640 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           namespace$1126,
             (moonbit_string_t)moonbit_string_literal_65.data,
             _tmp$2640,
             (moonbit_string_t)moonbit_string_literal_71.data
         );
  $join$1124:;
  _tmp$2639 = 0;
  return $moonbitlang$core$builtin$assert_false(
           1, _tmp$2639, (moonbit_string_t)moonbit_string_literal_72.data
         );
}

struct $Resource* $$azimuth$telemetry$simple_test$Resource$$with_attributes(
  struct $Resource* resource$1112,
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* attributes$1111
) {
  struct $Resource* _block$3602;
  moonbit_decref(resource$1112);
  _block$3602 = (struct $Resource*)moonbit_malloc(sizeof(struct $Resource));
  Moonbit_object_header(_block$3602)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Resource, $0) >> 2, 1, 0
  );
  _block$3602->$0 = attributes$1111;
  return _block$3602;
}

struct $Resource* $$azimuth$telemetry$simple_test$Resource$$new() {
  struct $$3c$String$2a$AttributeValue$3e$** _tmp$2632 =
    (struct $$3c$String$2a$AttributeValue$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* _tmp$2631 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$
      )
    );
  struct $Resource* _block$3603;
  Moonbit_object_header(_tmp$2631)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  _tmp$2631->$0 = _tmp$2632;
  _tmp$2631->$1 = 0;
  _block$3603 = (struct $Resource*)moonbit_malloc(sizeof(struct $Resource));
  Moonbit_object_header(_block$3603)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Resource, $0) >> 2, 1, 0
  );
  _block$3603->$0 = _tmp$2631;
  return _block$3603;
}

void* $$azimuth$telemetry$simple_test$Resource$$get_attribute(
  struct $Resource* resource$1105,
  moonbit_string_t key$1109
) {
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* _field$3041 =
    resource$1105->$0;
  int32_t _cnt$3375 = Moonbit_object_header(resource$1105)->rc;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* _arr$1104;
  int32_t _len$1106;
  int32_t _i$1107;
  if (_cnt$3375 > 1) {
    int32_t _new_cnt$3376;
    moonbit_incref(_field$3041);
    _new_cnt$3376 = _cnt$3375 - 1;
    Moonbit_object_header(resource$1105)->rc = _new_cnt$3376;
  } else if (_cnt$3375 == 1) {
    moonbit_free(resource$1105);
  }
  _arr$1104 = _field$3041;
  moonbit_incref(_arr$1104);
  _len$1106 = $$moonbitlang$core$builtin$Array$$length$2(_arr$1104);
  _i$1107 = 0;
  while (1) {
    if (_i$1107 < _len$1106) {
      struct $$3c$String$2a$AttributeValue$3e$* attr$1108;
      moonbit_string_t _field$3040;
      moonbit_string_t _tmp$2628;
      int32_t _tmp$3039;
      int32_t _tmp$2630;
      moonbit_incref(_arr$1104);
      attr$1108
      = $$moonbitlang$core$builtin$Array$$unsafe_get$2(
        _arr$1104, _i$1107
      );
      _field$3040 = attr$1108->$0;
      _tmp$2628 = _field$3040;
      _tmp$3039 = moonbit_val_array_equal(_tmp$2628, key$1109);
      if (_tmp$3039) {
        void* _field$3038;
        void* _tmp$2629;
        moonbit_decref(key$1109);
        moonbit_decref(_arr$1104);
        _field$3038 = attr$1108->$1;
        moonbit_incref(_field$3038);
        moonbit_decref(attr$1108);
        _tmp$2629 = _field$3038;
        return _tmp$2629;
      } else {
        moonbit_decref(attr$1108);
      }
      _tmp$2630 = _i$1107 + 1;
      _i$1107 = _tmp$2630;
      continue;
    } else {
      moonbit_decref(key$1109);
      moonbit_decref(_arr$1104);
      return 0;
    }
    break;
  }
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3(
  
) {
  struct $Attributes* attrs$1094 =
    $$azimuth$telemetry$simple_test$Attributes$$new();
  void* string_val$1095;
  void* int_val$1096;
  void* missing_val$1097;
  moonbit_string_t _tmp$2614;
  struct moonbit_result_0 _tmp$3608;
  moonbit_string_t _tmp$2620;
  struct moonbit_result_0 _tmp$3613;
  int32_t _tmp$3042;
  moonbit_incref(attrs$1094);
  string_val$1095
  = $$azimuth$telemetry$simple_test$Attributes$$get(
    attrs$1094, (moonbit_string_t)moonbit_string_literal_73.data
  );
  moonbit_incref(attrs$1094);
  int_val$1096
  = $$azimuth$telemetry$simple_test$Attributes$$get(
    attrs$1094, (moonbit_string_t)moonbit_string_literal_74.data
  );
  missing_val$1097
  = $$azimuth$telemetry$simple_test$Attributes$$get(
    attrs$1094, (moonbit_string_t)moonbit_string_literal_75.data
  );
  if (string_val$1095 == 0) {
    if (string_val$1095) {
      moonbit_decref(string_val$1095);
    }
    goto $join$1098;
  } else {
    void* _Some$1099 = string_val$1095;
    void* _x$1100 = _Some$1099;
    switch (Moonbit_object_tag(_x$1100)) {
      case 0: {
        moonbit_string_t _tmp$2617;
        struct moonbit_result_0 _tmp$3606;
        moonbit_decref(_x$1100);
        _tmp$2617 = 0;
        _tmp$3606
        = $moonbitlang$core$builtin$assert_true(
          1, _tmp$2617, (moonbit_string_t)moonbit_string_literal_76.data
        );
        if (_tmp$3606.tag) {
          int32_t const _ok$2618 = _tmp$3606.data.ok;
        } else {
          void* const _err$2619 = _tmp$3606.data.err;
          struct moonbit_result_0 _result$3607;
          if (missing_val$1097) {
            moonbit_decref(missing_val$1097);
          }
          if (int_val$1096) {
            moonbit_decref(int_val$1096);
          }
          _result$3607.tag = 0;
          _result$3607.data.err = _err$2619;
          return _result$3607;
        }
        break;
      }
      default: {
        moonbit_decref(_x$1100);
        goto $join$1098;
        break;
      }
    }
  }
  goto $joinlet$3605;
  $join$1098:;
  _tmp$2614 = 0;
  _tmp$3608
  = $moonbitlang$core$builtin$assert_false(
    1, _tmp$2614, (moonbit_string_t)moonbit_string_literal_77.data
  );
  if (_tmp$3608.tag) {
    int32_t const _ok$2615 = _tmp$3608.data.ok;
  } else {
    void* const _err$2616 = _tmp$3608.data.err;
    struct moonbit_result_0 _result$3609;
    if (missing_val$1097) {
      moonbit_decref(missing_val$1097);
    }
    if (int_val$1096) {
      moonbit_decref(int_val$1096);
    }
    _result$3609.tag = 0;
    _result$3609.data.err = _err$2616;
    return _result$3609;
  }
  $joinlet$3605:;
  if (int_val$1096 == 0) {
    if (int_val$1096) {
      moonbit_decref(int_val$1096);
    }
    goto $join$1101;
  } else {
    void* _Some$1102 = int_val$1096;
    void* _x$1103 = _Some$1102;
    switch (Moonbit_object_tag(_x$1103)) {
      case 1: {
        moonbit_string_t _tmp$2623;
        struct moonbit_result_0 _tmp$3611;
        moonbit_decref(_x$1103);
        _tmp$2623 = 0;
        _tmp$3611
        = $moonbitlang$core$builtin$assert_true(
          1, _tmp$2623, (moonbit_string_t)moonbit_string_literal_78.data
        );
        if (_tmp$3611.tag) {
          int32_t const _ok$2624 = _tmp$3611.data.ok;
        } else {
          void* const _err$2625 = _tmp$3611.data.err;
          struct moonbit_result_0 _result$3612;
          if (missing_val$1097) {
            moonbit_decref(missing_val$1097);
          }
          _result$3612.tag = 0;
          _result$3612.data.err = _err$2625;
          return _result$3612;
        }
        break;
      }
      default: {
        moonbit_decref(_x$1103);
        goto $join$1101;
        break;
      }
    }
  }
  goto $joinlet$3610;
  $join$1101:;
  _tmp$2620 = 0;
  _tmp$3613
  = $moonbitlang$core$builtin$assert_false(
    1, _tmp$2620, (moonbit_string_t)moonbit_string_literal_79.data
  );
  if (_tmp$3613.tag) {
    int32_t const _ok$2621 = _tmp$3613.data.ok;
  } else {
    void* const _err$2622 = _tmp$3613.data.err;
    struct moonbit_result_0 _result$3614;
    if (missing_val$1097) {
      moonbit_decref(missing_val$1097);
    }
    _result$3614.tag = 0;
    _result$3614.data.err = _err$2622;
    return _result$3614;
  }
  $joinlet$3610:;
  _tmp$3042 = missing_val$1097 == 0;
  if (missing_val$1097) {
    moonbit_decref(missing_val$1097);
  }
  if (_tmp$3042) {
    moonbit_string_t _tmp$2626 = 0;
    return $moonbitlang$core$builtin$assert_true(
             1, _tmp$2626, (moonbit_string_t)moonbit_string_literal_80.data
           );
  } else {
    moonbit_string_t _tmp$2627 = 0;
    return $moonbitlang$core$builtin$assert_false(
             1, _tmp$2627, (moonbit_string_t)moonbit_string_literal_81.data
           );
  }
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2(
  
) {
  int32_t provider$1088 =
    $$azimuth$telemetry$simple_test$MeterProvider$$default();
  struct $Meter* meter$1089 =
    $$azimuth$telemetry$simple_test$MeterProvider$$get_meter(
      provider$1088, (moonbit_string_t)moonbit_string_literal_82.data
    );
  struct $Counter* counter$1090;
  void* None$2612;
  void* None$2613;
  struct $Histogram* histogram$1091;
  void* None$2610;
  void* None$2611;
  struct $UpDownCounter* updown_counter$1092;
  void* None$2608;
  void* None$2609;
  struct $Gauge* gauge$1093;
  moonbit_string_t _field$3046;
  int32_t _cnt$3377;
  moonbit_string_t name$2594;
  moonbit_string_t _tmp$2595;
  struct moonbit_result_0 _tmp$3615;
  moonbit_string_t _field$3045;
  int32_t _cnt$3381;
  moonbit_string_t name$2598;
  moonbit_string_t _tmp$2599;
  struct moonbit_result_0 _tmp$3617;
  moonbit_string_t _field$3044;
  int32_t _cnt$3385;
  moonbit_string_t name$2602;
  moonbit_string_t _tmp$2603;
  struct moonbit_result_0 _tmp$3619;
  moonbit_string_t _field$3043;
  int32_t _cnt$3389;
  moonbit_string_t name$2606;
  moonbit_string_t _tmp$2607;
  moonbit_incref(meter$1089);
  counter$1090
  = $$azimuth$telemetry$simple_test$Meter$$create_counter(
    meter$1089, (moonbit_string_t)moonbit_string_literal_83.data
  );
  None$2612 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  None$2613 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  moonbit_incref(meter$1089);
  histogram$1091
  = $$azimuth$telemetry$simple_test$Meter$$create_histogram(
    meter$1089,
      (moonbit_string_t)moonbit_string_literal_84.data,
      None$2612,
      None$2613
  );
  None$2610 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  None$2611 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  moonbit_incref(meter$1089);
  updown_counter$1092
  = $$azimuth$telemetry$simple_test$Meter$$create_updown_counter(
    meter$1089,
      (moonbit_string_t)moonbit_string_literal_85.data,
      None$2610,
      None$2611
  );
  None$2608 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  None$2609 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  gauge$1093
  = $$azimuth$telemetry$simple_test$Meter$$create_gauge(
    meter$1089,
      (moonbit_string_t)moonbit_string_literal_86.data,
      None$2608,
      None$2609
  );
  _field$3046 = counter$1090->$0;
  _cnt$3377 = Moonbit_object_header(counter$1090)->rc;
  if (_cnt$3377 > 1) {
    int32_t _new_cnt$3380;
    moonbit_incref(_field$3046);
    _new_cnt$3380 = _cnt$3377 - 1;
    Moonbit_object_header(counter$1090)->rc = _new_cnt$3380;
  } else if (_cnt$3377 == 1) {
    moonbit_string_t _field$3379 = counter$1090->$2;
    moonbit_string_t _field$3378;
    if (_field$3379) {
      moonbit_decref(_field$3379);
    }
    _field$3378 = counter$1090->$1;
    if (_field$3378) {
      moonbit_decref(_field$3378);
    }
    moonbit_free(counter$1090);
  }
  name$2594 = _field$3046;
  _tmp$2595 = 0;
  _tmp$3615
  = $moonbitlang$core$builtin$assert_eq$0(
    name$2594,
      (moonbit_string_t)moonbit_string_literal_83.data,
      _tmp$2595,
      (moonbit_string_t)moonbit_string_literal_87.data
  );
  if (_tmp$3615.tag) {
    int32_t const _ok$2596 = _tmp$3615.data.ok;
  } else {
    void* const _err$2597 = _tmp$3615.data.err;
    struct moonbit_result_0 _result$3616;
    moonbit_decref(gauge$1093);
    moonbit_decref(updown_counter$1092);
    moonbit_decref(histogram$1091);
    _result$3616.tag = 0;
    _result$3616.data.err = _err$2597;
    return _result$3616;
  }
  _field$3045 = histogram$1091->$0;
  _cnt$3381 = Moonbit_object_header(histogram$1091)->rc;
  if (_cnt$3381 > 1) {
    int32_t _new_cnt$3384;
    moonbit_incref(_field$3045);
    _new_cnt$3384 = _cnt$3381 - 1;
    Moonbit_object_header(histogram$1091)->rc = _new_cnt$3384;
  } else if (_cnt$3381 == 1) {
    moonbit_string_t _field$3383 = histogram$1091->$2;
    moonbit_string_t _field$3382;
    if (_field$3383) {
      moonbit_decref(_field$3383);
    }
    _field$3382 = histogram$1091->$1;
    if (_field$3382) {
      moonbit_decref(_field$3382);
    }
    moonbit_free(histogram$1091);
  }
  name$2598 = _field$3045;
  _tmp$2599 = 0;
  _tmp$3617
  = $moonbitlang$core$builtin$assert_eq$0(
    name$2598,
      (moonbit_string_t)moonbit_string_literal_84.data,
      _tmp$2599,
      (moonbit_string_t)moonbit_string_literal_88.data
  );
  if (_tmp$3617.tag) {
    int32_t const _ok$2600 = _tmp$3617.data.ok;
  } else {
    void* const _err$2601 = _tmp$3617.data.err;
    struct moonbit_result_0 _result$3618;
    moonbit_decref(gauge$1093);
    moonbit_decref(updown_counter$1092);
    _result$3618.tag = 0;
    _result$3618.data.err = _err$2601;
    return _result$3618;
  }
  _field$3044 = updown_counter$1092->$0;
  _cnt$3385 = Moonbit_object_header(updown_counter$1092)->rc;
  if (_cnt$3385 > 1) {
    int32_t _new_cnt$3388;
    moonbit_incref(_field$3044);
    _new_cnt$3388 = _cnt$3385 - 1;
    Moonbit_object_header(updown_counter$1092)->rc = _new_cnt$3388;
  } else if (_cnt$3385 == 1) {
    moonbit_string_t _field$3387 = updown_counter$1092->$2;
    moonbit_string_t _field$3386;
    if (_field$3387) {
      moonbit_decref(_field$3387);
    }
    _field$3386 = updown_counter$1092->$1;
    if (_field$3386) {
      moonbit_decref(_field$3386);
    }
    moonbit_free(updown_counter$1092);
  }
  name$2602 = _field$3044;
  _tmp$2603 = 0;
  _tmp$3619
  = $moonbitlang$core$builtin$assert_eq$0(
    name$2602,
      (moonbit_string_t)moonbit_string_literal_85.data,
      _tmp$2603,
      (moonbit_string_t)moonbit_string_literal_89.data
  );
  if (_tmp$3619.tag) {
    int32_t const _ok$2604 = _tmp$3619.data.ok;
  } else {
    void* const _err$2605 = _tmp$3619.data.err;
    struct moonbit_result_0 _result$3620;
    moonbit_decref(gauge$1093);
    _result$3620.tag = 0;
    _result$3620.data.err = _err$2605;
    return _result$3620;
  }
  _field$3043 = gauge$1093->$0;
  _cnt$3389 = Moonbit_object_header(gauge$1093)->rc;
  if (_cnt$3389 > 1) {
    int32_t _new_cnt$3392;
    moonbit_incref(_field$3043);
    _new_cnt$3392 = _cnt$3389 - 1;
    Moonbit_object_header(gauge$1093)->rc = _new_cnt$3392;
  } else if (_cnt$3389 == 1) {
    moonbit_string_t _field$3391 = gauge$1093->$2;
    moonbit_string_t _field$3390;
    if (_field$3391) {
      moonbit_decref(_field$3391);
    }
    _field$3390 = gauge$1093->$1;
    if (_field$3390) {
      moonbit_decref(_field$3390);
    }
    moonbit_free(gauge$1093);
  }
  name$2606 = _field$3043;
  _tmp$2607 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           name$2606,
             (moonbit_string_t)moonbit_string_literal_86.data,
             _tmp$2607,
             (moonbit_string_t)moonbit_string_literal_90.data
         );
}

struct $UpDownCounter* $$azimuth$telemetry$simple_test$Meter$$create_updown_counter(
  struct $Meter* meter$1086,
  moonbit_string_t name$1087,
  void* description$opt$1081,
  void* unit$opt$1084
) {
  moonbit_string_t description$1080;
  moonbit_string_t unit$1083;
  switch (Moonbit_object_tag(description$opt$1081)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$1082 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)description$opt$1081;
      moonbit_string_t _field$3048 = _Some$1082->$0;
      int32_t _cnt$3393 = Moonbit_object_header(_Some$1082)->rc;
      if (_cnt$3393 > 1) {
        int32_t _new_cnt$3394;
        if (_field$3048) {
          moonbit_incref(_field$3048);
        }
        _new_cnt$3394 = _cnt$3393 - 1;
        Moonbit_object_header(_Some$1082)->rc = _new_cnt$3394;
      } else if (_cnt$3393 == 1) {
        moonbit_free(_Some$1082);
      }
      description$1080 = _field$3048;
      break;
    }
    default: {
      moonbit_decref(description$opt$1081);
      description$1080 = 0;
      break;
    }
  }
  switch (Moonbit_object_tag(unit$opt$1084)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$1085 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)unit$opt$1084;
      moonbit_string_t _field$3047 = _Some$1085->$0;
      int32_t _cnt$3395 = Moonbit_object_header(_Some$1085)->rc;
      if (_cnt$3395 > 1) {
        int32_t _new_cnt$3396;
        if (_field$3047) {
          moonbit_incref(_field$3047);
        }
        _new_cnt$3396 = _cnt$3395 - 1;
        Moonbit_object_header(_Some$1085)->rc = _new_cnt$3396;
      } else if (_cnt$3395 == 1) {
        moonbit_free(_Some$1085);
      }
      unit$1083 = _field$3047;
      break;
    }
    default: {
      moonbit_decref(unit$opt$1084);
      unit$1083 = 0;
      break;
    }
  }
  return $$azimuth$telemetry$simple_test$Meter$$create_updown_counter$inner(
           meter$1086, name$1087, description$1080, unit$1083
         );
}

struct $UpDownCounter* $$azimuth$telemetry$simple_test$Meter$$create_updown_counter$inner(
  struct $Meter* meter$1079,
  moonbit_string_t name$1076,
  moonbit_string_t description$1077,
  moonbit_string_t unit$1078
) {
  struct $UpDownCounter* _block$3621;
  moonbit_decref(meter$1079);
  _block$3621
  = (struct $UpDownCounter*)moonbit_malloc(sizeof(struct $UpDownCounter));
  Moonbit_object_header(_block$3621)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $UpDownCounter, $0) >> 2, 3, 0
  );
  _block$3621->$0 = name$1076;
  _block$3621->$1 = description$1077;
  _block$3621->$2 = unit$1078;
  return _block$3621;
}

struct $Histogram* $$azimuth$telemetry$simple_test$Meter$$create_histogram(
  struct $Meter* meter$1074,
  moonbit_string_t name$1075,
  void* description$opt$1069,
  void* unit$opt$1072
) {
  moonbit_string_t description$1068;
  moonbit_string_t unit$1071;
  switch (Moonbit_object_tag(description$opt$1069)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$1070 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)description$opt$1069;
      moonbit_string_t _field$3050 = _Some$1070->$0;
      int32_t _cnt$3397 = Moonbit_object_header(_Some$1070)->rc;
      if (_cnt$3397 > 1) {
        int32_t _new_cnt$3398;
        if (_field$3050) {
          moonbit_incref(_field$3050);
        }
        _new_cnt$3398 = _cnt$3397 - 1;
        Moonbit_object_header(_Some$1070)->rc = _new_cnt$3398;
      } else if (_cnt$3397 == 1) {
        moonbit_free(_Some$1070);
      }
      description$1068 = _field$3050;
      break;
    }
    default: {
      moonbit_decref(description$opt$1069);
      description$1068 = 0;
      break;
    }
  }
  switch (Moonbit_object_tag(unit$opt$1072)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$1073 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)unit$opt$1072;
      moonbit_string_t _field$3049 = _Some$1073->$0;
      int32_t _cnt$3399 = Moonbit_object_header(_Some$1073)->rc;
      if (_cnt$3399 > 1) {
        int32_t _new_cnt$3400;
        if (_field$3049) {
          moonbit_incref(_field$3049);
        }
        _new_cnt$3400 = _cnt$3399 - 1;
        Moonbit_object_header(_Some$1073)->rc = _new_cnt$3400;
      } else if (_cnt$3399 == 1) {
        moonbit_free(_Some$1073);
      }
      unit$1071 = _field$3049;
      break;
    }
    default: {
      moonbit_decref(unit$opt$1072);
      unit$1071 = 0;
      break;
    }
  }
  return $$azimuth$telemetry$simple_test$Meter$$create_histogram$inner(
           meter$1074, name$1075, description$1068, unit$1071
         );
}

struct $Histogram* $$azimuth$telemetry$simple_test$Meter$$create_histogram$inner(
  struct $Meter* meter$1067,
  moonbit_string_t name$1064,
  moonbit_string_t description$1065,
  moonbit_string_t unit$1066
) {
  struct $Histogram* _block$3622;
  moonbit_decref(meter$1067);
  _block$3622 = (struct $Histogram*)moonbit_malloc(sizeof(struct $Histogram));
  Moonbit_object_header(_block$3622)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Histogram, $0) >> 2, 3, 0
  );
  _block$3622->$0 = name$1064;
  _block$3622->$1 = description$1065;
  _block$3622->$2 = unit$1066;
  return _block$3622;
}

struct $Gauge* $$azimuth$telemetry$simple_test$Meter$$create_gauge(
  struct $Meter* meter$1062,
  moonbit_string_t name$1063,
  void* description$opt$1057,
  void* unit$opt$1060
) {
  moonbit_string_t description$1056;
  moonbit_string_t unit$1059;
  switch (Moonbit_object_tag(description$opt$1057)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$1058 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)description$opt$1057;
      moonbit_string_t _field$3052 = _Some$1058->$0;
      int32_t _cnt$3401 = Moonbit_object_header(_Some$1058)->rc;
      if (_cnt$3401 > 1) {
        int32_t _new_cnt$3402;
        if (_field$3052) {
          moonbit_incref(_field$3052);
        }
        _new_cnt$3402 = _cnt$3401 - 1;
        Moonbit_object_header(_Some$1058)->rc = _new_cnt$3402;
      } else if (_cnt$3401 == 1) {
        moonbit_free(_Some$1058);
      }
      description$1056 = _field$3052;
      break;
    }
    default: {
      moonbit_decref(description$opt$1057);
      description$1056 = 0;
      break;
    }
  }
  switch (Moonbit_object_tag(unit$opt$1060)) {
    case 1: {
      struct $Option$3c$Option$3c$String$3e$$3e$$Some* _Some$1061 =
        (struct $Option$3c$Option$3c$String$3e$$3e$$Some*)unit$opt$1060;
      moonbit_string_t _field$3051 = _Some$1061->$0;
      int32_t _cnt$3403 = Moonbit_object_header(_Some$1061)->rc;
      if (_cnt$3403 > 1) {
        int32_t _new_cnt$3404;
        if (_field$3051) {
          moonbit_incref(_field$3051);
        }
        _new_cnt$3404 = _cnt$3403 - 1;
        Moonbit_object_header(_Some$1061)->rc = _new_cnt$3404;
      } else if (_cnt$3403 == 1) {
        moonbit_free(_Some$1061);
      }
      unit$1059 = _field$3051;
      break;
    }
    default: {
      moonbit_decref(unit$opt$1060);
      unit$1059 = 0;
      break;
    }
  }
  return $$azimuth$telemetry$simple_test$Meter$$create_gauge$inner(
           meter$1062, name$1063, description$1056, unit$1059
         );
}

struct $Gauge* $$azimuth$telemetry$simple_test$Meter$$create_gauge$inner(
  struct $Meter* meter$1055,
  moonbit_string_t name$1052,
  moonbit_string_t description$1053,
  moonbit_string_t unit$1054
) {
  struct $Gauge* _block$3623;
  moonbit_decref(meter$1055);
  _block$3623 = (struct $Gauge*)moonbit_malloc(sizeof(struct $Gauge));
  Moonbit_object_header(_block$3623)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Gauge, $0) >> 2, 3, 0
  );
  _block$3623->$0 = name$1052;
  _block$3623->$1 = description$1053;
  _block$3623->$2 = unit$1054;
  return _block$3623;
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1(
  
) {
  struct $Context* root_ctx$1045 =
    $$azimuth$telemetry$simple_test$Context$$root();
  struct $ContextKey$3c$String$3e$* key1$1046 =
    $$azimuth$telemetry$simple_test$ContextKey$$new(
      (moonbit_string_t)moonbit_string_literal_91.data
    );
  struct $ContextKey$3c$String$3e$* key2$1047 =
    $$azimuth$telemetry$simple_test$ContextKey$$new(
      (moonbit_string_t)moonbit_string_literal_92.data
    );
  struct $ContextKey$3c$String$3e$* key3$1048 =
    $$azimuth$telemetry$simple_test$ContextKey$$new(
      (moonbit_string_t)moonbit_string_literal_93.data
    );
  struct $Context* ctx1$1049;
  struct $Context* ctx2$1050;
  struct $Context* ctx3$1051;
  moonbit_string_t _tmp$2581;
  moonbit_string_t _tmp$2582;
  moonbit_string_t _tmp$2583;
  struct moonbit_result_0 _tmp$3624;
  moonbit_string_t _tmp$2586;
  moonbit_string_t _tmp$2587;
  moonbit_string_t _tmp$2588;
  struct moonbit_result_0 _tmp$3626;
  moonbit_string_t _tmp$2591;
  moonbit_string_t _tmp$2592;
  moonbit_string_t _tmp$2593;
  moonbit_incref(key1$1046);
  ctx1$1049
  = $$azimuth$telemetry$simple_test$Context$$with_value(
    root_ctx$1045,
      key1$1046,
      (moonbit_string_t)moonbit_string_literal_94.data
  );
  moonbit_incref(ctx1$1049);
  moonbit_incref(key2$1047);
  ctx2$1050
  = $$azimuth$telemetry$simple_test$Context$$with_value(
    ctx1$1049, key2$1047, (moonbit_string_t)moonbit_string_literal_95.data
  );
  moonbit_incref(ctx2$1050);
  moonbit_incref(key3$1048);
  ctx3$1051
  = $$azimuth$telemetry$simple_test$Context$$with_value(
    ctx2$1050, key3$1048, (moonbit_string_t)moonbit_string_literal_96.data
  );
  _tmp$2581
  = $$azimuth$telemetry$simple_test$Context$$get(
    ctx1$1049, key1$1046
  );
  _tmp$2582 = (moonbit_string_t)moonbit_string_literal_94.data;
  _tmp$2583 = 0;
  _tmp$3624
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$2581,
      _tmp$2582,
      _tmp$2583,
      (moonbit_string_t)moonbit_string_literal_97.data
  );
  if (_tmp$3624.tag) {
    int32_t const _ok$2584 = _tmp$3624.data.ok;
  } else {
    void* const _err$2585 = _tmp$3624.data.err;
    struct moonbit_result_0 _result$3625;
    moonbit_decref(ctx3$1051);
    moonbit_decref(ctx2$1050);
    moonbit_decref(key3$1048);
    moonbit_decref(key2$1047);
    _result$3625.tag = 0;
    _result$3625.data.err = _err$2585;
    return _result$3625;
  }
  _tmp$2586
  = $$azimuth$telemetry$simple_test$Context$$get(
    ctx2$1050, key2$1047
  );
  _tmp$2587 = (moonbit_string_t)moonbit_string_literal_95.data;
  _tmp$2588 = 0;
  _tmp$3626
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$2586,
      _tmp$2587,
      _tmp$2588,
      (moonbit_string_t)moonbit_string_literal_98.data
  );
  if (_tmp$3626.tag) {
    int32_t const _ok$2589 = _tmp$3626.data.ok;
  } else {
    void* const _err$2590 = _tmp$3626.data.err;
    struct moonbit_result_0 _result$3627;
    moonbit_decref(ctx3$1051);
    moonbit_decref(key3$1048);
    _result$3627.tag = 0;
    _result$3627.data.err = _err$2590;
    return _result$3627;
  }
  _tmp$2591
  = $$azimuth$telemetry$simple_test$Context$$get(
    ctx3$1051, key3$1048
  );
  _tmp$2592 = (moonbit_string_t)moonbit_string_literal_96.data;
  _tmp$2593 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           _tmp$2591,
             _tmp$2592,
             _tmp$2593,
             (moonbit_string_t)moonbit_string_literal_99.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0(
  
) {
  struct $SpanContext* valid_ctx$1043 =
    $$azimuth$telemetry$simple_test$SpanContext$$new(
      (moonbit_string_t)moonbit_string_literal_100.data,
        (moonbit_string_t)moonbit_string_literal_101.data,
        1,
        (moonbit_string_t)moonbit_string_literal_102.data
    );
  moonbit_string_t _tmp$2563;
  moonbit_string_t _tmp$2564;
  struct moonbit_result_0 _tmp$3628;
  moonbit_string_t _tmp$2567;
  moonbit_string_t _tmp$2568;
  struct moonbit_result_0 _tmp$3630;
  int32_t _tmp$2571;
  moonbit_string_t _tmp$2572;
  struct moonbit_result_0 _tmp$3632;
  int32_t _tmp$2575;
  moonbit_string_t _tmp$2576;
  struct moonbit_result_0 _tmp$3634;
  struct $SpanContext* invalid_ctx$1044;
  int32_t _tmp$2579;
  moonbit_string_t _tmp$2580;
  moonbit_incref(valid_ctx$1043);
  _tmp$2563
  = $$azimuth$telemetry$simple_test$SpanContext$$trace_id(
    valid_ctx$1043
  );
  _tmp$2564 = 0;
  _tmp$3628
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2563,
      (moonbit_string_t)moonbit_string_literal_100.data,
      _tmp$2564,
      (moonbit_string_t)moonbit_string_literal_103.data
  );
  if (_tmp$3628.tag) {
    int32_t const _ok$2565 = _tmp$3628.data.ok;
  } else {
    void* const _err$2566 = _tmp$3628.data.err;
    struct moonbit_result_0 _result$3629;
    moonbit_decref(valid_ctx$1043);
    _result$3629.tag = 0;
    _result$3629.data.err = _err$2566;
    return _result$3629;
  }
  moonbit_incref(valid_ctx$1043);
  _tmp$2567
  = $$azimuth$telemetry$simple_test$SpanContext$$span_id(
    valid_ctx$1043
  );
  _tmp$2568 = 0;
  _tmp$3630
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2567,
      (moonbit_string_t)moonbit_string_literal_101.data,
      _tmp$2568,
      (moonbit_string_t)moonbit_string_literal_104.data
  );
  if (_tmp$3630.tag) {
    int32_t const _ok$2569 = _tmp$3630.data.ok;
  } else {
    void* const _err$2570 = _tmp$3630.data.err;
    struct moonbit_result_0 _result$3631;
    moonbit_decref(valid_ctx$1043);
    _result$3631.tag = 0;
    _result$3631.data.err = _err$2570;
    return _result$3631;
  }
  moonbit_incref(valid_ctx$1043);
  _tmp$2571
  = $$azimuth$telemetry$simple_test$SpanContext$$is_sampled(
    valid_ctx$1043
  );
  _tmp$2572 = 0;
  _tmp$3632
  = $moonbitlang$core$builtin$assert_eq$2(
    _tmp$2571,
      1,
      _tmp$2572,
      (moonbit_string_t)moonbit_string_literal_105.data
  );
  if (_tmp$3632.tag) {
    int32_t const _ok$2573 = _tmp$3632.data.ok;
  } else {
    void* const _err$2574 = _tmp$3632.data.err;
    struct moonbit_result_0 _result$3633;
    moonbit_decref(valid_ctx$1043);
    _result$3633.tag = 0;
    _result$3633.data.err = _err$2574;
    return _result$3633;
  }
  _tmp$2575
  = $$azimuth$telemetry$simple_test$SpanContext$$is_valid(
    valid_ctx$1043
  );
  _tmp$2576 = 0;
  _tmp$3634
  = $moonbitlang$core$builtin$assert_eq$2(
    _tmp$2575,
      1,
      _tmp$2576,
      (moonbit_string_t)moonbit_string_literal_106.data
  );
  if (_tmp$3634.tag) {
    int32_t const _ok$2577 = _tmp$3634.data.ok;
  } else {
    void* const _err$2578 = _tmp$3634.data.err;
    struct moonbit_result_0 _result$3635;
    _result$3635.tag = 0;
    _result$3635.data.err = _err$2578;
    return _result$3635;
  }
  invalid_ctx$1044
  = $$azimuth$telemetry$simple_test$SpanContext$$new(
    (moonbit_string_t)moonbit_string_literal_3.data,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0,
      (moonbit_string_t)moonbit_string_literal_3.data
  );
  _tmp$2579
  = $$azimuth$telemetry$simple_test$SpanContext$$is_valid(
    invalid_ctx$1044
  );
  _tmp$2580 = 0;
  return $moonbitlang$core$builtin$assert_eq$2(
           _tmp$2579,
             0,
             _tmp$2580,
             (moonbit_string_t)moonbit_string_literal_107.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4(
  
) {
  struct $LogRecord* record$1041 =
    $$azimuth$telemetry$simple_test$LogRecord$$new(
      2, (moonbit_string_t)moonbit_string_literal_108.data
    );
  moonbit_string_t body$1042 =
    $$azimuth$telemetry$simple_test$LogRecord$$body(record$1041);
  moonbit_string_t _tmp$2561 =
    (moonbit_string_t)moonbit_string_literal_108.data;
  moonbit_string_t _tmp$2562 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           body$1042,
             _tmp$2561,
             _tmp$2562,
             (moonbit_string_t)moonbit_string_literal_109.data
         );
}

struct $LogRecord* $$azimuth$telemetry$simple_test$LogRecord$$new(
  int32_t severity$1039,
  moonbit_string_t body$1040
) {
  moonbit_string_t _tmp$2554 = body$1040;
  struct $Attributes* _tmp$2555 = 0;
  void* None$2556 =
    (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  void* None$2557 =
    (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  moonbit_string_t _tmp$2558 = 0;
  moonbit_string_t _tmp$2559 = 0;
  struct $Context* _tmp$2560 = 0;
  struct $LogRecord* _block$3636 =
    (struct $LogRecord*)moonbit_malloc(sizeof(struct $LogRecord));
  Moonbit_object_header(_block$3636)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $LogRecord, $1) >> 2, 7, 0
  );
  _block$3636->$0 = severity$1039;
  _block$3636->$1 = _tmp$2554;
  _block$3636->$2 = _tmp$2555;
  _block$3636->$3 = None$2556;
  _block$3636->$4 = None$2557;
  _block$3636->$5 = _tmp$2558;
  _block$3636->$6 = _tmp$2559;
  _block$3636->$7 = _tmp$2560;
  return _block$3636;
}

moonbit_string_t $$azimuth$telemetry$simple_test$LogRecord$$body(
  struct $LogRecord* record$1038
) {
  moonbit_string_t _field$3053 = record$1038->$1;
  int32_t _cnt$3405 = Moonbit_object_header(record$1038)->rc;
  if (_cnt$3405 > 1) {
    int32_t _new_cnt$3412;
    if (_field$3053) {
      moonbit_incref(_field$3053);
    }
    _new_cnt$3412 = _cnt$3405 - 1;
    Moonbit_object_header(record$1038)->rc = _new_cnt$3412;
  } else if (_cnt$3405 == 1) {
    struct $Context* _field$3411 = record$1038->$7;
    moonbit_string_t _field$3410;
    moonbit_string_t _field$3409;
    void* _field$3408;
    void* _field$3407;
    struct $Attributes* _field$3406;
    if (_field$3411) {
      moonbit_decref(_field$3411);
    }
    _field$3410 = record$1038->$6;
    if (_field$3410) {
      moonbit_decref(_field$3410);
    }
    _field$3409 = record$1038->$5;
    if (_field$3409) {
      moonbit_decref(_field$3409);
    }
    _field$3408 = record$1038->$4;
    moonbit_decref(_field$3408);
    _field$3407 = record$1038->$3;
    moonbit_decref(_field$3407);
    _field$3406 = record$1038->$2;
    if (_field$3406) {
      moonbit_decref(_field$3406);
    }
    moonbit_free(record$1038);
  }
  return _field$3053;
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3(
  
) {
  int32_t provider$1035 =
    $$azimuth$telemetry$simple_test$MeterProvider$$default();
  struct $Meter* meter$1036 =
    $$azimuth$telemetry$simple_test$MeterProvider$$get_meter(
      provider$1035, (moonbit_string_t)moonbit_string_literal_110.data
    );
  struct $Counter* counter$1037 =
    $$azimuth$telemetry$simple_test$Meter$$create_counter(
      meter$1036, (moonbit_string_t)moonbit_string_literal_83.data
    );
  moonbit_string_t _field$3054 = counter$1037->$0;
  moonbit_string_t name$2547 = _field$3054;
  moonbit_string_t _tmp$2548 = 0;
  struct moonbit_result_0 _tmp$3637;
  void* None$2551;
  void* None$2552;
  moonbit_string_t _tmp$2553;
  moonbit_incref(name$2547);
  _tmp$3637
  = $moonbitlang$core$builtin$assert_eq$0(
    name$2547,
      (moonbit_string_t)moonbit_string_literal_83.data,
      _tmp$2548,
      (moonbit_string_t)moonbit_string_literal_111.data
  );
  if (_tmp$3637.tag) {
    int32_t const _ok$2549 = _tmp$3637.data.ok;
  } else {
    void* const _err$2550 = _tmp$3637.data.err;
    struct moonbit_result_0 _result$3638;
    moonbit_decref(counter$1037);
    _result$3638.tag = 0;
    _result$3638.data.err = _err$2550;
    return _result$3638;
  }
  None$2551 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  moonbit_incref(counter$1037);
  $$azimuth$telemetry$simple_test$Counter$$add(
    counter$1037, 0x1p+0, None$2551
  );
  None$2552 = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
  $$azimuth$telemetry$simple_test$Counter$$add(
    counter$1037, 0x1.4p+2, None$2552
  );
  _tmp$2553 = 0;
  return $moonbitlang$core$builtin$assert_true(
           1, _tmp$2553, (moonbit_string_t)moonbit_string_literal_112.data
         );
}

struct $Meter* $$azimuth$telemetry$simple_test$MeterProvider$$get_meter(
  int32_t provider$1034,
  moonbit_string_t name$1033
) {
  moonbit_string_t _tmp$2545 = 0;
  moonbit_string_t _tmp$2546 = 0;
  struct $InstrumentationScope* scope$1032 =
    (struct $InstrumentationScope*)moonbit_malloc(
      sizeof(struct $InstrumentationScope)
    );
  struct $Meter* _block$3639;
  Moonbit_object_header(scope$1032)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $InstrumentationScope, $0) >> 2, 3, 0
  );
  scope$1032->$0 = name$1033;
  scope$1032->$1 = _tmp$2545;
  scope$1032->$2 = _tmp$2546;
  _block$3639 = (struct $Meter*)moonbit_malloc(sizeof(struct $Meter));
  Moonbit_object_header(_block$3639)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Meter, $0) >> 2, 1, 0
  );
  _block$3639->$0 = scope$1032;
  return _block$3639;
}

int32_t $$azimuth$telemetry$simple_test$MeterProvider$$default() {
  return 0;
}

struct $Counter* $$azimuth$telemetry$simple_test$Meter$$create_counter(
  struct $Meter* meter$1031,
  moonbit_string_t name$1030
) {
  moonbit_string_t _tmp$2543;
  moonbit_string_t _tmp$2544;
  struct $Counter* _block$3640;
  moonbit_decref(meter$1031);
  _tmp$2543 = 0;
  _tmp$2544 = 0;
  _block$3640 = (struct $Counter*)moonbit_malloc(sizeof(struct $Counter));
  Moonbit_object_header(_block$3640)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Counter, $0) >> 2, 3, 0
  );
  _block$3640->$0 = name$1030;
  _block$3640->$1 = _tmp$2543;
  _block$3640->$2 = _tmp$2544;
  return _block$3640;
}

int32_t $$azimuth$telemetry$simple_test$Counter$$add(
  struct $Counter* counter$1028,
  double value$1029,
  void* attributes$opt$1026
) {
  struct $Attributes* attributes$1025;
  switch (Moonbit_object_tag(attributes$opt$1026)) {
    case 1: {
      struct $Option$3c$Option$3c$Attributes$3e$$3e$$Some* _Some$1027 =
        (struct $Option$3c$Option$3c$Attributes$3e$$3e$$Some*)attributes$opt$1026;
      struct $Attributes* _field$3055 = _Some$1027->$0;
      int32_t _cnt$3413 = Moonbit_object_header(_Some$1027)->rc;
      if (_cnt$3413 > 1) {
        int32_t _new_cnt$3414;
        if (_field$3055) {
          moonbit_incref(_field$3055);
        }
        _new_cnt$3414 = _cnt$3413 - 1;
        Moonbit_object_header(_Some$1027)->rc = _new_cnt$3414;
      } else if (_cnt$3413 == 1) {
        moonbit_free(_Some$1027);
      }
      attributes$1025 = _field$3055;
      break;
    }
    default: {
      moonbit_decref(attributes$opt$1026);
      attributes$1025 = 0;
      break;
    }
  }
  $$azimuth$telemetry$simple_test$Counter$$add$inner(
    counter$1028, value$1029, attributes$1025
  );
  return 0;
}

int32_t $$azimuth$telemetry$simple_test$Counter$$add$inner(
  struct $Counter* counter$1022,
  double value$1023,
  struct $Attributes* attributes$1024
) {
  if (attributes$1024) {
    moonbit_decref(attributes$1024);
  }
  moonbit_decref(counter$1022);
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2(
  
) {
  struct $Context* ctx$1018 = $$azimuth$telemetry$simple_test$Context$$root();
  struct $ContextKey$3c$String$3e$* key$1019 =
    $$azimuth$telemetry$simple_test$ContextKey$$new(
      (moonbit_string_t)moonbit_string_literal_113.data
    );
  struct $Context* ctx_with_value$1020;
  moonbit_string_t retrieved_value$1021;
  moonbit_string_t _tmp$2541;
  moonbit_string_t _tmp$2542;
  moonbit_incref(key$1019);
  ctx_with_value$1020
  = $$azimuth$telemetry$simple_test$Context$$with_value(
    ctx$1018, key$1019, (moonbit_string_t)moonbit_string_literal_114.data
  );
  retrieved_value$1021
  = $$azimuth$telemetry$simple_test$Context$$get(
    ctx_with_value$1020, key$1019
  );
  _tmp$2541 = (moonbit_string_t)moonbit_string_literal_114.data;
  _tmp$2542 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           retrieved_value$1021,
             _tmp$2541,
             _tmp$2542,
             (moonbit_string_t)moonbit_string_literal_115.data
         );
}

struct $ContextKey$3c$String$3e$* $$azimuth$telemetry$simple_test$ContextKey$$new(
  moonbit_string_t key$1017
) {
  struct $ContextKey$3c$String$3e$* _block$3641 =
    (struct $ContextKey$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $ContextKey$3c$String$3e$)
    );
  Moonbit_object_header(_block$3641)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $ContextKey$3c$String$3e$, $0) >> 2, 1, 0
  );
  _block$3641->$0 = key$1017;
  return _block$3641;
}

struct $Context* $$azimuth$telemetry$simple_test$Context$$with_value(
  struct $Context* ctx$1016,
  struct $ContextKey$3c$String$3e$* key$1014,
  moonbit_string_t value$1015
) {
  moonbit_string_t _field$3056;
  int32_t _cnt$3415;
  moonbit_string_t key$2540;
  struct $$3c$String$2a$String$3e$* _tuple$2539;
  struct $$3c$String$2a$String$3e$* _tmp$2538;
  struct $Context* _block$3642;
  moonbit_decref(ctx$1016);
  _field$3056 = key$1014->$0;
  _cnt$3415 = Moonbit_object_header(key$1014)->rc;
  if (_cnt$3415 > 1) {
    int32_t _new_cnt$3416;
    moonbit_incref(_field$3056);
    _new_cnt$3416 = _cnt$3415 - 1;
    Moonbit_object_header(key$1014)->rc = _new_cnt$3416;
  } else if (_cnt$3415 == 1) {
    moonbit_free(key$1014);
  }
  key$2540 = _field$3056;
  _tuple$2539
  = (struct $$3c$String$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$String$2a$String$3e$)
    );
  Moonbit_object_header(_tuple$2539)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$3c$String$2a$String$3e$, $0) >> 2, 2, 0
  );
  _tuple$2539->$0 = key$2540;
  _tuple$2539->$1 = value$1015;
  _tmp$2538 = _tuple$2539;
  _block$3642 = (struct $Context*)moonbit_malloc(sizeof(struct $Context));
  Moonbit_object_header(_block$3642)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Context, $0) >> 2, 1, 0
  );
  _block$3642->$0 = _tmp$2538;
  return _block$3642;
}

struct $Context* $$azimuth$telemetry$simple_test$Context$$root() {
  struct $$3c$String$2a$String$3e$* _tmp$2537 = 0;
  struct $Context* _block$3643 =
    (struct $Context*)moonbit_malloc(sizeof(struct $Context));
  Moonbit_object_header(_block$3643)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Context, $0) >> 2, 1, 0
  );
  _block$3643->$0 = _tmp$2537;
  return _block$3643;
}

moonbit_string_t $$azimuth$telemetry$simple_test$Context$$get(
  struct $Context* ctx$1008,
  struct $ContextKey$3c$String$3e$* key$1013
) {
  moonbit_string_t k$1005;
  moonbit_string_t v$1006;
  struct $$3c$String$2a$String$3e$* _field$3061 = ctx$1008->$0;
  int32_t _cnt$3417 = Moonbit_object_header(ctx$1008)->rc;
  struct $$3c$String$2a$String$3e$* _bind$1007;
  if (_cnt$3417 > 1) {
    int32_t _new_cnt$3418;
    if (_field$3061) {
      moonbit_incref(_field$3061);
    }
    _new_cnt$3418 = _cnt$3417 - 1;
    Moonbit_object_header(ctx$1008)->rc = _new_cnt$3418;
  } else if (_cnt$3417 == 1) {
    moonbit_free(ctx$1008);
  }
  _bind$1007 = _field$3061;
  if (_bind$1007 == 0) {
    moonbit_decref(key$1013);
    if (_bind$1007) {
      moonbit_decref(_bind$1007);
    }
    goto $join$1003;
  } else {
    struct $$3c$String$2a$String$3e$* _Some$1009 = _bind$1007;
    struct $$3c$String$2a$String$3e$* _x$1010 = _Some$1009;
    moonbit_string_t _field$3060 = _x$1010->$0;
    moonbit_string_t _k$1011 = _field$3060;
    moonbit_string_t _field$3059 = _x$1010->$1;
    int32_t _cnt$3419 = Moonbit_object_header(_x$1010)->rc;
    moonbit_string_t _v$1012;
    moonbit_string_t _field$3058;
    int32_t _cnt$3421;
    moonbit_string_t key$2536;
    int32_t _tmp$3057;
    if (_cnt$3419 > 1) {
      int32_t _new_cnt$3420;
      moonbit_incref(_field$3059);
      moonbit_incref(_k$1011);
      _new_cnt$3420 = _cnt$3419 - 1;
      Moonbit_object_header(_x$1010)->rc = _new_cnt$3420;
    } else if (_cnt$3419 == 1) {
      moonbit_free(_x$1010);
    }
    _v$1012 = _field$3059;
    _field$3058 = key$1013->$0;
    _cnt$3421 = Moonbit_object_header(key$1013)->rc;
    if (_cnt$3421 > 1) {
      int32_t _new_cnt$3422;
      moonbit_incref(_field$3058);
      _new_cnt$3422 = _cnt$3421 - 1;
      Moonbit_object_header(key$1013)->rc = _new_cnt$3422;
    } else if (_cnt$3421 == 1) {
      moonbit_free(key$1013);
    }
    key$2536 = _field$3058;
    _tmp$3057 = moonbit_val_array_equal(_k$1011, key$2536);
    moonbit_decref(key$2536);
    if (_tmp$3057) {
      k$1005 = _k$1011;
      v$1006 = _v$1012;
      goto $join$1004;
    } else {
      moonbit_decref(_v$1012);
      moonbit_decref(_k$1011);
      goto $join$1003;
    }
  }
  $join$1004:;
  moonbit_decref(k$1005);
  return v$1006;
  $join$1003:;
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1(
  
) {
  moonbit_string_t trace_id$1000 =
    (moonbit_string_t)moonbit_string_literal_116.data;
  moonbit_string_t span_id$1001 =
    (moonbit_string_t)moonbit_string_literal_117.data;
  struct $SpanContext* span_ctx$1002;
  moonbit_string_t _tmp$2522;
  moonbit_string_t _tmp$2523;
  struct moonbit_result_0 _tmp$3646;
  moonbit_string_t _tmp$2526;
  moonbit_string_t _tmp$2527;
  struct moonbit_result_0 _tmp$3648;
  int32_t _tmp$2530;
  moonbit_string_t _tmp$2531;
  struct moonbit_result_0 _tmp$3650;
  int32_t _tmp$2534;
  moonbit_string_t _tmp$2535;
  moonbit_incref(span_id$1001);
  moonbit_incref(trace_id$1000);
  span_ctx$1002
  = $$azimuth$telemetry$simple_test$SpanContext$$new(
    trace_id$1000,
      span_id$1001,
      1,
      (moonbit_string_t)moonbit_string_literal_3.data
  );
  moonbit_incref(span_ctx$1002);
  _tmp$2522
  = $$azimuth$telemetry$simple_test$SpanContext$$trace_id(
    span_ctx$1002
  );
  _tmp$2523 = 0;
  _tmp$3646
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2522,
      trace_id$1000,
      _tmp$2523,
      (moonbit_string_t)moonbit_string_literal_118.data
  );
  if (_tmp$3646.tag) {
    int32_t const _ok$2524 = _tmp$3646.data.ok;
  } else {
    void* const _err$2525 = _tmp$3646.data.err;
    struct moonbit_result_0 _result$3647;
    moonbit_decref(span_ctx$1002);
    moonbit_decref(span_id$1001);
    _result$3647.tag = 0;
    _result$3647.data.err = _err$2525;
    return _result$3647;
  }
  moonbit_incref(span_ctx$1002);
  _tmp$2526
  = $$azimuth$telemetry$simple_test$SpanContext$$span_id(
    span_ctx$1002
  );
  _tmp$2527 = 0;
  _tmp$3648
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$2526,
      span_id$1001,
      _tmp$2527,
      (moonbit_string_t)moonbit_string_literal_119.data
  );
  if (_tmp$3648.tag) {
    int32_t const _ok$2528 = _tmp$3648.data.ok;
  } else {
    void* const _err$2529 = _tmp$3648.data.err;
    struct moonbit_result_0 _result$3649;
    moonbit_decref(span_ctx$1002);
    _result$3649.tag = 0;
    _result$3649.data.err = _err$2529;
    return _result$3649;
  }
  moonbit_incref(span_ctx$1002);
  _tmp$2530
  = $$azimuth$telemetry$simple_test$SpanContext$$is_sampled(
    span_ctx$1002
  );
  _tmp$2531 = 0;
  _tmp$3650
  = $moonbitlang$core$builtin$assert_true(
    _tmp$2530, _tmp$2531, (moonbit_string_t)moonbit_string_literal_120.data
  );
  if (_tmp$3650.tag) {
    int32_t const _ok$2532 = _tmp$3650.data.ok;
  } else {
    void* const _err$2533 = _tmp$3650.data.err;
    struct moonbit_result_0 _result$3651;
    moonbit_decref(span_ctx$1002);
    _result$3651.tag = 0;
    _result$3651.data.err = _err$2533;
    return _result$3651;
  }
  _tmp$2534
  = $$azimuth$telemetry$simple_test$SpanContext$$is_valid(
    span_ctx$1002
  );
  _tmp$2535 = 0;
  return $moonbitlang$core$builtin$assert_true(
           _tmp$2534,
             _tmp$2535,
             (moonbit_string_t)moonbit_string_literal_121.data
         );
}

moonbit_string_t $$azimuth$telemetry$simple_test$SpanContext$$trace_id(
  struct $SpanContext* ctx$999
) {
  moonbit_string_t _field$3062 = ctx$999->$0;
  int32_t _cnt$3423 = Moonbit_object_header(ctx$999)->rc;
  if (_cnt$3423 > 1) {
    int32_t _new_cnt$3426;
    moonbit_incref(_field$3062);
    _new_cnt$3426 = _cnt$3423 - 1;
    Moonbit_object_header(ctx$999)->rc = _new_cnt$3426;
  } else if (_cnt$3423 == 1) {
    moonbit_string_t _field$3425 = ctx$999->$3;
    moonbit_string_t _field$3424;
    moonbit_decref(_field$3425);
    _field$3424 = ctx$999->$1;
    moonbit_decref(_field$3424);
    moonbit_free(ctx$999);
  }
  return _field$3062;
}

moonbit_string_t $$azimuth$telemetry$simple_test$SpanContext$$span_id(
  struct $SpanContext* ctx$998
) {
  moonbit_string_t _field$3063 = ctx$998->$1;
  int32_t _cnt$3427 = Moonbit_object_header(ctx$998)->rc;
  if (_cnt$3427 > 1) {
    int32_t _new_cnt$3430;
    moonbit_incref(_field$3063);
    _new_cnt$3430 = _cnt$3427 - 1;
    Moonbit_object_header(ctx$998)->rc = _new_cnt$3430;
  } else if (_cnt$3427 == 1) {
    moonbit_string_t _field$3429 = ctx$998->$3;
    moonbit_string_t _field$3428;
    moonbit_decref(_field$3429);
    _field$3428 = ctx$998->$0;
    moonbit_decref(_field$3428);
    moonbit_free(ctx$998);
  }
  return _field$3063;
}

struct $SpanContext* $$azimuth$telemetry$simple_test$SpanContext$$new(
  moonbit_string_t trace_id$994,
  moonbit_string_t span_id$995,
  int32_t sampled$996,
  moonbit_string_t trace_state$997
) {
  struct $SpanContext* _block$3652 =
    (struct $SpanContext*)moonbit_malloc(sizeof(struct $SpanContext));
  Moonbit_object_header(_block$3652)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $SpanContext, $0) >> 2, 3, 0
  );
  _block$3652->$0 = trace_id$994;
  _block$3652->$1 = span_id$995;
  _block$3652->$2 = sampled$996;
  _block$3652->$3 = trace_state$997;
  return _block$3652;
}

int32_t $$azimuth$telemetry$simple_test$SpanContext$$is_valid(
  struct $SpanContext* ctx$993
) {
  moonbit_string_t _field$3065 = ctx$993->$0;
  moonbit_string_t trace_id$2521 = _field$3065;
  moonbit_incref(trace_id$2521);
  if (
    $moonbitlang$core$builtin$op_notequal$1(
      trace_id$2521, (moonbit_string_t)moonbit_string_literal_3.data
    )
  ) {
    moonbit_string_t _field$3064 = ctx$993->$1;
    int32_t _cnt$3431 = Moonbit_object_header(ctx$993)->rc;
    moonbit_string_t span_id$2520;
    if (_cnt$3431 > 1) {
      int32_t _new_cnt$3434;
      moonbit_incref(_field$3064);
      _new_cnt$3434 = _cnt$3431 - 1;
      Moonbit_object_header(ctx$993)->rc = _new_cnt$3434;
    } else if (_cnt$3431 == 1) {
      moonbit_string_t _field$3433 = ctx$993->$3;
      moonbit_string_t _field$3432;
      moonbit_decref(_field$3433);
      _field$3432 = ctx$993->$0;
      moonbit_decref(_field$3432);
      moonbit_free(ctx$993);
    }
    span_id$2520 = _field$3064;
    return $moonbitlang$core$builtin$op_notequal$1(
             span_id$2520, (moonbit_string_t)moonbit_string_literal_3.data
           );
  } else {
    moonbit_decref(ctx$993);
    return 0;
  }
}

int32_t $$azimuth$telemetry$simple_test$SpanContext$$is_sampled(
  struct $SpanContext* ctx$992
) {
  int32_t _field$3066 = ctx$992->$2;
  moonbit_decref(ctx$992);
  return _field$3066;
}

struct moonbit_result_0 $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0(
  
) {
  struct $Attributes* attrs$990 =
    $$azimuth$telemetry$simple_test$Attributes$$new();
  void* StringValue$2518 =
    (void*)moonbit_malloc(sizeof(struct $AttributeValue$StringValue));
  void* _tmp$3067;
  moonbit_string_t _tmp$2519;
  Moonbit_object_header(StringValue$2518)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $AttributeValue$StringValue, $0) >> 2, 1, 0
  );
  ((struct $AttributeValue$StringValue*)StringValue$2518)->$0
  = (moonbit_string_t)moonbit_string_literal_114.data;
  moonbit_incref(attrs$990);
  $$azimuth$telemetry$simple_test$Attributes$$set(
    attrs$990,
      (moonbit_string_t)moonbit_string_literal_113.data,
      StringValue$2518
  );
  _tmp$3067
  = $$azimuth$telemetry$simple_test$Attributes$$get(
    attrs$990, (moonbit_string_t)moonbit_string_literal_113.data
  );
  if (_tmp$3067) {
    moonbit_decref(_tmp$3067);
  }
  _tmp$2519 = 0;
  return $moonbitlang$core$builtin$assert_true(
           1, _tmp$2519, (moonbit_string_t)moonbit_string_literal_122.data
         );
}

int32_t $$azimuth$telemetry$simple_test$Attributes$$set(
  struct $Attributes* attrs$987,
  moonbit_string_t key$988,
  void* value$989
) {
  moonbit_decref(value$989);
  moonbit_decref(key$988);
  moonbit_decref(attrs$987);
  return 0;
}

struct $Attributes* $$azimuth$telemetry$simple_test$Attributes$$new() {
  struct $$3c$String$2a$AttributeValue$3e$** _tmp$2517 =
    (struct $$3c$String$2a$AttributeValue$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* _tmp$2516 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$
      )
    );
  struct $Attributes* _block$3653;
  Moonbit_object_header(_tmp$2516)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  _tmp$2516->$0 = _tmp$2517;
  _tmp$2516->$1 = 0;
  _block$3653
  = (struct $Attributes*)moonbit_malloc(sizeof(struct $Attributes));
  Moonbit_object_header(_block$3653)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Attributes, $0) >> 2, 1, 0
  );
  _block$3653->$0 = _tmp$2516;
  return _block$3653;
}

void* $$azimuth$telemetry$simple_test$Attributes$$get(
  struct $Attributes* attrs$986,
  moonbit_string_t key$985
) {
  moonbit_decref(attrs$986);
  if (
    moonbit_val_array_equal(
      key$985, (moonbit_string_t)moonbit_string_literal_73.data
    )
  ) {
    void* StringValue$2514;
    moonbit_decref(key$985);
    StringValue$2514
    = (void*)moonbit_malloc(sizeof(struct $AttributeValue$StringValue));
    Moonbit_object_header(StringValue$2514)->meta
    = Moonbit_make_regular_object_header(
      offsetof(struct $AttributeValue$StringValue, $0) >> 2, 1, 0
    );
    ((struct $AttributeValue$StringValue*)StringValue$2514)->$0
    = (moonbit_string_t)moonbit_string_literal_114.data;
    return StringValue$2514;
  } else {
    int32_t _tmp$3068 =
      moonbit_val_array_equal(
        key$985, (moonbit_string_t)moonbit_string_literal_74.data
      );
    moonbit_decref(key$985);
    if (_tmp$3068) {
      void* IntValue$2515 =
        (void*)moonbit_malloc(sizeof(struct $AttributeValue$IntValue));
      Moonbit_object_header(IntValue$2515)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $AttributeValue$IntValue) >> 2, 0, 1
      );
      ((struct $AttributeValue$IntValue*)IntValue$2515)->$0 = 42;
      return IntValue$2515;
    } else {
      return 0;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$984
) {
  int32_t _field$3069 = self$984->$1;
  int32_t len$2513;
  moonbit_decref(self$984);
  len$2513 = _field$3069;
  return len$2513 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$982,
  struct $$moonbitlang$core$builtin$Logger logger$983
) {
  moonbit_string_t _tmp$2512 = self$982;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$2511 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$2512);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$2511, logger$983
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$968,
  struct $$moonbitlang$core$builtin$Logger logger$981
) {
  struct $StringView _field$3078 =
    (struct $StringView){self$968->$0_1, self$968->$0_2, self$968->$0_0};
  struct $StringView pkg$967 = _field$3078;
  int32_t _tmp$2510 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$2509;
  int64_t _bind$969;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$970;
  struct $StringView _field$3077;
  struct $StringView _module_name$977;
  void* _field$3076;
  int32_t _cnt$3435;
  void* _package_name$978;
  struct $StringView _field$3074;
  struct $StringView filename$2492;
  struct $StringView _field$3073;
  struct $StringView start_line$2493;
  struct $StringView _field$3072;
  struct $StringView start_column$2494;
  struct $StringView _field$3071;
  struct $StringView end_line$2495;
  struct $StringView _field$3070;
  int32_t _cnt$3439;
  struct $StringView end_column$2496;
  struct $$moonbitlang$core$builtin$Logger _bind$2491;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$2509
  = (struct $StringView){
    0, _tmp$2510, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$967.$0);
  moonbit_incref(pkg$967.$0);
  _bind$969 = $StringView$$find(pkg$967, _tmp$2509);
  if (_bind$969 == 4294967296ll) {
    void* None$2497 =
      (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
    _bind$970
    = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
        sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
      );
    Moonbit_object_header(_bind$970)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
      )
      >> 2,
        2,
        0
    );
    _bind$970->$0_0 = pkg$967.$0;
    _bind$970->$0_1 = pkg$967.$1;
    _bind$970->$0_2 = pkg$967.$2;
    _bind$970->$1 = None$2497;
  } else {
    int64_t _Some$971 = _bind$969;
    int32_t _first_slash$972 = (int32_t)_Some$971;
    int32_t _tmp$2508 = _first_slash$972 + 1;
    struct $StringView _tmp$2505;
    int32_t _tmp$2507;
    struct $StringView _tmp$2506;
    int64_t _bind$973;
    moonbit_incref(pkg$967.$0);
    _tmp$2505 = $StringView$$view$inner(pkg$967, _tmp$2508, 4294967296ll);
    _tmp$2507
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$2506
    = (struct $StringView){
      0, _tmp$2507, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$973 = $StringView$$find(_tmp$2505, _tmp$2506);
    if (_bind$973 == 4294967296ll) {
      void* None$2498 =
        (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$970
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$970)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$970->$0_0 = pkg$967.$0;
      _bind$970->$0_1 = pkg$967.$1;
      _bind$970->$0_2 = pkg$967.$2;
      _bind$970->$1 = None$2498;
    } else {
      int64_t _Some$974 = _bind$973;
      int32_t _second_slash$975 = (int32_t)_Some$974;
      int32_t _tmp$2504 = _first_slash$972 + 1;
      int32_t module_name_end$976 = _tmp$2504 + _second_slash$975;
      int64_t _tmp$2503 = (int64_t)module_name_end$976;
      struct $StringView _tmp$2499;
      int32_t _tmp$2502;
      struct $StringView _tmp$2501;
      void* Some$2500;
      moonbit_incref(pkg$967.$0);
      _tmp$2499 = $StringView$$view$inner(pkg$967, 0, _tmp$2503);
      _tmp$2502 = module_name_end$976 + 1;
      _tmp$2501 = $StringView$$view$inner(pkg$967, _tmp$2502, 4294967296ll);
      Some$2500
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$2500)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$2500)->$0_0
      = _tmp$2501.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2500)->$0_1
      = _tmp$2501.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2500)->$0_2
      = _tmp$2501.$2;
      _bind$970
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$970)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$970->$0_0 = _tmp$2499.$0;
      _bind$970->$0_1 = _tmp$2499.$1;
      _bind$970->$0_2 = _tmp$2499.$2;
      _bind$970->$1 = Some$2500;
    }
  }
  _field$3077
  = (struct $StringView){
    _bind$970->$0_1, _bind$970->$0_2, _bind$970->$0_0
  };
  _module_name$977 = _field$3077;
  _field$3076 = _bind$970->$1;
  _cnt$3435 = Moonbit_object_header(_bind$970)->rc;
  if (_cnt$3435 > 1) {
    int32_t _new_cnt$3436;
    moonbit_incref(_field$3076);
    moonbit_incref(_module_name$977.$0);
    _new_cnt$3436 = _cnt$3435 - 1;
    Moonbit_object_header(_bind$970)->rc = _new_cnt$3436;
  } else if (_cnt$3435 == 1) {
    moonbit_free(_bind$970);
  }
  _package_name$978 = _field$3076;
  switch (Moonbit_object_tag(_package_name$978)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$979 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$978;
      struct $StringView _field$3075 =
        (struct $StringView){
          _Some$979->$0_1, _Some$979->$0_2, _Some$979->$0_0
        };
      int32_t _cnt$3437 = Moonbit_object_header(_Some$979)->rc;
      struct $StringView _pkg_name$980;
      struct $$moonbitlang$core$builtin$Logger _bind$2490;
      if (_cnt$3437 > 1) {
        int32_t _new_cnt$3438;
        moonbit_incref(_field$3075.$0);
        _new_cnt$3438 = _cnt$3437 - 1;
        Moonbit_object_header(_Some$979)->rc = _new_cnt$3438;
      } else if (_cnt$3437 == 1) {
        moonbit_free(_Some$979);
      }
      _pkg_name$980 = _field$3075;
      if (logger$981.$1) {
        moonbit_incref(logger$981.$1);
      }
      logger$981.$0->$method_2(logger$981.$1, _pkg_name$980);
      _bind$2490 = logger$981;
      if (_bind$2490.$1) {
        moonbit_incref(_bind$2490.$1);
      }
      _bind$2490.$0->$method_3(_bind$2490.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$978);
      break;
    }
  }
  _field$3074
  = (struct $StringView){
    self$968->$1_1, self$968->$1_2, self$968->$1_0
  };
  filename$2492 = _field$3074;
  moonbit_incref(filename$2492.$0);
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_2(logger$981.$1, filename$2492);
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_3(logger$981.$1, 58);
  _field$3073
  = (struct $StringView){
    self$968->$2_1, self$968->$2_2, self$968->$2_0
  };
  start_line$2493 = _field$3073;
  moonbit_incref(start_line$2493.$0);
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_2(logger$981.$1, start_line$2493);
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_3(logger$981.$1, 58);
  _field$3072
  = (struct $StringView){
    self$968->$3_1, self$968->$3_2, self$968->$3_0
  };
  start_column$2494 = _field$3072;
  moonbit_incref(start_column$2494.$0);
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_2(logger$981.$1, start_column$2494);
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_3(logger$981.$1, 45);
  _field$3071
  = (struct $StringView){
    self$968->$4_1, self$968->$4_2, self$968->$4_0
  };
  end_line$2495 = _field$3071;
  moonbit_incref(end_line$2495.$0);
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_2(logger$981.$1, end_line$2495);
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_3(logger$981.$1, 58);
  _field$3070
  = (struct $StringView){
    self$968->$5_1, self$968->$5_2, self$968->$5_0
  };
  _cnt$3439 = Moonbit_object_header(self$968)->rc;
  if (_cnt$3439 > 1) {
    int32_t _new_cnt$3445;
    moonbit_incref(_field$3070.$0);
    _new_cnt$3445 = _cnt$3439 - 1;
    Moonbit_object_header(self$968)->rc = _new_cnt$3445;
  } else if (_cnt$3439 == 1) {
    struct $StringView _field$3444 =
      (struct $StringView){self$968->$4_1, self$968->$4_2, self$968->$4_0};
    struct $StringView _field$3443;
    struct $StringView _field$3442;
    struct $StringView _field$3441;
    struct $StringView _field$3440;
    moonbit_decref(_field$3444.$0);
    _field$3443
    = (struct $StringView){
      self$968->$3_1, self$968->$3_2, self$968->$3_0
    };
    moonbit_decref(_field$3443.$0);
    _field$3442
    = (struct $StringView){
      self$968->$2_1, self$968->$2_2, self$968->$2_0
    };
    moonbit_decref(_field$3442.$0);
    _field$3441
    = (struct $StringView){
      self$968->$1_1, self$968->$1_2, self$968->$1_0
    };
    moonbit_decref(_field$3441.$0);
    _field$3440
    = (struct $StringView){
      self$968->$0_1, self$968->$0_2, self$968->$0_0
    };
    moonbit_decref(_field$3440.$0);
    moonbit_free(self$968);
  }
  end_column$2496 = _field$3070;
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_2(logger$981.$1, end_column$2496);
  if (logger$981.$1) {
    moonbit_incref(logger$981.$1);
  }
  logger$981.$0->$method_3(logger$981.$1, 64);
  _bind$2491 = logger$981;
  _bind$2491.$0->$method_2(_bind$2491.$1, _module_name$977);
  return 0;
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$966) {
  moonbit_string_t _tmp$2489 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$966);
  moonbit_println(_tmp$2489);
  moonbit_decref(_tmp$2489);
  return 0;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$961,
  moonbit_string_t msg$963,
  moonbit_string_t loc$965
) {
  if (!x$961) {
    moonbit_string_t fail_msg$962;
    if (msg$963 == 0) {
      moonbit_string_t _tmp$2487;
      moonbit_string_t _tmp$2486;
      if (msg$963) {
        moonbit_decref(msg$963);
      }
      _tmp$2487
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$961
      );
      _tmp$2486
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_123.data, _tmp$2487
      );
      fail_msg$962
      = moonbit_add_string(
        _tmp$2486, (moonbit_string_t)moonbit_string_literal_124.data
      );
    } else {
      moonbit_string_t _Some$964 = msg$963;
      fail_msg$962 = _Some$964;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$962, loc$965);
  } else {
    int32_t _tmp$2488;
    struct moonbit_result_0 _result$3654;
    moonbit_decref(loc$965);
    if (msg$963) {
      moonbit_decref(msg$963);
    }
    _tmp$2488 = 0;
    _result$3654.tag = 1;
    _result$3654.data.ok = _tmp$2488;
    return _result$3654;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$956,
  moonbit_string_t msg$958,
  moonbit_string_t loc$960
) {
  if (x$956) {
    moonbit_string_t fail_msg$957;
    if (msg$958 == 0) {
      moonbit_string_t _tmp$2484;
      moonbit_string_t _tmp$2483;
      if (msg$958) {
        moonbit_decref(msg$958);
      }
      _tmp$2484
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$956
      );
      _tmp$2483
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_123.data, _tmp$2484
      );
      fail_msg$957
      = moonbit_add_string(
        _tmp$2483, (moonbit_string_t)moonbit_string_literal_125.data
      );
    } else {
      moonbit_string_t _Some$959 = msg$958;
      fail_msg$957 = _Some$959;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$957, loc$960);
  } else {
    int32_t _tmp$2485;
    struct moonbit_result_0 _result$3655;
    moonbit_decref(loc$960);
    if (msg$958) {
      moonbit_decref(msg$958);
    }
    _tmp$2485 = 0;
    _result$3655.tag = 1;
    _result$3655.data.ok = _tmp$2485;
    return _result$3655;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$955,
  struct $$moonbitlang$core$builtin$Hasher* hasher$954
) {
  $$moonbitlang$core$builtin$Hasher$$combine_int(hasher$954, self$955);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$953,
  struct $$moonbitlang$core$builtin$Hasher* hasher$952
) {
  $$moonbitlang$core$builtin$Hasher$$combine_string(hasher$952, self$953);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$950,
  moonbit_string_t value$948
) {
  int32_t _end2448$947 = Moonbit_array_length(value$948);
  int32_t i$949 = 0;
  while (1) {
    if (i$949 < _end2448$947) {
      int32_t _tmp$2481 = value$948[i$949];
      uint32_t _tmp$2480 = *(uint32_t*)&_tmp$2481;
      int32_t _tmp$2482;
      moonbit_incref(self$950);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$950, _tmp$2480);
      _tmp$2482 = i$949 + 1;
      i$949 = _tmp$2482;
      continue;
    } else {
      moonbit_decref(self$950);
      moonbit_decref(value$948);
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$944,
  struct $$3c$String$3e$$3d$$3e$Int* f$946
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$3657 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$2477;
  int32_t _tmp$2476;
  Moonbit_object_header(_closure$3657)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$3657->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$3657->$0 = f$946;
  _tmp$2477 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$3657;
  _tmp$2476 = $$moonbitlang$core$builtin$Iter$$run$0(self$944, _tmp$2477);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$2476, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2478,
  moonbit_string_t k$945
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$2479 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$2478;
  struct $$3c$String$3e$$3d$$3e$Int* _field$3079 = _casted_env$2479->$0;
  int32_t _cnt$3446 = Moonbit_object_header(_casted_env$2479)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$946;
  if (_cnt$3446 > 1) {
    int32_t _new_cnt$3447;
    moonbit_incref(_field$3079);
    _new_cnt$3447 = _cnt$3446 - 1;
    Moonbit_object_header(_casted_env$2479)->rc = _new_cnt$3447;
  } else if (_cnt$3446 == 1) {
    moonbit_free(_casted_env$2479);
  }
  f$946 = _field$3079;
  if (f$946->code(f$946, k$945)) {
    return 0;
  } else {
    return 1;
  }
}

struct $$3c$String$2a$String$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$3(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$942,
  int32_t idx$943
) {
  struct $$3c$String$2a$String$3e$** _tmp$2475 =
    $$moonbitlang$core$builtin$Array$$buffer$4(self$942);
  struct $$3c$String$2a$String$3e$* _tmp$3080;
  if (idx$943 < 0 || idx$943 >= Moonbit_array_length(_tmp$2475)) {
    moonbit_panic();
  }
  _tmp$3080 = (struct $$3c$String$2a$String$3e$*)_tmp$2475[idx$943];
  if (_tmp$3080) {
    moonbit_incref(_tmp$3080);
  }
  moonbit_decref(_tmp$2475);
  return _tmp$3080;
}

struct $$3c$String$2a$AttributeValue$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$2(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$940,
  int32_t idx$941
) {
  struct $$3c$String$2a$AttributeValue$3e$** _tmp$2474 =
    $$moonbitlang$core$builtin$Array$$buffer$3(self$940);
  struct $$3c$String$2a$AttributeValue$3e$* _tmp$3081;
  if (idx$941 < 0 || idx$941 >= Moonbit_array_length(_tmp$2474)) {
    moonbit_panic();
  }
  _tmp$3081 = (struct $$3c$String$2a$AttributeValue$3e$*)_tmp$2474[idx$941];
  if (_tmp$3081) {
    moonbit_incref(_tmp$3081);
  }
  moonbit_decref(_tmp$2474);
  return _tmp$3081;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$938,
  int32_t idx$939
) {
  moonbit_string_t* _tmp$2473 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$938);
  moonbit_string_t _tmp$3082;
  if (idx$939 < 0 || idx$939 >= Moonbit_array_length(_tmp$2473)) {
    moonbit_panic();
  }
  _tmp$3082 = (moonbit_string_t)_tmp$2473[idx$939];
  moonbit_incref(_tmp$3082);
  moonbit_decref(_tmp$2473);
  return _tmp$3082;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$936,
  int32_t idx$937
) {
  struct $$3c$String$2a$Int$3e$** _tmp$2472 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$936);
  struct $$3c$String$2a$Int$3e$* _tmp$3083;
  if (idx$937 < 0 || idx$937 >= Moonbit_array_length(_tmp$2472)) {
    moonbit_panic();
  }
  _tmp$3083 = (struct $$3c$String$2a$Int$3e$*)_tmp$2472[idx$937];
  if (_tmp$3083) {
    moonbit_incref(_tmp$3083);
  }
  moonbit_decref(_tmp$2472);
  return _tmp$3083;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$932,
  int32_t key$928
) {
  int32_t hash$927 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$928);
  int32_t capacity_mask$2471 = self$932->$3;
  int32_t _tmp$2470 = hash$927 & capacity_mask$2471;
  int32_t i$929 = 0;
  int32_t idx$930 = _tmp$2470;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3087 =
      self$932->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2469 =
      _field$3087;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3086;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$931;
    if (idx$930 < 0 || idx$930 >= Moonbit_array_length(entries$2469)) {
      moonbit_panic();
    }
    _tmp$3086
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2469[
        idx$930
      ];
    _bind$931 = _tmp$3086;
    if (_bind$931 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2458;
      if (_bind$931) {
        moonbit_incref(_bind$931);
      }
      moonbit_decref(self$932);
      if (_bind$931) {
        moonbit_decref(_bind$931);
      }
      _tmp$2458 = 0;
      return _tmp$2458;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$933 =
        _bind$931;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$934 =
        _Some$933;
      int32_t hash$2460 = _entry$934->$3;
      int32_t _if_result$3659;
      int32_t _field$3084;
      int32_t psl$2463;
      int32_t _tmp$2465;
      int32_t _tmp$2467;
      int32_t capacity_mask$2468;
      int32_t _tmp$2466;
      if (hash$2460 == hash$927) {
        int32_t key$2459 = _entry$934->$4;
        _if_result$3659 = key$2459 == key$928;
      } else {
        _if_result$3659 = 0;
      }
      if (_if_result$3659) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3085;
        int32_t _cnt$3448;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2462;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2461;
        moonbit_incref(_entry$934);
        moonbit_decref(self$932);
        _field$3085 = _entry$934->$5;
        _cnt$3448 = Moonbit_object_header(_entry$934)->rc;
        if (_cnt$3448 > 1) {
          int32_t _new_cnt$3450;
          moonbit_incref(_field$3085);
          _new_cnt$3450 = _cnt$3448 - 1;
          Moonbit_object_header(_entry$934)->rc = _new_cnt$3450;
        } else if (_cnt$3448 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3449 =
            _entry$934->$1;
          if (_field$3449) {
            moonbit_decref(_field$3449);
          }
          moonbit_free(_entry$934);
        }
        value$2462 = _field$3085;
        _tmp$2461 = value$2462;
        return _tmp$2461;
      } else {
        moonbit_incref(_entry$934);
      }
      _field$3084 = _entry$934->$2;
      moonbit_decref(_entry$934);
      psl$2463 = _field$3084;
      if (i$929 > psl$2463) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2464;
        moonbit_decref(self$932);
        _tmp$2464 = 0;
        return _tmp$2464;
      }
      _tmp$2465 = i$929 + 1;
      _tmp$2467 = idx$930 + 1;
      capacity_mask$2468 = self$932->$3;
      _tmp$2466 = _tmp$2467 & capacity_mask$2468;
      i$929 = _tmp$2465;
      idx$930 = _tmp$2466;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$923,
  moonbit_string_t key$919
) {
  int32_t hash$918;
  int32_t capacity_mask$2457;
  int32_t _tmp$2456;
  int32_t i$920;
  int32_t idx$921;
  moonbit_incref(key$919);
  hash$918 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$919);
  capacity_mask$2457 = self$923->$3;
  _tmp$2456 = hash$918 & capacity_mask$2457;
  i$920 = 0;
  idx$921 = _tmp$2456;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3093 =
      self$923->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2455 =
      _field$3093;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3092;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$922;
    if (idx$921 < 0 || idx$921 >= Moonbit_array_length(entries$2455)) {
      moonbit_panic();
    }
    _tmp$3092
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2455[
        idx$921
      ];
    _bind$922 = _tmp$3092;
    if (_bind$922 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2444;
      if (_bind$922) {
        moonbit_incref(_bind$922);
      }
      moonbit_decref(self$923);
      if (_bind$922) {
        moonbit_decref(_bind$922);
      }
      moonbit_decref(key$919);
      _tmp$2444 = 0;
      return _tmp$2444;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$924 =
        _bind$922;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$925 =
        _Some$924;
      int32_t hash$2446 = _entry$925->$3;
      int32_t _if_result$3661;
      int32_t _field$3088;
      int32_t psl$2449;
      int32_t _tmp$2451;
      int32_t _tmp$2453;
      int32_t capacity_mask$2454;
      int32_t _tmp$2452;
      if (hash$2446 == hash$918) {
        moonbit_string_t _field$3091 = _entry$925->$4;
        moonbit_string_t key$2445 = _field$3091;
        int32_t _tmp$3090 = moonbit_val_array_equal(key$2445, key$919);
        _if_result$3661 = _tmp$3090;
      } else {
        _if_result$3661 = 0;
      }
      if (_if_result$3661) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3089;
        int32_t _cnt$3451;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2448;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2447;
        moonbit_incref(_entry$925);
        moonbit_decref(self$923);
        moonbit_decref(key$919);
        _field$3089 = _entry$925->$5;
        _cnt$3451 = Moonbit_object_header(_entry$925)->rc;
        if (_cnt$3451 > 1) {
          int32_t _new_cnt$3454;
          moonbit_incref(_field$3089);
          _new_cnt$3454 = _cnt$3451 - 1;
          Moonbit_object_header(_entry$925)->rc = _new_cnt$3454;
        } else if (_cnt$3451 == 1) {
          moonbit_string_t _field$3453 = _entry$925->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3452;
          moonbit_decref(_field$3453);
          _field$3452 = _entry$925->$1;
          if (_field$3452) {
            moonbit_decref(_field$3452);
          }
          moonbit_free(_entry$925);
        }
        value$2448 = _field$3089;
        _tmp$2447 = value$2448;
        return _tmp$2447;
      } else {
        moonbit_incref(_entry$925);
      }
      _field$3088 = _entry$925->$2;
      moonbit_decref(_entry$925);
      psl$2449 = _field$3088;
      if (i$920 > psl$2449) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2450;
        moonbit_decref(self$923);
        moonbit_decref(key$919);
        _tmp$2450 = 0;
        return _tmp$2450;
      }
      _tmp$2451 = i$920 + 1;
      _tmp$2453 = idx$921 + 1;
      capacity_mask$2454 = self$923->$3;
      _tmp$2452 = _tmp$2453 & capacity_mask$2454;
      i$920 = _tmp$2451;
      idx$921 = _tmp$2452;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$914,
  int32_t key$910
) {
  int32_t hash$909 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$910);
  int32_t capacity_mask$2443 = self$914->$3;
  int32_t _tmp$2442 = hash$909 & capacity_mask$2443;
  int32_t i$911 = 0;
  int32_t idx$912 = _tmp$2442;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3097 =
      self$914->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2441 =
      _field$3097;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3096;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$913;
    if (idx$912 < 0 || idx$912 >= Moonbit_array_length(entries$2441)) {
      moonbit_panic();
    }
    _tmp$3096
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2441[
        idx$912
      ];
    _bind$913 = _tmp$3096;
    if (_bind$913 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2430;
      if (_bind$913) {
        moonbit_incref(_bind$913);
      }
      moonbit_decref(self$914);
      if (_bind$913) {
        moonbit_decref(_bind$913);
      }
      _tmp$2430 = 0;
      return _tmp$2430;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$915 =
        _bind$913;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$916 =
        _Some$915;
      int32_t hash$2432 = _entry$916->$3;
      int32_t _if_result$3663;
      int32_t _field$3094;
      int32_t psl$2435;
      int32_t _tmp$2437;
      int32_t _tmp$2439;
      int32_t capacity_mask$2440;
      int32_t _tmp$2438;
      if (hash$2432 == hash$909) {
        int32_t key$2431 = _entry$916->$4;
        _if_result$3663 = key$2431 == key$910;
      } else {
        _if_result$3663 = 0;
      }
      if (_if_result$3663) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3095;
        int32_t _cnt$3455;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2434;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2433;
        moonbit_incref(_entry$916);
        moonbit_decref(self$914);
        _field$3095 = _entry$916->$5;
        _cnt$3455 = Moonbit_object_header(_entry$916)->rc;
        if (_cnt$3455 > 1) {
          int32_t _new_cnt$3457;
          moonbit_incref(_field$3095);
          _new_cnt$3457 = _cnt$3455 - 1;
          Moonbit_object_header(_entry$916)->rc = _new_cnt$3457;
        } else if (_cnt$3455 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3456 =
            _entry$916->$1;
          if (_field$3456) {
            moonbit_decref(_field$3456);
          }
          moonbit_free(_entry$916);
        }
        value$2434 = _field$3095;
        _tmp$2433 = value$2434;
        return _tmp$2433;
      } else {
        moonbit_incref(_entry$916);
      }
      _field$3094 = _entry$916->$2;
      moonbit_decref(_entry$916);
      psl$2435 = _field$3094;
      if (i$911 > psl$2435) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2436;
        moonbit_decref(self$914);
        _tmp$2436 = 0;
        return _tmp$2436;
      }
      _tmp$2437 = i$911 + 1;
      _tmp$2439 = idx$912 + 1;
      capacity_mask$2440 = self$914->$3;
      _tmp$2438 = _tmp$2439 & capacity_mask$2440;
      i$911 = _tmp$2437;
      idx$912 = _tmp$2438;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$905,
  moonbit_string_t key$901
) {
  int32_t hash$900;
  int32_t capacity_mask$2429;
  int32_t _tmp$2428;
  int32_t i$902;
  int32_t idx$903;
  moonbit_incref(key$901);
  hash$900 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$901);
  capacity_mask$2429 = self$905->$3;
  _tmp$2428 = hash$900 & capacity_mask$2429;
  i$902 = 0;
  idx$903 = _tmp$2428;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3103 =
      self$905->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2427 =
      _field$3103;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3102;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$904;
    if (idx$903 < 0 || idx$903 >= Moonbit_array_length(entries$2427)) {
      moonbit_panic();
    }
    _tmp$3102
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2427[
        idx$903
      ];
    _bind$904 = _tmp$3102;
    if (_bind$904 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2416;
      if (_bind$904) {
        moonbit_incref(_bind$904);
      }
      moonbit_decref(self$905);
      if (_bind$904) {
        moonbit_decref(_bind$904);
      }
      moonbit_decref(key$901);
      _tmp$2416 = 0;
      return _tmp$2416;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$906 =
        _bind$904;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$907 =
        _Some$906;
      int32_t hash$2418 = _entry$907->$3;
      int32_t _if_result$3665;
      int32_t _field$3098;
      int32_t psl$2421;
      int32_t _tmp$2423;
      int32_t _tmp$2425;
      int32_t capacity_mask$2426;
      int32_t _tmp$2424;
      if (hash$2418 == hash$900) {
        moonbit_string_t _field$3101 = _entry$907->$4;
        moonbit_string_t key$2417 = _field$3101;
        int32_t _tmp$3100 = moonbit_val_array_equal(key$2417, key$901);
        _if_result$3665 = _tmp$3100;
      } else {
        _if_result$3665 = 0;
      }
      if (_if_result$3665) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3099;
        int32_t _cnt$3458;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2420;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2419;
        moonbit_incref(_entry$907);
        moonbit_decref(self$905);
        moonbit_decref(key$901);
        _field$3099 = _entry$907->$5;
        _cnt$3458 = Moonbit_object_header(_entry$907)->rc;
        if (_cnt$3458 > 1) {
          int32_t _new_cnt$3461;
          moonbit_incref(_field$3099);
          _new_cnt$3461 = _cnt$3458 - 1;
          Moonbit_object_header(_entry$907)->rc = _new_cnt$3461;
        } else if (_cnt$3458 == 1) {
          moonbit_string_t _field$3460 = _entry$907->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3459;
          moonbit_decref(_field$3460);
          _field$3459 = _entry$907->$1;
          if (_field$3459) {
            moonbit_decref(_field$3459);
          }
          moonbit_free(_entry$907);
        }
        value$2420 = _field$3099;
        _tmp$2419 = value$2420;
        return _tmp$2419;
      } else {
        moonbit_incref(_entry$907);
      }
      _field$3098 = _entry$907->$2;
      moonbit_decref(_entry$907);
      psl$2421 = _field$3098;
      if (i$902 > psl$2421) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2422;
        moonbit_decref(self$905);
        moonbit_decref(key$901);
        _tmp$2422 = 0;
        return _tmp$2422;
      }
      _tmp$2423 = i$902 + 1;
      _tmp$2425 = idx$903 + 1;
      capacity_mask$2426 = self$905->$3;
      _tmp$2424 = _tmp$2425 & capacity_mask$2426;
      i$902 = _tmp$2423;
      idx$903 = _tmp$2424;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$896,
  int32_t key$892
) {
  int32_t hash$891 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$892);
  int32_t capacity_mask$2415 = self$896->$3;
  int32_t _tmp$2414 = hash$891 & capacity_mask$2415;
  int32_t i$893 = 0;
  int32_t idx$894 = _tmp$2414;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3107 =
      self$896->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2413 =
      _field$3107;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3106;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$895;
    if (idx$894 < 0 || idx$894 >= Moonbit_array_length(entries$2413)) {
      moonbit_panic();
    }
    _tmp$3106
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2413[
        idx$894
      ];
    _bind$895 = _tmp$3106;
    if (_bind$895 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2402;
      if (_bind$895) {
        moonbit_incref(_bind$895);
      }
      moonbit_decref(self$896);
      if (_bind$895) {
        moonbit_decref(_bind$895);
      }
      _tmp$2402 = 0;
      return _tmp$2402;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$897 =
        _bind$895;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$898 =
        _Some$897;
      int32_t hash$2404 = _entry$898->$3;
      int32_t _if_result$3667;
      int32_t _field$3104;
      int32_t psl$2407;
      int32_t _tmp$2409;
      int32_t _tmp$2411;
      int32_t capacity_mask$2412;
      int32_t _tmp$2410;
      if (hash$2404 == hash$891) {
        int32_t key$2403 = _entry$898->$4;
        _if_result$3667 = key$2403 == key$892;
      } else {
        _if_result$3667 = 0;
      }
      if (_if_result$3667) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3105;
        int32_t _cnt$3462;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2406;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2405;
        moonbit_incref(_entry$898);
        moonbit_decref(self$896);
        _field$3105 = _entry$898->$5;
        _cnt$3462 = Moonbit_object_header(_entry$898)->rc;
        if (_cnt$3462 > 1) {
          int32_t _new_cnt$3464;
          moonbit_incref(_field$3105);
          _new_cnt$3464 = _cnt$3462 - 1;
          Moonbit_object_header(_entry$898)->rc = _new_cnt$3464;
        } else if (_cnt$3462 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3463 =
            _entry$898->$1;
          if (_field$3463) {
            moonbit_decref(_field$3463);
          }
          moonbit_free(_entry$898);
        }
        value$2406 = _field$3105;
        _tmp$2405 = value$2406;
        return _tmp$2405;
      } else {
        moonbit_incref(_entry$898);
      }
      _field$3104 = _entry$898->$2;
      moonbit_decref(_entry$898);
      psl$2407 = _field$3104;
      if (i$893 > psl$2407) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2408;
        moonbit_decref(self$896);
        _tmp$2408 = 0;
        return _tmp$2408;
      }
      _tmp$2409 = i$893 + 1;
      _tmp$2411 = idx$894 + 1;
      capacity_mask$2412 = self$896->$3;
      _tmp$2410 = _tmp$2411 & capacity_mask$2412;
      i$893 = _tmp$2409;
      idx$894 = _tmp$2410;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$887,
  moonbit_string_t key$883
) {
  int32_t hash$882;
  int32_t capacity_mask$2401;
  int32_t _tmp$2400;
  int32_t i$884;
  int32_t idx$885;
  moonbit_incref(key$883);
  hash$882 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$883);
  capacity_mask$2401 = self$887->$3;
  _tmp$2400 = hash$882 & capacity_mask$2401;
  i$884 = 0;
  idx$885 = _tmp$2400;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3113 =
      self$887->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2399 =
      _field$3113;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3112;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$886;
    if (idx$885 < 0 || idx$885 >= Moonbit_array_length(entries$2399)) {
      moonbit_panic();
    }
    _tmp$3112
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2399[
        idx$885
      ];
    _bind$886 = _tmp$3112;
    if (_bind$886 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2388;
      if (_bind$886) {
        moonbit_incref(_bind$886);
      }
      moonbit_decref(self$887);
      if (_bind$886) {
        moonbit_decref(_bind$886);
      }
      moonbit_decref(key$883);
      _tmp$2388 = 0;
      return _tmp$2388;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$888 =
        _bind$886;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$889 =
        _Some$888;
      int32_t hash$2390 = _entry$889->$3;
      int32_t _if_result$3669;
      int32_t _field$3108;
      int32_t psl$2393;
      int32_t _tmp$2395;
      int32_t _tmp$2397;
      int32_t capacity_mask$2398;
      int32_t _tmp$2396;
      if (hash$2390 == hash$882) {
        moonbit_string_t _field$3111 = _entry$889->$4;
        moonbit_string_t key$2389 = _field$3111;
        int32_t _tmp$3110 = moonbit_val_array_equal(key$2389, key$883);
        _if_result$3669 = _tmp$3110;
      } else {
        _if_result$3669 = 0;
      }
      if (_if_result$3669) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3109;
        int32_t _cnt$3465;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2392;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2391;
        moonbit_incref(_entry$889);
        moonbit_decref(self$887);
        moonbit_decref(key$883);
        _field$3109 = _entry$889->$5;
        _cnt$3465 = Moonbit_object_header(_entry$889)->rc;
        if (_cnt$3465 > 1) {
          int32_t _new_cnt$3468;
          moonbit_incref(_field$3109);
          _new_cnt$3468 = _cnt$3465 - 1;
          Moonbit_object_header(_entry$889)->rc = _new_cnt$3468;
        } else if (_cnt$3465 == 1) {
          moonbit_string_t _field$3467 = _entry$889->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3466;
          moonbit_decref(_field$3467);
          _field$3466 = _entry$889->$1;
          if (_field$3466) {
            moonbit_decref(_field$3466);
          }
          moonbit_free(_entry$889);
        }
        value$2392 = _field$3109;
        _tmp$2391 = value$2392;
        return _tmp$2391;
      } else {
        moonbit_incref(_entry$889);
      }
      _field$3108 = _entry$889->$2;
      moonbit_decref(_entry$889);
      psl$2393 = _field$3108;
      if (i$884 > psl$2393) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2394;
        moonbit_decref(self$887);
        moonbit_decref(key$883);
        _tmp$2394 = 0;
        return _tmp$2394;
      }
      _tmp$2395 = i$884 + 1;
      _tmp$2397 = idx$885 + 1;
      capacity_mask$2398 = self$887->$3;
      _tmp$2396 = _tmp$2397 & capacity_mask$2398;
      i$884 = _tmp$2395;
      idx$885 = _tmp$2396;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$875
) {
  int32_t length$874;
  int32_t capacity$876;
  int32_t _tmp$2379;
  int32_t _tmp$2378;
  int32_t _tmp$2387;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$877;
  int32_t _len$878;
  int32_t _i$879;
  moonbit_incref(arr$875.$0);
  length$874 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$875);
  capacity$876 = $Int$$next_power_of_two(length$874);
  _tmp$2379 = capacity$876;
  _tmp$2378 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2379);
  if (length$874 > _tmp$2378) {
    int32_t _tmp$2380 = capacity$876;
    capacity$876 = _tmp$2380 * 2;
  }
  _tmp$2387 = capacity$876;
  m$877 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$2387);
  moonbit_incref(arr$875.$0);
  _len$878 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$875);
  _i$879 = 0;
  while (1) {
    if (_i$879 < _len$878) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3117 =
        arr$875.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2383 =
        _field$3117;
      int32_t start$2385 = arr$875.$1;
      int32_t _tmp$2384 = start$2385 + _i$879;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3116 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2383[
          _tmp$2384
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$880 =
        _tmp$3116;
      moonbit_string_t _field$3115 = e$880->$0;
      moonbit_string_t _tmp$2381 = _field$3115;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3114 =
        e$880->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2382 =
        _field$3114;
      int32_t _tmp$2386;
      moonbit_incref(_tmp$2382);
      moonbit_incref(_tmp$2381);
      moonbit_incref(m$877);
      $$moonbitlang$core$builtin$Map$$set$3(m$877, _tmp$2381, _tmp$2382);
      _tmp$2386 = _i$879 + 1;
      _i$879 = _tmp$2386;
      continue;
    } else {
      moonbit_decref(arr$875.$0);
    }
    break;
  }
  return m$877;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$867
) {
  int32_t length$866;
  int32_t capacity$868;
  int32_t _tmp$2369;
  int32_t _tmp$2368;
  int32_t _tmp$2377;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$869;
  int32_t _len$870;
  int32_t _i$871;
  moonbit_incref(arr$867.$0);
  length$866 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$867);
  capacity$868 = $Int$$next_power_of_two(length$866);
  _tmp$2369 = capacity$868;
  _tmp$2368 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2369);
  if (length$866 > _tmp$2368) {
    int32_t _tmp$2370 = capacity$868;
    capacity$868 = _tmp$2370 * 2;
  }
  _tmp$2377 = capacity$868;
  m$869 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$2377);
  moonbit_incref(arr$867.$0);
  _len$870 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$867);
  _i$871 = 0;
  while (1) {
    if (_i$871 < _len$870) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3121 =
        arr$867.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2373 =
        _field$3121;
      int32_t start$2375 = arr$867.$1;
      int32_t _tmp$2374 = start$2375 + _i$871;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3120 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2373[
          _tmp$2374
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$872 =
        _tmp$3120;
      moonbit_string_t _field$3119 = e$872->$0;
      moonbit_string_t _tmp$2371 = _field$3119;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3118 =
        e$872->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2372 =
        _field$3118;
      int32_t _tmp$2376;
      moonbit_incref(_tmp$2372);
      moonbit_incref(_tmp$2371);
      moonbit_incref(m$869);
      $$moonbitlang$core$builtin$Map$$set$2(m$869, _tmp$2371, _tmp$2372);
      _tmp$2376 = _i$871 + 1;
      _i$871 = _tmp$2376;
      continue;
    } else {
      moonbit_decref(arr$867.$0);
    }
    break;
  }
  return m$869;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$859
) {
  int32_t length$858;
  int32_t capacity$860;
  int32_t _tmp$2359;
  int32_t _tmp$2358;
  int32_t _tmp$2367;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$861;
  int32_t _len$862;
  int32_t _i$863;
  moonbit_incref(arr$859.$0);
  length$858 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$859);
  capacity$860 = $Int$$next_power_of_two(length$858);
  _tmp$2359 = capacity$860;
  _tmp$2358 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2359);
  if (length$858 > _tmp$2358) {
    int32_t _tmp$2360 = capacity$860;
    capacity$860 = _tmp$2360 * 2;
  }
  _tmp$2367 = capacity$860;
  m$861 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$2367);
  moonbit_incref(arr$859.$0);
  _len$862 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$859);
  _i$863 = 0;
  while (1) {
    if (_i$863 < _len$862) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3124 =
        arr$859.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$2363 =
        _field$3124;
      int32_t start$2365 = arr$859.$1;
      int32_t _tmp$2364 = start$2365 + _i$863;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3123 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$2363[
          _tmp$2364
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$864 =
        _tmp$3123;
      int32_t _tmp$2361 = e$864->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3122 =
        e$864->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2362 =
        _field$3122;
      int32_t _tmp$2366;
      moonbit_incref(_tmp$2362);
      moonbit_incref(m$861);
      $$moonbitlang$core$builtin$Map$$set$1(m$861, _tmp$2361, _tmp$2362);
      _tmp$2366 = _i$863 + 1;
      _i$863 = _tmp$2366;
      continue;
    } else {
      moonbit_decref(arr$859.$0);
    }
    break;
  }
  return m$861;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$851
) {
  int32_t length$850;
  int32_t capacity$852;
  int32_t _tmp$2349;
  int32_t _tmp$2348;
  int32_t _tmp$2357;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$853;
  int32_t _len$854;
  int32_t _i$855;
  moonbit_incref(arr$851.$0);
  length$850 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$851);
  capacity$852 = $Int$$next_power_of_two(length$850);
  _tmp$2349 = capacity$852;
  _tmp$2348 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2349);
  if (length$850 > _tmp$2348) {
    int32_t _tmp$2350 = capacity$852;
    capacity$852 = _tmp$2350 * 2;
  }
  _tmp$2357 = capacity$852;
  m$853 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$2357);
  moonbit_incref(arr$851.$0);
  _len$854 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$851);
  _i$855 = 0;
  while (1) {
    if (_i$855 < _len$854) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3128 =
        arr$851.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2353 =
        _field$3128;
      int32_t start$2355 = arr$851.$1;
      int32_t _tmp$2354 = start$2355 + _i$855;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3127 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2353[
          _tmp$2354
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$856 =
        _tmp$3127;
      moonbit_string_t _field$3126 = e$856->$0;
      moonbit_string_t _tmp$2351 = _field$3126;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3125 =
        e$856->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2352 =
        _field$3125;
      int32_t _tmp$2356;
      moonbit_incref(_tmp$2352);
      moonbit_incref(_tmp$2351);
      moonbit_incref(m$853);
      $$moonbitlang$core$builtin$Map$$set$0(m$853, _tmp$2351, _tmp$2352);
      _tmp$2356 = _i$855 + 1;
      _i$855 = _tmp$2356;
      continue;
    } else {
      moonbit_decref(arr$851.$0);
    }
    break;
  }
  return m$853;
}

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$847,
  moonbit_string_t key$848,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$849
) {
  int32_t _tmp$2347;
  moonbit_incref(key$848);
  _tmp$2347 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$848);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$847, key$848, value$849, _tmp$2347
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$844,
  moonbit_string_t key$845,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$846
) {
  int32_t _tmp$2346;
  moonbit_incref(key$845);
  _tmp$2346 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$845);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$844, key$845, value$846, _tmp$2346
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$841,
  int32_t key$842,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$843
) {
  int32_t _tmp$2345 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$842);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$841, key$842, value$843, _tmp$2345
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$838,
  moonbit_string_t key$839,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$840
) {
  int32_t _tmp$2344;
  moonbit_incref(key$839);
  _tmp$2344 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$839);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$838, key$839, value$840, _tmp$2344
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$828
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3135 =
    self$828->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$827 =
    _field$3135;
  int32_t capacity$2343 = self$828->$2;
  int32_t new_capacity$829 = capacity$2343 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2338 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2337 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$829, _tmp$2338
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$3134 =
    self$828->$0;
  int32_t _tmp$2339;
  int32_t capacity$2341;
  int32_t _tmp$2340;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2342;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3133;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$830;
  if (old_head$827) {
    moonbit_incref(old_head$827);
  }
  moonbit_decref(_old$3134);
  self$828->$0 = _tmp$2337;
  self$828->$2 = new_capacity$829;
  _tmp$2339 = new_capacity$829 - 1;
  self$828->$3 = _tmp$2339;
  capacity$2341 = self$828->$2;
  _tmp$2340 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2341);
  self$828->$4 = _tmp$2340;
  self$828->$1 = 0;
  _tmp$2342 = 0;
  _old$3133 = self$828->$5;
  if (_old$3133) {
    moonbit_decref(_old$3133);
  }
  self$828->$5 = _tmp$2342;
  self$828->$6 = -1;
  _param$830 = old_head$827;
  while (1) {
    if (_param$830 == 0) {
      if (_param$830) {
        moonbit_decref(_param$830);
      }
      moonbit_decref(self$828);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$831 =
        _param$830;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$832 =
        _Some$831;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3132 =
        _x$832->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$833 =
        _field$3132;
      moonbit_string_t _field$3131 = _x$832->$4;
      moonbit_string_t _key$834 = _field$3131;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3130 =
        _x$832->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$835 =
        _field$3130;
      int32_t _field$3129 = _x$832->$3;
      int32_t _cnt$3469 = Moonbit_object_header(_x$832)->rc;
      int32_t _hash$836;
      if (_cnt$3469 > 1) {
        int32_t _new_cnt$3470;
        moonbit_incref(_value$835);
        moonbit_incref(_key$834);
        if (_next$833) {
          moonbit_incref(_next$833);
        }
        _new_cnt$3470 = _cnt$3469 - 1;
        Moonbit_object_header(_x$832)->rc = _new_cnt$3470;
      } else if (_cnt$3469 == 1) {
        moonbit_free(_x$832);
      }
      _hash$836 = _field$3129;
      moonbit_incref(self$828);
      $$moonbitlang$core$builtin$Map$$set_with_hash$3(
        self$828, _key$834, _value$835, _hash$836
      );
      _param$830 = _next$833;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$817
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3142 =
    self$817->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$816 =
    _field$3142;
  int32_t capacity$2336 = self$817->$2;
  int32_t new_capacity$818 = capacity$2336 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2331 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2330 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$818, _tmp$2331
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$3141 =
    self$817->$0;
  int32_t _tmp$2332;
  int32_t capacity$2334;
  int32_t _tmp$2333;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2335;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3140;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$819;
  if (old_head$816) {
    moonbit_incref(old_head$816);
  }
  moonbit_decref(_old$3141);
  self$817->$0 = _tmp$2330;
  self$817->$2 = new_capacity$818;
  _tmp$2332 = new_capacity$818 - 1;
  self$817->$3 = _tmp$2332;
  capacity$2334 = self$817->$2;
  _tmp$2333 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2334);
  self$817->$4 = _tmp$2333;
  self$817->$1 = 0;
  _tmp$2335 = 0;
  _old$3140 = self$817->$5;
  if (_old$3140) {
    moonbit_decref(_old$3140);
  }
  self$817->$5 = _tmp$2335;
  self$817->$6 = -1;
  _param$819 = old_head$816;
  while (1) {
    if (_param$819 == 0) {
      if (_param$819) {
        moonbit_decref(_param$819);
      }
      moonbit_decref(self$817);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$820 =
        _param$819;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$821 =
        _Some$820;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3139 =
        _x$821->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$822 =
        _field$3139;
      moonbit_string_t _field$3138 = _x$821->$4;
      moonbit_string_t _key$823 = _field$3138;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3137 =
        _x$821->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$824 =
        _field$3137;
      int32_t _field$3136 = _x$821->$3;
      int32_t _cnt$3471 = Moonbit_object_header(_x$821)->rc;
      int32_t _hash$825;
      if (_cnt$3471 > 1) {
        int32_t _new_cnt$3472;
        moonbit_incref(_value$824);
        moonbit_incref(_key$823);
        if (_next$822) {
          moonbit_incref(_next$822);
        }
        _new_cnt$3472 = _cnt$3471 - 1;
        Moonbit_object_header(_x$821)->rc = _new_cnt$3472;
      } else if (_cnt$3471 == 1) {
        moonbit_free(_x$821);
      }
      _hash$825 = _field$3136;
      moonbit_incref(self$817);
      $$moonbitlang$core$builtin$Map$$set_with_hash$2(
        self$817, _key$823, _value$824, _hash$825
      );
      _param$819 = _next$822;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$806
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3148 =
    self$806->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$805 =
    _field$3148;
  int32_t capacity$2329 = self$806->$2;
  int32_t new_capacity$807 = capacity$2329 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2324 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$2323 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$807, _tmp$2324
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$3147 =
    self$806->$0;
  int32_t _tmp$2325;
  int32_t capacity$2327;
  int32_t _tmp$2326;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2328;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3146;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$808;
  if (old_head$805) {
    moonbit_incref(old_head$805);
  }
  moonbit_decref(_old$3147);
  self$806->$0 = _tmp$2323;
  self$806->$2 = new_capacity$807;
  _tmp$2325 = new_capacity$807 - 1;
  self$806->$3 = _tmp$2325;
  capacity$2327 = self$806->$2;
  _tmp$2326 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2327);
  self$806->$4 = _tmp$2326;
  self$806->$1 = 0;
  _tmp$2328 = 0;
  _old$3146 = self$806->$5;
  if (_old$3146) {
    moonbit_decref(_old$3146);
  }
  self$806->$5 = _tmp$2328;
  self$806->$6 = -1;
  _param$808 = old_head$805;
  while (1) {
    if (_param$808 == 0) {
      if (_param$808) {
        moonbit_decref(_param$808);
      }
      moonbit_decref(self$806);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$809 =
        _param$808;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _x$810 =
        _Some$809;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3145 =
        _x$810->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$811 =
        _field$3145;
      int32_t _key$812 = _x$810->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3144 =
        _x$810->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$813 =
        _field$3144;
      int32_t _field$3143 = _x$810->$3;
      int32_t _cnt$3473 = Moonbit_object_header(_x$810)->rc;
      int32_t _hash$814;
      if (_cnt$3473 > 1) {
        int32_t _new_cnt$3474;
        moonbit_incref(_value$813);
        if (_next$811) {
          moonbit_incref(_next$811);
        }
        _new_cnt$3474 = _cnt$3473 - 1;
        Moonbit_object_header(_x$810)->rc = _new_cnt$3474;
      } else if (_cnt$3473 == 1) {
        moonbit_free(_x$810);
      }
      _hash$814 = _field$3143;
      moonbit_incref(self$806);
      $$moonbitlang$core$builtin$Map$$set_with_hash$1(
        self$806, _key$812, _value$813, _hash$814
      );
      _param$808 = _next$811;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$795
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3155 =
    self$795->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$794 =
    _field$3155;
  int32_t capacity$2322 = self$795->$2;
  int32_t new_capacity$796 = capacity$2322 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2317 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2316 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$796, _tmp$2317
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$3154 =
    self$795->$0;
  int32_t _tmp$2318;
  int32_t capacity$2320;
  int32_t _tmp$2319;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2321;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3153;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$797;
  if (old_head$794) {
    moonbit_incref(old_head$794);
  }
  moonbit_decref(_old$3154);
  self$795->$0 = _tmp$2316;
  self$795->$2 = new_capacity$796;
  _tmp$2318 = new_capacity$796 - 1;
  self$795->$3 = _tmp$2318;
  capacity$2320 = self$795->$2;
  _tmp$2319 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2320);
  self$795->$4 = _tmp$2319;
  self$795->$1 = 0;
  _tmp$2321 = 0;
  _old$3153 = self$795->$5;
  if (_old$3153) {
    moonbit_decref(_old$3153);
  }
  self$795->$5 = _tmp$2321;
  self$795->$6 = -1;
  _param$797 = old_head$794;
  while (1) {
    if (_param$797 == 0) {
      if (_param$797) {
        moonbit_decref(_param$797);
      }
      moonbit_decref(self$795);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$798 =
        _param$797;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$799 =
        _Some$798;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3152 =
        _x$799->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$800 =
        _field$3152;
      moonbit_string_t _field$3151 = _x$799->$4;
      moonbit_string_t _key$801 = _field$3151;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3150 =
        _x$799->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$802 =
        _field$3150;
      int32_t _field$3149 = _x$799->$3;
      int32_t _cnt$3475 = Moonbit_object_header(_x$799)->rc;
      int32_t _hash$803;
      if (_cnt$3475 > 1) {
        int32_t _new_cnt$3476;
        moonbit_incref(_value$802);
        moonbit_incref(_key$801);
        if (_next$800) {
          moonbit_incref(_next$800);
        }
        _new_cnt$3476 = _cnt$3475 - 1;
        Moonbit_object_header(_x$799)->rc = _new_cnt$3476;
      } else if (_cnt$3475 == 1) {
        moonbit_free(_x$799);
      }
      _hash$803 = _field$3149;
      moonbit_incref(self$795);
      $$moonbitlang$core$builtin$Map$$set_with_hash$0(
        self$795, _key$801, _value$802, _hash$803
      );
      _param$797 = _next$800;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$778,
  moonbit_string_t key$787,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$788,
  int32_t hash$786
) {
  int32_t size$2302 = self$778->$1;
  int32_t grow_at$2303 = self$778->$4;
  int32_t capacity_mask$2315;
  int32_t _tmp$2314;
  struct $$3c$Int$2a$Int$3e$* _bind$779;
  int32_t psl$780;
  int32_t idx$781;
  int32_t _idx$789;
  int32_t _field$3156;
  int32_t _psl$790;
  int32_t _bind$791;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$792;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$793;
  if (size$2302 >= grow_at$2303) {
    moonbit_incref(self$778);
    $$moonbitlang$core$builtin$Map$$grow$3(self$778);
  }
  capacity_mask$2315 = self$778->$3;
  _tmp$2314 = hash$786 & capacity_mask$2315;
  psl$780 = 0;
  idx$781 = _tmp$2314;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3161 =
      self$778->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2313 =
      _field$3161;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3160;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$782;
    if (idx$781 < 0 || idx$781 >= Moonbit_array_length(entries$2313)) {
      moonbit_panic();
    }
    _tmp$3160
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2313[
        idx$781
      ];
    _bind$782 = _tmp$3160;
    if (_bind$782 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2304 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2304)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2304->$0 = idx$781;
      _tuple$2304->$1 = psl$780;
      _bind$779 = _tuple$2304;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$784 =
        _bind$782;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$785 =
        _Some$784;
      int32_t hash$2306 = _curr_entry$785->$3;
      int32_t _if_result$3679;
      int32_t psl$2307;
      int32_t _tmp$2309;
      int32_t _tmp$2311;
      int32_t capacity_mask$2312;
      int32_t _tmp$2310;
      if (hash$2306 == hash$786) {
        moonbit_string_t _field$3159 = _curr_entry$785->$4;
        moonbit_string_t key$2305 = _field$3159;
        int32_t _tmp$3158 = moonbit_val_array_equal(key$2305, key$787);
        _if_result$3679 = _tmp$3158;
      } else {
        _if_result$3679 = 0;
      }
      if (_if_result$3679) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3157;
        moonbit_incref(_curr_entry$785);
        moonbit_decref(key$787);
        moonbit_decref(self$778);
        _old$3157 = _curr_entry$785->$5;
        moonbit_decref(_old$3157);
        _curr_entry$785->$5 = value$788;
        moonbit_decref(_curr_entry$785);
        return 0;
      } else {
        moonbit_incref(_curr_entry$785);
      }
      psl$2307 = _curr_entry$785->$2;
      if (psl$780 > psl$2307) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2308;
        moonbit_incref(self$778);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$778, idx$781, _curr_entry$785
        );
        _tuple$2308
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2308)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2308->$0 = idx$781;
        _tuple$2308->$1 = psl$780;
        _bind$779 = _tuple$2308;
        break;
      } else {
        moonbit_decref(_curr_entry$785);
      }
      _tmp$2309 = psl$780 + 1;
      _tmp$2311 = idx$781 + 1;
      capacity_mask$2312 = self$778->$3;
      _tmp$2310 = _tmp$2311 & capacity_mask$2312;
      psl$780 = _tmp$2309;
      idx$781 = _tmp$2310;
      continue;
    }
    break;
  }
  _idx$789 = _bind$779->$0;
  _field$3156 = _bind$779->$1;
  moonbit_decref(_bind$779);
  _psl$790 = _field$3156;
  _bind$791 = self$778->$6;
  _bind$792 = 0;
  entry$793
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$793)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$793->$0 = _bind$791;
  entry$793->$1 = _bind$792;
  entry$793->$2 = _psl$790;
  entry$793->$3 = hash$786;
  entry$793->$4 = key$787;
  entry$793->$5 = value$788;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
    self$778, _idx$789, entry$793
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$762,
  moonbit_string_t key$771,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$772,
  int32_t hash$770
) {
  int32_t size$2288 = self$762->$1;
  int32_t grow_at$2289 = self$762->$4;
  int32_t capacity_mask$2301;
  int32_t _tmp$2300;
  struct $$3c$Int$2a$Int$3e$* _bind$763;
  int32_t psl$764;
  int32_t idx$765;
  int32_t _idx$773;
  int32_t _field$3162;
  int32_t _psl$774;
  int32_t _bind$775;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$776;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$777;
  if (size$2288 >= grow_at$2289) {
    moonbit_incref(self$762);
    $$moonbitlang$core$builtin$Map$$grow$2(self$762);
  }
  capacity_mask$2301 = self$762->$3;
  _tmp$2300 = hash$770 & capacity_mask$2301;
  psl$764 = 0;
  idx$765 = _tmp$2300;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3167 =
      self$762->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2299 =
      _field$3167;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3166;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$766;
    if (idx$765 < 0 || idx$765 >= Moonbit_array_length(entries$2299)) {
      moonbit_panic();
    }
    _tmp$3166
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2299[
        idx$765
      ];
    _bind$766 = _tmp$3166;
    if (_bind$766 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2290 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2290)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2290->$0 = idx$765;
      _tuple$2290->$1 = psl$764;
      _bind$763 = _tuple$2290;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$768 =
        _bind$766;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$769 =
        _Some$768;
      int32_t hash$2292 = _curr_entry$769->$3;
      int32_t _if_result$3681;
      int32_t psl$2293;
      int32_t _tmp$2295;
      int32_t _tmp$2297;
      int32_t capacity_mask$2298;
      int32_t _tmp$2296;
      if (hash$2292 == hash$770) {
        moonbit_string_t _field$3165 = _curr_entry$769->$4;
        moonbit_string_t key$2291 = _field$3165;
        int32_t _tmp$3164 = moonbit_val_array_equal(key$2291, key$771);
        _if_result$3681 = _tmp$3164;
      } else {
        _if_result$3681 = 0;
      }
      if (_if_result$3681) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3163;
        moonbit_incref(_curr_entry$769);
        moonbit_decref(key$771);
        moonbit_decref(self$762);
        _old$3163 = _curr_entry$769->$5;
        moonbit_decref(_old$3163);
        _curr_entry$769->$5 = value$772;
        moonbit_decref(_curr_entry$769);
        return 0;
      } else {
        moonbit_incref(_curr_entry$769);
      }
      psl$2293 = _curr_entry$769->$2;
      if (psl$764 > psl$2293) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2294;
        moonbit_incref(self$762);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$762, idx$765, _curr_entry$769
        );
        _tuple$2294
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2294)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2294->$0 = idx$765;
        _tuple$2294->$1 = psl$764;
        _bind$763 = _tuple$2294;
        break;
      } else {
        moonbit_decref(_curr_entry$769);
      }
      _tmp$2295 = psl$764 + 1;
      _tmp$2297 = idx$765 + 1;
      capacity_mask$2298 = self$762->$3;
      _tmp$2296 = _tmp$2297 & capacity_mask$2298;
      psl$764 = _tmp$2295;
      idx$765 = _tmp$2296;
      continue;
    }
    break;
  }
  _idx$773 = _bind$763->$0;
  _field$3162 = _bind$763->$1;
  moonbit_decref(_bind$763);
  _psl$774 = _field$3162;
  _bind$775 = self$762->$6;
  _bind$776 = 0;
  entry$777
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$777)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$777->$0 = _bind$775;
  entry$777->$1 = _bind$776;
  entry$777->$2 = _psl$774;
  entry$777->$3 = hash$770;
  entry$777->$4 = key$771;
  entry$777->$5 = value$772;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
    self$762, _idx$773, entry$777
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$746,
  int32_t key$755,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$756,
  int32_t hash$754
) {
  int32_t size$2274 = self$746->$1;
  int32_t grow_at$2275 = self$746->$4;
  int32_t capacity_mask$2287;
  int32_t _tmp$2286;
  struct $$3c$Int$2a$Int$3e$* _bind$747;
  int32_t psl$748;
  int32_t idx$749;
  int32_t _idx$757;
  int32_t _field$3168;
  int32_t _psl$758;
  int32_t _bind$759;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$760;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$761;
  if (size$2274 >= grow_at$2275) {
    moonbit_incref(self$746);
    $$moonbitlang$core$builtin$Map$$grow$1(self$746);
  }
  capacity_mask$2287 = self$746->$3;
  _tmp$2286 = hash$754 & capacity_mask$2287;
  psl$748 = 0;
  idx$749 = _tmp$2286;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3171 =
      self$746->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2285 =
      _field$3171;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3170;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$750;
    if (idx$749 < 0 || idx$749 >= Moonbit_array_length(entries$2285)) {
      moonbit_panic();
    }
    _tmp$3170
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2285[
        idx$749
      ];
    _bind$750 = _tmp$3170;
    if (_bind$750 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2276 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2276)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2276->$0 = idx$749;
      _tuple$2276->$1 = psl$748;
      _bind$747 = _tuple$2276;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$752 =
        _bind$750;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$753 =
        _Some$752;
      int32_t hash$2278 = _curr_entry$753->$3;
      int32_t _if_result$3683;
      int32_t psl$2279;
      int32_t _tmp$2281;
      int32_t _tmp$2283;
      int32_t capacity_mask$2284;
      int32_t _tmp$2282;
      if (hash$2278 == hash$754) {
        int32_t key$2277 = _curr_entry$753->$4;
        _if_result$3683 = key$2277 == key$755;
      } else {
        _if_result$3683 = 0;
      }
      if (_if_result$3683) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$3169;
        moonbit_incref(_curr_entry$753);
        moonbit_decref(self$746);
        _old$3169 = _curr_entry$753->$5;
        moonbit_decref(_old$3169);
        _curr_entry$753->$5 = value$756;
        moonbit_decref(_curr_entry$753);
        return 0;
      } else {
        moonbit_incref(_curr_entry$753);
      }
      psl$2279 = _curr_entry$753->$2;
      if (psl$748 > psl$2279) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2280;
        moonbit_incref(self$746);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$746, idx$749, _curr_entry$753
        );
        _tuple$2280
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2280)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2280->$0 = idx$749;
        _tuple$2280->$1 = psl$748;
        _bind$747 = _tuple$2280;
        break;
      } else {
        moonbit_decref(_curr_entry$753);
      }
      _tmp$2281 = psl$748 + 1;
      _tmp$2283 = idx$749 + 1;
      capacity_mask$2284 = self$746->$3;
      _tmp$2282 = _tmp$2283 & capacity_mask$2284;
      psl$748 = _tmp$2281;
      idx$749 = _tmp$2282;
      continue;
    }
    break;
  }
  _idx$757 = _bind$747->$0;
  _field$3168 = _bind$747->$1;
  moonbit_decref(_bind$747);
  _psl$758 = _field$3168;
  _bind$759 = self$746->$6;
  _bind$760 = 0;
  entry$761
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$761)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      2,
      0
  );
  entry$761->$0 = _bind$759;
  entry$761->$1 = _bind$760;
  entry$761->$2 = _psl$758;
  entry$761->$3 = hash$754;
  entry$761->$4 = key$755;
  entry$761->$5 = value$756;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
    self$746, _idx$757, entry$761
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$730,
  moonbit_string_t key$739,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$740,
  int32_t hash$738
) {
  int32_t size$2260 = self$730->$1;
  int32_t grow_at$2261 = self$730->$4;
  int32_t capacity_mask$2273;
  int32_t _tmp$2272;
  struct $$3c$Int$2a$Int$3e$* _bind$731;
  int32_t psl$732;
  int32_t idx$733;
  int32_t _idx$741;
  int32_t _field$3172;
  int32_t _psl$742;
  int32_t _bind$743;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$744;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$745;
  if (size$2260 >= grow_at$2261) {
    moonbit_incref(self$730);
    $$moonbitlang$core$builtin$Map$$grow$0(self$730);
  }
  capacity_mask$2273 = self$730->$3;
  _tmp$2272 = hash$738 & capacity_mask$2273;
  psl$732 = 0;
  idx$733 = _tmp$2272;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3177 =
      self$730->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2271 =
      _field$3177;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3176;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$734;
    if (idx$733 < 0 || idx$733 >= Moonbit_array_length(entries$2271)) {
      moonbit_panic();
    }
    _tmp$3176
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2271[
        idx$733
      ];
    _bind$734 = _tmp$3176;
    if (_bind$734 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2262 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2262)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2262->$0 = idx$733;
      _tuple$2262->$1 = psl$732;
      _bind$731 = _tuple$2262;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$736 =
        _bind$734;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$737 =
        _Some$736;
      int32_t hash$2264 = _curr_entry$737->$3;
      int32_t _if_result$3685;
      int32_t psl$2265;
      int32_t _tmp$2267;
      int32_t _tmp$2269;
      int32_t capacity_mask$2270;
      int32_t _tmp$2268;
      if (hash$2264 == hash$738) {
        moonbit_string_t _field$3175 = _curr_entry$737->$4;
        moonbit_string_t key$2263 = _field$3175;
        int32_t _tmp$3174 = moonbit_val_array_equal(key$2263, key$739);
        _if_result$3685 = _tmp$3174;
      } else {
        _if_result$3685 = 0;
      }
      if (_if_result$3685) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3173;
        moonbit_incref(_curr_entry$737);
        moonbit_decref(key$739);
        moonbit_decref(self$730);
        _old$3173 = _curr_entry$737->$5;
        moonbit_decref(_old$3173);
        _curr_entry$737->$5 = value$740;
        moonbit_decref(_curr_entry$737);
        return 0;
      } else {
        moonbit_incref(_curr_entry$737);
      }
      psl$2265 = _curr_entry$737->$2;
      if (psl$732 > psl$2265) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2266;
        moonbit_incref(self$730);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$730, idx$733, _curr_entry$737
        );
        _tuple$2266
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2266)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2266->$0 = idx$733;
        _tuple$2266->$1 = psl$732;
        _bind$731 = _tuple$2266;
        break;
      } else {
        moonbit_decref(_curr_entry$737);
      }
      _tmp$2267 = psl$732 + 1;
      _tmp$2269 = idx$733 + 1;
      capacity_mask$2270 = self$730->$3;
      _tmp$2268 = _tmp$2269 & capacity_mask$2270;
      psl$732 = _tmp$2267;
      idx$733 = _tmp$2268;
      continue;
    }
    break;
  }
  _idx$741 = _bind$731->$0;
  _field$3172 = _bind$731->$1;
  moonbit_decref(_bind$731);
  _psl$742 = _field$3172;
  _bind$743 = self$730->$6;
  _bind$744 = 0;
  entry$745
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$745)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$745->$0 = _bind$743;
  entry$745->$1 = _bind$744;
  entry$745->$2 = _psl$742;
  entry$745->$3 = hash$738;
  entry$745->$4 = key$739;
  entry$745->$5 = value$740;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
    self$730, _idx$741, entry$745
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$724,
  int32_t idx$729,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$728
) {
  int32_t psl$2259 = entry$728->$2;
  int32_t _tmp$2255 = psl$2259 + 1;
  int32_t _tmp$2257 = idx$729 + 1;
  int32_t capacity_mask$2258 = self$724->$3;
  int32_t _tmp$2256 = _tmp$2257 & capacity_mask$2258;
  int32_t psl$720 = _tmp$2255;
  int32_t idx$721 = _tmp$2256;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$722 =
    entry$728;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3179 =
      self$724->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2254 =
      _field$3179;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3178;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$723;
    if (idx$721 < 0 || idx$721 >= Moonbit_array_length(entries$2254)) {
      moonbit_panic();
    }
    _tmp$3178
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2254[
        idx$721
      ];
    _bind$723 = _tmp$3178;
    if (_bind$723 == 0) {
      entry$722->$2 = psl$720;
      $$moonbitlang$core$builtin$Map$$set_entry$3(
        self$724, entry$722, idx$721
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$726 =
        _bind$723;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$727 =
        _Some$726;
      int32_t psl$2244 = _curr_entry$727->$2;
      if (psl$720 > psl$2244) {
        int32_t psl$2249;
        int32_t _tmp$2245;
        int32_t _tmp$2247;
        int32_t capacity_mask$2248;
        int32_t _tmp$2246;
        entry$722->$2 = psl$720;
        moonbit_incref(_curr_entry$727);
        moonbit_incref(self$724);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$724, entry$722, idx$721
        );
        psl$2249 = _curr_entry$727->$2;
        _tmp$2245 = psl$2249 + 1;
        _tmp$2247 = idx$721 + 1;
        capacity_mask$2248 = self$724->$3;
        _tmp$2246 = _tmp$2247 & capacity_mask$2248;
        psl$720 = _tmp$2245;
        idx$721 = _tmp$2246;
        entry$722 = _curr_entry$727;
        continue;
      } else {
        int32_t _tmp$2250 = psl$720 + 1;
        int32_t _tmp$2252 = idx$721 + 1;
        int32_t capacity_mask$2253 = self$724->$3;
        int32_t _tmp$2251 = _tmp$2252 & capacity_mask$2253;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3687 =
          entry$722;
        psl$720 = _tmp$2250;
        idx$721 = _tmp$2251;
        entry$722 = _tmp$3687;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$714,
  int32_t idx$719,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$718
) {
  int32_t psl$2243 = entry$718->$2;
  int32_t _tmp$2239 = psl$2243 + 1;
  int32_t _tmp$2241 = idx$719 + 1;
  int32_t capacity_mask$2242 = self$714->$3;
  int32_t _tmp$2240 = _tmp$2241 & capacity_mask$2242;
  int32_t psl$710 = _tmp$2239;
  int32_t idx$711 = _tmp$2240;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$712 =
    entry$718;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3181 =
      self$714->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2238 =
      _field$3181;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3180;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$713;
    if (idx$711 < 0 || idx$711 >= Moonbit_array_length(entries$2238)) {
      moonbit_panic();
    }
    _tmp$3180
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2238[
        idx$711
      ];
    _bind$713 = _tmp$3180;
    if (_bind$713 == 0) {
      entry$712->$2 = psl$710;
      $$moonbitlang$core$builtin$Map$$set_entry$2(
        self$714, entry$712, idx$711
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$716 =
        _bind$713;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$717 =
        _Some$716;
      int32_t psl$2228 = _curr_entry$717->$2;
      if (psl$710 > psl$2228) {
        int32_t psl$2233;
        int32_t _tmp$2229;
        int32_t _tmp$2231;
        int32_t capacity_mask$2232;
        int32_t _tmp$2230;
        entry$712->$2 = psl$710;
        moonbit_incref(_curr_entry$717);
        moonbit_incref(self$714);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$714, entry$712, idx$711
        );
        psl$2233 = _curr_entry$717->$2;
        _tmp$2229 = psl$2233 + 1;
        _tmp$2231 = idx$711 + 1;
        capacity_mask$2232 = self$714->$3;
        _tmp$2230 = _tmp$2231 & capacity_mask$2232;
        psl$710 = _tmp$2229;
        idx$711 = _tmp$2230;
        entry$712 = _curr_entry$717;
        continue;
      } else {
        int32_t _tmp$2234 = psl$710 + 1;
        int32_t _tmp$2236 = idx$711 + 1;
        int32_t capacity_mask$2237 = self$714->$3;
        int32_t _tmp$2235 = _tmp$2236 & capacity_mask$2237;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3689 =
          entry$712;
        psl$710 = _tmp$2234;
        idx$711 = _tmp$2235;
        entry$712 = _tmp$3689;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$704,
  int32_t idx$709,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$708
) {
  int32_t psl$2227 = entry$708->$2;
  int32_t _tmp$2223 = psl$2227 + 1;
  int32_t _tmp$2225 = idx$709 + 1;
  int32_t capacity_mask$2226 = self$704->$3;
  int32_t _tmp$2224 = _tmp$2225 & capacity_mask$2226;
  int32_t psl$700 = _tmp$2223;
  int32_t idx$701 = _tmp$2224;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$702 =
    entry$708;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3183 =
      self$704->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2222 =
      _field$3183;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3182;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$703;
    if (idx$701 < 0 || idx$701 >= Moonbit_array_length(entries$2222)) {
      moonbit_panic();
    }
    _tmp$3182
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2222[
        idx$701
      ];
    _bind$703 = _tmp$3182;
    if (_bind$703 == 0) {
      entry$702->$2 = psl$700;
      $$moonbitlang$core$builtin$Map$$set_entry$1(
        self$704, entry$702, idx$701
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$706 =
        _bind$703;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$707 =
        _Some$706;
      int32_t psl$2212 = _curr_entry$707->$2;
      if (psl$700 > psl$2212) {
        int32_t psl$2217;
        int32_t _tmp$2213;
        int32_t _tmp$2215;
        int32_t capacity_mask$2216;
        int32_t _tmp$2214;
        entry$702->$2 = psl$700;
        moonbit_incref(_curr_entry$707);
        moonbit_incref(self$704);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$704, entry$702, idx$701
        );
        psl$2217 = _curr_entry$707->$2;
        _tmp$2213 = psl$2217 + 1;
        _tmp$2215 = idx$701 + 1;
        capacity_mask$2216 = self$704->$3;
        _tmp$2214 = _tmp$2215 & capacity_mask$2216;
        psl$700 = _tmp$2213;
        idx$701 = _tmp$2214;
        entry$702 = _curr_entry$707;
        continue;
      } else {
        int32_t _tmp$2218 = psl$700 + 1;
        int32_t _tmp$2220 = idx$701 + 1;
        int32_t capacity_mask$2221 = self$704->$3;
        int32_t _tmp$2219 = _tmp$2220 & capacity_mask$2221;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3691 =
          entry$702;
        psl$700 = _tmp$2218;
        idx$701 = _tmp$2219;
        entry$702 = _tmp$3691;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$694,
  int32_t idx$699,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$698
) {
  int32_t psl$2211 = entry$698->$2;
  int32_t _tmp$2207 = psl$2211 + 1;
  int32_t _tmp$2209 = idx$699 + 1;
  int32_t capacity_mask$2210 = self$694->$3;
  int32_t _tmp$2208 = _tmp$2209 & capacity_mask$2210;
  int32_t psl$690 = _tmp$2207;
  int32_t idx$691 = _tmp$2208;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$692 =
    entry$698;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3185 =
      self$694->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2206 =
      _field$3185;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3184;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$693;
    if (idx$691 < 0 || idx$691 >= Moonbit_array_length(entries$2206)) {
      moonbit_panic();
    }
    _tmp$3184
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2206[
        idx$691
      ];
    _bind$693 = _tmp$3184;
    if (_bind$693 == 0) {
      entry$692->$2 = psl$690;
      $$moonbitlang$core$builtin$Map$$set_entry$0(
        self$694, entry$692, idx$691
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$696 =
        _bind$693;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$697 =
        _Some$696;
      int32_t psl$2196 = _curr_entry$697->$2;
      if (psl$690 > psl$2196) {
        int32_t psl$2201;
        int32_t _tmp$2197;
        int32_t _tmp$2199;
        int32_t capacity_mask$2200;
        int32_t _tmp$2198;
        entry$692->$2 = psl$690;
        moonbit_incref(_curr_entry$697);
        moonbit_incref(self$694);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$694, entry$692, idx$691
        );
        psl$2201 = _curr_entry$697->$2;
        _tmp$2197 = psl$2201 + 1;
        _tmp$2199 = idx$691 + 1;
        capacity_mask$2200 = self$694->$3;
        _tmp$2198 = _tmp$2199 & capacity_mask$2200;
        psl$690 = _tmp$2197;
        idx$691 = _tmp$2198;
        entry$692 = _curr_entry$697;
        continue;
      } else {
        int32_t _tmp$2202 = psl$690 + 1;
        int32_t _tmp$2204 = idx$691 + 1;
        int32_t capacity_mask$2205 = self$694->$3;
        int32_t _tmp$2203 = _tmp$2204 & capacity_mask$2205;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3693 =
          entry$692;
        psl$690 = _tmp$2202;
        idx$691 = _tmp$2203;
        entry$692 = _tmp$3693;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$684,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$686,
  int32_t new_idx$685
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3188 =
    self$684->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2194 =
    _field$3188;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2195;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3187;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3186;
  int32_t _cnt$3477;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$687;
  moonbit_incref(entry$686);
  _tmp$2195 = entry$686;
  if (new_idx$685 < 0 || new_idx$685 >= Moonbit_array_length(entries$2194)) {
    moonbit_panic();
  }
  _old$3187
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2194[
      new_idx$685
    ];
  if (_old$3187) {
    moonbit_decref(_old$3187);
  }
  entries$2194[new_idx$685] = _tmp$2195;
  _field$3186 = entry$686->$1;
  _cnt$3477 = Moonbit_object_header(entry$686)->rc;
  if (_cnt$3477 > 1) {
    int32_t _new_cnt$3480;
    if (_field$3186) {
      moonbit_incref(_field$3186);
    }
    _new_cnt$3480 = _cnt$3477 - 1;
    Moonbit_object_header(entry$686)->rc = _new_cnt$3480;
  } else if (_cnt$3477 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3479 =
      entry$686->$5;
    moonbit_string_t _field$3478;
    moonbit_decref(_field$3479);
    _field$3478 = entry$686->$4;
    moonbit_decref(_field$3478);
    moonbit_free(entry$686);
  }
  _bind$687 = _field$3186;
  if (_bind$687 == 0) {
    if (_bind$687) {
      moonbit_decref(_bind$687);
    }
    self$684->$6 = new_idx$685;
    moonbit_decref(self$684);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$688;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$689;
    moonbit_decref(self$684);
    _Some$688 = _bind$687;
    _next$689 = _Some$688;
    _next$689->$0 = new_idx$685;
    moonbit_decref(_next$689);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$678,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$680,
  int32_t new_idx$679
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3191 =
    self$678->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2192 =
    _field$3191;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2193;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3190;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3189;
  int32_t _cnt$3481;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$681;
  moonbit_incref(entry$680);
  _tmp$2193 = entry$680;
  if (new_idx$679 < 0 || new_idx$679 >= Moonbit_array_length(entries$2192)) {
    moonbit_panic();
  }
  _old$3190
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2192[
      new_idx$679
    ];
  if (_old$3190) {
    moonbit_decref(_old$3190);
  }
  entries$2192[new_idx$679] = _tmp$2193;
  _field$3189 = entry$680->$1;
  _cnt$3481 = Moonbit_object_header(entry$680)->rc;
  if (_cnt$3481 > 1) {
    int32_t _new_cnt$3484;
    if (_field$3189) {
      moonbit_incref(_field$3189);
    }
    _new_cnt$3484 = _cnt$3481 - 1;
    Moonbit_object_header(entry$680)->rc = _new_cnt$3484;
  } else if (_cnt$3481 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3483 =
      entry$680->$5;
    moonbit_string_t _field$3482;
    moonbit_decref(_field$3483);
    _field$3482 = entry$680->$4;
    moonbit_decref(_field$3482);
    moonbit_free(entry$680);
  }
  _bind$681 = _field$3189;
  if (_bind$681 == 0) {
    if (_bind$681) {
      moonbit_decref(_bind$681);
    }
    self$678->$6 = new_idx$679;
    moonbit_decref(self$678);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$682;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$683;
    moonbit_decref(self$678);
    _Some$682 = _bind$681;
    _next$683 = _Some$682;
    _next$683->$0 = new_idx$679;
    moonbit_decref(_next$683);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$672,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$674,
  int32_t new_idx$673
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3194 =
    self$672->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2190 =
    _field$3194;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2191;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3193;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3192;
  int32_t _cnt$3485;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$675;
  moonbit_incref(entry$674);
  _tmp$2191 = entry$674;
  if (new_idx$673 < 0 || new_idx$673 >= Moonbit_array_length(entries$2190)) {
    moonbit_panic();
  }
  _old$3193
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2190[
      new_idx$673
    ];
  if (_old$3193) {
    moonbit_decref(_old$3193);
  }
  entries$2190[new_idx$673] = _tmp$2191;
  _field$3192 = entry$674->$1;
  _cnt$3485 = Moonbit_object_header(entry$674)->rc;
  if (_cnt$3485 > 1) {
    int32_t _new_cnt$3487;
    if (_field$3192) {
      moonbit_incref(_field$3192);
    }
    _new_cnt$3487 = _cnt$3485 - 1;
    Moonbit_object_header(entry$674)->rc = _new_cnt$3487;
  } else if (_cnt$3485 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$3486 =
      entry$674->$5;
    moonbit_decref(_field$3486);
    moonbit_free(entry$674);
  }
  _bind$675 = _field$3192;
  if (_bind$675 == 0) {
    if (_bind$675) {
      moonbit_decref(_bind$675);
    }
    self$672->$6 = new_idx$673;
    moonbit_decref(self$672);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$676;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$677;
    moonbit_decref(self$672);
    _Some$676 = _bind$675;
    _next$677 = _Some$676;
    _next$677->$0 = new_idx$673;
    moonbit_decref(_next$677);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$666,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$668,
  int32_t new_idx$667
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3197 =
    self$666->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2188 =
    _field$3197;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2189;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3196;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$3195;
  int32_t _cnt$3488;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$669;
  moonbit_incref(entry$668);
  _tmp$2189 = entry$668;
  if (new_idx$667 < 0 || new_idx$667 >= Moonbit_array_length(entries$2188)) {
    moonbit_panic();
  }
  _old$3196
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2188[
      new_idx$667
    ];
  if (_old$3196) {
    moonbit_decref(_old$3196);
  }
  entries$2188[new_idx$667] = _tmp$2189;
  _field$3195 = entry$668->$1;
  _cnt$3488 = Moonbit_object_header(entry$668)->rc;
  if (_cnt$3488 > 1) {
    int32_t _new_cnt$3491;
    if (_field$3195) {
      moonbit_incref(_field$3195);
    }
    _new_cnt$3491 = _cnt$3488 - 1;
    Moonbit_object_header(entry$668)->rc = _new_cnt$3491;
  } else if (_cnt$3488 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$3490 =
      entry$668->$5;
    moonbit_string_t _field$3489;
    moonbit_decref(_field$3490);
    _field$3489 = entry$668->$4;
    moonbit_decref(_field$3489);
    moonbit_free(entry$668);
  }
  _bind$669 = _field$3195;
  if (_bind$669 == 0) {
    if (_bind$669) {
      moonbit_decref(_bind$669);
    }
    self$666->$6 = new_idx$667;
    moonbit_decref(self$666);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$670;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$671;
    moonbit_decref(self$666);
    _Some$670 = _bind$669;
    _next$671 = _Some$670;
    _next$671->$0 = new_idx$667;
    moonbit_decref(_next$671);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$663,
  int32_t idx$665,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$664
) {
  int32_t _bind$662 = self$663->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3199;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2184;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2185;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3198;
  int32_t size$2187;
  int32_t _tmp$2186;
  switch (_bind$662) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2179;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3200;
      moonbit_incref(entry$664);
      _tmp$2179 = entry$664;
      _old$3200 = self$663->$5;
      if (_old$3200) {
        moonbit_decref(_old$3200);
      }
      self$663->$5 = _tmp$2179;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3203 =
        self$663->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2183 =
        _field$3203;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3202;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2182;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2180;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2181;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3201;
      if (_bind$662 < 0 || _bind$662 >= Moonbit_array_length(entries$2183)) {
        moonbit_panic();
      }
      _tmp$3202
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2183[
          _bind$662
        ];
      _tmp$2182 = _tmp$3202;
      if (_tmp$2182) {
        moonbit_incref(_tmp$2182);
      }
      _tmp$2180 = $Option$$unwrap$3(_tmp$2182);
      moonbit_incref(entry$664);
      _tmp$2181 = entry$664;
      _old$3201 = _tmp$2180->$1;
      if (_old$3201) {
        moonbit_decref(_old$3201);
      }
      _tmp$2180->$1 = _tmp$2181;
      moonbit_decref(_tmp$2180);
      break;
    }
  }
  self$663->$6 = idx$665;
  _field$3199 = self$663->$0;
  entries$2184 = _field$3199;
  _tmp$2185 = entry$664;
  if (idx$665 < 0 || idx$665 >= Moonbit_array_length(entries$2184)) {
    moonbit_panic();
  }
  _old$3198
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2184[
      idx$665
    ];
  if (_old$3198) {
    moonbit_decref(_old$3198);
  }
  entries$2184[idx$665] = _tmp$2185;
  size$2187 = self$663->$1;
  _tmp$2186 = size$2187 + 1;
  self$663->$1 = _tmp$2186;
  moonbit_decref(self$663);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$659,
  int32_t idx$661,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$660
) {
  int32_t _bind$658 = self$659->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3205;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2175;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2176;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3204;
  int32_t size$2178;
  int32_t _tmp$2177;
  switch (_bind$658) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2170;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3206;
      moonbit_incref(entry$660);
      _tmp$2170 = entry$660;
      _old$3206 = self$659->$5;
      if (_old$3206) {
        moonbit_decref(_old$3206);
      }
      self$659->$5 = _tmp$2170;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3209 =
        self$659->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2174 =
        _field$3209;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3208;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2173;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2171;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2172;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3207;
      if (_bind$658 < 0 || _bind$658 >= Moonbit_array_length(entries$2174)) {
        moonbit_panic();
      }
      _tmp$3208
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2174[
          _bind$658
        ];
      _tmp$2173 = _tmp$3208;
      if (_tmp$2173) {
        moonbit_incref(_tmp$2173);
      }
      _tmp$2171 = $Option$$unwrap$2(_tmp$2173);
      moonbit_incref(entry$660);
      _tmp$2172 = entry$660;
      _old$3207 = _tmp$2171->$1;
      if (_old$3207) {
        moonbit_decref(_old$3207);
      }
      _tmp$2171->$1 = _tmp$2172;
      moonbit_decref(_tmp$2171);
      break;
    }
  }
  self$659->$6 = idx$661;
  _field$3205 = self$659->$0;
  entries$2175 = _field$3205;
  _tmp$2176 = entry$660;
  if (idx$661 < 0 || idx$661 >= Moonbit_array_length(entries$2175)) {
    moonbit_panic();
  }
  _old$3204
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2175[
      idx$661
    ];
  if (_old$3204) {
    moonbit_decref(_old$3204);
  }
  entries$2175[idx$661] = _tmp$2176;
  size$2178 = self$659->$1;
  _tmp$2177 = size$2178 + 1;
  self$659->$1 = _tmp$2177;
  moonbit_decref(self$659);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$655,
  int32_t idx$657,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$656
) {
  int32_t _bind$654 = self$655->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3211;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2166;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2167;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3210;
  int32_t size$2169;
  int32_t _tmp$2168;
  switch (_bind$654) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2161;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3212;
      moonbit_incref(entry$656);
      _tmp$2161 = entry$656;
      _old$3212 = self$655->$5;
      if (_old$3212) {
        moonbit_decref(_old$3212);
      }
      self$655->$5 = _tmp$2161;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$3215 =
        self$655->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2165 =
        _field$3215;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$3214;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2164;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2162;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2163;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$3213;
      if (_bind$654 < 0 || _bind$654 >= Moonbit_array_length(entries$2165)) {
        moonbit_panic();
      }
      _tmp$3214
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2165[
          _bind$654
        ];
      _tmp$2164 = _tmp$3214;
      if (_tmp$2164) {
        moonbit_incref(_tmp$2164);
      }
      _tmp$2162 = $Option$$unwrap$1(_tmp$2164);
      moonbit_incref(entry$656);
      _tmp$2163 = entry$656;
      _old$3213 = _tmp$2162->$1;
      if (_old$3213) {
        moonbit_decref(_old$3213);
      }
      _tmp$2162->$1 = _tmp$2163;
      moonbit_decref(_tmp$2162);
      break;
    }
  }
  self$655->$6 = idx$657;
  _field$3211 = self$655->$0;
  entries$2166 = _field$3211;
  _tmp$2167 = entry$656;
  if (idx$657 < 0 || idx$657 >= Moonbit_array_length(entries$2166)) {
    moonbit_panic();
  }
  _old$3210
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2166[
      idx$657
    ];
  if (_old$3210) {
    moonbit_decref(_old$3210);
  }
  entries$2166[idx$657] = _tmp$2167;
  size$2169 = self$655->$1;
  _tmp$2168 = size$2169 + 1;
  self$655->$1 = _tmp$2168;
  moonbit_decref(self$655);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$651,
  int32_t idx$653,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$652
) {
  int32_t _bind$650 = self$651->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3217;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2157;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2158;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3216;
  int32_t size$2160;
  int32_t _tmp$2159;
  switch (_bind$650) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2152;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3218;
      moonbit_incref(entry$652);
      _tmp$2152 = entry$652;
      _old$3218 = self$651->$5;
      if (_old$3218) {
        moonbit_decref(_old$3218);
      }
      self$651->$5 = _tmp$2152;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$3221 =
        self$651->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2156 =
        _field$3221;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$3220;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2155;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2153;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2154;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$3219;
      if (_bind$650 < 0 || _bind$650 >= Moonbit_array_length(entries$2156)) {
        moonbit_panic();
      }
      _tmp$3220
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2156[
          _bind$650
        ];
      _tmp$2155 = _tmp$3220;
      if (_tmp$2155) {
        moonbit_incref(_tmp$2155);
      }
      _tmp$2153 = $Option$$unwrap$0(_tmp$2155);
      moonbit_incref(entry$652);
      _tmp$2154 = entry$652;
      _old$3219 = _tmp$2153->$1;
      if (_old$3219) {
        moonbit_decref(_old$3219);
      }
      _tmp$2153->$1 = _tmp$2154;
      moonbit_decref(_tmp$2153);
      break;
    }
  }
  self$651->$6 = idx$653;
  _field$3217 = self$651->$0;
  entries$2157 = _field$3217;
  _tmp$2158 = entry$652;
  if (idx$653 < 0 || idx$653 >= Moonbit_array_length(entries$2157)) {
    moonbit_panic();
  }
  _old$3216
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2157[
      idx$653
    ];
  if (_old$3216) {
    moonbit_decref(_old$3216);
  }
  entries$2157[idx$653] = _tmp$2158;
  size$2160 = self$651->$1;
  _tmp$2159 = size$2160 + 1;
  self$651->$1 = _tmp$2159;
  moonbit_decref(self$651);
  return 0;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$645
) {
  int32_t capacity$644 = $Int$$next_power_of_two(capacity$645);
  int32_t _bind$646 = capacity$644 - 1;
  int32_t _bind$647 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$644);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2151 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$648 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$644, _tmp$2151
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$649 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$3694 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$3694)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$3694->$0 = _bind$648;
  _block$3694->$1 = 0;
  _block$3694->$2 = capacity$644;
  _block$3694->$3 = _bind$646;
  _block$3694->$4 = _bind$647;
  _block$3694->$5 = _bind$649;
  _block$3694->$6 = -1;
  return _block$3694;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$639
) {
  int32_t capacity$638 = $Int$$next_power_of_two(capacity$639);
  int32_t _bind$640 = capacity$638 - 1;
  int32_t _bind$641 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$638);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2150 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$642 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$638, _tmp$2150
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$643 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$3695 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$3695)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$3695->$0 = _bind$642;
  _block$3695->$1 = 0;
  _block$3695->$2 = capacity$638;
  _block$3695->$3 = _bind$640;
  _block$3695->$4 = _bind$641;
  _block$3695->$5 = _bind$643;
  _block$3695->$6 = -1;
  return _block$3695;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$633
) {
  int32_t capacity$632 = $Int$$next_power_of_two(capacity$633);
  int32_t _bind$634 = capacity$632 - 1;
  int32_t _bind$635 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$632);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2149 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$636 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$632, _tmp$2149
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$637 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$3696 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$3696)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$3696->$0 = _bind$636;
  _block$3696->$1 = 0;
  _block$3696->$2 = capacity$632;
  _block$3696->$3 = _bind$634;
  _block$3696->$4 = _bind$635;
  _block$3696->$5 = _bind$637;
  _block$3696->$6 = -1;
  return _block$3696;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$627
) {
  int32_t capacity$626 = $Int$$next_power_of_two(capacity$627);
  int32_t _bind$628 = capacity$626 - 1;
  int32_t _bind$629 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$626);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2148 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$630 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$626, _tmp$2148
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$631 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$3697 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$3697)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$3697->$0 = _bind$630;
  _block$3697->$1 = 0;
  _block$3697->$2 = capacity$626;
  _block$3697->$3 = _bind$628;
  _block$3697->$4 = _bind$629;
  _block$3697->$5 = _bind$631;
  _block$3697->$6 = -1;
  return _block$3697;
}

int32_t $Int$$next_power_of_two(int32_t self$625) {
  if (self$625 >= 0) {
    int32_t _tmp$2147;
    int32_t _tmp$2146;
    int32_t _tmp$2145;
    int32_t _tmp$2144;
    if (self$625 <= 1) {
      return 1;
    }
    if (self$625 > 1073741824) {
      return 1073741824;
    }
    _tmp$2147 = self$625 - 1;
    _tmp$2146 = moonbit_clz32(_tmp$2147);
    _tmp$2145 = _tmp$2146 - 1;
    _tmp$2144 = 2147483647 >> (_tmp$2145 & 31);
    return _tmp$2144 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$624) {
  int32_t _tmp$2143 = capacity$624 * 13;
  return _tmp$2143 / 16;
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$622
) {
  if (self$622 == 0) {
    if (self$622) {
      moonbit_decref(self$622);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$623 =
      self$622;
    return _Some$623;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$620
) {
  if (self$620 == 0) {
    if (self$620) {
      moonbit_decref(self$620);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$621 =
      self$620;
    return _Some$621;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$618
) {
  if (self$618 == 0) {
    if (self$618) {
      moonbit_decref(self$618);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$619 =
      self$618;
    return _Some$619;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$616
) {
  if (self$616 == 0) {
    if (self$616) {
      moonbit_decref(self$616);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$617 =
      self$616;
    return _Some$617;
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  moonbit_string_t self$610,
  moonbit_string_t other$611
) {
  if (self$610 == 0) {
    int32_t _tmp$3222;
    if (self$610) {
      moonbit_decref(self$610);
    }
    _tmp$3222 = other$611 == 0;
    if (other$611) {
      moonbit_decref(other$611);
    }
    return _tmp$3222;
  } else {
    moonbit_string_t _Some$612 = self$610;
    moonbit_string_t _x$613 = _Some$612;
    if (other$611 == 0) {
      moonbit_decref(_x$613);
      if (other$611) {
        moonbit_decref(other$611);
      }
      return 0;
    } else {
      moonbit_string_t _Some$614 = other$611;
      moonbit_string_t _y$615 = _Some$614;
      int32_t _tmp$3223 = moonbit_val_array_equal(_x$613, _y$615);
      moonbit_decref(_x$613);
      moonbit_decref(_y$615);
      return _tmp$3223;
    }
  }
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$609
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$2142 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$609);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$2142);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$608
) {
  moonbit_string_t* _field$3225 = self$608->$0;
  moonbit_string_t* buf$2140 = _field$3225;
  int32_t _field$3224 = self$608->$1;
  int32_t _cnt$3492 = Moonbit_object_header(self$608)->rc;
  int32_t len$2141;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$2139;
  if (_cnt$3492 > 1) {
    int32_t _new_cnt$3493;
    moonbit_incref(buf$2140);
    _new_cnt$3493 = _cnt$3492 - 1;
    Moonbit_object_header(self$608)->rc = _new_cnt$3493;
  } else if (_cnt$3492 == 1) {
    moonbit_free(self$608);
  }
  len$2141 = _field$3224;
  _tmp$2139
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$2141, buf$2140
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$2139);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$606
) {
  struct $Ref$3c$Int$3e$* i$605 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$3698;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$2128;
  Moonbit_object_header(i$605)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$605->$0 = 0;
  _closure$3698
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$3698)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$3698->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$3698->$0_0 = self$606.$0;
  _closure$3698->$0_1 = self$606.$1;
  _closure$3698->$0_2 = self$606.$2;
  _closure$3698->$1 = i$605;
  _tmp$2128 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$3698;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$2128);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$2129
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$2130 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$2129;
  struct $Ref$3c$Int$3e$* _field$3230 = _casted_env$2130->$1;
  struct $Ref$3c$Int$3e$* i$605 = _field$3230;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$3229 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$2130->$0_1, _casted_env$2130->$0_2, _casted_env$2130->$0_0
    };
  int32_t _cnt$3494 = Moonbit_object_header(_casted_env$2130)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$606;
  int32_t val$2131;
  int32_t _tmp$2132;
  if (_cnt$3494 > 1) {
    int32_t _new_cnt$3495;
    moonbit_incref(i$605);
    moonbit_incref(_field$3229.$0);
    _new_cnt$3495 = _cnt$3494 - 1;
    Moonbit_object_header(_casted_env$2130)->rc = _new_cnt$3495;
  } else if (_cnt$3494 == 1) {
    moonbit_free(_casted_env$2130);
  }
  self$606 = _field$3229;
  val$2131 = i$605->$0;
  moonbit_incref(self$606.$0);
  _tmp$2132 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$606);
  if (val$2131 < _tmp$2132) {
    moonbit_string_t* _field$3228 = self$606.$0;
    moonbit_string_t* buf$2135 = _field$3228;
    int32_t _field$3227 = self$606.$1;
    int32_t start$2137 = _field$3227;
    int32_t val$2138 = i$605->$0;
    int32_t _tmp$2136 = start$2137 + val$2138;
    moonbit_string_t _tmp$3226 = (moonbit_string_t)buf$2135[_tmp$2136];
    moonbit_string_t elem$607;
    int32_t val$2134;
    int32_t _tmp$2133;
    moonbit_incref(_tmp$3226);
    moonbit_decref(buf$2135);
    elem$607 = _tmp$3226;
    val$2134 = i$605->$0;
    _tmp$2133 = val$2134 + 1;
    i$605->$0 = _tmp$2133;
    moonbit_decref(i$605);
    return elem$607;
  } else {
    moonbit_decref(self$606.$0);
    moonbit_decref(i$605);
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  moonbit_string_t self$601,
  struct $$moonbitlang$core$builtin$Logger logger$602
) {
  if (self$601 == 0) {
    if (self$601) {
      moonbit_decref(self$601);
    }
    logger$602.$0->$method_0(
      logger$602.$1, (moonbit_string_t)moonbit_string_literal_126.data
    );
  } else {
    moonbit_string_t _Some$603 = self$601;
    moonbit_string_t _arg$604 = _Some$603;
    struct $$moonbitlang$core$builtin$Logger _bind$2127;
    if (logger$602.$1) {
      moonbit_incref(logger$602.$1);
    }
    logger$602.$0->$method_0(
      logger$602.$1, (moonbit_string_t)moonbit_string_literal_127.data
    );
    if (logger$602.$1) {
      moonbit_incref(logger$602.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$0(logger$602, _arg$604);
    _bind$2127 = logger$602;
    _bind$2127.$0->$method_0(
      _bind$2127.$1, (moonbit_string_t)moonbit_string_literal_128.data
    );
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$600
) {
  return self$600;
}

int32_t $$moonbitlang$core$builtin$Show$$UInt64$$output(
  uint64_t self$599,
  struct $$moonbitlang$core$builtin$Logger logger$598
) {
  moonbit_string_t _tmp$2126 = $UInt64$$to_string$inner(self$599, 10);
  logger$598.$0->$method_0(logger$598.$1, _tmp$2126);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$597,
  struct $$moonbitlang$core$builtin$Logger logger$596
) {
  moonbit_string_t _tmp$2125 = $Int$$to_string$inner(self$597, 10);
  logger$596.$0->$method_0(logger$596.$1, _tmp$2125);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$594,
  struct $$moonbitlang$core$builtin$Logger logger$595
) {
  if (self$594) {
    logger$595.$0->$method_0(
      logger$595.$1, (moonbit_string_t)moonbit_string_literal_129.data
    );
  } else {
    logger$595.$0->$method_0(
      logger$595.$1, (moonbit_string_t)moonbit_string_literal_130.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$592,
  struct $$3c$String$3e$$3d$$3e$Int* f$593
) {
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _func$591 = self$592;
  return _func$591->code(_func$591, f$593);
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$588,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$590
) {
  int32_t len$2120 = self$588->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$2122;
  int32_t _tmp$3233;
  int32_t _tmp$2121;
  int32_t length$589;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$3232;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$2123;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$3231;
  int32_t _tmp$2124;
  moonbit_incref(self$588);
  _tmp$2122 = $$moonbitlang$core$builtin$Array$$buffer$2(self$588);
  _tmp$3233 = Moonbit_array_length(_tmp$2122);
  moonbit_decref(_tmp$2122);
  _tmp$2121 = _tmp$3233;
  if (len$2120 == _tmp$2121) {
    moonbit_incref(self$588);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$588);
  }
  length$589 = self$588->$1;
  _field$3232 = self$588->$0;
  buf$2123 = _field$3232;
  _old$3231
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$2123[
      length$589
    ];
  if (_old$3231) {
    moonbit_decref(_old$3231);
  }
  buf$2123[length$589] = value$590;
  _tmp$2124 = length$589 + 1;
  self$588->$1 = _tmp$2124;
  moonbit_decref(self$588);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$585,
  struct $$3c$String$2a$Int$3e$* value$587
) {
  int32_t len$2115 = self$585->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$2117;
  int32_t _tmp$3236;
  int32_t _tmp$2116;
  int32_t length$586;
  struct $$3c$String$2a$Int$3e$** _field$3235;
  struct $$3c$String$2a$Int$3e$** buf$2118;
  struct $$3c$String$2a$Int$3e$* _old$3234;
  int32_t _tmp$2119;
  moonbit_incref(self$585);
  _tmp$2117 = $$moonbitlang$core$builtin$Array$$buffer$0(self$585);
  _tmp$3236 = Moonbit_array_length(_tmp$2117);
  moonbit_decref(_tmp$2117);
  _tmp$2116 = _tmp$3236;
  if (len$2115 == _tmp$2116) {
    moonbit_incref(self$585);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$585);
  }
  length$586 = self$585->$1;
  _field$3235 = self$585->$0;
  buf$2118 = _field$3235;
  _old$3234 = (struct $$3c$String$2a$Int$3e$*)buf$2118[length$586];
  if (_old$3234) {
    moonbit_decref(_old$3234);
  }
  buf$2118[length$586] = value$587;
  _tmp$2119 = length$586 + 1;
  self$585->$1 = _tmp$2119;
  moonbit_decref(self$585);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$582,
  moonbit_string_t value$584
) {
  int32_t len$2110 = self$582->$1;
  moonbit_string_t* _tmp$2112;
  int32_t _tmp$3239;
  int32_t _tmp$2111;
  int32_t length$583;
  moonbit_string_t* _field$3238;
  moonbit_string_t* buf$2113;
  moonbit_string_t _old$3237;
  int32_t _tmp$2114;
  moonbit_incref(self$582);
  _tmp$2112 = $$moonbitlang$core$builtin$Array$$buffer$1(self$582);
  _tmp$3239 = Moonbit_array_length(_tmp$2112);
  moonbit_decref(_tmp$2112);
  _tmp$2111 = _tmp$3239;
  if (len$2110 == _tmp$2111) {
    moonbit_incref(self$582);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$582);
  }
  length$583 = self$582->$1;
  _field$3238 = self$582->$0;
  buf$2113 = _field$3238;
  _old$3237 = (moonbit_string_t)buf$2113[length$583];
  moonbit_decref(_old$3237);
  buf$2113[length$583] = value$584;
  _tmp$2114 = length$583 + 1;
  self$582->$1 = _tmp$2114;
  moonbit_decref(self$582);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$580
) {
  int32_t old_cap$579 = self$580->$1;
  int32_t new_cap$581;
  if (old_cap$579 == 0) {
    new_cap$581 = 8;
  } else {
    new_cap$581 = old_cap$579 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$580, new_cap$581);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$577
) {
  int32_t old_cap$576 = self$577->$1;
  int32_t new_cap$578;
  if (old_cap$576 == 0) {
    new_cap$578 = 8;
  } else {
    new_cap$578 = old_cap$576 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$577, new_cap$578);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$574
) {
  int32_t old_cap$573 = self$574->$1;
  int32_t new_cap$575;
  if (old_cap$573 == 0) {
    new_cap$575 = 8;
  } else {
    new_cap$575 = old_cap$573 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$574, new_cap$575);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$570,
  int32_t new_capacity$568
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** new_buf$567 =
    (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_make_ref_array(
      new_capacity$568, 0
    );
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$3241 =
    self$570->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$569 =
    _field$3241;
  int32_t old_cap$571 = Moonbit_array_length(old_buf$569);
  int32_t copy_len$572;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$3240;
  if (old_cap$571 < new_capacity$568) {
    copy_len$572 = old_cap$571;
  } else {
    copy_len$572 = new_capacity$568;
  }
  moonbit_incref(old_buf$569);
  moonbit_incref(new_buf$567);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$567, 0, old_buf$569, 0, copy_len$572
  );
  _old$3240 = self$570->$0;
  moonbit_decref(_old$3240);
  self$570->$0 = new_buf$567;
  moonbit_decref(self$570);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$564,
  int32_t new_capacity$562
) {
  struct $$3c$String$2a$Int$3e$** new_buf$561 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_make_ref_array(
      new_capacity$562, 0
    );
  struct $$3c$String$2a$Int$3e$** _field$3243 = self$564->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$563 = _field$3243;
  int32_t old_cap$565 = Moonbit_array_length(old_buf$563);
  int32_t copy_len$566;
  struct $$3c$String$2a$Int$3e$** _old$3242;
  if (old_cap$565 < new_capacity$562) {
    copy_len$566 = old_cap$565;
  } else {
    copy_len$566 = new_capacity$562;
  }
  moonbit_incref(old_buf$563);
  moonbit_incref(new_buf$561);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$561, 0, old_buf$563, 0, copy_len$566
  );
  _old$3242 = self$564->$0;
  moonbit_decref(_old$3242);
  self$564->$0 = new_buf$561;
  moonbit_decref(self$564);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$558,
  int32_t new_capacity$556
) {
  moonbit_string_t* new_buf$555 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$556, (moonbit_string_t)moonbit_string_literal_3.data
    );
  moonbit_string_t* _field$3245 = self$558->$0;
  moonbit_string_t* old_buf$557 = _field$3245;
  int32_t old_cap$559 = Moonbit_array_length(old_buf$557);
  int32_t copy_len$560;
  moonbit_string_t* _old$3244;
  if (old_cap$559 < new_capacity$556) {
    copy_len$560 = old_cap$559;
  } else {
    copy_len$560 = new_capacity$556;
  }
  moonbit_incref(old_buf$557);
  moonbit_incref(new_buf$555);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$555, 0, old_buf$557, 0, copy_len$560
  );
  _old$3244 = self$558->$0;
  moonbit_decref(_old$3244);
  self$558->$0 = new_buf$555;
  moonbit_decref(self$558);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$554
) {
  if (capacity$554 == 0) {
    moonbit_string_t* _tmp$2108 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$3699 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$3699)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$3699->$0 = _tmp$2108;
    _block$3699->$1 = 0;
    return _block$3699;
  } else {
    moonbit_string_t* _tmp$2109 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$554, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$3700 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$3700)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$3700->$0 = _tmp$2109;
    _block$3700->$1 = 0;
    return _block$3700;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$552,
  struct $StringView str$553
) {
  int32_t len$2096 = self$552->$1;
  int32_t _tmp$2098;
  int32_t _tmp$2097;
  int32_t _tmp$2095;
  moonbit_bytes_t _field$3246;
  moonbit_bytes_t data$2099;
  int32_t len$2100;
  moonbit_string_t _tmp$2101;
  int32_t _tmp$2102;
  int32_t _tmp$2103;
  int32_t len$2105;
  int32_t _tmp$2107;
  int32_t _tmp$2106;
  int32_t _tmp$2104;
  moonbit_incref(str$553.$0);
  _tmp$2098 = $StringView$$length(str$553);
  _tmp$2097 = _tmp$2098 * 2;
  _tmp$2095 = len$2096 + _tmp$2097;
  moonbit_incref(self$552);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$552, _tmp$2095
  );
  _field$3246 = self$552->$0;
  data$2099 = _field$3246;
  len$2100 = self$552->$1;
  moonbit_incref(data$2099);
  moonbit_incref(str$553.$0);
  _tmp$2101 = $StringView$$data(str$553);
  moonbit_incref(str$553.$0);
  _tmp$2102 = $StringView$$start_offset(str$553);
  moonbit_incref(str$553.$0);
  _tmp$2103 = $StringView$$length(str$553);
  $FixedArray$$blit_from_string(
    data$2099, len$2100, _tmp$2101, _tmp$2102, _tmp$2103
  );
  len$2105 = self$552->$1;
  _tmp$2107 = $StringView$$length(str$553);
  _tmp$2106 = _tmp$2107 * 2;
  _tmp$2104 = len$2105 + _tmp$2106;
  self$552->$1 = _tmp$2104;
  moonbit_decref(self$552);
  return 0;
}

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$549,
  int32_t i$550,
  int32_t start_offset$551,
  int64_t end_offset$547
) {
  int32_t end_offset$546;
  if (end_offset$547 == 4294967296ll) {
    end_offset$546 = Moonbit_array_length(self$549);
  } else {
    int64_t _Some$548 = end_offset$547;
    end_offset$546 = (int32_t)_Some$548;
  }
  if (i$550 >= 0) {
    return $String$$offset_of_nth_char_forward(
             self$549, i$550, start_offset$551, end_offset$546
           );
  } else {
    int32_t _tmp$2094 = -i$550;
    return $String$$offset_of_nth_char_backward(
             self$549, _tmp$2094, start_offset$551, end_offset$546
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$544,
  int32_t n$542,
  int32_t start_offset$538,
  int32_t end_offset$539
) {
  int32_t _if_result$3701;
  if (start_offset$538 >= 0) {
    _if_result$3701 = start_offset$538 <= end_offset$539;
  } else {
    _if_result$3701 = 0;
  }
  if (_if_result$3701) {
    int32_t utf16_offset$540 = start_offset$538;
    int32_t char_count$541 = 0;
    int32_t _tmp$2092;
    int32_t _if_result$3704;
    while (1) {
      int32_t _tmp$2086 = utf16_offset$540;
      int32_t _if_result$3703;
      if (_tmp$2086 < end_offset$539) {
        int32_t _tmp$2085 = char_count$541;
        _if_result$3703 = _tmp$2085 < n$542;
      } else {
        _if_result$3703 = 0;
      }
      if (_if_result$3703) {
        int32_t _tmp$2090 = utf16_offset$540;
        int32_t c$543 = self$544[_tmp$2090];
        int32_t _tmp$2089;
        if ($Int$$is_leading_surrogate(c$543)) {
          int32_t _tmp$2087 = utf16_offset$540;
          utf16_offset$540 = _tmp$2087 + 2;
        } else {
          int32_t _tmp$2088 = utf16_offset$540;
          utf16_offset$540 = _tmp$2088 + 1;
        }
        _tmp$2089 = char_count$541;
        char_count$541 = _tmp$2089 + 1;
        continue;
      } else {
        moonbit_decref(self$544);
      }
      break;
    }
    _tmp$2092 = char_count$541;
    if (_tmp$2092 < n$542) {
      _if_result$3704 = 1;
    } else {
      int32_t _tmp$2091 = utf16_offset$540;
      _if_result$3704 = _tmp$2091 >= end_offset$539;
    }
    if (_if_result$3704) {
      return 4294967296ll;
    } else {
      int32_t _tmp$2093 = utf16_offset$540;
      return (int64_t)_tmp$2093;
    }
  } else {
    moonbit_decref(self$544);
    return $moonbitlang$core$builtin$abort$3(
             (moonbit_string_t)moonbit_string_literal_131.data,
               (moonbit_string_t)moonbit_string_literal_132.data
           );
  }
}

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$536,
  int32_t n$534,
  int32_t start_offset$533,
  int32_t end_offset$532
) {
  int32_t char_count$530 = 0;
  int32_t utf16_offset$531 = end_offset$532;
  int32_t _tmp$2083;
  int32_t _if_result$3707;
  while (1) {
    int32_t _tmp$2076 = utf16_offset$531;
    int32_t _tmp$2075 = _tmp$2076 - 1;
    int32_t _if_result$3706;
    if (_tmp$2075 >= start_offset$533) {
      int32_t _tmp$2074 = char_count$530;
      _if_result$3706 = _tmp$2074 < n$534;
    } else {
      _if_result$3706 = 0;
    }
    if (_if_result$3706) {
      int32_t _tmp$2081 = utf16_offset$531;
      int32_t _tmp$2080 = _tmp$2081 - 1;
      int32_t c$535 = self$536[_tmp$2080];
      int32_t _tmp$2079;
      if ($Int$$is_trailing_surrogate(c$535)) {
        int32_t _tmp$2077 = utf16_offset$531;
        utf16_offset$531 = _tmp$2077 - 2;
      } else {
        int32_t _tmp$2078 = utf16_offset$531;
        utf16_offset$531 = _tmp$2078 - 1;
      }
      _tmp$2079 = char_count$530;
      char_count$530 = _tmp$2079 + 1;
      continue;
    } else {
      moonbit_decref(self$536);
    }
    break;
  }
  _tmp$2083 = char_count$530;
  if (_tmp$2083 < n$534) {
    _if_result$3707 = 1;
  } else {
    int32_t _tmp$2082 = utf16_offset$531;
    _if_result$3707 = _tmp$2082 < start_offset$533;
  }
  if (_if_result$3707) {
    return 4294967296ll;
  } else {
    int32_t _tmp$2084 = utf16_offset$531;
    return (int64_t)_tmp$2084;
  }
}

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$522,
  int32_t len$525,
  int32_t start_offset$529,
  int64_t end_offset$520
) {
  int32_t end_offset$519;
  int32_t index$523;
  int32_t count$524;
  if (end_offset$520 == 4294967296ll) {
    end_offset$519 = Moonbit_array_length(self$522);
  } else {
    int64_t _Some$521 = end_offset$520;
    end_offset$519 = (int32_t)_Some$521;
  }
  index$523 = start_offset$529;
  count$524 = 0;
  while (1) {
    int32_t _if_result$3709;
    if (index$523 < end_offset$519) {
      _if_result$3709 = count$524 < len$525;
    } else {
      _if_result$3709 = 0;
    }
    if (_if_result$3709) {
      int32_t c1$526 = self$522[index$523];
      int32_t _if_result$3710;
      int32_t _tmp$2072;
      int32_t _tmp$2073;
      if ($Int$$is_leading_surrogate(c1$526)) {
        int32_t _tmp$2068 = index$523 + 1;
        _if_result$3710 = _tmp$2068 < end_offset$519;
      } else {
        _if_result$3710 = 0;
      }
      if (_if_result$3710) {
        int32_t _tmp$2071 = index$523 + 1;
        int32_t c2$527 = self$522[_tmp$2071];
        if ($Int$$is_trailing_surrogate(c2$527)) {
          int32_t _tmp$2069 = index$523 + 2;
          int32_t _tmp$2070 = count$524 + 1;
          index$523 = _tmp$2069;
          count$524 = _tmp$2070;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_133.data,
              (moonbit_string_t)moonbit_string_literal_134.data
          );
        }
      }
      _tmp$2072 = index$523 + 1;
      _tmp$2073 = count$524 + 1;
      index$523 = _tmp$2072;
      count$524 = _tmp$2073;
      continue;
    } else {
      moonbit_decref(self$522);
      return count$524 >= len$525;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$518
) {
  int32_t end$2066 = self$518.$2;
  int32_t _field$3247 = self$518.$1;
  int32_t start$2067;
  moonbit_decref(self$518.$0);
  start$2067 = _field$3247;
  return end$2066 - start$2067;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$517
) {
  int32_t end$2064 = self$517.$2;
  int32_t _field$3248 = self$517.$1;
  int32_t start$2065;
  moonbit_decref(self$517.$0);
  start$2065 = _field$3248;
  return end$2064 - start$2065;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$516
) {
  int32_t end$2062 = self$516.$2;
  int32_t _field$3249 = self$516.$1;
  int32_t start$2063;
  moonbit_decref(self$516.$0);
  start$2063 = _field$3249;
  return end$2062 - start$2063;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$515
) {
  int32_t end$2060 = self$515.$2;
  int32_t _field$3250 = self$515.$1;
  int32_t start$2061;
  moonbit_decref(self$515.$0);
  start$2061 = _field$3250;
  return end$2060 - start$2061;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$514
) {
  int32_t end$2058 = self$514.$2;
  int32_t _field$3251 = self$514.$1;
  int32_t start$2059;
  moonbit_decref(self$514.$0);
  start$2059 = _field$3251;
  return end$2058 - start$2059;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$509
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$3711 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$3711)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$3711->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$3711->$0 = self$509;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$3711;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$2056,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$507
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$2057 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$2056;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$3252 =
    _casted_env$2057->$0;
  int32_t _cnt$3496 = Moonbit_object_header(_casted_env$2057)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$509;
  if (_cnt$3496 > 1) {
    int32_t _new_cnt$3497;
    moonbit_incref(_field$3252);
    _new_cnt$3497 = _cnt$3496 - 1;
    Moonbit_object_header(_casted_env$2057)->rc = _new_cnt$3497;
  } else if (_cnt$3496 == 1) {
    moonbit_free(_casted_env$2057);
  }
  self$509 = _field$3252;
  while (1) {
    moonbit_string_t _bind$508;
    moonbit_incref(self$509);
    _bind$508 = $$moonbitlang$core$builtin$Iterator$$next$0(self$509);
    if (_bind$508 == 0) {
      moonbit_decref(self$509);
      if (_bind$508) {
        moonbit_decref(_bind$508);
      }
      moonbit_decref(yield_$507);
      return 1;
    } else {
      moonbit_string_t _Some$510 = _bind$508;
      moonbit_string_t _x$511 = _Some$510;
      int32_t _bind$512;
      moonbit_incref(yield_$507);
      _bind$512 = yield_$507->code(yield_$507, _x$511);
      switch (_bind$512) {
        case 1:
          break;
        default: {
          moonbit_decref(self$509);
          moonbit_decref(yield_$507);
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$506
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _func$505 = self$506;
  return _func$505->code(_func$505);
}

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$497,
  struct $$moonbitlang$core$builtin$Logger logger$495
) {
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$496;
  int32_t len$498;
  int32_t i$499;
  int32_t seg$500;
  if (logger$495.$1) {
    moonbit_incref(logger$495.$1);
  }
  logger$495.$0->$method_3(logger$495.$1, 34);
  if (logger$495.$1) {
    moonbit_incref(logger$495.$1);
  }
  moonbit_incref(self$497);
  _env$496
  = (struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$)
    );
  Moonbit_object_header(_env$496)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$, $0_0
    )
    >> 2,
      3,
      0
  );
  _env$496->$0_0 = logger$495.$0;
  _env$496->$0_1 = logger$495.$1;
  _env$496->$1 = self$497;
  len$498 = Moonbit_array_length(self$497);
  i$499 = 0;
  seg$500 = 0;
  $$2a$for$501:;
  while (1) {
    int32_t code$502;
    int32_t c$504;
    struct $$moonbitlang$core$builtin$Logger _bind$2038;
    int32_t _tmp$2039;
    int32_t _tmp$2040;
    int32_t _tmp$2041;
    int32_t _tmp$3716;
    int32_t _tmp$3717;
    if (i$499 >= len$498) {
      moonbit_decref(self$497);
      $moonbitlang$core$builtin$output$flush_segment$7c$3831(
        _env$496, seg$500, i$499
      );
      break;
    }
    code$502 = self$497[i$499];
    switch (code$502) {
      case 34: {
        c$504 = code$502;
        goto $join$503;
        break;
      }
      
      case 92: {
        c$504 = code$502;
        goto $join$503;
        break;
      }
      
      case 10: {
        int32_t _tmp$2042;
        int32_t _tmp$2043;
        moonbit_incref(_env$496);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$496, seg$500, i$499
        );
        if (logger$495.$1) {
          moonbit_incref(logger$495.$1);
        }
        logger$495.$0->$method_0(
          logger$495.$1, (moonbit_string_t)moonbit_string_literal_135.data
        );
        _tmp$2042 = i$499 + 1;
        _tmp$2043 = i$499 + 1;
        i$499 = _tmp$2042;
        seg$500 = _tmp$2043;
        goto $$2a$for$501;
        break;
      }
      
      case 13: {
        int32_t _tmp$2044;
        int32_t _tmp$2045;
        moonbit_incref(_env$496);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$496, seg$500, i$499
        );
        if (logger$495.$1) {
          moonbit_incref(logger$495.$1);
        }
        logger$495.$0->$method_0(
          logger$495.$1, (moonbit_string_t)moonbit_string_literal_136.data
        );
        _tmp$2044 = i$499 + 1;
        _tmp$2045 = i$499 + 1;
        i$499 = _tmp$2044;
        seg$500 = _tmp$2045;
        goto $$2a$for$501;
        break;
      }
      
      case 8: {
        int32_t _tmp$2046;
        int32_t _tmp$2047;
        moonbit_incref(_env$496);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$496, seg$500, i$499
        );
        if (logger$495.$1) {
          moonbit_incref(logger$495.$1);
        }
        logger$495.$0->$method_0(
          logger$495.$1, (moonbit_string_t)moonbit_string_literal_137.data
        );
        _tmp$2046 = i$499 + 1;
        _tmp$2047 = i$499 + 1;
        i$499 = _tmp$2046;
        seg$500 = _tmp$2047;
        goto $$2a$for$501;
        break;
      }
      
      case 9: {
        int32_t _tmp$2048;
        int32_t _tmp$2049;
        moonbit_incref(_env$496);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$496, seg$500, i$499
        );
        if (logger$495.$1) {
          moonbit_incref(logger$495.$1);
        }
        logger$495.$0->$method_0(
          logger$495.$1, (moonbit_string_t)moonbit_string_literal_138.data
        );
        _tmp$2048 = i$499 + 1;
        _tmp$2049 = i$499 + 1;
        i$499 = _tmp$2048;
        seg$500 = _tmp$2049;
        goto $$2a$for$501;
        break;
      }
      default: {
        if (code$502 < 32) {
          int32_t _tmp$2052;
          moonbit_string_t _tmp$2051;
          struct $$moonbitlang$core$builtin$Logger _bind$2050;
          int32_t _tmp$2053;
          int32_t _tmp$2054;
          moonbit_incref(_env$496);
          $moonbitlang$core$builtin$output$flush_segment$7c$3831(
            _env$496, seg$500, i$499
          );
          if (logger$495.$1) {
            moonbit_incref(logger$495.$1);
          }
          logger$495.$0->$method_0(
            logger$495.$1, (moonbit_string_t)moonbit_string_literal_139.data
          );
          _tmp$2052 = code$502 & 0xff;
          _tmp$2051 = $Byte$$to_hex(_tmp$2052);
          if (logger$495.$1) {
            moonbit_incref(logger$495.$1);
          }
          logger$495.$0->$method_0(logger$495.$1, _tmp$2051);
          _bind$2050 = logger$495;
          if (_bind$2050.$1) {
            moonbit_incref(_bind$2050.$1);
          }
          _bind$2050.$0->$method_3(_bind$2050.$1, 125);
          _tmp$2053 = i$499 + 1;
          _tmp$2054 = i$499 + 1;
          i$499 = _tmp$2053;
          seg$500 = _tmp$2054;
          goto $$2a$for$501;
        } else {
          int32_t _tmp$2055 = i$499 + 1;
          int32_t _tmp$3715 = seg$500;
          i$499 = _tmp$2055;
          seg$500 = _tmp$3715;
          goto $$2a$for$501;
        }
        break;
      }
    }
    goto $joinlet$3714;
    $join$503:;
    moonbit_incref(_env$496);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$496, seg$500, i$499
    );
    if (logger$495.$1) {
      moonbit_incref(logger$495.$1);
    }
    logger$495.$0->$method_3(logger$495.$1, 92);
    _bind$2038 = logger$495;
    _tmp$2039 = c$504;
    if (_bind$2038.$1) {
      moonbit_incref(_bind$2038.$1);
    }
    _bind$2038.$0->$method_3(_bind$2038.$1, _tmp$2039);
    _tmp$2040 = i$499 + 1;
    _tmp$2041 = i$499 + 1;
    i$499 = _tmp$2040;
    seg$500 = _tmp$2041;
    continue;
    $joinlet$3714:;
    _tmp$3716 = i$499;
    _tmp$3717 = seg$500;
    i$499 = _tmp$3716;
    seg$500 = _tmp$3717;
    continue;
    break;
  }
  logger$495.$0->$method_3(logger$495.$1, 34);
  return 0;
}

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$491,
  int32_t seg$494,
  int32_t i$493
) {
  moonbit_string_t _field$3254 = _env$491->$1;
  moonbit_string_t self$490 = _field$3254;
  struct $$moonbitlang$core$builtin$Logger _field$3253 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$491->$0_0, _env$491->$0_1
    };
  int32_t _cnt$3498 = Moonbit_object_header(_env$491)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$492;
  if (_cnt$3498 > 1) {
    int32_t _new_cnt$3499;
    moonbit_incref(self$490);
    if (_field$3253.$1) {
      moonbit_incref(_field$3253.$1);
    }
    _new_cnt$3499 = _cnt$3498 - 1;
    Moonbit_object_header(_env$491)->rc = _new_cnt$3499;
  } else if (_cnt$3498 == 1) {
    moonbit_free(_env$491);
  }
  logger$492 = _field$3253;
  if (i$493 > seg$494) {
    int32_t _tmp$2037 = i$493 - seg$494;
    logger$492.$0->$method_1(logger$492.$1, self$490, seg$494, _tmp$2037);
  } else {
    if (logger$492.$1) {
      moonbit_decref(logger$492.$1);
    }
    moonbit_decref(self$490);
  }
  return 0;
}

moonbit_string_t $Byte$$to_hex(int32_t b$489) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$488 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _tmp$2034 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$489, 16);
  int32_t _tmp$2033 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$2034);
  int32_t _tmp$2036;
  int32_t _tmp$2035;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$2032;
  moonbit_incref(_self$488);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$488, _tmp$2033
  );
  _tmp$2036 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$489, 16);
  _tmp$2035
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$2036
  );
  moonbit_incref(_self$488);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$488, _tmp$2035
  );
  _tmp$2032 = _self$488;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$2032);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$487) {
  if (i$487 < 10) {
    int32_t _tmp$2029 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$487, 48);
    return $Byte$$to_char(_tmp$2029);
  } else {
    int32_t _tmp$2031 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$487, 97);
    int32_t _tmp$2030 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$2031, 10);
    return $Byte$$to_char(_tmp$2030);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$485,
  int32_t that$486
) {
  int32_t _tmp$2027 = (int32_t)self$485;
  int32_t _tmp$2028 = (int32_t)that$486;
  int32_t _tmp$2026 = _tmp$2027 - _tmp$2028;
  return _tmp$2026 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$483,
  int32_t that$484
) {
  int32_t _tmp$2024 = (int32_t)self$483;
  int32_t _tmp$2025 = (int32_t)that$484;
  int32_t _tmp$2023 = _tmp$2024 % _tmp$2025;
  return _tmp$2023 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$481,
  int32_t that$482
) {
  int32_t _tmp$2021 = (int32_t)self$481;
  int32_t _tmp$2022 = (int32_t)that$482;
  int32_t _tmp$2020 = _tmp$2021 / _tmp$2022;
  return _tmp$2020 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$479,
  int32_t that$480
) {
  int32_t _tmp$2018 = (int32_t)self$479;
  int32_t _tmp$2019 = (int32_t)that$480;
  int32_t _tmp$2017 = _tmp$2018 + _tmp$2019;
  return _tmp$2017 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$476,
  int32_t start$474,
  int32_t end$475
) {
  int32_t _if_result$3718;
  int32_t len$477;
  int32_t _tmp$2015;
  int32_t _tmp$2016;
  moonbit_bytes_t bytes$478;
  moonbit_bytes_t _tmp$2014;
  if (start$474 == 0) {
    int32_t _tmp$2013 = Moonbit_array_length(str$476);
    _if_result$3718 = end$475 == _tmp$2013;
  } else {
    _if_result$3718 = 0;
  }
  if (_if_result$3718) {
    return str$476;
  }
  len$477 = end$475 - start$474;
  _tmp$2015 = len$477 * 2;
  _tmp$2016 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$478 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$2015, _tmp$2016);
  moonbit_incref(bytes$478);
  $FixedArray$$blit_from_string(bytes$478, 0, str$476, start$474, len$477);
  _tmp$2014 = bytes$478;
  return $Bytes$$to_unchecked_string$inner(_tmp$2014, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$473
) {
  return f$473;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  uint64_t a$467,
  uint64_t b$468,
  moonbit_string_t msg$470,
  moonbit_string_t loc$472
) {
  if (a$467 != b$468) {
    moonbit_string_t fail_msg$469;
    if (msg$470 == 0) {
      moonbit_string_t _tmp$2011;
      moonbit_string_t _tmp$2010;
      moonbit_string_t _tmp$2009;
      moonbit_string_t _tmp$2006;
      moonbit_string_t _tmp$2008;
      moonbit_string_t _tmp$2007;
      moonbit_string_t _tmp$2005;
      if (msg$470) {
        moonbit_decref(msg$470);
      }
      _tmp$2011 = $moonbitlang$core$builtin$debug_string$4(a$467);
      _tmp$2010
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2011
      );
      _tmp$2009
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_123.data, _tmp$2010
      );
      _tmp$2006
      = moonbit_add_string(
        _tmp$2009, (moonbit_string_t)moonbit_string_literal_140.data
      );
      _tmp$2008 = $moonbitlang$core$builtin$debug_string$4(b$468);
      _tmp$2007
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2008
      );
      _tmp$2005 = moonbit_add_string(_tmp$2006, _tmp$2007);
      fail_msg$469
      = moonbit_add_string(
        _tmp$2005, (moonbit_string_t)moonbit_string_literal_123.data
      );
    } else {
      moonbit_string_t _Some$471 = msg$470;
      fail_msg$469 = _Some$471;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$469, loc$472);
  } else {
    int32_t _tmp$2012;
    struct moonbit_result_0 _result$3719;
    moonbit_decref(loc$472);
    if (msg$470) {
      moonbit_decref(msg$470);
    }
    _tmp$2012 = 0;
    _result$3719.tag = 1;
    _result$3719.data.ok = _tmp$2012;
    return _result$3719;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  int32_t a$461,
  int32_t b$462,
  moonbit_string_t msg$464,
  moonbit_string_t loc$466
) {
  if (a$461 != b$462) {
    moonbit_string_t fail_msg$463;
    if (msg$464 == 0) {
      moonbit_string_t _tmp$2003;
      moonbit_string_t _tmp$2002;
      moonbit_string_t _tmp$2001;
      moonbit_string_t _tmp$1998;
      moonbit_string_t _tmp$2000;
      moonbit_string_t _tmp$1999;
      moonbit_string_t _tmp$1997;
      if (msg$464) {
        moonbit_decref(msg$464);
      }
      _tmp$2003 = $moonbitlang$core$builtin$debug_string$3(a$461);
      _tmp$2002
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2003
      );
      _tmp$2001
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_123.data, _tmp$2002
      );
      _tmp$1998
      = moonbit_add_string(
        _tmp$2001, (moonbit_string_t)moonbit_string_literal_140.data
      );
      _tmp$2000 = $moonbitlang$core$builtin$debug_string$3(b$462);
      _tmp$1999
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2000
      );
      _tmp$1997 = moonbit_add_string(_tmp$1998, _tmp$1999);
      fail_msg$463
      = moonbit_add_string(
        _tmp$1997, (moonbit_string_t)moonbit_string_literal_123.data
      );
    } else {
      moonbit_string_t _Some$465 = msg$464;
      fail_msg$463 = _Some$465;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$463, loc$466);
  } else {
    int32_t _tmp$2004;
    struct moonbit_result_0 _result$3720;
    moonbit_decref(loc$466);
    if (msg$464) {
      moonbit_decref(msg$464);
    }
    _tmp$2004 = 0;
    _result$3720.tag = 1;
    _result$3720.data.ok = _tmp$2004;
    return _result$3720;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  int32_t a$455,
  int32_t b$456,
  moonbit_string_t msg$458,
  moonbit_string_t loc$460
) {
  if ($moonbitlang$core$builtin$op_notequal$3(a$455, b$456)) {
    moonbit_string_t fail_msg$457;
    if (msg$458 == 0) {
      moonbit_string_t _tmp$1995;
      moonbit_string_t _tmp$1994;
      moonbit_string_t _tmp$1993;
      moonbit_string_t _tmp$1990;
      moonbit_string_t _tmp$1992;
      moonbit_string_t _tmp$1991;
      moonbit_string_t _tmp$1989;
      if (msg$458) {
        moonbit_decref(msg$458);
      }
      _tmp$1995 = $moonbitlang$core$builtin$debug_string$2(a$455);
      _tmp$1994
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1995
      );
      _tmp$1993
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_123.data, _tmp$1994
      );
      _tmp$1990
      = moonbit_add_string(
        _tmp$1993, (moonbit_string_t)moonbit_string_literal_140.data
      );
      _tmp$1992 = $moonbitlang$core$builtin$debug_string$2(b$456);
      _tmp$1991
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1992
      );
      _tmp$1989 = moonbit_add_string(_tmp$1990, _tmp$1991);
      fail_msg$457
      = moonbit_add_string(
        _tmp$1989, (moonbit_string_t)moonbit_string_literal_123.data
      );
    } else {
      moonbit_string_t _Some$459 = msg$458;
      fail_msg$457 = _Some$459;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$457, loc$460);
  } else {
    int32_t _tmp$1996;
    struct moonbit_result_0 _result$3721;
    moonbit_decref(loc$460);
    if (msg$458) {
      moonbit_decref(msg$458);
    }
    _tmp$1996 = 0;
    _result$3721.tag = 1;
    _result$3721.data.ok = _tmp$1996;
    return _result$3721;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  moonbit_string_t a$449,
  moonbit_string_t b$450,
  moonbit_string_t msg$452,
  moonbit_string_t loc$454
) {
  if (b$450) {
    moonbit_incref(b$450);
  }
  if (a$449) {
    moonbit_incref(a$449);
  }
  if ($moonbitlang$core$builtin$op_notequal$2(a$449, b$450)) {
    moonbit_string_t fail_msg$451;
    if (msg$452 == 0) {
      moonbit_string_t _tmp$1987;
      moonbit_string_t _tmp$1986;
      moonbit_string_t _tmp$1985;
      moonbit_string_t _tmp$1982;
      moonbit_string_t _tmp$1984;
      moonbit_string_t _tmp$1983;
      moonbit_string_t _tmp$1981;
      if (msg$452) {
        moonbit_decref(msg$452);
      }
      _tmp$1987 = $moonbitlang$core$builtin$debug_string$1(a$449);
      _tmp$1986
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1987
      );
      _tmp$1985
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_123.data, _tmp$1986
      );
      _tmp$1982
      = moonbit_add_string(
        _tmp$1985, (moonbit_string_t)moonbit_string_literal_140.data
      );
      _tmp$1984 = $moonbitlang$core$builtin$debug_string$1(b$450);
      _tmp$1983
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1984
      );
      _tmp$1981 = moonbit_add_string(_tmp$1982, _tmp$1983);
      fail_msg$451
      = moonbit_add_string(
        _tmp$1981, (moonbit_string_t)moonbit_string_literal_123.data
      );
    } else {
      moonbit_string_t _Some$453;
      if (b$450) {
        moonbit_decref(b$450);
      }
      if (a$449) {
        moonbit_decref(a$449);
      }
      _Some$453 = msg$452;
      fail_msg$451 = _Some$453;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$451, loc$454);
  } else {
    int32_t _tmp$1988;
    struct moonbit_result_0 _result$3722;
    moonbit_decref(loc$454);
    if (msg$452) {
      moonbit_decref(msg$452);
    }
    if (b$450) {
      moonbit_decref(b$450);
    }
    if (a$449) {
      moonbit_decref(a$449);
    }
    _tmp$1988 = 0;
    _result$3722.tag = 1;
    _result$3722.data.ok = _tmp$1988;
    return _result$3722;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  moonbit_string_t a$443,
  moonbit_string_t b$444,
  moonbit_string_t msg$446,
  moonbit_string_t loc$448
) {
  moonbit_incref(b$444);
  moonbit_incref(a$443);
  if ($moonbitlang$core$builtin$op_notequal$1(a$443, b$444)) {
    moonbit_string_t fail_msg$445;
    if (msg$446 == 0) {
      moonbit_string_t _tmp$1979;
      moonbit_string_t _tmp$1978;
      moonbit_string_t _tmp$1977;
      moonbit_string_t _tmp$1974;
      moonbit_string_t _tmp$1976;
      moonbit_string_t _tmp$1975;
      moonbit_string_t _tmp$1973;
      if (msg$446) {
        moonbit_decref(msg$446);
      }
      _tmp$1979 = $moonbitlang$core$builtin$debug_string$0(a$443);
      _tmp$1978
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1979
      );
      _tmp$1977
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_123.data, _tmp$1978
      );
      _tmp$1974
      = moonbit_add_string(
        _tmp$1977, (moonbit_string_t)moonbit_string_literal_140.data
      );
      _tmp$1976 = $moonbitlang$core$builtin$debug_string$0(b$444);
      _tmp$1975
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$1976
      );
      _tmp$1973 = moonbit_add_string(_tmp$1974, _tmp$1975);
      fail_msg$445
      = moonbit_add_string(
        _tmp$1973, (moonbit_string_t)moonbit_string_literal_123.data
      );
    } else {
      moonbit_string_t _Some$447;
      moonbit_decref(b$444);
      moonbit_decref(a$443);
      _Some$447 = msg$446;
      fail_msg$445 = _Some$447;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$445, loc$448);
  } else {
    int32_t _tmp$1980;
    struct moonbit_result_0 _result$3723;
    moonbit_decref(loc$448);
    if (msg$446) {
      moonbit_decref(msg$446);
    }
    moonbit_decref(b$444);
    moonbit_decref(a$443);
    _tmp$1980 = 0;
    _result$3723.tag = 1;
    _result$3723.data.ok = _tmp$1980;
    return _result$3723;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$442,
  moonbit_string_t loc$441
) {
  moonbit_string_t _tmp$1972 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$441);
  moonbit_string_t _tmp$1970 =
    moonbit_add_string(
      _tmp$1972, (moonbit_string_t)moonbit_string_literal_141.data
    );
  moonbit_string_t _tmp$1971 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(msg$442);
  moonbit_string_t _tmp$1969 = moonbit_add_string(_tmp$1970, _tmp$1971);
  void* moonbitlang$core$builtin$Failure$Failure$1968 =
    (void*)moonbit_malloc(
      sizeof(struct $Error$moonbitlang$core$builtin$Failure$Failure)
    );
  struct moonbit_result_0 _result$3724;
  Moonbit_object_header(moonbitlang$core$builtin$Failure$Failure$1968)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Error$moonbitlang$core$builtin$Failure$Failure, $0) >> 2,
      1,
      2
  );
  ((struct $Error$moonbitlang$core$builtin$Failure$Failure*)moonbitlang$core$builtin$Failure$Failure$1968)->$0
  = _tmp$1969;
  _result$3724.tag = 0;
  _result$3724.data.err = moonbitlang$core$builtin$Failure$Failure$1968;
  return _result$3724;
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(uint64_t t$440) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$439 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1967;
  moonbit_incref(buf$439);
  _tmp$1967
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$439
  };
  $$moonbitlang$core$builtin$Show$$UInt64$$output(t$440, _tmp$1967);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$439);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(int32_t t$438) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$437 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1966;
  moonbit_incref(buf$437);
  _tmp$1966
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$437
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(t$438, _tmp$1966);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$437);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(int32_t t$436) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$435 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1965;
  moonbit_incref(buf$435);
  _tmp$1965
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$435
  };
  $$moonbitlang$core$builtin$Show$$Bool$$output(t$436, _tmp$1965);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$435);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(
  moonbit_string_t t$434
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$433 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1964;
  moonbit_incref(buf$433);
  _tmp$1964
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$433
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$0(t$434, _tmp$1964);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$433);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(
  moonbit_string_t t$432
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$431 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$1963;
  moonbit_incref(buf$431);
  _tmp$1963
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$431
  };
  $$moonbitlang$core$builtin$Show$$String$$output(t$432, _tmp$1963);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$431);
}

moonbit_string_t $UInt64$$to_string$inner(
  uint64_t self$423,
  int32_t radix$422
) {
  int32_t _if_result$3725;
  uint16_t* buffer$424;
  if (radix$422 < 2) {
    _if_result$3725 = 1;
  } else {
    _if_result$3725 = radix$422 > 36;
  }
  if (_if_result$3725) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_142.data,
        (moonbit_string_t)moonbit_string_literal_143.data
    );
  }
  if (self$423 == 0ull) {
    return (moonbit_string_t)moonbit_string_literal_144.data;
  }
  switch (radix$422) {
    case 10: {
      int32_t len$425 = $moonbitlang$core$builtin$dec_count64(self$423);
      uint16_t* buffer$426 = (uint16_t*)moonbit_make_string(len$425, 0);
      moonbit_incref(buffer$426);
      $moonbitlang$core$builtin$int64_to_string_dec(
        buffer$426, self$423, 0, len$425
      );
      buffer$424 = buffer$426;
      break;
    }
    
    case 16: {
      int32_t len$427 = $moonbitlang$core$builtin$hex_count64(self$423);
      uint16_t* buffer$428 = (uint16_t*)moonbit_make_string(len$427, 0);
      moonbit_incref(buffer$428);
      $moonbitlang$core$builtin$int64_to_string_hex(
        buffer$428, self$423, 0, len$427
      );
      buffer$424 = buffer$428;
      break;
    }
    default: {
      int32_t len$429 =
        $moonbitlang$core$builtin$radix_count64(self$423, radix$422);
      uint16_t* buffer$430 = (uint16_t*)moonbit_make_string(len$429, 0);
      moonbit_incref(buffer$430);
      $moonbitlang$core$builtin$int64_to_string_generic(
        buffer$430, self$423, 0, len$429, radix$422
      );
      buffer$424 = buffer$430;
      break;
    }
  }
  return buffer$424;
}

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$412,
  uint64_t num$400,
  int32_t digit_start$403,
  int32_t total_len$402
) {
  uint64_t num$399 = num$400;
  int32_t offset$401 = total_len$402 - digit_start$403;
  uint64_t _tmp$1962;
  int32_t remaining$414;
  int32_t _tmp$1943;
  while (1) {
    uint64_t _tmp$1906 = num$399;
    if (_tmp$1906 >= 10000ull) {
      uint64_t _tmp$1929 = num$399;
      uint64_t t$404 = _tmp$1929 / 10000ull;
      uint64_t _tmp$1928 = num$399;
      uint64_t _tmp$1927 = _tmp$1928 % 10000ull;
      int32_t r$405 = (int32_t)_tmp$1927;
      int32_t d1$406;
      int32_t d2$407;
      int32_t _tmp$1907;
      int32_t _tmp$1926;
      int32_t _tmp$1925;
      int32_t d1_hi$408;
      int32_t _tmp$1924;
      int32_t _tmp$1923;
      int32_t d1_lo$409;
      int32_t _tmp$1922;
      int32_t _tmp$1921;
      int32_t d2_hi$410;
      int32_t _tmp$1920;
      int32_t _tmp$1919;
      int32_t d2_lo$411;
      int32_t _tmp$1909;
      int32_t _tmp$1908;
      int32_t _tmp$1912;
      int32_t _tmp$1911;
      int32_t _tmp$1910;
      int32_t _tmp$1915;
      int32_t _tmp$1914;
      int32_t _tmp$1913;
      int32_t _tmp$1918;
      int32_t _tmp$1917;
      int32_t _tmp$1916;
      num$399 = t$404;
      d1$406 = r$405 / 100;
      d2$407 = r$405 % 100;
      _tmp$1907 = offset$401;
      offset$401 = _tmp$1907 - 4;
      _tmp$1926 = d1$406 / 10;
      _tmp$1925 = 48 + _tmp$1926;
      d1_hi$408 = (uint16_t)_tmp$1925;
      _tmp$1924 = d1$406 % 10;
      _tmp$1923 = 48 + _tmp$1924;
      d1_lo$409 = (uint16_t)_tmp$1923;
      _tmp$1922 = d2$407 / 10;
      _tmp$1921 = 48 + _tmp$1922;
      d2_hi$410 = (uint16_t)_tmp$1921;
      _tmp$1920 = d2$407 % 10;
      _tmp$1919 = 48 + _tmp$1920;
      d2_lo$411 = (uint16_t)_tmp$1919;
      _tmp$1909 = offset$401;
      _tmp$1908 = digit_start$403 + _tmp$1909;
      buffer$412[_tmp$1908] = d1_hi$408;
      _tmp$1912 = offset$401;
      _tmp$1911 = digit_start$403 + _tmp$1912;
      _tmp$1910 = _tmp$1911 + 1;
      buffer$412[_tmp$1910] = d1_lo$409;
      _tmp$1915 = offset$401;
      _tmp$1914 = digit_start$403 + _tmp$1915;
      _tmp$1913 = _tmp$1914 + 2;
      buffer$412[_tmp$1913] = d2_hi$410;
      _tmp$1918 = offset$401;
      _tmp$1917 = digit_start$403 + _tmp$1918;
      _tmp$1916 = _tmp$1917 + 3;
      buffer$412[_tmp$1916] = d2_lo$411;
      continue;
    }
    break;
  }
  _tmp$1962 = num$399;
  remaining$414 = (int32_t)_tmp$1962;
  while (1) {
    int32_t _tmp$1930 = remaining$414;
    if (_tmp$1930 >= 100) {
      int32_t _tmp$1942 = remaining$414;
      int32_t t$415 = _tmp$1942 / 100;
      int32_t _tmp$1941 = remaining$414;
      int32_t d$416 = _tmp$1941 % 100;
      int32_t _tmp$1931;
      int32_t _tmp$1940;
      int32_t _tmp$1939;
      int32_t d_hi$417;
      int32_t _tmp$1938;
      int32_t _tmp$1937;
      int32_t d_lo$418;
      int32_t _tmp$1933;
      int32_t _tmp$1932;
      int32_t _tmp$1936;
      int32_t _tmp$1935;
      int32_t _tmp$1934;
      remaining$414 = t$415;
      _tmp$1931 = offset$401;
      offset$401 = _tmp$1931 - 2;
      _tmp$1940 = d$416 / 10;
      _tmp$1939 = 48 + _tmp$1940;
      d_hi$417 = (uint16_t)_tmp$1939;
      _tmp$1938 = d$416 % 10;
      _tmp$1937 = 48 + _tmp$1938;
      d_lo$418 = (uint16_t)_tmp$1937;
      _tmp$1933 = offset$401;
      _tmp$1932 = digit_start$403 + _tmp$1933;
      buffer$412[_tmp$1932] = d_hi$417;
      _tmp$1936 = offset$401;
      _tmp$1935 = digit_start$403 + _tmp$1936;
      _tmp$1934 = _tmp$1935 + 1;
      buffer$412[_tmp$1934] = d_lo$418;
      continue;
    }
    break;
  }
  _tmp$1943 = remaining$414;
  if (_tmp$1943 >= 10) {
    int32_t _tmp$1944 = offset$401;
    int32_t _tmp$1955;
    int32_t _tmp$1954;
    int32_t _tmp$1953;
    int32_t d_hi$420;
    int32_t _tmp$1952;
    int32_t _tmp$1951;
    int32_t _tmp$1950;
    int32_t d_lo$421;
    int32_t _tmp$1946;
    int32_t _tmp$1945;
    int32_t _tmp$1949;
    int32_t _tmp$1948;
    int32_t _tmp$1947;
    offset$401 = _tmp$1944 - 2;
    _tmp$1955 = remaining$414;
    _tmp$1954 = _tmp$1955 / 10;
    _tmp$1953 = 48 + _tmp$1954;
    d_hi$420 = (uint16_t)_tmp$1953;
    _tmp$1952 = remaining$414;
    _tmp$1951 = _tmp$1952 % 10;
    _tmp$1950 = 48 + _tmp$1951;
    d_lo$421 = (uint16_t)_tmp$1950;
    _tmp$1946 = offset$401;
    _tmp$1945 = digit_start$403 + _tmp$1946;
    buffer$412[_tmp$1945] = d_hi$420;
    _tmp$1949 = offset$401;
    _tmp$1948 = digit_start$403 + _tmp$1949;
    _tmp$1947 = _tmp$1948 + 1;
    buffer$412[_tmp$1947] = d_lo$421;
    moonbit_decref(buffer$412);
  } else {
    int32_t _tmp$1956 = offset$401;
    int32_t _tmp$1961;
    int32_t _tmp$1957;
    int32_t _tmp$1960;
    int32_t _tmp$1959;
    int32_t _tmp$1958;
    offset$401 = _tmp$1956 - 1;
    _tmp$1961 = offset$401;
    _tmp$1957 = digit_start$403 + _tmp$1961;
    _tmp$1960 = remaining$414;
    _tmp$1959 = 48 + _tmp$1960;
    _tmp$1958 = (uint16_t)_tmp$1959;
    buffer$412[_tmp$1957] = _tmp$1958;
    moonbit_decref(buffer$412);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$394,
  uint64_t num$388,
  int32_t digit_start$386,
  int32_t total_len$385,
  int32_t radix$390
) {
  int32_t offset$384 = total_len$385 - digit_start$386;
  uint64_t n$387 = num$388;
  uint64_t base$389 = $Int$$to_uint64(radix$390);
  int32_t _tmp$1886 = radix$390 - 1;
  int32_t _tmp$1885 = radix$390 & _tmp$1886;
  if (_tmp$1885 == 0) {
    int32_t shift$391 = moonbit_ctz32(radix$390);
    uint64_t mask$392 = base$389 - 1ull;
    while (1) {
      uint64_t _tmp$1887 = n$387;
      if (_tmp$1887 > 0ull) {
        int32_t _tmp$1888 = offset$384;
        uint64_t _tmp$1895;
        uint64_t _tmp$1894;
        int32_t digit$393;
        int32_t _tmp$1892;
        int32_t _tmp$1889;
        int32_t _tmp$1891;
        int32_t _tmp$1890;
        uint64_t _tmp$1893;
        offset$384 = _tmp$1888 - 1;
        _tmp$1895 = n$387;
        _tmp$1894 = _tmp$1895 & mask$392;
        digit$393 = (int32_t)_tmp$1894;
        _tmp$1892 = offset$384;
        _tmp$1889 = digit_start$386 + _tmp$1892;
        _tmp$1891
        = ((moonbit_string_t)moonbit_string_literal_145.data)[
          digit$393
        ];
        _tmp$1890 = (uint16_t)_tmp$1891;
        buffer$394[_tmp$1889] = _tmp$1890;
        _tmp$1893 = n$387;
        n$387 = _tmp$1893 >> (shift$391 & 63);
        continue;
      } else {
        moonbit_decref(buffer$394);
      }
      break;
    }
  } else {
    while (1) {
      uint64_t _tmp$1896 = n$387;
      if (_tmp$1896 > 0ull) {
        int32_t _tmp$1897 = offset$384;
        uint64_t _tmp$1905;
        uint64_t q$396;
        uint64_t _tmp$1903;
        uint64_t _tmp$1904;
        uint64_t _tmp$1902;
        int32_t digit$397;
        int32_t _tmp$1901;
        int32_t _tmp$1898;
        int32_t _tmp$1900;
        int32_t _tmp$1899;
        offset$384 = _tmp$1897 - 1;
        _tmp$1905 = n$387;
        q$396 = _tmp$1905 / base$389;
        _tmp$1903 = n$387;
        _tmp$1904 = q$396 * base$389;
        _tmp$1902 = _tmp$1903 - _tmp$1904;
        digit$397 = (int32_t)_tmp$1902;
        _tmp$1901 = offset$384;
        _tmp$1898 = digit_start$386 + _tmp$1901;
        _tmp$1900
        = ((moonbit_string_t)moonbit_string_literal_145.data)[
          digit$397
        ];
        _tmp$1899 = (uint16_t)_tmp$1900;
        buffer$394[_tmp$1898] = _tmp$1899;
        n$387 = q$396;
        continue;
      } else {
        moonbit_decref(buffer$394);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$381,
  uint64_t num$377,
  int32_t digit_start$375,
  int32_t total_len$374
) {
  int32_t offset$373 = total_len$374 - digit_start$375;
  uint64_t n$376 = num$377;
  int32_t _tmp$1880;
  while (1) {
    int32_t _tmp$1866 = offset$373;
    if (_tmp$1866 >= 2) {
      int32_t _tmp$1867 = offset$373;
      uint64_t _tmp$1879;
      uint64_t _tmp$1878;
      int32_t byte_val$378;
      int32_t hi$379;
      int32_t lo$380;
      int32_t _tmp$1871;
      int32_t _tmp$1868;
      int32_t _tmp$1870;
      int32_t _tmp$1869;
      int32_t _tmp$1876;
      int32_t _tmp$1875;
      int32_t _tmp$1872;
      int32_t _tmp$1874;
      int32_t _tmp$1873;
      uint64_t _tmp$1877;
      offset$373 = _tmp$1867 - 2;
      _tmp$1879 = n$376;
      _tmp$1878 = _tmp$1879 & 255ull;
      byte_val$378 = (int32_t)_tmp$1878;
      hi$379 = byte_val$378 / 16;
      lo$380 = byte_val$378 % 16;
      _tmp$1871 = offset$373;
      _tmp$1868 = digit_start$375 + _tmp$1871;
      _tmp$1870 = ((moonbit_string_t)moonbit_string_literal_145.data)[hi$379];
      _tmp$1869 = (uint16_t)_tmp$1870;
      buffer$381[_tmp$1868] = _tmp$1869;
      _tmp$1876 = offset$373;
      _tmp$1875 = digit_start$375 + _tmp$1876;
      _tmp$1872 = _tmp$1875 + 1;
      _tmp$1874 = ((moonbit_string_t)moonbit_string_literal_145.data)[lo$380];
      _tmp$1873 = (uint16_t)_tmp$1874;
      buffer$381[_tmp$1872] = _tmp$1873;
      _tmp$1877 = n$376;
      n$376 = _tmp$1877 >> 8;
      continue;
    }
    break;
  }
  _tmp$1880 = offset$373;
  if (_tmp$1880 == 1) {
    uint64_t _tmp$1884 = n$376;
    uint64_t _tmp$1883 = _tmp$1884 & 15ull;
    int32_t nibble$383 = (int32_t)_tmp$1883;
    int32_t _tmp$1882 =
      ((moonbit_string_t)moonbit_string_literal_145.data)[nibble$383];
    int32_t _tmp$1881 = (uint16_t)_tmp$1882;
    buffer$381[digit_start$375] = _tmp$1881;
    moonbit_decref(buffer$381);
  } else {
    moonbit_decref(buffer$381);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$367,
  int32_t radix$370
) {
  uint64_t num$368;
  uint64_t base$369;
  int32_t count$371;
  if (value$367 == 0ull) {
    return 1;
  }
  num$368 = value$367;
  base$369 = $Int$$to_uint64(radix$370);
  count$371 = 0;
  while (1) {
    uint64_t _tmp$1863 = num$368;
    if (_tmp$1863 > 0ull) {
      int32_t _tmp$1864 = count$371;
      uint64_t _tmp$1865;
      count$371 = _tmp$1864 + 1;
      _tmp$1865 = num$368;
      num$368 = _tmp$1865 / base$369;
      continue;
    }
    break;
  }
  return count$371;
}

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$365) {
  if (value$365 == 0ull) {
    return 1;
  } else {
    int32_t leading_zeros$366 = moonbit_clz64(value$365);
    int32_t _tmp$1862 = 63 - leading_zeros$366;
    int32_t _tmp$1861 = _tmp$1862 / 4;
    return _tmp$1861 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$364) {
  if (value$364 >= 10000000000ull) {
    if (value$364 >= 100000000000000ull) {
      if (value$364 >= 10000000000000000ull) {
        if (value$364 >= 1000000000000000000ull) {
          if (value$364 >= 10000000000000000000ull) {
            return 20;
          } else {
            return 19;
          }
        } else if (value$364 >= 100000000000000000ull) {
          return 18;
        } else {
          return 17;
        }
      } else if (value$364 >= 1000000000000000ull) {
        return 16;
      } else {
        return 15;
      }
    } else if (value$364 >= 1000000000000ull) {
      if (value$364 >= 10000000000000ull) {
        return 14;
      } else {
        return 13;
      }
    } else if (value$364 >= 100000000000ull) {
      return 12;
    } else {
      return 11;
    }
  } else if (value$364 >= 100000ull) {
    if (value$364 >= 10000000ull) {
      if (value$364 >= 1000000000ull) {
        return 10;
      } else if (value$364 >= 100000000ull) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$364 >= 1000000ull) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$364 >= 1000ull) {
    if (value$364 >= 10000ull) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$364 >= 100ull) {
    return 3;
  } else if (value$364 >= 10ull) {
    return 2;
  } else {
    return 1;
  }
}

moonbit_string_t $Int$$to_string$inner(int32_t self$348, int32_t radix$347) {
  int32_t _if_result$3732;
  int32_t is_negative$349;
  uint32_t num$350;
  uint16_t* buffer$351;
  if (radix$347 < 2) {
    _if_result$3732 = 1;
  } else {
    _if_result$3732 = radix$347 > 36;
  }
  if (_if_result$3732) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_142.data,
        (moonbit_string_t)moonbit_string_literal_146.data
    );
  }
  if (self$348 == 0) {
    return (moonbit_string_t)moonbit_string_literal_144.data;
  }
  is_negative$349 = self$348 < 0;
  if (is_negative$349) {
    int32_t _tmp$1860 = -self$348;
    num$350 = *(uint32_t*)&_tmp$1860;
  } else {
    num$350 = *(uint32_t*)&self$348;
  }
  switch (radix$347) {
    case 10: {
      int32_t digit_len$352 = $moonbitlang$core$builtin$dec_count32(num$350);
      int32_t _tmp$1857;
      int32_t total_len$353;
      uint16_t* buffer$354;
      int32_t digit_start$355;
      if (is_negative$349) {
        _tmp$1857 = 1;
      } else {
        _tmp$1857 = 0;
      }
      total_len$353 = digit_len$352 + _tmp$1857;
      buffer$354 = (uint16_t*)moonbit_make_string(total_len$353, 0);
      if (is_negative$349) {
        digit_start$355 = 1;
      } else {
        digit_start$355 = 0;
      }
      moonbit_incref(buffer$354);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$354, num$350, digit_start$355, total_len$353
      );
      buffer$351 = buffer$354;
      break;
    }
    
    case 16: {
      int32_t digit_len$356 = $moonbitlang$core$builtin$hex_count32(num$350);
      int32_t _tmp$1858;
      int32_t total_len$357;
      uint16_t* buffer$358;
      int32_t digit_start$359;
      if (is_negative$349) {
        _tmp$1858 = 1;
      } else {
        _tmp$1858 = 0;
      }
      total_len$357 = digit_len$356 + _tmp$1858;
      buffer$358 = (uint16_t*)moonbit_make_string(total_len$357, 0);
      if (is_negative$349) {
        digit_start$359 = 1;
      } else {
        digit_start$359 = 0;
      }
      moonbit_incref(buffer$358);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$358, num$350, digit_start$359, total_len$357
      );
      buffer$351 = buffer$358;
      break;
    }
    default: {
      int32_t digit_len$360 =
        $moonbitlang$core$builtin$radix_count32(num$350, radix$347);
      int32_t _tmp$1859;
      int32_t total_len$361;
      uint16_t* buffer$362;
      int32_t digit_start$363;
      if (is_negative$349) {
        _tmp$1859 = 1;
      } else {
        _tmp$1859 = 0;
      }
      total_len$361 = digit_len$360 + _tmp$1859;
      buffer$362 = (uint16_t*)moonbit_make_string(total_len$361, 0);
      if (is_negative$349) {
        digit_start$363 = 1;
      } else {
        digit_start$363 = 0;
      }
      moonbit_incref(buffer$362);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$362, num$350, digit_start$363, total_len$361, radix$347
      );
      buffer$351 = buffer$362;
      break;
    }
  }
  if (is_negative$349) {
    buffer$351[0] = 45;
  }
  return buffer$351;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$341,
  int32_t radix$344
) {
  uint32_t num$342;
  uint32_t base$343;
  int32_t count$345;
  if (value$341 == 0u) {
    return 1;
  }
  num$342 = value$341;
  base$343 = *(uint32_t*)&radix$344;
  count$345 = 0;
  while (1) {
    uint32_t _tmp$1854 = num$342;
    if (_tmp$1854 > 0u) {
      int32_t _tmp$1855 = count$345;
      uint32_t _tmp$1856;
      count$345 = _tmp$1855 + 1;
      _tmp$1856 = num$342;
      num$342 = _tmp$1856 / base$343;
      continue;
    }
    break;
  }
  return count$345;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$339) {
  if (value$339 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$340 = moonbit_clz32(value$339);
    int32_t _tmp$1853 = 31 - leading_zeros$340;
    int32_t _tmp$1852 = _tmp$1853 / 4;
    return _tmp$1852 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$338) {
  if (value$338 >= 100000u) {
    if (value$338 >= 10000000u) {
      if (value$338 >= 1000000000u) {
        return 10;
      } else if (value$338 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$338 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$338 >= 1000u) {
    if (value$338 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$338 >= 100u) {
    return 3;
  } else if (value$338 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$328,
  uint32_t num$316,
  int32_t digit_start$319,
  int32_t total_len$318
) {
  uint32_t num$315 = num$316;
  int32_t offset$317 = total_len$318 - digit_start$319;
  uint32_t _tmp$1851;
  int32_t remaining$330;
  int32_t _tmp$1832;
  while (1) {
    uint32_t _tmp$1795 = num$315;
    if (_tmp$1795 >= 10000u) {
      uint32_t _tmp$1818 = num$315;
      uint32_t t$320 = _tmp$1818 / 10000u;
      uint32_t _tmp$1817 = num$315;
      uint32_t _tmp$1816 = _tmp$1817 % 10000u;
      int32_t r$321 = *(int32_t*)&_tmp$1816;
      int32_t d1$322;
      int32_t d2$323;
      int32_t _tmp$1796;
      int32_t _tmp$1815;
      int32_t _tmp$1814;
      int32_t d1_hi$324;
      int32_t _tmp$1813;
      int32_t _tmp$1812;
      int32_t d1_lo$325;
      int32_t _tmp$1811;
      int32_t _tmp$1810;
      int32_t d2_hi$326;
      int32_t _tmp$1809;
      int32_t _tmp$1808;
      int32_t d2_lo$327;
      int32_t _tmp$1798;
      int32_t _tmp$1797;
      int32_t _tmp$1801;
      int32_t _tmp$1800;
      int32_t _tmp$1799;
      int32_t _tmp$1804;
      int32_t _tmp$1803;
      int32_t _tmp$1802;
      int32_t _tmp$1807;
      int32_t _tmp$1806;
      int32_t _tmp$1805;
      num$315 = t$320;
      d1$322 = r$321 / 100;
      d2$323 = r$321 % 100;
      _tmp$1796 = offset$317;
      offset$317 = _tmp$1796 - 4;
      _tmp$1815 = d1$322 / 10;
      _tmp$1814 = 48 + _tmp$1815;
      d1_hi$324 = (uint16_t)_tmp$1814;
      _tmp$1813 = d1$322 % 10;
      _tmp$1812 = 48 + _tmp$1813;
      d1_lo$325 = (uint16_t)_tmp$1812;
      _tmp$1811 = d2$323 / 10;
      _tmp$1810 = 48 + _tmp$1811;
      d2_hi$326 = (uint16_t)_tmp$1810;
      _tmp$1809 = d2$323 % 10;
      _tmp$1808 = 48 + _tmp$1809;
      d2_lo$327 = (uint16_t)_tmp$1808;
      _tmp$1798 = offset$317;
      _tmp$1797 = digit_start$319 + _tmp$1798;
      buffer$328[_tmp$1797] = d1_hi$324;
      _tmp$1801 = offset$317;
      _tmp$1800 = digit_start$319 + _tmp$1801;
      _tmp$1799 = _tmp$1800 + 1;
      buffer$328[_tmp$1799] = d1_lo$325;
      _tmp$1804 = offset$317;
      _tmp$1803 = digit_start$319 + _tmp$1804;
      _tmp$1802 = _tmp$1803 + 2;
      buffer$328[_tmp$1802] = d2_hi$326;
      _tmp$1807 = offset$317;
      _tmp$1806 = digit_start$319 + _tmp$1807;
      _tmp$1805 = _tmp$1806 + 3;
      buffer$328[_tmp$1805] = d2_lo$327;
      continue;
    }
    break;
  }
  _tmp$1851 = num$315;
  remaining$330 = *(int32_t*)&_tmp$1851;
  while (1) {
    int32_t _tmp$1819 = remaining$330;
    if (_tmp$1819 >= 100) {
      int32_t _tmp$1831 = remaining$330;
      int32_t t$331 = _tmp$1831 / 100;
      int32_t _tmp$1830 = remaining$330;
      int32_t d$332 = _tmp$1830 % 100;
      int32_t _tmp$1820;
      int32_t _tmp$1829;
      int32_t _tmp$1828;
      int32_t d_hi$333;
      int32_t _tmp$1827;
      int32_t _tmp$1826;
      int32_t d_lo$334;
      int32_t _tmp$1822;
      int32_t _tmp$1821;
      int32_t _tmp$1825;
      int32_t _tmp$1824;
      int32_t _tmp$1823;
      remaining$330 = t$331;
      _tmp$1820 = offset$317;
      offset$317 = _tmp$1820 - 2;
      _tmp$1829 = d$332 / 10;
      _tmp$1828 = 48 + _tmp$1829;
      d_hi$333 = (uint16_t)_tmp$1828;
      _tmp$1827 = d$332 % 10;
      _tmp$1826 = 48 + _tmp$1827;
      d_lo$334 = (uint16_t)_tmp$1826;
      _tmp$1822 = offset$317;
      _tmp$1821 = digit_start$319 + _tmp$1822;
      buffer$328[_tmp$1821] = d_hi$333;
      _tmp$1825 = offset$317;
      _tmp$1824 = digit_start$319 + _tmp$1825;
      _tmp$1823 = _tmp$1824 + 1;
      buffer$328[_tmp$1823] = d_lo$334;
      continue;
    }
    break;
  }
  _tmp$1832 = remaining$330;
  if (_tmp$1832 >= 10) {
    int32_t _tmp$1833 = offset$317;
    int32_t _tmp$1844;
    int32_t _tmp$1843;
    int32_t _tmp$1842;
    int32_t d_hi$336;
    int32_t _tmp$1841;
    int32_t _tmp$1840;
    int32_t _tmp$1839;
    int32_t d_lo$337;
    int32_t _tmp$1835;
    int32_t _tmp$1834;
    int32_t _tmp$1838;
    int32_t _tmp$1837;
    int32_t _tmp$1836;
    offset$317 = _tmp$1833 - 2;
    _tmp$1844 = remaining$330;
    _tmp$1843 = _tmp$1844 / 10;
    _tmp$1842 = 48 + _tmp$1843;
    d_hi$336 = (uint16_t)_tmp$1842;
    _tmp$1841 = remaining$330;
    _tmp$1840 = _tmp$1841 % 10;
    _tmp$1839 = 48 + _tmp$1840;
    d_lo$337 = (uint16_t)_tmp$1839;
    _tmp$1835 = offset$317;
    _tmp$1834 = digit_start$319 + _tmp$1835;
    buffer$328[_tmp$1834] = d_hi$336;
    _tmp$1838 = offset$317;
    _tmp$1837 = digit_start$319 + _tmp$1838;
    _tmp$1836 = _tmp$1837 + 1;
    buffer$328[_tmp$1836] = d_lo$337;
    moonbit_decref(buffer$328);
  } else {
    int32_t _tmp$1845 = offset$317;
    int32_t _tmp$1850;
    int32_t _tmp$1846;
    int32_t _tmp$1849;
    int32_t _tmp$1848;
    int32_t _tmp$1847;
    offset$317 = _tmp$1845 - 1;
    _tmp$1850 = offset$317;
    _tmp$1846 = digit_start$319 + _tmp$1850;
    _tmp$1849 = remaining$330;
    _tmp$1848 = 48 + _tmp$1849;
    _tmp$1847 = (uint16_t)_tmp$1848;
    buffer$328[_tmp$1846] = _tmp$1847;
    moonbit_decref(buffer$328);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$310,
  uint32_t num$304,
  int32_t digit_start$302,
  int32_t total_len$301,
  int32_t radix$306
) {
  int32_t offset$300 = total_len$301 - digit_start$302;
  uint32_t n$303 = num$304;
  uint32_t base$305 = *(uint32_t*)&radix$306;
  int32_t _tmp$1775 = radix$306 - 1;
  int32_t _tmp$1774 = radix$306 & _tmp$1775;
  if (_tmp$1774 == 0) {
    int32_t shift$307 = moonbit_ctz32(radix$306);
    uint32_t mask$308 = base$305 - 1u;
    while (1) {
      uint32_t _tmp$1776 = n$303;
      if (_tmp$1776 > 0u) {
        int32_t _tmp$1777 = offset$300;
        uint32_t _tmp$1784;
        uint32_t _tmp$1783;
        int32_t digit$309;
        int32_t _tmp$1781;
        int32_t _tmp$1778;
        int32_t _tmp$1780;
        int32_t _tmp$1779;
        uint32_t _tmp$1782;
        offset$300 = _tmp$1777 - 1;
        _tmp$1784 = n$303;
        _tmp$1783 = _tmp$1784 & mask$308;
        digit$309 = *(int32_t*)&_tmp$1783;
        _tmp$1781 = offset$300;
        _tmp$1778 = digit_start$302 + _tmp$1781;
        _tmp$1780
        = ((moonbit_string_t)moonbit_string_literal_145.data)[
          digit$309
        ];
        _tmp$1779 = (uint16_t)_tmp$1780;
        buffer$310[_tmp$1778] = _tmp$1779;
        _tmp$1782 = n$303;
        n$303 = _tmp$1782 >> (shift$307 & 31);
        continue;
      } else {
        moonbit_decref(buffer$310);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1785 = n$303;
      if (_tmp$1785 > 0u) {
        int32_t _tmp$1786 = offset$300;
        uint32_t _tmp$1794;
        uint32_t q$312;
        uint32_t _tmp$1792;
        uint32_t _tmp$1793;
        uint32_t _tmp$1791;
        int32_t digit$313;
        int32_t _tmp$1790;
        int32_t _tmp$1787;
        int32_t _tmp$1789;
        int32_t _tmp$1788;
        offset$300 = _tmp$1786 - 1;
        _tmp$1794 = n$303;
        q$312 = _tmp$1794 / base$305;
        _tmp$1792 = n$303;
        _tmp$1793 = q$312 * base$305;
        _tmp$1791 = _tmp$1792 - _tmp$1793;
        digit$313 = *(int32_t*)&_tmp$1791;
        _tmp$1790 = offset$300;
        _tmp$1787 = digit_start$302 + _tmp$1790;
        _tmp$1789
        = ((moonbit_string_t)moonbit_string_literal_145.data)[
          digit$313
        ];
        _tmp$1788 = (uint16_t)_tmp$1789;
        buffer$310[_tmp$1787] = _tmp$1788;
        n$303 = q$312;
        continue;
      } else {
        moonbit_decref(buffer$310);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$297,
  uint32_t num$293,
  int32_t digit_start$291,
  int32_t total_len$290
) {
  int32_t offset$289 = total_len$290 - digit_start$291;
  uint32_t n$292 = num$293;
  int32_t _tmp$1769;
  while (1) {
    int32_t _tmp$1755 = offset$289;
    if (_tmp$1755 >= 2) {
      int32_t _tmp$1756 = offset$289;
      uint32_t _tmp$1768;
      uint32_t _tmp$1767;
      int32_t byte_val$294;
      int32_t hi$295;
      int32_t lo$296;
      int32_t _tmp$1760;
      int32_t _tmp$1757;
      int32_t _tmp$1759;
      int32_t _tmp$1758;
      int32_t _tmp$1765;
      int32_t _tmp$1764;
      int32_t _tmp$1761;
      int32_t _tmp$1763;
      int32_t _tmp$1762;
      uint32_t _tmp$1766;
      offset$289 = _tmp$1756 - 2;
      _tmp$1768 = n$292;
      _tmp$1767 = _tmp$1768 & 255u;
      byte_val$294 = *(int32_t*)&_tmp$1767;
      hi$295 = byte_val$294 / 16;
      lo$296 = byte_val$294 % 16;
      _tmp$1760 = offset$289;
      _tmp$1757 = digit_start$291 + _tmp$1760;
      _tmp$1759 = ((moonbit_string_t)moonbit_string_literal_145.data)[hi$295];
      _tmp$1758 = (uint16_t)_tmp$1759;
      buffer$297[_tmp$1757] = _tmp$1758;
      _tmp$1765 = offset$289;
      _tmp$1764 = digit_start$291 + _tmp$1765;
      _tmp$1761 = _tmp$1764 + 1;
      _tmp$1763 = ((moonbit_string_t)moonbit_string_literal_145.data)[lo$296];
      _tmp$1762 = (uint16_t)_tmp$1763;
      buffer$297[_tmp$1761] = _tmp$1762;
      _tmp$1766 = n$292;
      n$292 = _tmp$1766 >> 8;
      continue;
    }
    break;
  }
  _tmp$1769 = offset$289;
  if (_tmp$1769 == 1) {
    uint32_t _tmp$1773 = n$292;
    uint32_t _tmp$1772 = _tmp$1773 & 15u;
    int32_t nibble$299 = *(int32_t*)&_tmp$1772;
    int32_t _tmp$1771 =
      ((moonbit_string_t)moonbit_string_literal_145.data)[nibble$299];
    int32_t _tmp$1770 = (uint16_t)_tmp$1771;
    buffer$297[digit_start$291] = _tmp$1770;
    moonbit_decref(buffer$297);
  } else {
    moonbit_decref(buffer$297);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$288
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$287 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1754;
  moonbit_incref(logger$287);
  _tmp$1754
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$287
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$288, _tmp$1754
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$287);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  void* self$286
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$285 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1753;
  moonbit_incref(logger$285);
  _tmp$1753
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$285
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$286, _tmp$1753
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$285);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$284
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$283 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1752;
  moonbit_incref(logger$283);
  _tmp$1752
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$283
  };
  $$moonbitlang$core$builtin$Show$$Bool$$output(self$284, _tmp$1752);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$283);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$282
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$281 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1751;
  moonbit_incref(logger$281);
  _tmp$1751
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$281
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$282, _tmp$1751
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$281);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$280
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$279 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1750;
  moonbit_incref(logger$279);
  _tmp$1750
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$279
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$280, _tmp$1750);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$279);
}

int32_t $StringView$$start_offset(struct $StringView self$278) {
  int32_t _field$3255 = self$278.$1;
  moonbit_decref(self$278.$0);
  return _field$3255;
}

moonbit_string_t $StringView$$data(struct $StringView self$277) {
  moonbit_string_t _field$3256 = self$277.$0;
  return _field$3256;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$271,
  moonbit_string_t value$274,
  int32_t start$275,
  int32_t len$276
) {
  void* _try_err$273;
  struct $StringView _tmp$1745;
  int32_t _tmp$1747 = start$275 + len$276;
  int64_t _tmp$1746 = (int64_t)_tmp$1747;
  struct moonbit_result_1 _tmp$3740 =
    $String$$sub$inner(value$274, start$275, _tmp$1746);
  if (_tmp$3740.tag) {
    struct $StringView const _ok$1748 = _tmp$3740.data.ok;
    _tmp$1745 = _ok$1748;
  } else {
    void* const _err$1749 = _tmp$3740.data.err;
    _try_err$273 = _err$1749;
    goto $join$272;
  }
  goto $joinlet$3739;
  $join$272:;
  moonbit_decref(_try_err$273);
  moonbit_panic();
  $joinlet$3739:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$271, _tmp$1745
  );
  return 0;
}

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$264,
  int32_t start$270,
  int64_t end$266
) {
  int32_t len$263 = Moonbit_array_length(self$264);
  int32_t end$265;
  int32_t start$269;
  int32_t _if_result$3741;
  if (end$266 == 4294967296ll) {
    end$265 = len$263;
  } else {
    int64_t _Some$267 = end$266;
    int32_t _end$268 = (int32_t)_Some$267;
    if (_end$268 < 0) {
      end$265 = len$263 + _end$268;
    } else {
      end$265 = _end$268;
    }
  }
  if (start$270 < 0) {
    start$269 = len$263 + start$270;
  } else {
    start$269 = start$270;
  }
  if (start$269 >= 0) {
    if (start$269 <= end$265) {
      _if_result$3741 = end$265 <= len$263;
    } else {
      _if_result$3741 = 0;
    }
  } else {
    _if_result$3741 = 0;
  }
  if (_if_result$3741) {
    int32_t _if_result$3742;
    int32_t _if_result$3744;
    struct $StringView _tmp$1743;
    struct moonbit_result_1 _result$3746;
    if (start$269 < len$263) {
      int32_t _tmp$1739 = self$264[start$269];
      _if_result$3742 = $Int$$is_trailing_surrogate(_tmp$1739);
    } else {
      _if_result$3742 = 0;
    }
    if (_if_result$3742) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1740;
      struct moonbit_result_1 _result$3743;
      moonbit_decref(self$264);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1740
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$3743.tag = 0;
      _result$3743.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1740;
      return _result$3743;
    }
    if (end$265 < len$263) {
      int32_t _tmp$1741 = self$264[end$265];
      _if_result$3744 = $Int$$is_trailing_surrogate(_tmp$1741);
    } else {
      _if_result$3744 = 0;
    }
    if (_if_result$3744) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1742;
      struct moonbit_result_1 _result$3745;
      moonbit_decref(self$264);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1742
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$3745.tag = 0;
      _result$3745.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1742;
      return _result$3745;
    }
    _tmp$1743 = (struct $StringView){start$269, end$265, self$264};
    _result$3746.tag = 1;
    _result$3746.data.ok = _tmp$1743;
    return _result$3746;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1744;
    struct moonbit_result_1 _result$3747;
    moonbit_decref(self$264);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1744
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$3747.tag = 0;
    _result$3747.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1744;
    return _result$3747;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$262
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$261 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1738;
  moonbit_incref(_self$261);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$261, self$262);
  _tmp$1738 = _self$261;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1738);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$260
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$259 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1737;
  moonbit_incref(_self$259);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$259, self$260);
  _tmp$1737 = _self$259;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1737);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$257
) {
  int32_t seed$256;
  if (seed$opt$257 == 4294967296ll) {
    seed$256 = 0;
  } else {
    int64_t _Some$258 = seed$opt$257;
    seed$256 = (int32_t)_Some$258;
  }
  return $$moonbitlang$core$builtin$Hasher$$new$inner(seed$256);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$255
) {
  uint32_t _tmp$1736 = *(uint32_t*)&seed$255;
  uint32_t _tmp$1735 = _tmp$1736 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$3748 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$3748)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$3748->$0 = _tmp$1735;
  return _block$3748;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$254
) {
  uint32_t _tmp$1734 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$254);
  return *(int32_t*)&_tmp$1734;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$253
) {
  uint32_t _field$3257 = self$253->$0;
  uint32_t acc$252;
  uint32_t _tmp$1723;
  uint32_t _tmp$1725;
  uint32_t _tmp$1724;
  uint32_t _tmp$1726;
  uint32_t _tmp$1727;
  uint32_t _tmp$1729;
  uint32_t _tmp$1728;
  uint32_t _tmp$1730;
  uint32_t _tmp$1731;
  uint32_t _tmp$1733;
  uint32_t _tmp$1732;
  moonbit_decref(self$253);
  acc$252 = _field$3257;
  _tmp$1723 = acc$252;
  _tmp$1725 = acc$252;
  _tmp$1724 = _tmp$1725 >> 15;
  acc$252 = _tmp$1723 ^ _tmp$1724;
  _tmp$1726 = acc$252;
  acc$252 = _tmp$1726 * 2246822519u;
  _tmp$1727 = acc$252;
  _tmp$1729 = acc$252;
  _tmp$1728 = _tmp$1729 >> 13;
  acc$252 = _tmp$1727 ^ _tmp$1728;
  _tmp$1730 = acc$252;
  acc$252 = _tmp$1730 * 3266489917u;
  _tmp$1731 = acc$252;
  _tmp$1733 = acc$252;
  _tmp$1732 = _tmp$1733 >> 16;
  acc$252 = _tmp$1731 ^ _tmp$1732;
  return acc$252;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$251,
  int32_t value$250
) {
  $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(value$250, self$251);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$249,
  moonbit_string_t value$248
) {
  $$moonbitlang$core$builtin$Hash$$String$$hash_combine(value$248, self$249);
  return 0;
}

uint64_t $Int$$to_uint64(int32_t self$247) {
  int64_t _tmp$1722 = (int64_t)self$247;
  return *(uint64_t*)&_tmp$1722;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$245,
  int32_t value$246
) {
  uint32_t _tmp$1721 = *(uint32_t*)&value$246;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$245, _tmp$1721);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$243,
  uint32_t value$244
) {
  uint32_t acc$1720 = self$243->$0;
  uint32_t _tmp$1719 = acc$1720 + 4u;
  self$243->$0 = _tmp$1719;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$243, value$244);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$241,
  uint32_t input$242
) {
  uint32_t acc$1717 = self$241->$0;
  uint32_t _tmp$1718 = input$242 * 3266489917u;
  uint32_t _tmp$1716 = acc$1717 + _tmp$1718;
  uint32_t _tmp$1715 = $moonbitlang$core$builtin$rotl(_tmp$1716, 17);
  uint32_t _tmp$1714 = _tmp$1715 * 668265263u;
  self$241->$0 = _tmp$1714;
  moonbit_decref(self$241);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$239, int32_t r$240) {
  uint32_t _tmp$1711 = x$239 << (r$240 & 31);
  int32_t _tmp$1713 = 32 - r$240;
  uint32_t _tmp$1712 = x$239 >> (_tmp$1713 & 31);
  return _tmp$1711 | _tmp$1712;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$237,
  moonbit_string_t str$238
) {
  int32_t len$1701 = self$237->$1;
  int32_t _tmp$1703 = Moonbit_array_length(str$238);
  int32_t _tmp$1702 = _tmp$1703 * 2;
  int32_t _tmp$1700 = len$1701 + _tmp$1702;
  moonbit_bytes_t _field$3259;
  moonbit_bytes_t data$1704;
  int32_t len$1705;
  int32_t _tmp$1706;
  int32_t len$1708;
  int32_t _tmp$3258;
  int32_t _tmp$1710;
  int32_t _tmp$1709;
  int32_t _tmp$1707;
  moonbit_incref(self$237);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$237, _tmp$1700
  );
  _field$3259 = self$237->$0;
  data$1704 = _field$3259;
  len$1705 = self$237->$1;
  _tmp$1706 = Moonbit_array_length(str$238);
  moonbit_incref(data$1704);
  moonbit_incref(str$238);
  $FixedArray$$blit_from_string(data$1704, len$1705, str$238, 0, _tmp$1706);
  len$1708 = self$237->$1;
  _tmp$3258 = Moonbit_array_length(str$238);
  moonbit_decref(str$238);
  _tmp$1710 = _tmp$3258;
  _tmp$1709 = _tmp$1710 * 2;
  _tmp$1707 = len$1708 + _tmp$1709;
  self$237->$1 = _tmp$1707;
  moonbit_decref(self$237);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$229,
  int32_t bytes_offset$224,
  moonbit_string_t str$231,
  int32_t str_offset$227,
  int32_t length$225
) {
  int32_t _tmp$1699 = length$225 * 2;
  int32_t _tmp$1698 = bytes_offset$224 + _tmp$1699;
  int32_t e1$223 = _tmp$1698 - 1;
  int32_t _tmp$1697 = str_offset$227 + length$225;
  int32_t e2$226 = _tmp$1697 - 1;
  int32_t len1$228 = Moonbit_array_length(self$229);
  int32_t len2$230 = Moonbit_array_length(str$231);
  int32_t _if_result$3749;
  if (length$225 >= 0) {
    if (bytes_offset$224 >= 0) {
      if (e1$223 < len1$228) {
        if (str_offset$227 >= 0) {
          _if_result$3749 = e2$226 < len2$230;
        } else {
          _if_result$3749 = 0;
        }
      } else {
        _if_result$3749 = 0;
      }
    } else {
      _if_result$3749 = 0;
    }
  } else {
    _if_result$3749 = 0;
  }
  if (_if_result$3749) {
    int32_t end_str_offset$232 = str_offset$227 + length$225;
    int32_t i$233 = str_offset$227;
    int32_t j$234 = bytes_offset$224;
    while (1) {
      if (i$233 < end_str_offset$232) {
        int32_t _tmp$1694 = str$231[i$233];
        uint32_t c$235 = *(uint32_t*)&_tmp$1694;
        uint32_t _tmp$1690 = c$235 & 255u;
        int32_t _tmp$1689 = $UInt$$to_byte(_tmp$1690);
        int32_t _tmp$1691;
        uint32_t _tmp$1693;
        int32_t _tmp$1692;
        int32_t _tmp$1695;
        int32_t _tmp$1696;
        if (j$234 < 0 || j$234 >= Moonbit_array_length(self$229)) {
          moonbit_panic();
        }
        self$229[j$234] = _tmp$1689;
        _tmp$1691 = j$234 + 1;
        _tmp$1693 = c$235 >> 8;
        _tmp$1692 = $UInt$$to_byte(_tmp$1693);
        if (_tmp$1691 < 0 || _tmp$1691 >= Moonbit_array_length(self$229)) {
          moonbit_panic();
        }
        self$229[_tmp$1691] = _tmp$1692;
        _tmp$1695 = i$233 + 1;
        _tmp$1696 = j$234 + 2;
        i$233 = _tmp$1695;
        j$234 = _tmp$1696;
        continue;
      } else {
        moonbit_decref(str$231);
        moonbit_decref(self$229);
      }
      break;
    }
  } else {
    moonbit_decref(str$231);
    moonbit_decref(self$229);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$191
) {
  int32_t _tmp$1662 = Moonbit_array_length(repr$191);
  int64_t _tmp$1661 = (int64_t)_tmp$1662;
  moonbit_incref(repr$191);
  if ($String$$char_length_ge$inner(repr$191, 1, 0, _tmp$1661)) {
    int32_t _tmp$1688 = repr$191[0];
    int32_t _x$192 = _tmp$1688;
    if (_x$192 == 64) {
      int32_t _tmp$1687 = Moonbit_array_length(repr$191);
      int64_t _tmp$1686 = (int64_t)_tmp$1687;
      int64_t _bind$1382;
      int32_t _tmp$1684;
      int32_t _tmp$1685;
      struct $StringView _x$193;
      int32_t _tmp$1683;
      struct $StringView _tmp$1682;
      int64_t _bind$195;
      moonbit_incref(repr$191);
      _bind$1382
      = $String$$offset_of_nth_char$inner(
        repr$191, 1, 0, _tmp$1686
      );
      if (_bind$1382 == 4294967296ll) {
        _tmp$1684 = Moonbit_array_length(repr$191);
      } else {
        int64_t _Some$194 = _bind$1382;
        _tmp$1684 = (int32_t)_Some$194;
      }
      _tmp$1685 = Moonbit_array_length(repr$191);
      _x$193 = (struct $StringView){_tmp$1684, _tmp$1685, repr$191};
      _tmp$1683
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1682
      = (struct $StringView){
        0, _tmp$1683, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$193.$0);
      _bind$195 = $StringView$$find(_x$193, _tmp$1682);
      if (_bind$195 == 4294967296ll) {
        moonbit_decref(_x$193.$0);
        moonbit_panic();
      } else {
        int64_t _Some$196 = _bind$195;
        int32_t _pkg_end$197 = (int32_t)_Some$196;
        int64_t _tmp$1681 = (int64_t)_pkg_end$197;
        struct $StringView pkg$198;
        int32_t _tmp$1680;
        struct $StringView _tmp$1679;
        int64_t _bind$199;
        moonbit_incref(_x$193.$0);
        pkg$198 = $StringView$$view$inner(_x$193, 0, _tmp$1681);
        _tmp$1680
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1679
        = (struct $StringView){
          0, _tmp$1680, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$193.$0);
        _bind$199 = $StringView$$rev_find(_x$193, _tmp$1679);
        if (_bind$199 == 4294967296ll) {
          moonbit_decref(pkg$198.$0);
          moonbit_decref(_x$193.$0);
          moonbit_panic();
        } else {
          int64_t _Some$200 = _bind$199;
          int32_t _start_loc_end$201 = (int32_t)_Some$200;
          int32_t _tmp$1663 = _start_loc_end$201 + 1;
          int32_t _tmp$1664;
          moonbit_incref(_x$193.$0);
          _tmp$1664 = $StringView$$length(_x$193);
          if (_tmp$1663 < _tmp$1664) {
            int32_t _tmp$1678 = _start_loc_end$201 + 1;
            struct $StringView end_loc$202;
            struct $$3c$StringView$2a$StringView$3e$* _bind$203;
            moonbit_incref(_x$193.$0);
            end_loc$202
            = $StringView$$view$inner(
              _x$193, _tmp$1678, 4294967296ll
            );
            _bind$203
            = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
              end_loc$202
            );
            if (_bind$203 == 0) {
              if (_bind$203) {
                moonbit_decref(_bind$203);
              }
              moonbit_decref(pkg$198.$0);
              moonbit_decref(_x$193.$0);
              moonbit_panic();
            } else {
              struct $$3c$StringView$2a$StringView$3e$* _Some$204 = _bind$203;
              struct $$3c$StringView$2a$StringView$3e$* _x$205 = _Some$204;
              struct $StringView _field$3263 =
                (struct $StringView){
                  _x$205->$0_1, _x$205->$0_2, _x$205->$0_0
                };
              struct $StringView _end_line$206 = _field$3263;
              struct $StringView _field$3262 =
                (struct $StringView){
                  _x$205->$1_1, _x$205->$1_2, _x$205->$1_0
                };
              int32_t _cnt$3500 = Moonbit_object_header(_x$205)->rc;
              struct $StringView _end_column$207;
              int64_t _tmp$1677;
              struct $StringView rest$208;
              int32_t _tmp$1676;
              struct $StringView _tmp$1675;
              int64_t _bind$210;
              if (_cnt$3500 > 1) {
                int32_t _new_cnt$3501;
                moonbit_incref(_field$3262.$0);
                moonbit_incref(_end_line$206.$0);
                _new_cnt$3501 = _cnt$3500 - 1;
                Moonbit_object_header(_x$205)->rc = _new_cnt$3501;
              } else if (_cnt$3500 == 1) {
                moonbit_free(_x$205);
              }
              _end_column$207 = _field$3262;
              _tmp$1677 = (int64_t)_start_loc_end$201;
              rest$208 = $StringView$$view$inner(_x$193, 0, _tmp$1677);
              _tmp$1676
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1675
              = (struct $StringView){
                0,
                  _tmp$1676,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$208.$0);
              _bind$210 = $StringView$$rev_find(rest$208, _tmp$1675);
              if (_bind$210 == 4294967296ll) {
                moonbit_decref(rest$208.$0);
                moonbit_decref(_end_column$207.$0);
                moonbit_decref(_end_line$206.$0);
                moonbit_decref(pkg$198.$0);
                goto $join$209;
              } else {
                int64_t _Some$211 = _bind$210;
                int32_t _start_line_end$212 = (int32_t)_Some$211;
                int64_t _tmp$1674 = (int64_t)_start_line_end$212;
                struct $StringView _tmp$1671;
                int32_t _tmp$1673;
                struct $StringView _tmp$1672;
                int64_t _bind$213;
                moonbit_incref(rest$208.$0);
                _tmp$1671 = $StringView$$view$inner(rest$208, 0, _tmp$1674);
                _tmp$1673
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1672
                = (struct $StringView){
                  0,
                    _tmp$1673,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$213 = $StringView$$rev_find(_tmp$1671, _tmp$1672);
                if (_bind$213 == 4294967296ll) {
                  moonbit_decref(rest$208.$0);
                  moonbit_decref(_end_column$207.$0);
                  moonbit_decref(_end_line$206.$0);
                  moonbit_decref(pkg$198.$0);
                  goto $join$209;
                } else {
                  int64_t _Some$214 = _bind$213;
                  int32_t _filename_end$215 = (int32_t)_Some$214;
                  int32_t _tmp$1665 = _filename_end$215 + 1;
                  int32_t _tmp$1666;
                  moonbit_incref(rest$208.$0);
                  _tmp$1666 = $StringView$$length(rest$208);
                  if (_tmp$1665 < _tmp$1666) {
                    int32_t _tmp$1670 = _filename_end$215 + 1;
                    struct $StringView start_loc$216;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$217;
                    moonbit_incref(rest$208.$0);
                    start_loc$216
                    = $StringView$$view$inner(
                      rest$208, _tmp$1670, 4294967296ll
                    );
                    _bind$217
                    = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
                      start_loc$216
                    );
                    if (_bind$217 == 0) {
                      if (_bind$217) {
                        moonbit_decref(_bind$217);
                      }
                      moonbit_decref(rest$208.$0);
                      moonbit_decref(_end_column$207.$0);
                      moonbit_decref(_end_line$206.$0);
                      moonbit_decref(pkg$198.$0);
                      moonbit_panic();
                    } else {
                      struct $$3c$StringView$2a$StringView$3e$* _Some$218 =
                        _bind$217;
                      struct $$3c$StringView$2a$StringView$3e$* _x$219 =
                        _Some$218;
                      struct $StringView _field$3261 =
                        (struct $StringView){
                          _x$219->$0_1, _x$219->$0_2, _x$219->$0_0
                        };
                      struct $StringView _start_line$220 = _field$3261;
                      struct $StringView _field$3260 =
                        (struct $StringView){
                          _x$219->$1_1, _x$219->$1_2, _x$219->$1_0
                        };
                      int32_t _cnt$3502 = Moonbit_object_header(_x$219)->rc;
                      struct $StringView _start_column$221;
                      int32_t _tmp$1667;
                      if (_cnt$3502 > 1) {
                        int32_t _new_cnt$3503;
                        moonbit_incref(_field$3260.$0);
                        moonbit_incref(_start_line$220.$0);
                        _new_cnt$3503 = _cnt$3502 - 1;
                        Moonbit_object_header(_x$219)->rc = _new_cnt$3503;
                      } else if (_cnt$3502 == 1) {
                        moonbit_free(_x$219);
                      }
                      _start_column$221 = _field$3260;
                      _tmp$1667 = _pkg_end$197 + 1;
                      if (_filename_end$215 > _tmp$1667) {
                        int32_t _tmp$1668 = _pkg_end$197 + 1;
                        int64_t _tmp$1669 = (int64_t)_filename_end$215;
                        struct $StringView filename$222 =
                          $StringView$$view$inner(
                            rest$208, _tmp$1668, _tmp$1669
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$3753 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$3753)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$3753->$0_0 = pkg$198.$0;
                        _block$3753->$0_1 = pkg$198.$1;
                        _block$3753->$0_2 = pkg$198.$2;
                        _block$3753->$1_0 = filename$222.$0;
                        _block$3753->$1_1 = filename$222.$1;
                        _block$3753->$1_2 = filename$222.$2;
                        _block$3753->$2_0 = _start_line$220.$0;
                        _block$3753->$2_1 = _start_line$220.$1;
                        _block$3753->$2_2 = _start_line$220.$2;
                        _block$3753->$3_0 = _start_column$221.$0;
                        _block$3753->$3_1 = _start_column$221.$1;
                        _block$3753->$3_2 = _start_column$221.$2;
                        _block$3753->$4_0 = _end_line$206.$0;
                        _block$3753->$4_1 = _end_line$206.$1;
                        _block$3753->$4_2 = _end_line$206.$2;
                        _block$3753->$5_0 = _end_column$207.$0;
                        _block$3753->$5_1 = _end_column$207.$1;
                        _block$3753->$5_2 = _end_column$207.$2;
                        return _block$3753;
                      } else {
                        moonbit_decref(_start_column$221.$0);
                        moonbit_decref(_start_line$220.$0);
                        moonbit_decref(rest$208.$0);
                        moonbit_decref(_end_column$207.$0);
                        moonbit_decref(_end_line$206.$0);
                        moonbit_decref(pkg$198.$0);
                        moonbit_panic();
                      }
                    }
                  } else {
                    moonbit_decref(rest$208.$0);
                    moonbit_decref(_end_column$207.$0);
                    moonbit_decref(_end_line$206.$0);
                    moonbit_decref(pkg$198.$0);
                    moonbit_panic();
                  }
                }
              }
              $join$209:;
              moonbit_panic();
            }
          } else {
            moonbit_decref(pkg$198.$0);
            moonbit_decref(_x$193.$0);
            moonbit_panic();
          }
        }
      }
    } else {
      moonbit_decref(repr$191);
      goto $join$190;
    }
  } else {
    moonbit_decref(repr$191);
    goto $join$190;
  }
  $join$190:;
  moonbit_panic();
}

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$187
) {
  int32_t _tmp$1660 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1659;
  int64_t _bind$186;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1659
  = (struct $StringView){
    0, _tmp$1660, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$187.$0);
  _bind$186 = $StringView$$find(view$187, _tmp$1659);
  if (_bind$186 == 4294967296ll) {
    moonbit_decref(view$187.$0);
    return 0;
  } else {
    int64_t _Some$188 = _bind$186;
    int32_t _i$189 = (int32_t)_Some$188;
    int32_t _if_result$3754;
    if (_i$189 > 0) {
      int32_t _tmp$1652 = _i$189 + 1;
      int32_t _tmp$1653;
      moonbit_incref(view$187.$0);
      _tmp$1653 = $StringView$$length(view$187);
      _if_result$3754 = _tmp$1652 < _tmp$1653;
    } else {
      _if_result$3754 = 0;
    }
    if (_if_result$3754) {
      int64_t _tmp$1658 = (int64_t)_i$189;
      struct $StringView _tmp$1655;
      int32_t _tmp$1657;
      struct $StringView _tmp$1656;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1654;
      moonbit_incref(view$187.$0);
      _tmp$1655 = $StringView$$view$inner(view$187, 0, _tmp$1658);
      _tmp$1657 = _i$189 + 1;
      _tmp$1656 = $StringView$$view$inner(view$187, _tmp$1657, 4294967296ll);
      _tuple$1654
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1654)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1654->$0_0 = _tmp$1655.$0;
      _tuple$1654->$0_1 = _tmp$1655.$1;
      _tuple$1654->$0_2 = _tmp$1655.$2;
      _tuple$1654->$1_0 = _tmp$1656.$0;
      _tuple$1654->$1_1 = _tmp$1656.$1;
      _tuple$1654->$1_2 = _tmp$1656.$2;
      return _tuple$1654;
    } else {
      moonbit_decref(view$187.$0);
      return 0;
    }
  }
}

struct $StringView $StringView$$view$inner(
  struct $StringView self$184,
  int32_t start_offset$185,
  int64_t end_offset$182
) {
  int32_t end_offset$181;
  int32_t _if_result$3755;
  if (end_offset$182 == 4294967296ll) {
    moonbit_incref(self$184.$0);
    end_offset$181 = $StringView$$length(self$184);
  } else {
    int64_t _Some$183 = end_offset$182;
    end_offset$181 = (int32_t)_Some$183;
  }
  if (start_offset$185 >= 0) {
    if (start_offset$185 <= end_offset$181) {
      int32_t _tmp$1646;
      moonbit_incref(self$184.$0);
      _tmp$1646 = $StringView$$length(self$184);
      _if_result$3755 = end_offset$181 <= _tmp$1646;
    } else {
      _if_result$3755 = 0;
    }
  } else {
    _if_result$3755 = 0;
  }
  if (_if_result$3755) {
    moonbit_string_t _field$3265 = self$184.$0;
    moonbit_string_t str$1647 = _field$3265;
    int32_t start$1651 = self$184.$1;
    int32_t _tmp$1648 = start$1651 + start_offset$185;
    int32_t _field$3264 = self$184.$1;
    int32_t start$1650 = _field$3264;
    int32_t _tmp$1649 = start$1650 + end_offset$181;
    return (struct $StringView){_tmp$1648, _tmp$1649, str$1647};
  } else {
    moonbit_decref(self$184.$0);
    return $moonbitlang$core$builtin$abort$2(
             (moonbit_string_t)moonbit_string_literal_147.data,
               (moonbit_string_t)moonbit_string_literal_148.data
           );
  }
}

int64_t $StringView$$rev_find(
  struct $StringView self$180,
  struct $StringView str$179
) {
  int32_t _tmp$1645;
  moonbit_incref(str$179.$0);
  _tmp$1645 = $StringView$$length(str$179);
  if (_tmp$1645 <= 4) {
    return $moonbitlang$core$builtin$brute_force_rev_find(self$180, str$179);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
             self$180, str$179
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
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
      int32_t i$174;
      moonbit_incref(needle$172.$0);
      needle_first$173 = $StringView$$unsafe_charcode_at(needle$172, 0);
      i$174 = haystack_len$169 - needle_len$171;
      while (1) {
        int32_t _tmp$1632 = i$174;
        if (_tmp$1632 >= 0) {
          int32_t _tmp$1637;
          while (1) {
            int32_t _tmp$1635 = i$174;
            int32_t _if_result$3758;
            if (_tmp$1635 >= 0) {
              int32_t _tmp$1634 = i$174;
              int32_t _tmp$1633;
              moonbit_incref(haystack$170.$0);
              _tmp$1633
              = $StringView$$unsafe_charcode_at(
                haystack$170, _tmp$1634
              );
              _if_result$3758 = _tmp$1633 != needle_first$173;
            } else {
              _if_result$3758 = 0;
            }
            if (_if_result$3758) {
              int32_t _tmp$1636 = i$174;
              i$174 = _tmp$1636 - 1;
              continue;
            }
            break;
          }
          _tmp$1637 = i$174;
          if (_tmp$1637 >= 0) {
            int32_t j$176 = 1;
            int32_t _tmp$1644;
            while (1) {
              if (j$176 < needle_len$171) {
                int32_t _tmp$1641 = i$174;
                int32_t _tmp$1640 = _tmp$1641 + j$176;
                int32_t _tmp$1638;
                int32_t _tmp$1639;
                int32_t _tmp$1642;
                moonbit_incref(haystack$170.$0);
                _tmp$1638
                = $StringView$$unsafe_charcode_at(
                  haystack$170, _tmp$1640
                );
                moonbit_incref(needle$172.$0);
                _tmp$1639
                = $StringView$$unsafe_charcode_at(
                  needle$172, j$176
                );
                if (_tmp$1638 != _tmp$1639) {
                  break;
                }
                _tmp$1642 = j$176 + 1;
                j$176 = _tmp$1642;
                continue;
              } else {
                int32_t _tmp$1643;
                moonbit_decref(needle$172.$0);
                moonbit_decref(haystack$170.$0);
                _tmp$1643 = i$174;
                return (int64_t)_tmp$1643;
              }
              break;
            }
            _tmp$1644 = i$174;
            i$174 = _tmp$1644 - 1;
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
    return (int64_t)haystack_len$169;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$159,
  struct $StringView needle$161
) {
  int32_t haystack_len$158;
  int32_t needle_len$160;
  moonbit_incref(haystack$159.$0);
  haystack_len$158 = $StringView$$length(haystack$159);
  moonbit_incref(needle$161.$0);
  needle_len$160 = $StringView$$length(needle$161);
  if (needle_len$160 > 0) {
    if (haystack_len$158 >= needle_len$160) {
      int32_t* skip_table$162 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$160);
      int32_t _tmp$1622 = needle_len$160 - 1;
      int32_t i$163 = _tmp$1622;
      int32_t _tmp$1631;
      int32_t i$165;
      while (1) {
        if (i$163 > 0) {
          int32_t _tmp$1620;
          int32_t _tmp$1619;
          int32_t _tmp$1621;
          moonbit_incref(needle$161.$0);
          _tmp$1620 = $StringView$$unsafe_charcode_at(needle$161, i$163);
          _tmp$1619 = _tmp$1620 & 255;
          if (
            _tmp$1619 < 0
            || _tmp$1619 >= Moonbit_array_length(skip_table$162)
          ) {
            moonbit_panic();
          }
          skip_table$162[_tmp$1619] = i$163;
          _tmp$1621 = i$163 - 1;
          i$163 = _tmp$1621;
          continue;
        }
        break;
      }
      _tmp$1631 = haystack_len$158 - needle_len$160;
      i$165 = _tmp$1631;
      while (1) {
        if (i$165 >= 0) {
          int32_t j$166 = 0;
          int32_t _tmp$1630;
          int32_t _tmp$1629;
          int32_t _tmp$1628;
          int32_t _tmp$1627;
          while (1) {
            if (j$166 < needle_len$160) {
              int32_t _tmp$1625 = i$165 + j$166;
              int32_t _tmp$1623;
              int32_t _tmp$1624;
              int32_t _tmp$1626;
              moonbit_incref(haystack$159.$0);
              _tmp$1623
              = $StringView$$unsafe_charcode_at(
                haystack$159, _tmp$1625
              );
              moonbit_incref(needle$161.$0);
              _tmp$1624 = $StringView$$unsafe_charcode_at(needle$161, j$166);
              if (_tmp$1623 != _tmp$1624) {
                break;
              }
              _tmp$1626 = j$166 + 1;
              j$166 = _tmp$1626;
              continue;
            } else {
              moonbit_decref(skip_table$162);
              moonbit_decref(needle$161.$0);
              moonbit_decref(haystack$159.$0);
              return (int64_t)i$165;
            }
            break;
          }
          moonbit_incref(haystack$159.$0);
          _tmp$1630 = $StringView$$unsafe_charcode_at(haystack$159, i$165);
          _tmp$1629 = _tmp$1630 & 255;
          if (
            _tmp$1629 < 0
            || _tmp$1629 >= Moonbit_array_length(skip_table$162)
          ) {
            moonbit_panic();
          }
          _tmp$1628 = (int32_t)skip_table$162[_tmp$1629];
          _tmp$1627 = i$165 - _tmp$1628;
          i$165 = _tmp$1627;
          continue;
        } else {
          moonbit_decref(skip_table$162);
          moonbit_decref(needle$161.$0);
          moonbit_decref(haystack$159.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$161.$0);
      moonbit_decref(haystack$159.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$161.$0);
    moonbit_decref(haystack$159.$0);
    return (int64_t)haystack_len$158;
  }
}

int64_t $StringView$$find(
  struct $StringView self$157,
  struct $StringView str$156
) {
  int32_t _tmp$1618;
  moonbit_incref(str$156.$0);
  _tmp$1618 = $StringView$$length(str$156);
  if (_tmp$1618 <= 4) {
    return $moonbitlang$core$builtin$brute_force_find(self$157, str$156);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_find(
             self$157, str$156
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$146,
  struct $StringView needle$148
) {
  int32_t haystack_len$145;
  int32_t needle_len$147;
  moonbit_incref(haystack$146.$0);
  haystack_len$145 = $StringView$$length(haystack$146);
  moonbit_incref(needle$148.$0);
  needle_len$147 = $StringView$$length(needle$148);
  if (needle_len$147 > 0) {
    if (haystack_len$145 >= needle_len$147) {
      int32_t needle_first$149;
      int32_t forward_len$150;
      int32_t i$151;
      moonbit_incref(needle$148.$0);
      needle_first$149 = $StringView$$unsafe_charcode_at(needle$148, 0);
      forward_len$150 = haystack_len$145 - needle_len$147;
      i$151 = 0;
      while (1) {
        int32_t _tmp$1605 = i$151;
        if (_tmp$1605 <= forward_len$150) {
          int32_t _tmp$1610;
          while (1) {
            int32_t _tmp$1608 = i$151;
            int32_t _if_result$3765;
            if (_tmp$1608 <= forward_len$150) {
              int32_t _tmp$1607 = i$151;
              int32_t _tmp$1606;
              moonbit_incref(haystack$146.$0);
              _tmp$1606
              = $StringView$$unsafe_charcode_at(
                haystack$146, _tmp$1607
              );
              _if_result$3765 = _tmp$1606 != needle_first$149;
            } else {
              _if_result$3765 = 0;
            }
            if (_if_result$3765) {
              int32_t _tmp$1609 = i$151;
              i$151 = _tmp$1609 + 1;
              continue;
            }
            break;
          }
          _tmp$1610 = i$151;
          if (_tmp$1610 <= forward_len$150) {
            int32_t j$153 = 1;
            int32_t _tmp$1617;
            while (1) {
              if (j$153 < needle_len$147) {
                int32_t _tmp$1614 = i$151;
                int32_t _tmp$1613 = _tmp$1614 + j$153;
                int32_t _tmp$1611;
                int32_t _tmp$1612;
                int32_t _tmp$1615;
                moonbit_incref(haystack$146.$0);
                _tmp$1611
                = $StringView$$unsafe_charcode_at(
                  haystack$146, _tmp$1613
                );
                moonbit_incref(needle$148.$0);
                _tmp$1612
                = $StringView$$unsafe_charcode_at(
                  needle$148, j$153
                );
                if (_tmp$1611 != _tmp$1612) {
                  break;
                }
                _tmp$1615 = j$153 + 1;
                j$153 = _tmp$1615;
                continue;
              } else {
                int32_t _tmp$1616;
                moonbit_decref(needle$148.$0);
                moonbit_decref(haystack$146.$0);
                _tmp$1616 = i$151;
                return (int64_t)_tmp$1616;
              }
              break;
            }
            _tmp$1617 = i$151;
            i$151 = _tmp$1617 + 1;
          }
          continue;
        } else {
          moonbit_decref(needle$148.$0);
          moonbit_decref(haystack$146.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$148.$0);
      moonbit_decref(haystack$146.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$148.$0);
    moonbit_decref(haystack$146.$0);
    return $moonbitlang$core$builtin$brute_force_find$constr$144;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$132,
  struct $StringView needle$134
) {
  int32_t haystack_len$131;
  int32_t needle_len$133;
  moonbit_incref(haystack$132.$0);
  haystack_len$131 = $StringView$$length(haystack$132);
  moonbit_incref(needle$134.$0);
  needle_len$133 = $StringView$$length(needle$134);
  if (needle_len$133 > 0) {
    if (haystack_len$131 >= needle_len$133) {
      int32_t* skip_table$135 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$133);
      int32_t _end4301$136 = needle_len$133 - 1;
      int32_t i$137 = 0;
      int32_t i$139;
      while (1) {
        if (i$137 < _end4301$136) {
          int32_t _tmp$1592;
          int32_t _tmp$1589;
          int32_t _tmp$1591;
          int32_t _tmp$1590;
          int32_t _tmp$1593;
          moonbit_incref(needle$134.$0);
          _tmp$1592 = $StringView$$unsafe_charcode_at(needle$134, i$137);
          _tmp$1589 = _tmp$1592 & 255;
          _tmp$1591 = needle_len$133 - 1;
          _tmp$1590 = _tmp$1591 - i$137;
          if (
            _tmp$1589 < 0
            || _tmp$1589 >= Moonbit_array_length(skip_table$135)
          ) {
            moonbit_panic();
          }
          skip_table$135[_tmp$1589] = _tmp$1590;
          _tmp$1593 = i$137 + 1;
          i$137 = _tmp$1593;
          continue;
        }
        break;
      }
      i$139 = 0;
      while (1) {
        int32_t _tmp$1594 = haystack_len$131 - needle_len$133;
        if (i$139 <= _tmp$1594) {
          int32_t _end4307$140 = needle_len$133 - 1;
          int32_t j$141 = 0;
          int32_t _tmp$1604;
          int32_t _tmp$1603;
          int32_t _tmp$1602;
          int32_t _tmp$1601;
          int32_t _tmp$1600;
          int32_t _tmp$1599;
          while (1) {
            if (j$141 <= _end4307$140) {
              int32_t _tmp$1597 = i$139 + j$141;
              int32_t _tmp$1595;
              int32_t _tmp$1596;
              int32_t _tmp$1598;
              moonbit_incref(haystack$132.$0);
              _tmp$1595
              = $StringView$$unsafe_charcode_at(
                haystack$132, _tmp$1597
              );
              moonbit_incref(needle$134.$0);
              _tmp$1596 = $StringView$$unsafe_charcode_at(needle$134, j$141);
              if (_tmp$1595 != _tmp$1596) {
                break;
              }
              _tmp$1598 = j$141 + 1;
              j$141 = _tmp$1598;
              continue;
            } else {
              moonbit_decref(skip_table$135);
              moonbit_decref(needle$134.$0);
              moonbit_decref(haystack$132.$0);
              return (int64_t)i$139;
            }
            break;
          }
          _tmp$1604 = i$139 + needle_len$133;
          _tmp$1603 = _tmp$1604 - 1;
          moonbit_incref(haystack$132.$0);
          _tmp$1602
          = $StringView$$unsafe_charcode_at(
            haystack$132, _tmp$1603
          );
          _tmp$1601 = _tmp$1602 & 255;
          if (
            _tmp$1601 < 0
            || _tmp$1601 >= Moonbit_array_length(skip_table$135)
          ) {
            moonbit_panic();
          }
          _tmp$1600 = (int32_t)skip_table$135[_tmp$1601];
          _tmp$1599 = i$139 + _tmp$1600;
          i$139 = _tmp$1599;
          continue;
        } else {
          moonbit_decref(skip_table$135);
          moonbit_decref(needle$134.$0);
          moonbit_decref(haystack$132.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$134.$0);
      moonbit_decref(haystack$132.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$134.$0);
    moonbit_decref(haystack$132.$0);
    return $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$130;
  }
}

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$128,
  int32_t index$129
) {
  moonbit_string_t _field$3268 = self$128.$0;
  moonbit_string_t str$1586 = _field$3268;
  int32_t _field$3267 = self$128.$1;
  int32_t start$1588 = _field$3267;
  int32_t _tmp$1587 = start$1588 + index$129;
  int32_t _tmp$3266 = str$1586[_tmp$1587];
  moonbit_decref(str$1586);
  return _tmp$3266;
}

int32_t $StringView$$length(struct $StringView self$127) {
  int32_t end$1584 = self$127.$2;
  int32_t _field$3269 = self$127.$1;
  int32_t start$1585;
  moonbit_decref(self$127.$0);
  start$1585 = _field$3269;
  return end$1584 - start$1585;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$125,
  int32_t index$126
) {
  int32_t len$124 = self$125->$1;
  int32_t _if_result$3770;
  if (index$126 >= 0) {
    _if_result$3770 = index$126 < len$124;
  } else {
    _if_result$3770 = 0;
  }
  if (_if_result$3770) {
    moonbit_string_t* _tmp$1583 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$125);
    moonbit_string_t _tmp$3270;
    if (index$126 < 0 || index$126 >= Moonbit_array_length(_tmp$1583)) {
      moonbit_panic();
    }
    _tmp$3270 = (moonbit_string_t)_tmp$1583[index$126];
    moonbit_incref(_tmp$3270);
    moonbit_decref(_tmp$1583);
    return _tmp$3270;
  } else {
    moonbit_decref(self$125);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$123
) {
  int32_t _field$3271 = self$123->$1;
  moonbit_decref(self$123);
  return _field$3271;
}

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$122
) {
  int32_t _field$3272 = self$122->$1;
  moonbit_decref(self$122);
  return _field$3272;
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$121
) {
  int32_t _field$3273 = self$121->$1;
  moonbit_decref(self$121);
  return _field$3273;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$120
) {
  int32_t _field$3274 = self$120->$1;
  moonbit_decref(self$120);
  return _field$3274;
}

struct $$3c$String$2a$String$3e$** $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$String$3e$$3e$* self$119
) {
  struct $$3c$String$2a$String$3e$** _field$3275 = self$119->$0;
  int32_t _cnt$3504 = Moonbit_object_header(self$119)->rc;
  if (_cnt$3504 > 1) {
    int32_t _new_cnt$3505;
    moonbit_incref(_field$3275);
    _new_cnt$3505 = _cnt$3504 - 1;
    Moonbit_object_header(self$119)->rc = _new_cnt$3505;
  } else if (_cnt$3504 == 1) {
    moonbit_free(self$119);
  }
  return _field$3275;
}

struct $$3c$String$2a$AttributeValue$3e$** $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$AttributeValue$3e$$3e$* self$118
) {
  struct $$3c$String$2a$AttributeValue$3e$** _field$3276 = self$118->$0;
  int32_t _cnt$3506 = Moonbit_object_header(self$118)->rc;
  if (_cnt$3506 > 1) {
    int32_t _new_cnt$3507;
    moonbit_incref(_field$3276);
    _new_cnt$3507 = _cnt$3506 - 1;
    Moonbit_object_header(self$118)->rc = _new_cnt$3507;
  } else if (_cnt$3506 == 1) {
    moonbit_free(self$118);
  }
  return _field$3276;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$117
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$3277 =
    self$117->$0;
  int32_t _cnt$3508 = Moonbit_object_header(self$117)->rc;
  if (_cnt$3508 > 1) {
    int32_t _new_cnt$3509;
    moonbit_incref(_field$3277);
    _new_cnt$3509 = _cnt$3508 - 1;
    Moonbit_object_header(self$117)->rc = _new_cnt$3509;
  } else if (_cnt$3508 == 1) {
    moonbit_free(self$117);
  }
  return _field$3277;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$116
) {
  moonbit_string_t* _field$3278 = self$116->$0;
  int32_t _cnt$3510 = Moonbit_object_header(self$116)->rc;
  if (_cnt$3510 > 1) {
    int32_t _new_cnt$3511;
    moonbit_incref(_field$3278);
    _new_cnt$3511 = _cnt$3510 - 1;
    Moonbit_object_header(self$116)->rc = _new_cnt$3511;
  } else if (_cnt$3510 == 1) {
    moonbit_free(self$116);
  }
  return _field$3278;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$115
) {
  struct $$3c$String$2a$Int$3e$** _field$3279 = self$115->$0;
  int32_t _cnt$3512 = Moonbit_object_header(self$115)->rc;
  if (_cnt$3512 > 1) {
    int32_t _new_cnt$3513;
    moonbit_incref(_field$3279);
    _new_cnt$3513 = _cnt$3512 - 1;
    Moonbit_object_header(self$115)->rc = _new_cnt$3513;
  } else if (_cnt$3512 == 1) {
    moonbit_free(self$115);
  }
  return _field$3279;
}

moonbit_string_t $String$$escape(moonbit_string_t self$114) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$113 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1582;
  moonbit_incref(buf$113);
  _tmp$1582
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$113
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$114, _tmp$1582);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$113);
}

int32_t $moonbitlang$core$builtin$op_notequal$3(int32_t x$111, int32_t y$112) {
  int32_t _tmp$1581 = x$111 == y$112;
  return !_tmp$1581;
}

int32_t $moonbitlang$core$builtin$op_notequal$2(
  moonbit_string_t x$109,
  moonbit_string_t y$110
) {
  int32_t _tmp$1580 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$0(x$109, y$110);
  return !_tmp$1580;
}

int32_t $moonbitlang$core$builtin$op_notequal$1(
  moonbit_string_t x$107,
  moonbit_string_t y$108
) {
  int32_t _tmp$3280 = moonbit_val_array_equal(x$107, y$108);
  int32_t _tmp$1579;
  moonbit_decref(x$107);
  moonbit_decref(y$108);
  _tmp$1579 = _tmp$3280;
  return !_tmp$1579;
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$105, int32_t y$106) {
  int32_t _tmp$1578 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$105, y$106
    );
  return !_tmp$1578;
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
  int32_t len$1573 = self$100->$1;
  int32_t _tmp$1572 = len$1573 + 4;
  moonbit_bytes_t _field$3281;
  moonbit_bytes_t data$1576;
  int32_t len$1577;
  int32_t inc$101;
  int32_t len$1575;
  int32_t _tmp$1574;
  moonbit_incref(self$100);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$100, _tmp$1572
  );
  _field$3281 = self$100->$0;
  data$1576 = _field$3281;
  len$1577 = self$100->$1;
  moonbit_incref(data$1576);
  inc$101 = $FixedArray$$set_utf16le_char(data$1576, len$1577, ch$102);
  len$1575 = self$100->$1;
  _tmp$1574 = len$1575 + inc$101;
  self$100->$1 = _tmp$1574;
  moonbit_decref(self$100);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$95,
  int32_t required$96
) {
  moonbit_bytes_t _field$3285 = self$95->$0;
  moonbit_bytes_t data$1571 = _field$3285;
  int32_t _tmp$3284 = Moonbit_array_length(data$1571);
  int32_t current_len$94 = _tmp$3284;
  int32_t enough_space$97;
  int32_t _tmp$1569;
  int32_t _tmp$1570;
  moonbit_bytes_t new_data$99;
  moonbit_bytes_t _field$3283;
  moonbit_bytes_t data$1567;
  int32_t len$1568;
  moonbit_bytes_t _old$3282;
  if (required$96 <= current_len$94) {
    moonbit_decref(self$95);
    return 0;
  }
  enough_space$97 = current_len$94;
  while (1) {
    int32_t _tmp$1565 = enough_space$97;
    if (_tmp$1565 < required$96) {
      int32_t _tmp$1566 = enough_space$97;
      enough_space$97 = _tmp$1566 * 2;
      continue;
    }
    break;
  }
  _tmp$1569 = enough_space$97;
  _tmp$1570 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$99 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1569, _tmp$1570);
  _field$3283 = self$95->$0;
  data$1567 = _field$3283;
  len$1568 = self$95->$1;
  moonbit_incref(data$1567);
  moonbit_incref(new_data$99);
  $FixedArray$$unsafe_blit$0(new_data$99, 0, data$1567, 0, len$1568);
  _old$3282 = self$95->$0;
  moonbit_decref(_old$3282);
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
    uint32_t _tmp$1548 = code$87 & 255u;
    int32_t _tmp$1547 = $UInt$$to_byte(_tmp$1548);
    int32_t _tmp$1549;
    uint32_t _tmp$1551;
    int32_t _tmp$1550;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1547;
    _tmp$1549 = offset$90 + 1;
    _tmp$1551 = code$87 >> 8;
    _tmp$1550 = $UInt$$to_byte(_tmp$1551);
    if (_tmp$1549 < 0 || _tmp$1549 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1549] = _tmp$1550;
    moonbit_decref(self$89);
    return 2;
  } else if (code$87 < 1114112u) {
    uint32_t hi$91 = code$87 - 65536u;
    uint32_t _tmp$1564 = hi$91 >> 10;
    uint32_t lo$92 = _tmp$1564 | 55296u;
    uint32_t _tmp$1563 = hi$91 & 1023u;
    uint32_t hi$93 = _tmp$1563 | 56320u;
    uint32_t _tmp$1553 = lo$92 & 255u;
    int32_t _tmp$1552 = $UInt$$to_byte(_tmp$1553);
    int32_t _tmp$1554;
    uint32_t _tmp$1556;
    int32_t _tmp$1555;
    int32_t _tmp$1557;
    uint32_t _tmp$1559;
    int32_t _tmp$1558;
    int32_t _tmp$1560;
    uint32_t _tmp$1562;
    int32_t _tmp$1561;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1552;
    _tmp$1554 = offset$90 + 1;
    _tmp$1556 = lo$92 >> 8;
    _tmp$1555 = $UInt$$to_byte(_tmp$1556);
    if (_tmp$1554 < 0 || _tmp$1554 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1554] = _tmp$1555;
    _tmp$1557 = offset$90 + 2;
    _tmp$1559 = hi$93 & 255u;
    _tmp$1558 = $UInt$$to_byte(_tmp$1559);
    if (_tmp$1557 < 0 || _tmp$1557 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1557] = _tmp$1558;
    _tmp$1560 = offset$90 + 3;
    _tmp$1562 = hi$93 >> 8;
    _tmp$1561 = $UInt$$to_byte(_tmp$1562);
    if (_tmp$1560 < 0 || _tmp$1560 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1560] = _tmp$1561;
    moonbit_decref(self$89);
    return 4;
  } else {
    moonbit_decref(self$89);
    return $moonbitlang$core$builtin$abort$1(
             (moonbit_string_t)moonbit_string_literal_149.data,
               (moonbit_string_t)moonbit_string_literal_150.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$86) {
  int32_t _tmp$1546 = *(int32_t*)&self$86;
  return _tmp$1546 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$85) {
  int32_t _tmp$1545 = self$85;
  return *(uint32_t*)&_tmp$1545;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$84
) {
  moonbit_bytes_t _field$3287 = self$84->$0;
  moonbit_bytes_t data$1544 = _field$3287;
  moonbit_bytes_t _tmp$1541;
  int32_t _field$3286;
  int32_t len$1543;
  int64_t _tmp$1542;
  moonbit_incref(data$1544);
  _tmp$1541 = data$1544;
  _field$3286 = self$84->$1;
  moonbit_decref(self$84);
  len$1543 = _field$3286;
  _tmp$1542 = (int64_t)len$1543;
  return $Bytes$$to_unchecked_string$inner(_tmp$1541, 0, _tmp$1542);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$79,
  int32_t offset$83,
  int64_t length$81
) {
  int32_t len$78 = Moonbit_array_length(self$79);
  int32_t length$80;
  int32_t _if_result$3772;
  if (length$81 == 4294967296ll) {
    length$80 = len$78 - offset$83;
  } else {
    int64_t _Some$82 = length$81;
    length$80 = (int32_t)_Some$82;
  }
  if (offset$83 >= 0) {
    if (length$80 >= 0) {
      int32_t _tmp$1540 = offset$83 + length$80;
      _if_result$3772 = _tmp$1540 <= len$78;
    } else {
      _if_result$3772 = 0;
    }
  } else {
    _if_result$3772 = 0;
  }
  if (_if_result$3772) {
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
  struct $$moonbitlang$core$builtin$StringBuilder* _block$3773;
  if (size_hint$76 < 1) {
    initial$75 = 1;
  } else {
    initial$75 = size_hint$76;
  }
  data$77 = (moonbit_bytes_t)moonbit_make_bytes(initial$75, 0);
  _block$3773
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$3773)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$3773->$0 = data$77;
  _block$3773->$1 = 0;
  return _block$3773;
}

int32_t $Byte$$to_char(int32_t self$74) {
  int32_t _tmp$1539 = (int32_t)self$74;
  return _tmp$1539;
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
  int32_t _if_result$3774;
  if (dst$50 == src$51) {
    _if_result$3774 = dst_offset$52 < src_offset$53;
  } else {
    _if_result$3774 = 0;
  }
  if (_if_result$3774) {
    int32_t i$54 = 0;
    while (1) {
      if (i$54 < len$55) {
        int32_t _tmp$1530 = dst_offset$52 + i$54;
        int32_t _tmp$1532 = src_offset$53 + i$54;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3289;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1531;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$3288;
        int32_t _tmp$1533;
        if (_tmp$1532 < 0 || _tmp$1532 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$3289
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1532
          ];
        _tmp$1531 = _tmp$3289;
        if (_tmp$1530 < 0 || _tmp$1530 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$3288
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1530
          ];
        if (_tmp$1531) {
          moonbit_incref(_tmp$1531);
        }
        if (_old$3288) {
          moonbit_decref(_old$3288);
        }
        dst$50[_tmp$1530] = _tmp$1531;
        _tmp$1533 = i$54 + 1;
        i$54 = _tmp$1533;
        continue;
      } else {
        moonbit_decref(src$51);
        moonbit_decref(dst$50);
      }
      break;
    }
  } else {
    int32_t _tmp$1538 = len$55 - 1;
    int32_t i$57 = _tmp$1538;
    while (1) {
      if (i$57 >= 0) {
        int32_t _tmp$1534 = dst_offset$52 + i$57;
        int32_t _tmp$1536 = src_offset$53 + i$57;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3291;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1535;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$3290;
        int32_t _tmp$1537;
        if (_tmp$1536 < 0 || _tmp$1536 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$3291
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1536
          ];
        _tmp$1535 = _tmp$3291;
        if (_tmp$1534 < 0 || _tmp$1534 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$3290
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1534
          ];
        if (_tmp$1535) {
          moonbit_incref(_tmp$1535);
        }
        if (_old$3290) {
          moonbit_decref(_old$3290);
        }
        dst$50[_tmp$1534] = _tmp$1535;
        _tmp$1537 = i$57 - 1;
        i$57 = _tmp$1537;
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
  int32_t _if_result$3777;
  if (dst$41 == src$42) {
    _if_result$3777 = dst_offset$43 < src_offset$44;
  } else {
    _if_result$3777 = 0;
  }
  if (_if_result$3777) {
    int32_t i$45 = 0;
    while (1) {
      if (i$45 < len$46) {
        int32_t _tmp$1521 = dst_offset$43 + i$45;
        int32_t _tmp$1523 = src_offset$44 + i$45;
        struct $$3c$String$2a$Int$3e$* _tmp$3293;
        struct $$3c$String$2a$Int$3e$* _tmp$1522;
        struct $$3c$String$2a$Int$3e$* _old$3292;
        int32_t _tmp$1524;
        if (_tmp$1523 < 0 || _tmp$1523 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$3293 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1523];
        _tmp$1522 = _tmp$3293;
        if (_tmp$1521 < 0 || _tmp$1521 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$3292 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1521];
        if (_tmp$1522) {
          moonbit_incref(_tmp$1522);
        }
        if (_old$3292) {
          moonbit_decref(_old$3292);
        }
        dst$41[_tmp$1521] = _tmp$1522;
        _tmp$1524 = i$45 + 1;
        i$45 = _tmp$1524;
        continue;
      } else {
        moonbit_decref(src$42);
        moonbit_decref(dst$41);
      }
      break;
    }
  } else {
    int32_t _tmp$1529 = len$46 - 1;
    int32_t i$48 = _tmp$1529;
    while (1) {
      if (i$48 >= 0) {
        int32_t _tmp$1525 = dst_offset$43 + i$48;
        int32_t _tmp$1527 = src_offset$44 + i$48;
        struct $$3c$String$2a$Int$3e$* _tmp$3295;
        struct $$3c$String$2a$Int$3e$* _tmp$1526;
        struct $$3c$String$2a$Int$3e$* _old$3294;
        int32_t _tmp$1528;
        if (_tmp$1527 < 0 || _tmp$1527 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$3295 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1527];
        _tmp$1526 = _tmp$3295;
        if (_tmp$1525 < 0 || _tmp$1525 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$3294 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1525];
        if (_tmp$1526) {
          moonbit_incref(_tmp$1526);
        }
        if (_old$3294) {
          moonbit_decref(_old$3294);
        }
        dst$41[_tmp$1525] = _tmp$1526;
        _tmp$1528 = i$48 - 1;
        i$48 = _tmp$1528;
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
  int32_t _if_result$3780;
  if (dst$32 == src$33) {
    _if_result$3780 = dst_offset$34 < src_offset$35;
  } else {
    _if_result$3780 = 0;
  }
  if (_if_result$3780) {
    int32_t i$36 = 0;
    while (1) {
      if (i$36 < len$37) {
        int32_t _tmp$1512 = dst_offset$34 + i$36;
        int32_t _tmp$1514 = src_offset$35 + i$36;
        moonbit_string_t _tmp$3297;
        moonbit_string_t _tmp$1513;
        moonbit_string_t _old$3296;
        int32_t _tmp$1515;
        if (_tmp$1514 < 0 || _tmp$1514 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$3297 = (moonbit_string_t)src$33[_tmp$1514];
        _tmp$1513 = _tmp$3297;
        if (_tmp$1512 < 0 || _tmp$1512 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$3296 = (moonbit_string_t)dst$32[_tmp$1512];
        moonbit_incref(_tmp$1513);
        moonbit_decref(_old$3296);
        dst$32[_tmp$1512] = _tmp$1513;
        _tmp$1515 = i$36 + 1;
        i$36 = _tmp$1515;
        continue;
      } else {
        moonbit_decref(src$33);
        moonbit_decref(dst$32);
      }
      break;
    }
  } else {
    int32_t _tmp$1520 = len$37 - 1;
    int32_t i$39 = _tmp$1520;
    while (1) {
      if (i$39 >= 0) {
        int32_t _tmp$1516 = dst_offset$34 + i$39;
        int32_t _tmp$1518 = src_offset$35 + i$39;
        moonbit_string_t _tmp$3299;
        moonbit_string_t _tmp$1517;
        moonbit_string_t _old$3298;
        int32_t _tmp$1519;
        if (_tmp$1518 < 0 || _tmp$1518 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$3299 = (moonbit_string_t)src$33[_tmp$1518];
        _tmp$1517 = _tmp$3299;
        if (_tmp$1516 < 0 || _tmp$1516 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$3298 = (moonbit_string_t)dst$32[_tmp$1516];
        moonbit_incref(_tmp$1517);
        moonbit_decref(_old$3298);
        dst$32[_tmp$1516] = _tmp$1517;
        _tmp$1519 = i$39 - 1;
        i$39 = _tmp$1519;
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
  int32_t _if_result$3783;
  if (dst$23 == src$24) {
    _if_result$3783 = dst_offset$25 < src_offset$26;
  } else {
    _if_result$3783 = 0;
  }
  if (_if_result$3783) {
    int32_t i$27 = 0;
    while (1) {
      if (i$27 < len$28) {
        int32_t _tmp$1503 = dst_offset$25 + i$27;
        int32_t _tmp$1505 = src_offset$26 + i$27;
        int32_t _tmp$1504;
        int32_t _tmp$1506;
        if (_tmp$1505 < 0 || _tmp$1505 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1504 = (int32_t)src$24[_tmp$1505];
        if (_tmp$1503 < 0 || _tmp$1503 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1503] = _tmp$1504;
        _tmp$1506 = i$27 + 1;
        i$27 = _tmp$1506;
        continue;
      } else {
        moonbit_decref(src$24);
        moonbit_decref(dst$23);
      }
      break;
    }
  } else {
    int32_t _tmp$1511 = len$28 - 1;
    int32_t i$30 = _tmp$1511;
    while (1) {
      if (i$30 >= 0) {
        int32_t _tmp$1507 = dst_offset$25 + i$30;
        int32_t _tmp$1509 = src_offset$26 + i$30;
        int32_t _tmp$1508;
        int32_t _tmp$1510;
        if (_tmp$1509 < 0 || _tmp$1509 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1508 = (int32_t)src$24[_tmp$1509];
        if (_tmp$1507 < 0 || _tmp$1507 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1507] = _tmp$1508;
        _tmp$1510 = i$30 - 1;
        i$30 = _tmp$1510;
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
  moonbit_string_t _tmp$1502 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$1500 =
    moonbit_add_string(
      _tmp$1502, (moonbit_string_t)moonbit_string_literal_151.data
    );
  moonbit_string_t _tmp$1501 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$1499 = moonbit_add_string(_tmp$1500, _tmp$1501);
  moonbit_string_t _tmp$1498 =
    moonbit_add_string(
      _tmp$1499, (moonbit_string_t)moonbit_string_literal_152.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1498);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$1497 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$1495 =
    moonbit_add_string(
      _tmp$1497, (moonbit_string_t)moonbit_string_literal_151.data
    );
  moonbit_string_t _tmp$1496 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$1494 = moonbit_add_string(_tmp$1495, _tmp$1496);
  moonbit_string_t _tmp$1493 =
    moonbit_add_string(
      _tmp$1494, (moonbit_string_t)moonbit_string_literal_152.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1493);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
) {
  moonbit_string_t _tmp$1492 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$17);
  moonbit_string_t _tmp$1490 =
    moonbit_add_string(
      _tmp$1492, (moonbit_string_t)moonbit_string_literal_151.data
    );
  moonbit_string_t _tmp$1491 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$18);
  moonbit_string_t _tmp$1489 = moonbit_add_string(_tmp$1490, _tmp$1491);
  moonbit_string_t _tmp$1488 =
    moonbit_add_string(
      _tmp$1489, (moonbit_string_t)moonbit_string_literal_152.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1488);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
) {
  moonbit_string_t _tmp$1487 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$15);
  moonbit_string_t _tmp$1485 =
    moonbit_add_string(
      _tmp$1487, (moonbit_string_t)moonbit_string_literal_151.data
    );
  moonbit_string_t _tmp$1486 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$16);
  moonbit_string_t _tmp$1484 = moonbit_add_string(_tmp$1485, _tmp$1486);
  moonbit_string_t _tmp$1483 =
    moonbit_add_string(
      _tmp$1484, (moonbit_string_t)moonbit_string_literal_152.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1483);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$11,
  struct $$moonbitlang$core$builtin$Logger _x_5272$14
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$12 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$11;
  moonbit_string_t _field$3300 = _Failure$12->$0;
  int32_t _cnt$3514 = Moonbit_object_header(_Failure$12)->rc;
  moonbit_string_t _$2a$err_payload_5273$13;
  struct $$moonbitlang$core$builtin$Logger _bind$1482;
  if (_cnt$3514 > 1) {
    int32_t _new_cnt$3515;
    moonbit_incref(_field$3300);
    _new_cnt$3515 = _cnt$3514 - 1;
    Moonbit_object_header(_Failure$12)->rc = _new_cnt$3515;
  } else if (_cnt$3514 == 1) {
    moonbit_free(_Failure$12);
  }
  _$2a$err_payload_5273$13 = _field$3300;
  if (_x_5272$14.$1) {
    moonbit_incref(_x_5272$14.$1);
  }
  _x_5272$14.$0->$method_0(
    _x_5272$14.$1, (moonbit_string_t)moonbit_string_literal_153.data
  );
  if (_x_5272$14.$1) {
    moonbit_incref(_x_5272$14.$1);
  }
  $$moonbitlang$core$builtin$Logger$$write_object$0(
    _x_5272$14, _$2a$err_payload_5273$13
  );
  _bind$1482 = _x_5272$14;
  _bind$1482.$0->$method_0(
    _bind$1482.$1, (moonbit_string_t)moonbit_string_literal_128.data
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
        _x_5286$10.$1, (moonbit_string_t)moonbit_string_literal_154.data
      );
      break;
    }
    default: {
      moonbit_decref(_x_5285$9);
      _x_5286$10.$0->$method_0(
        _x_5286$10.$1, (moonbit_string_t)moonbit_string_literal_155.data
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

moonbit_string_t $Error$to_string(void* _e$1381) {
  switch (Moonbit_object_tag(_e$1381)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
               _e$1381
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
               _e$1381
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1381);
      return (moonbit_string_t)moonbit_string_literal_156.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1381
             );
      break;
    }
    
    case 3: {
      moonbit_decref(_e$1381);
      return (moonbit_string_t)moonbit_string_literal_157.data;
      break;
    }
    default: {
      moonbit_decref(_e$1381);
      return (moonbit_string_t)moonbit_string_literal_158.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1399,
  int32_t _param$1398
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1397 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1399;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1397, _param$1398
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1396,
  struct $StringView _param$1395
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1394 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1396;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1394, _param$1395
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1393,
  moonbit_string_t _param$1390,
  int32_t _param$1391,
  int32_t _param$1392
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1389 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1393;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1389, _param$1390, _param$1391, _param$1392
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1388,
  moonbit_string_t _param$1387
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1386 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1388;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1386, _param$1387
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1207;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1405;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1404;
  moonbit_string_t* _tmp$1479;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1478;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1477;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1460;
  moonbit_string_t* _tmp$1476;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1475;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1474;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1461;
  moonbit_string_t* _tmp$1473;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1472;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1471;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1462;
  moonbit_string_t* _tmp$1470;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1469;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1468;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1463;
  moonbit_string_t* _tmp$1467;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1466;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1465;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1464;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$1203;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1459;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1458;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1457;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1408;
  moonbit_string_t* _tmp$1456;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1455;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1454;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1417;
  moonbit_string_t* _tmp$1453;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1452;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1451;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1418;
  moonbit_string_t* _tmp$1450;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1449;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1448;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1419;
  moonbit_string_t* _tmp$1447;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1446;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1445;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1420;
  moonbit_string_t* _tmp$1444;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1443;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1442;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1421;
  moonbit_string_t* _tmp$1441;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1440;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1439;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1422;
  moonbit_string_t* _tmp$1438;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1437;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1436;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1423;
  moonbit_string_t* _tmp$1435;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1434;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1433;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1424;
  moonbit_string_t* _tmp$1432;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1431;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1430;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1425;
  moonbit_string_t* _tmp$1429;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1428;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1427;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1426;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$1204;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1416;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1415;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1414;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1409;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$1205;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1413;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1412;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1411;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1410;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1202;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1407;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1406;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1206;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1481;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1480;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0$dyncall$closure.data;
  $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1$dyncall$closure.data;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$130 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$144 = (int64_t)0;
  _bind$1207
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1405 = _bind$1207;
  _tmp$1404
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1405
  };
  $azimuth$telemetry$simple_test$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1404
  );
  _tmp$1479 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1479[0] = (moonbit_string_t)moonbit_string_literal_159.data;
  _tmp$1478
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1478)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1478->$0 = _tmp$1479;
  _tmp$1478->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0$clo
  );
  _tuple$1477
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1477)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1477->$0
  = $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_0$clo;
  _tuple$1477->$1 = _tmp$1478;
  _tuple$1460
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1460)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1460->$0 = 0;
  _tuple$1460->$1 = _tuple$1477;
  _tmp$1476 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1476[0] = (moonbit_string_t)moonbit_string_literal_160.data;
  _tmp$1475
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1475)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1475->$0 = _tmp$1476;
  _tmp$1475->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1$clo
  );
  _tuple$1474
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1474)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1474->$0
  = $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_1$clo;
  _tuple$1474->$1 = _tmp$1475;
  _tuple$1461
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1461)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1461->$0 = 1;
  _tuple$1461->$1 = _tuple$1474;
  _tmp$1473 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1473[0] = (moonbit_string_t)moonbit_string_literal_161.data;
  _tmp$1472
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1472)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1472->$0 = _tmp$1473;
  _tmp$1472->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2$clo
  );
  _tuple$1471
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1471)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1471->$0
  = $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_2$clo;
  _tuple$1471->$1 = _tmp$1472;
  _tuple$1462
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1462)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1462->$0 = 2;
  _tuple$1462->$1 = _tuple$1471;
  _tmp$1470 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1470[0] = (moonbit_string_t)moonbit_string_literal_162.data;
  _tmp$1469
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1469)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1469->$0 = _tmp$1470;
  _tmp$1469->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3$clo
  );
  _tuple$1468
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1468)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1468->$0
  = $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_3$clo;
  _tuple$1468->$1 = _tmp$1469;
  _tuple$1463
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1463)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1463->$0 = 3;
  _tuple$1463->$1 = _tuple$1468;
  _tmp$1467 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1467[0] = (moonbit_string_t)moonbit_string_literal_163.data;
  _tmp$1466
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1466)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1466->$0 = _tmp$1467;
  _tmp$1466->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4$clo
  );
  _tuple$1465
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1465)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1465->$0
  = $azimuth$telemetry$simple_test$__test_73696d706c655f6d6f6f6e6269745f74657374732e6d6274_4$clo;
  _tuple$1465->$1 = _tmp$1466;
  _tuple$1464
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1464)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1464->$0 = 4;
  _tuple$1464->$1 = _tuple$1465;
  _bind$1203
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      5
    );
  _bind$1203[0] = _tuple$1460;
  _bind$1203[1] = _tuple$1461;
  _bind$1203[2] = _tuple$1462;
  _bind$1203[3] = _tuple$1463;
  _bind$1203[4] = _tuple$1464;
  _tmp$1459 = _bind$1203;
  _tmp$1458
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 5, _tmp$1459
  };
  _tmp$1457 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1458);
  _tuple$1408
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1408)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1408->$0 = (moonbit_string_t)moonbit_string_literal_164.data;
  _tuple$1408->$1 = _tmp$1457;
  _tmp$1456 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1456[0] = (moonbit_string_t)moonbit_string_literal_165.data;
  _tmp$1455
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1455)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1455->$0 = _tmp$1456;
  _tmp$1455->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0$clo
  );
  _tuple$1454
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1454)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1454->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_0$clo;
  _tuple$1454->$1 = _tmp$1455;
  _tuple$1417
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1417)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1417->$0 = 0;
  _tuple$1417->$1 = _tuple$1454;
  _tmp$1453 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1453[0] = (moonbit_string_t)moonbit_string_literal_166.data;
  _tmp$1452
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1452)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1452->$0 = _tmp$1453;
  _tmp$1452->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1$clo
  );
  _tuple$1451
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1451)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1451->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_1$clo;
  _tuple$1451->$1 = _tmp$1452;
  _tuple$1418
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1418)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1418->$0 = 1;
  _tuple$1418->$1 = _tuple$1451;
  _tmp$1450 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1450[0] = (moonbit_string_t)moonbit_string_literal_167.data;
  _tmp$1449
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1449)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1449->$0 = _tmp$1450;
  _tmp$1449->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2$clo
  );
  _tuple$1448
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1448)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1448->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_2$clo;
  _tuple$1448->$1 = _tmp$1449;
  _tuple$1419
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1419)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1419->$0 = 2;
  _tuple$1419->$1 = _tuple$1448;
  _tmp$1447 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1447[0] = (moonbit_string_t)moonbit_string_literal_168.data;
  _tmp$1446
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1446)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1446->$0 = _tmp$1447;
  _tmp$1446->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3$clo
  );
  _tuple$1445
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1445)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1445->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_3$clo;
  _tuple$1445->$1 = _tmp$1446;
  _tuple$1420
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1420)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1420->$0 = 3;
  _tuple$1420->$1 = _tuple$1445;
  _tmp$1444 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1444[0] = (moonbit_string_t)moonbit_string_literal_169.data;
  _tmp$1443
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1443)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1443->$0 = _tmp$1444;
  _tmp$1443->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4$clo
  );
  _tuple$1442
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1442)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1442->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_4$clo;
  _tuple$1442->$1 = _tmp$1443;
  _tuple$1421
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1421)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1421->$0 = 4;
  _tuple$1421->$1 = _tuple$1442;
  _tmp$1441 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1441[0] = (moonbit_string_t)moonbit_string_literal_170.data;
  _tmp$1440
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1440)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1440->$0 = _tmp$1441;
  _tmp$1440->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5$clo
  );
  _tuple$1439
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1439)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1439->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_5$clo;
  _tuple$1439->$1 = _tmp$1440;
  _tuple$1422
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1422)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1422->$0 = 5;
  _tuple$1422->$1 = _tuple$1439;
  _tmp$1438 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1438[0] = (moonbit_string_t)moonbit_string_literal_171.data;
  _tmp$1437
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1437)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1437->$0 = _tmp$1438;
  _tmp$1437->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6$clo
  );
  _tuple$1436
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1436)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1436->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_6$clo;
  _tuple$1436->$1 = _tmp$1437;
  _tuple$1423
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1423)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1423->$0 = 6;
  _tuple$1423->$1 = _tuple$1436;
  _tmp$1435 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1435[0] = (moonbit_string_t)moonbit_string_literal_172.data;
  _tmp$1434
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1434)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1434->$0 = _tmp$1435;
  _tmp$1434->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7$clo
  );
  _tuple$1433
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1433)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1433->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_7$clo;
  _tuple$1433->$1 = _tmp$1434;
  _tuple$1424
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1424)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1424->$0 = 7;
  _tuple$1424->$1 = _tuple$1433;
  _tmp$1432 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1432[0] = (moonbit_string_t)moonbit_string_literal_173.data;
  _tmp$1431
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1431)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1431->$0 = _tmp$1432;
  _tmp$1431->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8$clo
  );
  _tuple$1430
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1430)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1430->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_8$clo;
  _tuple$1430->$1 = _tmp$1431;
  _tuple$1425
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1425)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1425->$0 = 8;
  _tuple$1425->$1 = _tuple$1430;
  _tmp$1429 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1429[0] = (moonbit_string_t)moonbit_string_literal_174.data;
  _tmp$1428
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1428)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1428->$0 = _tmp$1429;
  _tmp$1428->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9$clo
  );
  _tuple$1427
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1427)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1427->$0
  = $azimuth$telemetry$simple_test$__test_686967685f7175616c6974795f74657374732e6d6274_9$clo;
  _tuple$1427->$1 = _tmp$1428;
  _tuple$1426
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1426)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1426->$0 = 9;
  _tuple$1426->$1 = _tuple$1427;
  _bind$1204
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      10
    );
  _bind$1204[0] = _tuple$1417;
  _bind$1204[1] = _tuple$1418;
  _bind$1204[2] = _tuple$1419;
  _bind$1204[3] = _tuple$1420;
  _bind$1204[4] = _tuple$1421;
  _bind$1204[5] = _tuple$1422;
  _bind$1204[6] = _tuple$1423;
  _bind$1204[7] = _tuple$1424;
  _bind$1204[8] = _tuple$1425;
  _bind$1204[9] = _tuple$1426;
  _tmp$1416 = _bind$1204;
  _tmp$1415
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 10, _tmp$1416
  };
  _tmp$1414 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1415);
  _tuple$1409
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1409)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1409->$0 = (moonbit_string_t)moonbit_string_literal_175.data;
  _tuple$1409->$1 = _tmp$1414;
  _bind$1205
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1413 = _bind$1205;
  _tmp$1412
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1413
  };
  _tmp$1411 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1412);
  _tuple$1410
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1410)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1410->$0 = (moonbit_string_t)moonbit_string_literal_176.data;
  _tuple$1410->$1 = _tmp$1411;
  _bind$1202
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      3
    );
  _bind$1202[0] = _tuple$1408;
  _bind$1202[1] = _tuple$1409;
  _bind$1202[2] = _tuple$1410;
  _tmp$1407 = _bind$1202;
  _tmp$1406
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 3, _tmp$1407
  };
  $azimuth$telemetry$simple_test$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1406
  );
  _bind$1206
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1481 = _bind$1206;
  _tmp$1480
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1481
  };
  $azimuth$telemetry$simple_test$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1480
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1403;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1375;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1376;
  int32_t _len$1377;
  int32_t _i$1378;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1403
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1375
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1375)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1375->$0 = _tmp$1403;
  async_tests$1375->$1 = 0;
  _arr$1376
  = $azimuth$telemetry$simple_test$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1376);
  _len$1377 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1376);
  _i$1378 = 0;
  while (1) {
    if (_i$1378 < _len$1377) {
      struct $$3c$String$2a$Int$3e$* arg$1379;
      moonbit_string_t _field$3302;
      moonbit_string_t _tmp$1400;
      int32_t _field$3301;
      int32_t _cnt$3516;
      int32_t _tmp$1401;
      int32_t _tmp$1402;
      moonbit_incref(_arr$1376);
      arg$1379
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1376, _i$1378
      );
      _field$3302 = arg$1379->$0;
      _tmp$1400 = _field$3302;
      _field$3301 = arg$1379->$1;
      _cnt$3516 = Moonbit_object_header(arg$1379)->rc;
      if (_cnt$3516 > 1) {
        int32_t _new_cnt$3517;
        moonbit_incref(_tmp$1400);
        _new_cnt$3517 = _cnt$3516 - 1;
        Moonbit_object_header(arg$1379)->rc = _new_cnt$3517;
      } else if (_cnt$3516 == 1) {
        moonbit_free(arg$1379);
      }
      _tmp$1401 = _field$3301;
      moonbit_incref(async_tests$1375);
      $azimuth$telemetry$simple_test$moonbit_test_driver_internal_do_execute(
        async_tests$1375, _tmp$1400, _tmp$1401
      );
      _tmp$1402 = _i$1378 + 1;
      _i$1378 = _tmp$1402;
      continue;
    } else {
      moonbit_decref(_arr$1376);
    }
    break;
  }
  $azimuth$telemetry$simple_test$moonbit_test_driver_internal_run_async_tests(
    async_tests$1375
  );
  return 0;
}