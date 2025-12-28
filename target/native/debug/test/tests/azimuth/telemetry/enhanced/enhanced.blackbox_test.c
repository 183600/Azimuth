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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $Ref$3c$Int$3e$;

struct $StringView;

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$;

struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__TestCase;

struct $$3c$$3e$$3d$$3e$Unit;

struct $$3c$Int$2a$Int$3e$;

struct $$3c$String$3e$$3d$$3e$Int;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap;

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $$3c$StringView$2a$StringView$3e$;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$Error$3e$$3d$$3e$Unit;

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$;

struct $Error$moonbitlang$core$builtin$InspectError$InspectError;

struct $Moonbit_Test_Driver_Internal__F$F2;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $$moonbitlang$core$builtin$Hasher;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158 {
  void* $0;
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

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
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

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$ {
  int32_t $0;
  int32_t $2;
  int32_t $3;
  int32_t $4;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $1;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $5;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
  int32_t $0;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap {
  moonbit_string_t(* code)(struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*);
  int32_t $0_1;
  int32_t $0_2;
  moonbit_string_t* $0_0;
  struct $Ref$3c$Int$3e$* $1;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
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

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1008
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2099,
  moonbit_string_t s$986,
  int32_t sep$987
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$973
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2008,
  moonbit_bytes_t bytes$974
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$2001,
  moonbit_string_t s$968
);

#define $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$931,
  moonbit_string_t filename$892,
  int32_t index$893
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1994
);

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1990
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1988
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1972,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$932,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$933
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1983,
  int32_t _cont_param$952
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1980,
  void* _cont_param$953
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1974,
  void* _state$935
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1969,
  void* err$915
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1955,
  moonbit_string_t attr$908
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1939,
  moonbit_string_t test_name$895,
  moonbit_string_t file_name$896,
  moonbit_string_t message$897,
  int32_t skipped$898
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$890
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$888,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$889,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$886
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$849,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$862,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$875,
  moonbit_string_t file_filter$846,
  int32_t index_filter$847
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
  struct $$3c$String$3e$$3d$$3e$Int* _env$1895,
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1548
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
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1477,
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

moonbit_string_t $Error$to_string(void* _e$1015);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1033,
  int32_t _param$1032
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1030,
  struct $StringView _param$1029
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1027,
  moonbit_string_t _param$1024,
  int32_t _param$1025,
  int32_t _param$1026
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1022,
  moonbit_string_t _param$1021
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

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    116, 101, 115, 116, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[27]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 26), 
    101, 110, 104, 97, 110, 99, 101, 100, 95, 97, 122, 105, 109, 117, 
    116, 104, 95, 116, 101, 115, 116, 115, 46, 109, 98, 116, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_43 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[131]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 130), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 109, 
    117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 47, 
    101, 110, 104, 97, 110, 99, 101, 100, 95, 98, 108, 97, 99, 107, 98, 
    111, 120, 95, 116, 101, 115, 116, 46, 77, 111, 111, 110, 66, 105, 
    116, 84, 101, 115, 116, 68, 114, 105, 118, 101, 114, 73, 110, 116, 
    101, 114, 110, 97, 108, 74, 115, 69, 114, 114, 111, 114, 46, 77, 
    111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 68, 114, 105, 118, 
    101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 74, 115, 69, 114, 
    114, 111, 114, 0
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

struct { int32_t rc; uint32_t meta; uint16_t const data[32]; 
} const moonbit_string_literal_42 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 31), 
    97, 122, 105, 109, 117, 116, 104, 95, 101, 110, 104, 97, 110, 99, 
    101, 100, 95, 116, 101, 115, 116, 95, 115, 117, 105, 116, 101, 46, 
    109, 98, 116, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct { int32_t rc; uint32_t meta; struct $$3c$String$3e$$3d$$3e$Int data; 
} const $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5
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

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_async_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_no_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_with_args_tests;

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1008
) {
  moonbit_decref(_tests$1008);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$967 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$973 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$980 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$973;
  int32_t moonbit_test_driver_internal_split_mbt_string$985 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$2124 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$992 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$993;
  moonbit_string_t _tmp$2123;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$994;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$995;
  int32_t _len$996;
  int32_t _i$997;
  Moonbit_object_header(file_and_index$992)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$992->$0 = _tmp$2124;
  file_and_index$992->$1 = 0;
  cli_args$993
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$980
  );
  _tmp$2123 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$993, 1);
  test_args$994
  = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$985, _tmp$2123, 47
  );
  _arr$995 = test_args$994;
  moonbit_incref(_arr$995);
  _len$996 = $$moonbitlang$core$builtin$Array$$length$1(_arr$995);
  _i$997 = 0;
  while (1) {
    if (_i$997 < _len$996) {
      moonbit_string_t arg$998;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$999;
      moonbit_string_t file$1000;
      moonbit_string_t range$1001;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1002;
      moonbit_string_t _tmp$2121;
      int32_t start$1003;
      moonbit_string_t _tmp$2120;
      int32_t end$1004;
      int32_t i$1005;
      int32_t _tmp$2122;
      moonbit_incref(_arr$995);
      arg$998
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$995, _i$997
      );
      file_and_range$999
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$985, arg$998, 58
      );
      moonbit_incref(file_and_range$999);
      file$1000
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$999, 0
      );
      range$1001
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$999, 1
      );
      start_and_end$1002
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$985, range$1001, 45
      );
      moonbit_incref(start_and_end$1002);
      _tmp$2121
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1002, 0
      );
      start$1003
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$967, _tmp$2121
      );
      _tmp$2120
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1002, 1
      );
      end$1004
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$967, _tmp$2120
      );
      i$1005 = start$1003;
      while (1) {
        if (i$1005 < end$1004) {
          struct $$3c$String$2a$Int$3e$* _tuple$2118;
          int32_t _tmp$2119;
          moonbit_incref(file$1000);
          _tuple$2118
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$2118)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$2118->$0 = file$1000;
          _tuple$2118->$1 = i$1005;
          moonbit_incref(file_and_index$992);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$992, _tuple$2118
          );
          _tmp$2119 = i$1005 + 1;
          i$1005 = _tmp$2119;
          continue;
        } else {
          moonbit_decref(file$1000);
        }
        break;
      }
      _tmp$2122 = _i$997 + 1;
      _i$997 = _tmp$2122;
      continue;
    } else {
      moonbit_decref(_arr$995);
    }
    break;
  }
  return file_and_index$992;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$2099,
  moonbit_string_t s$986,
  int32_t sep$987
) {
  moonbit_string_t* _tmp$2117 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$988 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$989;
  struct $Ref$3c$Int$3e$* start$990;
  int32_t val$2112;
  int32_t _tmp$2113;
  Moonbit_object_header(res$988)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$988->$0 = _tmp$2117;
  res$988->$1 = 0;
  i$989
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$989)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$989->$0 = 0;
  start$990
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$990)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$990->$0 = 0;
  while (1) {
    int32_t val$2100 = i$989->$0;
    int32_t _tmp$2101 = Moonbit_array_length(s$986);
    if (val$2100 < _tmp$2101) {
      int32_t val$2104 = i$989->$0;
      int32_t _tmp$2103;
      int32_t _tmp$2102;
      int32_t val$2111;
      int32_t _tmp$2110;
      if (val$2104 < 0 || val$2104 >= Moonbit_array_length(s$986)) {
        moonbit_panic();
      }
      _tmp$2103 = s$986[val$2104];
      _tmp$2102 = _tmp$2103;
      if (_tmp$2102 == sep$987) {
        int32_t val$2106 = start$990->$0;
        int32_t val$2107 = i$989->$0;
        moonbit_string_t _tmp$2105;
        int32_t val$2109;
        int32_t _tmp$2108;
        moonbit_incref(s$986);
        _tmp$2105 = $String$$unsafe_substring(s$986, val$2106, val$2107);
        moonbit_incref(res$988);
        $$moonbitlang$core$builtin$Array$$push$0(res$988, _tmp$2105);
        val$2109 = i$989->$0;
        _tmp$2108 = val$2109 + 1;
        start$990->$0 = _tmp$2108;
      }
      val$2111 = i$989->$0;
      _tmp$2110 = val$2111 + 1;
      i$989->$0 = _tmp$2110;
      continue;
    } else {
      moonbit_decref(i$989);
    }
    break;
  }
  val$2112 = start$990->$0;
  _tmp$2113 = Moonbit_array_length(s$986);
  if (val$2112 < _tmp$2113) {
    int32_t _field$2125 = start$990->$0;
    int32_t val$2115;
    int32_t _tmp$2116;
    moonbit_string_t _tmp$2114;
    moonbit_decref(start$990);
    val$2115 = _field$2125;
    _tmp$2116 = Moonbit_array_length(s$986);
    _tmp$2114 = $String$$unsafe_substring(s$986, val$2115, _tmp$2116);
    moonbit_incref(res$988);
    $$moonbitlang$core$builtin$Array$$push$0(res$988, _tmp$2114);
  } else {
    moonbit_decref(start$990);
    moonbit_decref(s$986);
  }
  return res$988;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$973
) {
  moonbit_bytes_t* tmp$981 =
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$2098 = Moonbit_array_length(tmp$981);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$982 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$2098);
  int32_t i$983 = 0;
  while (1) {
    int32_t _tmp$2094 = Moonbit_array_length(tmp$981);
    if (i$983 < _tmp$2094) {
      moonbit_bytes_t _tmp$2126;
      moonbit_bytes_t _tmp$2096;
      moonbit_string_t _tmp$2095;
      int32_t _tmp$2097;
      if (i$983 < 0 || i$983 >= Moonbit_array_length(tmp$981)) {
        moonbit_panic();
      }
      _tmp$2126 = (moonbit_bytes_t)tmp$981[i$983];
      _tmp$2096 = _tmp$2126;
      moonbit_incref(_tmp$2096);
      _tmp$2095
      = $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$973, _tmp$2096
      );
      moonbit_incref(res$982);
      $$moonbitlang$core$builtin$Array$$push$0(res$982, _tmp$2095);
      _tmp$2097 = i$983 + 1;
      i$983 = _tmp$2097;
      continue;
    } else {
      moonbit_decref(tmp$981);
    }
    break;
  }
  return res$982;
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$2008,
  moonbit_bytes_t bytes$974
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$975 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$976 = Moonbit_array_length(bytes$974);
  struct $Ref$3c$Int$3e$* i$977 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$977)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$977->$0 = 0;
  while (1) {
    int32_t val$2009 = i$977->$0;
    if (val$2009 < len$976) {
      int32_t val$2093 = i$977->$0;
      int32_t _tmp$2092;
      int32_t _tmp$2091;
      struct $Ref$3c$Int$3e$* c$978;
      int32_t val$2010;
      if (val$2093 < 0 || val$2093 >= Moonbit_array_length(bytes$974)) {
        moonbit_panic();
      }
      _tmp$2092 = bytes$974[val$2093];
      _tmp$2091 = (int32_t)_tmp$2092;
      c$978
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$978)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$978->$0 = _tmp$2091;
      val$2010 = c$978->$0;
      if (val$2010 < 128) {
        int32_t _field$2127 = c$978->$0;
        int32_t val$2012;
        int32_t _tmp$2011;
        int32_t val$2014;
        int32_t _tmp$2013;
        moonbit_decref(c$978);
        val$2012 = _field$2127;
        _tmp$2011 = val$2012;
        moonbit_incref(res$975);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$975, _tmp$2011
        );
        val$2014 = i$977->$0;
        _tmp$2013 = val$2014 + 1;
        i$977->$0 = _tmp$2013;
      } else {
        int32_t val$2015 = c$978->$0;
        if (val$2015 < 224) {
          int32_t val$2017 = i$977->$0;
          int32_t _tmp$2016 = val$2017 + 1;
          int32_t val$2026;
          int32_t _tmp$2025;
          int32_t _tmp$2019;
          int32_t val$2024;
          int32_t _tmp$2023;
          int32_t _tmp$2022;
          int32_t _tmp$2021;
          int32_t _tmp$2020;
          int32_t _tmp$2018;
          int32_t _field$2128;
          int32_t val$2028;
          int32_t _tmp$2027;
          int32_t val$2030;
          int32_t _tmp$2029;
          if (_tmp$2016 >= len$976) {
            moonbit_decref(c$978);
            moonbit_decref(i$977);
            moonbit_decref(bytes$974);
            break;
          }
          val$2026 = c$978->$0;
          _tmp$2025 = val$2026 & 31;
          _tmp$2019 = _tmp$2025 << 6;
          val$2024 = i$977->$0;
          _tmp$2023 = val$2024 + 1;
          if (_tmp$2023 < 0 || _tmp$2023 >= Moonbit_array_length(bytes$974)) {
            moonbit_panic();
          }
          _tmp$2022 = bytes$974[_tmp$2023];
          _tmp$2021 = (int32_t)_tmp$2022;
          _tmp$2020 = _tmp$2021 & 63;
          _tmp$2018 = _tmp$2019 | _tmp$2020;
          c$978->$0 = _tmp$2018;
          _field$2128 = c$978->$0;
          moonbit_decref(c$978);
          val$2028 = _field$2128;
          _tmp$2027 = val$2028;
          moonbit_incref(res$975);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$975, _tmp$2027
          );
          val$2030 = i$977->$0;
          _tmp$2029 = val$2030 + 2;
          i$977->$0 = _tmp$2029;
        } else {
          int32_t val$2031 = c$978->$0;
          if (val$2031 < 240) {
            int32_t val$2033 = i$977->$0;
            int32_t _tmp$2032 = val$2033 + 2;
            int32_t val$2049;
            int32_t _tmp$2048;
            int32_t _tmp$2041;
            int32_t val$2047;
            int32_t _tmp$2046;
            int32_t _tmp$2045;
            int32_t _tmp$2044;
            int32_t _tmp$2043;
            int32_t _tmp$2042;
            int32_t _tmp$2035;
            int32_t val$2040;
            int32_t _tmp$2039;
            int32_t _tmp$2038;
            int32_t _tmp$2037;
            int32_t _tmp$2036;
            int32_t _tmp$2034;
            int32_t _field$2129;
            int32_t val$2051;
            int32_t _tmp$2050;
            int32_t val$2053;
            int32_t _tmp$2052;
            if (_tmp$2032 >= len$976) {
              moonbit_decref(c$978);
              moonbit_decref(i$977);
              moonbit_decref(bytes$974);
              break;
            }
            val$2049 = c$978->$0;
            _tmp$2048 = val$2049 & 15;
            _tmp$2041 = _tmp$2048 << 12;
            val$2047 = i$977->$0;
            _tmp$2046 = val$2047 + 1;
            if (
              _tmp$2046 < 0 || _tmp$2046 >= Moonbit_array_length(bytes$974)
            ) {
              moonbit_panic();
            }
            _tmp$2045 = bytes$974[_tmp$2046];
            _tmp$2044 = (int32_t)_tmp$2045;
            _tmp$2043 = _tmp$2044 & 63;
            _tmp$2042 = _tmp$2043 << 6;
            _tmp$2035 = _tmp$2041 | _tmp$2042;
            val$2040 = i$977->$0;
            _tmp$2039 = val$2040 + 2;
            if (
              _tmp$2039 < 0 || _tmp$2039 >= Moonbit_array_length(bytes$974)
            ) {
              moonbit_panic();
            }
            _tmp$2038 = bytes$974[_tmp$2039];
            _tmp$2037 = (int32_t)_tmp$2038;
            _tmp$2036 = _tmp$2037 & 63;
            _tmp$2034 = _tmp$2035 | _tmp$2036;
            c$978->$0 = _tmp$2034;
            _field$2129 = c$978->$0;
            moonbit_decref(c$978);
            val$2051 = _field$2129;
            _tmp$2050 = val$2051;
            moonbit_incref(res$975);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$975, _tmp$2050
            );
            val$2053 = i$977->$0;
            _tmp$2052 = val$2053 + 3;
            i$977->$0 = _tmp$2052;
          } else {
            int32_t val$2055 = i$977->$0;
            int32_t _tmp$2054 = val$2055 + 3;
            int32_t val$2078;
            int32_t _tmp$2077;
            int32_t _tmp$2070;
            int32_t val$2076;
            int32_t _tmp$2075;
            int32_t _tmp$2074;
            int32_t _tmp$2073;
            int32_t _tmp$2072;
            int32_t _tmp$2071;
            int32_t _tmp$2063;
            int32_t val$2069;
            int32_t _tmp$2068;
            int32_t _tmp$2067;
            int32_t _tmp$2066;
            int32_t _tmp$2065;
            int32_t _tmp$2064;
            int32_t _tmp$2057;
            int32_t val$2062;
            int32_t _tmp$2061;
            int32_t _tmp$2060;
            int32_t _tmp$2059;
            int32_t _tmp$2058;
            int32_t _tmp$2056;
            int32_t val$2080;
            int32_t _tmp$2079;
            int32_t val$2084;
            int32_t _tmp$2083;
            int32_t _tmp$2082;
            int32_t _tmp$2081;
            int32_t _field$2130;
            int32_t val$2088;
            int32_t _tmp$2087;
            int32_t _tmp$2086;
            int32_t _tmp$2085;
            int32_t val$2090;
            int32_t _tmp$2089;
            if (_tmp$2054 >= len$976) {
              moonbit_decref(c$978);
              moonbit_decref(i$977);
              moonbit_decref(bytes$974);
              break;
            }
            val$2078 = c$978->$0;
            _tmp$2077 = val$2078 & 7;
            _tmp$2070 = _tmp$2077 << 18;
            val$2076 = i$977->$0;
            _tmp$2075 = val$2076 + 1;
            if (
              _tmp$2075 < 0 || _tmp$2075 >= Moonbit_array_length(bytes$974)
            ) {
              moonbit_panic();
            }
            _tmp$2074 = bytes$974[_tmp$2075];
            _tmp$2073 = (int32_t)_tmp$2074;
            _tmp$2072 = _tmp$2073 & 63;
            _tmp$2071 = _tmp$2072 << 12;
            _tmp$2063 = _tmp$2070 | _tmp$2071;
            val$2069 = i$977->$0;
            _tmp$2068 = val$2069 + 2;
            if (
              _tmp$2068 < 0 || _tmp$2068 >= Moonbit_array_length(bytes$974)
            ) {
              moonbit_panic();
            }
            _tmp$2067 = bytes$974[_tmp$2068];
            _tmp$2066 = (int32_t)_tmp$2067;
            _tmp$2065 = _tmp$2066 & 63;
            _tmp$2064 = _tmp$2065 << 6;
            _tmp$2057 = _tmp$2063 | _tmp$2064;
            val$2062 = i$977->$0;
            _tmp$2061 = val$2062 + 3;
            if (
              _tmp$2061 < 0 || _tmp$2061 >= Moonbit_array_length(bytes$974)
            ) {
              moonbit_panic();
            }
            _tmp$2060 = bytes$974[_tmp$2061];
            _tmp$2059 = (int32_t)_tmp$2060;
            _tmp$2058 = _tmp$2059 & 63;
            _tmp$2056 = _tmp$2057 | _tmp$2058;
            c$978->$0 = _tmp$2056;
            val$2080 = c$978->$0;
            _tmp$2079 = val$2080 - 65536;
            c$978->$0 = _tmp$2079;
            val$2084 = c$978->$0;
            _tmp$2083 = val$2084 >> 10;
            _tmp$2082 = _tmp$2083 + 55296;
            _tmp$2081 = _tmp$2082;
            moonbit_incref(res$975);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$975, _tmp$2081
            );
            _field$2130 = c$978->$0;
            moonbit_decref(c$978);
            val$2088 = _field$2130;
            _tmp$2087 = val$2088 & 1023;
            _tmp$2086 = _tmp$2087 + 56320;
            _tmp$2085 = _tmp$2086;
            moonbit_incref(res$975);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$975, _tmp$2085
            );
            val$2090 = i$977->$0;
            _tmp$2089 = val$2090 + 4;
            i$977->$0 = _tmp$2089;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$977);
      moonbit_decref(bytes$974);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$975);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$2001,
  moonbit_string_t s$968
) {
  struct $Ref$3c$Int$3e$* res$969 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$970;
  int32_t i$971;
  int32_t _field$2131;
  Moonbit_object_header(res$969)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$969->$0 = 0;
  len$970 = Moonbit_array_length(s$968);
  i$971 = 0;
  while (1) {
    if (i$971 < len$970) {
      int32_t val$2006 = res$969->$0;
      int32_t _tmp$2003 = val$2006 * 10;
      int32_t _tmp$2005;
      int32_t _tmp$2004;
      int32_t _tmp$2002;
      int32_t _tmp$2007;
      if (i$971 < 0 || i$971 >= Moonbit_array_length(s$968)) {
        moonbit_panic();
      }
      _tmp$2005 = s$968[i$971];
      _tmp$2004 = _tmp$2005 - 48;
      _tmp$2002 = _tmp$2003 + _tmp$2004;
      res$969->$0 = _tmp$2002;
      _tmp$2007 = i$971 + 1;
      i$971 = _tmp$2007;
      continue;
    } else {
      moonbit_decref(s$968);
    }
    break;
  }
  _field$2131 = res$969->$0;
  moonbit_decref(res$969);
  return _field$2131;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$931,
  moonbit_string_t filename$892,
  int32_t index$893
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$891;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$2541;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$894;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$903;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2141;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$2000;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2140;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$904;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$2139;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$1999;
  moonbit_string_t _field$2138;
  moonbit_string_t file_name$905;
  moonbit_string_t name$906;
  int32_t _tmp$1996;
  moonbit_string_t name$907;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$1953;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1954;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$2543;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$914;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$930;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$955;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$957;
  void* _field$2135;
  int32_t _cnt$2412;
  void* _bind$958;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$2547;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1993;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$2548;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1986;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$2549;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$1987;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$2550;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1971;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_async_tests
  );
  filtered_test$891
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_async_tests,
      filename$892,
      index$893
  );
  _closure$2541
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$2541)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$2541->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$2541->$0 = index$893;
  handle_result$894
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$2541;
  if (filtered_test$891 == 0) {
    moonbit_decref(async_tests$931);
    if (filtered_test$891) {
      moonbit_decref(filtered_test$891);
    }
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$894,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$965 =
      filtered_test$891;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$966 = _Some$965;
    item$903 = _item$966;
    goto $join$902;
  }
  goto $joinlet$2542;
  $join$902:;
  _field$2141 = item$903->$1;
  meta$2000 = _field$2141;
  _field$2140 = meta$2000->$2;
  attrs$904 = _field$2140;
  _field$2139 = item$903->$1;
  meta$1999 = _field$2139;
  _field$2138 = meta$1999->$0;
  file_name$905 = _field$2138;
  moonbit_incref(attrs$904);
  moonbit_incref(file_name$905);
  moonbit_incref(attrs$904);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$904)) {
    name$906 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$904);
    name$906 = $$moonbitlang$core$builtin$Array$$at$0(attrs$904, 0);
  }
  _tmp$1996 = Moonbit_array_length(name$906);
  if (_tmp$1996 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2137;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$1998;
    int32_t _field$2136;
    int32_t index$1997;
    moonbit_decref(name$906);
    _field$2137 = item$903->$1;
    meta$1998 = _field$2137;
    _field$2136 = meta$1998->$1;
    index$1997 = _field$2136;
    name$907 = $Int$$to_string$inner(index$1997, 10);
  } else {
    name$907 = name$906;
  }
  moonbit_incref(attrs$904);
  _tmp$1953 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$904);
  _tmp$1954
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$1953, _tmp$1954)) {
    moonbit_string_t _tmp$1968;
    moonbit_string_t _tmp$1967;
    moonbit_string_t _tmp$1964;
    moonbit_string_t _tmp$1966;
    moonbit_string_t _tmp$1965;
    moonbit_string_t _tmp$1963;
    moonbit_decref(async_tests$931);
    moonbit_decref(item$903);
    moonbit_incref(file_name$905);
    _tmp$1968
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$905
    );
    _tmp$1967
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$1968
    );
    _tmp$1964
    = moonbit_add_string(
      _tmp$1967, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$1966 = $$moonbitlang$core$builtin$Array$$at$0(attrs$904, 0);
    _tmp$1965 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$1966);
    _tmp$1963 = moonbit_add_string(_tmp$1964, _tmp$1965);
    $moonbitlang$core$builtin$println$0(_tmp$1963);
    $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$894,
        name$907,
        file_name$905,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$904);
  }
  moonbit_incref(name$907);
  moonbit_incref(file_name$905);
  moonbit_incref(handle_result$894);
  _closure$2543
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$2543)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2543->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$2543->$0 = name$907;
  _closure$2543->$1 = file_name$905;
  _closure$2543->$2 = handle_result$894;
  on_err$914 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2543;
  _field$2135 = item$903->$0;
  _cnt$2412 = Moonbit_object_header(item$903)->rc;
  if (_cnt$2412 > 1) {
    int32_t _new_cnt$2414;
    moonbit_incref(_field$2135);
    _new_cnt$2414 = _cnt$2412 - 1;
    Moonbit_object_header(item$903)->rc = _new_cnt$2414;
  } else if (_cnt$2412 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$2413 = item$903->$1;
    moonbit_decref(_field$2413);
    moonbit_free(item$903);
  }
  _bind$958 = _field$2135;
  switch (Moonbit_object_tag(_bind$958)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$959;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2132;
      int32_t _cnt$2415;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$960;
      moonbit_decref(async_tests$931);
      _F0$959 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$958;
      _field$2132 = _F0$959->$0;
      _cnt$2415 = Moonbit_object_header(_F0$959)->rc;
      if (_cnt$2415 > 1) {
        int32_t _new_cnt$2416;
        moonbit_incref(_field$2132);
        _new_cnt$2416 = _cnt$2415 - 1;
        Moonbit_object_header(_F0$959)->rc = _new_cnt$2416;
      } else if (_cnt$2415 == 1) {
        moonbit_free(_F0$959);
      }
      _f$960 = _field$2132;
      f$957 = _f$960;
      goto $join$956;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$961;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2133;
      int32_t _cnt$2417;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$962;
      moonbit_decref(async_tests$931);
      _F1$961 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$958;
      _field$2133 = _F1$961->$0;
      _cnt$2417 = Moonbit_object_header(_F1$961)->rc;
      if (_cnt$2417 > 1) {
        int32_t _new_cnt$2418;
        moonbit_incref(_field$2133);
        _new_cnt$2418 = _cnt$2417 - 1;
        Moonbit_object_header(_F1$961)->rc = _new_cnt$2418;
      } else if (_cnt$2417 == 1) {
        moonbit_free(_F1$961);
      }
      _f$962 = _field$2133;
      f$955 = _f$962;
      goto $join$954;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$963 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$958;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2134 =
        _F2$963->$0;
      int32_t _cnt$2419 = Moonbit_object_header(_F2$963)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$964;
      if (_cnt$2419 > 1) {
        int32_t _new_cnt$2420;
        moonbit_incref(_field$2134);
        _new_cnt$2420 = _cnt$2419 - 1;
        Moonbit_object_header(_F2$963)->rc = _new_cnt$2420;
      } else if (_cnt$2419 == 1) {
        moonbit_free(_F2$963);
      }
      _f$964 = _field$2134;
      f$930 = _f$964;
      goto $join$929;
      break;
    }
  }
  goto $joinlet$2546;
  $join$956:;
  _closure$2547
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$2547)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2547->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$2547->$0 = name$907;
  _closure$2547->$1 = file_name$905;
  _closure$2547->$2 = handle_result$894;
  _tmp$1993 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2547;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_catch_error(
    f$957, _tmp$1993, on_err$914
  );
  $joinlet$2546:;
  goto $joinlet$2545;
  $join$954:;
  moonbit_incref(name$907);
  _closure$2548
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$2548)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$2548->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$2548->$0 = f$955;
  _closure$2548->$1 = name$907;
  _tmp$1986
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$2548;
  _closure$2549
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$2549)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$2549->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$2549->$0 = name$907;
  _closure$2549->$1 = file_name$905;
  _closure$2549->$2 = handle_result$894;
  _tmp$1987 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$2549;
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_catch_error(
    _tmp$1986, _tmp$1987, on_err$914
  );
  $joinlet$2545:;
  goto $joinlet$2544;
  $join$929:;
  _closure$2550
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$2550)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$2550->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$2550->$0 = f$930;
  _closure$2550->$1 = on_err$914;
  _closure$2550->$2 = name$907;
  _closure$2550->$3 = file_name$905;
  _closure$2550->$4 = handle_result$894;
  _tmp$1971
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$2550;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$931, _tmp$1971);
  $joinlet$2544:;
  $joinlet$2542:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1994
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$1995 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$1994;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2144 =
    _casted_env$1995->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$894 =
    _field$2144;
  moonbit_string_t _field$2143 = _casted_env$1995->$1;
  moonbit_string_t file_name$905 = _field$2143;
  moonbit_string_t _field$2142 = _casted_env$1995->$0;
  int32_t _cnt$2421 = Moonbit_object_header(_casted_env$1995)->rc;
  moonbit_string_t name$907;
  if (_cnt$2421 > 1) {
    int32_t _new_cnt$2422;
    moonbit_incref(handle_result$894);
    moonbit_incref(file_name$905);
    moonbit_incref(_field$2142);
    _new_cnt$2422 = _cnt$2421 - 1;
    Moonbit_object_header(_casted_env$1995)->rc = _new_cnt$2422;
  } else if (_cnt$2421 == 1) {
    moonbit_free(_casted_env$1995);
  }
  name$907 = _field$2142;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$894,
      name$907,
      file_name$905,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$1990
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$1991 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$1990;
  moonbit_string_t _field$2146 = _casted_env$1991->$1;
  moonbit_string_t name$907 = _field$2146;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2145 =
    _casted_env$1991->$0;
  int32_t _cnt$2423 = Moonbit_object_header(_casted_env$1991)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$955;
  int32_t _tmp$1992;
  if (_cnt$2423 > 1) {
    int32_t _new_cnt$2424;
    moonbit_incref(name$907);
    moonbit_incref(_field$2145);
    _new_cnt$2424 = _cnt$2423 - 1;
    Moonbit_object_header(_casted_env$1991)->rc = _new_cnt$2424;
  } else if (_cnt$2423 == 1) {
    moonbit_free(_casted_env$1991);
  }
  f$955 = _field$2145;
  _tmp$1992
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$907
  );
  return f$955->code(f$955, _tmp$1992);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$1988
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$1989 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$1988;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2149 =
    _casted_env$1989->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$894 =
    _field$2149;
  moonbit_string_t _field$2148 = _casted_env$1989->$1;
  moonbit_string_t file_name$905 = _field$2148;
  moonbit_string_t _field$2147 = _casted_env$1989->$0;
  int32_t _cnt$2425 = Moonbit_object_header(_casted_env$1989)->rc;
  moonbit_string_t name$907;
  if (_cnt$2425 > 1) {
    int32_t _new_cnt$2426;
    moonbit_incref(handle_result$894);
    moonbit_incref(file_name$905);
    moonbit_incref(_field$2147);
    _new_cnt$2426 = _cnt$2425 - 1;
    Moonbit_object_header(_casted_env$1989)->rc = _new_cnt$2426;
  } else if (_cnt$2425 == 1) {
    moonbit_free(_casted_env$1989);
  }
  name$907 = _field$2147;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$894,
      name$907,
      file_name$905,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$1972,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$932,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$933
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$1973 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$1972;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2154 =
    _casted_env$1973->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$894 =
    _field$2154;
  moonbit_string_t _field$2153 = _casted_env$1973->$3;
  moonbit_string_t file_name$905 = _field$2153;
  moonbit_string_t _field$2152 = _casted_env$1973->$2;
  moonbit_string_t name$907 = _field$2152;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2151 = _casted_env$1973->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$914 = _field$2151;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2150 =
    _casted_env$1973->$0;
  int32_t _cnt$2427 = Moonbit_object_header(_casted_env$1973)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$930;
  int32_t _async_driver$934;
  int32_t _tmp$1977;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$2551;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$1978;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$2552;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$1979;
  if (_cnt$2427 > 1) {
    int32_t _new_cnt$2428;
    moonbit_incref(handle_result$894);
    moonbit_incref(file_name$905);
    moonbit_incref(name$907);
    moonbit_incref(on_err$914);
    moonbit_incref(_field$2150);
    _new_cnt$2428 = _cnt$2427 - 1;
    Moonbit_object_header(_casted_env$1973)->rc = _new_cnt$2428;
  } else if (_cnt$2427 == 1) {
    moonbit_free(_casted_env$1973);
  }
  f$930 = _field$2150;
  _async_driver$934 = 0;
  moonbit_incref(name$907);
  _tmp$1977
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$907
  );
  moonbit_incref(_cont$932);
  _closure$2551
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$2551)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$2551->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$2551->$0 = _async_driver$934;
  _closure$2551->$1 = _cont$932;
  _closure$2551->$2 = name$907;
  _closure$2551->$3 = file_name$905;
  _closure$2551->$4 = handle_result$894;
  _tmp$1978 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$2551;
  _closure$2552
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$2552)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$2552->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$2552->$0 = _async_driver$934;
  _closure$2552->$1 = _err_cont$933;
  _closure$2552->$2 = _cont$932;
  _closure$2552->$3 = on_err$914;
  _tmp$1979 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$2552;
  f$930->code(f$930, _tmp$1977, _tmp$1978, _tmp$1979);
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$1983,
  int32_t _cont_param$952
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$1984 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$1983;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2159 =
    _casted_env$1984->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$894 =
    _field$2159;
  moonbit_string_t _field$2158 = _casted_env$1984->$3;
  moonbit_string_t file_name$905 = _field$2158;
  moonbit_string_t _field$2157 = _casted_env$1984->$2;
  moonbit_string_t name$907 = _field$2157;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2156 = _casted_env$1984->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$932 = _field$2156;
  int32_t _field$2155 = _casted_env$1984->$0;
  int32_t _cnt$2429 = Moonbit_object_header(_casted_env$1984)->rc;
  int32_t _async_driver$934;
  void* State_1$1985;
  if (_cnt$2429 > 1) {
    int32_t _new_cnt$2430;
    moonbit_incref(handle_result$894);
    moonbit_incref(file_name$905);
    moonbit_incref(name$907);
    moonbit_incref(_cont$932);
    _new_cnt$2430 = _cnt$2429 - 1;
    Moonbit_object_header(_casted_env$1984)->rc = _new_cnt$2430;
  } else if (_cnt$2429 == 1) {
    moonbit_free(_casted_env$1984);
  }
  _async_driver$934 = _field$2155;
  State_1$1985
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1
      )
    );
  Moonbit_object_header(State_1$1985)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1*)State_1$1985)->$0
  = _cont_param$952;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1*)State_1$1985)->$1
  = handle_result$894;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1*)State_1$1985)->$2
  = file_name$905;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1*)State_1$1985)->$3
  = name$907;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1*)State_1$1985)->$4
  = _cont$932;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$934, State_1$1985
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1980,
  void* _cont_param$953
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$1981 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$1980;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2163 = _casted_env$1981->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$914 = _field$2163;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2162 = _casted_env$1981->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$932 = _field$2162;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$2161 = _casted_env$1981->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$933 = _field$2161;
  int32_t _field$2160 = _casted_env$1981->$0;
  int32_t _cnt$2431 = Moonbit_object_header(_casted_env$1981)->rc;
  int32_t _async_driver$934;
  void* _try$158$1982;
  if (_cnt$2431 > 1) {
    int32_t _new_cnt$2432;
    moonbit_incref(on_err$914);
    moonbit_incref(_cont$932);
    moonbit_incref(_err_cont$933);
    _new_cnt$2432 = _cnt$2431 - 1;
    Moonbit_object_header(_casted_env$1981)->rc = _new_cnt$2432;
  } else if (_cnt$2431 == 1) {
    moonbit_free(_casted_env$1981);
  }
  _async_driver$934 = _field$2160;
  _try$158$1982
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158
      )
    );
  Moonbit_object_header(_try$158$1982)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158*)_try$158$1982)->$0
  = _cont_param$953;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158*)_try$158$1982)->$1
  = on_err$914;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158*)_try$158$1982)->$2
  = _cont$932;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158*)_try$158$1982)->$3
  = _err_cont$933;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$934, _try$158$1982
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$1974,
  void* _state$935
) {
  switch (Moonbit_object_tag(_state$935)) {
    case 0: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158* _$2a$try$158$936 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$$2a$try$158*)_state$935;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2167 = _$2a$try$158$936->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$937 = _field$2167;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2166 = _$2a$try$158$936->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$938 = _field$2166;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$2165 = _$2a$try$158$936->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$939 = _field$2165;
      void* _field$2164 = _$2a$try$158$936->$0;
      int32_t _cnt$2433 = Moonbit_object_header(_$2a$try$158$936)->rc;
      void* _try_err$940;
      void* err$942;
      void* err$944;
      int32_t _tmp$1976;
      if (_cnt$2433 > 1) {
        int32_t _new_cnt$2434;
        moonbit_incref(_err_cont$937);
        moonbit_incref(_cont$938);
        moonbit_incref(on_err$939);
        moonbit_incref(_field$2164);
        _new_cnt$2434 = _cnt$2433 - 1;
        Moonbit_object_header(_$2a$try$158$936)->rc = _new_cnt$2434;
      } else if (_cnt$2433 == 1) {
        moonbit_free(_$2a$try$158$936);
      }
      _try_err$940 = _field$2164;
      if (
        $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$939);
        moonbit_decref(_cont$938);
        err$944 = _try_err$940;
        goto $join$943;
      } else {
        moonbit_decref(_err_cont$937);
        err$942 = _try_err$940;
        goto $join$941;
      }
      goto $joinlet$2554;
      $join$943:;
      return _err_cont$937->code(_err_cont$937, err$944);
      $joinlet$2554:;
      goto $joinlet$2553;
      $join$941:;
      _tmp$1976 = on_err$939->code(on_err$939, err$942);
      _cont$938->code(_cont$938, _tmp$1976);
      $joinlet$2553:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1* _State_1$945 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$174$on_err$68$$2a$arm$166$lambda$192$State$State_1*)_state$935;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$2171 = _State_1$945->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$946 = _field$2171;
      moonbit_string_t _field$2170 = _State_1$945->$3;
      moonbit_string_t name$947 = _field$2170;
      moonbit_string_t _field$2169 = _State_1$945->$2;
      moonbit_string_t file_name$948 = _field$2169;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2168 =
        _State_1$945->$1;
      int32_t _cnt$2435 = Moonbit_object_header(_State_1$945)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$949;
      int32_t _tmp$1975;
      if (_cnt$2435 > 1) {
        int32_t _new_cnt$2436;
        moonbit_incref(_cont$946);
        moonbit_incref(name$947);
        moonbit_incref(file_name$948);
        moonbit_incref(_field$2168);
        _new_cnt$2436 = _cnt$2435 - 1;
        Moonbit_object_header(_State_1$945)->rc = _new_cnt$2436;
      } else if (_cnt$2435 == 1) {
        moonbit_free(_State_1$945);
      }
      handle_result$949 = _field$2168;
      _tmp$1975
      = handle_result$949->code(
        handle_result$949,
          name$947,
          file_name$948,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$946->code(_cont$946, _tmp$1975);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$1969,
  void* err$915
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$1970 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$1969;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$2178 =
    _casted_env$1970->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$894 =
    _field$2178;
  moonbit_string_t _field$2177 = _casted_env$1970->$1;
  moonbit_string_t file_name$905 = _field$2177;
  moonbit_string_t _field$2176 = _casted_env$1970->$0;
  int32_t _cnt$2437 = Moonbit_object_header(_casted_env$1970)->rc;
  moonbit_string_t name$907;
  void* e$917;
  moonbit_string_t e$920;
  moonbit_string_t message$918;
  if (_cnt$2437 > 1) {
    int32_t _new_cnt$2438;
    moonbit_incref(handle_result$894);
    moonbit_incref(file_name$905);
    moonbit_incref(_field$2176);
    _new_cnt$2438 = _cnt$2437 - 1;
    Moonbit_object_header(_casted_env$1970)->rc = _new_cnt$2438;
  } else if (_cnt$2437 == 1) {
    moonbit_free(_casted_env$1970);
  }
  name$907 = _field$2176;
  switch (Moonbit_object_tag(err$915)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$921 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$915;
      moonbit_string_t _field$2172 = _Failure$921->$0;
      int32_t _cnt$2439 = Moonbit_object_header(_Failure$921)->rc;
      moonbit_string_t _e$922;
      if (_cnt$2439 > 1) {
        int32_t _new_cnt$2440;
        moonbit_incref(_field$2172);
        _new_cnt$2440 = _cnt$2439 - 1;
        Moonbit_object_header(_Failure$921)->rc = _new_cnt$2440;
      } else if (_cnt$2439 == 1) {
        moonbit_free(_Failure$921);
      }
      _e$922 = _field$2172;
      e$920 = _e$922;
      goto $join$919;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$923 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$915;
      moonbit_string_t _field$2173 = _InspectError$923->$0;
      int32_t _cnt$2441 = Moonbit_object_header(_InspectError$923)->rc;
      moonbit_string_t _e$924;
      if (_cnt$2441 > 1) {
        int32_t _new_cnt$2442;
        moonbit_incref(_field$2173);
        _new_cnt$2442 = _cnt$2441 - 1;
        Moonbit_object_header(_InspectError$923)->rc = _new_cnt$2442;
      } else if (_cnt$2441 == 1) {
        moonbit_free(_InspectError$923);
      }
      _e$924 = _field$2173;
      e$920 = _e$924;
      goto $join$919;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$925 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$915;
      moonbit_string_t _field$2174 = _SnapshotError$925->$0;
      int32_t _cnt$2443 = Moonbit_object_header(_SnapshotError$925)->rc;
      moonbit_string_t _e$926;
      if (_cnt$2443 > 1) {
        int32_t _new_cnt$2444;
        moonbit_incref(_field$2174);
        _new_cnt$2444 = _cnt$2443 - 1;
        Moonbit_object_header(_SnapshotError$925)->rc = _new_cnt$2444;
      } else if (_cnt$2443 == 1) {
        moonbit_free(_SnapshotError$925);
      }
      _e$926 = _field$2174;
      e$920 = _e$926;
      goto $join$919;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$927 =
        (struct $Error$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$915;
      moonbit_string_t _field$2175 =
        _MoonBitTestDriverInternalJsError$927->$0;
      int32_t _cnt$2445 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$927)->rc;
      moonbit_string_t _e$928;
      if (_cnt$2445 > 1) {
        int32_t _new_cnt$2446;
        moonbit_incref(_field$2175);
        _new_cnt$2446 = _cnt$2445 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$927)->rc
        = _new_cnt$2446;
      } else if (_cnt$2445 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$927);
      }
      _e$928 = _field$2175;
      e$920 = _e$928;
      goto $join$919;
      break;
    }
    default: {
      e$917 = err$915;
      goto $join$916;
      break;
    }
  }
  goto $joinlet$2556;
  $join$919:;
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$894, name$907, file_name$905, e$920, 0
  );
  $joinlet$2556:;
  goto $joinlet$2555;
  $join$916:;
  message$918 = $Error$to_string(e$917);
  $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$894, name$907, file_name$905, message$918, 0
  );
  $joinlet$2555:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1955,
  moonbit_string_t attr$908
) {
  int32_t _tmp$1957;
  int64_t _tmp$1956;
  moonbit_decref(_env$1955);
  _tmp$1957 = Moonbit_array_length(attr$908);
  _tmp$1956 = (int64_t)_tmp$1957;
  moonbit_incref(attr$908);
  if ($String$$char_length_ge$inner(attr$908, 5, 0, _tmp$1956)) {
    int32_t _tmp$1962 = attr$908[0];
    int32_t _x$909 = _tmp$1962;
    if (_x$909 == 112) {
      int32_t _tmp$1961 = attr$908[1];
      int32_t _x$910 = _tmp$1961;
      if (_x$910 == 97) {
        int32_t _tmp$1960 = attr$908[2];
        int32_t _x$911 = _tmp$1960;
        if (_x$911 == 110) {
          int32_t _tmp$1959 = attr$908[3];
          int32_t _x$912 = _tmp$1959;
          if (_x$912 == 105) {
            int32_t _tmp$2179 = attr$908[4];
            int32_t _tmp$1958;
            int32_t _x$913;
            moonbit_decref(attr$908);
            _tmp$1958 = _tmp$2179;
            _x$913 = _tmp$1958;
            return _x$913 == 99 || 0;
          } else {
            moonbit_decref(attr$908);
            return 0;
          }
        } else {
          moonbit_decref(attr$908);
          return 0;
        }
      } else {
        moonbit_decref(attr$908);
        return 0;
      }
    } else {
      moonbit_decref(attr$908);
      return 0;
    }
  } else {
    moonbit_decref(attr$908);
    return 0;
  }
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$1939,
  moonbit_string_t test_name$895,
  moonbit_string_t file_name$896,
  moonbit_string_t message$897,
  int32_t skipped$898
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$1940 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$1939;
  int32_t _field$2180 = _casted_env$1940->$0;
  int32_t index$893;
  int32_t _if_result$2557;
  moonbit_string_t file_name$899;
  moonbit_string_t test_name$900;
  moonbit_string_t message$901;
  moonbit_string_t _tmp$1952;
  moonbit_string_t _tmp$1951;
  moonbit_string_t _tmp$1949;
  moonbit_string_t _tmp$1950;
  moonbit_string_t _tmp$1948;
  moonbit_string_t _tmp$1946;
  moonbit_string_t _tmp$1947;
  moonbit_string_t _tmp$1945;
  moonbit_string_t _tmp$1943;
  moonbit_string_t _tmp$1944;
  moonbit_string_t _tmp$1942;
  moonbit_string_t _tmp$1941;
  moonbit_decref(_casted_env$1940);
  index$893 = _field$2180;
  if (!skipped$898) {
    _if_result$2557 = 1;
  } else {
    _if_result$2557 = 0;
  }
  if (_if_result$2557) {
    
  }
  file_name$899 = $String$$escape(file_name$896);
  test_name$900 = $String$$escape(test_name$895);
  message$901 = $String$$escape(message$897);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$1952
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$899
  );
  _tmp$1951
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$1952
  );
  _tmp$1949
  = moonbit_add_string(
    _tmp$1951, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$1950
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$893
  );
  _tmp$1948 = moonbit_add_string(_tmp$1949, _tmp$1950);
  _tmp$1946
  = moonbit_add_string(
    _tmp$1948, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$1947
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$900
  );
  _tmp$1945 = moonbit_add_string(_tmp$1946, _tmp$1947);
  _tmp$1943
  = moonbit_add_string(
    _tmp$1945, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$1944 = $$moonbitlang$core$builtin$Show$$String$$to_string(message$901);
  _tmp$1942 = moonbit_add_string(_tmp$1943, _tmp$1944);
  _tmp$1941
  = moonbit_add_string(
    _tmp$1942, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$1941);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$890
) {
  moonbit_decref(_discard_$890);
  return 42;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$888,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$889,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$886
) {
  void* _try_err$884;
  struct moonbit_result_0 _tmp$2559 = f$888->code(f$888);
  void* err$885;
  if (_tmp$2559.tag) {
    int32_t const _ok$1937 = _tmp$2559.data.ok;
    moonbit_decref(on_err$886);
  } else {
    void* const _err$1938 = _tmp$2559.data.err;
    moonbit_decref(on_ok$889);
    _try_err$884 = _err$1938;
    goto $join$883;
  }
  on_ok$889->code(on_ok$889);
  goto $joinlet$2558;
  $join$883:;
  err$885 = _try_err$884;
  on_err$886->code(on_err$886, err$885);
  $joinlet$2558:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$849,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$862,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$875,
  moonbit_string_t file_filter$846,
  int32_t index_filter$847
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$843;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$844;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$848;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2186;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1928;
  void* F0$1925;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2185;
  int32_t _cnt$2447;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1927;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1926;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$845;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$858;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$859;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$861;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2184;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$1932;
  void* F1$1929;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2183;
  int32_t _cnt$2450;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1931;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1930;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$860;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$871;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$872;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$874;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2182;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1936;
  void* F2$1933;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$2181;
  int32_t _cnt$2453;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1935;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$1934;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$873;
  moonbit_incref(file_filter$846);
  _bind$848
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$849, file_filter$846
  );
  if (_bind$848 == 0) {
    if (_bind$848) {
      moonbit_decref(_bind$848);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$850 =
      _bind$848;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$851 =
      _Some$850;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$853;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$854;
    moonbit_incref(_index_func_map$851);
    _bind$854
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$851, index_filter$847
    );
    if (_bind$854 == 0) {
      if (_bind$854) {
        moonbit_decref(_bind$854);
      }
      moonbit_decref(_index_func_map$851);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$855;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$856;
      moonbit_decref(async_tests$875);
      moonbit_decref(with_args_tests$862);
      _Some$855 = _bind$854;
      _func_attrs_tuple$856 = _Some$855;
      func_attrs_tuple$853 = _func_attrs_tuple$856;
      goto $join$852;
    }
    goto $joinlet$2561;
    $join$852:;
    index_func_map$843 = _index_func_map$851;
    func_attrs_tuple$844 = func_attrs_tuple$853;
    goto $join$842;
    $joinlet$2561:;
  }
  goto $joinlet$2560;
  $join$842:;
  moonbit_decref(index_func_map$843);
  _field$2186 = func_attrs_tuple$844->$0;
  _tmp$1928 = _field$2186;
  moonbit_incref(_tmp$1928);
  F0$1925
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$1925)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$1925)->$0 = _tmp$1928;
  _field$2185 = func_attrs_tuple$844->$1;
  _cnt$2447 = Moonbit_object_header(func_attrs_tuple$844)->rc;
  if (_cnt$2447 > 1) {
    int32_t _new_cnt$2449;
    moonbit_incref(_field$2185);
    _new_cnt$2449 = _cnt$2447 - 1;
    Moonbit_object_header(func_attrs_tuple$844)->rc = _new_cnt$2449;
  } else if (_cnt$2447 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2448 =
      func_attrs_tuple$844->$0;
    moonbit_decref(_field$2448);
    moonbit_free(func_attrs_tuple$844);
  }
  _tmp$1927 = _field$2185;
  _tmp$1926
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1926)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1926->$0 = file_filter$846;
  _tmp$1926->$1 = index_filter$847;
  _tmp$1926->$2 = _tmp$1927;
  k$845
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$845)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$845->$0 = F0$1925;
  k$845->$1 = _tmp$1926;
  return k$845;
  $joinlet$2560:;
  moonbit_incref(file_filter$846);
  _bind$861
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$862, file_filter$846
  );
  if (_bind$861 == 0) {
    if (_bind$861) {
      moonbit_decref(_bind$861);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$863 =
      _bind$861;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$864 =
      _Some$863;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$866;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$867;
    moonbit_incref(_index_func_map$864);
    _bind$867
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$864, index_filter$847
    );
    if (_bind$867 == 0) {
      if (_bind$867) {
        moonbit_decref(_bind$867);
      }
      moonbit_decref(_index_func_map$864);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$868;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$869;
      moonbit_decref(async_tests$875);
      _Some$868 = _bind$867;
      _func_attrs_tuple$869 = _Some$868;
      func_attrs_tuple$866 = _func_attrs_tuple$869;
      goto $join$865;
    }
    goto $joinlet$2563;
    $join$865:;
    index_func_map$858 = _index_func_map$864;
    func_attrs_tuple$859 = func_attrs_tuple$866;
    goto $join$857;
    $joinlet$2563:;
  }
  goto $joinlet$2562;
  $join$857:;
  moonbit_decref(index_func_map$858);
  _field$2184 = func_attrs_tuple$859->$0;
  _tmp$1932 = _field$2184;
  moonbit_incref(_tmp$1932);
  F1$1929
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$1929)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$1929)->$0 = _tmp$1932;
  _field$2183 = func_attrs_tuple$859->$1;
  _cnt$2450 = Moonbit_object_header(func_attrs_tuple$859)->rc;
  if (_cnt$2450 > 1) {
    int32_t _new_cnt$2452;
    moonbit_incref(_field$2183);
    _new_cnt$2452 = _cnt$2450 - 1;
    Moonbit_object_header(func_attrs_tuple$859)->rc = _new_cnt$2452;
  } else if (_cnt$2450 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$2451 =
      func_attrs_tuple$859->$0;
    moonbit_decref(_field$2451);
    moonbit_free(func_attrs_tuple$859);
  }
  _tmp$1931 = _field$2183;
  _tmp$1930
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1930)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1930->$0 = file_filter$846;
  _tmp$1930->$1 = index_filter$847;
  _tmp$1930->$2 = _tmp$1931;
  k$860
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$860)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$860->$0 = F1$1929;
  k$860->$1 = _tmp$1930;
  return k$860;
  $joinlet$2562:;
  moonbit_incref(file_filter$846);
  _bind$874
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$875, file_filter$846
  );
  if (_bind$874 == 0) {
    if (_bind$874) {
      moonbit_decref(_bind$874);
    }
    moonbit_decref(file_filter$846);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$876 =
      _bind$874;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$877 =
      _Some$876;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$879;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$880;
    moonbit_incref(_index_func_map$877);
    _bind$880
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$877, index_filter$847
    );
    if (_bind$880 == 0) {
      if (_bind$880) {
        moonbit_decref(_bind$880);
      }
      moonbit_decref(_index_func_map$877);
      moonbit_decref(file_filter$846);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$881 =
        _bind$880;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$882 =
        _Some$881;
      func_attrs_tuple$879 = _func_attrs_tuple$882;
      goto $join$878;
    }
    goto $joinlet$2565;
    $join$878:;
    index_func_map$871 = _index_func_map$877;
    func_attrs_tuple$872 = func_attrs_tuple$879;
    goto $join$870;
    $joinlet$2565:;
  }
  goto $joinlet$2564;
  $join$870:;
  moonbit_decref(index_func_map$871);
  _field$2182 = func_attrs_tuple$872->$0;
  _tmp$1936 = _field$2182;
  moonbit_incref(_tmp$1936);
  F2$1933
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$1933)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$1933)->$0 = _tmp$1936;
  _field$2181 = func_attrs_tuple$872->$1;
  _cnt$2453 = Moonbit_object_header(func_attrs_tuple$872)->rc;
  if (_cnt$2453 > 1) {
    int32_t _new_cnt$2455;
    moonbit_incref(_field$2181);
    _new_cnt$2455 = _cnt$2453 - 1;
    Moonbit_object_header(func_attrs_tuple$872)->rc = _new_cnt$2455;
  } else if (_cnt$2453 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$2454 =
      func_attrs_tuple$872->$0;
    moonbit_decref(_field$2454);
    moonbit_free(func_attrs_tuple$872);
  }
  _tmp$1935 = _field$2181;
  _tmp$1934
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$1934)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$1934->$0 = file_filter$846;
  _tmp$1934->$1 = index_filter$847;
  _tmp$1934->$2 = _tmp$1935;
  k$873
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$873)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$873->$0 = F2$1933;
  k$873->$1 = _tmp$1934;
  return k$873;
  $joinlet$2564:;
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$834
) {
  int32_t _field$2187 = self$834->$1;
  int32_t len$1924;
  moonbit_decref(self$834);
  len$1924 = _field$2187;
  return len$1924 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$832,
  struct $$moonbitlang$core$builtin$Logger logger$833
) {
  moonbit_string_t _tmp$1923 = self$832;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$1922 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$1923);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$1922, logger$833
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$818,
  struct $$moonbitlang$core$builtin$Logger logger$831
) {
  struct $StringView _field$2196 =
    (struct $StringView){self$818->$0_1, self$818->$0_2, self$818->$0_0};
  struct $StringView pkg$817 = _field$2196;
  int32_t _tmp$1921 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$1920;
  int64_t _bind$819;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$820;
  struct $StringView _field$2195;
  struct $StringView _module_name$827;
  void* _field$2194;
  int32_t _cnt$2456;
  void* _package_name$828;
  struct $StringView _field$2192;
  struct $StringView filename$1903;
  struct $StringView _field$2191;
  struct $StringView start_line$1904;
  struct $StringView _field$2190;
  struct $StringView start_column$1905;
  struct $StringView _field$2189;
  struct $StringView end_line$1906;
  struct $StringView _field$2188;
  int32_t _cnt$2460;
  struct $StringView end_column$1907;
  struct $$moonbitlang$core$builtin$Logger _bind$1902;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$1920
  = (struct $StringView){
    0, _tmp$1921, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$817.$0);
  moonbit_incref(pkg$817.$0);
  _bind$819 = $StringView$$find(pkg$817, _tmp$1920);
  if (_bind$819 == 4294967296ll) {
    void* None$1908 =
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
    _bind$820->$1 = None$1908;
  } else {
    int64_t _Some$821 = _bind$819;
    int32_t _first_slash$822 = (int32_t)_Some$821;
    int32_t _tmp$1919 = _first_slash$822 + 1;
    struct $StringView _tmp$1916;
    int32_t _tmp$1918;
    struct $StringView _tmp$1917;
    int64_t _bind$823;
    moonbit_incref(pkg$817.$0);
    _tmp$1916 = $StringView$$view$inner(pkg$817, _tmp$1919, 4294967296ll);
    _tmp$1918
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$1917
    = (struct $StringView){
      0, _tmp$1918, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$823 = $StringView$$find(_tmp$1916, _tmp$1917);
    if (_bind$823 == 4294967296ll) {
      void* None$1909 =
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
      _bind$820->$1 = None$1909;
    } else {
      int64_t _Some$824 = _bind$823;
      int32_t _second_slash$825 = (int32_t)_Some$824;
      int32_t _tmp$1915 = _first_slash$822 + 1;
      int32_t module_name_end$826 = _tmp$1915 + _second_slash$825;
      int64_t _tmp$1914 = (int64_t)module_name_end$826;
      struct $StringView _tmp$1910;
      int32_t _tmp$1913;
      struct $StringView _tmp$1912;
      void* Some$1911;
      moonbit_incref(pkg$817.$0);
      _tmp$1910 = $StringView$$view$inner(pkg$817, 0, _tmp$1914);
      _tmp$1913 = module_name_end$826 + 1;
      _tmp$1912 = $StringView$$view$inner(pkg$817, _tmp$1913, 4294967296ll);
      Some$1911
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$1911)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$1911)->$0_0
      = _tmp$1912.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1911)->$0_1
      = _tmp$1912.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$1911)->$0_2
      = _tmp$1912.$2;
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
      _bind$820->$0_0 = _tmp$1910.$0;
      _bind$820->$0_1 = _tmp$1910.$1;
      _bind$820->$0_2 = _tmp$1910.$2;
      _bind$820->$1 = Some$1911;
    }
  }
  _field$2195
  = (struct $StringView){
    _bind$820->$0_1, _bind$820->$0_2, _bind$820->$0_0
  };
  _module_name$827 = _field$2195;
  _field$2194 = _bind$820->$1;
  _cnt$2456 = Moonbit_object_header(_bind$820)->rc;
  if (_cnt$2456 > 1) {
    int32_t _new_cnt$2457;
    moonbit_incref(_field$2194);
    moonbit_incref(_module_name$827.$0);
    _new_cnt$2457 = _cnt$2456 - 1;
    Moonbit_object_header(_bind$820)->rc = _new_cnt$2457;
  } else if (_cnt$2456 == 1) {
    moonbit_free(_bind$820);
  }
  _package_name$828 = _field$2194;
  switch (Moonbit_object_tag(_package_name$828)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$829 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$828;
      struct $StringView _field$2193 =
        (struct $StringView){
          _Some$829->$0_1, _Some$829->$0_2, _Some$829->$0_0
        };
      int32_t _cnt$2458 = Moonbit_object_header(_Some$829)->rc;
      struct $StringView _pkg_name$830;
      struct $$moonbitlang$core$builtin$Logger _bind$1901;
      if (_cnt$2458 > 1) {
        int32_t _new_cnt$2459;
        moonbit_incref(_field$2193.$0);
        _new_cnt$2459 = _cnt$2458 - 1;
        Moonbit_object_header(_Some$829)->rc = _new_cnt$2459;
      } else if (_cnt$2458 == 1) {
        moonbit_free(_Some$829);
      }
      _pkg_name$830 = _field$2193;
      if (logger$831.$1) {
        moonbit_incref(logger$831.$1);
      }
      logger$831.$0->$method_2(logger$831.$1, _pkg_name$830);
      _bind$1901 = logger$831;
      if (_bind$1901.$1) {
        moonbit_incref(_bind$1901.$1);
      }
      _bind$1901.$0->$method_3(_bind$1901.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$828);
      break;
    }
  }
  _field$2192
  = (struct $StringView){
    self$818->$1_1, self$818->$1_2, self$818->$1_0
  };
  filename$1903 = _field$2192;
  moonbit_incref(filename$1903.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, filename$1903);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2191
  = (struct $StringView){
    self$818->$2_1, self$818->$2_2, self$818->$2_0
  };
  start_line$1904 = _field$2191;
  moonbit_incref(start_line$1904.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, start_line$1904);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2190
  = (struct $StringView){
    self$818->$3_1, self$818->$3_2, self$818->$3_0
  };
  start_column$1905 = _field$2190;
  moonbit_incref(start_column$1905.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, start_column$1905);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 45);
  _field$2189
  = (struct $StringView){
    self$818->$4_1, self$818->$4_2, self$818->$4_0
  };
  end_line$1906 = _field$2189;
  moonbit_incref(end_line$1906.$0);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, end_line$1906);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 58);
  _field$2188
  = (struct $StringView){
    self$818->$5_1, self$818->$5_2, self$818->$5_0
  };
  _cnt$2460 = Moonbit_object_header(self$818)->rc;
  if (_cnt$2460 > 1) {
    int32_t _new_cnt$2466;
    moonbit_incref(_field$2188.$0);
    _new_cnt$2466 = _cnt$2460 - 1;
    Moonbit_object_header(self$818)->rc = _new_cnt$2466;
  } else if (_cnt$2460 == 1) {
    struct $StringView _field$2465 =
      (struct $StringView){self$818->$4_1, self$818->$4_2, self$818->$4_0};
    struct $StringView _field$2464;
    struct $StringView _field$2463;
    struct $StringView _field$2462;
    struct $StringView _field$2461;
    moonbit_decref(_field$2465.$0);
    _field$2464
    = (struct $StringView){
      self$818->$3_1, self$818->$3_2, self$818->$3_0
    };
    moonbit_decref(_field$2464.$0);
    _field$2463
    = (struct $StringView){
      self$818->$2_1, self$818->$2_2, self$818->$2_0
    };
    moonbit_decref(_field$2463.$0);
    _field$2462
    = (struct $StringView){
      self$818->$1_1, self$818->$1_2, self$818->$1_0
    };
    moonbit_decref(_field$2462.$0);
    _field$2461
    = (struct $StringView){
      self$818->$0_1, self$818->$0_2, self$818->$0_0
    };
    moonbit_decref(_field$2461.$0);
    moonbit_free(self$818);
  }
  end_column$1907 = _field$2188;
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_2(logger$831.$1, end_column$1907);
  if (logger$831.$1) {
    moonbit_incref(logger$831.$1);
  }
  logger$831.$0->$method_3(logger$831.$1, 64);
  _bind$1902 = logger$831;
  _bind$1902.$0->$method_2(_bind$1902.$1, _module_name$827);
  return 0;
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$816) {
  moonbit_string_t _tmp$1900 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$816);
  moonbit_println(_tmp$1900);
  moonbit_decref(_tmp$1900);
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
      int32_t _tmp$1898 = value$808[i$809];
      uint32_t _tmp$1897 = *(uint32_t*)&_tmp$1898;
      int32_t _tmp$1899;
      moonbit_incref(self$810);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$810, _tmp$1897);
      _tmp$1899 = i$809 + 1;
      i$809 = _tmp$1899;
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
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$2567 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$1894;
  int32_t _tmp$1893;
  Moonbit_object_header(_closure$2567)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2567->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$2567->$0 = f$806;
  _tmp$1894 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$2567;
  _tmp$1893 = $$moonbitlang$core$builtin$Iter$$run$0(self$804, _tmp$1894);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$1893, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$1895,
  moonbit_string_t k$805
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$1896 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$1895;
  struct $$3c$String$3e$$3d$$3e$Int* _field$2197 = _casted_env$1896->$0;
  int32_t _cnt$2467 = Moonbit_object_header(_casted_env$1896)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$806;
  if (_cnt$2467 > 1) {
    int32_t _new_cnt$2468;
    moonbit_incref(_field$2197);
    _new_cnt$2468 = _cnt$2467 - 1;
    Moonbit_object_header(_casted_env$1896)->rc = _new_cnt$2468;
  } else if (_cnt$2467 == 1) {
    moonbit_free(_casted_env$1896);
  }
  f$806 = _field$2197;
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
  moonbit_string_t* _tmp$1892 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$802);
  moonbit_string_t _tmp$2198;
  if (idx$803 < 0 || idx$803 >= Moonbit_array_length(_tmp$1892)) {
    moonbit_panic();
  }
  _tmp$2198 = (moonbit_string_t)_tmp$1892[idx$803];
  moonbit_incref(_tmp$2198);
  moonbit_decref(_tmp$1892);
  return _tmp$2198;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$800,
  int32_t idx$801
) {
  struct $$3c$String$2a$Int$3e$** _tmp$1891 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$800);
  struct $$3c$String$2a$Int$3e$* _tmp$2199;
  if (idx$801 < 0 || idx$801 >= Moonbit_array_length(_tmp$1891)) {
    moonbit_panic();
  }
  _tmp$2199 = (struct $$3c$String$2a$Int$3e$*)_tmp$1891[idx$801];
  if (_tmp$2199) {
    moonbit_incref(_tmp$2199);
  }
  moonbit_decref(_tmp$1891);
  return _tmp$2199;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$796,
  int32_t key$792
) {
  int32_t hash$791 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$792);
  int32_t capacity_mask$1890 = self$796->$3;
  int32_t _tmp$1889 = hash$791 & capacity_mask$1890;
  int32_t i$793 = 0;
  int32_t idx$794 = _tmp$1889;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2203 =
      self$796->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1888 =
      _field$2203;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2202;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$795;
    if (idx$794 < 0 || idx$794 >= Moonbit_array_length(entries$1888)) {
      moonbit_panic();
    }
    _tmp$2202
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1888[
        idx$794
      ];
    _bind$795 = _tmp$2202;
    if (_bind$795 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1877;
      if (_bind$795) {
        moonbit_incref(_bind$795);
      }
      moonbit_decref(self$796);
      if (_bind$795) {
        moonbit_decref(_bind$795);
      }
      _tmp$1877 = 0;
      return _tmp$1877;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$797 =
        _bind$795;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$798 =
        _Some$797;
      int32_t hash$1879 = _entry$798->$3;
      int32_t _if_result$2569;
      int32_t _field$2200;
      int32_t psl$1882;
      int32_t _tmp$1884;
      int32_t _tmp$1886;
      int32_t capacity_mask$1887;
      int32_t _tmp$1885;
      if (hash$1879 == hash$791) {
        int32_t key$1878 = _entry$798->$4;
        _if_result$2569 = key$1878 == key$792;
      } else {
        _if_result$2569 = 0;
      }
      if (_if_result$2569) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2201;
        int32_t _cnt$2469;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1881;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1880;
        moonbit_incref(_entry$798);
        moonbit_decref(self$796);
        _field$2201 = _entry$798->$5;
        _cnt$2469 = Moonbit_object_header(_entry$798)->rc;
        if (_cnt$2469 > 1) {
          int32_t _new_cnt$2471;
          moonbit_incref(_field$2201);
          _new_cnt$2471 = _cnt$2469 - 1;
          Moonbit_object_header(_entry$798)->rc = _new_cnt$2471;
        } else if (_cnt$2469 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2470 =
            _entry$798->$1;
          if (_field$2470) {
            moonbit_decref(_field$2470);
          }
          moonbit_free(_entry$798);
        }
        value$1881 = _field$2201;
        _tmp$1880 = value$1881;
        return _tmp$1880;
      } else {
        moonbit_incref(_entry$798);
      }
      _field$2200 = _entry$798->$2;
      moonbit_decref(_entry$798);
      psl$1882 = _field$2200;
      if (i$793 > psl$1882) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1883;
        moonbit_decref(self$796);
        _tmp$1883 = 0;
        return _tmp$1883;
      }
      _tmp$1884 = i$793 + 1;
      _tmp$1886 = idx$794 + 1;
      capacity_mask$1887 = self$796->$3;
      _tmp$1885 = _tmp$1886 & capacity_mask$1887;
      i$793 = _tmp$1884;
      idx$794 = _tmp$1885;
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
  int32_t capacity_mask$1876;
  int32_t _tmp$1875;
  int32_t i$784;
  int32_t idx$785;
  moonbit_incref(key$783);
  hash$782 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$783);
  capacity_mask$1876 = self$787->$3;
  _tmp$1875 = hash$782 & capacity_mask$1876;
  i$784 = 0;
  idx$785 = _tmp$1875;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2209 =
      self$787->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1874 =
      _field$2209;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2208;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$786;
    if (idx$785 < 0 || idx$785 >= Moonbit_array_length(entries$1874)) {
      moonbit_panic();
    }
    _tmp$2208
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1874[
        idx$785
      ];
    _bind$786 = _tmp$2208;
    if (_bind$786 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1863;
      if (_bind$786) {
        moonbit_incref(_bind$786);
      }
      moonbit_decref(self$787);
      if (_bind$786) {
        moonbit_decref(_bind$786);
      }
      moonbit_decref(key$783);
      _tmp$1863 = 0;
      return _tmp$1863;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$788 =
        _bind$786;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$789 =
        _Some$788;
      int32_t hash$1865 = _entry$789->$3;
      int32_t _if_result$2571;
      int32_t _field$2204;
      int32_t psl$1868;
      int32_t _tmp$1870;
      int32_t _tmp$1872;
      int32_t capacity_mask$1873;
      int32_t _tmp$1871;
      if (hash$1865 == hash$782) {
        moonbit_string_t _field$2207 = _entry$789->$4;
        moonbit_string_t key$1864 = _field$2207;
        int32_t _tmp$2206 = moonbit_val_array_equal(key$1864, key$783);
        _if_result$2571 = _tmp$2206;
      } else {
        _if_result$2571 = 0;
      }
      if (_if_result$2571) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2205;
        int32_t _cnt$2472;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1867;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1866;
        moonbit_incref(_entry$789);
        moonbit_decref(self$787);
        moonbit_decref(key$783);
        _field$2205 = _entry$789->$5;
        _cnt$2472 = Moonbit_object_header(_entry$789)->rc;
        if (_cnt$2472 > 1) {
          int32_t _new_cnt$2475;
          moonbit_incref(_field$2205);
          _new_cnt$2475 = _cnt$2472 - 1;
          Moonbit_object_header(_entry$789)->rc = _new_cnt$2475;
        } else if (_cnt$2472 == 1) {
          moonbit_string_t _field$2474 = _entry$789->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2473;
          moonbit_decref(_field$2474);
          _field$2473 = _entry$789->$1;
          if (_field$2473) {
            moonbit_decref(_field$2473);
          }
          moonbit_free(_entry$789);
        }
        value$1867 = _field$2205;
        _tmp$1866 = value$1867;
        return _tmp$1866;
      } else {
        moonbit_incref(_entry$789);
      }
      _field$2204 = _entry$789->$2;
      moonbit_decref(_entry$789);
      psl$1868 = _field$2204;
      if (i$784 > psl$1868) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1869;
        moonbit_decref(self$787);
        moonbit_decref(key$783);
        _tmp$1869 = 0;
        return _tmp$1869;
      }
      _tmp$1870 = i$784 + 1;
      _tmp$1872 = idx$785 + 1;
      capacity_mask$1873 = self$787->$3;
      _tmp$1871 = _tmp$1872 & capacity_mask$1873;
      i$784 = _tmp$1870;
      idx$785 = _tmp$1871;
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
  int32_t capacity_mask$1862 = self$778->$3;
  int32_t _tmp$1861 = hash$773 & capacity_mask$1862;
  int32_t i$775 = 0;
  int32_t idx$776 = _tmp$1861;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2213 =
      self$778->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1860 =
      _field$2213;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2212;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$777;
    if (idx$776 < 0 || idx$776 >= Moonbit_array_length(entries$1860)) {
      moonbit_panic();
    }
    _tmp$2212
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1860[
        idx$776
      ];
    _bind$777 = _tmp$2212;
    if (_bind$777 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1849;
      if (_bind$777) {
        moonbit_incref(_bind$777);
      }
      moonbit_decref(self$778);
      if (_bind$777) {
        moonbit_decref(_bind$777);
      }
      _tmp$1849 = 0;
      return _tmp$1849;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$779 =
        _bind$777;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$780 =
        _Some$779;
      int32_t hash$1851 = _entry$780->$3;
      int32_t _if_result$2573;
      int32_t _field$2210;
      int32_t psl$1854;
      int32_t _tmp$1856;
      int32_t _tmp$1858;
      int32_t capacity_mask$1859;
      int32_t _tmp$1857;
      if (hash$1851 == hash$773) {
        int32_t key$1850 = _entry$780->$4;
        _if_result$2573 = key$1850 == key$774;
      } else {
        _if_result$2573 = 0;
      }
      if (_if_result$2573) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2211;
        int32_t _cnt$2476;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1853;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1852;
        moonbit_incref(_entry$780);
        moonbit_decref(self$778);
        _field$2211 = _entry$780->$5;
        _cnt$2476 = Moonbit_object_header(_entry$780)->rc;
        if (_cnt$2476 > 1) {
          int32_t _new_cnt$2478;
          moonbit_incref(_field$2211);
          _new_cnt$2478 = _cnt$2476 - 1;
          Moonbit_object_header(_entry$780)->rc = _new_cnt$2478;
        } else if (_cnt$2476 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2477 =
            _entry$780->$1;
          if (_field$2477) {
            moonbit_decref(_field$2477);
          }
          moonbit_free(_entry$780);
        }
        value$1853 = _field$2211;
        _tmp$1852 = value$1853;
        return _tmp$1852;
      } else {
        moonbit_incref(_entry$780);
      }
      _field$2210 = _entry$780->$2;
      moonbit_decref(_entry$780);
      psl$1854 = _field$2210;
      if (i$775 > psl$1854) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1855;
        moonbit_decref(self$778);
        _tmp$1855 = 0;
        return _tmp$1855;
      }
      _tmp$1856 = i$775 + 1;
      _tmp$1858 = idx$776 + 1;
      capacity_mask$1859 = self$778->$3;
      _tmp$1857 = _tmp$1858 & capacity_mask$1859;
      i$775 = _tmp$1856;
      idx$776 = _tmp$1857;
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
  int32_t capacity_mask$1848;
  int32_t _tmp$1847;
  int32_t i$766;
  int32_t idx$767;
  moonbit_incref(key$765);
  hash$764 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$765);
  capacity_mask$1848 = self$769->$3;
  _tmp$1847 = hash$764 & capacity_mask$1848;
  i$766 = 0;
  idx$767 = _tmp$1847;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2219 =
      self$769->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1846 =
      _field$2219;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2218;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$768;
    if (idx$767 < 0 || idx$767 >= Moonbit_array_length(entries$1846)) {
      moonbit_panic();
    }
    _tmp$2218
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1846[
        idx$767
      ];
    _bind$768 = _tmp$2218;
    if (_bind$768 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1835;
      if (_bind$768) {
        moonbit_incref(_bind$768);
      }
      moonbit_decref(self$769);
      if (_bind$768) {
        moonbit_decref(_bind$768);
      }
      moonbit_decref(key$765);
      _tmp$1835 = 0;
      return _tmp$1835;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$770 =
        _bind$768;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$771 =
        _Some$770;
      int32_t hash$1837 = _entry$771->$3;
      int32_t _if_result$2575;
      int32_t _field$2214;
      int32_t psl$1840;
      int32_t _tmp$1842;
      int32_t _tmp$1844;
      int32_t capacity_mask$1845;
      int32_t _tmp$1843;
      if (hash$1837 == hash$764) {
        moonbit_string_t _field$2217 = _entry$771->$4;
        moonbit_string_t key$1836 = _field$2217;
        int32_t _tmp$2216 = moonbit_val_array_equal(key$1836, key$765);
        _if_result$2575 = _tmp$2216;
      } else {
        _if_result$2575 = 0;
      }
      if (_if_result$2575) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2215;
        int32_t _cnt$2479;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1839;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1838;
        moonbit_incref(_entry$771);
        moonbit_decref(self$769);
        moonbit_decref(key$765);
        _field$2215 = _entry$771->$5;
        _cnt$2479 = Moonbit_object_header(_entry$771)->rc;
        if (_cnt$2479 > 1) {
          int32_t _new_cnt$2482;
          moonbit_incref(_field$2215);
          _new_cnt$2482 = _cnt$2479 - 1;
          Moonbit_object_header(_entry$771)->rc = _new_cnt$2482;
        } else if (_cnt$2479 == 1) {
          moonbit_string_t _field$2481 = _entry$771->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2480;
          moonbit_decref(_field$2481);
          _field$2480 = _entry$771->$1;
          if (_field$2480) {
            moonbit_decref(_field$2480);
          }
          moonbit_free(_entry$771);
        }
        value$1839 = _field$2215;
        _tmp$1838 = value$1839;
        return _tmp$1838;
      } else {
        moonbit_incref(_entry$771);
      }
      _field$2214 = _entry$771->$2;
      moonbit_decref(_entry$771);
      psl$1840 = _field$2214;
      if (i$766 > psl$1840) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1841;
        moonbit_decref(self$769);
        moonbit_decref(key$765);
        _tmp$1841 = 0;
        return _tmp$1841;
      }
      _tmp$1842 = i$766 + 1;
      _tmp$1844 = idx$767 + 1;
      capacity_mask$1845 = self$769->$3;
      _tmp$1843 = _tmp$1844 & capacity_mask$1845;
      i$766 = _tmp$1842;
      idx$767 = _tmp$1843;
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
  int32_t capacity_mask$1834 = self$760->$3;
  int32_t _tmp$1833 = hash$755 & capacity_mask$1834;
  int32_t i$757 = 0;
  int32_t idx$758 = _tmp$1833;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2223 =
      self$760->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1832 =
      _field$2223;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2222;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$759;
    if (idx$758 < 0 || idx$758 >= Moonbit_array_length(entries$1832)) {
      moonbit_panic();
    }
    _tmp$2222
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1832[
        idx$758
      ];
    _bind$759 = _tmp$2222;
    if (_bind$759 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1821;
      if (_bind$759) {
        moonbit_incref(_bind$759);
      }
      moonbit_decref(self$760);
      if (_bind$759) {
        moonbit_decref(_bind$759);
      }
      _tmp$1821 = 0;
      return _tmp$1821;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$761 =
        _bind$759;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$762 =
        _Some$761;
      int32_t hash$1823 = _entry$762->$3;
      int32_t _if_result$2577;
      int32_t _field$2220;
      int32_t psl$1826;
      int32_t _tmp$1828;
      int32_t _tmp$1830;
      int32_t capacity_mask$1831;
      int32_t _tmp$1829;
      if (hash$1823 == hash$755) {
        int32_t key$1822 = _entry$762->$4;
        _if_result$2577 = key$1822 == key$756;
      } else {
        _if_result$2577 = 0;
      }
      if (_if_result$2577) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2221;
        int32_t _cnt$2483;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$1825;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1824;
        moonbit_incref(_entry$762);
        moonbit_decref(self$760);
        _field$2221 = _entry$762->$5;
        _cnt$2483 = Moonbit_object_header(_entry$762)->rc;
        if (_cnt$2483 > 1) {
          int32_t _new_cnt$2485;
          moonbit_incref(_field$2221);
          _new_cnt$2485 = _cnt$2483 - 1;
          Moonbit_object_header(_entry$762)->rc = _new_cnt$2485;
        } else if (_cnt$2483 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2484 =
            _entry$762->$1;
          if (_field$2484) {
            moonbit_decref(_field$2484);
          }
          moonbit_free(_entry$762);
        }
        value$1825 = _field$2221;
        _tmp$1824 = value$1825;
        return _tmp$1824;
      } else {
        moonbit_incref(_entry$762);
      }
      _field$2220 = _entry$762->$2;
      moonbit_decref(_entry$762);
      psl$1826 = _field$2220;
      if (i$757 > psl$1826) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1827;
        moonbit_decref(self$760);
        _tmp$1827 = 0;
        return _tmp$1827;
      }
      _tmp$1828 = i$757 + 1;
      _tmp$1830 = idx$758 + 1;
      capacity_mask$1831 = self$760->$3;
      _tmp$1829 = _tmp$1830 & capacity_mask$1831;
      i$757 = _tmp$1828;
      idx$758 = _tmp$1829;
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
  int32_t capacity_mask$1820;
  int32_t _tmp$1819;
  int32_t i$748;
  int32_t idx$749;
  moonbit_incref(key$747);
  hash$746 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$747);
  capacity_mask$1820 = self$751->$3;
  _tmp$1819 = hash$746 & capacity_mask$1820;
  i$748 = 0;
  idx$749 = _tmp$1819;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2229 =
      self$751->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1818 =
      _field$2229;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2228;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$750;
    if (idx$749 < 0 || idx$749 >= Moonbit_array_length(entries$1818)) {
      moonbit_panic();
    }
    _tmp$2228
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1818[
        idx$749
      ];
    _bind$750 = _tmp$2228;
    if (_bind$750 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1807;
      if (_bind$750) {
        moonbit_incref(_bind$750);
      }
      moonbit_decref(self$751);
      if (_bind$750) {
        moonbit_decref(_bind$750);
      }
      moonbit_decref(key$747);
      _tmp$1807 = 0;
      return _tmp$1807;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$752 =
        _bind$750;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$753 =
        _Some$752;
      int32_t hash$1809 = _entry$753->$3;
      int32_t _if_result$2579;
      int32_t _field$2224;
      int32_t psl$1812;
      int32_t _tmp$1814;
      int32_t _tmp$1816;
      int32_t capacity_mask$1817;
      int32_t _tmp$1815;
      if (hash$1809 == hash$746) {
        moonbit_string_t _field$2227 = _entry$753->$4;
        moonbit_string_t key$1808 = _field$2227;
        int32_t _tmp$2226 = moonbit_val_array_equal(key$1808, key$747);
        _if_result$2579 = _tmp$2226;
      } else {
        _if_result$2579 = 0;
      }
      if (_if_result$2579) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2225;
        int32_t _cnt$2486;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$1811;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1810;
        moonbit_incref(_entry$753);
        moonbit_decref(self$751);
        moonbit_decref(key$747);
        _field$2225 = _entry$753->$5;
        _cnt$2486 = Moonbit_object_header(_entry$753)->rc;
        if (_cnt$2486 > 1) {
          int32_t _new_cnt$2489;
          moonbit_incref(_field$2225);
          _new_cnt$2489 = _cnt$2486 - 1;
          Moonbit_object_header(_entry$753)->rc = _new_cnt$2489;
        } else if (_cnt$2486 == 1) {
          moonbit_string_t _field$2488 = _entry$753->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2487;
          moonbit_decref(_field$2488);
          _field$2487 = _entry$753->$1;
          if (_field$2487) {
            moonbit_decref(_field$2487);
          }
          moonbit_free(_entry$753);
        }
        value$1811 = _field$2225;
        _tmp$1810 = value$1811;
        return _tmp$1810;
      } else {
        moonbit_incref(_entry$753);
      }
      _field$2224 = _entry$753->$2;
      moonbit_decref(_entry$753);
      psl$1812 = _field$2224;
      if (i$748 > psl$1812) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1813;
        moonbit_decref(self$751);
        moonbit_decref(key$747);
        _tmp$1813 = 0;
        return _tmp$1813;
      }
      _tmp$1814 = i$748 + 1;
      _tmp$1816 = idx$749 + 1;
      capacity_mask$1817 = self$751->$3;
      _tmp$1815 = _tmp$1816 & capacity_mask$1817;
      i$748 = _tmp$1814;
      idx$749 = _tmp$1815;
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
  int32_t _tmp$1798;
  int32_t _tmp$1797;
  int32_t _tmp$1806;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$741;
  int32_t _len$742;
  int32_t _i$743;
  moonbit_incref(arr$739.$0);
  length$738 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$739);
  capacity$740 = $Int$$next_power_of_two(length$738);
  _tmp$1798 = capacity$740;
  _tmp$1797 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1798);
  if (length$738 > _tmp$1797) {
    int32_t _tmp$1799 = capacity$740;
    capacity$740 = _tmp$1799 * 2;
  }
  _tmp$1806 = capacity$740;
  m$741 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$1806);
  moonbit_incref(arr$739.$0);
  _len$742 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$739);
  _i$743 = 0;
  while (1) {
    if (_i$743 < _len$742) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2233 =
        arr$739.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1802 =
        _field$2233;
      int32_t start$1804 = arr$739.$1;
      int32_t _tmp$1803 = start$1804 + _i$743;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2232 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1802[
          _tmp$1803
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$744 =
        _tmp$2232;
      moonbit_string_t _field$2231 = e$744->$0;
      moonbit_string_t _tmp$1800 = _field$2231;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2230 =
        e$744->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1801 =
        _field$2230;
      int32_t _tmp$1805;
      moonbit_incref(_tmp$1801);
      moonbit_incref(_tmp$1800);
      moonbit_incref(m$741);
      $$moonbitlang$core$builtin$Map$$set$3(m$741, _tmp$1800, _tmp$1801);
      _tmp$1805 = _i$743 + 1;
      _i$743 = _tmp$1805;
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
  int32_t _tmp$1788;
  int32_t _tmp$1787;
  int32_t _tmp$1796;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$733;
  int32_t _len$734;
  int32_t _i$735;
  moonbit_incref(arr$731.$0);
  length$730 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$731);
  capacity$732 = $Int$$next_power_of_two(length$730);
  _tmp$1788 = capacity$732;
  _tmp$1787 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1788);
  if (length$730 > _tmp$1787) {
    int32_t _tmp$1789 = capacity$732;
    capacity$732 = _tmp$1789 * 2;
  }
  _tmp$1796 = capacity$732;
  m$733 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$1796);
  moonbit_incref(arr$731.$0);
  _len$734 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$731);
  _i$735 = 0;
  while (1) {
    if (_i$735 < _len$734) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2237 =
        arr$731.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1792 =
        _field$2237;
      int32_t start$1794 = arr$731.$1;
      int32_t _tmp$1793 = start$1794 + _i$735;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2236 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1792[
          _tmp$1793
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$736 =
        _tmp$2236;
      moonbit_string_t _field$2235 = e$736->$0;
      moonbit_string_t _tmp$1790 = _field$2235;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2234 =
        e$736->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1791 =
        _field$2234;
      int32_t _tmp$1795;
      moonbit_incref(_tmp$1791);
      moonbit_incref(_tmp$1790);
      moonbit_incref(m$733);
      $$moonbitlang$core$builtin$Map$$set$2(m$733, _tmp$1790, _tmp$1791);
      _tmp$1795 = _i$735 + 1;
      _i$735 = _tmp$1795;
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
  int32_t _tmp$1778;
  int32_t _tmp$1777;
  int32_t _tmp$1786;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$725;
  int32_t _len$726;
  int32_t _i$727;
  moonbit_incref(arr$723.$0);
  length$722 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$723);
  capacity$724 = $Int$$next_power_of_two(length$722);
  _tmp$1778 = capacity$724;
  _tmp$1777 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1778);
  if (length$722 > _tmp$1777) {
    int32_t _tmp$1779 = capacity$724;
    capacity$724 = _tmp$1779 * 2;
  }
  _tmp$1786 = capacity$724;
  m$725 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$1786);
  moonbit_incref(arr$723.$0);
  _len$726 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$723);
  _i$727 = 0;
  while (1) {
    if (_i$727 < _len$726) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2240 =
        arr$723.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$1782 =
        _field$2240;
      int32_t start$1784 = arr$723.$1;
      int32_t _tmp$1783 = start$1784 + _i$727;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2239 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$1782[
          _tmp$1783
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$728 =
        _tmp$2239;
      int32_t _tmp$1780 = e$728->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2238 =
        e$728->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$1781 =
        _field$2238;
      int32_t _tmp$1785;
      moonbit_incref(_tmp$1781);
      moonbit_incref(m$725);
      $$moonbitlang$core$builtin$Map$$set$1(m$725, _tmp$1780, _tmp$1781);
      _tmp$1785 = _i$727 + 1;
      _i$727 = _tmp$1785;
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
  int32_t _tmp$1768;
  int32_t _tmp$1767;
  int32_t _tmp$1776;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$717;
  int32_t _len$718;
  int32_t _i$719;
  moonbit_incref(arr$715.$0);
  length$714 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$715);
  capacity$716 = $Int$$next_power_of_two(length$714);
  _tmp$1768 = capacity$716;
  _tmp$1767 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$1768);
  if (length$714 > _tmp$1767) {
    int32_t _tmp$1769 = capacity$716;
    capacity$716 = _tmp$1769 * 2;
  }
  _tmp$1776 = capacity$716;
  m$717 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$1776);
  moonbit_incref(arr$715.$0);
  _len$718 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$715);
  _i$719 = 0;
  while (1) {
    if (_i$719 < _len$718) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2244 =
        arr$715.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$1772 =
        _field$2244;
      int32_t start$1774 = arr$715.$1;
      int32_t _tmp$1773 = start$1774 + _i$719;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2243 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$1772[
          _tmp$1773
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$720 =
        _tmp$2243;
      moonbit_string_t _field$2242 = e$720->$0;
      moonbit_string_t _tmp$1770 = _field$2242;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2241 =
        e$720->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1771 =
        _field$2241;
      int32_t _tmp$1775;
      moonbit_incref(_tmp$1771);
      moonbit_incref(_tmp$1770);
      moonbit_incref(m$717);
      $$moonbitlang$core$builtin$Map$$set$0(m$717, _tmp$1770, _tmp$1771);
      _tmp$1775 = _i$719 + 1;
      _i$719 = _tmp$1775;
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
  int32_t _tmp$1766;
  moonbit_incref(key$712);
  _tmp$1766 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$712);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$711, key$712, value$713, _tmp$1766
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$708,
  moonbit_string_t key$709,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$710
) {
  int32_t _tmp$1765;
  moonbit_incref(key$709);
  _tmp$1765 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$709);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$708, key$709, value$710, _tmp$1765
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$705,
  int32_t key$706,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$707
) {
  int32_t _tmp$1764 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$706);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$705, key$706, value$707, _tmp$1764
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$702,
  moonbit_string_t key$703,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$704
) {
  int32_t _tmp$1763;
  moonbit_incref(key$703);
  _tmp$1763 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$703);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$702, key$703, value$704, _tmp$1763
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$692
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2251 =
    self$692->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$691 =
    _field$2251;
  int32_t capacity$1762 = self$692->$2;
  int32_t new_capacity$693 = capacity$1762 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1757 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1756 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$693, _tmp$1757
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2250 =
    self$692->$0;
  int32_t _tmp$1758;
  int32_t capacity$1760;
  int32_t _tmp$1759;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1761;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2249;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$694;
  if (old_head$691) {
    moonbit_incref(old_head$691);
  }
  moonbit_decref(_old$2250);
  self$692->$0 = _tmp$1756;
  self$692->$2 = new_capacity$693;
  _tmp$1758 = new_capacity$693 - 1;
  self$692->$3 = _tmp$1758;
  capacity$1760 = self$692->$2;
  _tmp$1759 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1760);
  self$692->$4 = _tmp$1759;
  self$692->$1 = 0;
  _tmp$1761 = 0;
  _old$2249 = self$692->$5;
  if (_old$2249) {
    moonbit_decref(_old$2249);
  }
  self$692->$5 = _tmp$1761;
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
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2248 =
        _x$696->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$697 =
        _field$2248;
      moonbit_string_t _field$2247 = _x$696->$4;
      moonbit_string_t _key$698 = _field$2247;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2246 =
        _x$696->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$699 =
        _field$2246;
      int32_t _field$2245 = _x$696->$3;
      int32_t _cnt$2490 = Moonbit_object_header(_x$696)->rc;
      int32_t _hash$700;
      if (_cnt$2490 > 1) {
        int32_t _new_cnt$2491;
        moonbit_incref(_value$699);
        moonbit_incref(_key$698);
        if (_next$697) {
          moonbit_incref(_next$697);
        }
        _new_cnt$2491 = _cnt$2490 - 1;
        Moonbit_object_header(_x$696)->rc = _new_cnt$2491;
      } else if (_cnt$2490 == 1) {
        moonbit_free(_x$696);
      }
      _hash$700 = _field$2245;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2258 =
    self$681->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$680 =
    _field$2258;
  int32_t capacity$1755 = self$681->$2;
  int32_t new_capacity$682 = capacity$1755 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1750 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1749 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$682, _tmp$1750
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2257 =
    self$681->$0;
  int32_t _tmp$1751;
  int32_t capacity$1753;
  int32_t _tmp$1752;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1754;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2256;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$683;
  if (old_head$680) {
    moonbit_incref(old_head$680);
  }
  moonbit_decref(_old$2257);
  self$681->$0 = _tmp$1749;
  self$681->$2 = new_capacity$682;
  _tmp$1751 = new_capacity$682 - 1;
  self$681->$3 = _tmp$1751;
  capacity$1753 = self$681->$2;
  _tmp$1752 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1753);
  self$681->$4 = _tmp$1752;
  self$681->$1 = 0;
  _tmp$1754 = 0;
  _old$2256 = self$681->$5;
  if (_old$2256) {
    moonbit_decref(_old$2256);
  }
  self$681->$5 = _tmp$1754;
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
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2255 =
        _x$685->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$686 =
        _field$2255;
      moonbit_string_t _field$2254 = _x$685->$4;
      moonbit_string_t _key$687 = _field$2254;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2253 =
        _x$685->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$688 =
        _field$2253;
      int32_t _field$2252 = _x$685->$3;
      int32_t _cnt$2492 = Moonbit_object_header(_x$685)->rc;
      int32_t _hash$689;
      if (_cnt$2492 > 1) {
        int32_t _new_cnt$2493;
        moonbit_incref(_value$688);
        moonbit_incref(_key$687);
        if (_next$686) {
          moonbit_incref(_next$686);
        }
        _new_cnt$2493 = _cnt$2492 - 1;
        Moonbit_object_header(_x$685)->rc = _new_cnt$2493;
      } else if (_cnt$2492 == 1) {
        moonbit_free(_x$685);
      }
      _hash$689 = _field$2252;
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
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2264 =
    self$670->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$669 =
    _field$2264;
  int32_t capacity$1748 = self$670->$2;
  int32_t new_capacity$671 = capacity$1748 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1743 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1742 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$671, _tmp$1743
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$2263 =
    self$670->$0;
  int32_t _tmp$1744;
  int32_t capacity$1746;
  int32_t _tmp$1745;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1747;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2262;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$672;
  if (old_head$669) {
    moonbit_incref(old_head$669);
  }
  moonbit_decref(_old$2263);
  self$670->$0 = _tmp$1742;
  self$670->$2 = new_capacity$671;
  _tmp$1744 = new_capacity$671 - 1;
  self$670->$3 = _tmp$1744;
  capacity$1746 = self$670->$2;
  _tmp$1745 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1746);
  self$670->$4 = _tmp$1745;
  self$670->$1 = 0;
  _tmp$1747 = 0;
  _old$2262 = self$670->$5;
  if (_old$2262) {
    moonbit_decref(_old$2262);
  }
  self$670->$5 = _tmp$1747;
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
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2261 =
        _x$674->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$675 =
        _field$2261;
      int32_t _key$676 = _x$674->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2260 =
        _x$674->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$677 =
        _field$2260;
      int32_t _field$2259 = _x$674->$3;
      int32_t _cnt$2494 = Moonbit_object_header(_x$674)->rc;
      int32_t _hash$678;
      if (_cnt$2494 > 1) {
        int32_t _new_cnt$2495;
        moonbit_incref(_value$677);
        if (_next$675) {
          moonbit_incref(_next$675);
        }
        _new_cnt$2495 = _cnt$2494 - 1;
        Moonbit_object_header(_x$674)->rc = _new_cnt$2495;
      } else if (_cnt$2494 == 1) {
        moonbit_free(_x$674);
      }
      _hash$678 = _field$2259;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2271 =
    self$659->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$658 =
    _field$2271;
  int32_t capacity$1741 = self$659->$2;
  int32_t new_capacity$660 = capacity$1741 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1736 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1735 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$660, _tmp$1736
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$2270 =
    self$659->$0;
  int32_t _tmp$1737;
  int32_t capacity$1739;
  int32_t _tmp$1738;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1740;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2269;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$661;
  if (old_head$658) {
    moonbit_incref(old_head$658);
  }
  moonbit_decref(_old$2270);
  self$659->$0 = _tmp$1735;
  self$659->$2 = new_capacity$660;
  _tmp$1737 = new_capacity$660 - 1;
  self$659->$3 = _tmp$1737;
  capacity$1739 = self$659->$2;
  _tmp$1738 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$1739);
  self$659->$4 = _tmp$1738;
  self$659->$1 = 0;
  _tmp$1740 = 0;
  _old$2269 = self$659->$5;
  if (_old$2269) {
    moonbit_decref(_old$2269);
  }
  self$659->$5 = _tmp$1740;
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
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2268 =
        _x$663->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$664 =
        _field$2268;
      moonbit_string_t _field$2267 = _x$663->$4;
      moonbit_string_t _key$665 = _field$2267;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2266 =
        _x$663->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$666 =
        _field$2266;
      int32_t _field$2265 = _x$663->$3;
      int32_t _cnt$2496 = Moonbit_object_header(_x$663)->rc;
      int32_t _hash$667;
      if (_cnt$2496 > 1) {
        int32_t _new_cnt$2497;
        moonbit_incref(_value$666);
        moonbit_incref(_key$665);
        if (_next$664) {
          moonbit_incref(_next$664);
        }
        _new_cnt$2497 = _cnt$2496 - 1;
        Moonbit_object_header(_x$663)->rc = _new_cnt$2497;
      } else if (_cnt$2496 == 1) {
        moonbit_free(_x$663);
      }
      _hash$667 = _field$2265;
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
  int32_t size$1721 = self$642->$1;
  int32_t grow_at$1722 = self$642->$4;
  int32_t capacity_mask$1734;
  int32_t _tmp$1733;
  struct $$3c$Int$2a$Int$3e$* _bind$643;
  int32_t psl$644;
  int32_t idx$645;
  int32_t _idx$653;
  int32_t _field$2272;
  int32_t _psl$654;
  int32_t _bind$655;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$656;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$657;
  if (size$1721 >= grow_at$1722) {
    moonbit_incref(self$642);
    $$moonbitlang$core$builtin$Map$$grow$3(self$642);
  }
  capacity_mask$1734 = self$642->$3;
  _tmp$1733 = hash$650 & capacity_mask$1734;
  psl$644 = 0;
  idx$645 = _tmp$1733;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2277 =
      self$642->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1732 =
      _field$2277;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2276;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$646;
    if (idx$645 < 0 || idx$645 >= Moonbit_array_length(entries$1732)) {
      moonbit_panic();
    }
    _tmp$2276
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1732[
        idx$645
      ];
    _bind$646 = _tmp$2276;
    if (_bind$646 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1723 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1723)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1723->$0 = idx$645;
      _tuple$1723->$1 = psl$644;
      _bind$643 = _tuple$1723;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$648 =
        _bind$646;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$649 =
        _Some$648;
      int32_t hash$1725 = _curr_entry$649->$3;
      int32_t _if_result$2589;
      int32_t psl$1726;
      int32_t _tmp$1728;
      int32_t _tmp$1730;
      int32_t capacity_mask$1731;
      int32_t _tmp$1729;
      if (hash$1725 == hash$650) {
        moonbit_string_t _field$2275 = _curr_entry$649->$4;
        moonbit_string_t key$1724 = _field$2275;
        int32_t _tmp$2274 = moonbit_val_array_equal(key$1724, key$651);
        _if_result$2589 = _tmp$2274;
      } else {
        _if_result$2589 = 0;
      }
      if (_if_result$2589) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2273;
        moonbit_incref(_curr_entry$649);
        moonbit_decref(key$651);
        moonbit_decref(self$642);
        _old$2273 = _curr_entry$649->$5;
        moonbit_decref(_old$2273);
        _curr_entry$649->$5 = value$652;
        moonbit_decref(_curr_entry$649);
        return 0;
      } else {
        moonbit_incref(_curr_entry$649);
      }
      psl$1726 = _curr_entry$649->$2;
      if (psl$644 > psl$1726) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1727;
        moonbit_incref(self$642);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$642, idx$645, _curr_entry$649
        );
        _tuple$1727
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1727)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1727->$0 = idx$645;
        _tuple$1727->$1 = psl$644;
        _bind$643 = _tuple$1727;
        break;
      } else {
        moonbit_decref(_curr_entry$649);
      }
      _tmp$1728 = psl$644 + 1;
      _tmp$1730 = idx$645 + 1;
      capacity_mask$1731 = self$642->$3;
      _tmp$1729 = _tmp$1730 & capacity_mask$1731;
      psl$644 = _tmp$1728;
      idx$645 = _tmp$1729;
      continue;
    }
    break;
  }
  _idx$653 = _bind$643->$0;
  _field$2272 = _bind$643->$1;
  moonbit_decref(_bind$643);
  _psl$654 = _field$2272;
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
  int32_t size$1707 = self$626->$1;
  int32_t grow_at$1708 = self$626->$4;
  int32_t capacity_mask$1720;
  int32_t _tmp$1719;
  struct $$3c$Int$2a$Int$3e$* _bind$627;
  int32_t psl$628;
  int32_t idx$629;
  int32_t _idx$637;
  int32_t _field$2278;
  int32_t _psl$638;
  int32_t _bind$639;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$640;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$641;
  if (size$1707 >= grow_at$1708) {
    moonbit_incref(self$626);
    $$moonbitlang$core$builtin$Map$$grow$2(self$626);
  }
  capacity_mask$1720 = self$626->$3;
  _tmp$1719 = hash$634 & capacity_mask$1720;
  psl$628 = 0;
  idx$629 = _tmp$1719;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2283 =
      self$626->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1718 =
      _field$2283;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2282;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$630;
    if (idx$629 < 0 || idx$629 >= Moonbit_array_length(entries$1718)) {
      moonbit_panic();
    }
    _tmp$2282
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1718[
        idx$629
      ];
    _bind$630 = _tmp$2282;
    if (_bind$630 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1709 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1709)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1709->$0 = idx$629;
      _tuple$1709->$1 = psl$628;
      _bind$627 = _tuple$1709;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$632 =
        _bind$630;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$633 =
        _Some$632;
      int32_t hash$1711 = _curr_entry$633->$3;
      int32_t _if_result$2591;
      int32_t psl$1712;
      int32_t _tmp$1714;
      int32_t _tmp$1716;
      int32_t capacity_mask$1717;
      int32_t _tmp$1715;
      if (hash$1711 == hash$634) {
        moonbit_string_t _field$2281 = _curr_entry$633->$4;
        moonbit_string_t key$1710 = _field$2281;
        int32_t _tmp$2280 = moonbit_val_array_equal(key$1710, key$635);
        _if_result$2591 = _tmp$2280;
      } else {
        _if_result$2591 = 0;
      }
      if (_if_result$2591) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2279;
        moonbit_incref(_curr_entry$633);
        moonbit_decref(key$635);
        moonbit_decref(self$626);
        _old$2279 = _curr_entry$633->$5;
        moonbit_decref(_old$2279);
        _curr_entry$633->$5 = value$636;
        moonbit_decref(_curr_entry$633);
        return 0;
      } else {
        moonbit_incref(_curr_entry$633);
      }
      psl$1712 = _curr_entry$633->$2;
      if (psl$628 > psl$1712) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1713;
        moonbit_incref(self$626);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$626, idx$629, _curr_entry$633
        );
        _tuple$1713
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1713)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1713->$0 = idx$629;
        _tuple$1713->$1 = psl$628;
        _bind$627 = _tuple$1713;
        break;
      } else {
        moonbit_decref(_curr_entry$633);
      }
      _tmp$1714 = psl$628 + 1;
      _tmp$1716 = idx$629 + 1;
      capacity_mask$1717 = self$626->$3;
      _tmp$1715 = _tmp$1716 & capacity_mask$1717;
      psl$628 = _tmp$1714;
      idx$629 = _tmp$1715;
      continue;
    }
    break;
  }
  _idx$637 = _bind$627->$0;
  _field$2278 = _bind$627->$1;
  moonbit_decref(_bind$627);
  _psl$638 = _field$2278;
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
  int32_t size$1693 = self$610->$1;
  int32_t grow_at$1694 = self$610->$4;
  int32_t capacity_mask$1706;
  int32_t _tmp$1705;
  struct $$3c$Int$2a$Int$3e$* _bind$611;
  int32_t psl$612;
  int32_t idx$613;
  int32_t _idx$621;
  int32_t _field$2284;
  int32_t _psl$622;
  int32_t _bind$623;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$624;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$625;
  if (size$1693 >= grow_at$1694) {
    moonbit_incref(self$610);
    $$moonbitlang$core$builtin$Map$$grow$1(self$610);
  }
  capacity_mask$1706 = self$610->$3;
  _tmp$1705 = hash$618 & capacity_mask$1706;
  psl$612 = 0;
  idx$613 = _tmp$1705;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2287 =
      self$610->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1704 =
      _field$2287;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2286;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$614;
    if (idx$613 < 0 || idx$613 >= Moonbit_array_length(entries$1704)) {
      moonbit_panic();
    }
    _tmp$2286
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1704[
        idx$613
      ];
    _bind$614 = _tmp$2286;
    if (_bind$614 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1695 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1695)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1695->$0 = idx$613;
      _tuple$1695->$1 = psl$612;
      _bind$611 = _tuple$1695;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$616 =
        _bind$614;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$617 =
        _Some$616;
      int32_t hash$1697 = _curr_entry$617->$3;
      int32_t _if_result$2593;
      int32_t psl$1698;
      int32_t _tmp$1700;
      int32_t _tmp$1702;
      int32_t capacity_mask$1703;
      int32_t _tmp$1701;
      if (hash$1697 == hash$618) {
        int32_t key$1696 = _curr_entry$617->$4;
        _if_result$2593 = key$1696 == key$619;
      } else {
        _if_result$2593 = 0;
      }
      if (_if_result$2593) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$2285;
        moonbit_incref(_curr_entry$617);
        moonbit_decref(self$610);
        _old$2285 = _curr_entry$617->$5;
        moonbit_decref(_old$2285);
        _curr_entry$617->$5 = value$620;
        moonbit_decref(_curr_entry$617);
        return 0;
      } else {
        moonbit_incref(_curr_entry$617);
      }
      psl$1698 = _curr_entry$617->$2;
      if (psl$612 > psl$1698) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1699;
        moonbit_incref(self$610);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$610, idx$613, _curr_entry$617
        );
        _tuple$1699
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1699)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1699->$0 = idx$613;
        _tuple$1699->$1 = psl$612;
        _bind$611 = _tuple$1699;
        break;
      } else {
        moonbit_decref(_curr_entry$617);
      }
      _tmp$1700 = psl$612 + 1;
      _tmp$1702 = idx$613 + 1;
      capacity_mask$1703 = self$610->$3;
      _tmp$1701 = _tmp$1702 & capacity_mask$1703;
      psl$612 = _tmp$1700;
      idx$613 = _tmp$1701;
      continue;
    }
    break;
  }
  _idx$621 = _bind$611->$0;
  _field$2284 = _bind$611->$1;
  moonbit_decref(_bind$611);
  _psl$622 = _field$2284;
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
  int32_t size$1679 = self$594->$1;
  int32_t grow_at$1680 = self$594->$4;
  int32_t capacity_mask$1692;
  int32_t _tmp$1691;
  struct $$3c$Int$2a$Int$3e$* _bind$595;
  int32_t psl$596;
  int32_t idx$597;
  int32_t _idx$605;
  int32_t _field$2288;
  int32_t _psl$606;
  int32_t _bind$607;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$608;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$609;
  if (size$1679 >= grow_at$1680) {
    moonbit_incref(self$594);
    $$moonbitlang$core$builtin$Map$$grow$0(self$594);
  }
  capacity_mask$1692 = self$594->$3;
  _tmp$1691 = hash$602 & capacity_mask$1692;
  psl$596 = 0;
  idx$597 = _tmp$1691;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2293 =
      self$594->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1690 =
      _field$2293;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2292;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$598;
    if (idx$597 < 0 || idx$597 >= Moonbit_array_length(entries$1690)) {
      moonbit_panic();
    }
    _tmp$2292
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1690[
        idx$597
      ];
    _bind$598 = _tmp$2292;
    if (_bind$598 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$1681 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$1681)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$1681->$0 = idx$597;
      _tuple$1681->$1 = psl$596;
      _bind$595 = _tuple$1681;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$600 =
        _bind$598;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$601 =
        _Some$600;
      int32_t hash$1683 = _curr_entry$601->$3;
      int32_t _if_result$2595;
      int32_t psl$1684;
      int32_t _tmp$1686;
      int32_t _tmp$1688;
      int32_t capacity_mask$1689;
      int32_t _tmp$1687;
      if (hash$1683 == hash$602) {
        moonbit_string_t _field$2291 = _curr_entry$601->$4;
        moonbit_string_t key$1682 = _field$2291;
        int32_t _tmp$2290 = moonbit_val_array_equal(key$1682, key$603);
        _if_result$2595 = _tmp$2290;
      } else {
        _if_result$2595 = 0;
      }
      if (_if_result$2595) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2289;
        moonbit_incref(_curr_entry$601);
        moonbit_decref(key$603);
        moonbit_decref(self$594);
        _old$2289 = _curr_entry$601->$5;
        moonbit_decref(_old$2289);
        _curr_entry$601->$5 = value$604;
        moonbit_decref(_curr_entry$601);
        return 0;
      } else {
        moonbit_incref(_curr_entry$601);
      }
      psl$1684 = _curr_entry$601->$2;
      if (psl$596 > psl$1684) {
        struct $$3c$Int$2a$Int$3e$* _tuple$1685;
        moonbit_incref(self$594);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$594, idx$597, _curr_entry$601
        );
        _tuple$1685
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$1685)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$1685->$0 = idx$597;
        _tuple$1685->$1 = psl$596;
        _bind$595 = _tuple$1685;
        break;
      } else {
        moonbit_decref(_curr_entry$601);
      }
      _tmp$1686 = psl$596 + 1;
      _tmp$1688 = idx$597 + 1;
      capacity_mask$1689 = self$594->$3;
      _tmp$1687 = _tmp$1688 & capacity_mask$1689;
      psl$596 = _tmp$1686;
      idx$597 = _tmp$1687;
      continue;
    }
    break;
  }
  _idx$605 = _bind$595->$0;
  _field$2288 = _bind$595->$1;
  moonbit_decref(_bind$595);
  _psl$606 = _field$2288;
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
  int32_t psl$1678 = entry$592->$2;
  int32_t _tmp$1674 = psl$1678 + 1;
  int32_t _tmp$1676 = idx$593 + 1;
  int32_t capacity_mask$1677 = self$588->$3;
  int32_t _tmp$1675 = _tmp$1676 & capacity_mask$1677;
  int32_t psl$584 = _tmp$1674;
  int32_t idx$585 = _tmp$1675;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$586 =
    entry$592;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2295 =
      self$588->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1673 =
      _field$2295;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2294;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$587;
    if (idx$585 < 0 || idx$585 >= Moonbit_array_length(entries$1673)) {
      moonbit_panic();
    }
    _tmp$2294
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1673[
        idx$585
      ];
    _bind$587 = _tmp$2294;
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
      int32_t psl$1663 = _curr_entry$591->$2;
      if (psl$584 > psl$1663) {
        int32_t psl$1668;
        int32_t _tmp$1664;
        int32_t _tmp$1666;
        int32_t capacity_mask$1667;
        int32_t _tmp$1665;
        entry$586->$2 = psl$584;
        moonbit_incref(_curr_entry$591);
        moonbit_incref(self$588);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$588, entry$586, idx$585
        );
        psl$1668 = _curr_entry$591->$2;
        _tmp$1664 = psl$1668 + 1;
        _tmp$1666 = idx$585 + 1;
        capacity_mask$1667 = self$588->$3;
        _tmp$1665 = _tmp$1666 & capacity_mask$1667;
        psl$584 = _tmp$1664;
        idx$585 = _tmp$1665;
        entry$586 = _curr_entry$591;
        continue;
      } else {
        int32_t _tmp$1669 = psl$584 + 1;
        int32_t _tmp$1671 = idx$585 + 1;
        int32_t capacity_mask$1672 = self$588->$3;
        int32_t _tmp$1670 = _tmp$1671 & capacity_mask$1672;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2597 =
          entry$586;
        psl$584 = _tmp$1669;
        idx$585 = _tmp$1670;
        entry$586 = _tmp$2597;
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
  int32_t psl$1662 = entry$582->$2;
  int32_t _tmp$1658 = psl$1662 + 1;
  int32_t _tmp$1660 = idx$583 + 1;
  int32_t capacity_mask$1661 = self$578->$3;
  int32_t _tmp$1659 = _tmp$1660 & capacity_mask$1661;
  int32_t psl$574 = _tmp$1658;
  int32_t idx$575 = _tmp$1659;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$576 =
    entry$582;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2297 =
      self$578->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1657 =
      _field$2297;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2296;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$577;
    if (idx$575 < 0 || idx$575 >= Moonbit_array_length(entries$1657)) {
      moonbit_panic();
    }
    _tmp$2296
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1657[
        idx$575
      ];
    _bind$577 = _tmp$2296;
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
      int32_t psl$1647 = _curr_entry$581->$2;
      if (psl$574 > psl$1647) {
        int32_t psl$1652;
        int32_t _tmp$1648;
        int32_t _tmp$1650;
        int32_t capacity_mask$1651;
        int32_t _tmp$1649;
        entry$576->$2 = psl$574;
        moonbit_incref(_curr_entry$581);
        moonbit_incref(self$578);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$578, entry$576, idx$575
        );
        psl$1652 = _curr_entry$581->$2;
        _tmp$1648 = psl$1652 + 1;
        _tmp$1650 = idx$575 + 1;
        capacity_mask$1651 = self$578->$3;
        _tmp$1649 = _tmp$1650 & capacity_mask$1651;
        psl$574 = _tmp$1648;
        idx$575 = _tmp$1649;
        entry$576 = _curr_entry$581;
        continue;
      } else {
        int32_t _tmp$1653 = psl$574 + 1;
        int32_t _tmp$1655 = idx$575 + 1;
        int32_t capacity_mask$1656 = self$578->$3;
        int32_t _tmp$1654 = _tmp$1655 & capacity_mask$1656;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2599 =
          entry$576;
        psl$574 = _tmp$1653;
        idx$575 = _tmp$1654;
        entry$576 = _tmp$2599;
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
  int32_t psl$1646 = entry$572->$2;
  int32_t _tmp$1642 = psl$1646 + 1;
  int32_t _tmp$1644 = idx$573 + 1;
  int32_t capacity_mask$1645 = self$568->$3;
  int32_t _tmp$1643 = _tmp$1644 & capacity_mask$1645;
  int32_t psl$564 = _tmp$1642;
  int32_t idx$565 = _tmp$1643;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$566 =
    entry$572;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2299 =
      self$568->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1641 =
      _field$2299;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2298;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$567;
    if (idx$565 < 0 || idx$565 >= Moonbit_array_length(entries$1641)) {
      moonbit_panic();
    }
    _tmp$2298
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1641[
        idx$565
      ];
    _bind$567 = _tmp$2298;
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
      int32_t psl$1631 = _curr_entry$571->$2;
      if (psl$564 > psl$1631) {
        int32_t psl$1636;
        int32_t _tmp$1632;
        int32_t _tmp$1634;
        int32_t capacity_mask$1635;
        int32_t _tmp$1633;
        entry$566->$2 = psl$564;
        moonbit_incref(_curr_entry$571);
        moonbit_incref(self$568);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$568, entry$566, idx$565
        );
        psl$1636 = _curr_entry$571->$2;
        _tmp$1632 = psl$1636 + 1;
        _tmp$1634 = idx$565 + 1;
        capacity_mask$1635 = self$568->$3;
        _tmp$1633 = _tmp$1634 & capacity_mask$1635;
        psl$564 = _tmp$1632;
        idx$565 = _tmp$1633;
        entry$566 = _curr_entry$571;
        continue;
      } else {
        int32_t _tmp$1637 = psl$564 + 1;
        int32_t _tmp$1639 = idx$565 + 1;
        int32_t capacity_mask$1640 = self$568->$3;
        int32_t _tmp$1638 = _tmp$1639 & capacity_mask$1640;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2601 =
          entry$566;
        psl$564 = _tmp$1637;
        idx$565 = _tmp$1638;
        entry$566 = _tmp$2601;
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
  int32_t psl$1630 = entry$562->$2;
  int32_t _tmp$1626 = psl$1630 + 1;
  int32_t _tmp$1628 = idx$563 + 1;
  int32_t capacity_mask$1629 = self$558->$3;
  int32_t _tmp$1627 = _tmp$1628 & capacity_mask$1629;
  int32_t psl$554 = _tmp$1626;
  int32_t idx$555 = _tmp$1627;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$556 =
    entry$562;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2301 =
      self$558->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1625 =
      _field$2301;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2300;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$557;
    if (idx$555 < 0 || idx$555 >= Moonbit_array_length(entries$1625)) {
      moonbit_panic();
    }
    _tmp$2300
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1625[
        idx$555
      ];
    _bind$557 = _tmp$2300;
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
      int32_t psl$1615 = _curr_entry$561->$2;
      if (psl$554 > psl$1615) {
        int32_t psl$1620;
        int32_t _tmp$1616;
        int32_t _tmp$1618;
        int32_t capacity_mask$1619;
        int32_t _tmp$1617;
        entry$556->$2 = psl$554;
        moonbit_incref(_curr_entry$561);
        moonbit_incref(self$558);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$558, entry$556, idx$555
        );
        psl$1620 = _curr_entry$561->$2;
        _tmp$1616 = psl$1620 + 1;
        _tmp$1618 = idx$555 + 1;
        capacity_mask$1619 = self$558->$3;
        _tmp$1617 = _tmp$1618 & capacity_mask$1619;
        psl$554 = _tmp$1616;
        idx$555 = _tmp$1617;
        entry$556 = _curr_entry$561;
        continue;
      } else {
        int32_t _tmp$1621 = psl$554 + 1;
        int32_t _tmp$1623 = idx$555 + 1;
        int32_t capacity_mask$1624 = self$558->$3;
        int32_t _tmp$1622 = _tmp$1623 & capacity_mask$1624;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2603 =
          entry$556;
        psl$554 = _tmp$1621;
        idx$555 = _tmp$1622;
        entry$556 = _tmp$2603;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2304 =
    self$548->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1613 =
    _field$2304;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1614;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2303;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2302;
  int32_t _cnt$2498;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$551;
  moonbit_incref(entry$550);
  _tmp$1614 = entry$550;
  if (new_idx$549 < 0 || new_idx$549 >= Moonbit_array_length(entries$1613)) {
    moonbit_panic();
  }
  _old$2303
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1613[
      new_idx$549
    ];
  if (_old$2303) {
    moonbit_decref(_old$2303);
  }
  entries$1613[new_idx$549] = _tmp$1614;
  _field$2302 = entry$550->$1;
  _cnt$2498 = Moonbit_object_header(entry$550)->rc;
  if (_cnt$2498 > 1) {
    int32_t _new_cnt$2501;
    if (_field$2302) {
      moonbit_incref(_field$2302);
    }
    _new_cnt$2501 = _cnt$2498 - 1;
    Moonbit_object_header(entry$550)->rc = _new_cnt$2501;
  } else if (_cnt$2498 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2500 =
      entry$550->$5;
    moonbit_string_t _field$2499;
    moonbit_decref(_field$2500);
    _field$2499 = entry$550->$4;
    moonbit_decref(_field$2499);
    moonbit_free(entry$550);
  }
  _bind$551 = _field$2302;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2307 =
    self$542->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1611 =
    _field$2307;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1612;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2306;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2305;
  int32_t _cnt$2502;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$545;
  moonbit_incref(entry$544);
  _tmp$1612 = entry$544;
  if (new_idx$543 < 0 || new_idx$543 >= Moonbit_array_length(entries$1611)) {
    moonbit_panic();
  }
  _old$2306
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1611[
      new_idx$543
    ];
  if (_old$2306) {
    moonbit_decref(_old$2306);
  }
  entries$1611[new_idx$543] = _tmp$1612;
  _field$2305 = entry$544->$1;
  _cnt$2502 = Moonbit_object_header(entry$544)->rc;
  if (_cnt$2502 > 1) {
    int32_t _new_cnt$2505;
    if (_field$2305) {
      moonbit_incref(_field$2305);
    }
    _new_cnt$2505 = _cnt$2502 - 1;
    Moonbit_object_header(entry$544)->rc = _new_cnt$2505;
  } else if (_cnt$2502 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2504 =
      entry$544->$5;
    moonbit_string_t _field$2503;
    moonbit_decref(_field$2504);
    _field$2503 = entry$544->$4;
    moonbit_decref(_field$2503);
    moonbit_free(entry$544);
  }
  _bind$545 = _field$2305;
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
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2310 =
    self$536->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1609 =
    _field$2310;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1610;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2309;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2308;
  int32_t _cnt$2506;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$539;
  moonbit_incref(entry$538);
  _tmp$1610 = entry$538;
  if (new_idx$537 < 0 || new_idx$537 >= Moonbit_array_length(entries$1609)) {
    moonbit_panic();
  }
  _old$2309
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1609[
      new_idx$537
    ];
  if (_old$2309) {
    moonbit_decref(_old$2309);
  }
  entries$1609[new_idx$537] = _tmp$1610;
  _field$2308 = entry$538->$1;
  _cnt$2506 = Moonbit_object_header(entry$538)->rc;
  if (_cnt$2506 > 1) {
    int32_t _new_cnt$2508;
    if (_field$2308) {
      moonbit_incref(_field$2308);
    }
    _new_cnt$2508 = _cnt$2506 - 1;
    Moonbit_object_header(entry$538)->rc = _new_cnt$2508;
  } else if (_cnt$2506 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$2507 =
      entry$538->$5;
    moonbit_decref(_field$2507);
    moonbit_free(entry$538);
  }
  _bind$539 = _field$2308;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2313 =
    self$530->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1607 =
    _field$2313;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1608;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2312;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$2311;
  int32_t _cnt$2509;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$533;
  moonbit_incref(entry$532);
  _tmp$1608 = entry$532;
  if (new_idx$531 < 0 || new_idx$531 >= Moonbit_array_length(entries$1607)) {
    moonbit_panic();
  }
  _old$2312
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1607[
      new_idx$531
    ];
  if (_old$2312) {
    moonbit_decref(_old$2312);
  }
  entries$1607[new_idx$531] = _tmp$1608;
  _field$2311 = entry$532->$1;
  _cnt$2509 = Moonbit_object_header(entry$532)->rc;
  if (_cnt$2509 > 1) {
    int32_t _new_cnt$2512;
    if (_field$2311) {
      moonbit_incref(_field$2311);
    }
    _new_cnt$2512 = _cnt$2509 - 1;
    Moonbit_object_header(entry$532)->rc = _new_cnt$2512;
  } else if (_cnt$2509 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$2511 =
      entry$532->$5;
    moonbit_string_t _field$2510;
    moonbit_decref(_field$2511);
    _field$2510 = entry$532->$4;
    moonbit_decref(_field$2510);
    moonbit_free(entry$532);
  }
  _bind$533 = _field$2311;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2315;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1603;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1604;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2314;
  int32_t size$1606;
  int32_t _tmp$1605;
  switch (_bind$526) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1598;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2316;
      moonbit_incref(entry$528);
      _tmp$1598 = entry$528;
      _old$2316 = self$527->$5;
      if (_old$2316) {
        moonbit_decref(_old$2316);
      }
      self$527->$5 = _tmp$1598;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2319 =
        self$527->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1602 =
        _field$2319;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2318;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1601;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1599;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1600;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2317;
      if (_bind$526 < 0 || _bind$526 >= Moonbit_array_length(entries$1602)) {
        moonbit_panic();
      }
      _tmp$2318
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1602[
          _bind$526
        ];
      _tmp$1601 = _tmp$2318;
      if (_tmp$1601) {
        moonbit_incref(_tmp$1601);
      }
      _tmp$1599 = $Option$$unwrap$3(_tmp$1601);
      moonbit_incref(entry$528);
      _tmp$1600 = entry$528;
      _old$2317 = _tmp$1599->$1;
      if (_old$2317) {
        moonbit_decref(_old$2317);
      }
      _tmp$1599->$1 = _tmp$1600;
      moonbit_decref(_tmp$1599);
      break;
    }
  }
  self$527->$6 = idx$529;
  _field$2315 = self$527->$0;
  entries$1603 = _field$2315;
  _tmp$1604 = entry$528;
  if (idx$529 < 0 || idx$529 >= Moonbit_array_length(entries$1603)) {
    moonbit_panic();
  }
  _old$2314
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1603[
      idx$529
    ];
  if (_old$2314) {
    moonbit_decref(_old$2314);
  }
  entries$1603[idx$529] = _tmp$1604;
  size$1606 = self$527->$1;
  _tmp$1605 = size$1606 + 1;
  self$527->$1 = _tmp$1605;
  moonbit_decref(self$527);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$523,
  int32_t idx$525,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$524
) {
  int32_t _bind$522 = self$523->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2321;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1594;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1595;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2320;
  int32_t size$1597;
  int32_t _tmp$1596;
  switch (_bind$522) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1589;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2322;
      moonbit_incref(entry$524);
      _tmp$1589 = entry$524;
      _old$2322 = self$523->$5;
      if (_old$2322) {
        moonbit_decref(_old$2322);
      }
      self$523->$5 = _tmp$1589;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2325 =
        self$523->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1593 =
        _field$2325;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2324;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1592;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1590;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1591;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2323;
      if (_bind$522 < 0 || _bind$522 >= Moonbit_array_length(entries$1593)) {
        moonbit_panic();
      }
      _tmp$2324
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1593[
          _bind$522
        ];
      _tmp$1592 = _tmp$2324;
      if (_tmp$1592) {
        moonbit_incref(_tmp$1592);
      }
      _tmp$1590 = $Option$$unwrap$2(_tmp$1592);
      moonbit_incref(entry$524);
      _tmp$1591 = entry$524;
      _old$2323 = _tmp$1590->$1;
      if (_old$2323) {
        moonbit_decref(_old$2323);
      }
      _tmp$1590->$1 = _tmp$1591;
      moonbit_decref(_tmp$1590);
      break;
    }
  }
  self$523->$6 = idx$525;
  _field$2321 = self$523->$0;
  entries$1594 = _field$2321;
  _tmp$1595 = entry$524;
  if (idx$525 < 0 || idx$525 >= Moonbit_array_length(entries$1594)) {
    moonbit_panic();
  }
  _old$2320
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1594[
      idx$525
    ];
  if (_old$2320) {
    moonbit_decref(_old$2320);
  }
  entries$1594[idx$525] = _tmp$1595;
  size$1597 = self$523->$1;
  _tmp$1596 = size$1597 + 1;
  self$523->$1 = _tmp$1596;
  moonbit_decref(self$523);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$519,
  int32_t idx$521,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$520
) {
  int32_t _bind$518 = self$519->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2327;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1585;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1586;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2326;
  int32_t size$1588;
  int32_t _tmp$1587;
  switch (_bind$518) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1580;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2328;
      moonbit_incref(entry$520);
      _tmp$1580 = entry$520;
      _old$2328 = self$519->$5;
      if (_old$2328) {
        moonbit_decref(_old$2328);
      }
      self$519->$5 = _tmp$1580;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$2331 =
        self$519->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$1584 =
        _field$2331;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2330;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1583;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1581;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1582;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$2329;
      if (_bind$518 < 0 || _bind$518 >= Moonbit_array_length(entries$1584)) {
        moonbit_panic();
      }
      _tmp$2330
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1584[
          _bind$518
        ];
      _tmp$1583 = _tmp$2330;
      if (_tmp$1583) {
        moonbit_incref(_tmp$1583);
      }
      _tmp$1581 = $Option$$unwrap$1(_tmp$1583);
      moonbit_incref(entry$520);
      _tmp$1582 = entry$520;
      _old$2329 = _tmp$1581->$1;
      if (_old$2329) {
        moonbit_decref(_old$2329);
      }
      _tmp$1581->$1 = _tmp$1582;
      moonbit_decref(_tmp$1581);
      break;
    }
  }
  self$519->$6 = idx$521;
  _field$2327 = self$519->$0;
  entries$1585 = _field$2327;
  _tmp$1586 = entry$520;
  if (idx$521 < 0 || idx$521 >= Moonbit_array_length(entries$1585)) {
    moonbit_panic();
  }
  _old$2326
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$1585[
      idx$521
    ];
  if (_old$2326) {
    moonbit_decref(_old$2326);
  }
  entries$1585[idx$521] = _tmp$1586;
  size$1588 = self$519->$1;
  _tmp$1587 = size$1588 + 1;
  self$519->$1 = _tmp$1587;
  moonbit_decref(self$519);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$515,
  int32_t idx$517,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$516
) {
  int32_t _bind$514 = self$515->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2333;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1576;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1577;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2332;
  int32_t size$1579;
  int32_t _tmp$1578;
  switch (_bind$514) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1571;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2334;
      moonbit_incref(entry$516);
      _tmp$1571 = entry$516;
      _old$2334 = self$515->$5;
      if (_old$2334) {
        moonbit_decref(_old$2334);
      }
      self$515->$5 = _tmp$1571;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$2337 =
        self$515->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$1575 =
        _field$2337;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2336;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1574;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1572;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1573;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$2335;
      if (_bind$514 < 0 || _bind$514 >= Moonbit_array_length(entries$1575)) {
        moonbit_panic();
      }
      _tmp$2336
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1575[
          _bind$514
        ];
      _tmp$1574 = _tmp$2336;
      if (_tmp$1574) {
        moonbit_incref(_tmp$1574);
      }
      _tmp$1572 = $Option$$unwrap$0(_tmp$1574);
      moonbit_incref(entry$516);
      _tmp$1573 = entry$516;
      _old$2335 = _tmp$1572->$1;
      if (_old$2335) {
        moonbit_decref(_old$2335);
      }
      _tmp$1572->$1 = _tmp$1573;
      moonbit_decref(_tmp$1572);
      break;
    }
  }
  self$515->$6 = idx$517;
  _field$2333 = self$515->$0;
  entries$1576 = _field$2333;
  _tmp$1577 = entry$516;
  if (idx$517 < 0 || idx$517 >= Moonbit_array_length(entries$1576)) {
    moonbit_panic();
  }
  _old$2332
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$1576[
      idx$517
    ];
  if (_old$2332) {
    moonbit_decref(_old$2332);
  }
  entries$1576[idx$517] = _tmp$1577;
  size$1579 = self$515->$1;
  _tmp$1578 = size$1579 + 1;
  self$515->$1 = _tmp$1578;
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
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1570 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$512 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$508, _tmp$1570
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$513 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2604 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2604)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2604->$0 = _bind$512;
  _block$2604->$1 = 0;
  _block$2604->$2 = capacity$508;
  _block$2604->$3 = _bind$510;
  _block$2604->$4 = _bind$511;
  _block$2604->$5 = _bind$513;
  _block$2604->$6 = -1;
  return _block$2604;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$503
) {
  int32_t capacity$502 = $Int$$next_power_of_two(capacity$503);
  int32_t _bind$504 = capacity$502 - 1;
  int32_t _bind$505 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$502);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1569 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$506 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$502, _tmp$1569
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$507 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2605 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2605)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2605->$0 = _bind$506;
  _block$2605->$1 = 0;
  _block$2605->$2 = capacity$502;
  _block$2605->$3 = _bind$504;
  _block$2605->$4 = _bind$505;
  _block$2605->$5 = _bind$507;
  _block$2605->$6 = -1;
  return _block$2605;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$497
) {
  int32_t capacity$496 = $Int$$next_power_of_two(capacity$497);
  int32_t _bind$498 = capacity$496 - 1;
  int32_t _bind$499 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$496);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1568 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$500 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$496, _tmp$1568
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$501 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$2606 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2606)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2606->$0 = _bind$500;
  _block$2606->$1 = 0;
  _block$2606->$2 = capacity$496;
  _block$2606->$3 = _bind$498;
  _block$2606->$4 = _bind$499;
  _block$2606->$5 = _bind$501;
  _block$2606->$6 = -1;
  return _block$2606;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$491
) {
  int32_t capacity$490 = $Int$$next_power_of_two(capacity$491);
  int32_t _bind$492 = capacity$490 - 1;
  int32_t _bind$493 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$490);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$1567 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$494 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$490, _tmp$1567
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$495 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$2607 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$2607)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$2607->$0 = _bind$494;
  _block$2607->$1 = 0;
  _block$2607->$2 = capacity$490;
  _block$2607->$3 = _bind$492;
  _block$2607->$4 = _bind$493;
  _block$2607->$5 = _bind$495;
  _block$2607->$6 = -1;
  return _block$2607;
}

int32_t $Int$$next_power_of_two(int32_t self$489) {
  if (self$489 >= 0) {
    int32_t _tmp$1566;
    int32_t _tmp$1565;
    int32_t _tmp$1564;
    int32_t _tmp$1563;
    if (self$489 <= 1) {
      return 1;
    }
    if (self$489 > 1073741824) {
      return 1073741824;
    }
    _tmp$1566 = self$489 - 1;
    _tmp$1565 = moonbit_clz32(_tmp$1566);
    _tmp$1564 = _tmp$1565 - 1;
    _tmp$1563 = 2147483647 >> (_tmp$1564 & 31);
    return _tmp$1563 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$488) {
  int32_t _tmp$1562 = capacity$488 * 13;
  return _tmp$1562 / 16;
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1561 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$479);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$1561);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$478
) {
  moonbit_string_t* _field$2339 = self$478->$0;
  moonbit_string_t* buf$1559 = _field$2339;
  int32_t _field$2338 = self$478->$1;
  int32_t _cnt$2513 = Moonbit_object_header(self$478)->rc;
  int32_t len$1560;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$1558;
  if (_cnt$2513 > 1) {
    int32_t _new_cnt$2514;
    moonbit_incref(buf$1559);
    _new_cnt$2514 = _cnt$2513 - 1;
    Moonbit_object_header(self$478)->rc = _new_cnt$2514;
  } else if (_cnt$2513 == 1) {
    moonbit_free(self$478);
  }
  len$1560 = _field$2338;
  _tmp$1558
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$1560, buf$1559
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$1558);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$476
) {
  struct $Ref$3c$Int$3e$* i$475 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$2608;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$1547;
  Moonbit_object_header(i$475)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$475->$0 = 0;
  _closure$2608
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$2608)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$2608->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$2608->$0_0 = self$476.$0;
  _closure$2608->$0_1 = self$476.$1;
  _closure$2608->$0_2 = self$476.$2;
  _closure$2608->$1 = i$475;
  _tmp$1547 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$2608;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$1547);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$1548
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$1549 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$1548;
  struct $Ref$3c$Int$3e$* _field$2344 = _casted_env$1549->$1;
  struct $Ref$3c$Int$3e$* i$475 = _field$2344;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$2343 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$1549->$0_1, _casted_env$1549->$0_2, _casted_env$1549->$0_0
    };
  int32_t _cnt$2515 = Moonbit_object_header(_casted_env$1549)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$476;
  int32_t val$1550;
  int32_t _tmp$1551;
  if (_cnt$2515 > 1) {
    int32_t _new_cnt$2516;
    moonbit_incref(i$475);
    moonbit_incref(_field$2343.$0);
    _new_cnt$2516 = _cnt$2515 - 1;
    Moonbit_object_header(_casted_env$1549)->rc = _new_cnt$2516;
  } else if (_cnt$2515 == 1) {
    moonbit_free(_casted_env$1549);
  }
  self$476 = _field$2343;
  val$1550 = i$475->$0;
  moonbit_incref(self$476.$0);
  _tmp$1551 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$476);
  if (val$1550 < _tmp$1551) {
    moonbit_string_t* _field$2342 = self$476.$0;
    moonbit_string_t* buf$1554 = _field$2342;
    int32_t _field$2341 = self$476.$1;
    int32_t start$1556 = _field$2341;
    int32_t val$1557 = i$475->$0;
    int32_t _tmp$1555 = start$1556 + val$1557;
    moonbit_string_t _tmp$2340 = (moonbit_string_t)buf$1554[_tmp$1555];
    moonbit_string_t elem$477;
    int32_t val$1553;
    int32_t _tmp$1552;
    moonbit_incref(_tmp$2340);
    moonbit_decref(buf$1554);
    elem$477 = _tmp$2340;
    val$1553 = i$475->$0;
    _tmp$1552 = val$1553 + 1;
    i$475->$0 = _tmp$1552;
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
  moonbit_string_t _tmp$1546 = $Int$$to_string$inner(self$473, 10);
  logger$472.$0->$method_0(logger$472.$1, _tmp$1546);
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
  int32_t len$1541 = self$466->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1543;
  int32_t _tmp$2347;
  int32_t _tmp$1542;
  int32_t length$467;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2346;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$1544;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2345;
  int32_t _tmp$1545;
  moonbit_incref(self$466);
  _tmp$1543 = $$moonbitlang$core$builtin$Array$$buffer$2(self$466);
  _tmp$2347 = Moonbit_array_length(_tmp$1543);
  moonbit_decref(_tmp$1543);
  _tmp$1542 = _tmp$2347;
  if (len$1541 == _tmp$1542) {
    moonbit_incref(self$466);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$466);
  }
  length$467 = self$466->$1;
  _field$2346 = self$466->$0;
  buf$1544 = _field$2346;
  _old$2345
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$1544[
      length$467
    ];
  if (_old$2345) {
    moonbit_decref(_old$2345);
  }
  buf$1544[length$467] = value$468;
  _tmp$1545 = length$467 + 1;
  self$466->$1 = _tmp$1545;
  moonbit_decref(self$466);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$463,
  struct $$3c$String$2a$Int$3e$* value$465
) {
  int32_t len$1536 = self$463->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$1538;
  int32_t _tmp$2350;
  int32_t _tmp$1537;
  int32_t length$464;
  struct $$3c$String$2a$Int$3e$** _field$2349;
  struct $$3c$String$2a$Int$3e$** buf$1539;
  struct $$3c$String$2a$Int$3e$* _old$2348;
  int32_t _tmp$1540;
  moonbit_incref(self$463);
  _tmp$1538 = $$moonbitlang$core$builtin$Array$$buffer$0(self$463);
  _tmp$2350 = Moonbit_array_length(_tmp$1538);
  moonbit_decref(_tmp$1538);
  _tmp$1537 = _tmp$2350;
  if (len$1536 == _tmp$1537) {
    moonbit_incref(self$463);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$463);
  }
  length$464 = self$463->$1;
  _field$2349 = self$463->$0;
  buf$1539 = _field$2349;
  _old$2348 = (struct $$3c$String$2a$Int$3e$*)buf$1539[length$464];
  if (_old$2348) {
    moonbit_decref(_old$2348);
  }
  buf$1539[length$464] = value$465;
  _tmp$1540 = length$464 + 1;
  self$463->$1 = _tmp$1540;
  moonbit_decref(self$463);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$460,
  moonbit_string_t value$462
) {
  int32_t len$1531 = self$460->$1;
  moonbit_string_t* _tmp$1533;
  int32_t _tmp$2353;
  int32_t _tmp$1532;
  int32_t length$461;
  moonbit_string_t* _field$2352;
  moonbit_string_t* buf$1534;
  moonbit_string_t _old$2351;
  int32_t _tmp$1535;
  moonbit_incref(self$460);
  _tmp$1533 = $$moonbitlang$core$builtin$Array$$buffer$1(self$460);
  _tmp$2353 = Moonbit_array_length(_tmp$1533);
  moonbit_decref(_tmp$1533);
  _tmp$1532 = _tmp$2353;
  if (len$1531 == _tmp$1532) {
    moonbit_incref(self$460);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$460);
  }
  length$461 = self$460->$1;
  _field$2352 = self$460->$0;
  buf$1534 = _field$2352;
  _old$2351 = (moonbit_string_t)buf$1534[length$461];
  moonbit_decref(_old$2351);
  buf$1534[length$461] = value$462;
  _tmp$1535 = length$461 + 1;
  self$460->$1 = _tmp$1535;
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
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2355 =
    self$448->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$447 =
    _field$2355;
  int32_t old_cap$449 = Moonbit_array_length(old_buf$447);
  int32_t copy_len$450;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$2354;
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
  _old$2354 = self$448->$0;
  moonbit_decref(_old$2354);
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
  struct $$3c$String$2a$Int$3e$** _field$2357 = self$442->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$441 = _field$2357;
  int32_t old_cap$443 = Moonbit_array_length(old_buf$441);
  int32_t copy_len$444;
  struct $$3c$String$2a$Int$3e$** _old$2356;
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
  _old$2356 = self$442->$0;
  moonbit_decref(_old$2356);
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
  moonbit_string_t* _field$2359 = self$436->$0;
  moonbit_string_t* old_buf$435 = _field$2359;
  int32_t old_cap$437 = Moonbit_array_length(old_buf$435);
  int32_t copy_len$438;
  moonbit_string_t* _old$2358;
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
  _old$2358 = self$436->$0;
  moonbit_decref(_old$2358);
  self$436->$0 = new_buf$433;
  moonbit_decref(self$436);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$432
) {
  if (capacity$432 == 0) {
    moonbit_string_t* _tmp$1529 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2609 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2609)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2609->$0 = _tmp$1529;
    _block$2609->$1 = 0;
    return _block$2609;
  } else {
    moonbit_string_t* _tmp$1530 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$432, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$2610 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$2610)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$2610->$0 = _tmp$1530;
    _block$2610->$1 = 0;
    return _block$2610;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$430,
  struct $StringView str$431
) {
  int32_t len$1517 = self$430->$1;
  int32_t _tmp$1519;
  int32_t _tmp$1518;
  int32_t _tmp$1516;
  moonbit_bytes_t _field$2360;
  moonbit_bytes_t data$1520;
  int32_t len$1521;
  moonbit_string_t _tmp$1522;
  int32_t _tmp$1523;
  int32_t _tmp$1524;
  int32_t len$1526;
  int32_t _tmp$1528;
  int32_t _tmp$1527;
  int32_t _tmp$1525;
  moonbit_incref(str$431.$0);
  _tmp$1519 = $StringView$$length(str$431);
  _tmp$1518 = _tmp$1519 * 2;
  _tmp$1516 = len$1517 + _tmp$1518;
  moonbit_incref(self$430);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$430, _tmp$1516
  );
  _field$2360 = self$430->$0;
  data$1520 = _field$2360;
  len$1521 = self$430->$1;
  moonbit_incref(data$1520);
  moonbit_incref(str$431.$0);
  _tmp$1522 = $StringView$$data(str$431);
  moonbit_incref(str$431.$0);
  _tmp$1523 = $StringView$$start_offset(str$431);
  moonbit_incref(str$431.$0);
  _tmp$1524 = $StringView$$length(str$431);
  $FixedArray$$blit_from_string(
    data$1520, len$1521, _tmp$1522, _tmp$1523, _tmp$1524
  );
  len$1526 = self$430->$1;
  _tmp$1528 = $StringView$$length(str$431);
  _tmp$1527 = _tmp$1528 * 2;
  _tmp$1525 = len$1526 + _tmp$1527;
  self$430->$1 = _tmp$1525;
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
    int32_t _tmp$1515 = -i$428;
    return $String$$offset_of_nth_char_backward(
             self$427, _tmp$1515, start_offset$429, end_offset$424
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$422,
  int32_t n$420,
  int32_t start_offset$416,
  int32_t end_offset$417
) {
  int32_t _if_result$2611;
  if (start_offset$416 >= 0) {
    _if_result$2611 = start_offset$416 <= end_offset$417;
  } else {
    _if_result$2611 = 0;
  }
  if (_if_result$2611) {
    int32_t utf16_offset$418 = start_offset$416;
    int32_t char_count$419 = 0;
    int32_t _tmp$1513;
    int32_t _if_result$2614;
    while (1) {
      int32_t _tmp$1507 = utf16_offset$418;
      int32_t _if_result$2613;
      if (_tmp$1507 < end_offset$417) {
        int32_t _tmp$1506 = char_count$419;
        _if_result$2613 = _tmp$1506 < n$420;
      } else {
        _if_result$2613 = 0;
      }
      if (_if_result$2613) {
        int32_t _tmp$1511 = utf16_offset$418;
        int32_t c$421 = self$422[_tmp$1511];
        int32_t _tmp$1510;
        if ($Int$$is_leading_surrogate(c$421)) {
          int32_t _tmp$1508 = utf16_offset$418;
          utf16_offset$418 = _tmp$1508 + 2;
        } else {
          int32_t _tmp$1509 = utf16_offset$418;
          utf16_offset$418 = _tmp$1509 + 1;
        }
        _tmp$1510 = char_count$419;
        char_count$419 = _tmp$1510 + 1;
        continue;
      } else {
        moonbit_decref(self$422);
      }
      break;
    }
    _tmp$1513 = char_count$419;
    if (_tmp$1513 < n$420) {
      _if_result$2614 = 1;
    } else {
      int32_t _tmp$1512 = utf16_offset$418;
      _if_result$2614 = _tmp$1512 >= end_offset$417;
    }
    if (_if_result$2614) {
      return 4294967296ll;
    } else {
      int32_t _tmp$1514 = utf16_offset$418;
      return (int64_t)_tmp$1514;
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
  int32_t _tmp$1504;
  int32_t _if_result$2617;
  while (1) {
    int32_t _tmp$1497 = utf16_offset$409;
    int32_t _tmp$1496 = _tmp$1497 - 1;
    int32_t _if_result$2616;
    if (_tmp$1496 >= start_offset$411) {
      int32_t _tmp$1495 = char_count$408;
      _if_result$2616 = _tmp$1495 < n$412;
    } else {
      _if_result$2616 = 0;
    }
    if (_if_result$2616) {
      int32_t _tmp$1502 = utf16_offset$409;
      int32_t _tmp$1501 = _tmp$1502 - 1;
      int32_t c$413 = self$414[_tmp$1501];
      int32_t _tmp$1500;
      if ($Int$$is_trailing_surrogate(c$413)) {
        int32_t _tmp$1498 = utf16_offset$409;
        utf16_offset$409 = _tmp$1498 - 2;
      } else {
        int32_t _tmp$1499 = utf16_offset$409;
        utf16_offset$409 = _tmp$1499 - 1;
      }
      _tmp$1500 = char_count$408;
      char_count$408 = _tmp$1500 + 1;
      continue;
    } else {
      moonbit_decref(self$414);
    }
    break;
  }
  _tmp$1504 = char_count$408;
  if (_tmp$1504 < n$412) {
    _if_result$2617 = 1;
  } else {
    int32_t _tmp$1503 = utf16_offset$409;
    _if_result$2617 = _tmp$1503 < start_offset$411;
  }
  if (_if_result$2617) {
    return 4294967296ll;
  } else {
    int32_t _tmp$1505 = utf16_offset$409;
    return (int64_t)_tmp$1505;
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
    int32_t _if_result$2619;
    if (index$401 < end_offset$397) {
      _if_result$2619 = count$402 < len$403;
    } else {
      _if_result$2619 = 0;
    }
    if (_if_result$2619) {
      int32_t c1$404 = self$400[index$401];
      int32_t _if_result$2620;
      int32_t _tmp$1493;
      int32_t _tmp$1494;
      if ($Int$$is_leading_surrogate(c1$404)) {
        int32_t _tmp$1489 = index$401 + 1;
        _if_result$2620 = _tmp$1489 < end_offset$397;
      } else {
        _if_result$2620 = 0;
      }
      if (_if_result$2620) {
        int32_t _tmp$1492 = index$401 + 1;
        int32_t c2$405 = self$400[_tmp$1492];
        if ($Int$$is_trailing_surrogate(c2$405)) {
          int32_t _tmp$1490 = index$401 + 2;
          int32_t _tmp$1491 = count$402 + 1;
          index$401 = _tmp$1490;
          count$402 = _tmp$1491;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_16.data,
              (moonbit_string_t)moonbit_string_literal_17.data
          );
        }
      }
      _tmp$1493 = index$401 + 1;
      _tmp$1494 = count$402 + 1;
      index$401 = _tmp$1493;
      count$402 = _tmp$1494;
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
  int32_t end$1487 = self$396.$2;
  int32_t _field$2361 = self$396.$1;
  int32_t start$1488;
  moonbit_decref(self$396.$0);
  start$1488 = _field$2361;
  return end$1487 - start$1488;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$395
) {
  int32_t end$1485 = self$395.$2;
  int32_t _field$2362 = self$395.$1;
  int32_t start$1486;
  moonbit_decref(self$395.$0);
  start$1486 = _field$2362;
  return end$1485 - start$1486;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$394
) {
  int32_t end$1483 = self$394.$2;
  int32_t _field$2363 = self$394.$1;
  int32_t start$1484;
  moonbit_decref(self$394.$0);
  start$1484 = _field$2363;
  return end$1483 - start$1484;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$393
) {
  int32_t end$1481 = self$393.$2;
  int32_t _field$2364 = self$393.$1;
  int32_t start$1482;
  moonbit_decref(self$393.$0);
  start$1482 = _field$2364;
  return end$1481 - start$1482;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$392
) {
  int32_t end$1479 = self$392.$2;
  int32_t _field$2365 = self$392.$1;
  int32_t start$1480;
  moonbit_decref(self$392.$0);
  start$1480 = _field$2365;
  return end$1479 - start$1480;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$2621 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$2621)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$2621->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$2621->$0 = self$387;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$2621;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$1477,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$385
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$1478 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$1477;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$2366 =
    _casted_env$1478->$0;
  int32_t _cnt$2517 = Moonbit_object_header(_casted_env$1478)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$387;
  if (_cnt$2517 > 1) {
    int32_t _new_cnt$2518;
    moonbit_incref(_field$2366);
    _new_cnt$2518 = _cnt$2517 - 1;
    Moonbit_object_header(_casted_env$1478)->rc = _new_cnt$2518;
  } else if (_cnt$2517 == 1) {
    moonbit_free(_casted_env$1478);
  }
  self$387 = _field$2366;
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
    struct $$moonbitlang$core$builtin$Logger _bind$1459;
    int32_t _tmp$1460;
    int32_t _tmp$1461;
    int32_t _tmp$1462;
    int32_t _tmp$2626;
    int32_t _tmp$2627;
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
        int32_t _tmp$1463;
        int32_t _tmp$1464;
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
        _tmp$1463 = i$377 + 1;
        _tmp$1464 = i$377 + 1;
        i$377 = _tmp$1463;
        seg$378 = _tmp$1464;
        goto $$2a$for$379;
        break;
      }
      
      case 13: {
        int32_t _tmp$1465;
        int32_t _tmp$1466;
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
        _tmp$1465 = i$377 + 1;
        _tmp$1466 = i$377 + 1;
        i$377 = _tmp$1465;
        seg$378 = _tmp$1466;
        goto $$2a$for$379;
        break;
      }
      
      case 8: {
        int32_t _tmp$1467;
        int32_t _tmp$1468;
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
        _tmp$1467 = i$377 + 1;
        _tmp$1468 = i$377 + 1;
        i$377 = _tmp$1467;
        seg$378 = _tmp$1468;
        goto $$2a$for$379;
        break;
      }
      
      case 9: {
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
          logger$373.$1, (moonbit_string_t)moonbit_string_literal_21.data
        );
        _tmp$1469 = i$377 + 1;
        _tmp$1470 = i$377 + 1;
        i$377 = _tmp$1469;
        seg$378 = _tmp$1470;
        goto $$2a$for$379;
        break;
      }
      default: {
        if (code$380 < 32) {
          int32_t _tmp$1473;
          moonbit_string_t _tmp$1472;
          struct $$moonbitlang$core$builtin$Logger _bind$1471;
          int32_t _tmp$1474;
          int32_t _tmp$1475;
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
          _tmp$1473 = code$380 & 0xff;
          _tmp$1472 = $Byte$$to_hex(_tmp$1473);
          if (logger$373.$1) {
            moonbit_incref(logger$373.$1);
          }
          logger$373.$0->$method_0(logger$373.$1, _tmp$1472);
          _bind$1471 = logger$373;
          if (_bind$1471.$1) {
            moonbit_incref(_bind$1471.$1);
          }
          _bind$1471.$0->$method_3(_bind$1471.$1, 125);
          _tmp$1474 = i$377 + 1;
          _tmp$1475 = i$377 + 1;
          i$377 = _tmp$1474;
          seg$378 = _tmp$1475;
          goto $$2a$for$379;
        } else {
          int32_t _tmp$1476 = i$377 + 1;
          int32_t _tmp$2625 = seg$378;
          i$377 = _tmp$1476;
          seg$378 = _tmp$2625;
          goto $$2a$for$379;
        }
        break;
      }
    }
    goto $joinlet$2624;
    $join$381:;
    moonbit_incref(_env$374);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$374, seg$378, i$377
    );
    if (logger$373.$1) {
      moonbit_incref(logger$373.$1);
    }
    logger$373.$0->$method_3(logger$373.$1, 92);
    _bind$1459 = logger$373;
    _tmp$1460 = c$382;
    if (_bind$1459.$1) {
      moonbit_incref(_bind$1459.$1);
    }
    _bind$1459.$0->$method_3(_bind$1459.$1, _tmp$1460);
    _tmp$1461 = i$377 + 1;
    _tmp$1462 = i$377 + 1;
    i$377 = _tmp$1461;
    seg$378 = _tmp$1462;
    continue;
    $joinlet$2624:;
    _tmp$2626 = i$377;
    _tmp$2627 = seg$378;
    i$377 = _tmp$2626;
    seg$378 = _tmp$2627;
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
  moonbit_string_t _field$2368 = _env$369->$1;
  moonbit_string_t self$368 = _field$2368;
  struct $$moonbitlang$core$builtin$Logger _field$2367 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$369->$0_0, _env$369->$0_1
    };
  int32_t _cnt$2519 = Moonbit_object_header(_env$369)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$370;
  if (_cnt$2519 > 1) {
    int32_t _new_cnt$2520;
    moonbit_incref(self$368);
    if (_field$2367.$1) {
      moonbit_incref(_field$2367.$1);
    }
    _new_cnt$2520 = _cnt$2519 - 1;
    Moonbit_object_header(_env$369)->rc = _new_cnt$2520;
  } else if (_cnt$2519 == 1) {
    moonbit_free(_env$369);
  }
  logger$370 = _field$2367;
  if (i$371 > seg$372) {
    int32_t _tmp$1458 = i$371 - seg$372;
    logger$370.$0->$method_1(logger$370.$1, self$368, seg$372, _tmp$1458);
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
  int32_t _tmp$1455 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$367, 16);
  int32_t _tmp$1454 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$1455);
  int32_t _tmp$1457;
  int32_t _tmp$1456;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$1453;
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1454
  );
  _tmp$1457 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$367, 16);
  _tmp$1456
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$1457
  );
  moonbit_incref(_self$366);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$366, _tmp$1456
  );
  _tmp$1453 = _self$366;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$1453);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$365) {
  if (i$365 < 10) {
    int32_t _tmp$1450 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 48);
    return $Byte$$to_char(_tmp$1450);
  } else {
    int32_t _tmp$1452 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$365, 97);
    int32_t _tmp$1451 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$1452, 10);
    return $Byte$$to_char(_tmp$1451);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$363,
  int32_t that$364
) {
  int32_t _tmp$1448 = (int32_t)self$363;
  int32_t _tmp$1449 = (int32_t)that$364;
  int32_t _tmp$1447 = _tmp$1448 - _tmp$1449;
  return _tmp$1447 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$361,
  int32_t that$362
) {
  int32_t _tmp$1445 = (int32_t)self$361;
  int32_t _tmp$1446 = (int32_t)that$362;
  int32_t _tmp$1444 = _tmp$1445 % _tmp$1446;
  return _tmp$1444 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$359,
  int32_t that$360
) {
  int32_t _tmp$1442 = (int32_t)self$359;
  int32_t _tmp$1443 = (int32_t)that$360;
  int32_t _tmp$1441 = _tmp$1442 / _tmp$1443;
  return _tmp$1441 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$357,
  int32_t that$358
) {
  int32_t _tmp$1439 = (int32_t)self$357;
  int32_t _tmp$1440 = (int32_t)that$358;
  int32_t _tmp$1438 = _tmp$1439 + _tmp$1440;
  return _tmp$1438 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$354,
  int32_t start$352,
  int32_t end$353
) {
  int32_t _if_result$2628;
  int32_t len$355;
  int32_t _tmp$1436;
  int32_t _tmp$1437;
  moonbit_bytes_t bytes$356;
  moonbit_bytes_t _tmp$1435;
  if (start$352 == 0) {
    int32_t _tmp$1434 = Moonbit_array_length(str$354);
    _if_result$2628 = end$353 == _tmp$1434;
  } else {
    _if_result$2628 = 0;
  }
  if (_if_result$2628) {
    return str$354;
  }
  len$355 = end$353 - start$352;
  _tmp$1436 = len$355 * 2;
  _tmp$1437 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$356 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1436, _tmp$1437);
  moonbit_incref(bytes$356);
  $FixedArray$$blit_from_string(bytes$356, 0, str$354, start$352, len$355);
  _tmp$1435 = bytes$356;
  return $Bytes$$to_unchecked_string$inner(_tmp$1435, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$351
) {
  return f$351;
}

moonbit_string_t $Int$$to_string$inner(int32_t self$335, int32_t radix$334) {
  int32_t _if_result$2629;
  int32_t is_negative$336;
  uint32_t num$337;
  uint16_t* buffer$338;
  if (radix$334 < 2) {
    _if_result$2629 = 1;
  } else {
    _if_result$2629 = radix$334 > 36;
  }
  if (_if_result$2629) {
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
    int32_t _tmp$1433 = -self$335;
    num$337 = *(uint32_t*)&_tmp$1433;
  } else {
    num$337 = *(uint32_t*)&self$335;
  }
  switch (radix$334) {
    case 10: {
      int32_t digit_len$339 = $moonbitlang$core$builtin$dec_count32(num$337);
      int32_t _tmp$1430;
      int32_t total_len$340;
      uint16_t* buffer$341;
      int32_t digit_start$342;
      if (is_negative$336) {
        _tmp$1430 = 1;
      } else {
        _tmp$1430 = 0;
      }
      total_len$340 = digit_len$339 + _tmp$1430;
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
      int32_t _tmp$1431;
      int32_t total_len$344;
      uint16_t* buffer$345;
      int32_t digit_start$346;
      if (is_negative$336) {
        _tmp$1431 = 1;
      } else {
        _tmp$1431 = 0;
      }
      total_len$344 = digit_len$343 + _tmp$1431;
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
      int32_t _tmp$1432;
      int32_t total_len$348;
      uint16_t* buffer$349;
      int32_t digit_start$350;
      if (is_negative$336) {
        _tmp$1432 = 1;
      } else {
        _tmp$1432 = 0;
      }
      total_len$348 = digit_len$347 + _tmp$1432;
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
    uint32_t _tmp$1427 = num$329;
    if (_tmp$1427 > 0u) {
      int32_t _tmp$1428 = count$332;
      uint32_t _tmp$1429;
      count$332 = _tmp$1428 + 1;
      _tmp$1429 = num$329;
      num$329 = _tmp$1429 / base$330;
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
    int32_t _tmp$1426 = 31 - leading_zeros$327;
    int32_t _tmp$1425 = _tmp$1426 / 4;
    return _tmp$1425 + 1;
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
  uint32_t _tmp$1424;
  int32_t remaining$317;
  int32_t _tmp$1405;
  while (1) {
    uint32_t _tmp$1368 = num$302;
    if (_tmp$1368 >= 10000u) {
      uint32_t _tmp$1391 = num$302;
      uint32_t t$307 = _tmp$1391 / 10000u;
      uint32_t _tmp$1390 = num$302;
      uint32_t _tmp$1389 = _tmp$1390 % 10000u;
      int32_t r$308 = *(int32_t*)&_tmp$1389;
      int32_t d1$309;
      int32_t d2$310;
      int32_t _tmp$1369;
      int32_t _tmp$1388;
      int32_t _tmp$1387;
      int32_t d1_hi$311;
      int32_t _tmp$1386;
      int32_t _tmp$1385;
      int32_t d1_lo$312;
      int32_t _tmp$1384;
      int32_t _tmp$1383;
      int32_t d2_hi$313;
      int32_t _tmp$1382;
      int32_t _tmp$1381;
      int32_t d2_lo$314;
      int32_t _tmp$1371;
      int32_t _tmp$1370;
      int32_t _tmp$1374;
      int32_t _tmp$1373;
      int32_t _tmp$1372;
      int32_t _tmp$1377;
      int32_t _tmp$1376;
      int32_t _tmp$1375;
      int32_t _tmp$1380;
      int32_t _tmp$1379;
      int32_t _tmp$1378;
      num$302 = t$307;
      d1$309 = r$308 / 100;
      d2$310 = r$308 % 100;
      _tmp$1369 = offset$304;
      offset$304 = _tmp$1369 - 4;
      _tmp$1388 = d1$309 / 10;
      _tmp$1387 = 48 + _tmp$1388;
      d1_hi$311 = (uint16_t)_tmp$1387;
      _tmp$1386 = d1$309 % 10;
      _tmp$1385 = 48 + _tmp$1386;
      d1_lo$312 = (uint16_t)_tmp$1385;
      _tmp$1384 = d2$310 / 10;
      _tmp$1383 = 48 + _tmp$1384;
      d2_hi$313 = (uint16_t)_tmp$1383;
      _tmp$1382 = d2$310 % 10;
      _tmp$1381 = 48 + _tmp$1382;
      d2_lo$314 = (uint16_t)_tmp$1381;
      _tmp$1371 = offset$304;
      _tmp$1370 = digit_start$306 + _tmp$1371;
      buffer$315[_tmp$1370] = d1_hi$311;
      _tmp$1374 = offset$304;
      _tmp$1373 = digit_start$306 + _tmp$1374;
      _tmp$1372 = _tmp$1373 + 1;
      buffer$315[_tmp$1372] = d1_lo$312;
      _tmp$1377 = offset$304;
      _tmp$1376 = digit_start$306 + _tmp$1377;
      _tmp$1375 = _tmp$1376 + 2;
      buffer$315[_tmp$1375] = d2_hi$313;
      _tmp$1380 = offset$304;
      _tmp$1379 = digit_start$306 + _tmp$1380;
      _tmp$1378 = _tmp$1379 + 3;
      buffer$315[_tmp$1378] = d2_lo$314;
      continue;
    }
    break;
  }
  _tmp$1424 = num$302;
  remaining$317 = *(int32_t*)&_tmp$1424;
  while (1) {
    int32_t _tmp$1392 = remaining$317;
    if (_tmp$1392 >= 100) {
      int32_t _tmp$1404 = remaining$317;
      int32_t t$318 = _tmp$1404 / 100;
      int32_t _tmp$1403 = remaining$317;
      int32_t d$319 = _tmp$1403 % 100;
      int32_t _tmp$1393;
      int32_t _tmp$1402;
      int32_t _tmp$1401;
      int32_t d_hi$320;
      int32_t _tmp$1400;
      int32_t _tmp$1399;
      int32_t d_lo$321;
      int32_t _tmp$1395;
      int32_t _tmp$1394;
      int32_t _tmp$1398;
      int32_t _tmp$1397;
      int32_t _tmp$1396;
      remaining$317 = t$318;
      _tmp$1393 = offset$304;
      offset$304 = _tmp$1393 - 2;
      _tmp$1402 = d$319 / 10;
      _tmp$1401 = 48 + _tmp$1402;
      d_hi$320 = (uint16_t)_tmp$1401;
      _tmp$1400 = d$319 % 10;
      _tmp$1399 = 48 + _tmp$1400;
      d_lo$321 = (uint16_t)_tmp$1399;
      _tmp$1395 = offset$304;
      _tmp$1394 = digit_start$306 + _tmp$1395;
      buffer$315[_tmp$1394] = d_hi$320;
      _tmp$1398 = offset$304;
      _tmp$1397 = digit_start$306 + _tmp$1398;
      _tmp$1396 = _tmp$1397 + 1;
      buffer$315[_tmp$1396] = d_lo$321;
      continue;
    }
    break;
  }
  _tmp$1405 = remaining$317;
  if (_tmp$1405 >= 10) {
    int32_t _tmp$1406 = offset$304;
    int32_t _tmp$1417;
    int32_t _tmp$1416;
    int32_t _tmp$1415;
    int32_t d_hi$323;
    int32_t _tmp$1414;
    int32_t _tmp$1413;
    int32_t _tmp$1412;
    int32_t d_lo$324;
    int32_t _tmp$1408;
    int32_t _tmp$1407;
    int32_t _tmp$1411;
    int32_t _tmp$1410;
    int32_t _tmp$1409;
    offset$304 = _tmp$1406 - 2;
    _tmp$1417 = remaining$317;
    _tmp$1416 = _tmp$1417 / 10;
    _tmp$1415 = 48 + _tmp$1416;
    d_hi$323 = (uint16_t)_tmp$1415;
    _tmp$1414 = remaining$317;
    _tmp$1413 = _tmp$1414 % 10;
    _tmp$1412 = 48 + _tmp$1413;
    d_lo$324 = (uint16_t)_tmp$1412;
    _tmp$1408 = offset$304;
    _tmp$1407 = digit_start$306 + _tmp$1408;
    buffer$315[_tmp$1407] = d_hi$323;
    _tmp$1411 = offset$304;
    _tmp$1410 = digit_start$306 + _tmp$1411;
    _tmp$1409 = _tmp$1410 + 1;
    buffer$315[_tmp$1409] = d_lo$324;
    moonbit_decref(buffer$315);
  } else {
    int32_t _tmp$1418 = offset$304;
    int32_t _tmp$1423;
    int32_t _tmp$1419;
    int32_t _tmp$1422;
    int32_t _tmp$1421;
    int32_t _tmp$1420;
    offset$304 = _tmp$1418 - 1;
    _tmp$1423 = offset$304;
    _tmp$1419 = digit_start$306 + _tmp$1423;
    _tmp$1422 = remaining$317;
    _tmp$1421 = 48 + _tmp$1422;
    _tmp$1420 = (uint16_t)_tmp$1421;
    buffer$315[_tmp$1419] = _tmp$1420;
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
  int32_t _tmp$1348 = radix$293 - 1;
  int32_t _tmp$1347 = radix$293 & _tmp$1348;
  if (_tmp$1347 == 0) {
    int32_t shift$294 = moonbit_ctz32(radix$293);
    uint32_t mask$295 = base$292 - 1u;
    while (1) {
      uint32_t _tmp$1349 = n$290;
      if (_tmp$1349 > 0u) {
        int32_t _tmp$1350 = offset$287;
        uint32_t _tmp$1357;
        uint32_t _tmp$1356;
        int32_t digit$296;
        int32_t _tmp$1354;
        int32_t _tmp$1351;
        int32_t _tmp$1353;
        int32_t _tmp$1352;
        uint32_t _tmp$1355;
        offset$287 = _tmp$1350 - 1;
        _tmp$1357 = n$290;
        _tmp$1356 = _tmp$1357 & mask$295;
        digit$296 = *(int32_t*)&_tmp$1356;
        _tmp$1354 = offset$287;
        _tmp$1351 = digit_start$289 + _tmp$1354;
        _tmp$1353
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$296
        ];
        _tmp$1352 = (uint16_t)_tmp$1353;
        buffer$297[_tmp$1351] = _tmp$1352;
        _tmp$1355 = n$290;
        n$290 = _tmp$1355 >> (shift$294 & 31);
        continue;
      } else {
        moonbit_decref(buffer$297);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$1358 = n$290;
      if (_tmp$1358 > 0u) {
        int32_t _tmp$1359 = offset$287;
        uint32_t _tmp$1367;
        uint32_t q$299;
        uint32_t _tmp$1365;
        uint32_t _tmp$1366;
        uint32_t _tmp$1364;
        int32_t digit$300;
        int32_t _tmp$1363;
        int32_t _tmp$1360;
        int32_t _tmp$1362;
        int32_t _tmp$1361;
        offset$287 = _tmp$1359 - 1;
        _tmp$1367 = n$290;
        q$299 = _tmp$1367 / base$292;
        _tmp$1365 = n$290;
        _tmp$1366 = q$299 * base$292;
        _tmp$1364 = _tmp$1365 - _tmp$1366;
        digit$300 = *(int32_t*)&_tmp$1364;
        _tmp$1363 = offset$287;
        _tmp$1360 = digit_start$289 + _tmp$1363;
        _tmp$1362
        = ((moonbit_string_t)moonbit_string_literal_26.data)[
          digit$300
        ];
        _tmp$1361 = (uint16_t)_tmp$1362;
        buffer$297[_tmp$1360] = _tmp$1361;
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
  int32_t _tmp$1342;
  while (1) {
    int32_t _tmp$1328 = offset$276;
    if (_tmp$1328 >= 2) {
      int32_t _tmp$1329 = offset$276;
      uint32_t _tmp$1341;
      uint32_t _tmp$1340;
      int32_t byte_val$281;
      int32_t hi$282;
      int32_t lo$283;
      int32_t _tmp$1333;
      int32_t _tmp$1330;
      int32_t _tmp$1332;
      int32_t _tmp$1331;
      int32_t _tmp$1338;
      int32_t _tmp$1337;
      int32_t _tmp$1334;
      int32_t _tmp$1336;
      int32_t _tmp$1335;
      uint32_t _tmp$1339;
      offset$276 = _tmp$1329 - 2;
      _tmp$1341 = n$279;
      _tmp$1340 = _tmp$1341 & 255u;
      byte_val$281 = *(int32_t*)&_tmp$1340;
      hi$282 = byte_val$281 / 16;
      lo$283 = byte_val$281 % 16;
      _tmp$1333 = offset$276;
      _tmp$1330 = digit_start$278 + _tmp$1333;
      _tmp$1332 = ((moonbit_string_t)moonbit_string_literal_26.data)[hi$282];
      _tmp$1331 = (uint16_t)_tmp$1332;
      buffer$284[_tmp$1330] = _tmp$1331;
      _tmp$1338 = offset$276;
      _tmp$1337 = digit_start$278 + _tmp$1338;
      _tmp$1334 = _tmp$1337 + 1;
      _tmp$1336 = ((moonbit_string_t)moonbit_string_literal_26.data)[lo$283];
      _tmp$1335 = (uint16_t)_tmp$1336;
      buffer$284[_tmp$1334] = _tmp$1335;
      _tmp$1339 = n$279;
      n$279 = _tmp$1339 >> 8;
      continue;
    }
    break;
  }
  _tmp$1342 = offset$276;
  if (_tmp$1342 == 1) {
    uint32_t _tmp$1346 = n$279;
    uint32_t _tmp$1345 = _tmp$1346 & 15u;
    int32_t nibble$286 = *(int32_t*)&_tmp$1345;
    int32_t _tmp$1344 =
      ((moonbit_string_t)moonbit_string_literal_26.data)[nibble$286];
    int32_t _tmp$1343 = (uint16_t)_tmp$1344;
    buffer$284[digit_start$278] = _tmp$1343;
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
  struct $$moonbitlang$core$builtin$Logger _tmp$1327;
  moonbit_incref(logger$274);
  _tmp$1327
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$274
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$275, _tmp$1327
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$274);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  void* self$273
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$272 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1326;
  moonbit_incref(logger$272);
  _tmp$1326
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$272
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$273, _tmp$1326
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$272);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$271
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$270 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1325;
  moonbit_incref(logger$270);
  _tmp$1325
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$270
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$271, _tmp$1325
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$270);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$269
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$268 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1324;
  moonbit_incref(logger$268);
  _tmp$1324
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$268
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$269, _tmp$1324);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$268);
}

int32_t $StringView$$start_offset(struct $StringView self$267) {
  int32_t _field$2369 = self$267.$1;
  moonbit_decref(self$267.$0);
  return _field$2369;
}

moonbit_string_t $StringView$$data(struct $StringView self$266) {
  moonbit_string_t _field$2370 = self$266.$0;
  return _field$2370;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$260,
  moonbit_string_t value$263,
  int32_t start$264,
  int32_t len$265
) {
  void* _try_err$262;
  struct $StringView _tmp$1319;
  int32_t _tmp$1321 = start$264 + len$265;
  int64_t _tmp$1320 = (int64_t)_tmp$1321;
  struct moonbit_result_1 _tmp$2637 =
    $String$$sub$inner(value$263, start$264, _tmp$1320);
  if (_tmp$2637.tag) {
    struct $StringView const _ok$1322 = _tmp$2637.data.ok;
    _tmp$1319 = _ok$1322;
  } else {
    void* const _err$1323 = _tmp$2637.data.err;
    _try_err$262 = _err$1323;
    goto $join$261;
  }
  goto $joinlet$2636;
  $join$261:;
  moonbit_decref(_try_err$262);
  moonbit_panic();
  $joinlet$2636:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$260, _tmp$1319
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
  int32_t _if_result$2638;
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
      _if_result$2638 = end$254 <= len$252;
    } else {
      _if_result$2638 = 0;
    }
  } else {
    _if_result$2638 = 0;
  }
  if (_if_result$2638) {
    int32_t _if_result$2639;
    int32_t _if_result$2641;
    struct $StringView _tmp$1317;
    struct moonbit_result_1 _result$2643;
    if (start$258 < len$252) {
      int32_t _tmp$1313 = self$253[start$258];
      _if_result$2639 = $Int$$is_trailing_surrogate(_tmp$1313);
    } else {
      _if_result$2639 = 0;
    }
    if (_if_result$2639) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1314;
      struct moonbit_result_1 _result$2640;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1314
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2640.tag = 0;
      _result$2640.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1314;
      return _result$2640;
    }
    if (end$254 < len$252) {
      int32_t _tmp$1315 = self$253[end$254];
      _if_result$2641 = $Int$$is_trailing_surrogate(_tmp$1315);
    } else {
      _if_result$2641 = 0;
    }
    if (_if_result$2641) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1316;
      struct moonbit_result_1 _result$2642;
      moonbit_decref(self$253);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1316
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$2642.tag = 0;
      _result$2642.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$1316;
      return _result$2642;
    }
    _tmp$1317 = (struct $StringView){start$258, end$254, self$253};
    _result$2643.tag = 1;
    _result$2643.data.ok = _tmp$1317;
    return _result$2643;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1318;
    struct moonbit_result_1 _result$2644;
    moonbit_decref(self$253);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1318
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$2644.tag = 0;
    _result$2644.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$1318;
    return _result$2644;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$251
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$250 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1312;
  moonbit_incref(_self$250);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$250, self$251);
  _tmp$1312 = _self$250;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1312);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$249
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$248 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$1311;
  moonbit_incref(_self$248);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$248, self$249);
  _tmp$1311 = _self$248;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$1311);
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
  uint32_t _tmp$1310 = *(uint32_t*)&seed$244;
  uint32_t _tmp$1309 = _tmp$1310 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$2645 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$2645)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$2645->$0 = _tmp$1309;
  return _block$2645;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$243
) {
  uint32_t _tmp$1308 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$243);
  return *(int32_t*)&_tmp$1308;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$242
) {
  uint32_t _field$2371 = self$242->$0;
  uint32_t acc$241;
  uint32_t _tmp$1297;
  uint32_t _tmp$1299;
  uint32_t _tmp$1298;
  uint32_t _tmp$1300;
  uint32_t _tmp$1301;
  uint32_t _tmp$1303;
  uint32_t _tmp$1302;
  uint32_t _tmp$1304;
  uint32_t _tmp$1305;
  uint32_t _tmp$1307;
  uint32_t _tmp$1306;
  moonbit_decref(self$242);
  acc$241 = _field$2371;
  _tmp$1297 = acc$241;
  _tmp$1299 = acc$241;
  _tmp$1298 = _tmp$1299 >> 15;
  acc$241 = _tmp$1297 ^ _tmp$1298;
  _tmp$1300 = acc$241;
  acc$241 = _tmp$1300 * 2246822519u;
  _tmp$1301 = acc$241;
  _tmp$1303 = acc$241;
  _tmp$1302 = _tmp$1303 >> 13;
  acc$241 = _tmp$1301 ^ _tmp$1302;
  _tmp$1304 = acc$241;
  acc$241 = _tmp$1304 * 3266489917u;
  _tmp$1305 = acc$241;
  _tmp$1307 = acc$241;
  _tmp$1306 = _tmp$1307 >> 16;
  acc$241 = _tmp$1305 ^ _tmp$1306;
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
  uint32_t _tmp$1296 = *(uint32_t*)&value$236;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$235, _tmp$1296);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$233,
  uint32_t value$234
) {
  uint32_t acc$1295 = self$233->$0;
  uint32_t _tmp$1294 = acc$1295 + 4u;
  self$233->$0 = _tmp$1294;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$233, value$234);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$231,
  uint32_t input$232
) {
  uint32_t acc$1292 = self$231->$0;
  uint32_t _tmp$1293 = input$232 * 3266489917u;
  uint32_t _tmp$1291 = acc$1292 + _tmp$1293;
  uint32_t _tmp$1290 = $moonbitlang$core$builtin$rotl(_tmp$1291, 17);
  uint32_t _tmp$1289 = _tmp$1290 * 668265263u;
  self$231->$0 = _tmp$1289;
  moonbit_decref(self$231);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$229, int32_t r$230) {
  uint32_t _tmp$1286 = x$229 << (r$230 & 31);
  int32_t _tmp$1288 = 32 - r$230;
  uint32_t _tmp$1287 = x$229 >> (_tmp$1288 & 31);
  return _tmp$1286 | _tmp$1287;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$227,
  moonbit_string_t str$228
) {
  int32_t len$1276 = self$227->$1;
  int32_t _tmp$1278 = Moonbit_array_length(str$228);
  int32_t _tmp$1277 = _tmp$1278 * 2;
  int32_t _tmp$1275 = len$1276 + _tmp$1277;
  moonbit_bytes_t _field$2373;
  moonbit_bytes_t data$1279;
  int32_t len$1280;
  int32_t _tmp$1281;
  int32_t len$1283;
  int32_t _tmp$2372;
  int32_t _tmp$1285;
  int32_t _tmp$1284;
  int32_t _tmp$1282;
  moonbit_incref(self$227);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$227, _tmp$1275
  );
  _field$2373 = self$227->$0;
  data$1279 = _field$2373;
  len$1280 = self$227->$1;
  _tmp$1281 = Moonbit_array_length(str$228);
  moonbit_incref(data$1279);
  moonbit_incref(str$228);
  $FixedArray$$blit_from_string(data$1279, len$1280, str$228, 0, _tmp$1281);
  len$1283 = self$227->$1;
  _tmp$2372 = Moonbit_array_length(str$228);
  moonbit_decref(str$228);
  _tmp$1285 = _tmp$2372;
  _tmp$1284 = _tmp$1285 * 2;
  _tmp$1282 = len$1283 + _tmp$1284;
  self$227->$1 = _tmp$1282;
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
  int32_t _tmp$1274 = length$215 * 2;
  int32_t _tmp$1273 = bytes_offset$214 + _tmp$1274;
  int32_t e1$213 = _tmp$1273 - 1;
  int32_t _tmp$1272 = str_offset$217 + length$215;
  int32_t e2$216 = _tmp$1272 - 1;
  int32_t len1$218 = Moonbit_array_length(self$219);
  int32_t len2$220 = Moonbit_array_length(str$221);
  int32_t _if_result$2646;
  if (length$215 >= 0) {
    if (bytes_offset$214 >= 0) {
      if (e1$213 < len1$218) {
        if (str_offset$217 >= 0) {
          _if_result$2646 = e2$216 < len2$220;
        } else {
          _if_result$2646 = 0;
        }
      } else {
        _if_result$2646 = 0;
      }
    } else {
      _if_result$2646 = 0;
    }
  } else {
    _if_result$2646 = 0;
  }
  if (_if_result$2646) {
    int32_t end_str_offset$222 = str_offset$217 + length$215;
    int32_t i$223 = str_offset$217;
    int32_t j$224 = bytes_offset$214;
    while (1) {
      if (i$223 < end_str_offset$222) {
        int32_t _tmp$1269 = str$221[i$223];
        uint32_t c$225 = *(uint32_t*)&_tmp$1269;
        uint32_t _tmp$1265 = c$225 & 255u;
        int32_t _tmp$1264 = $UInt$$to_byte(_tmp$1265);
        int32_t _tmp$1266;
        uint32_t _tmp$1268;
        int32_t _tmp$1267;
        int32_t _tmp$1270;
        int32_t _tmp$1271;
        if (j$224 < 0 || j$224 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[j$224] = _tmp$1264;
        _tmp$1266 = j$224 + 1;
        _tmp$1268 = c$225 >> 8;
        _tmp$1267 = $UInt$$to_byte(_tmp$1268);
        if (_tmp$1266 < 0 || _tmp$1266 >= Moonbit_array_length(self$219)) {
          moonbit_panic();
        }
        self$219[_tmp$1266] = _tmp$1267;
        _tmp$1270 = i$223 + 1;
        _tmp$1271 = j$224 + 2;
        i$223 = _tmp$1270;
        j$224 = _tmp$1271;
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
  int32_t _tmp$1237 = Moonbit_array_length(repr$181);
  int64_t _tmp$1236 = (int64_t)_tmp$1237;
  moonbit_incref(repr$181);
  if ($String$$char_length_ge$inner(repr$181, 1, 0, _tmp$1236)) {
    int32_t _tmp$1263 = repr$181[0];
    int32_t _x$182 = _tmp$1263;
    if (_x$182 == 64) {
      int32_t _tmp$1262 = Moonbit_array_length(repr$181);
      int64_t _tmp$1261 = (int64_t)_tmp$1262;
      int64_t _bind$1016;
      int32_t _tmp$1259;
      int32_t _tmp$1260;
      struct $StringView _x$183;
      int32_t _tmp$1258;
      struct $StringView _tmp$1257;
      int64_t _bind$185;
      moonbit_incref(repr$181);
      _bind$1016
      = $String$$offset_of_nth_char$inner(
        repr$181, 1, 0, _tmp$1261
      );
      if (_bind$1016 == 4294967296ll) {
        _tmp$1259 = Moonbit_array_length(repr$181);
      } else {
        int64_t _Some$184 = _bind$1016;
        _tmp$1259 = (int32_t)_Some$184;
      }
      _tmp$1260 = Moonbit_array_length(repr$181);
      _x$183 = (struct $StringView){_tmp$1259, _tmp$1260, repr$181};
      _tmp$1258
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1257
      = (struct $StringView){
        0, _tmp$1258, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$183.$0);
      _bind$185 = $StringView$$find(_x$183, _tmp$1257);
      if (_bind$185 == 4294967296ll) {
        moonbit_decref(_x$183.$0);
        moonbit_panic();
      } else {
        int64_t _Some$186 = _bind$185;
        int32_t _pkg_end$187 = (int32_t)_Some$186;
        int64_t _tmp$1256 = (int64_t)_pkg_end$187;
        struct $StringView pkg$188;
        int32_t _tmp$1255;
        struct $StringView _tmp$1254;
        int64_t _bind$189;
        moonbit_incref(_x$183.$0);
        pkg$188 = $StringView$$view$inner(_x$183, 0, _tmp$1256);
        _tmp$1255
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1254
        = (struct $StringView){
          0, _tmp$1255, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$183.$0);
        _bind$189 = $StringView$$rev_find(_x$183, _tmp$1254);
        if (_bind$189 == 4294967296ll) {
          moonbit_decref(pkg$188.$0);
          moonbit_decref(_x$183.$0);
          moonbit_panic();
        } else {
          int64_t _Some$190 = _bind$189;
          int32_t _start_loc_end$191 = (int32_t)_Some$190;
          int32_t _tmp$1238 = _start_loc_end$191 + 1;
          int32_t _tmp$1239;
          moonbit_incref(_x$183.$0);
          _tmp$1239 = $StringView$$length(_x$183);
          if (_tmp$1238 < _tmp$1239) {
            int32_t _tmp$1253 = _start_loc_end$191 + 1;
            struct $StringView end_loc$192;
            struct $$3c$StringView$2a$StringView$3e$* _bind$193;
            moonbit_incref(_x$183.$0);
            end_loc$192
            = $StringView$$view$inner(
              _x$183, _tmp$1253, 4294967296ll
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
              struct $StringView _field$2377 =
                (struct $StringView){
                  _x$195->$0_1, _x$195->$0_2, _x$195->$0_0
                };
              struct $StringView _end_line$196 = _field$2377;
              struct $StringView _field$2376 =
                (struct $StringView){
                  _x$195->$1_1, _x$195->$1_2, _x$195->$1_0
                };
              int32_t _cnt$2521 = Moonbit_object_header(_x$195)->rc;
              struct $StringView _end_column$197;
              int64_t _tmp$1252;
              struct $StringView rest$198;
              int32_t _tmp$1251;
              struct $StringView _tmp$1250;
              int64_t _bind$200;
              if (_cnt$2521 > 1) {
                int32_t _new_cnt$2522;
                moonbit_incref(_field$2376.$0);
                moonbit_incref(_end_line$196.$0);
                _new_cnt$2522 = _cnt$2521 - 1;
                Moonbit_object_header(_x$195)->rc = _new_cnt$2522;
              } else if (_cnt$2521 == 1) {
                moonbit_free(_x$195);
              }
              _end_column$197 = _field$2376;
              _tmp$1252 = (int64_t)_start_loc_end$191;
              rest$198 = $StringView$$view$inner(_x$183, 0, _tmp$1252);
              _tmp$1251
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1250
              = (struct $StringView){
                0,
                  _tmp$1251,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$198.$0);
              _bind$200 = $StringView$$rev_find(rest$198, _tmp$1250);
              if (_bind$200 == 4294967296ll) {
                moonbit_decref(rest$198.$0);
                moonbit_decref(_end_column$197.$0);
                moonbit_decref(_end_line$196.$0);
                moonbit_decref(pkg$188.$0);
                goto $join$199;
              } else {
                int64_t _Some$201 = _bind$200;
                int32_t _start_line_end$202 = (int32_t)_Some$201;
                int64_t _tmp$1249 = (int64_t)_start_line_end$202;
                struct $StringView _tmp$1246;
                int32_t _tmp$1248;
                struct $StringView _tmp$1247;
                int64_t _bind$203;
                moonbit_incref(rest$198.$0);
                _tmp$1246 = $StringView$$view$inner(rest$198, 0, _tmp$1249);
                _tmp$1248
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1247
                = (struct $StringView){
                  0,
                    _tmp$1248,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$203 = $StringView$$rev_find(_tmp$1246, _tmp$1247);
                if (_bind$203 == 4294967296ll) {
                  moonbit_decref(rest$198.$0);
                  moonbit_decref(_end_column$197.$0);
                  moonbit_decref(_end_line$196.$0);
                  moonbit_decref(pkg$188.$0);
                  goto $join$199;
                } else {
                  int64_t _Some$204 = _bind$203;
                  int32_t _filename_end$205 = (int32_t)_Some$204;
                  int32_t _tmp$1240 = _filename_end$205 + 1;
                  int32_t _tmp$1241;
                  moonbit_incref(rest$198.$0);
                  _tmp$1241 = $StringView$$length(rest$198);
                  if (_tmp$1240 < _tmp$1241) {
                    int32_t _tmp$1245 = _filename_end$205 + 1;
                    struct $StringView start_loc$206;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$207;
                    moonbit_incref(rest$198.$0);
                    start_loc$206
                    = $StringView$$view$inner(
                      rest$198, _tmp$1245, 4294967296ll
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
                      struct $StringView _field$2375 =
                        (struct $StringView){
                          _x$209->$0_1, _x$209->$0_2, _x$209->$0_0
                        };
                      struct $StringView _start_line$210 = _field$2375;
                      struct $StringView _field$2374 =
                        (struct $StringView){
                          _x$209->$1_1, _x$209->$1_2, _x$209->$1_0
                        };
                      int32_t _cnt$2523 = Moonbit_object_header(_x$209)->rc;
                      struct $StringView _start_column$211;
                      int32_t _tmp$1242;
                      if (_cnt$2523 > 1) {
                        int32_t _new_cnt$2524;
                        moonbit_incref(_field$2374.$0);
                        moonbit_incref(_start_line$210.$0);
                        _new_cnt$2524 = _cnt$2523 - 1;
                        Moonbit_object_header(_x$209)->rc = _new_cnt$2524;
                      } else if (_cnt$2523 == 1) {
                        moonbit_free(_x$209);
                      }
                      _start_column$211 = _field$2374;
                      _tmp$1242 = _pkg_end$187 + 1;
                      if (_filename_end$205 > _tmp$1242) {
                        int32_t _tmp$1243 = _pkg_end$187 + 1;
                        int64_t _tmp$1244 = (int64_t)_filename_end$205;
                        struct $StringView filename$212 =
                          $StringView$$view$inner(
                            rest$198, _tmp$1243, _tmp$1244
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$2650 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$2650)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$2650->$0_0 = pkg$188.$0;
                        _block$2650->$0_1 = pkg$188.$1;
                        _block$2650->$0_2 = pkg$188.$2;
                        _block$2650->$1_0 = filename$212.$0;
                        _block$2650->$1_1 = filename$212.$1;
                        _block$2650->$1_2 = filename$212.$2;
                        _block$2650->$2_0 = _start_line$210.$0;
                        _block$2650->$2_1 = _start_line$210.$1;
                        _block$2650->$2_2 = _start_line$210.$2;
                        _block$2650->$3_0 = _start_column$211.$0;
                        _block$2650->$3_1 = _start_column$211.$1;
                        _block$2650->$3_2 = _start_column$211.$2;
                        _block$2650->$4_0 = _end_line$196.$0;
                        _block$2650->$4_1 = _end_line$196.$1;
                        _block$2650->$4_2 = _end_line$196.$2;
                        _block$2650->$5_0 = _end_column$197.$0;
                        _block$2650->$5_1 = _end_column$197.$1;
                        _block$2650->$5_2 = _end_column$197.$2;
                        return _block$2650;
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
  int32_t _tmp$1235 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1234;
  int64_t _bind$176;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1234
  = (struct $StringView){
    0, _tmp$1235, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$177.$0);
  _bind$176 = $StringView$$find(view$177, _tmp$1234);
  if (_bind$176 == 4294967296ll) {
    moonbit_decref(view$177.$0);
    return 0;
  } else {
    int64_t _Some$178 = _bind$176;
    int32_t _i$179 = (int32_t)_Some$178;
    int32_t _if_result$2651;
    if (_i$179 > 0) {
      int32_t _tmp$1227 = _i$179 + 1;
      int32_t _tmp$1228;
      moonbit_incref(view$177.$0);
      _tmp$1228 = $StringView$$length(view$177);
      _if_result$2651 = _tmp$1227 < _tmp$1228;
    } else {
      _if_result$2651 = 0;
    }
    if (_if_result$2651) {
      int64_t _tmp$1233 = (int64_t)_i$179;
      struct $StringView _tmp$1230;
      int32_t _tmp$1232;
      struct $StringView _tmp$1231;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1229;
      moonbit_incref(view$177.$0);
      _tmp$1230 = $StringView$$view$inner(view$177, 0, _tmp$1233);
      _tmp$1232 = _i$179 + 1;
      _tmp$1231 = $StringView$$view$inner(view$177, _tmp$1232, 4294967296ll);
      _tuple$1229
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1229)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1229->$0_0 = _tmp$1230.$0;
      _tuple$1229->$0_1 = _tmp$1230.$1;
      _tuple$1229->$0_2 = _tmp$1230.$2;
      _tuple$1229->$1_0 = _tmp$1231.$0;
      _tuple$1229->$1_1 = _tmp$1231.$1;
      _tuple$1229->$1_2 = _tmp$1231.$2;
      return _tuple$1229;
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
  int32_t _if_result$2652;
  if (end_offset$172 == 4294967296ll) {
    moonbit_incref(self$174.$0);
    end_offset$171 = $StringView$$length(self$174);
  } else {
    int64_t _Some$173 = end_offset$172;
    end_offset$171 = (int32_t)_Some$173;
  }
  if (start_offset$175 >= 0) {
    if (start_offset$175 <= end_offset$171) {
      int32_t _tmp$1221;
      moonbit_incref(self$174.$0);
      _tmp$1221 = $StringView$$length(self$174);
      _if_result$2652 = end_offset$171 <= _tmp$1221;
    } else {
      _if_result$2652 = 0;
    }
  } else {
    _if_result$2652 = 0;
  }
  if (_if_result$2652) {
    moonbit_string_t _field$2379 = self$174.$0;
    moonbit_string_t str$1222 = _field$2379;
    int32_t start$1226 = self$174.$1;
    int32_t _tmp$1223 = start$1226 + start_offset$175;
    int32_t _field$2378 = self$174.$1;
    int32_t start$1225 = _field$2378;
    int32_t _tmp$1224 = start$1225 + end_offset$171;
    return (struct $StringView){_tmp$1223, _tmp$1224, str$1222};
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
  int32_t _tmp$1220;
  moonbit_incref(str$169.$0);
  _tmp$1220 = $StringView$$length(str$169);
  if (_tmp$1220 <= 4) {
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
        int32_t _tmp$1207 = i$164;
        if (_tmp$1207 >= 0) {
          int32_t _tmp$1212;
          while (1) {
            int32_t _tmp$1210 = i$164;
            int32_t _if_result$2655;
            if (_tmp$1210 >= 0) {
              int32_t _tmp$1209 = i$164;
              int32_t _tmp$1208;
              moonbit_incref(haystack$160.$0);
              _tmp$1208
              = $StringView$$unsafe_charcode_at(
                haystack$160, _tmp$1209
              );
              _if_result$2655 = _tmp$1208 != needle_first$163;
            } else {
              _if_result$2655 = 0;
            }
            if (_if_result$2655) {
              int32_t _tmp$1211 = i$164;
              i$164 = _tmp$1211 - 1;
              continue;
            }
            break;
          }
          _tmp$1212 = i$164;
          if (_tmp$1212 >= 0) {
            int32_t j$166 = 1;
            int32_t _tmp$1219;
            while (1) {
              if (j$166 < needle_len$161) {
                int32_t _tmp$1216 = i$164;
                int32_t _tmp$1215 = _tmp$1216 + j$166;
                int32_t _tmp$1213;
                int32_t _tmp$1214;
                int32_t _tmp$1217;
                moonbit_incref(haystack$160.$0);
                _tmp$1213
                = $StringView$$unsafe_charcode_at(
                  haystack$160, _tmp$1215
                );
                moonbit_incref(needle$162.$0);
                _tmp$1214
                = $StringView$$unsafe_charcode_at(
                  needle$162, j$166
                );
                if (_tmp$1213 != _tmp$1214) {
                  break;
                }
                _tmp$1217 = j$166 + 1;
                j$166 = _tmp$1217;
                continue;
              } else {
                int32_t _tmp$1218;
                moonbit_decref(needle$162.$0);
                moonbit_decref(haystack$160.$0);
                _tmp$1218 = i$164;
                return (int64_t)_tmp$1218;
              }
              break;
            }
            _tmp$1219 = i$164;
            i$164 = _tmp$1219 - 1;
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
      int32_t _tmp$1197 = needle_len$150 - 1;
      int32_t i$153 = _tmp$1197;
      int32_t _tmp$1206;
      int32_t i$155;
      while (1) {
        if (i$153 > 0) {
          int32_t _tmp$1195;
          int32_t _tmp$1194;
          int32_t _tmp$1196;
          moonbit_incref(needle$151.$0);
          _tmp$1195 = $StringView$$unsafe_charcode_at(needle$151, i$153);
          _tmp$1194 = _tmp$1195 & 255;
          if (
            _tmp$1194 < 0
            || _tmp$1194 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          skip_table$152[_tmp$1194] = i$153;
          _tmp$1196 = i$153 - 1;
          i$153 = _tmp$1196;
          continue;
        }
        break;
      }
      _tmp$1206 = haystack_len$148 - needle_len$150;
      i$155 = _tmp$1206;
      while (1) {
        if (i$155 >= 0) {
          int32_t j$156 = 0;
          int32_t _tmp$1205;
          int32_t _tmp$1204;
          int32_t _tmp$1203;
          int32_t _tmp$1202;
          while (1) {
            if (j$156 < needle_len$150) {
              int32_t _tmp$1200 = i$155 + j$156;
              int32_t _tmp$1198;
              int32_t _tmp$1199;
              int32_t _tmp$1201;
              moonbit_incref(haystack$149.$0);
              _tmp$1198
              = $StringView$$unsafe_charcode_at(
                haystack$149, _tmp$1200
              );
              moonbit_incref(needle$151.$0);
              _tmp$1199 = $StringView$$unsafe_charcode_at(needle$151, j$156);
              if (_tmp$1198 != _tmp$1199) {
                break;
              }
              _tmp$1201 = j$156 + 1;
              j$156 = _tmp$1201;
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
          _tmp$1205 = $StringView$$unsafe_charcode_at(haystack$149, i$155);
          _tmp$1204 = _tmp$1205 & 255;
          if (
            _tmp$1204 < 0
            || _tmp$1204 >= Moonbit_array_length(skip_table$152)
          ) {
            moonbit_panic();
          }
          _tmp$1203 = (int32_t)skip_table$152[_tmp$1204];
          _tmp$1202 = i$155 - _tmp$1203;
          i$155 = _tmp$1202;
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
  int32_t _tmp$1193;
  moonbit_incref(str$146.$0);
  _tmp$1193 = $StringView$$length(str$146);
  if (_tmp$1193 <= 4) {
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
        int32_t _tmp$1180 = i$141;
        if (_tmp$1180 <= forward_len$140) {
          int32_t _tmp$1185;
          while (1) {
            int32_t _tmp$1183 = i$141;
            int32_t _if_result$2662;
            if (_tmp$1183 <= forward_len$140) {
              int32_t _tmp$1182 = i$141;
              int32_t _tmp$1181;
              moonbit_incref(haystack$136.$0);
              _tmp$1181
              = $StringView$$unsafe_charcode_at(
                haystack$136, _tmp$1182
              );
              _if_result$2662 = _tmp$1181 != needle_first$139;
            } else {
              _if_result$2662 = 0;
            }
            if (_if_result$2662) {
              int32_t _tmp$1184 = i$141;
              i$141 = _tmp$1184 + 1;
              continue;
            }
            break;
          }
          _tmp$1185 = i$141;
          if (_tmp$1185 <= forward_len$140) {
            int32_t j$143 = 1;
            int32_t _tmp$1192;
            while (1) {
              if (j$143 < needle_len$137) {
                int32_t _tmp$1189 = i$141;
                int32_t _tmp$1188 = _tmp$1189 + j$143;
                int32_t _tmp$1186;
                int32_t _tmp$1187;
                int32_t _tmp$1190;
                moonbit_incref(haystack$136.$0);
                _tmp$1186
                = $StringView$$unsafe_charcode_at(
                  haystack$136, _tmp$1188
                );
                moonbit_incref(needle$138.$0);
                _tmp$1187
                = $StringView$$unsafe_charcode_at(
                  needle$138, j$143
                );
                if (_tmp$1186 != _tmp$1187) {
                  break;
                }
                _tmp$1190 = j$143 + 1;
                j$143 = _tmp$1190;
                continue;
              } else {
                int32_t _tmp$1191;
                moonbit_decref(needle$138.$0);
                moonbit_decref(haystack$136.$0);
                _tmp$1191 = i$141;
                return (int64_t)_tmp$1191;
              }
              break;
            }
            _tmp$1192 = i$141;
            i$141 = _tmp$1192 + 1;
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
          int32_t _tmp$1167;
          int32_t _tmp$1164;
          int32_t _tmp$1166;
          int32_t _tmp$1165;
          int32_t _tmp$1168;
          moonbit_incref(needle$124.$0);
          _tmp$1167 = $StringView$$unsafe_charcode_at(needle$124, i$127);
          _tmp$1164 = _tmp$1167 & 255;
          _tmp$1166 = needle_len$123 - 1;
          _tmp$1165 = _tmp$1166 - i$127;
          if (
            _tmp$1164 < 0
            || _tmp$1164 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          skip_table$125[_tmp$1164] = _tmp$1165;
          _tmp$1168 = i$127 + 1;
          i$127 = _tmp$1168;
          continue;
        }
        break;
      }
      i$129 = 0;
      while (1) {
        int32_t _tmp$1169 = haystack_len$121 - needle_len$123;
        if (i$129 <= _tmp$1169) {
          int32_t _end4307$130 = needle_len$123 - 1;
          int32_t j$131 = 0;
          int32_t _tmp$1179;
          int32_t _tmp$1178;
          int32_t _tmp$1177;
          int32_t _tmp$1176;
          int32_t _tmp$1175;
          int32_t _tmp$1174;
          while (1) {
            if (j$131 <= _end4307$130) {
              int32_t _tmp$1172 = i$129 + j$131;
              int32_t _tmp$1170;
              int32_t _tmp$1171;
              int32_t _tmp$1173;
              moonbit_incref(haystack$122.$0);
              _tmp$1170
              = $StringView$$unsafe_charcode_at(
                haystack$122, _tmp$1172
              );
              moonbit_incref(needle$124.$0);
              _tmp$1171 = $StringView$$unsafe_charcode_at(needle$124, j$131);
              if (_tmp$1170 != _tmp$1171) {
                break;
              }
              _tmp$1173 = j$131 + 1;
              j$131 = _tmp$1173;
              continue;
            } else {
              moonbit_decref(skip_table$125);
              moonbit_decref(needle$124.$0);
              moonbit_decref(haystack$122.$0);
              return (int64_t)i$129;
            }
            break;
          }
          _tmp$1179 = i$129 + needle_len$123;
          _tmp$1178 = _tmp$1179 - 1;
          moonbit_incref(haystack$122.$0);
          _tmp$1177
          = $StringView$$unsafe_charcode_at(
            haystack$122, _tmp$1178
          );
          _tmp$1176 = _tmp$1177 & 255;
          if (
            _tmp$1176 < 0
            || _tmp$1176 >= Moonbit_array_length(skip_table$125)
          ) {
            moonbit_panic();
          }
          _tmp$1175 = (int32_t)skip_table$125[_tmp$1176];
          _tmp$1174 = i$129 + _tmp$1175;
          i$129 = _tmp$1174;
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
  moonbit_string_t _field$2382 = self$118.$0;
  moonbit_string_t str$1161 = _field$2382;
  int32_t _field$2381 = self$118.$1;
  int32_t start$1163 = _field$2381;
  int32_t _tmp$1162 = start$1163 + index$119;
  int32_t _tmp$2380 = str$1161[_tmp$1162];
  moonbit_decref(str$1161);
  return _tmp$2380;
}

int32_t $StringView$$length(struct $StringView self$117) {
  int32_t end$1159 = self$117.$2;
  int32_t _field$2383 = self$117.$1;
  int32_t start$1160;
  moonbit_decref(self$117.$0);
  start$1160 = _field$2383;
  return end$1159 - start$1160;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$115,
  int32_t index$116
) {
  int32_t len$114 = self$115->$1;
  int32_t _if_result$2667;
  if (index$116 >= 0) {
    _if_result$2667 = index$116 < len$114;
  } else {
    _if_result$2667 = 0;
  }
  if (_if_result$2667) {
    moonbit_string_t* _tmp$1158 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$115);
    moonbit_string_t _tmp$2384;
    if (index$116 < 0 || index$116 >= Moonbit_array_length(_tmp$1158)) {
      moonbit_panic();
    }
    _tmp$2384 = (moonbit_string_t)_tmp$1158[index$116];
    moonbit_incref(_tmp$2384);
    moonbit_decref(_tmp$1158);
    return _tmp$2384;
  } else {
    moonbit_decref(self$115);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$113
) {
  int32_t _field$2385 = self$113->$1;
  moonbit_decref(self$113);
  return _field$2385;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$112
) {
  int32_t _field$2386 = self$112->$1;
  moonbit_decref(self$112);
  return _field$2386;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$111
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$2387 =
    self$111->$0;
  int32_t _cnt$2525 = Moonbit_object_header(self$111)->rc;
  if (_cnt$2525 > 1) {
    int32_t _new_cnt$2526;
    moonbit_incref(_field$2387);
    _new_cnt$2526 = _cnt$2525 - 1;
    Moonbit_object_header(self$111)->rc = _new_cnt$2526;
  } else if (_cnt$2525 == 1) {
    moonbit_free(self$111);
  }
  return _field$2387;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$110
) {
  moonbit_string_t* _field$2388 = self$110->$0;
  int32_t _cnt$2527 = Moonbit_object_header(self$110)->rc;
  if (_cnt$2527 > 1) {
    int32_t _new_cnt$2528;
    moonbit_incref(_field$2388);
    _new_cnt$2528 = _cnt$2527 - 1;
    Moonbit_object_header(self$110)->rc = _new_cnt$2528;
  } else if (_cnt$2527 == 1) {
    moonbit_free(self$110);
  }
  return _field$2388;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$109
) {
  struct $$3c$String$2a$Int$3e$** _field$2389 = self$109->$0;
  int32_t _cnt$2529 = Moonbit_object_header(self$109)->rc;
  if (_cnt$2529 > 1) {
    int32_t _new_cnt$2530;
    moonbit_incref(_field$2389);
    _new_cnt$2530 = _cnt$2529 - 1;
    Moonbit_object_header(self$109)->rc = _new_cnt$2530;
  } else if (_cnt$2529 == 1) {
    moonbit_free(self$109);
  }
  return _field$2389;
}

moonbit_string_t $String$$escape(moonbit_string_t self$108) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$107 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1157;
  moonbit_incref(buf$107);
  _tmp$1157
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$107
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$108, _tmp$1157);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$107);
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$105, int32_t y$106) {
  int32_t _tmp$1156 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$105, y$106
    );
  return !_tmp$1156;
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
  int32_t len$1151 = self$100->$1;
  int32_t _tmp$1150 = len$1151 + 4;
  moonbit_bytes_t _field$2390;
  moonbit_bytes_t data$1154;
  int32_t len$1155;
  int32_t inc$101;
  int32_t len$1153;
  int32_t _tmp$1152;
  moonbit_incref(self$100);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$100, _tmp$1150
  );
  _field$2390 = self$100->$0;
  data$1154 = _field$2390;
  len$1155 = self$100->$1;
  moonbit_incref(data$1154);
  inc$101 = $FixedArray$$set_utf16le_char(data$1154, len$1155, ch$102);
  len$1153 = self$100->$1;
  _tmp$1152 = len$1153 + inc$101;
  self$100->$1 = _tmp$1152;
  moonbit_decref(self$100);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$95,
  int32_t required$96
) {
  moonbit_bytes_t _field$2394 = self$95->$0;
  moonbit_bytes_t data$1149 = _field$2394;
  int32_t _tmp$2393 = Moonbit_array_length(data$1149);
  int32_t current_len$94 = _tmp$2393;
  int32_t enough_space$97;
  int32_t _tmp$1147;
  int32_t _tmp$1148;
  moonbit_bytes_t new_data$99;
  moonbit_bytes_t _field$2392;
  moonbit_bytes_t data$1145;
  int32_t len$1146;
  moonbit_bytes_t _old$2391;
  if (required$96 <= current_len$94) {
    moonbit_decref(self$95);
    return 0;
  }
  enough_space$97 = current_len$94;
  while (1) {
    int32_t _tmp$1143 = enough_space$97;
    if (_tmp$1143 < required$96) {
      int32_t _tmp$1144 = enough_space$97;
      enough_space$97 = _tmp$1144 * 2;
      continue;
    }
    break;
  }
  _tmp$1147 = enough_space$97;
  _tmp$1148 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$99 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1147, _tmp$1148);
  _field$2392 = self$95->$0;
  data$1145 = _field$2392;
  len$1146 = self$95->$1;
  moonbit_incref(data$1145);
  moonbit_incref(new_data$99);
  $FixedArray$$unsafe_blit$0(new_data$99, 0, data$1145, 0, len$1146);
  _old$2391 = self$95->$0;
  moonbit_decref(_old$2391);
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
    uint32_t _tmp$1126 = code$87 & 255u;
    int32_t _tmp$1125 = $UInt$$to_byte(_tmp$1126);
    int32_t _tmp$1127;
    uint32_t _tmp$1129;
    int32_t _tmp$1128;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1125;
    _tmp$1127 = offset$90 + 1;
    _tmp$1129 = code$87 >> 8;
    _tmp$1128 = $UInt$$to_byte(_tmp$1129);
    if (_tmp$1127 < 0 || _tmp$1127 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1127] = _tmp$1128;
    moonbit_decref(self$89);
    return 2;
  } else if (code$87 < 1114112u) {
    uint32_t hi$91 = code$87 - 65536u;
    uint32_t _tmp$1142 = hi$91 >> 10;
    uint32_t lo$92 = _tmp$1142 | 55296u;
    uint32_t _tmp$1141 = hi$91 & 1023u;
    uint32_t hi$93 = _tmp$1141 | 56320u;
    uint32_t _tmp$1131 = lo$92 & 255u;
    int32_t _tmp$1130 = $UInt$$to_byte(_tmp$1131);
    int32_t _tmp$1132;
    uint32_t _tmp$1134;
    int32_t _tmp$1133;
    int32_t _tmp$1135;
    uint32_t _tmp$1137;
    int32_t _tmp$1136;
    int32_t _tmp$1138;
    uint32_t _tmp$1140;
    int32_t _tmp$1139;
    if (offset$90 < 0 || offset$90 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[offset$90] = _tmp$1130;
    _tmp$1132 = offset$90 + 1;
    _tmp$1134 = lo$92 >> 8;
    _tmp$1133 = $UInt$$to_byte(_tmp$1134);
    if (_tmp$1132 < 0 || _tmp$1132 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1132] = _tmp$1133;
    _tmp$1135 = offset$90 + 2;
    _tmp$1137 = hi$93 & 255u;
    _tmp$1136 = $UInt$$to_byte(_tmp$1137);
    if (_tmp$1135 < 0 || _tmp$1135 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1135] = _tmp$1136;
    _tmp$1138 = offset$90 + 3;
    _tmp$1140 = hi$93 >> 8;
    _tmp$1139 = $UInt$$to_byte(_tmp$1140);
    if (_tmp$1138 < 0 || _tmp$1138 >= Moonbit_array_length(self$89)) {
      moonbit_panic();
    }
    self$89[_tmp$1138] = _tmp$1139;
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
  int32_t _tmp$1124 = *(int32_t*)&self$86;
  return _tmp$1124 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$85) {
  int32_t _tmp$1123 = self$85;
  return *(uint32_t*)&_tmp$1123;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$84
) {
  moonbit_bytes_t _field$2396 = self$84->$0;
  moonbit_bytes_t data$1122 = _field$2396;
  moonbit_bytes_t _tmp$1119;
  int32_t _field$2395;
  int32_t len$1121;
  int64_t _tmp$1120;
  moonbit_incref(data$1122);
  _tmp$1119 = data$1122;
  _field$2395 = self$84->$1;
  moonbit_decref(self$84);
  len$1121 = _field$2395;
  _tmp$1120 = (int64_t)len$1121;
  return $Bytes$$to_unchecked_string$inner(_tmp$1119, 0, _tmp$1120);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$79,
  int32_t offset$83,
  int64_t length$81
) {
  int32_t len$78 = Moonbit_array_length(self$79);
  int32_t length$80;
  int32_t _if_result$2669;
  if (length$81 == 4294967296ll) {
    length$80 = len$78 - offset$83;
  } else {
    int64_t _Some$82 = length$81;
    length$80 = (int32_t)_Some$82;
  }
  if (offset$83 >= 0) {
    if (length$80 >= 0) {
      int32_t _tmp$1118 = offset$83 + length$80;
      _if_result$2669 = _tmp$1118 <= len$78;
    } else {
      _if_result$2669 = 0;
    }
  } else {
    _if_result$2669 = 0;
  }
  if (_if_result$2669) {
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
  struct $$moonbitlang$core$builtin$StringBuilder* _block$2670;
  if (size_hint$76 < 1) {
    initial$75 = 1;
  } else {
    initial$75 = size_hint$76;
  }
  data$77 = (moonbit_bytes_t)moonbit_make_bytes(initial$75, 0);
  _block$2670
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$2670)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$2670->$0 = data$77;
  _block$2670->$1 = 0;
  return _block$2670;
}

int32_t $Byte$$to_char(int32_t self$74) {
  int32_t _tmp$1117 = (int32_t)self$74;
  return _tmp$1117;
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
  int32_t _if_result$2671;
  if (dst$50 == src$51) {
    _if_result$2671 = dst_offset$52 < src_offset$53;
  } else {
    _if_result$2671 = 0;
  }
  if (_if_result$2671) {
    int32_t i$54 = 0;
    while (1) {
      if (i$54 < len$55) {
        int32_t _tmp$1108 = dst_offset$52 + i$54;
        int32_t _tmp$1110 = src_offset$53 + i$54;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2398;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1109;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2397;
        int32_t _tmp$1111;
        if (_tmp$1110 < 0 || _tmp$1110 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2398
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1110
          ];
        _tmp$1109 = _tmp$2398;
        if (_tmp$1108 < 0 || _tmp$1108 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2397
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1108
          ];
        if (_tmp$1109) {
          moonbit_incref(_tmp$1109);
        }
        if (_old$2397) {
          moonbit_decref(_old$2397);
        }
        dst$50[_tmp$1108] = _tmp$1109;
        _tmp$1111 = i$54 + 1;
        i$54 = _tmp$1111;
        continue;
      } else {
        moonbit_decref(src$51);
        moonbit_decref(dst$50);
      }
      break;
    }
  } else {
    int32_t _tmp$1116 = len$55 - 1;
    int32_t i$57 = _tmp$1116;
    while (1) {
      if (i$57 >= 0) {
        int32_t _tmp$1112 = dst_offset$52 + i$57;
        int32_t _tmp$1114 = src_offset$53 + i$57;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$2400;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1113;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$2399;
        int32_t _tmp$1115;
        if (_tmp$1114 < 0 || _tmp$1114 >= Moonbit_array_length(src$51)) {
          moonbit_panic();
        }
        _tmp$2400
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$51[
            _tmp$1114
          ];
        _tmp$1113 = _tmp$2400;
        if (_tmp$1112 < 0 || _tmp$1112 >= Moonbit_array_length(dst$50)) {
          moonbit_panic();
        }
        _old$2399
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$50[
            _tmp$1112
          ];
        if (_tmp$1113) {
          moonbit_incref(_tmp$1113);
        }
        if (_old$2399) {
          moonbit_decref(_old$2399);
        }
        dst$50[_tmp$1112] = _tmp$1113;
        _tmp$1115 = i$57 - 1;
        i$57 = _tmp$1115;
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
  int32_t _if_result$2674;
  if (dst$41 == src$42) {
    _if_result$2674 = dst_offset$43 < src_offset$44;
  } else {
    _if_result$2674 = 0;
  }
  if (_if_result$2674) {
    int32_t i$45 = 0;
    while (1) {
      if (i$45 < len$46) {
        int32_t _tmp$1099 = dst_offset$43 + i$45;
        int32_t _tmp$1101 = src_offset$44 + i$45;
        struct $$3c$String$2a$Int$3e$* _tmp$2402;
        struct $$3c$String$2a$Int$3e$* _tmp$1100;
        struct $$3c$String$2a$Int$3e$* _old$2401;
        int32_t _tmp$1102;
        if (_tmp$1101 < 0 || _tmp$1101 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2402 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1101];
        _tmp$1100 = _tmp$2402;
        if (_tmp$1099 < 0 || _tmp$1099 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2401 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1099];
        if (_tmp$1100) {
          moonbit_incref(_tmp$1100);
        }
        if (_old$2401) {
          moonbit_decref(_old$2401);
        }
        dst$41[_tmp$1099] = _tmp$1100;
        _tmp$1102 = i$45 + 1;
        i$45 = _tmp$1102;
        continue;
      } else {
        moonbit_decref(src$42);
        moonbit_decref(dst$41);
      }
      break;
    }
  } else {
    int32_t _tmp$1107 = len$46 - 1;
    int32_t i$48 = _tmp$1107;
    while (1) {
      if (i$48 >= 0) {
        int32_t _tmp$1103 = dst_offset$43 + i$48;
        int32_t _tmp$1105 = src_offset$44 + i$48;
        struct $$3c$String$2a$Int$3e$* _tmp$2404;
        struct $$3c$String$2a$Int$3e$* _tmp$1104;
        struct $$3c$String$2a$Int$3e$* _old$2403;
        int32_t _tmp$1106;
        if (_tmp$1105 < 0 || _tmp$1105 >= Moonbit_array_length(src$42)) {
          moonbit_panic();
        }
        _tmp$2404 = (struct $$3c$String$2a$Int$3e$*)src$42[_tmp$1105];
        _tmp$1104 = _tmp$2404;
        if (_tmp$1103 < 0 || _tmp$1103 >= Moonbit_array_length(dst$41)) {
          moonbit_panic();
        }
        _old$2403 = (struct $$3c$String$2a$Int$3e$*)dst$41[_tmp$1103];
        if (_tmp$1104) {
          moonbit_incref(_tmp$1104);
        }
        if (_old$2403) {
          moonbit_decref(_old$2403);
        }
        dst$41[_tmp$1103] = _tmp$1104;
        _tmp$1106 = i$48 - 1;
        i$48 = _tmp$1106;
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
  int32_t _if_result$2677;
  if (dst$32 == src$33) {
    _if_result$2677 = dst_offset$34 < src_offset$35;
  } else {
    _if_result$2677 = 0;
  }
  if (_if_result$2677) {
    int32_t i$36 = 0;
    while (1) {
      if (i$36 < len$37) {
        int32_t _tmp$1090 = dst_offset$34 + i$36;
        int32_t _tmp$1092 = src_offset$35 + i$36;
        moonbit_string_t _tmp$2406;
        moonbit_string_t _tmp$1091;
        moonbit_string_t _old$2405;
        int32_t _tmp$1093;
        if (_tmp$1092 < 0 || _tmp$1092 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2406 = (moonbit_string_t)src$33[_tmp$1092];
        _tmp$1091 = _tmp$2406;
        if (_tmp$1090 < 0 || _tmp$1090 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2405 = (moonbit_string_t)dst$32[_tmp$1090];
        moonbit_incref(_tmp$1091);
        moonbit_decref(_old$2405);
        dst$32[_tmp$1090] = _tmp$1091;
        _tmp$1093 = i$36 + 1;
        i$36 = _tmp$1093;
        continue;
      } else {
        moonbit_decref(src$33);
        moonbit_decref(dst$32);
      }
      break;
    }
  } else {
    int32_t _tmp$1098 = len$37 - 1;
    int32_t i$39 = _tmp$1098;
    while (1) {
      if (i$39 >= 0) {
        int32_t _tmp$1094 = dst_offset$34 + i$39;
        int32_t _tmp$1096 = src_offset$35 + i$39;
        moonbit_string_t _tmp$2408;
        moonbit_string_t _tmp$1095;
        moonbit_string_t _old$2407;
        int32_t _tmp$1097;
        if (_tmp$1096 < 0 || _tmp$1096 >= Moonbit_array_length(src$33)) {
          moonbit_panic();
        }
        _tmp$2408 = (moonbit_string_t)src$33[_tmp$1096];
        _tmp$1095 = _tmp$2408;
        if (_tmp$1094 < 0 || _tmp$1094 >= Moonbit_array_length(dst$32)) {
          moonbit_panic();
        }
        _old$2407 = (moonbit_string_t)dst$32[_tmp$1094];
        moonbit_incref(_tmp$1095);
        moonbit_decref(_old$2407);
        dst$32[_tmp$1094] = _tmp$1095;
        _tmp$1097 = i$39 - 1;
        i$39 = _tmp$1097;
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
  int32_t _if_result$2680;
  if (dst$23 == src$24) {
    _if_result$2680 = dst_offset$25 < src_offset$26;
  } else {
    _if_result$2680 = 0;
  }
  if (_if_result$2680) {
    int32_t i$27 = 0;
    while (1) {
      if (i$27 < len$28) {
        int32_t _tmp$1081 = dst_offset$25 + i$27;
        int32_t _tmp$1083 = src_offset$26 + i$27;
        int32_t _tmp$1082;
        int32_t _tmp$1084;
        if (_tmp$1083 < 0 || _tmp$1083 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1082 = (int32_t)src$24[_tmp$1083];
        if (_tmp$1081 < 0 || _tmp$1081 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1081] = _tmp$1082;
        _tmp$1084 = i$27 + 1;
        i$27 = _tmp$1084;
        continue;
      } else {
        moonbit_decref(src$24);
        moonbit_decref(dst$23);
      }
      break;
    }
  } else {
    int32_t _tmp$1089 = len$28 - 1;
    int32_t i$30 = _tmp$1089;
    while (1) {
      if (i$30 >= 0) {
        int32_t _tmp$1085 = dst_offset$25 + i$30;
        int32_t _tmp$1087 = src_offset$26 + i$30;
        int32_t _tmp$1086;
        int32_t _tmp$1088;
        if (_tmp$1087 < 0 || _tmp$1087 >= Moonbit_array_length(src$24)) {
          moonbit_panic();
        }
        _tmp$1086 = (int32_t)src$24[_tmp$1087];
        if (_tmp$1085 < 0 || _tmp$1085 >= Moonbit_array_length(dst$23)) {
          moonbit_panic();
        }
        dst$23[_tmp$1085] = _tmp$1086;
        _tmp$1088 = i$30 - 1;
        i$30 = _tmp$1088;
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
  moonbit_string_t _tmp$1080 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$1078 =
    moonbit_add_string(
      _tmp$1080, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1079 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$1077 = moonbit_add_string(_tmp$1078, _tmp$1079);
  moonbit_string_t _tmp$1076 =
    moonbit_add_string(
      _tmp$1077, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1076);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$1075 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$1073 =
    moonbit_add_string(
      _tmp$1075, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1074 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$1072 = moonbit_add_string(_tmp$1073, _tmp$1074);
  moonbit_string_t _tmp$1071 =
    moonbit_add_string(
      _tmp$1072, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1071);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$17,
  moonbit_string_t loc$18
) {
  moonbit_string_t _tmp$1070 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$17);
  moonbit_string_t _tmp$1068 =
    moonbit_add_string(
      _tmp$1070, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1069 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$18);
  moonbit_string_t _tmp$1067 = moonbit_add_string(_tmp$1068, _tmp$1069);
  moonbit_string_t _tmp$1066 =
    moonbit_add_string(
      _tmp$1067, (moonbit_string_t)moonbit_string_literal_32.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1066);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$15,
  moonbit_string_t loc$16
) {
  moonbit_string_t _tmp$1065 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$15);
  moonbit_string_t _tmp$1063 =
    moonbit_add_string(
      _tmp$1065, (moonbit_string_t)moonbit_string_literal_31.data
    );
  moonbit_string_t _tmp$1064 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$16);
  moonbit_string_t _tmp$1062 = moonbit_add_string(_tmp$1063, _tmp$1064);
  moonbit_string_t _tmp$1061 =
    moonbit_add_string(
      _tmp$1062, (moonbit_string_t)moonbit_string_literal_32.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1061);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$11,
  struct $$moonbitlang$core$builtin$Logger _x_5272$14
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$12 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$11;
  moonbit_string_t _field$2409 = _Failure$12->$0;
  int32_t _cnt$2531 = Moonbit_object_header(_Failure$12)->rc;
  moonbit_string_t _$2a$err_payload_5273$13;
  struct $$moonbitlang$core$builtin$Logger _bind$1060;
  if (_cnt$2531 > 1) {
    int32_t _new_cnt$2532;
    moonbit_incref(_field$2409);
    _new_cnt$2532 = _cnt$2531 - 1;
    Moonbit_object_header(_Failure$12)->rc = _new_cnt$2532;
  } else if (_cnt$2531 == 1) {
    moonbit_free(_Failure$12);
  }
  _$2a$err_payload_5273$13 = _field$2409;
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
  _bind$1060 = _x_5272$14;
  _bind$1060.$0->$method_0(
    _bind$1060.$1, (moonbit_string_t)moonbit_string_literal_34.data
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

moonbit_string_t $Error$to_string(void* _e$1015) {
  switch (Moonbit_object_tag(_e$1015)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1015
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
               _e$1015
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1015);
      return (moonbit_string_t)moonbit_string_literal_37.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
               _e$1015
             );
      break;
    }
    
    case 3: {
      moonbit_decref(_e$1015);
      return (moonbit_string_t)moonbit_string_literal_38.data;
      break;
    }
    default: {
      moonbit_decref(_e$1015);
      return (moonbit_string_t)moonbit_string_literal_39.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1033,
  int32_t _param$1032
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1031 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1033;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1031, _param$1032
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1030,
  struct $StringView _param$1029
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1028 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1030;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1028, _param$1029
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1027,
  moonbit_string_t _param$1024,
  int32_t _param$1025,
  int32_t _param$1026
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1023 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1027;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1023, _param$1024, _param$1025, _param$1026
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1022,
  moonbit_string_t _param$1021
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1020 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1022;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1020, _param$1021
  );
  return 0;
}

void moonbit_init() {
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$841;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1039;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1038;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$836;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1057;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1056;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1055;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1042;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$837;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1054;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1053;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1052;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1043;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$838;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1051;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1050;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1049;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1044;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$839;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1048;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1047;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1046;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1045;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$835;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1041;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1040;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$840;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1059;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1058;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$120 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$134 = (int64_t)0;
  _bind$841
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1039 = _bind$841;
  _tmp$1038
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1039
  };
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1038
  );
  _bind$836
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1057 = _bind$836;
  _tmp$1056
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1057
  };
  _tmp$1055 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1056);
  _tuple$1042
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1042)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1042->$0 = (moonbit_string_t)moonbit_string_literal_40.data;
  _tuple$1042->$1 = _tmp$1055;
  _bind$837
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1054 = _bind$837;
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
  _tuple$1043->$0 = (moonbit_string_t)moonbit_string_literal_41.data;
  _tuple$1043->$1 = _tmp$1052;
  _bind$838
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1051 = _bind$838;
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
  _tuple$1044->$0 = (moonbit_string_t)moonbit_string_literal_42.data;
  _tuple$1044->$1 = _tmp$1049;
  _bind$839
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1048 = _bind$839;
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
  _tuple$1045->$0 = (moonbit_string_t)moonbit_string_literal_43.data;
  _tuple$1045->$1 = _tmp$1046;
  _bind$835
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      4
    );
  _bind$835[0] = _tuple$1042;
  _bind$835[1] = _tuple$1043;
  _bind$835[2] = _tuple$1044;
  _bind$835[3] = _tuple$1045;
  _tmp$1041 = _bind$835;
  _tmp$1040
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 4, _tmp$1041
  };
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1040
  );
  _bind$840
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1059 = _bind$840;
  _tmp$1058
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1059
  };
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1058
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1037;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1009;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1010;
  int32_t _len$1011;
  int32_t _i$1012;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1037
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1009
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1009)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1009->$0 = _tmp$1037;
  async_tests$1009->$1 = 0;
  _arr$1010
  = $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1010);
  _len$1011 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1010);
  _i$1012 = 0;
  while (1) {
    if (_i$1012 < _len$1011) {
      struct $$3c$String$2a$Int$3e$* arg$1013;
      moonbit_string_t _field$2411;
      moonbit_string_t _tmp$1034;
      int32_t _field$2410;
      int32_t _cnt$2533;
      int32_t _tmp$1035;
      int32_t _tmp$1036;
      moonbit_incref(_arr$1010);
      arg$1013
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1010, _i$1012
      );
      _field$2411 = arg$1013->$0;
      _tmp$1034 = _field$2411;
      _field$2410 = arg$1013->$1;
      _cnt$2533 = Moonbit_object_header(arg$1013)->rc;
      if (_cnt$2533 > 1) {
        int32_t _new_cnt$2534;
        moonbit_incref(_tmp$1034);
        _new_cnt$2534 = _cnt$2533 - 1;
        Moonbit_object_header(arg$1013)->rc = _new_cnt$2534;
      } else if (_cnt$2533 == 1) {
        moonbit_free(arg$1013);
      }
      _tmp$1035 = _field$2410;
      moonbit_incref(async_tests$1009);
      $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_do_execute(
        async_tests$1009, _tmp$1034, _tmp$1035
      );
      _tmp$1036 = _i$1012 + 1;
      _i$1012 = _tmp$1036;
      continue;
    } else {
      moonbit_decref(_arr$1010);
    }
    break;
  }
  $azimuth$telemetry$tests$azimuth$telemetry$enhanced_blackbox_test$moonbit_test_driver_internal_run_async_tests(
    async_tests$1009
  );
  return 0;
}