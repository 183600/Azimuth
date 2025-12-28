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
struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1;

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Err;

struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$3c$String$2a$Int$3e$;

struct $$moonbitlang$core$builtin$Array$3c$String$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap;

struct $$3c$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$;

struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap;

struct $Ref$3c$Int$3e$;

struct $StringView;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$moonbitlang$core$double$internal$ryu$Umul128;

struct $Moonbit_Test_Driver_Internal_Meta;

struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$;

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $Result$3c$Unit$2a$Error$3e$$Ok;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap;

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

struct $Ref$3c$Bool$3e$;

struct $Moonbit_Test_Driver_Internal__TestCase;

struct $$3c$$3e$$3d$$3e$Unit;

struct $$3c$Int$2a$Int$3e$;

struct $$3c$String$3e$$3d$$3e$Int;

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError;

struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$;

struct $Error$moonbitlang$core$builtin$Failure$Failure;

struct $Option$3c$Option$3c$Int$3e$$3e$$Some;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$;

struct $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger;

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap;

struct $$moonbitlang$core$builtin$Array$3c$Double$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Iter$$any$7c$String$7c$$fn$3$2d$cap;

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

struct $$moonbitlang$core$builtin$Hasher;

struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$;

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError;

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int;

struct $Moonbit_Test_Driver_Internal__F$F0;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap;

struct $Result$3c$Unit$2a$Error$3e$$Err;

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$;

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$;

struct $Moonbit_Test_Driver_Internal__F$F1;

struct $Result$3c$StringView$2a$$moonbitlang$core$builtin$CreatingViewError$3e$$Ok;

struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit;

struct $$moonbitlang$core$builtin$SourceLocRepr;

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair;

struct $Option$3c$StringView$3e$$Some;

struct $Result$3c$Unit$2a$$moonbitlang$core$builtin$Failure$3e$$Ok;

struct $$moonbitlang$core$builtin$Logger$static_method_table;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap;

struct $$moonbitlang$core$builtin$StringBuilder;

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap;

struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$;

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1 {
  int32_t $0;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $4;
  
};

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

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap {
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312 {
  void* $0;
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap {
  int32_t(* code)(
    struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*,
    moonbit_string_t,
    moonbit_string_t,
    moonbit_string_t,
    int32_t
  );
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

struct $Ref$3c$Bool$3e$ {
  int32_t $0;
  
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

struct $Error$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError {
  moonbit_string_t $0;
  
};

struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$ {
  int32_t $1;
  int32_t* $0;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap {
  int32_t(* code)(struct $$3c$$3e$$3d$$3e$Unit*);
  moonbit_string_t $0;
  moonbit_string_t $1;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $2;
  
};

struct $$moonbitlang$core$builtin$Array$3c$Double$3e$ {
  int32_t $1;
  double* $0;
  
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

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap {
  struct moonbit_result_0(* code)(
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*
  );
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $0;
  moonbit_string_t $1;
  
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

struct $$moonbitlang$core$builtin$Logger$static_method_table {
  int32_t(* $method_0)(void*, moonbit_string_t);
  int32_t(* $method_1)(void*, moonbit_string_t, int32_t, int32_t);
  int32_t(* $method_2)(void*, struct $StringView);
  int32_t(* $method_3)(void*, int32_t);
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap {
  int32_t(* code)(struct $$3c$Unit$3e$$3d$$3e$Unit*, int32_t);
  int32_t $0;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $1;
  moonbit_string_t $2;
  moonbit_string_t $3;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* $4;
  
};

struct $$moonbitlang$core$builtin$StringBuilder {
  int32_t $1;
  moonbit_bytes_t $0;
  
};

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result {
  uint64_t $0;
  uint64_t $1;
  uint64_t $2;
  
};

struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap {
  int32_t(* code)(struct $$3c$Error$3e$$3d$$3e$Unit*, void*);
  int32_t $0;
  struct $$3c$Error$3e$$3d$$3e$Unit* $1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* $2;
  struct $$3c$Error$3e$$3d$$3e$Unit* $3;
  
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

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4100
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4099
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4098
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4097
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4096
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4095
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4094
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4093
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4092
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4091
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4090
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4089
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4088
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4087
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1636
);

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$4061,
  moonbit_string_t s$1614,
  int32_t sep$1615
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1601
);

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$3970,
  moonbit_bytes_t bytes$1602
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$3963,
  moonbit_string_t s$1596
);

#define $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi moonbit_get_cli_args

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1559,
  moonbit_string_t filename$1520,
  int32_t index$1521
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3956
);

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3952
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3950
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$3934,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1560,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1561
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$3945,
  int32_t _cont_param$1580
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3942,
  void* _cont_param$1581
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$3936,
  void* _state$1563
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3931,
  void* err$1543
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$3917,
  moonbit_string_t attr$1536
);

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$3901,
  moonbit_string_t test_name$1523,
  moonbit_string_t file_name$1524,
  moonbit_string_t message$1525,
  int32_t skipped$1526
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1518
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
);

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1516,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1517,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1514
);

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1477,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1490,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1503,
  moonbit_string_t file_filter$1474,
  int32_t index_filter$1475
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1(
  
);

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0(
  
);

int32_t $$moonbitlang$core$builtin$Show$$Double$$output(
  double self$1323,
  struct $$moonbitlang$core$builtin$Logger logger$1322
);

moonbit_string_t $Double$$to_string(double self$1321);

moonbit_string_t $moonbitlang$core$double$internal$ryu$ryu_to_string(
  double val$1308
);

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d_small_int(
  uint64_t ieeeMantissa$1302,
  int32_t ieeeExponent$1304
);

moonbit_string_t $moonbitlang$core$double$internal$ryu$to_chars(
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* v$1275,
  int32_t sign$1273
);

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d(
  uint64_t ieeeMantissa$1217,
  uint32_t ieeeExponent$1216
);

int32_t $moonbitlang$core$double$internal$ryu$decimal_length17(
  uint64_t v$1213
);

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computeInvPow5(
  int32_t i$1196
);

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computePow5(
  int32_t i$1178
);

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result $moonbitlang$core$double$internal$ryu$mulShiftAll64(
  uint64_t m$1151,
  struct $$moonbitlang$core$double$internal$ryu$Pow5Pair mul$1148,
  int32_t j$1164,
  int32_t mmShift$1166
);

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf2(
  uint64_t value$1145,
  int32_t p$1146
);

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
  uint64_t value$1143,
  int32_t p$1144
);

int32_t $moonbitlang$core$double$internal$ryu$pow5Factor(uint64_t value$1139);

uint64_t $moonbitlang$core$double$internal$ryu$shiftright128(
  uint64_t lo$1138,
  uint64_t hi$1136,
  int32_t dist$1137
);

struct $$moonbitlang$core$double$internal$ryu$Umul128 $moonbitlang$core$double$internal$ryu$umul128(
  uint64_t a$1126,
  uint64_t b$1129
);

moonbit_string_t $moonbitlang$core$double$internal$ryu$string_from_bytes(
  moonbit_bytes_t bytes$1120,
  int32_t from$1124,
  int32_t to$1122
);

int32_t $moonbitlang$core$double$internal$ryu$log10Pow2(int32_t e$1118);

int32_t $moonbitlang$core$double$internal$ryu$log10Pow5(int32_t e$1117);

moonbit_string_t $moonbitlang$core$double$internal$ryu$copy_special_str(
  int32_t sign$1115,
  int32_t exponent$1116,
  int32_t mantissa$1113
);

int32_t $moonbitlang$core$double$internal$ryu$pow5bits(int32_t e$1112);

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1111
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$1109,
  struct $$moonbitlang$core$builtin$Logger logger$1110
);

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$1095,
  struct $$moonbitlang$core$builtin$Logger logger$1108
);

uint64_t $Bool$$to_uint64(int32_t self$1093);

int64_t $Bool$$to_int64(int32_t self$1092);

int32_t $Bool$$to_int(int32_t self$1091);

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$1090);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$1085,
  moonbit_string_t msg$1087,
  moonbit_string_t loc$1089
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$1080,
  moonbit_string_t msg$1082,
  moonbit_string_t loc$1084
);

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$1079,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1078
);

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$1077,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1076
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$1074,
  moonbit_string_t value$1072
);

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$1068,
  struct $$3c$String$3e$$3d$$3e$Int* f$1070
);

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2792,
  moonbit_string_t k$1069
);

int32_t $$moonbitlang$core$builtin$Array$$unsafe_get$3(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$1066,
  int32_t idx$1067
);

int32_t $$moonbitlang$core$builtin$Array$$unsafe_get$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$1064,
  int32_t idx$1065
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1062,
  int32_t idx$1063
);

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$1060,
  int32_t idx$1061
);

uint64_t $UInt$$to_uint64(uint32_t self$1059);

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$1055,
  int32_t key$1051
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$1046,
  moonbit_string_t key$1042
);

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$1037,
  int32_t key$1033
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$1028,
  moonbit_string_t key$1024
);

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$1019,
  int32_t key$1015
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$1010,
  moonbit_string_t key$1006
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$998
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$990
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$982
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$974
);

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$970,
  moonbit_string_t key$971,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$972
);

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$967,
  moonbit_string_t key$968,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$969
);

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$964,
  int32_t key$965,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$966
);

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$961,
  moonbit_string_t key$962,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$963
);

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$951
);

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$940
);

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$929
);

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$918
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$901,
  moonbit_string_t key$910,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$911,
  int32_t hash$909
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$885,
  moonbit_string_t key$894,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$895,
  int32_t hash$893
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$869,
  int32_t key$878,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$879,
  int32_t hash$877
);

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$853,
  moonbit_string_t key$862,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$863,
  int32_t hash$861
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$847,
  int32_t idx$852,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$851
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$837,
  int32_t idx$842,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$841
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$827,
  int32_t idx$832,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$831
);

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$817,
  int32_t idx$822,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$821
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$807,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$809,
  int32_t new_idx$808
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$801,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$803,
  int32_t new_idx$802
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$795,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$797,
  int32_t new_idx$796
);

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$789,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$791,
  int32_t new_idx$790
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$786,
  int32_t idx$788,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$787
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$782,
  int32_t idx$784,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$783
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$778,
  int32_t idx$780,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$779
);

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$774,
  int32_t idx$776,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$775
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$768
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$762
);

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$756
);

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$750
);

int32_t $Int$$next_power_of_two(int32_t self$748);

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$747);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$745
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$743
);

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$741
);

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$739
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$2(
  int32_t self$733,
  int32_t other$734
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$1(
  moonbit_string_t self$727,
  moonbit_string_t other$728
);

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  int64_t self$721,
  int64_t other$722
);

uint32_t $ReadOnlyArray$$at$1(uint32_t* self$719, int32_t index$720);

uint64_t $ReadOnlyArray$$at$0(uint64_t* self$717, int32_t index$718);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$716
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$715
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$713
);

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$2441
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$2(
  int32_t self$708,
  struct $$moonbitlang$core$builtin$Logger logger$709
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$1(
  moonbit_string_t self$704,
  struct $$moonbitlang$core$builtin$Logger logger$705
);

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  int64_t self$700,
  struct $$moonbitlang$core$builtin$Logger logger$701
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$699
);

int32_t $$moonbitlang$core$builtin$Show$$UInt16$$output(
  int32_t self$698,
  struct $$moonbitlang$core$builtin$Logger logger$697
);

int32_t $$moonbitlang$core$builtin$Show$$UInt64$$output(
  uint64_t self$696,
  struct $$moonbitlang$core$builtin$Logger logger$695
);

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$694,
  struct $$moonbitlang$core$builtin$Logger logger$693
);

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$691,
  struct $$moonbitlang$core$builtin$Logger logger$692
);

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$689,
  struct $$3c$String$3e$$3d$$3e$Int* f$690
);

int32_t $String$$contains(
  moonbit_string_t self$686,
  struct $StringView str$687
);

int32_t $StringView$$contains(
  struct $StringView self$684,
  struct $StringView str$685
);

int32_t $$moonbitlang$core$builtin$Array$$push$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$680,
  int32_t value$682
);

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$677,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$679
);

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$674,
  struct $$3c$String$2a$Int$3e$* value$676
);

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$671,
  moonbit_string_t value$673
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$669
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$666
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$663
);

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$660
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$656,
  int32_t new_capacity$654
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$650,
  int32_t new_capacity$648
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$644,
  int32_t new_capacity$642
);

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$638,
  int32_t new_capacity$636
);

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$634
);

int32_t $String$$has_prefix(
  moonbit_string_t self$632,
  struct $StringView str$633
);

int32_t $StringView$$has_prefix(
  struct $StringView self$628,
  struct $StringView str$629
);

int32_t $String$$has_suffix(
  moonbit_string_t self$625,
  struct $StringView str$626
);

int32_t $StringView$$has_suffix(
  struct $StringView self$621,
  struct $StringView str$622
);

moonbit_string_t $String$$repeat(moonbit_string_t self$614, int32_t n$613);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$611,
  struct $StringView str$612
);

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$608,
  int32_t i$609,
  int32_t start_offset$610,
  int64_t end_offset$606
);

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$603,
  int32_t n$601,
  int32_t start_offset$597,
  int32_t end_offset$598
);

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$595,
  int32_t n$593,
  int32_t start_offset$592,
  int32_t end_offset$591
);

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$581,
  int32_t len$584,
  int32_t start_offset$588,
  int64_t end_offset$579
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$577
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$576
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$575
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$574
);

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$573
);

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$568
);

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$2348,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$566
);

moonbit_string_t $$moonbitlang$core$builtin$Iterator$$next$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$565
);

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$556,
  struct $$moonbitlang$core$builtin$Logger logger$554
);

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$550,
  int32_t seg$553,
  int32_t i$552
);

moonbit_string_t $Byte$$to_hex(int32_t b$548);

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$546);

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$544,
  int32_t that$545
);

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$542,
  int32_t that$543
);

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$540,
  int32_t that$541
);

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$538,
  int32_t that$539
);

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$535,
  int32_t start$533,
  int32_t end$534
);

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$532
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$7(
  int32_t a$526,
  int32_t b$527,
  moonbit_string_t msg$529,
  moonbit_string_t loc$531
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$6(
  double a$520,
  double b$521,
  moonbit_string_t msg$523,
  moonbit_string_t loc$525
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$5(
  int32_t a$514,
  int32_t b$515,
  moonbit_string_t msg$517,
  moonbit_string_t loc$519
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  int32_t a$508,
  int32_t b$509,
  moonbit_string_t msg$511,
  moonbit_string_t loc$513
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  moonbit_string_t a$502,
  moonbit_string_t b$503,
  moonbit_string_t msg$505,
  moonbit_string_t loc$507
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  int64_t a$496,
  int64_t b$497,
  moonbit_string_t msg$499,
  moonbit_string_t loc$501
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  moonbit_string_t a$490,
  moonbit_string_t b$491,
  moonbit_string_t msg$493,
  moonbit_string_t loc$495
);

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  int32_t a$484,
  int32_t b$485,
  moonbit_string_t msg$487,
  moonbit_string_t loc$489
);

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$483,
  moonbit_string_t loc$482
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$7(int32_t t$481);

moonbit_string_t $moonbitlang$core$builtin$debug_string$6(double t$479);

moonbit_string_t $moonbitlang$core$builtin$debug_string$5(int32_t t$477);

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(int32_t t$475);

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(
  moonbit_string_t t$473
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(int64_t t$471);

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(
  moonbit_string_t t$469
);

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(int32_t t$467);

moonbit_string_t $UInt16$$to_string$inner(
  int32_t self$464,
  int32_t radix$465
);

moonbit_string_t $UInt64$$to_string$inner(
  uint64_t self$456,
  int32_t radix$455
);

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$445,
  uint64_t num$433,
  int32_t digit_start$436,
  int32_t total_len$435
);

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$427,
  uint64_t num$421,
  int32_t digit_start$419,
  int32_t total_len$418,
  int32_t radix$423
);

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$414,
  uint64_t num$410,
  int32_t digit_start$408,
  int32_t total_len$407
);

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$400,
  int32_t radix$403
);

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$398);

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$397);

moonbit_string_t $Int$$to_string$inner(int32_t self$381, int32_t radix$380);

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$374,
  int32_t radix$377
);

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$372);

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$371);

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$361,
  uint32_t num$349,
  int32_t digit_start$352,
  int32_t total_len$351
);

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$343,
  uint32_t num$337,
  int32_t digit_start$335,
  int32_t total_len$334,
  int32_t radix$339
);

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$330,
  uint32_t num$326,
  int32_t digit_start$324,
  int32_t total_len$323
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
  void* self$321
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$319
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  uint64_t self$317
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$315
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$313
);

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$311
);

int32_t $StringView$$start_offset(struct $StringView self$309);

moonbit_string_t $StringView$$data(struct $StringView self$308);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$302,
  moonbit_string_t value$305,
  int32_t start$306,
  int32_t len$307
);

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$295,
  int32_t start$301,
  int64_t end$297
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$293
);

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$291
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$288
);

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$286
);

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$285
);

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$284
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$282,
  int32_t value$281
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$280,
  moonbit_string_t value$279
);

uint64_t $Int$$to_uint64(int32_t self$278);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$276,
  int32_t value$277
);

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$274,
  uint32_t value$275
);

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$272,
  uint32_t input$273
);

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$270, int32_t r$271);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$268,
  moonbit_string_t str$269
);

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$260,
  int32_t bytes_offset$255,
  moonbit_string_t str$262,
  int32_t str_offset$258,
  int32_t length$256
);

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$222
);

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$218
);

struct $StringView $StringView$$view$inner(
  struct $StringView self$215,
  int32_t start_offset$216,
  int64_t end_offset$213
);

int64_t $StringView$$rev_find(
  struct $StringView self$211,
  struct $StringView str$210
);

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$201,
  struct $StringView needle$203
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$190,
  struct $StringView needle$192
);

int64_t $StringView$$find(
  struct $StringView self$188,
  struct $StringView str$187
);

int64_t $moonbitlang$core$builtin$brute_force_find(
  struct $StringView haystack$177,
  struct $StringView needle$179
);

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$163,
  struct $StringView needle$165
);

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$159,
  int32_t index$160
);

int32_t $StringView$$length(struct $StringView self$158);

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* $$moonbitlang$core$builtin$Array$$at$2(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$156,
  int32_t index$157
);

int32_t $$moonbitlang$core$builtin$Array$$at$1(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$153,
  int32_t index$154
);

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$150,
  int32_t index$151
);

int32_t $$moonbitlang$core$builtin$Array$$length$5(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$148
);

int32_t $$moonbitlang$core$builtin$Array$$length$4(
  struct $$moonbitlang$core$builtin$Array$3c$Double$3e$* self$147
);

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$146
);

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$145
);

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$144
);

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$143
);

int32_t* $$moonbitlang$core$builtin$Array$$buffer$5(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$142
);

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$141
);

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$140
);

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$139
);

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$138
);

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$137
);

moonbit_string_t $String$$escape(moonbit_string_t self$136);

int32_t $moonbitlang$core$builtin$op_notequal$5(
  moonbit_string_t x$133,
  moonbit_string_t y$134
);

int32_t $moonbitlang$core$builtin$op_notequal$4(int32_t x$131, int32_t y$132);

int32_t $moonbitlang$core$builtin$op_notequal$3(int32_t x$129, int32_t y$130);

int32_t $moonbitlang$core$builtin$op_notequal$2(
  moonbit_string_t x$127,
  moonbit_string_t y$128
);

int32_t $moonbitlang$core$builtin$op_notequal$1(int64_t x$125, int64_t y$126);

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$123, int32_t y$124);

int32_t $Int$$is_trailing_surrogate(int32_t self$122);

int32_t $Int$$is_leading_surrogate(int32_t self$121);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$118,
  int32_t ch$120
);

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$113,
  int32_t required$114
);

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default();

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$107,
  int32_t offset$108,
  int32_t value$106
);

int32_t $UInt$$to_byte(uint32_t self$104);

uint32_t $Char$$to_uint(int32_t self$103);

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$102
);

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$97,
  int32_t offset$101,
  int64_t length$99
);

#define $moonbitlang$core$builtin$unsafe_sub_string moonbit_unsafe_bytes_sub_string

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$94
);

int32_t $Byte$$to_char(int32_t self$92);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$3(
  int32_t* dst$87,
  int32_t dst_offset$88,
  int32_t* src$89,
  int32_t src_offset$90,
  int32_t len$91
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$82,
  int32_t dst_offset$83,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$84,
  int32_t src_offset$85,
  int32_t len$86
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$77,
  int32_t dst_offset$78,
  struct $$3c$String$2a$Int$3e$** src$79,
  int32_t src_offset$80,
  int32_t len$81
);

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$72,
  int32_t dst_offset$73,
  moonbit_string_t* src$74,
  int32_t src_offset$75,
  int32_t len$76
);

int32_t $FixedArray$$unsafe_blit$4(
  int32_t* dst$63,
  int32_t dst_offset$65,
  int32_t* src$64,
  int32_t src_offset$66,
  int32_t len$68
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

moonbit_string_t $Error$to_string(void* _e$1643);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1661,
  int32_t _param$1660
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1658,
  struct $StringView _param$1657
);

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1655,
  moonbit_string_t _param$1652,
  int32_t _param$1653,
  int32_t _param$1654
);

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1650,
  moonbit_string_t _param$1649
);

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_124 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    104, 101, 108, 108, 111, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_118 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    122, 101, 114, 111, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_34 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 51, 55, 58, 51, 45, 
    53, 51, 55, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[1]; 
} const moonbit_string_literal_3 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 0), 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[103]; 
} const moonbit_string_literal_212 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 102), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 58, 51, 45, 54, 58, 
    50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_65 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 97, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_140 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 51, 48, 58, 51, 45, 
    50, 51, 48, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_209 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 56, 48, 58, 51, 45, 56, 
    48, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_223 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 54, 58, 51, 45, 50, 
    54, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_218 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 53, 58, 51, 45, 49, 
    53, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_246 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    78, 111, 110, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_120 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 50, 53, 58, 51, 45, 
    51, 50, 53, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_205 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 49, 58, 51, 45, 55, 
    49, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_234 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 54, 58, 51, 45, 52, 
    54, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_98 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 52, 55, 58, 51, 45, 
    51, 52, 55, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_36 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 52, 49, 58, 51, 45, 
    53, 52, 49, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_146 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    49, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_135 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 53, 56, 58, 51, 45, 
    50, 53, 56, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_185 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 48, 58, 51, 45, 57, 
    48, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_68 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_94 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[136]; 
} const moonbit_string_literal_274 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 135), 
    97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 
    116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 109, 
    117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 47, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 46, 77, 
    111, 111, 110, 66, 105, 116, 84, 101, 115, 116, 68, 114, 105, 118, 
    101, 114, 73, 110, 116, 101, 114, 110, 97, 108, 74, 115, 69, 114, 
    114, 111, 114, 46, 77, 111, 111, 110, 66, 105, 116, 84, 101, 115, 
    116, 68, 114, 105, 118, 101, 114, 73, 110, 116, 101, 114, 110, 97, 
    108, 74, 115, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_74 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 56, 50, 58, 51, 45, 
    51, 56, 50, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_204 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 48, 58, 51, 45, 55, 
    48, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_91 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 32, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_19 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 57, 57, 58, 51, 45, 
    52, 57, 57, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_247 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    83, 111, 109, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[103]; 
} const moonbit_string_literal_214 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 102), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 56, 58, 51, 45, 56, 58, 
    50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_53 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 54, 53, 58, 51, 45, 
    52, 54, 53, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_261 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 54, 54, 51, 
    58, 53, 45, 54, 54, 51, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_250 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 51, 54, 54, 58, 53, 45, 
    51, 54, 54, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_148 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 48, 57, 58, 51, 45, 
    50, 48, 57, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[30]; 
} const moonbit_string_literal_103 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 29), 
    33, 64, 35, 36, 37, 94, 38, 42, 40, 41, 95, 43, 45, 61, 91, 93, 123, 
    125, 124, 59, 39, 58, 34, 44, 46, 47, 60, 62, 63, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_2 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 47, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[26]; 
} const moonbit_string_literal_238 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 25), 
    73, 108, 108, 101, 103, 97, 108, 65, 114, 103, 117, 109, 101, 110, 
    116, 69, 120, 99, 101, 112, 116, 105, 111, 110, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_233 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 53, 58, 51, 45, 52, 
    53, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[34]; 
} const moonbit_string_literal_279 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 33), 
    97, 114, 114, 97, 121, 32, 109, 97, 110, 105, 112, 117, 108, 97, 
    116, 105, 111, 110, 32, 97, 110, 100, 32, 101, 100, 103, 101, 32, 
    99, 97, 115, 101, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_290 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_179 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 55, 58, 51, 45, 
    49, 50, 55, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_176 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 49, 58, 51, 45, 
    49, 50, 49, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_60 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 55, 52, 58, 51, 45, 
    52, 55, 52, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_245 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 102, 97, 108, 115, 101, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_133 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 48, 51, 58, 51, 45, 
    51, 48, 51, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_256 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_75 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 56, 51, 58, 51, 45, 
    51, 56, 51, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[24]; 
} const moonbit_string_literal_282 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 23), 
    99, 111, 109, 112, 108, 101, 120, 32, 100, 97, 116, 97, 32, 115, 
    116, 114, 117, 99, 116, 117, 114, 101, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_186 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 49, 58, 51, 45, 57, 
    49, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_171 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    97, 112, 112, 108, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_87 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 49, 54, 58, 51, 45, 
    52, 49, 54, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_26 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 49, 53, 58, 51, 45, 
    53, 49, 53, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_66 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 98, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_132 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 48, 50, 58, 51, 45, 
    51, 48, 50, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_70 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 55, 53, 58, 51, 45, 
    51, 55, 53, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_82 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 57, 52, 58, 51, 45, 
    51, 57, 52, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_52 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 54, 50, 58, 51, 45, 
    52, 54, 50, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[30]; 
} const moonbit_string_literal_276 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 29), 
    98, 97, 115, 105, 99, 32, 116, 101, 108, 101, 109, 101, 116, 114, 
    121, 32, 102, 117, 110, 99, 116, 105, 111, 110, 97, 108, 105, 116, 
    121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[28]; 
} const moonbit_string_literal_288 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 27), 
    97, 100, 118, 97, 110, 99, 101, 100, 32, 110, 117, 109, 101, 114, 
    105, 99, 32, 111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_217 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 52, 58, 51, 45, 49, 
    52, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_59 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 55, 51, 58, 51, 45, 
    52, 55, 51, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_122 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 55, 53, 58, 51, 45, 
    50, 55, 53, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_162 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 54, 49, 58, 51, 45, 
    49, 54, 49, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_43 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 52, 55, 58, 51, 45, 
    52, 52, 55, 58, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_169 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 56, 49, 58, 51, 45, 
    49, 56, 49, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_159 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 53, 50, 58, 51, 45, 
    49, 53, 50, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_139 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 50, 57, 58, 51, 45, 
    50, 50, 57, 58, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_39 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 51, 54, 58, 51, 45, 
    52, 51, 54, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_90 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    87, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_105 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    97, 98, 99, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_95 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 52, 52, 58, 51, 45, 
    51, 52, 52, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_144 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 48, 49, 58, 51, 45, 
    50, 48, 49, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_127 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    101, 118, 101, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_149 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 49, 48, 58, 51, 45, 
    50, 49, 48, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_201 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 53, 58, 51, 45, 54, 
    53, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_187 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 53, 58, 51, 45, 57, 
    53, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_100 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 53, 49, 58, 51, 45, 
    51, 53, 49, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_40 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 51, 55, 58, 51, 45, 
    52, 51, 55, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_4 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[50]; 
} const moonbit_string_literal_252 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 49), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 46, 109, 98, 116, 58, 52, 57, 49, 58, 57, 45, 
    52, 57, 49, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_182 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 51, 53, 58, 51, 45, 
    49, 51, 53, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_251 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    105, 110, 118, 97, 108, 105, 100, 32, 115, 117, 114, 114, 111, 103, 
    97, 116, 101, 32, 112, 97, 105, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_77 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 56, 55, 58, 51, 45, 
    51, 56, 55, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_56 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 55, 48, 58, 51, 45, 
    52, 55, 48, 58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[31]; 
} const moonbit_string_literal_260 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 30), 
    114, 97, 100, 105, 120, 32, 109, 117, 115, 116, 32, 98, 101, 32, 
    98, 101, 116, 119, 101, 101, 110, 32, 50, 32, 97, 110, 100, 32, 51, 
    54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_29 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    102, 97, 108, 115, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[34]; 
} const moonbit_string_literal_280 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 33), 
    111, 112, 116, 105, 111, 110, 32, 116, 121, 112, 101, 32, 99, 111, 
    109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 32, 116, 101, 
    115, 116, 105, 110, 103, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[84]; 
} const moonbit_string_literal_8 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 83), 
    123, 34, 112, 97, 99, 107, 97, 103, 101, 34, 58, 32, 34, 97, 122, 
    105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 
    121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 109, 117, 116, 
    104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 47, 99, 111, 
    109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 34, 44, 32, 
    34, 102, 105, 108, 101, 110, 97, 109, 101, 34, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_177 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 51, 58, 51, 45, 
    49, 50, 51, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_137 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 54, 51, 58, 51, 45, 
    50, 54, 51, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_37 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 52, 50, 58, 51, 45, 
    53, 52, 50, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_271 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    73, 110, 100, 101, 120, 79, 117, 116, 79, 102, 66, 111, 117, 110, 
    100, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_222 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 53, 58, 51, 45, 50, 
    53, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_183 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 51, 54, 58, 51, 45, 
    49, 51, 54, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_113 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    100, 97, 116, 97, 95, 36, 123, 105, 125, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_262 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102, 
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 
    116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[103]; 
} const moonbit_string_literal_213 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 102), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 58, 51, 45, 55, 58, 
    50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_206 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 52, 58, 51, 45, 55, 
    52, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_47 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 53, 53, 58, 51, 45, 
    52, 53, 53, 58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_166 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_143 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 52, 48, 58, 51, 45, 
    50, 52, 48, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_89 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    72, 101, 108, 108, 111, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_17 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 57, 55, 58, 51, 45, 
    52, 57, 55, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_31 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 50, 51, 58, 51, 45, 
    53, 50, 51, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_108 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 54, 49, 58, 51, 45, 
    51, 54, 49, 58, 52, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_76 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 56, 52, 58, 51, 45, 
    51, 56, 52, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_115 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    115, 116, 114, 105, 110, 103, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_20 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 48, 49, 58, 51, 45, 
    53, 48, 49, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_277 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    110, 117, 109, 101, 114, 105, 99, 32, 99, 111, 109, 112, 97, 114, 
    105, 115, 111, 110, 115, 32, 97, 110, 100, 32, 101, 100, 103, 101, 
    32, 99, 97, 115, 101, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_72 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 55, 55, 58, 51, 45, 
    51, 55, 55, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_270 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    70, 97, 105, 108, 117, 114, 101, 40, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_232 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 57, 58, 51, 45, 51, 
    57, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[17]; 
} const moonbit_string_literal_10 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 16), 
    34, 44, 32, 34, 116, 101, 115, 116, 95, 110, 97, 109, 101, 34, 58, 
    32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_175 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 48, 58, 51, 45, 
    49, 50, 48, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_67 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_25 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 49, 52, 58, 51, 45, 
    53, 49, 52, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_58 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 55, 50, 58, 51, 45, 
    52, 55, 50, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_193 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 48, 52, 58, 51, 45, 
    49, 48, 52, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[49]; 
} const moonbit_string_literal_267 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 48), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 98, 121, 
    116, 101, 115, 46, 109, 98, 116, 58, 50, 57, 48, 58, 53, 45, 50, 
    57, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_134 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 53, 50, 58, 51, 45, 
    50, 53, 50, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_57 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 55, 49, 58, 51, 45, 
    52, 55, 49, 58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_199 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 51, 58, 51, 45, 54, 
    51, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_278 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    115, 116, 114, 105, 110, 103, 32, 109, 97, 110, 105, 112, 117, 108, 
    97, 116, 105, 111, 110, 32, 97, 110, 100, 32, 101, 100, 103, 101, 
    32, 99, 97, 115, 101, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_207 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 53, 58, 51, 45, 55, 
    53, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_178 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 50, 52, 58, 51, 45, 
    49, 50, 52, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_243 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 96, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[18]; 
} const moonbit_string_literal_266 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 17), 
    67, 104, 97, 114, 32, 111, 117, 116, 32, 111, 102, 32, 114, 97, 110, 
    103, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_23 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 48, 54, 58, 51, 45, 
    53, 48, 54, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_21 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 48, 50, 58, 51, 45, 
    53, 48, 50, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_14 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 57, 51, 58, 51, 45, 
    52, 57, 51, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_28 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_208 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 55, 54, 58, 51, 45, 55, 
    54, 58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_138 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 50, 56, 58, 51, 45, 
    50, 50, 56, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_255 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 98, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_129 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 57, 57, 58, 51, 45, 
    50, 57, 57, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_44 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 53, 48, 58, 51, 45, 
    52, 53, 48, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_84 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 57, 57, 58, 51, 45, 
    51, 57, 57, 58, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_62 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 56, 48, 58, 51, 45, 
    52, 56, 48, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_35 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 51, 56, 58, 51, 45, 
    53, 51, 56, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_157 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 52, 57, 58, 51, 45, 
    49, 52, 57, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_242 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    48, 46, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_117 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    98, 111, 111, 108, 101, 97, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[34]; 
} const moonbit_string_literal_281 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 33), 
    116, 121, 112, 101, 32, 99, 111, 110, 118, 101, 114, 115, 105, 111, 
    110, 32, 97, 110, 100, 32, 99, 111, 109, 112, 97, 116, 105, 98, 105, 
    108, 105, 116, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_88 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 50, 53, 58, 51, 45, 
    52, 50, 53, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_38 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 52, 51, 58, 51, 45, 
    53, 52, 51, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_196 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 48, 58, 51, 45, 54, 
    48, 58, 50, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_97 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 52, 54, 58, 51, 45, 
    51, 52, 54, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_6 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_202 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 56, 58, 51, 45, 54, 
    56, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_273 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 46, 83, 110, 97, 
    112, 115, 104, 111, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_111 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    122, 122, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_253 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 110, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[9]; 
} const moonbit_string_literal_241 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 8), 
    73, 110, 102, 105, 110, 105, 116, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_240 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    78, 97, 78, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_81 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 57, 51, 58, 51, 45, 
    51, 57, 51, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[27]; 
} const moonbit_string_literal_286 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 26), 
    97, 100, 118, 97, 110, 99, 101, 100, 32, 115, 116, 114, 105, 110, 
    103, 32, 111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_191 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 48, 48, 58, 51, 45, 
    49, 48, 48, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_237 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 48, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_154 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 49, 53, 58, 51, 45, 
    50, 49, 53, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_107 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_50 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 54, 48, 58, 51, 45, 
    52, 54, 48, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_200 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 52, 58, 51, 45, 54, 
    52, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_163 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 54, 50, 58, 51, 45, 
    49, 54, 50, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_32 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 51, 51, 58, 51, 45, 
    53, 51, 51, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_236 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 52, 58, 51, 45, 53, 
    52, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_221 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 51, 58, 51, 45, 50, 
    51, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_215 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    32, 87, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[54]; 
} const moonbit_string_literal_265 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 53), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 115, 116, 
    114, 105, 110, 103, 118, 105, 101, 119, 46, 109, 98, 116, 58, 49, 
    49, 49, 58, 53, 45, 49, 49, 49, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_226 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 49, 58, 51, 45, 51, 
    49, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_195 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 48, 56, 58, 51, 45, 
    49, 48, 56, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_141 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 51, 56, 58, 51, 45, 
    50, 51, 56, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_9 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    44, 32, 34, 105, 110, 100, 101, 120, 34, 58, 32, 34, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[53]; 
} const moonbit_string_literal_263 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 52), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 58, 116, 111, 
    95, 115, 116, 114, 105, 110, 103, 46, 109, 98, 116, 58, 50, 51, 57, 
    58, 53, 45, 50, 51, 57, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_248 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 41, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_224 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 55, 58, 51, 45, 50, 
    55, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_78 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 56, 56, 58, 51, 45, 
    51, 56, 56, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[8]; 
} const moonbit_string_literal_165 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 7), 
    100, 101, 102, 97, 117, 108, 116, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_167 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_0 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 58, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_45 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 53, 49, 58, 51, 45, 
    52, 53, 49, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_210 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 56, 49, 58, 51, 45, 56, 
    49, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_172 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    98, 97, 110, 97, 110, 97, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_160 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 53, 51, 58, 51, 45, 
    49, 53, 51, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_254 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    92, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_244 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    96, 32, 105, 115, 32, 110, 111, 116, 32, 116, 114, 117, 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_121 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 55, 52, 58, 51, 45, 
    50, 55, 52, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_16 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 57, 53, 58, 51, 45, 
    52, 57, 53, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_1 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 45, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_203 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 57, 58, 51, 45, 54, 
    57, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_131 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 48, 49, 58, 51, 45, 
    51, 48, 49, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_230 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 54, 58, 51, 45, 51, 
    54, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_220 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 50, 58, 51, 45, 50, 
    50, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_235 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 55, 58, 51, 45, 52, 
    55, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[37]; 
} const moonbit_string_literal_285 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 36), 
    115, 121, 115, 116, 101, 109, 32, 105, 110, 116, 101, 103, 114, 97, 
    116, 105, 111, 110, 32, 97, 110, 100, 32, 99, 111, 109, 112, 97, 
    116, 105, 98, 105, 108, 105, 116, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_85 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 48, 48, 58, 51, 45, 
    52, 48, 48, 58, 52, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_130 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 48, 48, 58, 51, 45, 
    51, 48, 48, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_73 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 55, 56, 58, 51, 45, 
    51, 55, 56, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_102 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 53, 51, 58, 51, 45, 
    51, 53, 51, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_142 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 51, 57, 58, 51, 45, 
    50, 51, 57, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_41 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 52, 53, 58, 51, 45, 
    52, 52, 53, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[20]; 
} const moonbit_string_literal_249 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 19), 
    73, 110, 118, 97, 108, 105, 100, 32, 115, 116, 97, 114, 116, 32, 
    105, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_155 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 52, 55, 58, 51, 45, 
    49, 52, 55, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[59]; 
} const moonbit_string_literal_239 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 58), 
    64, 109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 
    111, 114, 101, 47, 100, 111, 117, 98, 108, 101, 47, 105, 110, 116, 
    101, 114, 110, 97, 108, 47, 114, 121, 117, 58, 114, 121, 117, 46, 
    109, 98, 116, 58, 49, 49, 54, 58, 51, 45, 49, 49, 54, 58, 52, 53, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_126 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 56, 56, 58, 51, 45, 
    50, 56, 56, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_54 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 54, 54, 58, 51, 45, 
    52, 54, 54, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[26]; 
} const moonbit_string_literal_287 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 25), 
    97, 100, 118, 97, 110, 99, 101, 100, 32, 97, 114, 114, 97, 121, 32, 
    111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_15 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 57, 52, 58, 51, 45, 
    52, 57, 52, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_51 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 54, 49, 58, 51, 45, 
    52, 54, 49, 58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_27 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 49, 54, 58, 51, 45, 
    53, 49, 54, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_258 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    32, 33, 61, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_227 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 50, 58, 51, 45, 51, 
    50, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_101 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 53, 50, 58, 51, 45, 
    51, 53, 50, 58, 51, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_42 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 52, 54, 58, 51, 45, 
    52, 52, 54, 58, 52, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_197 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 49, 58, 51, 45, 54, 
    49, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_194 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 11), 
    72, 101, 108, 108, 111, 32, 19990, 30028, 32, 55357, 56960, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_147 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 48, 54, 58, 51, 45, 
    50, 48, 54, 58, 51, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_48 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 53, 54, 58, 51, 45, 
    52, 53, 54, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_96 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 52, 53, 58, 51, 45, 
    51, 52, 53, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_22 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 48, 53, 58, 51, 45, 
    53, 48, 53, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_231 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 56, 58, 51, 45, 51, 
    56, 58, 50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_181 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    111, 110, 108, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_69 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 101, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_268 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    10, 32, 32, 97, 116, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_104 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 53, 55, 58, 51, 45, 
    51, 53, 55, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_83 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 57, 56, 58, 51, 45, 
    51, 57, 56, 58, 51, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[5]; 
} const moonbit_string_literal_109 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 4), 
    109, 110, 111, 112, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_79 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 57, 49, 58, 51, 45, 
    51, 57, 49, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[30]; 
} const moonbit_string_literal_283 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 29), 
    101, 114, 114, 111, 114, 32, 104, 97, 110, 100, 108, 105, 110, 103, 
    32, 97, 110, 100, 32, 101, 100, 103, 101, 32, 99, 97, 115, 101, 115, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[10]; 
} const moonbit_string_literal_259 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 9), 
    32, 70, 65, 73, 76, 69, 68, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_128 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    111, 100, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_93 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 51, 54, 58, 51, 45, 
    51, 51, 54, 58, 52, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_61 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 55, 53, 58, 51, 45, 
    52, 55, 53, 58, 50, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_145 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 48, 50, 58, 51, 45, 
    50, 48, 50, 58, 52, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_112 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 54, 51, 58, 51, 45, 
    51, 54, 51, 58, 52, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_64 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 56, 54, 58, 51, 45, 
    52, 56, 54, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_5 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    115, 107, 105, 112, 112, 101, 100, 32, 116, 101, 115, 116, 32, 98, 
    108, 111, 99, 107, 58, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_119 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    101, 109, 112, 116, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[28]; 
} const moonbit_string_literal_289 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 27), 
    97, 100, 118, 97, 110, 99, 101, 100, 32, 98, 111, 111, 108, 101, 
    97, 110, 32, 111, 112, 101, 114, 97, 116, 105, 111, 110, 115, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_161 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 54, 48, 58, 51, 45, 
    49, 54, 48, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_71 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 55, 54, 58, 51, 45, 
    51, 55, 54, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_33 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 51, 54, 58, 51, 45, 
    53, 51, 54, 58, 50, 52, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_173 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    99, 104, 101, 114, 114, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_136 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 53, 57, 58, 51, 45, 
    50, 53, 57, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[51]; 
} const moonbit_string_literal_275 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 50), 
    109, 111, 111, 110, 98, 105, 116, 108, 97, 110, 103, 47, 99, 111, 
    114, 101, 47, 98, 117, 105, 108, 116, 105, 110, 46, 73, 110, 115, 
    112, 101, 99, 116, 69, 114, 114, 111, 114, 46, 73, 110, 115, 112, 
    101, 99, 116, 69, 114, 114, 111, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[2]; 
} const moonbit_string_literal_269 =
  { -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 1), 10, 0};

struct { int32_t rc; uint32_t meta; uint16_t const data[103]; 
} const moonbit_string_literal_211 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 102), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 58, 51, 45, 53, 58, 
    50, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_198 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 54, 50, 58, 51, 45, 54, 
    50, 58, 50, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[12]; 
} const moonbit_string_literal_216 =
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

struct { int32_t rc; uint32_t meta; uint16_t const data[15]; 
} const moonbit_string_literal_92 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 14), 
    72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 32, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_228 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 52, 58, 51, 45, 51, 
    52, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_158 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 53, 49, 58, 51, 45, 
    49, 53, 49, 58, 51, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_225 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 48, 58, 51, 45, 51, 
    48, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_180 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 51, 49, 58, 51, 45, 
    49, 51, 49, 58, 51, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_164 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 54, 57, 58, 51, 45, 
    49, 54, 57, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_153 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 49, 52, 58, 51, 45, 
    50, 49, 52, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_229 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 53, 58, 51, 45, 51, 
    53, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[35]; 
} const moonbit_string_literal_7 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 34), 
    45, 45, 45, 45, 45, 32, 66, 69, 71, 73, 78, 32, 77, 79, 79, 78, 32, 
    84, 69, 83, 84, 32, 82, 69, 83, 85, 76, 84, 32, 45, 45, 45, 45, 45, 
    0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_168 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 55, 53, 58, 51, 45, 
    49, 55, 53, 58, 52, 50, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[21]; 
} const moonbit_string_literal_192 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 20), 
    72, 101, 108, 108, 111, 33, 64, 35, 36, 37, 94, 38, 42, 40, 41, 87, 
    111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[23]; 
} const moonbit_string_literal_264 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 22), 
    73, 110, 118, 97, 108, 105, 100, 32, 105, 110, 100, 101, 120, 32, 
    102, 111, 114, 32, 86, 105, 101, 119, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_30 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 50, 48, 58, 51, 45, 
    53, 50, 48, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_156 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 52, 56, 58, 51, 45, 
    49, 52, 56, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[6]; 
} const moonbit_string_literal_152 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 5), 
    119, 111, 114, 108, 100, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_55 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 54, 55, 58, 51, 45, 
    52, 54, 55, 58, 50, 55, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_106 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 54, 48, 58, 51, 45, 
    51, 54, 48, 58, 52, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[27]; 
} const moonbit_string_literal_284 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 26), 
    112, 101, 114, 102, 111, 114, 109, 97, 110, 99, 101, 32, 97, 110, 
    100, 32, 101, 102, 102, 105, 99, 105, 101, 110, 99, 121, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[4]; 
} const moonbit_string_literal_257 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 3), 
    92, 117, 123, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_46 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 53, 52, 58, 51, 45, 
    52, 53, 52, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_80 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 57, 50, 58, 51, 45, 
    51, 57, 50, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_151 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 49, 51, 58, 51, 45, 
    50, 49, 51, 58, 50, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_125 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 56, 49, 58, 51, 45, 
    50, 56, 49, 58, 52, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_114 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 49, 52, 58, 51, 45, 
    51, 49, 52, 58, 53, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_63 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 56, 49, 58, 51, 45, 
    52, 56, 49, 58, 51, 54, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_24 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 53, 48, 55, 58, 51, 45, 
    53, 48, 55, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_18 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 57, 56, 58, 51, 45, 
    52, 57, 56, 58, 50, 57, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[27]; 
} const moonbit_string_literal_99 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 26), 
    97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 
    111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[14]; 
} const moonbit_string_literal_190 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 13), 
    32, 104, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 32, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_184 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 56, 57, 58, 51, 45, 56, 
    57, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_174 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 49, 57, 58, 51, 45, 
    49, 49, 57, 58, 51, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_123 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 55, 54, 58, 51, 45, 
    50, 55, 54, 58, 51, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[3]; 
} const moonbit_string_literal_188 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 2), 
    97, 97, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_86 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 48, 55, 58, 51, 45, 
    52, 48, 55, 58, 50, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_49 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 52, 53, 57, 58, 51, 45, 
    52, 53, 57, 58, 50, 51, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_219 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 54, 58, 51, 45, 49, 
    54, 58, 51, 48, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[105]; 
} const moonbit_string_literal_189 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 104), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 57, 54, 58, 51, 45, 57, 
    54, 58, 52, 53, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_170 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 49, 57, 48, 58, 51, 45, 
    49, 57, 48, 58, 51, 49, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_150 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 50, 49, 49, 58, 51, 45, 
    50, 49, 49, 58, 50, 56, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[7]; 
} const moonbit_string_literal_116 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 6), 
    110, 117, 109, 98, 101, 114, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[13]; 
} const moonbit_string_literal_272 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 12), 
    73, 110, 118, 97, 108, 105, 100, 73, 110, 100, 101, 120, 0
  };

struct { int32_t rc; uint32_t meta; uint16_t const data[107]; 
} const moonbit_string_literal_110 =
  {
    -1, Moonbit_make_array_header(moonbit_BLOCK_KIND_VAL_ARRAY, 1, 106), 
    64, 97, 122, 105, 109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 
    101, 116, 114, 121, 47, 116, 101, 115, 116, 115, 47, 97, 122, 105, 
    109, 117, 116, 104, 47, 116, 101, 108, 101, 109, 101, 116, 114, 121, 
    47, 99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 
    95, 98, 108, 97, 99, 107, 98, 111, 120, 95, 116, 101, 115, 116, 58, 
    99, 111, 109, 112, 114, 101, 104, 101, 110, 115, 105, 118, 101, 95, 
    116, 101, 115, 116, 46, 109, 98, 116, 58, 51, 54, 50, 58, 51, 45, 
    51, 54, 50, 58, 52, 52, 0
  };

struct moonbit_object const moonbit_constant_constructor_0 =
  { -1, Moonbit_make_regular_object_header(2, 0, 0)};

struct moonbit_object const moonbit_constant_constructor_1 =
  { -1, Moonbit_make_regular_object_header(2, 0, 1)};

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1$dyncall
  };

struct { int32_t rc; uint32_t meta; struct $$3c$String$3e$$3d$$3e$Int data; 
} const $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7$dyncall
  };

struct {
  int32_t rc;
  uint32_t meta;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$ data;
  
} const $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2$dyncall$closure =
  {
    -1, Moonbit_make_regular_object_header(2, 0, 0),
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2$dyncall
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

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$161;

int64_t $moonbitlang$core$builtin$brute_force_find$constr$175;

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
  
} $moonbitlang$core$double$internal$ryu$ryu_to_string$record$1307$object =
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

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$ryu_to_string$record$1307 =
  &$moonbitlang$core$double$internal$ryu$ryu_to_string$record$1307$object.data;

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_max_concurrent_tests =
  10;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_no_args_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_async_tests;

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_with_args_tests;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7$clo;

struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10$clo;

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4100
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4099
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4098
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4097
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4096
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4095
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4094
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4093
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4092
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4091
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4090
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4089
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4088
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9();
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3$dyncall(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$4087
) {
  return $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3();
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_run_async_tests(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* _tests$1636
) {
  moonbit_decref(_tests$1636);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args(
  
) {
  int32_t moonbit_test_driver_internal_parse_int_$1595 = 0;
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1601 = 0;
  int32_t moonbit_test_driver_internal_get_cli_args_internal$1608 =
    moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1601;
  int32_t moonbit_test_driver_internal_split_mbt_string$1613 = 0;
  struct $$3c$String$2a$Int$3e$** _tmp$4086 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* file_and_index$1620 =
    (struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$
      )
    );
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* cli_args$1621;
  moonbit_string_t _tmp$4085;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* test_args$1622;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _arr$1623;
  int32_t _len$1624;
  int32_t _i$1625;
  Moonbit_object_header(file_and_index$1620)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  file_and_index$1620->$0 = _tmp$4086;
  file_and_index$1620->$1 = 0;
  cli_args$1621
  = $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
    moonbit_test_driver_internal_get_cli_args_internal$1608
  );
  _tmp$4085 = $$moonbitlang$core$builtin$Array$$at$0(cli_args$1621, 1);
  test_args$1622
  = $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
    moonbit_test_driver_internal_split_mbt_string$1613, _tmp$4085, 47
  );
  _arr$1623 = test_args$1622;
  moonbit_incref(_arr$1623);
  _len$1624 = $$moonbitlang$core$builtin$Array$$length$1(_arr$1623);
  _i$1625 = 0;
  while (1) {
    if (_i$1625 < _len$1624) {
      moonbit_string_t arg$1626;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* file_and_range$1627;
      moonbit_string_t file$1628;
      moonbit_string_t range$1629;
      struct $$moonbitlang$core$builtin$Array$3c$String$3e$* start_and_end$1630;
      moonbit_string_t _tmp$4083;
      int32_t start$1631;
      moonbit_string_t _tmp$4082;
      int32_t end$1632;
      int32_t i$1633;
      int32_t _tmp$4084;
      moonbit_incref(_arr$1623);
      arg$1626
      = $$moonbitlang$core$builtin$Array$$unsafe_get$1(
        _arr$1623, _i$1625
      );
      file_and_range$1627
      = $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1613, arg$1626, 58
      );
      moonbit_incref(file_and_range$1627);
      file$1628
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1627, 0
      );
      range$1629
      = $$moonbitlang$core$builtin$Array$$at$0(
        file_and_range$1627, 1
      );
      start_and_end$1630
      = $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
        moonbit_test_driver_internal_split_mbt_string$1613, range$1629, 45
      );
      moonbit_incref(start_and_end$1630);
      _tmp$4083
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1630, 0
      );
      start$1631
      = $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1595, _tmp$4083
      );
      _tmp$4082
      = $$moonbitlang$core$builtin$Array$$at$0(
        start_and_end$1630, 1
      );
      end$1632
      = $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
        moonbit_test_driver_internal_parse_int_$1595, _tmp$4082
      );
      i$1633 = start$1631;
      while (1) {
        if (i$1633 < end$1632) {
          struct $$3c$String$2a$Int$3e$* _tuple$4080;
          int32_t _tmp$4081;
          moonbit_incref(file$1628);
          _tuple$4080
          = (struct $$3c$String$2a$Int$3e$*)moonbit_malloc(
              sizeof(struct $$3c$String$2a$Int$3e$)
            );
          Moonbit_object_header(_tuple$4080)->meta
          = Moonbit_make_regular_object_header(
            offsetof(struct $$3c$String$2a$Int$3e$, $0) >> 2, 1, 0
          );
          _tuple$4080->$0 = file$1628;
          _tuple$4080->$1 = i$1633;
          moonbit_incref(file_and_index$1620);
          $$moonbitlang$core$builtin$Array$$push$1(
            file_and_index$1620, _tuple$4080
          );
          _tmp$4081 = i$1633 + 1;
          i$1633 = _tmp$4081;
          continue;
        } else {
          moonbit_decref(file$1628);
        }
        break;
      }
      _tmp$4084 = _i$1625 + 1;
      _i$1625 = _tmp$4084;
      continue;
    } else {
      moonbit_decref(_arr$1623);
    }
    break;
  }
  return file_and_index$1620;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_split_mbt_string$fn$17(
  int32_t _env$4061,
  moonbit_string_t s$1614,
  int32_t sep$1615
) {
  moonbit_string_t* _tmp$4079 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1616 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  struct $Ref$3c$Int$3e$* i$1617;
  struct $Ref$3c$Int$3e$* start$1618;
  int32_t val$4074;
  int32_t _tmp$4075;
  Moonbit_object_header(res$1616)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  res$1616->$0 = _tmp$4079;
  res$1616->$1 = 0;
  i$1617
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1617)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1617->$0 = 0;
  start$1618
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(start$1618)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  start$1618->$0 = 0;
  while (1) {
    int32_t val$4062 = i$1617->$0;
    int32_t _tmp$4063 = Moonbit_array_length(s$1614);
    if (val$4062 < _tmp$4063) {
      int32_t val$4066 = i$1617->$0;
      int32_t _tmp$4065;
      int32_t _tmp$4064;
      int32_t val$4073;
      int32_t _tmp$4072;
      if (val$4066 < 0 || val$4066 >= Moonbit_array_length(s$1614)) {
        moonbit_panic();
      }
      _tmp$4065 = s$1614[val$4066];
      _tmp$4064 = _tmp$4065;
      if (_tmp$4064 == sep$1615) {
        int32_t val$4068 = start$1618->$0;
        int32_t val$4069 = i$1617->$0;
        moonbit_string_t _tmp$4067;
        int32_t val$4071;
        int32_t _tmp$4070;
        moonbit_incref(s$1614);
        _tmp$4067 = $String$$unsafe_substring(s$1614, val$4068, val$4069);
        moonbit_incref(res$1616);
        $$moonbitlang$core$builtin$Array$$push$0(res$1616, _tmp$4067);
        val$4071 = i$1617->$0;
        _tmp$4070 = val$4071 + 1;
        start$1618->$0 = _tmp$4070;
      }
      val$4073 = i$1617->$0;
      _tmp$4072 = val$4073 + 1;
      i$1617->$0 = _tmp$4072;
      continue;
    } else {
      moonbit_decref(i$1617);
    }
    break;
  }
  val$4074 = start$1618->$0;
  _tmp$4075 = Moonbit_array_length(s$1614);
  if (val$4074 < _tmp$4075) {
    int32_t _field$4101 = start$1618->$0;
    int32_t val$4077;
    int32_t _tmp$4078;
    moonbit_string_t _tmp$4076;
    moonbit_decref(start$1618);
    val$4077 = _field$4101;
    _tmp$4078 = Moonbit_array_length(s$1614);
    _tmp$4076 = $String$$unsafe_substring(s$1614, val$4077, _tmp$4078);
    moonbit_incref(res$1616);
    $$moonbitlang$core$builtin$Array$$push$0(res$1616, _tmp$4076);
  } else {
    moonbit_decref(start$1618);
    moonbit_decref(s$1614);
  }
  return res$1616;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_get_cli_args_internal$fn$16(
  int32_t moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1601
) {
  moonbit_bytes_t* tmp$1609 =
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_get_cli_args_ffi();
  int32_t _tmp$4060 = Moonbit_array_length(tmp$1609);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* res$1610 =
    $$moonbitlang$core$builtin$Array$$new$inner$0(_tmp$4060);
  int32_t i$1611 = 0;
  while (1) {
    int32_t _tmp$4056 = Moonbit_array_length(tmp$1609);
    if (i$1611 < _tmp$4056) {
      moonbit_bytes_t _tmp$4102;
      moonbit_bytes_t _tmp$4058;
      moonbit_string_t _tmp$4057;
      int32_t _tmp$4059;
      if (i$1611 < 0 || i$1611 >= Moonbit_array_length(tmp$1609)) {
        moonbit_panic();
      }
      _tmp$4102 = (moonbit_bytes_t)tmp$1609[i$1611];
      _tmp$4058 = _tmp$4102;
      moonbit_incref(_tmp$4058);
      _tmp$4057
      = $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
        moonbit_test_driver_internal_utf8_bytes_to_mbt_string$1601, _tmp$4058
      );
      moonbit_incref(res$1610);
      $$moonbitlang$core$builtin$Array$$push$0(res$1610, _tmp$4057);
      _tmp$4059 = i$1611 + 1;
      i$1611 = _tmp$4059;
      continue;
    } else {
      moonbit_decref(tmp$1609);
    }
    break;
  }
  return res$1610;
}

moonbit_string_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_utf8_bytes_to_mbt_string$fn$15(
  int32_t _env$3970,
  moonbit_bytes_t bytes$1602
) {
  struct $$moonbitlang$core$builtin$StringBuilder* res$1603 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t len$1604 = Moonbit_array_length(bytes$1602);
  struct $Ref$3c$Int$3e$* i$1605 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(i$1605)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$1605->$0 = 0;
  while (1) {
    int32_t val$3971 = i$1605->$0;
    if (val$3971 < len$1604) {
      int32_t val$4055 = i$1605->$0;
      int32_t _tmp$4054;
      int32_t _tmp$4053;
      struct $Ref$3c$Int$3e$* c$1606;
      int32_t val$3972;
      if (val$4055 < 0 || val$4055 >= Moonbit_array_length(bytes$1602)) {
        moonbit_panic();
      }
      _tmp$4054 = bytes$1602[val$4055];
      _tmp$4053 = (int32_t)_tmp$4054;
      c$1606
      = (struct $Ref$3c$Int$3e$*)moonbit_malloc(
          sizeof(struct $Ref$3c$Int$3e$)
        );
      Moonbit_object_header(c$1606)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
      );
      c$1606->$0 = _tmp$4053;
      val$3972 = c$1606->$0;
      if (val$3972 < 128) {
        int32_t _field$4103 = c$1606->$0;
        int32_t val$3974;
        int32_t _tmp$3973;
        int32_t val$3976;
        int32_t _tmp$3975;
        moonbit_decref(c$1606);
        val$3974 = _field$4103;
        _tmp$3973 = val$3974;
        moonbit_incref(res$1603);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
          res$1603, _tmp$3973
        );
        val$3976 = i$1605->$0;
        _tmp$3975 = val$3976 + 1;
        i$1605->$0 = _tmp$3975;
      } else {
        int32_t val$3977 = c$1606->$0;
        if (val$3977 < 224) {
          int32_t val$3979 = i$1605->$0;
          int32_t _tmp$3978 = val$3979 + 1;
          int32_t val$3988;
          int32_t _tmp$3987;
          int32_t _tmp$3981;
          int32_t val$3986;
          int32_t _tmp$3985;
          int32_t _tmp$3984;
          int32_t _tmp$3983;
          int32_t _tmp$3982;
          int32_t _tmp$3980;
          int32_t _field$4104;
          int32_t val$3990;
          int32_t _tmp$3989;
          int32_t val$3992;
          int32_t _tmp$3991;
          if (_tmp$3978 >= len$1604) {
            moonbit_decref(c$1606);
            moonbit_decref(i$1605);
            moonbit_decref(bytes$1602);
            break;
          }
          val$3988 = c$1606->$0;
          _tmp$3987 = val$3988 & 31;
          _tmp$3981 = _tmp$3987 << 6;
          val$3986 = i$1605->$0;
          _tmp$3985 = val$3986 + 1;
          if (_tmp$3985 < 0 || _tmp$3985 >= Moonbit_array_length(bytes$1602)) {
            moonbit_panic();
          }
          _tmp$3984 = bytes$1602[_tmp$3985];
          _tmp$3983 = (int32_t)_tmp$3984;
          _tmp$3982 = _tmp$3983 & 63;
          _tmp$3980 = _tmp$3981 | _tmp$3982;
          c$1606->$0 = _tmp$3980;
          _field$4104 = c$1606->$0;
          moonbit_decref(c$1606);
          val$3990 = _field$4104;
          _tmp$3989 = val$3990;
          moonbit_incref(res$1603);
          $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
            res$1603, _tmp$3989
          );
          val$3992 = i$1605->$0;
          _tmp$3991 = val$3992 + 2;
          i$1605->$0 = _tmp$3991;
        } else {
          int32_t val$3993 = c$1606->$0;
          if (val$3993 < 240) {
            int32_t val$3995 = i$1605->$0;
            int32_t _tmp$3994 = val$3995 + 2;
            int32_t val$4011;
            int32_t _tmp$4010;
            int32_t _tmp$4003;
            int32_t val$4009;
            int32_t _tmp$4008;
            int32_t _tmp$4007;
            int32_t _tmp$4006;
            int32_t _tmp$4005;
            int32_t _tmp$4004;
            int32_t _tmp$3997;
            int32_t val$4002;
            int32_t _tmp$4001;
            int32_t _tmp$4000;
            int32_t _tmp$3999;
            int32_t _tmp$3998;
            int32_t _tmp$3996;
            int32_t _field$4105;
            int32_t val$4013;
            int32_t _tmp$4012;
            int32_t val$4015;
            int32_t _tmp$4014;
            if (_tmp$3994 >= len$1604) {
              moonbit_decref(c$1606);
              moonbit_decref(i$1605);
              moonbit_decref(bytes$1602);
              break;
            }
            val$4011 = c$1606->$0;
            _tmp$4010 = val$4011 & 15;
            _tmp$4003 = _tmp$4010 << 12;
            val$4009 = i$1605->$0;
            _tmp$4008 = val$4009 + 1;
            if (
              _tmp$4008 < 0 || _tmp$4008 >= Moonbit_array_length(bytes$1602)
            ) {
              moonbit_panic();
            }
            _tmp$4007 = bytes$1602[_tmp$4008];
            _tmp$4006 = (int32_t)_tmp$4007;
            _tmp$4005 = _tmp$4006 & 63;
            _tmp$4004 = _tmp$4005 << 6;
            _tmp$3997 = _tmp$4003 | _tmp$4004;
            val$4002 = i$1605->$0;
            _tmp$4001 = val$4002 + 2;
            if (
              _tmp$4001 < 0 || _tmp$4001 >= Moonbit_array_length(bytes$1602)
            ) {
              moonbit_panic();
            }
            _tmp$4000 = bytes$1602[_tmp$4001];
            _tmp$3999 = (int32_t)_tmp$4000;
            _tmp$3998 = _tmp$3999 & 63;
            _tmp$3996 = _tmp$3997 | _tmp$3998;
            c$1606->$0 = _tmp$3996;
            _field$4105 = c$1606->$0;
            moonbit_decref(c$1606);
            val$4013 = _field$4105;
            _tmp$4012 = val$4013;
            moonbit_incref(res$1603);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1603, _tmp$4012
            );
            val$4015 = i$1605->$0;
            _tmp$4014 = val$4015 + 3;
            i$1605->$0 = _tmp$4014;
          } else {
            int32_t val$4017 = i$1605->$0;
            int32_t _tmp$4016 = val$4017 + 3;
            int32_t val$4040;
            int32_t _tmp$4039;
            int32_t _tmp$4032;
            int32_t val$4038;
            int32_t _tmp$4037;
            int32_t _tmp$4036;
            int32_t _tmp$4035;
            int32_t _tmp$4034;
            int32_t _tmp$4033;
            int32_t _tmp$4025;
            int32_t val$4031;
            int32_t _tmp$4030;
            int32_t _tmp$4029;
            int32_t _tmp$4028;
            int32_t _tmp$4027;
            int32_t _tmp$4026;
            int32_t _tmp$4019;
            int32_t val$4024;
            int32_t _tmp$4023;
            int32_t _tmp$4022;
            int32_t _tmp$4021;
            int32_t _tmp$4020;
            int32_t _tmp$4018;
            int32_t val$4042;
            int32_t _tmp$4041;
            int32_t val$4046;
            int32_t _tmp$4045;
            int32_t _tmp$4044;
            int32_t _tmp$4043;
            int32_t _field$4106;
            int32_t val$4050;
            int32_t _tmp$4049;
            int32_t _tmp$4048;
            int32_t _tmp$4047;
            int32_t val$4052;
            int32_t _tmp$4051;
            if (_tmp$4016 >= len$1604) {
              moonbit_decref(c$1606);
              moonbit_decref(i$1605);
              moonbit_decref(bytes$1602);
              break;
            }
            val$4040 = c$1606->$0;
            _tmp$4039 = val$4040 & 7;
            _tmp$4032 = _tmp$4039 << 18;
            val$4038 = i$1605->$0;
            _tmp$4037 = val$4038 + 1;
            if (
              _tmp$4037 < 0 || _tmp$4037 >= Moonbit_array_length(bytes$1602)
            ) {
              moonbit_panic();
            }
            _tmp$4036 = bytes$1602[_tmp$4037];
            _tmp$4035 = (int32_t)_tmp$4036;
            _tmp$4034 = _tmp$4035 & 63;
            _tmp$4033 = _tmp$4034 << 12;
            _tmp$4025 = _tmp$4032 | _tmp$4033;
            val$4031 = i$1605->$0;
            _tmp$4030 = val$4031 + 2;
            if (
              _tmp$4030 < 0 || _tmp$4030 >= Moonbit_array_length(bytes$1602)
            ) {
              moonbit_panic();
            }
            _tmp$4029 = bytes$1602[_tmp$4030];
            _tmp$4028 = (int32_t)_tmp$4029;
            _tmp$4027 = _tmp$4028 & 63;
            _tmp$4026 = _tmp$4027 << 6;
            _tmp$4019 = _tmp$4025 | _tmp$4026;
            val$4024 = i$1605->$0;
            _tmp$4023 = val$4024 + 3;
            if (
              _tmp$4023 < 0 || _tmp$4023 >= Moonbit_array_length(bytes$1602)
            ) {
              moonbit_panic();
            }
            _tmp$4022 = bytes$1602[_tmp$4023];
            _tmp$4021 = (int32_t)_tmp$4022;
            _tmp$4020 = _tmp$4021 & 63;
            _tmp$4018 = _tmp$4019 | _tmp$4020;
            c$1606->$0 = _tmp$4018;
            val$4042 = c$1606->$0;
            _tmp$4041 = val$4042 - 65536;
            c$1606->$0 = _tmp$4041;
            val$4046 = c$1606->$0;
            _tmp$4045 = val$4046 >> 10;
            _tmp$4044 = _tmp$4045 + 55296;
            _tmp$4043 = _tmp$4044;
            moonbit_incref(res$1603);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1603, _tmp$4043
            );
            _field$4106 = c$1606->$0;
            moonbit_decref(c$1606);
            val$4050 = _field$4106;
            _tmp$4049 = val$4050 & 1023;
            _tmp$4048 = _tmp$4049 + 56320;
            _tmp$4047 = _tmp$4048;
            moonbit_incref(res$1603);
            $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
              res$1603, _tmp$4047
            );
            val$4052 = i$1605->$0;
            _tmp$4051 = val$4052 + 4;
            i$1605->$0 = _tmp$4051;
          }
        }
      }
      continue;
    } else {
      moonbit_decref(i$1605);
      moonbit_decref(bytes$1602);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(res$1603);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args$moonbit_test_driver_internal_parse_int_$fn$14(
  int32_t _env$3963,
  moonbit_string_t s$1596
) {
  struct $Ref$3c$Int$3e$* res$1597 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  int32_t len$1598;
  int32_t i$1599;
  int32_t _field$4107;
  Moonbit_object_header(res$1597)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  res$1597->$0 = 0;
  len$1598 = Moonbit_array_length(s$1596);
  i$1599 = 0;
  while (1) {
    if (i$1599 < len$1598) {
      int32_t val$3968 = res$1597->$0;
      int32_t _tmp$3965 = val$3968 * 10;
      int32_t _tmp$3967;
      int32_t _tmp$3966;
      int32_t _tmp$3964;
      int32_t _tmp$3969;
      if (i$1599 < 0 || i$1599 >= Moonbit_array_length(s$1596)) {
        moonbit_panic();
      }
      _tmp$3967 = s$1596[i$1599];
      _tmp$3966 = _tmp$3967 - 48;
      _tmp$3964 = _tmp$3965 + _tmp$3966;
      res$1597->$0 = _tmp$3964;
      _tmp$3969 = i$1599 + 1;
      i$1599 = _tmp$3969;
      continue;
    } else {
      moonbit_decref(s$1596);
    }
    break;
  }
  _field$4107 = res$1597->$0;
  moonbit_decref(res$1597);
  return _field$4107;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1559,
  moonbit_string_t filename$1520,
  int32_t index$1521
) {
  struct $Moonbit_Test_Driver_Internal__TestCase* filtered_test$1519;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _closure$4566;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1522;
  struct $Moonbit_Test_Driver_Internal__TestCase* item$1531;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$4117;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$3962;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$4116;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* attrs$1532;
  struct $Moonbit_Test_Driver_Internal_Meta* _field$4115;
  struct $Moonbit_Test_Driver_Internal_Meta* meta$3961;
  moonbit_string_t _field$4114;
  moonbit_string_t file_name$1533;
  moonbit_string_t name$1534;
  int32_t _tmp$3958;
  moonbit_string_t name$1535;
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _tmp$3915;
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$3916;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _closure$4568;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1542;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1558;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1583;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1585;
  void* _field$4111;
  int32_t _cnt$4431;
  void* _bind$1586;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _closure$4572;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$3955;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _closure$4573;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$3948;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _closure$4574;
  struct $$3c$$3e$$3d$$3e$Unit* _tmp$3949;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _closure$4575;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3933;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_with_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_no_args_tests
  );
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_async_tests
  );
  filtered_test$1519
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_apply_filter(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_no_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_with_args_tests,
      $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_async_tests,
      filename$1520,
      index$1521
  );
  _closure$4566
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
      )
    );
  Moonbit_object_header(_closure$4566)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap
    )
    >> 2,
      0,
      0
  );
  _closure$4566->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4;
  _closure$4566->$0 = index$1521;
  handle_result$1522
  = (struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit*)_closure$4566;
  if (filtered_test$1519 == 0) {
    moonbit_decref(async_tests$1559);
    if (filtered_test$1519) {
      moonbit_decref(filtered_test$1519);
    }
    $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1522,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_3.data,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
  } else {
    struct $Moonbit_Test_Driver_Internal__TestCase* _Some$1593 =
      filtered_test$1519;
    struct $Moonbit_Test_Driver_Internal__TestCase* _item$1594 = _Some$1593;
    item$1531 = _item$1594;
    goto $join$1530;
  }
  goto $joinlet$4567;
  $join$1530:;
  _field$4117 = item$1531->$1;
  meta$3962 = _field$4117;
  _field$4116 = meta$3962->$2;
  attrs$1532 = _field$4116;
  _field$4115 = item$1531->$1;
  meta$3961 = _field$4115;
  _field$4114 = meta$3961->$0;
  file_name$1533 = _field$4114;
  moonbit_incref(attrs$1532);
  moonbit_incref(file_name$1533);
  moonbit_incref(attrs$1532);
  if ($$moonbitlang$core$builtin$Array$$is_empty$0(attrs$1532)) {
    name$1534 = (moonbit_string_t)moonbit_string_literal_3.data;
  } else {
    moonbit_incref(attrs$1532);
    name$1534 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1532, 0);
  }
  _tmp$3958 = Moonbit_array_length(name$1534);
  if (_tmp$3958 == 0) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$4113;
    struct $Moonbit_Test_Driver_Internal_Meta* meta$3960;
    int32_t _field$4112;
    int32_t index$3959;
    moonbit_decref(name$1534);
    _field$4113 = item$1531->$1;
    meta$3960 = _field$4113;
    _field$4112 = meta$3960->$1;
    index$3959 = _field$4112;
    name$1535 = $Int$$to_string$inner(index$3959, 10);
  } else {
    name$1535 = name$1534;
  }
  moonbit_incref(attrs$1532);
  _tmp$3915 = $$moonbitlang$core$builtin$Array$$iter$0(attrs$1532);
  _tmp$3916
  = (struct $$3c$String$3e$$3d$$3e$Int*)&$$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5$closure.data;
  if ($$moonbitlang$core$builtin$Iter$$any$0(_tmp$3915, _tmp$3916)) {
    moonbit_string_t _tmp$3930;
    moonbit_string_t _tmp$3929;
    moonbit_string_t _tmp$3926;
    moonbit_string_t _tmp$3928;
    moonbit_string_t _tmp$3927;
    moonbit_string_t _tmp$3925;
    moonbit_decref(async_tests$1559);
    moonbit_decref(item$1531);
    moonbit_incref(file_name$1533);
    _tmp$3930
    = $$moonbitlang$core$builtin$Show$$String$$to_string(
      file_name$1533
    );
    _tmp$3929
    = moonbit_add_string(
      (moonbit_string_t)moonbit_string_literal_5.data, _tmp$3930
    );
    _tmp$3926
    = moonbit_add_string(
      _tmp$3929, (moonbit_string_t)moonbit_string_literal_6.data
    );
    _tmp$3928 = $$moonbitlang$core$builtin$Array$$at$0(attrs$1532, 0);
    _tmp$3927 = $$moonbitlang$core$builtin$Show$$String$$to_string(_tmp$3928);
    _tmp$3925 = moonbit_add_string(_tmp$3926, _tmp$3927);
    $moonbitlang$core$builtin$println$0(_tmp$3925);
    $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
      handle_result$1522,
        name$1535,
        file_name$1533,
        (moonbit_string_t)moonbit_string_literal_4.data,
        1
    );
    return 0;
  } else {
    moonbit_decref(attrs$1532);
  }
  moonbit_incref(name$1535);
  moonbit_incref(file_name$1533);
  moonbit_incref(handle_result$1522);
  _closure$4568
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap
      )
    );
  Moonbit_object_header(_closure$4568)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$4568->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6;
  _closure$4568->$0 = name$1535;
  _closure$4568->$1 = file_name$1533;
  _closure$4568->$2 = handle_result$1522;
  on_err$1542 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$4568;
  _field$4111 = item$1531->$0;
  _cnt$4431 = Moonbit_object_header(item$1531)->rc;
  if (_cnt$4431 > 1) {
    int32_t _new_cnt$4433;
    moonbit_incref(_field$4111);
    _new_cnt$4433 = _cnt$4431 - 1;
    Moonbit_object_header(item$1531)->rc = _new_cnt$4433;
  } else if (_cnt$4431 == 1) {
    struct $Moonbit_Test_Driver_Internal_Meta* _field$4432 = item$1531->$1;
    moonbit_decref(_field$4432);
    moonbit_free(item$1531);
  }
  _bind$1586 = _field$4111;
  switch (Moonbit_object_tag(_bind$1586)) {
    case 0: {
      struct $Moonbit_Test_Driver_Internal__F$F0* _F0$1587;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$4108;
      int32_t _cnt$4434;
      struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1588;
      moonbit_decref(async_tests$1559);
      _F0$1587 = (struct $Moonbit_Test_Driver_Internal__F$F0*)_bind$1586;
      _field$4108 = _F0$1587->$0;
      _cnt$4434 = Moonbit_object_header(_F0$1587)->rc;
      if (_cnt$4434 > 1) {
        int32_t _new_cnt$4435;
        moonbit_incref(_field$4108);
        _new_cnt$4435 = _cnt$4434 - 1;
        Moonbit_object_header(_F0$1587)->rc = _new_cnt$4435;
      } else if (_cnt$4434 == 1) {
        moonbit_free(_F0$1587);
      }
      _f$1588 = _field$4108;
      f$1585 = _f$1588;
      goto $join$1584;
      break;
    }
    
    case 1: {
      struct $Moonbit_Test_Driver_Internal__F$F1* _F1$1589;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$4109;
      int32_t _cnt$4436;
      struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _f$1590;
      moonbit_decref(async_tests$1559);
      _F1$1589 = (struct $Moonbit_Test_Driver_Internal__F$F1*)_bind$1586;
      _field$4109 = _F1$1589->$0;
      _cnt$4436 = Moonbit_object_header(_F1$1589)->rc;
      if (_cnt$4436 > 1) {
        int32_t _new_cnt$4437;
        moonbit_incref(_field$4109);
        _new_cnt$4437 = _cnt$4436 - 1;
        Moonbit_object_header(_F1$1589)->rc = _new_cnt$4437;
      } else if (_cnt$4436 == 1) {
        moonbit_free(_F1$1589);
      }
      _f$1590 = _field$4109;
      f$1583 = _f$1590;
      goto $join$1582;
      break;
    }
    default: {
      struct $Moonbit_Test_Driver_Internal__F$F2* _F2$1591 =
        (struct $Moonbit_Test_Driver_Internal__F$F2*)_bind$1586;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$4110 =
        _F2$1591->$0;
      int32_t _cnt$4438 = Moonbit_object_header(_F2$1591)->rc;
      struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _f$1592;
      if (_cnt$4438 > 1) {
        int32_t _new_cnt$4439;
        moonbit_incref(_field$4110);
        _new_cnt$4439 = _cnt$4438 - 1;
        Moonbit_object_header(_F2$1591)->rc = _new_cnt$4439;
      } else if (_cnt$4438 == 1) {
        moonbit_free(_F2$1591);
      }
      _f$1592 = _field$4110;
      f$1558 = _f$1592;
      goto $join$1557;
      break;
    }
  }
  goto $joinlet$4571;
  $join$1584:;
  _closure$4572
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap
      )
    );
  Moonbit_object_header(_closure$4572)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$4572->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13;
  _closure$4572->$0 = name$1535;
  _closure$4572->$1 = file_name$1533;
  _closure$4572->$2 = handle_result$1522;
  _tmp$3955 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$4572;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_catch_error(
    f$1585, _tmp$3955, on_err$1542
  );
  $joinlet$4571:;
  goto $joinlet$4570;
  $join$1582:;
  moonbit_incref(name$1535);
  _closure$4573
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap
      )
    );
  Moonbit_object_header(_closure$4573)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap,
        $0
    )
    >> 2,
      2,
      0
  );
  _closure$4573->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12;
  _closure$4573->$0 = f$1583;
  _closure$4573->$1 = name$1535;
  _tmp$3948
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)_closure$4573;
  _closure$4574
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap
      )
    );
  Moonbit_object_header(_closure$4574)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap,
        $0
    )
    >> 2,
      3,
      0
  );
  _closure$4574->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11;
  _closure$4574->$0 = name$1535;
  _closure$4574->$1 = file_name$1533;
  _closure$4574->$2 = handle_result$1522;
  _tmp$3949 = (struct $$3c$$3e$$3d$$3e$Unit*)_closure$4574;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_catch_error(
    _tmp$3948, _tmp$3949, on_err$1542
  );
  $joinlet$4570:;
  goto $joinlet$4569;
  $join$1557:;
  _closure$4575
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap
      )
    );
  Moonbit_object_header(_closure$4575)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap,
        $0
    )
    >> 2,
      5,
      0
  );
  _closure$4575->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7;
  _closure$4575->$0 = f$1558;
  _closure$4575->$1 = on_err$1542;
  _closure$4575->$2 = name$1535;
  _closure$4575->$3 = file_name$1533;
  _closure$4575->$4 = handle_result$1522;
  _tmp$3933
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)_closure$4575;
  $$moonbitlang$core$builtin$Array$$push$2(async_tests$1559, _tmp$3933);
  $joinlet$4569:;
  $joinlet$4567:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3956
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap* _casted_env$3957 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$13$2d$cap*)_env$3956;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$4120 =
    _casted_env$3957->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1522 =
    _field$4120;
  moonbit_string_t _field$4119 = _casted_env$3957->$1;
  moonbit_string_t file_name$1533 = _field$4119;
  moonbit_string_t _field$4118 = _casted_env$3957->$0;
  int32_t _cnt$4440 = Moonbit_object_header(_casted_env$3957)->rc;
  moonbit_string_t name$1535;
  if (_cnt$4440 > 1) {
    int32_t _new_cnt$4441;
    moonbit_incref(handle_result$1522);
    moonbit_incref(file_name$1533);
    moonbit_incref(_field$4118);
    _new_cnt$4441 = _cnt$4440 - 1;
    Moonbit_object_header(_casted_env$3957)->rc = _new_cnt$4441;
  } else if (_cnt$4440 == 1) {
    moonbit_free(_casted_env$3957);
  }
  name$1535 = _field$4118;
  $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1522,
      name$1535,
      file_name$1533,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

struct moonbit_result_0 $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _env$3952
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap* _casted_env$3953 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$12$2d$cap*)_env$3952;
  moonbit_string_t _field$4122 = _casted_env$3953->$1;
  moonbit_string_t name$1535 = _field$4122;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$4121 =
    _casted_env$3953->$0;
  int32_t _cnt$4442 = Moonbit_object_header(_casted_env$3953)->rc;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1583;
  int32_t _tmp$3954;
  if (_cnt$4442 > 1) {
    int32_t _new_cnt$4443;
    moonbit_incref(name$1535);
    moonbit_incref(_field$4121);
    _new_cnt$4443 = _cnt$4442 - 1;
    Moonbit_object_header(_casted_env$3953)->rc = _new_cnt$4443;
  } else if (_cnt$4442 == 1) {
    moonbit_free(_casted_env$3953);
  }
  f$1583 = _field$4121;
  _tmp$3954
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$1535
  );
  return f$1583->code(f$1583, _tmp$3954);
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11(
  struct $$3c$$3e$$3d$$3e$Unit* _env$3950
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap* _casted_env$3951 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$11$2d$cap*)_env$3950;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$4125 =
    _casted_env$3951->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1522 =
    _field$4125;
  moonbit_string_t _field$4124 = _casted_env$3951->$1;
  moonbit_string_t file_name$1533 = _field$4124;
  moonbit_string_t _field$4123 = _casted_env$3951->$0;
  int32_t _cnt$4444 = Moonbit_object_header(_casted_env$3951)->rc;
  moonbit_string_t name$1535;
  if (_cnt$4444 > 1) {
    int32_t _new_cnt$4445;
    moonbit_incref(handle_result$1522);
    moonbit_incref(file_name$1533);
    moonbit_incref(_field$4123);
    _new_cnt$4445 = _cnt$4444 - 1;
    Moonbit_object_header(_casted_env$3951)->rc = _new_cnt$4445;
  } else if (_cnt$4444 == 1) {
    moonbit_free(_casted_env$3951);
  }
  name$1535 = _field$4123;
  $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1522,
      name$1535,
      file_name$1533,
      (moonbit_string_t)moonbit_string_literal_3.data,
      0
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _env$3934,
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1560,
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1561
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap* _casted_env$3935 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$7$2d$cap*)_env$3934;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$4130 =
    _casted_env$3935->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1522 =
    _field$4130;
  moonbit_string_t _field$4129 = _casted_env$3935->$3;
  moonbit_string_t file_name$1533 = _field$4129;
  moonbit_string_t _field$4128 = _casted_env$3935->$2;
  moonbit_string_t name$1535 = _field$4128;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$4127 = _casted_env$3935->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1542 = _field$4127;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$4126 =
    _casted_env$3935->$0;
  int32_t _cnt$4446 = Moonbit_object_header(_casted_env$3935)->rc;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* f$1558;
  int32_t _async_driver$1562;
  int32_t _tmp$3939;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _closure$4576;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _tmp$3940;
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _closure$4577;
  struct $$3c$Error$3e$$3d$$3e$Unit* _tmp$3941;
  if (_cnt$4446 > 1) {
    int32_t _new_cnt$4447;
    moonbit_incref(handle_result$1522);
    moonbit_incref(file_name$1533);
    moonbit_incref(name$1535);
    moonbit_incref(on_err$1542);
    moonbit_incref(_field$4126);
    _new_cnt$4447 = _cnt$4446 - 1;
    Moonbit_object_header(_casted_env$3935)->rc = _new_cnt$4447;
  } else if (_cnt$4446 == 1) {
    moonbit_free(_casted_env$3935);
  }
  f$1558 = _field$4126;
  _async_driver$1562 = 0;
  moonbit_incref(name$1535);
  _tmp$3939
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_new_test_arg(
    name$1535
  );
  moonbit_incref(_cont$1560);
  _closure$4576
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap
      )
    );
  Moonbit_object_header(_closure$4576)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap,
        $1
    )
    >> 2,
      4,
      0
  );
  _closure$4576->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10;
  _closure$4576->$0 = _async_driver$1562;
  _closure$4576->$1 = _cont$1560;
  _closure$4576->$2 = name$1535;
  _closure$4576->$3 = file_name$1533;
  _closure$4576->$4 = handle_result$1522;
  _tmp$3940 = (struct $$3c$Unit$3e$$3d$$3e$Unit*)_closure$4576;
  _closure$4577
  = (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap
      )
    );
  Moonbit_object_header(_closure$4577)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap,
        $1
    )
    >> 2,
      3,
      0
  );
  _closure$4577->code
  = &$$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9;
  _closure$4577->$0 = _async_driver$1562;
  _closure$4577->$1 = _err_cont$1561;
  _closure$4577->$2 = _cont$1560;
  _closure$4577->$3 = on_err$1542;
  _tmp$3941 = (struct $$3c$Error$3e$$3d$$3e$Unit*)_closure$4577;
  f$1558->code(f$1558, _tmp$3939, _tmp$3940, _tmp$3941);
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10(
  struct $$3c$Unit$3e$$3d$$3e$Unit* _env$3945,
  int32_t _cont_param$1580
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap* _casted_env$3946 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$10$2d$cap*)_env$3945;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$4135 =
    _casted_env$3946->$4;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1522 =
    _field$4135;
  moonbit_string_t _field$4134 = _casted_env$3946->$3;
  moonbit_string_t file_name$1533 = _field$4134;
  moonbit_string_t _field$4133 = _casted_env$3946->$2;
  moonbit_string_t name$1535 = _field$4133;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$4132 = _casted_env$3946->$1;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1560 = _field$4132;
  int32_t _field$4131 = _casted_env$3946->$0;
  int32_t _cnt$4448 = Moonbit_object_header(_casted_env$3946)->rc;
  int32_t _async_driver$1562;
  void* State_1$3947;
  if (_cnt$4448 > 1) {
    int32_t _new_cnt$4449;
    moonbit_incref(handle_result$1522);
    moonbit_incref(file_name$1533);
    moonbit_incref(name$1535);
    moonbit_incref(_cont$1560);
    _new_cnt$4449 = _cnt$4448 - 1;
    Moonbit_object_header(_casted_env$3946)->rc = _new_cnt$4449;
  } else if (_cnt$4448 == 1) {
    moonbit_free(_casted_env$3946);
  }
  _async_driver$1562 = _field$4131;
  State_1$3947
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1
      )
    );
  Moonbit_object_header(State_1$3947)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1,
        $1
    )
    >> 2,
      4,
      1
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1*)State_1$3947)->$0
  = _cont_param$1580;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1*)State_1$3947)->$1
  = handle_result$1522;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1*)State_1$3947)->$2
  = file_name$1533;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1*)State_1$3947)->$3
  = name$1535;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1*)State_1$3947)->$4
  = _cont$1560;
  $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1562, State_1$3947
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3942,
  void* _cont_param$1581
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap* _casted_env$3943 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$9$2d$cap*)_env$3942;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$4139 = _casted_env$3943->$3;
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1542 = _field$4139;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _field$4138 = _casted_env$3943->$2;
  struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1560 = _field$4138;
  struct $$3c$Error$3e$$3d$$3e$Unit* _field$4137 = _casted_env$3943->$1;
  struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1561 = _field$4137;
  int32_t _field$4136 = _casted_env$3943->$0;
  int32_t _cnt$4450 = Moonbit_object_header(_casted_env$3943)->rc;
  int32_t _async_driver$1562;
  void* _try$312$3944;
  if (_cnt$4450 > 1) {
    int32_t _new_cnt$4451;
    moonbit_incref(on_err$1542);
    moonbit_incref(_cont$1560);
    moonbit_incref(_err_cont$1561);
    _new_cnt$4451 = _cnt$4450 - 1;
    Moonbit_object_header(_casted_env$3943)->rc = _new_cnt$4451;
  } else if (_cnt$4450 == 1) {
    moonbit_free(_casted_env$3943);
  }
  _async_driver$1562 = _field$4136;
  _try$312$3944
  = (void*)moonbit_malloc(
      sizeof(
        struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312
      )
    );
  Moonbit_object_header(_try$312$3944)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312,
        $0
    )
    >> 2,
      4,
      0
  );
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312*)_try$312$3944)->$0
  = _cont_param$1581;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312*)_try$312$3944)->$1
  = on_err$1542;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312*)_try$312$3944)->$2
  = _cont$1560;
  ((struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312*)_try$312$3944)->$3
  = _err_cont$1561;
  $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
    _async_driver$1562, _try$312$3944
  );
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$$2a$async_driver$fn$8(
  int32_t _env$3936,
  void* _state$1563
) {
  switch (Moonbit_object_tag(_state$1563)) {
    case 0: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312* _$2a$try$312$1564 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$$2a$try$312*)_state$1563;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$4143 = _$2a$try$312$1564->$3;
      struct $$3c$Error$3e$$3d$$3e$Unit* _err_cont$1565 = _field$4143;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$4142 = _$2a$try$312$1564->$2;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1566 = _field$4142;
      struct $$3c$Error$3e$$3d$$3e$Unit* _field$4141 = _$2a$try$312$1564->$1;
      struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1567 = _field$4141;
      void* _field$4140 = _$2a$try$312$1564->$0;
      int32_t _cnt$4452 = Moonbit_object_header(_$2a$try$312$1564)->rc;
      void* _try_err$1568;
      void* err$1570;
      void* err$1572;
      int32_t _tmp$3938;
      if (_cnt$4452 > 1) {
        int32_t _new_cnt$4453;
        moonbit_incref(_err_cont$1565);
        moonbit_incref(_cont$1566);
        moonbit_incref(on_err$1567);
        moonbit_incref(_field$4140);
        _new_cnt$4453 = _cnt$4452 - 1;
        Moonbit_object_header(_$2a$try$312$1564)->rc = _new_cnt$4453;
      } else if (_cnt$4452 == 1) {
        moonbit_free(_$2a$try$312$1564);
      }
      _try_err$1568 = _field$4140;
      if (
        $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_is_being_cancelled()
      ) {
        moonbit_decref(on_err$1567);
        moonbit_decref(_cont$1566);
        err$1572 = _try_err$1568;
        goto $join$1571;
      } else {
        moonbit_decref(_err_cont$1565);
        err$1570 = _try_err$1568;
        goto $join$1569;
      }
      goto $joinlet$4579;
      $join$1571:;
      return _err_cont$1565->code(_err_cont$1565, err$1572);
      $joinlet$4579:;
      goto $joinlet$4578;
      $join$1569:;
      _tmp$3938 = on_err$1567->code(on_err$1567, err$1570);
      _cont$1566->code(_cont$1566, _tmp$3938);
      $joinlet$4578:;
      break;
    }
    default: {
      struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1* _State_1$1573 =
        (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$54$$2a$arm$328$on_err$68$$2a$arm$320$lambda$346$State$State_1*)_state$1563;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _field$4147 = _State_1$1573->$4;
      struct $$3c$Unit$3e$$3d$$3e$Unit* _cont$1574 = _field$4147;
      moonbit_string_t _field$4146 = _State_1$1573->$3;
      moonbit_string_t name$1575 = _field$4146;
      moonbit_string_t _field$4145 = _State_1$1573->$2;
      moonbit_string_t file_name$1576 = _field$4145;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$4144 =
        _State_1$1573->$1;
      int32_t _cnt$4454 = Moonbit_object_header(_State_1$1573)->rc;
      struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1577;
      int32_t _tmp$3937;
      if (_cnt$4454 > 1) {
        int32_t _new_cnt$4455;
        moonbit_incref(_cont$1574);
        moonbit_incref(name$1575);
        moonbit_incref(file_name$1576);
        moonbit_incref(_field$4144);
        _new_cnt$4455 = _cnt$4454 - 1;
        Moonbit_object_header(_State_1$1573)->rc = _new_cnt$4455;
      } else if (_cnt$4454 == 1) {
        moonbit_free(_State_1$1573);
      }
      handle_result$1577 = _field$4144;
      _tmp$3937
      = handle_result$1577->code(
        handle_result$1577,
          name$1575,
          file_name$1576,
          (moonbit_string_t)moonbit_string_literal_3.data,
          0
      );
      _cont$1574->code(_cont$1574, _tmp$3937);
      break;
    }
  }
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6(
  struct $$3c$Error$3e$$3d$$3e$Unit* _env$3931,
  void* err$1543
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap* _casted_env$3932 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$on_err$fn$6$2d$cap*)_env$3931;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _field$4154 =
    _casted_env$3932->$2;
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* handle_result$1522 =
    _field$4154;
  moonbit_string_t _field$4153 = _casted_env$3932->$1;
  moonbit_string_t file_name$1533 = _field$4153;
  moonbit_string_t _field$4152 = _casted_env$3932->$0;
  int32_t _cnt$4456 = Moonbit_object_header(_casted_env$3932)->rc;
  moonbit_string_t name$1535;
  void* e$1545;
  moonbit_string_t e$1548;
  moonbit_string_t message$1546;
  if (_cnt$4456 > 1) {
    int32_t _new_cnt$4457;
    moonbit_incref(handle_result$1522);
    moonbit_incref(file_name$1533);
    moonbit_incref(_field$4152);
    _new_cnt$4457 = _cnt$4456 - 1;
    Moonbit_object_header(_casted_env$3932)->rc = _new_cnt$4457;
  } else if (_cnt$4456 == 1) {
    moonbit_free(_casted_env$3932);
  }
  name$1535 = _field$4152;
  switch (Moonbit_object_tag(err$1543)) {
    case 2: {
      struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$1549 =
        (struct $Error$moonbitlang$core$builtin$Failure$Failure*)err$1543;
      moonbit_string_t _field$4148 = _Failure$1549->$0;
      int32_t _cnt$4458 = Moonbit_object_header(_Failure$1549)->rc;
      moonbit_string_t _e$1550;
      if (_cnt$4458 > 1) {
        int32_t _new_cnt$4459;
        moonbit_incref(_field$4148);
        _new_cnt$4459 = _cnt$4458 - 1;
        Moonbit_object_header(_Failure$1549)->rc = _new_cnt$4459;
      } else if (_cnt$4458 == 1) {
        moonbit_free(_Failure$1549);
      }
      _e$1550 = _field$4148;
      e$1548 = _e$1550;
      goto $join$1547;
      break;
    }
    
    case 3: {
      struct $Error$moonbitlang$core$builtin$InspectError$InspectError* _InspectError$1551 =
        (struct $Error$moonbitlang$core$builtin$InspectError$InspectError*)err$1543;
      moonbit_string_t _field$4149 = _InspectError$1551->$0;
      int32_t _cnt$4460 = Moonbit_object_header(_InspectError$1551)->rc;
      moonbit_string_t _e$1552;
      if (_cnt$4460 > 1) {
        int32_t _new_cnt$4461;
        moonbit_incref(_field$4149);
        _new_cnt$4461 = _cnt$4460 - 1;
        Moonbit_object_header(_InspectError$1551)->rc = _new_cnt$4461;
      } else if (_cnt$4460 == 1) {
        moonbit_free(_InspectError$1551);
      }
      _e$1552 = _field$4149;
      e$1548 = _e$1552;
      goto $join$1547;
      break;
    }
    
    case 4: {
      struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError* _SnapshotError$1553 =
        (struct $Error$moonbitlang$core$builtin$SnapshotError$SnapshotError*)err$1543;
      moonbit_string_t _field$4150 = _SnapshotError$1553->$0;
      int32_t _cnt$4462 = Moonbit_object_header(_SnapshotError$1553)->rc;
      moonbit_string_t _e$1554;
      if (_cnt$4462 > 1) {
        int32_t _new_cnt$4463;
        moonbit_incref(_field$4150);
        _new_cnt$4463 = _cnt$4462 - 1;
        Moonbit_object_header(_SnapshotError$1553)->rc = _new_cnt$4463;
      } else if (_cnt$4462 == 1) {
        moonbit_free(_SnapshotError$1553);
      }
      _e$1554 = _field$4150;
      e$1548 = _e$1554;
      goto $join$1547;
      break;
    }
    
    case 5: {
      struct $Error$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError* _MoonBitTestDriverInternalJsError$1555 =
        (struct $Error$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$MoonBitTestDriverInternalJsError$MoonBitTestDriverInternalJsError*)err$1543;
      moonbit_string_t _field$4151 =
        _MoonBitTestDriverInternalJsError$1555->$0;
      int32_t _cnt$4464 =
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1555)->rc;
      moonbit_string_t _e$1556;
      if (_cnt$4464 > 1) {
        int32_t _new_cnt$4465;
        moonbit_incref(_field$4151);
        _new_cnt$4465 = _cnt$4464 - 1;
        Moonbit_object_header(_MoonBitTestDriverInternalJsError$1555)->rc
        = _new_cnt$4465;
      } else if (_cnt$4464 == 1) {
        moonbit_free(_MoonBitTestDriverInternalJsError$1555);
      }
      _e$1556 = _field$4151;
      e$1548 = _e$1556;
      goto $join$1547;
      break;
    }
    default: {
      e$1545 = err$1543;
      goto $join$1544;
      break;
    }
  }
  goto $joinlet$4581;
  $join$1547:;
  $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1522, name$1535, file_name$1533, e$1548, 0
  );
  $joinlet$4581:;
  goto $joinlet$4580;
  $join$1544:;
  message$1546 = $Error$to_string(e$1545);
  $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
    handle_result$1522, name$1535, file_name$1533, message$1546, 0
  );
  $joinlet$4580:;
  return 0;
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$fn$5(
  struct $$3c$String$3e$$3d$$3e$Int* _env$3917,
  moonbit_string_t attr$1536
) {
  int32_t _tmp$3919;
  int64_t _tmp$3918;
  moonbit_decref(_env$3917);
  _tmp$3919 = Moonbit_array_length(attr$1536);
  _tmp$3918 = (int64_t)_tmp$3919;
  moonbit_incref(attr$1536);
  if ($String$$char_length_ge$inner(attr$1536, 5, 0, _tmp$3918)) {
    int32_t _tmp$3924 = attr$1536[0];
    int32_t _x$1537 = _tmp$3924;
    if (_x$1537 == 112) {
      int32_t _tmp$3923 = attr$1536[1];
      int32_t _x$1538 = _tmp$3923;
      if (_x$1538 == 97) {
        int32_t _tmp$3922 = attr$1536[2];
        int32_t _x$1539 = _tmp$3922;
        if (_x$1539 == 110) {
          int32_t _tmp$3921 = attr$1536[3];
          int32_t _x$1540 = _tmp$3921;
          if (_x$1540 == 105) {
            int32_t _tmp$4155 = attr$1536[4];
            int32_t _tmp$3920;
            int32_t _x$1541;
            moonbit_decref(attr$1536);
            _tmp$3920 = _tmp$4155;
            _x$1541 = _tmp$3920;
            return _x$1541 == 99 || 0;
          } else {
            moonbit_decref(attr$1536);
            return 0;
          }
        } else {
          moonbit_decref(attr$1536);
          return 0;
        }
      } else {
        moonbit_decref(attr$1536);
        return 0;
      }
    } else {
      moonbit_decref(attr$1536);
      return 0;
    }
  } else {
    moonbit_decref(attr$1536);
    return 0;
  }
}

int32_t $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4(
  struct $$3c$String$2a$String$2a$String$2a$Bool$3e$$3d$$3e$Unit* _env$3901,
  moonbit_string_t test_name$1523,
  moonbit_string_t file_name$1524,
  moonbit_string_t message$1525,
  int32_t skipped$1526
) {
  struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap* _casted_env$3902 =
    (struct $$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute$handle_result$fn$4$2d$cap*)_env$3901;
  int32_t _field$4156 = _casted_env$3902->$0;
  int32_t index$1521;
  int32_t _if_result$4582;
  moonbit_string_t file_name$1527;
  moonbit_string_t test_name$1528;
  moonbit_string_t message$1529;
  moonbit_string_t _tmp$3914;
  moonbit_string_t _tmp$3913;
  moonbit_string_t _tmp$3911;
  moonbit_string_t _tmp$3912;
  moonbit_string_t _tmp$3910;
  moonbit_string_t _tmp$3908;
  moonbit_string_t _tmp$3909;
  moonbit_string_t _tmp$3907;
  moonbit_string_t _tmp$3905;
  moonbit_string_t _tmp$3906;
  moonbit_string_t _tmp$3904;
  moonbit_string_t _tmp$3903;
  moonbit_decref(_casted_env$3902);
  index$1521 = _field$4156;
  if (!skipped$1526) {
    _if_result$4582 = 1;
  } else {
    _if_result$4582 = 0;
  }
  if (_if_result$4582) {
    
  }
  file_name$1527 = $String$$escape(file_name$1524);
  test_name$1528 = $String$$escape(test_name$1523);
  message$1529 = $String$$escape(message$1525);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_7.data
  );
  _tmp$3914
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    file_name$1527
  );
  _tmp$3913
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_8.data, _tmp$3914
  );
  _tmp$3911
  = moonbit_add_string(
    _tmp$3913, (moonbit_string_t)moonbit_string_literal_9.data
  );
  _tmp$3912
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
    index$1521
  );
  _tmp$3910 = moonbit_add_string(_tmp$3911, _tmp$3912);
  _tmp$3908
  = moonbit_add_string(
    _tmp$3910, (moonbit_string_t)moonbit_string_literal_10.data
  );
  _tmp$3909
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    test_name$1528
  );
  _tmp$3907 = moonbit_add_string(_tmp$3908, _tmp$3909);
  _tmp$3905
  = moonbit_add_string(
    _tmp$3907, (moonbit_string_t)moonbit_string_literal_11.data
  );
  _tmp$3906
  = $$moonbitlang$core$builtin$Show$$String$$to_string(
    message$1529
  );
  _tmp$3904 = moonbit_add_string(_tmp$3905, _tmp$3906);
  _tmp$3903
  = moonbit_add_string(
    _tmp$3904, (moonbit_string_t)moonbit_string_literal_12.data
  );
  $moonbitlang$core$builtin$println$0(_tmp$3903);
  $moonbitlang$core$builtin$println$0(
    (moonbit_string_t)moonbit_string_literal_13.data
  );
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_new_test_arg(
  moonbit_string_t _discard_$1518
) {
  moonbit_decref(_discard_$1518);
  return 42;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_is_being_cancelled(
  
) {
  return 0;
}

int32_t $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_catch_error(
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* f$1516,
  struct $$3c$$3e$$3d$$3e$Unit* on_ok$1517,
  struct $$3c$Error$3e$$3d$$3e$Unit* on_err$1514
) {
  void* _try_err$1512;
  struct moonbit_result_0 _tmp$4584 = f$1516->code(f$1516);
  void* err$1513;
  if (_tmp$4584.tag) {
    int32_t const _ok$3899 = _tmp$4584.data.ok;
    moonbit_decref(on_err$1514);
  } else {
    void* const _err$3900 = _tmp$4584.data.err;
    moonbit_decref(on_ok$1517);
    _try_err$1512 = _err$3900;
    goto $join$1511;
  }
  on_ok$1517->code(on_ok$1517);
  goto $joinlet$4583;
  $join$1511:;
  err$1513 = _try_err$1512;
  on_err$1514->code(on_err$1514, err$1513);
  $joinlet$4583:;
  return 0;
}

struct $Moonbit_Test_Driver_Internal__TestCase* $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_apply_filter(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* no_args_tests$1477,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* with_args_tests$1490,
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* async_tests$1503,
  moonbit_string_t file_filter$1474,
  int32_t index_filter$1475
) {
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1471;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1472;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1476;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$4162;
  struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$3890;
  void* F0$3887;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$4161;
  int32_t _cnt$4466;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$3889;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$3888;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1473;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1486;
  struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1487;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1489;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$4160;
  struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _tmp$3894;
  void* F1$3891;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$4159;
  int32_t _cnt$4469;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$3893;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$3892;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1488;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* index_func_map$1499;
  struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1500;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1502;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$4158;
  struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$3898;
  void* F2$3895;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _field$4157;
  int32_t _cnt$4472;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$3897;
  struct $Moonbit_Test_Driver_Internal_Meta* _tmp$3896;
  struct $Moonbit_Test_Driver_Internal__TestCase* k$1501;
  moonbit_incref(file_filter$1474);
  _bind$1476
  = $$moonbitlang$core$builtin$Map$$get$0(
    no_args_tests$1477, file_filter$1474
  );
  if (_bind$1476 == 0) {
    if (_bind$1476) {
      moonbit_decref(_bind$1476);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1478 =
      _bind$1476;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1479 =
      _Some$1478;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1481;
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1482;
    moonbit_incref(_index_func_map$1479);
    _bind$1482
    = $$moonbitlang$core$builtin$Map$$get$1(
      _index_func_map$1479, index_filter$1475
    );
    if (_bind$1482 == 0) {
      if (_bind$1482) {
        moonbit_decref(_bind$1482);
      }
      moonbit_decref(_index_func_map$1479);
    } else {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1483;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1484;
      moonbit_decref(async_tests$1503);
      moonbit_decref(with_args_tests$1490);
      _Some$1483 = _bind$1482;
      _func_attrs_tuple$1484 = _Some$1483;
      func_attrs_tuple$1481 = _func_attrs_tuple$1484;
      goto $join$1480;
    }
    goto $joinlet$4586;
    $join$1480:;
    index_func_map$1471 = _index_func_map$1479;
    func_attrs_tuple$1472 = func_attrs_tuple$1481;
    goto $join$1470;
    $joinlet$4586:;
  }
  goto $joinlet$4585;
  $join$1470:;
  moonbit_decref(index_func_map$1471);
  _field$4162 = func_attrs_tuple$1472->$0;
  _tmp$3890 = _field$4162;
  moonbit_incref(_tmp$3890);
  F0$3887
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F0));
  Moonbit_object_header(F0$3887)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F0, $0) >> 2, 1, 0
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F0*)F0$3887)->$0 = _tmp$3890;
  _field$4161 = func_attrs_tuple$1472->$1;
  _cnt$4466 = Moonbit_object_header(func_attrs_tuple$1472)->rc;
  if (_cnt$4466 > 1) {
    int32_t _new_cnt$4468;
    moonbit_incref(_field$4161);
    _new_cnt$4468 = _cnt$4466 - 1;
    Moonbit_object_header(func_attrs_tuple$1472)->rc = _new_cnt$4468;
  } else if (_cnt$4466 == 1) {
    struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$4467 =
      func_attrs_tuple$1472->$0;
    moonbit_decref(_field$4467);
    moonbit_free(func_attrs_tuple$1472);
  }
  _tmp$3889 = _field$4161;
  _tmp$3888
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$3888)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$3888->$0 = file_filter$1474;
  _tmp$3888->$1 = index_filter$1475;
  _tmp$3888->$2 = _tmp$3889;
  k$1473
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1473)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1473->$0 = F0$3887;
  k$1473->$1 = _tmp$3888;
  return k$1473;
  $joinlet$4585:;
  moonbit_incref(file_filter$1474);
  _bind$1489
  = $$moonbitlang$core$builtin$Map$$get$2(
    with_args_tests$1490, file_filter$1474
  );
  if (_bind$1489 == 0) {
    if (_bind$1489) {
      moonbit_decref(_bind$1489);
    }
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1491 =
      _bind$1489;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1492 =
      _Some$1491;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1494;
    struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1495;
    moonbit_incref(_index_func_map$1492);
    _bind$1495
    = $$moonbitlang$core$builtin$Map$$get$3(
      _index_func_map$1492, index_filter$1475
    );
    if (_bind$1495 == 0) {
      if (_bind$1495) {
        moonbit_decref(_bind$1495);
      }
      moonbit_decref(_index_func_map$1492);
    } else {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1496;
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1497;
      moonbit_decref(async_tests$1503);
      _Some$1496 = _bind$1495;
      _func_attrs_tuple$1497 = _Some$1496;
      func_attrs_tuple$1494 = _func_attrs_tuple$1497;
      goto $join$1493;
    }
    goto $joinlet$4588;
    $join$1493:;
    index_func_map$1486 = _index_func_map$1492;
    func_attrs_tuple$1487 = func_attrs_tuple$1494;
    goto $join$1485;
    $joinlet$4588:;
  }
  goto $joinlet$4587;
  $join$1485:;
  moonbit_decref(index_func_map$1486);
  _field$4160 = func_attrs_tuple$1487->$0;
  _tmp$3894 = _field$4160;
  moonbit_incref(_tmp$3894);
  F1$3891
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F1));
  Moonbit_object_header(F1$3891)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F1, $0) >> 2, 1, 1
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F1*)F1$3891)->$0 = _tmp$3894;
  _field$4159 = func_attrs_tuple$1487->$1;
  _cnt$4469 = Moonbit_object_header(func_attrs_tuple$1487)->rc;
  if (_cnt$4469 > 1) {
    int32_t _new_cnt$4471;
    moonbit_incref(_field$4159);
    _new_cnt$4471 = _cnt$4469 - 1;
    Moonbit_object_header(func_attrs_tuple$1487)->rc = _new_cnt$4471;
  } else if (_cnt$4469 == 1) {
    struct $$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$* _field$4470 =
      func_attrs_tuple$1487->$0;
    moonbit_decref(_field$4470);
    moonbit_free(func_attrs_tuple$1487);
  }
  _tmp$3893 = _field$4159;
  _tmp$3892
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$3892)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$3892->$0 = file_filter$1474;
  _tmp$3892->$1 = index_filter$1475;
  _tmp$3892->$2 = _tmp$3893;
  k$1488
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1488)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1488->$0 = F1$3891;
  k$1488->$1 = _tmp$3892;
  return k$1488;
  $joinlet$4587:;
  moonbit_incref(file_filter$1474);
  _bind$1502
  = $$moonbitlang$core$builtin$Map$$get$4(
    async_tests$1503, file_filter$1474
  );
  if (_bind$1502 == 0) {
    if (_bind$1502) {
      moonbit_decref(_bind$1502);
    }
    moonbit_decref(file_filter$1474);
  } else {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1504 =
      _bind$1502;
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _index_func_map$1505 =
      _Some$1504;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* func_attrs_tuple$1507;
    struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _bind$1508;
    moonbit_incref(_index_func_map$1505);
    _bind$1508
    = $$moonbitlang$core$builtin$Map$$get$5(
      _index_func_map$1505, index_filter$1475
    );
    if (_bind$1508 == 0) {
      if (_bind$1508) {
        moonbit_decref(_bind$1508);
      }
      moonbit_decref(_index_func_map$1505);
      moonbit_decref(file_filter$1474);
    } else {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _Some$1509 =
        _bind$1508;
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _func_attrs_tuple$1510 =
        _Some$1509;
      func_attrs_tuple$1507 = _func_attrs_tuple$1510;
      goto $join$1506;
    }
    goto $joinlet$4590;
    $join$1506:;
    index_func_map$1499 = _index_func_map$1505;
    func_attrs_tuple$1500 = func_attrs_tuple$1507;
    goto $join$1498;
    $joinlet$4590:;
  }
  goto $joinlet$4589;
  $join$1498:;
  moonbit_decref(index_func_map$1499);
  _field$4158 = func_attrs_tuple$1500->$0;
  _tmp$3898 = _field$4158;
  moonbit_incref(_tmp$3898);
  F2$3895
  = (void*)moonbit_malloc(sizeof(struct $Moonbit_Test_Driver_Internal__F$F2));
  Moonbit_object_header(F2$3895)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__F$F2, $0) >> 2, 1, 2
  );
  ((struct $Moonbit_Test_Driver_Internal__F$F2*)F2$3895)->$0 = _tmp$3898;
  _field$4157 = func_attrs_tuple$1500->$1;
  _cnt$4472 = Moonbit_object_header(func_attrs_tuple$1500)->rc;
  if (_cnt$4472 > 1) {
    int32_t _new_cnt$4474;
    moonbit_incref(_field$4157);
    _new_cnt$4474 = _cnt$4472 - 1;
    Moonbit_object_header(func_attrs_tuple$1500)->rc = _new_cnt$4474;
  } else if (_cnt$4472 == 1) {
    struct $$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _field$4473 =
      func_attrs_tuple$1500->$0;
    moonbit_decref(_field$4473);
    moonbit_free(func_attrs_tuple$1500);
  }
  _tmp$3897 = _field$4157;
  _tmp$3896
  = (struct $Moonbit_Test_Driver_Internal_Meta*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal_Meta)
    );
  Moonbit_object_header(_tmp$3896)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal_Meta, $0) >> 2, 2, 0
  );
  _tmp$3896->$0 = file_filter$1474;
  _tmp$3896->$1 = index_filter$1475;
  _tmp$3896->$2 = _tmp$3897;
  k$1501
  = (struct $Moonbit_Test_Driver_Internal__TestCase*)moonbit_malloc(
      sizeof(struct $Moonbit_Test_Driver_Internal__TestCase)
    );
  Moonbit_object_header(k$1501)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Moonbit_Test_Driver_Internal__TestCase, $0) >> 2, 2, 0
  );
  k$1501->$0 = F2$3895;
  k$1501->$1 = _tmp$3896;
  return k$1501;
  $joinlet$4589:;
  return 0;
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13(
  
) {
  moonbit_string_t _tmp$3806 = 0;
  struct moonbit_result_0 _tmp$4591 =
    $moonbitlang$core$builtin$assert_true(
      1, _tmp$3806, (moonbit_string_t)moonbit_string_literal_14.data
    );
  moonbit_string_t _tmp$3809;
  struct moonbit_result_0 _tmp$4593;
  moonbit_string_t _tmp$3812;
  struct moonbit_result_0 _tmp$4595;
  moonbit_string_t _tmp$3815;
  struct moonbit_result_0 _tmp$4597;
  moonbit_string_t _tmp$3818;
  struct moonbit_result_0 _tmp$4599;
  moonbit_string_t _tmp$3821;
  struct moonbit_result_0 _tmp$4601;
  moonbit_string_t _tmp$3824;
  struct moonbit_result_0 _tmp$4603;
  moonbit_string_t _tmp$3827;
  struct moonbit_result_0 _tmp$4605;
  int32_t _tmp$3830;
  moonbit_string_t _tmp$3831;
  struct moonbit_result_0 _tmp$4607;
  int32_t _tmp$3834;
  moonbit_string_t _tmp$3835;
  struct moonbit_result_0 _tmp$4609;
  int32_t _tmp$3838;
  moonbit_string_t _tmp$3839;
  struct moonbit_result_0 _tmp$4611;
  int32_t a$1454;
  int32_t b$1455;
  int32_t c$1456;
  int32_t _tmp$3842;
  moonbit_string_t _tmp$3843;
  struct moonbit_result_0 _tmp$4613;
  int32_t _tmp$3846;
  moonbit_string_t _tmp$3847;
  struct moonbit_result_0 _tmp$4615;
  int32_t _tmp$3850;
  moonbit_string_t _tmp$3851;
  struct moonbit_result_0 _tmp$4617;
  moonbit_string_t result$1457;
  moonbit_string_t _tmp$3854;
  struct moonbit_result_0 _tmp$4619;
  moonbit_string_t result2$1458;
  moonbit_string_t _tmp$3857;
  struct moonbit_result_0 _tmp$4621;
  int32_t* _tmp$3886;
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* bool_array$1459;
  struct $Ref$3c$Int$3e$* true_count$1460;
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* _arr$1461;
  int32_t _len$1462;
  int32_t _i$1463;
  int32_t _field$4163;
  int32_t val$3863;
  moonbit_string_t _tmp$3864;
  struct moonbit_result_0 _tmp$4624;
  moonbit_string_t _tmp$3867;
  struct moonbit_result_0 _tmp$4626;
  moonbit_string_t _tmp$3870;
  struct moonbit_result_0 _tmp$4628;
  moonbit_string_t _tmp$3873;
  struct moonbit_result_0 _tmp$4630;
  int32_t _tmp$3876;
  moonbit_string_t _tmp$3877;
  struct moonbit_result_0 _tmp$4632;
  int32_t _tmp$3880;
  moonbit_string_t _tmp$3881;
  struct moonbit_result_0 _tmp$4634;
  int32_t _tmp$3884;
  moonbit_string_t _tmp$3885;
  if (_tmp$4591.tag) {
    int32_t const _ok$3807 = _tmp$4591.data.ok;
  } else {
    void* const _err$3808 = _tmp$4591.data.err;
    struct moonbit_result_0 _result$4592;
    _result$4592.tag = 0;
    _result$4592.data.err = _err$3808;
    return _result$4592;
  }
  _tmp$3809 = 0;
  _tmp$4593
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3809, (moonbit_string_t)moonbit_string_literal_15.data
  );
  if (_tmp$4593.tag) {
    int32_t const _ok$3810 = _tmp$4593.data.ok;
  } else {
    void* const _err$3811 = _tmp$4593.data.err;
    struct moonbit_result_0 _result$4594;
    _result$4594.tag = 0;
    _result$4594.data.err = _err$3811;
    return _result$4594;
  }
  _tmp$3812 = 0;
  _tmp$4595
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3812, (moonbit_string_t)moonbit_string_literal_16.data
  );
  if (_tmp$4595.tag) {
    int32_t const _ok$3813 = _tmp$4595.data.ok;
  } else {
    void* const _err$3814 = _tmp$4595.data.err;
    struct moonbit_result_0 _result$4596;
    _result$4596.tag = 0;
    _result$4596.data.err = _err$3814;
    return _result$4596;
  }
  _tmp$3815 = 0;
  _tmp$4597
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3815, (moonbit_string_t)moonbit_string_literal_17.data
  );
  if (_tmp$4597.tag) {
    int32_t const _ok$3816 = _tmp$4597.data.ok;
  } else {
    void* const _err$3817 = _tmp$4597.data.err;
    struct moonbit_result_0 _result$4598;
    _result$4598.tag = 0;
    _result$4598.data.err = _err$3817;
    return _result$4598;
  }
  _tmp$3818 = 0;
  _tmp$4599
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3818, (moonbit_string_t)moonbit_string_literal_18.data
  );
  if (_tmp$4599.tag) {
    int32_t const _ok$3819 = _tmp$4599.data.ok;
  } else {
    void* const _err$3820 = _tmp$4599.data.err;
    struct moonbit_result_0 _result$4600;
    _result$4600.tag = 0;
    _result$4600.data.err = _err$3820;
    return _result$4600;
  }
  _tmp$3821 = 0;
  _tmp$4601
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3821, (moonbit_string_t)moonbit_string_literal_19.data
  );
  if (_tmp$4601.tag) {
    int32_t const _ok$3822 = _tmp$4601.data.ok;
  } else {
    void* const _err$3823 = _tmp$4601.data.err;
    struct moonbit_result_0 _result$4602;
    _result$4602.tag = 0;
    _result$4602.data.err = _err$3823;
    return _result$4602;
  }
  _tmp$3824 = 0;
  _tmp$4603
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3824, (moonbit_string_t)moonbit_string_literal_20.data
  );
  if (_tmp$4603.tag) {
    int32_t const _ok$3825 = _tmp$4603.data.ok;
  } else {
    void* const _err$3826 = _tmp$4603.data.err;
    struct moonbit_result_0 _result$4604;
    _result$4604.tag = 0;
    _result$4604.data.err = _err$3826;
    return _result$4604;
  }
  _tmp$3827 = 0;
  _tmp$4605
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3827, (moonbit_string_t)moonbit_string_literal_21.data
  );
  if (_tmp$4605.tag) {
    int32_t const _ok$3828 = _tmp$4605.data.ok;
  } else {
    void* const _err$3829 = _tmp$4605.data.err;
    struct moonbit_result_0 _result$4606;
    _result$4606.tag = 0;
    _result$4606.data.err = _err$3829;
    return _result$4606;
  }
  _tmp$3830 = 1 == 1;
  _tmp$3831 = 0;
  _tmp$4607
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3830, _tmp$3831, (moonbit_string_t)moonbit_string_literal_22.data
  );
  if (_tmp$4607.tag) {
    int32_t const _ok$3832 = _tmp$4607.data.ok;
  } else {
    void* const _err$3833 = _tmp$4607.data.err;
    struct moonbit_result_0 _result$4608;
    _result$4608.tag = 0;
    _result$4608.data.err = _err$3833;
    return _result$4608;
  }
  _tmp$3834 = 1 == 0;
  _tmp$3835 = 0;
  _tmp$4609
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3834, _tmp$3835, (moonbit_string_t)moonbit_string_literal_23.data
  );
  if (_tmp$4609.tag) {
    int32_t const _ok$3836 = _tmp$4609.data.ok;
  } else {
    void* const _err$3837 = _tmp$4609.data.err;
    struct moonbit_result_0 _result$4610;
    _result$4610.tag = 0;
    _result$4610.data.err = _err$3837;
    return _result$4610;
  }
  _tmp$3838 = $moonbitlang$core$builtin$op_notequal$4(1, 0);
  _tmp$3839 = 0;
  _tmp$4611
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3838, _tmp$3839, (moonbit_string_t)moonbit_string_literal_24.data
  );
  if (_tmp$4611.tag) {
    int32_t const _ok$3840 = _tmp$4611.data.ok;
  } else {
    void* const _err$3841 = _tmp$4611.data.err;
    struct moonbit_result_0 _result$4612;
    _result$4612.tag = 0;
    _result$4612.data.err = _err$3841;
    return _result$4612;
  }
  a$1454 = 1;
  b$1455 = 0;
  c$1456 = 1;
  _tmp$3842 = a$1454 && b$1455 || c$1456;
  _tmp$3843 = 0;
  _tmp$4613
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3842, _tmp$3843, (moonbit_string_t)moonbit_string_literal_25.data
  );
  if (_tmp$4613.tag) {
    int32_t const _ok$3844 = _tmp$4613.data.ok;
  } else {
    void* const _err$3845 = _tmp$4613.data.err;
    struct moonbit_result_0 _result$4614;
    _result$4614.tag = 0;
    _result$4614.data.err = _err$3845;
    return _result$4614;
  }
  if (a$1454) {
    _tmp$3846 = 1;
  } else {
    _tmp$3846 = b$1455 && c$1456;
  }
  _tmp$3847 = 0;
  _tmp$4615
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3846, _tmp$3847, (moonbit_string_t)moonbit_string_literal_26.data
  );
  if (_tmp$4615.tag) {
    int32_t const _ok$3848 = _tmp$4615.data.ok;
  } else {
    void* const _err$3849 = _tmp$4615.data.err;
    struct moonbit_result_0 _result$4616;
    _result$4616.tag = 0;
    _result$4616.data.err = _err$3849;
    return _result$4616;
  }
  if (!a$1454) {
    if (!b$1455) {
      _tmp$3850 = !c$1456;
    } else {
      _tmp$3850 = 0;
    }
  } else {
    _tmp$3850 = 0;
  }
  _tmp$3851 = 0;
  _tmp$4617
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3850, _tmp$3851, (moonbit_string_t)moonbit_string_literal_27.data
  );
  if (_tmp$4617.tag) {
    int32_t const _ok$3852 = _tmp$4617.data.ok;
  } else {
    void* const _err$3853 = _tmp$4617.data.err;
    struct moonbit_result_0 _result$4618;
    _result$4618.tag = 0;
    _result$4618.data.err = _err$3853;
    return _result$4618;
  }
  if (a$1454) {
    result$1457 = (moonbit_string_t)moonbit_string_literal_28.data;
  } else {
    result$1457 = (moonbit_string_t)moonbit_string_literal_29.data;
  }
  _tmp$3854 = 0;
  _tmp$4619
  = $moonbitlang$core$builtin$assert_eq$1(
    result$1457,
      (moonbit_string_t)moonbit_string_literal_28.data,
      _tmp$3854,
      (moonbit_string_t)moonbit_string_literal_30.data
  );
  if (_tmp$4619.tag) {
    int32_t const _ok$3855 = _tmp$4619.data.ok;
  } else {
    void* const _err$3856 = _tmp$4619.data.err;
    struct moonbit_result_0 _result$4620;
    _result$4620.tag = 0;
    _result$4620.data.err = _err$3856;
    return _result$4620;
  }
  if (b$1455) {
    result2$1458 = (moonbit_string_t)moonbit_string_literal_28.data;
  } else {
    result2$1458 = (moonbit_string_t)moonbit_string_literal_29.data;
  }
  _tmp$3857 = 0;
  _tmp$4621
  = $moonbitlang$core$builtin$assert_eq$1(
    result2$1458,
      (moonbit_string_t)moonbit_string_literal_29.data,
      _tmp$3857,
      (moonbit_string_t)moonbit_string_literal_31.data
  );
  if (_tmp$4621.tag) {
    int32_t const _ok$3858 = _tmp$4621.data.ok;
  } else {
    void* const _err$3859 = _tmp$4621.data.err;
    struct moonbit_result_0 _result$4622;
    _result$4622.tag = 0;
    _result$4622.data.err = _err$3859;
    return _result$4622;
  }
  _tmp$3886 = (int32_t*)moonbit_make_int32_array_raw(4);
  _tmp$3886[0] = 1;
  _tmp$3886[1] = 0;
  _tmp$3886[2] = 1;
  _tmp$3886[3] = 0;
  bool_array$1459
  = (struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$)
    );
  Moonbit_object_header(bool_array$1459)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$, $0) >> 2,
      1,
      0
  );
  bool_array$1459->$0 = _tmp$3886;
  bool_array$1459->$1 = 4;
  true_count$1460
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(true_count$1460)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  true_count$1460->$0 = 0;
  _arr$1461 = bool_array$1459;
  moonbit_incref(_arr$1461);
  _len$1462 = $$moonbitlang$core$builtin$Array$$length$5(_arr$1461);
  _i$1463 = 0;
  while (1) {
    if (_i$1463 < _len$1462) {
      int32_t bool_val$1464;
      int32_t _tmp$3862;
      moonbit_incref(_arr$1461);
      bool_val$1464
      = $$moonbitlang$core$builtin$Array$$unsafe_get$3(
        _arr$1461, _i$1463
      );
      if (bool_val$1464) {
        int32_t val$3861 = true_count$1460->$0;
        int32_t _tmp$3860 = val$3861 + 1;
        true_count$1460->$0 = _tmp$3860;
      }
      _tmp$3862 = _i$1463 + 1;
      _i$1463 = _tmp$3862;
      continue;
    } else {
      moonbit_decref(_arr$1461);
    }
    break;
  }
  _field$4163 = true_count$1460->$0;
  moonbit_decref(true_count$1460);
  val$3863 = _field$4163;
  _tmp$3864 = 0;
  _tmp$4624
  = $moonbitlang$core$builtin$assert_eq$0(
    val$3863, 2, _tmp$3864, (moonbit_string_t)moonbit_string_literal_32.data
  );
  if (_tmp$4624.tag) {
    int32_t const _ok$3865 = _tmp$4624.data.ok;
  } else {
    void* const _err$3866 = _tmp$4624.data.err;
    struct moonbit_result_0 _result$4625;
    _result$4625.tag = 0;
    _result$4625.data.err = _err$3866;
    return _result$4625;
  }
  _tmp$3867 = 0;
  _tmp$4626
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3867, (moonbit_string_t)moonbit_string_literal_33.data
  );
  if (_tmp$4626.tag) {
    int32_t const _ok$3868 = _tmp$4626.data.ok;
  } else {
    void* const _err$3869 = _tmp$4626.data.err;
    struct moonbit_result_0 _result$4627;
    _result$4627.tag = 0;
    _result$4627.data.err = _err$3869;
    return _result$4627;
  }
  _tmp$3870 = 0;
  _tmp$4628
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3870, (moonbit_string_t)moonbit_string_literal_34.data
  );
  if (_tmp$4628.tag) {
    int32_t const _ok$3871 = _tmp$4628.data.ok;
  } else {
    void* const _err$3872 = _tmp$4628.data.err;
    struct moonbit_result_0 _result$4629;
    _result$4629.tag = 0;
    _result$4629.data.err = _err$3872;
    return _result$4629;
  }
  _tmp$3873 = 0;
  _tmp$4630
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3873, (moonbit_string_t)moonbit_string_literal_35.data
  );
  if (_tmp$4630.tag) {
    int32_t const _ok$3874 = _tmp$4630.data.ok;
  } else {
    void* const _err$3875 = _tmp$4630.data.err;
    struct moonbit_result_0 _result$4631;
    _result$4631.tag = 0;
    _result$4631.data.err = _err$3875;
    return _result$4631;
  }
  _tmp$3876 = 1 == 1;
  _tmp$3877 = 0;
  _tmp$4632
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3876, _tmp$3877, (moonbit_string_t)moonbit_string_literal_36.data
  );
  if (_tmp$4632.tag) {
    int32_t const _ok$3878 = _tmp$4632.data.ok;
  } else {
    void* const _err$3879 = _tmp$4632.data.err;
    struct moonbit_result_0 _result$4633;
    _result$4633.tag = 0;
    _result$4633.data.err = _err$3879;
    return _result$4633;
  }
  _tmp$3880 = 0 == 0;
  _tmp$3881 = 0;
  _tmp$4634
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3880, _tmp$3881, (moonbit_string_t)moonbit_string_literal_37.data
  );
  if (_tmp$4634.tag) {
    int32_t const _ok$3882 = _tmp$4634.data.ok;
  } else {
    void* const _err$3883 = _tmp$4634.data.err;
    struct moonbit_result_0 _result$4635;
    _result$4635.tag = 0;
    _result$4635.data.err = _err$3883;
    return _result$4635;
  }
  _tmp$3884 = 1 == 1;
  _tmp$3885 = 0;
  return $moonbitlang$core$builtin$assert_true(
           _tmp$3884,
             _tmp$3885,
             (moonbit_string_t)moonbit_string_literal_38.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12(
  
) {
  int32_t int_max$1443 = 2147483647;
  int32_t int_min$1444 = (int32_t)0x80000000;
  int32_t int_zero$1445 = 0;
  int32_t _tmp$3723 = int_zero$1445 * int_max$1443;
  moonbit_string_t _tmp$3724 = 0;
  struct moonbit_result_0 _tmp$4636 =
    $moonbitlang$core$builtin$assert_eq$0(
      _tmp$3723,
        0,
        _tmp$3724,
        (moonbit_string_t)moonbit_string_literal_39.data
    );
  int32_t _tmp$3727;
  moonbit_string_t _tmp$3728;
  struct moonbit_result_0 _tmp$4638;
  double float_max$1446;
  double float_min$1447;
  double float_zero$1448;
  double float_small$1449;
  double _tmp$3731;
  moonbit_string_t _tmp$3732;
  struct moonbit_result_0 _tmp$4640;
  double _tmp$3735;
  moonbit_string_t _tmp$3736;
  struct moonbit_result_0 _tmp$4642;
  double _tmp$3739;
  moonbit_string_t _tmp$3740;
  struct moonbit_result_0 _tmp$4644;
  moonbit_string_t _tmp$3743;
  struct moonbit_result_0 _tmp$4646;
  moonbit_string_t _tmp$3746;
  struct moonbit_result_0 _tmp$4648;
  moonbit_string_t _tmp$3749;
  struct moonbit_result_0 _tmp$4650;
  moonbit_string_t _tmp$3752;
  struct moonbit_result_0 _tmp$4652;
  moonbit_string_t _tmp$3755;
  struct moonbit_result_0 _tmp$4654;
  moonbit_string_t _tmp$3758;
  struct moonbit_result_0 _tmp$4656;
  moonbit_string_t _tmp$3761;
  struct moonbit_result_0 _tmp$4658;
  moonbit_string_t _tmp$3764;
  struct moonbit_result_0 _tmp$4660;
  moonbit_string_t _tmp$3767;
  struct moonbit_result_0 _tmp$4662;
  moonbit_string_t _tmp$3770;
  struct moonbit_result_0 _tmp$4664;
  moonbit_string_t _tmp$3773;
  struct moonbit_result_0 _tmp$4666;
  moonbit_string_t _tmp$3776;
  struct moonbit_result_0 _tmp$4668;
  moonbit_string_t _tmp$3779;
  struct moonbit_result_0 _tmp$4670;
  moonbit_string_t _tmp$3782;
  struct moonbit_result_0 _tmp$4672;
  moonbit_string_t _tmp$3785;
  struct moonbit_result_0 _tmp$4674;
  moonbit_string_t _tmp$3788;
  struct moonbit_result_0 _tmp$4676;
  moonbit_string_t _tmp$3791;
  struct moonbit_result_0 _tmp$4678;
  moonbit_string_t _tmp$3794;
  struct moonbit_result_0 _tmp$4680;
  int32_t neg_int$1450;
  double neg_double$1451;
  int32_t _tmp$3797;
  moonbit_string_t _tmp$3798;
  struct moonbit_result_0 _tmp$4682;
  double _tmp$3801;
  moonbit_string_t _tmp$3802;
  struct moonbit_result_0 _tmp$4684;
  int32_t test_neg$1452;
  int32_t abs_value$1453;
  moonbit_string_t _tmp$3805;
  if (_tmp$4636.tag) {
    int32_t const _ok$3725 = _tmp$4636.data.ok;
  } else {
    void* const _err$3726 = _tmp$4636.data.err;
    struct moonbit_result_0 _result$4637;
    _result$4637.tag = 0;
    _result$4637.data.err = _err$3726;
    return _result$4637;
  }
  _tmp$3727 = int_zero$1445 * int_min$1444;
  _tmp$3728 = 0;
  _tmp$4638
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3727, 0, _tmp$3728, (moonbit_string_t)moonbit_string_literal_40.data
  );
  if (_tmp$4638.tag) {
    int32_t const _ok$3729 = _tmp$4638.data.ok;
  } else {
    void* const _err$3730 = _tmp$4638.data.err;
    struct moonbit_result_0 _result$4639;
    _result$4639.tag = 0;
    _result$4639.data.err = _err$3730;
    return _result$4639;
  }
  float_max$1446 = 0x1.e847fff7ced91p+19;
  float_min$1447 = -0x1.e847fff7ced91p+19;
  float_zero$1448 = 0x0p+0;
  float_small$1449 = 0x1.0c6f7a0b5ed8dp-20;
  _tmp$3731 = float_max$1446 + float_min$1447;
  _tmp$3732 = 0;
  _tmp$4640
  = $moonbitlang$core$builtin$assert_eq$6(
    _tmp$3731,
      0x0p+0,
      _tmp$3732,
      (moonbit_string_t)moonbit_string_literal_41.data
  );
  if (_tmp$4640.tag) {
    int32_t const _ok$3733 = _tmp$4640.data.ok;
  } else {
    void* const _err$3734 = _tmp$4640.data.err;
    struct moonbit_result_0 _result$4641;
    _result$4641.tag = 0;
    _result$4641.data.err = _err$3734;
    return _result$4641;
  }
  _tmp$3735 = float_zero$1448 * float_max$1446;
  _tmp$3736 = 0;
  _tmp$4642
  = $moonbitlang$core$builtin$assert_eq$6(
    _tmp$3735,
      0x0p+0,
      _tmp$3736,
      (moonbit_string_t)moonbit_string_literal_42.data
  );
  if (_tmp$4642.tag) {
    int32_t const _ok$3737 = _tmp$4642.data.ok;
  } else {
    void* const _err$3738 = _tmp$4642.data.err;
    struct moonbit_result_0 _result$4643;
    _result$4643.tag = 0;
    _result$4643.data.err = _err$3738;
    return _result$4643;
  }
  _tmp$3739 = float_small$1449 * 0x1.e848p+19;
  _tmp$3740 = 0;
  _tmp$4644
  = $moonbitlang$core$builtin$assert_eq$6(
    _tmp$3739,
      0x1p+0,
      _tmp$3740,
      (moonbit_string_t)moonbit_string_literal_43.data
  );
  if (_tmp$4644.tag) {
    int32_t const _ok$3741 = _tmp$4644.data.ok;
  } else {
    void* const _err$3742 = _tmp$4644.data.err;
    struct moonbit_result_0 _result$4645;
    _result$4645.tag = 0;
    _result$4645.data.err = _err$3742;
    return _result$4645;
  }
  _tmp$3743 = 0;
  _tmp$4646
  = $moonbitlang$core$builtin$assert_eq$0(
    3, 3, _tmp$3743, (moonbit_string_t)moonbit_string_literal_44.data
  );
  if (_tmp$4646.tag) {
    int32_t const _ok$3744 = _tmp$4646.data.ok;
  } else {
    void* const _err$3745 = _tmp$4646.data.err;
    struct moonbit_result_0 _result$4647;
    _result$4647.tag = 0;
    _result$4647.data.err = _err$3745;
    return _result$4647;
  }
  _tmp$3746 = 0;
  _tmp$4648
  = $moonbitlang$core$builtin$assert_eq$6(
    0x1.aaaaaaaaaaaabp+1,
      0x1.aaaaaaaaaaaabp+1,
      _tmp$3746,
      (moonbit_string_t)moonbit_string_literal_45.data
  );
  if (_tmp$4648.tag) {
    int32_t const _ok$3747 = _tmp$4648.data.ok;
  } else {
    void* const _err$3748 = _tmp$4648.data.err;
    struct moonbit_result_0 _result$4649;
    _result$4649.tag = 0;
    _result$4649.data.err = _err$3748;
    return _result$4649;
  }
  _tmp$3749 = 0;
  _tmp$4650
  = $moonbitlang$core$builtin$assert_eq$0(
    1, 1, _tmp$3749, (moonbit_string_t)moonbit_string_literal_46.data
  );
  if (_tmp$4650.tag) {
    int32_t const _ok$3750 = _tmp$4650.data.ok;
  } else {
    void* const _err$3751 = _tmp$4650.data.err;
    struct moonbit_result_0 _result$4651;
    _result$4651.tag = 0;
    _result$4651.data.err = _err$3751;
    return _result$4651;
  }
  _tmp$3752 = 0;
  _tmp$4652
  = $moonbitlang$core$builtin$assert_eq$0(
    -1, -1, _tmp$3752, (moonbit_string_t)moonbit_string_literal_47.data
  );
  if (_tmp$4652.tag) {
    int32_t const _ok$3753 = _tmp$4652.data.ok;
  } else {
    void* const _err$3754 = _tmp$4652.data.err;
    struct moonbit_result_0 _result$4653;
    _result$4653.tag = 0;
    _result$4653.data.err = _err$3754;
    return _result$4653;
  }
  _tmp$3755 = 0;
  _tmp$4654
  = $moonbitlang$core$builtin$assert_eq$0(
    1, 1, _tmp$3755, (moonbit_string_t)moonbit_string_literal_48.data
  );
  if (_tmp$4654.tag) {
    int32_t const _ok$3756 = _tmp$4654.data.ok;
  } else {
    void* const _err$3757 = _tmp$4654.data.err;
    struct moonbit_result_0 _result$4655;
    _result$4655.tag = 0;
    _result$4655.data.err = _err$3757;
    return _result$4655;
  }
  _tmp$3758 = 0;
  _tmp$4656
  = $moonbitlang$core$builtin$assert_eq$0(
    10, 10, _tmp$3758, (moonbit_string_t)moonbit_string_literal_49.data
  );
  if (_tmp$4656.tag) {
    int32_t const _ok$3759 = _tmp$4656.data.ok;
  } else {
    void* const _err$3760 = _tmp$4656.data.err;
    struct moonbit_result_0 _result$4657;
    _result$4657.tag = 0;
    _result$4657.data.err = _err$3760;
    return _result$4657;
  }
  _tmp$3761 = 0;
  _tmp$4658
  = $moonbitlang$core$builtin$assert_eq$0(
    21, 21, _tmp$3761, (moonbit_string_t)moonbit_string_literal_50.data
  );
  if (_tmp$4658.tag) {
    int32_t const _ok$3762 = _tmp$4658.data.ok;
  } else {
    void* const _err$3763 = _tmp$4658.data.err;
    struct moonbit_result_0 _result$4659;
    _result$4659.tag = 0;
    _result$4659.data.err = _err$3763;
    return _result$4659;
  }
  _tmp$3764 = 0;
  _tmp$4660
  = $moonbitlang$core$builtin$assert_eq$0(
    30, 30, _tmp$3764, (moonbit_string_t)moonbit_string_literal_51.data
  );
  if (_tmp$4660.tag) {
    int32_t const _ok$3765 = _tmp$4660.data.ok;
  } else {
    void* const _err$3766 = _tmp$4660.data.err;
    struct moonbit_result_0 _result$4661;
    _result$4661.tag = 0;
    _result$4661.data.err = _err$3766;
    return _result$4661;
  }
  _tmp$3767 = 0;
  _tmp$4662
  = $moonbitlang$core$builtin$assert_eq$0(
    5, 5, _tmp$3767, (moonbit_string_t)moonbit_string_literal_52.data
  );
  if (_tmp$4662.tag) {
    int32_t const _ok$3768 = _tmp$4662.data.ok;
  } else {
    void* const _err$3769 = _tmp$4662.data.err;
    struct moonbit_result_0 _result$4663;
    _result$4663.tag = 0;
    _result$4663.data.err = _err$3769;
    return _result$4663;
  }
  _tmp$3770 = 0;
  _tmp$4664
  = $moonbitlang$core$builtin$assert_eq$0(
    5, 5, _tmp$3770, (moonbit_string_t)moonbit_string_literal_53.data
  );
  if (_tmp$4664.tag) {
    int32_t const _ok$3771 = _tmp$4664.data.ok;
  } else {
    void* const _err$3772 = _tmp$4664.data.err;
    struct moonbit_result_0 _result$4665;
    _result$4665.tag = 0;
    _result$4665.data.err = _err$3772;
    return _result$4665;
  }
  _tmp$3773 = 0;
  _tmp$4666
  = $moonbitlang$core$builtin$assert_eq$0(
    -5, -5, _tmp$3773, (moonbit_string_t)moonbit_string_literal_54.data
  );
  if (_tmp$4666.tag) {
    int32_t const _ok$3774 = _tmp$4666.data.ok;
  } else {
    void* const _err$3775 = _tmp$4666.data.err;
    struct moonbit_result_0 _result$4667;
    _result$4667.tag = 0;
    _result$4667.data.err = _err$3775;
    return _result$4667;
  }
  _tmp$3776 = 0;
  _tmp$4668
  = $moonbitlang$core$builtin$assert_eq$0(
    5, 5, _tmp$3776, (moonbit_string_t)moonbit_string_literal_55.data
  );
  if (_tmp$4668.tag) {
    int32_t const _ok$3777 = _tmp$4668.data.ok;
  } else {
    void* const _err$3778 = _tmp$4668.data.err;
    struct moonbit_result_0 _result$4669;
    _result$4669.tag = 0;
    _result$4669.data.err = _err$3778;
    return _result$4669;
  }
  _tmp$3779 = 0;
  _tmp$4670
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3779, (moonbit_string_t)moonbit_string_literal_56.data
  );
  if (_tmp$4670.tag) {
    int32_t const _ok$3780 = _tmp$4670.data.ok;
  } else {
    void* const _err$3781 = _tmp$4670.data.err;
    struct moonbit_result_0 _result$4671;
    _result$4671.tag = 0;
    _result$4671.data.err = _err$3781;
    return _result$4671;
  }
  _tmp$3782 = 0;
  _tmp$4672
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3782, (moonbit_string_t)moonbit_string_literal_57.data
  );
  if (_tmp$4672.tag) {
    int32_t const _ok$3783 = _tmp$4672.data.ok;
  } else {
    void* const _err$3784 = _tmp$4672.data.err;
    struct moonbit_result_0 _result$4673;
    _result$4673.tag = 0;
    _result$4673.data.err = _err$3784;
    return _result$4673;
  }
  _tmp$3785 = 0;
  _tmp$4674
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3785, (moonbit_string_t)moonbit_string_literal_58.data
  );
  if (_tmp$4674.tag) {
    int32_t const _ok$3786 = _tmp$4674.data.ok;
  } else {
    void* const _err$3787 = _tmp$4674.data.err;
    struct moonbit_result_0 _result$4675;
    _result$4675.tag = 0;
    _result$4675.data.err = _err$3787;
    return _result$4675;
  }
  _tmp$3788 = 0;
  _tmp$4676
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3788, (moonbit_string_t)moonbit_string_literal_59.data
  );
  if (_tmp$4676.tag) {
    int32_t const _ok$3789 = _tmp$4676.data.ok;
  } else {
    void* const _err$3790 = _tmp$4676.data.err;
    struct moonbit_result_0 _result$4677;
    _result$4677.tag = 0;
    _result$4677.data.err = _err$3790;
    return _result$4677;
  }
  _tmp$3791 = 0;
  _tmp$4678
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3791, (moonbit_string_t)moonbit_string_literal_60.data
  );
  if (_tmp$4678.tag) {
    int32_t const _ok$3792 = _tmp$4678.data.ok;
  } else {
    void* const _err$3793 = _tmp$4678.data.err;
    struct moonbit_result_0 _result$4679;
    _result$4679.tag = 0;
    _result$4679.data.err = _err$3793;
    return _result$4679;
  }
  _tmp$3794 = 0;
  _tmp$4680
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3794, (moonbit_string_t)moonbit_string_literal_61.data
  );
  if (_tmp$4680.tag) {
    int32_t const _ok$3795 = _tmp$4680.data.ok;
  } else {
    void* const _err$3796 = _tmp$4680.data.err;
    struct moonbit_result_0 _result$4681;
    _result$4681.tag = 0;
    _result$4681.data.err = _err$3796;
    return _result$4681;
  }
  neg_int$1450 = -42;
  neg_double$1451 = -0x1.91eb851eb851fp+1;
  _tmp$3797 = neg_int$1450 + 42;
  _tmp$3798 = 0;
  _tmp$4682
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3797, 0, _tmp$3798, (moonbit_string_t)moonbit_string_literal_62.data
  );
  if (_tmp$4682.tag) {
    int32_t const _ok$3799 = _tmp$4682.data.ok;
  } else {
    void* const _err$3800 = _tmp$4682.data.err;
    struct moonbit_result_0 _result$4683;
    _result$4683.tag = 0;
    _result$4683.data.err = _err$3800;
    return _result$4683;
  }
  _tmp$3801 = neg_double$1451 + 0x1.91eb851eb851fp+1;
  _tmp$3802 = 0;
  _tmp$4684
  = $moonbitlang$core$builtin$assert_eq$6(
    _tmp$3801,
      0x0p+0,
      _tmp$3802,
      (moonbit_string_t)moonbit_string_literal_63.data
  );
  if (_tmp$4684.tag) {
    int32_t const _ok$3803 = _tmp$4684.data.ok;
  } else {
    void* const _err$3804 = _tmp$4684.data.err;
    struct moonbit_result_0 _result$4685;
    _result$4685.tag = 0;
    _result$4685.data.err = _err$3804;
    return _result$4685;
  }
  test_neg$1452 = -100;
  if (test_neg$1452 < 0) {
    abs_value$1453 = -test_neg$1452;
  } else {
    abs_value$1453 = test_neg$1452;
  }
  _tmp$3805 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           abs_value$1453,
             100,
             _tmp$3805,
             (moonbit_string_t)moonbit_string_literal_64.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11(
  
) {
  int32_t* _tmp$3722 = (int32_t*)moonbit_empty_int32_array;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* empty_array$1424 =
    (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  int32_t* _tmp$3721;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* int_array$1425;
  moonbit_string_t* _tmp$3720;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* string_array$1426;
  double* _tmp$3719;
  struct $$moonbitlang$core$builtin$Array$3c$Double$3e$* double_array$1427;
  int32_t _tmp$3623;
  moonbit_string_t _tmp$3624;
  struct moonbit_result_0 _tmp$4686;
  int32_t _tmp$3627;
  moonbit_string_t _tmp$3628;
  struct moonbit_result_0 _tmp$4688;
  int32_t _tmp$3631;
  moonbit_string_t _tmp$3632;
  struct moonbit_result_0 _tmp$4690;
  int32_t _tmp$3635;
  moonbit_string_t _tmp$3636;
  struct moonbit_result_0 _tmp$4692;
  int32_t* _tmp$3718;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* test_array$1428;
  int32_t _tmp$3639;
  moonbit_string_t _tmp$3640;
  struct moonbit_result_0 _tmp$4694;
  int32_t _tmp$3643;
  moonbit_string_t _tmp$3644;
  struct moonbit_result_0 _tmp$4696;
  int32_t _tmp$3647;
  moonbit_string_t _tmp$3648;
  struct moonbit_result_0 _tmp$4698;
  int32_t _tmp$3651;
  moonbit_string_t _tmp$3652;
  struct moonbit_result_0 _tmp$4700;
  int32_t _tmp$3657;
  int32_t _tmp$3655;
  moonbit_string_t _tmp$3656;
  struct moonbit_result_0 _tmp$4702;
  int32_t _tmp$3660;
  moonbit_string_t _tmp$3661;
  struct moonbit_result_0 _tmp$4704;
  int32_t _tmp$3664;
  moonbit_string_t _tmp$3665;
  struct moonbit_result_0 _tmp$4706;
  moonbit_string_t _tmp$3668;
  moonbit_string_t _tmp$3669;
  struct moonbit_result_0 _tmp$4708;
  moonbit_string_t _tmp$3672;
  moonbit_string_t _tmp$3673;
  struct moonbit_result_0 _tmp$4710;
  int32_t* _tmp$3717;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* large_numbers$1429;
  int32_t _tmp$3676;
  moonbit_string_t _tmp$3677;
  struct moonbit_result_0 _tmp$4712;
  int32_t _tmp$3680;
  moonbit_string_t _tmp$3681;
  struct moonbit_result_0 _tmp$4714;
  int32_t _tmp$3684;
  moonbit_string_t _tmp$3685;
  struct moonbit_result_0 _tmp$4716;
  struct $Ref$3c$Int$3e$* sum$1430;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _arr$1431;
  int32_t _len$1432;
  int32_t _i$1433;
  int32_t _field$4166;
  int32_t val$3691;
  moonbit_string_t _tmp$3692;
  struct moonbit_result_0 _tmp$4719;
  struct $Ref$3c$Bool$3e$* contains_three$1436;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _arr$1437;
  int32_t _len$1438;
  int32_t _i$1439;
  int32_t _field$4165;
  int32_t val$3696;
  moonbit_string_t _tmp$3697;
  struct moonbit_result_0 _tmp$4722;
  struct $Ref$3c$Int$3e$* reverse_sum$1442;
  int32_t val$3701;
  int32_t _tmp$3702;
  int32_t _tmp$3700;
  int32_t val$3704;
  int32_t _tmp$3705;
  int32_t _tmp$3703;
  int32_t val$3707;
  int32_t _tmp$3708;
  int32_t _tmp$3706;
  int32_t val$3710;
  int32_t _tmp$3711;
  int32_t _tmp$3709;
  int32_t val$3713;
  int32_t _tmp$3714;
  int32_t _tmp$3712;
  int32_t _field$4164;
  int32_t val$3715;
  moonbit_string_t _tmp$3716;
  Moonbit_object_header(empty_array$1424)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  empty_array$1424->$0 = _tmp$3722;
  empty_array$1424->$1 = 0;
  _tmp$3721 = (int32_t*)moonbit_make_int32_array_raw(5);
  _tmp$3721[0] = 1;
  _tmp$3721[1] = 2;
  _tmp$3721[2] = 3;
  _tmp$3721[3] = 4;
  _tmp$3721[4] = 5;
  int_array$1425
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(int_array$1425)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  int_array$1425->$0 = _tmp$3721;
  int_array$1425->$1 = 5;
  _tmp$3720 = (moonbit_string_t*)moonbit_make_ref_array_raw(5);
  _tmp$3720[0] = (moonbit_string_t)moonbit_string_literal_65.data;
  _tmp$3720[1] = (moonbit_string_t)moonbit_string_literal_66.data;
  _tmp$3720[2] = (moonbit_string_t)moonbit_string_literal_67.data;
  _tmp$3720[3] = (moonbit_string_t)moonbit_string_literal_68.data;
  _tmp$3720[4] = (moonbit_string_t)moonbit_string_literal_69.data;
  string_array$1426
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(string_array$1426)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  string_array$1426->$0 = _tmp$3720;
  string_array$1426->$1 = 5;
  _tmp$3719 = (double*)moonbit_make_double_array_raw(5);
  _tmp$3719[0] = 0x1p+0;
  _tmp$3719[1] = 0x1p+1;
  _tmp$3719[2] = 0x1.8p+1;
  _tmp$3719[3] = 0x1p+2;
  _tmp$3719[4] = 0x1.4p+2;
  double_array$1427
  = (struct $$moonbitlang$core$builtin$Array$3c$Double$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Double$3e$)
    );
  Moonbit_object_header(double_array$1427)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Double$3e$, $0) >> 2,
      1,
      0
  );
  double_array$1427->$0 = _tmp$3719;
  double_array$1427->$1 = 5;
  _tmp$3623 = $$moonbitlang$core$builtin$Array$$length$2(empty_array$1424);
  _tmp$3624 = 0;
  _tmp$4686
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3623, 0, _tmp$3624, (moonbit_string_t)moonbit_string_literal_70.data
  );
  if (_tmp$4686.tag) {
    int32_t const _ok$3625 = _tmp$4686.data.ok;
  } else {
    void* const _err$3626 = _tmp$4686.data.err;
    struct moonbit_result_0 _result$4687;
    moonbit_decref(double_array$1427);
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4687.tag = 0;
    _result$4687.data.err = _err$3626;
    return _result$4687;
  }
  moonbit_incref(int_array$1425);
  _tmp$3627 = $$moonbitlang$core$builtin$Array$$length$2(int_array$1425);
  _tmp$3628 = 0;
  _tmp$4688
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3627, 5, _tmp$3628, (moonbit_string_t)moonbit_string_literal_71.data
  );
  if (_tmp$4688.tag) {
    int32_t const _ok$3629 = _tmp$4688.data.ok;
  } else {
    void* const _err$3630 = _tmp$4688.data.err;
    struct moonbit_result_0 _result$4689;
    moonbit_decref(double_array$1427);
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4689.tag = 0;
    _result$4689.data.err = _err$3630;
    return _result$4689;
  }
  moonbit_incref(string_array$1426);
  _tmp$3631 = $$moonbitlang$core$builtin$Array$$length$1(string_array$1426);
  _tmp$3632 = 0;
  _tmp$4690
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3631, 5, _tmp$3632, (moonbit_string_t)moonbit_string_literal_72.data
  );
  if (_tmp$4690.tag) {
    int32_t const _ok$3633 = _tmp$4690.data.ok;
  } else {
    void* const _err$3634 = _tmp$4690.data.err;
    struct moonbit_result_0 _result$4691;
    moonbit_decref(double_array$1427);
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4691.tag = 0;
    _result$4691.data.err = _err$3634;
    return _result$4691;
  }
  _tmp$3635 = $$moonbitlang$core$builtin$Array$$length$4(double_array$1427);
  _tmp$3636 = 0;
  _tmp$4692
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3635, 5, _tmp$3636, (moonbit_string_t)moonbit_string_literal_73.data
  );
  if (_tmp$4692.tag) {
    int32_t const _ok$3637 = _tmp$4692.data.ok;
  } else {
    void* const _err$3638 = _tmp$4692.data.err;
    struct moonbit_result_0 _result$4693;
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4693.tag = 0;
    _result$4693.data.err = _err$3638;
    return _result$4693;
  }
  _tmp$3718 = (int32_t*)moonbit_make_int32_array_raw(6);
  _tmp$3718[0] = 1;
  _tmp$3718[1] = 2;
  _tmp$3718[2] = 3;
  _tmp$3718[3] = 4;
  _tmp$3718[4] = 5;
  _tmp$3718[5] = 6;
  test_array$1428
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(test_array$1428)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  test_array$1428->$0 = _tmp$3718;
  test_array$1428->$1 = 6;
  moonbit_incref(test_array$1428);
  _tmp$3639 = $$moonbitlang$core$builtin$Array$$length$2(test_array$1428);
  _tmp$3640 = 0;
  _tmp$4694
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3639, 6, _tmp$3640, (moonbit_string_t)moonbit_string_literal_74.data
  );
  if (_tmp$4694.tag) {
    int32_t const _ok$3641 = _tmp$4694.data.ok;
  } else {
    void* const _err$3642 = _tmp$4694.data.err;
    struct moonbit_result_0 _result$4695;
    moonbit_decref(test_array$1428);
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4695.tag = 0;
    _result$4695.data.err = _err$3642;
    return _result$4695;
  }
  moonbit_incref(test_array$1428);
  _tmp$3643 = $$moonbitlang$core$builtin$Array$$at$1(test_array$1428, 0);
  _tmp$3644 = 0;
  _tmp$4696
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3643, 1, _tmp$3644, (moonbit_string_t)moonbit_string_literal_75.data
  );
  if (_tmp$4696.tag) {
    int32_t const _ok$3645 = _tmp$4696.data.ok;
  } else {
    void* const _err$3646 = _tmp$4696.data.err;
    struct moonbit_result_0 _result$4697;
    moonbit_decref(test_array$1428);
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4697.tag = 0;
    _result$4697.data.err = _err$3646;
    return _result$4697;
  }
  moonbit_incref(test_array$1428);
  _tmp$3647 = $$moonbitlang$core$builtin$Array$$at$1(test_array$1428, 5);
  _tmp$3648 = 0;
  _tmp$4698
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3647, 6, _tmp$3648, (moonbit_string_t)moonbit_string_literal_76.data
  );
  if (_tmp$4698.tag) {
    int32_t const _ok$3649 = _tmp$4698.data.ok;
  } else {
    void* const _err$3650 = _tmp$4698.data.err;
    struct moonbit_result_0 _result$4699;
    moonbit_decref(test_array$1428);
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4699.tag = 0;
    _result$4699.data.err = _err$3650;
    return _result$4699;
  }
  moonbit_incref(test_array$1428);
  _tmp$3651 = $$moonbitlang$core$builtin$Array$$length$2(test_array$1428);
  _tmp$3652 = 0;
  _tmp$4700
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3651, 6, _tmp$3652, (moonbit_string_t)moonbit_string_literal_77.data
  );
  if (_tmp$4700.tag) {
    int32_t const _ok$3653 = _tmp$4700.data.ok;
  } else {
    void* const _err$3654 = _tmp$4700.data.err;
    struct moonbit_result_0 _result$4701;
    moonbit_decref(test_array$1428);
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4701.tag = 0;
    _result$4701.data.err = _err$3654;
    return _result$4701;
  }
  _tmp$3657 = $$moonbitlang$core$builtin$Array$$length$2(test_array$1428);
  _tmp$3655 = _tmp$3657 > 0;
  _tmp$3656 = 0;
  _tmp$4702
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3655, _tmp$3656, (moonbit_string_t)moonbit_string_literal_78.data
  );
  if (_tmp$4702.tag) {
    int32_t const _ok$3658 = _tmp$4702.data.ok;
  } else {
    void* const _err$3659 = _tmp$4702.data.err;
    struct moonbit_result_0 _result$4703;
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4703.tag = 0;
    _result$4703.data.err = _err$3659;
    return _result$4703;
  }
  moonbit_incref(int_array$1425);
  _tmp$3660 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1425, 0);
  _tmp$3661 = 0;
  _tmp$4704
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3660, 1, _tmp$3661, (moonbit_string_t)moonbit_string_literal_79.data
  );
  if (_tmp$4704.tag) {
    int32_t const _ok$3662 = _tmp$4704.data.ok;
  } else {
    void* const _err$3663 = _tmp$4704.data.err;
    struct moonbit_result_0 _result$4705;
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4705.tag = 0;
    _result$4705.data.err = _err$3663;
    return _result$4705;
  }
  moonbit_incref(int_array$1425);
  _tmp$3664 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1425, 4);
  _tmp$3665 = 0;
  _tmp$4706
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3664, 5, _tmp$3665, (moonbit_string_t)moonbit_string_literal_80.data
  );
  if (_tmp$4706.tag) {
    int32_t const _ok$3666 = _tmp$4706.data.ok;
  } else {
    void* const _err$3667 = _tmp$4706.data.err;
    struct moonbit_result_0 _result$4707;
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4707.tag = 0;
    _result$4707.data.err = _err$3667;
    return _result$4707;
  }
  moonbit_incref(string_array$1426);
  _tmp$3668 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1426, 0);
  _tmp$3669 = 0;
  _tmp$4708
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3668,
      (moonbit_string_t)moonbit_string_literal_65.data,
      _tmp$3669,
      (moonbit_string_t)moonbit_string_literal_81.data
  );
  if (_tmp$4708.tag) {
    int32_t const _ok$3670 = _tmp$4708.data.ok;
  } else {
    void* const _err$3671 = _tmp$4708.data.err;
    struct moonbit_result_0 _result$4709;
    moonbit_decref(string_array$1426);
    moonbit_decref(int_array$1425);
    _result$4709.tag = 0;
    _result$4709.data.err = _err$3671;
    return _result$4709;
  }
  _tmp$3672 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1426, 4);
  _tmp$3673 = 0;
  _tmp$4710
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3672,
      (moonbit_string_t)moonbit_string_literal_69.data,
      _tmp$3673,
      (moonbit_string_t)moonbit_string_literal_82.data
  );
  if (_tmp$4710.tag) {
    int32_t const _ok$3674 = _tmp$4710.data.ok;
  } else {
    void* const _err$3675 = _tmp$4710.data.err;
    struct moonbit_result_0 _result$4711;
    moonbit_decref(int_array$1425);
    _result$4711.tag = 0;
    _result$4711.data.err = _err$3675;
    return _result$4711;
  }
  _tmp$3717 = (int32_t*)moonbit_make_int32_array_raw(4);
  _tmp$3717[0] = 2147483647;
  _tmp$3717[1] = (int32_t)0x80000000;
  _tmp$3717[2] = 1000000;
  _tmp$3717[3] = -1000000;
  large_numbers$1429
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(large_numbers$1429)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  large_numbers$1429->$0 = _tmp$3717;
  large_numbers$1429->$1 = 4;
  moonbit_incref(large_numbers$1429);
  _tmp$3676 = $$moonbitlang$core$builtin$Array$$length$2(large_numbers$1429);
  _tmp$3677 = 0;
  _tmp$4712
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3676, 4, _tmp$3677, (moonbit_string_t)moonbit_string_literal_83.data
  );
  if (_tmp$4712.tag) {
    int32_t const _ok$3678 = _tmp$4712.data.ok;
  } else {
    void* const _err$3679 = _tmp$4712.data.err;
    struct moonbit_result_0 _result$4713;
    moonbit_decref(large_numbers$1429);
    moonbit_decref(int_array$1425);
    _result$4713.tag = 0;
    _result$4713.data.err = _err$3679;
    return _result$4713;
  }
  moonbit_incref(large_numbers$1429);
  _tmp$3680 = $$moonbitlang$core$builtin$Array$$at$1(large_numbers$1429, 0);
  _tmp$3681 = 0;
  _tmp$4714
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3680,
      2147483647,
      _tmp$3681,
      (moonbit_string_t)moonbit_string_literal_84.data
  );
  if (_tmp$4714.tag) {
    int32_t const _ok$3682 = _tmp$4714.data.ok;
  } else {
    void* const _err$3683 = _tmp$4714.data.err;
    struct moonbit_result_0 _result$4715;
    moonbit_decref(large_numbers$1429);
    moonbit_decref(int_array$1425);
    _result$4715.tag = 0;
    _result$4715.data.err = _err$3683;
    return _result$4715;
  }
  _tmp$3684 = $$moonbitlang$core$builtin$Array$$at$1(large_numbers$1429, 1);
  _tmp$3685 = 0;
  _tmp$4716
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3684,
      (int32_t)0x80000000,
      _tmp$3685,
      (moonbit_string_t)moonbit_string_literal_85.data
  );
  if (_tmp$4716.tag) {
    int32_t const _ok$3686 = _tmp$4716.data.ok;
  } else {
    void* const _err$3687 = _tmp$4716.data.err;
    struct moonbit_result_0 _result$4717;
    moonbit_decref(int_array$1425);
    _result$4717.tag = 0;
    _result$4717.data.err = _err$3687;
    return _result$4717;
  }
  sum$1430
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(sum$1430)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  sum$1430->$0 = 0;
  _arr$1431 = int_array$1425;
  moonbit_incref(_arr$1431);
  moonbit_incref(_arr$1431);
  _len$1432 = $$moonbitlang$core$builtin$Array$$length$2(_arr$1431);
  _i$1433 = 0;
  while (1) {
    if (_i$1433 < _len$1432) {
      int32_t i$1434;
      int32_t val$3689;
      int32_t _tmp$3688;
      int32_t _tmp$3690;
      moonbit_incref(_arr$1431);
      i$1434
      = $$moonbitlang$core$builtin$Array$$unsafe_get$2(
        _arr$1431, _i$1433
      );
      val$3689 = sum$1430->$0;
      _tmp$3688 = val$3689 + i$1434;
      sum$1430->$0 = _tmp$3688;
      _tmp$3690 = _i$1433 + 1;
      _i$1433 = _tmp$3690;
      continue;
    } else {
      moonbit_decref(_arr$1431);
    }
    break;
  }
  _field$4166 = sum$1430->$0;
  moonbit_decref(sum$1430);
  val$3691 = _field$4166;
  _tmp$3692 = 0;
  _tmp$4719
  = $moonbitlang$core$builtin$assert_eq$0(
    val$3691, 15, _tmp$3692, (moonbit_string_t)moonbit_string_literal_86.data
  );
  if (_tmp$4719.tag) {
    int32_t const _ok$3693 = _tmp$4719.data.ok;
  } else {
    void* const _err$3694 = _tmp$4719.data.err;
    struct moonbit_result_0 _result$4720;
    moonbit_decref(int_array$1425);
    _result$4720.tag = 0;
    _result$4720.data.err = _err$3694;
    return _result$4720;
  }
  contains_three$1436
  = (struct $Ref$3c$Bool$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Bool$3e$));
  Moonbit_object_header(contains_three$1436)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Bool$3e$) >> 2, 0, 0
  );
  contains_three$1436->$0 = 0;
  _arr$1437 = int_array$1425;
  moonbit_incref(_arr$1437);
  moonbit_incref(_arr$1437);
  _len$1438 = $$moonbitlang$core$builtin$Array$$length$2(_arr$1437);
  _i$1439 = 0;
  while (1) {
    if (_i$1439 < _len$1438) {
      int32_t i$1440;
      int32_t _tmp$3695;
      moonbit_incref(_arr$1437);
      i$1440
      = $$moonbitlang$core$builtin$Array$$unsafe_get$2(
        _arr$1437, _i$1439
      );
      if (i$1440 == 3) {
        contains_three$1436->$0 = 1;
      }
      _tmp$3695 = _i$1439 + 1;
      _i$1439 = _tmp$3695;
      continue;
    } else {
      moonbit_decref(_arr$1437);
    }
    break;
  }
  _field$4165 = contains_three$1436->$0;
  moonbit_decref(contains_three$1436);
  val$3696 = _field$4165;
  _tmp$3697 = 0;
  _tmp$4722
  = $moonbitlang$core$builtin$assert_true(
    val$3696, _tmp$3697, (moonbit_string_t)moonbit_string_literal_87.data
  );
  if (_tmp$4722.tag) {
    int32_t const _ok$3698 = _tmp$4722.data.ok;
  } else {
    void* const _err$3699 = _tmp$4722.data.err;
    struct moonbit_result_0 _result$4723;
    moonbit_decref(int_array$1425);
    _result$4723.tag = 0;
    _result$4723.data.err = _err$3699;
    return _result$4723;
  }
  reverse_sum$1442
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(reverse_sum$1442)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  reverse_sum$1442->$0 = 0;
  val$3701 = reverse_sum$1442->$0;
  moonbit_incref(int_array$1425);
  _tmp$3702 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1425, 4);
  _tmp$3700 = val$3701 + _tmp$3702;
  reverse_sum$1442->$0 = _tmp$3700;
  val$3704 = reverse_sum$1442->$0;
  moonbit_incref(int_array$1425);
  _tmp$3705 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1425, 3);
  _tmp$3703 = val$3704 + _tmp$3705;
  reverse_sum$1442->$0 = _tmp$3703;
  val$3707 = reverse_sum$1442->$0;
  moonbit_incref(int_array$1425);
  _tmp$3708 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1425, 2);
  _tmp$3706 = val$3707 + _tmp$3708;
  reverse_sum$1442->$0 = _tmp$3706;
  val$3710 = reverse_sum$1442->$0;
  moonbit_incref(int_array$1425);
  _tmp$3711 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1425, 1);
  _tmp$3709 = val$3710 + _tmp$3711;
  reverse_sum$1442->$0 = _tmp$3709;
  val$3713 = reverse_sum$1442->$0;
  _tmp$3714 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1425, 0);
  _tmp$3712 = val$3713 + _tmp$3714;
  reverse_sum$1442->$0 = _tmp$3712;
  _field$4164 = reverse_sum$1442->$0;
  moonbit_decref(reverse_sum$1442);
  val$3715 = _field$4164;
  _tmp$3716 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           val$3715,
             15,
             _tmp$3716,
             (moonbit_string_t)moonbit_string_literal_88.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10(
  
) {
  moonbit_string_t str1$1410 =
    (moonbit_string_t)moonbit_string_literal_89.data;
  moonbit_string_t str2$1411 =
    (moonbit_string_t)moonbit_string_literal_90.data;
  int32_t num$1412 = 42;
  moonbit_string_t _tmp$3622 =
    moonbit_add_string(
      str1$1410, (moonbit_string_t)moonbit_string_literal_91.data
    );
  moonbit_string_t _tmp$3621 = moonbit_add_string(_tmp$3622, str2$1411);
  moonbit_string_t _tmp$3619 =
    moonbit_add_string(
      _tmp$3621, (moonbit_string_t)moonbit_string_literal_91.data
    );
  moonbit_string_t _tmp$3620 = $Int$$to_string$inner(num$1412, 10);
  moonbit_string_t combined$1413 = moonbit_add_string(_tmp$3619, _tmp$3620);
  moonbit_string_t _tmp$3562 = 0;
  struct moonbit_result_0 _tmp$4724 =
    $moonbitlang$core$builtin$assert_eq$1(
      combined$1413,
        (moonbit_string_t)moonbit_string_literal_92.data,
        _tmp$3562,
        (moonbit_string_t)moonbit_string_literal_93.data
    );
  moonbit_string_t empty_string$1414;
  moonbit_string_t single_char$1415;
  moonbit_string_t long_string$1416;
  moonbit_string_t unicode_string$1417;
  int32_t _tmp$4171;
  int32_t _tmp$3565;
  moonbit_string_t _tmp$3566;
  struct moonbit_result_0 _tmp$4726;
  int32_t _tmp$4170;
  int32_t _tmp$3569;
  moonbit_string_t _tmp$3570;
  struct moonbit_result_0 _tmp$4728;
  int32_t _tmp$4169;
  int32_t _tmp$3573;
  moonbit_string_t _tmp$3574;
  struct moonbit_result_0 _tmp$4730;
  int32_t _tmp$4168;
  int32_t _tmp$3577;
  moonbit_string_t _tmp$3578;
  struct moonbit_result_0 _tmp$4732;
  moonbit_string_t test_string$1418;
  int32_t _tmp$3581;
  moonbit_string_t _tmp$3582;
  struct moonbit_result_0 _tmp$4734;
  int32_t _tmp$3585;
  moonbit_string_t _tmp$3586;
  struct moonbit_result_0 _tmp$4736;
  int32_t _tmp$3589;
  moonbit_string_t _tmp$3590;
  struct moonbit_result_0 _tmp$4738;
  moonbit_string_t special_chars$1419;
  int32_t _tmp$4167;
  int32_t _tmp$3593;
  moonbit_string_t _tmp$3594;
  struct moonbit_result_0 _tmp$4740;
  moonbit_string_t _bind$1420;
  int32_t _tmp$3600;
  struct $StringView _tmp$3599;
  int32_t _tmp$3597;
  moonbit_string_t _tmp$3598;
  struct moonbit_result_0 _tmp$4742;
  moonbit_string_t _bind$1421;
  int32_t _tmp$3606;
  struct $StringView _tmp$3605;
  int32_t _tmp$3603;
  moonbit_string_t _tmp$3604;
  struct moonbit_result_0 _tmp$4744;
  moonbit_string_t _bind$1422;
  int32_t _tmp$3612;
  struct $StringView _tmp$3611;
  int32_t _tmp$3609;
  moonbit_string_t _tmp$3610;
  struct moonbit_result_0 _tmp$4746;
  moonbit_string_t _bind$1423;
  int32_t _tmp$3618;
  struct $StringView _tmp$3617;
  int32_t _tmp$3615;
  moonbit_string_t _tmp$3616;
  if (_tmp$4724.tag) {
    int32_t const _ok$3563 = _tmp$4724.data.ok;
  } else {
    void* const _err$3564 = _tmp$4724.data.err;
    struct moonbit_result_0 _result$4725;
    _result$4725.tag = 0;
    _result$4725.data.err = _err$3564;
    return _result$4725;
  }
  empty_string$1414 = (moonbit_string_t)moonbit_string_literal_3.data;
  single_char$1415 = (moonbit_string_t)moonbit_string_literal_65.data;
  long_string$1416
  = $String$$repeat(
    (moonbit_string_t)moonbit_string_literal_65.data, 100
  );
  unicode_string$1417 = (moonbit_string_t)moonbit_string_literal_94.data;
  _tmp$4171 = Moonbit_array_length(empty_string$1414);
  moonbit_decref(empty_string$1414);
  _tmp$3565 = _tmp$4171;
  _tmp$3566 = 0;
  _tmp$4726
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3565, 0, _tmp$3566, (moonbit_string_t)moonbit_string_literal_95.data
  );
  if (_tmp$4726.tag) {
    int32_t const _ok$3567 = _tmp$4726.data.ok;
  } else {
    void* const _err$3568 = _tmp$4726.data.err;
    struct moonbit_result_0 _result$4727;
    moonbit_decref(unicode_string$1417);
    moonbit_decref(long_string$1416);
    moonbit_decref(single_char$1415);
    _result$4727.tag = 0;
    _result$4727.data.err = _err$3568;
    return _result$4727;
  }
  _tmp$4170 = Moonbit_array_length(single_char$1415);
  moonbit_decref(single_char$1415);
  _tmp$3569 = _tmp$4170;
  _tmp$3570 = 0;
  _tmp$4728
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3569, 1, _tmp$3570, (moonbit_string_t)moonbit_string_literal_96.data
  );
  if (_tmp$4728.tag) {
    int32_t const _ok$3571 = _tmp$4728.data.ok;
  } else {
    void* const _err$3572 = _tmp$4728.data.err;
    struct moonbit_result_0 _result$4729;
    moonbit_decref(unicode_string$1417);
    moonbit_decref(long_string$1416);
    _result$4729.tag = 0;
    _result$4729.data.err = _err$3572;
    return _result$4729;
  }
  _tmp$4169 = Moonbit_array_length(long_string$1416);
  moonbit_decref(long_string$1416);
  _tmp$3573 = _tmp$4169;
  _tmp$3574 = 0;
  _tmp$4730
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3573,
      100,
      _tmp$3574,
      (moonbit_string_t)moonbit_string_literal_97.data
  );
  if (_tmp$4730.tag) {
    int32_t const _ok$3575 = _tmp$4730.data.ok;
  } else {
    void* const _err$3576 = _tmp$4730.data.err;
    struct moonbit_result_0 _result$4731;
    moonbit_decref(unicode_string$1417);
    _result$4731.tag = 0;
    _result$4731.data.err = _err$3576;
    return _result$4731;
  }
  _tmp$4168 = Moonbit_array_length(unicode_string$1417);
  moonbit_decref(unicode_string$1417);
  _tmp$3577 = _tmp$4168;
  _tmp$3578 = 0;
  _tmp$4732
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3577, 4, _tmp$3578, (moonbit_string_t)moonbit_string_literal_98.data
  );
  if (_tmp$4732.tag) {
    int32_t const _ok$3579 = _tmp$4732.data.ok;
  } else {
    void* const _err$3580 = _tmp$4732.data.err;
    struct moonbit_result_0 _result$4733;
    _result$4733.tag = 0;
    _result$4733.data.err = _err$3580;
    return _result$4733;
  }
  test_string$1418 = (moonbit_string_t)moonbit_string_literal_99.data;
  if (0 < 0 || 0 >= Moonbit_array_length(test_string$1418)) {
    moonbit_panic();
  }
  _tmp$3581 = test_string$1418[0];
  _tmp$3582 = 0;
  _tmp$4734
  = $moonbitlang$core$builtin$assert_eq$7(
    _tmp$3581,
      97,
      _tmp$3582,
      (moonbit_string_t)moonbit_string_literal_100.data
  );
  if (_tmp$4734.tag) {
    int32_t const _ok$3583 = _tmp$4734.data.ok;
  } else {
    void* const _err$3584 = _tmp$4734.data.err;
    struct moonbit_result_0 _result$4735;
    moonbit_decref(test_string$1418);
    _result$4735.tag = 0;
    _result$4735.data.err = _err$3584;
    return _result$4735;
  }
  if (25 < 0 || 25 >= Moonbit_array_length(test_string$1418)) {
    moonbit_panic();
  }
  _tmp$3585 = test_string$1418[25];
  _tmp$3586 = 0;
  _tmp$4736
  = $moonbitlang$core$builtin$assert_eq$7(
    _tmp$3585,
      122,
      _tmp$3586,
      (moonbit_string_t)moonbit_string_literal_101.data
  );
  if (_tmp$4736.tag) {
    int32_t const _ok$3587 = _tmp$4736.data.ok;
  } else {
    void* const _err$3588 = _tmp$4736.data.err;
    struct moonbit_result_0 _result$4737;
    moonbit_decref(test_string$1418);
    _result$4737.tag = 0;
    _result$4737.data.err = _err$3588;
    return _result$4737;
  }
  _tmp$3589 = Moonbit_array_length(test_string$1418);
  _tmp$3590 = 0;
  _tmp$4738
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3589,
      26,
      _tmp$3590,
      (moonbit_string_t)moonbit_string_literal_102.data
  );
  if (_tmp$4738.tag) {
    int32_t const _ok$3591 = _tmp$4738.data.ok;
  } else {
    void* const _err$3592 = _tmp$4738.data.err;
    struct moonbit_result_0 _result$4739;
    moonbit_decref(test_string$1418);
    _result$4739.tag = 0;
    _result$4739.data.err = _err$3592;
    return _result$4739;
  }
  special_chars$1419 = (moonbit_string_t)moonbit_string_literal_103.data;
  _tmp$4167 = Moonbit_array_length(special_chars$1419);
  moonbit_decref(special_chars$1419);
  _tmp$3593 = _tmp$4167;
  _tmp$3594 = 0;
  _tmp$4740
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3593,
      29,
      _tmp$3594,
      (moonbit_string_t)moonbit_string_literal_104.data
  );
  if (_tmp$4740.tag) {
    int32_t const _ok$3595 = _tmp$4740.data.ok;
  } else {
    void* const _err$3596 = _tmp$4740.data.err;
    struct moonbit_result_0 _result$4741;
    moonbit_decref(test_string$1418);
    _result$4741.tag = 0;
    _result$4741.data.err = _err$3596;
    return _result$4741;
  }
  _bind$1420 = (moonbit_string_t)moonbit_string_literal_105.data;
  _tmp$3600 = Moonbit_array_length(_bind$1420);
  _tmp$3599 = (struct $StringView){0, _tmp$3600, _bind$1420};
  moonbit_incref(test_string$1418);
  _tmp$3597 = $String$$has_prefix(test_string$1418, _tmp$3599);
  _tmp$3598 = 0;
  _tmp$4742
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3597, _tmp$3598, (moonbit_string_t)moonbit_string_literal_106.data
  );
  if (_tmp$4742.tag) {
    int32_t const _ok$3601 = _tmp$4742.data.ok;
  } else {
    void* const _err$3602 = _tmp$4742.data.err;
    struct moonbit_result_0 _result$4743;
    moonbit_decref(test_string$1418);
    _result$4743.tag = 0;
    _result$4743.data.err = _err$3602;
    return _result$4743;
  }
  _bind$1421 = (moonbit_string_t)moonbit_string_literal_107.data;
  _tmp$3606 = Moonbit_array_length(_bind$1421);
  _tmp$3605 = (struct $StringView){0, _tmp$3606, _bind$1421};
  moonbit_incref(test_string$1418);
  _tmp$3603 = $String$$has_suffix(test_string$1418, _tmp$3605);
  _tmp$3604 = 0;
  _tmp$4744
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3603, _tmp$3604, (moonbit_string_t)moonbit_string_literal_108.data
  );
  if (_tmp$4744.tag) {
    int32_t const _ok$3607 = _tmp$4744.data.ok;
  } else {
    void* const _err$3608 = _tmp$4744.data.err;
    struct moonbit_result_0 _result$4745;
    moonbit_decref(test_string$1418);
    _result$4745.tag = 0;
    _result$4745.data.err = _err$3608;
    return _result$4745;
  }
  _bind$1422 = (moonbit_string_t)moonbit_string_literal_109.data;
  _tmp$3612 = Moonbit_array_length(_bind$1422);
  _tmp$3611 = (struct $StringView){0, _tmp$3612, _bind$1422};
  moonbit_incref(test_string$1418);
  _tmp$3609 = $String$$contains(test_string$1418, _tmp$3611);
  _tmp$3610 = 0;
  _tmp$4746
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3609, _tmp$3610, (moonbit_string_t)moonbit_string_literal_110.data
  );
  if (_tmp$4746.tag) {
    int32_t const _ok$3613 = _tmp$4746.data.ok;
  } else {
    void* const _err$3614 = _tmp$4746.data.err;
    struct moonbit_result_0 _result$4747;
    moonbit_decref(test_string$1418);
    _result$4747.tag = 0;
    _result$4747.data.err = _err$3614;
    return _result$4747;
  }
  _bind$1423 = (moonbit_string_t)moonbit_string_literal_111.data;
  _tmp$3618 = Moonbit_array_length(_bind$1423);
  _tmp$3617 = (struct $StringView){0, _tmp$3618, _bind$1423};
  _tmp$3615 = $String$$contains(test_string$1418, _tmp$3617);
  _tmp$3616 = 0;
  return $moonbitlang$core$builtin$assert_false(
           _tmp$3615,
             _tmp$3616,
             (moonbit_string_t)moonbit_string_literal_112.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9(
  
) {
  moonbit_string_t* _tmp$3561 = (moonbit_string_t*)moonbit_empty_ref_array;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* memory_intensive_data$1406 =
    (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  int32_t i$1407;
  int32_t _tmp$3554;
  moonbit_string_t _tmp$3555;
  struct moonbit_result_0 _tmp$4749;
  moonbit_string_t* _tmp$3560;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* mixed_data$1409;
  int32_t _tmp$3558;
  moonbit_string_t _tmp$3559;
  Moonbit_object_header(memory_intensive_data$1406)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  memory_intensive_data$1406->$0 = _tmp$3561;
  memory_intensive_data$1406->$1 = 0;
  i$1407 = 0;
  while (1) {
    if (i$1407 < 10000) {
      int32_t _tmp$3553;
      moonbit_incref(memory_intensive_data$1406);
      $$moonbitlang$core$builtin$Array$$push$0(
        memory_intensive_data$1406,
          (moonbit_string_t)moonbit_string_literal_113.data
      );
      _tmp$3553 = i$1407 + 1;
      i$1407 = _tmp$3553;
      continue;
    }
    break;
  }
  _tmp$3554
  = $$moonbitlang$core$builtin$Array$$length$1(
    memory_intensive_data$1406
  );
  _tmp$3555 = 0;
  _tmp$4749
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3554,
      10000,
      _tmp$3555,
      (moonbit_string_t)moonbit_string_literal_114.data
  );
  if (_tmp$4749.tag) {
    int32_t const _ok$3556 = _tmp$4749.data.ok;
  } else {
    void* const _err$3557 = _tmp$4749.data.err;
    struct moonbit_result_0 _result$4750;
    _result$4750.tag = 0;
    _result$4750.data.err = _err$3557;
    return _result$4750;
  }
  _tmp$3560 = (moonbit_string_t*)moonbit_make_ref_array_raw(6);
  _tmp$3560[0] = (moonbit_string_t)moonbit_string_literal_115.data;
  _tmp$3560[1] = (moonbit_string_t)moonbit_string_literal_116.data;
  _tmp$3560[2] = (moonbit_string_t)moonbit_string_literal_117.data;
  _tmp$3560[3] = (moonbit_string_t)moonbit_string_literal_29.data;
  _tmp$3560[4] = (moonbit_string_t)moonbit_string_literal_118.data;
  _tmp$3560[5] = (moonbit_string_t)moonbit_string_literal_119.data;
  mixed_data$1409
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(mixed_data$1409)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  mixed_data$1409->$0 = _tmp$3560;
  mixed_data$1409->$1 = 6;
  _tmp$3558 = $$moonbitlang$core$builtin$Array$$length$1(mixed_data$1409);
  _tmp$3559 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           _tmp$3558,
             6,
             _tmp$3559,
             (moonbit_string_t)moonbit_string_literal_120.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8(
  
) {
  int32_t* _tmp$3552 = (int32_t*)moonbit_empty_int32_array;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* large_array$1395 =
    (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  int32_t i$1396;
  int32_t _tmp$3508;
  moonbit_string_t _tmp$3509;
  struct moonbit_result_0 _tmp$4752;
  int32_t _tmp$3512;
  moonbit_string_t _tmp$3513;
  struct moonbit_result_0 _tmp$4754;
  int32_t _tmp$3516;
  moonbit_string_t _tmp$3517;
  struct moonbit_result_0 _tmp$4756;
  moonbit_string_t base_string$1398;
  moonbit_string_t repeated_string$1399;
  int32_t _tmp$4173;
  int32_t _tmp$3520;
  moonbit_string_t _tmp$3521;
  struct moonbit_result_0 _tmp$4758;
  struct $Ref$3c$Int$3e$* counter$1400;
  int32_t i$1401;
  int32_t _field$4172;
  int32_t val$3527;
  moonbit_string_t _tmp$3528;
  struct moonbit_result_0 _tmp$4761;
  moonbit_string_t* _tmp$3551;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* results$1403;
  int32_t i$1404;
  int32_t _tmp$3533;
  moonbit_string_t _tmp$3534;
  struct moonbit_result_0 _tmp$4764;
  moonbit_string_t _tmp$3537;
  moonbit_string_t _tmp$3538;
  struct moonbit_result_0 _tmp$4766;
  moonbit_string_t _tmp$3541;
  moonbit_string_t _tmp$3542;
  struct moonbit_result_0 _tmp$4768;
  moonbit_string_t _tmp$3545;
  moonbit_string_t _tmp$3546;
  struct moonbit_result_0 _tmp$4770;
  moonbit_string_t _tmp$3549;
  moonbit_string_t _tmp$3550;
  Moonbit_object_header(large_array$1395)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  large_array$1395->$0 = _tmp$3552;
  large_array$1395->$1 = 0;
  i$1396 = 0;
  while (1) {
    if (i$1396 < 1000) {
      int32_t _tmp$3507;
      moonbit_incref(large_array$1395);
      $$moonbitlang$core$builtin$Array$$push$3(large_array$1395, i$1396);
      _tmp$3507 = i$1396 + 1;
      i$1396 = _tmp$3507;
      continue;
    }
    break;
  }
  moonbit_incref(large_array$1395);
  _tmp$3508 = $$moonbitlang$core$builtin$Array$$length$2(large_array$1395);
  _tmp$3509 = 0;
  _tmp$4752
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3508,
      1000,
      _tmp$3509,
      (moonbit_string_t)moonbit_string_literal_121.data
  );
  if (_tmp$4752.tag) {
    int32_t const _ok$3510 = _tmp$4752.data.ok;
  } else {
    void* const _err$3511 = _tmp$4752.data.err;
    struct moonbit_result_0 _result$4753;
    moonbit_decref(large_array$1395);
    _result$4753.tag = 0;
    _result$4753.data.err = _err$3511;
    return _result$4753;
  }
  moonbit_incref(large_array$1395);
  _tmp$3512 = $$moonbitlang$core$builtin$Array$$at$1(large_array$1395, 0);
  _tmp$3513 = 0;
  _tmp$4754
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3512,
      0,
      _tmp$3513,
      (moonbit_string_t)moonbit_string_literal_122.data
  );
  if (_tmp$4754.tag) {
    int32_t const _ok$3514 = _tmp$4754.data.ok;
  } else {
    void* const _err$3515 = _tmp$4754.data.err;
    struct moonbit_result_0 _result$4755;
    moonbit_decref(large_array$1395);
    _result$4755.tag = 0;
    _result$4755.data.err = _err$3515;
    return _result$4755;
  }
  _tmp$3516 = $$moonbitlang$core$builtin$Array$$at$1(large_array$1395, 999);
  _tmp$3517 = 0;
  _tmp$4756
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3516,
      999,
      _tmp$3517,
      (moonbit_string_t)moonbit_string_literal_123.data
  );
  if (_tmp$4756.tag) {
    int32_t const _ok$3518 = _tmp$4756.data.ok;
  } else {
    void* const _err$3519 = _tmp$4756.data.err;
    struct moonbit_result_0 _result$4757;
    _result$4757.tag = 0;
    _result$4757.data.err = _err$3519;
    return _result$4757;
  }
  base_string$1398 = (moonbit_string_t)moonbit_string_literal_124.data;
  repeated_string$1399 = $String$$repeat(base_string$1398, 100);
  _tmp$4173 = Moonbit_array_length(repeated_string$1399);
  moonbit_decref(repeated_string$1399);
  _tmp$3520 = _tmp$4173;
  _tmp$3521 = 0;
  _tmp$4758
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3520,
      500,
      _tmp$3521,
      (moonbit_string_t)moonbit_string_literal_125.data
  );
  if (_tmp$4758.tag) {
    int32_t const _ok$3522 = _tmp$4758.data.ok;
  } else {
    void* const _err$3523 = _tmp$4758.data.err;
    struct moonbit_result_0 _result$4759;
    _result$4759.tag = 0;
    _result$4759.data.err = _err$3523;
    return _result$4759;
  }
  counter$1400
  = (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  Moonbit_object_header(counter$1400)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  counter$1400->$0 = 0;
  i$1401 = 0;
  while (1) {
    if (i$1401 < 1000) {
      int32_t val$3525 = counter$1400->$0;
      int32_t _tmp$3524 = val$3525 + 1;
      int32_t _tmp$3526;
      counter$1400->$0 = _tmp$3524;
      _tmp$3526 = i$1401 + 1;
      i$1401 = _tmp$3526;
      continue;
    }
    break;
  }
  _field$4172 = counter$1400->$0;
  moonbit_decref(counter$1400);
  val$3527 = _field$4172;
  _tmp$3528 = 0;
  _tmp$4761
  = $moonbitlang$core$builtin$assert_eq$0(
    val$3527,
      1000,
      _tmp$3528,
      (moonbit_string_t)moonbit_string_literal_126.data
  );
  if (_tmp$4761.tag) {
    int32_t const _ok$3529 = _tmp$4761.data.ok;
  } else {
    void* const _err$3530 = _tmp$4761.data.err;
    struct moonbit_result_0 _result$4762;
    _result$4762.tag = 0;
    _result$4762.data.err = _err$3530;
    return _result$4762;
  }
  _tmp$3551 = (moonbit_string_t*)moonbit_empty_ref_array;
  results$1403
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(results$1403)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  results$1403->$0 = _tmp$3551;
  results$1403->$1 = 0;
  i$1404 = 0;
  while (1) {
    if (i$1404 < 100) {
      int32_t _tmp$3531 = i$1404 % 2;
      int32_t _tmp$3532;
      if (_tmp$3531 == 0) {
        moonbit_incref(results$1403);
        $$moonbitlang$core$builtin$Array$$push$0(
          results$1403, (moonbit_string_t)moonbit_string_literal_127.data
        );
      } else {
        moonbit_incref(results$1403);
        $$moonbitlang$core$builtin$Array$$push$0(
          results$1403, (moonbit_string_t)moonbit_string_literal_128.data
        );
      }
      _tmp$3532 = i$1404 + 1;
      i$1404 = _tmp$3532;
      continue;
    }
    break;
  }
  moonbit_incref(results$1403);
  _tmp$3533 = $$moonbitlang$core$builtin$Array$$length$1(results$1403);
  _tmp$3534 = 0;
  _tmp$4764
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3533,
      100,
      _tmp$3534,
      (moonbit_string_t)moonbit_string_literal_129.data
  );
  if (_tmp$4764.tag) {
    int32_t const _ok$3535 = _tmp$4764.data.ok;
  } else {
    void* const _err$3536 = _tmp$4764.data.err;
    struct moonbit_result_0 _result$4765;
    moonbit_decref(results$1403);
    _result$4765.tag = 0;
    _result$4765.data.err = _err$3536;
    return _result$4765;
  }
  moonbit_incref(results$1403);
  _tmp$3537 = $$moonbitlang$core$builtin$Array$$at$0(results$1403, 0);
  _tmp$3538 = 0;
  _tmp$4766
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3537,
      (moonbit_string_t)moonbit_string_literal_127.data,
      _tmp$3538,
      (moonbit_string_t)moonbit_string_literal_130.data
  );
  if (_tmp$4766.tag) {
    int32_t const _ok$3539 = _tmp$4766.data.ok;
  } else {
    void* const _err$3540 = _tmp$4766.data.err;
    struct moonbit_result_0 _result$4767;
    moonbit_decref(results$1403);
    _result$4767.tag = 0;
    _result$4767.data.err = _err$3540;
    return _result$4767;
  }
  moonbit_incref(results$1403);
  _tmp$3541 = $$moonbitlang$core$builtin$Array$$at$0(results$1403, 1);
  _tmp$3542 = 0;
  _tmp$4768
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3541,
      (moonbit_string_t)moonbit_string_literal_128.data,
      _tmp$3542,
      (moonbit_string_t)moonbit_string_literal_131.data
  );
  if (_tmp$4768.tag) {
    int32_t const _ok$3543 = _tmp$4768.data.ok;
  } else {
    void* const _err$3544 = _tmp$4768.data.err;
    struct moonbit_result_0 _result$4769;
    moonbit_decref(results$1403);
    _result$4769.tag = 0;
    _result$4769.data.err = _err$3544;
    return _result$4769;
  }
  moonbit_incref(results$1403);
  _tmp$3545 = $$moonbitlang$core$builtin$Array$$at$0(results$1403, 98);
  _tmp$3546 = 0;
  _tmp$4770
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3545,
      (moonbit_string_t)moonbit_string_literal_127.data,
      _tmp$3546,
      (moonbit_string_t)moonbit_string_literal_132.data
  );
  if (_tmp$4770.tag) {
    int32_t const _ok$3547 = _tmp$4770.data.ok;
  } else {
    void* const _err$3548 = _tmp$4770.data.err;
    struct moonbit_result_0 _result$4771;
    moonbit_decref(results$1403);
    _result$4771.tag = 0;
    _result$4771.data.err = _err$3548;
    return _result$4771;
  }
  _tmp$3549 = $$moonbitlang$core$builtin$Array$$at$0(results$1403, 99);
  _tmp$3550 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           _tmp$3549,
             (moonbit_string_t)moonbit_string_literal_128.data,
             _tmp$3550,
             (moonbit_string_t)moonbit_string_literal_133.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7(
  
) {
  int64_t none_value$1386 = 4294967296ll;
  int32_t x$1389;
  int32_t unwrapped_result$1387;
  moonbit_string_t _tmp$3494;
  struct moonbit_result_0 _tmp$4773;
  int32_t very_large$1392;
  int32_t very_small$1393;
  int32_t _tmp$3497;
  moonbit_string_t _tmp$3498;
  struct moonbit_result_0 _tmp$4775;
  int32_t _tmp$3501;
  moonbit_string_t _tmp$3502;
  struct moonbit_result_0 _tmp$4777;
  moonbit_string_t long_string$1394;
  int32_t _tmp$4174;
  int32_t _tmp$3505;
  moonbit_string_t _tmp$3506;
  if (none_value$1386 == 4294967296ll) {
    unwrapped_result$1387 = 0;
  } else {
    int64_t _Some$1390 = none_value$1386;
    int32_t _x$1391 = (int32_t)_Some$1390;
    x$1389 = _x$1391;
    goto $join$1388;
  }
  goto $joinlet$4772;
  $join$1388:;
  unwrapped_result$1387 = x$1389;
  $joinlet$4772:;
  _tmp$3494 = 0;
  _tmp$4773
  = $moonbitlang$core$builtin$assert_eq$0(
    unwrapped_result$1387,
      0,
      _tmp$3494,
      (moonbit_string_t)moonbit_string_literal_134.data
  );
  if (_tmp$4773.tag) {
    int32_t const _ok$3495 = _tmp$4773.data.ok;
  } else {
    void* const _err$3496 = _tmp$4773.data.err;
    struct moonbit_result_0 _result$4774;
    _result$4774.tag = 0;
    _result$4774.data.err = _err$3496;
    return _result$4774;
  }
  very_large$1392 = 2147483647;
  very_small$1393 = (int32_t)0x80000000;
  _tmp$3497 = very_large$1392 > 0;
  _tmp$3498 = 0;
  _tmp$4775
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3497, _tmp$3498, (moonbit_string_t)moonbit_string_literal_135.data
  );
  if (_tmp$4775.tag) {
    int32_t const _ok$3499 = _tmp$4775.data.ok;
  } else {
    void* const _err$3500 = _tmp$4775.data.err;
    struct moonbit_result_0 _result$4776;
    _result$4776.tag = 0;
    _result$4776.data.err = _err$3500;
    return _result$4776;
  }
  _tmp$3501 = very_small$1393 < 0;
  _tmp$3502 = 0;
  _tmp$4777
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3501, _tmp$3502, (moonbit_string_t)moonbit_string_literal_136.data
  );
  if (_tmp$4777.tag) {
    int32_t const _ok$3503 = _tmp$4777.data.ok;
  } else {
    void* const _err$3504 = _tmp$4777.data.err;
    struct moonbit_result_0 _result$4778;
    _result$4778.tag = 0;
    _result$4778.data.err = _err$3504;
    return _result$4778;
  }
  long_string$1394
  = $String$$repeat(
    (moonbit_string_t)moonbit_string_literal_65.data, 1000
  );
  _tmp$4174 = Moonbit_array_length(long_string$1394);
  moonbit_decref(long_string$1394);
  _tmp$3505 = _tmp$4174;
  _tmp$3506 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           _tmp$3505,
             1000,
             _tmp$3506,
             (moonbit_string_t)moonbit_string_literal_137.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6(
  
) {
  int32_t* _tmp$3493 = (int32_t*)moonbit_make_int32_array_raw(3);
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$3488;
  int32_t* _tmp$3492;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$3489;
  int32_t* _tmp$3491;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$3490;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** _tmp$3487;
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* nested_arrays$1378;
  int32_t _tmp$3459;
  moonbit_string_t _tmp$3460;
  struct moonbit_result_0 _tmp$4779;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$3465;
  int32_t _tmp$3463;
  moonbit_string_t _tmp$3464;
  struct moonbit_result_0 _tmp$4781;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$3470;
  int32_t _tmp$3468;
  moonbit_string_t _tmp$3469;
  struct moonbit_result_0 _tmp$4783;
  int32_t* _tmp$3486;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* numbers$1379;
  int32_t* _tmp$3485;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* doubled$1380;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _arr$1381;
  int32_t _len$1382;
  int32_t _i$1383;
  int32_t _tmp$3475;
  moonbit_string_t _tmp$3476;
  struct moonbit_result_0 _tmp$4786;
  int32_t _tmp$3479;
  moonbit_string_t _tmp$3480;
  struct moonbit_result_0 _tmp$4788;
  int32_t _tmp$3483;
  moonbit_string_t _tmp$3484;
  _tmp$3493[0] = 1;
  _tmp$3493[1] = 2;
  _tmp$3493[2] = 3;
  _tmp$3488
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(_tmp$3488)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$3488->$0 = _tmp$3493;
  _tmp$3488->$1 = 3;
  _tmp$3492 = (int32_t*)moonbit_make_int32_array_raw(3);
  _tmp$3492[0] = 4;
  _tmp$3492[1] = 5;
  _tmp$3492[2] = 6;
  _tmp$3489
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(_tmp$3489)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$3489->$0 = _tmp$3492;
  _tmp$3489->$1 = 3;
  _tmp$3491 = (int32_t*)moonbit_make_int32_array_raw(3);
  _tmp$3491[0] = 7;
  _tmp$3491[1] = 8;
  _tmp$3491[2] = 9;
  _tmp$3490
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(_tmp$3490)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$3490->$0 = _tmp$3491;
  _tmp$3490->$1 = 3;
  _tmp$3487
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$**)moonbit_make_ref_array_raw(
      3
    );
  _tmp$3487[0] = _tmp$3488;
  _tmp$3487[1] = _tmp$3489;
  _tmp$3487[2] = _tmp$3490;
  nested_arrays$1378
  = (struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$
      )
    );
  Moonbit_object_header(nested_arrays$1378)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  nested_arrays$1378->$0 = _tmp$3487;
  nested_arrays$1378->$1 = 3;
  moonbit_incref(nested_arrays$1378);
  _tmp$3459 = $$moonbitlang$core$builtin$Array$$length$3(nested_arrays$1378);
  _tmp$3460 = 0;
  _tmp$4779
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3459,
      3,
      _tmp$3460,
      (moonbit_string_t)moonbit_string_literal_138.data
  );
  if (_tmp$4779.tag) {
    int32_t const _ok$3461 = _tmp$4779.data.ok;
  } else {
    void* const _err$3462 = _tmp$4779.data.err;
    struct moonbit_result_0 _result$4780;
    moonbit_decref(nested_arrays$1378);
    _result$4780.tag = 0;
    _result$4780.data.err = _err$3462;
    return _result$4780;
  }
  moonbit_incref(nested_arrays$1378);
  _tmp$3465 = $$moonbitlang$core$builtin$Array$$at$2(nested_arrays$1378, 0);
  _tmp$3463 = $$moonbitlang$core$builtin$Array$$length$2(_tmp$3465);
  _tmp$3464 = 0;
  _tmp$4781
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3463,
      3,
      _tmp$3464,
      (moonbit_string_t)moonbit_string_literal_139.data
  );
  if (_tmp$4781.tag) {
    int32_t const _ok$3466 = _tmp$4781.data.ok;
  } else {
    void* const _err$3467 = _tmp$4781.data.err;
    struct moonbit_result_0 _result$4782;
    moonbit_decref(nested_arrays$1378);
    _result$4782.tag = 0;
    _result$4782.data.err = _err$3467;
    return _result$4782;
  }
  _tmp$3470 = $$moonbitlang$core$builtin$Array$$at$2(nested_arrays$1378, 1);
  _tmp$3468 = $$moonbitlang$core$builtin$Array$$at$1(_tmp$3470, 1);
  _tmp$3469 = 0;
  _tmp$4783
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3468,
      5,
      _tmp$3469,
      (moonbit_string_t)moonbit_string_literal_140.data
  );
  if (_tmp$4783.tag) {
    int32_t const _ok$3471 = _tmp$4783.data.ok;
  } else {
    void* const _err$3472 = _tmp$4783.data.err;
    struct moonbit_result_0 _result$4784;
    _result$4784.tag = 0;
    _result$4784.data.err = _err$3472;
    return _result$4784;
  }
  _tmp$3486 = (int32_t*)moonbit_make_int32_array_raw(5);
  _tmp$3486[0] = 1;
  _tmp$3486[1] = 2;
  _tmp$3486[2] = 3;
  _tmp$3486[3] = 4;
  _tmp$3486[4] = 5;
  numbers$1379
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(numbers$1379)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  numbers$1379->$0 = _tmp$3486;
  numbers$1379->$1 = 5;
  _tmp$3485 = (int32_t*)moonbit_empty_int32_array;
  doubled$1380
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(doubled$1380)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  doubled$1380->$0 = _tmp$3485;
  doubled$1380->$1 = 0;
  _arr$1381 = numbers$1379;
  moonbit_incref(_arr$1381);
  _len$1382 = $$moonbitlang$core$builtin$Array$$length$2(_arr$1381);
  _i$1383 = 0;
  while (1) {
    if (_i$1383 < _len$1382) {
      int32_t num$1384;
      int32_t _tmp$3473;
      int32_t _tmp$3474;
      moonbit_incref(_arr$1381);
      num$1384
      = $$moonbitlang$core$builtin$Array$$unsafe_get$2(
        _arr$1381, _i$1383
      );
      _tmp$3473 = num$1384 * 2;
      moonbit_incref(doubled$1380);
      $$moonbitlang$core$builtin$Array$$push$3(doubled$1380, _tmp$3473);
      _tmp$3474 = _i$1383 + 1;
      _i$1383 = _tmp$3474;
      continue;
    } else {
      moonbit_decref(_arr$1381);
    }
    break;
  }
  moonbit_incref(doubled$1380);
  _tmp$3475 = $$moonbitlang$core$builtin$Array$$length$2(doubled$1380);
  _tmp$3476 = 0;
  _tmp$4786
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3475,
      5,
      _tmp$3476,
      (moonbit_string_t)moonbit_string_literal_141.data
  );
  if (_tmp$4786.tag) {
    int32_t const _ok$3477 = _tmp$4786.data.ok;
  } else {
    void* const _err$3478 = _tmp$4786.data.err;
    struct moonbit_result_0 _result$4787;
    moonbit_decref(doubled$1380);
    _result$4787.tag = 0;
    _result$4787.data.err = _err$3478;
    return _result$4787;
  }
  moonbit_incref(doubled$1380);
  _tmp$3479 = $$moonbitlang$core$builtin$Array$$at$1(doubled$1380, 0);
  _tmp$3480 = 0;
  _tmp$4788
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3479,
      2,
      _tmp$3480,
      (moonbit_string_t)moonbit_string_literal_142.data
  );
  if (_tmp$4788.tag) {
    int32_t const _ok$3481 = _tmp$4788.data.ok;
  } else {
    void* const _err$3482 = _tmp$4788.data.err;
    struct moonbit_result_0 _result$4789;
    moonbit_decref(doubled$1380);
    _result$4789.tag = 0;
    _result$4789.data.err = _err$3482;
    return _result$4789;
  }
  _tmp$3483 = $$moonbitlang$core$builtin$Array$$at$1(doubled$1380, 4);
  _tmp$3484 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           _tmp$3483,
             10,
             _tmp$3484,
             (moonbit_string_t)moonbit_string_literal_143.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5(
  
) {
  int32_t int_val$1375 = 42;
  double float_val$1376 = 0x1.91eb851eb851fp+1;
  int32_t _tmp$3427 = int_val$1375 + int_val$1375;
  moonbit_string_t _tmp$3428 = 0;
  struct moonbit_result_0 _tmp$4790 =
    $moonbitlang$core$builtin$assert_eq$0(
      _tmp$3427,
        84,
        _tmp$3428,
        (moonbit_string_t)moonbit_string_literal_144.data
    );
  double _tmp$3431;
  moonbit_string_t _tmp$3432;
  struct moonbit_result_0 _tmp$4792;
  moonbit_string_t str_num$1377;
  int32_t _tmp$4175;
  int32_t _tmp$3435;
  moonbit_string_t _tmp$3436;
  struct moonbit_result_0 _tmp$4794;
  moonbit_string_t _tmp$3439;
  struct moonbit_result_0 _tmp$4796;
  int32_t _tmp$3442;
  moonbit_string_t _tmp$3443;
  struct moonbit_result_0 _tmp$4798;
  int32_t _tmp$3446;
  moonbit_string_t _tmp$3447;
  struct moonbit_result_0 _tmp$4800;
  moonbit_string_t _tmp$3450;
  struct moonbit_result_0 _tmp$4802;
  int32_t _tmp$3453;
  moonbit_string_t _tmp$3454;
  struct moonbit_result_0 _tmp$4804;
  int32_t _tmp$3457;
  moonbit_string_t _tmp$3458;
  if (_tmp$4790.tag) {
    int32_t const _ok$3429 = _tmp$4790.data.ok;
  } else {
    void* const _err$3430 = _tmp$4790.data.err;
    struct moonbit_result_0 _result$4791;
    _result$4791.tag = 0;
    _result$4791.data.err = _err$3430;
    return _result$4791;
  }
  _tmp$3431 = float_val$1376 + float_val$1376;
  _tmp$3432 = 0;
  _tmp$4792
  = $moonbitlang$core$builtin$assert_eq$6(
    _tmp$3431,
      0x1.91eb851eb851fp+2,
      _tmp$3432,
      (moonbit_string_t)moonbit_string_literal_145.data
  );
  if (_tmp$4792.tag) {
    int32_t const _ok$3433 = _tmp$4792.data.ok;
  } else {
    void* const _err$3434 = _tmp$4792.data.err;
    struct moonbit_result_0 _result$4793;
    _result$4793.tag = 0;
    _result$4793.data.err = _err$3434;
    return _result$4793;
  }
  str_num$1377 = (moonbit_string_t)moonbit_string_literal_146.data;
  _tmp$4175 = Moonbit_array_length(str_num$1377);
  moonbit_decref(str_num$1377);
  _tmp$3435 = _tmp$4175;
  _tmp$3436 = 0;
  _tmp$4794
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3435,
      3,
      _tmp$3436,
      (moonbit_string_t)moonbit_string_literal_147.data
  );
  if (_tmp$4794.tag) {
    int32_t const _ok$3437 = _tmp$4794.data.ok;
  } else {
    void* const _err$3438 = _tmp$4794.data.err;
    struct moonbit_result_0 _result$4795;
    _result$4795.tag = 0;
    _result$4795.data.err = _err$3438;
    return _result$4795;
  }
  _tmp$3439 = 0;
  _tmp$4796
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3439, (moonbit_string_t)moonbit_string_literal_148.data
  );
  if (_tmp$4796.tag) {
    int32_t const _ok$3440 = _tmp$4796.data.ok;
  } else {
    void* const _err$3441 = _tmp$4796.data.err;
    struct moonbit_result_0 _result$4797;
    _result$4797.tag = 0;
    _result$4797.data.err = _err$3441;
    return _result$4797;
  }
  _tmp$3442
  = moonbit_val_array_equal(
    (moonbit_string_t)moonbit_string_literal_124.data,
      (moonbit_string_t)moonbit_string_literal_124.data
  );
  _tmp$3443 = 0;
  _tmp$4798
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3442, _tmp$3443, (moonbit_string_t)moonbit_string_literal_149.data
  );
  if (_tmp$4798.tag) {
    int32_t const _ok$3444 = _tmp$4798.data.ok;
  } else {
    void* const _err$3445 = _tmp$4798.data.err;
    struct moonbit_result_0 _result$4799;
    _result$4799.tag = 0;
    _result$4799.data.err = _err$3445;
    return _result$4799;
  }
  _tmp$3446 = 1 == 1;
  _tmp$3447 = 0;
  _tmp$4800
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3446, _tmp$3447, (moonbit_string_t)moonbit_string_literal_150.data
  );
  if (_tmp$4800.tag) {
    int32_t const _ok$3448 = _tmp$4800.data.ok;
  } else {
    void* const _err$3449 = _tmp$4800.data.err;
    struct moonbit_result_0 _result$4801;
    _result$4801.tag = 0;
    _result$4801.data.err = _err$3449;
    return _result$4801;
  }
  _tmp$3450 = 0;
  _tmp$4802
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3450, (moonbit_string_t)moonbit_string_literal_151.data
  );
  if (_tmp$4802.tag) {
    int32_t const _ok$3451 = _tmp$4802.data.ok;
  } else {
    void* const _err$3452 = _tmp$4802.data.err;
    struct moonbit_result_0 _result$4803;
    _result$4803.tag = 0;
    _result$4803.data.err = _err$3452;
    return _result$4803;
  }
  _tmp$3453
  = moonbit_val_array_equal(
    (moonbit_string_t)moonbit_string_literal_124.data,
      (moonbit_string_t)moonbit_string_literal_152.data
  );
  _tmp$3454 = 0;
  _tmp$4804
  = $moonbitlang$core$builtin$assert_false(
    _tmp$3453, _tmp$3454, (moonbit_string_t)moonbit_string_literal_153.data
  );
  if (_tmp$4804.tag) {
    int32_t const _ok$3455 = _tmp$4804.data.ok;
  } else {
    void* const _err$3456 = _tmp$4804.data.err;
    struct moonbit_result_0 _result$4805;
    _result$4805.tag = 0;
    _result$4805.data.err = _err$3456;
    return _result$4805;
  }
  _tmp$3457 = 1 == 0;
  _tmp$3458 = 0;
  return $moonbitlang$core$builtin$assert_false(
           _tmp$3457,
             _tmp$3458,
             (moonbit_string_t)moonbit_string_literal_154.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4(
  
) {
  int64_t some_int$1346 = (int64_t)42;
  moonbit_string_t some_string$1347 =
    (moonbit_string_t)moonbit_string_literal_124.data;
  int32_t some_bool$1348 = 1;
  int32_t _tmp$3378 =
    $moonbitlang$core$builtin$op_notequal$1(some_int$1346, 4294967296ll);
  moonbit_string_t _tmp$3379 = 0;
  struct moonbit_result_0 _tmp$4806 =
    $moonbitlang$core$builtin$assert_true(
      _tmp$3378, _tmp$3379, (moonbit_string_t)moonbit_string_literal_155.data
    );
  moonbit_string_t _tmp$3384;
  int32_t _tmp$3382;
  moonbit_string_t _tmp$3383;
  struct moonbit_result_0 _tmp$4808;
  int32_t _tmp$3387;
  moonbit_string_t _tmp$3388;
  struct moonbit_result_0 _tmp$4810;
  int64_t _tmp$3391;
  moonbit_string_t _tmp$3392;
  struct moonbit_result_0 _tmp$4812;
  moonbit_string_t _tmp$3395;
  moonbit_string_t _tmp$3396;
  struct moonbit_result_0 _tmp$4814;
  int32_t _tmp$3399;
  moonbit_string_t _tmp$3400;
  struct moonbit_result_0 _tmp$4816;
  int64_t none_int$1349;
  moonbit_string_t none_string$1350;
  int32_t none_bool$1351;
  int32_t _tmp$3403;
  moonbit_string_t _tmp$3404;
  struct moonbit_result_0 _tmp$4818;
  moonbit_string_t _tmp$3409;
  int32_t _tmp$3407;
  moonbit_string_t _tmp$3408;
  struct moonbit_result_0 _tmp$4820;
  int32_t _tmp$3412;
  moonbit_string_t _tmp$3413;
  struct moonbit_result_0 _tmp$4822;
  int32_t x$1354;
  int32_t int_result$1352;
  moonbit_string_t _tmp$3416;
  struct moonbit_result_0 _tmp$4825;
  moonbit_string_t s$1359;
  moonbit_string_t string_result$1357;
  moonbit_string_t _tmp$3419;
  struct moonbit_result_0 _tmp$4828;
  int32_t b$1364;
  int32_t bool_result$1362;
  moonbit_string_t _tmp$3422;
  struct moonbit_result_0 _tmp$4831;
  int64_t _tmp$3426;
  void* nested_option$1367;
  int32_t x$1370;
  int32_t nested_result$1368;
  moonbit_string_t _tmp$3425;
  if (_tmp$4806.tag) {
    int32_t const _ok$3380 = _tmp$4806.data.ok;
  } else {
    void* const _err$3381 = _tmp$4806.data.err;
    struct moonbit_result_0 _result$4807;
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4807.tag = 0;
    _result$4807.data.err = _err$3381;
    return _result$4807;
  }
  _tmp$3384 = 0;
  if (some_string$1347) {
    moonbit_incref(some_string$1347);
  }
  _tmp$3382
  = $moonbitlang$core$builtin$op_notequal$2(
    some_string$1347, _tmp$3384
  );
  _tmp$3383 = 0;
  _tmp$4808
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3382, _tmp$3383, (moonbit_string_t)moonbit_string_literal_156.data
  );
  if (_tmp$4808.tag) {
    int32_t const _ok$3385 = _tmp$4808.data.ok;
  } else {
    void* const _err$3386 = _tmp$4808.data.err;
    struct moonbit_result_0 _result$4809;
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4809.tag = 0;
    _result$4809.data.err = _err$3386;
    return _result$4809;
  }
  _tmp$3387 = $moonbitlang$core$builtin$op_notequal$3(some_bool$1348, -1);
  _tmp$3388 = 0;
  _tmp$4810
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3387, _tmp$3388, (moonbit_string_t)moonbit_string_literal_157.data
  );
  if (_tmp$4810.tag) {
    int32_t const _ok$3389 = _tmp$4810.data.ok;
  } else {
    void* const _err$3390 = _tmp$4810.data.err;
    struct moonbit_result_0 _result$4811;
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4811.tag = 0;
    _result$4811.data.err = _err$3390;
    return _result$4811;
  }
  _tmp$3391 = (int64_t)42;
  _tmp$3392 = 0;
  _tmp$4812
  = $moonbitlang$core$builtin$assert_eq$2(
    some_int$1346,
      _tmp$3391,
      _tmp$3392,
      (moonbit_string_t)moonbit_string_literal_158.data
  );
  if (_tmp$4812.tag) {
    int32_t const _ok$3393 = _tmp$4812.data.ok;
  } else {
    void* const _err$3394 = _tmp$4812.data.err;
    struct moonbit_result_0 _result$4813;
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4813.tag = 0;
    _result$4813.data.err = _err$3394;
    return _result$4813;
  }
  _tmp$3395 = (moonbit_string_t)moonbit_string_literal_124.data;
  _tmp$3396 = 0;
  if (some_string$1347) {
    moonbit_incref(some_string$1347);
  }
  _tmp$4814
  = $moonbitlang$core$builtin$assert_eq$3(
    some_string$1347,
      _tmp$3395,
      _tmp$3396,
      (moonbit_string_t)moonbit_string_literal_159.data
  );
  if (_tmp$4814.tag) {
    int32_t const _ok$3397 = _tmp$4814.data.ok;
  } else {
    void* const _err$3398 = _tmp$4814.data.err;
    struct moonbit_result_0 _result$4815;
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4815.tag = 0;
    _result$4815.data.err = _err$3398;
    return _result$4815;
  }
  _tmp$3399 = 1;
  _tmp$3400 = 0;
  _tmp$4816
  = $moonbitlang$core$builtin$assert_eq$4(
    some_bool$1348,
      _tmp$3399,
      _tmp$3400,
      (moonbit_string_t)moonbit_string_literal_160.data
  );
  if (_tmp$4816.tag) {
    int32_t const _ok$3401 = _tmp$4816.data.ok;
  } else {
    void* const _err$3402 = _tmp$4816.data.err;
    struct moonbit_result_0 _result$4817;
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4817.tag = 0;
    _result$4817.data.err = _err$3402;
    return _result$4817;
  }
  none_int$1349 = 4294967296ll;
  none_string$1350 = 0;
  none_bool$1351 = -1;
  _tmp$3403
  = $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
    none_int$1349, 4294967296ll
  );
  _tmp$3404 = 0;
  _tmp$4818
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3403, _tmp$3404, (moonbit_string_t)moonbit_string_literal_161.data
  );
  if (_tmp$4818.tag) {
    int32_t const _ok$3405 = _tmp$4818.data.ok;
  } else {
    void* const _err$3406 = _tmp$4818.data.err;
    struct moonbit_result_0 _result$4819;
    if (none_string$1350) {
      moonbit_decref(none_string$1350);
    }
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4819.tag = 0;
    _result$4819.data.err = _err$3406;
    return _result$4819;
  }
  _tmp$3409 = 0;
  _tmp$3407
  = $$moonbitlang$core$builtin$Eq$$Option$$equal$1(
    none_string$1350, _tmp$3409
  );
  _tmp$3408 = 0;
  _tmp$4820
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3407, _tmp$3408, (moonbit_string_t)moonbit_string_literal_162.data
  );
  if (_tmp$4820.tag) {
    int32_t const _ok$3410 = _tmp$4820.data.ok;
  } else {
    void* const _err$3411 = _tmp$4820.data.err;
    struct moonbit_result_0 _result$4821;
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4821.tag = 0;
    _result$4821.data.err = _err$3411;
    return _result$4821;
  }
  _tmp$3412
  = $$moonbitlang$core$builtin$Eq$$Option$$equal$2(
    none_bool$1351, -1
  );
  _tmp$3413 = 0;
  _tmp$4822
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3412, _tmp$3413, (moonbit_string_t)moonbit_string_literal_163.data
  );
  if (_tmp$4822.tag) {
    int32_t const _ok$3414 = _tmp$4822.data.ok;
  } else {
    void* const _err$3415 = _tmp$4822.data.err;
    struct moonbit_result_0 _result$4823;
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4823.tag = 0;
    _result$4823.data.err = _err$3415;
    return _result$4823;
  }
  if (some_int$1346 == 4294967296ll) {
    int_result$1352 = 0;
  } else {
    int64_t _Some$1355 = some_int$1346;
    int32_t _x$1356 = (int32_t)_Some$1355;
    x$1354 = _x$1356;
    goto $join$1353;
  }
  goto $joinlet$4824;
  $join$1353:;
  int_result$1352 = x$1354 + 10;
  $joinlet$4824:;
  _tmp$3416 = 0;
  _tmp$4825
  = $moonbitlang$core$builtin$assert_eq$0(
    int_result$1352,
      52,
      _tmp$3416,
      (moonbit_string_t)moonbit_string_literal_164.data
  );
  if (_tmp$4825.tag) {
    int32_t const _ok$3417 = _tmp$4825.data.ok;
  } else {
    void* const _err$3418 = _tmp$4825.data.err;
    struct moonbit_result_0 _result$4826;
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    _result$4826.tag = 0;
    _result$4826.data.err = _err$3418;
    return _result$4826;
  }
  if (some_string$1347 == 0) {
    if (some_string$1347) {
      moonbit_decref(some_string$1347);
    }
    string_result$1357 = (moonbit_string_t)moonbit_string_literal_165.data;
  } else {
    moonbit_string_t _Some$1360 = some_string$1347;
    moonbit_string_t _s$1361 = _Some$1360;
    s$1359 = _s$1361;
    goto $join$1358;
  }
  goto $joinlet$4827;
  $join$1358:;
  string_result$1357
  = moonbit_add_string(
    s$1359, (moonbit_string_t)moonbit_string_literal_166.data
  );
  $joinlet$4827:;
  _tmp$3419 = 0;
  _tmp$4828
  = $moonbitlang$core$builtin$assert_eq$1(
    string_result$1357,
      (moonbit_string_t)moonbit_string_literal_167.data,
      _tmp$3419,
      (moonbit_string_t)moonbit_string_literal_168.data
  );
  if (_tmp$4828.tag) {
    int32_t const _ok$3420 = _tmp$4828.data.ok;
  } else {
    void* const _err$3421 = _tmp$4828.data.err;
    struct moonbit_result_0 _result$4829;
    _result$4829.tag = 0;
    _result$4829.data.err = _err$3421;
    return _result$4829;
  }
  if (some_bool$1348 == -1) {
    bool_result$1362 = 0;
  } else {
    int32_t _Some$1365 = some_bool$1348;
    int32_t _b$1366 = _Some$1365;
    b$1364 = _b$1366;
    goto $join$1363;
  }
  goto $joinlet$4830;
  $join$1363:;
  bool_result$1362 = !b$1364;
  $joinlet$4830:;
  _tmp$3422 = 0;
  _tmp$4831
  = $moonbitlang$core$builtin$assert_eq$5(
    bool_result$1362,
      0,
      _tmp$3422,
      (moonbit_string_t)moonbit_string_literal_169.data
  );
  if (_tmp$4831.tag) {
    int32_t const _ok$3423 = _tmp$4831.data.ok;
  } else {
    void* const _err$3424 = _tmp$4831.data.err;
    struct moonbit_result_0 _result$4832;
    _result$4832.tag = 0;
    _result$4832.data.err = _err$3424;
    return _result$4832;
  }
  _tmp$3426 = (int64_t)42;
  nested_option$1367
  = (void*)moonbit_malloc(
      sizeof(struct $Option$3c$Option$3c$Int$3e$$3e$$Some)
    );
  Moonbit_object_header(nested_option$1367)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Option$3c$Option$3c$Int$3e$$3e$$Some) >> 2, 0, 1
  );
  ((struct $Option$3c$Option$3c$Int$3e$$3e$$Some*)nested_option$1367)->$0
  = _tmp$3426;
  switch (Moonbit_object_tag(nested_option$1367)) {
    case 1: {
      struct $Option$3c$Option$3c$Int$3e$$3e$$Some* _Some$1371 =
        (struct $Option$3c$Option$3c$Int$3e$$3e$$Some*)nested_option$1367;
      int64_t _field$4176 = _Some$1371->$0;
      int64_t _x$1372;
      moonbit_decref(_Some$1371);
      _x$1372 = _field$4176;
      if (_x$1372 == 4294967296ll) {
        nested_result$1368 = 0;
      } else {
        int64_t _Some$1373 = _x$1372;
        int32_t _x$1374 = (int32_t)_Some$1373;
        x$1370 = _x$1374;
        goto $join$1369;
      }
      break;
    }
    default: {
      moonbit_decref(nested_option$1367);
      nested_result$1368 = -1;
      break;
    }
  }
  goto $joinlet$4833;
  $join$1369:;
  nested_result$1368 = x$1370 * 2;
  $joinlet$4833:;
  _tmp$3425 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           nested_result$1368,
             84,
             _tmp$3425,
             (moonbit_string_t)moonbit_string_literal_170.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3(
  
) {
  moonbit_string_t* _tmp$3377 =
    (moonbit_string_t*)moonbit_make_ref_array_raw(3);
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* string_array$1342;
  int32_t* _tmp$3376;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* int_array$1343;
  moonbit_string_t _tmp$3340;
  moonbit_string_t _tmp$3341;
  struct moonbit_result_0 _tmp$4834;
  moonbit_string_t _tmp$3344;
  moonbit_string_t _tmp$3345;
  struct moonbit_result_0 _tmp$4836;
  moonbit_string_t _tmp$3348;
  moonbit_string_t _tmp$3349;
  struct moonbit_result_0 _tmp$4838;
  int32_t _tmp$3352;
  moonbit_string_t _tmp$3353;
  struct moonbit_result_0 _tmp$4840;
  int32_t _tmp$3356;
  moonbit_string_t _tmp$3357;
  struct moonbit_result_0 _tmp$4842;
  int32_t _tmp$3360;
  moonbit_string_t _tmp$3361;
  struct moonbit_result_0 _tmp$4844;
  moonbit_string_t* _tmp$3375;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* empty_array$1344;
  int32_t _tmp$3364;
  moonbit_string_t _tmp$3365;
  struct moonbit_result_0 _tmp$4846;
  moonbit_string_t* _tmp$3374;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* single_array$1345;
  int32_t _tmp$3368;
  moonbit_string_t _tmp$3369;
  struct moonbit_result_0 _tmp$4848;
  moonbit_string_t _tmp$3372;
  moonbit_string_t _tmp$3373;
  _tmp$3377[0] = (moonbit_string_t)moonbit_string_literal_171.data;
  _tmp$3377[1] = (moonbit_string_t)moonbit_string_literal_172.data;
  _tmp$3377[2] = (moonbit_string_t)moonbit_string_literal_173.data;
  string_array$1342
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(string_array$1342)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  string_array$1342->$0 = _tmp$3377;
  string_array$1342->$1 = 3;
  _tmp$3376 = (int32_t*)moonbit_make_int32_array_raw(5);
  _tmp$3376[0] = 1;
  _tmp$3376[1] = 2;
  _tmp$3376[2] = 3;
  _tmp$3376[3] = 4;
  _tmp$3376[4] = 5;
  int_array$1343
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(int_array$1343)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  int_array$1343->$0 = _tmp$3376;
  int_array$1343->$1 = 5;
  moonbit_incref(string_array$1342);
  _tmp$3340 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1342, 0);
  _tmp$3341 = 0;
  _tmp$4834
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3340,
      (moonbit_string_t)moonbit_string_literal_171.data,
      _tmp$3341,
      (moonbit_string_t)moonbit_string_literal_174.data
  );
  if (_tmp$4834.tag) {
    int32_t const _ok$3342 = _tmp$4834.data.ok;
  } else {
    void* const _err$3343 = _tmp$4834.data.err;
    struct moonbit_result_0 _result$4835;
    moonbit_decref(int_array$1343);
    moonbit_decref(string_array$1342);
    _result$4835.tag = 0;
    _result$4835.data.err = _err$3343;
    return _result$4835;
  }
  moonbit_incref(string_array$1342);
  _tmp$3344 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1342, 1);
  _tmp$3345 = 0;
  _tmp$4836
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3344,
      (moonbit_string_t)moonbit_string_literal_172.data,
      _tmp$3345,
      (moonbit_string_t)moonbit_string_literal_175.data
  );
  if (_tmp$4836.tag) {
    int32_t const _ok$3346 = _tmp$4836.data.ok;
  } else {
    void* const _err$3347 = _tmp$4836.data.err;
    struct moonbit_result_0 _result$4837;
    moonbit_decref(int_array$1343);
    moonbit_decref(string_array$1342);
    _result$4837.tag = 0;
    _result$4837.data.err = _err$3347;
    return _result$4837;
  }
  _tmp$3348 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1342, 2);
  _tmp$3349 = 0;
  _tmp$4838
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3348,
      (moonbit_string_t)moonbit_string_literal_173.data,
      _tmp$3349,
      (moonbit_string_t)moonbit_string_literal_176.data
  );
  if (_tmp$4838.tag) {
    int32_t const _ok$3350 = _tmp$4838.data.ok;
  } else {
    void* const _err$3351 = _tmp$4838.data.err;
    struct moonbit_result_0 _result$4839;
    moonbit_decref(int_array$1343);
    _result$4839.tag = 0;
    _result$4839.data.err = _err$3351;
    return _result$4839;
  }
  moonbit_incref(int_array$1343);
  _tmp$3352 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1343, 0);
  _tmp$3353 = 0;
  _tmp$4840
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3352,
      1,
      _tmp$3353,
      (moonbit_string_t)moonbit_string_literal_177.data
  );
  if (_tmp$4840.tag) {
    int32_t const _ok$3354 = _tmp$4840.data.ok;
  } else {
    void* const _err$3355 = _tmp$4840.data.err;
    struct moonbit_result_0 _result$4841;
    moonbit_decref(int_array$1343);
    _result$4841.tag = 0;
    _result$4841.data.err = _err$3355;
    return _result$4841;
  }
  moonbit_incref(int_array$1343);
  _tmp$3356 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1343, 4);
  _tmp$3357 = 0;
  _tmp$4842
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3356,
      5,
      _tmp$3357,
      (moonbit_string_t)moonbit_string_literal_178.data
  );
  if (_tmp$4842.tag) {
    int32_t const _ok$3358 = _tmp$4842.data.ok;
  } else {
    void* const _err$3359 = _tmp$4842.data.err;
    struct moonbit_result_0 _result$4843;
    moonbit_decref(int_array$1343);
    _result$4843.tag = 0;
    _result$4843.data.err = _err$3359;
    return _result$4843;
  }
  _tmp$3360 = $$moonbitlang$core$builtin$Array$$length$2(int_array$1343);
  _tmp$3361 = 0;
  _tmp$4844
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3360,
      5,
      _tmp$3361,
      (moonbit_string_t)moonbit_string_literal_179.data
  );
  if (_tmp$4844.tag) {
    int32_t const _ok$3362 = _tmp$4844.data.ok;
  } else {
    void* const _err$3363 = _tmp$4844.data.err;
    struct moonbit_result_0 _result$4845;
    _result$4845.tag = 0;
    _result$4845.data.err = _err$3363;
    return _result$4845;
  }
  _tmp$3375 = (moonbit_string_t*)moonbit_empty_ref_array;
  empty_array$1344
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(empty_array$1344)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  empty_array$1344->$0 = _tmp$3375;
  empty_array$1344->$1 = 0;
  _tmp$3364 = $$moonbitlang$core$builtin$Array$$length$1(empty_array$1344);
  _tmp$3365 = 0;
  _tmp$4846
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3364,
      0,
      _tmp$3365,
      (moonbit_string_t)moonbit_string_literal_180.data
  );
  if (_tmp$4846.tag) {
    int32_t const _ok$3366 = _tmp$4846.data.ok;
  } else {
    void* const _err$3367 = _tmp$4846.data.err;
    struct moonbit_result_0 _result$4847;
    _result$4847.tag = 0;
    _result$4847.data.err = _err$3367;
    return _result$4847;
  }
  _tmp$3374 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$3374[0] = (moonbit_string_t)moonbit_string_literal_181.data;
  single_array$1345
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(single_array$1345)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  single_array$1345->$0 = _tmp$3374;
  single_array$1345->$1 = 1;
  moonbit_incref(single_array$1345);
  _tmp$3368 = $$moonbitlang$core$builtin$Array$$length$1(single_array$1345);
  _tmp$3369 = 0;
  _tmp$4848
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3368,
      1,
      _tmp$3369,
      (moonbit_string_t)moonbit_string_literal_182.data
  );
  if (_tmp$4848.tag) {
    int32_t const _ok$3370 = _tmp$4848.data.ok;
  } else {
    void* const _err$3371 = _tmp$4848.data.err;
    struct moonbit_result_0 _result$4849;
    moonbit_decref(single_array$1345);
    _result$4849.tag = 0;
    _result$4849.data.err = _err$3371;
    return _result$4849;
  }
  _tmp$3372 = $$moonbitlang$core$builtin$Array$$at$0(single_array$1345, 0);
  _tmp$3373 = 0;
  return $moonbitlang$core$builtin$assert_eq$1(
           _tmp$3372,
             (moonbit_string_t)moonbit_string_literal_181.data,
             _tmp$3373,
             (moonbit_string_t)moonbit_string_literal_183.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2(
  
) {
  moonbit_string_t empty_str$1337 =
    (moonbit_string_t)moonbit_string_literal_3.data;
  int32_t _tmp$3310 = Moonbit_array_length(empty_str$1337);
  moonbit_string_t _tmp$3311 = 0;
  struct moonbit_result_0 _tmp$4850 =
    $moonbitlang$core$builtin$assert_eq$0(
      _tmp$3310,
        0,
        _tmp$3311,
        (moonbit_string_t)moonbit_string_literal_184.data
    );
  moonbit_string_t _tmp$3314;
  moonbit_string_t _tmp$3315;
  struct moonbit_result_0 _tmp$4852;
  moonbit_string_t _tmp$3318;
  moonbit_string_t _tmp$3319;
  struct moonbit_result_0 _tmp$4854;
  moonbit_string_t single_char$1338;
  int32_t _tmp$3322;
  moonbit_string_t _tmp$3323;
  struct moonbit_result_0 _tmp$4856;
  moonbit_string_t _tmp$3326;
  moonbit_string_t _tmp$3327;
  struct moonbit_result_0 _tmp$4858;
  moonbit_string_t spaced_str$1339;
  int32_t _tmp$4179;
  int32_t _tmp$3330;
  moonbit_string_t _tmp$3331;
  struct moonbit_result_0 _tmp$4860;
  moonbit_string_t special_str$1340;
  int32_t _tmp$4178;
  int32_t _tmp$3334;
  moonbit_string_t _tmp$3335;
  struct moonbit_result_0 _tmp$4862;
  moonbit_string_t unicode_str$1341;
  int32_t _tmp$4177;
  int32_t _tmp$3338;
  moonbit_string_t _tmp$3339;
  if (_tmp$4850.tag) {
    int32_t const _ok$3312 = _tmp$4850.data.ok;
  } else {
    void* const _err$3313 = _tmp$4850.data.err;
    struct moonbit_result_0 _result$4851;
    moonbit_decref(empty_str$1337);
    _result$4851.tag = 0;
    _result$4851.data.err = _err$3313;
    return _result$4851;
  }
  moonbit_incref(empty_str$1337);
  _tmp$3314
  = moonbit_add_string(
    empty_str$1337, (moonbit_string_t)moonbit_string_literal_94.data
  );
  _tmp$3315 = 0;
  _tmp$4852
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3314,
      (moonbit_string_t)moonbit_string_literal_94.data,
      _tmp$3315,
      (moonbit_string_t)moonbit_string_literal_185.data
  );
  if (_tmp$4852.tag) {
    int32_t const _ok$3316 = _tmp$4852.data.ok;
  } else {
    void* const _err$3317 = _tmp$4852.data.err;
    struct moonbit_result_0 _result$4853;
    moonbit_decref(empty_str$1337);
    _result$4853.tag = 0;
    _result$4853.data.err = _err$3317;
    return _result$4853;
  }
  _tmp$3318
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_94.data, empty_str$1337
  );
  _tmp$3319 = 0;
  _tmp$4854
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3318,
      (moonbit_string_t)moonbit_string_literal_94.data,
      _tmp$3319,
      (moonbit_string_t)moonbit_string_literal_186.data
  );
  if (_tmp$4854.tag) {
    int32_t const _ok$3320 = _tmp$4854.data.ok;
  } else {
    void* const _err$3321 = _tmp$4854.data.err;
    struct moonbit_result_0 _result$4855;
    _result$4855.tag = 0;
    _result$4855.data.err = _err$3321;
    return _result$4855;
  }
  single_char$1338 = (moonbit_string_t)moonbit_string_literal_65.data;
  _tmp$3322 = Moonbit_array_length(single_char$1338);
  _tmp$3323 = 0;
  _tmp$4856
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3322,
      1,
      _tmp$3323,
      (moonbit_string_t)moonbit_string_literal_187.data
  );
  if (_tmp$4856.tag) {
    int32_t const _ok$3324 = _tmp$4856.data.ok;
  } else {
    void* const _err$3325 = _tmp$4856.data.err;
    struct moonbit_result_0 _result$4857;
    moonbit_decref(single_char$1338);
    _result$4857.tag = 0;
    _result$4857.data.err = _err$3325;
    return _result$4857;
  }
  moonbit_incref(single_char$1338);
  _tmp$3326 = moonbit_add_string(single_char$1338, single_char$1338);
  _tmp$3327 = 0;
  _tmp$4858
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3326,
      (moonbit_string_t)moonbit_string_literal_188.data,
      _tmp$3327,
      (moonbit_string_t)moonbit_string_literal_189.data
  );
  if (_tmp$4858.tag) {
    int32_t const _ok$3328 = _tmp$4858.data.ok;
  } else {
    void* const _err$3329 = _tmp$4858.data.err;
    struct moonbit_result_0 _result$4859;
    _result$4859.tag = 0;
    _result$4859.data.err = _err$3329;
    return _result$4859;
  }
  spaced_str$1339 = (moonbit_string_t)moonbit_string_literal_190.data;
  _tmp$4179 = Moonbit_array_length(spaced_str$1339);
  moonbit_decref(spaced_str$1339);
  _tmp$3330 = _tmp$4179;
  _tmp$3331 = 0;
  _tmp$4860
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3330,
      13,
      _tmp$3331,
      (moonbit_string_t)moonbit_string_literal_191.data
  );
  if (_tmp$4860.tag) {
    int32_t const _ok$3332 = _tmp$4860.data.ok;
  } else {
    void* const _err$3333 = _tmp$4860.data.err;
    struct moonbit_result_0 _result$4861;
    _result$4861.tag = 0;
    _result$4861.data.err = _err$3333;
    return _result$4861;
  }
  special_str$1340 = (moonbit_string_t)moonbit_string_literal_192.data;
  _tmp$4178 = Moonbit_array_length(special_str$1340);
  moonbit_decref(special_str$1340);
  _tmp$3334 = _tmp$4178;
  _tmp$3335 = 0;
  _tmp$4862
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3334,
      20,
      _tmp$3335,
      (moonbit_string_t)moonbit_string_literal_193.data
  );
  if (_tmp$4862.tag) {
    int32_t const _ok$3336 = _tmp$4862.data.ok;
  } else {
    void* const _err$3337 = _tmp$4862.data.err;
    struct moonbit_result_0 _result$4863;
    _result$4863.tag = 0;
    _result$4863.data.err = _err$3337;
    return _result$4863;
  }
  unicode_str$1341 = (moonbit_string_t)moonbit_string_literal_194.data;
  _tmp$4177 = Moonbit_array_length(unicode_str$1341);
  moonbit_decref(unicode_str$1341);
  _tmp$3338 = _tmp$4177;
  _tmp$3339 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           _tmp$3338,
             11,
             _tmp$3339,
             (moonbit_string_t)moonbit_string_literal_195.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1(
  
) {
  moonbit_string_t _tmp$3265 = 0;
  struct moonbit_result_0 _tmp$4864 =
    $moonbitlang$core$builtin$assert_true(
      1, _tmp$3265, (moonbit_string_t)moonbit_string_literal_196.data
    );
  moonbit_string_t _tmp$3268;
  struct moonbit_result_0 _tmp$4866;
  moonbit_string_t _tmp$3271;
  struct moonbit_result_0 _tmp$4868;
  moonbit_string_t _tmp$3274;
  struct moonbit_result_0 _tmp$4870;
  moonbit_string_t _tmp$3277;
  struct moonbit_result_0 _tmp$4872;
  moonbit_string_t _tmp$3280;
  struct moonbit_result_0 _tmp$4874;
  moonbit_string_t _tmp$3283;
  struct moonbit_result_0 _tmp$4876;
  moonbit_string_t _tmp$3286;
  struct moonbit_result_0 _tmp$4878;
  moonbit_string_t _tmp$3289;
  struct moonbit_result_0 _tmp$4880;
  moonbit_string_t _tmp$3292;
  struct moonbit_result_0 _tmp$4882;
  moonbit_string_t _tmp$3295;
  struct moonbit_result_0 _tmp$4884;
  moonbit_string_t _tmp$3298;
  struct moonbit_result_0 _tmp$4886;
  moonbit_string_t _tmp$3301;
  struct moonbit_result_0 _tmp$4888;
  int32_t large_num$1336;
  int32_t _tmp$3304;
  moonbit_string_t _tmp$3305;
  struct moonbit_result_0 _tmp$4890;
  int32_t _tmp$3308;
  moonbit_string_t _tmp$3309;
  if (_tmp$4864.tag) {
    int32_t const _ok$3266 = _tmp$4864.data.ok;
  } else {
    void* const _err$3267 = _tmp$4864.data.err;
    struct moonbit_result_0 _result$4865;
    _result$4865.tag = 0;
    _result$4865.data.err = _err$3267;
    return _result$4865;
  }
  _tmp$3268 = 0;
  _tmp$4866
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3268, (moonbit_string_t)moonbit_string_literal_197.data
  );
  if (_tmp$4866.tag) {
    int32_t const _ok$3269 = _tmp$4866.data.ok;
  } else {
    void* const _err$3270 = _tmp$4866.data.err;
    struct moonbit_result_0 _result$4867;
    _result$4867.tag = 0;
    _result$4867.data.err = _err$3270;
    return _result$4867;
  }
  _tmp$3271 = 0;
  _tmp$4868
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3271, (moonbit_string_t)moonbit_string_literal_198.data
  );
  if (_tmp$4868.tag) {
    int32_t const _ok$3272 = _tmp$4868.data.ok;
  } else {
    void* const _err$3273 = _tmp$4868.data.err;
    struct moonbit_result_0 _result$4869;
    _result$4869.tag = 0;
    _result$4869.data.err = _err$3273;
    return _result$4869;
  }
  _tmp$3274 = 0;
  _tmp$4870
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3274, (moonbit_string_t)moonbit_string_literal_199.data
  );
  if (_tmp$4870.tag) {
    int32_t const _ok$3275 = _tmp$4870.data.ok;
  } else {
    void* const _err$3276 = _tmp$4870.data.err;
    struct moonbit_result_0 _result$4871;
    _result$4871.tag = 0;
    _result$4871.data.err = _err$3276;
    return _result$4871;
  }
  _tmp$3277 = 0;
  _tmp$4872
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3277, (moonbit_string_t)moonbit_string_literal_200.data
  );
  if (_tmp$4872.tag) {
    int32_t const _ok$3278 = _tmp$4872.data.ok;
  } else {
    void* const _err$3279 = _tmp$4872.data.err;
    struct moonbit_result_0 _result$4873;
    _result$4873.tag = 0;
    _result$4873.data.err = _err$3279;
    return _result$4873;
  }
  _tmp$3280 = 0;
  _tmp$4874
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3280, (moonbit_string_t)moonbit_string_literal_201.data
  );
  if (_tmp$4874.tag) {
    int32_t const _ok$3281 = _tmp$4874.data.ok;
  } else {
    void* const _err$3282 = _tmp$4874.data.err;
    struct moonbit_result_0 _result$4875;
    _result$4875.tag = 0;
    _result$4875.data.err = _err$3282;
    return _result$4875;
  }
  _tmp$3283 = 0;
  _tmp$4876
  = $moonbitlang$core$builtin$assert_eq$0(
    0, 0, _tmp$3283, (moonbit_string_t)moonbit_string_literal_202.data
  );
  if (_tmp$4876.tag) {
    int32_t const _ok$3284 = _tmp$4876.data.ok;
  } else {
    void* const _err$3285 = _tmp$4876.data.err;
    struct moonbit_result_0 _result$4877;
    _result$4877.tag = 0;
    _result$4877.data.err = _err$3285;
    return _result$4877;
  }
  _tmp$3286 = 0;
  _tmp$4878
  = $moonbitlang$core$builtin$assert_eq$0(
    0, 0, _tmp$3286, (moonbit_string_t)moonbit_string_literal_203.data
  );
  if (_tmp$4878.tag) {
    int32_t const _ok$3287 = _tmp$4878.data.ok;
  } else {
    void* const _err$3288 = _tmp$4878.data.err;
    struct moonbit_result_0 _result$4879;
    _result$4879.tag = 0;
    _result$4879.data.err = _err$3288;
    return _result$4879;
  }
  _tmp$3289 = 0;
  _tmp$4880
  = $moonbitlang$core$builtin$assert_eq$0(
    0, 0, _tmp$3289, (moonbit_string_t)moonbit_string_literal_204.data
  );
  if (_tmp$4880.tag) {
    int32_t const _ok$3290 = _tmp$4880.data.ok;
  } else {
    void* const _err$3291 = _tmp$4880.data.err;
    struct moonbit_result_0 _result$4881;
    _result$4881.tag = 0;
    _result$4881.data.err = _err$3291;
    return _result$4881;
  }
  _tmp$3292 = 0;
  _tmp$4882
  = $moonbitlang$core$builtin$assert_eq$0(
    0, 0, _tmp$3292, (moonbit_string_t)moonbit_string_literal_205.data
  );
  if (_tmp$4882.tag) {
    int32_t const _ok$3293 = _tmp$4882.data.ok;
  } else {
    void* const _err$3294 = _tmp$4882.data.err;
    struct moonbit_result_0 _result$4883;
    _result$4883.tag = 0;
    _result$4883.data.err = _err$3294;
    return _result$4883;
  }
  _tmp$3295 = 0;
  _tmp$4884
  = $moonbitlang$core$builtin$assert_eq$0(
    -2, -2, _tmp$3295, (moonbit_string_t)moonbit_string_literal_206.data
  );
  if (_tmp$4884.tag) {
    int32_t const _ok$3296 = _tmp$4884.data.ok;
  } else {
    void* const _err$3297 = _tmp$4884.data.err;
    struct moonbit_result_0 _result$4885;
    _result$4885.tag = 0;
    _result$4885.data.err = _err$3297;
    return _result$4885;
  }
  _tmp$3298 = 0;
  _tmp$4886
  = $moonbitlang$core$builtin$assert_eq$0(
    -8, -8, _tmp$3298, (moonbit_string_t)moonbit_string_literal_207.data
  );
  if (_tmp$4886.tag) {
    int32_t const _ok$3299 = _tmp$4886.data.ok;
  } else {
    void* const _err$3300 = _tmp$4886.data.err;
    struct moonbit_result_0 _result$4887;
    _result$4887.tag = 0;
    _result$4887.data.err = _err$3300;
    return _result$4887;
  }
  _tmp$3301 = 0;
  _tmp$4888
  = $moonbitlang$core$builtin$assert_eq$0(
    -15, -15, _tmp$3301, (moonbit_string_t)moonbit_string_literal_208.data
  );
  if (_tmp$4888.tag) {
    int32_t const _ok$3302 = _tmp$4888.data.ok;
  } else {
    void* const _err$3303 = _tmp$4888.data.err;
    struct moonbit_result_0 _result$4889;
    _result$4889.tag = 0;
    _result$4889.data.err = _err$3303;
    return _result$4889;
  }
  large_num$1336 = 1000000;
  _tmp$3304 = large_num$1336 + large_num$1336;
  _tmp$3305 = 0;
  _tmp$4890
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3304,
      2000000,
      _tmp$3305,
      (moonbit_string_t)moonbit_string_literal_209.data
  );
  if (_tmp$4890.tag) {
    int32_t const _ok$3306 = _tmp$4890.data.ok;
  } else {
    void* const _err$3307 = _tmp$4890.data.err;
    struct moonbit_result_0 _result$4891;
    _result$4891.tag = 0;
    _result$4891.data.err = _err$3307;
    return _result$4891;
  }
  _tmp$3308 = large_num$1336 * 2;
  _tmp$3309 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           _tmp$3308,
             2000000,
             _tmp$3309,
             (moonbit_string_t)moonbit_string_literal_210.data
         );
}

struct moonbit_result_0 $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0(
  
) {
  moonbit_string_t _tmp$3183 = 0;
  struct moonbit_result_0 _tmp$4892 =
    $moonbitlang$core$builtin$assert_eq$0(
      2, 2, _tmp$3183, (moonbit_string_t)moonbit_string_literal_211.data
    );
  moonbit_string_t _tmp$3186;
  struct moonbit_result_0 _tmp$4894;
  moonbit_string_t _tmp$3189;
  struct moonbit_result_0 _tmp$4896;
  moonbit_string_t _tmp$3192;
  struct moonbit_result_0 _tmp$4898;
  moonbit_string_t str1$1324;
  moonbit_string_t str2$1325;
  moonbit_string_t combined$1326;
  moonbit_string_t _tmp$3195;
  struct moonbit_result_0 _tmp$4900;
  int32_t _tmp$4181;
  int32_t _tmp$3198;
  moonbit_string_t _tmp$3199;
  struct moonbit_result_0 _tmp$4902;
  int32_t _tmp$4180;
  int32_t _tmp$3202;
  moonbit_string_t _tmp$3203;
  struct moonbit_result_0 _tmp$4904;
  int32_t* _tmp$3264;
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* int_array$1327;
  moonbit_string_t* _tmp$3263;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* string_array$1328;
  int32_t _tmp$3206;
  moonbit_string_t _tmp$3207;
  struct moonbit_result_0 _tmp$4906;
  int32_t _tmp$3210;
  moonbit_string_t _tmp$3211;
  struct moonbit_result_0 _tmp$4908;
  int32_t _tmp$3214;
  moonbit_string_t _tmp$3215;
  struct moonbit_result_0 _tmp$4910;
  int32_t _tmp$3218;
  moonbit_string_t _tmp$3219;
  struct moonbit_result_0 _tmp$4912;
  moonbit_string_t _tmp$3222;
  moonbit_string_t _tmp$3223;
  struct moonbit_result_0 _tmp$4914;
  moonbit_string_t _tmp$3226;
  struct moonbit_result_0 _tmp$4916;
  moonbit_string_t _tmp$3229;
  struct moonbit_result_0 _tmp$4918;
  moonbit_string_t _tmp$3232;
  struct moonbit_result_0 _tmp$4920;
  moonbit_string_t _tmp$3235;
  struct moonbit_result_0 _tmp$4922;
  moonbit_string_t _tmp$3238;
  struct moonbit_result_0 _tmp$4924;
  moonbit_string_t _tmp$3241;
  struct moonbit_result_0 _tmp$4926;
  moonbit_string_t _tmp$3244;
  struct moonbit_result_0 _tmp$4928;
  moonbit_string_t _tmp$3247;
  struct moonbit_result_0 _tmp$4930;
  int64_t some_value$1329;
  int64_t none_value$1330;
  int32_t _tmp$3250;
  moonbit_string_t _tmp$3251;
  struct moonbit_result_0 _tmp$4932;
  int32_t _tmp$3254;
  moonbit_string_t _tmp$3255;
  struct moonbit_result_0 _tmp$4934;
  int64_t _tmp$3258;
  moonbit_string_t _tmp$3259;
  struct moonbit_result_0 _tmp$4936;
  int32_t x$1333;
  int32_t result$1331;
  moonbit_string_t _tmp$3262;
  if (_tmp$4892.tag) {
    int32_t const _ok$3184 = _tmp$4892.data.ok;
  } else {
    void* const _err$3185 = _tmp$4892.data.err;
    struct moonbit_result_0 _result$4893;
    _result$4893.tag = 0;
    _result$4893.data.err = _err$3185;
    return _result$4893;
  }
  _tmp$3186 = 0;
  _tmp$4894
  = $moonbitlang$core$builtin$assert_eq$0(
    6, 6, _tmp$3186, (moonbit_string_t)moonbit_string_literal_212.data
  );
  if (_tmp$4894.tag) {
    int32_t const _ok$3187 = _tmp$4894.data.ok;
  } else {
    void* const _err$3188 = _tmp$4894.data.err;
    struct moonbit_result_0 _result$4895;
    _result$4895.tag = 0;
    _result$4895.data.err = _err$3188;
    return _result$4895;
  }
  _tmp$3189 = 0;
  _tmp$4896
  = $moonbitlang$core$builtin$assert_eq$0(
    6, 6, _tmp$3189, (moonbit_string_t)moonbit_string_literal_213.data
  );
  if (_tmp$4896.tag) {
    int32_t const _ok$3190 = _tmp$4896.data.ok;
  } else {
    void* const _err$3191 = _tmp$4896.data.err;
    struct moonbit_result_0 _result$4897;
    _result$4897.tag = 0;
    _result$4897.data.err = _err$3191;
    return _result$4897;
  }
  _tmp$3192 = 0;
  _tmp$4898
  = $moonbitlang$core$builtin$assert_eq$0(
    4, 4, _tmp$3192, (moonbit_string_t)moonbit_string_literal_214.data
  );
  if (_tmp$4898.tag) {
    int32_t const _ok$3193 = _tmp$4898.data.ok;
  } else {
    void* const _err$3194 = _tmp$4898.data.err;
    struct moonbit_result_0 _result$4899;
    _result$4899.tag = 0;
    _result$4899.data.err = _err$3194;
    return _result$4899;
  }
  str1$1324 = (moonbit_string_t)moonbit_string_literal_89.data;
  str2$1325 = (moonbit_string_t)moonbit_string_literal_215.data;
  moonbit_incref(str1$1324);
  moonbit_incref(str2$1325);
  combined$1326 = moonbit_add_string(str1$1324, str2$1325);
  _tmp$3195 = 0;
  _tmp$4900
  = $moonbitlang$core$builtin$assert_eq$1(
    combined$1326,
      (moonbit_string_t)moonbit_string_literal_216.data,
      _tmp$3195,
      (moonbit_string_t)moonbit_string_literal_217.data
  );
  if (_tmp$4900.tag) {
    int32_t const _ok$3196 = _tmp$4900.data.ok;
  } else {
    void* const _err$3197 = _tmp$4900.data.err;
    struct moonbit_result_0 _result$4901;
    moonbit_decref(str2$1325);
    moonbit_decref(str1$1324);
    _result$4901.tag = 0;
    _result$4901.data.err = _err$3197;
    return _result$4901;
  }
  _tmp$4181 = Moonbit_array_length(str1$1324);
  moonbit_decref(str1$1324);
  _tmp$3198 = _tmp$4181;
  _tmp$3199 = 0;
  _tmp$4902
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3198,
      5,
      _tmp$3199,
      (moonbit_string_t)moonbit_string_literal_218.data
  );
  if (_tmp$4902.tag) {
    int32_t const _ok$3200 = _tmp$4902.data.ok;
  } else {
    void* const _err$3201 = _tmp$4902.data.err;
    struct moonbit_result_0 _result$4903;
    moonbit_decref(str2$1325);
    _result$4903.tag = 0;
    _result$4903.data.err = _err$3201;
    return _result$4903;
  }
  _tmp$4180 = Moonbit_array_length(str2$1325);
  moonbit_decref(str2$1325);
  _tmp$3202 = _tmp$4180;
  _tmp$3203 = 0;
  _tmp$4904
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3202,
      6,
      _tmp$3203,
      (moonbit_string_t)moonbit_string_literal_219.data
  );
  if (_tmp$4904.tag) {
    int32_t const _ok$3204 = _tmp$4904.data.ok;
  } else {
    void* const _err$3205 = _tmp$4904.data.err;
    struct moonbit_result_0 _result$4905;
    _result$4905.tag = 0;
    _result$4905.data.err = _err$3205;
    return _result$4905;
  }
  _tmp$3264 = (int32_t*)moonbit_make_int32_array_raw(5);
  _tmp$3264[0] = 1;
  _tmp$3264[1] = 2;
  _tmp$3264[2] = 3;
  _tmp$3264[3] = 4;
  _tmp$3264[4] = 5;
  int_array$1327
  = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$)
    );
  Moonbit_object_header(int_array$1327)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$Int$3e$, $0) >> 2,
      1,
      0
  );
  int_array$1327->$0 = _tmp$3264;
  int_array$1327->$1 = 5;
  _tmp$3263 = (moonbit_string_t*)moonbit_make_ref_array_raw(3);
  _tmp$3263[0] = (moonbit_string_t)moonbit_string_literal_65.data;
  _tmp$3263[1] = (moonbit_string_t)moonbit_string_literal_66.data;
  _tmp$3263[2] = (moonbit_string_t)moonbit_string_literal_67.data;
  string_array$1328
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(string_array$1328)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  string_array$1328->$0 = _tmp$3263;
  string_array$1328->$1 = 3;
  moonbit_incref(int_array$1327);
  _tmp$3206 = $$moonbitlang$core$builtin$Array$$length$2(int_array$1327);
  _tmp$3207 = 0;
  _tmp$4906
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3206,
      5,
      _tmp$3207,
      (moonbit_string_t)moonbit_string_literal_220.data
  );
  if (_tmp$4906.tag) {
    int32_t const _ok$3208 = _tmp$4906.data.ok;
  } else {
    void* const _err$3209 = _tmp$4906.data.err;
    struct moonbit_result_0 _result$4907;
    moonbit_decref(string_array$1328);
    moonbit_decref(int_array$1327);
    _result$4907.tag = 0;
    _result$4907.data.err = _err$3209;
    return _result$4907;
  }
  moonbit_incref(string_array$1328);
  _tmp$3210 = $$moonbitlang$core$builtin$Array$$length$1(string_array$1328);
  _tmp$3211 = 0;
  _tmp$4908
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3210,
      3,
      _tmp$3211,
      (moonbit_string_t)moonbit_string_literal_221.data
  );
  if (_tmp$4908.tag) {
    int32_t const _ok$3212 = _tmp$4908.data.ok;
  } else {
    void* const _err$3213 = _tmp$4908.data.err;
    struct moonbit_result_0 _result$4909;
    moonbit_decref(string_array$1328);
    moonbit_decref(int_array$1327);
    _result$4909.tag = 0;
    _result$4909.data.err = _err$3213;
    return _result$4909;
  }
  moonbit_incref(int_array$1327);
  _tmp$3214 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1327, 0);
  _tmp$3215 = 0;
  _tmp$4910
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3214,
      1,
      _tmp$3215,
      (moonbit_string_t)moonbit_string_literal_222.data
  );
  if (_tmp$4910.tag) {
    int32_t const _ok$3216 = _tmp$4910.data.ok;
  } else {
    void* const _err$3217 = _tmp$4910.data.err;
    struct moonbit_result_0 _result$4911;
    moonbit_decref(string_array$1328);
    moonbit_decref(int_array$1327);
    _result$4911.tag = 0;
    _result$4911.data.err = _err$3217;
    return _result$4911;
  }
  _tmp$3218 = $$moonbitlang$core$builtin$Array$$at$1(int_array$1327, 4);
  _tmp$3219 = 0;
  _tmp$4912
  = $moonbitlang$core$builtin$assert_eq$0(
    _tmp$3218,
      5,
      _tmp$3219,
      (moonbit_string_t)moonbit_string_literal_223.data
  );
  if (_tmp$4912.tag) {
    int32_t const _ok$3220 = _tmp$4912.data.ok;
  } else {
    void* const _err$3221 = _tmp$4912.data.err;
    struct moonbit_result_0 _result$4913;
    moonbit_decref(string_array$1328);
    _result$4913.tag = 0;
    _result$4913.data.err = _err$3221;
    return _result$4913;
  }
  _tmp$3222 = $$moonbitlang$core$builtin$Array$$at$0(string_array$1328, 1);
  _tmp$3223 = 0;
  _tmp$4914
  = $moonbitlang$core$builtin$assert_eq$1(
    _tmp$3222,
      (moonbit_string_t)moonbit_string_literal_66.data,
      _tmp$3223,
      (moonbit_string_t)moonbit_string_literal_224.data
  );
  if (_tmp$4914.tag) {
    int32_t const _ok$3224 = _tmp$4914.data.ok;
  } else {
    void* const _err$3225 = _tmp$4914.data.err;
    struct moonbit_result_0 _result$4915;
    _result$4915.tag = 0;
    _result$4915.data.err = _err$3225;
    return _result$4915;
  }
  _tmp$3226 = 0;
  _tmp$4916
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3226, (moonbit_string_t)moonbit_string_literal_225.data
  );
  if (_tmp$4916.tag) {
    int32_t const _ok$3227 = _tmp$4916.data.ok;
  } else {
    void* const _err$3228 = _tmp$4916.data.err;
    struct moonbit_result_0 _result$4917;
    _result$4917.tag = 0;
    _result$4917.data.err = _err$3228;
    return _result$4917;
  }
  _tmp$3229 = 0;
  _tmp$4918
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3229, (moonbit_string_t)moonbit_string_literal_226.data
  );
  if (_tmp$4918.tag) {
    int32_t const _ok$3230 = _tmp$4918.data.ok;
  } else {
    void* const _err$3231 = _tmp$4918.data.err;
    struct moonbit_result_0 _result$4919;
    _result$4919.tag = 0;
    _result$4919.data.err = _err$3231;
    return _result$4919;
  }
  _tmp$3232 = 0;
  _tmp$4920
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3232, (moonbit_string_t)moonbit_string_literal_227.data
  );
  if (_tmp$4920.tag) {
    int32_t const _ok$3233 = _tmp$4920.data.ok;
  } else {
    void* const _err$3234 = _tmp$4920.data.err;
    struct moonbit_result_0 _result$4921;
    _result$4921.tag = 0;
    _result$4921.data.err = _err$3234;
    return _result$4921;
  }
  _tmp$3235 = 0;
  _tmp$4922
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3235, (moonbit_string_t)moonbit_string_literal_228.data
  );
  if (_tmp$4922.tag) {
    int32_t const _ok$3236 = _tmp$4922.data.ok;
  } else {
    void* const _err$3237 = _tmp$4922.data.err;
    struct moonbit_result_0 _result$4923;
    _result$4923.tag = 0;
    _result$4923.data.err = _err$3237;
    return _result$4923;
  }
  _tmp$3238 = 0;
  _tmp$4924
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3238, (moonbit_string_t)moonbit_string_literal_229.data
  );
  if (_tmp$4924.tag) {
    int32_t const _ok$3239 = _tmp$4924.data.ok;
  } else {
    void* const _err$3240 = _tmp$4924.data.err;
    struct moonbit_result_0 _result$4925;
    _result$4925.tag = 0;
    _result$4925.data.err = _err$3240;
    return _result$4925;
  }
  _tmp$3241 = 0;
  _tmp$4926
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3241, (moonbit_string_t)moonbit_string_literal_230.data
  );
  if (_tmp$4926.tag) {
    int32_t const _ok$3242 = _tmp$4926.data.ok;
  } else {
    void* const _err$3243 = _tmp$4926.data.err;
    struct moonbit_result_0 _result$4927;
    _result$4927.tag = 0;
    _result$4927.data.err = _err$3243;
    return _result$4927;
  }
  _tmp$3244 = 0;
  _tmp$4928
  = $moonbitlang$core$builtin$assert_true(
    1, _tmp$3244, (moonbit_string_t)moonbit_string_literal_231.data
  );
  if (_tmp$4928.tag) {
    int32_t const _ok$3245 = _tmp$4928.data.ok;
  } else {
    void* const _err$3246 = _tmp$4928.data.err;
    struct moonbit_result_0 _result$4929;
    _result$4929.tag = 0;
    _result$4929.data.err = _err$3246;
    return _result$4929;
  }
  _tmp$3247 = 0;
  _tmp$4930
  = $moonbitlang$core$builtin$assert_false(
    0, _tmp$3247, (moonbit_string_t)moonbit_string_literal_232.data
  );
  if (_tmp$4930.tag) {
    int32_t const _ok$3248 = _tmp$4930.data.ok;
  } else {
    void* const _err$3249 = _tmp$4930.data.err;
    struct moonbit_result_0 _result$4931;
    _result$4931.tag = 0;
    _result$4931.data.err = _err$3249;
    return _result$4931;
  }
  some_value$1329 = (int64_t)42;
  none_value$1330 = 4294967296ll;
  _tmp$3250
  = $moonbitlang$core$builtin$op_notequal$1(
    some_value$1329, 4294967296ll
  );
  _tmp$3251 = 0;
  _tmp$4932
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3250, _tmp$3251, (moonbit_string_t)moonbit_string_literal_233.data
  );
  if (_tmp$4932.tag) {
    int32_t const _ok$3252 = _tmp$4932.data.ok;
  } else {
    void* const _err$3253 = _tmp$4932.data.err;
    struct moonbit_result_0 _result$4933;
    _result$4933.tag = 0;
    _result$4933.data.err = _err$3253;
    return _result$4933;
  }
  _tmp$3254
  = $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
    none_value$1330, 4294967296ll
  );
  _tmp$3255 = 0;
  _tmp$4934
  = $moonbitlang$core$builtin$assert_true(
    _tmp$3254, _tmp$3255, (moonbit_string_t)moonbit_string_literal_234.data
  );
  if (_tmp$4934.tag) {
    int32_t const _ok$3256 = _tmp$4934.data.ok;
  } else {
    void* const _err$3257 = _tmp$4934.data.err;
    struct moonbit_result_0 _result$4935;
    _result$4935.tag = 0;
    _result$4935.data.err = _err$3257;
    return _result$4935;
  }
  _tmp$3258 = (int64_t)42;
  _tmp$3259 = 0;
  _tmp$4936
  = $moonbitlang$core$builtin$assert_eq$2(
    some_value$1329,
      _tmp$3258,
      _tmp$3259,
      (moonbit_string_t)moonbit_string_literal_235.data
  );
  if (_tmp$4936.tag) {
    int32_t const _ok$3260 = _tmp$4936.data.ok;
  } else {
    void* const _err$3261 = _tmp$4936.data.err;
    struct moonbit_result_0 _result$4937;
    _result$4937.tag = 0;
    _result$4937.data.err = _err$3261;
    return _result$4937;
  }
  if (some_value$1329 == 4294967296ll) {
    result$1331 = 0;
  } else {
    int64_t _Some$1334 = some_value$1329;
    int32_t _x$1335 = (int32_t)_Some$1334;
    x$1333 = _x$1335;
    goto $join$1332;
  }
  goto $joinlet$4938;
  $join$1332:;
  result$1331 = x$1333 * 2;
  $joinlet$4938:;
  _tmp$3262 = 0;
  return $moonbitlang$core$builtin$assert_eq$0(
           result$1331,
             84,
             _tmp$3262,
             (moonbit_string_t)moonbit_string_literal_236.data
         );
}

int32_t $$moonbitlang$core$builtin$Show$$Double$$output(
  double self$1323,
  struct $$moonbitlang$core$builtin$Logger logger$1322
) {
  moonbit_string_t _tmp$3182 = $Double$$to_string(self$1323);
  logger$1322.$0->$method_0(logger$1322.$1, _tmp$3182);
  return 0;
}

moonbit_string_t $Double$$to_string(double self$1321) {
  return $moonbitlang$core$double$internal$ryu$ryu_to_string(self$1321);
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$ryu_to_string(
  double val$1308
) {
  uint64_t bits$1309;
  uint64_t _tmp$3181;
  uint64_t _tmp$3180;
  int32_t ieeeSign$1310;
  uint64_t ieeeMantissa$1311;
  uint64_t _tmp$3179;
  uint64_t _tmp$3178;
  int32_t ieeeExponent$1312;
  int32_t _if_result$4939;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* v$1313;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* small$1314;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3177;
  if (val$1308 == 0x0p+0) {
    return (moonbit_string_t)moonbit_string_literal_237.data;
  }
  bits$1309 = *(int64_t*)&val$1308;
  _tmp$3181 = bits$1309 >> 63;
  _tmp$3180 = _tmp$3181 & 1ull;
  ieeeSign$1310 = _tmp$3180 != 0ull;
  ieeeMantissa$1311 = bits$1309 & 4503599627370495ull;
  _tmp$3179 = bits$1309 >> 52;
  _tmp$3178 = _tmp$3179 & 2047ull;
  ieeeExponent$1312 = (int32_t)_tmp$3178;
  if (ieeeExponent$1312 == 2047) {
    _if_result$4939 = 1;
  } else if (ieeeExponent$1312 == 0) {
    _if_result$4939 = ieeeMantissa$1311 == 0ull;
  } else {
    _if_result$4939 = 0;
  }
  if (_if_result$4939) {
    int32_t _tmp$3166 = ieeeExponent$1312 != 0;
    int32_t _tmp$3167 = ieeeMantissa$1311 != 0ull;
    return $moonbitlang$core$double$internal$ryu$copy_special_str(
             ieeeSign$1310, _tmp$3166, _tmp$3167
           );
  }
  v$1313 = $moonbitlang$core$double$internal$ryu$ryu_to_string$record$1307;
  small$1314
  = $moonbitlang$core$double$internal$ryu$d2d_small_int(
    ieeeMantissa$1311, ieeeExponent$1312
  );
  if (small$1314 == 0) {
    uint32_t _tmp$3168;
    if (small$1314) {
      moonbit_decref(small$1314);
    }
    _tmp$3168 = *(uint32_t*)&ieeeExponent$1312;
    v$1313
    = $moonbitlang$core$double$internal$ryu$d2d(
      ieeeMantissa$1311, _tmp$3168
    );
  } else {
    struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _Some$1315 =
      small$1314;
    struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _f$1316 =
      _Some$1315;
    struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* x$1317 =
      _f$1316;
    while (1) {
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3176 =
        x$1317;
      uint64_t _field$4184 = _tmp$3176->$0;
      uint64_t mantissa$3175 = _field$4184;
      uint64_t q$1318 = mantissa$3175 / 10ull;
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3174 =
        x$1317;
      uint64_t _field$4183 = _tmp$3174->$0;
      uint64_t mantissa$3172 = _field$4183;
      uint64_t _tmp$3173 = 10ull * q$1318;
      uint64_t r$1319 = mantissa$3172 - _tmp$3173;
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3171;
      int32_t _field$4182;
      int32_t exponent$3170;
      int32_t _tmp$3169;
      if (r$1319 != 0ull) {
        break;
      }
      _tmp$3171 = x$1317;
      _field$4182 = _tmp$3171->$1;
      moonbit_decref(_tmp$3171);
      exponent$3170 = _field$4182;
      _tmp$3169 = exponent$3170 + 1;
      x$1317
      = (struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64*)moonbit_malloc(
          sizeof(
            struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
          )
        );
      Moonbit_object_header(x$1317)->meta
      = Moonbit_make_regular_object_header(
        sizeof(
          struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
        )
        >> 2,
          0,
          0
      );
      x$1317->$0 = q$1318;
      x$1317->$1 = _tmp$3169;
      continue;
      break;
    }
    v$1313 = x$1317;
  }
  _tmp$3177 = v$1313;
  return $moonbitlang$core$double$internal$ryu$to_chars(
           _tmp$3177, ieeeSign$1310
         );
}

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d_small_int(
  uint64_t ieeeMantissa$1302,
  int32_t ieeeExponent$1304
) {
  uint64_t m2$1301 = 4503599627370496ull | ieeeMantissa$1302;
  int32_t _tmp$3165 = ieeeExponent$1304 - 1023;
  int32_t e2$1303 = _tmp$3165 - 52;
  int32_t _tmp$3164;
  uint64_t _tmp$3163;
  uint64_t mask$1305;
  uint64_t fraction$1306;
  int32_t _tmp$3162;
  uint64_t _tmp$3161;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _tmp$3160;
  if (e2$1303 > 0) {
    return 0;
  }
  if (e2$1303 < -52) {
    return 0;
  }
  _tmp$3164 = -e2$1303;
  _tmp$3163 = 1ull << (_tmp$3164 & 63);
  mask$1305 = _tmp$3163 - 1ull;
  fraction$1306 = m2$1301 & mask$1305;
  if (fraction$1306 != 0ull) {
    return 0;
  }
  _tmp$3162 = -e2$1303;
  _tmp$3161 = m2$1301 >> (_tmp$3162 & 63);
  _tmp$3160
  = (struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64)
    );
  Moonbit_object_header(_tmp$3160)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
    )
    >> 2,
      0,
      0
  );
  _tmp$3160->$0 = _tmp$3161;
  _tmp$3160->$1 = 0;
  return _tmp$3160;
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$to_chars(
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* v$1275,
  int32_t sign$1273
) {
  int32_t _tmp$3159 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  moonbit_bytes_t result$1271 =
    (moonbit_bytes_t)moonbit_make_bytes(25, _tmp$3159);
  int32_t index$1272 = 0;
  uint64_t output$1274;
  uint64_t _tmp$3158;
  int32_t olength$1276;
  int32_t _field$4185;
  int32_t exponent$3157;
  int32_t _tmp$3156;
  int32_t exp$1277;
  int32_t _tmp$3155;
  int32_t _tmp$3153;
  int32_t scientificNotation$1278;
  if (sign$1273) {
    int32_t _tmp$3028 = index$1272;
    int32_t _tmp$3029;
    if (_tmp$3028 < 0 || _tmp$3028 >= Moonbit_array_length(result$1271)) {
      moonbit_panic();
    }
    result$1271[_tmp$3028] = 45;
    _tmp$3029 = index$1272;
    index$1272 = _tmp$3029 + 1;
  }
  output$1274 = v$1275->$0;
  _tmp$3158 = output$1274;
  olength$1276
  = $moonbitlang$core$double$internal$ryu$decimal_length17(
    _tmp$3158
  );
  _field$4185 = v$1275->$1;
  moonbit_decref(v$1275);
  exponent$3157 = _field$4185;
  _tmp$3156 = exponent$3157 + olength$1276;
  exp$1277 = _tmp$3156 - 1;
  _tmp$3155 = exp$1277;
  if (_tmp$3155 >= -6) {
    int32_t _tmp$3154 = exp$1277;
    _tmp$3153 = _tmp$3154 < 21;
  } else {
    _tmp$3153 = 0;
  }
  scientificNotation$1278 = !_tmp$3153;
  if (scientificNotation$1278) {
    int32_t _end41$1279 = olength$1276 - 1;
    int32_t i$1280 = 0;
    int32_t _tmp$3039;
    uint64_t _tmp$3044;
    int32_t _tmp$3043;
    int32_t _tmp$3042;
    int32_t _tmp$3041;
    int32_t _tmp$3040;
    int32_t _tmp$3048;
    int32_t _tmp$3049;
    int32_t _tmp$3050;
    int32_t _tmp$3051;
    int32_t _tmp$3052;
    int32_t _tmp$3058;
    int32_t _tmp$3091;
    while (1) {
      if (i$1280 < _end41$1279) {
        uint64_t _tmp$3037 = output$1274;
        uint64_t c$1281 = _tmp$3037 % 10ull;
        uint64_t _tmp$3030 = output$1274;
        int32_t _tmp$3036;
        int32_t _tmp$3035;
        int32_t _tmp$3031;
        int32_t _tmp$3034;
        int32_t _tmp$3033;
        int32_t _tmp$3032;
        int32_t _tmp$3038;
        output$1274 = _tmp$3030 / 10ull;
        _tmp$3036 = index$1272;
        _tmp$3035 = _tmp$3036 + olength$1276;
        _tmp$3031 = _tmp$3035 - i$1280;
        _tmp$3034 = (int32_t)c$1281;
        _tmp$3033 = 48 + _tmp$3034;
        _tmp$3032 = _tmp$3033 & 0xff;
        if (_tmp$3031 < 0 || _tmp$3031 >= Moonbit_array_length(result$1271)) {
          moonbit_panic();
        }
        result$1271[_tmp$3031] = _tmp$3032;
        _tmp$3038 = i$1280 + 1;
        i$1280 = _tmp$3038;
        continue;
      }
      break;
    }
    _tmp$3039 = index$1272;
    _tmp$3044 = output$1274;
    _tmp$3043 = (int32_t)_tmp$3044;
    _tmp$3042 = _tmp$3043 % 10;
    _tmp$3041 = 48 + _tmp$3042;
    _tmp$3040 = _tmp$3041 & 0xff;
    if (_tmp$3039 < 0 || _tmp$3039 >= Moonbit_array_length(result$1271)) {
      moonbit_panic();
    }
    result$1271[_tmp$3039] = _tmp$3040;
    if (olength$1276 > 1) {
      int32_t _tmp$3046 = index$1272;
      int32_t _tmp$3045 = _tmp$3046 + 1;
      if (_tmp$3045 < 0 || _tmp$3045 >= Moonbit_array_length(result$1271)) {
        moonbit_panic();
      }
      result$1271[_tmp$3045] = 46;
    } else {
      int32_t _tmp$3047 = index$1272;
      index$1272 = _tmp$3047 - 1;
    }
    _tmp$3048 = index$1272;
    _tmp$3049 = olength$1276 + 1;
    index$1272 = _tmp$3048 + _tmp$3049;
    _tmp$3050 = index$1272;
    if (_tmp$3050 < 0 || _tmp$3050 >= Moonbit_array_length(result$1271)) {
      moonbit_panic();
    }
    result$1271[_tmp$3050] = 101;
    _tmp$3051 = index$1272;
    index$1272 = _tmp$3051 + 1;
    _tmp$3052 = exp$1277;
    if (_tmp$3052 < 0) {
      int32_t _tmp$3053 = index$1272;
      int32_t _tmp$3054;
      int32_t _tmp$3055;
      if (_tmp$3053 < 0 || _tmp$3053 >= Moonbit_array_length(result$1271)) {
        moonbit_panic();
      }
      result$1271[_tmp$3053] = 45;
      _tmp$3054 = index$1272;
      index$1272 = _tmp$3054 + 1;
      _tmp$3055 = exp$1277;
      exp$1277 = -_tmp$3055;
    } else {
      int32_t _tmp$3056 = index$1272;
      int32_t _tmp$3057;
      if (_tmp$3056 < 0 || _tmp$3056 >= Moonbit_array_length(result$1271)) {
        moonbit_panic();
      }
      result$1271[_tmp$3056] = 43;
      _tmp$3057 = index$1272;
      index$1272 = _tmp$3057 + 1;
    }
    _tmp$3058 = exp$1277;
    if (_tmp$3058 >= 100) {
      int32_t _tmp$3074 = exp$1277;
      int32_t a$1283 = _tmp$3074 / 100;
      int32_t _tmp$3073 = exp$1277;
      int32_t _tmp$3072 = _tmp$3073 / 10;
      int32_t b$1284 = _tmp$3072 % 10;
      int32_t _tmp$3071 = exp$1277;
      int32_t c$1285 = _tmp$3071 % 10;
      int32_t _tmp$3059 = index$1272;
      int32_t _tmp$3061 = 48 + a$1283;
      int32_t _tmp$3060 = _tmp$3061 & 0xff;
      int32_t _tmp$3065;
      int32_t _tmp$3062;
      int32_t _tmp$3064;
      int32_t _tmp$3063;
      int32_t _tmp$3069;
      int32_t _tmp$3066;
      int32_t _tmp$3068;
      int32_t _tmp$3067;
      int32_t _tmp$3070;
      if (_tmp$3059 < 0 || _tmp$3059 >= Moonbit_array_length(result$1271)) {
        moonbit_panic();
      }
      result$1271[_tmp$3059] = _tmp$3060;
      _tmp$3065 = index$1272;
      _tmp$3062 = _tmp$3065 + 1;
      _tmp$3064 = 48 + b$1284;
      _tmp$3063 = _tmp$3064 & 0xff;
      if (_tmp$3062 < 0 || _tmp$3062 >= Moonbit_array_length(result$1271)) {
        moonbit_panic();
      }
      result$1271[_tmp$3062] = _tmp$3063;
      _tmp$3069 = index$1272;
      _tmp$3066 = _tmp$3069 + 2;
      _tmp$3068 = 48 + c$1285;
      _tmp$3067 = _tmp$3068 & 0xff;
      if (_tmp$3066 < 0 || _tmp$3066 >= Moonbit_array_length(result$1271)) {
        moonbit_panic();
      }
      result$1271[_tmp$3066] = _tmp$3067;
      _tmp$3070 = index$1272;
      index$1272 = _tmp$3070 + 3;
    } else {
      int32_t _tmp$3075 = exp$1277;
      if (_tmp$3075 >= 10) {
        int32_t _tmp$3085 = exp$1277;
        int32_t a$1286 = _tmp$3085 / 10;
        int32_t _tmp$3084 = exp$1277;
        int32_t b$1287 = _tmp$3084 % 10;
        int32_t _tmp$3076 = index$1272;
        int32_t _tmp$3078 = 48 + a$1286;
        int32_t _tmp$3077 = _tmp$3078 & 0xff;
        int32_t _tmp$3082;
        int32_t _tmp$3079;
        int32_t _tmp$3081;
        int32_t _tmp$3080;
        int32_t _tmp$3083;
        if (_tmp$3076 < 0 || _tmp$3076 >= Moonbit_array_length(result$1271)) {
          moonbit_panic();
        }
        result$1271[_tmp$3076] = _tmp$3077;
        _tmp$3082 = index$1272;
        _tmp$3079 = _tmp$3082 + 1;
        _tmp$3081 = 48 + b$1287;
        _tmp$3080 = _tmp$3081 & 0xff;
        if (_tmp$3079 < 0 || _tmp$3079 >= Moonbit_array_length(result$1271)) {
          moonbit_panic();
        }
        result$1271[_tmp$3079] = _tmp$3080;
        _tmp$3083 = index$1272;
        index$1272 = _tmp$3083 + 2;
      } else {
        int32_t _tmp$3086 = index$1272;
        int32_t _tmp$3089 = exp$1277;
        int32_t _tmp$3088 = 48 + _tmp$3089;
        int32_t _tmp$3087 = _tmp$3088 & 0xff;
        int32_t _tmp$3090;
        if (_tmp$3086 < 0 || _tmp$3086 >= Moonbit_array_length(result$1271)) {
          moonbit_panic();
        }
        result$1271[_tmp$3086] = _tmp$3087;
        _tmp$3090 = index$1272;
        index$1272 = _tmp$3090 + 1;
      }
    }
    _tmp$3091 = index$1272;
    return $moonbitlang$core$double$internal$ryu$string_from_bytes(
             result$1271, 0, _tmp$3091
           );
  } else {
    int32_t _tmp$3092 = exp$1277;
    int32_t _tmp$3152;
    if (_tmp$3092 < 0) {
      int32_t _tmp$3093 = index$1272;
      int32_t _tmp$3094;
      int32_t _tmp$3095;
      int32_t _tmp$3096;
      int32_t i$1288;
      int32_t current$1290;
      int32_t i$1291;
      if (_tmp$3093 < 0 || _tmp$3093 >= Moonbit_array_length(result$1271)) {
        moonbit_panic();
      }
      result$1271[_tmp$3093] = 48;
      _tmp$3094 = index$1272;
      index$1272 = _tmp$3094 + 1;
      _tmp$3095 = index$1272;
      if (_tmp$3095 < 0 || _tmp$3095 >= Moonbit_array_length(result$1271)) {
        moonbit_panic();
      }
      result$1271[_tmp$3095] = 46;
      _tmp$3096 = index$1272;
      index$1272 = _tmp$3096 + 1;
      i$1288 = -1;
      while (1) {
        int32_t _tmp$3097 = exp$1277;
        if (i$1288 > _tmp$3097) {
          int32_t _tmp$3098 = index$1272;
          int32_t _tmp$3099;
          int32_t _tmp$3100;
          if (
            _tmp$3098 < 0 || _tmp$3098 >= Moonbit_array_length(result$1271)
          ) {
            moonbit_panic();
          }
          result$1271[_tmp$3098] = 48;
          _tmp$3099 = index$1272;
          index$1272 = _tmp$3099 + 1;
          _tmp$3100 = i$1288 - 1;
          i$1288 = _tmp$3100;
          continue;
        }
        break;
      }
      current$1290 = index$1272;
      i$1291 = 0;
      while (1) {
        if (i$1291 < olength$1276) {
          int32_t _tmp$3108 = current$1290 + olength$1276;
          int32_t _tmp$3107 = _tmp$3108 - i$1291;
          int32_t _tmp$3101 = _tmp$3107 - 1;
          uint64_t _tmp$3106 = output$1274;
          uint64_t _tmp$3105 = _tmp$3106 % 10ull;
          int32_t _tmp$3104 = (int32_t)_tmp$3105;
          int32_t _tmp$3103 = 48 + _tmp$3104;
          int32_t _tmp$3102 = _tmp$3103 & 0xff;
          uint64_t _tmp$3109;
          int32_t _tmp$3110;
          int32_t _tmp$3111;
          if (
            _tmp$3101 < 0 || _tmp$3101 >= Moonbit_array_length(result$1271)
          ) {
            moonbit_panic();
          }
          result$1271[_tmp$3101] = _tmp$3102;
          _tmp$3109 = output$1274;
          output$1274 = _tmp$3109 / 10ull;
          _tmp$3110 = index$1272;
          index$1272 = _tmp$3110 + 1;
          _tmp$3111 = i$1291 + 1;
          i$1291 = _tmp$3111;
          continue;
        }
        break;
      }
    } else {
      int32_t _tmp$3113 = exp$1277;
      int32_t _tmp$3112 = _tmp$3113 + 1;
      if (_tmp$3112 >= olength$1276) {
        int32_t i$1293 = 0;
        int32_t _tmp$3125;
        int32_t _tmp$3129;
        int32_t _end64$1295;
        int32_t i$1296;
        while (1) {
          if (i$1293 < olength$1276) {
            int32_t _tmp$3122 = index$1272;
            int32_t _tmp$3121 = _tmp$3122 + olength$1276;
            int32_t _tmp$3120 = _tmp$3121 - i$1293;
            int32_t _tmp$3114 = _tmp$3120 - 1;
            uint64_t _tmp$3119 = output$1274;
            uint64_t _tmp$3118 = _tmp$3119 % 10ull;
            int32_t _tmp$3117 = (int32_t)_tmp$3118;
            int32_t _tmp$3116 = 48 + _tmp$3117;
            int32_t _tmp$3115 = _tmp$3116 & 0xff;
            uint64_t _tmp$3123;
            int32_t _tmp$3124;
            if (
              _tmp$3114 < 0 || _tmp$3114 >= Moonbit_array_length(result$1271)
            ) {
              moonbit_panic();
            }
            result$1271[_tmp$3114] = _tmp$3115;
            _tmp$3123 = output$1274;
            output$1274 = _tmp$3123 / 10ull;
            _tmp$3124 = i$1293 + 1;
            i$1293 = _tmp$3124;
            continue;
          }
          break;
        }
        _tmp$3125 = index$1272;
        index$1272 = _tmp$3125 + olength$1276;
        _tmp$3129 = exp$1277;
        _end64$1295 = _tmp$3129 + 1;
        i$1296 = olength$1276;
        while (1) {
          if (i$1296 < _end64$1295) {
            int32_t _tmp$3126 = index$1272;
            int32_t _tmp$3127;
            int32_t _tmp$3128;
            if (
              _tmp$3126 < 0 || _tmp$3126 >= Moonbit_array_length(result$1271)
            ) {
              moonbit_panic();
            }
            result$1271[_tmp$3126] = 48;
            _tmp$3127 = index$1272;
            index$1272 = _tmp$3127 + 1;
            _tmp$3128 = i$1296 + 1;
            i$1296 = _tmp$3128;
            continue;
          }
          break;
        }
      } else {
        int32_t _tmp$3151 = index$1272;
        int32_t current$1298 = _tmp$3151 + 1;
        int32_t i$1299 = 0;
        int32_t _tmp$3149;
        int32_t _tmp$3150;
        while (1) {
          if (i$1299 < olength$1276) {
            int32_t _tmp$3132 = olength$1276 - i$1299;
            int32_t _tmp$3130 = _tmp$3132 - 1;
            int32_t _tmp$3131 = exp$1277;
            int32_t _tmp$3146;
            int32_t _tmp$3145;
            int32_t _tmp$3144;
            int32_t _tmp$3138;
            uint64_t _tmp$3143;
            uint64_t _tmp$3142;
            int32_t _tmp$3141;
            int32_t _tmp$3140;
            int32_t _tmp$3139;
            uint64_t _tmp$3147;
            int32_t _tmp$3148;
            if (_tmp$3130 == _tmp$3131) {
              int32_t _tmp$3136 = current$1298;
              int32_t _tmp$3135 = _tmp$3136 + olength$1276;
              int32_t _tmp$3134 = _tmp$3135 - i$1299;
              int32_t _tmp$3133 = _tmp$3134 - 1;
              int32_t _tmp$3137;
              if (
                _tmp$3133 < 0
                || _tmp$3133 >= Moonbit_array_length(result$1271)
              ) {
                moonbit_panic();
              }
              result$1271[_tmp$3133] = 46;
              _tmp$3137 = current$1298;
              current$1298 = _tmp$3137 - 1;
            }
            _tmp$3146 = current$1298;
            _tmp$3145 = _tmp$3146 + olength$1276;
            _tmp$3144 = _tmp$3145 - i$1299;
            _tmp$3138 = _tmp$3144 - 1;
            _tmp$3143 = output$1274;
            _tmp$3142 = _tmp$3143 % 10ull;
            _tmp$3141 = (int32_t)_tmp$3142;
            _tmp$3140 = 48 + _tmp$3141;
            _tmp$3139 = _tmp$3140 & 0xff;
            if (
              _tmp$3138 < 0 || _tmp$3138 >= Moonbit_array_length(result$1271)
            ) {
              moonbit_panic();
            }
            result$1271[_tmp$3138] = _tmp$3139;
            _tmp$3147 = output$1274;
            output$1274 = _tmp$3147 / 10ull;
            _tmp$3148 = i$1299 + 1;
            i$1299 = _tmp$3148;
            continue;
          }
          break;
        }
        _tmp$3149 = index$1272;
        _tmp$3150 = olength$1276 + 1;
        index$1272 = _tmp$3149 + _tmp$3150;
      }
    }
    _tmp$3152 = index$1272;
    return $moonbitlang$core$double$internal$ryu$string_from_bytes(
             result$1271, 0, _tmp$3152
           );
  }
}

struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* $moonbitlang$core$double$internal$ryu$d2d(
  uint64_t ieeeMantissa$1217,
  uint32_t ieeeExponent$1216
) {
  int32_t e2$1214 = 0;
  uint64_t m2$1215 = 0ull;
  uint64_t _tmp$3027;
  uint64_t _tmp$3026;
  int32_t even$1218;
  uint64_t _tmp$3025;
  uint64_t mv$1219;
  int32_t mmShift$1220;
  uint64_t vr$1221;
  uint64_t vp$1222;
  uint64_t vm$1223;
  int32_t e10$1224;
  int32_t vmIsTrailingZeros$1225;
  int32_t vrIsTrailingZeros$1226;
  int32_t _tmp$2927;
  int32_t removed$1245;
  int32_t lastRemovedDigit$1246;
  uint64_t output$1247;
  int32_t _tmp$3023;
  int32_t _tmp$3024;
  int32_t exp$1270;
  uint64_t _tmp$3022;
  struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64* _block$4952;
  if (ieeeExponent$1216 == 0u) {
    e2$1214 = -1076;
    m2$1215 = ieeeMantissa$1217;
  } else {
    int32_t _tmp$2926 = *(int32_t*)&ieeeExponent$1216;
    int32_t _tmp$2925 = _tmp$2926 - 1023;
    int32_t _tmp$2924 = _tmp$2925 - 52;
    e2$1214 = _tmp$2924 - 2;
    m2$1215 = 4503599627370496ull | ieeeMantissa$1217;
  }
  _tmp$3027 = m2$1215;
  _tmp$3026 = _tmp$3027 & 1ull;
  even$1218 = _tmp$3026 == 0ull;
  _tmp$3025 = m2$1215;
  mv$1219 = 4ull * _tmp$3025;
  if (ieeeMantissa$1217 != 0ull) {
    mmShift$1220 = 1;
  } else {
    mmShift$1220 = ieeeExponent$1216 <= 1u;
  }
  vr$1221 = 0ull;
  vp$1222 = 0ull;
  vm$1223 = 0ull;
  e10$1224 = 0;
  vmIsTrailingZeros$1225 = 0;
  vrIsTrailingZeros$1226 = 0;
  _tmp$2927 = e2$1214;
  if (_tmp$2927 >= 0) {
    int32_t _tmp$2949 = e2$1214;
    int32_t _tmp$2945 =
      $moonbitlang$core$double$internal$ryu$log10Pow2(_tmp$2949);
    int32_t _tmp$2948 = e2$1214;
    int32_t _tmp$2947 = _tmp$2948 > 3;
    int32_t _tmp$2946 = $Bool$$to_int(_tmp$2947);
    int32_t q$1227 = _tmp$2945 - _tmp$2946;
    int32_t _tmp$2944;
    int32_t _tmp$2943;
    int32_t k$1228;
    int32_t _tmp$2942;
    int32_t _tmp$2941;
    int32_t _tmp$2940;
    int32_t i$1229;
    struct $$moonbitlang$core$double$internal$ryu$Pow5Pair pow5$1230;
    uint64_t _tmp$2939;
    struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result _bind$1231;
    uint64_t _vrOut$1232;
    uint64_t _vpOut$1233;
    uint64_t _vmOut$1234;
    e10$1224 = q$1227;
    _tmp$2944 = $moonbitlang$core$double$internal$ryu$pow5bits(q$1227);
    _tmp$2943 = 125 + _tmp$2944;
    k$1228 = _tmp$2943 - 1;
    _tmp$2942 = e2$1214;
    _tmp$2941 = -_tmp$2942;
    _tmp$2940 = _tmp$2941 + q$1227;
    i$1229 = _tmp$2940 + k$1228;
    pow5$1230
    = $moonbitlang$core$double$internal$ryu$double_computeInvPow5(
      q$1227
    );
    _tmp$2939 = m2$1215;
    _bind$1231
    = $moonbitlang$core$double$internal$ryu$mulShiftAll64(
      _tmp$2939, pow5$1230, i$1229, mmShift$1220
    );
    _vrOut$1232 = _bind$1231.$0;
    _vpOut$1233 = _bind$1231.$1;
    _vmOut$1234 = _bind$1231.$2;
    vr$1221 = _vrOut$1232;
    vp$1222 = _vpOut$1233;
    vm$1223 = _vmOut$1234;
    if (q$1227 <= 21) {
      int32_t _tmp$2935 = (int32_t)mv$1219;
      uint64_t _tmp$2938 = mv$1219 / 5ull;
      int32_t _tmp$2937 = (int32_t)_tmp$2938;
      int32_t _tmp$2936 = 5 * _tmp$2937;
      int32_t mvMod5$1235 = _tmp$2935 - _tmp$2936;
      if (mvMod5$1235 == 0) {
        vrIsTrailingZeros$1226
        = $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
          mv$1219, q$1227
        );
      } else if (even$1218) {
        uint64_t _tmp$2929 = mv$1219 - 1ull;
        uint64_t _tmp$2930 = $Bool$$to_uint64(mmShift$1220);
        uint64_t _tmp$2928 = _tmp$2929 - _tmp$2930;
        vmIsTrailingZeros$1225
        = $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
          _tmp$2928, q$1227
        );
      } else {
        uint64_t _tmp$2931 = vp$1222;
        uint64_t _tmp$2934 = mv$1219 + 2ull;
        int32_t _tmp$2933 =
          $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
            _tmp$2934, q$1227
          );
        uint64_t _tmp$2932 = $Bool$$to_uint64(_tmp$2933);
        vp$1222 = _tmp$2931 - _tmp$2932;
      }
    }
  } else {
    int32_t _tmp$2963 = e2$1214;
    int32_t _tmp$2962 = -_tmp$2963;
    int32_t _tmp$2957 =
      $moonbitlang$core$double$internal$ryu$log10Pow5(_tmp$2962);
    int32_t _tmp$2961 = e2$1214;
    int32_t _tmp$2960 = -_tmp$2961;
    int32_t _tmp$2959 = _tmp$2960 > 1;
    int32_t _tmp$2958 = $Bool$$to_int(_tmp$2959);
    int32_t q$1236 = _tmp$2957 - _tmp$2958;
    int32_t _tmp$2950 = e2$1214;
    int32_t _tmp$2956;
    int32_t _tmp$2955;
    int32_t i$1237;
    int32_t _tmp$2954;
    int32_t k$1238;
    int32_t j$1239;
    struct $$moonbitlang$core$double$internal$ryu$Pow5Pair pow5$1240;
    uint64_t _tmp$2953;
    struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result _bind$1241;
    uint64_t _vrOut$1242;
    uint64_t _vpOut$1243;
    uint64_t _vmOut$1244;
    e10$1224 = q$1236 + _tmp$2950;
    _tmp$2956 = e2$1214;
    _tmp$2955 = -_tmp$2956;
    i$1237 = _tmp$2955 - q$1236;
    _tmp$2954 = $moonbitlang$core$double$internal$ryu$pow5bits(i$1237);
    k$1238 = _tmp$2954 - 125;
    j$1239 = q$1236 - k$1238;
    pow5$1240
    = $moonbitlang$core$double$internal$ryu$double_computePow5(
      i$1237
    );
    _tmp$2953 = m2$1215;
    _bind$1241
    = $moonbitlang$core$double$internal$ryu$mulShiftAll64(
      _tmp$2953, pow5$1240, j$1239, mmShift$1220
    );
    _vrOut$1242 = _bind$1241.$0;
    _vpOut$1243 = _bind$1241.$1;
    _vmOut$1244 = _bind$1241.$2;
    vr$1221 = _vrOut$1242;
    vp$1222 = _vpOut$1243;
    vm$1223 = _vmOut$1244;
    if (q$1236 <= 1) {
      vrIsTrailingZeros$1226 = 1;
      if (even$1218) {
        int32_t _tmp$2951 = $Bool$$to_int(mmShift$1220);
        vmIsTrailingZeros$1225 = _tmp$2951 == 1;
      } else {
        uint64_t _tmp$2952 = vp$1222;
        vp$1222 = _tmp$2952 - 1ull;
      }
    } else if (q$1236 < 63) {
      vrIsTrailingZeros$1226
      = $moonbitlang$core$double$internal$ryu$multipleOfPowerOf2(
        mv$1219, q$1236
      );
    }
  }
  removed$1245 = 0;
  lastRemovedDigit$1246 = 0;
  output$1247 = 0ull;
  if (vmIsTrailingZeros$1225 || vrIsTrailingZeros$1226) {
    int32_t _if_result$4949;
    uint64_t _tmp$2993;
    uint64_t _tmp$2999;
    uint64_t _tmp$3000;
    int32_t _if_result$4950;
    int32_t _tmp$2996;
    int64_t _tmp$2995;
    uint64_t _tmp$2994;
    while (1) {
      uint64_t _tmp$2976 = vp$1222;
      uint64_t vpDiv10$1248 = _tmp$2976 / 10ull;
      uint64_t _tmp$2975 = vm$1223;
      uint64_t vmDiv10$1249 = _tmp$2975 / 10ull;
      uint64_t _tmp$2974;
      int32_t _tmp$2971;
      int32_t _tmp$2973;
      int32_t _tmp$2972;
      int32_t vmMod10$1251;
      uint64_t _tmp$2970;
      uint64_t vrDiv10$1252;
      uint64_t _tmp$2969;
      int32_t _tmp$2966;
      int32_t _tmp$2968;
      int32_t _tmp$2967;
      int32_t vrMod10$1253;
      int32_t _tmp$2965;
      if (vpDiv10$1248 <= vmDiv10$1249) {
        break;
      }
      _tmp$2974 = vm$1223;
      _tmp$2971 = (int32_t)_tmp$2974;
      _tmp$2973 = (int32_t)vmDiv10$1249;
      _tmp$2972 = 10 * _tmp$2973;
      vmMod10$1251 = _tmp$2971 - _tmp$2972;
      _tmp$2970 = vr$1221;
      vrDiv10$1252 = _tmp$2970 / 10ull;
      _tmp$2969 = vr$1221;
      _tmp$2966 = (int32_t)_tmp$2969;
      _tmp$2968 = (int32_t)vrDiv10$1252;
      _tmp$2967 = 10 * _tmp$2968;
      vrMod10$1253 = _tmp$2966 - _tmp$2967;
      if (vmIsTrailingZeros$1225) {
        vmIsTrailingZeros$1225 = vmMod10$1251 == 0;
      } else {
        vmIsTrailingZeros$1225 = 0;
      }
      if (vrIsTrailingZeros$1226) {
        int32_t _tmp$2964 = lastRemovedDigit$1246;
        vrIsTrailingZeros$1226 = _tmp$2964 == 0;
      } else {
        vrIsTrailingZeros$1226 = 0;
      }
      lastRemovedDigit$1246 = vrMod10$1253;
      vr$1221 = vrDiv10$1252;
      vp$1222 = vpDiv10$1248;
      vm$1223 = vmDiv10$1249;
      _tmp$2965 = removed$1245;
      removed$1245 = _tmp$2965 + 1;
      continue;
      break;
    }
    if (vmIsTrailingZeros$1225) {
      while (1) {
        uint64_t _tmp$2989 = vm$1223;
        uint64_t vmDiv10$1254 = _tmp$2989 / 10ull;
        uint64_t _tmp$2988 = vm$1223;
        int32_t _tmp$2985 = (int32_t)_tmp$2988;
        int32_t _tmp$2987 = (int32_t)vmDiv10$1254;
        int32_t _tmp$2986 = 10 * _tmp$2987;
        int32_t vmMod10$1255 = _tmp$2985 - _tmp$2986;
        uint64_t _tmp$2984;
        uint64_t vpDiv10$1257;
        uint64_t _tmp$2983;
        uint64_t vrDiv10$1258;
        uint64_t _tmp$2982;
        int32_t _tmp$2979;
        int32_t _tmp$2981;
        int32_t _tmp$2980;
        int32_t vrMod10$1259;
        int32_t _tmp$2978;
        if (vmMod10$1255 != 0) {
          break;
        }
        _tmp$2984 = vp$1222;
        vpDiv10$1257 = _tmp$2984 / 10ull;
        _tmp$2983 = vr$1221;
        vrDiv10$1258 = _tmp$2983 / 10ull;
        _tmp$2982 = vr$1221;
        _tmp$2979 = (int32_t)_tmp$2982;
        _tmp$2981 = (int32_t)vrDiv10$1258;
        _tmp$2980 = 10 * _tmp$2981;
        vrMod10$1259 = _tmp$2979 - _tmp$2980;
        if (vrIsTrailingZeros$1226) {
          int32_t _tmp$2977 = lastRemovedDigit$1246;
          vrIsTrailingZeros$1226 = _tmp$2977 == 0;
        } else {
          vrIsTrailingZeros$1226 = 0;
        }
        lastRemovedDigit$1246 = vrMod10$1259;
        vr$1221 = vrDiv10$1258;
        vp$1222 = vpDiv10$1257;
        vm$1223 = vmDiv10$1254;
        _tmp$2978 = removed$1245;
        removed$1245 = _tmp$2978 + 1;
        continue;
        break;
      }
    }
    if (vrIsTrailingZeros$1226) {
      int32_t _tmp$2992 = lastRemovedDigit$1246;
      if (_tmp$2992 == 5) {
        uint64_t _tmp$2991 = vr$1221;
        uint64_t _tmp$2990 = _tmp$2991 % 2ull;
        _if_result$4949 = _tmp$2990 == 0ull;
      } else {
        _if_result$4949 = 0;
      }
    } else {
      _if_result$4949 = 0;
    }
    if (_if_result$4949) {
      lastRemovedDigit$1246 = 4;
    }
    _tmp$2993 = vr$1221;
    _tmp$2999 = vr$1221;
    _tmp$3000 = vm$1223;
    if (_tmp$2999 == _tmp$3000) {
      if (!even$1218) {
        _if_result$4950 = 1;
      } else {
        int32_t _tmp$2998 = vmIsTrailingZeros$1225;
        _if_result$4950 = !_tmp$2998;
      }
    } else {
      _if_result$4950 = 0;
    }
    if (_if_result$4950) {
      _tmp$2996 = 1;
    } else {
      int32_t _tmp$2997 = lastRemovedDigit$1246;
      _tmp$2996 = _tmp$2997 >= 5;
    }
    _tmp$2995 = $Bool$$to_int64(_tmp$2996);
    _tmp$2994 = *(uint64_t*)&_tmp$2995;
    output$1247 = _tmp$2993 + _tmp$2994;
  } else {
    int32_t roundUp$1260 = 0;
    uint64_t _tmp$3021 = vp$1222;
    uint64_t vpDiv100$1261 = _tmp$3021 / 100ull;
    uint64_t _tmp$3020 = vm$1223;
    uint64_t vmDiv100$1262 = _tmp$3020 / 100ull;
    uint64_t _tmp$3015;
    uint64_t _tmp$3018;
    uint64_t _tmp$3019;
    int32_t _tmp$3017;
    uint64_t _tmp$3016;
    if (vpDiv100$1261 > vmDiv100$1262) {
      uint64_t _tmp$3006 = vr$1221;
      uint64_t vrDiv100$1263 = _tmp$3006 / 100ull;
      uint64_t _tmp$3005 = vr$1221;
      int32_t _tmp$3002 = (int32_t)_tmp$3005;
      int32_t _tmp$3004 = (int32_t)vrDiv100$1263;
      int32_t _tmp$3003 = 100 * _tmp$3004;
      int32_t vrMod100$1264 = _tmp$3002 - _tmp$3003;
      int32_t _tmp$3001;
      roundUp$1260 = vrMod100$1264 >= 50;
      vr$1221 = vrDiv100$1263;
      vp$1222 = vpDiv100$1261;
      vm$1223 = vmDiv100$1262;
      _tmp$3001 = removed$1245;
      removed$1245 = _tmp$3001 + 2;
    }
    while (1) {
      uint64_t _tmp$3014 = vp$1222;
      uint64_t vpDiv10$1265 = _tmp$3014 / 10ull;
      uint64_t _tmp$3013 = vm$1223;
      uint64_t vmDiv10$1266 = _tmp$3013 / 10ull;
      uint64_t _tmp$3012;
      uint64_t vrDiv10$1268;
      uint64_t _tmp$3011;
      int32_t _tmp$3008;
      int32_t _tmp$3010;
      int32_t _tmp$3009;
      int32_t vrMod10$1269;
      int32_t _tmp$3007;
      if (vpDiv10$1265 <= vmDiv10$1266) {
        break;
      }
      _tmp$3012 = vr$1221;
      vrDiv10$1268 = _tmp$3012 / 10ull;
      _tmp$3011 = vr$1221;
      _tmp$3008 = (int32_t)_tmp$3011;
      _tmp$3010 = (int32_t)vrDiv10$1268;
      _tmp$3009 = 10 * _tmp$3010;
      vrMod10$1269 = _tmp$3008 - _tmp$3009;
      roundUp$1260 = vrMod10$1269 >= 5;
      vr$1221 = vrDiv10$1268;
      vp$1222 = vpDiv10$1265;
      vm$1223 = vmDiv10$1266;
      _tmp$3007 = removed$1245;
      removed$1245 = _tmp$3007 + 1;
      continue;
      break;
    }
    _tmp$3015 = vr$1221;
    _tmp$3018 = vr$1221;
    _tmp$3019 = vm$1223;
    _tmp$3017 = _tmp$3018 == _tmp$3019 || roundUp$1260;
    _tmp$3016 = $Bool$$to_uint64(_tmp$3017);
    output$1247 = _tmp$3015 + _tmp$3016;
  }
  _tmp$3023 = e10$1224;
  _tmp$3024 = removed$1245;
  exp$1270 = _tmp$3023 + _tmp$3024;
  _tmp$3022 = output$1247;
  _block$4952
  = (struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64)
    );
  Moonbit_object_header(_block$4952)->meta
  = Moonbit_make_regular_object_header(
    sizeof(
      struct $$moonbitlang$core$double$internal$ryu$FloatingDecimal64
    )
    >> 2,
      0,
      0
  );
  _block$4952->$0 = _tmp$3022;
  _block$4952->$1 = exp$1270;
  return _block$4952;
}

int32_t $moonbitlang$core$double$internal$ryu$decimal_length17(
  uint64_t v$1213
) {
  if (v$1213 >= 10000000000000000ull) {
    return 17;
  }
  if (v$1213 >= 1000000000000000ull) {
    return 16;
  }
  if (v$1213 >= 100000000000000ull) {
    return 15;
  }
  if (v$1213 >= 10000000000000ull) {
    return 14;
  }
  if (v$1213 >= 1000000000000ull) {
    return 13;
  }
  if (v$1213 >= 100000000000ull) {
    return 12;
  }
  if (v$1213 >= 10000000000ull) {
    return 11;
  }
  if (v$1213 >= 1000000000ull) {
    return 10;
  }
  if (v$1213 >= 100000000ull) {
    return 9;
  }
  if (v$1213 >= 10000000ull) {
    return 8;
  }
  if (v$1213 >= 1000000ull) {
    return 7;
  }
  if (v$1213 >= 100000ull) {
    return 6;
  }
  if (v$1213 >= 10000ull) {
    return 5;
  }
  if (v$1213 >= 1000ull) {
    return 4;
  }
  if (v$1213 >= 100ull) {
    return 3;
  }
  if (v$1213 >= 10ull) {
    return 2;
  }
  return 1;
}

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computeInvPow5(
  int32_t i$1196
) {
  int32_t _tmp$2923 = i$1196 + 26;
  int32_t _tmp$2922 = _tmp$2923 - 1;
  int32_t base$1195 = _tmp$2922 / 26;
  int32_t base2$1197 = base$1195 * 26;
  int32_t offset$1198 = base2$1197 - i$1196;
  int32_t _tmp$2921 = base$1195 * 2;
  uint64_t mul0$1199;
  int32_t _tmp$2920;
  int32_t _tmp$2919;
  uint64_t mul1$1200;
  uint64_t m$1201;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1202;
  uint64_t _low1$1203;
  uint64_t _high1$1204;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1205;
  uint64_t _low0$1206;
  uint64_t _high0$1207;
  uint64_t sum$1208;
  uint64_t high1$1209;
  int32_t _tmp$2917;
  int32_t _tmp$2918;
  int32_t delta$1210;
  uint64_t _tmp$2916;
  uint64_t _tmp$2908;
  int32_t _tmp$2915;
  uint32_t _tmp$2912;
  int32_t _tmp$2914;
  int32_t _tmp$2913;
  uint32_t _tmp$2911;
  uint32_t _tmp$2910;
  uint64_t _tmp$2909;
  uint64_t a$1211;
  uint64_t _tmp$2907;
  uint64_t b$1212;
  moonbit_incref(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2
  );
  mul0$1199
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2, _tmp$2921
  );
  _tmp$2920 = base$1195 * 2;
  _tmp$2919 = _tmp$2920 + 1;
  moonbit_incref(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2
  );
  mul1$1200
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_INV_SPLIT2, _tmp$2919
  );
  if (offset$1198 == 0) {
    return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
             mul0$1199, mul1$1200
           };
  }
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE);
  m$1201
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE, offset$1198
  );
  _bind$1202
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1201, mul1$1200
  );
  _low1$1203 = _bind$1202.$0;
  _high1$1204 = _bind$1202.$1;
  _bind$1205
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1201, mul0$1199
  );
  _low0$1206 = _bind$1205.$0;
  _high0$1207 = _bind$1205.$1;
  sum$1208 = _high0$1207 + _low1$1203;
  high1$1209 = _high1$1204;
  if (sum$1208 < _high0$1207) {
    uint64_t _tmp$2906 = high1$1209;
    high1$1209 = _tmp$2906 + 1ull;
  }
  _tmp$2917 = $moonbitlang$core$double$internal$ryu$pow5bits(base2$1197);
  _tmp$2918 = $moonbitlang$core$double$internal$ryu$pow5bits(i$1196);
  delta$1210 = _tmp$2917 - _tmp$2918;
  _tmp$2916
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    _low0$1206, sum$1208, delta$1210
  );
  _tmp$2908 = _tmp$2916 + 1ull;
  _tmp$2915 = i$1196 / 16;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gPOW5_INV_OFFSETS);
  _tmp$2912
  = $ReadOnlyArray$$at$1(
    $moonbitlang$core$double$internal$ryu$gPOW5_INV_OFFSETS, _tmp$2915
  );
  _tmp$2914 = i$1196 % 16;
  _tmp$2913 = _tmp$2914 << 1;
  _tmp$2911 = _tmp$2912 >> (_tmp$2913 & 31);
  _tmp$2910 = _tmp$2911 & 3u;
  _tmp$2909 = $UInt$$to_uint64(_tmp$2910);
  a$1211 = _tmp$2908 + _tmp$2909;
  _tmp$2907 = high1$1209;
  b$1212
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    sum$1208, _tmp$2907, delta$1210
  );
  return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
           a$1211, b$1212
         };
}

struct $$moonbitlang$core$double$internal$ryu$Pow5Pair $moonbitlang$core$double$internal$ryu$double_computePow5(
  int32_t i$1178
) {
  int32_t base$1177 = i$1178 / 26;
  int32_t base2$1179 = base$1177 * 26;
  int32_t offset$1180 = i$1178 - base2$1179;
  int32_t _tmp$2905 = base$1177 * 2;
  uint64_t mul0$1181;
  int32_t _tmp$2904;
  int32_t _tmp$2903;
  uint64_t mul1$1182;
  uint64_t m$1183;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1184;
  uint64_t _low1$1185;
  uint64_t _high1$1186;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1187;
  uint64_t _low0$1188;
  uint64_t _high0$1189;
  uint64_t sum$1190;
  uint64_t high1$1191;
  int32_t _tmp$2901;
  int32_t _tmp$2902;
  int32_t delta$1192;
  uint64_t _tmp$2893;
  int32_t _tmp$2900;
  uint32_t _tmp$2897;
  int32_t _tmp$2899;
  int32_t _tmp$2898;
  uint32_t _tmp$2896;
  uint32_t _tmp$2895;
  uint64_t _tmp$2894;
  uint64_t a$1193;
  uint64_t _tmp$2892;
  uint64_t b$1194;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2);
  mul0$1181
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2, _tmp$2905
  );
  _tmp$2904 = base$1177 * 2;
  _tmp$2903 = _tmp$2904 + 1;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2);
  mul1$1182
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_SPLIT2, _tmp$2903
  );
  if (offset$1180 == 0) {
    return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
             mul0$1181, mul1$1182
           };
  }
  moonbit_incref($moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE);
  m$1183
  = $ReadOnlyArray$$at$0(
    $moonbitlang$core$double$internal$ryu$gDOUBLE_POW5_TABLE, offset$1180
  );
  _bind$1184
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1183, mul1$1182
  );
  _low1$1185 = _bind$1184.$0;
  _high1$1186 = _bind$1184.$1;
  _bind$1187
  = $moonbitlang$core$double$internal$ryu$umul128(
    m$1183, mul0$1181
  );
  _low0$1188 = _bind$1187.$0;
  _high0$1189 = _bind$1187.$1;
  sum$1190 = _high0$1189 + _low1$1185;
  high1$1191 = _high1$1186;
  if (sum$1190 < _high0$1189) {
    uint64_t _tmp$2891 = high1$1191;
    high1$1191 = _tmp$2891 + 1ull;
  }
  _tmp$2901 = $moonbitlang$core$double$internal$ryu$pow5bits(i$1178);
  _tmp$2902 = $moonbitlang$core$double$internal$ryu$pow5bits(base2$1179);
  delta$1192 = _tmp$2901 - _tmp$2902;
  _tmp$2893
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    _low0$1188, sum$1190, delta$1192
  );
  _tmp$2900 = i$1178 / 16;
  moonbit_incref($moonbitlang$core$double$internal$ryu$gPOW5_OFFSETS);
  _tmp$2897
  = $ReadOnlyArray$$at$1(
    $moonbitlang$core$double$internal$ryu$gPOW5_OFFSETS, _tmp$2900
  );
  _tmp$2899 = i$1178 % 16;
  _tmp$2898 = _tmp$2899 << 1;
  _tmp$2896 = _tmp$2897 >> (_tmp$2898 & 31);
  _tmp$2895 = _tmp$2896 & 3u;
  _tmp$2894 = $UInt$$to_uint64(_tmp$2895);
  a$1193 = _tmp$2893 + _tmp$2894;
  _tmp$2892 = high1$1191;
  b$1194
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    sum$1190, _tmp$2892, delta$1192
  );
  return (struct $$moonbitlang$core$double$internal$ryu$Pow5Pair){
           a$1193, b$1194
         };
}

struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result $moonbitlang$core$double$internal$ryu$mulShiftAll64(
  uint64_t m$1151,
  struct $$moonbitlang$core$double$internal$ryu$Pow5Pair mul$1148,
  int32_t j$1164,
  int32_t mmShift$1166
) {
  uint64_t _mul0$1147 = mul$1148.$0;
  uint64_t _mul1$1149 = mul$1148.$1;
  uint64_t m$1150 = m$1151 << 1;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1152 =
    $moonbitlang$core$double$internal$ryu$umul128(m$1150, _mul0$1147);
  uint64_t _lo$1153 = _bind$1152.$0;
  uint64_t _tmp$1154 = _bind$1152.$1;
  struct $$moonbitlang$core$double$internal$ryu$Umul128 _bind$1155 =
    $moonbitlang$core$double$internal$ryu$umul128(m$1150, _mul1$1149);
  uint64_t _lo2$1156 = _bind$1155.$0;
  uint64_t _hi2$1157 = _bind$1155.$1;
  uint64_t mid$1158 = _tmp$1154 + _lo2$1156;
  uint64_t _tmp$2890;
  uint64_t hi$1159;
  uint64_t lo2$1160;
  uint64_t _tmp$2888;
  uint64_t _tmp$2889;
  uint64_t mid2$1161;
  uint64_t _tmp$2887;
  uint64_t hi2$1162;
  int32_t _tmp$2886;
  int32_t _tmp$2885;
  uint64_t vp$1163;
  uint64_t vm$1165;
  int32_t _tmp$2884;
  int32_t _tmp$2883;
  uint64_t vr$1176;
  uint64_t _tmp$2882;
  if (mid$1158 < _tmp$1154) {
    _tmp$2890 = 1ull;
  } else {
    _tmp$2890 = 0ull;
  }
  hi$1159 = _hi2$1157 + _tmp$2890;
  lo2$1160 = _lo$1153 + _mul0$1147;
  _tmp$2888 = mid$1158 + _mul1$1149;
  if (lo2$1160 < _lo$1153) {
    _tmp$2889 = 1ull;
  } else {
    _tmp$2889 = 0ull;
  }
  mid2$1161 = _tmp$2888 + _tmp$2889;
  if (mid2$1161 < mid$1158) {
    _tmp$2887 = 1ull;
  } else {
    _tmp$2887 = 0ull;
  }
  hi2$1162 = hi$1159 + _tmp$2887;
  _tmp$2886 = j$1164 - 64;
  _tmp$2885 = _tmp$2886 - 1;
  vp$1163
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    mid2$1161, hi2$1162, _tmp$2885
  );
  vm$1165 = 0ull;
  if (mmShift$1166) {
    uint64_t lo3$1167 = _lo$1153 - _mul0$1147;
    uint64_t _tmp$2872 = mid$1158 - _mul1$1149;
    uint64_t _tmp$2873;
    uint64_t mid3$1168;
    uint64_t _tmp$2871;
    uint64_t hi3$1169;
    int32_t _tmp$2870;
    int32_t _tmp$2869;
    if (_lo$1153 < lo3$1167) {
      _tmp$2873 = 1ull;
    } else {
      _tmp$2873 = 0ull;
    }
    mid3$1168 = _tmp$2872 - _tmp$2873;
    if (mid$1158 < mid3$1168) {
      _tmp$2871 = 1ull;
    } else {
      _tmp$2871 = 0ull;
    }
    hi3$1169 = hi$1159 - _tmp$2871;
    _tmp$2870 = j$1164 - 64;
    _tmp$2869 = _tmp$2870 - 1;
    vm$1165
    = $moonbitlang$core$double$internal$ryu$shiftright128(
      mid3$1168, hi3$1169, _tmp$2869
    );
  } else {
    uint64_t lo3$1170 = _lo$1153 + _lo$1153;
    uint64_t _tmp$2880 = mid$1158 + mid$1158;
    uint64_t _tmp$2881;
    uint64_t mid3$1171;
    uint64_t _tmp$2878;
    uint64_t _tmp$2879;
    uint64_t hi3$1172;
    uint64_t lo4$1173;
    uint64_t _tmp$2876;
    uint64_t _tmp$2877;
    uint64_t mid4$1174;
    uint64_t _tmp$2875;
    uint64_t hi4$1175;
    int32_t _tmp$2874;
    if (lo3$1170 < _lo$1153) {
      _tmp$2881 = 1ull;
    } else {
      _tmp$2881 = 0ull;
    }
    mid3$1171 = _tmp$2880 + _tmp$2881;
    _tmp$2878 = hi$1159 + hi$1159;
    if (mid3$1171 < mid$1158) {
      _tmp$2879 = 1ull;
    } else {
      _tmp$2879 = 0ull;
    }
    hi3$1172 = _tmp$2878 + _tmp$2879;
    lo4$1173 = lo3$1170 - _mul0$1147;
    _tmp$2876 = mid3$1171 - _mul1$1149;
    if (lo3$1170 < lo4$1173) {
      _tmp$2877 = 1ull;
    } else {
      _tmp$2877 = 0ull;
    }
    mid4$1174 = _tmp$2876 - _tmp$2877;
    if (mid3$1171 < mid4$1174) {
      _tmp$2875 = 1ull;
    } else {
      _tmp$2875 = 0ull;
    }
    hi4$1175 = hi3$1172 - _tmp$2875;
    _tmp$2874 = j$1164 - 64;
    vm$1165
    = $moonbitlang$core$double$internal$ryu$shiftright128(
      mid4$1174, hi4$1175, _tmp$2874
    );
  }
  _tmp$2884 = j$1164 - 64;
  _tmp$2883 = _tmp$2884 - 1;
  vr$1176
  = $moonbitlang$core$double$internal$ryu$shiftright128(
    mid$1158, hi$1159, _tmp$2883
  );
  _tmp$2882 = vm$1165;
  return (struct $$moonbitlang$core$double$internal$ryu$MulShiftAll64Result){
           vr$1176, vp$1163, _tmp$2882
         };
}

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf2(
  uint64_t value$1145,
  int32_t p$1146
) {
  uint64_t _tmp$2868 = 1ull << (p$1146 & 63);
  uint64_t _tmp$2867 = _tmp$2868 - 1ull;
  uint64_t _tmp$2866 = value$1145 & _tmp$2867;
  return _tmp$2866 == 0ull;
}

int32_t $moonbitlang$core$double$internal$ryu$multipleOfPowerOf5(
  uint64_t value$1143,
  int32_t p$1144
) {
  int32_t _tmp$2865 =
    $moonbitlang$core$double$internal$ryu$pow5Factor(value$1143);
  return _tmp$2865 >= p$1144;
}

int32_t $moonbitlang$core$double$internal$ryu$pow5Factor(uint64_t value$1139) {
  uint64_t _tmp$2853 = value$1139 % 5ull;
  uint64_t _tmp$2854;
  uint64_t _tmp$2855;
  uint64_t _tmp$2856;
  int32_t count$1140;
  uint64_t value$1141;
  uint64_t _tmp$2864;
  moonbit_string_t _tmp$2863;
  moonbit_string_t _tmp$2862;
  if (_tmp$2853 != 0ull) {
    return 0;
  }
  _tmp$2854 = value$1139 % 25ull;
  if (_tmp$2854 != 0ull) {
    return 1;
  }
  _tmp$2855 = value$1139 % 125ull;
  if (_tmp$2855 != 0ull) {
    return 2;
  }
  _tmp$2856 = value$1139 % 625ull;
  if (_tmp$2856 != 0ull) {
    return 3;
  }
  count$1140 = 4;
  value$1141 = value$1139 / 625ull;
  while (1) {
    uint64_t _tmp$2857 = value$1141;
    if (_tmp$2857 > 0ull) {
      uint64_t _tmp$2859 = value$1141;
      uint64_t _tmp$2858 = _tmp$2859 % 5ull;
      uint64_t _tmp$2860;
      int32_t _tmp$2861;
      if (_tmp$2858 != 0ull) {
        return count$1140;
      }
      _tmp$2860 = value$1141;
      value$1141 = _tmp$2860 / 5ull;
      _tmp$2861 = count$1140;
      count$1140 = _tmp$2861 + 1;
      continue;
    }
    break;
  }
  _tmp$2864 = value$1141;
  _tmp$2863
  = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
    _tmp$2864
  );
  _tmp$2862
  = moonbit_add_string(
    (moonbit_string_t)moonbit_string_literal_238.data, _tmp$2863
  );
  return $moonbitlang$core$builtin$abort$1(
           _tmp$2862, (moonbit_string_t)moonbit_string_literal_239.data
         );
}

uint64_t $moonbitlang$core$double$internal$ryu$shiftright128(
  uint64_t lo$1138,
  uint64_t hi$1136,
  int32_t dist$1137
) {
  int32_t _tmp$2852 = 64 - dist$1137;
  uint64_t _tmp$2850 = hi$1136 << (_tmp$2852 & 63);
  uint64_t _tmp$2851 = lo$1138 >> (dist$1137 & 63);
  return _tmp$2850 | _tmp$2851;
}

struct $$moonbitlang$core$double$internal$ryu$Umul128 $moonbitlang$core$double$internal$ryu$umul128(
  uint64_t a$1126,
  uint64_t b$1129
) {
  uint64_t aLo$1125 = a$1126 & 4294967295ull;
  uint64_t aHi$1127 = a$1126 >> 32;
  uint64_t bLo$1128 = b$1129 & 4294967295ull;
  uint64_t bHi$1130 = b$1129 >> 32;
  uint64_t x$1131 = aLo$1125 * bLo$1128;
  uint64_t _tmp$2848 = aHi$1127 * bLo$1128;
  uint64_t _tmp$2849 = x$1131 >> 32;
  uint64_t y$1132 = _tmp$2848 + _tmp$2849;
  uint64_t _tmp$2846 = aLo$1125 * bHi$1130;
  uint64_t _tmp$2847 = y$1132 & 4294967295ull;
  uint64_t z$1133 = _tmp$2846 + _tmp$2847;
  uint64_t _tmp$2844 = aHi$1127 * bHi$1130;
  uint64_t _tmp$2845 = y$1132 >> 32;
  uint64_t _tmp$2842 = _tmp$2844 + _tmp$2845;
  uint64_t _tmp$2843 = z$1133 >> 32;
  uint64_t w$1134 = _tmp$2842 + _tmp$2843;
  uint64_t lo$1135 = a$1126 * b$1129;
  return (struct $$moonbitlang$core$double$internal$ryu$Umul128){
           lo$1135, w$1134
         };
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$string_from_bytes(
  moonbit_bytes_t bytes$1120,
  int32_t from$1124,
  int32_t to$1122
) {
  int32_t _tmp$2841 = Moonbit_array_length(bytes$1120);
  struct $$moonbitlang$core$builtin$StringBuilder* buf$1119 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(_tmp$2841);
  int32_t i$1121 = from$1124;
  while (1) {
    if (i$1121 < to$1122) {
      int32_t _tmp$2839;
      int32_t _tmp$2838;
      int32_t _tmp$2840;
      if (i$1121 < 0 || i$1121 >= Moonbit_array_length(bytes$1120)) {
        moonbit_panic();
      }
      _tmp$2839 = (int32_t)bytes$1120[i$1121];
      _tmp$2838 = $Byte$$to_char(_tmp$2839);
      moonbit_incref(buf$1119);
      $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
        buf$1119, _tmp$2838
      );
      _tmp$2840 = i$1121 + 1;
      i$1121 = _tmp$2840;
      continue;
    } else {
      moonbit_decref(bytes$1120);
    }
    break;
  }
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$1119);
}

int32_t $moonbitlang$core$double$internal$ryu$log10Pow2(int32_t e$1118) {
  int32_t _tmp$2837 = e$1118 * 78913;
  uint32_t _tmp$2836 = *(uint32_t*)&_tmp$2837;
  uint32_t _tmp$2835 = _tmp$2836 >> 18;
  return *(int32_t*)&_tmp$2835;
}

int32_t $moonbitlang$core$double$internal$ryu$log10Pow5(int32_t e$1117) {
  int32_t _tmp$2834 = e$1117 * 732923;
  uint32_t _tmp$2833 = *(uint32_t*)&_tmp$2834;
  uint32_t _tmp$2832 = _tmp$2833 >> 20;
  return *(int32_t*)&_tmp$2832;
}

moonbit_string_t $moonbitlang$core$double$internal$ryu$copy_special_str(
  int32_t sign$1115,
  int32_t exponent$1116,
  int32_t mantissa$1113
) {
  moonbit_string_t s$1114;
  if (mantissa$1113) {
    return (moonbit_string_t)moonbit_string_literal_240.data;
  }
  if (sign$1115) {
    s$1114 = (moonbit_string_t)moonbit_string_literal_1.data;
  } else {
    s$1114 = (moonbit_string_t)moonbit_string_literal_3.data;
  }
  if (exponent$1116) {
    return moonbit_add_string(
             s$1114, (moonbit_string_t)moonbit_string_literal_241.data
           );
  }
  return moonbit_add_string(
           s$1114, (moonbit_string_t)moonbit_string_literal_242.data
         );
}

int32_t $moonbitlang$core$double$internal$ryu$pow5bits(int32_t e$1112) {
  int32_t _tmp$2831 = e$1112 * 1217359;
  uint32_t _tmp$2830 = *(uint32_t*)&_tmp$2831;
  uint32_t _tmp$2829 = _tmp$2830 >> 19;
  int32_t _tmp$2828 = *(int32_t*)&_tmp$2829;
  return _tmp$2828 + 1;
}

int32_t $$moonbitlang$core$builtin$Array$$is_empty$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1111
) {
  int32_t _field$4186 = self$1111->$1;
  int32_t len$2827;
  moonbit_decref(self$1111);
  len$2827 = _field$4186;
  return len$2827 == 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
  moonbit_string_t self$1109,
  struct $$moonbitlang$core$builtin$Logger logger$1110
) {
  moonbit_string_t _tmp$2826 = self$1109;
  struct $$moonbitlang$core$builtin$SourceLocRepr* _tmp$2825 =
    $$moonbitlang$core$builtin$SourceLocRepr$$parse(_tmp$2826);
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
    _tmp$2825, logger$1110
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLocRepr$$output(
  struct $$moonbitlang$core$builtin$SourceLocRepr* self$1095,
  struct $$moonbitlang$core$builtin$Logger logger$1108
) {
  struct $StringView _field$4195 =
    (struct $StringView){self$1095->$0_1, self$1095->$0_2, self$1095->$0_0};
  struct $StringView pkg$1094 = _field$4195;
  int32_t _tmp$2824 =
    Moonbit_array_length($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  struct $StringView _tmp$2823;
  int64_t _bind$1096;
  struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$* _bind$1097;
  struct $StringView _field$4194;
  struct $StringView _module_name$1104;
  void* _field$4193;
  int32_t _cnt$4475;
  void* _package_name$1105;
  struct $StringView _field$4191;
  struct $StringView filename$2806;
  struct $StringView _field$4190;
  struct $StringView start_line$2807;
  struct $StringView _field$4189;
  struct $StringView start_column$2808;
  struct $StringView _field$4188;
  struct $StringView end_line$2809;
  struct $StringView _field$4187;
  int32_t _cnt$4479;
  struct $StringView end_column$2810;
  struct $$moonbitlang$core$builtin$Logger _bind$2805;
  moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8193);
  _tmp$2823
  = (struct $StringView){
    0, _tmp$2824, $moonbitlang$core$builtin$output$$2a$bind$7c$8193
  };
  moonbit_incref(pkg$1094.$0);
  moonbit_incref(pkg$1094.$0);
  _bind$1096 = $StringView$$find(pkg$1094, _tmp$2823);
  if (_bind$1096 == 4294967296ll) {
    void* None$2811 =
      (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
    _bind$1097
    = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
        sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
      );
    Moonbit_object_header(_bind$1097)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
      )
      >> 2,
        2,
        0
    );
    _bind$1097->$0_0 = pkg$1094.$0;
    _bind$1097->$0_1 = pkg$1094.$1;
    _bind$1097->$0_2 = pkg$1094.$2;
    _bind$1097->$1 = None$2811;
  } else {
    int64_t _Some$1098 = _bind$1096;
    int32_t _first_slash$1099 = (int32_t)_Some$1098;
    int32_t _tmp$2822 = _first_slash$1099 + 1;
    struct $StringView _tmp$2819;
    int32_t _tmp$2821;
    struct $StringView _tmp$2820;
    int64_t _bind$1100;
    moonbit_incref(pkg$1094.$0);
    _tmp$2819 = $StringView$$view$inner(pkg$1094, _tmp$2822, 4294967296ll);
    _tmp$2821
    = Moonbit_array_length(
      $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    );
    moonbit_incref($moonbitlang$core$builtin$output$$2a$bind$7c$8187);
    _tmp$2820
    = (struct $StringView){
      0, _tmp$2821, $moonbitlang$core$builtin$output$$2a$bind$7c$8187
    };
    _bind$1100 = $StringView$$find(_tmp$2819, _tmp$2820);
    if (_bind$1100 == 4294967296ll) {
      void* None$2812 =
        (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _bind$1097
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$1097)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$1097->$0_0 = pkg$1094.$0;
      _bind$1097->$0_1 = pkg$1094.$1;
      _bind$1097->$0_2 = pkg$1094.$2;
      _bind$1097->$1 = None$2812;
    } else {
      int64_t _Some$1101 = _bind$1100;
      int32_t _second_slash$1102 = (int32_t)_Some$1101;
      int32_t _tmp$2818 = _first_slash$1099 + 1;
      int32_t module_name_end$1103 = _tmp$2818 + _second_slash$1102;
      int64_t _tmp$2817 = (int64_t)module_name_end$1103;
      struct $StringView _tmp$2813;
      int32_t _tmp$2816;
      struct $StringView _tmp$2815;
      void* Some$2814;
      moonbit_incref(pkg$1094.$0);
      _tmp$2813 = $StringView$$view$inner(pkg$1094, 0, _tmp$2817);
      _tmp$2816 = module_name_end$1103 + 1;
      _tmp$2815 = $StringView$$view$inner(pkg$1094, _tmp$2816, 4294967296ll);
      Some$2814
      = (void*)moonbit_malloc(sizeof(struct $Option$3c$StringView$3e$$Some));
      Moonbit_object_header(Some$2814)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $Option$3c$StringView$3e$$Some, $0_0) >> 2, 1, 1
      );
      ((struct $Option$3c$StringView$3e$$Some*)Some$2814)->$0_0
      = _tmp$2815.$0;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2814)->$0_1
      = _tmp$2815.$1;
      ((struct $Option$3c$StringView$3e$$Some*)Some$2814)->$0_2
      = _tmp$2815.$2;
      _bind$1097
      = (struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$)
        );
      Moonbit_object_header(_bind$1097)->meta
      = Moonbit_make_regular_object_header(
        offsetof(
          struct $$3c$StringView$2a$Option$3c$StringView$3e$$3e$, $0_0
        )
        >> 2,
          2,
          0
      );
      _bind$1097->$0_0 = _tmp$2813.$0;
      _bind$1097->$0_1 = _tmp$2813.$1;
      _bind$1097->$0_2 = _tmp$2813.$2;
      _bind$1097->$1 = Some$2814;
    }
  }
  _field$4194
  = (struct $StringView){
    _bind$1097->$0_1, _bind$1097->$0_2, _bind$1097->$0_0
  };
  _module_name$1104 = _field$4194;
  _field$4193 = _bind$1097->$1;
  _cnt$4475 = Moonbit_object_header(_bind$1097)->rc;
  if (_cnt$4475 > 1) {
    int32_t _new_cnt$4476;
    moonbit_incref(_field$4193);
    moonbit_incref(_module_name$1104.$0);
    _new_cnt$4476 = _cnt$4475 - 1;
    Moonbit_object_header(_bind$1097)->rc = _new_cnt$4476;
  } else if (_cnt$4475 == 1) {
    moonbit_free(_bind$1097);
  }
  _package_name$1105 = _field$4193;
  switch (Moonbit_object_tag(_package_name$1105)) {
    case 1: {
      struct $Option$3c$StringView$3e$$Some* _Some$1106 =
        (struct $Option$3c$StringView$3e$$Some*)_package_name$1105;
      struct $StringView _field$4192 =
        (struct $StringView){
          _Some$1106->$0_1, _Some$1106->$0_2, _Some$1106->$0_0
        };
      int32_t _cnt$4477 = Moonbit_object_header(_Some$1106)->rc;
      struct $StringView _pkg_name$1107;
      struct $$moonbitlang$core$builtin$Logger _bind$2804;
      if (_cnt$4477 > 1) {
        int32_t _new_cnt$4478;
        moonbit_incref(_field$4192.$0);
        _new_cnt$4478 = _cnt$4477 - 1;
        Moonbit_object_header(_Some$1106)->rc = _new_cnt$4478;
      } else if (_cnt$4477 == 1) {
        moonbit_free(_Some$1106);
      }
      _pkg_name$1107 = _field$4192;
      if (logger$1108.$1) {
        moonbit_incref(logger$1108.$1);
      }
      logger$1108.$0->$method_2(logger$1108.$1, _pkg_name$1107);
      _bind$2804 = logger$1108;
      if (_bind$2804.$1) {
        moonbit_incref(_bind$2804.$1);
      }
      _bind$2804.$0->$method_3(_bind$2804.$1, 47);
      break;
    }
    default: {
      moonbit_decref(_package_name$1105);
      break;
    }
  }
  _field$4191
  = (struct $StringView){
    self$1095->$1_1, self$1095->$1_2, self$1095->$1_0
  };
  filename$2806 = _field$4191;
  moonbit_incref(filename$2806.$0);
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_2(logger$1108.$1, filename$2806);
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_3(logger$1108.$1, 58);
  _field$4190
  = (struct $StringView){
    self$1095->$2_1, self$1095->$2_2, self$1095->$2_0
  };
  start_line$2807 = _field$4190;
  moonbit_incref(start_line$2807.$0);
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_2(logger$1108.$1, start_line$2807);
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_3(logger$1108.$1, 58);
  _field$4189
  = (struct $StringView){
    self$1095->$3_1, self$1095->$3_2, self$1095->$3_0
  };
  start_column$2808 = _field$4189;
  moonbit_incref(start_column$2808.$0);
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_2(logger$1108.$1, start_column$2808);
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_3(logger$1108.$1, 45);
  _field$4188
  = (struct $StringView){
    self$1095->$4_1, self$1095->$4_2, self$1095->$4_0
  };
  end_line$2809 = _field$4188;
  moonbit_incref(end_line$2809.$0);
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_2(logger$1108.$1, end_line$2809);
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_3(logger$1108.$1, 58);
  _field$4187
  = (struct $StringView){
    self$1095->$5_1, self$1095->$5_2, self$1095->$5_0
  };
  _cnt$4479 = Moonbit_object_header(self$1095)->rc;
  if (_cnt$4479 > 1) {
    int32_t _new_cnt$4485;
    moonbit_incref(_field$4187.$0);
    _new_cnt$4485 = _cnt$4479 - 1;
    Moonbit_object_header(self$1095)->rc = _new_cnt$4485;
  } else if (_cnt$4479 == 1) {
    struct $StringView _field$4484 =
      (struct $StringView){self$1095->$4_1, self$1095->$4_2, self$1095->$4_0};
    struct $StringView _field$4483;
    struct $StringView _field$4482;
    struct $StringView _field$4481;
    struct $StringView _field$4480;
    moonbit_decref(_field$4484.$0);
    _field$4483
    = (struct $StringView){
      self$1095->$3_1, self$1095->$3_2, self$1095->$3_0
    };
    moonbit_decref(_field$4483.$0);
    _field$4482
    = (struct $StringView){
      self$1095->$2_1, self$1095->$2_2, self$1095->$2_0
    };
    moonbit_decref(_field$4482.$0);
    _field$4481
    = (struct $StringView){
      self$1095->$1_1, self$1095->$1_2, self$1095->$1_0
    };
    moonbit_decref(_field$4481.$0);
    _field$4480
    = (struct $StringView){
      self$1095->$0_1, self$1095->$0_2, self$1095->$0_0
    };
    moonbit_decref(_field$4480.$0);
    moonbit_free(self$1095);
  }
  end_column$2810 = _field$4187;
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_2(logger$1108.$1, end_column$2810);
  if (logger$1108.$1) {
    moonbit_incref(logger$1108.$1);
  }
  logger$1108.$0->$method_3(logger$1108.$1, 64);
  _bind$2805 = logger$1108;
  _bind$2805.$0->$method_2(_bind$2805.$1, _module_name$1104);
  return 0;
}

uint64_t $Bool$$to_uint64(int32_t self$1093) {
  if (self$1093) {
    return 1ull;
  } else {
    return 0ull;
  }
}

int64_t $Bool$$to_int64(int32_t self$1092) {
  if (self$1092) {
    return 1ll;
  } else {
    return 0ll;
  }
}

int32_t $Bool$$to_int(int32_t self$1091) {
  if (self$1091) {
    return 1;
  } else {
    return 0;
  }
}

int32_t $moonbitlang$core$builtin$println$0(moonbit_string_t input$1090) {
  moonbit_string_t _tmp$2803 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(input$1090);
  moonbit_println(_tmp$2803);
  moonbit_decref(_tmp$2803);
  return 0;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_true(
  int32_t x$1085,
  moonbit_string_t msg$1087,
  moonbit_string_t loc$1089
) {
  if (!x$1085) {
    moonbit_string_t fail_msg$1086;
    if (msg$1087 == 0) {
      moonbit_string_t _tmp$2801;
      moonbit_string_t _tmp$2800;
      if (msg$1087) {
        moonbit_decref(msg$1087);
      }
      _tmp$2801
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$1085
      );
      _tmp$2800
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2801
      );
      fail_msg$1086
      = moonbit_add_string(
        _tmp$2800, (moonbit_string_t)moonbit_string_literal_244.data
      );
    } else {
      moonbit_string_t _Some$1088 = msg$1087;
      fail_msg$1086 = _Some$1088;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$1086, loc$1089);
  } else {
    int32_t _tmp$2802;
    struct moonbit_result_0 _result$4955;
    moonbit_decref(loc$1089);
    if (msg$1087) {
      moonbit_decref(msg$1087);
    }
    _tmp$2802 = 0;
    _result$4955.tag = 1;
    _result$4955.data.ok = _tmp$2802;
    return _result$4955;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_false(
  int32_t x$1080,
  moonbit_string_t msg$1082,
  moonbit_string_t loc$1084
) {
  if (x$1080) {
    moonbit_string_t fail_msg$1081;
    if (msg$1082 == 0) {
      moonbit_string_t _tmp$2798;
      moonbit_string_t _tmp$2797;
      if (msg$1082) {
        moonbit_decref(msg$1082);
      }
      _tmp$2798
      = $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
        x$1080
      );
      _tmp$2797
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2798
      );
      fail_msg$1081
      = moonbit_add_string(
        _tmp$2797, (moonbit_string_t)moonbit_string_literal_245.data
      );
    } else {
      moonbit_string_t _Some$1083 = msg$1082;
      fail_msg$1081 = _Some$1083;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$1081, loc$1084);
  } else {
    int32_t _tmp$2799;
    struct moonbit_result_0 _result$4956;
    moonbit_decref(loc$1084);
    if (msg$1082) {
      moonbit_decref(msg$1082);
    }
    _tmp$2799 = 0;
    _result$4956.tag = 1;
    _result$4956.data.ok = _tmp$2799;
    return _result$4956;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(
  int32_t self$1079,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1078
) {
  $$moonbitlang$core$builtin$Hasher$$combine_int(hasher$1078, self$1079);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hash$$String$$hash_combine(
  moonbit_string_t self$1077,
  struct $$moonbitlang$core$builtin$Hasher* hasher$1076
) {
  $$moonbitlang$core$builtin$Hasher$$combine_string(hasher$1076, self$1077);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_string(
  struct $$moonbitlang$core$builtin$Hasher* self$1074,
  moonbit_string_t value$1072
) {
  int32_t _end2448$1071 = Moonbit_array_length(value$1072);
  int32_t i$1073 = 0;
  while (1) {
    if (i$1073 < _end2448$1071) {
      int32_t _tmp$2795 = value$1072[i$1073];
      uint32_t _tmp$2794 = *(uint32_t*)&_tmp$2795;
      int32_t _tmp$2796;
      moonbit_incref(self$1074);
      $$moonbitlang$core$builtin$Hasher$$combine_uint(self$1074, _tmp$2794);
      _tmp$2796 = i$1073 + 1;
      i$1073 = _tmp$2796;
      continue;
    } else {
      moonbit_decref(self$1074);
      moonbit_decref(value$1072);
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$any$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$1068,
  struct $$3c$String$3e$$3d$$3e$Int* f$1070
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _closure$4958 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)moonbit_malloc(
      sizeof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap)
    );
  struct $$3c$String$3e$$3d$$3e$Int* _tmp$2791;
  int32_t _tmp$2790;
  Moonbit_object_header(_closure$4958)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iter$$any$7c$String$7c$$fn$3$2d$cap, $0) >> 2, 1, 0
  );
  _closure$4958->code = &$Iter$$any$7c$String$7c$$fn$3;
  _closure$4958->$0 = f$1070;
  _tmp$2791 = (struct $$3c$String$3e$$3d$$3e$Int*)_closure$4958;
  _tmp$2790 = $$moonbitlang$core$builtin$Iter$$run$0(self$1068, _tmp$2791);
  return $moonbitlang$core$builtin$op_notequal$0(_tmp$2790, 1);
}

int32_t $Iter$$any$7c$String$7c$$fn$3(
  struct $$3c$String$3e$$3d$$3e$Int* _env$2792,
  moonbit_string_t k$1069
) {
  struct $Iter$$any$7c$String$7c$$fn$3$2d$cap* _casted_env$2793 =
    (struct $Iter$$any$7c$String$7c$$fn$3$2d$cap*)_env$2792;
  struct $$3c$String$3e$$3d$$3e$Int* _field$4196 = _casted_env$2793->$0;
  int32_t _cnt$4486 = Moonbit_object_header(_casted_env$2793)->rc;
  struct $$3c$String$3e$$3d$$3e$Int* f$1070;
  if (_cnt$4486 > 1) {
    int32_t _new_cnt$4487;
    moonbit_incref(_field$4196);
    _new_cnt$4487 = _cnt$4486 - 1;
    Moonbit_object_header(_casted_env$2793)->rc = _new_cnt$4487;
  } else if (_cnt$4486 == 1) {
    moonbit_free(_casted_env$2793);
  }
  f$1070 = _field$4196;
  if (f$1070->code(f$1070, k$1069)) {
    return 0;
  } else {
    return 1;
  }
}

int32_t $$moonbitlang$core$builtin$Array$$unsafe_get$3(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$1066,
  int32_t idx$1067
) {
  int32_t* _tmp$2789 = $$moonbitlang$core$builtin$Array$$buffer$5(self$1066);
  int32_t _tmp$4197;
  if (idx$1067 < 0 || idx$1067 >= Moonbit_array_length(_tmp$2789)) {
    moonbit_panic();
  }
  _tmp$4197 = (int32_t)_tmp$2789[idx$1067];
  moonbit_decref(_tmp$2789);
  return _tmp$4197;
}

int32_t $$moonbitlang$core$builtin$Array$$unsafe_get$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$1064,
  int32_t idx$1065
) {
  int32_t* _tmp$2788 = $$moonbitlang$core$builtin$Array$$buffer$3(self$1064);
  int32_t _tmp$4198;
  if (idx$1065 < 0 || idx$1065 >= Moonbit_array_length(_tmp$2788)) {
    moonbit_panic();
  }
  _tmp$4198 = (int32_t)_tmp$2788[idx$1065];
  moonbit_decref(_tmp$2788);
  return _tmp$4198;
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$unsafe_get$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$1062,
  int32_t idx$1063
) {
  moonbit_string_t* _tmp$2787 =
    $$moonbitlang$core$builtin$Array$$buffer$1(self$1062);
  moonbit_string_t _tmp$4199;
  if (idx$1063 < 0 || idx$1063 >= Moonbit_array_length(_tmp$2787)) {
    moonbit_panic();
  }
  _tmp$4199 = (moonbit_string_t)_tmp$2787[idx$1063];
  moonbit_incref(_tmp$4199);
  moonbit_decref(_tmp$2787);
  return _tmp$4199;
}

struct $$3c$String$2a$Int$3e$* $$moonbitlang$core$builtin$Array$$unsafe_get$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$1060,
  int32_t idx$1061
) {
  struct $$3c$String$2a$Int$3e$** _tmp$2786 =
    $$moonbitlang$core$builtin$Array$$buffer$0(self$1060);
  struct $$3c$String$2a$Int$3e$* _tmp$4200;
  if (idx$1061 < 0 || idx$1061 >= Moonbit_array_length(_tmp$2786)) {
    moonbit_panic();
  }
  _tmp$4200 = (struct $$3c$String$2a$Int$3e$*)_tmp$2786[idx$1061];
  if (_tmp$4200) {
    moonbit_incref(_tmp$4200);
  }
  moonbit_decref(_tmp$2786);
  return _tmp$4200;
}

uint64_t $UInt$$to_uint64(uint32_t self$1059) {
  return (uint64_t)self$1059;
}

struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$5(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$1055,
  int32_t key$1051
) {
  int32_t hash$1050 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$1051);
  int32_t capacity_mask$2785 = self$1055->$3;
  int32_t _tmp$2784 = hash$1050 & capacity_mask$2785;
  int32_t i$1052 = 0;
  int32_t idx$1053 = _tmp$2784;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$4204 =
      self$1055->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2783 =
      _field$4204;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4203;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1054;
    if (idx$1053 < 0 || idx$1053 >= Moonbit_array_length(entries$2783)) {
      moonbit_panic();
    }
    _tmp$4203
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2783[
        idx$1053
      ];
    _bind$1054 = _tmp$4203;
    if (_bind$1054 == 0) {
      struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2772;
      if (_bind$1054) {
        moonbit_incref(_bind$1054);
      }
      moonbit_decref(self$1055);
      if (_bind$1054) {
        moonbit_decref(_bind$1054);
      }
      _tmp$2772 = 0;
      return _tmp$2772;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1056 =
        _bind$1054;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$1057 =
        _Some$1056;
      int32_t hash$2774 = _entry$1057->$3;
      int32_t _if_result$4960;
      int32_t _field$4201;
      int32_t psl$2777;
      int32_t _tmp$2779;
      int32_t _tmp$2781;
      int32_t capacity_mask$2782;
      int32_t _tmp$2780;
      if (hash$2774 == hash$1050) {
        int32_t key$2773 = _entry$1057->$4;
        _if_result$4960 = key$2773 == key$1051;
      } else {
        _if_result$4960 = 0;
      }
      if (_if_result$4960) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$4202;
        int32_t _cnt$4488;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2776;
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2775;
        moonbit_incref(_entry$1057);
        moonbit_decref(self$1055);
        _field$4202 = _entry$1057->$5;
        _cnt$4488 = Moonbit_object_header(_entry$1057)->rc;
        if (_cnt$4488 > 1) {
          int32_t _new_cnt$4490;
          moonbit_incref(_field$4202);
          _new_cnt$4490 = _cnt$4488 - 1;
          Moonbit_object_header(_entry$1057)->rc = _new_cnt$4490;
        } else if (_cnt$4488 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4489 =
            _entry$1057->$1;
          if (_field$4489) {
            moonbit_decref(_field$4489);
          }
          moonbit_free(_entry$1057);
        }
        value$2776 = _field$4202;
        _tmp$2775 = value$2776;
        return _tmp$2775;
      } else {
        moonbit_incref(_entry$1057);
      }
      _field$4201 = _entry$1057->$2;
      moonbit_decref(_entry$1057);
      psl$2777 = _field$4201;
      if (i$1052 > psl$2777) {
        struct $$3c$$3c$Int$2a$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2778;
        moonbit_decref(self$1055);
        _tmp$2778 = 0;
        return _tmp$2778;
      }
      _tmp$2779 = i$1052 + 1;
      _tmp$2781 = idx$1053 + 1;
      capacity_mask$2782 = self$1055->$3;
      _tmp$2780 = _tmp$2781 & capacity_mask$2782;
      i$1052 = _tmp$2779;
      idx$1053 = _tmp$2780;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$4(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$1046,
  moonbit_string_t key$1042
) {
  int32_t hash$1041;
  int32_t capacity_mask$2771;
  int32_t _tmp$2770;
  int32_t i$1043;
  int32_t idx$1044;
  moonbit_incref(key$1042);
  hash$1041
  = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
    key$1042
  );
  capacity_mask$2771 = self$1046->$3;
  _tmp$2770 = hash$1041 & capacity_mask$2771;
  i$1043 = 0;
  idx$1044 = _tmp$2770;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4210 =
      self$1046->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2769 =
      _field$4210;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4209;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$1045;
    if (idx$1044 < 0 || idx$1044 >= Moonbit_array_length(entries$2769)) {
      moonbit_panic();
    }
    _tmp$4209
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2769[
        idx$1044
      ];
    _bind$1045 = _tmp$4209;
    if (_bind$1045 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2758;
      if (_bind$1045) {
        moonbit_incref(_bind$1045);
      }
      moonbit_decref(self$1046);
      if (_bind$1045) {
        moonbit_decref(_bind$1045);
      }
      moonbit_decref(key$1042);
      _tmp$2758 = 0;
      return _tmp$2758;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$1047 =
        _bind$1045;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$1048 =
        _Some$1047;
      int32_t hash$2760 = _entry$1048->$3;
      int32_t _if_result$4962;
      int32_t _field$4205;
      int32_t psl$2763;
      int32_t _tmp$2765;
      int32_t _tmp$2767;
      int32_t capacity_mask$2768;
      int32_t _tmp$2766;
      if (hash$2760 == hash$1041) {
        moonbit_string_t _field$4208 = _entry$1048->$4;
        moonbit_string_t key$2759 = _field$4208;
        int32_t _tmp$4207 = moonbit_val_array_equal(key$2759, key$1042);
        _if_result$4962 = _tmp$4207;
      } else {
        _if_result$4962 = 0;
      }
      if (_if_result$4962) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4206;
        int32_t _cnt$4491;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2762;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2761;
        moonbit_incref(_entry$1048);
        moonbit_decref(self$1046);
        moonbit_decref(key$1042);
        _field$4206 = _entry$1048->$5;
        _cnt$4491 = Moonbit_object_header(_entry$1048)->rc;
        if (_cnt$4491 > 1) {
          int32_t _new_cnt$4494;
          moonbit_incref(_field$4206);
          _new_cnt$4494 = _cnt$4491 - 1;
          Moonbit_object_header(_entry$1048)->rc = _new_cnt$4494;
        } else if (_cnt$4491 == 1) {
          moonbit_string_t _field$4493 = _entry$1048->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4492;
          moonbit_decref(_field$4493);
          _field$4492 = _entry$1048->$1;
          if (_field$4492) {
            moonbit_decref(_field$4492);
          }
          moonbit_free(_entry$1048);
        }
        value$2762 = _field$4206;
        _tmp$2761 = value$2762;
        return _tmp$2761;
      } else {
        moonbit_incref(_entry$1048);
      }
      _field$4205 = _entry$1048->$2;
      moonbit_decref(_entry$1048);
      psl$2763 = _field$4205;
      if (i$1043 > psl$2763) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2764;
        moonbit_decref(self$1046);
        moonbit_decref(key$1042);
        _tmp$2764 = 0;
        return _tmp$2764;
      }
      _tmp$2765 = i$1043 + 1;
      _tmp$2767 = idx$1044 + 1;
      capacity_mask$2768 = self$1046->$3;
      _tmp$2766 = _tmp$2767 & capacity_mask$2768;
      i$1043 = _tmp$2765;
      idx$1044 = _tmp$2766;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$3(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$1037,
  int32_t key$1033
) {
  int32_t hash$1032 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$1033);
  int32_t capacity_mask$2757 = self$1037->$3;
  int32_t _tmp$2756 = hash$1032 & capacity_mask$2757;
  int32_t i$1034 = 0;
  int32_t idx$1035 = _tmp$2756;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$4214 =
      self$1037->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2755 =
      _field$4214;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4213;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1036;
    if (idx$1035 < 0 || idx$1035 >= Moonbit_array_length(entries$2755)) {
      moonbit_panic();
    }
    _tmp$4213
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2755[
        idx$1035
      ];
    _bind$1036 = _tmp$4213;
    if (_bind$1036 == 0) {
      struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2744;
      if (_bind$1036) {
        moonbit_incref(_bind$1036);
      }
      moonbit_decref(self$1037);
      if (_bind$1036) {
        moonbit_decref(_bind$1036);
      }
      _tmp$2744 = 0;
      return _tmp$2744;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1038 =
        _bind$1036;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$1039 =
        _Some$1038;
      int32_t hash$2746 = _entry$1039->$3;
      int32_t _if_result$4964;
      int32_t _field$4211;
      int32_t psl$2749;
      int32_t _tmp$2751;
      int32_t _tmp$2753;
      int32_t capacity_mask$2754;
      int32_t _tmp$2752;
      if (hash$2746 == hash$1032) {
        int32_t key$2745 = _entry$1039->$4;
        _if_result$4964 = key$2745 == key$1033;
      } else {
        _if_result$4964 = 0;
      }
      if (_if_result$4964) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$4212;
        int32_t _cnt$4495;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2748;
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2747;
        moonbit_incref(_entry$1039);
        moonbit_decref(self$1037);
        _field$4212 = _entry$1039->$5;
        _cnt$4495 = Moonbit_object_header(_entry$1039)->rc;
        if (_cnt$4495 > 1) {
          int32_t _new_cnt$4497;
          moonbit_incref(_field$4212);
          _new_cnt$4497 = _cnt$4495 - 1;
          Moonbit_object_header(_entry$1039)->rc = _new_cnt$4497;
        } else if (_cnt$4495 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4496 =
            _entry$1039->$1;
          if (_field$4496) {
            moonbit_decref(_field$4496);
          }
          moonbit_free(_entry$1039);
        }
        value$2748 = _field$4212;
        _tmp$2747 = value$2748;
        return _tmp$2747;
      } else {
        moonbit_incref(_entry$1039);
      }
      _field$4211 = _entry$1039->$2;
      moonbit_decref(_entry$1039);
      psl$2749 = _field$4211;
      if (i$1034 > psl$2749) {
        struct $$3c$$3c$Int$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2750;
        moonbit_decref(self$1037);
        _tmp$2750 = 0;
        return _tmp$2750;
      }
      _tmp$2751 = i$1034 + 1;
      _tmp$2753 = idx$1035 + 1;
      capacity_mask$2754 = self$1037->$3;
      _tmp$2752 = _tmp$2753 & capacity_mask$2754;
      i$1034 = _tmp$2751;
      idx$1035 = _tmp$2752;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$1028,
  moonbit_string_t key$1024
) {
  int32_t hash$1023;
  int32_t capacity_mask$2743;
  int32_t _tmp$2742;
  int32_t i$1025;
  int32_t idx$1026;
  moonbit_incref(key$1024);
  hash$1023
  = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
    key$1024
  );
  capacity_mask$2743 = self$1028->$3;
  _tmp$2742 = hash$1023 & capacity_mask$2743;
  i$1025 = 0;
  idx$1026 = _tmp$2742;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4220 =
      self$1028->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2741 =
      _field$4220;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4219;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$1027;
    if (idx$1026 < 0 || idx$1026 >= Moonbit_array_length(entries$2741)) {
      moonbit_panic();
    }
    _tmp$4219
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2741[
        idx$1026
      ];
    _bind$1027 = _tmp$4219;
    if (_bind$1027 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2730;
      if (_bind$1027) {
        moonbit_incref(_bind$1027);
      }
      moonbit_decref(self$1028);
      if (_bind$1027) {
        moonbit_decref(_bind$1027);
      }
      moonbit_decref(key$1024);
      _tmp$2730 = 0;
      return _tmp$2730;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$1029 =
        _bind$1027;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$1030 =
        _Some$1029;
      int32_t hash$2732 = _entry$1030->$3;
      int32_t _if_result$4966;
      int32_t _field$4215;
      int32_t psl$2735;
      int32_t _tmp$2737;
      int32_t _tmp$2739;
      int32_t capacity_mask$2740;
      int32_t _tmp$2738;
      if (hash$2732 == hash$1023) {
        moonbit_string_t _field$4218 = _entry$1030->$4;
        moonbit_string_t key$2731 = _field$4218;
        int32_t _tmp$4217 = moonbit_val_array_equal(key$2731, key$1024);
        _if_result$4966 = _tmp$4217;
      } else {
        _if_result$4966 = 0;
      }
      if (_if_result$4966) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4216;
        int32_t _cnt$4498;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2734;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2733;
        moonbit_incref(_entry$1030);
        moonbit_decref(self$1028);
        moonbit_decref(key$1024);
        _field$4216 = _entry$1030->$5;
        _cnt$4498 = Moonbit_object_header(_entry$1030)->rc;
        if (_cnt$4498 > 1) {
          int32_t _new_cnt$4501;
          moonbit_incref(_field$4216);
          _new_cnt$4501 = _cnt$4498 - 1;
          Moonbit_object_header(_entry$1030)->rc = _new_cnt$4501;
        } else if (_cnt$4498 == 1) {
          moonbit_string_t _field$4500 = _entry$1030->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4499;
          moonbit_decref(_field$4500);
          _field$4499 = _entry$1030->$1;
          if (_field$4499) {
            moonbit_decref(_field$4499);
          }
          moonbit_free(_entry$1030);
        }
        value$2734 = _field$4216;
        _tmp$2733 = value$2734;
        return _tmp$2733;
      } else {
        moonbit_incref(_entry$1030);
      }
      _field$4215 = _entry$1030->$2;
      moonbit_decref(_entry$1030);
      psl$2735 = _field$4215;
      if (i$1025 > psl$2735) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2736;
        moonbit_decref(self$1028);
        moonbit_decref(key$1024);
        _tmp$2736 = 0;
        return _tmp$2736;
      }
      _tmp$2737 = i$1025 + 1;
      _tmp$2739 = idx$1026 + 1;
      capacity_mask$2740 = self$1028->$3;
      _tmp$2738 = _tmp$2739 & capacity_mask$2740;
      i$1025 = _tmp$2737;
      idx$1026 = _tmp$2738;
      continue;
    }
    break;
  }
}

struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$1019,
  int32_t key$1015
) {
  int32_t hash$1014 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$1015);
  int32_t capacity_mask$2729 = self$1019->$3;
  int32_t _tmp$2728 = hash$1014 & capacity_mask$2729;
  int32_t i$1016 = 0;
  int32_t idx$1017 = _tmp$2728;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$4224 =
      self$1019->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2727 =
      _field$4224;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4223;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$1018;
    if (idx$1017 < 0 || idx$1017 >= Moonbit_array_length(entries$2727)) {
      moonbit_panic();
    }
    _tmp$4223
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2727[
        idx$1017
      ];
    _bind$1018 = _tmp$4223;
    if (_bind$1018 == 0) {
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2716;
      if (_bind$1018) {
        moonbit_incref(_bind$1018);
      }
      moonbit_decref(self$1019);
      if (_bind$1018) {
        moonbit_decref(_bind$1018);
      }
      _tmp$2716 = 0;
      return _tmp$2716;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$1020 =
        _bind$1018;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _entry$1021 =
        _Some$1020;
      int32_t hash$2718 = _entry$1021->$3;
      int32_t _if_result$4968;
      int32_t _field$4221;
      int32_t psl$2721;
      int32_t _tmp$2723;
      int32_t _tmp$2725;
      int32_t capacity_mask$2726;
      int32_t _tmp$2724;
      if (hash$2718 == hash$1014) {
        int32_t key$2717 = _entry$1021->$4;
        _if_result$4968 = key$2717 == key$1015;
      } else {
        _if_result$4968 = 0;
      }
      if (_if_result$4968) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$4222;
        int32_t _cnt$4502;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$2720;
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2719;
        moonbit_incref(_entry$1021);
        moonbit_decref(self$1019);
        _field$4222 = _entry$1021->$5;
        _cnt$4502 = Moonbit_object_header(_entry$1021)->rc;
        if (_cnt$4502 > 1) {
          int32_t _new_cnt$4504;
          moonbit_incref(_field$4222);
          _new_cnt$4504 = _cnt$4502 - 1;
          Moonbit_object_header(_entry$1021)->rc = _new_cnt$4504;
        } else if (_cnt$4502 == 1) {
          struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4503 =
            _entry$1021->$1;
          if (_field$4503) {
            moonbit_decref(_field$4503);
          }
          moonbit_free(_entry$1021);
        }
        value$2720 = _field$4222;
        _tmp$2719 = value$2720;
        return _tmp$2719;
      } else {
        moonbit_incref(_entry$1021);
      }
      _field$4221 = _entry$1021->$2;
      moonbit_decref(_entry$1021);
      psl$2721 = _field$4221;
      if (i$1016 > psl$2721) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2722;
        moonbit_decref(self$1019);
        _tmp$2722 = 0;
        return _tmp$2722;
      }
      _tmp$2723 = i$1016 + 1;
      _tmp$2725 = idx$1017 + 1;
      capacity_mask$2726 = self$1019->$3;
      _tmp$2724 = _tmp$2725 & capacity_mask$2726;
      i$1016 = _tmp$2723;
      idx$1017 = _tmp$2724;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$get$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$1010,
  moonbit_string_t key$1006
) {
  int32_t hash$1005;
  int32_t capacity_mask$2715;
  int32_t _tmp$2714;
  int32_t i$1007;
  int32_t idx$1008;
  moonbit_incref(key$1006);
  hash$1005
  = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
    key$1006
  );
  capacity_mask$2715 = self$1010->$3;
  _tmp$2714 = hash$1005 & capacity_mask$2715;
  i$1007 = 0;
  idx$1008 = _tmp$2714;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4230 =
      self$1010->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2713 =
      _field$4230;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4229;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$1009;
    if (idx$1008 < 0 || idx$1008 >= Moonbit_array_length(entries$2713)) {
      moonbit_panic();
    }
    _tmp$4229
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2713[
        idx$1008
      ];
    _bind$1009 = _tmp$4229;
    if (_bind$1009 == 0) {
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2702;
      if (_bind$1009) {
        moonbit_incref(_bind$1009);
      }
      moonbit_decref(self$1010);
      if (_bind$1009) {
        moonbit_decref(_bind$1009);
      }
      moonbit_decref(key$1006);
      _tmp$2702 = 0;
      return _tmp$2702;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$1011 =
        _bind$1009;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _entry$1012 =
        _Some$1011;
      int32_t hash$2704 = _entry$1012->$3;
      int32_t _if_result$4970;
      int32_t _field$4225;
      int32_t psl$2707;
      int32_t _tmp$2709;
      int32_t _tmp$2711;
      int32_t capacity_mask$2712;
      int32_t _tmp$2710;
      if (hash$2704 == hash$1005) {
        moonbit_string_t _field$4228 = _entry$1012->$4;
        moonbit_string_t key$2703 = _field$4228;
        int32_t _tmp$4227 = moonbit_val_array_equal(key$2703, key$1006);
        _if_result$4970 = _tmp$4227;
      } else {
        _if_result$4970 = 0;
      }
      if (_if_result$4970) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4226;
        int32_t _cnt$4505;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$2706;
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2705;
        moonbit_incref(_entry$1012);
        moonbit_decref(self$1010);
        moonbit_decref(key$1006);
        _field$4226 = _entry$1012->$5;
        _cnt$4505 = Moonbit_object_header(_entry$1012)->rc;
        if (_cnt$4505 > 1) {
          int32_t _new_cnt$4508;
          moonbit_incref(_field$4226);
          _new_cnt$4508 = _cnt$4505 - 1;
          Moonbit_object_header(_entry$1012)->rc = _new_cnt$4508;
        } else if (_cnt$4505 == 1) {
          moonbit_string_t _field$4507 = _entry$1012->$4;
          struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4506;
          moonbit_decref(_field$4507);
          _field$4506 = _entry$1012->$1;
          if (_field$4506) {
            moonbit_decref(_field$4506);
          }
          moonbit_free(_entry$1012);
        }
        value$2706 = _field$4226;
        _tmp$2705 = value$2706;
        return _tmp$2705;
      } else {
        moonbit_incref(_entry$1012);
      }
      _field$4225 = _entry$1012->$2;
      moonbit_decref(_entry$1012);
      psl$2707 = _field$4225;
      if (i$1007 > psl$2707) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2708;
        moonbit_decref(self$1010);
        moonbit_decref(key$1006);
        _tmp$2708 = 0;
        return _tmp$2708;
      }
      _tmp$2709 = i$1007 + 1;
      _tmp$2711 = idx$1008 + 1;
      capacity_mask$2712 = self$1010->$3;
      _tmp$2710 = _tmp$2711 & capacity_mask$2712;
      i$1007 = _tmp$2709;
      idx$1008 = _tmp$2710;
      continue;
    }
    break;
  }
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$998
) {
  int32_t length$997;
  int32_t capacity$999;
  int32_t _tmp$2693;
  int32_t _tmp$2692;
  int32_t _tmp$2701;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$1000;
  int32_t _len$1001;
  int32_t _i$1002;
  moonbit_incref(arr$998.$0);
  length$997 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$998);
  capacity$999 = $Int$$next_power_of_two(length$997);
  _tmp$2693 = capacity$999;
  _tmp$2692 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2693);
  if (length$997 > _tmp$2692) {
    int32_t _tmp$2694 = capacity$999;
    capacity$999 = _tmp$2694 * 2;
  }
  _tmp$2701 = capacity$999;
  m$1000 = $$moonbitlang$core$builtin$Map$$new$inner$3(_tmp$2701);
  moonbit_incref(arr$998.$0);
  _len$1001 = $$moonbitlang$core$builtin$ArrayView$$length$3(arr$998);
  _i$1002 = 0;
  while (1) {
    if (_i$1002 < _len$1001) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4234 =
        arr$998.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2697 =
        _field$4234;
      int32_t start$2699 = arr$998.$1;
      int32_t _tmp$2698 = start$2699 + _i$1002;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4233 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2697[
          _tmp$2698
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$1003 =
        _tmp$4233;
      moonbit_string_t _field$4232 = e$1003->$0;
      moonbit_string_t _tmp$2695 = _field$4232;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4231 =
        e$1003->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2696 =
        _field$4231;
      int32_t _tmp$2700;
      moonbit_incref(_tmp$2696);
      moonbit_incref(_tmp$2695);
      moonbit_incref(m$1000);
      $$moonbitlang$core$builtin$Map$$set$3(m$1000, _tmp$2695, _tmp$2696);
      _tmp$2700 = _i$1002 + 1;
      _i$1002 = _tmp$2700;
      continue;
    } else {
      moonbit_decref(arr$998.$0);
    }
    break;
  }
  return m$1000;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$990
) {
  int32_t length$989;
  int32_t capacity$991;
  int32_t _tmp$2683;
  int32_t _tmp$2682;
  int32_t _tmp$2691;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$992;
  int32_t _len$993;
  int32_t _i$994;
  moonbit_incref(arr$990.$0);
  length$989 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$990);
  capacity$991 = $Int$$next_power_of_two(length$989);
  _tmp$2683 = capacity$991;
  _tmp$2682 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2683);
  if (length$989 > _tmp$2682) {
    int32_t _tmp$2684 = capacity$991;
    capacity$991 = _tmp$2684 * 2;
  }
  _tmp$2691 = capacity$991;
  m$992 = $$moonbitlang$core$builtin$Map$$new$inner$2(_tmp$2691);
  moonbit_incref(arr$990.$0);
  _len$993 = $$moonbitlang$core$builtin$ArrayView$$length$2(arr$990);
  _i$994 = 0;
  while (1) {
    if (_i$994 < _len$993) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4238 =
        arr$990.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2687 =
        _field$4238;
      int32_t start$2689 = arr$990.$1;
      int32_t _tmp$2688 = start$2689 + _i$994;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4237 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2687[
          _tmp$2688
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$995 =
        _tmp$4237;
      moonbit_string_t _field$4236 = e$995->$0;
      moonbit_string_t _tmp$2685 = _field$4236;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4235 =
        e$995->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2686 =
        _field$4235;
      int32_t _tmp$2690;
      moonbit_incref(_tmp$2686);
      moonbit_incref(_tmp$2685);
      moonbit_incref(m$992);
      $$moonbitlang$core$builtin$Map$$set$2(m$992, _tmp$2685, _tmp$2686);
      _tmp$2690 = _i$994 + 1;
      _i$994 = _tmp$2690;
      continue;
    } else {
      moonbit_decref(arr$990.$0);
    }
    break;
  }
  return m$992;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ arr$982
) {
  int32_t length$981;
  int32_t capacity$983;
  int32_t _tmp$2673;
  int32_t _tmp$2672;
  int32_t _tmp$2681;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* m$984;
  int32_t _len$985;
  int32_t _i$986;
  moonbit_incref(arr$982.$0);
  length$981 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$982);
  capacity$983 = $Int$$next_power_of_two(length$981);
  _tmp$2673 = capacity$983;
  _tmp$2672 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2673);
  if (length$981 > _tmp$2672) {
    int32_t _tmp$2674 = capacity$983;
    capacity$983 = _tmp$2674 * 2;
  }
  _tmp$2681 = capacity$983;
  m$984 = $$moonbitlang$core$builtin$Map$$new$inner$1(_tmp$2681);
  moonbit_incref(arr$982.$0);
  _len$985 = $$moonbitlang$core$builtin$ArrayView$$length$1(arr$982);
  _i$986 = 0;
  while (1) {
    if (_i$986 < _len$985) {
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$4241 =
        arr$982.$0;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** buf$2677 =
        _field$4241;
      int32_t start$2679 = arr$982.$1;
      int32_t _tmp$2678 = start$2679 + _i$986;
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4240 =
        (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)buf$2677[
          _tmp$2678
        ];
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* e$987 =
        _tmp$4240;
      int32_t _tmp$2675 = e$987->$0;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$4239 =
        e$987->$1;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tmp$2676 =
        _field$4239;
      int32_t _tmp$2680;
      moonbit_incref(_tmp$2676);
      moonbit_incref(m$984);
      $$moonbitlang$core$builtin$Map$$set$1(m$984, _tmp$2675, _tmp$2676);
      _tmp$2680 = _i$986 + 1;
      _i$986 = _tmp$2680;
      continue;
    } else {
      moonbit_decref(arr$982.$0);
    }
    break;
  }
  return m$984;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$from_array$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ arr$974
) {
  int32_t length$973;
  int32_t capacity$975;
  int32_t _tmp$2663;
  int32_t _tmp$2662;
  int32_t _tmp$2671;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* m$976;
  int32_t _len$977;
  int32_t _i$978;
  moonbit_incref(arr$974.$0);
  length$973 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$974);
  capacity$975 = $Int$$next_power_of_two(length$973);
  _tmp$2663 = capacity$975;
  _tmp$2662 = $moonbitlang$core$builtin$calc_grow_threshold(_tmp$2663);
  if (length$973 > _tmp$2662) {
    int32_t _tmp$2664 = capacity$975;
    capacity$975 = _tmp$2664 * 2;
  }
  _tmp$2671 = capacity$975;
  m$976 = $$moonbitlang$core$builtin$Map$$new$inner$0(_tmp$2671);
  moonbit_incref(arr$974.$0);
  _len$977 = $$moonbitlang$core$builtin$ArrayView$$length$0(arr$974);
  _i$978 = 0;
  while (1) {
    if (_i$978 < _len$977) {
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4245 =
        arr$974.$0;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** buf$2667 =
        _field$4245;
      int32_t start$2669 = arr$974.$1;
      int32_t _tmp$2668 = start$2669 + _i$978;
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4244 =
        (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)buf$2667[
          _tmp$2668
        ];
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* e$979 =
        _tmp$4244;
      moonbit_string_t _field$4243 = e$979->$0;
      moonbit_string_t _tmp$2665 = _field$4243;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4242 =
        e$979->$1;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2666 =
        _field$4242;
      int32_t _tmp$2670;
      moonbit_incref(_tmp$2666);
      moonbit_incref(_tmp$2665);
      moonbit_incref(m$976);
      $$moonbitlang$core$builtin$Map$$set$0(m$976, _tmp$2665, _tmp$2666);
      _tmp$2670 = _i$978 + 1;
      _i$978 = _tmp$2670;
      continue;
    } else {
      moonbit_decref(arr$974.$0);
    }
    break;
  }
  return m$976;
}

int32_t $$moonbitlang$core$builtin$Map$$set$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$970,
  moonbit_string_t key$971,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$972
) {
  int32_t _tmp$2661;
  moonbit_incref(key$971);
  _tmp$2661 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$971);
  $$moonbitlang$core$builtin$Map$$set_with_hash$3(
    self$970, key$971, value$972, _tmp$2661
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$967,
  moonbit_string_t key$968,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$969
) {
  int32_t _tmp$2660;
  moonbit_incref(key$968);
  _tmp$2660 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$968);
  $$moonbitlang$core$builtin$Map$$set_with_hash$2(
    self$967, key$968, value$969, _tmp$2660
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$964,
  int32_t key$965,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$966
) {
  int32_t _tmp$2659 =
    $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(key$965);
  $$moonbitlang$core$builtin$Map$$set_with_hash$1(
    self$964, key$965, value$966, _tmp$2659
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$961,
  moonbit_string_t key$962,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$963
) {
  int32_t _tmp$2658;
  moonbit_incref(key$962);
  _tmp$2658 = $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(key$962);
  $$moonbitlang$core$builtin$Map$$set_with_hash$0(
    self$961, key$962, value$963, _tmp$2658
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$951
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4252 =
    self$951->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$950 =
    _field$4252;
  int32_t capacity$2657 = self$951->$2;
  int32_t new_capacity$952 = capacity$2657 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2652 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2651 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$952, _tmp$2652
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$4251 =
    self$951->$0;
  int32_t _tmp$2653;
  int32_t capacity$2655;
  int32_t _tmp$2654;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2656;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4250;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$953;
  if (old_head$950) {
    moonbit_incref(old_head$950);
  }
  moonbit_decref(_old$4251);
  self$951->$0 = _tmp$2651;
  self$951->$2 = new_capacity$952;
  _tmp$2653 = new_capacity$952 - 1;
  self$951->$3 = _tmp$2653;
  capacity$2655 = self$951->$2;
  _tmp$2654 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2655);
  self$951->$4 = _tmp$2654;
  self$951->$1 = 0;
  _tmp$2656 = 0;
  _old$4250 = self$951->$5;
  if (_old$4250) {
    moonbit_decref(_old$4250);
  }
  self$951->$5 = _tmp$2656;
  self$951->$6 = -1;
  _param$953 = old_head$950;
  while (1) {
    if (_param$953 == 0) {
      if (_param$953) {
        moonbit_decref(_param$953);
      }
      moonbit_decref(self$951);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$954 =
        _param$953;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$955 =
        _Some$954;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4249 =
        _x$955->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$956 =
        _field$4249;
      moonbit_string_t _field$4248 = _x$955->$4;
      moonbit_string_t _key$957 = _field$4248;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4247 =
        _x$955->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$958 =
        _field$4247;
      int32_t _field$4246 = _x$955->$3;
      int32_t _cnt$4509 = Moonbit_object_header(_x$955)->rc;
      int32_t _hash$959;
      if (_cnt$4509 > 1) {
        int32_t _new_cnt$4510;
        moonbit_incref(_value$958);
        moonbit_incref(_key$957);
        if (_next$956) {
          moonbit_incref(_next$956);
        }
        _new_cnt$4510 = _cnt$4509 - 1;
        Moonbit_object_header(_x$955)->rc = _new_cnt$4510;
      } else if (_cnt$4509 == 1) {
        moonbit_free(_x$955);
      }
      _hash$959 = _field$4246;
      moonbit_incref(self$951);
      $$moonbitlang$core$builtin$Map$$set_with_hash$3(
        self$951, _key$957, _value$958, _hash$959
      );
      _param$953 = _next$956;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$940
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4259 =
    self$940->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$939 =
    _field$4259;
  int32_t capacity$2650 = self$940->$2;
  int32_t new_capacity$941 = capacity$2650 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2645 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2644 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$941, _tmp$2645
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$4258 =
    self$940->$0;
  int32_t _tmp$2646;
  int32_t capacity$2648;
  int32_t _tmp$2647;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2649;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4257;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$942;
  if (old_head$939) {
    moonbit_incref(old_head$939);
  }
  moonbit_decref(_old$4258);
  self$940->$0 = _tmp$2644;
  self$940->$2 = new_capacity$941;
  _tmp$2646 = new_capacity$941 - 1;
  self$940->$3 = _tmp$2646;
  capacity$2648 = self$940->$2;
  _tmp$2647 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2648);
  self$940->$4 = _tmp$2647;
  self$940->$1 = 0;
  _tmp$2649 = 0;
  _old$4257 = self$940->$5;
  if (_old$4257) {
    moonbit_decref(_old$4257);
  }
  self$940->$5 = _tmp$2649;
  self$940->$6 = -1;
  _param$942 = old_head$939;
  while (1) {
    if (_param$942 == 0) {
      if (_param$942) {
        moonbit_decref(_param$942);
      }
      moonbit_decref(self$940);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$943 =
        _param$942;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$944 =
        _Some$943;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4256 =
        _x$944->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$945 =
        _field$4256;
      moonbit_string_t _field$4255 = _x$944->$4;
      moonbit_string_t _key$946 = _field$4255;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4254 =
        _x$944->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$947 =
        _field$4254;
      int32_t _field$4253 = _x$944->$3;
      int32_t _cnt$4511 = Moonbit_object_header(_x$944)->rc;
      int32_t _hash$948;
      if (_cnt$4511 > 1) {
        int32_t _new_cnt$4512;
        moonbit_incref(_value$947);
        moonbit_incref(_key$946);
        if (_next$945) {
          moonbit_incref(_next$945);
        }
        _new_cnt$4512 = _cnt$4511 - 1;
        Moonbit_object_header(_x$944)->rc = _new_cnt$4512;
      } else if (_cnt$4511 == 1) {
        moonbit_free(_x$944);
      }
      _hash$948 = _field$4253;
      moonbit_incref(self$940);
      $$moonbitlang$core$builtin$Map$$set_with_hash$2(
        self$940, _key$946, _value$947, _hash$948
      );
      _param$942 = _next$945;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$929
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4265 =
    self$929->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* old_head$928 =
    _field$4265;
  int32_t capacity$2643 = self$929->$2;
  int32_t new_capacity$930 = capacity$2643 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2638 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$2637 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$930, _tmp$2638
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _old$4264 =
    self$929->$0;
  int32_t _tmp$2639;
  int32_t capacity$2641;
  int32_t _tmp$2640;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2642;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$4263;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _param$931;
  if (old_head$928) {
    moonbit_incref(old_head$928);
  }
  moonbit_decref(_old$4264);
  self$929->$0 = _tmp$2637;
  self$929->$2 = new_capacity$930;
  _tmp$2639 = new_capacity$930 - 1;
  self$929->$3 = _tmp$2639;
  capacity$2641 = self$929->$2;
  _tmp$2640 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2641);
  self$929->$4 = _tmp$2640;
  self$929->$1 = 0;
  _tmp$2642 = 0;
  _old$4263 = self$929->$5;
  if (_old$4263) {
    moonbit_decref(_old$4263);
  }
  self$929->$5 = _tmp$2642;
  self$929->$6 = -1;
  _param$931 = old_head$928;
  while (1) {
    if (_param$931 == 0) {
      if (_param$931) {
        moonbit_decref(_param$931);
      }
      moonbit_decref(self$929);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$932 =
        _param$931;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _x$933 =
        _Some$932;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4262 =
        _x$933->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$934 =
        _field$4262;
      int32_t _key$935 = _x$933->$4;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$4261 =
        _x$933->$5;
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _value$936 =
        _field$4261;
      int32_t _field$4260 = _x$933->$3;
      int32_t _cnt$4513 = Moonbit_object_header(_x$933)->rc;
      int32_t _hash$937;
      if (_cnt$4513 > 1) {
        int32_t _new_cnt$4514;
        moonbit_incref(_value$936);
        if (_next$934) {
          moonbit_incref(_next$934);
        }
        _new_cnt$4514 = _cnt$4513 - 1;
        Moonbit_object_header(_x$933)->rc = _new_cnt$4514;
      } else if (_cnt$4513 == 1) {
        moonbit_free(_x$933);
      }
      _hash$937 = _field$4260;
      moonbit_incref(self$929);
      $$moonbitlang$core$builtin$Map$$set_with_hash$1(
        self$929, _key$935, _value$936, _hash$937
      );
      _param$931 = _next$934;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$grow$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$918
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4272 =
    self$918->$5;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* old_head$917 =
    _field$4272;
  int32_t capacity$2636 = self$918->$2;
  int32_t new_capacity$919 = capacity$2636 << 1;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2631 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$2630 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      new_capacity$919, _tmp$2631
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _old$4271 =
    self$918->$0;
  int32_t _tmp$2632;
  int32_t capacity$2634;
  int32_t _tmp$2633;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2635;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4270;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _param$920;
  if (old_head$917) {
    moonbit_incref(old_head$917);
  }
  moonbit_decref(_old$4271);
  self$918->$0 = _tmp$2630;
  self$918->$2 = new_capacity$919;
  _tmp$2632 = new_capacity$919 - 1;
  self$918->$3 = _tmp$2632;
  capacity$2634 = self$918->$2;
  _tmp$2633 = $moonbitlang$core$builtin$calc_grow_threshold(capacity$2634);
  self$918->$4 = _tmp$2633;
  self$918->$1 = 0;
  _tmp$2635 = 0;
  _old$4270 = self$918->$5;
  if (_old$4270) {
    moonbit_decref(_old$4270);
  }
  self$918->$5 = _tmp$2635;
  self$918->$6 = -1;
  _param$920 = old_head$917;
  while (1) {
    if (_param$920 == 0) {
      if (_param$920) {
        moonbit_decref(_param$920);
      }
      moonbit_decref(self$918);
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$921 =
        _param$920;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _x$922 =
        _Some$921;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4269 =
        _x$922->$1;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$923 =
        _field$4269;
      moonbit_string_t _field$4268 = _x$922->$4;
      moonbit_string_t _key$924 = _field$4268;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4267 =
        _x$922->$5;
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _value$925 =
        _field$4267;
      int32_t _field$4266 = _x$922->$3;
      int32_t _cnt$4515 = Moonbit_object_header(_x$922)->rc;
      int32_t _hash$926;
      if (_cnt$4515 > 1) {
        int32_t _new_cnt$4516;
        moonbit_incref(_value$925);
        moonbit_incref(_key$924);
        if (_next$923) {
          moonbit_incref(_next$923);
        }
        _new_cnt$4516 = _cnt$4515 - 1;
        Moonbit_object_header(_x$922)->rc = _new_cnt$4516;
      } else if (_cnt$4515 == 1) {
        moonbit_free(_x$922);
      }
      _hash$926 = _field$4266;
      moonbit_incref(self$918);
      $$moonbitlang$core$builtin$Map$$set_with_hash$0(
        self$918, _key$924, _value$925, _hash$926
      );
      _param$920 = _next$923;
      continue;
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$901,
  moonbit_string_t key$910,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$911,
  int32_t hash$909
) {
  int32_t size$2616 = self$901->$1;
  int32_t grow_at$2617 = self$901->$4;
  int32_t capacity_mask$2629;
  int32_t _tmp$2628;
  struct $$3c$Int$2a$Int$3e$* _bind$902;
  int32_t psl$903;
  int32_t idx$904;
  int32_t _idx$912;
  int32_t _field$4273;
  int32_t _psl$913;
  int32_t _bind$914;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$915;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$916;
  if (size$2616 >= grow_at$2617) {
    moonbit_incref(self$901);
    $$moonbitlang$core$builtin$Map$$grow$3(self$901);
  }
  capacity_mask$2629 = self$901->$3;
  _tmp$2628 = hash$909 & capacity_mask$2629;
  psl$903 = 0;
  idx$904 = _tmp$2628;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4278 =
      self$901->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2627 =
      _field$4278;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4277;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$905;
    if (idx$904 < 0 || idx$904 >= Moonbit_array_length(entries$2627)) {
      moonbit_panic();
    }
    _tmp$4277
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2627[
        idx$904
      ];
    _bind$905 = _tmp$4277;
    if (_bind$905 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2618 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2618)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2618->$0 = idx$904;
      _tuple$2618->$1 = psl$903;
      _bind$902 = _tuple$2618;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$907 =
        _bind$905;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$908 =
        _Some$907;
      int32_t hash$2620 = _curr_entry$908->$3;
      int32_t _if_result$4980;
      int32_t psl$2621;
      int32_t _tmp$2623;
      int32_t _tmp$2625;
      int32_t capacity_mask$2626;
      int32_t _tmp$2624;
      if (hash$2620 == hash$909) {
        moonbit_string_t _field$4276 = _curr_entry$908->$4;
        moonbit_string_t key$2619 = _field$4276;
        int32_t _tmp$4275 = moonbit_val_array_equal(key$2619, key$910);
        _if_result$4980 = _tmp$4275;
      } else {
        _if_result$4980 = 0;
      }
      if (_if_result$4980) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$4274;
        moonbit_incref(_curr_entry$908);
        moonbit_decref(key$910);
        moonbit_decref(self$901);
        _old$4274 = _curr_entry$908->$5;
        moonbit_decref(_old$4274);
        _curr_entry$908->$5 = value$911;
        moonbit_decref(_curr_entry$908);
        return 0;
      } else {
        moonbit_incref(_curr_entry$908);
      }
      psl$2621 = _curr_entry$908->$2;
      if (psl$903 > psl$2621) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2622;
        moonbit_incref(self$901);
        $$moonbitlang$core$builtin$Map$$push_away$3(
          self$901, idx$904, _curr_entry$908
        );
        _tuple$2622
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2622)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2622->$0 = idx$904;
        _tuple$2622->$1 = psl$903;
        _bind$902 = _tuple$2622;
        break;
      } else {
        moonbit_decref(_curr_entry$908);
      }
      _tmp$2623 = psl$903 + 1;
      _tmp$2625 = idx$904 + 1;
      capacity_mask$2626 = self$901->$3;
      _tmp$2624 = _tmp$2625 & capacity_mask$2626;
      psl$903 = _tmp$2623;
      idx$904 = _tmp$2624;
      continue;
    }
    break;
  }
  _idx$912 = _bind$902->$0;
  _field$4273 = _bind$902->$1;
  moonbit_decref(_bind$902);
  _psl$913 = _field$4273;
  _bind$914 = self$901->$6;
  _bind$915 = 0;
  entry$916
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$916)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$916->$0 = _bind$914;
  entry$916->$1 = _bind$915;
  entry$916->$2 = _psl$913;
  entry$916->$3 = hash$909;
  entry$916->$4 = key$910;
  entry$916->$5 = value$911;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
    self$901, _idx$912, entry$916
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$885,
  moonbit_string_t key$894,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$895,
  int32_t hash$893
) {
  int32_t size$2602 = self$885->$1;
  int32_t grow_at$2603 = self$885->$4;
  int32_t capacity_mask$2615;
  int32_t _tmp$2614;
  struct $$3c$Int$2a$Int$3e$* _bind$886;
  int32_t psl$887;
  int32_t idx$888;
  int32_t _idx$896;
  int32_t _field$4279;
  int32_t _psl$897;
  int32_t _bind$898;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$899;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$900;
  if (size$2602 >= grow_at$2603) {
    moonbit_incref(self$885);
    $$moonbitlang$core$builtin$Map$$grow$2(self$885);
  }
  capacity_mask$2615 = self$885->$3;
  _tmp$2614 = hash$893 & capacity_mask$2615;
  psl$887 = 0;
  idx$888 = _tmp$2614;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4284 =
      self$885->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2613 =
      _field$4284;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4283;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$889;
    if (idx$888 < 0 || idx$888 >= Moonbit_array_length(entries$2613)) {
      moonbit_panic();
    }
    _tmp$4283
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2613[
        idx$888
      ];
    _bind$889 = _tmp$4283;
    if (_bind$889 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2604 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2604)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2604->$0 = idx$888;
      _tuple$2604->$1 = psl$887;
      _bind$886 = _tuple$2604;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$891 =
        _bind$889;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$892 =
        _Some$891;
      int32_t hash$2606 = _curr_entry$892->$3;
      int32_t _if_result$4982;
      int32_t psl$2607;
      int32_t _tmp$2609;
      int32_t _tmp$2611;
      int32_t capacity_mask$2612;
      int32_t _tmp$2610;
      if (hash$2606 == hash$893) {
        moonbit_string_t _field$4282 = _curr_entry$892->$4;
        moonbit_string_t key$2605 = _field$4282;
        int32_t _tmp$4281 = moonbit_val_array_equal(key$2605, key$894);
        _if_result$4982 = _tmp$4281;
      } else {
        _if_result$4982 = 0;
      }
      if (_if_result$4982) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$4280;
        moonbit_incref(_curr_entry$892);
        moonbit_decref(key$894);
        moonbit_decref(self$885);
        _old$4280 = _curr_entry$892->$5;
        moonbit_decref(_old$4280);
        _curr_entry$892->$5 = value$895;
        moonbit_decref(_curr_entry$892);
        return 0;
      } else {
        moonbit_incref(_curr_entry$892);
      }
      psl$2607 = _curr_entry$892->$2;
      if (psl$887 > psl$2607) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2608;
        moonbit_incref(self$885);
        $$moonbitlang$core$builtin$Map$$push_away$2(
          self$885, idx$888, _curr_entry$892
        );
        _tuple$2608
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2608)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2608->$0 = idx$888;
        _tuple$2608->$1 = psl$887;
        _bind$886 = _tuple$2608;
        break;
      } else {
        moonbit_decref(_curr_entry$892);
      }
      _tmp$2609 = psl$887 + 1;
      _tmp$2611 = idx$888 + 1;
      capacity_mask$2612 = self$885->$3;
      _tmp$2610 = _tmp$2611 & capacity_mask$2612;
      psl$887 = _tmp$2609;
      idx$888 = _tmp$2610;
      continue;
    }
    break;
  }
  _idx$896 = _bind$886->$0;
  _field$4279 = _bind$886->$1;
  moonbit_decref(_bind$886);
  _psl$897 = _field$4279;
  _bind$898 = self$885->$6;
  _bind$899 = 0;
  entry$900
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$900)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$900->$0 = _bind$898;
  entry$900->$1 = _bind$899;
  entry$900->$2 = _psl$897;
  entry$900->$3 = hash$893;
  entry$900->$4 = key$894;
  entry$900->$5 = value$895;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
    self$885, _idx$896, entry$900
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$869,
  int32_t key$878,
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* value$879,
  int32_t hash$877
) {
  int32_t size$2588 = self$869->$1;
  int32_t grow_at$2589 = self$869->$4;
  int32_t capacity_mask$2601;
  int32_t _tmp$2600;
  struct $$3c$Int$2a$Int$3e$* _bind$870;
  int32_t psl$871;
  int32_t idx$872;
  int32_t _idx$880;
  int32_t _field$4285;
  int32_t _psl$881;
  int32_t _bind$882;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$883;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$884;
  if (size$2588 >= grow_at$2589) {
    moonbit_incref(self$869);
    $$moonbitlang$core$builtin$Map$$grow$1(self$869);
  }
  capacity_mask$2601 = self$869->$3;
  _tmp$2600 = hash$877 & capacity_mask$2601;
  psl$871 = 0;
  idx$872 = _tmp$2600;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$4288 =
      self$869->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2599 =
      _field$4288;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4287;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$873;
    if (idx$872 < 0 || idx$872 >= Moonbit_array_length(entries$2599)) {
      moonbit_panic();
    }
    _tmp$4287
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2599[
        idx$872
      ];
    _bind$873 = _tmp$4287;
    if (_bind$873 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2590 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2590)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2590->$0 = idx$872;
      _tuple$2590->$1 = psl$871;
      _bind$870 = _tuple$2590;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$875 =
        _bind$873;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$876 =
        _Some$875;
      int32_t hash$2592 = _curr_entry$876->$3;
      int32_t _if_result$4984;
      int32_t psl$2593;
      int32_t _tmp$2595;
      int32_t _tmp$2597;
      int32_t capacity_mask$2598;
      int32_t _tmp$2596;
      if (hash$2592 == hash$877) {
        int32_t key$2591 = _curr_entry$876->$4;
        _if_result$4984 = key$2591 == key$878;
      } else {
        _if_result$4984 = 0;
      }
      if (_if_result$4984) {
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _old$4286;
        moonbit_incref(_curr_entry$876);
        moonbit_decref(self$869);
        _old$4286 = _curr_entry$876->$5;
        moonbit_decref(_old$4286);
        _curr_entry$876->$5 = value$879;
        moonbit_decref(_curr_entry$876);
        return 0;
      } else {
        moonbit_incref(_curr_entry$876);
      }
      psl$2593 = _curr_entry$876->$2;
      if (psl$871 > psl$2593) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2594;
        moonbit_incref(self$869);
        $$moonbitlang$core$builtin$Map$$push_away$1(
          self$869, idx$872, _curr_entry$876
        );
        _tuple$2594
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2594)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2594->$0 = idx$872;
        _tuple$2594->$1 = psl$871;
        _bind$870 = _tuple$2594;
        break;
      } else {
        moonbit_decref(_curr_entry$876);
      }
      _tmp$2595 = psl$871 + 1;
      _tmp$2597 = idx$872 + 1;
      capacity_mask$2598 = self$869->$3;
      _tmp$2596 = _tmp$2597 & capacity_mask$2598;
      psl$871 = _tmp$2595;
      idx$872 = _tmp$2596;
      continue;
    }
    break;
  }
  _idx$880 = _bind$870->$0;
  _field$4285 = _bind$870->$1;
  moonbit_decref(_bind$870);
  _psl$881 = _field$4285;
  _bind$882 = self$869->$6;
  _bind$883 = 0;
  entry$884
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$884)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      2,
      0
  );
  entry$884->$0 = _bind$882;
  entry$884->$1 = _bind$883;
  entry$884->$2 = _psl$881;
  entry$884->$3 = hash$877;
  entry$884->$4 = key$878;
  entry$884->$5 = value$879;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
    self$869, _idx$880, entry$884
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_with_hash$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$853,
  moonbit_string_t key$862,
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* value$863,
  int32_t hash$861
) {
  int32_t size$2574 = self$853->$1;
  int32_t grow_at$2575 = self$853->$4;
  int32_t capacity_mask$2587;
  int32_t _tmp$2586;
  struct $$3c$Int$2a$Int$3e$* _bind$854;
  int32_t psl$855;
  int32_t idx$856;
  int32_t _idx$864;
  int32_t _field$4289;
  int32_t _psl$865;
  int32_t _bind$866;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$867;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$868;
  if (size$2574 >= grow_at$2575) {
    moonbit_incref(self$853);
    $$moonbitlang$core$builtin$Map$$grow$0(self$853);
  }
  capacity_mask$2587 = self$853->$3;
  _tmp$2586 = hash$861 & capacity_mask$2587;
  psl$855 = 0;
  idx$856 = _tmp$2586;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4294 =
      self$853->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2585 =
      _field$4294;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4293;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$857;
    if (idx$856 < 0 || idx$856 >= Moonbit_array_length(entries$2585)) {
      moonbit_panic();
    }
    _tmp$4293
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2585[
        idx$856
      ];
    _bind$857 = _tmp$4293;
    if (_bind$857 == 0) {
      struct $$3c$Int$2a$Int$3e$* _tuple$2576 =
        (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
          sizeof(struct $$3c$Int$2a$Int$3e$)
        );
      Moonbit_object_header(_tuple$2576)->meta
      = Moonbit_make_regular_object_header(
        sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
      );
      _tuple$2576->$0 = idx$856;
      _tuple$2576->$1 = psl$855;
      _bind$854 = _tuple$2576;
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$859 =
        _bind$857;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$860 =
        _Some$859;
      int32_t hash$2578 = _curr_entry$860->$3;
      int32_t _if_result$4986;
      int32_t psl$2579;
      int32_t _tmp$2581;
      int32_t _tmp$2583;
      int32_t capacity_mask$2584;
      int32_t _tmp$2582;
      if (hash$2578 == hash$861) {
        moonbit_string_t _field$4292 = _curr_entry$860->$4;
        moonbit_string_t key$2577 = _field$4292;
        int32_t _tmp$4291 = moonbit_val_array_equal(key$2577, key$862);
        _if_result$4986 = _tmp$4291;
      } else {
        _if_result$4986 = 0;
      }
      if (_if_result$4986) {
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$4290;
        moonbit_incref(_curr_entry$860);
        moonbit_decref(key$862);
        moonbit_decref(self$853);
        _old$4290 = _curr_entry$860->$5;
        moonbit_decref(_old$4290);
        _curr_entry$860->$5 = value$863;
        moonbit_decref(_curr_entry$860);
        return 0;
      } else {
        moonbit_incref(_curr_entry$860);
      }
      psl$2579 = _curr_entry$860->$2;
      if (psl$855 > psl$2579) {
        struct $$3c$Int$2a$Int$3e$* _tuple$2580;
        moonbit_incref(self$853);
        $$moonbitlang$core$builtin$Map$$push_away$0(
          self$853, idx$856, _curr_entry$860
        );
        _tuple$2580
        = (struct $$3c$Int$2a$Int$3e$*)moonbit_malloc(
            sizeof(struct $$3c$Int$2a$Int$3e$)
          );
        Moonbit_object_header(_tuple$2580)->meta
        = Moonbit_make_regular_object_header(
          sizeof(struct $$3c$Int$2a$Int$3e$) >> 2, 0, 0
        );
        _tuple$2580->$0 = idx$856;
        _tuple$2580->$1 = psl$855;
        _bind$854 = _tuple$2580;
        break;
      } else {
        moonbit_decref(_curr_entry$860);
      }
      _tmp$2581 = psl$855 + 1;
      _tmp$2583 = idx$856 + 1;
      capacity_mask$2584 = self$853->$3;
      _tmp$2582 = _tmp$2583 & capacity_mask$2584;
      psl$855 = _tmp$2581;
      idx$856 = _tmp$2582;
      continue;
    }
    break;
  }
  _idx$864 = _bind$854->$0;
  _field$4289 = _bind$854->$1;
  moonbit_decref(_bind$854);
  _psl$865 = _field$4289;
  _bind$866 = self$853->$6;
  _bind$867 = 0;
  entry$868
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(entry$868)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $1
    )
    >> 2,
      3,
      0
  );
  entry$868->$0 = _bind$866;
  entry$868->$1 = _bind$867;
  entry$868->$2 = _psl$865;
  entry$868->$3 = hash$861;
  entry$868->$4 = key$862;
  entry$868->$5 = value$863;
  $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
    self$853, _idx$864, entry$868
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$847,
  int32_t idx$852,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$851
) {
  int32_t psl$2573 = entry$851->$2;
  int32_t _tmp$2569 = psl$2573 + 1;
  int32_t _tmp$2571 = idx$852 + 1;
  int32_t capacity_mask$2572 = self$847->$3;
  int32_t _tmp$2570 = _tmp$2571 & capacity_mask$2572;
  int32_t psl$843 = _tmp$2569;
  int32_t idx$844 = _tmp$2570;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$845 =
    entry$851;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4296 =
      self$847->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2568 =
      _field$4296;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4295;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$846;
    if (idx$844 < 0 || idx$844 >= Moonbit_array_length(entries$2568)) {
      moonbit_panic();
    }
    _tmp$4295
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2568[
        idx$844
      ];
    _bind$846 = _tmp$4295;
    if (_bind$846 == 0) {
      entry$845->$2 = psl$843;
      $$moonbitlang$core$builtin$Map$$set_entry$3(
        self$847, entry$845, idx$844
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$849 =
        _bind$846;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$850 =
        _Some$849;
      int32_t psl$2558 = _curr_entry$850->$2;
      if (psl$843 > psl$2558) {
        int32_t psl$2563;
        int32_t _tmp$2559;
        int32_t _tmp$2561;
        int32_t capacity_mask$2562;
        int32_t _tmp$2560;
        entry$845->$2 = psl$843;
        moonbit_incref(_curr_entry$850);
        moonbit_incref(self$847);
        $$moonbitlang$core$builtin$Map$$set_entry$3(
          self$847, entry$845, idx$844
        );
        psl$2563 = _curr_entry$850->$2;
        _tmp$2559 = psl$2563 + 1;
        _tmp$2561 = idx$844 + 1;
        capacity_mask$2562 = self$847->$3;
        _tmp$2560 = _tmp$2561 & capacity_mask$2562;
        psl$843 = _tmp$2559;
        idx$844 = _tmp$2560;
        entry$845 = _curr_entry$850;
        continue;
      } else {
        int32_t _tmp$2564 = psl$843 + 1;
        int32_t _tmp$2566 = idx$844 + 1;
        int32_t capacity_mask$2567 = self$847->$3;
        int32_t _tmp$2565 = _tmp$2566 & capacity_mask$2567;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4988 =
          entry$845;
        psl$843 = _tmp$2564;
        idx$844 = _tmp$2565;
        entry$845 = _tmp$4988;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$837,
  int32_t idx$842,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$841
) {
  int32_t psl$2557 = entry$841->$2;
  int32_t _tmp$2553 = psl$2557 + 1;
  int32_t _tmp$2555 = idx$842 + 1;
  int32_t capacity_mask$2556 = self$837->$3;
  int32_t _tmp$2554 = _tmp$2555 & capacity_mask$2556;
  int32_t psl$833 = _tmp$2553;
  int32_t idx$834 = _tmp$2554;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$835 =
    entry$841;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4298 =
      self$837->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2552 =
      _field$4298;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4297;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$836;
    if (idx$834 < 0 || idx$834 >= Moonbit_array_length(entries$2552)) {
      moonbit_panic();
    }
    _tmp$4297
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2552[
        idx$834
      ];
    _bind$836 = _tmp$4297;
    if (_bind$836 == 0) {
      entry$835->$2 = psl$833;
      $$moonbitlang$core$builtin$Map$$set_entry$2(
        self$837, entry$835, idx$834
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$839 =
        _bind$836;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$840 =
        _Some$839;
      int32_t psl$2542 = _curr_entry$840->$2;
      if (psl$833 > psl$2542) {
        int32_t psl$2547;
        int32_t _tmp$2543;
        int32_t _tmp$2545;
        int32_t capacity_mask$2546;
        int32_t _tmp$2544;
        entry$835->$2 = psl$833;
        moonbit_incref(_curr_entry$840);
        moonbit_incref(self$837);
        $$moonbitlang$core$builtin$Map$$set_entry$2(
          self$837, entry$835, idx$834
        );
        psl$2547 = _curr_entry$840->$2;
        _tmp$2543 = psl$2547 + 1;
        _tmp$2545 = idx$834 + 1;
        capacity_mask$2546 = self$837->$3;
        _tmp$2544 = _tmp$2545 & capacity_mask$2546;
        psl$833 = _tmp$2543;
        idx$834 = _tmp$2544;
        entry$835 = _curr_entry$840;
        continue;
      } else {
        int32_t _tmp$2548 = psl$833 + 1;
        int32_t _tmp$2550 = idx$834 + 1;
        int32_t capacity_mask$2551 = self$837->$3;
        int32_t _tmp$2549 = _tmp$2550 & capacity_mask$2551;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4990 =
          entry$835;
        psl$833 = _tmp$2548;
        idx$834 = _tmp$2549;
        entry$835 = _tmp$4990;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$827,
  int32_t idx$832,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$831
) {
  int32_t psl$2541 = entry$831->$2;
  int32_t _tmp$2537 = psl$2541 + 1;
  int32_t _tmp$2539 = idx$832 + 1;
  int32_t capacity_mask$2540 = self$827->$3;
  int32_t _tmp$2538 = _tmp$2539 & capacity_mask$2540;
  int32_t psl$823 = _tmp$2537;
  int32_t idx$824 = _tmp$2538;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$825 =
    entry$831;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$4300 =
      self$827->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2536 =
      _field$4300;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4299;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$826;
    if (idx$824 < 0 || idx$824 >= Moonbit_array_length(entries$2536)) {
      moonbit_panic();
    }
    _tmp$4299
    = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2536[
        idx$824
      ];
    _bind$826 = _tmp$4299;
    if (_bind$826 == 0) {
      entry$825->$2 = psl$823;
      $$moonbitlang$core$builtin$Map$$set_entry$1(
        self$827, entry$825, idx$824
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$829 =
        _bind$826;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _curr_entry$830 =
        _Some$829;
      int32_t psl$2526 = _curr_entry$830->$2;
      if (psl$823 > psl$2526) {
        int32_t psl$2531;
        int32_t _tmp$2527;
        int32_t _tmp$2529;
        int32_t capacity_mask$2530;
        int32_t _tmp$2528;
        entry$825->$2 = psl$823;
        moonbit_incref(_curr_entry$830);
        moonbit_incref(self$827);
        $$moonbitlang$core$builtin$Map$$set_entry$1(
          self$827, entry$825, idx$824
        );
        psl$2531 = _curr_entry$830->$2;
        _tmp$2527 = psl$2531 + 1;
        _tmp$2529 = idx$824 + 1;
        capacity_mask$2530 = self$827->$3;
        _tmp$2528 = _tmp$2529 & capacity_mask$2530;
        psl$823 = _tmp$2527;
        idx$824 = _tmp$2528;
        entry$825 = _curr_entry$830;
        continue;
      } else {
        int32_t _tmp$2532 = psl$823 + 1;
        int32_t _tmp$2534 = idx$824 + 1;
        int32_t capacity_mask$2535 = self$827->$3;
        int32_t _tmp$2533 = _tmp$2534 & capacity_mask$2535;
        struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4992 =
          entry$825;
        psl$823 = _tmp$2532;
        idx$824 = _tmp$2533;
        entry$825 = _tmp$4992;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$push_away$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$817,
  int32_t idx$822,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$821
) {
  int32_t psl$2525 = entry$821->$2;
  int32_t _tmp$2521 = psl$2525 + 1;
  int32_t _tmp$2523 = idx$822 + 1;
  int32_t capacity_mask$2524 = self$817->$3;
  int32_t _tmp$2522 = _tmp$2523 & capacity_mask$2524;
  int32_t psl$813 = _tmp$2521;
  int32_t idx$814 = _tmp$2522;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$815 =
    entry$821;
  while (1) {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4302 =
      self$817->$0;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2520 =
      _field$4302;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4301;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$816;
    if (idx$814 < 0 || idx$814 >= Moonbit_array_length(entries$2520)) {
      moonbit_panic();
    }
    _tmp$4301
    = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2520[
        idx$814
      ];
    _bind$816 = _tmp$4301;
    if (_bind$816 == 0) {
      entry$815->$2 = psl$813;
      $$moonbitlang$core$builtin$Map$$set_entry$0(
        self$817, entry$815, idx$814
      );
      break;
    } else {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$819 =
        _bind$816;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _curr_entry$820 =
        _Some$819;
      int32_t psl$2510 = _curr_entry$820->$2;
      if (psl$813 > psl$2510) {
        int32_t psl$2515;
        int32_t _tmp$2511;
        int32_t _tmp$2513;
        int32_t capacity_mask$2514;
        int32_t _tmp$2512;
        entry$815->$2 = psl$813;
        moonbit_incref(_curr_entry$820);
        moonbit_incref(self$817);
        $$moonbitlang$core$builtin$Map$$set_entry$0(
          self$817, entry$815, idx$814
        );
        psl$2515 = _curr_entry$820->$2;
        _tmp$2511 = psl$2515 + 1;
        _tmp$2513 = idx$814 + 1;
        capacity_mask$2514 = self$817->$3;
        _tmp$2512 = _tmp$2513 & capacity_mask$2514;
        psl$813 = _tmp$2511;
        idx$814 = _tmp$2512;
        entry$815 = _curr_entry$820;
        continue;
      } else {
        int32_t _tmp$2516 = psl$813 + 1;
        int32_t _tmp$2518 = idx$814 + 1;
        int32_t capacity_mask$2519 = self$817->$3;
        int32_t _tmp$2517 = _tmp$2518 & capacity_mask$2519;
        struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4994 =
          entry$815;
        psl$813 = _tmp$2516;
        idx$814 = _tmp$2517;
        entry$815 = _tmp$4994;
        continue;
      }
    }
    break;
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$807,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$809,
  int32_t new_idx$808
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4305 =
    self$807->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2508 =
    _field$4305;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2509;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4304;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4303;
  int32_t _cnt$4517;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$810;
  moonbit_incref(entry$809);
  _tmp$2509 = entry$809;
  if (new_idx$808 < 0 || new_idx$808 >= Moonbit_array_length(entries$2508)) {
    moonbit_panic();
  }
  _old$4304
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2508[
      new_idx$808
    ];
  if (_old$4304) {
    moonbit_decref(_old$4304);
  }
  entries$2508[new_idx$808] = _tmp$2509;
  _field$4303 = entry$809->$1;
  _cnt$4517 = Moonbit_object_header(entry$809)->rc;
  if (_cnt$4517 > 1) {
    int32_t _new_cnt$4520;
    if (_field$4303) {
      moonbit_incref(_field$4303);
    }
    _new_cnt$4520 = _cnt$4517 - 1;
    Moonbit_object_header(entry$809)->rc = _new_cnt$4520;
  } else if (_cnt$4517 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4519 =
      entry$809->$5;
    moonbit_string_t _field$4518;
    moonbit_decref(_field$4519);
    _field$4518 = entry$809->$4;
    moonbit_decref(_field$4518);
    moonbit_free(entry$809);
  }
  _bind$810 = _field$4303;
  if (_bind$810 == 0) {
    if (_bind$810) {
      moonbit_decref(_bind$810);
    }
    self$807->$6 = new_idx$808;
    moonbit_decref(self$807);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$811;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$812;
    moonbit_decref(self$807);
    _Some$811 = _bind$810;
    _next$812 = _Some$811;
    _next$812->$0 = new_idx$808;
    moonbit_decref(_next$812);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$801,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$803,
  int32_t new_idx$802
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4308 =
    self$801->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2506 =
    _field$4308;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2507;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4307;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4306;
  int32_t _cnt$4521;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$804;
  moonbit_incref(entry$803);
  _tmp$2507 = entry$803;
  if (new_idx$802 < 0 || new_idx$802 >= Moonbit_array_length(entries$2506)) {
    moonbit_panic();
  }
  _old$4307
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2506[
      new_idx$802
    ];
  if (_old$4307) {
    moonbit_decref(_old$4307);
  }
  entries$2506[new_idx$802] = _tmp$2507;
  _field$4306 = entry$803->$1;
  _cnt$4521 = Moonbit_object_header(entry$803)->rc;
  if (_cnt$4521 > 1) {
    int32_t _new_cnt$4524;
    if (_field$4306) {
      moonbit_incref(_field$4306);
    }
    _new_cnt$4524 = _cnt$4521 - 1;
    Moonbit_object_header(entry$803)->rc = _new_cnt$4524;
  } else if (_cnt$4521 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4523 =
      entry$803->$5;
    moonbit_string_t _field$4522;
    moonbit_decref(_field$4523);
    _field$4522 = entry$803->$4;
    moonbit_decref(_field$4522);
    moonbit_free(entry$803);
  }
  _bind$804 = _field$4306;
  if (_bind$804 == 0) {
    if (_bind$804) {
      moonbit_decref(_bind$804);
    }
    self$801->$6 = new_idx$802;
    moonbit_decref(self$801);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$805;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$806;
    moonbit_decref(self$801);
    _Some$805 = _bind$804;
    _next$806 = _Some$805;
    _next$806->$0 = new_idx$802;
    moonbit_decref(_next$806);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$795,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$797,
  int32_t new_idx$796
) {
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$4311 =
    self$795->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2504 =
    _field$4311;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2505;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$4310;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4309;
  int32_t _cnt$4525;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$798;
  moonbit_incref(entry$797);
  _tmp$2505 = entry$797;
  if (new_idx$796 < 0 || new_idx$796 >= Moonbit_array_length(entries$2504)) {
    moonbit_panic();
  }
  _old$4310
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2504[
      new_idx$796
    ];
  if (_old$4310) {
    moonbit_decref(_old$4310);
  }
  entries$2504[new_idx$796] = _tmp$2505;
  _field$4309 = entry$797->$1;
  _cnt$4525 = Moonbit_object_header(entry$797)->rc;
  if (_cnt$4525 > 1) {
    int32_t _new_cnt$4527;
    if (_field$4309) {
      moonbit_incref(_field$4309);
    }
    _new_cnt$4527 = _cnt$4525 - 1;
    Moonbit_object_header(entry$797)->rc = _new_cnt$4527;
  } else if (_cnt$4525 == 1) {
    struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _field$4526 =
      entry$797->$5;
    moonbit_decref(_field$4526);
    moonbit_free(entry$797);
  }
  _bind$798 = _field$4309;
  if (_bind$798 == 0) {
    if (_bind$798) {
      moonbit_decref(_bind$798);
    }
    self$795->$6 = new_idx$796;
    moonbit_decref(self$795);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$799;
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _next$800;
    moonbit_decref(self$795);
    _Some$799 = _bind$798;
    _next$800 = _Some$799;
    _next$800->$0 = new_idx$796;
    moonbit_decref(_next$800);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$set_entry$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$789,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$791,
  int32_t new_idx$790
) {
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4314 =
    self$789->$0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2502 =
    _field$4314;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2503;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4313;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _field$4312;
  int32_t _cnt$4528;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$792;
  moonbit_incref(entry$791);
  _tmp$2503 = entry$791;
  if (new_idx$790 < 0 || new_idx$790 >= Moonbit_array_length(entries$2502)) {
    moonbit_panic();
  }
  _old$4313
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2502[
      new_idx$790
    ];
  if (_old$4313) {
    moonbit_decref(_old$4313);
  }
  entries$2502[new_idx$790] = _tmp$2503;
  _field$4312 = entry$791->$1;
  _cnt$4528 = Moonbit_object_header(entry$791)->rc;
  if (_cnt$4528 > 1) {
    int32_t _new_cnt$4531;
    if (_field$4312) {
      moonbit_incref(_field$4312);
    }
    _new_cnt$4531 = _cnt$4528 - 1;
    Moonbit_object_header(entry$791)->rc = _new_cnt$4531;
  } else if (_cnt$4528 == 1) {
    struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _field$4530 =
      entry$791->$5;
    moonbit_string_t _field$4529;
    moonbit_decref(_field$4530);
    _field$4529 = entry$791->$4;
    moonbit_decref(_field$4529);
    moonbit_free(entry$791);
  }
  _bind$792 = _field$4312;
  if (_bind$792 == 0) {
    if (_bind$792) {
      moonbit_decref(_bind$792);
    }
    self$789->$6 = new_idx$790;
    moonbit_decref(self$789);
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$793;
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _next$794;
    moonbit_decref(self$789);
    _Some$793 = _bind$792;
    _next$794 = _Some$793;
    _next$794->$0 = new_idx$790;
    moonbit_decref(_next$794);
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$3(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$786,
  int32_t idx$788,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$787
) {
  int32_t _bind$785 = self$786->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4316;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2498;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2499;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4315;
  int32_t size$2501;
  int32_t _tmp$2500;
  switch (_bind$785) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2493;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4317;
      moonbit_incref(entry$787);
      _tmp$2493 = entry$787;
      _old$4317 = self$786->$5;
      if (_old$4317) {
        moonbit_decref(_old$4317);
      }
      self$786->$5 = _tmp$2493;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4320 =
        self$786->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2497 =
        _field$4320;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4319;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2496;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2494;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2495;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4318;
      if (_bind$785 < 0 || _bind$785 >= Moonbit_array_length(entries$2497)) {
        moonbit_panic();
      }
      _tmp$4319
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2497[
          _bind$785
        ];
      _tmp$2496 = _tmp$4319;
      if (_tmp$2496) {
        moonbit_incref(_tmp$2496);
      }
      _tmp$2494 = $Option$$unwrap$3(_tmp$2496);
      moonbit_incref(entry$787);
      _tmp$2495 = entry$787;
      _old$4318 = _tmp$2494->$1;
      if (_old$4318) {
        moonbit_decref(_old$4318);
      }
      _tmp$2494->$1 = _tmp$2495;
      moonbit_decref(_tmp$2494);
      break;
    }
  }
  self$786->$6 = idx$788;
  _field$4316 = self$786->$0;
  entries$2498 = _field$4316;
  _tmp$2499 = entry$787;
  if (idx$788 < 0 || idx$788 >= Moonbit_array_length(entries$2498)) {
    moonbit_panic();
  }
  _old$4315
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2498[
      idx$788
    ];
  if (_old$4315) {
    moonbit_decref(_old$4315);
  }
  entries$2498[idx$788] = _tmp$2499;
  size$2501 = self$786->$1;
  _tmp$2500 = size$2501 + 1;
  self$786->$1 = _tmp$2500;
  moonbit_decref(self$786);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$2(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$782,
  int32_t idx$784,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$783
) {
  int32_t _bind$781 = self$782->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4322;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2489;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2490;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4321;
  int32_t size$2492;
  int32_t _tmp$2491;
  switch (_bind$781) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2484;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4323;
      moonbit_incref(entry$783);
      _tmp$2484 = entry$783;
      _old$4323 = self$782->$5;
      if (_old$4323) {
        moonbit_decref(_old$4323);
      }
      self$782->$5 = _tmp$2484;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4326 =
        self$782->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2488 =
        _field$4326;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4325;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2487;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2485;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2486;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4324;
      if (_bind$781 < 0 || _bind$781 >= Moonbit_array_length(entries$2488)) {
        moonbit_panic();
      }
      _tmp$4325
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2488[
          _bind$781
        ];
      _tmp$2487 = _tmp$4325;
      if (_tmp$2487) {
        moonbit_incref(_tmp$2487);
      }
      _tmp$2485 = $Option$$unwrap$2(_tmp$2487);
      moonbit_incref(entry$783);
      _tmp$2486 = entry$783;
      _old$4324 = _tmp$2485->$1;
      if (_old$4324) {
        moonbit_decref(_old$4324);
      }
      _tmp$2485->$1 = _tmp$2486;
      moonbit_decref(_tmp$2485);
      break;
    }
  }
  self$782->$6 = idx$784;
  _field$4322 = self$782->$0;
  entries$2489 = _field$4322;
  _tmp$2490 = entry$783;
  if (idx$784 < 0 || idx$784 >= Moonbit_array_length(entries$2489)) {
    moonbit_panic();
  }
  _old$4321
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2489[
      idx$784
    ];
  if (_old$4321) {
    moonbit_decref(_old$4321);
  }
  entries$2489[idx$784] = _tmp$2490;
  size$2492 = self$782->$1;
  _tmp$2491 = size$2492 + 1;
  self$782->$1 = _tmp$2491;
  moonbit_decref(self$782);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$1(
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$778,
  int32_t idx$780,
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* entry$779
) {
  int32_t _bind$777 = self$778->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$4328;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2480;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2481;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$4327;
  int32_t size$2483;
  int32_t _tmp$2482;
  switch (_bind$777) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2475;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$4329;
      moonbit_incref(entry$779);
      _tmp$2475 = entry$779;
      _old$4329 = self$778->$5;
      if (_old$4329) {
        moonbit_decref(_old$4329);
      }
      self$778->$5 = _tmp$2475;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _field$4332 =
        self$778->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** entries$2479 =
        _field$4332;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$4331;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2478;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2476;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2477;
      struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _old$4330;
      if (_bind$777 < 0 || _bind$777 >= Moonbit_array_length(entries$2479)) {
        moonbit_panic();
      }
      _tmp$4331
      = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2479[
          _bind$777
        ];
      _tmp$2478 = _tmp$4331;
      if (_tmp$2478) {
        moonbit_incref(_tmp$2478);
      }
      _tmp$2476 = $Option$$unwrap$1(_tmp$2478);
      moonbit_incref(entry$779);
      _tmp$2477 = entry$779;
      _old$4330 = _tmp$2476->$1;
      if (_old$4330) {
        moonbit_decref(_old$4330);
      }
      _tmp$2476->$1 = _tmp$2477;
      moonbit_decref(_tmp$2476);
      break;
    }
  }
  self$778->$6 = idx$780;
  _field$4328 = self$778->$0;
  entries$2480 = _field$4328;
  _tmp$2481 = entry$779;
  if (idx$780 < 0 || idx$780 >= Moonbit_array_length(entries$2480)) {
    moonbit_panic();
  }
  _old$4327
  = (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)entries$2480[
      idx$780
    ];
  if (_old$4327) {
    moonbit_decref(_old$4327);
  }
  entries$2480[idx$780] = _tmp$2481;
  size$2483 = self$778->$1;
  _tmp$2482 = size$2483 + 1;
  self$778->$1 = _tmp$2482;
  moonbit_decref(self$778);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Map$$add_entry_to_tail$0(
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$774,
  int32_t idx$776,
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* entry$775
) {
  int32_t _bind$773 = self$774->$6;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4334;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2471;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2472;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4333;
  int32_t size$2474;
  int32_t _tmp$2473;
  switch (_bind$773) {
    case -1: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2466;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4335;
      moonbit_incref(entry$775);
      _tmp$2466 = entry$775;
      _old$4335 = self$774->$5;
      if (_old$4335) {
        moonbit_decref(_old$4335);
      }
      self$774->$5 = _tmp$2466;
      break;
    }
    default: {
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _field$4338 =
        self$774->$0;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** entries$2470 =
        _field$4338;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$4337;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2469;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2467;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2468;
      struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _old$4336;
      if (_bind$773 < 0 || _bind$773 >= Moonbit_array_length(entries$2470)) {
        moonbit_panic();
      }
      _tmp$4337
      = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2470[
          _bind$773
        ];
      _tmp$2469 = _tmp$4337;
      if (_tmp$2469) {
        moonbit_incref(_tmp$2469);
      }
      _tmp$2467 = $Option$$unwrap$0(_tmp$2469);
      moonbit_incref(entry$775);
      _tmp$2468 = entry$775;
      _old$4336 = _tmp$2467->$1;
      if (_old$4336) {
        moonbit_decref(_old$4336);
      }
      _tmp$2467->$1 = _tmp$2468;
      moonbit_decref(_tmp$2467);
      break;
    }
  }
  self$774->$6 = idx$776;
  _field$4334 = self$774->$0;
  entries$2471 = _field$4334;
  _tmp$2472 = entry$775;
  if (idx$776 < 0 || idx$776 >= Moonbit_array_length(entries$2471)) {
    moonbit_panic();
  }
  _old$4333
  = (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)entries$2471[
      idx$776
    ];
  if (_old$4333) {
    moonbit_decref(_old$4333);
  }
  entries$2471[idx$776] = _tmp$2472;
  size$2474 = self$774->$1;
  _tmp$2473 = size$2474 + 1;
  self$774->$1 = _tmp$2473;
  moonbit_decref(self$774);
  return 0;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$3(
  int32_t capacity$768
) {
  int32_t capacity$767 = $Int$$next_power_of_two(capacity$768);
  int32_t _bind$769 = capacity$767 - 1;
  int32_t _bind$770 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$767);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2465 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$771 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$767, _tmp$2465
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$772 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$4995 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4995)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4995->$0 = _bind$771;
  _block$4995->$1 = 0;
  _block$4995->$2 = capacity$767;
  _block$4995->$3 = _bind$769;
  _block$4995->$4 = _bind$770;
  _block$4995->$5 = _bind$772;
  _block$4995->$6 = -1;
  return _block$4995;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$2(
  int32_t capacity$762
) {
  int32_t capacity$761 = $Int$$next_power_of_two(capacity$762);
  int32_t _bind$763 = capacity$761 - 1;
  int32_t _bind$764 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$761);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2464 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$765 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$761, _tmp$2464
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$766 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$4996 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4996)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4996->$0 = _bind$765;
  _block$4996->$1 = 0;
  _block$4996->$2 = capacity$761;
  _block$4996->$3 = _bind$763;
  _block$4996->$4 = _bind$764;
  _block$4996->$5 = _bind$766;
  _block$4996->$6 = -1;
  return _block$4996;
}

struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$1(
  int32_t capacity$756
) {
  int32_t capacity$755 = $Int$$next_power_of_two(capacity$756);
  int32_t _bind$757 = capacity$755 - 1;
  int32_t _bind$758 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$755);
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$2463 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$759 =
    (struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$755, _tmp$2463
    );
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _bind$760 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _block$4997 =
    (struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4997)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4997->$0 = _bind$759;
  _block$4997->$1 = 0;
  _block$4997->$2 = capacity$755;
  _block$4997->$3 = _bind$757;
  _block$4997->$4 = _bind$758;
  _block$4997->$5 = _bind$760;
  _block$4997->$6 = -1;
  return _block$4997;
}

struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $$moonbitlang$core$builtin$Map$$new$inner$0(
  int32_t capacity$750
) {
  int32_t capacity$749 = $Int$$next_power_of_two(capacity$750);
  int32_t _bind$751 = capacity$749 - 1;
  int32_t _bind$752 =
    $moonbitlang$core$builtin$calc_grow_threshold(capacity$749);
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tmp$2462 =
    0;
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$753 =
    (struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array(
      capacity$749, _tmp$2462
    );
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _bind$754 =
    0;
  struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _block$4998 =
    (struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_block$4998)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Map$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _block$4998->$0 = _bind$753;
  _block$4998->$1 = 0;
  _block$4998->$2 = capacity$749;
  _block$4998->$3 = _bind$751;
  _block$4998->$4 = _bind$752;
  _block$4998->$5 = _bind$754;
  _block$4998->$6 = -1;
  return _block$4998;
}

int32_t $Int$$next_power_of_two(int32_t self$748) {
  if (self$748 >= 0) {
    int32_t _tmp$2461;
    int32_t _tmp$2460;
    int32_t _tmp$2459;
    int32_t _tmp$2458;
    if (self$748 <= 1) {
      return 1;
    }
    if (self$748 > 1073741824) {
      return 1073741824;
    }
    _tmp$2461 = self$748 - 1;
    _tmp$2460 = moonbit_clz32(_tmp$2461);
    _tmp$2459 = _tmp$2460 - 1;
    _tmp$2458 = 2147483647 >> (_tmp$2459 & 31);
    return _tmp$2458 + 1;
  } else {
    moonbit_panic();
  }
}

int32_t $moonbitlang$core$builtin$calc_grow_threshold(int32_t capacity$747) {
  int32_t _tmp$2457 = capacity$747 * 13;
  return _tmp$2457 / 16;
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$3(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$745
) {
  if (self$745 == 0) {
    if (self$745) {
      moonbit_decref(self$745);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$746 =
      self$745;
    return _Some$746;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$2(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$743
) {
  if (self$743 == 0) {
    if (self$743) {
      moonbit_decref(self$743);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$744 =
      self$743;
    return _Some$744;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* $Option$$unwrap$1(
  struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* self$741
) {
  if (self$741 == 0) {
    if (self$741) {
      moonbit_decref(self$741);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _Some$742 =
      self$741;
    return _Some$742;
  }
}

struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* $Option$$unwrap$0(
  struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* self$739
) {
  if (self$739 == 0) {
    if (self$739) {
      moonbit_decref(self$739);
    }
    moonbit_panic();
  } else {
    struct $$moonbitlang$core$builtin$Entry$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _Some$740 =
      self$739;
    return _Some$740;
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$2(
  int32_t self$733,
  int32_t other$734
) {
  if (self$733 == -1) {
    return other$734 == -1;
  } else {
    int32_t _Some$735 = self$733;
    int32_t _x$736 = _Some$735;
    if (other$734 == -1) {
      return 0;
    } else {
      int32_t _Some$737 = other$734;
      int32_t _y$738 = _Some$737;
      return _x$736 == _y$738;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$1(
  moonbit_string_t self$727,
  moonbit_string_t other$728
) {
  if (self$727 == 0) {
    int32_t _tmp$4339;
    if (self$727) {
      moonbit_decref(self$727);
    }
    _tmp$4339 = other$728 == 0;
    if (other$728) {
      moonbit_decref(other$728);
    }
    return _tmp$4339;
  } else {
    moonbit_string_t _Some$729 = self$727;
    moonbit_string_t _x$730 = _Some$729;
    if (other$728 == 0) {
      moonbit_decref(_x$730);
      if (other$728) {
        moonbit_decref(other$728);
      }
      return 0;
    } else {
      moonbit_string_t _Some$731 = other$728;
      moonbit_string_t _y$732 = _Some$731;
      int32_t _tmp$4340 = moonbit_val_array_equal(_x$730, _y$732);
      moonbit_decref(_x$730);
      moonbit_decref(_y$732);
      return _tmp$4340;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Eq$$Option$$equal$0(
  int64_t self$721,
  int64_t other$722
) {
  if (self$721 == 4294967296ll) {
    return other$722 == 4294967296ll;
  } else {
    int64_t _Some$723 = self$721;
    int32_t _x$724 = (int32_t)_Some$723;
    if (other$722 == 4294967296ll) {
      return 0;
    } else {
      int64_t _Some$725 = other$722;
      int32_t _y$726 = (int32_t)_Some$725;
      return _x$724 == _y$726;
    }
  }
}

uint32_t $ReadOnlyArray$$at$1(uint32_t* self$719, int32_t index$720) {
  uint32_t* _tmp$2456 = self$719;
  uint32_t _tmp$4341;
  if (index$720 < 0 || index$720 >= Moonbit_array_length(_tmp$2456)) {
    moonbit_panic();
  }
  _tmp$4341 = (uint32_t)_tmp$2456[index$720];
  moonbit_decref(_tmp$2456);
  return _tmp$4341;
}

uint64_t $ReadOnlyArray$$at$0(uint64_t* self$717, int32_t index$718) {
  uint64_t* _tmp$2455 = self$717;
  uint64_t _tmp$4342;
  if (index$718 < 0 || index$718 >= Moonbit_array_length(_tmp$2455)) {
    moonbit_panic();
  }
  _tmp$4342 = (uint64_t)_tmp$2455[index$718];
  moonbit_decref(_tmp$2455);
  return _tmp$4342;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Array$$iter$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$716
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$2454 =
    $$moonbitlang$core$builtin$Array$$iterator$0(self$716);
  return $$moonbitlang$core$builtin$Iterator$$iter$0(_tmp$2454);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Array$$iterator$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$715
) {
  moonbit_string_t* _field$4344 = self$715->$0;
  moonbit_string_t* buf$2452 = _field$4344;
  int32_t _field$4343 = self$715->$1;
  int32_t _cnt$4532 = Moonbit_object_header(self$715)->rc;
  int32_t len$2453;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _tmp$2451;
  if (_cnt$4532 > 1) {
    int32_t _new_cnt$4533;
    moonbit_incref(buf$2452);
    _new_cnt$4533 = _cnt$4532 - 1;
    Moonbit_object_header(self$715)->rc = _new_cnt$4533;
  } else if (_cnt$4532 == 1) {
    moonbit_free(self$715);
  }
  len$2453 = _field$4343;
  _tmp$2451
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
    0, len$2453, buf$2452
  };
  return $$moonbitlang$core$builtin$ArrayView$$iterator$0(_tmp$2451);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$ArrayView$$iterator$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$713
) {
  struct $Ref$3c$Int$3e$* i$712 =
    (struct $Ref$3c$Int$3e$*)moonbit_malloc(sizeof(struct $Ref$3c$Int$3e$));
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _closure$4999;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _tmp$2440;
  Moonbit_object_header(i$712)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $Ref$3c$Int$3e$) >> 2, 0, 0
  );
  i$712->$0 = 0;
  _closure$4999
  = (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)moonbit_malloc(
      sizeof(struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap)
    );
  Moonbit_object_header(_closure$4999)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap, $0_0
    )
    >> 2,
      2,
      0
  );
  _closure$4999->code = &$ArrayView$$iterator$7c$String$7c$$fn$2;
  _closure$4999->$0_0 = self$713.$0;
  _closure$4999->$0_1 = self$713.$1;
  _closure$4999->$0_2 = self$713.$2;
  _closure$4999->$1 = i$712;
  _tmp$2440 = (struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$*)_closure$4999;
  return $$moonbitlang$core$builtin$Iterator$$new$0(_tmp$2440);
}

moonbit_string_t $ArrayView$$iterator$7c$String$7c$$fn$2(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _env$2441
) {
  struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap* _casted_env$2442 =
    (struct $ArrayView$$iterator$7c$String$7c$$fn$2$2d$cap*)_env$2441;
  struct $Ref$3c$Int$3e$* _field$4349 = _casted_env$2442->$1;
  struct $Ref$3c$Int$3e$* i$712 = _field$4349;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ _field$4348 =
    (struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$){
      _casted_env$2442->$0_1, _casted_env$2442->$0_2, _casted_env$2442->$0_0
    };
  int32_t _cnt$4534 = Moonbit_object_header(_casted_env$2442)->rc;
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$713;
  int32_t val$2443;
  int32_t _tmp$2444;
  if (_cnt$4534 > 1) {
    int32_t _new_cnt$4535;
    moonbit_incref(i$712);
    moonbit_incref(_field$4348.$0);
    _new_cnt$4535 = _cnt$4534 - 1;
    Moonbit_object_header(_casted_env$2442)->rc = _new_cnt$4535;
  } else if (_cnt$4534 == 1) {
    moonbit_free(_casted_env$2442);
  }
  self$713 = _field$4348;
  val$2443 = i$712->$0;
  moonbit_incref(self$713.$0);
  _tmp$2444 = $$moonbitlang$core$builtin$ArrayView$$length$4(self$713);
  if (val$2443 < _tmp$2444) {
    moonbit_string_t* _field$4347 = self$713.$0;
    moonbit_string_t* buf$2447 = _field$4347;
    int32_t _field$4346 = self$713.$1;
    int32_t start$2449 = _field$4346;
    int32_t val$2450 = i$712->$0;
    int32_t _tmp$2448 = start$2449 + val$2450;
    moonbit_string_t _tmp$4345 = (moonbit_string_t)buf$2447[_tmp$2448];
    moonbit_string_t elem$714;
    int32_t val$2446;
    int32_t _tmp$2445;
    moonbit_incref(_tmp$4345);
    moonbit_decref(buf$2447);
    elem$714 = _tmp$4345;
    val$2446 = i$712->$0;
    _tmp$2445 = val$2446 + 1;
    i$712->$0 = _tmp$2445;
    moonbit_decref(i$712);
    return elem$714;
  } else {
    moonbit_decref(self$713.$0);
    moonbit_decref(i$712);
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$2(
  int32_t self$708,
  struct $$moonbitlang$core$builtin$Logger logger$709
) {
  if (self$708 == -1) {
    logger$709.$0->$method_0(
      logger$709.$1, (moonbit_string_t)moonbit_string_literal_246.data
    );
  } else {
    int32_t _Some$710 = self$708;
    int32_t _arg$711 = _Some$710;
    struct $$moonbitlang$core$builtin$Logger _bind$2439;
    if (logger$709.$1) {
      moonbit_incref(logger$709.$1);
    }
    logger$709.$0->$method_0(
      logger$709.$1, (moonbit_string_t)moonbit_string_literal_247.data
    );
    if (logger$709.$1) {
      moonbit_incref(logger$709.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$2(logger$709, _arg$711);
    _bind$2439 = logger$709;
    _bind$2439.$0->$method_0(
      _bind$2439.$1, (moonbit_string_t)moonbit_string_literal_248.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$1(
  moonbit_string_t self$704,
  struct $$moonbitlang$core$builtin$Logger logger$705
) {
  if (self$704 == 0) {
    if (self$704) {
      moonbit_decref(self$704);
    }
    logger$705.$0->$method_0(
      logger$705.$1, (moonbit_string_t)moonbit_string_literal_246.data
    );
  } else {
    moonbit_string_t _Some$706 = self$704;
    moonbit_string_t _arg$707 = _Some$706;
    struct $$moonbitlang$core$builtin$Logger _bind$2438;
    if (logger$705.$1) {
      moonbit_incref(logger$705.$1);
    }
    logger$705.$0->$method_0(
      logger$705.$1, (moonbit_string_t)moonbit_string_literal_247.data
    );
    if (logger$705.$1) {
      moonbit_incref(logger$705.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$1(logger$705, _arg$707);
    _bind$2438 = logger$705;
    _bind$2438.$0->$method_0(
      _bind$2438.$1, (moonbit_string_t)moonbit_string_literal_248.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Option$$output$0(
  int64_t self$700,
  struct $$moonbitlang$core$builtin$Logger logger$701
) {
  if (self$700 == 4294967296ll) {
    logger$701.$0->$method_0(
      logger$701.$1, (moonbit_string_t)moonbit_string_literal_246.data
    );
  } else {
    int64_t _Some$702 = self$700;
    int32_t _arg$703 = (int32_t)_Some$702;
    struct $$moonbitlang$core$builtin$Logger _bind$2437;
    if (logger$701.$1) {
      moonbit_incref(logger$701.$1);
    }
    logger$701.$0->$method_0(
      logger$701.$1, (moonbit_string_t)moonbit_string_literal_247.data
    );
    if (logger$701.$1) {
      moonbit_incref(logger$701.$1);
    }
    $$moonbitlang$core$builtin$Logger$$write_object$0(logger$701, _arg$703);
    _bind$2437 = logger$701;
    _bind$2437.$0->$method_0(
      _bind$2437.$1, (moonbit_string_t)moonbit_string_literal_248.data
    );
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$String$$to_string(
  moonbit_string_t self$699
) {
  return self$699;
}

int32_t $$moonbitlang$core$builtin$Show$$UInt16$$output(
  int32_t self$698,
  struct $$moonbitlang$core$builtin$Logger logger$697
) {
  moonbit_string_t _tmp$2436 = $UInt16$$to_string$inner(self$698, 10);
  logger$697.$0->$method_0(logger$697.$1, _tmp$2436);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$UInt64$$output(
  uint64_t self$696,
  struct $$moonbitlang$core$builtin$Logger logger$695
) {
  moonbit_string_t _tmp$2435 = $UInt64$$to_string$inner(self$696, 10);
  logger$695.$0->$method_0(logger$695.$1, _tmp$2435);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Int$$output(
  int32_t self$694,
  struct $$moonbitlang$core$builtin$Logger logger$693
) {
  moonbit_string_t _tmp$2434 = $Int$$to_string$inner(self$694, 10);
  logger$693.$0->$method_0(logger$693.$1, _tmp$2434);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$Bool$$output(
  int32_t self$691,
  struct $$moonbitlang$core$builtin$Logger logger$692
) {
  if (self$691) {
    logger$692.$0->$method_0(
      logger$692.$1, (moonbit_string_t)moonbit_string_literal_28.data
    );
  } else {
    logger$692.$0->$method_0(
      logger$692.$1, (moonbit_string_t)moonbit_string_literal_29.data
    );
  }
  return 0;
}

int32_t $$moonbitlang$core$builtin$Iter$$run$0(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* self$689,
  struct $$3c$String$3e$$3d$$3e$Int* f$690
) {
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _func$688 = self$689;
  return _func$688->code(_func$688, f$690);
}

int32_t $String$$contains(
  moonbit_string_t self$686,
  struct $StringView str$687
) {
  int32_t _tmp$2433 = Moonbit_array_length(self$686);
  struct $StringView _tmp$2432 = (struct $StringView){0, _tmp$2433, self$686};
  return $StringView$$contains(_tmp$2432, str$687);
}

int32_t $StringView$$contains(
  struct $StringView self$684,
  struct $StringView str$685
) {
  int64_t _bind$683 = $StringView$$find(self$684, str$685);
  int32_t _tmp$2431 = _bind$683 == 4294967296ll;
  return !_tmp$2431;
}

int32_t $$moonbitlang$core$builtin$Array$$push$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$680,
  int32_t value$682
) {
  int32_t len$2426 = self$680->$1;
  int32_t* _tmp$2428;
  int32_t _tmp$4351;
  int32_t _tmp$2427;
  int32_t length$681;
  int32_t* _field$4350;
  int32_t* buf$2429;
  int32_t _tmp$2430;
  moonbit_incref(self$680);
  _tmp$2428 = $$moonbitlang$core$builtin$Array$$buffer$3(self$680);
  _tmp$4351 = Moonbit_array_length(_tmp$2428);
  moonbit_decref(_tmp$2428);
  _tmp$2427 = _tmp$4351;
  if (len$2426 == _tmp$2427) {
    moonbit_incref(self$680);
    $$moonbitlang$core$builtin$Array$$realloc$3(self$680);
  }
  length$681 = self$680->$1;
  _field$4350 = self$680->$0;
  buf$2429 = _field$4350;
  buf$2429[length$681] = value$682;
  _tmp$2430 = length$681 + 1;
  self$680->$1 = _tmp$2430;
  moonbit_decref(self$680);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$677,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* value$679
) {
  int32_t len$2421 = self$677->$1;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$2423;
  int32_t _tmp$4354;
  int32_t _tmp$2422;
  int32_t length$678;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$4353;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** buf$2424;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$4352;
  int32_t _tmp$2425;
  moonbit_incref(self$677);
  _tmp$2423 = $$moonbitlang$core$builtin$Array$$buffer$2(self$677);
  _tmp$4354 = Moonbit_array_length(_tmp$2423);
  moonbit_decref(_tmp$2423);
  _tmp$2422 = _tmp$4354;
  if (len$2421 == _tmp$2422) {
    moonbit_incref(self$677);
    $$moonbitlang$core$builtin$Array$$realloc$2(self$677);
  }
  length$678 = self$677->$1;
  _field$4353 = self$677->$0;
  buf$2424 = _field$4353;
  _old$4352
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)buf$2424[
      length$678
    ];
  if (_old$4352) {
    moonbit_decref(_old$4352);
  }
  buf$2424[length$678] = value$679;
  _tmp$2425 = length$678 + 1;
  self$677->$1 = _tmp$2425;
  moonbit_decref(self$677);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$674,
  struct $$3c$String$2a$Int$3e$* value$676
) {
  int32_t len$2416 = self$674->$1;
  struct $$3c$String$2a$Int$3e$** _tmp$2418;
  int32_t _tmp$4357;
  int32_t _tmp$2417;
  int32_t length$675;
  struct $$3c$String$2a$Int$3e$** _field$4356;
  struct $$3c$String$2a$Int$3e$** buf$2419;
  struct $$3c$String$2a$Int$3e$* _old$4355;
  int32_t _tmp$2420;
  moonbit_incref(self$674);
  _tmp$2418 = $$moonbitlang$core$builtin$Array$$buffer$0(self$674);
  _tmp$4357 = Moonbit_array_length(_tmp$2418);
  moonbit_decref(_tmp$2418);
  _tmp$2417 = _tmp$4357;
  if (len$2416 == _tmp$2417) {
    moonbit_incref(self$674);
    $$moonbitlang$core$builtin$Array$$realloc$1(self$674);
  }
  length$675 = self$674->$1;
  _field$4356 = self$674->$0;
  buf$2419 = _field$4356;
  _old$4355 = (struct $$3c$String$2a$Int$3e$*)buf$2419[length$675];
  if (_old$4355) {
    moonbit_decref(_old$4355);
  }
  buf$2419[length$675] = value$676;
  _tmp$2420 = length$675 + 1;
  self$674->$1 = _tmp$2420;
  moonbit_decref(self$674);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$push$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$671,
  moonbit_string_t value$673
) {
  int32_t len$2411 = self$671->$1;
  moonbit_string_t* _tmp$2413;
  int32_t _tmp$4360;
  int32_t _tmp$2412;
  int32_t length$672;
  moonbit_string_t* _field$4359;
  moonbit_string_t* buf$2414;
  moonbit_string_t _old$4358;
  int32_t _tmp$2415;
  moonbit_incref(self$671);
  _tmp$2413 = $$moonbitlang$core$builtin$Array$$buffer$1(self$671);
  _tmp$4360 = Moonbit_array_length(_tmp$2413);
  moonbit_decref(_tmp$2413);
  _tmp$2412 = _tmp$4360;
  if (len$2411 == _tmp$2412) {
    moonbit_incref(self$671);
    $$moonbitlang$core$builtin$Array$$realloc$0(self$671);
  }
  length$672 = self$671->$1;
  _field$4359 = self$671->$0;
  buf$2414 = _field$4359;
  _old$4358 = (moonbit_string_t)buf$2414[length$672];
  moonbit_decref(_old$4358);
  buf$2414[length$672] = value$673;
  _tmp$2415 = length$672 + 1;
  self$671->$1 = _tmp$2415;
  moonbit_decref(self$671);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$669
) {
  int32_t old_cap$668 = self$669->$1;
  int32_t new_cap$670;
  if (old_cap$668 == 0) {
    new_cap$670 = 8;
  } else {
    new_cap$670 = old_cap$668 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$3(self$669, new_cap$670);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$666
) {
  int32_t old_cap$665 = self$666->$1;
  int32_t new_cap$667;
  if (old_cap$665 == 0) {
    new_cap$667 = 8;
  } else {
    new_cap$667 = old_cap$665 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$2(self$666, new_cap$667);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$663
) {
  int32_t old_cap$662 = self$663->$1;
  int32_t new_cap$664;
  if (old_cap$662 == 0) {
    new_cap$664 = 8;
  } else {
    new_cap$664 = old_cap$662 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$1(self$663, new_cap$664);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$realloc$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$660
) {
  int32_t old_cap$659 = self$660->$1;
  int32_t new_cap$661;
  if (old_cap$659 == 0) {
    new_cap$661 = 8;
  } else {
    new_cap$661 = old_cap$659 * 2;
  }
  $$moonbitlang$core$builtin$Array$$resize_buffer$0(self$660, new_cap$661);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$656,
  int32_t new_capacity$654
) {
  int32_t* new_buf$653 =
    (int32_t*)moonbit_make_int32_array_raw(new_capacity$654);
  int32_t* _field$4362 = self$656->$0;
  int32_t* old_buf$655 = _field$4362;
  int32_t old_cap$657 = Moonbit_array_length(old_buf$655);
  int32_t copy_len$658;
  int32_t* _old$4361;
  if (old_cap$657 < new_capacity$654) {
    copy_len$658 = old_cap$657;
  } else {
    copy_len$658 = new_capacity$654;
  }
  moonbit_incref(old_buf$655);
  moonbit_incref(new_buf$653);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$3(
    new_buf$653, 0, old_buf$655, 0, copy_len$658
  );
  _old$4361 = self$656->$0;
  moonbit_decref(_old$4361);
  self$656->$0 = new_buf$653;
  moonbit_decref(self$656);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$650,
  int32_t new_capacity$648
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** new_buf$647 =
    (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_make_ref_array(
      new_capacity$648, 0
    );
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$4364 =
    self$650->$0;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** old_buf$649 =
    _field$4364;
  int32_t old_cap$651 = Moonbit_array_length(old_buf$649);
  int32_t copy_len$652;
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _old$4363;
  if (old_cap$651 < new_capacity$648) {
    copy_len$652 = old_cap$651;
  } else {
    copy_len$652 = new_capacity$648;
  }
  moonbit_incref(old_buf$649);
  moonbit_incref(new_buf$647);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
    new_buf$647, 0, old_buf$649, 0, copy_len$652
  );
  _old$4363 = self$650->$0;
  moonbit_decref(_old$4363);
  self$650->$0 = new_buf$647;
  moonbit_decref(self$650);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$644,
  int32_t new_capacity$642
) {
  struct $$3c$String$2a$Int$3e$** new_buf$641 =
    (struct $$3c$String$2a$Int$3e$**)moonbit_make_ref_array(
      new_capacity$642, 0
    );
  struct $$3c$String$2a$Int$3e$** _field$4366 = self$644->$0;
  struct $$3c$String$2a$Int$3e$** old_buf$643 = _field$4366;
  int32_t old_cap$645 = Moonbit_array_length(old_buf$643);
  int32_t copy_len$646;
  struct $$3c$String$2a$Int$3e$** _old$4365;
  if (old_cap$645 < new_capacity$642) {
    copy_len$646 = old_cap$645;
  } else {
    copy_len$646 = new_capacity$642;
  }
  moonbit_incref(old_buf$643);
  moonbit_incref(new_buf$641);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
    new_buf$641, 0, old_buf$643, 0, copy_len$646
  );
  _old$4365 = self$644->$0;
  moonbit_decref(_old$4365);
  self$644->$0 = new_buf$641;
  moonbit_decref(self$644);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Array$$resize_buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$638,
  int32_t new_capacity$636
) {
  moonbit_string_t* new_buf$635 =
    (moonbit_string_t*)moonbit_make_ref_array(
      new_capacity$636, (moonbit_string_t)moonbit_string_literal_3.data
    );
  moonbit_string_t* _field$4368 = self$638->$0;
  moonbit_string_t* old_buf$637 = _field$4368;
  int32_t old_cap$639 = Moonbit_array_length(old_buf$637);
  int32_t copy_len$640;
  moonbit_string_t* _old$4367;
  if (old_cap$639 < new_capacity$636) {
    copy_len$640 = old_cap$639;
  } else {
    copy_len$640 = new_capacity$636;
  }
  moonbit_incref(old_buf$637);
  moonbit_incref(new_buf$635);
  $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
    new_buf$635, 0, old_buf$637, 0, copy_len$640
  );
  _old$4367 = self$638->$0;
  moonbit_decref(_old$4367);
  self$638->$0 = new_buf$635;
  moonbit_decref(self$638);
  return 0;
}

struct $$moonbitlang$core$builtin$Array$3c$String$3e$* $$moonbitlang$core$builtin$Array$$new$inner$0(
  int32_t capacity$634
) {
  if (capacity$634 == 0) {
    moonbit_string_t* _tmp$2409 = (moonbit_string_t*)moonbit_empty_ref_array;
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$5000 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$5000)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$5000->$0 = _tmp$2409;
    _block$5000->$1 = 0;
    return _block$5000;
  } else {
    moonbit_string_t* _tmp$2410 =
      (moonbit_string_t*)moonbit_make_ref_array(
        capacity$634, (moonbit_string_t)moonbit_string_literal_3.data
      );
    struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _block$5001 =
      (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
        sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
      );
    Moonbit_object_header(_block$5001)->meta
    = Moonbit_make_regular_object_header(
      offsetof(
        struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0
      )
      >> 2,
        1,
        0
    );
    _block$5001->$0 = _tmp$2410;
    _block$5001->$1 = 0;
    return _block$5001;
  }
}

int32_t $String$$has_prefix(
  moonbit_string_t self$632,
  struct $StringView str$633
) {
  int32_t _tmp$2408 = Moonbit_array_length(self$632);
  struct $StringView _tmp$2407 = (struct $StringView){0, _tmp$2408, self$632};
  return $StringView$$has_prefix(_tmp$2407, str$633);
}

int32_t $StringView$$has_prefix(
  struct $StringView self$628,
  struct $StringView str$629
) {
  int64_t _bind$627 = $StringView$$find(self$628, str$629);
  if (_bind$627 == 4294967296ll) {
    return 0;
  } else {
    int64_t _Some$630 = _bind$627;
    int32_t _i$631 = (int32_t)_Some$630;
    return _i$631 == 0;
  }
}

int32_t $String$$has_suffix(
  moonbit_string_t self$625,
  struct $StringView str$626
) {
  int32_t _tmp$2406 = Moonbit_array_length(self$625);
  struct $StringView _tmp$2405 = (struct $StringView){0, _tmp$2406, self$625};
  return $StringView$$has_suffix(_tmp$2405, str$626);
}

int32_t $StringView$$has_suffix(
  struct $StringView self$621,
  struct $StringView str$622
) {
  int64_t _bind$620;
  moonbit_incref(str$622.$0);
  moonbit_incref(self$621.$0);
  _bind$620 = $StringView$$rev_find(self$621, str$622);
  if (_bind$620 == 4294967296ll) {
    moonbit_decref(str$622.$0);
    moonbit_decref(self$621.$0);
    return 0;
  } else {
    int64_t _Some$623 = _bind$620;
    int32_t _i$624 = (int32_t)_Some$623;
    int32_t _tmp$2403 = $StringView$$length(self$621);
    int32_t _tmp$2404 = $StringView$$length(str$622);
    int32_t _tmp$2402 = _tmp$2403 - _tmp$2404;
    return _i$624 == _tmp$2402;
  }
}

moonbit_string_t $String$$repeat(moonbit_string_t self$614, int32_t n$613) {
  if (n$613 <= 0) {
    moonbit_decref(self$614);
    return (moonbit_string_t)moonbit_string_literal_3.data;
  } else if (n$613 == 1) {
    return self$614;
  } else {
    int32_t len$615 = Moonbit_array_length(self$614);
    int32_t _tmp$2401 = len$615 * n$613;
    struct $$moonbitlang$core$builtin$StringBuilder* buf$616 =
      $$moonbitlang$core$builtin$StringBuilder$$new$inner(_tmp$2401);
    moonbit_string_t str$617 = self$614;
    int32_t _$618 = 0;
    while (1) {
      if (_$618 < n$613) {
        int32_t _tmp$2400;
        moonbit_incref(str$617);
        moonbit_incref(buf$616);
        $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
          buf$616, str$617
        );
        _tmp$2400 = _$618 + 1;
        _$618 = _tmp$2400;
        continue;
      } else {
        moonbit_decref(str$617);
      }
      break;
    }
    return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$616);
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
  struct $$moonbitlang$core$builtin$StringBuilder* self$611,
  struct $StringView str$612
) {
  int32_t len$2388 = self$611->$1;
  int32_t _tmp$2390;
  int32_t _tmp$2389;
  int32_t _tmp$2387;
  moonbit_bytes_t _field$4369;
  moonbit_bytes_t data$2391;
  int32_t len$2392;
  moonbit_string_t _tmp$2393;
  int32_t _tmp$2394;
  int32_t _tmp$2395;
  int32_t len$2397;
  int32_t _tmp$2399;
  int32_t _tmp$2398;
  int32_t _tmp$2396;
  moonbit_incref(str$612.$0);
  _tmp$2390 = $StringView$$length(str$612);
  _tmp$2389 = _tmp$2390 * 2;
  _tmp$2387 = len$2388 + _tmp$2389;
  moonbit_incref(self$611);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$611, _tmp$2387
  );
  _field$4369 = self$611->$0;
  data$2391 = _field$4369;
  len$2392 = self$611->$1;
  moonbit_incref(data$2391);
  moonbit_incref(str$612.$0);
  _tmp$2393 = $StringView$$data(str$612);
  moonbit_incref(str$612.$0);
  _tmp$2394 = $StringView$$start_offset(str$612);
  moonbit_incref(str$612.$0);
  _tmp$2395 = $StringView$$length(str$612);
  $FixedArray$$blit_from_string(
    data$2391, len$2392, _tmp$2393, _tmp$2394, _tmp$2395
  );
  len$2397 = self$611->$1;
  _tmp$2399 = $StringView$$length(str$612);
  _tmp$2398 = _tmp$2399 * 2;
  _tmp$2396 = len$2397 + _tmp$2398;
  self$611->$1 = _tmp$2396;
  moonbit_decref(self$611);
  return 0;
}

int64_t $String$$offset_of_nth_char$inner(
  moonbit_string_t self$608,
  int32_t i$609,
  int32_t start_offset$610,
  int64_t end_offset$606
) {
  int32_t end_offset$605;
  if (end_offset$606 == 4294967296ll) {
    end_offset$605 = Moonbit_array_length(self$608);
  } else {
    int64_t _Some$607 = end_offset$606;
    end_offset$605 = (int32_t)_Some$607;
  }
  if (i$609 >= 0) {
    return $String$$offset_of_nth_char_forward(
             self$608, i$609, start_offset$610, end_offset$605
           );
  } else {
    int32_t _tmp$2386 = -i$609;
    return $String$$offset_of_nth_char_backward(
             self$608, _tmp$2386, start_offset$610, end_offset$605
           );
  }
}

int64_t $String$$offset_of_nth_char_forward(
  moonbit_string_t self$603,
  int32_t n$601,
  int32_t start_offset$597,
  int32_t end_offset$598
) {
  int32_t _if_result$5003;
  if (start_offset$597 >= 0) {
    _if_result$5003 = start_offset$597 <= end_offset$598;
  } else {
    _if_result$5003 = 0;
  }
  if (_if_result$5003) {
    int32_t utf16_offset$599 = start_offset$597;
    int32_t char_count$600 = 0;
    int32_t _tmp$2384;
    int32_t _if_result$5006;
    while (1) {
      int32_t _tmp$2378 = utf16_offset$599;
      int32_t _if_result$5005;
      if (_tmp$2378 < end_offset$598) {
        int32_t _tmp$2377 = char_count$600;
        _if_result$5005 = _tmp$2377 < n$601;
      } else {
        _if_result$5005 = 0;
      }
      if (_if_result$5005) {
        int32_t _tmp$2382 = utf16_offset$599;
        int32_t c$602 = self$603[_tmp$2382];
        int32_t _tmp$2381;
        if ($Int$$is_leading_surrogate(c$602)) {
          int32_t _tmp$2379 = utf16_offset$599;
          utf16_offset$599 = _tmp$2379 + 2;
        } else {
          int32_t _tmp$2380 = utf16_offset$599;
          utf16_offset$599 = _tmp$2380 + 1;
        }
        _tmp$2381 = char_count$600;
        char_count$600 = _tmp$2381 + 1;
        continue;
      } else {
        moonbit_decref(self$603);
      }
      break;
    }
    _tmp$2384 = char_count$600;
    if (_tmp$2384 < n$601) {
      _if_result$5006 = 1;
    } else {
      int32_t _tmp$2383 = utf16_offset$599;
      _if_result$5006 = _tmp$2383 >= end_offset$598;
    }
    if (_if_result$5006) {
      return 4294967296ll;
    } else {
      int32_t _tmp$2385 = utf16_offset$599;
      return (int64_t)_tmp$2385;
    }
  } else {
    moonbit_decref(self$603);
    return $moonbitlang$core$builtin$abort$3(
             (moonbit_string_t)moonbit_string_literal_249.data,
               (moonbit_string_t)moonbit_string_literal_250.data
           );
  }
}

int64_t $String$$offset_of_nth_char_backward(
  moonbit_string_t self$595,
  int32_t n$593,
  int32_t start_offset$592,
  int32_t end_offset$591
) {
  int32_t char_count$589 = 0;
  int32_t utf16_offset$590 = end_offset$591;
  int32_t _tmp$2375;
  int32_t _if_result$5009;
  while (1) {
    int32_t _tmp$2368 = utf16_offset$590;
    int32_t _tmp$2367 = _tmp$2368 - 1;
    int32_t _if_result$5008;
    if (_tmp$2367 >= start_offset$592) {
      int32_t _tmp$2366 = char_count$589;
      _if_result$5008 = _tmp$2366 < n$593;
    } else {
      _if_result$5008 = 0;
    }
    if (_if_result$5008) {
      int32_t _tmp$2373 = utf16_offset$590;
      int32_t _tmp$2372 = _tmp$2373 - 1;
      int32_t c$594 = self$595[_tmp$2372];
      int32_t _tmp$2371;
      if ($Int$$is_trailing_surrogate(c$594)) {
        int32_t _tmp$2369 = utf16_offset$590;
        utf16_offset$590 = _tmp$2369 - 2;
      } else {
        int32_t _tmp$2370 = utf16_offset$590;
        utf16_offset$590 = _tmp$2370 - 1;
      }
      _tmp$2371 = char_count$589;
      char_count$589 = _tmp$2371 + 1;
      continue;
    } else {
      moonbit_decref(self$595);
    }
    break;
  }
  _tmp$2375 = char_count$589;
  if (_tmp$2375 < n$593) {
    _if_result$5009 = 1;
  } else {
    int32_t _tmp$2374 = utf16_offset$590;
    _if_result$5009 = _tmp$2374 < start_offset$592;
  }
  if (_if_result$5009) {
    return 4294967296ll;
  } else {
    int32_t _tmp$2376 = utf16_offset$590;
    return (int64_t)_tmp$2376;
  }
}

int32_t $String$$char_length_ge$inner(
  moonbit_string_t self$581,
  int32_t len$584,
  int32_t start_offset$588,
  int64_t end_offset$579
) {
  int32_t end_offset$578;
  int32_t index$582;
  int32_t count$583;
  if (end_offset$579 == 4294967296ll) {
    end_offset$578 = Moonbit_array_length(self$581);
  } else {
    int64_t _Some$580 = end_offset$579;
    end_offset$578 = (int32_t)_Some$580;
  }
  index$582 = start_offset$588;
  count$583 = 0;
  while (1) {
    int32_t _if_result$5011;
    if (index$582 < end_offset$578) {
      _if_result$5011 = count$583 < len$584;
    } else {
      _if_result$5011 = 0;
    }
    if (_if_result$5011) {
      int32_t c1$585 = self$581[index$582];
      int32_t _if_result$5012;
      int32_t _tmp$2364;
      int32_t _tmp$2365;
      if ($Int$$is_leading_surrogate(c1$585)) {
        int32_t _tmp$2360 = index$582 + 1;
        _if_result$5012 = _tmp$2360 < end_offset$578;
      } else {
        _if_result$5012 = 0;
      }
      if (_if_result$5012) {
        int32_t _tmp$2363 = index$582 + 1;
        int32_t c2$586 = self$581[_tmp$2363];
        if ($Int$$is_trailing_surrogate(c2$586)) {
          int32_t _tmp$2361 = index$582 + 2;
          int32_t _tmp$2362 = count$583 + 1;
          index$582 = _tmp$2361;
          count$583 = _tmp$2362;
          continue;
        } else {
          $moonbitlang$core$builtin$abort$0(
            (moonbit_string_t)moonbit_string_literal_251.data,
              (moonbit_string_t)moonbit_string_literal_252.data
          );
        }
      }
      _tmp$2364 = index$582 + 1;
      _tmp$2365 = count$583 + 1;
      index$582 = _tmp$2364;
      count$583 = _tmp$2365;
      continue;
    } else {
      moonbit_decref(self$581);
      return count$583 >= len$584;
    }
    break;
  }
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$4(
  struct $$moonbitlang$core$builtin$ArrayView$3c$String$3e$ self$577
) {
  int32_t end$2358 = self$577.$2;
  int32_t _field$4370 = self$577.$1;
  int32_t start$2359;
  moonbit_decref(self$577.$0);
  start$2359 = _field$4370;
  return end$2358 - start$2359;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$3(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$576
) {
  int32_t end$2356 = self$576.$2;
  int32_t _field$4371 = self$576.$1;
  int32_t start$2357;
  moonbit_decref(self$576.$0);
  start$2357 = _field$4371;
  return end$2356 - start$2357;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$2(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$575
) {
  int32_t end$2354 = self$575.$2;
  int32_t _field$4372 = self$575.$1;
  int32_t start$2355;
  moonbit_decref(self$575.$0);
  start$2355 = _field$4372;
  return end$2354 - start$2355;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$1(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ self$574
) {
  int32_t end$2352 = self$574.$2;
  int32_t _field$4373 = self$574.$1;
  int32_t start$2353;
  moonbit_decref(self$574.$0);
  start$2353 = _field$4373;
  return end$2352 - start$2353;
}

int32_t $$moonbitlang$core$builtin$ArrayView$$length$0(
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ self$573
) {
  int32_t end$2350 = self$573.$2;
  int32_t _field$4374 = self$573.$1;
  int32_t start$2351;
  moonbit_decref(self$573.$0);
  start$2351 = _field$4374;
  return end$2350 - start$2351;
}

struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* $$moonbitlang$core$builtin$Iterator$$iter$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$568
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _closure$5013 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)moonbit_malloc(
      sizeof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap)
    );
  Moonbit_object_header(_closure$5013)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap, $0) >> 2, 1, 0
  );
  _closure$5013->code = &$Iterator$$iter$7c$String$7c$$fn$1;
  _closure$5013->$0 = self$568;
  return (struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int*)_closure$5013;
}

int32_t $Iterator$$iter$7c$String$7c$$fn$1(
  struct $$3c$$3c$String$3e$$3d$$3e$Int$3e$$3d$$3e$Int* _env$2348,
  struct $$3c$String$3e$$3d$$3e$Int* yield_$566
) {
  struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap* _casted_env$2349 =
    (struct $Iterator$$iter$7c$String$7c$$fn$1$2d$cap*)_env$2348;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _field$4375 =
    _casted_env$2349->$0;
  int32_t _cnt$4536 = Moonbit_object_header(_casted_env$2349)->rc;
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$568;
  if (_cnt$4536 > 1) {
    int32_t _new_cnt$4537;
    moonbit_incref(_field$4375);
    _new_cnt$4537 = _cnt$4536 - 1;
    Moonbit_object_header(_casted_env$2349)->rc = _new_cnt$4537;
  } else if (_cnt$4536 == 1) {
    moonbit_free(_casted_env$2349);
  }
  self$568 = _field$4375;
  while (1) {
    moonbit_string_t _bind$567;
    moonbit_incref(self$568);
    _bind$567 = $$moonbitlang$core$builtin$Iterator$$next$0(self$568);
    if (_bind$567 == 0) {
      moonbit_decref(self$568);
      if (_bind$567) {
        moonbit_decref(_bind$567);
      }
      moonbit_decref(yield_$566);
      return 1;
    } else {
      moonbit_string_t _Some$569 = _bind$567;
      moonbit_string_t _x$570 = _Some$569;
      int32_t _bind$571;
      moonbit_incref(yield_$566);
      _bind$571 = yield_$566->code(yield_$566, _x$570);
      switch (_bind$571) {
        case 1:
          break;
        default: {
          moonbit_decref(self$568);
          moonbit_decref(yield_$566);
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
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* self$565
) {
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* _func$564 = self$565;
  return _func$564->code(_func$564);
}

int32_t $$moonbitlang$core$builtin$Show$$String$$output(
  moonbit_string_t self$556,
  struct $$moonbitlang$core$builtin$Logger logger$554
) {
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$555;
  int32_t len$557;
  int32_t i$558;
  int32_t seg$559;
  if (logger$554.$1) {
    moonbit_incref(logger$554.$1);
  }
  logger$554.$0->$method_3(logger$554.$1, 34);
  if (logger$554.$1) {
    moonbit_incref(logger$554.$1);
  }
  moonbit_incref(self$556);
  _env$555
  = (struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$*)moonbit_malloc(
      sizeof(struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$)
    );
  Moonbit_object_header(_env$555)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$, $0_0
    )
    >> 2,
      3,
      0
  );
  _env$555->$0_0 = logger$554.$0;
  _env$555->$0_1 = logger$554.$1;
  _env$555->$1 = self$556;
  len$557 = Moonbit_array_length(self$556);
  i$558 = 0;
  seg$559 = 0;
  $$2a$for$560:;
  while (1) {
    int32_t code$561;
    int32_t c$563;
    struct $$moonbitlang$core$builtin$Logger _bind$2330;
    int32_t _tmp$2331;
    int32_t _tmp$2332;
    int32_t _tmp$2333;
    int32_t _tmp$5018;
    int32_t _tmp$5019;
    if (i$558 >= len$557) {
      moonbit_decref(self$556);
      $moonbitlang$core$builtin$output$flush_segment$7c$3831(
        _env$555, seg$559, i$558
      );
      break;
    }
    code$561 = self$556[i$558];
    switch (code$561) {
      case 34: {
        c$563 = code$561;
        goto $join$562;
        break;
      }
      
      case 92: {
        c$563 = code$561;
        goto $join$562;
        break;
      }
      
      case 10: {
        int32_t _tmp$2334;
        int32_t _tmp$2335;
        moonbit_incref(_env$555);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$555, seg$559, i$558
        );
        if (logger$554.$1) {
          moonbit_incref(logger$554.$1);
        }
        logger$554.$0->$method_0(
          logger$554.$1, (moonbit_string_t)moonbit_string_literal_253.data
        );
        _tmp$2334 = i$558 + 1;
        _tmp$2335 = i$558 + 1;
        i$558 = _tmp$2334;
        seg$559 = _tmp$2335;
        goto $$2a$for$560;
        break;
      }
      
      case 13: {
        int32_t _tmp$2336;
        int32_t _tmp$2337;
        moonbit_incref(_env$555);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$555, seg$559, i$558
        );
        if (logger$554.$1) {
          moonbit_incref(logger$554.$1);
        }
        logger$554.$0->$method_0(
          logger$554.$1, (moonbit_string_t)moonbit_string_literal_254.data
        );
        _tmp$2336 = i$558 + 1;
        _tmp$2337 = i$558 + 1;
        i$558 = _tmp$2336;
        seg$559 = _tmp$2337;
        goto $$2a$for$560;
        break;
      }
      
      case 8: {
        int32_t _tmp$2338;
        int32_t _tmp$2339;
        moonbit_incref(_env$555);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$555, seg$559, i$558
        );
        if (logger$554.$1) {
          moonbit_incref(logger$554.$1);
        }
        logger$554.$0->$method_0(
          logger$554.$1, (moonbit_string_t)moonbit_string_literal_255.data
        );
        _tmp$2338 = i$558 + 1;
        _tmp$2339 = i$558 + 1;
        i$558 = _tmp$2338;
        seg$559 = _tmp$2339;
        goto $$2a$for$560;
        break;
      }
      
      case 9: {
        int32_t _tmp$2340;
        int32_t _tmp$2341;
        moonbit_incref(_env$555);
        $moonbitlang$core$builtin$output$flush_segment$7c$3831(
          _env$555, seg$559, i$558
        );
        if (logger$554.$1) {
          moonbit_incref(logger$554.$1);
        }
        logger$554.$0->$method_0(
          logger$554.$1, (moonbit_string_t)moonbit_string_literal_256.data
        );
        _tmp$2340 = i$558 + 1;
        _tmp$2341 = i$558 + 1;
        i$558 = _tmp$2340;
        seg$559 = _tmp$2341;
        goto $$2a$for$560;
        break;
      }
      default: {
        if (code$561 < 32) {
          int32_t _tmp$2344;
          moonbit_string_t _tmp$2343;
          struct $$moonbitlang$core$builtin$Logger _bind$2342;
          int32_t _tmp$2345;
          int32_t _tmp$2346;
          moonbit_incref(_env$555);
          $moonbitlang$core$builtin$output$flush_segment$7c$3831(
            _env$555, seg$559, i$558
          );
          if (logger$554.$1) {
            moonbit_incref(logger$554.$1);
          }
          logger$554.$0->$method_0(
            logger$554.$1, (moonbit_string_t)moonbit_string_literal_257.data
          );
          _tmp$2344 = code$561 & 0xff;
          _tmp$2343 = $Byte$$to_hex(_tmp$2344);
          if (logger$554.$1) {
            moonbit_incref(logger$554.$1);
          }
          logger$554.$0->$method_0(logger$554.$1, _tmp$2343);
          _bind$2342 = logger$554;
          if (_bind$2342.$1) {
            moonbit_incref(_bind$2342.$1);
          }
          _bind$2342.$0->$method_3(_bind$2342.$1, 125);
          _tmp$2345 = i$558 + 1;
          _tmp$2346 = i$558 + 1;
          i$558 = _tmp$2345;
          seg$559 = _tmp$2346;
          goto $$2a$for$560;
        } else {
          int32_t _tmp$2347 = i$558 + 1;
          int32_t _tmp$5017 = seg$559;
          i$558 = _tmp$2347;
          seg$559 = _tmp$5017;
          goto $$2a$for$560;
        }
        break;
      }
    }
    goto $joinlet$5016;
    $join$562:;
    moonbit_incref(_env$555);
    $moonbitlang$core$builtin$output$flush_segment$7c$3831(
      _env$555, seg$559, i$558
    );
    if (logger$554.$1) {
      moonbit_incref(logger$554.$1);
    }
    logger$554.$0->$method_3(logger$554.$1, 92);
    _bind$2330 = logger$554;
    _tmp$2331 = c$563;
    if (_bind$2330.$1) {
      moonbit_incref(_bind$2330.$1);
    }
    _bind$2330.$0->$method_3(_bind$2330.$1, _tmp$2331);
    _tmp$2332 = i$558 + 1;
    _tmp$2333 = i$558 + 1;
    i$558 = _tmp$2332;
    seg$559 = _tmp$2333;
    continue;
    $joinlet$5016:;
    _tmp$5018 = i$558;
    _tmp$5019 = seg$559;
    i$558 = _tmp$5018;
    seg$559 = _tmp$5019;
    continue;
    break;
  }
  logger$554.$0->$method_3(logger$554.$1, 34);
  return 0;
}

int32_t $moonbitlang$core$builtin$output$flush_segment$7c$3831(
  struct $$3c$$moonbitlang$core$builtin$Logger$2a$String$3e$* _env$550,
  int32_t seg$553,
  int32_t i$552
) {
  moonbit_string_t _field$4377 = _env$550->$1;
  moonbit_string_t self$549 = _field$4377;
  struct $$moonbitlang$core$builtin$Logger _field$4376 =
    (struct $$moonbitlang$core$builtin$Logger){
      _env$550->$0_0, _env$550->$0_1
    };
  int32_t _cnt$4538 = Moonbit_object_header(_env$550)->rc;
  struct $$moonbitlang$core$builtin$Logger logger$551;
  if (_cnt$4538 > 1) {
    int32_t _new_cnt$4539;
    moonbit_incref(self$549);
    if (_field$4376.$1) {
      moonbit_incref(_field$4376.$1);
    }
    _new_cnt$4539 = _cnt$4538 - 1;
    Moonbit_object_header(_env$550)->rc = _new_cnt$4539;
  } else if (_cnt$4538 == 1) {
    moonbit_free(_env$550);
  }
  logger$551 = _field$4376;
  if (i$552 > seg$553) {
    int32_t _tmp$2329 = i$552 - seg$553;
    logger$551.$0->$method_1(logger$551.$1, self$549, seg$553, _tmp$2329);
  } else {
    if (logger$551.$1) {
      moonbit_decref(logger$551.$1);
    }
    moonbit_decref(self$549);
  }
  return 0;
}

moonbit_string_t $Byte$$to_hex(int32_t b$548) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$547 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  int32_t _tmp$2326 = $$moonbitlang$core$builtin$Div$$Byte$$div(b$548, 16);
  int32_t _tmp$2325 =
    $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(_tmp$2326);
  int32_t _tmp$2328;
  int32_t _tmp$2327;
  struct $$moonbitlang$core$builtin$StringBuilder* _tmp$2324;
  moonbit_incref(_self$547);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$547, _tmp$2325
  );
  _tmp$2328 = $$moonbitlang$core$builtin$Mod$$Byte$$mod(b$548, 16);
  _tmp$2327
  = $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(
    _tmp$2328
  );
  moonbit_incref(_self$547);
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$547, _tmp$2327
  );
  _tmp$2324 = _self$547;
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(_tmp$2324);
}

int32_t $moonbitlang$core$builtin$to_hex$to_hex_digit$7c$3841(int32_t i$546) {
  if (i$546 < 10) {
    int32_t _tmp$2321 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$546, 48);
    return $Byte$$to_char(_tmp$2321);
  } else {
    int32_t _tmp$2323 = $$moonbitlang$core$builtin$Add$$Byte$$add(i$546, 97);
    int32_t _tmp$2322 =
      $$moonbitlang$core$builtin$Sub$$Byte$$sub(_tmp$2323, 10);
    return $Byte$$to_char(_tmp$2322);
  }
}

int32_t $$moonbitlang$core$builtin$Sub$$Byte$$sub(
  int32_t self$544,
  int32_t that$545
) {
  int32_t _tmp$2319 = (int32_t)self$544;
  int32_t _tmp$2320 = (int32_t)that$545;
  int32_t _tmp$2318 = _tmp$2319 - _tmp$2320;
  return _tmp$2318 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Mod$$Byte$$mod(
  int32_t self$542,
  int32_t that$543
) {
  int32_t _tmp$2316 = (int32_t)self$542;
  int32_t _tmp$2317 = (int32_t)that$543;
  int32_t _tmp$2315 = _tmp$2316 % _tmp$2317;
  return _tmp$2315 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Div$$Byte$$div(
  int32_t self$540,
  int32_t that$541
) {
  int32_t _tmp$2313 = (int32_t)self$540;
  int32_t _tmp$2314 = (int32_t)that$541;
  int32_t _tmp$2312 = _tmp$2313 / _tmp$2314;
  return _tmp$2312 & 0xff;
}

int32_t $$moonbitlang$core$builtin$Add$$Byte$$add(
  int32_t self$538,
  int32_t that$539
) {
  int32_t _tmp$2310 = (int32_t)self$538;
  int32_t _tmp$2311 = (int32_t)that$539;
  int32_t _tmp$2309 = _tmp$2310 + _tmp$2311;
  return _tmp$2309 & 0xff;
}

moonbit_string_t $String$$unsafe_substring(
  moonbit_string_t str$535,
  int32_t start$533,
  int32_t end$534
) {
  int32_t _if_result$5020;
  int32_t len$536;
  int32_t _tmp$2307;
  int32_t _tmp$2308;
  moonbit_bytes_t bytes$537;
  moonbit_bytes_t _tmp$2306;
  if (start$533 == 0) {
    int32_t _tmp$2305 = Moonbit_array_length(str$535);
    _if_result$5020 = end$534 == _tmp$2305;
  } else {
    _if_result$5020 = 0;
  }
  if (_if_result$5020) {
    return str$535;
  }
  len$536 = end$534 - start$533;
  _tmp$2307 = len$536 * 2;
  _tmp$2308 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  bytes$537 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$2307, _tmp$2308);
  moonbit_incref(bytes$537);
  $FixedArray$$blit_from_string(bytes$537, 0, str$535, start$533, len$536);
  _tmp$2306 = bytes$537;
  return $Bytes$$to_unchecked_string$inner(_tmp$2306, 0, 4294967296ll);
}

struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* $$moonbitlang$core$builtin$Iterator$$new$0(
  struct $$3c$$3e$$3d$$3e$Option$3c$String$3e$* f$532
) {
  return f$532;
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$7(
  int32_t a$526,
  int32_t b$527,
  moonbit_string_t msg$529,
  moonbit_string_t loc$531
) {
  if (a$526 != b$527) {
    moonbit_string_t fail_msg$528;
    if (msg$529 == 0) {
      moonbit_string_t _tmp$2303;
      moonbit_string_t _tmp$2302;
      moonbit_string_t _tmp$2301;
      moonbit_string_t _tmp$2298;
      moonbit_string_t _tmp$2300;
      moonbit_string_t _tmp$2299;
      moonbit_string_t _tmp$2297;
      if (msg$529) {
        moonbit_decref(msg$529);
      }
      _tmp$2303 = $moonbitlang$core$builtin$debug_string$7(a$526);
      _tmp$2302
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2303
      );
      _tmp$2301
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2302
      );
      _tmp$2298
      = moonbit_add_string(
        _tmp$2301, (moonbit_string_t)moonbit_string_literal_258.data
      );
      _tmp$2300 = $moonbitlang$core$builtin$debug_string$7(b$527);
      _tmp$2299
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2300
      );
      _tmp$2297 = moonbit_add_string(_tmp$2298, _tmp$2299);
      fail_msg$528
      = moonbit_add_string(
        _tmp$2297, (moonbit_string_t)moonbit_string_literal_243.data
      );
    } else {
      moonbit_string_t _Some$530 = msg$529;
      fail_msg$528 = _Some$530;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$528, loc$531);
  } else {
    int32_t _tmp$2304;
    struct moonbit_result_0 _result$5021;
    moonbit_decref(loc$531);
    if (msg$529) {
      moonbit_decref(msg$529);
    }
    _tmp$2304 = 0;
    _result$5021.tag = 1;
    _result$5021.data.ok = _tmp$2304;
    return _result$5021;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$6(
  double a$520,
  double b$521,
  moonbit_string_t msg$523,
  moonbit_string_t loc$525
) {
  if (a$520 != b$521) {
    moonbit_string_t fail_msg$522;
    if (msg$523 == 0) {
      moonbit_string_t _tmp$2295;
      moonbit_string_t _tmp$2294;
      moonbit_string_t _tmp$2293;
      moonbit_string_t _tmp$2290;
      moonbit_string_t _tmp$2292;
      moonbit_string_t _tmp$2291;
      moonbit_string_t _tmp$2289;
      if (msg$523) {
        moonbit_decref(msg$523);
      }
      _tmp$2295 = $moonbitlang$core$builtin$debug_string$6(a$520);
      _tmp$2294
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2295
      );
      _tmp$2293
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2294
      );
      _tmp$2290
      = moonbit_add_string(
        _tmp$2293, (moonbit_string_t)moonbit_string_literal_258.data
      );
      _tmp$2292 = $moonbitlang$core$builtin$debug_string$6(b$521);
      _tmp$2291
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2292
      );
      _tmp$2289 = moonbit_add_string(_tmp$2290, _tmp$2291);
      fail_msg$522
      = moonbit_add_string(
        _tmp$2289, (moonbit_string_t)moonbit_string_literal_243.data
      );
    } else {
      moonbit_string_t _Some$524 = msg$523;
      fail_msg$522 = _Some$524;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$522, loc$525);
  } else {
    int32_t _tmp$2296;
    struct moonbit_result_0 _result$5022;
    moonbit_decref(loc$525);
    if (msg$523) {
      moonbit_decref(msg$523);
    }
    _tmp$2296 = 0;
    _result$5022.tag = 1;
    _result$5022.data.ok = _tmp$2296;
    return _result$5022;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$5(
  int32_t a$514,
  int32_t b$515,
  moonbit_string_t msg$517,
  moonbit_string_t loc$519
) {
  if ($moonbitlang$core$builtin$op_notequal$4(a$514, b$515)) {
    moonbit_string_t fail_msg$516;
    if (msg$517 == 0) {
      moonbit_string_t _tmp$2287;
      moonbit_string_t _tmp$2286;
      moonbit_string_t _tmp$2285;
      moonbit_string_t _tmp$2282;
      moonbit_string_t _tmp$2284;
      moonbit_string_t _tmp$2283;
      moonbit_string_t _tmp$2281;
      if (msg$517) {
        moonbit_decref(msg$517);
      }
      _tmp$2287 = $moonbitlang$core$builtin$debug_string$5(a$514);
      _tmp$2286
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2287
      );
      _tmp$2285
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2286
      );
      _tmp$2282
      = moonbit_add_string(
        _tmp$2285, (moonbit_string_t)moonbit_string_literal_258.data
      );
      _tmp$2284 = $moonbitlang$core$builtin$debug_string$5(b$515);
      _tmp$2283
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2284
      );
      _tmp$2281 = moonbit_add_string(_tmp$2282, _tmp$2283);
      fail_msg$516
      = moonbit_add_string(
        _tmp$2281, (moonbit_string_t)moonbit_string_literal_243.data
      );
    } else {
      moonbit_string_t _Some$518 = msg$517;
      fail_msg$516 = _Some$518;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$516, loc$519);
  } else {
    int32_t _tmp$2288;
    struct moonbit_result_0 _result$5023;
    moonbit_decref(loc$519);
    if (msg$517) {
      moonbit_decref(msg$517);
    }
    _tmp$2288 = 0;
    _result$5023.tag = 1;
    _result$5023.data.ok = _tmp$2288;
    return _result$5023;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$4(
  int32_t a$508,
  int32_t b$509,
  moonbit_string_t msg$511,
  moonbit_string_t loc$513
) {
  if ($moonbitlang$core$builtin$op_notequal$3(a$508, b$509)) {
    moonbit_string_t fail_msg$510;
    if (msg$511 == 0) {
      moonbit_string_t _tmp$2279;
      moonbit_string_t _tmp$2278;
      moonbit_string_t _tmp$2277;
      moonbit_string_t _tmp$2274;
      moonbit_string_t _tmp$2276;
      moonbit_string_t _tmp$2275;
      moonbit_string_t _tmp$2273;
      if (msg$511) {
        moonbit_decref(msg$511);
      }
      _tmp$2279 = $moonbitlang$core$builtin$debug_string$4(a$508);
      _tmp$2278
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2279
      );
      _tmp$2277
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2278
      );
      _tmp$2274
      = moonbit_add_string(
        _tmp$2277, (moonbit_string_t)moonbit_string_literal_258.data
      );
      _tmp$2276 = $moonbitlang$core$builtin$debug_string$4(b$509);
      _tmp$2275
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2276
      );
      _tmp$2273 = moonbit_add_string(_tmp$2274, _tmp$2275);
      fail_msg$510
      = moonbit_add_string(
        _tmp$2273, (moonbit_string_t)moonbit_string_literal_243.data
      );
    } else {
      moonbit_string_t _Some$512 = msg$511;
      fail_msg$510 = _Some$512;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$510, loc$513);
  } else {
    int32_t _tmp$2280;
    struct moonbit_result_0 _result$5024;
    moonbit_decref(loc$513);
    if (msg$511) {
      moonbit_decref(msg$511);
    }
    _tmp$2280 = 0;
    _result$5024.tag = 1;
    _result$5024.data.ok = _tmp$2280;
    return _result$5024;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$3(
  moonbit_string_t a$502,
  moonbit_string_t b$503,
  moonbit_string_t msg$505,
  moonbit_string_t loc$507
) {
  if (b$503) {
    moonbit_incref(b$503);
  }
  if (a$502) {
    moonbit_incref(a$502);
  }
  if ($moonbitlang$core$builtin$op_notequal$2(a$502, b$503)) {
    moonbit_string_t fail_msg$504;
    if (msg$505 == 0) {
      moonbit_string_t _tmp$2271;
      moonbit_string_t _tmp$2270;
      moonbit_string_t _tmp$2269;
      moonbit_string_t _tmp$2266;
      moonbit_string_t _tmp$2268;
      moonbit_string_t _tmp$2267;
      moonbit_string_t _tmp$2265;
      if (msg$505) {
        moonbit_decref(msg$505);
      }
      _tmp$2271 = $moonbitlang$core$builtin$debug_string$3(a$502);
      _tmp$2270
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2271
      );
      _tmp$2269
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2270
      );
      _tmp$2266
      = moonbit_add_string(
        _tmp$2269, (moonbit_string_t)moonbit_string_literal_258.data
      );
      _tmp$2268 = $moonbitlang$core$builtin$debug_string$3(b$503);
      _tmp$2267
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2268
      );
      _tmp$2265 = moonbit_add_string(_tmp$2266, _tmp$2267);
      fail_msg$504
      = moonbit_add_string(
        _tmp$2265, (moonbit_string_t)moonbit_string_literal_243.data
      );
    } else {
      moonbit_string_t _Some$506;
      if (b$503) {
        moonbit_decref(b$503);
      }
      if (a$502) {
        moonbit_decref(a$502);
      }
      _Some$506 = msg$505;
      fail_msg$504 = _Some$506;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$504, loc$507);
  } else {
    int32_t _tmp$2272;
    struct moonbit_result_0 _result$5025;
    moonbit_decref(loc$507);
    if (msg$505) {
      moonbit_decref(msg$505);
    }
    if (b$503) {
      moonbit_decref(b$503);
    }
    if (a$502) {
      moonbit_decref(a$502);
    }
    _tmp$2272 = 0;
    _result$5025.tag = 1;
    _result$5025.data.ok = _tmp$2272;
    return _result$5025;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$2(
  int64_t a$496,
  int64_t b$497,
  moonbit_string_t msg$499,
  moonbit_string_t loc$501
) {
  if ($moonbitlang$core$builtin$op_notequal$1(a$496, b$497)) {
    moonbit_string_t fail_msg$498;
    if (msg$499 == 0) {
      moonbit_string_t _tmp$2263;
      moonbit_string_t _tmp$2262;
      moonbit_string_t _tmp$2261;
      moonbit_string_t _tmp$2258;
      moonbit_string_t _tmp$2260;
      moonbit_string_t _tmp$2259;
      moonbit_string_t _tmp$2257;
      if (msg$499) {
        moonbit_decref(msg$499);
      }
      _tmp$2263 = $moonbitlang$core$builtin$debug_string$2(a$496);
      _tmp$2262
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2263
      );
      _tmp$2261
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2262
      );
      _tmp$2258
      = moonbit_add_string(
        _tmp$2261, (moonbit_string_t)moonbit_string_literal_258.data
      );
      _tmp$2260 = $moonbitlang$core$builtin$debug_string$2(b$497);
      _tmp$2259
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2260
      );
      _tmp$2257 = moonbit_add_string(_tmp$2258, _tmp$2259);
      fail_msg$498
      = moonbit_add_string(
        _tmp$2257, (moonbit_string_t)moonbit_string_literal_243.data
      );
    } else {
      moonbit_string_t _Some$500 = msg$499;
      fail_msg$498 = _Some$500;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$498, loc$501);
  } else {
    int32_t _tmp$2264;
    struct moonbit_result_0 _result$5026;
    moonbit_decref(loc$501);
    if (msg$499) {
      moonbit_decref(msg$499);
    }
    _tmp$2264 = 0;
    _result$5026.tag = 1;
    _result$5026.data.ok = _tmp$2264;
    return _result$5026;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$1(
  moonbit_string_t a$490,
  moonbit_string_t b$491,
  moonbit_string_t msg$493,
  moonbit_string_t loc$495
) {
  moonbit_incref(b$491);
  moonbit_incref(a$490);
  if ($moonbitlang$core$builtin$op_notequal$5(a$490, b$491)) {
    moonbit_string_t fail_msg$492;
    if (msg$493 == 0) {
      moonbit_string_t _tmp$2255;
      moonbit_string_t _tmp$2254;
      moonbit_string_t _tmp$2253;
      moonbit_string_t _tmp$2250;
      moonbit_string_t _tmp$2252;
      moonbit_string_t _tmp$2251;
      moonbit_string_t _tmp$2249;
      if (msg$493) {
        moonbit_decref(msg$493);
      }
      _tmp$2255 = $moonbitlang$core$builtin$debug_string$1(a$490);
      _tmp$2254
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2255
      );
      _tmp$2253
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2254
      );
      _tmp$2250
      = moonbit_add_string(
        _tmp$2253, (moonbit_string_t)moonbit_string_literal_258.data
      );
      _tmp$2252 = $moonbitlang$core$builtin$debug_string$1(b$491);
      _tmp$2251
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2252
      );
      _tmp$2249 = moonbit_add_string(_tmp$2250, _tmp$2251);
      fail_msg$492
      = moonbit_add_string(
        _tmp$2249, (moonbit_string_t)moonbit_string_literal_243.data
      );
    } else {
      moonbit_string_t _Some$494;
      moonbit_decref(b$491);
      moonbit_decref(a$490);
      _Some$494 = msg$493;
      fail_msg$492 = _Some$494;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$492, loc$495);
  } else {
    int32_t _tmp$2256;
    struct moonbit_result_0 _result$5027;
    moonbit_decref(loc$495);
    if (msg$493) {
      moonbit_decref(msg$493);
    }
    moonbit_decref(b$491);
    moonbit_decref(a$490);
    _tmp$2256 = 0;
    _result$5027.tag = 1;
    _result$5027.data.ok = _tmp$2256;
    return _result$5027;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$assert_eq$0(
  int32_t a$484,
  int32_t b$485,
  moonbit_string_t msg$487,
  moonbit_string_t loc$489
) {
  if (a$484 != b$485) {
    moonbit_string_t fail_msg$486;
    if (msg$487 == 0) {
      moonbit_string_t _tmp$2247;
      moonbit_string_t _tmp$2246;
      moonbit_string_t _tmp$2245;
      moonbit_string_t _tmp$2242;
      moonbit_string_t _tmp$2244;
      moonbit_string_t _tmp$2243;
      moonbit_string_t _tmp$2241;
      if (msg$487) {
        moonbit_decref(msg$487);
      }
      _tmp$2247 = $moonbitlang$core$builtin$debug_string$0(a$484);
      _tmp$2246
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2247
      );
      _tmp$2245
      = moonbit_add_string(
        (moonbit_string_t)moonbit_string_literal_243.data, _tmp$2246
      );
      _tmp$2242
      = moonbit_add_string(
        _tmp$2245, (moonbit_string_t)moonbit_string_literal_258.data
      );
      _tmp$2244 = $moonbitlang$core$builtin$debug_string$0(b$485);
      _tmp$2243
      = $$moonbitlang$core$builtin$Show$$String$$to_string(
        _tmp$2244
      );
      _tmp$2241 = moonbit_add_string(_tmp$2242, _tmp$2243);
      fail_msg$486
      = moonbit_add_string(
        _tmp$2241, (moonbit_string_t)moonbit_string_literal_243.data
      );
    } else {
      moonbit_string_t _Some$488 = msg$487;
      fail_msg$486 = _Some$488;
    }
    return $moonbitlang$core$builtin$fail$0(fail_msg$486, loc$489);
  } else {
    int32_t _tmp$2248;
    struct moonbit_result_0 _result$5028;
    moonbit_decref(loc$489);
    if (msg$487) {
      moonbit_decref(msg$487);
    }
    _tmp$2248 = 0;
    _result$5028.tag = 1;
    _result$5028.data.ok = _tmp$2248;
    return _result$5028;
  }
}

struct moonbit_result_0 $moonbitlang$core$builtin$fail$0(
  moonbit_string_t msg$483,
  moonbit_string_t loc$482
) {
  moonbit_string_t _tmp$2240 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$482);
  moonbit_string_t _tmp$2238 =
    moonbit_add_string(
      _tmp$2240, (moonbit_string_t)moonbit_string_literal_259.data
    );
  moonbit_string_t _tmp$2239 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(msg$483);
  moonbit_string_t _tmp$2237 = moonbit_add_string(_tmp$2238, _tmp$2239);
  void* moonbitlang$core$builtin$Failure$Failure$2236 =
    (void*)moonbit_malloc(
      sizeof(struct $Error$moonbitlang$core$builtin$Failure$Failure)
    );
  struct moonbit_result_0 _result$5029;
  Moonbit_object_header(moonbitlang$core$builtin$Failure$Failure$2236)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $Error$moonbitlang$core$builtin$Failure$Failure, $0) >> 2,
      1,
      2
  );
  ((struct $Error$moonbitlang$core$builtin$Failure$Failure*)moonbitlang$core$builtin$Failure$Failure$2236)->$0
  = _tmp$2237;
  _result$5029.tag = 0;
  _result$5029.data.err = moonbitlang$core$builtin$Failure$Failure$2236;
  return _result$5029;
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$7(int32_t t$481) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$480 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2235;
  moonbit_incref(buf$480);
  _tmp$2235
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$480
  };
  $$moonbitlang$core$builtin$Show$$UInt16$$output(t$481, _tmp$2235);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$480);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$6(double t$479) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$478 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2234;
  moonbit_incref(buf$478);
  _tmp$2234
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$478
  };
  $$moonbitlang$core$builtin$Show$$Double$$output(t$479, _tmp$2234);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$478);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$5(int32_t t$477) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$476 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2233;
  moonbit_incref(buf$476);
  _tmp$2233
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$476
  };
  $$moonbitlang$core$builtin$Show$$Bool$$output(t$477, _tmp$2233);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$476);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$4(int32_t t$475) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$474 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2232;
  moonbit_incref(buf$474);
  _tmp$2232
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$474
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$2(t$475, _tmp$2232);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$474);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$3(
  moonbit_string_t t$473
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$472 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2231;
  moonbit_incref(buf$472);
  _tmp$2231
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$472
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$1(t$473, _tmp$2231);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$472);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$2(int64_t t$471) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$470 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2230;
  moonbit_incref(buf$470);
  _tmp$2230
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$470
  };
  $$moonbitlang$core$builtin$Show$$Option$$output$0(t$471, _tmp$2230);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$470);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$1(
  moonbit_string_t t$469
) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$468 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2229;
  moonbit_incref(buf$468);
  _tmp$2229
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$468
  };
  $$moonbitlang$core$builtin$Show$$String$$output(t$469, _tmp$2229);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$468);
}

moonbit_string_t $moonbitlang$core$builtin$debug_string$0(int32_t t$467) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$466 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(50);
  struct $$moonbitlang$core$builtin$Logger _tmp$2228;
  moonbit_incref(buf$466);
  _tmp$2228
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$466
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(t$467, _tmp$2228);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$466);
}

moonbit_string_t $UInt16$$to_string$inner(
  int32_t self$464,
  int32_t radix$465
) {
  int32_t _tmp$2227 = (int32_t)self$464;
  return $Int$$to_string$inner(_tmp$2227, radix$465);
}

moonbit_string_t $UInt64$$to_string$inner(
  uint64_t self$456,
  int32_t radix$455
) {
  int32_t _if_result$5030;
  uint16_t* buffer$457;
  if (radix$455 < 2) {
    _if_result$5030 = 1;
  } else {
    _if_result$5030 = radix$455 > 36;
  }
  if (_if_result$5030) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_260.data,
        (moonbit_string_t)moonbit_string_literal_261.data
    );
  }
  if (self$456 == 0ull) {
    return (moonbit_string_t)moonbit_string_literal_237.data;
  }
  switch (radix$455) {
    case 10: {
      int32_t len$458 = $moonbitlang$core$builtin$dec_count64(self$456);
      uint16_t* buffer$459 = (uint16_t*)moonbit_make_string(len$458, 0);
      moonbit_incref(buffer$459);
      $moonbitlang$core$builtin$int64_to_string_dec(
        buffer$459, self$456, 0, len$458
      );
      buffer$457 = buffer$459;
      break;
    }
    
    case 16: {
      int32_t len$460 = $moonbitlang$core$builtin$hex_count64(self$456);
      uint16_t* buffer$461 = (uint16_t*)moonbit_make_string(len$460, 0);
      moonbit_incref(buffer$461);
      $moonbitlang$core$builtin$int64_to_string_hex(
        buffer$461, self$456, 0, len$460
      );
      buffer$457 = buffer$461;
      break;
    }
    default: {
      int32_t len$462 =
        $moonbitlang$core$builtin$radix_count64(self$456, radix$455);
      uint16_t* buffer$463 = (uint16_t*)moonbit_make_string(len$462, 0);
      moonbit_incref(buffer$463);
      $moonbitlang$core$builtin$int64_to_string_generic(
        buffer$463, self$456, 0, len$462, radix$455
      );
      buffer$457 = buffer$463;
      break;
    }
  }
  return buffer$457;
}

int32_t $moonbitlang$core$builtin$int64_to_string_dec(
  uint16_t* buffer$445,
  uint64_t num$433,
  int32_t digit_start$436,
  int32_t total_len$435
) {
  uint64_t num$432 = num$433;
  int32_t offset$434 = total_len$435 - digit_start$436;
  uint64_t _tmp$2226;
  int32_t remaining$447;
  int32_t _tmp$2207;
  while (1) {
    uint64_t _tmp$2170 = num$432;
    if (_tmp$2170 >= 10000ull) {
      uint64_t _tmp$2193 = num$432;
      uint64_t t$437 = _tmp$2193 / 10000ull;
      uint64_t _tmp$2192 = num$432;
      uint64_t _tmp$2191 = _tmp$2192 % 10000ull;
      int32_t r$438 = (int32_t)_tmp$2191;
      int32_t d1$439;
      int32_t d2$440;
      int32_t _tmp$2171;
      int32_t _tmp$2190;
      int32_t _tmp$2189;
      int32_t d1_hi$441;
      int32_t _tmp$2188;
      int32_t _tmp$2187;
      int32_t d1_lo$442;
      int32_t _tmp$2186;
      int32_t _tmp$2185;
      int32_t d2_hi$443;
      int32_t _tmp$2184;
      int32_t _tmp$2183;
      int32_t d2_lo$444;
      int32_t _tmp$2173;
      int32_t _tmp$2172;
      int32_t _tmp$2176;
      int32_t _tmp$2175;
      int32_t _tmp$2174;
      int32_t _tmp$2179;
      int32_t _tmp$2178;
      int32_t _tmp$2177;
      int32_t _tmp$2182;
      int32_t _tmp$2181;
      int32_t _tmp$2180;
      num$432 = t$437;
      d1$439 = r$438 / 100;
      d2$440 = r$438 % 100;
      _tmp$2171 = offset$434;
      offset$434 = _tmp$2171 - 4;
      _tmp$2190 = d1$439 / 10;
      _tmp$2189 = 48 + _tmp$2190;
      d1_hi$441 = (uint16_t)_tmp$2189;
      _tmp$2188 = d1$439 % 10;
      _tmp$2187 = 48 + _tmp$2188;
      d1_lo$442 = (uint16_t)_tmp$2187;
      _tmp$2186 = d2$440 / 10;
      _tmp$2185 = 48 + _tmp$2186;
      d2_hi$443 = (uint16_t)_tmp$2185;
      _tmp$2184 = d2$440 % 10;
      _tmp$2183 = 48 + _tmp$2184;
      d2_lo$444 = (uint16_t)_tmp$2183;
      _tmp$2173 = offset$434;
      _tmp$2172 = digit_start$436 + _tmp$2173;
      buffer$445[_tmp$2172] = d1_hi$441;
      _tmp$2176 = offset$434;
      _tmp$2175 = digit_start$436 + _tmp$2176;
      _tmp$2174 = _tmp$2175 + 1;
      buffer$445[_tmp$2174] = d1_lo$442;
      _tmp$2179 = offset$434;
      _tmp$2178 = digit_start$436 + _tmp$2179;
      _tmp$2177 = _tmp$2178 + 2;
      buffer$445[_tmp$2177] = d2_hi$443;
      _tmp$2182 = offset$434;
      _tmp$2181 = digit_start$436 + _tmp$2182;
      _tmp$2180 = _tmp$2181 + 3;
      buffer$445[_tmp$2180] = d2_lo$444;
      continue;
    }
    break;
  }
  _tmp$2226 = num$432;
  remaining$447 = (int32_t)_tmp$2226;
  while (1) {
    int32_t _tmp$2194 = remaining$447;
    if (_tmp$2194 >= 100) {
      int32_t _tmp$2206 = remaining$447;
      int32_t t$448 = _tmp$2206 / 100;
      int32_t _tmp$2205 = remaining$447;
      int32_t d$449 = _tmp$2205 % 100;
      int32_t _tmp$2195;
      int32_t _tmp$2204;
      int32_t _tmp$2203;
      int32_t d_hi$450;
      int32_t _tmp$2202;
      int32_t _tmp$2201;
      int32_t d_lo$451;
      int32_t _tmp$2197;
      int32_t _tmp$2196;
      int32_t _tmp$2200;
      int32_t _tmp$2199;
      int32_t _tmp$2198;
      remaining$447 = t$448;
      _tmp$2195 = offset$434;
      offset$434 = _tmp$2195 - 2;
      _tmp$2204 = d$449 / 10;
      _tmp$2203 = 48 + _tmp$2204;
      d_hi$450 = (uint16_t)_tmp$2203;
      _tmp$2202 = d$449 % 10;
      _tmp$2201 = 48 + _tmp$2202;
      d_lo$451 = (uint16_t)_tmp$2201;
      _tmp$2197 = offset$434;
      _tmp$2196 = digit_start$436 + _tmp$2197;
      buffer$445[_tmp$2196] = d_hi$450;
      _tmp$2200 = offset$434;
      _tmp$2199 = digit_start$436 + _tmp$2200;
      _tmp$2198 = _tmp$2199 + 1;
      buffer$445[_tmp$2198] = d_lo$451;
      continue;
    }
    break;
  }
  _tmp$2207 = remaining$447;
  if (_tmp$2207 >= 10) {
    int32_t _tmp$2208 = offset$434;
    int32_t _tmp$2219;
    int32_t _tmp$2218;
    int32_t _tmp$2217;
    int32_t d_hi$453;
    int32_t _tmp$2216;
    int32_t _tmp$2215;
    int32_t _tmp$2214;
    int32_t d_lo$454;
    int32_t _tmp$2210;
    int32_t _tmp$2209;
    int32_t _tmp$2213;
    int32_t _tmp$2212;
    int32_t _tmp$2211;
    offset$434 = _tmp$2208 - 2;
    _tmp$2219 = remaining$447;
    _tmp$2218 = _tmp$2219 / 10;
    _tmp$2217 = 48 + _tmp$2218;
    d_hi$453 = (uint16_t)_tmp$2217;
    _tmp$2216 = remaining$447;
    _tmp$2215 = _tmp$2216 % 10;
    _tmp$2214 = 48 + _tmp$2215;
    d_lo$454 = (uint16_t)_tmp$2214;
    _tmp$2210 = offset$434;
    _tmp$2209 = digit_start$436 + _tmp$2210;
    buffer$445[_tmp$2209] = d_hi$453;
    _tmp$2213 = offset$434;
    _tmp$2212 = digit_start$436 + _tmp$2213;
    _tmp$2211 = _tmp$2212 + 1;
    buffer$445[_tmp$2211] = d_lo$454;
    moonbit_decref(buffer$445);
  } else {
    int32_t _tmp$2220 = offset$434;
    int32_t _tmp$2225;
    int32_t _tmp$2221;
    int32_t _tmp$2224;
    int32_t _tmp$2223;
    int32_t _tmp$2222;
    offset$434 = _tmp$2220 - 1;
    _tmp$2225 = offset$434;
    _tmp$2221 = digit_start$436 + _tmp$2225;
    _tmp$2224 = remaining$447;
    _tmp$2223 = 48 + _tmp$2224;
    _tmp$2222 = (uint16_t)_tmp$2223;
    buffer$445[_tmp$2221] = _tmp$2222;
    moonbit_decref(buffer$445);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_generic(
  uint16_t* buffer$427,
  uint64_t num$421,
  int32_t digit_start$419,
  int32_t total_len$418,
  int32_t radix$423
) {
  int32_t offset$417 = total_len$418 - digit_start$419;
  uint64_t n$420 = num$421;
  uint64_t base$422 = $Int$$to_uint64(radix$423);
  int32_t _tmp$2150 = radix$423 - 1;
  int32_t _tmp$2149 = radix$423 & _tmp$2150;
  if (_tmp$2149 == 0) {
    int32_t shift$424 = moonbit_ctz32(radix$423);
    uint64_t mask$425 = base$422 - 1ull;
    while (1) {
      uint64_t _tmp$2151 = n$420;
      if (_tmp$2151 > 0ull) {
        int32_t _tmp$2152 = offset$417;
        uint64_t _tmp$2159;
        uint64_t _tmp$2158;
        int32_t digit$426;
        int32_t _tmp$2156;
        int32_t _tmp$2153;
        int32_t _tmp$2155;
        int32_t _tmp$2154;
        uint64_t _tmp$2157;
        offset$417 = _tmp$2152 - 1;
        _tmp$2159 = n$420;
        _tmp$2158 = _tmp$2159 & mask$425;
        digit$426 = (int32_t)_tmp$2158;
        _tmp$2156 = offset$417;
        _tmp$2153 = digit_start$419 + _tmp$2156;
        _tmp$2155
        = ((moonbit_string_t)moonbit_string_literal_262.data)[
          digit$426
        ];
        _tmp$2154 = (uint16_t)_tmp$2155;
        buffer$427[_tmp$2153] = _tmp$2154;
        _tmp$2157 = n$420;
        n$420 = _tmp$2157 >> (shift$424 & 63);
        continue;
      } else {
        moonbit_decref(buffer$427);
      }
      break;
    }
  } else {
    while (1) {
      uint64_t _tmp$2160 = n$420;
      if (_tmp$2160 > 0ull) {
        int32_t _tmp$2161 = offset$417;
        uint64_t _tmp$2169;
        uint64_t q$429;
        uint64_t _tmp$2167;
        uint64_t _tmp$2168;
        uint64_t _tmp$2166;
        int32_t digit$430;
        int32_t _tmp$2165;
        int32_t _tmp$2162;
        int32_t _tmp$2164;
        int32_t _tmp$2163;
        offset$417 = _tmp$2161 - 1;
        _tmp$2169 = n$420;
        q$429 = _tmp$2169 / base$422;
        _tmp$2167 = n$420;
        _tmp$2168 = q$429 * base$422;
        _tmp$2166 = _tmp$2167 - _tmp$2168;
        digit$430 = (int32_t)_tmp$2166;
        _tmp$2165 = offset$417;
        _tmp$2162 = digit_start$419 + _tmp$2165;
        _tmp$2164
        = ((moonbit_string_t)moonbit_string_literal_262.data)[
          digit$430
        ];
        _tmp$2163 = (uint16_t)_tmp$2164;
        buffer$427[_tmp$2162] = _tmp$2163;
        n$420 = q$429;
        continue;
      } else {
        moonbit_decref(buffer$427);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int64_to_string_hex(
  uint16_t* buffer$414,
  uint64_t num$410,
  int32_t digit_start$408,
  int32_t total_len$407
) {
  int32_t offset$406 = total_len$407 - digit_start$408;
  uint64_t n$409 = num$410;
  int32_t _tmp$2144;
  while (1) {
    int32_t _tmp$2130 = offset$406;
    if (_tmp$2130 >= 2) {
      int32_t _tmp$2131 = offset$406;
      uint64_t _tmp$2143;
      uint64_t _tmp$2142;
      int32_t byte_val$411;
      int32_t hi$412;
      int32_t lo$413;
      int32_t _tmp$2135;
      int32_t _tmp$2132;
      int32_t _tmp$2134;
      int32_t _tmp$2133;
      int32_t _tmp$2140;
      int32_t _tmp$2139;
      int32_t _tmp$2136;
      int32_t _tmp$2138;
      int32_t _tmp$2137;
      uint64_t _tmp$2141;
      offset$406 = _tmp$2131 - 2;
      _tmp$2143 = n$409;
      _tmp$2142 = _tmp$2143 & 255ull;
      byte_val$411 = (int32_t)_tmp$2142;
      hi$412 = byte_val$411 / 16;
      lo$413 = byte_val$411 % 16;
      _tmp$2135 = offset$406;
      _tmp$2132 = digit_start$408 + _tmp$2135;
      _tmp$2134 = ((moonbit_string_t)moonbit_string_literal_262.data)[hi$412];
      _tmp$2133 = (uint16_t)_tmp$2134;
      buffer$414[_tmp$2132] = _tmp$2133;
      _tmp$2140 = offset$406;
      _tmp$2139 = digit_start$408 + _tmp$2140;
      _tmp$2136 = _tmp$2139 + 1;
      _tmp$2138 = ((moonbit_string_t)moonbit_string_literal_262.data)[lo$413];
      _tmp$2137 = (uint16_t)_tmp$2138;
      buffer$414[_tmp$2136] = _tmp$2137;
      _tmp$2141 = n$409;
      n$409 = _tmp$2141 >> 8;
      continue;
    }
    break;
  }
  _tmp$2144 = offset$406;
  if (_tmp$2144 == 1) {
    uint64_t _tmp$2148 = n$409;
    uint64_t _tmp$2147 = _tmp$2148 & 15ull;
    int32_t nibble$416 = (int32_t)_tmp$2147;
    int32_t _tmp$2146 =
      ((moonbit_string_t)moonbit_string_literal_262.data)[nibble$416];
    int32_t _tmp$2145 = (uint16_t)_tmp$2146;
    buffer$414[digit_start$408] = _tmp$2145;
    moonbit_decref(buffer$414);
  } else {
    moonbit_decref(buffer$414);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$radix_count64(
  uint64_t value$400,
  int32_t radix$403
) {
  uint64_t num$401;
  uint64_t base$402;
  int32_t count$404;
  if (value$400 == 0ull) {
    return 1;
  }
  num$401 = value$400;
  base$402 = $Int$$to_uint64(radix$403);
  count$404 = 0;
  while (1) {
    uint64_t _tmp$2127 = num$401;
    if (_tmp$2127 > 0ull) {
      int32_t _tmp$2128 = count$404;
      uint64_t _tmp$2129;
      count$404 = _tmp$2128 + 1;
      _tmp$2129 = num$401;
      num$401 = _tmp$2129 / base$402;
      continue;
    }
    break;
  }
  return count$404;
}

int32_t $moonbitlang$core$builtin$hex_count64(uint64_t value$398) {
  if (value$398 == 0ull) {
    return 1;
  } else {
    int32_t leading_zeros$399 = moonbit_clz64(value$398);
    int32_t _tmp$2126 = 63 - leading_zeros$399;
    int32_t _tmp$2125 = _tmp$2126 / 4;
    return _tmp$2125 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count64(uint64_t value$397) {
  if (value$397 >= 10000000000ull) {
    if (value$397 >= 100000000000000ull) {
      if (value$397 >= 10000000000000000ull) {
        if (value$397 >= 1000000000000000000ull) {
          if (value$397 >= 10000000000000000000ull) {
            return 20;
          } else {
            return 19;
          }
        } else if (value$397 >= 100000000000000000ull) {
          return 18;
        } else {
          return 17;
        }
      } else if (value$397 >= 1000000000000000ull) {
        return 16;
      } else {
        return 15;
      }
    } else if (value$397 >= 1000000000000ull) {
      if (value$397 >= 10000000000000ull) {
        return 14;
      } else {
        return 13;
      }
    } else if (value$397 >= 100000000000ull) {
      return 12;
    } else {
      return 11;
    }
  } else if (value$397 >= 100000ull) {
    if (value$397 >= 10000000ull) {
      if (value$397 >= 1000000000ull) {
        return 10;
      } else if (value$397 >= 100000000ull) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$397 >= 1000000ull) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$397 >= 1000ull) {
    if (value$397 >= 10000ull) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$397 >= 100ull) {
    return 3;
  } else if (value$397 >= 10ull) {
    return 2;
  } else {
    return 1;
  }
}

moonbit_string_t $Int$$to_string$inner(int32_t self$381, int32_t radix$380) {
  int32_t _if_result$5037;
  int32_t is_negative$382;
  uint32_t num$383;
  uint16_t* buffer$384;
  if (radix$380 < 2) {
    _if_result$5037 = 1;
  } else {
    _if_result$5037 = radix$380 > 36;
  }
  if (_if_result$5037) {
    $moonbitlang$core$builtin$abort$0(
      (moonbit_string_t)moonbit_string_literal_260.data,
        (moonbit_string_t)moonbit_string_literal_263.data
    );
  }
  if (self$381 == 0) {
    return (moonbit_string_t)moonbit_string_literal_237.data;
  }
  is_negative$382 = self$381 < 0;
  if (is_negative$382) {
    int32_t _tmp$2124 = -self$381;
    num$383 = *(uint32_t*)&_tmp$2124;
  } else {
    num$383 = *(uint32_t*)&self$381;
  }
  switch (radix$380) {
    case 10: {
      int32_t digit_len$385 = $moonbitlang$core$builtin$dec_count32(num$383);
      int32_t _tmp$2121;
      int32_t total_len$386;
      uint16_t* buffer$387;
      int32_t digit_start$388;
      if (is_negative$382) {
        _tmp$2121 = 1;
      } else {
        _tmp$2121 = 0;
      }
      total_len$386 = digit_len$385 + _tmp$2121;
      buffer$387 = (uint16_t*)moonbit_make_string(total_len$386, 0);
      if (is_negative$382) {
        digit_start$388 = 1;
      } else {
        digit_start$388 = 0;
      }
      moonbit_incref(buffer$387);
      $moonbitlang$core$builtin$int_to_string_dec(
        buffer$387, num$383, digit_start$388, total_len$386
      );
      buffer$384 = buffer$387;
      break;
    }
    
    case 16: {
      int32_t digit_len$389 = $moonbitlang$core$builtin$hex_count32(num$383);
      int32_t _tmp$2122;
      int32_t total_len$390;
      uint16_t* buffer$391;
      int32_t digit_start$392;
      if (is_negative$382) {
        _tmp$2122 = 1;
      } else {
        _tmp$2122 = 0;
      }
      total_len$390 = digit_len$389 + _tmp$2122;
      buffer$391 = (uint16_t*)moonbit_make_string(total_len$390, 0);
      if (is_negative$382) {
        digit_start$392 = 1;
      } else {
        digit_start$392 = 0;
      }
      moonbit_incref(buffer$391);
      $moonbitlang$core$builtin$int_to_string_hex(
        buffer$391, num$383, digit_start$392, total_len$390
      );
      buffer$384 = buffer$391;
      break;
    }
    default: {
      int32_t digit_len$393 =
        $moonbitlang$core$builtin$radix_count32(num$383, radix$380);
      int32_t _tmp$2123;
      int32_t total_len$394;
      uint16_t* buffer$395;
      int32_t digit_start$396;
      if (is_negative$382) {
        _tmp$2123 = 1;
      } else {
        _tmp$2123 = 0;
      }
      total_len$394 = digit_len$393 + _tmp$2123;
      buffer$395 = (uint16_t*)moonbit_make_string(total_len$394, 0);
      if (is_negative$382) {
        digit_start$396 = 1;
      } else {
        digit_start$396 = 0;
      }
      moonbit_incref(buffer$395);
      $moonbitlang$core$builtin$int_to_string_generic(
        buffer$395, num$383, digit_start$396, total_len$394, radix$380
      );
      buffer$384 = buffer$395;
      break;
    }
  }
  if (is_negative$382) {
    buffer$384[0] = 45;
  }
  return buffer$384;
}

int32_t $moonbitlang$core$builtin$radix_count32(
  uint32_t value$374,
  int32_t radix$377
) {
  uint32_t num$375;
  uint32_t base$376;
  int32_t count$378;
  if (value$374 == 0u) {
    return 1;
  }
  num$375 = value$374;
  base$376 = *(uint32_t*)&radix$377;
  count$378 = 0;
  while (1) {
    uint32_t _tmp$2118 = num$375;
    if (_tmp$2118 > 0u) {
      int32_t _tmp$2119 = count$378;
      uint32_t _tmp$2120;
      count$378 = _tmp$2119 + 1;
      _tmp$2120 = num$375;
      num$375 = _tmp$2120 / base$376;
      continue;
    }
    break;
  }
  return count$378;
}

int32_t $moonbitlang$core$builtin$hex_count32(uint32_t value$372) {
  if (value$372 == 0u) {
    return 1;
  } else {
    int32_t leading_zeros$373 = moonbit_clz32(value$372);
    int32_t _tmp$2117 = 31 - leading_zeros$373;
    int32_t _tmp$2116 = _tmp$2117 / 4;
    return _tmp$2116 + 1;
  }
}

int32_t $moonbitlang$core$builtin$dec_count32(uint32_t value$371) {
  if (value$371 >= 100000u) {
    if (value$371 >= 10000000u) {
      if (value$371 >= 1000000000u) {
        return 10;
      } else if (value$371 >= 100000000u) {
        return 9;
      } else {
        return 8;
      }
    } else if (value$371 >= 1000000u) {
      return 7;
    } else {
      return 6;
    }
  } else if (value$371 >= 1000u) {
    if (value$371 >= 10000u) {
      return 5;
    } else {
      return 4;
    }
  } else if (value$371 >= 100u) {
    return 3;
  } else if (value$371 >= 10u) {
    return 2;
  } else {
    return 1;
  }
}

int32_t $moonbitlang$core$builtin$int_to_string_dec(
  uint16_t* buffer$361,
  uint32_t num$349,
  int32_t digit_start$352,
  int32_t total_len$351
) {
  uint32_t num$348 = num$349;
  int32_t offset$350 = total_len$351 - digit_start$352;
  uint32_t _tmp$2115;
  int32_t remaining$363;
  int32_t _tmp$2096;
  while (1) {
    uint32_t _tmp$2059 = num$348;
    if (_tmp$2059 >= 10000u) {
      uint32_t _tmp$2082 = num$348;
      uint32_t t$353 = _tmp$2082 / 10000u;
      uint32_t _tmp$2081 = num$348;
      uint32_t _tmp$2080 = _tmp$2081 % 10000u;
      int32_t r$354 = *(int32_t*)&_tmp$2080;
      int32_t d1$355;
      int32_t d2$356;
      int32_t _tmp$2060;
      int32_t _tmp$2079;
      int32_t _tmp$2078;
      int32_t d1_hi$357;
      int32_t _tmp$2077;
      int32_t _tmp$2076;
      int32_t d1_lo$358;
      int32_t _tmp$2075;
      int32_t _tmp$2074;
      int32_t d2_hi$359;
      int32_t _tmp$2073;
      int32_t _tmp$2072;
      int32_t d2_lo$360;
      int32_t _tmp$2062;
      int32_t _tmp$2061;
      int32_t _tmp$2065;
      int32_t _tmp$2064;
      int32_t _tmp$2063;
      int32_t _tmp$2068;
      int32_t _tmp$2067;
      int32_t _tmp$2066;
      int32_t _tmp$2071;
      int32_t _tmp$2070;
      int32_t _tmp$2069;
      num$348 = t$353;
      d1$355 = r$354 / 100;
      d2$356 = r$354 % 100;
      _tmp$2060 = offset$350;
      offset$350 = _tmp$2060 - 4;
      _tmp$2079 = d1$355 / 10;
      _tmp$2078 = 48 + _tmp$2079;
      d1_hi$357 = (uint16_t)_tmp$2078;
      _tmp$2077 = d1$355 % 10;
      _tmp$2076 = 48 + _tmp$2077;
      d1_lo$358 = (uint16_t)_tmp$2076;
      _tmp$2075 = d2$356 / 10;
      _tmp$2074 = 48 + _tmp$2075;
      d2_hi$359 = (uint16_t)_tmp$2074;
      _tmp$2073 = d2$356 % 10;
      _tmp$2072 = 48 + _tmp$2073;
      d2_lo$360 = (uint16_t)_tmp$2072;
      _tmp$2062 = offset$350;
      _tmp$2061 = digit_start$352 + _tmp$2062;
      buffer$361[_tmp$2061] = d1_hi$357;
      _tmp$2065 = offset$350;
      _tmp$2064 = digit_start$352 + _tmp$2065;
      _tmp$2063 = _tmp$2064 + 1;
      buffer$361[_tmp$2063] = d1_lo$358;
      _tmp$2068 = offset$350;
      _tmp$2067 = digit_start$352 + _tmp$2068;
      _tmp$2066 = _tmp$2067 + 2;
      buffer$361[_tmp$2066] = d2_hi$359;
      _tmp$2071 = offset$350;
      _tmp$2070 = digit_start$352 + _tmp$2071;
      _tmp$2069 = _tmp$2070 + 3;
      buffer$361[_tmp$2069] = d2_lo$360;
      continue;
    }
    break;
  }
  _tmp$2115 = num$348;
  remaining$363 = *(int32_t*)&_tmp$2115;
  while (1) {
    int32_t _tmp$2083 = remaining$363;
    if (_tmp$2083 >= 100) {
      int32_t _tmp$2095 = remaining$363;
      int32_t t$364 = _tmp$2095 / 100;
      int32_t _tmp$2094 = remaining$363;
      int32_t d$365 = _tmp$2094 % 100;
      int32_t _tmp$2084;
      int32_t _tmp$2093;
      int32_t _tmp$2092;
      int32_t d_hi$366;
      int32_t _tmp$2091;
      int32_t _tmp$2090;
      int32_t d_lo$367;
      int32_t _tmp$2086;
      int32_t _tmp$2085;
      int32_t _tmp$2089;
      int32_t _tmp$2088;
      int32_t _tmp$2087;
      remaining$363 = t$364;
      _tmp$2084 = offset$350;
      offset$350 = _tmp$2084 - 2;
      _tmp$2093 = d$365 / 10;
      _tmp$2092 = 48 + _tmp$2093;
      d_hi$366 = (uint16_t)_tmp$2092;
      _tmp$2091 = d$365 % 10;
      _tmp$2090 = 48 + _tmp$2091;
      d_lo$367 = (uint16_t)_tmp$2090;
      _tmp$2086 = offset$350;
      _tmp$2085 = digit_start$352 + _tmp$2086;
      buffer$361[_tmp$2085] = d_hi$366;
      _tmp$2089 = offset$350;
      _tmp$2088 = digit_start$352 + _tmp$2089;
      _tmp$2087 = _tmp$2088 + 1;
      buffer$361[_tmp$2087] = d_lo$367;
      continue;
    }
    break;
  }
  _tmp$2096 = remaining$363;
  if (_tmp$2096 >= 10) {
    int32_t _tmp$2097 = offset$350;
    int32_t _tmp$2108;
    int32_t _tmp$2107;
    int32_t _tmp$2106;
    int32_t d_hi$369;
    int32_t _tmp$2105;
    int32_t _tmp$2104;
    int32_t _tmp$2103;
    int32_t d_lo$370;
    int32_t _tmp$2099;
    int32_t _tmp$2098;
    int32_t _tmp$2102;
    int32_t _tmp$2101;
    int32_t _tmp$2100;
    offset$350 = _tmp$2097 - 2;
    _tmp$2108 = remaining$363;
    _tmp$2107 = _tmp$2108 / 10;
    _tmp$2106 = 48 + _tmp$2107;
    d_hi$369 = (uint16_t)_tmp$2106;
    _tmp$2105 = remaining$363;
    _tmp$2104 = _tmp$2105 % 10;
    _tmp$2103 = 48 + _tmp$2104;
    d_lo$370 = (uint16_t)_tmp$2103;
    _tmp$2099 = offset$350;
    _tmp$2098 = digit_start$352 + _tmp$2099;
    buffer$361[_tmp$2098] = d_hi$369;
    _tmp$2102 = offset$350;
    _tmp$2101 = digit_start$352 + _tmp$2102;
    _tmp$2100 = _tmp$2101 + 1;
    buffer$361[_tmp$2100] = d_lo$370;
    moonbit_decref(buffer$361);
  } else {
    int32_t _tmp$2109 = offset$350;
    int32_t _tmp$2114;
    int32_t _tmp$2110;
    int32_t _tmp$2113;
    int32_t _tmp$2112;
    int32_t _tmp$2111;
    offset$350 = _tmp$2109 - 1;
    _tmp$2114 = offset$350;
    _tmp$2110 = digit_start$352 + _tmp$2114;
    _tmp$2113 = remaining$363;
    _tmp$2112 = 48 + _tmp$2113;
    _tmp$2111 = (uint16_t)_tmp$2112;
    buffer$361[_tmp$2110] = _tmp$2111;
    moonbit_decref(buffer$361);
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_generic(
  uint16_t* buffer$343,
  uint32_t num$337,
  int32_t digit_start$335,
  int32_t total_len$334,
  int32_t radix$339
) {
  int32_t offset$333 = total_len$334 - digit_start$335;
  uint32_t n$336 = num$337;
  uint32_t base$338 = *(uint32_t*)&radix$339;
  int32_t _tmp$2039 = radix$339 - 1;
  int32_t _tmp$2038 = radix$339 & _tmp$2039;
  if (_tmp$2038 == 0) {
    int32_t shift$340 = moonbit_ctz32(radix$339);
    uint32_t mask$341 = base$338 - 1u;
    while (1) {
      uint32_t _tmp$2040 = n$336;
      if (_tmp$2040 > 0u) {
        int32_t _tmp$2041 = offset$333;
        uint32_t _tmp$2048;
        uint32_t _tmp$2047;
        int32_t digit$342;
        int32_t _tmp$2045;
        int32_t _tmp$2042;
        int32_t _tmp$2044;
        int32_t _tmp$2043;
        uint32_t _tmp$2046;
        offset$333 = _tmp$2041 - 1;
        _tmp$2048 = n$336;
        _tmp$2047 = _tmp$2048 & mask$341;
        digit$342 = *(int32_t*)&_tmp$2047;
        _tmp$2045 = offset$333;
        _tmp$2042 = digit_start$335 + _tmp$2045;
        _tmp$2044
        = ((moonbit_string_t)moonbit_string_literal_262.data)[
          digit$342
        ];
        _tmp$2043 = (uint16_t)_tmp$2044;
        buffer$343[_tmp$2042] = _tmp$2043;
        _tmp$2046 = n$336;
        n$336 = _tmp$2046 >> (shift$340 & 31);
        continue;
      } else {
        moonbit_decref(buffer$343);
      }
      break;
    }
  } else {
    while (1) {
      uint32_t _tmp$2049 = n$336;
      if (_tmp$2049 > 0u) {
        int32_t _tmp$2050 = offset$333;
        uint32_t _tmp$2058;
        uint32_t q$345;
        uint32_t _tmp$2056;
        uint32_t _tmp$2057;
        uint32_t _tmp$2055;
        int32_t digit$346;
        int32_t _tmp$2054;
        int32_t _tmp$2051;
        int32_t _tmp$2053;
        int32_t _tmp$2052;
        offset$333 = _tmp$2050 - 1;
        _tmp$2058 = n$336;
        q$345 = _tmp$2058 / base$338;
        _tmp$2056 = n$336;
        _tmp$2057 = q$345 * base$338;
        _tmp$2055 = _tmp$2056 - _tmp$2057;
        digit$346 = *(int32_t*)&_tmp$2055;
        _tmp$2054 = offset$333;
        _tmp$2051 = digit_start$335 + _tmp$2054;
        _tmp$2053
        = ((moonbit_string_t)moonbit_string_literal_262.data)[
          digit$346
        ];
        _tmp$2052 = (uint16_t)_tmp$2053;
        buffer$343[_tmp$2051] = _tmp$2052;
        n$336 = q$345;
        continue;
      } else {
        moonbit_decref(buffer$343);
      }
      break;
    }
  }
  return 0;
}

int32_t $moonbitlang$core$builtin$int_to_string_hex(
  uint16_t* buffer$330,
  uint32_t num$326,
  int32_t digit_start$324,
  int32_t total_len$323
) {
  int32_t offset$322 = total_len$323 - digit_start$324;
  uint32_t n$325 = num$326;
  int32_t _tmp$2033;
  while (1) {
    int32_t _tmp$2019 = offset$322;
    if (_tmp$2019 >= 2) {
      int32_t _tmp$2020 = offset$322;
      uint32_t _tmp$2032;
      uint32_t _tmp$2031;
      int32_t byte_val$327;
      int32_t hi$328;
      int32_t lo$329;
      int32_t _tmp$2024;
      int32_t _tmp$2021;
      int32_t _tmp$2023;
      int32_t _tmp$2022;
      int32_t _tmp$2029;
      int32_t _tmp$2028;
      int32_t _tmp$2025;
      int32_t _tmp$2027;
      int32_t _tmp$2026;
      uint32_t _tmp$2030;
      offset$322 = _tmp$2020 - 2;
      _tmp$2032 = n$325;
      _tmp$2031 = _tmp$2032 & 255u;
      byte_val$327 = *(int32_t*)&_tmp$2031;
      hi$328 = byte_val$327 / 16;
      lo$329 = byte_val$327 % 16;
      _tmp$2024 = offset$322;
      _tmp$2021 = digit_start$324 + _tmp$2024;
      _tmp$2023 = ((moonbit_string_t)moonbit_string_literal_262.data)[hi$328];
      _tmp$2022 = (uint16_t)_tmp$2023;
      buffer$330[_tmp$2021] = _tmp$2022;
      _tmp$2029 = offset$322;
      _tmp$2028 = digit_start$324 + _tmp$2029;
      _tmp$2025 = _tmp$2028 + 1;
      _tmp$2027 = ((moonbit_string_t)moonbit_string_literal_262.data)[lo$329];
      _tmp$2026 = (uint16_t)_tmp$2027;
      buffer$330[_tmp$2025] = _tmp$2026;
      _tmp$2030 = n$325;
      n$325 = _tmp$2030 >> 8;
      continue;
    }
    break;
  }
  _tmp$2033 = offset$322;
  if (_tmp$2033 == 1) {
    uint32_t _tmp$2037 = n$325;
    uint32_t _tmp$2036 = _tmp$2037 & 15u;
    int32_t nibble$332 = *(int32_t*)&_tmp$2036;
    int32_t _tmp$2035 =
      ((moonbit_string_t)moonbit_string_literal_262.data)[nibble$332];
    int32_t _tmp$2034 = (uint16_t)_tmp$2035;
    buffer$330[digit_start$324] = _tmp$2034;
    moonbit_decref(buffer$330);
  } else {
    moonbit_decref(buffer$330);
  }
  return 0;
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
  void* self$321
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$320 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$2018;
  moonbit_incref(logger$320);
  _tmp$2018
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$320
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$CreatingViewError$$output(
    self$321, _tmp$2018
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$320);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
  void* self$319
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$318 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$2017;
  moonbit_incref(logger$318);
  _tmp$2017
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$318
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
    self$319, _tmp$2017
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$318);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$3(
  uint64_t self$317
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$316 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$2016;
  moonbit_incref(logger$316);
  _tmp$2016
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$316
  };
  $$moonbitlang$core$builtin$Show$$UInt64$$output(self$317, _tmp$2016);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$316);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$2(
  int32_t self$315
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$314 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$2015;
  moonbit_incref(logger$314);
  _tmp$2015
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$314
  };
  $$moonbitlang$core$builtin$Show$$Bool$$output(self$315, _tmp$2015);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$314);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(
  moonbit_string_t self$313
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$312 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$2014;
  moonbit_incref(logger$312);
  _tmp$2014
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$312
  };
  $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$SourceLoc$$output(
    self$313, _tmp$2014
  );
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$312);
}

moonbit_string_t $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$0(
  int32_t self$311
) {
  struct $$moonbitlang$core$builtin$StringBuilder* logger$310 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$2013;
  moonbit_incref(logger$310);
  _tmp$2013
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      logger$310
  };
  $$moonbitlang$core$builtin$Show$$Int$$output(self$311, _tmp$2013);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(logger$310);
}

int32_t $StringView$$start_offset(struct $StringView self$309) {
  int32_t _field$4378 = self$309.$1;
  moonbit_decref(self$309.$0);
  return _field$4378;
}

moonbit_string_t $StringView$$data(struct $StringView self$308) {
  moonbit_string_t _field$4379 = self$308.$0;
  return _field$4379;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
  struct $$moonbitlang$core$builtin$StringBuilder* self$302,
  moonbit_string_t value$305,
  int32_t start$306,
  int32_t len$307
) {
  void* _try_err$304;
  struct $StringView _tmp$2008;
  int32_t _tmp$2010 = start$306 + len$307;
  int64_t _tmp$2009 = (int64_t)_tmp$2010;
  struct moonbit_result_1 _tmp$5045 =
    $String$$sub$inner(value$305, start$306, _tmp$2009);
  if (_tmp$5045.tag) {
    struct $StringView const _ok$2011 = _tmp$5045.data.ok;
    _tmp$2008 = _ok$2011;
  } else {
    void* const _err$2012 = _tmp$5045.data.err;
    _try_err$304 = _err$2012;
    goto $join$303;
  }
  goto $joinlet$5044;
  $join$303:;
  moonbit_decref(_try_err$304);
  moonbit_panic();
  $joinlet$5044:;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    self$302, _tmp$2008
  );
  return 0;
}

struct moonbit_result_1 $String$$sub$inner(
  moonbit_string_t self$295,
  int32_t start$301,
  int64_t end$297
) {
  int32_t len$294 = Moonbit_array_length(self$295);
  int32_t end$296;
  int32_t start$300;
  int32_t _if_result$5046;
  if (end$297 == 4294967296ll) {
    end$296 = len$294;
  } else {
    int64_t _Some$298 = end$297;
    int32_t _end$299 = (int32_t)_Some$298;
    if (_end$299 < 0) {
      end$296 = len$294 + _end$299;
    } else {
      end$296 = _end$299;
    }
  }
  if (start$301 < 0) {
    start$300 = len$294 + start$301;
  } else {
    start$300 = start$301;
  }
  if (start$300 >= 0) {
    if (start$300 <= end$296) {
      _if_result$5046 = end$296 <= len$294;
    } else {
      _if_result$5046 = 0;
    }
  } else {
    _if_result$5046 = 0;
  }
  if (_if_result$5046) {
    int32_t _if_result$5047;
    int32_t _if_result$5049;
    struct $StringView _tmp$2006;
    struct moonbit_result_1 _result$5051;
    if (start$300 < len$294) {
      int32_t _tmp$2002 = self$295[start$300];
      _if_result$5047 = $Int$$is_trailing_surrogate(_tmp$2002);
    } else {
      _if_result$5047 = 0;
    }
    if (_if_result$5047) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$2003;
      struct moonbit_result_1 _result$5048;
      moonbit_decref(self$295);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$2003
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$5048.tag = 0;
      _result$5048.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$2003;
      return _result$5048;
    }
    if (end$296 < len$294) {
      int32_t _tmp$2004 = self$295[end$296];
      _if_result$5049 = $Int$$is_trailing_surrogate(_tmp$2004);
    } else {
      _if_result$5049 = 0;
    }
    if (_if_result$5049) {
      void* moonbitlang$core$builtin$CreatingViewError$InvalidIndex$2005;
      struct moonbit_result_1 _result$5050;
      moonbit_decref(self$295);
      moonbitlang$core$builtin$CreatingViewError$InvalidIndex$2005
      = (struct moonbit_object*)&moonbit_constant_constructor_0 + 1;
      _result$5050.tag = 0;
      _result$5050.data.err
      = moonbitlang$core$builtin$CreatingViewError$InvalidIndex$2005;
      return _result$5050;
    }
    _tmp$2006 = (struct $StringView){start$300, end$296, self$295};
    _result$5051.tag = 1;
    _result$5051.data.ok = _tmp$2006;
    return _result$5051;
  } else {
    void* moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$2007;
    struct moonbit_result_1 _result$5052;
    moonbit_decref(self$295);
    moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$2007
    = (struct moonbit_object*)&moonbit_constant_constructor_1 + 1;
    _result$5052.tag = 0;
    _result$5052.data.err
    = moonbitlang$core$builtin$CreatingViewError$IndexOutOfBounds$2007;
    return _result$5052;
  }
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$1(
  int32_t self$293
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$292 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$2001;
  moonbit_incref(_self$292);
  $$moonbitlang$core$builtin$Hasher$$combine$1(_self$292, self$293);
  _tmp$2001 = _self$292;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$2001);
}

int32_t $$moonbitlang$core$builtin$Hash$$$default_impl$$hash$0(
  moonbit_string_t self$291
) {
  struct $$moonbitlang$core$builtin$Hasher* _self$290 =
    $$moonbitlang$core$builtin$Hasher$$new(4294967296ll);
  struct $$moonbitlang$core$builtin$Hasher* _tmp$2000;
  moonbit_incref(_self$290);
  $$moonbitlang$core$builtin$Hasher$$combine$0(_self$290, self$291);
  _tmp$2000 = _self$290;
  return $$moonbitlang$core$builtin$Hasher$$finalize(_tmp$2000);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new(
  int64_t seed$opt$288
) {
  int32_t seed$287;
  if (seed$opt$288 == 4294967296ll) {
    seed$287 = 0;
  } else {
    int64_t _Some$289 = seed$opt$288;
    seed$287 = (int32_t)_Some$289;
  }
  return $$moonbitlang$core$builtin$Hasher$$new$inner(seed$287);
}

struct $$moonbitlang$core$builtin$Hasher* $$moonbitlang$core$builtin$Hasher$$new$inner(
  int32_t seed$286
) {
  uint32_t _tmp$1999 = *(uint32_t*)&seed$286;
  uint32_t _tmp$1998 = _tmp$1999 + 374761393u;
  struct $$moonbitlang$core$builtin$Hasher* _block$5053 =
    (struct $$moonbitlang$core$builtin$Hasher*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Hasher)
    );
  Moonbit_object_header(_block$5053)->meta
  = Moonbit_make_regular_object_header(
    sizeof(struct $$moonbitlang$core$builtin$Hasher) >> 2, 0, 0
  );
  _block$5053->$0 = _tmp$1998;
  return _block$5053;
}

int32_t $$moonbitlang$core$builtin$Hasher$$finalize(
  struct $$moonbitlang$core$builtin$Hasher* self$285
) {
  uint32_t _tmp$1997 = $$moonbitlang$core$builtin$Hasher$$avalanche(self$285);
  return *(int32_t*)&_tmp$1997;
}

uint32_t $$moonbitlang$core$builtin$Hasher$$avalanche(
  struct $$moonbitlang$core$builtin$Hasher* self$284
) {
  uint32_t _field$4380 = self$284->$0;
  uint32_t acc$283;
  uint32_t _tmp$1986;
  uint32_t _tmp$1988;
  uint32_t _tmp$1987;
  uint32_t _tmp$1989;
  uint32_t _tmp$1990;
  uint32_t _tmp$1992;
  uint32_t _tmp$1991;
  uint32_t _tmp$1993;
  uint32_t _tmp$1994;
  uint32_t _tmp$1996;
  uint32_t _tmp$1995;
  moonbit_decref(self$284);
  acc$283 = _field$4380;
  _tmp$1986 = acc$283;
  _tmp$1988 = acc$283;
  _tmp$1987 = _tmp$1988 >> 15;
  acc$283 = _tmp$1986 ^ _tmp$1987;
  _tmp$1989 = acc$283;
  acc$283 = _tmp$1989 * 2246822519u;
  _tmp$1990 = acc$283;
  _tmp$1992 = acc$283;
  _tmp$1991 = _tmp$1992 >> 13;
  acc$283 = _tmp$1990 ^ _tmp$1991;
  _tmp$1993 = acc$283;
  acc$283 = _tmp$1993 * 3266489917u;
  _tmp$1994 = acc$283;
  _tmp$1996 = acc$283;
  _tmp$1995 = _tmp$1996 >> 16;
  acc$283 = _tmp$1994 ^ _tmp$1995;
  return acc$283;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$1(
  struct $$moonbitlang$core$builtin$Hasher* self$282,
  int32_t value$281
) {
  $$moonbitlang$core$builtin$Hash$$Int$$hash_combine(value$281, self$282);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine$0(
  struct $$moonbitlang$core$builtin$Hasher* self$280,
  moonbit_string_t value$279
) {
  $$moonbitlang$core$builtin$Hash$$String$$hash_combine(value$279, self$280);
  return 0;
}

uint64_t $Int$$to_uint64(int32_t self$278) {
  int64_t _tmp$1985 = (int64_t)self$278;
  return *(uint64_t*)&_tmp$1985;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_int(
  struct $$moonbitlang$core$builtin$Hasher* self$276,
  int32_t value$277
) {
  uint32_t _tmp$1984 = *(uint32_t*)&value$277;
  $$moonbitlang$core$builtin$Hasher$$combine_uint(self$276, _tmp$1984);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$combine_uint(
  struct $$moonbitlang$core$builtin$Hasher* self$274,
  uint32_t value$275
) {
  uint32_t acc$1983 = self$274->$0;
  uint32_t _tmp$1982 = acc$1983 + 4u;
  self$274->$0 = _tmp$1982;
  $$moonbitlang$core$builtin$Hasher$$consume4(self$274, value$275);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Hasher$$consume4(
  struct $$moonbitlang$core$builtin$Hasher* self$272,
  uint32_t input$273
) {
  uint32_t acc$1980 = self$272->$0;
  uint32_t _tmp$1981 = input$273 * 3266489917u;
  uint32_t _tmp$1979 = acc$1980 + _tmp$1981;
  uint32_t _tmp$1978 = $moonbitlang$core$builtin$rotl(_tmp$1979, 17);
  uint32_t _tmp$1977 = _tmp$1978 * 668265263u;
  self$272->$0 = _tmp$1977;
  moonbit_decref(self$272);
  return 0;
}

uint32_t $moonbitlang$core$builtin$rotl(uint32_t x$270, int32_t r$271) {
  uint32_t _tmp$1974 = x$270 << (r$271 & 31);
  int32_t _tmp$1976 = 32 - r$271;
  uint32_t _tmp$1975 = x$270 >> (_tmp$1976 & 31);
  return _tmp$1974 | _tmp$1975;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$268,
  moonbit_string_t str$269
) {
  int32_t len$1964 = self$268->$1;
  int32_t _tmp$1966 = Moonbit_array_length(str$269);
  int32_t _tmp$1965 = _tmp$1966 * 2;
  int32_t _tmp$1963 = len$1964 + _tmp$1965;
  moonbit_bytes_t _field$4382;
  moonbit_bytes_t data$1967;
  int32_t len$1968;
  int32_t _tmp$1969;
  int32_t len$1971;
  int32_t _tmp$4381;
  int32_t _tmp$1973;
  int32_t _tmp$1972;
  int32_t _tmp$1970;
  moonbit_incref(self$268);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$268, _tmp$1963
  );
  _field$4382 = self$268->$0;
  data$1967 = _field$4382;
  len$1968 = self$268->$1;
  _tmp$1969 = Moonbit_array_length(str$269);
  moonbit_incref(data$1967);
  moonbit_incref(str$269);
  $FixedArray$$blit_from_string(data$1967, len$1968, str$269, 0, _tmp$1969);
  len$1971 = self$268->$1;
  _tmp$4381 = Moonbit_array_length(str$269);
  moonbit_decref(str$269);
  _tmp$1973 = _tmp$4381;
  _tmp$1972 = _tmp$1973 * 2;
  _tmp$1970 = len$1971 + _tmp$1972;
  self$268->$1 = _tmp$1970;
  moonbit_decref(self$268);
  return 0;
}

int32_t $FixedArray$$blit_from_string(
  moonbit_bytes_t self$260,
  int32_t bytes_offset$255,
  moonbit_string_t str$262,
  int32_t str_offset$258,
  int32_t length$256
) {
  int32_t _tmp$1962 = length$256 * 2;
  int32_t _tmp$1961 = bytes_offset$255 + _tmp$1962;
  int32_t e1$254 = _tmp$1961 - 1;
  int32_t _tmp$1960 = str_offset$258 + length$256;
  int32_t e2$257 = _tmp$1960 - 1;
  int32_t len1$259 = Moonbit_array_length(self$260);
  int32_t len2$261 = Moonbit_array_length(str$262);
  int32_t _if_result$5054;
  if (length$256 >= 0) {
    if (bytes_offset$255 >= 0) {
      if (e1$254 < len1$259) {
        if (str_offset$258 >= 0) {
          _if_result$5054 = e2$257 < len2$261;
        } else {
          _if_result$5054 = 0;
        }
      } else {
        _if_result$5054 = 0;
      }
    } else {
      _if_result$5054 = 0;
    }
  } else {
    _if_result$5054 = 0;
  }
  if (_if_result$5054) {
    int32_t end_str_offset$263 = str_offset$258 + length$256;
    int32_t i$264 = str_offset$258;
    int32_t j$265 = bytes_offset$255;
    while (1) {
      if (i$264 < end_str_offset$263) {
        int32_t _tmp$1957 = str$262[i$264];
        uint32_t c$266 = *(uint32_t*)&_tmp$1957;
        uint32_t _tmp$1953 = c$266 & 255u;
        int32_t _tmp$1952 = $UInt$$to_byte(_tmp$1953);
        int32_t _tmp$1954;
        uint32_t _tmp$1956;
        int32_t _tmp$1955;
        int32_t _tmp$1958;
        int32_t _tmp$1959;
        if (j$265 < 0 || j$265 >= Moonbit_array_length(self$260)) {
          moonbit_panic();
        }
        self$260[j$265] = _tmp$1952;
        _tmp$1954 = j$265 + 1;
        _tmp$1956 = c$266 >> 8;
        _tmp$1955 = $UInt$$to_byte(_tmp$1956);
        if (_tmp$1954 < 0 || _tmp$1954 >= Moonbit_array_length(self$260)) {
          moonbit_panic();
        }
        self$260[_tmp$1954] = _tmp$1955;
        _tmp$1958 = i$264 + 1;
        _tmp$1959 = j$265 + 2;
        i$264 = _tmp$1958;
        j$265 = _tmp$1959;
        continue;
      } else {
        moonbit_decref(str$262);
        moonbit_decref(self$260);
      }
      break;
    }
  } else {
    moonbit_decref(str$262);
    moonbit_decref(self$260);
    moonbit_panic();
  }
  return 0;
}

struct $$moonbitlang$core$builtin$SourceLocRepr* $$moonbitlang$core$builtin$SourceLocRepr$$parse(
  moonbit_string_t repr$222
) {
  int32_t _tmp$1925 = Moonbit_array_length(repr$222);
  int64_t _tmp$1924 = (int64_t)_tmp$1925;
  moonbit_incref(repr$222);
  if ($String$$char_length_ge$inner(repr$222, 1, 0, _tmp$1924)) {
    int32_t _tmp$1951 = repr$222[0];
    int32_t _x$223 = _tmp$1951;
    if (_x$223 == 64) {
      int32_t _tmp$1950 = Moonbit_array_length(repr$222);
      int64_t _tmp$1949 = (int64_t)_tmp$1950;
      int64_t _bind$1644;
      int32_t _tmp$1947;
      int32_t _tmp$1948;
      struct $StringView _x$224;
      int32_t _tmp$1946;
      struct $StringView _tmp$1945;
      int64_t _bind$226;
      moonbit_incref(repr$222);
      _bind$1644
      = $String$$offset_of_nth_char$inner(
        repr$222, 1, 0, _tmp$1949
      );
      if (_bind$1644 == 4294967296ll) {
        _tmp$1947 = Moonbit_array_length(repr$222);
      } else {
        int64_t _Some$225 = _bind$1644;
        _tmp$1947 = (int32_t)_Some$225;
      }
      _tmp$1948 = Moonbit_array_length(repr$222);
      _x$224 = (struct $StringView){_tmp$1947, _tmp$1948, repr$222};
      _tmp$1946
      = Moonbit_array_length(
        $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      );
      moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5443);
      _tmp$1945
      = (struct $StringView){
        0, _tmp$1946, $moonbitlang$core$builtin$parse$$2a$bind$7c$5443
      };
      moonbit_incref(_x$224.$0);
      _bind$226 = $StringView$$find(_x$224, _tmp$1945);
      if (_bind$226 == 4294967296ll) {
        moonbit_decref(_x$224.$0);
        moonbit_panic();
      } else {
        int64_t _Some$227 = _bind$226;
        int32_t _pkg_end$228 = (int32_t)_Some$227;
        int64_t _tmp$1944 = (int64_t)_pkg_end$228;
        struct $StringView pkg$229;
        int32_t _tmp$1943;
        struct $StringView _tmp$1942;
        int64_t _bind$230;
        moonbit_incref(_x$224.$0);
        pkg$229 = $StringView$$view$inner(_x$224, 0, _tmp$1944);
        _tmp$1943
        = Moonbit_array_length(
          $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        );
        moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5437);
        _tmp$1942
        = (struct $StringView){
          0, _tmp$1943, $moonbitlang$core$builtin$parse$$2a$bind$7c$5437
        };
        moonbit_incref(_x$224.$0);
        _bind$230 = $StringView$$rev_find(_x$224, _tmp$1942);
        if (_bind$230 == 4294967296ll) {
          moonbit_decref(pkg$229.$0);
          moonbit_decref(_x$224.$0);
          moonbit_panic();
        } else {
          int64_t _Some$231 = _bind$230;
          int32_t _start_loc_end$232 = (int32_t)_Some$231;
          int32_t _tmp$1926 = _start_loc_end$232 + 1;
          int32_t _tmp$1927;
          moonbit_incref(_x$224.$0);
          _tmp$1927 = $StringView$$length(_x$224);
          if (_tmp$1926 < _tmp$1927) {
            int32_t _tmp$1941 = _start_loc_end$232 + 1;
            struct $StringView end_loc$233;
            struct $$3c$StringView$2a$StringView$3e$* _bind$234;
            moonbit_incref(_x$224.$0);
            end_loc$233
            = $StringView$$view$inner(
              _x$224, _tmp$1941, 4294967296ll
            );
            _bind$234
            = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
              end_loc$233
            );
            if (_bind$234 == 0) {
              if (_bind$234) {
                moonbit_decref(_bind$234);
              }
              moonbit_decref(pkg$229.$0);
              moonbit_decref(_x$224.$0);
              moonbit_panic();
            } else {
              struct $$3c$StringView$2a$StringView$3e$* _Some$235 = _bind$234;
              struct $$3c$StringView$2a$StringView$3e$* _x$236 = _Some$235;
              struct $StringView _field$4386 =
                (struct $StringView){
                  _x$236->$0_1, _x$236->$0_2, _x$236->$0_0
                };
              struct $StringView _end_line$237 = _field$4386;
              struct $StringView _field$4385 =
                (struct $StringView){
                  _x$236->$1_1, _x$236->$1_2, _x$236->$1_0
                };
              int32_t _cnt$4540 = Moonbit_object_header(_x$236)->rc;
              struct $StringView _end_column$238;
              int64_t _tmp$1940;
              struct $StringView rest$239;
              int32_t _tmp$1939;
              struct $StringView _tmp$1938;
              int64_t _bind$241;
              if (_cnt$4540 > 1) {
                int32_t _new_cnt$4541;
                moonbit_incref(_field$4385.$0);
                moonbit_incref(_end_line$237.$0);
                _new_cnt$4541 = _cnt$4540 - 1;
                Moonbit_object_header(_x$236)->rc = _new_cnt$4541;
              } else if (_cnt$4540 == 1) {
                moonbit_free(_x$236);
              }
              _end_column$238 = _field$4385;
              _tmp$1940 = (int64_t)_start_loc_end$232;
              rest$239 = $StringView$$view$inner(_x$224, 0, _tmp$1940);
              _tmp$1939
              = Moonbit_array_length(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              moonbit_incref(
                $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              );
              _tmp$1938
              = (struct $StringView){
                0,
                  _tmp$1939,
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5424
              };
              moonbit_incref(rest$239.$0);
              _bind$241 = $StringView$$rev_find(rest$239, _tmp$1938);
              if (_bind$241 == 4294967296ll) {
                moonbit_decref(rest$239.$0);
                moonbit_decref(_end_column$238.$0);
                moonbit_decref(_end_line$237.$0);
                moonbit_decref(pkg$229.$0);
                goto $join$240;
              } else {
                int64_t _Some$242 = _bind$241;
                int32_t _start_line_end$243 = (int32_t)_Some$242;
                int64_t _tmp$1937 = (int64_t)_start_line_end$243;
                struct $StringView _tmp$1934;
                int32_t _tmp$1936;
                struct $StringView _tmp$1935;
                int64_t _bind$244;
                moonbit_incref(rest$239.$0);
                _tmp$1934 = $StringView$$view$inner(rest$239, 0, _tmp$1937);
                _tmp$1936
                = Moonbit_array_length(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                moonbit_incref(
                  $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                );
                _tmp$1935
                = (struct $StringView){
                  0,
                    _tmp$1936,
                    $moonbitlang$core$builtin$parse$$2a$bind$7c$5418
                };
                _bind$244 = $StringView$$rev_find(_tmp$1934, _tmp$1935);
                if (_bind$244 == 4294967296ll) {
                  moonbit_decref(rest$239.$0);
                  moonbit_decref(_end_column$238.$0);
                  moonbit_decref(_end_line$237.$0);
                  moonbit_decref(pkg$229.$0);
                  goto $join$240;
                } else {
                  int64_t _Some$245 = _bind$244;
                  int32_t _filename_end$246 = (int32_t)_Some$245;
                  int32_t _tmp$1928 = _filename_end$246 + 1;
                  int32_t _tmp$1929;
                  moonbit_incref(rest$239.$0);
                  _tmp$1929 = $StringView$$length(rest$239);
                  if (_tmp$1928 < _tmp$1929) {
                    int32_t _tmp$1933 = _filename_end$246 + 1;
                    struct $StringView start_loc$247;
                    struct $$3c$StringView$2a$StringView$3e$* _bind$248;
                    moonbit_incref(rest$239.$0);
                    start_loc$247
                    = $StringView$$view$inner(
                      rest$239, _tmp$1933, 4294967296ll
                    );
                    _bind$248
                    = $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
                      start_loc$247
                    );
                    if (_bind$248 == 0) {
                      if (_bind$248) {
                        moonbit_decref(_bind$248);
                      }
                      moonbit_decref(rest$239.$0);
                      moonbit_decref(_end_column$238.$0);
                      moonbit_decref(_end_line$237.$0);
                      moonbit_decref(pkg$229.$0);
                      moonbit_panic();
                    } else {
                      struct $$3c$StringView$2a$StringView$3e$* _Some$249 =
                        _bind$248;
                      struct $$3c$StringView$2a$StringView$3e$* _x$250 =
                        _Some$249;
                      struct $StringView _field$4384 =
                        (struct $StringView){
                          _x$250->$0_1, _x$250->$0_2, _x$250->$0_0
                        };
                      struct $StringView _start_line$251 = _field$4384;
                      struct $StringView _field$4383 =
                        (struct $StringView){
                          _x$250->$1_1, _x$250->$1_2, _x$250->$1_0
                        };
                      int32_t _cnt$4542 = Moonbit_object_header(_x$250)->rc;
                      struct $StringView _start_column$252;
                      int32_t _tmp$1930;
                      if (_cnt$4542 > 1) {
                        int32_t _new_cnt$4543;
                        moonbit_incref(_field$4383.$0);
                        moonbit_incref(_start_line$251.$0);
                        _new_cnt$4543 = _cnt$4542 - 1;
                        Moonbit_object_header(_x$250)->rc = _new_cnt$4543;
                      } else if (_cnt$4542 == 1) {
                        moonbit_free(_x$250);
                      }
                      _start_column$252 = _field$4383;
                      _tmp$1930 = _pkg_end$228 + 1;
                      if (_filename_end$246 > _tmp$1930) {
                        int32_t _tmp$1931 = _pkg_end$228 + 1;
                        int64_t _tmp$1932 = (int64_t)_filename_end$246;
                        struct $StringView filename$253 =
                          $StringView$$view$inner(
                            rest$239, _tmp$1931, _tmp$1932
                          );
                        struct $$moonbitlang$core$builtin$SourceLocRepr* _block$5058 =
                          (struct $$moonbitlang$core$builtin$SourceLocRepr*)moonbit_malloc(
                            sizeof(
                              struct $$moonbitlang$core$builtin$SourceLocRepr
                            )
                          );
                        Moonbit_object_header(_block$5058)->meta
                        = Moonbit_make_regular_object_header(
                          offsetof(
                            struct $$moonbitlang$core$builtin$SourceLocRepr,
                              $0_0
                          )
                          >> 2,
                            6,
                            0
                        );
                        _block$5058->$0_0 = pkg$229.$0;
                        _block$5058->$0_1 = pkg$229.$1;
                        _block$5058->$0_2 = pkg$229.$2;
                        _block$5058->$1_0 = filename$253.$0;
                        _block$5058->$1_1 = filename$253.$1;
                        _block$5058->$1_2 = filename$253.$2;
                        _block$5058->$2_0 = _start_line$251.$0;
                        _block$5058->$2_1 = _start_line$251.$1;
                        _block$5058->$2_2 = _start_line$251.$2;
                        _block$5058->$3_0 = _start_column$252.$0;
                        _block$5058->$3_1 = _start_column$252.$1;
                        _block$5058->$3_2 = _start_column$252.$2;
                        _block$5058->$4_0 = _end_line$237.$0;
                        _block$5058->$4_1 = _end_line$237.$1;
                        _block$5058->$4_2 = _end_line$237.$2;
                        _block$5058->$5_0 = _end_column$238.$0;
                        _block$5058->$5_1 = _end_column$238.$1;
                        _block$5058->$5_2 = _end_column$238.$2;
                        return _block$5058;
                      } else {
                        moonbit_decref(_start_column$252.$0);
                        moonbit_decref(_start_line$251.$0);
                        moonbit_decref(rest$239.$0);
                        moonbit_decref(_end_column$238.$0);
                        moonbit_decref(_end_line$237.$0);
                        moonbit_decref(pkg$229.$0);
                        moonbit_panic();
                      }
                    }
                  } else {
                    moonbit_decref(rest$239.$0);
                    moonbit_decref(_end_column$238.$0);
                    moonbit_decref(_end_line$237.$0);
                    moonbit_decref(pkg$229.$0);
                    moonbit_panic();
                  }
                }
              }
              $join$240:;
              moonbit_panic();
            }
          } else {
            moonbit_decref(pkg$229.$0);
            moonbit_decref(_x$224.$0);
            moonbit_panic();
          }
        }
      }
    } else {
      moonbit_decref(repr$222);
      goto $join$221;
    }
  } else {
    moonbit_decref(repr$222);
    goto $join$221;
  }
  $join$221:;
  moonbit_panic();
}

struct $$3c$StringView$2a$StringView$3e$* $moonbitlang$core$builtin$parse$parse_loc$7c$1101(
  struct $StringView view$218
) {
  int32_t _tmp$1923 =
    Moonbit_array_length($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  struct $StringView _tmp$1922;
  int64_t _bind$217;
  moonbit_incref($moonbitlang$core$builtin$parse$$2a$bind$7c$5404);
  _tmp$1922
  = (struct $StringView){
    0, _tmp$1923, $moonbitlang$core$builtin$parse$$2a$bind$7c$5404
  };
  moonbit_incref(view$218.$0);
  _bind$217 = $StringView$$find(view$218, _tmp$1922);
  if (_bind$217 == 4294967296ll) {
    moonbit_decref(view$218.$0);
    return 0;
  } else {
    int64_t _Some$219 = _bind$217;
    int32_t _i$220 = (int32_t)_Some$219;
    int32_t _if_result$5059;
    if (_i$220 > 0) {
      int32_t _tmp$1915 = _i$220 + 1;
      int32_t _tmp$1916;
      moonbit_incref(view$218.$0);
      _tmp$1916 = $StringView$$length(view$218);
      _if_result$5059 = _tmp$1915 < _tmp$1916;
    } else {
      _if_result$5059 = 0;
    }
    if (_if_result$5059) {
      int64_t _tmp$1921 = (int64_t)_i$220;
      struct $StringView _tmp$1918;
      int32_t _tmp$1920;
      struct $StringView _tmp$1919;
      struct $$3c$StringView$2a$StringView$3e$* _tuple$1917;
      moonbit_incref(view$218.$0);
      _tmp$1918 = $StringView$$view$inner(view$218, 0, _tmp$1921);
      _tmp$1920 = _i$220 + 1;
      _tmp$1919 = $StringView$$view$inner(view$218, _tmp$1920, 4294967296ll);
      _tuple$1917
      = (struct $$3c$StringView$2a$StringView$3e$*)moonbit_malloc(
          sizeof(struct $$3c$StringView$2a$StringView$3e$)
        );
      Moonbit_object_header(_tuple$1917)->meta
      = Moonbit_make_regular_object_header(
        offsetof(struct $$3c$StringView$2a$StringView$3e$, $0_0) >> 2, 2, 0
      );
      _tuple$1917->$0_0 = _tmp$1918.$0;
      _tuple$1917->$0_1 = _tmp$1918.$1;
      _tuple$1917->$0_2 = _tmp$1918.$2;
      _tuple$1917->$1_0 = _tmp$1919.$0;
      _tuple$1917->$1_1 = _tmp$1919.$1;
      _tuple$1917->$1_2 = _tmp$1919.$2;
      return _tuple$1917;
    } else {
      moonbit_decref(view$218.$0);
      return 0;
    }
  }
}

struct $StringView $StringView$$view$inner(
  struct $StringView self$215,
  int32_t start_offset$216,
  int64_t end_offset$213
) {
  int32_t end_offset$212;
  int32_t _if_result$5060;
  if (end_offset$213 == 4294967296ll) {
    moonbit_incref(self$215.$0);
    end_offset$212 = $StringView$$length(self$215);
  } else {
    int64_t _Some$214 = end_offset$213;
    end_offset$212 = (int32_t)_Some$214;
  }
  if (start_offset$216 >= 0) {
    if (start_offset$216 <= end_offset$212) {
      int32_t _tmp$1909;
      moonbit_incref(self$215.$0);
      _tmp$1909 = $StringView$$length(self$215);
      _if_result$5060 = end_offset$212 <= _tmp$1909;
    } else {
      _if_result$5060 = 0;
    }
  } else {
    _if_result$5060 = 0;
  }
  if (_if_result$5060) {
    moonbit_string_t _field$4388 = self$215.$0;
    moonbit_string_t str$1910 = _field$4388;
    int32_t start$1914 = self$215.$1;
    int32_t _tmp$1911 = start$1914 + start_offset$216;
    int32_t _field$4387 = self$215.$1;
    int32_t start$1913 = _field$4387;
    int32_t _tmp$1912 = start$1913 + end_offset$212;
    return (struct $StringView){_tmp$1911, _tmp$1912, str$1910};
  } else {
    moonbit_decref(self$215.$0);
    return $moonbitlang$core$builtin$abort$2(
             (moonbit_string_t)moonbit_string_literal_264.data,
               (moonbit_string_t)moonbit_string_literal_265.data
           );
  }
}

int64_t $StringView$$rev_find(
  struct $StringView self$211,
  struct $StringView str$210
) {
  int32_t _tmp$1908;
  moonbit_incref(str$210.$0);
  _tmp$1908 = $StringView$$length(str$210);
  if (_tmp$1908 <= 4) {
    return $moonbitlang$core$builtin$brute_force_rev_find(self$211, str$210);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
             self$211, str$210
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_rev_find(
  struct $StringView haystack$201,
  struct $StringView needle$203
) {
  int32_t haystack_len$200;
  int32_t needle_len$202;
  moonbit_incref(haystack$201.$0);
  haystack_len$200 = $StringView$$length(haystack$201);
  moonbit_incref(needle$203.$0);
  needle_len$202 = $StringView$$length(needle$203);
  if (needle_len$202 > 0) {
    if (haystack_len$200 >= needle_len$202) {
      int32_t needle_first$204;
      int32_t i$205;
      moonbit_incref(needle$203.$0);
      needle_first$204 = $StringView$$unsafe_charcode_at(needle$203, 0);
      i$205 = haystack_len$200 - needle_len$202;
      while (1) {
        int32_t _tmp$1895 = i$205;
        if (_tmp$1895 >= 0) {
          int32_t _tmp$1900;
          while (1) {
            int32_t _tmp$1898 = i$205;
            int32_t _if_result$5063;
            if (_tmp$1898 >= 0) {
              int32_t _tmp$1897 = i$205;
              int32_t _tmp$1896;
              moonbit_incref(haystack$201.$0);
              _tmp$1896
              = $StringView$$unsafe_charcode_at(
                haystack$201, _tmp$1897
              );
              _if_result$5063 = _tmp$1896 != needle_first$204;
            } else {
              _if_result$5063 = 0;
            }
            if (_if_result$5063) {
              int32_t _tmp$1899 = i$205;
              i$205 = _tmp$1899 - 1;
              continue;
            }
            break;
          }
          _tmp$1900 = i$205;
          if (_tmp$1900 >= 0) {
            int32_t j$207 = 1;
            int32_t _tmp$1907;
            while (1) {
              if (j$207 < needle_len$202) {
                int32_t _tmp$1904 = i$205;
                int32_t _tmp$1903 = _tmp$1904 + j$207;
                int32_t _tmp$1901;
                int32_t _tmp$1902;
                int32_t _tmp$1905;
                moonbit_incref(haystack$201.$0);
                _tmp$1901
                = $StringView$$unsafe_charcode_at(
                  haystack$201, _tmp$1903
                );
                moonbit_incref(needle$203.$0);
                _tmp$1902
                = $StringView$$unsafe_charcode_at(
                  needle$203, j$207
                );
                if (_tmp$1901 != _tmp$1902) {
                  break;
                }
                _tmp$1905 = j$207 + 1;
                j$207 = _tmp$1905;
                continue;
              } else {
                int32_t _tmp$1906;
                moonbit_decref(needle$203.$0);
                moonbit_decref(haystack$201.$0);
                _tmp$1906 = i$205;
                return (int64_t)_tmp$1906;
              }
              break;
            }
            _tmp$1907 = i$205;
            i$205 = _tmp$1907 - 1;
          }
          continue;
        } else {
          moonbit_decref(needle$203.$0);
          moonbit_decref(haystack$201.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$203.$0);
      moonbit_decref(haystack$201.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$203.$0);
    moonbit_decref(haystack$201.$0);
    return (int64_t)haystack_len$200;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_rev_find(
  struct $StringView haystack$190,
  struct $StringView needle$192
) {
  int32_t haystack_len$189;
  int32_t needle_len$191;
  moonbit_incref(haystack$190.$0);
  haystack_len$189 = $StringView$$length(haystack$190);
  moonbit_incref(needle$192.$0);
  needle_len$191 = $StringView$$length(needle$192);
  if (needle_len$191 > 0) {
    if (haystack_len$189 >= needle_len$191) {
      int32_t* skip_table$193 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$191);
      int32_t _tmp$1885 = needle_len$191 - 1;
      int32_t i$194 = _tmp$1885;
      int32_t _tmp$1894;
      int32_t i$196;
      while (1) {
        if (i$194 > 0) {
          int32_t _tmp$1883;
          int32_t _tmp$1882;
          int32_t _tmp$1884;
          moonbit_incref(needle$192.$0);
          _tmp$1883 = $StringView$$unsafe_charcode_at(needle$192, i$194);
          _tmp$1882 = _tmp$1883 & 255;
          if (
            _tmp$1882 < 0
            || _tmp$1882 >= Moonbit_array_length(skip_table$193)
          ) {
            moonbit_panic();
          }
          skip_table$193[_tmp$1882] = i$194;
          _tmp$1884 = i$194 - 1;
          i$194 = _tmp$1884;
          continue;
        }
        break;
      }
      _tmp$1894 = haystack_len$189 - needle_len$191;
      i$196 = _tmp$1894;
      while (1) {
        if (i$196 >= 0) {
          int32_t j$197 = 0;
          int32_t _tmp$1893;
          int32_t _tmp$1892;
          int32_t _tmp$1891;
          int32_t _tmp$1890;
          while (1) {
            if (j$197 < needle_len$191) {
              int32_t _tmp$1888 = i$196 + j$197;
              int32_t _tmp$1886;
              int32_t _tmp$1887;
              int32_t _tmp$1889;
              moonbit_incref(haystack$190.$0);
              _tmp$1886
              = $StringView$$unsafe_charcode_at(
                haystack$190, _tmp$1888
              );
              moonbit_incref(needle$192.$0);
              _tmp$1887 = $StringView$$unsafe_charcode_at(needle$192, j$197);
              if (_tmp$1886 != _tmp$1887) {
                break;
              }
              _tmp$1889 = j$197 + 1;
              j$197 = _tmp$1889;
              continue;
            } else {
              moonbit_decref(skip_table$193);
              moonbit_decref(needle$192.$0);
              moonbit_decref(haystack$190.$0);
              return (int64_t)i$196;
            }
            break;
          }
          moonbit_incref(haystack$190.$0);
          _tmp$1893 = $StringView$$unsafe_charcode_at(haystack$190, i$196);
          _tmp$1892 = _tmp$1893 & 255;
          if (
            _tmp$1892 < 0
            || _tmp$1892 >= Moonbit_array_length(skip_table$193)
          ) {
            moonbit_panic();
          }
          _tmp$1891 = (int32_t)skip_table$193[_tmp$1892];
          _tmp$1890 = i$196 - _tmp$1891;
          i$196 = _tmp$1890;
          continue;
        } else {
          moonbit_decref(skip_table$193);
          moonbit_decref(needle$192.$0);
          moonbit_decref(haystack$190.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$192.$0);
      moonbit_decref(haystack$190.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$192.$0);
    moonbit_decref(haystack$190.$0);
    return (int64_t)haystack_len$189;
  }
}

int64_t $StringView$$find(
  struct $StringView self$188,
  struct $StringView str$187
) {
  int32_t _tmp$1881;
  moonbit_incref(str$187.$0);
  _tmp$1881 = $StringView$$length(str$187);
  if (_tmp$1881 <= 4) {
    return $moonbitlang$core$builtin$brute_force_find(self$188, str$187);
  } else {
    return $moonbitlang$core$builtin$boyer_moore_horspool_find(
             self$188, str$187
           );
  }
}

int64_t $moonbitlang$core$builtin$brute_force_find(
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
      int32_t needle_first$180;
      int32_t forward_len$181;
      int32_t i$182;
      moonbit_incref(needle$179.$0);
      needle_first$180 = $StringView$$unsafe_charcode_at(needle$179, 0);
      forward_len$181 = haystack_len$176 - needle_len$178;
      i$182 = 0;
      while (1) {
        int32_t _tmp$1868 = i$182;
        if (_tmp$1868 <= forward_len$181) {
          int32_t _tmp$1873;
          while (1) {
            int32_t _tmp$1871 = i$182;
            int32_t _if_result$5070;
            if (_tmp$1871 <= forward_len$181) {
              int32_t _tmp$1870 = i$182;
              int32_t _tmp$1869;
              moonbit_incref(haystack$177.$0);
              _tmp$1869
              = $StringView$$unsafe_charcode_at(
                haystack$177, _tmp$1870
              );
              _if_result$5070 = _tmp$1869 != needle_first$180;
            } else {
              _if_result$5070 = 0;
            }
            if (_if_result$5070) {
              int32_t _tmp$1872 = i$182;
              i$182 = _tmp$1872 + 1;
              continue;
            }
            break;
          }
          _tmp$1873 = i$182;
          if (_tmp$1873 <= forward_len$181) {
            int32_t j$184 = 1;
            int32_t _tmp$1880;
            while (1) {
              if (j$184 < needle_len$178) {
                int32_t _tmp$1877 = i$182;
                int32_t _tmp$1876 = _tmp$1877 + j$184;
                int32_t _tmp$1874;
                int32_t _tmp$1875;
                int32_t _tmp$1878;
                moonbit_incref(haystack$177.$0);
                _tmp$1874
                = $StringView$$unsafe_charcode_at(
                  haystack$177, _tmp$1876
                );
                moonbit_incref(needle$179.$0);
                _tmp$1875
                = $StringView$$unsafe_charcode_at(
                  needle$179, j$184
                );
                if (_tmp$1874 != _tmp$1875) {
                  break;
                }
                _tmp$1878 = j$184 + 1;
                j$184 = _tmp$1878;
                continue;
              } else {
                int32_t _tmp$1879;
                moonbit_decref(needle$179.$0);
                moonbit_decref(haystack$177.$0);
                _tmp$1879 = i$182;
                return (int64_t)_tmp$1879;
              }
              break;
            }
            _tmp$1880 = i$182;
            i$182 = _tmp$1880 + 1;
          }
          continue;
        } else {
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
    return $moonbitlang$core$builtin$brute_force_find$constr$175;
  }
}

int64_t $moonbitlang$core$builtin$boyer_moore_horspool_find(
  struct $StringView haystack$163,
  struct $StringView needle$165
) {
  int32_t haystack_len$162;
  int32_t needle_len$164;
  moonbit_incref(haystack$163.$0);
  haystack_len$162 = $StringView$$length(haystack$163);
  moonbit_incref(needle$165.$0);
  needle_len$164 = $StringView$$length(needle$165);
  if (needle_len$164 > 0) {
    if (haystack_len$162 >= needle_len$164) {
      int32_t* skip_table$166 =
        (int32_t*)moonbit_make_int32_array(256, needle_len$164);
      int32_t _end4301$167 = needle_len$164 - 1;
      int32_t i$168 = 0;
      int32_t i$170;
      while (1) {
        if (i$168 < _end4301$167) {
          int32_t _tmp$1855;
          int32_t _tmp$1852;
          int32_t _tmp$1854;
          int32_t _tmp$1853;
          int32_t _tmp$1856;
          moonbit_incref(needle$165.$0);
          _tmp$1855 = $StringView$$unsafe_charcode_at(needle$165, i$168);
          _tmp$1852 = _tmp$1855 & 255;
          _tmp$1854 = needle_len$164 - 1;
          _tmp$1853 = _tmp$1854 - i$168;
          if (
            _tmp$1852 < 0
            || _tmp$1852 >= Moonbit_array_length(skip_table$166)
          ) {
            moonbit_panic();
          }
          skip_table$166[_tmp$1852] = _tmp$1853;
          _tmp$1856 = i$168 + 1;
          i$168 = _tmp$1856;
          continue;
        }
        break;
      }
      i$170 = 0;
      while (1) {
        int32_t _tmp$1857 = haystack_len$162 - needle_len$164;
        if (i$170 <= _tmp$1857) {
          int32_t _end4307$171 = needle_len$164 - 1;
          int32_t j$172 = 0;
          int32_t _tmp$1867;
          int32_t _tmp$1866;
          int32_t _tmp$1865;
          int32_t _tmp$1864;
          int32_t _tmp$1863;
          int32_t _tmp$1862;
          while (1) {
            if (j$172 <= _end4307$171) {
              int32_t _tmp$1860 = i$170 + j$172;
              int32_t _tmp$1858;
              int32_t _tmp$1859;
              int32_t _tmp$1861;
              moonbit_incref(haystack$163.$0);
              _tmp$1858
              = $StringView$$unsafe_charcode_at(
                haystack$163, _tmp$1860
              );
              moonbit_incref(needle$165.$0);
              _tmp$1859 = $StringView$$unsafe_charcode_at(needle$165, j$172);
              if (_tmp$1858 != _tmp$1859) {
                break;
              }
              _tmp$1861 = j$172 + 1;
              j$172 = _tmp$1861;
              continue;
            } else {
              moonbit_decref(skip_table$166);
              moonbit_decref(needle$165.$0);
              moonbit_decref(haystack$163.$0);
              return (int64_t)i$170;
            }
            break;
          }
          _tmp$1867 = i$170 + needle_len$164;
          _tmp$1866 = _tmp$1867 - 1;
          moonbit_incref(haystack$163.$0);
          _tmp$1865
          = $StringView$$unsafe_charcode_at(
            haystack$163, _tmp$1866
          );
          _tmp$1864 = _tmp$1865 & 255;
          if (
            _tmp$1864 < 0
            || _tmp$1864 >= Moonbit_array_length(skip_table$166)
          ) {
            moonbit_panic();
          }
          _tmp$1863 = (int32_t)skip_table$166[_tmp$1864];
          _tmp$1862 = i$170 + _tmp$1863;
          i$170 = _tmp$1862;
          continue;
        } else {
          moonbit_decref(skip_table$166);
          moonbit_decref(needle$165.$0);
          moonbit_decref(haystack$163.$0);
        }
        break;
      }
      return 4294967296ll;
    } else {
      moonbit_decref(needle$165.$0);
      moonbit_decref(haystack$163.$0);
      return 4294967296ll;
    }
  } else {
    moonbit_decref(needle$165.$0);
    moonbit_decref(haystack$163.$0);
    return $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$161;
  }
}

int32_t $StringView$$unsafe_charcode_at(
  struct $StringView self$159,
  int32_t index$160
) {
  moonbit_string_t _field$4391 = self$159.$0;
  moonbit_string_t str$1849 = _field$4391;
  int32_t _field$4390 = self$159.$1;
  int32_t start$1851 = _field$4390;
  int32_t _tmp$1850 = start$1851 + index$160;
  int32_t _tmp$4389 = str$1849[_tmp$1850];
  moonbit_decref(str$1849);
  return _tmp$4389;
}

int32_t $StringView$$length(struct $StringView self$158) {
  int32_t end$1847 = self$158.$2;
  int32_t _field$4392 = self$158.$1;
  int32_t start$1848;
  moonbit_decref(self$158.$0);
  start$1848 = _field$4392;
  return end$1847 - start$1848;
}

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* $$moonbitlang$core$builtin$Array$$at$2(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$156,
  int32_t index$157
) {
  int32_t len$155 = self$156->$1;
  int32_t _if_result$5075;
  if (index$157 >= 0) {
    _if_result$5075 = index$157 < len$155;
  } else {
    _if_result$5075 = 0;
  }
  if (_if_result$5075) {
    struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** _tmp$1846 =
      $$moonbitlang$core$builtin$Array$$buffer$4(self$156);
    struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* _tmp$4393;
    if (index$157 < 0 || index$157 >= Moonbit_array_length(_tmp$1846)) {
      moonbit_panic();
    }
    _tmp$4393
    = (struct $$moonbitlang$core$builtin$Array$3c$Int$3e$*)_tmp$1846[
        index$157
      ];
    if (_tmp$4393) {
      moonbit_incref(_tmp$4393);
    }
    moonbit_decref(_tmp$1846);
    return _tmp$4393;
  } else {
    moonbit_decref(self$156);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$at$1(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$153,
  int32_t index$154
) {
  int32_t len$152 = self$153->$1;
  int32_t _if_result$5076;
  if (index$154 >= 0) {
    _if_result$5076 = index$154 < len$152;
  } else {
    _if_result$5076 = 0;
  }
  if (_if_result$5076) {
    int32_t* _tmp$1845 = $$moonbitlang$core$builtin$Array$$buffer$3(self$153);
    int32_t _tmp$4394;
    if (index$154 < 0 || index$154 >= Moonbit_array_length(_tmp$1845)) {
      moonbit_panic();
    }
    _tmp$4394 = (int32_t)_tmp$1845[index$154];
    moonbit_decref(_tmp$1845);
    return _tmp$4394;
  } else {
    moonbit_decref(self$153);
    moonbit_panic();
  }
}

moonbit_string_t $$moonbitlang$core$builtin$Array$$at$0(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$150,
  int32_t index$151
) {
  int32_t len$149 = self$150->$1;
  int32_t _if_result$5077;
  if (index$151 >= 0) {
    _if_result$5077 = index$151 < len$149;
  } else {
    _if_result$5077 = 0;
  }
  if (_if_result$5077) {
    moonbit_string_t* _tmp$1844 =
      $$moonbitlang$core$builtin$Array$$buffer$1(self$150);
    moonbit_string_t _tmp$4395;
    if (index$151 < 0 || index$151 >= Moonbit_array_length(_tmp$1844)) {
      moonbit_panic();
    }
    _tmp$4395 = (moonbit_string_t)_tmp$1844[index$151];
    moonbit_incref(_tmp$4395);
    moonbit_decref(_tmp$1844);
    return _tmp$4395;
  } else {
    moonbit_decref(self$150);
    moonbit_panic();
  }
}

int32_t $$moonbitlang$core$builtin$Array$$length$5(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$148
) {
  int32_t _field$4396 = self$148->$1;
  moonbit_decref(self$148);
  return _field$4396;
}

int32_t $$moonbitlang$core$builtin$Array$$length$4(
  struct $$moonbitlang$core$builtin$Array$3c$Double$3e$* self$147
) {
  int32_t _field$4397 = self$147->$1;
  moonbit_decref(self$147);
  return _field$4397;
}

int32_t $$moonbitlang$core$builtin$Array$$length$3(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$146
) {
  int32_t _field$4398 = self$146->$1;
  moonbit_decref(self$146);
  return _field$4398;
}

int32_t $$moonbitlang$core$builtin$Array$$length$2(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$145
) {
  int32_t _field$4399 = self$145->$1;
  moonbit_decref(self$145);
  return _field$4399;
}

int32_t $$moonbitlang$core$builtin$Array$$length$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$144
) {
  int32_t _field$4400 = self$144->$1;
  moonbit_decref(self$144);
  return _field$4400;
}

int32_t $$moonbitlang$core$builtin$Array$$length$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$143
) {
  int32_t _field$4401 = self$143->$1;
  moonbit_decref(self$143);
  return _field$4401;
}

int32_t* $$moonbitlang$core$builtin$Array$$buffer$5(
  struct $$moonbitlang$core$builtin$Array$3c$Bool$3e$* self$142
) {
  int32_t* _field$4402 = self$142->$0;
  int32_t _cnt$4544 = Moonbit_object_header(self$142)->rc;
  if (_cnt$4544 > 1) {
    int32_t _new_cnt$4545;
    moonbit_incref(_field$4402);
    _new_cnt$4545 = _cnt$4544 - 1;
    Moonbit_object_header(self$142)->rc = _new_cnt$4545;
  } else if (_cnt$4544 == 1) {
    moonbit_free(self$142);
  }
  return _field$4402;
}

struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$4(
  struct $$moonbitlang$core$builtin$Array$3c$$moonbitlang$core$builtin$Array$3c$Int$3e$$3e$* self$141
) {
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$** _field$4403 =
    self$141->$0;
  int32_t _cnt$4546 = Moonbit_object_header(self$141)->rc;
  if (_cnt$4546 > 1) {
    int32_t _new_cnt$4547;
    moonbit_incref(_field$4403);
    _new_cnt$4547 = _cnt$4546 - 1;
    Moonbit_object_header(self$141)->rc = _new_cnt$4547;
  } else if (_cnt$4546 == 1) {
    moonbit_free(self$141);
  }
  return _field$4403;
}

int32_t* $$moonbitlang$core$builtin$Array$$buffer$3(
  struct $$moonbitlang$core$builtin$Array$3c$Int$3e$* self$140
) {
  int32_t* _field$4404 = self$140->$0;
  int32_t _cnt$4548 = Moonbit_object_header(self$140)->rc;
  if (_cnt$4548 > 1) {
    int32_t _new_cnt$4549;
    moonbit_incref(_field$4404);
    _new_cnt$4549 = _cnt$4548 - 1;
    Moonbit_object_header(self$140)->rc = _new_cnt$4549;
  } else if (_cnt$4548 == 1) {
    moonbit_free(self$140);
  }
  return _field$4404;
}

struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** $$moonbitlang$core$builtin$Array$$buffer$2(
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* self$139
) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _field$4405 =
    self$139->$0;
  int32_t _cnt$4550 = Moonbit_object_header(self$139)->rc;
  if (_cnt$4550 > 1) {
    int32_t _new_cnt$4551;
    moonbit_incref(_field$4405);
    _new_cnt$4551 = _cnt$4550 - 1;
    Moonbit_object_header(self$139)->rc = _new_cnt$4551;
  } else if (_cnt$4550 == 1) {
    moonbit_free(self$139);
  }
  return _field$4405;
}

moonbit_string_t* $$moonbitlang$core$builtin$Array$$buffer$1(
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* self$138
) {
  moonbit_string_t* _field$4406 = self$138->$0;
  int32_t _cnt$4552 = Moonbit_object_header(self$138)->rc;
  if (_cnt$4552 > 1) {
    int32_t _new_cnt$4553;
    moonbit_incref(_field$4406);
    _new_cnt$4553 = _cnt$4552 - 1;
    Moonbit_object_header(self$138)->rc = _new_cnt$4553;
  } else if (_cnt$4552 == 1) {
    moonbit_free(self$138);
  }
  return _field$4406;
}

struct $$3c$String$2a$Int$3e$** $$moonbitlang$core$builtin$Array$$buffer$0(
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* self$137
) {
  struct $$3c$String$2a$Int$3e$** _field$4407 = self$137->$0;
  int32_t _cnt$4554 = Moonbit_object_header(self$137)->rc;
  if (_cnt$4554 > 1) {
    int32_t _new_cnt$4555;
    moonbit_incref(_field$4407);
    _new_cnt$4555 = _cnt$4554 - 1;
    Moonbit_object_header(self$137)->rc = _new_cnt$4555;
  } else if (_cnt$4554 == 1) {
    moonbit_free(self$137);
  }
  return _field$4407;
}

moonbit_string_t $String$$escape(moonbit_string_t self$136) {
  struct $$moonbitlang$core$builtin$StringBuilder* buf$135 =
    $$moonbitlang$core$builtin$StringBuilder$$new$inner(0);
  struct $$moonbitlang$core$builtin$Logger _tmp$1843;
  moonbit_incref(buf$135);
  _tmp$1843
  = (struct $$moonbitlang$core$builtin$Logger){
    $$moonbitlang$core$builtin$StringBuilder$as_$moonbitlang$core$builtin$Logger$static_method_table_id,
      buf$135
  };
  $$moonbitlang$core$builtin$Show$$String$$output(self$136, _tmp$1843);
  return $$moonbitlang$core$builtin$StringBuilder$$to_string(buf$135);
}

int32_t $moonbitlang$core$builtin$op_notequal$5(
  moonbit_string_t x$133,
  moonbit_string_t y$134
) {
  int32_t _tmp$4408 = moonbit_val_array_equal(x$133, y$134);
  int32_t _tmp$1842;
  moonbit_decref(x$133);
  moonbit_decref(y$134);
  _tmp$1842 = _tmp$4408;
  return !_tmp$1842;
}

int32_t $moonbitlang$core$builtin$op_notequal$4(int32_t x$131, int32_t y$132) {
  int32_t _tmp$1841 = x$131 == y$132;
  return !_tmp$1841;
}

int32_t $moonbitlang$core$builtin$op_notequal$3(int32_t x$129, int32_t y$130) {
  int32_t _tmp$1840 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$2(x$129, y$130);
  return !_tmp$1840;
}

int32_t $moonbitlang$core$builtin$op_notequal$2(
  moonbit_string_t x$127,
  moonbit_string_t y$128
) {
  int32_t _tmp$1839 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$1(x$127, y$128);
  return !_tmp$1839;
}

int32_t $moonbitlang$core$builtin$op_notequal$1(int64_t x$125, int64_t y$126) {
  int32_t _tmp$1838 =
    $$moonbitlang$core$builtin$Eq$$Option$$equal$0(x$125, y$126);
  return !_tmp$1838;
}

int32_t $moonbitlang$core$builtin$op_notequal$0(int32_t x$123, int32_t y$124) {
  int32_t _tmp$1837 =
    $$moonbitlang$core$builtin$Eq$$$moonbitlang$core$builtin$IterResult$$equal(
      x$123, y$124
    );
  return !_tmp$1837;
}

int32_t $Int$$is_trailing_surrogate(int32_t self$122) {
  if (56320 <= self$122) {
    return self$122 <= 57343;
  } else {
    return 0;
  }
}

int32_t $Int$$is_leading_surrogate(int32_t self$121) {
  if (55296 <= self$121) {
    return self$121 <= 56319;
  } else {
    return 0;
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
  struct $$moonbitlang$core$builtin$StringBuilder* self$118,
  int32_t ch$120
) {
  int32_t len$1832 = self$118->$1;
  int32_t _tmp$1831 = len$1832 + 4;
  moonbit_bytes_t _field$4409;
  moonbit_bytes_t data$1835;
  int32_t len$1836;
  int32_t inc$119;
  int32_t len$1834;
  int32_t _tmp$1833;
  moonbit_incref(self$118);
  $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
    self$118, _tmp$1831
  );
  _field$4409 = self$118->$0;
  data$1835 = _field$4409;
  len$1836 = self$118->$1;
  moonbit_incref(data$1835);
  inc$119 = $FixedArray$$set_utf16le_char(data$1835, len$1836, ch$120);
  len$1834 = self$118->$1;
  _tmp$1833 = len$1834 + inc$119;
  self$118->$1 = _tmp$1833;
  moonbit_decref(self$118);
  return 0;
}

int32_t $$moonbitlang$core$builtin$StringBuilder$$grow_if_necessary(
  struct $$moonbitlang$core$builtin$StringBuilder* self$113,
  int32_t required$114
) {
  moonbit_bytes_t _field$4413 = self$113->$0;
  moonbit_bytes_t data$1830 = _field$4413;
  int32_t _tmp$4412 = Moonbit_array_length(data$1830);
  int32_t current_len$112 = _tmp$4412;
  int32_t enough_space$115;
  int32_t _tmp$1828;
  int32_t _tmp$1829;
  moonbit_bytes_t new_data$117;
  moonbit_bytes_t _field$4411;
  moonbit_bytes_t data$1826;
  int32_t len$1827;
  moonbit_bytes_t _old$4410;
  if (required$114 <= current_len$112) {
    moonbit_decref(self$113);
    return 0;
  }
  enough_space$115 = current_len$112;
  while (1) {
    int32_t _tmp$1824 = enough_space$115;
    if (_tmp$1824 < required$114) {
      int32_t _tmp$1825 = enough_space$115;
      enough_space$115 = _tmp$1825 * 2;
      continue;
    }
    break;
  }
  _tmp$1828 = enough_space$115;
  _tmp$1829 = $$moonbitlang$core$builtin$Default$$Byte$$default();
  new_data$117 = (moonbit_bytes_t)moonbit_make_bytes(_tmp$1828, _tmp$1829);
  _field$4411 = self$113->$0;
  data$1826 = _field$4411;
  len$1827 = self$113->$1;
  moonbit_incref(data$1826);
  moonbit_incref(new_data$117);
  $FixedArray$$unsafe_blit$0(new_data$117, 0, data$1826, 0, len$1827);
  _old$4410 = self$113->$0;
  moonbit_decref(_old$4410);
  self$113->$0 = new_data$117;
  moonbit_decref(self$113);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Default$$Byte$$default() {
  return 0;
}

int32_t $FixedArray$$set_utf16le_char(
  moonbit_bytes_t self$107,
  int32_t offset$108,
  int32_t value$106
) {
  uint32_t code$105 = $Char$$to_uint(value$106);
  if (code$105 < 65536u) {
    uint32_t _tmp$1807 = code$105 & 255u;
    int32_t _tmp$1806 = $UInt$$to_byte(_tmp$1807);
    int32_t _tmp$1808;
    uint32_t _tmp$1810;
    int32_t _tmp$1809;
    if (offset$108 < 0 || offset$108 >= Moonbit_array_length(self$107)) {
      moonbit_panic();
    }
    self$107[offset$108] = _tmp$1806;
    _tmp$1808 = offset$108 + 1;
    _tmp$1810 = code$105 >> 8;
    _tmp$1809 = $UInt$$to_byte(_tmp$1810);
    if (_tmp$1808 < 0 || _tmp$1808 >= Moonbit_array_length(self$107)) {
      moonbit_panic();
    }
    self$107[_tmp$1808] = _tmp$1809;
    moonbit_decref(self$107);
    return 2;
  } else if (code$105 < 1114112u) {
    uint32_t hi$109 = code$105 - 65536u;
    uint32_t _tmp$1823 = hi$109 >> 10;
    uint32_t lo$110 = _tmp$1823 | 55296u;
    uint32_t _tmp$1822 = hi$109 & 1023u;
    uint32_t hi$111 = _tmp$1822 | 56320u;
    uint32_t _tmp$1812 = lo$110 & 255u;
    int32_t _tmp$1811 = $UInt$$to_byte(_tmp$1812);
    int32_t _tmp$1813;
    uint32_t _tmp$1815;
    int32_t _tmp$1814;
    int32_t _tmp$1816;
    uint32_t _tmp$1818;
    int32_t _tmp$1817;
    int32_t _tmp$1819;
    uint32_t _tmp$1821;
    int32_t _tmp$1820;
    if (offset$108 < 0 || offset$108 >= Moonbit_array_length(self$107)) {
      moonbit_panic();
    }
    self$107[offset$108] = _tmp$1811;
    _tmp$1813 = offset$108 + 1;
    _tmp$1815 = lo$110 >> 8;
    _tmp$1814 = $UInt$$to_byte(_tmp$1815);
    if (_tmp$1813 < 0 || _tmp$1813 >= Moonbit_array_length(self$107)) {
      moonbit_panic();
    }
    self$107[_tmp$1813] = _tmp$1814;
    _tmp$1816 = offset$108 + 2;
    _tmp$1818 = hi$111 & 255u;
    _tmp$1817 = $UInt$$to_byte(_tmp$1818);
    if (_tmp$1816 < 0 || _tmp$1816 >= Moonbit_array_length(self$107)) {
      moonbit_panic();
    }
    self$107[_tmp$1816] = _tmp$1817;
    _tmp$1819 = offset$108 + 3;
    _tmp$1821 = hi$111 >> 8;
    _tmp$1820 = $UInt$$to_byte(_tmp$1821);
    if (_tmp$1819 < 0 || _tmp$1819 >= Moonbit_array_length(self$107)) {
      moonbit_panic();
    }
    self$107[_tmp$1819] = _tmp$1820;
    moonbit_decref(self$107);
    return 4;
  } else {
    moonbit_decref(self$107);
    return $moonbitlang$core$builtin$abort$1(
             (moonbit_string_t)moonbit_string_literal_266.data,
               (moonbit_string_t)moonbit_string_literal_267.data
           );
  }
}

int32_t $UInt$$to_byte(uint32_t self$104) {
  int32_t _tmp$1805 = *(int32_t*)&self$104;
  return _tmp$1805 & 0xff;
}

uint32_t $Char$$to_uint(int32_t self$103) {
  int32_t _tmp$1804 = self$103;
  return *(uint32_t*)&_tmp$1804;
}

moonbit_string_t $$moonbitlang$core$builtin$StringBuilder$$to_string(
  struct $$moonbitlang$core$builtin$StringBuilder* self$102
) {
  moonbit_bytes_t _field$4415 = self$102->$0;
  moonbit_bytes_t data$1803 = _field$4415;
  moonbit_bytes_t _tmp$1800;
  int32_t _field$4414;
  int32_t len$1802;
  int64_t _tmp$1801;
  moonbit_incref(data$1803);
  _tmp$1800 = data$1803;
  _field$4414 = self$102->$1;
  moonbit_decref(self$102);
  len$1802 = _field$4414;
  _tmp$1801 = (int64_t)len$1802;
  return $Bytes$$to_unchecked_string$inner(_tmp$1800, 0, _tmp$1801);
}

moonbit_string_t $Bytes$$to_unchecked_string$inner(
  moonbit_bytes_t self$97,
  int32_t offset$101,
  int64_t length$99
) {
  int32_t len$96 = Moonbit_array_length(self$97);
  int32_t length$98;
  int32_t _if_result$5079;
  if (length$99 == 4294967296ll) {
    length$98 = len$96 - offset$101;
  } else {
    int64_t _Some$100 = length$99;
    length$98 = (int32_t)_Some$100;
  }
  if (offset$101 >= 0) {
    if (length$98 >= 0) {
      int32_t _tmp$1799 = offset$101 + length$98;
      _if_result$5079 = _tmp$1799 <= len$96;
    } else {
      _if_result$5079 = 0;
    }
  } else {
    _if_result$5079 = 0;
  }
  if (_if_result$5079) {
    return $moonbitlang$core$builtin$unsafe_sub_string(
             self$97, offset$101, length$98
           );
  } else {
    moonbit_decref(self$97);
    moonbit_panic();
  }
}

struct $$moonbitlang$core$builtin$StringBuilder* $$moonbitlang$core$builtin$StringBuilder$$new$inner(
  int32_t size_hint$94
) {
  int32_t initial$93;
  moonbit_bytes_t data$95;
  struct $$moonbitlang$core$builtin$StringBuilder* _block$5080;
  if (size_hint$94 < 1) {
    initial$93 = 1;
  } else {
    initial$93 = size_hint$94;
  }
  data$95 = (moonbit_bytes_t)moonbit_make_bytes(initial$93, 0);
  _block$5080
  = (struct $$moonbitlang$core$builtin$StringBuilder*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$StringBuilder)
    );
  Moonbit_object_header(_block$5080)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$StringBuilder, $0) >> 2, 1, 0
  );
  _block$5080->$0 = data$95;
  _block$5080->$1 = 0;
  return _block$5080;
}

int32_t $Byte$$to_char(int32_t self$92) {
  int32_t _tmp$1798 = (int32_t)self$92;
  return _tmp$1798;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$3(
  int32_t* dst$87,
  int32_t dst_offset$88,
  int32_t* src$89,
  int32_t src_offset$90,
  int32_t len$91
) {
  $FixedArray$$unsafe_blit$4(
    dst$87, dst_offset$88, src$89, src_offset$90, len$91
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$2(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$82,
  int32_t dst_offset$83,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$84,
  int32_t src_offset$85,
  int32_t len$86
) {
  $FixedArray$$unsafe_blit$3(
    dst$82, dst_offset$83, src$84, src_offset$85, len$86
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$1(
  struct $$3c$String$2a$Int$3e$** dst$77,
  int32_t dst_offset$78,
  struct $$3c$String$2a$Int$3e$** src$79,
  int32_t src_offset$80,
  int32_t len$81
) {
  $FixedArray$$unsafe_blit$2(
    dst$77, dst_offset$78, src$79, src_offset$80, len$81
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$UninitializedArray$$unsafe_blit$0(
  moonbit_string_t* dst$72,
  int32_t dst_offset$73,
  moonbit_string_t* src$74,
  int32_t src_offset$75,
  int32_t len$76
) {
  $FixedArray$$unsafe_blit$1(
    dst$72, dst_offset$73, src$74, src_offset$75, len$76
  );
  return 0;
}

int32_t $FixedArray$$unsafe_blit$4(
  int32_t* dst$63,
  int32_t dst_offset$65,
  int32_t* src$64,
  int32_t src_offset$66,
  int32_t len$68
) {
  int32_t _if_result$5081;
  if (dst$63 == src$64) {
    _if_result$5081 = dst_offset$65 < src_offset$66;
  } else {
    _if_result$5081 = 0;
  }
  if (_if_result$5081) {
    int32_t i$67 = 0;
    while (1) {
      if (i$67 < len$68) {
        int32_t _tmp$1789 = dst_offset$65 + i$67;
        int32_t _tmp$1791 = src_offset$66 + i$67;
        int32_t _tmp$1790;
        int32_t _tmp$1792;
        if (_tmp$1791 < 0 || _tmp$1791 >= Moonbit_array_length(src$64)) {
          moonbit_panic();
        }
        _tmp$1790 = (int32_t)src$64[_tmp$1791];
        if (_tmp$1789 < 0 || _tmp$1789 >= Moonbit_array_length(dst$63)) {
          moonbit_panic();
        }
        dst$63[_tmp$1789] = _tmp$1790;
        _tmp$1792 = i$67 + 1;
        i$67 = _tmp$1792;
        continue;
      } else {
        moonbit_decref(src$64);
        moonbit_decref(dst$63);
      }
      break;
    }
  } else {
    int32_t _tmp$1797 = len$68 - 1;
    int32_t i$70 = _tmp$1797;
    while (1) {
      if (i$70 >= 0) {
        int32_t _tmp$1793 = dst_offset$65 + i$70;
        int32_t _tmp$1795 = src_offset$66 + i$70;
        int32_t _tmp$1794;
        int32_t _tmp$1796;
        if (_tmp$1795 < 0 || _tmp$1795 >= Moonbit_array_length(src$64)) {
          moonbit_panic();
        }
        _tmp$1794 = (int32_t)src$64[_tmp$1795];
        if (_tmp$1793 < 0 || _tmp$1793 >= Moonbit_array_length(dst$63)) {
          moonbit_panic();
        }
        dst$63[_tmp$1793] = _tmp$1794;
        _tmp$1796 = i$70 - 1;
        i$70 = _tmp$1796;
        continue;
      } else {
        moonbit_decref(src$64);
        moonbit_decref(dst$63);
      }
      break;
    }
  }
  return 0;
}

int32_t $FixedArray$$unsafe_blit$3(
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** dst$54,
  int32_t dst_offset$56,
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** src$55,
  int32_t src_offset$57,
  int32_t len$59
) {
  int32_t _if_result$5084;
  if (dst$54 == src$55) {
    _if_result$5084 = dst_offset$56 < src_offset$57;
  } else {
    _if_result$5084 = 0;
  }
  if (_if_result$5084) {
    int32_t i$58 = 0;
    while (1) {
      if (i$58 < len$59) {
        int32_t _tmp$1780 = dst_offset$56 + i$58;
        int32_t _tmp$1782 = src_offset$57 + i$58;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$4417;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1781;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$4416;
        int32_t _tmp$1783;
        if (_tmp$1782 < 0 || _tmp$1782 >= Moonbit_array_length(src$55)) {
          moonbit_panic();
        }
        _tmp$4417
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$55[
            _tmp$1782
          ];
        _tmp$1781 = _tmp$4417;
        if (_tmp$1780 < 0 || _tmp$1780 >= Moonbit_array_length(dst$54)) {
          moonbit_panic();
        }
        _old$4416
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$54[
            _tmp$1780
          ];
        if (_tmp$1781) {
          moonbit_incref(_tmp$1781);
        }
        if (_old$4416) {
          moonbit_decref(_old$4416);
        }
        dst$54[_tmp$1780] = _tmp$1781;
        _tmp$1783 = i$58 + 1;
        i$58 = _tmp$1783;
        continue;
      } else {
        moonbit_decref(src$55);
        moonbit_decref(dst$54);
      }
      break;
    }
  } else {
    int32_t _tmp$1788 = len$59 - 1;
    int32_t i$61 = _tmp$1788;
    while (1) {
      if (i$61 >= 0) {
        int32_t _tmp$1784 = dst_offset$56 + i$61;
        int32_t _tmp$1786 = src_offset$57 + i$61;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$4419;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _tmp$1785;
        struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit* _old$4418;
        int32_t _tmp$1787;
        if (_tmp$1786 < 0 || _tmp$1786 >= Moonbit_array_length(src$55)) {
          moonbit_panic();
        }
        _tmp$4419
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)src$55[
            _tmp$1786
          ];
        _tmp$1785 = _tmp$4419;
        if (_tmp$1784 < 0 || _tmp$1784 >= Moonbit_array_length(dst$54)) {
          moonbit_panic();
        }
        _old$4418
        = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit*)dst$54[
            _tmp$1784
          ];
        if (_tmp$1785) {
          moonbit_incref(_tmp$1785);
        }
        if (_old$4418) {
          moonbit_decref(_old$4418);
        }
        dst$54[_tmp$1784] = _tmp$1785;
        _tmp$1787 = i$61 - 1;
        i$61 = _tmp$1787;
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
  int32_t _if_result$5087;
  if (dst$45 == src$46) {
    _if_result$5087 = dst_offset$47 < src_offset$48;
  } else {
    _if_result$5087 = 0;
  }
  if (_if_result$5087) {
    int32_t i$49 = 0;
    while (1) {
      if (i$49 < len$50) {
        int32_t _tmp$1771 = dst_offset$47 + i$49;
        int32_t _tmp$1773 = src_offset$48 + i$49;
        struct $$3c$String$2a$Int$3e$* _tmp$4421;
        struct $$3c$String$2a$Int$3e$* _tmp$1772;
        struct $$3c$String$2a$Int$3e$* _old$4420;
        int32_t _tmp$1774;
        if (_tmp$1773 < 0 || _tmp$1773 >= Moonbit_array_length(src$46)) {
          moonbit_panic();
        }
        _tmp$4421 = (struct $$3c$String$2a$Int$3e$*)src$46[_tmp$1773];
        _tmp$1772 = _tmp$4421;
        if (_tmp$1771 < 0 || _tmp$1771 >= Moonbit_array_length(dst$45)) {
          moonbit_panic();
        }
        _old$4420 = (struct $$3c$String$2a$Int$3e$*)dst$45[_tmp$1771];
        if (_tmp$1772) {
          moonbit_incref(_tmp$1772);
        }
        if (_old$4420) {
          moonbit_decref(_old$4420);
        }
        dst$45[_tmp$1771] = _tmp$1772;
        _tmp$1774 = i$49 + 1;
        i$49 = _tmp$1774;
        continue;
      } else {
        moonbit_decref(src$46);
        moonbit_decref(dst$45);
      }
      break;
    }
  } else {
    int32_t _tmp$1779 = len$50 - 1;
    int32_t i$52 = _tmp$1779;
    while (1) {
      if (i$52 >= 0) {
        int32_t _tmp$1775 = dst_offset$47 + i$52;
        int32_t _tmp$1777 = src_offset$48 + i$52;
        struct $$3c$String$2a$Int$3e$* _tmp$4423;
        struct $$3c$String$2a$Int$3e$* _tmp$1776;
        struct $$3c$String$2a$Int$3e$* _old$4422;
        int32_t _tmp$1778;
        if (_tmp$1777 < 0 || _tmp$1777 >= Moonbit_array_length(src$46)) {
          moonbit_panic();
        }
        _tmp$4423 = (struct $$3c$String$2a$Int$3e$*)src$46[_tmp$1777];
        _tmp$1776 = _tmp$4423;
        if (_tmp$1775 < 0 || _tmp$1775 >= Moonbit_array_length(dst$45)) {
          moonbit_panic();
        }
        _old$4422 = (struct $$3c$String$2a$Int$3e$*)dst$45[_tmp$1775];
        if (_tmp$1776) {
          moonbit_incref(_tmp$1776);
        }
        if (_old$4422) {
          moonbit_decref(_old$4422);
        }
        dst$45[_tmp$1775] = _tmp$1776;
        _tmp$1778 = i$52 - 1;
        i$52 = _tmp$1778;
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
  int32_t _if_result$5090;
  if (dst$36 == src$37) {
    _if_result$5090 = dst_offset$38 < src_offset$39;
  } else {
    _if_result$5090 = 0;
  }
  if (_if_result$5090) {
    int32_t i$40 = 0;
    while (1) {
      if (i$40 < len$41) {
        int32_t _tmp$1762 = dst_offset$38 + i$40;
        int32_t _tmp$1764 = src_offset$39 + i$40;
        moonbit_string_t _tmp$4425;
        moonbit_string_t _tmp$1763;
        moonbit_string_t _old$4424;
        int32_t _tmp$1765;
        if (_tmp$1764 < 0 || _tmp$1764 >= Moonbit_array_length(src$37)) {
          moonbit_panic();
        }
        _tmp$4425 = (moonbit_string_t)src$37[_tmp$1764];
        _tmp$1763 = _tmp$4425;
        if (_tmp$1762 < 0 || _tmp$1762 >= Moonbit_array_length(dst$36)) {
          moonbit_panic();
        }
        _old$4424 = (moonbit_string_t)dst$36[_tmp$1762];
        moonbit_incref(_tmp$1763);
        moonbit_decref(_old$4424);
        dst$36[_tmp$1762] = _tmp$1763;
        _tmp$1765 = i$40 + 1;
        i$40 = _tmp$1765;
        continue;
      } else {
        moonbit_decref(src$37);
        moonbit_decref(dst$36);
      }
      break;
    }
  } else {
    int32_t _tmp$1770 = len$41 - 1;
    int32_t i$43 = _tmp$1770;
    while (1) {
      if (i$43 >= 0) {
        int32_t _tmp$1766 = dst_offset$38 + i$43;
        int32_t _tmp$1768 = src_offset$39 + i$43;
        moonbit_string_t _tmp$4427;
        moonbit_string_t _tmp$1767;
        moonbit_string_t _old$4426;
        int32_t _tmp$1769;
        if (_tmp$1768 < 0 || _tmp$1768 >= Moonbit_array_length(src$37)) {
          moonbit_panic();
        }
        _tmp$4427 = (moonbit_string_t)src$37[_tmp$1768];
        _tmp$1767 = _tmp$4427;
        if (_tmp$1766 < 0 || _tmp$1766 >= Moonbit_array_length(dst$36)) {
          moonbit_panic();
        }
        _old$4426 = (moonbit_string_t)dst$36[_tmp$1766];
        moonbit_incref(_tmp$1767);
        moonbit_decref(_old$4426);
        dst$36[_tmp$1766] = _tmp$1767;
        _tmp$1769 = i$43 - 1;
        i$43 = _tmp$1769;
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
  int32_t _if_result$5093;
  if (dst$27 == src$28) {
    _if_result$5093 = dst_offset$29 < src_offset$30;
  } else {
    _if_result$5093 = 0;
  }
  if (_if_result$5093) {
    int32_t i$31 = 0;
    while (1) {
      if (i$31 < len$32) {
        int32_t _tmp$1753 = dst_offset$29 + i$31;
        int32_t _tmp$1755 = src_offset$30 + i$31;
        int32_t _tmp$1754;
        int32_t _tmp$1756;
        if (_tmp$1755 < 0 || _tmp$1755 >= Moonbit_array_length(src$28)) {
          moonbit_panic();
        }
        _tmp$1754 = (int32_t)src$28[_tmp$1755];
        if (_tmp$1753 < 0 || _tmp$1753 >= Moonbit_array_length(dst$27)) {
          moonbit_panic();
        }
        dst$27[_tmp$1753] = _tmp$1754;
        _tmp$1756 = i$31 + 1;
        i$31 = _tmp$1756;
        continue;
      } else {
        moonbit_decref(src$28);
        moonbit_decref(dst$27);
      }
      break;
    }
  } else {
    int32_t _tmp$1761 = len$32 - 1;
    int32_t i$34 = _tmp$1761;
    while (1) {
      if (i$34 >= 0) {
        int32_t _tmp$1757 = dst_offset$29 + i$34;
        int32_t _tmp$1759 = src_offset$30 + i$34;
        int32_t _tmp$1758;
        int32_t _tmp$1760;
        if (_tmp$1759 < 0 || _tmp$1759 >= Moonbit_array_length(src$28)) {
          moonbit_panic();
        }
        _tmp$1758 = (int32_t)src$28[_tmp$1759];
        if (_tmp$1757 < 0 || _tmp$1757 >= Moonbit_array_length(dst$27)) {
          moonbit_panic();
        }
        dst$27[_tmp$1757] = _tmp$1758;
        _tmp$1760 = i$34 - 1;
        i$34 = _tmp$1760;
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
  moonbit_string_t _tmp$1752 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$25);
  moonbit_string_t _tmp$1750 =
    moonbit_add_string(
      _tmp$1752, (moonbit_string_t)moonbit_string_literal_268.data
    );
  moonbit_string_t _tmp$1751 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$26);
  moonbit_string_t _tmp$1749 = moonbit_add_string(_tmp$1750, _tmp$1751);
  moonbit_string_t _tmp$1748 =
    moonbit_add_string(
      _tmp$1749, (moonbit_string_t)moonbit_string_literal_269.data
    );
  return $moonbitlang$core$abort$abort$3(_tmp$1748);
}

struct $StringView $moonbitlang$core$builtin$abort$2(
  moonbit_string_t string$23,
  moonbit_string_t loc$24
) {
  moonbit_string_t _tmp$1747 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$23);
  moonbit_string_t _tmp$1745 =
    moonbit_add_string(
      _tmp$1747, (moonbit_string_t)moonbit_string_literal_268.data
    );
  moonbit_string_t _tmp$1746 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$24);
  moonbit_string_t _tmp$1744 = moonbit_add_string(_tmp$1745, _tmp$1746);
  moonbit_string_t _tmp$1743 =
    moonbit_add_string(
      _tmp$1744, (moonbit_string_t)moonbit_string_literal_269.data
    );
  return $moonbitlang$core$abort$abort$2(_tmp$1743);
}

int32_t $moonbitlang$core$builtin$abort$1(
  moonbit_string_t string$21,
  moonbit_string_t loc$22
) {
  moonbit_string_t _tmp$1742 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$21);
  moonbit_string_t _tmp$1740 =
    moonbit_add_string(
      _tmp$1742, (moonbit_string_t)moonbit_string_literal_268.data
    );
  moonbit_string_t _tmp$1741 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$22);
  moonbit_string_t _tmp$1739 = moonbit_add_string(_tmp$1740, _tmp$1741);
  moonbit_string_t _tmp$1738 =
    moonbit_add_string(
      _tmp$1739, (moonbit_string_t)moonbit_string_literal_269.data
    );
  return $moonbitlang$core$abort$abort$1(_tmp$1738);
}

int32_t $moonbitlang$core$builtin$abort$0(
  moonbit_string_t string$19,
  moonbit_string_t loc$20
) {
  moonbit_string_t _tmp$1737 =
    $$moonbitlang$core$builtin$Show$$String$$to_string(string$19);
  moonbit_string_t _tmp$1735 =
    moonbit_add_string(
      _tmp$1737, (moonbit_string_t)moonbit_string_literal_268.data
    );
  moonbit_string_t _tmp$1736 =
    $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$1(loc$20);
  moonbit_string_t _tmp$1734 = moonbit_add_string(_tmp$1735, _tmp$1736);
  moonbit_string_t _tmp$1733 =
    moonbit_add_string(
      _tmp$1734, (moonbit_string_t)moonbit_string_literal_269.data
    );
  $moonbitlang$core$abort$abort$0(_tmp$1733);
  return 0;
}

int32_t $$moonbitlang$core$builtin$Show$$$moonbitlang$core$builtin$Failure$$output(
  void* _x_5271$15,
  struct $$moonbitlang$core$builtin$Logger _x_5272$18
) {
  struct $Error$moonbitlang$core$builtin$Failure$Failure* _Failure$16 =
    (struct $Error$moonbitlang$core$builtin$Failure$Failure*)_x_5271$15;
  moonbit_string_t _field$4428 = _Failure$16->$0;
  int32_t _cnt$4556 = Moonbit_object_header(_Failure$16)->rc;
  moonbit_string_t _$2a$err_payload_5273$17;
  struct $$moonbitlang$core$builtin$Logger _bind$1732;
  if (_cnt$4556 > 1) {
    int32_t _new_cnt$4557;
    moonbit_incref(_field$4428);
    _new_cnt$4557 = _cnt$4556 - 1;
    Moonbit_object_header(_Failure$16)->rc = _new_cnt$4557;
  } else if (_cnt$4556 == 1) {
    moonbit_free(_Failure$16);
  }
  _$2a$err_payload_5273$17 = _field$4428;
  if (_x_5272$18.$1) {
    moonbit_incref(_x_5272$18.$1);
  }
  _x_5272$18.$0->$method_0(
    _x_5272$18.$1, (moonbit_string_t)moonbit_string_literal_270.data
  );
  if (_x_5272$18.$1) {
    moonbit_incref(_x_5272$18.$1);
  }
  $$moonbitlang$core$builtin$Logger$$write_object$1(
    _x_5272$18, _$2a$err_payload_5273$17
  );
  _bind$1732 = _x_5272$18;
  _bind$1732.$0->$method_0(
    _bind$1732.$1, (moonbit_string_t)moonbit_string_literal_248.data
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
        _x_5286$14.$1, (moonbit_string_t)moonbit_string_literal_271.data
      );
      break;
    }
    default: {
      moonbit_decref(_x_5285$13);
      _x_5286$14.$0->$method_0(
        _x_5286$14.$1, (moonbit_string_t)moonbit_string_literal_272.data
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

moonbit_string_t $Error$to_string(void* _e$1643) {
  switch (Moonbit_object_tag(_e$1643)) {
    case 0: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
               _e$1643
             );
      break;
    }
    
    case 1: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$5(
               _e$1643
             );
      break;
    }
    
    case 4: {
      moonbit_decref(_e$1643);
      return (moonbit_string_t)moonbit_string_literal_273.data;
      break;
    }
    
    case 2: {
      return $$moonbitlang$core$builtin$Show$$$default_impl$$to_string$4(
               _e$1643
             );
      break;
    }
    
    case 5: {
      moonbit_decref(_e$1643);
      return (moonbit_string_t)moonbit_string_literal_274.data;
      break;
    }
    default: {
      moonbit_decref(_e$1643);
      return (moonbit_string_t)moonbit_string_literal_275.data;
      break;
    }
  }
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1661,
  int32_t _param$1660
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1659 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1661;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_char(
    _self$1659, _param$1660
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1658,
  struct $StringView _param$1657
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1656 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1658;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_view(
    _self$1656, _param$1657
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$dyncall_as_$moonbitlang$core$builtin$Logger$0(
  void* _obj_ptr$1655,
  moonbit_string_t _param$1652,
  int32_t _param$1653,
  int32_t _param$1654
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1651 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1655;
  $$moonbitlang$core$builtin$Logger$$$default_impl$$write_substring$0(
    _self$1651, _param$1652, _param$1653, _param$1654
  );
  return 0;
}

int32_t $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string$dyncall_as_$moonbitlang$core$builtin$Logger(
  void* _obj_ptr$1650,
  moonbit_string_t _param$1649
) {
  struct $$moonbitlang$core$builtin$StringBuilder* _self$1648 =
    (struct $$moonbitlang$core$builtin$StringBuilder*)_obj_ptr$1650;
  $$moonbitlang$core$builtin$Logger$$$moonbitlang$core$builtin$StringBuilder$$write_string(
    _self$1648, _param$1649
  );
  return 0;
}

void moonbit_init() {
  moonbit_string_t* _tmp$1727;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1726;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1725;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1672;
  moonbit_string_t* _tmp$1724;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1723;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1722;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1673;
  moonbit_string_t* _tmp$1721;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1720;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1719;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1674;
  moonbit_string_t* _tmp$1718;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1717;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1716;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1675;
  moonbit_string_t* _tmp$1715;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1714;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1713;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1676;
  moonbit_string_t* _tmp$1712;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1711;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1710;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1677;
  moonbit_string_t* _tmp$1709;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1708;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1707;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1678;
  moonbit_string_t* _tmp$1706;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1705;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1704;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1679;
  moonbit_string_t* _tmp$1703;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1702;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1701;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1680;
  moonbit_string_t* _tmp$1700;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1699;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1698;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1681;
  moonbit_string_t* _tmp$1697;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1696;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1695;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1682;
  moonbit_string_t* _tmp$1694;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1693;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1692;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1683;
  moonbit_string_t* _tmp$1691;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1690;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1689;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1684;
  moonbit_string_t* _tmp$1688;
  struct $$moonbitlang$core$builtin$Array$3c$String$3e$* _tmp$1687;
  struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$* _tuple$1686;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tuple$1685;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _bind$1467;
  struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$** _tmp$1671;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$ _tmp$1670;
  struct $$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$* _tmp$1669;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$* _tuple$1668;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1466;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1667;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1666;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1469;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1729;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1728;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _bind$1468;
  struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$** _tmp$1731;
  struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$ _tmp$1730;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7$dyncall$closure.data;
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10$clo
  = (struct $$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$*)&$azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10$dyncall$closure.data;
  $moonbitlang$core$builtin$boyer_moore_horspool_find$constr$161 = (int64_t)0;
  $moonbitlang$core$builtin$brute_force_find$constr$175 = (int64_t)0;
  _tmp$1727 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1727[0] = (moonbit_string_t)moonbit_string_literal_276.data;
  _tmp$1726
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1726)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1726->$0 = _tmp$1727;
  _tmp$1726->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0$clo
  );
  _tuple$1725
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1725)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1725->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_0$clo;
  _tuple$1725->$1 = _tmp$1726;
  _tuple$1672
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1672)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1672->$0 = 0;
  _tuple$1672->$1 = _tuple$1725;
  _tmp$1724 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1724[0] = (moonbit_string_t)moonbit_string_literal_277.data;
  _tmp$1723
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1723)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1723->$0 = _tmp$1724;
  _tmp$1723->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1$clo
  );
  _tuple$1722
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1722)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1722->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_1$clo;
  _tuple$1722->$1 = _tmp$1723;
  _tuple$1673
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1673)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1673->$0 = 1;
  _tuple$1673->$1 = _tuple$1722;
  _tmp$1721 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1721[0] = (moonbit_string_t)moonbit_string_literal_278.data;
  _tmp$1720
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1720)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1720->$0 = _tmp$1721;
  _tmp$1720->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2$clo
  );
  _tuple$1719
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1719)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1719->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_2$clo;
  _tuple$1719->$1 = _tmp$1720;
  _tuple$1674
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1674)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1674->$0 = 2;
  _tuple$1674->$1 = _tuple$1719;
  _tmp$1718 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1718[0] = (moonbit_string_t)moonbit_string_literal_279.data;
  _tmp$1717
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1717)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1717->$0 = _tmp$1718;
  _tmp$1717->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3$clo
  );
  _tuple$1716
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1716)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1716->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_3$clo;
  _tuple$1716->$1 = _tmp$1717;
  _tuple$1675
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1675)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1675->$0 = 3;
  _tuple$1675->$1 = _tuple$1716;
  _tmp$1715 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1715[0] = (moonbit_string_t)moonbit_string_literal_280.data;
  _tmp$1714
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1714)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1714->$0 = _tmp$1715;
  _tmp$1714->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4$clo
  );
  _tuple$1713
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1713)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1713->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_4$clo;
  _tuple$1713->$1 = _tmp$1714;
  _tuple$1676
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1676)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1676->$0 = 4;
  _tuple$1676->$1 = _tuple$1713;
  _tmp$1712 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1712[0] = (moonbit_string_t)moonbit_string_literal_281.data;
  _tmp$1711
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1711)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1711->$0 = _tmp$1712;
  _tmp$1711->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5$clo
  );
  _tuple$1710
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1710)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1710->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_5$clo;
  _tuple$1710->$1 = _tmp$1711;
  _tuple$1677
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1677)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1677->$0 = 5;
  _tuple$1677->$1 = _tuple$1710;
  _tmp$1709 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1709[0] = (moonbit_string_t)moonbit_string_literal_282.data;
  _tmp$1708
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1708)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1708->$0 = _tmp$1709;
  _tmp$1708->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6$clo
  );
  _tuple$1707
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1707)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1707->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_6$clo;
  _tuple$1707->$1 = _tmp$1708;
  _tuple$1678
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1678)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1678->$0 = 6;
  _tuple$1678->$1 = _tuple$1707;
  _tmp$1706 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1706[0] = (moonbit_string_t)moonbit_string_literal_283.data;
  _tmp$1705
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1705)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1705->$0 = _tmp$1706;
  _tmp$1705->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7$clo
  );
  _tuple$1704
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1704)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1704->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_7$clo;
  _tuple$1704->$1 = _tmp$1705;
  _tuple$1679
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1679)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1679->$0 = 7;
  _tuple$1679->$1 = _tuple$1704;
  _tmp$1703 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1703[0] = (moonbit_string_t)moonbit_string_literal_284.data;
  _tmp$1702
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1702)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1702->$0 = _tmp$1703;
  _tmp$1702->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8$clo
  );
  _tuple$1701
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1701)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1701->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_8$clo;
  _tuple$1701->$1 = _tmp$1702;
  _tuple$1680
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1680)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1680->$0 = 8;
  _tuple$1680->$1 = _tuple$1701;
  _tmp$1700 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1700[0] = (moonbit_string_t)moonbit_string_literal_285.data;
  _tmp$1699
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1699)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1699->$0 = _tmp$1700;
  _tmp$1699->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9$clo
  );
  _tuple$1698
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1698)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1698->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_9$clo;
  _tuple$1698->$1 = _tmp$1699;
  _tuple$1681
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1681)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1681->$0 = 9;
  _tuple$1681->$1 = _tuple$1698;
  _tmp$1697 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1697[0] = (moonbit_string_t)moonbit_string_literal_286.data;
  _tmp$1696
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1696)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1696->$0 = _tmp$1697;
  _tmp$1696->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10$clo
  );
  _tuple$1695
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1695)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1695->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_10$clo;
  _tuple$1695->$1 = _tmp$1696;
  _tuple$1682
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1682)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1682->$0 = 10;
  _tuple$1682->$1 = _tuple$1695;
  _tmp$1694 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1694[0] = (moonbit_string_t)moonbit_string_literal_287.data;
  _tmp$1693
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1693)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1693->$0 = _tmp$1694;
  _tmp$1693->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11$clo
  );
  _tuple$1692
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1692)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1692->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_11$clo;
  _tuple$1692->$1 = _tmp$1693;
  _tuple$1683
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1683)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1683->$0 = 11;
  _tuple$1683->$1 = _tuple$1692;
  _tmp$1691 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1691[0] = (moonbit_string_t)moonbit_string_literal_288.data;
  _tmp$1690
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1690)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1690->$0 = _tmp$1691;
  _tmp$1690->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12$clo
  );
  _tuple$1689
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1689)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1689->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_12$clo;
  _tuple$1689->$1 = _tmp$1690;
  _tuple$1684
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1684)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1684->$0 = 12;
  _tuple$1684->$1 = _tuple$1689;
  _tmp$1688 = (moonbit_string_t*)moonbit_make_ref_array_raw(1);
  _tmp$1688[0] = (moonbit_string_t)moonbit_string_literal_289.data;
  _tmp$1687
  = (struct $$moonbitlang$core$builtin$Array$3c$String$3e$*)moonbit_malloc(
      sizeof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$)
    );
  Moonbit_object_header(_tmp$1687)->meta
  = Moonbit_make_regular_object_header(
    offsetof(struct $$moonbitlang$core$builtin$Array$3c$String$3e$, $0) >> 2,
      1,
      0
  );
  _tmp$1687->$0 = _tmp$1688;
  _tmp$1687->$1 = 1;
  moonbit_incref(
    $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13$clo
  );
  _tuple$1686
  = (struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1686)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1686->$0
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$__test_636f6d70726568656e736976655f746573742e6d6274_13$clo;
  _tuple$1686->$1 = _tmp$1687;
  _tuple$1685
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1685)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$,
        $1
    )
    >> 2,
      1,
      0
  );
  _tuple$1685->$0 = 13;
  _tuple$1685->$1 = _tuple$1686;
  _bind$1467
  = (struct $$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$MultiValueResult$3c$Unit$2b$Error$3e$$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      14
    );
  _bind$1467[0] = _tuple$1672;
  _bind$1467[1] = _tuple$1673;
  _bind$1467[2] = _tuple$1674;
  _bind$1467[3] = _tuple$1675;
  _bind$1467[4] = _tuple$1676;
  _bind$1467[5] = _tuple$1677;
  _bind$1467[6] = _tuple$1678;
  _bind$1467[7] = _tuple$1679;
  _bind$1467[8] = _tuple$1680;
  _bind$1467[9] = _tuple$1681;
  _bind$1467[10] = _tuple$1682;
  _bind$1467[11] = _tuple$1683;
  _bind$1467[12] = _tuple$1684;
  _bind$1467[13] = _tuple$1685;
  _tmp$1671 = _bind$1467;
  _tmp$1670
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$){
    0, 14, _tmp$1671
  };
  _tmp$1669 = $$moonbitlang$core$builtin$Map$$from_array$1(_tmp$1670);
  _tuple$1668
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$*)moonbit_malloc(
      sizeof(
        struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$
      )
    );
  Moonbit_object_header(_tuple$1668)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$,
        $0
    )
    >> 2,
      2,
      0
  );
  _tuple$1668->$0 = (moonbit_string_t)moonbit_string_literal_290.data;
  _tuple$1668->$1 = _tmp$1669;
  _bind$1466
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_make_ref_array_raw(
      1
    );
  _bind$1466[0] = _tuple$1668;
  _tmp$1667 = _bind$1466;
  _tmp$1666
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 1, _tmp$1667
  };
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_no_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$0(
    _tmp$1666
  );
  _bind$1469
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1729 = _bind$1469;
  _tmp$1728
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$async$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1729
  };
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_async_tests
  = $$moonbitlang$core$builtin$Map$$from_array$3(
    _tmp$1728
  );
  _bind$1468
  = (struct $$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$**)moonbit_empty_ref_array;
  _tmp$1731 = _bind$1468;
  _tmp$1730
  = (struct $$moonbitlang$core$builtin$ArrayView$3c$$3c$String$2a$$moonbitlang$core$builtin$Map$3c$Int$2a$$3c$$3c$Int$3e$$3d$$3e$Unit$21$Error$2a$$moonbitlang$core$builtin$Array$3c$String$3e$$3e$$3e$$3e$$3e$){
    0, 0, _tmp$1731
  };
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_with_args_tests
  = $$moonbitlang$core$builtin$Map$$from_array$2(
    _tmp$1730
  );
}

int main(int argc, char** argv) {
  struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit** _tmp$1665;
  struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$* async_tests$1637;
  struct $$moonbitlang$core$builtin$Array$3c$$3c$String$2a$Int$3e$$3e$* _arr$1638;
  int32_t _len$1639;
  int32_t _i$1640;
  moonbit_runtime_init(argc, argv);
  moonbit_init();
  _tmp$1665
  = (struct $$3c$$3c$Unit$3e$$3d$$3e$Unit$2a$$3c$Error$3e$$3d$$3e$Unit$3e$$3d$$3e$Unit**)moonbit_empty_ref_array;
  async_tests$1637
  = (struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$*)moonbit_malloc(
      sizeof(
        struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$
      )
    );
  Moonbit_object_header(async_tests$1637)->meta
  = Moonbit_make_regular_object_header(
    offsetof(
      struct $$moonbitlang$core$builtin$Array$3c$async$3c$$3e$$3d$$3e$Unit$21$Error$3e$,
        $0
    )
    >> 2,
      1,
      0
  );
  async_tests$1637->$0 = _tmp$1665;
  async_tests$1637->$1 = 0;
  _arr$1638
  = $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_native_parse_args();
  moonbit_incref(_arr$1638);
  _len$1639 = $$moonbitlang$core$builtin$Array$$length$0(_arr$1638);
  _i$1640 = 0;
  while (1) {
    if (_i$1640 < _len$1639) {
      struct $$3c$String$2a$Int$3e$* arg$1641;
      moonbit_string_t _field$4430;
      moonbit_string_t _tmp$1662;
      int32_t _field$4429;
      int32_t _cnt$4558;
      int32_t _tmp$1663;
      int32_t _tmp$1664;
      moonbit_incref(_arr$1638);
      arg$1641
      = $$moonbitlang$core$builtin$Array$$unsafe_get$0(
        _arr$1638, _i$1640
      );
      _field$4430 = arg$1641->$0;
      _tmp$1662 = _field$4430;
      _field$4429 = arg$1641->$1;
      _cnt$4558 = Moonbit_object_header(arg$1641)->rc;
      if (_cnt$4558 > 1) {
        int32_t _new_cnt$4559;
        moonbit_incref(_tmp$1662);
        _new_cnt$4559 = _cnt$4558 - 1;
        Moonbit_object_header(arg$1641)->rc = _new_cnt$4559;
      } else if (_cnt$4558 == 1) {
        moonbit_free(arg$1641);
      }
      _tmp$1663 = _field$4429;
      moonbit_incref(async_tests$1637);
      $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_do_execute(
        async_tests$1637, _tmp$1662, _tmp$1663
      );
      _tmp$1664 = _i$1640 + 1;
      _i$1640 = _tmp$1664;
      continue;
    } else {
      moonbit_decref(_arr$1638);
    }
    break;
  }
  $azimuth$telemetry$tests$azimuth$telemetry$comprehensive_blackbox_test$moonbit_test_driver_internal_run_async_tests(
    async_tests$1637
  );
  return 0;
}