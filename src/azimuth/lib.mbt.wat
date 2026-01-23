(data $moonbit.const_data "!\00H\00e\00l\00l\00o\00,\00 \00S\00t\00r\00i\00n\00g\00 \00a\00s\00s\00e\00r\00t\00i\00o\00n\00 \00f\00a\00i\00l\00e\00d\00A\00s\00s\00e\00r\00t\00i\00o\00n\00 \00f\00a\00i\00l\00e\00d\00")
(type $moonbit.string
 (array (mut i16)))
(type $moonbit.string_pool_type
 (array (mut (ref null $moonbit.string))))
(func $moonbit.add_string (param $x (ref $moonbit.string)) (param $y (ref $moonbit.string)) (result (ref $moonbit.string))
 (local $lenx i32)
 (local $leny i32)
 (local $len i32)
 (local $ptr (ref $moonbit.string))
 (local.set $lenx
  (array.len
   (local.get $x)))
 (local.set $leny
  (array.len
   (local.get $y)))
 (local.set $len
  (i32.add
   (local.get $lenx)
   (local.get $leny)))
 (local.set $ptr
  (array.new_default $moonbit.string
   (local.get $len)))
 (array.copy $moonbit.string $moonbit.string
  (local.get $ptr)
  (i32.const 0)
  (local.get $x)
  (i32.const 0)
  (local.get $lenx))
 (array.copy $moonbit.string $moonbit.string
  (local.get $ptr)
  (local.get $lenx)
  (local.get $y)
  (i32.const 0)
  (local.get $leny))
 (local.get $ptr))
(func $moonbit.string_literal (param $index i32) (param $offset i32) (param $length i32) (result (ref $moonbit.string))
 (local $cached (ref null $moonbit.string))
 (local $new_string (ref $moonbit.string))
 (if
  (i32.eqz
   (ref.is_null
    (local.tee $cached
     (array.get $moonbit.string_pool_type
      (global.get $moonbit.string_pool)
      (local.get $index)))))
  (then
   (ref.as_non_null
    (local.get $cached))
   (return))
  (else))
 (local.set $new_string
  (array.new_data $moonbit.string $moonbit.const_data
   (local.get $offset)
   (local.get $length)))
 (array.set $moonbit.string_pool_type
  (global.get $moonbit.string_pool)
  (local.get $index)
  (local.get $new_string))
 (local.get $new_string))
(memory $moonbit.memory 1)
(rec
 (type $<String*String>=>Bool-sig
  (func
   (param (ref $<String*String>=>Bool)) (param (ref $moonbit.string)) (param (ref $moonbit.string))
   (result i32)))
 (type $<String*String>=>Bool
  (sub
   (struct
    (field  (mut (ref null $<String*String>=>Bool-sig)))))))
(global $moonbit.string_pool
 (ref $moonbit.string_pool_type)
 (i32.const 4)
 (array.new_default $moonbit.string_pool_type)
)
(func $divide_with_ceil (param $a/12 i32) (param $b/10 i32) (result i32)
 (local $quotient/11 i32)
 (local $remainder/13 i32)
 (if (result i32)
  (i32.eq
   (local.get $b/10)
   (i32.const 0))
  (then
   (i32.const 0))
  (else
   (local.set $quotient/11
    (i32.div_s
     (local.get $a/12)
     (local.get $b/10)))
   (if (result i32)
    (i32.eq
     (local.tee $remainder/13
      (i32.rem_s
       (local.get $a/12)
       (local.get $b/10)))
     (i32.const 0))
    (then
     (local.get $quotient/11))
    (else
     (if (result i32)
      (if (result i32)
       (i32.gt_s
        (local.get $a/12)
        (i32.const 0))
       (then
        (i32.gt_s
         (local.get $b/10)
         (i32.const 0)))
       (else
        (i32.const 0)))
      (then
       (i32.add
        (local.get $quotient/11)
        (i32.const 1)))
      (else
       (if (result i32)
        (if (result i32)
         (i32.lt_s
          (local.get $a/12)
          (i32.const 0))
         (then
          (i32.lt_s
           (local.get $b/10)
           (i32.const 0)))
         (else
          (i32.const 0)))
        (then
         (i32.add
          (local.get $quotient/11)
          (i32.const 1)))
        (else
         (local.get $quotient/11))))))))))
(export "divide_with_ceil" (func $divide_with_ceil))
(func $greet (param $name/9 (ref $moonbit.string)) (result (ref $moonbit.string))
 (call $moonbit.add_string
  (call $moonbit.add_string
   (call $moonbit.string_literal
    (i32.const 1)
    (i32.const 2)
    (i32.const 7))
   (local.get $name/9))
  (call $moonbit.string_literal
   (i32.const 0)
   (i32.const 0)
   (i32.const 1))))
(export "greet" (func $greet))
(func $multiply (param $a/7 i32) (param $b/8 i32) (result i32)
 (i32.mul
  (local.get $a/7)
  (local.get $b/8)))
(export "multiply" (func $multiply))
(func $add (param $a/5 i32) (param $b/6 i32) (result i32)
 (i32.add
  (local.get $a/5)
  (local.get $b/6)))
(export "add" (func $add))
(func $assert_eq_string_wrapper/1 (param $lhs/3 (ref $moonbit.string)) (param $rhs/4 (ref $moonbit.string))
 (drop
  (call $assert_eq_string
   (local.get $lhs/3)
   (local.get $rhs/4))))
(export "assert_eq_string" (func $assert_eq_string_wrapper/1))
(func $assert_eq_string (param $lhs/3 (ref $moonbit.string)) (param $rhs/4 (ref $moonbit.string)) (result i32)
 (if
  (call_ref $<String*String>=>Bool-sig
   (ref.as_non_null
    (global.get $@moonbitlang/core/builtin.Eq::$default_impl::not_equal|String|))
   (local.get $lhs/3)
   (local.get $rhs/4)
   (struct.get $<String*String>=>Bool 0
    (ref.as_non_null
     (global.get $@moonbitlang/core/builtin.Eq::$default_impl::not_equal|String|))))
  (then
   (drop
    (call $moonbit.string_literal
     (i32.const 2)
     (i32.const 16)
     (i32.const 23))))
  (else))
 (i32.const 0))
(func $assert_eq_wrapper/2 (param $lhs/1 i32) (param $rhs/2 i32)
 (drop
  (call $assert_eq
   (local.get $lhs/1)
   (local.get $rhs/2))))
(export "assert_eq" (func $assert_eq_wrapper/2))
(func $assert_eq (param $lhs/1 i32) (param $rhs/2 i32) (result i32)
 (local.get $lhs/1)
 (if
  (i32.ne
   (local.get $rhs/2))
  (then
   (drop
    (call $moonbit.string_literal
     (i32.const 3)
     (i32.const 62)
     (i32.const 16))))
  (else))
 (i32.const 0))