(data $moonbit.const_data "!\00H\00e\00l\00l\00o\00,\00 \00")
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
(global $moonbit.string_pool
 (ref $moonbit.string_pool_type)
 (i32.const 2)
 (array.new_default $moonbit.string_pool_type)
)
(func $assert_false_wrapper/1 (param $condition/15 i32)
 (drop
  (call $assert_false
   (local.get $condition/15))))
(export "assert_false" (func $assert_false_wrapper/1))
(func $assert_false (param $condition/15 i32) (result i32)
 (i32.const 0))
(func $assert_true_wrapper/2 (param $condition/14 i32)
 (drop
  (call $assert_true
   (local.get $condition/14))))
(export "assert_true" (func $assert_true_wrapper/2))
(func $assert_true (param $condition/14 i32) (result i32)
 (i32.const 0))
(func $assert_eq_string_wrapper/3 (param $expected/12 (ref $moonbit.string)) (param $actual/13 (ref $moonbit.string))
 (drop
  (call $assert_eq_string
   (local.get $expected/12)
   (local.get $actual/13))))
(export "assert_eq_string" (func $assert_eq_string_wrapper/3))
(func $assert_eq_string (param $expected/12 (ref $moonbit.string)) (param $actual/13 (ref $moonbit.string)) (result i32)
 (i32.const 0))
(func $assert_eq_wrapper/4 (param $expected/10 i32) (param $actual/11 i32)
 (drop
  (call $assert_eq
   (local.get $expected/10)
   (local.get $actual/11))))
(export "assert_eq" (func $assert_eq_wrapper/4))
(func $assert_eq (param $expected/10 i32) (param $actual/11 i32) (result i32)
 (i32.const 0))
(func $divide_with_ceil (param $a/8 i32) (param $b/6 i32) (result i32)
 (local $quotient/7 i32)
 (local $remainder/9 i32)
 (if (result i32)
  (i32.eq
   (local.get $b/6)
   (i32.const 0))
  (then
   (i32.const 0))
  (else
   (local.set $quotient/7
    (i32.div_s
     (local.get $a/8)
     (local.get $b/6)))
   (if (result i32)
    (i32.eq
     (local.tee $remainder/9
      (i32.rem_s
       (local.get $a/8)
       (local.get $b/6)))
     (i32.const 0))
    (then
     (local.get $quotient/7))
    (else
     (if (result i32)
      (if (result i32)
       (i32.gt_s
        (local.get $a/8)
        (i32.const 0))
       (then
        (i32.gt_s
         (local.get $b/6)
         (i32.const 0)))
       (else
        (i32.const 0)))
      (then
       (i32.add
        (local.get $quotient/7)
        (i32.const 1)))
      (else
       (if (result i32)
        (if (result i32)
         (i32.lt_s
          (local.get $a/8)
          (i32.const 0))
         (then
          (i32.lt_s
           (local.get $b/6)
           (i32.const 0)))
         (else
          (i32.const 0)))
        (then
         (i32.add
          (local.get $quotient/7)
          (i32.const 1)))
        (else
         (local.get $quotient/7))))))))))
(export "divide_with_ceil" (func $divide_with_ceil))
(func $greet (param $name/5 (ref $moonbit.string)) (result (ref $moonbit.string))
 (call $moonbit.add_string
  (call $moonbit.add_string
   (call $moonbit.string_literal
    (i32.const 1)
    (i32.const 2)
    (i32.const 7))
   (local.get $name/5))
  (call $moonbit.string_literal
   (i32.const 0)
   (i32.const 0)
   (i32.const 1))))
(export "greet" (func $greet))
(func $multiply (param $a/3 i32) (param $b/4 i32) (result i32)
 (i32.mul
  (local.get $a/3)
  (local.get $b/4)))
(export "multiply" (func $multiply))
(func $add (param $a/1 i32) (param $b/2 i32) (result i32)
 (i32.add
  (local.get $a/1)
  (local.get $b/2)))
(export "add" (func $add))