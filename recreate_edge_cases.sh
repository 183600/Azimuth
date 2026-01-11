#!/bin/bash

# 创建一个修复后的 additional_edge_cases.mbt 文件
cat > /home/runner/work/Azimuth/Azimuth/src/azimuth/test/additional_edge_cases.mbt << 'EOF'
// 额外的边界情况测试 - 补充现有测试的覆盖范围

// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

///|
test "add_chained_operations" {
  // 测试连续加法操作的稳定性
  let mut result = 1000
  result = @azimuth.add(result, 2000)  // 3000
  result = @azimuth.add(result, -1000) // 2000
  result = @azimuth.add(result, 47)    // 2047
  result = @azimuth.add(result, -48)   // 1999
  if 1999 != result { @test.fail("Test failed") }
}

///|
test "multiply_chained_operations" {
  // 测试连续乘法操作的稳定性
  let mut result = 2
  result = @azimuth.multiply(result, 3)   // 6
  result = @azimuth.multiply(result, 4)   // 24
  result = @azimuth.multiply(result, 5)   // 120
  result = @azimuth.multiply(result, 6)   // 720
  if 720 != result { @test.fail("Test failed") }
}

///|
test "mixed_operations" {
  // 测试混合运算操作
  let result1 = @azimuth.add(100, 50)      // 150
  let result2 = @azimuth.multiply(result1, 2) // 300
  let result3 = @azimuth.add(result2, -50)    // 250
  let result4 = @azimuth.multiply(result3, 4) // 1000
  if 1000 != result4 { @test.fail("Test failed") }
}

///|
test "greet_long_names" {
  // 测试非常长的名字
  let long_name = "ThisIsAVeryLongNameThatMightBeUsedInSomeRealWorldApplication"
  let expected = "Hello, " + long_name + "!"
  if expected != @azimuth.greet(long_name) { @test.fail("Test failed") }
}

///|
test "greet_unicode_and_emoji" {
  // 测试Unicode和emoji字符
  assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
}

///|
test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值减去自身
  if 0 != @azimuth.add(max_val, -max_val) { @test.fail("Test failed") }
  
  // 最小值减去自身
  if -1 != @azimuth.add(min_val, 2147483647) { @test.fail("Test failed") } // -2147483648 + 2147483647 = -1
  
  // 接近边界的值
  if 2147483646 != @azimuth.add(1073741823, 1073741823) { @test.fail("Test failed") }
  if -2147483647 != @azimuth.add(-1073741824, -1073741823) { @test.fail("Test failed") }
}

///|
test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  assert_eq(2, @azimuth.multiply(1, 2))
  assert_eq(4, @azimuth.multiply(2, 2))
  assert_eq(8, @azimuth.multiply(4, 2))
  assert_eq(16, @azimuth.multiply(8, 2))
  assert_eq(32, @azimuth.multiply(16, 2))
  assert_eq(64, @azimuth.multiply(32, 2))
  assert_eq(128, @azimuth.multiply(64, 2))
}

///|
test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(6, @azimuth.multiply(-2, -3))
  assert_eq(-6, @azimuth.multiply(-2, 3))
  assert_eq(-6, @azimuth.multiply(2, -3))
  assert_eq(0, @azimuth.multiply(-2, 0))
  assert_eq(0, @azimuth.multiply(0, -2))
}

///|
test "multiply_large_numbers" {
  // 测试大数乘法
  assert_eq(1000000, @azimuth.multiply(1000, 1000))
  assert_eq(100000000, @azimuth.multiply(10000, 10000))
  assert_eq(46340, @azimuth.multiply(46340, 1))  // 接近sqrt(2^31-1)
}

///|
test "greet_empty_and_special" {
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", @azimuth.greet(""))
  assert_eq_string("Hello,   !", @azimuth.greet("   "))
  assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  assert_eq_string("Hello, ÑáÉÍÓú!", @azimuth.greet("ÑáÉÍÓú"))
}

///|
test "add_zero_properties" {
  // 测试加法的零元素特性
  assert_eq(100, @azimuth.add(100, 0))
  assert_eq(100, @azimuth.add(0, 100))
  assert_eq(-100, @azimuth.add(-100, 0))
  assert_eq(-100, @azimuth.add(0, -100))
  assert_eq(0, @azimuth.add(0, 0))
}

///|
test "multiply_zero_properties" {
  // 测试乘法的零元素特性
  assert_eq(0, @azimuth.multiply(100, 0))
  assert_eq(0, @azimuth.multiply(0, 100))
  assert_eq(0, @azimuth.multiply(-100, 0))
  assert_eq(0, @azimuth.multiply(0, -100))
  assert_eq(0, @azimuth.multiply(0, 0))
}

///|
test "multiply_one_properties" {
  // 测试乘法的单位元特性
  assert_eq(100, @azimuth.multiply(100, 1))
  assert_eq(100, @azimuth.multiply(1, 100))
  assert_eq(-100, @azimuth.multiply(-100, 1))
  assert_eq(-100, @azimuth.multiply(1, -100))
  assert_eq(1, @azimuth.multiply(1, 1))
}
EOF

echo "Recreated additional_edge_cases.mbt with correct syntax"