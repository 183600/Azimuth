// 额外的测试用例 - 使用标准 MoonBit 测试语法
// 覆盖各种边界情况和组合运算

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
}

test "add_min_value_special_cases" {
  // 测试最小值的特殊情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-25, @azimuth.multiply(-5, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "greet_unicode_and_spaces" {
  // 测试包含Unicode和空格的问候
  @azimuth.assert_eq_string("Hello, MoonBit 中文!", @azimuth.greet("MoonBit 中文"))
  @azimuth.assert_eq_string("Hello,   multiple   spaces   !", @azimuth.greet("  multiple   spaces  "))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "assert_functions_various_types" {
  // 测试各种断言函数
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 < 100)
  @azimuth.assert_false(1 == 2)
  @azimuth.assert_false(100 < 0)
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("", "")
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算: ((a + b) * c) + (d * e)
  let a = 10
  let b = 20
  let c = 3
  let d = 4
  let e = 5
  
  let step1 = @azimuth.add(a, b)      // 30
  let step2 = @azimuth.multiply(step1, c)  // 90
  let step3 = @azimuth.multiply(d, e)      // 20
  let result = @azimuth.add(step2, step3)  // 110
  
  @azimuth.assert_eq(110, result)
}

test "edge_case_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "sequential_operations" {
  // 测试连续操作
  let value = 5
  
  // 连续加法
  value = @azimuth.add(value, 3)  // 8
  value = @azimuth.add(value, 2)  // 10
  value = @azimuth.add(value, -5) // 5
  @azimuth.assert_eq(5, value)
  
  // 连续乘法
  value = @azimuth.multiply(value, 2)  // 10
  value = @azimuth.multiply(value, 3)  // 30
  value = @azimuth.multiply(value, 0)  // 0
  @azimuth.assert_eq(0, value)
}