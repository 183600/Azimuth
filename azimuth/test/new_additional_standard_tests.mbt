// 新增的标准 MoonBit 测试用例
// 覆盖边界情况和特殊场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值和最小值的特殊处理
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1001))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let min_val = -2147483648
  
  // 测试最小值的特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-1, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法的各种情况
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(3, @azimuth.add(-5, 8))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1000000000, -1147483648))
}

test "multiply_function_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46340, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(46340, -46341))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(-46340, -46341))
}

test "greet_function_special_cases" {
  // 测试 greet 函数的特殊情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-4, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(5, 2))
}

test "combined_operations" {
  // 测试组合操作
  let a = 10
  let b = 20
  let c = -5
  
  @azimuth.assert_eq(30, @azimuth.add(a, b))
  @azimuth.assert_eq(25, @azimuth.add(@azimuth.multiply(a, b), @azimuth.divide_with_ceil(c, 2)))
  @azimuth.assert_eq(-200, @azimuth.multiply(a, c))
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.multiply(a, 2), @azimuth.divide_with_ceil(b, 4)))
}

test "assert_functions_comprehensive" {
  // 全面测试断言函数
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 != 1)
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(false == true)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq(-100, -100)
  @azimuth.assert_eq_string("MoonBit", "MoonBit")
  @azimuth.assert_eq_string("", "")
}