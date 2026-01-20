// 新增的标准 MoonBit 测试用例
// 覆盖边界情况和特殊场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值和最小值的特殊处理
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, add(max_val - 1000, 1001))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最小值的特殊处理
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(0, multiply(min_val, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法的各种情况
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(3, add(-5, 8))
  assert_eq(-2147483648, add(-2147483647, -1))
  assert_eq(-2147483648, add(-1000000000, -1147483648))
}

test "multiply_function_large_numbers" {
  // 测试大数乘法
  assert_eq(2147483647, multiply(46340, 46341))
  assert_eq(-2147483648, multiply(-46340, 46341))
  assert_eq(-2147483648, multiply(46340, -46341))
  assert_eq(2147483647, multiply(-46340, -46341))
}

test "greet_function_special_cases" {
  // 测试 greet 函数的特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(-4, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(3, divide_with_ceil(5, 2))
}

test "combined_operations" {
  // 测试组合操作
  let a = 10
  let b = 20
  let c = -5
  
  assert_eq(30, add(a, b))
  assert_eq(25, add(multiply(a, b), divide_with_ceil(c, 2)))
  assert_eq(-200, multiply(a, c))
  assert_eq(15, add(multiply(a, 2), divide_with_ceil(b, 4)))
}

test "assert_functions_comprehensive" {
  // 全面测试断言函数
  assert_true(1 == 1)
  assert_true(0 != 1)
  assert_true(true)
  assert_false(false)
  assert_false(1 == 0)
  assert_false(false == true)
  assert_eq(100, 100)
  assert_eq(-100, -100)
  assert_eq_string("MoonBit", "MoonBit")
  assert_eq_string("", "")
}

test "new_custom_test_case" {
  // 新增的自定义测试用例
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_true(5 > 3)
  assert_false(3 > 5)
}