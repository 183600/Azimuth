// Azimuth 综合测试套件
// 覆盖核心功能和边界情况的标准 MoonBit 测试用例

test "add_function_negative_numbers" {
  // 测试负数加法运算
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(-2, add(-7, 5))
  assert_eq(-100, add(-50, -50))
  assert_eq(3, add(-10, 13))
}

test "multiply_function_large_numbers" {
  // 测试大数乘法运算
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(999999, multiply(999, 1001))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
  assert_eq(0, multiply(999999, 0))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符和符号处理
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, #hashtag!", greet("#hashtag"))
  assert_eq_string("Hello, $money!", greet("$money"))
  assert_eq_string("Hello, %percent!", greet("%percent"))
  assert_eq_string("Hello, &ampersand!", greet("&ampersand"))
}

test "divide_with_ceil_negative_division" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(0, divide_with_ceil(-1, -2))
}

test "subtract_function_zero_cases" {
  // 测试减法函数的零值情况
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
  assert_eq(0, subtract(0, 0))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
}

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  let a = 15
  let b = 3
  
  // 测试基本运算的组合
  assert_eq(18, add(a, b))
  assert_eq(45, multiply(a, b))
  assert_eq(12, subtract(a, b))
  assert_eq(5, divide_with_ceil(a, b))
  
  // 验证运算关系
  assert_eq(a, add(subtract(a, b), b))
  assert_eq(a, divide_with_ceil(multiply(a, b), b))
}

test "edge_case_boundary_values" {
  // 边界值测试
  assert_eq(2147483647, add(2147483646, 1))  // 接近 Int 最大值
  assert_eq(-2147483648, subtract(-2147483647, 1))  // 接近 Int 最小值
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))  // 大数相除
  assert_eq(0, multiply(2147483647, 0))  // 最大值乘零
  assert_eq_string("Hello, !", greet(""))  // 空字符串
}

test "error_handling_division_by_zero" {
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
}