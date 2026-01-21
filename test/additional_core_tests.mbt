// 额外的核心功能测试用例
// 测试 azimuth 库的核心功能：add, multiply, greet 函数

test "add_chained_operations" {
  // 测试链式加法运算
  let result = add(add(1, 2), add(3, 4))
  assert_eq(10, result)
  
  let result2 = add(add(100, 200), add(300, 400))
  assert_eq(1000, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 3
  let c = 2
  
  let left = multiply(a, add(b, c))
  let right = add(multiply(a, b), multiply(a, c))
  assert_eq(left, right)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-12, multiply(3, -4))
}

test "zero_value_properties" {
  // 测试零值特性
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, add(0, 0))
}

test "large_number_calculations" {
  // 测试大数计算
  let large1 = 1000000
  let large2 = 2000000
  
  assert_eq(3000000, add(large1, large2))
  assert_eq(2000000000, multiply(large1, large2))
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, spaces and	tabs!", greet("spaces and	tabs"))
  assert_eq_string("Hello, new line!", greet("new line"))
}

test "mathematical_series" {
  // 测试数列计算：等差数列求和
  // 1 + 2 + 3 + 4 + 5 = 15
  let sum1 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum1)
  
  // 10 + 20 + 30 + 40 = 100
  let sum2 = add(add(add(10, 20), 30), 40)
  assert_eq(100, sum2)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息：本金 + 本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(160000, total)
}

test "temperature_conversion_extended" {
  // 测试温度转换扩展场景
  // 摄氏度转华氏度：F = C × 2 + 30 (简化版)
  
  assert_eq(30, add(multiply(0, 2), 30))
  assert_eq(50, add(multiply(10, 2), 30))
  assert_eq(70, add(multiply(20, 2), 30))
  assert_eq(90, add(multiply(30, 2), 30))
  assert_eq(110, add(multiply(40, 2), 30))
}

test "boundary_comprehensive_test" {
  // 综合边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 边界值与普通值的运算
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}