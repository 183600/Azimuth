// 综合测试套件 - 10个标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(0, 100))
}

test "add_function_overflow_protection" {
  // 测试 add 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(0, 123))
  assert_eq(100, multiply(10, 10))
}

test "multiply_function_overflow_protection" {
  // 测试 multiply 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负数溢出
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 测试最小值的特殊处理
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_comprehensive" {
  // 测试 greet 函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律: a * (b + c) = a * b + a * c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 圆的面积计算: π * r² (简化为 3 * r²)
  let radius = 10
  let area = multiply(3, multiply(radius, radius))
  assert_eq(300, area)
  
  // 简单利息计算: 本金 * (1 + 利率 * 时间)
  let principal = 1000
  let rate = 5
  let time = 2
  let interest = multiply(principal, add(1, multiply(rate, time)))
  assert_eq(11000, interest)
}

test "edge_cases_and_boundary_values" {
  // 测试边界情况和特殊值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试与1的运算
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算 (a + b) * (c - d) + e
  let a = 15
  let b = 25
  let c = 20
  let d = 8
  let e = 100
  
  let sum_ab = add(a, b)  // 40
  let diff_cd = add(c, -d)  // 12
  let product = multiply(sum_ab, diff_cd)  // 480
  let result = add(product, e)  // 580
  
  assert_eq(580, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?! ", greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "  leading and trailing spaces  "
  assert_eq_string("Hello,   leading and trailing spaces  !", greet(spaced_string))
  
  // 测试包含换行符的字符串
  let newline_string = "Line1\nLine2"
  assert_eq_string("Hello, Line1\nLine2!", greet(newline_string))
}