// Azimuth 项目综合单元测试
// 测试核心功能：add, multiply, greet, divide_with_ceil 函数

test "add_function_comprehensive" {
  // 测试基本加法运算
  assert_eq(8, add(3, 5))
  assert_eq(0, add(10, -10))
  assert_eq(-6, add(-2, -4))
  assert_eq(100, add(100, 0))
  assert_eq(100, add(0, 100))
}

test "add_function_overflow_handling" {
  // 测试溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_comprehensive" {
  // 测试基本乘法运算
  assert_eq(12, multiply(3, 4))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(8, 0))
  assert_eq(20, multiply(4, 5))
  assert_eq(25, multiply(5, 5))
}

test "multiply_function_special_cases" {
  // 测试乘法特殊情况和溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_comprehensive" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, user123!", greet("user123"))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, \"John\"!", greet("\"John\""))
  assert_eq_string("Hello, John & Jane!", greet("John & Jane"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(2, divide_with_ceil(5, 2))
  assert_eq(-2, divide_with_ceil(-5, 2))
  assert_eq(-3, divide_with_ceil(-7, 3))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 圆的面积和周长（简化计算）
  let radius = 5
  let pi_approx = 3
  
  // 周长 = 2 × π × r
  let circumference = multiply(multiply(2, pi_approx), radius)
  assert_eq(30, circumference)
  
  // 面积 = π × r²
  let area = multiply(pi_approx, multiply(radius, radius))
  assert_eq(75, area)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  let c = 3
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}