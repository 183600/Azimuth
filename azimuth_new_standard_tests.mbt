// 新增的 MoonBit 测试用例
// 这些测试用例覆盖了 azimuth 库的核心功能

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = add(a, b)
  let result2 = add(b, a)
  assert_eq(result1, result2)
}

test "add_associative_property" {
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = add(add(a, b), c)
  let result2 = add(a, add(b, c))
  assert_eq(result1, result2)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 10
  let c = 15
  
  let result1 = multiply(a, add(b, c))
  let result2 = add(multiply(a, b), multiply(a, c))
  assert_eq(result1, result2)
}

test "multiply_identity_property" {
  // 测试乘法单位元：a × 1 = a
  let values = [0, 1, -1, 42, -42, 2147483647, -2147483648]
  
  // 测试多个值
  assert_eq(0, multiply(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(42, multiply(42, 1))
  assert_eq(-42, multiply(-42, 1))
  assert_eq(2147483647, multiply(2147483647, 1))
  assert_eq(-2147483648, multiply(-2147483648, 1))
}

test "greet_with_various_input_types" {
  // 测试greet函数处理各种输入类型
  // 普通字符串
  assert_eq_string("Hello, World!", greet("World"))
  
  // 包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  
  // 包含特殊字符的字符串
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  
  // 包含Unicode字符的字符串
  assert_eq_string("Hello, 测试!", greet("测试"))
  
  // 数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 8
  let width = 6
  
  // 计算面积
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 计算周长
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(28, perimeter)
  
  // 计算对角线长度的平方（简化计算）
  let diagonal_squared = add(multiply(length, length), multiply(width, width))
  assert_eq(100, diagonal_squared)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：简单利息和复利计算
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  // 简单利息：本金 × 利率 × 时间
  let simple_interest = multiply(principal, multiply(rate, time))
  let simple_total = add(principal, simple_interest)
  assert_eq(160000, simple_total)
  
  // 简化复利：本金 × (1 + 利率 × 时间)
  let compound_factor = add(1, multiply(rate, time))
  let compound_total = multiply(principal, compound_factor)
  assert_eq(160000, compound_total)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30
  let celsius_temps = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  // 测试多个温度点
  assert_eq(30, add(multiply(0, 2), 30))
  assert_eq(50, add(multiply(10, 2), 30))
  assert_eq(70, add(multiply(20, 2), 30))
  assert_eq(90, add(multiply(30, 2), 30))
  assert_eq(110, add(multiply(40, 2), 30))
}

test "boundary_value_comprehensive_test" {
  // 测试边界值的综合情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(max_val, -1))
  assert_eq(0, multiply(max_val, 0))
  
  // 测试最小值的各种运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试最大值与最小值的运算
  assert_eq(-1, add(max_val, min_val))
  assert_eq(min_val, multiply(min_val, 1))
}