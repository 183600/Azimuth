// 新增的 MoonBit 测试用例
// 测试 azimuth 包的核心功能和边界情况

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-8, add(-3, -5))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, John@Doe!", greet("John@Doe"))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello,   spaced   !", greet("  spaced  "))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款利息计算
  let principal = 10000  // 本金
  let rate = 5          // 利率 5%
  let years = 3         // 年数
  
  // 计算总利息：本金 × 利率 × 年数
  let total_interest = multiply(principal, multiply(rate, years))
  // 计算总还款：本金 + 利息
  let total_payment = add(principal, total_interest)
  
  assert_eq(160000, total_payment)  // 10000 + 10000*5*3 = 160000
}

test "geometric_calculation" {
  // 测试几何计算：矩形面积和周长
  let length = 8
  let width = 6
  
  // 计算面积：长 × 宽
  let area = multiply(length, width)
  // 计算周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(48, area)      // 8 * 6 = 48
  assert_eq(28, perimeter) // 2 * (8 + 6) = 28
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版 F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1和-1的乘法
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(max_val, -1))
  assert_eq(max_val, multiply(min_val, -1))
}

test "sequential_operations" {
  // 测试连续运算：(a + b) * (c + d) - e
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 100
  
  let sum1 = add(a, b)      // 30
  let sum2 = add(c, d)      // 20
  let product = multiply(sum1, sum2)  // 600
  let result = add(product, -e)       // 500
  
  assert_eq(500, result)
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, ThisIsAVeryLongNameForTestingPurposes!", greet("ThisIsAVeryLongNameForTestingPurposes"))
  assert_eq_string("Hello, Line1\nLine2!", greet("Line1\nLine2"))
}

test "mathematical_properties" {
  // 测试数学性质：交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}