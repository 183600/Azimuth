// Azimuth 综合单元测试
// 测试核心功能：add, multiply, greet 函数以及边界情况

test "add_function_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
  assert_eq(100, add(50, 50))
}

test "add_function_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 负数溢出测试
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 极值相加
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(0, 12345))
  assert_eq(100, multiply(10, 10))
}

test "multiply_function_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  assert_eq(max_val, multiply(46341, 46341))
  
  // 负数溢出测试
  assert_eq(min_val, multiply(-46341, 46341))
  
  // 最小值特殊处理
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_function_comprehensive" {
  // 测试greet函数的各种情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 面积计算：长 × 宽
  let length = 15
  let width = 8
  let area = multiply(length, width)
  assert_eq(120, area)
  
  // 周长计算：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(46, perimeter)
}

test "financial_calculations" {
  // 测试金融计算场景
  // 本金：1000，利率：5%，时间：3年
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简单利息：本金 × 利率 × 时间
  let interest = multiply(principal, multiply(rate, years))
  // 总金额：本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(16000, total)  // 1000 + 1000*5*3 = 16000
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = add(multiply(0, 2), 30)
  assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = add(multiply(100, 2), 30)
  assert_eq(230, boiling)
}