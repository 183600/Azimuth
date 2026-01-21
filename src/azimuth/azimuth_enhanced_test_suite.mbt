// Azimuth 增强测试套件
// 标准 MoonBit 测试用例，覆盖核心功能

test "add_comprehensive_operations" {
  // 测试加法的各种情况
  assert_eq(8, add(3, 5))
  assert_eq(-2, add(3, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(0, 100))
  assert_eq(-100, add(0, -100))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_comprehensive_operations" {
  // 测试乘法的各种情况
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 3))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_formula_applications" {
  // 测试数学公式的实际应用
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  let area = multiply(length, width)
  assert_eq(96, area)
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(40, perimeter)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(160000, total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（整数近似）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) ÷ 2（整数近似）
  let f_to_c = add(fahrenheit, -30) / 2
  assert_eq(25, f_to_c)
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  let unit_price = 199
  let quantity = 3
  let discount_threshold = 500
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算折扣
  let discount = if (subtotal > discount_threshold) {
    multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_price = add(subtotal, -discount)
  
  // 计算税费
  let tax = multiply(discounted_price, tax_rate) / 100
  
  // 计算总价
  let total = add(add(discounted_price, tax), shipping)
  
  assert_eq(1076, total)
}

test "assertion_functions_validation" {
  // 测试断言函数本身
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  // 测试计算结果的断言
  let result = add(17, 25)
  assert_eq(42, result)
  
  let greeting = greet("Developer")
  assert_eq_string("Hello, Developer!", greeting)
  
  let product = multiply(6, 7)
  assert_true(product == 42)
}