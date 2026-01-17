// Azimuth 项目标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 测试加法的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试正常边界情况
  assert_eq(-1, add(max_val, min_val))
  assert_eq(0, add(min_val, 1))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 100))
  assert_eq(-1, multiply(-1, 100))
}

test "multiply_boundary_values" {
  // 测试乘法的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 测试特殊情况
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
}

test "greet_basic_functionality" {
  // 测试基本的问候语功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候语
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形周长：2 × (长 + 宽)
  let length = 10
  let width = 5
  let perimeter = multiply(2, add(length, width))
  assert_eq(30, perimeter)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 8
  let height = 6
  let area = multiply(base, height) / 2
  assert_eq(24, area)
}

test "financial_calculations" {
  // 测试金融计算场景
  // 简单利息计算：本金 + (本金 × 利率 × 时间)
  let principal = 1000
  let rate = 5
  let years = 2
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(11000, total)  // 1000 + 1000×5×2 = 11000
  
  // 折扣计算：原价 - (原价 × 折扣率)
  let original_price = 200
  let discount_rate = 20
  let discount_amount = multiply(original_price, discount_rate) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(160, final_price)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) ÷ 2（简化公式）
  let fahrenheit2 = 100
  let celsius2 = add(fahrenheit2, -30) / 2
  assert_eq(35, celsius2)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  let tax_rate = 10
  
  // 计算商品总价
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let subtotal = add(subtotal1, subtotal2)
  
  // 计算税费
  let tax = multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(736, total)  // (199×2 + 299×1) + (697×10/100) + 20 = 697 + 69 + 20 = 786
}