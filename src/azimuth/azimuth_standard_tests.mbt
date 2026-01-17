// Azimuth 项目标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖核心功能

test "basic_addition" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
}

test "addition_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "basic_multiplication" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(5, 0))
}

test "multiplication_boundary_values" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试问候语功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_special_characters" {
  // 测试特殊字符处理
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_calculation" {
  // 测试复杂计算：矩形周长
  let length = 10
  let width = 5
  let perimeter = add(multiply(2, length), multiply(2, width))
  assert_eq(30, perimeter)
}

test "financial_calculation" {
  // 测试金融计算：简单利息
  let principal = 1000
  let rate = 5
  let years = 2
  let interest = multiply(principal, multiply(rate, years))
  let total = add(principal, interest)
  assert_eq(11000, total)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  let celsius = 20
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(70, fahrenheit)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let subtotal = add(subtotal1, subtotal2)
  let total = add(subtotal, shipping)
  
  assert_eq(896, total)
}