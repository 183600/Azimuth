// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_positive_numbers" {
  // 测试正数相加
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(100, azimuth::add(50, 50))
  assert_eq(0, azimuth::add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(-100, azimuth::add(-50, -50))
  assert_eq(0, azimuth::add(-5, 5))
}

test "add_boundary_values" {
  // 测试边界值相加
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "multiply_basic_operations" {
  // 测试基本乘法运算
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(0, 5))
  assert_eq(-6, azimuth::multiply(-2, 3))
  assert_eq(6, azimuth::multiply(-2, -3))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  assert_eq(10000, azimuth::multiply(100, 100))
  assert_eq(1000000, azimuth::multiply(1000, 1000))
  assert_eq(-1000000, azimuth::multiply(-1000, 1000))
}

test "multiply_boundary_values" {
  // 测试边界值相乘
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(0, azimuth::multiply(min_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "greet_functional_tests" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "greet_special_characters" {
  // 测试 greet 函数处理特殊字符
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
  assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形面积：长 × 宽
  let length = 10
  let width = 5
  let area = azimuth::multiply(length, width)
  assert_eq(50, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(30, perimeter)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  assert_eq(617, total)
  
  // 温度转换（简化版）：华氏度 = 摄氏度 × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}