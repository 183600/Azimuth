// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 导入要测试的模块
use azimuth

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(azimuth.add(2, 3), 5)
  assert_eq(azimuth.add(-2, 3), 1)
  assert_eq(azimuth.add(0, 0), 0)
}

test "add_boundary_values" {
  // 测试加法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(azimuth.add(max_val, 1), max_val)
  assert_eq(azimuth.add(min_val, -1), min_val)
  assert_eq(azimuth.add(max_val, min_val), -1)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(azimuth.multiply(3, 4), 12)
  assert_eq(azimuth.multiply(-2, 3), -6)
  assert_eq(azimuth.multiply(0, 5), 0)
  assert_eq(azimuth.multiply(5, 1), 5)
}

test "multiply_boundary_values" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(azimuth.multiply(max_val, 2), max_val)
  assert_eq(azimuth.multiply(min_val, -1), min_val)
  assert_eq(azimuth.multiply(46341, 46341), max_val)
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string(azimuth.greet("World"), "Hello, World!")
  assert_eq_string(azimuth.greet(""), "Hello, !")
  assert_eq_string(azimuth.greet("MoonBit"), "Hello, MoonBit!")
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string(azimuth.greet("测试123"), "Hello, 测试123!")
  assert_eq_string(azimuth.greet("user@example.com"), "Hello, user@example.com!")
  assert_eq_string(azimuth.greet("🌍"), "Hello, 🌍!")
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(azimuth.divide_with_ceil(10, 3), 4)
  assert_eq(azimuth.divide_with_ceil(9, 3), 3)
  assert_eq(azimuth.divide_with_ceil(1, 2), 1)
  assert_eq(azimuth.divide_with_ceil(0, 5), 0)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let length = 10
  let width = 5
  let perimeter = azimuth.add(azimuth.multiply(2, length), azimuth.multiply(2, width))
  assert_eq(perimeter, 30)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
}

test "real_world_application" {
  // 测试实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let subtotal = azimuth.multiply(item_price, quantity)
  let total = azimuth.add(subtotal, shipping)
  assert_eq(total, 612)
}