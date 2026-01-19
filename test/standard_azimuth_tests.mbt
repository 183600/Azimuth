// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

// 导入 azimuth 模块的核心功能
use "azimuth"

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-2, azimuth.add(-5, 3))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(-15, azimuth.multiply(-3, 5))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth.multiply(max_val, 2))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
}

test "complex_calculation" {
  // 测试复合计算：矩形周长
  let length = 10
  let width = 5
  let perimeter = azimuth.add(
    azimuth.multiply(2, length), 
    azimuth.multiply(2, width)
  )
  assert_eq(30, perimeter)
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

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值相加
  assert_eq(-1, azimuth.add(max_val, min_val))
  
  // 边界值与0相乘
  assert_eq(0, azimuth.multiply(max_val, 0))
  assert_eq(0, azimuth.multiply(min_val, 0))
}

test "real_world_scenario" {
  // 测试实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = azimuth.multiply(item_price, quantity)
  let total = azimuth.add(subtotal, shipping)
  
  assert_eq(617, total)
}

test "unicode_string_handling" {
  // 测试Unicode字符串处理
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))
  assert_eq_string("Hello, 中文123!", azimuth.greet("中文123"))
}