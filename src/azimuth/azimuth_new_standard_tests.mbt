// MoonBit 标准测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_cases" {
  // 基本加法测试
  assert_eq(10, azimuth.add(3, 7))
  assert_eq(0, azimuth.add(-5, 5))
  assert_eq(-10, azimuth.add(-3, -7))
  assert_eq(100, azimuth.add(50, 50))
}

test "add_function_edge_cases" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值
  assert_eq(42, azimuth.add(0, 42))
  assert_eq(42, azimuth.add(42, 0))
  assert_eq(0, azimuth.add(0, 0))
  
  // 测试边界值
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(-1, azimuth.add(max_val, min_val))
}

test "multiply_function_basic_cases" {
  // 基本乘法测试
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-15, azimuth.multiply(3, -5))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(1, azimuth.multiply(1, 123))
  assert_eq(-123, azimuth.multiply(-1, 123))
}

test "multiply_function_edge_cases" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值乘法
  assert_eq(max_val, azimuth.multiply(max_val, 1))
  assert_eq(min_val, azimuth.multiply(min_val, 1))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  assert_eq(0, azimuth.multiply(max_val, 0))
}

test "greet_function_standard_cases" {
  // 标准字符串测试
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, A!", azimuth.greet("A"))
}

test "greet_function_special_characters" {
  // 特殊字符测试
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, user@123!", azimuth.greet("user@123"))
  assert_eq_string("Hello, space test!", azimuth.greet("space test"))
  assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = azimuth.multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  assert_eq(26, perimeter)
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 7
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 3
  assert_eq(azimuth.multiply(a, azimuth.add(b, c)), azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c)))
}

test "real_world_application" {
  // 实际应用场景测试
  // 购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let shipping_fee = 15
  
  // 计算商品总价
  let total1 = azimuth.multiply(item1_price, item1_quantity)
  let total2 = azimuth.multiply(item2_price, item2_quantity)
  let subtotal = azimuth.add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = azimuth.add(subtotal, shipping_fee)
  
  // 验证：199×2 + 99×3 + 15 = 398 + 297 + 15 = 710
  assert_eq(710, final_total)
}

test "assertion_functions" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试布尔表达式
  let x = 10
  let y = 20
  assert_true(x < y)
  assert_false(x > y)
  assert_true(azimuth.add(x, y) == 30)
}