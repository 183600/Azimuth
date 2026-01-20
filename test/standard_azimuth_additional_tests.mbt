// 标准 MoonBit 测试用例 - Azimuth 项目补充测试
// 测试核心功能：add, multiply, greet 和断言函数

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(-1, azimuth.add(-2, 1))
  assert_eq(100, azimuth.add(50, 50))
}

test "add_function_negative_numbers" {
  // 测试 add 函数处理负数的能力
  assert_eq(-10, azimuth.add(-5, -5))
  assert_eq(0, azimuth.add(-10, 10))
  assert_eq(-20, azimuth.add(-30, 10))
  assert_eq(5, azimuth.add(-15, 20))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(-6, azimuth.multiply(-2, 3))
  assert_eq(6, azimuth.multiply(-2, -3))
}

test "multiply_function_large_numbers" {
  // 测试 multiply 函数处理大数的能力
  assert_eq(10000, azimuth.multiply(100, 100))
  assert_eq(1000000, azimuth.multiply(1000, 1000))
  assert_eq(-1000000, azimuth.multiply(-1000, 1000))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数处理各种输入的能力
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
}

test "overflow_protection_add" {
  // 测试 add 函数的溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(max_val, azimuth.add(max_val, max_val))
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(min_val, azimuth.add(min_val, min_val))
}

test "overflow_protection_multiply" {
  // 测试 multiply 函数的溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth.multiply(max_val, 2))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  assert_eq(max_val, azimuth.multiply(46341, 46341))
  assert_eq(min_val, azimuth.multiply(-46341, 46341))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = azimuth.multiply(azimuth.add(a, b), c)
  assert_eq(900, result1)
  
  // a * b + c
  let result2 = azimuth.add(azimuth.multiply(a, b), c)
  assert_eq(230, result2)
}

test "assertion_functions_validation" {
  // 测试断言函数的正确性
  azimuth.assert_true(true)
  azimuth.assert_false(false)
  azimuth.assert_eq(42, 42)
  azimuth.assert_eq_string("MoonBit", "MoonBit")
  
  // 测试复合断言
  let x = 5
  let y = 10
  azimuth.assert_true(azimuth.add(x, y) == 15)
  azimuth.assert_false(azimuth.multiply(x, y) == 40)
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = azimuth.multiply(item1_price, item1_quantity)
  let total2 = azimuth.multiply(item2_price, item2_quantity)
  let cart_total = azimuth.add(total1, total2)
  
  azimuth.assert_eq(697, cart_total)
  
  // 场景2：温度转换（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  azimuth.assert_eq(80, fahrenheit)
}