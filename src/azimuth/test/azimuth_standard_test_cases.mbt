// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

use azimuth

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(-1, azimuth.add(-2, 1))
  assert_eq(100, azimuth.add(50, 50))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(-1, azimuth.add(max_val, min_val))
  
  // 测试与0相加
  assert_eq(42, azimuth.add(42, 0))
  assert_eq(42, azimuth.add(0, 42))
  assert_eq(min_val, azimuth.add(min_val, 0))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(0, azimuth.multiply(0, 5))
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(-6, azimuth.multiply(-2, 3))
  assert_eq(6, azimuth.multiply(-2, -3))
  assert_eq(15, azimuth.multiply(5, 3))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, azimuth.multiply(max_val, 2))
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  
  // 测试与1和-1相乘
  assert_eq(42, azimuth.multiply(42, 1))
  assert_eq(42, azimuth.multiply(1, 42))
  assert_eq(-42, azimuth.multiply(42, -1))
  assert_eq(-42, azimuth.multiply(-1, 42))
}

test "greet_function_variations" {
  // 测试greet函数的各种情况
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
}

test "complex_calculations" {
  // 测试复杂的计算场景
  // 计算矩形面积：长 × 宽
  let length = 10
  let width = 5
  let area = azimuth.multiply(length, width)
  assert_eq(50, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
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
  
  // 分配律
  assert_eq(azimuth.multiply(a, azimuth.add(b, 3)), azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let total = azimuth.add(azimuth.multiply(item_price, quantity), shipping)
  assert_eq(612, total)
  
  // 温度转换（简化版）：摄氏度转华氏度 F = C × 2 + 30
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "assert_functions" {
  // 测试断言函数
  azimuth.assert_true(true)
  azimuth.assert_false(false)
  azimuth.assert_eq(42, 42)
  azimuth.assert_eq_string("test", "test")
  
  // 测试断言函数与实际功能结合
  let result = azimuth.add(10, 20)
  azimuth.assert_true(result == 30)
  azimuth.assert_false(result == 25)
}