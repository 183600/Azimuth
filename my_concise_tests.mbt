// Azimuth 项目简明测试用例
// 使用标准 MoonBit 测试语法

test "basic_addition" {
  // 基本加法测试
  assert_eq(azimuth::add(2, 3), 5)
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(-5, 5), 0)
}

test "basic_multiplication" {
  // 基本乘法测试
  assert_eq(azimuth::multiply(3, 4), 12)
  assert_eq(azimuth::multiply(0, 100), 0)
  assert_eq(azimuth::multiply(-2, 3), -6)
}

test "greet_functionality" {
  // 问候功能测试
  assert_eq(azimuth::greet("World"), "Hello, World!")
  assert_eq(azimuth::greet(""), "Hello, !")
  assert_eq(azimuth::greet("测试"), "Hello, 测试!")
}

test "overflow_protection" {
  // 溢出保护测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(azimuth::add(max_val, 1), max_val)
  assert_eq(azimuth::add(min_val, -1), min_val)
  assert_eq(azimuth::multiply(max_val, 2), max_val)
}

test "complex_calculations" {
  // 复合计算测试
  // (a + b) * c
  let result = azimuth::multiply(azimuth::add(5, 3), 2)
  assert_eq(result, 16)
  
  // a * b + c
  let result2 = azimuth::add(azimuth::multiply(3, 4), 5)
  assert_eq(result2, 17)
}

test "edge_cases" {
  // 边界情况测试
  assert_eq(azimuth::multiply(-1, -1), 1)
  assert_eq(azimuth::multiply(1, -1), -1)
  assert_eq(azimuth::add(-1, 1), 0)
}

test "string_operations" {
  // 字符串操作测试
  assert_eq(azimuth::greet("123"), "Hello, 123!")
  assert_eq(azimuth::greet("!@#"), "Hello, !@#!")
  assert_eq(azimuth::greet("   "), "Hello,    !")
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 购物车计算
  let price = 199
  let quantity = 3
  let shipping = 20
  let total = azimuth::add(azimuth::multiply(price, quantity), shipping)
  assert_eq(total, 617)
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(fahrenheit, 80)
}

test "special_values" {
  // 特殊值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  assert_eq(azimuth::add(max_val, min_val), -1)
  
  // 最小值与-1相乘
  assert_eq(azimuth::multiply(min_val, -1), min_val)
}