// 标准 MoonBit 测试用例 - azimuth 项目
// 包含核心功能测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let result1 = azimuth::add(5, 3)
  assert_eq(result1, 8)
  
  let result2 = azimuth::multiply(4, 6)
  assert_eq(result2, 24)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  let result1 = azimuth::greet("World")
  assert_eq(result1, "Hello, World!")
  
  let result2 = azimuth::greet("MoonBit")
  assert_eq(result2, "Hello, MoonBit!")
}

test "negative_number_operations" {
  // 测试负数运算
  let result1 = azimuth::add(-10, 5)
  assert_eq(result1, -5)
  
  let result2 = azimuth::multiply(-3, 7)
  assert_eq(result2, -21)
}

test "zero_value_operations" {
  // 测试零值运算
  let result1 = azimuth::add(0, 42)
  assert_eq(result1, 42)
  
  let result2 = azimuth::multiply(0, 100)
  assert_eq(result2, 0)
}

test "large_number_handling" {
  // 测试大数处理
  let large_num = 1000000
  let result1 = azimuth::add(large_num, 500000)
  assert_eq(result1, 1500000)
  
  let result2 = azimuth::multiply(large_num, 2)
  assert_eq(result2, 2000000)
}

test "boundary_value_testing" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  let result1 = azimuth::add(max_int, 0)
  assert_eq(result1, max_int)
  
  let result2 = azimuth::add(min_int, 0)
  assert_eq(result2, min_int)
}

test "empty_string_handling" {
  // 测试空字符串处理
  let result = azimuth::greet("")
  assert_eq(result, "Hello, !")
}

test "special_character_strings" {
  // 测试特殊字符字符串
  let result1 = azimuth::greet("test@example.com")
  assert_eq(result1, "Hello, test@example.com!")
  
  let result2 = azimuth::greet("user@domain.org")
  assert_eq(result2, "Hello, user@domain.org!")
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  let result1 = azimuth::add(a, b)
  let result2 = azimuth::add(b, a)
  assert_eq(result1, result2)
  
  let result3 = azimuth::multiply(a, b)
  let result4 = azimuth::multiply(b, a)
  assert_eq(result3, result4)
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  // 计算：(10 + 5) * 3 - 7
  let step1 = azimuth::add(10, 5)  // 15
  let step2 = azimuth::multiply(step1, 3)  // 45
  let result = azimuth::add(step2, -7)  // 38
  
  assert_eq(result, 38)
}