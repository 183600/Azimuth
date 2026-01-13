test "basic_add_test" {
  // 测试基本加法运算
  let result = azimuth::add(2, 3)
  assert_eq(5, result)
}

test "basic_multiply_test" {
  // 测试基本乘法运算
  let result = azimuth::multiply(2, 3)
  assert_eq(6, result)
}

test "basic_greet_test" {
  // 测试基本问候功能
  let result = azimuth::greet("World")
  assert_eq_string("Hello, World!", result)
}

test "add_edge_case_max_min_values" {
  // 测试最大值和最小值相加的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "add_consecutive_operations" {
  // 测试连续加法操作
  let result = azimuth::add(azimuth::add(1, 2), azimuth::add(3, 4))
  assert_eq(10, result)
  
  let result2 = azimuth::add(azimuth::add(-5, 10), azimuth::add(-3, 8))
  assert_eq(10, result2)
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  assert_eq(8, azimuth::multiply(2, 4))
  assert_eq(16, azimuth::multiply(4, 4))
  assert_eq(1024, azimuth::multiply(32, 32))
  assert_eq(-1024, azimuth::multiply(-32, 32))
}

test "multiply_fractional_simulation" {
  // 通过整数运算模拟小数乘法（例如：计算百分比）
  // 25% of 400 = 400 * 25 / 100
  let value = 400
  let percentage = 25
  let result = azimuth::multiply(value, percentage) / 100
  assert_eq(100, result)
  
  // 75% of 200 = 200 * 75 / 100
  let value2 = 200
  let percentage2 = 75
  let result2 = azimuth::multiply(value2, percentage2) / 100
  assert_eq(150, result2)
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, العربية!", azimuth::greet("العربية"))
  assert_eq_string("Hello, עברית!", azimuth::greet("עברית"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, Line1\nLine2!", azimuth::greet("Line1\nLine2"))
  assert_eq_string("Hello, Tab\tSeparated!", azimuth::greet("Tab\tSeparated"))
  assert_eq_string("Hello, Emoji🌍!", azimuth::greet("Emoji🌍"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  // 计算：1 + 2*3 + 4*5*6
  let term1 = 1
  let term2 = azimuth::multiply(2, 3)
  let term3 = azimuth::multiply(azimuth::multiply(4, 5), 6)
  let result = azimuth::add(azimuth::add(term1, term2), term3)
  assert_eq(127, result)  // 1 + 6 + 120 = 127
}