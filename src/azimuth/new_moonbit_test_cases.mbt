// 新增的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_negative_numbers" {
  // 测试负数相加
  let result = azimuth::add(-10, -20)
  assert_eq(-30, result)
}

test "add_mixed_positive_negative" {
  // 测试正数和负数混合相加
  let result1 = azimuth::add(100, -50)
  assert_eq(50, result1)
  
  let result2 = azimuth::add(-100, 50)
  assert_eq(-50, result2)
}

test "multiply_large_numbers" {
  // 测试大数相乘
  let result = azimuth::multiply(10000, 10000)
  assert_eq(100000000, result)
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  let result1 = azimuth::multiply(-5, -6)
  assert_eq(30, result1)
  
  let result2 = azimuth::multiply(-5, 6)
  assert_eq(-30, result2)
}

test "greet_with_unicode" {
  // 测试包含 Unicode 字符的问候
  let result = azimuth::greet("世界")
  assert_eq_string("Hello, 世界!", result)
}

test "greet_with_special_chars" {
  // 测试包含特殊字符的问候
  let result = azimuth::greet("Test@123")
  assert_eq_string("Hello, Test@123!", result)
}

test "complex_arithmetic_expression" {
  // 测试复杂算术表达式：(a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(sum, c)
  let result = azimuth::add(product, -d)
  
  assert_eq(75, result)
}

test "boundary_value_operations" {
  // 测试边界值操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与0的运算
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(0, azimuth::multiply(max_val, 0))
  
  // 测试最小值与0的运算
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(0, azimuth::multiply(min_val, 0))
}

test "sequential_operations" {
  // 测试连续操作
  // 计算：1 + 2 + 3 + 4 + 5
  let result1 = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4), 5)
  assert_eq(15, result1)
  
  // 计算：2 * 3 * 4
  let result2 = azimuth::multiply(azimuth::multiply(2, 3), 4)
  assert_eq(24, result2)
}

test "assert_functions_functionality" {
  // 测试断言函数的功能
  azimuth::assert_true(5 > 3)
  azimuth::assert_false(5 < 3)
  azimuth::assert_eq(42, 42)
  azimuth::assert_eq_string("test", "test")
}