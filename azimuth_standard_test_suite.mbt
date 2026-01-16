// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

test "add_function_basic_addition" {
  // 测试基本的加法功能
  let result1 = azimuth::add(5, 3)
  assert_eq(result1, 8)
  
  let result2 = azimuth::add(10, 20)
  assert_eq(result2, 30)
  
  let result3 = azimuth::add(0, 7)
  assert_eq(result3, 7)
}

test "add_function_with_negative_numbers" {
  // 测试负数加法
  let result1 = azimuth::add(-5, 3)
  assert_eq(result1, -2)
  
  let result2 = azimuth::add(10, -3)
  assert_eq(result2, 7)
  
  let result3 = azimuth::add(-8, -2)
  assert_eq(result3, -10)
}

test "multiply_function_basic_multiplication" {
  // 测试基本的乘法功能
  let result1 = azimuth::multiply(4, 5)
  assert_eq(result1, 20)
  
  let result2 = azimuth::multiply(7, 3)
  assert_eq(result2, 21)
  
  let result3 = azimuth::multiply(10, 0)
  assert_eq(result3, 0)
}

test "multiply_function_with_negative_numbers" {
  // 测试负数乘法
  let result1 = azimuth::multiply(-4, 5)
  assert_eq(result1, -20)
  
  let result2 = azimuth::multiply(6, -3)
  assert_eq(result2, -18)
  
  let result3 = azimuth::multiply(-2, -4)
  assert_eq(result3, 8)
}

test "greet_function_with_different_names" {
  // 测试问候功能
  let result1 = azimuth::greet("World")
  assert_eq_string(result1, "Hello, World!")
  
  let result2 = azimuth::greet("Alice")
  assert_eq_string(result2, "Hello, Alice!")
  
  let result3 = azimuth::greet("")
  assert_eq_string(result3, "Hello, !")
}

test "edge_case_zero_operations" {
  // 测试零的边界情况
  let add_result1 = azimuth::add(0, 0)
  assert_eq(add_result1, 0)
  
  let add_result2 = azimuth::add(0, 15)
  assert_eq(add_result2, 15)
  
  let add_result3 = azimuth::add(15, 0)
  assert_eq(add_result3, 15)
  
  let mul_result1 = azimuth::multiply(0, 0)
  assert_eq(mul_result1, 0)
  
  let mul_result2 = azimuth::multiply(0, 15)
  assert_eq(mul_result2, 0)
  
  let mul_result3 = azimuth::multiply(15, 0)
  assert_eq(mul_result3, 0)
}

test "edge_case_one_operations" {
  // 测试一的边界情况
  let mul_result1 = azimuth::multiply(1, 15)
  assert_eq(mul_result1, 15)
  
  let mul_result2 = azimuth::multiply(15, 1)
  assert_eq(mul_result2, 15)
  
  let mul_result3 = azimuth::multiply(1, 1)
  assert_eq(mul_result3, 1)
}

test "complex_calculations" {
  // 测试复杂计算
  // (2 + 3) * 4 = 5 * 4 = 20
  let step1 = azimuth::add(2, 3)
  let result1 = azimuth::multiply(step1, 4)
  assert_eq(result1, 20)
  
  // 5 * (6 + 7) = 5 * 13 = 65
  let step2 = azimuth::add(6, 7)
  let result2 = azimuth::multiply(5, step2)
  assert_eq(result2, 65)
  
  // (8 * 2) + (3 * 4) = 16 + 12 = 28
  let step3 = azimuth::multiply(8, 2)
  let step4 = azimuth::multiply(3, 4)
  let result3 = azimuth::add(step3, step4)
  assert_eq(result3, 28)
}

test "boolean_logic_simulation" {
  // 使用数字模拟布尔逻辑测试
  let true_val = 1
  let false_val = 0
  
  // 模拟 AND: true AND false = false
  let and_result = azimuth::multiply(true_val, false_val)
  assert_eq(and_result, 0)
  
  // 模拟 OR: true OR false = true (使用加法，如果结果>0则为真)
  let or_result = azimuth::add(true_val, false_val)
  assert_true(or_result > 0)
  
  // 模拟 NOT: NOT true = false (1 - true = false)
  let not_true = azimuth::add(1, -true_val)
  assert_eq(not_true, 0)
  
  // 模拟 NOT: NOT false = true (1 - false = true)
  let not_false = azimuth::add(1, -false_val)
  assert_eq(not_false, 1)
}