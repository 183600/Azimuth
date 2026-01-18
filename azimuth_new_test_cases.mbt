// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，测试核心功能的各种场景

test "add_function_commutative_property" {
  // 测试加法交换律
  let result1 = add(15, 27)
  let result2 = add(27, 15)
  assert_eq(result1, result2)
}

test "add_function_associative_property" {
  // 测试加法结合律
  let result1 = add(add(10, 20), 30)
  let result2 = add(10, add(20, 30))
  assert_eq(result1, result2)
}

test "multiply_function_commutative_property" {
  // 测试乘法交换律
  let result1 = multiply(12, 8)
  let result2 = multiply(8, 12)
  assert_eq(result1, result2)
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 5
  let b = 3
  let c = 2
  let result1 = multiply(a, add(b, c))
  let result2 = add(multiply(a, b), multiply(a, c))
  assert_eq(result1, result2)
}

test "greet_function_with_unicode" {
  // 测试包含Unicode字符的问候功能
  let result1 = greet("世界")
  let result2 = greet("🌍")
  let result3 = greet("MoonBit测试")
  assert_eq_string("Hello, 世界!", result1)
  assert_eq_string("Hello, 🌍!", result2)
  assert_eq_string("Hello, MoonBit测试!", result3)
}

test "combined_operations" {
  // 测试组合运算
  let x = 10
  let y = 5
  let z = 3
  
  // 计算 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(45, result1)
  
  // 计算 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(53, result2)
}

test "edge_case_large_numbers" {
  // 测试大数运算
  let large1 = 1000000
  let large2 = 2000000
  
  let add_result = add(large1, large2)
  assert_eq(3000000, add_result)
  
  let mul_result = multiply(large1, 2)
  assert_eq(2000000, mul_result)
}

test "edge_case_negative_operations" {
  // 测试负数运算
  let neg1 = -15
  let neg2 = -8
  let pos = 10
  
  // 负数加法
  let result1 = add(neg1, neg2)
  assert_eq(-23, result1)
  
  // 正负数加法
  let result2 = add(neg1, pos)
  assert_eq(-5, result2)
  
  // 负数乘法
  let result3 = multiply(neg1, neg2)
  assert_eq(120, result3)
  
  // 正负数乘法
  let result4 = multiply(neg1, pos)
  assert_eq(-150, result4)
}

test "greet_function_with_whitespace" {
  // 测试包含空格的问候功能
  let result1 = greet("  spaced  ")
  let result2 = greet("tabbed")
  let result3 = greet("newlined")
  assert_eq_string("Hello,   spaced  !", result1)
  assert_eq_string("Hello, tabbed!", result2)
  assert_eq_string("Hello, newlined!", result3)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base_price = 100
  let quantity = 5
  let discount = 20
  let tax_rate = 8
  
  // 计算总价
  let subtotal = multiply(base_price, quantity)
  
  // 应用折扣
  let discounted = add(subtotal, -discount)
  
  // 计算税费（简化为固定值）
  let tax = 38
  
  // 最终价格
  let final_price = add(discounted, tax)
  
  // 验证：(100*5 - 20) + 38 = 480 + 38 = 518
  assert_eq(518, final_price)
}