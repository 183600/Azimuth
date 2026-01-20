// 简单的测试运行器
// 直接运行测试用例以验证功能

// 导入azimuth包的函数
use azimuth.{add, multiply, greet, assert_eq, assert_eq_string, assert_true, assert_false}

// 测试负数相加
fn test_add_negative_numbers() {
  assert_eq(-8, add(-3, -5))
  assert_eq(0, add(-10, 10))
  assert_eq(-5, add(-7, 2))
  println("✓ add_negative_numbers test passed")
}

// 测试负数组合的乘法
fn test_multiply_negative_combinations() {
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(-3, 0))
  println("✓ multiply_negative_combinations test passed")
}

// 测试Unicode字符处理
fn test_greet_unicode_characters() {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  println("✓ greet_unicode_characters test passed")
}

// 测试复杂算术序列
fn test_complex_arithmetic_sequence() {
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c - a * b
  let result1 = add(a, b)
  let result2 = multiply(result1, c)
  let result3 = multiply(a, b)
  let final_result = add(result2, -result3)
  
  assert_eq(700, final_result)  // (10 + 20) * 30 - 10 * 20 = 900 - 200 = 700
  println("✓ complex_arithmetic_sequence test passed")
}

// 测试边界值运算
fn test_boundary_value_operations() {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与普通值的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  println("✓ boundary_value_operations test passed")
}

// 测试字符串边界情况
fn test_string_edge_cases() {
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a b c!", greet("a b c"))
  println("✓ string_edge_cases test passed")
}

// 测试数学性质
fn test_mathematical_properties() {
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 5)), add(multiply(x, y), multiply(x, 5)))
  println("✓ mathematical_properties test passed")
}

// 测试实际应用计算
fn test_real_world_calculations() {
  // 计算购物车总价：商品价格 × 数量 + 运费
  let price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(617, total)  // 199 * 3 + 20 = 597 + 20 = 617
  println("✓ real_world_calculations test passed")
}

// 测试条件逻辑运算
fn test_conditional_logic_operations() {
  let base_value = 100
  let multiplier = 2
  let offset = 10
  
  // 根据条件选择不同的计算方式
  let use_multiplication = true
  let result = if use_multiplication {
    multiply(base_value, multiplier)
  } else {
    add(base_value, offset)
  }
  
  assert_eq(200, result)
  
  let use_multiplication_2 = false
  let result2 = if use_multiplication_2 {
    multiply(base_value, multiplier)
  } else {
    add(base_value, offset)
  }
  
  assert_eq(110, result2)
  println("✓ conditional_logic_operations test passed")
}

// 测试溢出保护机制
fn test_overflow_protection() {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  assert_eq(max_val, add(max_val, 100))
  assert_eq(min_val, add(min_val, -100))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -2))
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  println("✓ overflow_protection test passed")
}

// 主函数：运行所有测试
fn main() {
  println("Running additional core functionality tests...")
  
  test_add_negative_numbers()
  test_multiply_negative_combinations()
  test_greet_unicode_characters()
  test_complex_arithmetic_sequence()
  test_boundary_value_operations()
  test_string_edge_cases()
  test_mathematical_properties()
  test_real_world_calculations()
  test_conditional_logic_operations()
  test_overflow_protection()
  
  println("All tests passed successfully!")
}