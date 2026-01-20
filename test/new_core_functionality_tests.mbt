// 新的核心功能测试用例
// 测试 azimuth 包的核心功能，涵盖边界情况和实际应用场景

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-8, add(-3, -5))
  assert_eq(0, add(-10, 10))
  assert_eq(-5, add(-7, 2))
}

test "multiply_negative_combinations" {
  // 测试各种负数组合的乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(0, multiply(-3, 0))
}

test "greet_unicode_characters" {
  // 测试Unicode字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂算术序列
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c - a * b
  let result1 = add(a, b)
  let result2 = multiply(result1, c)
  let result3 = multiply(a, b)
  let final_result = add(result2, -result3)
  
  assert_eq(700, final_result)  // (10 + 20) * 30 - 10 * 20 = 900 - 200 = 700
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与普通值的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a b c!", greet("a b c"))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 5)), add(multiply(x, y), multiply(x, 5)))
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 计算购物车总价：商品价格 × 数量 + 运费
  let price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = multiply(price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(617, total)  // 199 * 3 + 20 = 597 + 20 = 617
}

test "conditional_logic_operations" {
  // 测试条件逻辑运算
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
}

test "overflow_protection" {
  // 测试溢出保护机制
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
}