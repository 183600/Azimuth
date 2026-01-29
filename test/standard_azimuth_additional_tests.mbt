// 标准 MoonBit 测试用例 - azimuth 库的额外测试
// 包含10个标准测试用例，覆盖核心功能

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "greet_function_with_special_characters" {
  // 测试问候函数处理特殊字符
  assert_eq_string("Hello, @moonbit!", greet("@moonbit"))
  assert_eq_string("Hello, test+case@example.com!", greet("test+case@example.com"))
  assert_eq_string("Hello, 123_456!", greet("123_456"))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  assert_eq(7, divide_with_ceil(40, 6))    // 40/6 = 6.666... -> 7
  assert_eq(3, divide_with_ceil(-20, 7))   // -20/7 = -2.857... -> -2
  assert_eq(0, divide_with_ceil(0, 100))   // 0/任何数 = 0
  assert_eq(0, divide_with_ceil(50, 0))    // 除数为0时返回0
}

test "combined_arithmetic_operations" {
  // 测试组合算术运算
  let x = 10
  let y = 15
  let z = 20
  
  // 计算：(x + y) * z - x
  let result = add(multiply(add(x, y), z), -x)
  assert_eq(490, result)  // (10 + 15) * 20 - 10 = 25 * 20 - 10 = 500 - 10 = 490
}

test "negative_number_operations_comprehensive" {
  // 测试负数的综合运算
  assert_eq(-10, add(-3, -7))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(3, divide_with_ceil(-10, -4))
}

test "boundary_value_testing" {
  // 测试边界值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(0, 1000))
  assert_eq(1000, multiply(1, 1000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
}

test "real_world_shopping_cart_calculation" {
  // 模拟真实购物车计算
  let item1_price = 29
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let discount = 10
  
  let subtotal1 = multiply(item1_price, item1_quantity)
  let subtotal2 = multiply(item2_price, item2_quantity)
  let total_before_discount = add(subtotal1, subtotal2)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(103, final_total)  // 29*2 + 15*3 - 10 = 58 + 45 - 10 = 93
}

test "string_greeting_with_unicode_emoji" {
  // 测试Unicode表情符号的问候
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 💻!", greet("💻"))
  assert_eq_string("Hello, 🎯!", greet("🎯"))
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：等差数列求和
  let first_term = 3
  let common_diff = 2
  let num_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(48, sum)  // 6/2 * (6 + 10) = 3 * 16 = 48
}