// 标准 MoonBit 测试用例 - 覆盖 azimuth 包的核心功能
// 这些测试用例验证基本数学运算和字符串处理功能

test "integer_addition_comprehensive" {
  // 测试整数加法的各种情况
  assert_eq(8, add(3, 5))
  assert_eq(0, add(-3, 3))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
  assert_eq(1, add(2147483647, -2147483646))
}

test "integer_multiplication_comprehensive" {
  // 测试整数乘法的各种情况
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(0, 12345))
  assert_eq(100, multiply(10, 10))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "mathematical_operations_combination" {
  // 测试数学运算的组合
  let a = 12
  let b = 8
  let c = 5
  
  // (a + b) * c
  let sum_ab = add(a, b)
  let result1 = multiply(sum_ab, c)
  assert_eq(100, result1)
  
  // a * b + c
  let product_ab = multiply(a, b)
  let result2 = add(product_ab, c)
  assert_eq(101, result2)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
  
  // 边界值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
}

test "assertion_functions_verification" {
  // 测试断言函数的功能
  assert_true(5 > 3)
  assert_false(5 < 3)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 复合断言
  let x = 10
  let y = 20
  assert_true(add(x, y) == 30)
  assert_true(multiply(x, y) == 200)
}

test "real_world_calculation_scenarios" {
  // 测试实际应用计算场景
  // 购物车总价计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "error_prevention_and_safety" {
  // 测试错误预防和安全性
  let max_val = 2147483647
  
  // 溢出保护测试
  assert_true(add(max_val, 100) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
  
  // 零值处理
  assert_eq(0, multiply(0, max_val))
  assert_eq(max_val, add(0, max_val))
  
  // 负数处理
  assert_true(multiply(-1, max_val) < 0)
  assert_true(add(-1, max_val) > 0)
}