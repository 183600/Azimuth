// 额外的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(-5, 5))
  assert_eq(-100, add(-50, -50))
}

test "multiply_by_negative" {
  // 测试负数乘法
  assert_eq(-15, multiply(3, -5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(0, multiply(-10, 0))
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, Paris!", greet("Paris"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  let a = 12
  let b = 8
  let c = 5
  
  // 计算 (a + b) * c
  let result = multiply(add(a, b), c)
  assert_eq(100, result)
  
  // 计算 a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(101, result2)
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的值
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  assert_eq(0, add(max_val, min_val))
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值相乘
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
}

test "string_special_characters" {
  // 测试特殊字符字符串
  assert_eq_string("Hello, John&Doe!", greet("John&Doe"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, 123#$%!", greet("123#$%"))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(multiply(x, add(y, 10)), add(multiply(x, y), multiply(x, 10)))
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  
  // 计算商品总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let subtotal = add(total1, total2)
  
  // 应用折扣
  let final_total = add(subtotal, -discount)
  
  assert_eq(647, final_total)
}

test "error_handling" {
  // 测试错误处理
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  assert_true(add(max_val, 1) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}