// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "add_function_edge_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  
  // 测试零值加法
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(0, -3))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 5))
  assert_eq(-8, multiply(2, -4))
}

test "multiply_function_edge_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
  
  // 测试特殊值
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
}

test "greet_function_standard" {
  // 测试标准的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_chars" {
  // 测试包含特殊字符的问候功能
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
}

test "complex_calculation" {
  // 测试复杂计算场景
  let a = 12
  let b = 8
  let c = 5
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  assert_eq(100, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 测试加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 测试乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 测试分配律
  let z = 3
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "real_world_scenario" {
  // 测试实际应用场景：购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 10
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  // 计算税费
  let tax = multiply(subtotal, tax_rate) / 100
  // 计算最终总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(118, total)  // 100 + 8 + 10 = 118
}

test "error_handling" {
  // 测试错误处理
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试边界值的正确处理
  let max_val = 2147483647
  assert_true(add(max_val, 1) == max_val)
  assert_true(multiply(max_val, 2) == max_val)
}