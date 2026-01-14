// MoonBit 标准测试用例
// 测试 azimuth 包中的核心功能

test "add_function_basic" {
  // 基本加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-10, add(-3, -7))
}

test "add_function_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_function_basic" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(3, 0))
  assert_eq(3, multiply(3, 1))
}

test "multiply_function_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, multiply(46341, 46341))
  assert_eq(min_val, multiply(-46341, 46341))
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_function_basic" {
  // 基本字符串拼接测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_function_special_characters" {
  // 特殊字符测试
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let a = 12
  let b = 8
  let c = 5
  
  // 计算 (a + b) * c
  let sum = add(a, b)
  let result = multiply(sum, c)
  
  assert_eq(100, result)
}

test "real_world_application" {
  // 实际应用场景测试：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  // 计算商品总价
  let subtotal = multiply(item_price, quantity)
  // 计算最终总价（含运费）
  let total = add(subtotal, shipping)
  
  assert_eq(617, total)
}