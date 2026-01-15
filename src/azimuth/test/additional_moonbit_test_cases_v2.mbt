// 额外的 MoonBit 测试用例 v2
// 测试 azimuth 包的基本功能

test "add_function_basic" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "add_function_mixed_signs" {
  // 测试混合符号的加法
  assert_eq(0, add(5, -5))
  assert_eq(-2, add(3, -5))
  assert_eq(2, add(-3, 5))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_function_large_numbers" {
  // 测试大数乘法
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
}

test "greet_function_standard" {
  // 测试标准的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "boundary_values_add" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "boundary_values_multiply" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(max_val, multiply(max_val, 1))
}

test "complex_calculation" {
  // 测试复杂计算场景
  // 计算 (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = add(a, b)
  let product = multiply(sum_ab, c)
  let result = add(product, -d)
  
  assert_eq(75, result)
}

test "real_world_scenario" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  
  assert_eq(697, cart_total)
}