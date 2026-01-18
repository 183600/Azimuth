// 额外的 MoonBit 测试用例
// 包含10个标准测试用例，使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-8, multiply(-2, 4))
}

test "greet_standard_cases" {
  // 测试标准的问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-2, 7))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(6, multiply(-2, -3))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(0, multiply(-5, 0))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, John_Doe!", greet("John_Doe"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, test#1!", greet("test#1"))
}

test "complex_calculations" {
  // 测试复杂计算
  // 计算 (5 + 3) * (10 - 4) = 8 * 6 = 48
  let sum = add(5, 3)
  let diff = add(10, -4)
  let result = multiply(sum, diff)
  assert_eq(48, result)
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算每种商品的总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = add(total1, total2)
  
  // 验证总价：199*2 + 299*1 = 398 + 299 = 697
  assert_eq(697, cart_total)
}