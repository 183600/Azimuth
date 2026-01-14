// 标准 MoonBit 测试用例 - 新增10个测试
// 这些测试用例使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-3, add(-1, -2))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
}

test "greet_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "add_with_negative_numbers" {
  // 测试包含负数的加法
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-5, -5))
  assert_eq(3, add(10, -7))
}

test "multiply_boundary_values" {
  // 测试边界值的乘法
  assert_eq(2147483647, multiply(2147483647, 1))
  assert_eq(-2147483648, multiply(-2147483648, 1))
  assert_eq(0, multiply(2147483647, 0))
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
}

test "complex_calculations" {
  // 测试复合计算
  // (2 + 3) * 4 - 5 = 5 * 4 - 5 = 20 - 5 = 15
  let result = add(multiply(add(2, 3), 4), -5)
  assert_eq(15, result)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let cart_total = add(total1, total2)
  
  assert_eq(350, cart_total)  // 100*2 + 50*3 = 200 + 150 = 350
}