// 简单的额外测试用例
// 测试 azimuth 包的基本功能

test "simple_addition_test" {
  // 测试简单加法
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(10, add(5, 5))
}

test "simple_multiplication_test" {
  // 测试简单乘法
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_greet_test" {
  // 测试简单问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "boundary_value_test" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "assert_functions_test" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "simple_calculation_test" {
  // 测试简单计算场景
  let length = 10
  let width = 5
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(50, area)
  assert_eq(30, perimeter)
}

test "string_processing_test" {
  // 测试字符串处理
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "测试"
  
  assert_eq_string("Hello, Alice!", greet(name1))
  assert_eq_string("Hello, Bob!", greet(name2))
  assert_eq_string("Hello, 测试!", greet(name3))
}

test "mathematical_properties_test" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "error_handling_test" {
  // 测试错误处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
}

test "real_world_scenario_test" {
  // 测试实际应用场景：购物车计算
  let item_price = 100
  let quantity = 3
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(315, total)
}