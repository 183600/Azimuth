// 新增的 MoonBit 测试用例
// 这些测试用例涵盖了项目的核心功能：add, multiply, greet

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
}

test "add_function_with_zero" {
  // 测试 add 函数与零的运算
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(0, add(0, 0))
  assert_eq(-100, add(-100, 0))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(0, multiply(5, 0))
}

test "multiply_function_with_one" {
  // 测试 multiply 函数与一的运算
  assert_eq(25, multiply(25, 1))
  assert_eq(25, multiply(1, 25))
  assert_eq(-30, multiply(-30, 1))
  assert_eq(-30, multiply(1, -30))
}

test "greet_function_basic_functionality" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, World!", greet("World"))
}

test "greet_function_edge_cases" {
  // 测试 greet 函数的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "combined_operations_simple" {
  // 测试简单的组合运算
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)  // 6 + 20 = 26
  
  let result2 = multiply(add(10, 5), add(3, 2))
  assert_eq(75, result2)  // 15 * 5 = 75
}

test "overflow_protection_add" {
  // 测试 add 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(max_val, add(max_val, max_val))
}

test "overflow_protection_multiply" {
  // 测试 multiply 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "real_world_application_calculations" {
  // 测试实际应用场景的计算
  // 购物车计算：商品价格 × 数量 + 运费
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(413, total)  // 199 * 2 + 15 = 413
}