// 标准 MoonBit 测试用例 - 针对 azimuth 库核心功能
// 包含对基本数学和字符串函数的测试

test "add_function_basic_test" {
  // 测试基本的加法功能
  assert_eq(7, add(3, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_function_basic_test" {
  // 测试基本的乘法功能
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "greet_function_basic_test" {
  // 测试基本的问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic_test" {
  // 测试基本的向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_test" {
  // 测试数学交换律
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "negative_number_operations_test" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "large_number_operations_test" {
  // 测试大数运算
  assert_eq(2000, add(1000, 1000))
  assert_eq(10000, multiply(100, 100))
}

test "real_world_application_test" {
  // 测试实际应用场景
  let price = 25
  let quantity = 4
  let tax_rate = 8
  let subtotal = multiply(price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  assert_eq(108, total)
}