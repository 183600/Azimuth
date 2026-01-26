// Azimuth 新增测试用例
// 使用标准的 MoonBit 测试语法，测试 azimuth 库的核心功能

test "new_add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(45, 55))
}

test "new_multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(0, 7))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(64, multiply(8, 8))
}

test "new_greet_standard_cases" {
  // 测试标准问候功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, User!", greet("User"))
  assert_eq_string("Hello, !", greet(""))
}

test "new_divide_with_ceil_standard_cases" {
  // 测试向上取整除法的标准情况
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(4, 2))
  assert_eq(1, divide_with_ceil(5, 10))
  assert_eq(0, divide_with_ceil(7, 0))
}

test "new_mathematical_commutativity" {
  // 测试数学交换律
  let x = 12
  let y = 23
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "new_zero_element_properties" {
  // 测试零元素性质
  assert_eq(15, add(15, 0))
  assert_eq(0, multiply(15, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "new_negative_number_division" {
  // 测试负数除法
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(-1, divide_with_ceil(-5, 10))
}

test "new_complex_calculation_scenario" {
  // 复杂计算场景：计算订单总价
  let item_price = 35
  let quantity = 4
  let shipping = 12
  let subtotal = multiply(item_price, quantity)
  let total = add(subtotal, shipping)
  assert_eq(140, subtotal)
  assert_eq(152, total)
}

test "new_packaging_efficiency" {
  // 包装效率计算
  let products = 37
  let capacity = 8
  let packages = divide_with_ceil(products, capacity)
  assert_eq(5, packages)
}

test "new_unicode_greeting_support" {
  // Unicode 问候支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, Café!", greet("Café"))
}