// 新的 MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "basic_addition_test" {
  // 测试 add 函数的基本功能
  assert_eq(5, azimuth::add(2, 3))
  assert_eq(0, azimuth::add(0, 0))
  assert_eq(-5, azimuth::add(-2, -3))
}

test "basic_multiplication_test" {
  // 测试 multiply 函数的基本功能
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(-6, azimuth::multiply(2, -3))
}

test "basic_greet_test" {
  // 测试 greet 函数的基本功能
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
}

test "boundary_addition_test" {
  // 测试 add 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
}

test "boundary_multiplication_test" {
  // 测试 multiply 函数的边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  assert_eq(0, azimuth::multiply(max_val, 0))
}

test "complex_calculation_test" {
  // 测试复杂计算场景
  let length = 10
  let width = 5
  
  let area = azimuth::multiply(length, width)
  let perimeter = azimuth::add(azimuth::multiply(2, length), azimuth::multiply(2, width))
  
  assert_eq(50, area)
  assert_eq(30, perimeter)
}

test "financial_calculation_test" {
  // 测试金融计算示例
  let original_price = 1000
  let discount_percentage = 20
  let discount_amount = azimuth::multiply(original_price, discount_percentage) / 100
  let final_price = azimuth::add(original_price, -discount_amount)
  
  assert_eq(800, final_price)
}

test "string_processing_test" {
  // 测试字符串处理的边界情况
  let special_chars = "!@#$%^&*()"
  let unicode_chars = "测试"
  
  assert_eq_string("Hello, !@#$%^&*()!", azimuth::greet(special_chars))
  assert_eq_string("Hello, 测试!", azimuth::greet(unicode_chars))
}

test "mathematical_series_test" {
  // 测试数学级数计算
  let n = 5
  let sum = azimuth::multiply(n, azimuth::add(1, n)) / 2
  assert_eq(15, sum)
}

test "shopping_cart_test" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  let subtotal = azimuth::add(azimuth::multiply(item1_price, item1_quantity), azimuth::multiply(item2_price, item2_quantity))
  let tax = azimuth::multiply(subtotal, 10) / 100
  let total = azimuth::add(azimuth::add(subtotal, tax), shipping)
  
  assert_eq(786, total)
}