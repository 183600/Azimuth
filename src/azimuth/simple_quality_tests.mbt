// 简单的质量测试套件
// 验证基本功能

test "basic_addition_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "basic_multiplication_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "basic_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "basic_divide_with_ceil_test" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  // 测试加法交换律
  assert_eq(add(7, 13), add(13, 7))
  
  // 测试乘法交换律
  assert_eq(multiply(7, 13), multiply(13, 7))
  
  // 测试零元素性质
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
}

test "business_calculation_scenario" {
  // 简单业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total = add(subtotal, shipping)
  
  assert_eq(200, subtotal)
  assert_eq(215, total)
}

test "packaging_calculation" {
  // 包装计算场景
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
}

test "unicode_and_special_characters" {
  // 测试 Unicode 和特殊字符
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "error_handling" {
  // 测试错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}