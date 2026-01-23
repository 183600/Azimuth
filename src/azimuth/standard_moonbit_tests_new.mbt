// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的标准测试

test "add_function_standard" {
  // 测试加法功能的标准情况
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_standard" {
  // 测试乘法功能的标准情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 7))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_standard" {
  // 测试问候功能的标准情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_standard" {
  // 测试向上取整除法的标准情况
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "identity_elements" {
  // 测试单位元
  // 加法单位元
  assert_eq(5, add(5, 0))
  assert_eq(-3, add(-3, 0))
  
  // 乘法单位元
  assert_eq(7, multiply(7, 1))
  assert_eq(-4, multiply(-4, 1))
}

test "practical_calculation_scenario" {
  // 测试实际计算场景
  let unit_price = 25
  let quantity = 4
  let tax_rate = 8
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  
  assert_eq(100, subtotal)
  assert_eq(8, tax)
  assert_eq(108, total)
}

test "packaging_calculation" {
  // 测试包装计算场景
  let total_items = 23
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(2, divide_with_ceil(-7, -3))
}

test "unicode_string_support" {
  // 测试Unicode字符串支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}