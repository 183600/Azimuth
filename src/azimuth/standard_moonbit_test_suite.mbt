// 标准 MoonBit 测试套件
// 针对 azimuth 库核心功能的标准测试用例

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(add(7, 13), add(13, 7))
  assert_eq(add(-5, 10), add(10, -5))
  assert_eq(add(0, 100), add(100, 0))
}

test "multiply_function_zero_property" {
  // 测试乘法零元素性质
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-10, 0))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(0, divide_with_ceil(0, 10))  // 0/10 = 0 -> 0
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total)
}

test "mathematical_distributive_property" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 9
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "edge_case_boundary_values" {
  // 测试边界值
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "string_greeting_special_characters" {
  // 测试特殊字符字符串问候
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
  assert_eq_string("Hello, !", greet(""))
}

test "real_world_packaging_scenario" {
  // 真实世界包装场景
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
}