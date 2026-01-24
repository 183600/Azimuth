// 标准 MoonBit 单元测试
// 针对 azimuth 库核心功能的测试用例

test "add_simple_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "multiply_simple_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
}

test "greet_simple_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_simple_test" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties_test" {
  let a = 12
  let b = 8
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "identity_elements_test" {
  assert_eq(15, add(15, 0))
  assert_eq(15, multiply(15, 1))
  assert_eq(0, multiply(15, 0))
}

test "complex_calculation_test" {
  let base_price = 25
  let quantity = 4
  let tax_rate = 8
  let shipping = 10
  
  let subtotal = multiply(base_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(100, subtotal)
  assert_eq(8, tax)
  assert_eq(118, total)
}

test "edge_case_test" {
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq_string("Hello, A!", greet("A"))
}

test "unicode_test" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
}

test "business_calculation_test" {
  let items_per_box = 12
  let total_items = 47
  let cost_per_item = 5
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_cost = multiply(total_items, cost_per_item)
  
  assert_eq(4, boxes_needed)
  assert_eq(235, total_cost)
}