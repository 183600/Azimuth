// 简单的新测试用例

test "simple_addition_test" {
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 10))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_greeting_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "simple_division_test" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "simple_combined_operations" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)  // 2*3 + 4*5 = 6 + 20 = 26
}

test "simple_negative_operations" {
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_packaging_calculation" {
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
}

test "simple_business_calculation" {
  let price = 15
  let quantity = 4
  let total_cost = multiply(price, quantity)
  assert_eq(60, total_cost)
}