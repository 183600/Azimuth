// 标准 MoonBit 测试用例 - 包含 10 个核心功能测试用例
test "basic_addition" {
  assert_eq(5, add(2, 3))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "greeting_function" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "divide_with_ceil_function" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "zero_operations" {
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "negative_operations" {
  assert_eq(-5, add(-2, -3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "large_number_operations" {
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "combined_operations" {
  let result1 = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result1)
}

test "practical_packaging" {
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)
}

test "special_characters" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}