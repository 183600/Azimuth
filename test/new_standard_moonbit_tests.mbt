// 新增 MoonBit 测试用例 - 10个标准测试
// 简洁的标准 MoonBit 测试语法

test "add_basic" {
  assert_eq(10, add(3, 7))
}

test "multiply_basic" {
  assert_eq(15, multiply(3, 5))
}

test "greet_basic" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "zero_operations" {
  assert_eq(0, multiply(5, 0))
  assert_eq(5, add(5, 0))
}

test "negative_operations" {
  assert_eq(-5, add(-2, -3))
  assert_eq(-6, multiply(-2, 3))
}

test "combined_operations" {
  let result = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, result)
}

test "packaging_calculation" {
  let total_items = 25
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  assert_eq(5, boxes_needed)
}

test "unicode_greeting" {
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "error_handling" {
  assert_eq(0, divide_with_ceil(100, 0))
}