// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "basic_addition" {
  assert_eq(5, add(2, 3))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "basic_greet" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "basic_divide_with_ceil" {
  assert_eq(3, divide_with_ceil(8, 3))
}

test "basic_subtract" {
  assert_eq(2, subtract(5, 3))
}

test "mathematical_commutative" {
  let a = 12
  let b = 7
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "negative_operations" {
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "boundary_values" {
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "business_logic" {
  let total_items = 35
  let items_per_package = 8
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  assert_eq(5, packages_needed)
}

test "error_handling" {
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}