// 标准 MoonBit 测试用例 - 为 azimuth 项目添加的标准测试用例

test "add_function_basic" {
  assert_eq(7, add(3, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_function_basic" {
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "subtract_function_basic" {
  assert_eq(2, subtract(8, 6))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_positive" {
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(3, 5))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_function_standard" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_commutative" {
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_workflow" {
  let items = 35
  let capacity = 8
  let boxes = divide_with_ceil(items, capacity)
  let total_capacity = multiply(boxes, capacity)
  let remaining = subtract(total_capacity, items)
  
  assert_eq(5, boxes)
  assert_eq(40, total_capacity)
  assert_eq(5, remaining)
}

test "negative_operations" {
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "unicode_characters" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "divide_zero_divisor" {
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
}