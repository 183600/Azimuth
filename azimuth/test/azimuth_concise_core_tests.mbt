// Azimuth 核心功能简洁测试用例
// 包含8个标准 MoonBit 测试用例，覆盖主要功能

test "basic_addition" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "greet_functionality" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_properties" {
  let x = 7
  let y = 13
  assert_eq(add(x, y), add(y, x))
  assert_eq(multiply(x, y), multiply(y, x))
}

test "business_packaging_scenario" {
  let items = 25
  let capacity = 6
  let boxes = divide_with_ceil(items, capacity)
  assert_eq(5, boxes)
  assert_eq(30, multiply(boxes, capacity))
}

test "negative_number_operations" {
  assert_eq(-10, add(-5, -5))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "complex_calculation_chain" {
  let result = divide_with_ceil(add(multiply(2, 3), multiply(4, 5)), 4)
  assert_eq(7, result)  // ceil((6 + 20)/4) = ceil(26/4) = 7
}