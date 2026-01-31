// 简单的 Azimuth 测试用例 - 10个高质量测试用例

test "simple_addition_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_divide_with_ceil_test" {
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(2, divide_with_ceil(8, 4))
  assert_eq(0, divide_with_ceil(0, 4))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "simple_subtract_test" {
  assert_eq(-1, subtract(2, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(10, subtract(15, 5))
}

test "simple_commutative_test" {
  assert_eq(add(3, 7), add(7, 3))
  assert_eq(multiply(3, 7), multiply(7, 3))
}

test "simple_identity_test" {
  let x = 42
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "simple_business_test" {
  let items = 25
  let box_capacity = 6
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(5, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(30, total_capacity)
}

test "simple_pipeline_test" {
  let base = 5
  let multiplier = 3
  let divisor = 4
  let addition = 2
  
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(6, result)
}

test "simple_boundary_test" {
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq_string("Hello, 世界!", greet("世界"))
}