test "add_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-2, 1))
}

test "add_edge_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(-10, add(-3, -7))
}

test "multiply_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_edge_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(46340, 46341))
  assert_eq(-100, multiply(-10, 10))
  assert_eq(16, multiply(4, 4))
}

test "greet_function" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "divide_with_ceil_function" {
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "assert_functions" {
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "complex_arithmetic" {
  let a = 10
  let b = 20
  let c = -5
  
  assert_eq(200, multiply(a, b))
  assert_eq(30, add(a, b))
  assert_eq(5, add(a, c))
  assert_eq(50, multiply(add(a, b), c))
}