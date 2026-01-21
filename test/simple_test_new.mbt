// 简单测试用例
test "simple_add_test" {
  let result = 2 + 3
  assert_eq(5, result)
}

// 新增 MoonBit 测试用例
// 覆盖更多边界情况和实际应用场景

test "ceil_division_basic" {
  assert_eq(3, divide_with_ceil(10, 4))
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(1, divide_with_ceil(1, 1))
}

test "overflow_protection" {
  let max_val = 2147483647
  let result = add(max_val, 100)
  assert_eq(max_val, result)
}

test "negative_operations" {
  let min_val = -2147483648
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(min_val, 0))
}

test "special_characters" {
  let special = "!@#$%^&*()"
  assert_eq_string("Hello, !@#$%^&*()!", greet(special))
}

test "fibonacci_sum" {
  let sum = add(add(add(add(1, 1), 2), 3), 5)
  assert_eq(12, sum)
}

test "compound_interest" {
  let principal = 1000
  let rate = 5
  let years = 3
  let total = multiply(principal, add(100, multiply(rate, years))) / 100
  assert_eq(1600, total)
}

test "physics_calculation" {
  let velocity = 10
  let time = 3
  let distance = multiply(velocity, time)
  assert_eq(30, distance)
}

test "permutation_calculation" {
  let n = 6
  let perm = multiply(multiply(n, n - 1), n - 2)
  assert_eq(120, perm)
}

test "complexity_analysis" {
  let n = 100
  let quadratic = multiply(n, n)
  assert_eq(10000, quadratic)
}

test "boundary_stress" {
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(-1, add(max_val, min_val))
}