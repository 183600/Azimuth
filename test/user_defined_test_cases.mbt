// 用户定义的测试用例
test "user_add_test" {
  assert_eq(5, add(2, 3))
}

test "user_multiply_test" {
  assert_eq(6, multiply(2, 3))
}

test "user_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "user_divide_test" {
  assert_eq(4, divide_with_ceil(10, 3))
}

test "user_math_test" {
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
}

test "user_edge_test" {
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq_string("Hello, !", greet(""))
}

test "user_unicode_test" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "user_complex_test" {
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = add(step3, 1)          // 5
  assert_eq(5, step4)
}