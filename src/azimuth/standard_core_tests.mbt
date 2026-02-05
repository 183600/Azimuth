test "int_basic_arithmetic" {
  assert_eq(1 + 1, 2)
  assert_eq(10 - 3, 7)
  assert_eq(4 * 5, 20)
  assert_eq(15 / 3, 5)
  assert_eq(17 % 5, 2)
}

test "int_comparisons" {
  assert_eq(5 > 3, true)
  assert_eq(5 >= 5, true)
  assert_eq(3 < 5, true)
  assert_eq(5 <= 5, true)
  assert_eq(5 == 5, true)
  assert_eq(5 != 3, true)
}

test "int_bitwise_operations" {
  assert_eq(1 << 3, 8)
  assert_eq(8 >> 1, 4)
  assert_eq(6 & 3, 2)
  assert_eq(5 | 3, 7)
  assert_eq(5 ^ 3, 6)
}

test "int_negative_operations" {
  assert_eq(-5 + 3, -2)
  assert_eq(-5 * 3, -15)
  assert_eq(-10 / 2, -5)
  assert_eq(-10 % 3, -1)
}

test "int_zero_operations" {
  assert_eq(0 + 5, 5)
  assert_eq(0 * 5, 0)
  assert_eq(5 - 0, 5)
  assert_eq(5 / 1, 5)
}

test "string_operations" {
  assert_eq_string("Hello" + " " + "World", "Hello World")
  assert_eq_string("", "")
  assert_eq_string("Test", "Test")
}

test "boolean_logic" {
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(false || false, false)
  assert_eq(!true, false)
  assert_eq(!false, true)
}

test "complex_expressions" {
  assert_eq((2 + 3) * 4, 20)
  assert_eq(10 * (5 + 3), 80)
  assert_eq(2 + 3 * 4, 14)
  assert_eq((10 - 3) * (2 + 1), 21)
}

test "function_composition" {
  assert_eq(add(5, 3), 8)
  assert_eq(multiply(4, 6), 24)
  assert_eq(add(multiply(2, 3), 4), 10)
  assert_eq(multiply(add(2, 3), 4), 20)
}

test "edge_cases" {
  assert_eq(2147483647 + 1, -2147483648)
  assert_eq(-2147483648 - 1, 2147483647)
  assert_eq(0 - 1, -1)
  assert_eq(-1 + 1, 0)
}