test "simple_add" {
  let result = @clean_test.add(1, 2)
  @clean_test.assert_eq(3, result)
}

test "simple_multiply" {
  let result = @clean_test.multiply(2, 3)
  @clean_test.assert_eq(6, result)
}

test "simple_greet" {
  let result = @clean_test.greet("World")
  @clean_test.assert_eq_string("Hello, World!", result)
}
