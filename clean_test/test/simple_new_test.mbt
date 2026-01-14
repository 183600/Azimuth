test "new_basic_add_test" {
  let result = @clean_test.add(2, 3)
  @clean_test.assert_eq(5, result)
}

test "new_basic_multiply_test" {
  let result = @clean_test.multiply(2, 3)
  @clean_test.assert_eq(6, result)
}