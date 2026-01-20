test "verify_simple_add" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "verify_simple_multiply" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}