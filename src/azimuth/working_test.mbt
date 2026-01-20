test "new_addition_test" {
  let sum = azimuth::add(2, 3)
  assert_eq(5, sum)
}