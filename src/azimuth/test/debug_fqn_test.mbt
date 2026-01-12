test "azimuth_add_test" {
  // Try using the fully qualified name
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}
