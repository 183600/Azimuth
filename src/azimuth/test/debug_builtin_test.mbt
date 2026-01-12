test "azimuth_add_test" {
  let result = @azimuth.add(2, 3)
  if result != 5 {
    @builtin.panic()
  }
}
test "azimuth_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  if result != 6 {
    @builtin.panic()
  }
}
test "azimuth_greet_test" {
  let result = "Hello, World!"
  if result != "Hello, World!" {
    @builtin.panic()
  }
}
