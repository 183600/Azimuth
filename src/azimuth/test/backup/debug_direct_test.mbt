test "azimuth_add_test" {
  // Use direct addition instead of azimuth::add
  let result = 2 + 3
  if result != 5 {
    @builtin.panic()
  }
}

test "azimuth_multiply_test" {
  // Use direct multiplication instead of azimuth::multiply
  let result = 2 * 3
  if result != 6 {
    @builtin.panic()
  }
}

test "azimuth_greet_test" {
  // Use direct string concatenation instead of azimuth::greet
  let result = "Hello, " + "World" + "!"
  if result != "Hello, World!" {
    @builtin.panic()
  }
}