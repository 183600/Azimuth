test "basic_addition" {
  2 + 3 = 5
}

test "add_function_extended" {
  add(7, 3) = 10
  add(0, 0) = 0
  add(-2, -3) = -5
}

test "multiply_function_basic" {
  multiply(2, 3) = 6
  multiply(5, 0) = 0
  multiply(-2, 3) = -6
}

test "greet_function_simple" {
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
}

test "divide_with_ceil_basic" {
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(5, 0) = 0
}

test "mathematical_commutative" {
  add(7, 13) = add(13, 7)
  multiply(7, 13) = multiply(13, 7)
}

test "zero_element_properties" {
  add(5, 0) = 5
  multiply(5, 0) = 0
}

test "complex_arithmetic" {
  add(multiply(2, 3), multiply(4, 5)) = 26
}

test "packaging_calculation" {
  divide_with_ceil(25, 6) = 5
}

test "unicode_greeting" {
  greet("世界") = "Hello, 世界!"
  greet("🚀") = "Hello, 🚀!"
}