// 标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_addition" {
  add(2, 3) = 5
  add(0, 0) = 0
  add(-5, 5) = 0
  add(-3, -7) = -10
}

test "basic_arithmetic_multiplication" {
  multiply(3, 4) = 12
  multiply(5, 0) = 0
  multiply(-2, 3) = -6
  multiply(-3, -3) = 9
}

test "basic_arithmetic_subtraction" {
  subtract(10, 3) = 7
  subtract(5, 5) = 0
  subtract(3, 10) = -7
  subtract(-5, -3) = -2
}

test "divide_with_ceil_functionality" {
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(1, 2) = 1
  divide_with_ceil(0, 5) = 0
  divide_with_ceil(5, 0) = 0
}

test "greet_function_basic" {
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
  greet("") = "Hello, !"
  greet("123") = "Hello, 123!"
}

test "mathematical_commutative_properties" {
  add(7, 13) = add(13, 7)
  multiply(7, 13) = multiply(13, 7)
}

test "mathematical_identity_properties" {
  add(5, 0) = 5
  multiply(5, 1) = 5
  multiply(5, 0) = 0
  subtract(5, 0) = 5
}

test "complex_calculation_chain" {
  add(multiply(2, 3), multiply(4, 5)) = 26
  divide_with_ceil(add(10, 5), 4) = 4
  subtract(multiply(6, 7), divide_with_ceil(20, 3)) = 38
}

test "business_logic_packaging" {
  divide_with_ceil(25, 6) = 5
  divide_with_ceil(47, 12) = 4
  divide_with_ceil(100, 25) = 4
}

test "unicode_and_special_characters" {
  greet("世界") = "Hello, 世界!"
  greet("🚀") = "Hello, 🚀!"
  greet("café") = "Hello, café!"
  greet("@#$%") = "Hello, @#$%!"
}