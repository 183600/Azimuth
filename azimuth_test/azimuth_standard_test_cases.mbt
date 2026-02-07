// 标准 MoonBit 测试用例集
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_addition" {
  assert_eq(5, add(2, 3))
}

test "basic_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "basic_greeting" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "basic_division" {
  assert_eq(3, divide_with_ceil(10, 4))
}

test "basic_subtraction" {
  assert_eq(2, subtract(5, 3))
}

test "zero_operations" {
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, subtract(5, 5))
}

test "negative_operations" {
  assert_eq(-5, add(-2, -3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "large_numbers" {
  assert_eq(2000, add(1000, 1000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(500, divide_with_ceil(1500, 3))
  assert_eq(0, subtract(1000, 1000))
}

test "mixed_operations" {
  assert_eq(0, add(5, -5))
  assert_eq(-15, multiply(5, -3))
  assert_eq(8, subtract(3, -5))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "complex_operations" {
  let x = 10
  let y = 4
  let sum = add(x, y)
  let product = multiply(sum, 2)
  let difference = subtract(product, y)
  let result = divide_with_ceil(difference, 2)
  assert_eq(14, sum)
  assert_eq(28, product)
  assert_eq(24, difference)
  assert_eq(12, result)
}