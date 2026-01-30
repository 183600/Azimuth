// 简单的标准 MoonBit 测试用例

test "simple_add_test" {
  add(2, 3) = 5
  add(0, 0) = 0
  add(-5, 5) = 0
}

test "simple_multiply_test" {
  multiply(3, 4) = 12
  multiply(5, 0) = 0
  multiply(-2, 3) = -6
}

test "simple_greet_test" {
  greet("World") = "Hello, World!"
  greet("MoonBit") = "Hello, MoonBit!"
  greet("") = "Hello, !"
}

test "simple_divide_ceil_test" {
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(9, 3) = 3
  divide_with_ceil(0, 5) = 0
}

test "simple_subtract_test" {
  subtract(10, 3) = 7
  subtract(5, 5) = 0
  subtract(3, 10) = -7
}