// Azimuth 项目简单标准 MoonBit 测试用例
// 包含5个基本测试用例，使用标准 MoonBit 测试语法

test "addition_test" {
  // 基本加法测试
  let result = add(2, 3)
  assert_eq(5, result)
}

test "multiplication_test" {
  // 基本乘法测试
  let result = multiply(3, 4)
  assert_eq(12, result)
}

test "subtraction_test" {
  // 基本减法测试
  let result = subtract(10, 3)
  assert_eq(7, result)
}

test "greet_test" {
  // 问候函数测试
  let result = greet("World")
  assert_eq_string("Hello, World!", result)
}

test "division_test" {
  // 除法测试
  let result = divide_with_ceil(10, 3)
  assert_eq(4, result)
}