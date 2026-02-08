// Azimuth 简洁测试套件
// 包含 8 个标准 MoonBit 测试用例，测试核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-5, 5), 0)
  assert_eq(add(100, 200), 300)
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 10), 0)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(-3, -3), 9)
}

test "subtraction_operation_test" {
  // 测试减法运算
  assert_eq(subtract(10, 5), 5)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(-5, -5), 0)
}

test "divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
}

test "assert_eq_functionality" {
  // 测试断言函数的功能
  assert_eq(5, 5)
  assert_eq(0, 0)
  assert_eq(-10, -10)
  assert_eq(1000, 1000)
}

test "assert_eq_string_functionality" {
  // 测试字符串断言函数的功能
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
  assert_eq_string("123", "123")
}