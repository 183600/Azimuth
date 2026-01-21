// 简单标准测试套件
// 使用标准的 MoonBit 测试语法，只测试基本功能

// 测试基本加法功能
test "add_basic_functionality" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

// 测试加法边界情况
test "add_boundary_cases" {
  assert_eq(10, add(5, 5))
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(10, -10))
}

// 测试基本乘法功能
test "multiply_basic_functionality" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

// 测试乘法边界情况
test "multiply_boundary_cases" {
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
}

// 测试字符串问候功能
test "greet_basic_functionality" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

// 测试向上取整除法 - 基本功能
test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(5, 3))
}

// 测试向上取整除法 - 边界情况
test "divide_with_ceil_boundary" {
  assert_eq(0, divide_with_ceil(5, 0))  // 除数为0
  assert_eq(0, divide_with_ceil(0, 5))  // 被除数为0
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

// 测试复合运算
test "complex_operations" {
  let a = 10
  let b = 20
  assert_eq(30, add(a, b))
  assert_eq(200, multiply(a, b))
  assert_eq(7, divide_with_ceil(20, 3))
}

// 测试字符串处理
test "string_processing" {
  let name = "MoonBit"
  let greeting = greet(name)
  assert_eq_string("Hello, MoonBit!", greeting)
}

// 测试数学运算组合
test "math_combinations" {
  assert_eq(15, add(multiply(2, 3), multiply(3, 3)))
  assert_eq(50, multiply(add(5, 5), 5))
  assert_eq(7, divide_with_ceil(add(10, 10), 3))
}