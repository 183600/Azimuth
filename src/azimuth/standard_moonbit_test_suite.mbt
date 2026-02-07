// 标准 MoonBit 测试套件
// 包含10个高质量的测试用例，使用标准 MoonBit 测试语法

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
  assert_eq(100, add(50, 50))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "basic_subtraction_functionality" {
  // 测试基本减法功能
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
}

test "divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !@#!", greet("!@#"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "assert_eq_function_basic" {
  // 测试相等断言函数的基本用法
  assert_eq(5, 5)
  assert_eq(0, 0)
  assert_eq(-10, -10)
  assert_eq(1000000, 1000000)
}

test "assert_eq_string_function_basic" {
  // 测试字符串相等断言函数的基本用法
  assert_eq_string("hello", "hello")
  assert_eq_string("", "")
  assert_eq_string("12345", "12345")
  assert_eq_string("中文测试", "中文测试")
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let a = 10
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 26)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 32)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 4)
}