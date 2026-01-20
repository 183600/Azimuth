// 增强的单元测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试基本的加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(1, 99))
}

test "add_function_zero_cases" {
  // 测试与0相加的情况
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(-17, add(-17, 0))
  assert_eq(-17, add(0, -17))
  assert_eq(0, add(0, 0))
}

test "multiply_function_basic_cases" {
  // 测试基本的乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(8, 0))
}

test "multiply_function_identity_cases" {
  // 测试乘法单位元情况
  assert_eq(25, multiply(25, 1))
  assert_eq(25, multiply(1, 25))
  assert_eq(-25, multiply(-25, 1))
  assert_eq(-25, multiply(1, -25))
  assert_eq(-25, multiply(25, -1))
  assert_eq(-25, multiply(-1, 25))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(6, -3))
  assert_eq(-1, divide_with_ceil(-3, 3))
}

test "boolean_assertions_various_cases" {
  // 测试布尔断言的各种情况
  assert_true(5 > 3)
  assert_true(-2 < 0)
  assert_true(10 == 10)
  assert_false(3 > 5)
  assert_false(-2 > 0)
  assert_false(10 == 5)
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  let a = 12
  let b = 8
  let c = 3
  
  // 计算 (a * b) + c
  let product = multiply(a, b)
  let result = add(product, c)
  assert_eq(99, result)
  
  // 计算 a * (b + c)
  let sum = add(b, c)
  let result2 = multiply(a, sum)
  assert_eq(132, result2)
}