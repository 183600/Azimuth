// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-1, 1), 0)
  assert_eq(add(100, 200), 300)
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 5), 0)
  assert_eq(multiply(1, 7), 7)
  assert_eq(multiply(-2, 3), -6)
  assert_eq(multiply(-2, -3), 6)
}

test "basic_subtraction_functionality" {
  // 测试基本减法功能
  assert_eq(subtract(10, 5), 5)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(100, 1), 99)
}

test "division_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(5, 5), 1)
}

test "division_edge_cases" {
  // 测试除法的边界情况
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("测试"), "Hello, 测试!")
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet(" "), "Hello,  !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_calculation_chain" {
  // 测试复合计算链
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(result1, 16)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(result2, 17)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 4
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}