// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic" {
  // 测试基本加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
}

test "multiply_function_basic" {
  // 测试基本乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "subtract_function_basic" {
  // 测试基本减法功能
  assert_eq(7, subtract(10, 3))
  assert_eq(-7, subtract(3, 10))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "greet_function_basic" {
  // 测试问候函数基本功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "complex_calculation" {
  // 测试复合计算
  let a = 5
  let b = 3
  let result = add(multiply(a, b), subtract(a, b))
  assert_eq(17, result)  // 5*3 + (5-3) = 15 + 2 = 17
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
}

test "edge_cases" {
  // 测试边界情况
  // 测试负数运算
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-3, -5))
  
  // 测试零值
  assert_eq(0, multiply(0, 100))
  assert_eq(100, subtract(100, 0))
}

test "string_operations_edge" {
  // 测试字符串操作边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, A!", greet("A"))
}