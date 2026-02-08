// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-5, 10), 5)
  assert_eq(add(100, 200), 300)
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 5), 0)
  assert_eq(multiply(-2, 6), -12)
  assert_eq(multiply(-3, -3), 9)
}

test "basic_subtraction_functionality" {
  // 测试基本减法功能
  assert_eq(subtract(10, 5), 5)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(-5, -10), 5)
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
  // 测试断言相等功能
  assert_eq(5, 5)
  assert_eq(0, 0)
  assert_eq(-10, -10)
  assert_eq(1000, 1000)
}

test "assert_eq_string_functionality" {
  // 测试字符串断言相等功能
  assert_eq_string("test", "test")
  assert_eq_string("", "")
  assert_eq_string("Hello, World!", "Hello, World!")
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let a = 5
  let b = 3
  let result1 = multiply(add(a, b), 2)  // (5 + 3) * 2 = 16
  let result2 = divide_with_ceil(subtract(20, 5), 3)  // ceil(15/3) = 5
  assert_eq(result1, 16)
  assert_eq(result2, 5)
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