// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(2 + 3, 5)
  assert_eq(add(10, 15), 25)
  assert_eq(add(-5, 5), 0)
  assert_eq(add(0, 0), 0)
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(4 * 5, 20)
  assert_eq(multiply(6, 7), 42)
  assert_eq(multiply(-3, 3), -9)
  assert_eq(multiply(0, 100), 0)
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  assert_eq(10 - 3, 7)
  assert_eq(subtract(15, 5), 10)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), -3)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(5, 0), 0)
}

test "greet_function" {
  // 测试字符串拼接函数
  assert_eq(greet("World"), "Hello, World!")
  assert_eq(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq(greet(""), "Hello, !")
  assert_eq(greet("123"), "Hello, 123!")
}

test "string_assertions" {
  // 测试字符串断言
  assert_eq_string("hello", "hello")
  assert_eq_string("", "")
  assert_eq_string("MoonBit", "MoonBit")
  assert_eq_string("12345", "12345")
}

test "complex_arithmetic_expressions" {
  // 测试复合算术表达式
  let result1 = add(multiply(3, 4), subtract(10, 2))
  let result2 = multiply(add(5, 3), subtract(10, 5))
  let result3 = divide_with_ceil(add(multiply(2, 3), 4), 5)
  
  assert_eq(result1, 20)
  assert_eq(result2, 40)
  assert_eq(result3, 2)
}

test "edge_cases_and_boundary_values" {
  // 测试边界值和特殊情况
  assert_eq(add(1000000, 1000000), 2000000)
  assert_eq(multiply(10000, 10000), 100000000)
  assert_eq(subtract(-1000000, 1000000), -2000000)
  assert_eq(divide_with_ceil(1, 1000000), 1)
}

test "mathematical_properties" {
  // 测试数学性质
  // 加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 减法性质
  assert_eq(subtract(a, b), -subtract(b, a))
  
  // 零元素性质
  assert_eq(add(a, 0), a)
  assert_eq(multiply(a, 0), 0)
}