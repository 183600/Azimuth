// 高质量 MoonBit 测试套件 - 新增标准测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(add(2, 3), 5)
  assert_eq(add(-2, 3), 1)
  assert_eq(add(0, 0), 0)
  assert_eq(add(100, 200), 300)
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(7, 8), 56)
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(100, 50), 50)
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(divide_with_ceil(10, 0), 0)  // 除数为0的情况
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "greet_function_basic" {
  // 测试问候函数的基本功能
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
}

test "complex_calculation_sequence" {
  // 测试复合计算序列
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
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(add(x, 0), x)
  
  // 乘法单位元
  assert_eq(multiply(x, 1), x)
}

test "practical_application_calculations" {
  // 测试实际应用计算
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(area, 40)
  assert_eq(perimeter, 26)
}