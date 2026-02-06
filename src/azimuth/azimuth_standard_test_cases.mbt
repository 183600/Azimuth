// 标准 MoonBit 测试用例 - 10个核心测试
// 测试 azimuth 库的基本功能

test "addition_basic" {
  // 测试基本加法运算
  assert_eq(add(2, 3), 5)
  assert_eq(add(-1, 1), 0)
  assert_eq(add(0, 0), 0)
  assert_eq(add(100, 200), 300)
}

test "multiplication_basic" {
  // 测试基本乘法运算
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(-2, 5), -10)
  assert_eq(multiply(0, 100), 0)
  assert_eq(multiply(7, 8), 56)
}

test "subtraction_basic" {
  // 测试基本减法运算
  assert_eq(subtract(10, 3), 7)
  assert_eq(subtract(5, 10), -5)
  assert_eq(subtract(0, 0), 0)
  assert_eq(subtract(100, 50), 50)
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(15, 5), 3)
}

test "greet_function" {
  // 测试问候函数
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_arithmetic" {
  // 测试复合算术运算
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

test "negative_operations" {
  // 测试负数运算
  assert_eq(add(-5, -3), -8)
  assert_eq(multiply(-4, -6), 24)
  assert_eq(subtract(-10, -5), -5)
  assert_eq(divide_with_ceil(-10, 3), -3)
}

test "edge_cases" {
  // 测试边界情况
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(multiply(1, 0), 0)
  assert_eq(add(0, 100), 100)
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

test "practical_calculations" {
  // 测试实际应用计算
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  assert_eq(area, 40)
  assert_eq(perimeter, 26)
  
  // 计算平均值（向上取整）
  let sum = add(add(15, 20), 25)
  let count = 3
  let average = divide_with_ceil(sum, count)
  
  assert_eq(sum, 60)
  assert_eq(average, 20)
}