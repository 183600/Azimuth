// 简洁核心测试用例 - 8个标准 MoonBit 测试
test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(add(2, 3), 5)
  assert_eq(add(-1, 1), 0)
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

test "division_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(-7, 3), -2)
  assert_eq(divide_with_ceil(7, -3), 2)
  assert_eq(divide_with_ceil(-7, -3), 3)
}

test "greet_function_basic" {
  // 测试问候函数基本功能
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
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(result3, 1)
}

test "edge_case_zero_operations" {
  // 测试零值边界情况
  assert_eq(add(0, 5), 5)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(5, 0), 5)
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(5, 0), 0)
}

test "mathematical_properties" {
  // 测试基本数学性质
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