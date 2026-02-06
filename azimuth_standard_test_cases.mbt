// 标准MoonBit测试用例 - 10个高质量测试用例
// 测试azimuth库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(2, add(1, 1))
  assert_eq(5, add(0, 5))
  assert_eq(4, add(-3, 7))
  assert_eq(300, add(100, 200))
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 10))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(56, multiply(7, 8))
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  assert_eq(2, subtract(5, 3))
  assert_eq(10, subtract(10, 0))
  assert_eq(-4, subtract(3, 7))
  assert_eq(50, subtract(100, 50))
}

test "division_with_ceil_basic" {
  // 测试向上取整除法的基本情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "division_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(-1, 2))
}

test "division_edge_cases" {
  // 测试除法的边界情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "string_greet_function" {
  // 测试字符串问候函数
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_calculation_sequence" {
  // 测试复合计算序列
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
  
  // (a - b) / c (向上取整)
  let result3 = divide_with_ceil(subtract(a, b), c)
  assert_eq(1, result3)
}

test "mathematical_properties_verification" {
  // 测试数学性质的验证
  let x = 7
  let y = 3
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  
  // 减法的逆运算
  assert_eq(x, add(subtract(x, y), y))
}

test "boundary_value_analysis" {
  // 测试边界值分析
  // 测试零值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, subtract(0, 0))
  
  // 测试单位值
  assert_eq(1, add(1, 0))
  assert_eq(5, multiply(1, 5))
  assert_eq(1, subtract(1, 0))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 测试负一
  assert_eq(0, add(-1, 1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(1, subtract(0, -1))
}