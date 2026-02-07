// 标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，覆盖核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  assert_eq(add(2, 3), 5)
  assert_eq(multiply(4, 5), 20)
  assert_eq(subtract(10, 7), 3)
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("测试"), "Hello, 测试!")
}

test "division_edge_cases" {
  // 测试除法的边界情况
  assert_eq(divide_with_ceil(0, 5), 0)
  assert_eq(divide_with_ceil(0, 1), 0)
  assert_eq(divide_with_ceil(0, 0), 0)
  assert_eq(divide_with_ceil(10, 0), 0)
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), 3)
}

test "negative_number_calculations" {
  // 测试负数计算
  assert_eq(add(-5, 3), -2)
  assert_eq(add(5, -3), 2)
  assert_eq(multiply(-4, 6), -24)
  assert_eq(multiply(4, -6), -24)
  assert_eq(multiply(-4, -6), 24)
  assert_eq(subtract(-10, 7), -17)
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

test "complex_calculation_sequence" {
  // 测试复杂计算序列
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

test "boundary_value_analysis" {
  // 测试边界值分析
  // 零值测试
  assert_eq(add(0, 0), 0)
  assert_eq(multiply(0, 5), 0)
  assert_eq(subtract(0, 0), 0)
  
  // 单位值测试
  assert_eq(add(1, 0), 1)
  assert_eq(multiply(1, 5), 5)
  assert_eq(subtract(1, 0), 1)
  assert_eq(divide_with_ceil(5, 1), 5)
  
  // 负一测试
  assert_eq(add(-1, 1), 0)
  assert_eq(multiply(-1, 5), -5)
  assert_eq(subtract(0, -1), 1)
}

test "precision_and_rounding" {
  // 测试精度和舍入
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(2, 3), 1)
  assert_eq(divide_with_ceil(3, 4), 1)
  assert_eq(divide_with_ceil(100, 99), 2)
  assert_eq(divide_with_ceil(99, 100), 1)
  assert_eq(divide_with_ceil(7, 2), 4)
  assert_eq(divide_with_ceil(8, 2), 4)
}