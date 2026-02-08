// 简单标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法编写的10个基础测试用例

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(2 + 3, 5)
  assert_eq(0 + 0, 0)
  assert_eq(-3 + 2, -1)
  assert_eq(50 + 50, 100)
  assert_eq(-50 + -50, -100)
}

test "basic_arithmetic_multiplication" {
  // 基本乘法运算测试
  assert_eq(2 * 3, 6)
  assert_eq(5 * 0, 0)
  assert_eq(-3 * 5, -15)
  assert_eq(-3 * -5, 15)
  assert_eq(100 * 100, 10000)
}

test "basic_arithmetic_subtraction" {
  // 基本减法运算测试
  assert_eq(10 - 5, 5)
  assert_eq(5 - 10, -5)
  assert_eq(15 - 15, 0)
  assert_eq(10 - -10, 20)
  assert_eq(-10 - 10, -20)
}

test "basic_arithmetic_division" {
  // 基本除法运算测试
  assert_eq(10 / 3, 3)
  assert_eq(9 / 3, 3)
  assert_eq(1 / 5, 0)
  assert_eq(0 / 10, 0)
}

test "basic_arithmetic_modulo" {
  // 基本取模运算测试
  assert_eq(10 % 3, 1)
  assert_eq(9 % 3, 0)
  assert_eq(1 % 5, 1)
  assert_eq(0 % 10, 0)
}

test "integer_comparisons" {
  // 整数比较测试
  assert_eq(5 > 3, true)
  assert_eq(5 < 3, false)
  assert_eq(5 >= 5, true)
  assert_eq(5 <= 5, true)
  assert_eq(5 == 5, true)
  assert_eq(5 != 3, true)
}

test "boolean_operations" {
  // 布尔运算测试
  assert_eq(true && true, true)
  assert_eq(true && false, false)
  assert_eq(true || false, true)
  assert_eq(false || false, false)
  assert_eq(!true, false)
  assert_eq(!false, true)
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(a + b, b + a)
  
  // 乘法交换律
  assert_eq(a * b, b * a)
  
  // 加法单位元
  assert_eq(a + 0, a)
  
  // 乘法单位元
  assert_eq(a * 1, a)
}

test "boundary_values" {
  // 边界值测试
  // 零值测试
  assert_eq(0 + 0, 0)
  assert_eq(0 * 5, 0)
  assert_eq(0 - 0, 0)
  
  // 单位值测试
  assert_eq(1 + 0, 1)
  assert_eq(1 * 5, 5)
  assert_eq(1 - 0, 1)
  assert_eq(5 / 1, 5)
}

test "complex_expressions" {
  // 复杂表达式测试
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = (a + b) * c
  assert_eq(result1, 16)
  
  // a * b + c
  let result2 = a * b + c
  assert_eq(result2, 17)
  
  // (a - b) / c
  let result3 = (a - b) / c
  assert_eq(result3, 1)
  
  // a + b * c
  let result4 = a + b * c
  assert_eq(result4, 11)
}