// Azimuth 项目扩展测试用例
// 使用标准 MoonBit 测试语法

test "subtract_operations" {
  // 减法运算测试（通过加法实现）
  assert_eq(5, add(10, -5))
  assert_eq(-3, add(2, -5))
  assert_eq(0, add(7, -7))
}

test "division_simulation" {
  // 除法模拟测试（通过重复减法）
  let dividend = 20
  let divisor = 4
  let mut quotient = 0
  let mut remainder = dividend
  
  // 简单的除法模拟
  while multiply(divisor, quotient) < dividend {
    remainder = add(remainder, -divisor)
    quotient = add(quotient, 1)
  }
  
  assert_eq(5, quotient)
  assert_eq(0, remainder)
}

test "string_concatenation" {
  // 字符串连接测试
  let name1 = "Alice"
  let name2 = "Bob"
  let greeting1 = greet(name1)
  let greeting2 = greet(name2)
  
  assert_eq_string("Hello, Alice!", greeting1)
  assert_eq_string("Hello, Bob!", greeting2)
}

test "negative_number_operations" {
  // 负数运算测试
  let a = -10
  let b = -5
  
  assert_eq(-15, add(a, b))
  assert_eq(50, multiply(a, b))
  assert_eq(10, multiply(a, -1))
  assert_eq(5, multiply(b, -1))
}

test "zero_properties" {
  // 零属性测试
  let x = 42
  
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  assert_eq(0, multiply(0, 0))
}

test "sequence_operations" {
  // 序列运算测试
  let mut result = 0
  result = add(result, 1)
  result = multiply(result, 2)
  result = add(result, 3)
  result = multiply(result, 4)
  
  assert_eq(32, result)
}

test "power_simulation" {
  // 幂运算模拟测试
  let base = 3
  let exponent = 4
  let mut result = 1
  
  // 3^4 = 81
  let mut i = 0
  while i < exponent {
    result = multiply(result, base)
    i = add(i, 1)
  }
  
  assert_eq(81, result)
}

test "factorial_calculation" {
  // 阶乘计算测试 (5! = 120)
  let n = 5
  let mut result = 1
  
  let mut i = 1
  while i <= n {
    result = multiply(result, i)
    i = add(i, 1)
  }
  
  assert_eq(120, result)
}

test "greeting_edge_cases" {
  // 问候函数边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello,   !", greet("   "))
}