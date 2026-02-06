// 额外的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 测试基本加法运算
  assert_eq(2 + 3, 5)
  assert_eq(0 + 0, 0)
  assert_eq(-5 + 10, 5)
  assert_eq(100 + 200, 300)
}

test "basic_arithmetic_multiplication" {
  // 测试基本乘法运算
  assert_eq(3 * 4, 12)
  assert_eq(0 * 5, 0)
  assert_eq(-2 * 3, -6)
  assert_eq(10 * 10, 100)
}

test "basic_arithmetic_subtraction" {
  // 测试基本减法运算
  assert_eq(10 - 5, 5)
  assert_eq(5 - 10, -5)
  assert_eq(0 - 0, 0)
  assert_eq(100 - 50, 50)
}

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本情况
  assert_eq(divide_with_ceil(10, 3), 4)
  assert_eq(divide_with_ceil(9, 3), 3)
  assert_eq(divide_with_ceil(1, 2), 1)
  assert_eq(divide_with_ceil(0, 5), 0)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(divide_with_ceil(10, 0), 0)  // 除数为零的情况
  assert_eq(divide_with_ceil(-10, 3), -3)
  assert_eq(divide_with_ceil(10, -3), -3)
  assert_eq(divide_with_ceil(-10, -3), 4)
}

test "greet_function_basic" {
  // 测试问候函数
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
  assert_eq_string(greet(""), "Hello, !")
  assert_eq_string(greet("123"), "Hello, 123!")
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let base = 10
  let multiplier = 3
  let increment = 5
  let divisor = 4
  
  // 复合运算：(base * multiplier + increment) / divisor 向上取整
  let step1 = multiply(base, multiplier)
  let step2 = add(step1, increment)
  let result = divide_with_ceil(step2, divisor)
  
  assert_eq(step1, 30)
  assert_eq(step2, 35)
  assert_eq(result, 9)
}

test "string_processing_chain" {
  // 测试字符串处理链
  let name1 = "Alice"
  let name2 = "Bob"
  
  let greeting1 = greet(name1)
  let greeting2 = greet(name2)
  
  // 验证两个问候字符串的长度
  assert_eq(greeting1.length(), 12)
  assert_eq(greeting2.length(), 11)
  
  // 验证问候内容
  assert_eq_string(greeting1, "Hello, Alice!")
  assert_eq_string(greeting2, "Hello, Bob!")
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列 1, 3, 5, 7, 9 的和
  let term1 = 1
  let difference = 2
  let num_terms = 5
  
  let term2 = add(term1, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  let term5 = add(term4, difference)
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  let average = divide_with_ceil(sum, num_terms)
  
  assert_eq(term2, 3)
  assert_eq(term3, 5)
  assert_eq(term4, 7)
  assert_eq(term5, 9)
  assert_eq(sum, 25)
  assert_eq(average, 5)
}