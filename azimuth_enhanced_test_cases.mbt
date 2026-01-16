// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "addition_with_large_numbers" {
  // 测试大数加法
  let large_num1 = 1000000
  let large_num2 = 2000000
  let result = azimuth::add(large_num1, large_num2)
  assert_eq(result, 3000000)
}

test "multiplication_table_pattern" {
  // 测试乘法表模式
  let base = 7
  let expected = [7, 14, 21, 28, 35]
  let actual = []
  
  for i in 1..5 {
    actual = azimuth::push(actual, azimuth::multiply(base, i))
  }
  
  for i in 0..length(expected) {
    assert_eq(actual[i], expected[i])
  }
}

test "string_concatenation_through_greet" {
  // 测试通过 greet 函数进行字符串连接
  let names = ["Alice", "Bob", "Charlie"]
  let expected = ["Hello, Alice!", "Hello, Bob!", "Hello, Charlie!"]
  
  for i in 0..length(names) {
    let result = azimuth::greet(names[i])
    assert_eq(result, expected[i])
  }
}

test "nested_arithmetic_operations" {
  // 测试嵌套算术运算
  // 计算: ((5 + 3) * 2) - (10 / 2)
  let step1 = azimuth::add(5, 3)  // 8
  let step2 = azimuth::multiply(step1, 2)  // 16
  let step3 = 10 / 2  // 5
  let result = azimuth::add(step2, -step3)  // 11
  
  assert_eq(result, 11)
}

test "alternating_addition_subtraction" {
  // 测试交替加减法
  let start = 100
  let operations = [10, -5, 20, -15, 30]
  let current = start
  
  for op in operations {
    current = azimuth::add(current, op)
  }
  
  assert_eq(current, 140)
}

test "mathematical_series_summation" {
  // 测试数学级数求和
  // 计算 1 + 2 + 3 + ... + 10
  let sum = 0
  for i in 1..11 {
    sum = azimuth::add(sum, i)
  }
  assert_eq(sum, 55)
}

test "exponential_growth_simulation" {
  // 测试指数增长模拟
  // 模拟细菌增长：每小时翻倍，共3小时
  let initial_count = 100
  let hours = 3
  let current = initial_count
  
  for i in 1..(hours + 1) {
    current = azimuth::multiply(current, 2)
  }
  
  assert_eq(current, 800)
}

test "string_greeting_with_numbers" {
  // 测试包含数字的字符串问候
  let numeric_inputs = ["123", "user456", "test_789"]
  let expected = ["Hello, 123!", "Hello, user456!", "Hello, test_789!"]
  
  for i in 0..length(numeric_inputs) {
    let result = azimuth::greet(numeric_inputs[i])
    assert_eq(result, expected[i])
  }
}

test "factorial_calculation_simulation" {
  // 测试阶乘计算模拟
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let n = 5
  let factorial = 1
  
  for i in 1..(n + 1) {
    factorial = azimuth::multiply(factorial, i)
  }
  
  assert_eq(factorial, 120)
}

test "compound_interest_calculation" {
  // 测试复利计算
  // 本金1000，年利率5%，3年复利
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  let current = principal
  
  for i in 1..(years + 1) {
    let interest = azimuth::multiply(current, rate) / 100
    current = azimuth::add(current, interest)
  }
  
  assert_eq(current, 1157)
}