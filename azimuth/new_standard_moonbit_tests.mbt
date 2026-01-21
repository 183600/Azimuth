// 新增 MoonBit 标准测试用例
// 专注于边缘情况和实际应用场景

test "add_function_stress_test" {
  // 加法函数压力测试：测试大量连续加法运算
  let mut sum = 0
  for i = 1; i <= 100; i = i + 1 {
    sum = add(sum, i)
  }
  assert_eq(5050, sum)  // 1+2+...+100 = 5050
}

test "multiply_function_geometric_progression" {
  // 乘法函数几何级数测试
  let mut result = 1
  for i = 1; i <= 10; i = i + 1 {
    result = multiply(result, 2)
  }
  assert_eq(1024, result)  // 2^10 = 1024
}

test "divide_with_ceil_fibonacci_sequence" {
  // 使用斐波那契数列测试向上取整除法
  let fib1 = 34
  let fib2 = 55
  let fib3 = 89
  
  // 测试斐波那契数列的除法关系
  assert_eq(2, divide_with_ceil(fib2, fib1))
  assert_eq(2, divide_with_ceil(fib3, fib2))
  assert_eq(3, divide_with_ceil(fib3, fib1))
}

test "greet_function_multilingual_support" {
  // 多语言支持测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, русский!", greet("русский"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景：计算复利
  let principal = 10000  // 本金
  let rate = 7  // 利率7%
  let years = 3  // 3年
  
  // 简化的复利计算：principal * rate^years
  let mut amount = principal
  for i = 1; i <= years; i = i + 1 {
    amount = multiply(amount, rate)
  }
  
  // 计算平均每年收益（向上取整）
  let average_annual_return = divide_with_ceil(amount, years)
  assert_eq(2286667, average_annual_return)
}

test "scientific_notation_simulation" {
  // 科学计数法模拟测试
  let base = 2
  let exponent = 10
  
  // 计算 2^10
  let mut result = 1
  for i = 1; i <= exponent; i = i + 1 {
    result = multiply(result, base)
  }
  assert_eq(1024, result)
  
  // 测试大数运算
  let large_number = multiply(12345, 6789)
  assert_eq(83810205, large_number)
}

test "temperature_conversion_simulation" {
  // 温度转换模拟测试（简化的整数运算）
  let celsius = 25
  let fahrenheit = add(multiply(divide_with_ceil(multiply(celsius, 9), 5), 1), 32)
  assert_eq(77, fahrenheit)
  
  // 反向转换（简化）
  let back_to_celsius = divide_with_ceil(multiply(add(fahrenheit, -32), 5), 9)
  assert_eq(25, back_to_celsius)
}

test "array_indexing_simulation" {
  // 数组索引模拟测试：计算二维数组的一维索引
  let cols = 15
  let row = 3
  let col = 7
  
  // 计算一维索引：row * cols + col
  let index = add(multiply(row, cols), col)
  assert_eq(52, index)  // 3 * 15 + 7 = 52
}

test "game_score_calculation" {
  // 游戏分数计算测试
  let base_score = 100
  let multiplier = 5
  let bonus = 50
  
  // 计算最终分数：base_score * multiplier + bonus
  let final_score = add(multiply(base_score, multiplier), bonus)
  assert_eq(550, final_score)
  
  // 计算平均每关得分（向上取整）
  let levels = 8
  let average_per_level = divide_with_ceil(final_score, levels)
  assert_eq(69, average_per_level)
}

test "error_boundary_comprehensive_test" {
  // 错误边界综合测试
  let max_val = 2147483647
  
  // 测试溢出后的函数仍然可用
  let _ = add(max_val, max_val)  // 这会溢出
  let _ = multiply(max_val, 2)   // 这会溢出
  
  // 验证函数仍然正常工作
  assert_eq(10, add(5, 5))
  assert_eq(25, multiply(5, 5))
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq_string("Hello, Test!", greet("Test"))
}