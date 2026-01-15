// 全面的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖 azimuth 模块的核心功能

// 导入主模块中的函数
use azimuth

test "basic_arithmetic_operations" {
  // 测试基本的算术运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(3, -4))
  assert_eq(0, multiply(8, 0))
}

test "mathematical_series_summation" {
  // 测试数学级数求和
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum1)
  
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let sum2 = add(add(add(2, 4), 8), 16)
  assert_eq(30, sum2)
  
  // 平方数列求和：1² + 2² + 3² + 4² = 1 + 4 + 9 + 16 = 30
  let sum3 = add(add(add(1, 4), 9), 16)
  assert_eq(30, sum3)
}

test "string_greeting_variations" {
  // 测试各种字符串问候情况
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob123!", greet("Bob123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, Space Test!", greet("Space Test"))
}

test "geometric_calculations" {
  // 测试几何计算
  let length = 10
  let width = 6
  
  // 计算矩形面积
  let area = multiply(length, width)
  assert_eq(60, area)
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(32, perimeter)
  
  // 计算正方形面积（边长为8）
  let side = 8
  let square_area = multiply(side, side)
  assert_eq(64, square_area)
}

test "time_calculations" {
  // 测试时间计算
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为总秒数：2×3600 + 30×60 + 45 = 7200 + 1800 + 45 = 9045
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  assert_eq(9045, total_seconds)
  
  // 计算天数：9045 ÷ 86400 = 0（整数除法）
  let days = total_seconds / 86400
  assert_eq(0, days)
}

test "financial_calculations" {
  // 测试金融计算
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简单利息计算：本金 × 利率 × 年数
  let simple_interest = multiply(multiply(principal, rate), years) / 100
  assert_eq(150, simple_interest)
  
  // 总金额：本金 + 利息
  let total_amount = add(principal, simple_interest)
  assert_eq(1150, total_amount)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  let celsius = 20
  
  // 摄氏度转华氏度（近似）：C × 2 - C/10 + 32
  let fahrenheit = add(add(multiply(celsius, 2), multiply(celsius, -1) / 10), 32)
  assert_eq(70, fahrenheit)  // 20 × 2 - 20/10 + 32 = 40 - 2 + 32 = 70
  
  // 华氏度转摄氏度（近似）：(F - 32) ÷ 2
  let fahrenheit_back = 70
  let celsius_back = (fahrenheit_back - 32) / 2
  assert_eq(19, celsius_back)
}

test "game_score_calculations" {
  // 测试游戏分数计算
  let base_score = 100
  let multiplier = 2
  let bonus = 50
  let penalty = 20
  
  // 计算最终分数：基础分数 × 倍数 + 奖励 - 惩罚
  let final_score = add(add(multiply(base_score, multiplier), bonus), -penalty)
  assert_eq(230, final_score)
  
  // 计算等级分数（每100分一级）
  let level = final_score / 100
  assert_eq(2, level)
}

test "boundary_value_testing" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试与0的运算
  assert_eq(max_int, add(max_int, 0))
  assert_eq(min_int, add(min_int, 0))
  assert_eq(0, multiply(max_int, 0))
  assert_eq(0, multiply(min_int, 0))
  
  // 测试与1的运算
  assert_eq(max_int, multiply(max_int, 1))
  assert_eq(min_int, multiply(min_int, 1))
  assert_eq(add(max_int, 1), max_int)  // 溢出保护
  assert_eq(add(min_int, -1), min_int)  // 溢出保护
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值
  let a = 5
  let b = 3
  let c = 2
  
  // 表达式：(a + b) × c - a × b
  let result1 = add(multiply(add(a, b), c), -multiply(a, b))
  assert_eq(16, result1)  // (5 + 3) × 2 - 5 × 3 = 8 × 2 - 15 = 16 - 15 = 1
  
  // 表达式：a × b + b × c + c × a
  let result2 = add(add(multiply(a, b), multiply(b, c)), multiply(c, a))
  assert_eq(31, result2)  // 5 × 3 + 3 × 2 + 2 × 5 = 15 + 6 + 10 = 31
  
  // 表达式：(a + b + c) × (a + b + c)
  let sum = add(add(a, b), c)
  let result3 = multiply(sum, sum)
  assert_eq(100, result3)  // (5 + 3 + 2) × (5 + 3 + 2) = 10 × 10 = 100
}