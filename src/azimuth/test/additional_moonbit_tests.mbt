// 额外的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法测试 azimuth 模块功能

test "integer_division_and_remainder" {
  // 测试整数除法和取余运算
  let dividend = 17
  let divisor = 5
  
  // 计算商和余数
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  // 验证除法恒等式：dividend = divisor * quotient + remainder
  let reconstructed = @azimuth.add(@azimuth.multiply(divisor, quotient), remainder)
  @azimuth.assert_eq(dividend, reconstructed)
  
  // 测试边界情况
  @azimuth.assert_eq(0, 0 / 5)
  @azimuth.assert_eq(0, 0 % 5)
  @azimuth.assert_eq(1, 5 / 3)
  @azimuth.assert_eq(2, 5 % 3)
}

test "absolute_value_simulation" {
  // 模拟绝对值计算
  let positive = 42
  let negative = -42
  let zero = 0
  
  // 使用条件表达式模拟绝对值
  let abs_positive = if positive >= 0 { positive } else { -positive }
  let abs_negative = if negative >= 0 { negative } else { -negative }
  let abs_zero = if zero >= 0 { zero } else { -zero }
  
  @azimuth.assert_eq(42, abs_positive)
  @azimuth.assert_eq(42, abs_negative)
  @azimuth.assert_eq(0, abs_zero)
}

test "sign_function_simulation" {
  // 模拟符号函数：返回-1（负数）、0（零）或1（正数）
  let positive = 10
  let negative = -10
  let zero = 0
  
  // 使用条件表达式模拟符号函数
  let sign_positive = if positive > 0 { 1 } else { if positive < 0 { -1 } else { 0 } }
  let sign_negative = if negative > 0 { 1 } else { if negative < 0 { -1 } else { 0 } }
  let sign_zero = if zero > 0 { 1 } else { if zero < 0 { -1 } else { 0 } }
  
  @azimuth.assert_eq(1, sign_positive)
  @azimuth.assert_eq(-1, sign_negative)
  @azimuth.assert_eq(0, sign_zero)
}

test "max_function_simulation" {
  // 模拟最大值函数
  let a = 15
  let b = 25
  let c = 20
  
  // 使用条件表达式模拟最大值函数
  let max_ab = if a > b { a } else { b }
  let max_abc = if max_ab > c { max_ab } else { c }
  
  @azimuth.assert_eq(25, max_ab)
  @azimuth.assert_eq(25, max_abc)
  
  // 测试相等的情况
  let equal_a = 10
  let equal_b = 10
  let max_equal = if equal_a > equal_b { equal_a } else { equal_b }
  @azimuth.assert_eq(10, max_equal)
}

test "min_function_simulation" {
  // 模拟最小值函数
  let a = 15
  let b = 25
  let c = 10
  
  // 使用条件表达式模拟最小值函数
  let min_ab = if a < b { a } else { b }
  let min_abc = if min_ab < c { min_ab } else { c }
  
  @azimuth.assert_eq(15, min_ab)
  @azimuth.assert_eq(10, min_abc)
  
  // 测试负数的情况
  let neg_a = -5
  let neg_b = -10
  let min_neg = if neg_a < neg_b { neg_a } else { neg_b }
  @azimuth.assert_eq(-10, min_neg)
}

test "string_length_estimation" {
  // 通过字符串操作估算字符串长度
  let short = "Hi"
  let medium = "MoonBit"
  let long = "This is a very long string for testing purposes"
  
  // 使用 greet 函数来验证字符串处理
  let short_greeting = @azimuth.greet(short)
  let medium_greeting = @azimuth.greet(medium)
  let long_greeting = @azimuth.greet(long)
  
  // 验证字符串拼接的正确性
  @azimuth.assert_eq_string("Hello, Hi!", short_greeting)
  @azimuth.assert_eq_string("Hello, MoonBit!", medium_greeting)
  @azimuth.assert_eq_string("Hello, This is a very long string for testing purposes!", long_greeting)
  
  // 测试空字符串
  let empty = ""
  let empty_greeting = @azimuth.greet(empty)
  @azimuth.assert_eq_string("Hello, !", empty_greeting)
}

test "boolean_logic_evaluation" {
  // 测试布尔逻辑运算的模拟
  let true_val = 1
  let false_val = 0
  
  // 使用数值模拟布尔运算：AND = min(a, b), OR = max(a, b)
  let and_true = min_function_simulation(true_val, true_val)
  let and_false1 = min_function_simulation(true_val, false_val)
  let and_false2 = min_function_simulation(false_val, true_val)
  let and_false3 = min_function_simulation(false_val, false_val)
  
  let or_true1 = max_function_simulation(true_val, true_val)
  let or_true2 = max_function_simulation(true_val, false_val)
  let or_true3 = max_function_simulation(false_val, true_val)
  let or_false = max_function_simulation(false_val, false_val)
  
  @azimuth.assert_eq(1, and_true)
  @azimuth.assert_eq(0, and_false1)
  @azimuth.assert_eq(0, and_false2)
  @azimuth.assert_eq(0, and_false3)
  
  @azimuth.assert_eq(1, or_true1)
  @azimuth.assert_eq(1, or_true2)
  @azimuth.assert_eq(1, or_true3)
  @azimuth.assert_eq(0, or_false)
}

test "arithmetic_progression_sum" {
  // 计算等差数列的和：n × (首项 + 末项) / 2
  let first_term = 3
  let last_term = 15
  let n = 7  // 项数
  
  // 计算等差数列的和
  let sum = @azimuth.multiply(n, @azimuth.add(first_term, last_term)) / 2
  
  // 手动验证：3 + 5 + 7 + 9 + 11 + 13 + 15 = 63
  @azimuth.assert_eq(63, sum)
  
  // 测试另一个等差数列
  let first_term2 = 1
  let last_term2 = 10
  let n2 = 10
  
  let sum2 = @azimuth.multiply(n2, @azimuth.add(first_term2, last_term2)) / 2
  
  // 手动验证：1 + 2 + 3 + ... + 10 = 55
  @azimuth.assert_eq(55, sum2)
}

test "geometric_progression_product" {
  // 计算等比数列的积（简化版）
  let first_term = 2
  let ratio = 3
  let n = 4  // 项数
  
  // 计算等比数列的积：2 × 6 × 18 × 54
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  
  let product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(term1, term2), term3), term4)
  
  // 手动验证：2 × 6 × 18 × 54 = 11664
  @azimuth.assert_eq(11664, product)
  
  // 测试比率为1的特殊情况
  let ratio_one = 1
  let n2 = 5
  
  // 所有项都是相同的值
  let product_with_ratio_one = @azimuth.multiply(first_term, @azimuth.multiply(first_term, @azimuth.multiply(first_term, @azimuth.multiply(first_term, first_term))))
  
  // 2 × 2 × 2 × 2 × 2 = 32
  @azimuth.assert_eq(32, product_with_ratio_one)
}

// 辅助函数：最小值函数
fn min_function_simulation(a : Int, b : Int) -> Int {
  if a < b { a } else { b }
}

// 辅助函数：最大值函数
fn max_function_simulation(a : Int, b : Int) -> Int {
  if a > b { a } else { b }
}