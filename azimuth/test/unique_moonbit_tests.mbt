// 独特的 MoonBit 测试用例
// 覆盖未充分测试的功能和场景

test "fibonacci_sequence_calculation" {
  // 斐波那契数列计算测试
  // 使用 add 函数计算斐波那契数列的前几项
  
  // F(0) = 0, F(1) = 1
  let f0 = 0
  let f1 = 1
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let f2 = @azimuth.add(f1, f0)
  @azimuth.assert_eq(1, f2)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = @azimuth.add(f2, f1)
  @azimuth.assert_eq(2, f3)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = @azimuth.add(f3, f2)
  @azimuth.assert_eq(3, f4)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = @azimuth.add(f4, f3)
  @azimuth.assert_eq(5, f5)
}

test "factorial_calculation" {
  // 阶乘计算测试
  // 使用 multiply 函数计算阶乘
  
  // 0! = 1
  let fact0 = 1
  @azimuth.assert_eq(1, fact0)
  
  // 1! = 1
  let fact1 = 1
  @azimuth.assert_eq(1, fact1)
  
  // 2! = 2 × 1 = 2
  let fact2 = @azimuth.multiply(2, fact1)
  @azimuth.assert_eq(2, fact2)
  
  // 3! = 3 × 2! = 3 × 2 = 6
  let fact3 = @azimuth.multiply(3, fact2)
  @azimuth.assert_eq(6, fact3)
  
  // 4! = 4 × 3! = 4 × 6 = 24
  let fact4 = @azimuth.multiply(4, fact3)
  @azimuth.assert_eq(24, fact4)
  
  // 5! = 5 × 4! = 5 × 24 = 120
  let fact5 = @azimuth.multiply(5, fact4)
  @azimuth.assert_eq(120, fact5)
}

test "compound_interest_calculation" {
  // 复利计算测试
  // 公式：A = P × (1 + r)^n
  // 由于只有基本运算，我们简化为逐年计算
  
  let principal = 10000  // 本金
  let rate = 5  // 利率 5%
  let years = 3  // 年数
  
  // 第一年：10000 + 10000×5% = 10000 + 500 = 10500
  let year1 = @azimuth.add(principal, @azimuth.multiply(principal, rate) / 100)
  @azimuth.assert_eq(10500, year1)
  
  // 第二年：10500 + 10500×5% = 10500 + 525 = 11025
  let year2 = @azimuth.add(year1, @azimuth.multiply(year1, rate) / 100)
  @azimuth.assert_eq(11025, year2)
  
  // 第三年：11025 + 11025×5% = 11025 + 551 = 11576
  let year3 = @azimuth.add(year2, @azimuth.multiply(year2, rate) / 100)
  @azimuth.assert_eq(11576, year3)
}

test "geometric_progression_sum" {
  // 等比数列求和测试
  // 公式：S_n = a × (1 - r^n) / (1 - r)
  // 由于只有基本运算，我们逐项计算
  
  let first_term = 2  // 首项
  let ratio = 3  // 公比
  let terms = 4  // 项数
  
  // 逐项计算：2, 6, 18, 54
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  
  // 求和：2 + 6 + 18 + 54 = 80
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(80, sum)
}

test "string_concatenation_edge_cases" {
  // 字符串拼接边界情况测试
  
  // 长字符串测试
  let long_name = "VeryLongNameThatExceedsNormalLength"
  let long_greeting = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongNameThatExceedsNormalLength!", long_greeting)
  
  // 包含数字和特殊字符的字符串
  let mixed_chars = "User123!@#$%"
  let mixed_greeting = @azimuth.greet(mixed_chars)
  @azimuth.assert_eq_string("Hello, User123!@#$%!", mixed_greeting)
  
  // 包含空格的字符串
  let spaced_name = "  spaced  name  "
  let spaced_greeting = @azimuth.greet(spaced_name)
  @azimuth.assert_eq_string("Hello,   spaced  name  !", spaced_greeting)
}

test "power_calculation" {
  // 幂运算测试（使用重复乘法）
  
  // 2^3 = 2 × 2 × 2 = 8
  let power2_3 = @azimuth.multiply(@azimuth.multiply(2, 2), 2)
  @azimuth.assert_eq(8, power2_3)
  
  // 3^4 = 3 × 3 × 3 × 3 = 81
  let power3_4 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(3, 3), 3), 3)
  @azimuth.assert_eq(81, power3_4)
  
  // 5^2 = 5 × 5 = 25
  let power5_2 = @azimuth.multiply(5, 5)
  @azimuth.assert_eq(25, power5_2)
}

test "binary_conversion_simulation" {
  // 二进制转换模拟测试
  // 将二进制数 1011 转换为十进制
  // 1×2^3 + 0×2^2 + 1×2^1 + 1×2^0 = 8 + 0 + 2 + 1 = 11
  
  let bit3 = 1  // 最高位
  let bit2 = 0
  let bit1 = 1
  let bit0 = 1  // 最低位
  
  // 计算每一位的值
  let value3 = @azimuth.multiply(bit3, @azimuth.multiply(@azimuth.multiply(2, 2), 2))  // 1 × 2^3 = 8
  let value2 = @azimuth.multiply(bit2, @azimuth.multiply(2, 2))  // 0 × 2^2 = 0
  let value1 = @azimuth.multiply(bit1, 2)  // 1 × 2^1 = 2
  let value0 = @azimuth.multiply(bit0, 1)  // 1 × 2^0 = 1
  
  // 求和
  let decimal = @azimuth.add(@azimuth.add(@azimuth.add(value3, value2), value1), value0)
  @azimuth.assert_eq(11, decimal)
}

test "gpa_calculation" {
  // GPA 计算测试
  // 假设 A=4, B=3, C=2, D=1, F=0
  // 计算 3 门课程的 GPA：(4+3+2)/3 = 3
  
  let grade_a = 4
  let grade_b = 3
  let grade_c = 2
  let course_count = 3
  
  // 计算总分
  let total_points = @azimuth.add(@azimuth.add(grade_a, grade_b), grade_c)
  
  // 计算平均分（整数除法）
  let gpa = total_points / course_count
  @azimuth.assert_eq(3, gpa)
}

test "discount_calculation" {
  // 折扣计算测试
  
  let original_price = 1000
  let discount_percent = 20  // 20% 折扣
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 验证：1000 - 1000×20% = 1000 - 200 = 800
  @azimuth.assert_eq(800, discounted_price)
}