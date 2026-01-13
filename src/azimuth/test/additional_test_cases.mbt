// 额外的 MoonBit 测试用例
// 这些测试用例展示了不同的测试场景和技术

test "number_sequence_operations" {
  // 测试数字序列操作：计算等差数列的和
  // 1 + 2 + 3 + ... + n = n * (n + 1) / 2
  let n = 10
  let expected_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  
  // 手动计算前10个自然数的和
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  
  @azimuth.assert_eq(expected_sum, manual_sum)
  @azimuth.assert_eq(55, manual_sum)  // 1+2+3+4+5+6+7+8+9+10 = 55
}

test "geometric_progression_test" {
  // 测试等比数列：a, ar, ar², ar³, ...
  let a = 2  // 首项
  let r = 3  // 公比
  
  // 计算前5项的和：a + ar + ar² + ar³ + ar⁴
  let term1 = a
  let term2 = @azimuth.multiply(a, r)
  let term3 = @azimuth.multiply(term2, r)
  let term4 = @azimuth.multiply(term3, r)
  let term5 = @azimuth.multiply(term4, r)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  
  // 2 + 6 + 18 + 54 + 162 = 242
  @azimuth.assert_eq(242, sum)
}

test "string_template_processing" {
  // 测试字符串模板处理
  let name = "Alice"
  let age = "30"
  let city = "Beijing"
  
  // 使用greet函数模拟模板处理
  let greeting1 = @azimuth.greet(name)
  let greeting2 = @azimuth.greet(name + " from " + city)
  let greeting3 = @azimuth.greet(name + " (age " + age + ")")
  
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Alice from Beijing!", greeting2)
  @azimuth.assert_eq_string("Hello, Alice (age 30)!", greeting3)
}

test "discount_calculation_scenario" {
  // 测试折扣计算场景
  let original_price = 1000
  let discount_percent = 20  // 20% 折扣
  
  // 计算折扣金额：原价 × 折扣百分比 ÷ 100
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  
  // 计算最终价格：原价 - 折扣金额
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(200, discount_amount)  // 1000 × 20 ÷ 100 = 200
  @azimuth.assert_eq(800, final_price)      // 1000 - 200 = 800
}

test "compound_interest_calculation" {
  // 测试复利计算（简化版）
  let principal = 10000  // 本金
  let rate = 5          // 利率 5%
  let years = 3         // 年数
  
  // 简化复利计算：本息和 = 本金 + 本金 × 利率 × 年数
  let interest_per_year = @azimuth.multiply(principal, rate) / 100
  let total_interest = @azimuth.multiply(interest_per_year, years)
  let total_amount = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(500, interest_per_year)    // 10000 × 5 ÷ 100 = 500
  @azimuth.assert_eq(1500, total_interest)     // 500 × 3 = 1500
  @azimuth.assert_eq(11500, total_amount)      // 10000 + 1500 = 11500
}

test "array_index_calculation" {
  // 测试数组索引计算（模拟二维数组到一维数组的索引转换）
  let rows = 3
  let cols = 4
  let row_index = 1  // 第二行（从0开始）
  let col_index = 2  // 第三列（从0开始）
  
  // 二维数组到一维数组的索引转换：index = row * cols + col
  let linear_index = @azimuth.add(@azimuth.multiply(row_index, cols), col_index)
  
  @azimuth.assert_eq(6, linear_index)  // 1 * 4 + 2 = 6
  
  // 测试边界情况
  let first_element = @azimuth.add(@azimuth.multiply(0, cols), 0)
  let last_element = @azimuth.add(@azimuth.multiply(@azimuth.add(rows, -1), @azimuth.add(cols, -1)), @azimuth.add(cols, -1))
  
  @azimuth.assert_eq(0, first_element)   // 第一行第一列
  @azimuth.assert_eq(11, last_element)   // 最后一行最后一列：(3-1)*4 + (4-1) = 2*4 + 3 = 11
}

test "time_conversion_calculations" {
  // 测试时间转换计算
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 将时间转换为总秒数
  let total_seconds = @azimuth.add(@azimuth.add(
    @azimuth.multiply(hours, 3600),    // 小时转秒
    @azimuth.multiply(minutes, 60)     // 分钟转秒
  ), seconds)
  
  @azimuth.assert_eq(9045, total_seconds)  // 2*3600 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
  
  // 测试反向计算：从总秒数计算小时和分钟
  let total_seconds_rounded = 9000  // 简化为可被整除的值
  let calculated_hours = total_seconds_rounded / 3600
  let remaining_seconds = total_seconds_rounded % 3600
  let calculated_minutes = remaining_seconds / 60
  
  @azimuth.assert_eq(2, calculated_hours)     // 9000 ÷ 3600 = 2
  @azimuth.assert_eq(30, calculated_minutes)  // (9000 % 3600) ÷ 60 = 1800 ÷ 60 = 30
}

test "temperature_conversion_precise" {
  // 测试更精确的温度转换（使用整数运算近似）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数运算：F = C × 2 - C/5 + 32 （近似公式）
  
  let celsius = 25
  
  // 使用近似公式计算
  let term1 = @azimuth.multiply(celsius, 2)           // C × 2
  let term2 = celsius / 5                             // C ÷ 5（整数除法）
  let fahrenheit = @azimuth.add(@azimuth.add(term1, -term2), 32)
  
  @azimuth.assert_eq(77, fahrenheit)  // 25 × 2 - 25/5 + 32 = 50 - 5 + 32 = 77
  
  // 测试其他温度点
  let freezing = 0
  let freezing_f = @azimuth.add(@azimuth.add(@azimuth.multiply(freezing, 2), -(freezing / 5)), 32)
  @azimuth.assert_eq(32, freezing_f)  // 0 × 2 - 0/5 + 32 = 32
  
  let boiling = 100
  let boiling_f = @azimuth.add(@azimuth.add(@azimuth.multiply(boiling, 2), -(boiling / 5)), 32)
  @azimuth.assert_eq(212, boiling_f)  // 100 × 2 - 100/5 + 32 = 200 - 20 + 32 = 212
}

test "perimeter_and_area_calculations" {
  // 测试周长和面积计算
  let length = 8
  let width = 5
  
  // 矩形周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  // 矩形面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  
  @azimuth.assert_eq(26, perimeter)  // 2 × (8 + 5) = 2 × 13 = 26
  @azimuth.assert_eq(40, area)       // 8 × 5 = 40
  
  // 计算相同面积的正方形边长（简化，只考虑完全平方数）
  let square_area = 36
  let square_side = 6  // 已知36是完全平方数
  
  // 验证正方形面积
  let calculated_square_area = @azimuth.multiply(square_side, square_side)
  @azimuth.assert_eq(square_area, calculated_square_area)
  
  // 计算正方形周长
  let square_perimeter = @azimuth.multiply(4, square_side)
  @azimuth.assert_eq(24, square_perimeter)  // 4 × 6 = 24
}

test "string_length_processing" {
  // 测试字符串长度处理（通过字符数估算）
  let short_name = "Bob"
  let medium_name = "Elizabeth"
  let long_name = "Alexander the Great"
  
  // 使用greet函数处理不同长度的字符串
  let short_greeting = @azimuth.greet(short_name)
  let medium_greeting = @azimuth.greet(medium_name)
  let long_greeting = @azimuth.greet(long_name)
  
  @azimuth.assert_eq_string("Hello, Bob!", short_greeting)
  @azimuth.assert_eq_string("Hello, Elizabeth!", medium_greeting)
  @azimuth.assert_eq_string("Hello, Alexander the Great!", long_greeting)
  
  // 测试包含数字和特殊字符的字符串
  let mixed_name = "User123@Example"
  let mixed_greeting = @azimuth.greet(mixed_name)
  @azimuth.assert_eq_string("Hello, User123@Example!", mixed_greeting)
  
  // 测试多语言字符串
  let chinese_name = "李明"
  let chinese_greeting = @azimuth.greet(chinese_name)
  @azimuth.assert_eq_string("Hello, 李明!", chinese_greeting)
  
  let japanese_name = "田中太郎"
  let japanese_greeting = @azimuth.greet(japanese_name)
  @azimuth.assert_eq_string("Hello, 田中太郎!", japanese_greeting)
}