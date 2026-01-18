// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 加法测试
  let sum_result = azimuth.add(a, b)
  @assertion.assert_eq(22, sum_result)?
  
  // 乘法测试
  let product_result = azimuth.multiply(a, b)
  @assertion.assert_eq(105, product_result)?
  
  // 复合运算测试
  let complex_result = azimuth.add(azimuth.multiply(a, 2), azimuth.multiply(b, 3))
  @assertion.assert_eq(51, complex_result)?
}

test "greeting_functionality" {
  // 问候功能测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "Charlie"
  
  // 测试基本问候
  let greeting1 = azimuth.greet(name1)
  @assertion.assert_eq_string("Hello, Alice!", greeting1)?
  
  // 测试空名字
  let empty_greeting = azimuth.greet("")
  @assertion.assert_eq_string("Hello, !", empty_greeting)?
  
  // 测试带空格的名字
  let spaced_name = "John Doe"
  let spaced_greeting = azimuth.greet(spaced_name)
  @assertion.assert_eq_string("Hello, John Doe!", spaced_greeting)?
}

test "edge_case_numbers" {
  // 边界数值测试
  let zero = 0
  let positive = 42
  let negative = -17
  
  // 零值测试
  let zero_add = azimuth.add(zero, positive)
  @assertion.assert_eq(42, zero_add)?
  
  let zero_mul = azimuth.multiply(zero, positive)
  @assertion.assert_eq(0, zero_mul)?
  
  // 负数测试
  let neg_add = azimuth.add(negative, positive)
  @assertion.assert_eq(25, neg_add)?
  
  let neg_mul = azimuth.multiply(negative, positive)
  @assertion.assert_eq(-714, neg_mul)?
}

test "mathematical_series" {
  // 数学级数测试
  let n = 5
  
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let arithmetic_sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  @assertion.assert_eq(15, arithmetic_sum)?
  
  // 等比数列求和：2 + 4 + 8 + 16 + 32 = 62
  let a = 2
  let r = 2
  let geometric_sum = azimuth.multiply(a, azimuth.subtract(azimuth.multiply(r, n), 1)) / azimuth.subtract(r, 1)
  @assertion.assert_eq(62, geometric_sum)?
}

test "business_calculations" {
  // 商业计算测试
  let unit_price = 25
  let quantity = 8
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税率
  
  // 计算小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  @assertion.assert_eq(200, subtotal)?
  
  // 计算折扣
  let discount = azimuth.multiply(subtotal, discount_rate) / 100
  @assertion.assert_eq(20, discount)?
  
  // 计算折后价格
  let discounted_price = azimuth.add(subtotal, -discount)
  @assertion.assert_eq(180, discounted_price)?
  
  // 计算税费
  let tax = azimuth.multiply(discounted_price, tax_rate) / 100
  @assertion.assert_eq(14, tax)?
  
  // 计算最终价格
  let final_price = azimuth.add(discounted_price, tax)
  @assertion.assert_eq(194, final_price)?
}

test "unit_conversions" {
  // 单位转换测试
  let meters = 100
  
  // 米转厘米：1米 = 100厘米
  let centimeters = azimuth.multiply(meters, 100)
  @assertion.assert_eq(10000, centimeters)?
  
  // 米转千米：1千米 = 1000米
  let kilometers = meters / 1000
  @assertion.assert_eq(0, kilometers)?  // 整数除法
  
  // 厘米转米
  let meters_from_cm = centimeters / 100
  @assertion.assert_eq(100, meters_from_cm)?
}

test "age_calculations" {
  // 年龄计算测试
  let birth_year = 1990
  let current_year = 2023
  
  // 计算年龄
  let age = azimuth.add(current_year, -birth_year)
  @assertion.assert_eq(33, age)?
  
  // 计算退休年龄（假设65岁退休）
  let retirement_age = 65
  let years_to_retirement = azimuth.add(retirement_age, -age)
  @assertion.assert_eq(32, years_to_retirement)?
  
  // 计算工作年限（假设22岁开始工作）
  let work_start_age = 22
  let work_years = azimuth.add(age, -work_start_age)
  @assertion.assert_eq(11, work_years)?
}

test "array_index_calculations" {
  // 数组索引计算测试
  let array_size = 10
  let start_index = 0
  let end_index = azimuth.add(array_size, -1)
  
  // 验证起始和结束索引
  @assertion.assert_eq(0, start_index)?
  @assertion.assert_eq(9, end_index)?
  
  // 计算中间索引
  let middle_index = array_size / 2
  @assertion.assert_eq(5, middle_index)?
  
  // 计算数组长度
  let length = azimuth.add(end_index, 1)
  @assertion.assert_eq(10, length)?
}

test "score_calculations" {
  // 分数计算测试
  let exam_score = 85
  let homework_score = 90
  let project_score = 78
  let quiz_score = 92
  
  // 计算平均分（权重：考试40%，作业30%，项目20%，测验10%）
  let weighted_exam = azimuth.multiply(exam_score, 40) / 100
  let weighted_homework = azimuth.multiply(homework_score, 30) / 100
  let weighted_project = azimuth.multiply(project_score, 20) / 100
  let weighted_quiz = azimuth.multiply(quiz_score, 10) / 100
  
  let final_score = azimuth.add(azimuth.add(
    azimuth.add(weighted_exam, weighted_homework), 
    weighted_project), weighted_quiz)
  
  @assertion.assert_eq(85, final_score)?
}

test "time_zone_calculations" {
  // 时区计算测试
  let local_time = 14  // 下午2点
  let time_difference = 8  // 与UTC时差8小时
  
  // 计算UTC时间
  let utc_time = azimuth.add(local_time, -time_difference)
  @assertion.assert_eq(6, utc_time)?  // 早上6点
  
  // 计算另一个时区的时间（比本地时间晚5小时）
  let other_timezone = azimuth.add(local_time, -5)
  @assertion.assert_eq(9, other_timezone)?  // 早上9点
  
  // 计算跨越日期的时间
  let late_night = 23  // 晚上11点
  let early_morning = azimuth.add(late_night, 3)  // 3小时后
  @assertion.assert_eq(26, early_morning)?  // 第二天凌晨2点
}