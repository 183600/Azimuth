// Azimuth 项目实用测试用例
// 使用标准 MoonBit 测试语法，专注于实际应用场景

test "array_sum_calculation" {
  // 数组求和测试
  let numbers = [1, 2, 3, 4, 5]
  let sum = 0
  let i = 0
  
  // 手动计算数组元素之和
  sum = @azimuth.add(sum, numbers[0])  // sum = 1
  sum = @azimuth.add(sum, numbers[1])  // sum = 3
  sum = @azimuth.add(sum, numbers[2])  // sum = 6
  sum = @azimuth.add(sum, numbers[3])  // sum = 10
  sum = @azimuth.add(sum, numbers[4])  // sum = 15
  
  @azimuth.assert_eq(15, sum)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let part = 50
  
  // 计算百分比: (part / total) * 100
  let percentage = @azimuth.multiply(@azimuth.divide(part, total), 100)
  @azimuth.assert_eq(25, percentage)
  
  // 计算折扣后的价格
  let discount_rate = 15  // 15% 折扣
  let discount_amount = @azimuth.multiply(total, discount_rate) / 100
  let discounted_price = @azimuth.add(total, -discount_amount)
  @azimuth.assert_eq(170, discounted_price)
}

test "speed_distance_time" {
  // 速度、距离、时间关系测试
  let distance = 300  // 公里
  let speed = 60      // 公里/小时
  
  // 计算时间: 距离 / 速度
  let time = @azimuth.divide(distance, speed)
  @azimuth.assert_eq(5, time)
  
  // 计算平均速度
  let distance2 = 150
  let time2 = 3
  let avg_speed = @azimuth.divide(distance2, time2)
  @azimuth.assert_eq(50, avg_speed)
}

test "compound_interest" {
  // 复利计算测试
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算 (逐年计算)
  let year1 = @azimuth.add(principal, @azimuth.multiply(principal, rate) / 100)
  let year2 = @azimuth.add(year1, @azimuth.multiply(year1, rate) / 100)
  let year3 = @azimuth.add(year2, @azimuth.multiply(year2, rate) / 100)
  
  // 验证第三年金额大于本金
  @azimuth.assert_eq(true, year3 > principal)
}

test "unit_conversions" {
  // 单位转换测试
  
  // 公斤到克: 1kg = 1000g
  let kg = 2.5
  let grams = @azimuth.multiply(kg, 1000)
  @azimuth.assert_eq(2500, grams)
  
  // 米到厘米: 1m = 100cm
  let meters = 3.7
  let cm = @azimuth.multiply(meters, 100)
  @azimuth.assert_eq(370, cm)
  
  // 升到毫升: 1L = 1000mL
  let liters = 1.5
  let ml = @azimuth.multiply(liters, 1000)
  @azimuth.assert_eq(1500, ml)
}

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 100
  let sales = 30
  let purchases = 50
  let returns = 5
  
  // 计算期末库存: 期初 - 销售 + 采购 + 退货
  let final_stock = @azimuth.add(@azimuth.add(@azimuth.add(initial_stock, -sales), purchases), returns)
  @azimuth.assert_eq(125, final_stock)
  
  // 计算库存周转率 (简化版)
  let avg_stock = @azimuth.divide(@azimuth.add(initial_stock, final_stock), 2)
  let turnover = @azimuth.divide(sales, avg_stock)
  @azimuth.assert_eq(true, turnover > 0)
}

test "grade_calculations" {
  // 成绩计算测试
  let exam_score = 85
  let homework_score = 90
  let project_score = 88
  
  // 权重: 考试50%, 作业30%, 项目20%
  let final_grade = @azimuth.add(@azimuth.add(
    @azimuth.multiply(exam_score, 50) / 100,
    @azimuth.multiply(homework_score, 30) / 100),
    @azimuth.multiply(project_score, 20) / 100)
  
  @azimuth.assert_eq(true, final_grade >= 80)  // 优良成绩
}

test "phone_number_validation" {
  // 电话号码验证测试 (简化版)
  let phone1 = "1234567890"
  let phone2 = "0987654321"
  let phone3 = "12345"  // 太短
  let phone4 = "123456789012345"  // 太长
  
  // 检查长度是否在合理范围内 (10-12位)
  let len1 = @azimuth.string_length(phone1)
  let len2 = @azimuth.string_length(phone2)
  let len3 = @azimuth.string_length(phone3)
  let len4 = @azimuth.string_length(phone4)
  
  @azimuth.assert_eq(true, len1 >= 10 && len1 <= 12)
  @azimuth.assert_eq(true, len2 >= 10 && len2 <= 12)
  @azimuth.assert_eq(false, len3 >= 10 && len3 <= 12)
  @azimuth.assert_eq(false, len4 >= 10 && len4 <= 12)
}

test "bmi_calculation" {
  // BMI计算测试
  let weight = 70  // 公斤
  let height = 175  // 厘米
  
  // BMI = 体重(kg) / (身高(m))^2
  let height_m = @azimuth.divide(height, 100)  // 转换为米
  let bmi = @azimuth.divide(weight, @azimuth.multiply(height_m, height_m))
  
  // BMI应该在正常范围内 (18.5-24.9)
  @azimuth.assert_eq(true, bmi >= 18.5 && bmi <= 24.9)
}