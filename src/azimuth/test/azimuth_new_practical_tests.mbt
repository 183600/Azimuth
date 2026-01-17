// Azimuth 项目实用测试用例
// 使用标准 MoonBit 测试语法，包含实际应用场景

test "array_sum_calculation" {
  // 数组求和测试
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let sum = 0
  
  // 模拟数组求和
  let sum1 = sum + 1 + 2 + 3 + 4 + 5
  let sum2 = sum1 + 6 + 7 + 8 + 9 + 10
  
  let _ = sum2
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let percentage = 25
  
  // 计算 25% 的值
  let result = total * percentage / 100
  
  // 计算增长后的值
  let increased = total + result
  
  let _ = result
  let _ = increased
}

test "average_calculation" {
  // 平均值计算测试
  let values = [85, 90, 78, 92, 88]
  let count = 5
  
  // 计算总和
  let sum = 85 + 90 + 78 + 92 + 88
  
  // 计算平均值
  let average = sum / count
  
  let _ = sum
  let _ = average
}

test "currency_conversion" {
  // 货币转换测试
  let usd = 100
  let exchange_rate = 65  // 假设汇率 (6.5 * 10)
  
  // 转换为人民币 (需要除以10)
  let cny = usd * exchange_rate / 10
  
  // 加入手续费 (2%)
  let fee = cny * 2 / 100
  let total = cny + fee
  
  let _ = cny
  let _ = total
}

test "speed_distance_time" {
  // 速度、距离、时间计算测试
  let distance = 300  // 公里
  let time = 5        // 小时
  
  // 计算速度
  let speed = distance / time
  
  // 计算不同时间下的距离
  let distance_2h = speed * 2
  let distance_8h = speed * 8
  
  let _ = speed
  let _ = distance_2h
  let _ = distance_8h
}

test "compound_interest" {
  // 复利计算测试
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 第一年
  let year1 = principal + principal * rate / 100
  
  // 第二年
  let year2 = year1 + year1 * rate / 100
  
  // 第三年
  let year3 = year2 + year2 * rate / 100
  
  let _ = year1
  let _ = year2
  let _ = year3
}

test "discount_pricing" {
  // 折扣定价测试
  let original_price = 120
  let discount1 = 10  // 第一级折扣 10%
  let discount2 = 5   // 额外折扣 5%
  
  // 应用第一级折扣
  let price1 = original_price - original_price * discount1 / 100
  
  // 应用额外折扣
  let final_price = price1 - price1 * discount2 / 100
  
  // 计算总折扣金额
  let total_discount = original_price - final_price
  
  let _ = price1
  let _ = final_price
  let _ = total_discount
}

test " bmi_calculation" {
  // BMI 计算测试
  let weight = 70    // 公斤
  let height = 175   // 厘米
  
  // 转换为米
  let height_m = height / 100
  
  // 计算 BMI: 体重 / (身高^2)
  let bmi = weight / (height_m * height_m)
  
  let _ = height_m
  let _ = bmi
}

test "loan_repayment" {
  // 贷款还款测试
  let loan_amount = 50000
  let annual_rate = 6  // 年利率 6%
  let years = 10
  
  // 计算年利率
  let yearly_interest = loan_amount * annual_rate / 100
  
  // 计算总利息
  let total_interest = yearly_interest * years
  
  // 计算还款总额
  let total_repayment = loan_amount + total_interest
  
  // 计算月还款额
  let monthly_payment = total_repayment / (years * 12)
  
  let _ = yearly_interest
  let _ = total_repayment
  let _ = monthly_payment
}

test "data_transfer_calculation" {
  // 数据传输计算测试
  let file_size = 800    // MB
  let speed_mbps = 20    // Mbps
  
  // 计算传输时间 (秒)
  let time_seconds = file_size * 8 / speed_mbps
  
  // 转换为分钟
  let time_minutes = time_seconds / 60
  
  // 计算多个文件的传输时间
  let files_count = 5
  let total_time = time_minutes * files_count
  
  let _ = time_seconds
  let _ = time_minutes
  let _ = total_time
}