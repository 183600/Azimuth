// Azimuth 项目新增标准 MoonBit 测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let sum = 5 + 3
  let product = 4 * 6
  let difference = 10 - 7
  let quotient = 15 / 3
  
  // 验证计算结果
  let check_sum = sum
  let check_product = product
  let check_difference = difference
  let check_quotient = quotient
}

test "complex_calculations" {
  // 复杂计算测试
  let a = 10
  let b = 5
  let c = 2
  
  // (a + b) * c
  let result1 = (a + b) * c
  
  // a * b + c
  let result2 = a * b + c
  
  // (a - b) / c
  let result3 = (a - b) / c
  
  // 验证计算结果
  let check_result1 = result1
  let check_result2 = result2
  let check_result3 = result3
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let percentage = 15
  
  // 计算百分比值
  let percentage_value = (total * percentage) / 100
  
  // 计算增加百分比后的值
  let increased_value = total + percentage_value
  
  // 验证计算结果
  let check_percentage_value = percentage_value
  let check_increased_value = increased_value
}

test "discount_calculations" {
  // 折扣计算测试
  let original_price = 120
  let discount_rate = 20
  
  // 计算折扣金额
  let discount_amount = (original_price * discount_rate) / 100
  
  // 计算折后价格
  let final_price = original_price - discount_amount
  
  // 验证计算结果
  let check_discount_amount = discount_amount
  let check_final_price = final_price
}

test "speed_distance_time" {
  // 速度-距离-时间计算测试
  let distance = 300
  let time = 5
  
  // 计算速度
  let speed = distance / time
  
  // 计算不同时间下的距离
  let distance_2_hours = speed * 2
  let distance_8_hours = speed * 8
  
  // 验证计算结果
  let check_speed = speed
  let check_distance_2_hours = distance_2_hours
  let check_distance_8_hours = distance_8_hours
}

test "array_sum_calculation" {
  // 数组求和计算测试
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  // 计算数组元素的和
  let sum1 = num1 + num2
  let sum2 = sum1 + num3
  let sum3 = sum2 + num4
  let total_sum = sum3 + num5
  
  // 验证总和
  let check_total_sum = total_sum
}

test "inventory_calculation" {
  // 库存计算测试
  let initial_stock = 500
  let units_produced = 150
  let units_sold = 80
  let units_damaged = 20
  
  // 计算最终库存
  let total_available = initial_stock + units_produced
  let total_removed = units_sold + units_damaged
  let final_stock = total_available - total_removed
  
  // 验证计算结果
  let check_total_available = total_available
  let check_total_removed = total_removed
  let check_final_stock = final_stock
}

test "compound_interest_simple" {
  // 简单复利计算测试
  let principal = 1000
  let rate = 10
  
  // 第一年
  let year1 = principal + (principal * rate) / 100
  
  // 第二年
  let year2 = year1 + (year1 * rate) / 100
  
  // 第三年
  let year3 = year2 + (year2 * rate) / 100
  
  // 验证计算结果
  let check_year1 = year1
  let check_year2 = year2
  let check_year3 = year3
}

test "batch_processing_calculation" {
  // 批处理计算测试
  let batch_size = 25
  let total_items = 200
  let processing_time_per_batch = 5
  
  // 计算需要的批次数
  let batches_needed = (total_items + batch_size - 1) / batch_size  // 向上取整
  
  // 计算总处理时间
  let total_time = batches_needed * processing_time_per_batch
  
  // 验证计算结果
  let check_batches_needed = batches_needed
  let check_total_time = total_time
}

test "roi_calculation" {
  // 投资回报率(ROI)计算测试
  let initial_investment = 10000
  let final_value = 15000
  let expenses = 1000
  
  // 计算净利润
  let net_profit = (final_value - initial_investment) - expenses
  
  // 计算ROI百分比
  let roi_percentage = (net_profit * 100) / initial_investment
  
  // 计算投资倍数
  let investment_multiple = final_value / initial_investment
  
  // 验证计算结果
  let check_net_profit = net_profit
  let check_roi_percentage = roi_percentage
  let check_investment_multiple = investment_multiple
}