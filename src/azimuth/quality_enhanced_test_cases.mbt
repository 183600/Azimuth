// 简化的高质量测试用例
// 针对 azimuth 库的核心功能进行实际应用场景测试

test "simple_financial_calculation" {
  // 简单的金融计算测试
  let principal = 1000
  let interest_rate = 5
  
  // 计算一年的利息
  let interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let total = add(principal, interest)
  
  assert_eq(1050, total)
}

test "simple_packaging_calculation" {
  // 简单的包装计算测试
  let total_items = 25
  let items_per_box = 6
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(5, boxes_needed)
}

test "simple_area_calculation" {
  // 简单的面积计算测试
  let length = 10
  let width = 5
  
  // 计算面积
  let area = multiply(length, width)
  
  assert_eq(50, area)
}

test "simple_greeting_test" {
  // 简单的问候测试
  let name = "MoonBit"
  let greeting = greet(name)
  
  assert_eq_string("Hello, MoonBit!", greeting)
}

test "simple_temperature_conversion" {
  // 简单的温度转换测试
  let celsius = 25
  
  // 转换为华氏度（简化版本）
  let fahrenheit = add(multiply(celsius, 9), 160) / 5
  
  assert_eq(77, fahrenheit)
}

test "simple_batch_processing" {
  // 简单的批处理测试
  let total_items = 85
  let batch_size = 20
  
  // 计算批次数
  let batches = divide_with_ceil(total_items, batch_size)
  
  assert_eq(5, batches)
}

test "simple_time_calculation" {
  // 简单的时间计算测试
  let total_minutes = 150
  
  // 转换为小时和分钟
  let hours = total_minutes / 60
  let remaining_minutes = total_minutes - multiply(hours, 60)
  
  assert_eq(2, hours)
  assert_eq(30, remaining_minutes)
}

test "simple_percentage_calculation" {
  // 简单的百分比计算测试
  let total = 200
  let percentage = 15
  
  // 计算百分比值
  let result = divide_with_ceil(multiply(total, percentage), 100)
  
  assert_eq(30, result)
}

test "simple_average_calculation" {
  // 简单的平均值计算测试
  let value1 = 85
  let value2 = 90
  let value3 = 95
  
  // 计算平均值
  let sum = add(add(value1, value2), value3)
  let average = divide_with_ceil(sum, 3)
  
  assert_eq(90, average)
}

test "simple_discount_calculation" {
  // 简单的折扣计算测试
  let original_price = 120
  let discount_percentage = 20
  
  // 计算折扣后的价格
  let discount_amount = divide_with_ceil(multiply(original_price, discount_percentage), 100)
  let final_price = add(original_price, -discount_amount)
  
  assert_eq(96, final_price)
}