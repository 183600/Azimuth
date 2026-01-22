// 标准 MoonBit 增强测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "power_calculation_simulation" {
  // 模拟幂运算计算 (使用重复乘法)
  let base = 3
  let result = multiply(multiply(multiply(base, base), base), base)
  assert_eq(81, result)  // 3^4 = 81
}

test "factorial_calculation" {
  // 模拟阶乘计算
  let factorial = multiply(multiply(multiply(multiply(1, 2), 3), 4), 5)
  assert_eq(120, factorial)  // 5! = 120
}

test "percentage_calculations" {
  // 百分比计算测试
  let original_value = 200
  let percentage = 25
  
  // 计算百分比值
  let percentage_value = divide_with_ceil(multiply(original_value, percentage), 100)
  
  // 计算增加后的值
  let increased_value = add(original_value, percentage_value)
  
  // 计算减少后的值
  let decreased_value = add(original_value, -percentage_value)
  
  assert_eq(50, percentage_value)
  assert_eq(250, increased_value)
  assert_eq(150, decreased_value)
}

test "time_conversion_calculations" {
  // 时间转换计算
  let hours = 2
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = add(multiply(hours, 60), minutes)
  
  // 转换为总秒数
  let total_seconds = multiply(total_minutes, 60)
  
  assert_eq(165, total_minutes)
  assert_eq(9900, total_seconds)
}

test "geometric_calculations" {
  // 几何计算
  let radius = 5
  
  // 计算直径
  let diameter = multiply(radius, 2)
  
  // 计算周长 (简化为 π * d，使用π≈3)
  let circumference = multiply(diameter, 3)
  
  // 计算面积 (简化为 π * r²，使用π≈3)
  let area = multiply(multiply(radius, radius), 3)
  
  assert_eq(10, diameter)
  assert_eq(30, circumference)
  assert_eq(75, area)
}

test "temperature_conversion" {
  // 温度转换计算 (简化版本)
  let celsius = 25
  
  // 摄氏度转华氏度 (简化公式: F = C * 2 + 30)
  let fahrenheit = add(multiply(celsius, 2), 30)
  
  // 华氏度转摄氏度 (简化公式: C = (F - 30) / 2)
  let back_to_celsius = divide_with_ceil(add(fahrenheit, -30), 2)
  
  assert_eq(80, fahrenheit)
  assert_eq(25, back_to_celsius)
}

test "array_sum_simulation" {
  // 模拟数组求和 (使用累加)
  let value1 = 15
  let value2 = 23
  let value3 = 7
  let value4 = 12
  
  let total_sum = add(add(add(value1, value2), value3), value4)
  let average = divide_with_ceil(total_sum, 4)
  
  assert_eq(57, total_sum)
  assert_eq(15, average)  // ceil(57/4) = ceil(14.25) = 15
}

test "compound_interest_simplified" {
  // 简化的复利计算
  let principal = 1000
  let rate = 5  // 5%
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate), 100))
  
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate), 100))
  
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "batch_processing_simulation" {
  // 批处理模拟计算
  let batch_size = 25
  let total_items = 180
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_items, batch_size)
  
  // 计算最后一批的实际项目数
  let last_batch_size = add(total_items, -multiply(batches_needed - 1, batch_size))
  
  assert_eq(8, batches_needed)  // ceil(180/25) = 8
  assert_eq(5, last_batch_size)  // 180 - 7*25 = 5
}

test "string_greeting_with_special_characters" {
  // 测试特殊字符的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}