// 标准 MoonBit 测试用例
// 针对 azimuth 库的额外测试用例，确保不超过10个

test "subtract_functionality" {
  // 测试减法功能（使用 add 和负数实现）
  assert_eq(5, add(8, -3))
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(10, -10))
  assert_eq(100, add(50, -(-50)))
}

test "modulo_operation_simulation" {
  // 模拟取模运算
  let dividend = 17
  let divisor = 5
  let quotient = divide_with_ceil(dividend, divisor)
  let product = multiply(divisor, quotient)
  let remainder = add(dividend, -product)
  
  assert_eq(4, quotient)  // ceil(17/5) = 4
  assert_eq(20, product)  // 5 * 4 = 20
  assert_eq(-3, remainder)  // 17 - 20 = -3
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let percentage_25 = divide_with_ceil(multiply(total, 25), 100)
  let percentage_50 = divide_with_ceil(multiply(total, 50), 100)
  let percentage_75 = divide_with_ceil(multiply(total, 75), 100)
  
  assert_eq(50, percentage_25)
  assert_eq(100, percentage_50)
  assert_eq(150, percentage_75)
}

test "geometric_progression" {
  // 等比数列计算
  let first_term = 3
  let ratio = 2
  let terms = 5
  
  let term2 = multiply(first_term, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let sum = add(add(add(add(first_term, term2), term3), term4), term5)
  
  assert_eq(6, term2)
  assert_eq(12, term3)
  assert_eq(24, term4)
  assert_eq(48, term5)
  assert_eq(93, sum)
}

test "temperature_conversion" {
  // 温度转换模拟（简化的华氏度转摄氏度）
  let fahrenheit = 68
  let celsius_approx = divide_with_ceil(multiply(fahrenheit - 32, 5), 9)
  
  assert_eq(20, celsius_approx)  // (68-32)*5/9 = 20
}

test "array_index_simulation" {
  // 模拟数组索引计算
  let page_size = 10
  let item_index = 47
  
  let page_number = divide_with_ceil(item_index + 1, page_size)
  let index_on_page = add(item_index, -multiply(page_number - 1, page_size))
  
  assert_eq(5, page_number)  // ceil(48/10) = 5
  assert_eq(7, index_on_page)  // 47 - (5-1)*10 = 7
}

test "time_zone_conversion" {
  // 时区转换模拟
  let local_time = 23  // 晚上11点
  let time_difference = 5  // 时差5小时
  
  let utc_time = add(local_time, -time_difference)
  let adjusted_utc_time = if utc_time < 0 { add(utc_time, 24) } else { utc_time }
  
  assert_eq(18, adjusted_utc_time)  // 23 - 5 = 18
}

test "batch_processing" {
  // 批处理计算
  let total_items = 157
  let batch_size = 25
  
  let full_batches = divide_with_ceil(total_items, batch_size)
  let last_batch_size = add(total_items, -multiply(full_batches - 1, batch_size))
  
  assert_eq(7, full_batches)  // ceil(157/25) = 7
  assert_eq(7, last_batch_size)  // 157 - (7-1)*25 = 7
}

test "compound_interest_simple" {
  // 简单复利计算
  let principal = 1000
  let rate_percent = 10
  let years = 3
  
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1100, year1)  // 1000 + 100
  assert_eq(1210, year2)  // 1100 + 110
  assert_eq(1331, year3)  // 1210 + 121
}

test "greeting_with_special_formatting" {
  // 特殊格式化的问候测试
  let name_with_title = "Dr. Smith"
  let name_with_suffix = "John Jr."
  let name_with_special = "O'Connor"
  
  assert_eq_string("Hello, Dr. Smith!", greet(name_with_title))
  assert_eq_string("Hello, John Jr.!", greet(name_with_suffix))
  assert_eq_string("Hello, O'Connor!", greet(name_with_special))
}