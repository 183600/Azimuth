// 新增的标准 MoonBit 测试用例
// 覆盖 azimuth 库的各种功能场景

test "arithmetics_edge_cases" {
  // 测试算术运算的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(21, multiply(-3, -7))
  assert_eq(4, subtract(-3, -7))
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-1, divide_with_ceil(-9, 10))
}

test "mixed_sign_operations" {
  // 测试混合符号运算
  assert_eq(-4, add(3, -7))
  assert_eq(-21, multiply(3, -7))
  assert_eq(10, subtract(3, -7))
  assert_eq(-1, divide_with_ceil(-10, 11))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  assert_eq(0, subtract(large_num, large_num))
  assert_eq(1000, divide_with_ceil(large_num, 1000))
}

test "string_greeting_variations" {
  // 测试各种字符串问候场景
  assert_eq_string("Hello, John!", greet("John"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, special@chars#!", greet("special@chars#"))
}

test "financial_calculations" {
  // 金融计算场景测试
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简单利息计算
  let simple_interest = multiply(principal, multiply(rate, years)) / 100
  let total_amount = add(principal, simple_interest)
  
  assert_eq(150, simple_interest)
  assert_eq(1150, total_amount)
}

test "inventory_management" {
  // 库存管理场景测试
  let initial_stock = 120
  let sold_items = 37
  let new_shipment = 85
  let items_per_box = 12
  
  let remaining_stock = subtract(initial_stock, sold_items)
  let updated_stock = add(remaining_stock, new_shipment)
  let boxes_needed = divide_with_ceil(updated_stock, items_per_box)
  
  assert_eq(83, remaining_stock)
  assert_eq(168, updated_stock)
  assert_eq(14, boxes_needed)
}

test "time_calculations" {
  // 时间计算场景测试
  let hours = 47
  let minutes_per_hour = 60
  let seconds_per_minute = 60
  
  let total_minutes = multiply(hours, minutes_per_hour)
  let total_seconds = multiply(total_minutes, seconds_per_minute)
  let days = divide_with_ceil(hours, 24)
  
  assert_eq(2820, total_minutes)
  assert_eq(169200, total_seconds)
  assert_eq(2, days)
}

test "temperature_conversion" {
  // 温度转换场景测试（简化版）
  let celsius = 25
  let fahrenheit_offset = 32
  let conversion_factor = 5
  let conversion_divisor = 9
  
  // 简化的摄氏度到华氏度转换
  let fahrenheit = add(fahrenheit_offset, multiply(celsius, conversion_divisor) / conversion_factor)
  
  assert_eq(77, fahrenheit)
}