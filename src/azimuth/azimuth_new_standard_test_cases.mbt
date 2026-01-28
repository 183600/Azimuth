// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "emoji_greeting_test" {
  // 测试包含 Emoji 的问候
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "extreme_values_test" {
  // 测试极端值的向上取整除法
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "math_series_test" {
  // 测试数学级数求和
  let n = 10
  let sum_formula = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, sum_formula)
}

test "business_calculation_test" {
  // 测试商业利润计算
  let revenue = 50000
  let costs = 32000
  let profit_before_tax = add(revenue, -costs)
  assert_eq(18000, profit_before_tax)
}

test "coordinate_distance_test" {
  // 测试曼哈顿距离计算
  let x1 = 3
  let y1 = 7
  let x2 = 10
  let y2 = 2
  let x_distance = if x2 > x1 { add(x2, -x1) } else { add(x1, -x2) }
  let y_distance = if y2 > y1 { add(y2, -y1) } else { add(y1, -y2) }
  let manhattan_distance = add(x_distance, y_distance)
  assert_eq(12, manhattan_distance)
}

test "time_conversion_test" {
  // 测试时区转换计算
  let local_time = 14
  let time_difference = -8
  let target_time = add(local_time, time_difference)
  let adjusted_time = if target_time < 0 {
    add(target_time, 24)
  } else {
    target_time
  }
  assert_eq(6, adjusted_time)
}

test "array_index_test" {
  // 测试数组索引计算
  let buffer_size = 8
  let current_index = 6
  let offset = 5
  let raw_index = add(current_index, offset)
  let new_index = raw_index - multiply(buffer_size, divide_with_ceil(raw_index, buffer_size))
  assert_eq(3, new_index)
}

test "digital_root_test" {
  // 测试数字根计算
  let number = 9876
  let thousands = divide_with_ceil(number, 1000)
  let remainder_after_thousands = add(number, -multiply(thousands, 1000))
  let hundreds = divide_with_ceil(remainder_after_thousands, 100)
  let digit_sum = add(add(add(thousands, hundreds), 7), 6)
  assert_eq(30, digit_sum)
}

test "compound_interest_test" {
  // 测试简单复利计算
  let principal = 1000
  let rate_percent = 5
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let year1_total = add(principal, year1_interest)
  assert_eq(1050, year1_total)
}

test "temperature_conversion_test" {
  // 测试温度转换
  let celsius = 25
  let multiplied = multiply(celsius, 9)
  let divided = divide_with_ceil(multiplied, 5)
  let fahrenheit = add(divided, 32)
  assert_eq(77, fahrenheit)
}