// 新增的 MoonBit 测试用例
// 这些测试用例进一步扩展了对 azimuth 库功能的验证

test "add_with_large_positive_numbers" {
  // 测试大正数相加
  let num1 = 1000000000
  let num2 = 1000000000
  let result = add(num1, num2)
  assert_eq(2000000000, result)
  
  // 测试接近最大值的数相加
  let near_max = 2147483640
  let small = 5
  let result2 = add(near_max, small)
  assert_eq(2147483645, result2)
}

test "add_with_large_negative_numbers" {
  // 测试大负数相加
  let num1 = -1000000000
  let num2 = -1000000000
  let result = add(num1, num2)
  assert_eq(-2000000000, result)
  
  // 测试接近最小值的数相加
  let near_min = -2147483640
  let small = -5
  let result2 = add(near_min, small)
  assert_eq(-2147483645, result2)
}

test "multiply_with_decimal_simulation" {
  // 通过整数运算模拟小数乘法（例如：计算折扣）
  // 原价 200，打 8 折 = 200 * 80 / 100 = 160
  let original_price = 200
  let discount_percent = 80
  let discounted_price = multiply(original_price, discount_percent) / 100
  assert_eq(160, discounted_price)
  
  // 原价 150，打 7 折 = 150 * 70 / 100 = 105
  let original_price2 = 150
  let discount_percent2 = 70
  let discounted_price2 = multiply(original_price2, discount_percent2) / 100
  assert_eq(105, discounted_price2)
}

test "compound_interest_calculation" {
  // 简化的复利计算：本金 * (1 + 利率 * 年数)
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  let interest_rate = multiply(rate, years)
  let total_rate = add(100, interest_rate)  // 100 + 5*3 = 115
  let total_amount = multiply(principal, total_rate) / 100
  assert_eq(11500, total_amount)  // 10000 * 115 / 100 = 11500
}

test "greet_with_special_unicode_characters" {
  // 测试包含特殊 Unicode 字符的字符串
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ⭐!", greet("⭐"))
  assert_eq_string("Hello, ❤️!", greet("❤️"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
}

test "greet_with_mathematical_symbols" {
  // 测试包含数学符号的字符串
  assert_eq_string("Hello, ∑!", greet("∑"))
  assert_eq_string("Hello, ∆!", greet("∆"))
  assert_eq_string("Hello, ∏!", greet("∏"))
  assert_eq_string("Hello, ∫!", greet("∫"))
}

test "nested_arithmetic_operations" {
  // 测试嵌套算术运算：(a + b) * (c + d)
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  
  let sum1 = add(a, b)
  let sum2 = add(c, d)
  let result = multiply(sum1, sum2)
  assert_eq(2100, result)  // (10 + 20) * (30 + 40) = 30 * 70 = 2100
}

test "area_of_triangle" {
  // 测试三角形面积计算：底 * 高 / 2
  let base = 10
  let height = 8
  let area = multiply(base, height) / 2
  assert_eq(40, area)
  
  // 测试另一个三角形
  let base2 = 15
  let height2 = 12
  let area2 = multiply(base2, height2) / 2
  assert_eq(90, area2)
}

test "time_conversion" {
  // 测试时间转换：小时转换为分钟
  let hours = 5
  let minutes = multiply(hours, 60)
  assert_eq(300, minutes)
  
  // 测试天转换为小时
  let days = 3
  let hours_in_days = multiply(days, 24)
  assert_eq(72, hours_in_days)
  
  // 测试周转换为天
  let weeks = 2
  let days_in_weeks = multiply(weeks, 7)
  assert_eq(14, days_in_weeks)
}

test "speed_distance_calculation" {
  // 测试速度、距离、时间的关系：距离 = 速度 * 时间
  let speed = 60  // km/h
  let time = 2    // hours
  let distance = multiply(speed, time)
  assert_eq(120, distance)  // 60 km/h * 2 h = 120 km
  
  // 测试计算总距离：分段距离相加
  let distance1 = multiply(50, 1)  // 50 km/h * 1 h = 50 km
  let distance2 = multiply(80, 2)  // 80 km/h * 2 h = 160 km
  let total_distance = add(distance1, distance2)
  assert_eq(210, total_distance)  // 50 + 160 = 210 km
}