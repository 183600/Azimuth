// 额外的 MoonBit 测试用例
// 包含各种实用场景和边界情况测试

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
}

test "prime_number_validation" {
  // 素数验证测试（通过检查是否能被整除）
  let num = 17
  // 简化的素数检查：检查是否能被2, 3, 4, 5整除
  let divisible_by_2 = num % 2 == 0
  let divisible_by_3 = num % 3 == 0
  let divisible_by_4 = num % 4 == 0
  let divisible_by_5 = num % 5 == 0
  
  // 17不能被2, 3, 4, 5整除，所以是素数
  @azimuth.assert_false(divisible_by_2)
  @azimuth.assert_false(divisible_by_3)
  @azimuth.assert_false(divisible_by_4)
  @azimuth.assert_false(divisible_by_5)
  
  // 测试非素数
  let composite_num = 15
  let composite_divisible_by_3 = composite_num % 3 == 0
  let composite_divisible_by_5 = composite_num % 5 == 0
  
  @azimuth.assert_true(composite_divisible_by_3 || composite_divisible_by_5)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 + 30
  let celsius_0 = 0
  let celsius_20 = 20
  let celsius_100 = 100
  let celsius_minus_10 = -10
  
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 2), 30)
  let fahrenheit_20 = @azimuth.add(@azimuth.multiply(celsius_20, 2), 30)
  let fahrenheit_100 = @azimuth.add(@azimuth.multiply(celsius_100, 2), 30)
  let fahrenheit_minus_10 = @azimuth.add(@azimuth.multiply(celsius_minus_10, 2), 30)
  
  @azimuth.assert_eq(30, fahrenheit_0)      // 0°C ≈ 30°F
  @azimuth.assert_eq(70, fahrenheit_20)    // 20°C ≈ 70°F
  @azimuth.assert_eq(230, fahrenheit_100)  // 100°C ≈ 230°F
  @azimuth.assert_eq(10, fahrenheit_minus_10) // -10°C ≈ 10°F
}

test "simple_interest_calculation" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate_percent = 5
  let time_years = 3
  
  // 计算利息（利率为百分比，需要除以100）
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate_percent, time_years)) / 100
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(150, interest)      // 1000 × 5 × 3 / 100 = 150
  @azimuth.assert_eq(1150, total_amount) // 1000 + 150 = 1150
}

test "array_sum_and_average" {
  // 数组求和与平均值计算
  // 模拟数组 [10, 20, 30, 40, 50]
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  // 计算平均值
  let average = sum / 5
  
  @azimuth.assert_eq(150, sum)
  @azimuth.assert_eq(30, average)
}

test "percentage_increase_decrease" {
  // 百分比增加和减少计算
  let original_value = 200
  let increase_percent = 25
  let decrease_percent = 20
  
  // 计算增加后的值
  let increase_amount = @azimuth.multiply(original_value, increase_percent) / 100
  let increased_value = @azimuth.add(original_value, increase_amount)
  
  // 计算减少后的值
  let decrease_amount = @azimuth.multiply(original_value, decrease_percent) / 100
  let decreased_value = @azimuth.add(original_value, -decrease_amount)
  
  @azimuth.assert_eq(50, increase_amount)
  @azimuth.assert_eq(250, increased_value)
  @azimuth.assert_eq(40, decrease_amount)
  @azimuth.assert_eq(160, decreased_value)
}

test "string_case_and_formatting" {
  // 字符串大小写和格式化测试
  let lowercase_name = "john"
  let uppercase_name = "JOHN"
  let mixed_case_name = "JoHn"
  let name_with_spaces = " john doe "
  
  // 测试greet函数处理不同格式的字符串
  @azimuth.assert_eq_string("Hello, john!", @azimuth.greet(lowercase_name))
  @azimuth.assert_eq_string("Hello, JOHN!", @azimuth.greet(uppercase_name))
  @azimuth.assert_eq_string("Hello, JoHn!", @azimuth.greet(mixed_case_name))
  @azimuth.assert_eq_string("Hello,  john doe !", @azimuth.greet(name_with_spaces))
}

test "geometric_area_calculations" {
  // 几何面积计算
  // 正方形面积 = 边长²
  let square_side = 5
  let square_area = @azimuth.multiply(square_side, square_side)
  
  // 矩形面积 = 长 × 宽
  let rectangle_length = 8
  let rectangle_width = 4
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  
  // 三角形面积 = 底 × 高 ÷ 2
  let triangle_base = 6
  let triangle_height = 8
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  
  // 梯形面积 = (上底 + 下底) × 高 ÷ 2
  let trapezoid_top = 4
  let trapezoid_bottom = 8
  let trapezoid_height = 5
  let trapezoid_area = @azimuth.multiply(@azimuth.add(trapezoid_top, trapezoid_bottom), trapezoid_height) / 2
  
  @azimuth.assert_eq(25, square_area)
  @azimuth.assert_eq(32, rectangle_area)
  @azimuth.assert_eq(24, triangle_area)
  @azimuth.assert_eq(30, trapezoid_area)
}

test "binary_number_simulation" {
  // 二进制数运算模拟
  // 模拟二进制 1010 (10) 和 0101 (5) 的运算
  let binary1_decimal = 10  // 1010 in binary
  let binary2_decimal = 5   // 0101 in binary
  
  // 二进制加法：1010 + 0101 = 1111 (15)
  let binary_sum = @azimuth.add(binary1_decimal, binary2_decimal)
  
  // 二进制乘法：1010 × 0101 = 00101010 (42)
  let binary_product = @azimuth.multiply(binary1_decimal, binary2_decimal)
  
  // 二进制与运算（模拟）：1010 & 0101 = 0000 (0)
  // 简化为检查是否有共同的位
  let has_common_bits = (binary1_decimal % 2 == 1) && (binary2_decimal % 2 == 1)
  
  // 二进制或运算（模拟）：1010 | 0101 = 1111 (15)
  // 简化为加法
  let binary_or = @azimuth.add(binary1_decimal, binary2_decimal)
  
  @azimuth.assert_eq(15, binary_sum)
  @azimuth.assert_eq(50, binary_product)
  @azimuth.assert_false(has_common_bits)
  @azimuth.assert_eq(15, binary_or)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_hour = 14  // 下午2点
  let timezone_offset = 8  // 与UTC的时差（小时）
  
  // 转换为UTC时间
  let utc_hour = @azimuth.add(local_hour, -timezone_offset)
  // 处理负数情况（前一天）
  let adjusted_utc_hour = if utc_hour < 0 { @azimuth.add(utc_hour, 24) } else { utc_hour }
  
  // 转换为另一个时区（例如比UTC快5小时）
  let target_timezone = 5
  let target_hour = @azimuth.add(adjusted_utc_hour, target_timezone)
  // 处理超过24小时的情况（下一天）
  let adjusted_target_hour = if target_hour >= 24 { @azimuth.add(target_hour, -24) } else { target_hour }
  
  @azimuth.assert_eq(6, adjusted_utc_hour)    // 14 - 8 = 6 (UTC时间)
  @azimuth.assert_eq(11, adjusted_target_hour) // 6 + 5 = 11 (目标时区时间)
}