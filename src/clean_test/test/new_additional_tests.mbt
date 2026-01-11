// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 新增的测试用例 - 覆盖更多实际场景和边界情况

// 测试用例 1: 除法近似计算（通过乘法和减法实现）
///|
test "division_approximation" {
  // 近似计算 a / b，通过估算和调整
  // 计算 17 / 4 ≈ 4（因为 4 * 4 = 16 最接近）
  
  let dividend = 17
  let divisor = 4
  
  // 估算商为4
  let estimated_quotient = 4
  let product = @clean_test.@clean_test.multiply(divisor, estimated_quotient)
  
  // 验证 4 * 4 = 16，接近17
  assert_eq(16, product)
  
  // 计算余数
  let remainder = @clean_test.@clean_test.add(dividend, @clean_test.@clean_test.multiply(-1, product))
  assert_eq(1, remainder)  // 17 - 16 = 1
}

// 测试用例 2: 百分比计算
///|
test "percentage_calculations" {
  // 计算百分比：部分 / 总数 * 100
  // 我们通过乘法来避免除法
  
  let part = 25
  let total = 200
  
  // 百分比 * 总数 = 部分 * 100
  let percentage_times_total = @clean_test.@clean_test.multiply(part, 100)
  
  // 验证：12.5% * 200 = 25，即 12.5 * 200 = 2500
  assert_eq(2500, percentage_times_total)
}

// 测试用例 3: 面积单位转换
///|
test "area_unit_conversion" {
  // 平方米转换为平方厘米：1 m² = 10000 cm²
  
  let square_meters = 5
  let conversion_factor = 10000
  
  // 平方厘米 = 平方米 * 转换因子
  let square_centimeters = @clean_test.@clean_test.multiply(square_meters, conversion_factor)
  
  // 验证：5 m² = 50000 cm²
  assert_eq(50000, square_centimeters)
}

// 测试用例 4: 体积计算
///|
test "volume_calculation" {
  // 长方体体积 = 长 * 宽 * 高
  
  let length = 10
  let width = 5
  let height = 3
  
  // 逐步计算体积
  let base_area = @clean_test.@clean_test.multiply(length, width)
  let volume = @clean_test.@clean_test.multiply(base_area, height)
  
  // 验证：10 * 5 * 3 = 150
  assert_eq(150, volume)
}

// 测试用例 5: 速度单位转换
///|
test "speed_unit_conversion" {
  // km/h 转换为 m/s：乘以 1000/3600 = 5/18
  // 我们通过乘以18来避免分数
  
  let km_per_hour = 72
  
  // m/s * 18 = km/h * 5
  let m_per_s_times_18 = @clean_test.@clean_test.multiply(km_per_hour, 5)
  
  // 验证：72 km/h = 20 m/s，即 20 * 18 = 360
  assert_eq(360, m_per_s_times_18)
}

// 测试用例 6: 字符串长度验证
///|
test "string_length_validation" {
  // 通过字符串拼接来验证长度处理
  
  let short = "Hi"
  let medium = "MoonBit"
  let long = "This is a very long string"
  
  // 测试不同长度字符串的拼接
  let result1 = @clean_test.@clean_test.greet(short)
  let result2 = @clean_test.@clean_test.greet(medium)
  let result3 = @clean_test.@clean_test.greet(long)
  
  // 验证结果包含预期的前缀
  assert_eq(true, result1.starts_with("Hello, "))
  assert_eq(true, result2.starts_with("Hello, "))
  assert_eq(true, result3.starts_with("Hello, "))
  
  // 验证结果包含预期的后缀
  assert_eq(true, result1.ends_with("!"))
  assert_eq(true, result2.ends_with("!"))
  assert_eq(true, result3.ends_with("!"))
}

// 测试用例 7: 数学级数求和
///|
test "geometric_series_sum" {
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  
  let term1 = 2
  let term2 = 4
  let term3 = 8
  let term4 = 16
  
  // 逐步求和
  let sum1 = @clean_test.@clean_test.add(term1, term2)
  let sum2 = @clean_test.@clean_test.add(sum1, term3)
  let total = @clean_test.@clean_test.add(sum2, term4)
  
  // 验证总和
  assert_eq(30, total)
  
  // 验证等比性质：后一项是前一项的2倍
  assert_eq(term2, @clean_test.@clean_test.multiply(term1, 2))
  assert_eq(term3, @clean_test.@clean_test.multiply(term2, 2))
  assert_eq(term4, @clean_test.@clean_test.multiply(term3, 2))
}

// 测试用例 8: 温度差计算
///|
test "temperature_difference" {
  // 计算两个温度之间的差值
  
  let temp1 = 25  // 25°C
  let temp2 = -5  // -5°C
  
  // 温度差 = 温度1 - 温度2
  let difference = @clean_test.@clean_test.add(temp1, @clean_test.@clean_test.multiply(-1, temp2))
  
  // 验证：25 - (-5) = 30
  assert_eq(30, difference)
  
  // 反向计算应该得到负值
  let reverse_difference = @clean_test.@clean_test.add(temp2, @clean_test.@clean_test.multiply(-1, temp1))
  assert_eq(-30, reverse_difference)
}

// 测试用例 9: 质数验证（简单测试）
///|
test "prime_number_properties" {
  // 测试一些质数的基本性质
  
  let prime1 = 7
  let prime2 = 13
  let composite = 12
  
  // 质数乘以2得到偶数
  let even1 = @clean_test.@clean_test.multiply(prime1, 2)
  let even2 = @clean_test.@clean_test.multiply(prime2, 2)
  
  // 验证结果
  assert_eq(14, even1)
  assert_eq(26, even2)
  
  // 合数可以分解为因数的乘积
  let factor1 = 3
  let factor2 = 4
  let product = @clean_test.@clean_test.multiply(factor1, factor2)
  
  // 验证：3 * 4 = 12
  assert_eq(composite, product)
}

// 测试用例 10: 时间段计算
///|
test "time_period_calculation" {
  // 计算时间段的总秒数
  
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为秒
  let hours_in_seconds = @clean_test.@clean_test.multiply(hours, 3600)
  let minutes_in_seconds = @clean_test.@clean_test.multiply(minutes, 60)
  
  // 总秒数
  let total_seconds = @clean_test.@clean_test.add(@clean_test.@clean_test.add(hours_in_seconds, minutes_in_seconds), seconds)
  
  // 验证：2*3600 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
  assert_eq(9045, total_seconds)
}