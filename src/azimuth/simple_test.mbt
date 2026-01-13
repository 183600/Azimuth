test "basic_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}
test "basic_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}
test "basic_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}

// 新增测试用例
test "add_edge_case_max_min_values" {
  // 测试最大值和最小值相加的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "add_consecutive_operations" {
  // 测试连续加法操作
  let result = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result)
  
  let result2 = @azimuth.add(@azimuth.add(-5, 10), @azimuth.add(-3, 8))
  @azimuth.assert_eq(10, result2)
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(8, @azimuth.multiply(2, 4))
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
  @azimuth.assert_eq(-1024, @azimuth.multiply(-32, 32))
}

test "multiply_fractional_simulation" {
  // 通过整数运算模拟小数乘法（例如：计算百分比）
  // 25% of 400 = 400 * 25 / 100
  let value = 400
  let percentage = 25
  let result = @azimuth.multiply(value, percentage) / 100
  @azimuth.assert_eq(100, result)
  
  // 75% of 200 = 200 * 75 / 100
  let value2 = 200
  let percentage2 = 75
  let result2 = @azimuth.multiply(value2, percentage2) / 100
  @azimuth.assert_eq(150, result2)
}

test "greet_multilingual" {
  // 测试多语言问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, עברית!", @azimuth.greet("עברית"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", @azimuth.greet("Line1\nLine2"))
  @azimuth.assert_eq_string("Hello, Tab\tSeparated!", @azimuth.greet("Tab\tSeparated"))
  @azimuth.assert_eq_string("Hello, Emoji🌍!", @azimuth.greet("Emoji🌍"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  // 计算：1 + 2*3 + 4*5*6
  let term1 = 1
  let term2 = @azimuth.multiply(2, 3)
  let term3 = @azimuth.multiply(@azimuth.multiply(4, 5), 6)
  let result = @azimuth.add(@azimuth.add(term1, term2), term3)
  @azimuth.assert_eq(127, result)  // 1 + 6 + 120 = 127
}

// 新增的测试用例
test "power_calculation" {
  // 计算 x 的 n 次方（仅适用于正整数指数）
  let base = 3
  let step1 = @azimuth.multiply(base, base)  // 3 * 3 = 9
  let step2 = @azimuth.multiply(step1, base)  // 9 * 3 = 27
  let result = @azimuth.multiply(step2, base)  // 27 * 3 = 81
  @azimuth.assert_eq(result, 81)
}

test "factorial_calculation" {
  // 计算 n 的阶乘（n!）
  let step1 = @azimuth.multiply(5, 4)  // 5 * 4 = 20
  let step2 = @azimuth.multiply(step1, 3)  // 20 * 3 = 60
  let step3 = @azimuth.multiply(step2, 2)  // 60 * 2 = 120
  let result = @azimuth.multiply(step3, 1)  // 120 * 1 = 120
  @azimuth.assert_eq(result, 120)
}

test "compound_interest_calculation" {
  // 复利计算：A = P * (1 + r)^n
  // 简化版本，使用整数运算
  let principal = 1000  // 本金
  let rate = 5  // 利率（百分比）
  let years = 3  // 年数
  
  // 简化计算：每年利息 = 本金 * 利率 / 100
  let year1_interest = @azimuth.multiply(principal, rate) / 100  // 1000 * 5 / 100 = 50
  let year1_total = @azimuth.add(principal, year1_interest)  // 1000 + 50 = 1050
  
  let year2_interest = @azimuth.multiply(year1_total, rate) / 100  // 1050 * 5 / 100 = 52.5 ≈ 52
  let year2_total = @azimuth.add(year1_total, year2_interest)  // 1050 + 52 = 1102
  
  let year3_interest = @azimuth.multiply(year2_total, rate) / 100  // 1102 * 5 / 100 = 55.1 ≈ 55
  let final_total = @azimuth.add(year2_total, year3_interest)  // 1102 + 55 = 1157
  
  @azimuth.assert_eq(final_total, 1157)
}

test "geometric_calculations" {
  // 几何计算：圆的周长和面积（简化版，使用π≈3）
  let radius = 5
  
  // 周长 = 2 * π * r ≈ 2 * 3 * 5 = 30
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(circumference, 30)
  
  // 面积 = π * r^2 ≈ 3 * 5^2 = 3 * 25 = 75
  let radius_squared = @azimuth.multiply(radius, radius)
  let area = @azimuth.multiply(3, radius_squared)
  @azimuth.assert_eq(area, 75)
}

test "time_calculations" {
  // 时间计算：秒转换为分钟和小时
  let total_seconds = 3661  // 1小时1分钟1秒
  
  // 计算小时
  let hours = total_seconds / 3600  // 3661 / 3600 = 1
  @azimuth.assert_eq(hours, 1)
  
  // 计算剩余秒数
  let remaining_seconds = @azimuth.add(total_seconds, -hours * 3600)  // 3661 - 1*3600 = 61
  @azimuth.assert_eq(remaining_seconds, 61)
  
  // 计算分钟
  let minutes = remaining_seconds / 60  // 61 / 60 = 1
  @azimuth.assert_eq(minutes, 1)
  
  // 计算剩余秒数
  let final_seconds = @azimuth.add(remaining_seconds, -minutes * 60)  // 61 - 1*60 = 1
  @azimuth.assert_eq(final_seconds, 1)
}

test "currency_conversion" {
  // 货币转换（简化版）
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY（简化汇率）
  
  // 转换为人民币
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(cny_amount, 700)
  
  // 反向转换
  let converted_back = cny_amount / exchange_rate
  @azimuth.assert_eq(converted_back, usd_amount)
}

test "temperature_conversion_detailed" {
  // 温度转换：摄氏度转华氏度和开尔文
  let celsius = 25
  
  // 摄氏度转华氏度：F = C * 9/5 + 32（简化版：F = C * 2 + 30）
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(fahrenheit, 80)  // 25 * 2 + 30 = 80
  
  // 摄氏度转开尔文：K = C + 273
  let kelvin = @azimuth.add(celsius, 273)
  @azimuth.assert_eq(kelvin, 298)  // 25 + 273 = 298
  
  // 华氏度转回摄氏度（简化版）：C = (F - 30) / 2
  let converted_celsius = @azimuth.add(fahrenheit, -30) / 2
  @azimuth.assert_eq(converted_celsius, celsius)
}

test "max_min_value_search" {
  // 模拟数组处理：查找最大值和最小值
  let value1 = 15
  let value2 = 42
  let value3 = 7
  let value4 = 89
  let value5 = 23
  
  // 查找最大值
  let max1 = if value1 > value2 { value1 } else { value2 }
  let max2 = if max1 > value3 { max1 } else { value3 }
  let max3 = if max2 > value4 { max2 } else { value4 }
  let max_value = if max3 > value5 { max3 } else { value5 }
  
  @azimuth.assert_eq(max_value, 89)
  
  // 查找最小值
  let min1 = if value1 < value2 { value1 } else { value2 }
  let min2 = if min1 < value3 { min1 } else { value3 }
  let min3 = if min2 < value4 { min2 } else { value4 }
  let min_value = if min3 < value5 { min3 } else { value5 }
  
  @azimuth.assert_eq(min_value, 7)
}