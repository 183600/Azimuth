// 新增的 MoonBit 测试用例
// 这些测试用例使用标准的 MoonBit 测试语法，覆盖 azimuth 模块的新场景

// 导入主模块中的函数
use azimuth

test "string_with_unicode_characters" {
  // 测试包含各种 Unicode 字符的字符串
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
  assert_eq_string("Hello, 💻!", greet("💻"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, русский!", greet("русский"))
}

test "arithmetic_progression_sum" {
  // 计算等差数列的和：2 + 5 + 8 + 11 + 14
  // 使用公式：n × (首项 + 末项) / 2
  let n = 5
  let first = 2
  let last = 14
  let common_diff = 3
  
  // 验证等差数列的性质
  assert_eq(5, add(first, multiply(common_diff, n - 1)))
  
  // 计算总和
  let sum = multiply(n, add(first, last)) / 2
  assert_eq(40, sum)
  
  // 手动验证
  let manual_sum = add(add(add(add(2, 5), 8), 11), 14)
  assert_eq(sum, manual_sum)
}

test "geometric_progression_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16
  let first = 2
  let ratio = 2
  let n = 4
  
  // 手动计算
  let term1 = first
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  
  let product = multiply(multiply(multiply(term1, term2), term3), term4)
  assert_eq(1024, product)
  
  // 验证每一项
  assert_eq(2, term1)
  assert_eq(4, term2)
  assert_eq(8, term3)
  assert_eq(16, term4)
}

test "complex_nested_calculations" {
  // 复杂嵌套计算测试
  let a = 10
  let b = 20
  let c = 5
  let d = 3
  
  // 计算：(a + b) × (c - d) + a × b - c × d
  let sum_ab = add(a, b)
  let diff_cd = add(c, -d)
  let product_ab_cd = multiply(sum_ab, diff_cd)
  let product_ab = multiply(a, b)
  let product_cd = multiply(c, d)
  
  let result = add(add(product_ab_cd, product_ab), -product_cd)
  assert_eq(265, result)
}

test "price_calculation_with_discounts" {
  // 带折扣的价格计算
  let original_price = 1000
  let discount1 = 10  // 第一级折扣 10%
  let discount2 = 5   // 第二级折扣 5%
  let tax_rate = 8    // 税率 8%
  
  // 计算第一级折扣后的价格
  let discount1_amount = multiply(original_price, discount1) / 100
  let price_after_discount1 = add(original_price, -discount1_amount)
  
  // 计算第二级折扣后的价格
  let discount2_amount = multiply(price_after_discount1, discount2) / 100
  let final_price_before_tax = add(price_after_discount1, -discount2_amount)
  
  // 计算含税价格
  let tax_amount = multiply(final_price_before_tax, tax_rate) / 100
  let final_price_with_tax = add(final_price_before_tax, tax_amount)
  
  assert_eq(864, final_price_with_tax)
}

test "temperature_conversion_extended" {
  // 扩展的温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 由于只有整数运算，使用近似值：F = C × 2 - C × 0.2 + 30
  // 进一步简化为：F = C × 2 + 30 - C/5
  
  let celsius_temps = [(-40), 0, 10, 20, 37, 100]
  let expected_fahrenheit = [-40, 30, 50, 70, 104, 230]
  
  // 测试-40°C（唯一一个摄氏度和华氏度相等的温度）
  let temp1 = -40
  let fahrenheit1 = add(add(multiply(temp1, 2), 30), add(temp1, -temp1 / 5))
  assert_eq(-40, fahrenheit1)
  
  // 测试0°C（冰点）
  let temp2 = 0
  let fahrenheit2 = add(multiply(temp2, 2), 30)
  assert_eq(30, fahrenheit2)
  
  // 测试100°C（沸点）
  let temp3 = 100
  let fahrenheit3 = add(add(multiply(temp3, 2), 30), add(temp3, -temp3 / 5))
  assert_eq(210, fahrenheit3)
}

test "speed_distance_time_calculations" {
  // 速度、距离、时间计算
  let speed1 = 60  // km/h
  let time1 = 2    // hours
  let distance1 = multiply(speed1, time1)
  assert_eq(120, distance1)
  
  let speed2 = 80  // km/h
  let distance2 = 200  // km
  let time2 = distance2 / speed2
  assert_eq(2, time2)
  
  let distance3 = 150  // km
  let time3 = 3    // hours
  let speed3 = distance3 / time3
  assert_eq(50, speed3)
  
  // 复合计算：计算平均速度
  let total_distance = add(distance1, distance2)
  let total_time = add(time1, time2)
  let average_speed = total_distance / total_time
  assert_eq(80, average_speed)
}

test "simple_interest_calculation" {
  // 单利计算测试
  let principal = 10000
  let rate = 5  // 5% 年利率
  let time = 3  // 3年
  
  // 计算利息：本金 × 利率 × 时间
  let interest = multiply(principal, multiply(rate, time))
  let total_amount = add(principal, interest)
  
  assert_eq(160000, total_amount)
  
  // 测试不同期限
  let time2 = 5  // 5年
  let interest2 = multiply(principal, multiply(rate, time2))
  let total_amount2 = add(principal, interest2)
  
  assert_eq(260000, total_amount2)
}

test "array_manipulation_simulation" {
  // 数组操作模拟（使用基本运算）
  // 模拟数组元素求和
  let element1 = 15
  let element2 = 25
  let element3 = 35
  let element4 = 45
  let element5 = 55
  
  let sum = add(add(add(add(element1, element2), element3), element4), element5)
  assert_eq(175, sum)
  
  // 模拟查找最大值
  let max1 = if element1 > element2 { element1 } else { element2 }
  let max2 = if max1 > element3 { max1 } else { element3 }
  let max3 = if max2 > element4 { max2 } else { element4 }
  let max4 = if max3 > element5 { max3 } else { element5 }
  
  assert_eq(55, max4)
  
  // 模拟查找最小值
  let min1 = if element1 < element2 { element1 } else { element2 }
  let min2 = if min1 < element3 { min1 } else { element3 }
  let min3 = if min2 < element4 { min2 } else { element4 }
  let min4 = if min3 < element5 { min3 } else { element5 }
  
  assert_eq(15, min4)
}

test "logical_expression_evaluation" {
  // 逻辑表达式评估测试
  let x = 10
  let y = 20
  let z = 30
  
  // 测试与运算
  let condition1 = x < y && y < z
  let result1 = if condition1 { 1 } else { 0 }
  assert_eq(1, result1)
  
  // 测试或运算
  let condition2 = x > y || y < z
  let result2 = if condition2 { 1 } else { 0 }
  assert_eq(1, result2)
  
  // 测试非运算
  let condition3 = !(x > y)
  let result3 = if condition3 { 1 } else { 0 }
  assert_eq(1, result3)
  
  // 复合逻辑表达式
  let complex_condition = (x < y && y < z) || (x > y && y > z)
  let complex_result = if complex_condition { 100 } else { 200 }
  assert_eq(100, complex_result)
}