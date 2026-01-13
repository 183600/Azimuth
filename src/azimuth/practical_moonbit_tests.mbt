// 新增的 MoonBit 测试用例
// 覆盖一些实际应用场景

test "simple_arithmetic_sequence" {
  // 测试等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let total = @azimuth.add(sum3, 5)
  @azimuth.assert_eq(15, total)
}

test "geometric_progression" {
  // 测试等比数列：2, 4, 8, 16
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 2)
  let term3 = @azimuth.multiply(term2, 2)
  let term4 = @azimuth.multiply(term3, 2)
  @azimuth.assert_eq(16, term4)
}

test "price_calculation" {
  // 测试价格计算：商品价格 × 数量 + 税费
  let price = 199
  let quantity = 3
  let tax = 50
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(647, total)  // 199 * 3 + 50 = 597 + 50 = 647
}

test "area_perimeter_calculation" {
  // 测试面积和周长计算
  let length = 8
  let width = 6
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(48, area)      // 8 * 6 = 48
  @azimuth.assert_eq(28, perimeter) // 2 * (8 + 6) = 28
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // F = C × 2 + 30（近似值）
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)  // 20 * 2 + 30 = 70
}

test "string_greeting_variations" {
  // 测试不同的问候场景
  @azimuth.assert_eq_string(@azimuth.greet("Alice"), "Hello, Alice!")
  @azimuth.assert_eq_string(@azimuth.greet("Bob Smith"), "Hello, Bob Smith!")
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
}

test "compound_calculation" {
  // 测试复合计算：(a + b) × (c - d)
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  
  let sum_ab = @azimuth.add(a, b)      // 10 + 5 = 15
  let diff_cd = @azimuth.add(c, -d)    // 20 - 8 = 12
  let result = @azimuth.multiply(sum_ab, diff_cd)
  
  @azimuth.assert_eq(180, result)      // 15 * 12 = 180
}

test "average_calculation" {
  // 测试平均值计算
  let num1 = 85
  let num2 = 90
  let num3 = 95
  
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  let average = sum / 3
  
  @azimuth.assert_eq(90, average)      // (85 + 90 + 95) / 3 = 270 / 3 = 90
}

test "discount_and_tax_calculation" {
  // 测试折扣和税费计算
  let original_price = 1000
  let discount_rate = 10  // 10% 折扣
  let tax_rate = 8        // 8% 税费
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(100, discount_amount)   // 1000 * 10 / 100 = 100
  @azimuth.assert_eq(900, discounted_price)  // 1000 - 100 = 900
  @azimuth.assert_eq(72, tax_amount)         // 900 * 8 / 100 = 72
  @azimuth.assert_eq(972, final_price)       // 900 + 72 = 972
}

test "time_conversion" {
  // 测试时间转换：将小时、分钟、秒转换为总秒数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let hours_in_seconds = @azimuth.multiply(hours, 3600)
  let minutes_in_seconds = @azimuth.multiply(minutes, 60)
  let total_seconds = @azimuth.add(@azimuth.add(hours_in_seconds, minutes_in_seconds), seconds)
  
  @azimuth.assert_eq(9045, total_seconds)  // 2*3600 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
}