// 新的 MoonBit 测试用例
// 这些测试用例补充现有的测试，覆盖更多实际应用场景

// 测试用例1: 幂运算模拟
test "power_calculation" {
  // 计算 2^4 = 2 * 2 * 2 * 2 = 16
  let base = 2
  let result1 = @azimuth.multiply(base, base)  // 2 * 2 = 4
  let result2 = @azimuth.multiply(result1, base)  // 4 * 2 = 8
  let final_result = @azimuth.multiply(result2, base)  // 8 * 2 = 16
  @azimuth.assert_eq(16, final_result)
}

// 测试用例2: 阶乘计算
test "factorial_calculation" {
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let n = 5
  let step1 = @azimuth.multiply(n, 4)  // 5 * 4 = 20
  let step2 = @azimuth.multiply(step1, 3)  // 20 * 3 = 60
  let step3 = @azimuth.multiply(step2, 2)  // 60 * 2 = 120
  let final_result = @azimuth.multiply(step3, 1)  // 120 * 1 = 120
  @azimuth.assert_eq(120, final_result)
}

// 测试用例3: 百分比计算
test "percentage_calculation" {
  // 计算 200 的 25% = 200 * 25 / 100 = 50
  let value = 200
  let percentage = 25
  let intermediate = @azimuth.multiply(value, percentage)  // 200 * 25 = 5000
  let result = intermediate / 100  // 5000 / 100 = 50
  @azimuth.assert_eq(50, result)
}

// 测试用例4: 距离单位转换
test "distance_conversion" {
  // 公里转米：1公里 = 1000米
  let kilometers = 5
  let meters = @azimuth.multiply(kilometers, 1000)  // 5 * 1000 = 5000
  @azimuth.assert_eq(5000, meters)
}

// 测试用例5: 时间计算
test "time_calculation" {
  // 计算总秒数：2小时30分钟15秒 = 2*3600 + 30*60 + 15 = 7200 + 1800 + 15 = 9015
  let hours = 2
  let minutes = 30
  let seconds = 15
  let seconds_from_hours = @azimuth.multiply(hours, 3600)  // 2 * 3600 = 7200
  let seconds_from_minutes = @azimuth.multiply(minutes, 60)  // 30 * 60 = 1800
  let total_seconds = @azimuth.add(@azimuth.add(seconds_from_hours, seconds_from_minutes), seconds)  // 7200 + 1800 + 15 = 9015
  @azimuth.assert_eq(9015, total_seconds)
}

// 测试用例6: 字符串比较
test "string_comparison" {
  let short_name = "Tom"
  let long_name = "Alexander"
  let short_greeting = @azimuth.greet(short_name)  // "Hello, Tom!"
  let long_greeting = @azimuth.greet(long_name)  // "Hello, Alexander!"
  @azimuth.assert_false(short_greeting == long_greeting)
}

// 测试用例7: 数列求和
test "arithmetic_series_sum" {
  // 计算等差数列 1, 3, 5, 7, 9 的和
  // 使用公式：和 = (首项 + 末项) * 项数 / 2
  let first_term = 1
  let last_term = 9
  let number_of_terms = 5
  let sum_of_terms = @azimuth.add(first_term, last_term)  // 1 + 9 = 10
  let product = @azimuth.multiply(sum_of_terms, number_of_terms)  // 10 * 5 = 50
  let series_sum = product / 2  // 50 / 2 = 25
  @azimuth.assert_eq(25, series_sum)
}

// 测试用例8: 货币计算
test "currency_calculation" {
  // 计算总价：商品价格 × 数量 + 税费
  let item_price = 199  // 商品价格（分为单位）
  let quantity = 3  // 数量
  let tax_rate = 10  // 税率（百分比）
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)  // 199 * 3 = 597
  // 计算税费
  let tax_amount = subtotal / tax_rate  // 597 / 10 = 59（整数除法）
  // 计算最终总价
  let total_price = @azimuth.add(subtotal, tax_amount)  // 597 + 59 = 656
  @azimuth.assert_eq(656, total_price)
}

// 测试用例9: 几何图形周长计算
test "geometric_perimeter_calculation" {
  // 计算正六边形周长：6 × 边长
  let hexagon_side = 7
  let hexagon_perimeter = @azimuth.multiply(6, hexagon_side)  // 6 * 7 = 42
  @azimuth.assert_eq(42, hexagon_perimeter)
}

// 测试用例10: 数值范围检查
test "numeric_range_validation" {
  // 检查数值是否在特定范围内
  let value = 75
  let min_range = 10
  let max_range = 100
  // 检查 value >= min_range
  let above_min = value >= min_range
  @azimuth.assert_true(above_min)
  // 检查 value <= max_range
  let below_max = value <= max_range
  @azimuth.assert_true(below_max)
}