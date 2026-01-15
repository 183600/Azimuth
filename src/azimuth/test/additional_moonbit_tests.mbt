// 额外的 MoonBit 测试用例
// 覆盖更多实际应用场景和边界情况

test "power_calculation" {
  // 测试幂运算：2^4 = 16
  let base = 2
  let exponent = 4
  let result = @azimuth.multiply(@azimuth.multiply(base, base), @azimuth.multiply(base, base))
  @azimuth.assert_eq(16, result)
}

test "factorial_small_number" {
  // 测试小数阶乘：4! = 4 × 3 × 2 × 1 = 24
  let n = 4
  let result = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, n - 3)))
  @azimuth.assert_eq(24, result)
}

test "average_of_three_numbers" {
  // 测试三个数的平均值
  let num1 = 15
  let num2 = 25
  let num3 = 35
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  // 计算平均值
  let average = sum / 3
  
  @azimuth.assert_eq(25, average)
}

test "discount_calculation" {
  // 测试折扣计算
  let original_price = 200
  let discount_percentage = 20  // 20% 折扣
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  // 计算折后价格
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(160, final_price)
}

test "volume_of_cuboid" {
  // 测试长方体体积计算
  let length = 5
  let width = 4
  let height = 3
  
  // 体积 = 长 × 宽 × 高
  let base_area = @azimuth.multiply(length, width)
  let volume = @azimuth.multiply(base_area, height)
  
  @azimuth.assert_eq(60, volume)
}

test "speed_distance_time" {
  // 测试速度、距离、时间关系
  let distance = 300  // 公里
  let time = 5        // 小时
  
  // 速度 = 距离 / 时间
  let speed = distance / time
  
  @azimuth.assert_eq(60, speed)
}

test "currency_exchange" {
  // 测试货币兑换
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY
  
  // 兑换为人民币
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  
  @azimuth.assert_eq(700, cny_amount)
}

test "percentage_change" {
  // 测试百分比变化
  let original_value = 150
  let new_value = 180
  
  // 计算变化量
  let change = @azimuth.add(new_value, -original_value)
  // 计算百分比变化
  let percentage_change = @azimuth.multiply(change, 100) / original_value
  
  @azimuth.assert_eq(20, percentage_change)  // (180-150)/150 * 100 = 20%
}

test "triangle_area_calculation" {
  // 测试三角形面积计算
  let base = 12
  let height = 8
  
  // 面积 = 底 × 高 ÷ 2
  let area = @azimuth.multiply(base, height) / 2
  
  @azimuth.assert_eq(48, area)
}

test "simple_interest_calculation" {
  // 测试简单利息计算
  let principal = 1000
  let rate = 5      // 5% 年利率
  let time = 3      // 3 年
  
  // 利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time)
  // 总金额 = 本金 + 利息
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(16000, total_amount)  // 1000 + 1000*5*3 = 16000
}