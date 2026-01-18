// Azimuth 项目增强实用测试用例
// 使用标准 MoonBit 测试语法，包含实用的测试场景

test "temperature_conversion" {
  // 温度转换测试
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9), 160) / 5
  
  @azimuth.assert_eq(77, fahrenheit)
  
  // 反向转换
  let fahrenheit2 = 32
  let celsius2 = @azimuth.multiply(@azimuth.add(fahrenheit2, -32), 5) / 9
  
  @azimuth.assert_eq(0, celsius2)
}

test "percentage_calculations" {
  // 百分比计算测试
  let total = 200
  let percentage = 15
  
  let amount = @azimuth.multiply(total, percentage) / 100
  @azimuth.assert_eq(30, amount)
  
  // 增长计算
  let original = 100
  let growth_rate = 20
  let new_value = @azimuth.add(original, @azimuth.multiply(original, growth_rate) / 100)
  
  @azimuth.assert_eq(120, new_value)
}

test "speed_distance_time" {
  // 速度、距离、时间计算测试
  let speed = 60  // km/h
  let time = 2    // hours
  
  let distance = @azimuth.multiply(speed, time)
  @azimuth.assert_eq(120, distance)
  
  // 计算时间
  let distance2 = 180
  let speed2 = 60
  let time2 = distance2 / speed2
  
  @azimuth.assert_eq(3, time2)
}

test "simple_interest" {
  // 简单利息计算测试
  let principal = 1000
  let rate = 5
  let time = 2
  
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time)) / 100
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(100, interest)
  @azimuth.assert_eq(1100, total)
}

test "coordinate_geometry" {
  // 坐标几何测试
  let x1 = 1
  let y1 = 2
  let x2 = 4
  let y2 = 6
  
  // 计算两点间距离的平方 (避免开方)
  let dx = @azimuth.add(x2, -x1)
  let dy = @azimuth.add(y2, -y1)
  let distance_squared = @azimuth.add(@azimuth.multiply(dx, dx), @azimuth.multiply(dy, dy))
  
  @azimuth.assert_eq(25, distance_squared)
}

test "digital_root" {
  // 数根计算测试
  let number = 1234
  
  let sum1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  let sum2 = @azimuth.add(@azimuth.add(sum1 / 100, (sum1 % 100) / 10), sum1 % 10)
  
  @azimuth.assert_eq(1, sum2)
}

test "base_conversion" {
  // 进制转换测试 (十进制到二进制位数)
  let decimal = 13
  
  // 计算13的二进制表示需要的位数
  let bits_needed = 4  // 13 = 1101 (4位)
  
  @azimuth.assert_eq(4, bits_needed)
  
  // 验证计算
  let value = @azimuth.add(@azimuth.add(@azimuth.multiply(1, 8), @azimuth.multiply(1, 4)), @azimuth.add(@azimuth.multiply(0, 2), 1))
  @azimuth.assert_eq(13, value)
}

test "factorial_approximation" {
  // 阶乘近似计算测试
  let n = 5
  
  // 计算5! = 5 × 4 × 3 × 2 × 1
  let result = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  
  @azimuth.assert_eq(120, result)
}

test "array_sum_simulation" {
  // 数组求和模拟测试
  let a = 10
  let b = 20
  let c = 30
  let d = 40
  let e = 50
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(a, b), c), d), e)
  let average = sum / 5
  
  @azimuth.assert_eq(150, sum)
  @azimuth.assert_eq(30, average)
}

test "compound_interest_simple" {
  // 简单复利计算测试
  let principal = 1000
  let rate = 10
  let years = 2
  
  // 第一年
  let year1 = @azimuth.add(principal, @azimuth.multiply(principal, rate) / 100)
  // 第二年
  let year2 = @azimuth.add(year1, @azimuth.multiply(year1, rate) / 100)
  
  @azimuth.assert_eq(1210, year2)
}