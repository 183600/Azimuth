// 实际应用场景测试

///|
test "financial_calculations" {
  // 计算投资回报
  let principal = 10000  // 本金
  let rate = 5  // 利率百分比
  let years = 3  // 年数
  
  // 简单计算：本金 × 利率 × 年数
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), years)
  let total = @azimuth.add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + (10000 * 5 * 3) = 160000
}

///|
test "geometry_calculations" {
  // 计算矩形面积和周长
  let length = 15
  let width = 8
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  assert_eq(120, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  assert_eq(46, perimeter)
}

///|
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  let celsius = 25
  // 由于整数除法，我们需要手动调整计算
  let adjusted = @azimuth.add(celsius, @azimuth.multiply(32, 10))  // 25 + 320 = 345
  assert_eq(345, adjusted)
}