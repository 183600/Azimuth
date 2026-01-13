// 简单的 MoonBit 测试用例

// 导入 azimuth 包中的函数
test "basic_addition" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-1, 1))
  @azimuth.assert_eq(-4, @azimuth.add(-2, -2))
}

// 测试2: 基础乘法
///|
test "basic_multiplication" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(4, @azimuth.multiply(-2, -2))
}

// 测试3: 字符串拼接
///|
test "basic_greeting" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

// 测试4: 边界值测试
///|
test "boundary_values" {
  // 接近最大值
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  // 接近最小值
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  // 乘法边界
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
}

// 测试5: 数学性质
///|
test "mathematical_properties" {
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(7, 3), @azimuth.add(3, 7))
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(4, 5), @azimuth.multiply(5, 4))
  // 加法单位元
  @azimuth.assert_eq(8, @azimuth.add(8, 0))
  // 乘法单位元
  @azimuth.assert_eq(8, @azimuth.multiply(8, 1))
}

// 测试6: 实际应用 - 购物计算
///|
test "shopping_calculation" {
  let price = 15
  let quantity = 3
  let total = @azimuth.multiply(price, quantity)
  @azimuth.assert_eq(45, total)
}

// 测试7: 实际应用 - 面积计算
///|
test "area_calculation" {
  let length = 10
  let width = 5
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
}

// 测试8: 实际应用 - 温度转换
///|
test "temperature_conversion" {
  // 摄氏度到华氏度：F = C * 9/5 + 32
  // 我们用乘法避免除法：F * 5 = C * 9 + 160
  let celsius = 10
  let fahrenheit_times_5 = @azimuth.add(@azimuth.multiply(celsius, 9), 160)
  @azimuth.assert_eq(250, fahrenheit_times_5)  // 10 * 9 + 160 = 250，所以是50华氏度
}

// 测试9: 实际应用 - 速度距离计算
///|
test "speed_distance_calculation" {
  // 距离 = 速度 × 时间
  let speed = 80  // km/h
  let time = 2    // 小时
  let distance = @azimuth.multiply(speed, time)
  @azimuth.assert_eq(160, distance)
}

// 测试10: 实际应用 - 简单利息计算
///|
test "simple_interest" {
  // 利息 = 本金 × 利率 × 时间
  let principal = 1000
  let rate = 5       // 5%
  let years = 2
  // 我们用乘法表示：利息 = 本金 × (利率 × 时间)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  @azimuth.assert_eq(10000, interest)  // 1000 * 5 * 2 = 10000，表示100元
}
