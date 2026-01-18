// 用户新增的标准 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(21, 5))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(40, 6))
}

test "divide_with_ceil_negative_numbers" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-10, 4))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 100))
}

test "statistical_calculations" {
  // 测试统计计算场景
  // 计算平均值：(a + b + c + d) / 4
  let a = 85
  let b = 90
  let c = 78
  let d = 92
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(a, b), c), d)
  let average = sum / 4
  
  @azimuth.assert_eq(86, average)
}

test "financial_compound_interest" {
  // 测试金融复利计算
  // 本金 × (1 + 利率)^年数（简化为整数计算）
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简化计算：10000 × (1 + 5% × 3) = 10000 × 1.15 ≈ 11500
  let growth_factor = @azimuth.add(100, @azimuth.multiply(rate, years))
  let total = @azimuth.multiply(principal, growth_factor) / 100
  
  @azimuth.assert_eq(11500, total)
}

test "physics_kinematics" {
  // 测试物理运动学计算
  // 位移 = 初速度 × 时间 + 0.5 × 加速度 × 时间²
  // 简化为整数计算：s = v × t + a × t × t / 2
  let initial_velocity = 10  // m/s
  let acceleration = 2      // m/s²
  let time = 5              // s
  
  let distance1 = @azimuth.multiply(initial_velocity, time)
  let distance2 = @azimuth.multiply(acceleration, @azimuth.multiply(time, time)) / 2
  let total_distance = @azimuth.add(distance1, distance2)
  
  @azimuth.assert_eq(75, total_distance)  // 10×5 + 2×5×5/2 = 50 + 25 = 75
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  // 梯形面积 = (上底 + 下底) × 高 ÷ 2
  let top_base = 8
  let bottom_base = 12
  let height = 5
  
  let sum_bases = @azimuth.add(top_base, bottom_base)
  let trapezoid_area = @azimuth.multiply(sum_bases, height) / 2
  
  @azimuth.assert_eq(50, trapezoid_area)  // (8 + 12) × 5 ÷ 2 = 20 × 5 ÷ 2 = 50
}

test "combinatorial_calculations" {
  // 测试组合数学计算
  // 计算排列数 P(n, k) = n × (n-1) × ... × (n-k+1)
  // 简化计算：P(5, 3) = 5 × 4 × 3 = 60
  let n = 5
  let k = 3
  
  let p1 = @azimuth.multiply(n, n - 1)
  let p2 = @azimuth.multiply(p1, n - 2)
  
  @azimuth.assert_eq(60, p2)
}

test "temperature_conversion" {
  // 测试温度转换（摄氏度转华氏度）
  // 简化公式：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "array_index_calculations" {
  // 测试数组索引计算
  // 二维数组转一维数组的索引：index = row × width + col
  let row = 2
  let col = 3
  let width = 5
  
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  
  @azimuth.assert_eq(13, index)  // 2 × 5 + 3 = 13
}