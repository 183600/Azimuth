// 实际应用场景测试 - 补充现有测试的覆盖范围
// 专注于实际应用中可能遇到的场景

///|
test "financial_calculations" {
  // 测试金融计算场景
  assert_eq(100, @azimuth.add(50, 50))  // 50 + 50 = 100
  assert_eq(95, @azimuth.add(100, -5))  // 100 - 5 = 95
  assert_eq(200, @azimuth.multiply(20, 10))  // 20 * 10 = 200
  assert_eq(-50, @azimuth.multiply(10, -5))  // 10 * -5 = -50
  
  // 利息计算：本金1000，利率5%，2年
  let principal = 1000
  let interest_rate = 50  // 5%表示为50/1000
  let years = 2
  let simple_interest = @azimuth.multiply(multiply(principal, interest_rate), years) / 1000
  assert_eq(100, simple_interest)  // 简单利息应该是100
}

///|
test "temperature_conversion" {
  // 测试温度转换相关的计算
  // 摄氏度到华氏度：F = C * 9/5 + 32
  let celsius = 100
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  assert_eq(212, fahrenheit)  // 100°C = 212°F
  
  // 华氏度到摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 32
  let celsius2 = @azimuth.multiply(@azimuth.add(fahrenheit2, -32), 5) / 9
  assert_eq(0, celsius2)  // 32°F = 0°C
}

///|
test "coordinate_geometry" {
  // 测试坐标几何计算
  // 两点距离的平方：(x2-x1)² + (y2-y1)²
  let x1 = 3
  let y1 = 4
  let x2 = 0
  let y2 = 0
  let dx = @azimuth.add(x2, -x1)  // x2 - x1
  let dy = @azimuth.add(y2, -y1)  // y2 - y1
  let distance_squared = @azimuth.add(@azimuth.multiply(dx, dx), @azimuth.multiply(dy, dy))
  assert_eq(25, distance_squared)  // 3-4-5三角形，距离平方应该是25
  
  // 中点坐标：((x1+x2)/2, (y1+y2)/2)
  let midpoint_x = @azimuth.add(x1, x2) / 2
  let midpoint_y = @azimuth.add(y1, y2) / 2
  assert_eq(1, midpoint_x)  // (3+0)/2 = 1.5，整数除法为1
  assert_eq(2, midpoint_y)  // (4+0)/2 = 2
}

///|
test "array_index_calculations" {
  // 测试数组索引计算
  let array_size = 10
  let row = 2
  let col = 3
  
  // 二维数组转一维数组的索引：index = row * width + col
  let width = 5
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  assert_eq(13, index)  // 2 * 5 + 3 = 13
  
  // 边界检查
  let last_index = @azimuth.add(array_size, -1)  // 最后一个元素的索引
  assert_eq(9, last_index)
  
  // 中间索引
  let middle_index = array_size / 2
  assert_eq(5, middle_index)
}

///|
test "time_calculations" {
  // 测试时间计算
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  // 转换为总秒数
  let total_seconds = @azimuth.add(@azimuth.multiply(hours, 3600), 
                                   @azimuth.add(@azimuth.multiply(minutes, 60), seconds))
  assert_eq(9045, total_seconds)  // 2*3600 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
  
  // 12小时制转换为24小时制
  let hour_12 = 11
  let is_pm = 1  // 1表示PM，0表示AM
  let hour_24 = @azimuth.add(hour_12, @azimuth.multiply(is_pm, 12))
  assert_eq(23, hour_24)  // 11 PM = 23:00
}

///|
test "statistical_calculations" {
  // 测试统计计算
  let values = [10, 20, 30, 40, 50]
  
  // 计算总和
  let sum = @azimuth.add(add(add(add(values[0], values[1]), values[2]), values[3]), values[4])
  assert_eq(150, sum)  // 10+20+30+40+50 = 150
  
  // 计算平均值（整数除法）
  let count = 5
  let average = sum / count
  assert_eq(30, average)  // 150/5 = 30
  
  // 计算方差（简化版）
  let mean = 30
  let diff1 = @azimuth.add(values[0], -mean)  // 10-30 = -20
  let diff2 = @azimuth.add(values[1], -mean)  // 20-30 = -10
  let variance = (@azimuth.multiply(diff1, diff1) + @azimuth.multiply(diff2, diff2)) / count
  assert_eq(100, variance)  // ((-20)² + (-10)²)/5 = (400+100)/5 = 500/5 = 100
}

///|
test "greet_multilingual_scenarios" {
  // 测试多语言问候场景
  assert_eq("Hello, 世界!", @azimuth.greet("世界"))  // 中文
  assert_eq("Hello, Mundo!", @azimuth.greet("Mundo"))  // 西班牙语
  assert_eq("Hello, Monde!", @azimuth.greet("Monde"))  // 法语
  assert_eq("Hello, 世界!", @azimuth.greet("世界"))  // 中文
  assert_eq("Hello, 🌍!", @azimuth.greet("🌍"))  // 地球emoji
  assert_eq("Hello, John Doe!", @azimuth.greet("John Doe"))  // 英文全名
  assert_eq("Hello, a_b_c!", @azimuth.greet("a_b_c"))  // 包含下划线
  assert_eq("Hello, 123.45!", @azimuth.greet("123.45"))  // 小数
}

///|
test "extreme_boundary_combinations" {
  // 测试极端边界值的组合
  let max = 2147483647
  let min = -2147483648
  
  // 链式运算测试
  let result1 = @azimuth.add(add(1, 2), @azimuth.add(3, 4))  // (1+2)+(3+4) = 3+7 = 10
  assert_eq(10, result1)
  
  let result2 = @azimuth.multiply(multiply(2, 3), @azimuth.multiply(4, 5))  // (2*3)*(4*5) = 6*20 = 120
  assert_eq(120, result2)
  
  // 混合运算
  let result3 = @azimuth.add(@azimuth.multiply(10, 5), @azimuth.multiply(20, 3))  // 10*5 + 20*3 = 50 + 60 = 110
  assert_eq(110, result3)
  
  // 边界值的混合运算
  let result4 = @azimuth.add(@azimuth.multiply(max, 0), @azimuth.multiply(min, 0))  // max*0 + min*0 = 0 + 0 = 0
  assert_eq(0, result4)
}

///|
test "number_sequence_patterns" {
  // 测试数字序列模式
  // 等差数列：1, 3, 5, 7, 9...
  let first_term = 1
  let common_diff = 2
  let term_5 = @azimuth.add(first_term, @azimuth.multiply(common_diff, 4))  // 第5项 = 1 + 2*4 = 9
  assert_eq(9, term_5)
  
  // 等比数列：2, 4, 8, 16, 32...
  let first_geo = 2
  let common_ratio = 2
  let geo_term_5 = multiply(first_geo, multiply(common_ratio, 
                   @azimuth.multiply(common_ratio, multiply(common_ratio, common_ratio))))
  assert_eq(32, geo_term_5)  // 2 * 2^4 = 2 * 16 = 32
  
  // 斐波那契数列（简化版）：F(n) = F(n-1) + F(n-2)
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  assert_eq(5, fib5)
}