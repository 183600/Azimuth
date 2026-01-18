// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供额外的测试覆盖

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(10, @azimuth.multiply(-5, -2))
  @azimuth.assert_eq(-15, @azimuth.multiply(-5, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "zero_operations" {
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(-5, @azimuth.add(0, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
}

test "large_number_operations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000, @azimuth.multiply(1000, 2000))
  
  let large_num3 = 100000
  let large_num4 = 10000
  @azimuth.assert_eq(1000000000, @azimuth.multiply(large_num3, large_num4))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  let result1 = @azimuth.greet("A")
  @azimuth.assert_eq_string("Hello, A!", result1)
  
  let result2 = @azimuth.greet(" ")
  @azimuth.assert_eq_string("Hello,  !", result2)
  
  let result3 = @azimuth.greet("VeryLongNameThatExceedsNormalLengthExpectations")
  @azimuth.assert_eq_string("Hello, VeryLongNameThatExceedsNormalLengthExpectations!", result3)
}

test "decimal_simulation" {
  // 模拟小数运算（使用整数表示）
  // 将 3.5 表示为 35，0.1 表示为 1，然后除以 10
  let num1 = 35  // 3.5
  let num2 = 20  // 2.0
  
  let sum = @azimuth.add(num1, num2)  // 55，代表 5.5
  @azimuth.assert_eq(55, sum)
  
  let product = @azimuth.multiply(num1, num2)  // 700，代表 70.0
  @azimuth.assert_eq(700, product)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000  // 本金
  let rate = 5  // 利率 5%
  let years = 3  // 年数
  
  // 简单利息计算: principal * rate * years / 100
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), years) / 100
  @azimuth.assert_eq(1500, interest)
  
  // 总金额
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(11500, total)
}

test "physics_calculations" {
  // 测试物理计算场景
  // 计算速度: distance / time
  let distance = 300  // 米
  let time = 10  // 秒
  let velocity = distance / time
  @azimuth.assert_eq(30, velocity)
  
  // 计算加速度: (final_velocity - initial_velocity) / time
  let initial_velocity = 10
  let final_velocity = 50
  let acceleration = @azimuth.add(final_velocity, -initial_velocity) / time
  @azimuth.assert_eq(4, acceleration)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值: (sum of values) / count
  let values_sum = @azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40)  // 10+20+30+40
  let count = 4
  let average = values_sum / count
  @azimuth.assert_eq(25, average)
  
  // 计算加权平均
  let weighted_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(10, 1), @azimuth.multiply(20, 2)), @azimuth.multiply(30, 3))
  let total_weight = @azimuth.add(@azimuth.add(1, 2), 3)
  let weighted_average = weighted_sum / total_weight
  @azimuth.assert_eq(23, weighted_average)  // (10+40+90)/6 = 140/6 = 23.33 -> 23
}

test "time_calculations" {
  // 测试时间计算
  // 计算总秒数: hours * 3600 + minutes * 60 + seconds
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = @azimuth.add(@azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.multiply(minutes, 60)), seconds)
  @azimuth.assert_eq(9045, total_seconds)
  
  // 计算时间差
  let start_time = 480  // 8:00 AM in minutes
  let end_time = 1020   // 5:00 PM in minutes
  let duration = @azimuth.add(end_time, -start_time)
  @azimuth.assert_eq(540, duration)  // 9 hours
}

test "complex_expressions" {
  // 测试复杂表达式
  let a = 5
  let b = 3
  let c = 2
  let d = 4
  
  // 计算 (a + b) * (c + d)
  let expr1 = @azimuth.multiply(@azimuth.add(a, b), @azimuth.add(c, d))
  @azimuth.assert_eq(56, expr1)  // (5+3)*(2+4) = 8*6 = 48
  
  // 计算 a * b + c * d
  let expr2 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(c, d))
  @azimuth.assert_eq(23, expr2)  // 5*3 + 2*4 = 15 + 8 = 23
  
  // 计算 (a * b) + (a * c) + (a * d)
  let expr3 = @azimuth.add(@azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)), @azimuth.multiply(a, d))
  @azimuth.assert_eq(45, expr3)  // 5*3 + 5*2 + 5*4 = 15 + 10 + 20 = 45
}