// 新增 MoonBit 标准测试用例

test "add_with_large_positive_numbers" {
  // 测试大正数相加
  @azimuth.assert_eq(@azimuth.add(1000000000, 1000000000), 2000000000)
  @azimuth.assert_eq(@azimuth.add(1500000000, 1500000000), 3000000000)
}

test "add_with_large_negative_numbers" {
  // 测试大负数相加
  @azimuth.assert_eq(@azimuth.add(-1000000000, -1000000000), -2000000000)
  @azimuth.assert_eq(@azimuth.add(-1500000000, -1500000000), -3000000000)
}

test "add_with_mixed_large_numbers" {
  // 测试大数混合符号相加
  @azimuth.assert_eq(@azimuth.add(2000000000, -1000000000), 1000000000)
  @azimuth.assert_eq(@azimuth.add(-2000000000, 1000000000), -1000000000)
}

test "multiply_with_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(@azimuth.multiply(100000, 10000), 1000000000)
  @azimuth.assert_eq(@azimuth.multiply(50000, 20000), 1000000000)
}

test "multiply_with_negative_large_numbers" {
  // 测试大负数相乘
  @azimuth.assert_eq(@azimuth.multiply(-100000, 10000), -1000000000)
  @azimuth.assert_eq(@azimuth.multiply(100000, -10000), -1000000000)
  @azimuth.assert_eq(@azimuth.multiply(-100000, -10000), 1000000000)
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列：计算 (a + b) * (c + d)
  let a = 100
  let b = 200
  let c = 300
  let d = 400
  
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let result = @azimuth.multiply(sum1, sum2)
  
  @azimuth.assert_eq(result, 300000)  // (100+200)*(300+400) = 300*700 = 210000
}

test "financial_compound_interest" {
  // 测试金融复利计算（简化版）
  let principal = 10000  // 本金
  let rate = 5          // 利率（%）
  let years = 3         // 年数
  
  // 计算每年的利息：本金 * 利率
  let yearly_interest = @azimuth.multiply(principal, rate)
  // 计算总利息：年利息 * 年数
  let total_interest = @azimuth.multiply(yearly_interest, years)
  // 计算本息和：本金 + 总利息
  let total_amount = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(total_amount, 160000)  // 10000 + 10000*5*3 = 10000 + 150000 = 160000
}

test "string_with_various_special_characters" {
  // 测试各种特殊字符的字符串处理
  @azimuth.assert_eq_string(@azimuth.greet("John_Doe-123"), "Hello, John_Doe-123!")
  @azimuth.assert_eq_string(@azimuth.greet("user@domain.com"), "Hello, user@domain.com!")
  @azimuth.assert_eq_string(@azimuth.greet("path/to/file"), "Hello, path/to/file!")
  @azimuth.assert_eq_string(@azimuth.greet("API_KEY=secret"), "Hello, API_KEY=secret!")
}

test "multi_step_calculation_scenario" {
  // 测试多步骤计算场景：计算矩形房间的总墙面面积
  let room_length = 5    // 房间长度（米）
  let room_width = 4     // 房间宽度（米）
  let room_height = 3    // 房间高度（米）
  
  // 计算房间的周长：2 * (长度 + 宽度)
  let perimeter = @azimuth.multiply(2, @azimuth.add(room_length, room_width))
  // 计算总墙面面积：周长 * 高度
  let wall_area = @azimuth.multiply(perimeter, room_height)
  
  @azimuth.assert_eq(wall_area, 54)  // 2*(5+4)*3 = 2*9*3 = 54
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景（简化版）
  // 使用近似公式：F = C * 2 + 30
  
  // 测试常见温度点
  let celsius_0 = 0      // 冰点
  let celsius_25 = 25    // 室温
  let celsius_100 = 100  // 沸点
  
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 2), 30)
  let fahrenheit_25 = @azimuth.add(@azimuth.multiply(celsius_25, 2), 30)
  let fahrenheit_100 = @azimuth.add(@azimuth.multiply(celsius_100, 2), 30)
  
  @azimuth.assert_eq(fahrenheit_0, 30)      // 0*2+30 = 30
  @azimuth.assert_eq(fahrenheit_25, 80)     // 25*2+30 = 80
  @azimuth.assert_eq(fahrenheit_100, 230)   // 100*2+30 = 230
}