// MoonBit 标准测试用例示例

test "arithmetic_sequence_calculation" {
  // 测试等差数列求和：1 + 3 + 5 + ... + 19（前10个奇数）
  let first_term = 1
  let common_difference = 2
  let n = 10
  
  // 等差数列求和公式：Sn = n/2 * (2a1 + (n-1)d)
  // 使用整数运算：Sn = n * a1 + n*(n-1)/2 * d
  let term1 = @azimuth.multiply(n, first_term)
  let term2 = @azimuth.multiply(@azimuth.multiply(n, @azimuth.add(n, -1)) / 2, common_difference)
  let sum = @azimuth.add(term1, term2)
  
  @azimuth.assert_eq(sum, 100)  // 前10个奇数的和是100
}

test "geometric_sequence_calculation" {
  // 测试等比数列求和：2 + 4 + 8 + 16（前4项）
  let first_term = 2
  let ratio = 2
  let n = 4
  
  // 手动计算等比数列和
  let term1 = first_term
  let term2 = @azimuth.multiply(term1, ratio)
  let term3 = @azimuth.multiply(term2, ratio)
  let term4 = @azimuth.multiply(term3, ratio)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(sum, 30)  // 2 + 4 + 8 + 16 = 30
}

test "percentage_calculation" {
  // 测试百分比计算
  let original_price = 200
  let discount_percentage = 15  // 15%
  
  // 计算折扣金额：原价 * 折扣百分比 / 100
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  // 计算最终价格：原价 - 折扣金额
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(discount_amount, 30)  // 200 * 15 / 100 = 30
  @azimuth.assert_eq(final_price, 170)     // 200 - 30 = 170
}

test "time_conversion" {
  // 测试时间单位转换
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 将总时间转换为秒数
  let total_seconds = @azimuth.add(
    @azimuth.add(
      @azimuth.multiply(hours, 3600),  // 小时转秒
      @azimuth.multiply(minutes, 60)   // 分钟转秒
    ),
    seconds
  )
  
  @azimuth.assert_eq(total_seconds, 9930)  // 2*3600 + 45*60 + 30 = 7200 + 2700 + 30 = 9930
}

test "string_greeting_variations" {
  // 测试不同类型的问候字符串
  @azimuth.assert_eq_string(@azimuth.greet("Alice"), "Hello, Alice!")
  @azimuth.assert_eq_string(@azimuth.greet("Bob Smith"), "Hello, Bob Smith!")
  @azimuth.assert_eq_string(@azimuth.greet("用户123"), "Hello, 用户123!")
  @azimuth.assert_eq_string(@azimuth.greet("   "), "Hello,    !")
  @azimuth.assert_eq_string(@azimuth.greet("!@#$%"), "Hello, !@#$%!")
}

test "compound_interest_simple" {
  // 测试简化的复利计算（整数版本）
  let principal = 1000
  let annual_rate = 5  // 5%
  let years = 3
  
  // 简化计算：每年利息 = 本金 * 利率率
  let yearly_interest = @azimuth.multiply(principal, annual_rate) / 100
  // 总利息 = 年利息 * 年数
  let total_interest = @azimuth.multiply(yearly_interest, years)
  // 最终金额 = 本金 + 总利息
  let final_amount = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(final_amount, 1150)  // 1000 + 1000*5/100*3 = 1000 + 150 = 1150
}

test "volume_calculation" {
  // 测试体积计算
  // 长方体体积 = 长 × 宽 × 高
  let length = 5
  let width = 4
  let height = 3
  
  let base_area = @azimuth.multiply(length, width)
  let volume = @azimuth.multiply(base_area, height)
  
  @azimuth.assert_eq(volume, 60)  // 5 * 4 * 3 = 60
  
  // 圆柱体体积（简化为整数）：底面积 × 高
  let radius = 3
  let cylinder_height = 10
  // 使用π的近似值3
  let base_area_circle = @azimuth.multiply(@azimuth.multiply(radius, radius), 3)
  let cylinder_volume = @azimuth.multiply(base_area_circle, cylinder_height)
  
  @azimuth.assert_eq(cylinder_volume, 270)  // 3*3*3 * 10 = 27 * 10 = 270
}

test "speed_distance_time" {
  // 测试速度、距离、时间的关系
  let speed = 60  // km/h
  let time = 2.5  // 小时，使用整数近似：2.5 ≈ 5/2
  
  // 距离 = 速度 × 时间
  // 使用整数计算：60 * 5 / 2 = 150
  let distance = @azimuth.multiply(speed, 5) / 2
  
  @azimuth.assert_eq(distance, 150)  // 60 * 2.5 = 150 km
  
  // 反向计算：时间 = 距离 / 速度
  let calculated_time = @azimuth.multiply(distance, 2) / speed
  @azimuth.assert_eq(calculated_time, 5)  // 150 * 2 / 60 = 5 (表示2.5小时)
}

test "array_index_simulation" {
  // 模拟数组索引计算（一维数组转二维索引）
  let row_count = 4
  let column_count = 5
  let linear_index = 7  // 从0开始的线性索引
  
  // 计算行索引和列索引
  let row_index = linear_index / column_count
  let column_index = @azimuth.add(linear_index, @azimuth.multiply(-row_index, column_count))
  
  @azimuth.assert_eq(row_index, 1)      // 第7个元素在第1行（从0开始）
  @azimuth.assert_eq(column_index, 2)   // 第7个元素在第2列（从0开始）
  
  // 验证：线性索引 = 行索引 * 列数 + 列索引
  let verified_index = @azimuth.add(@azimuth.multiply(row_index, column_count), column_index)
  @azimuth.assert_eq(verified_index, linear_index)
}

test "temperature_conversion_detailed" {
  // 详细的温度转换测试
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 使用整数近似：F = C * 2 + 30（简化版）
  
  // 常见温度点测试
  let celsius_values = [0, 10, 20, 25, 30, 37, 100]
  let expected_fahrenheit = [30, 50, 70, 80, 90, 104, 230]
  
  // 测试几个关键温度点
  @azimuth.assert_eq(@azimuth.add(@azimuth.multiply(0, 2), 30), 30)    // 0°C ≈ 30°F
  @azimuth.assert_eq(@azimuth.add(@azimuth.multiply(25, 2), 30), 80)  // 25°C ≈ 80°F
  @azimuth.assert_eq(@azimuth.add(@azimuth.multiply(100, 2), 30), 230) // 100°C ≈ 230°F
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) / 2
  let fahrenheit = 80
  let celsius = @azimuth.add(fahrenheit, -30) / 2
  @azimuth.assert_eq(celsius, 25)  // (80 - 30) / 2 = 25
}