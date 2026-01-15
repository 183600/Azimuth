// 新增的 MoonBit 测试用例
// 这些测试用例涵盖了更多的实际应用场景和边界情况

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 1234567890!", @azimuth.greet("1234567890"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
}

test "mathematical_sequence_calculations" {
  // 测试数列计算
  
  // 计算等差数列的和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
  
  // 计算等比数列的和：2 + 4 + 8 + 16（前4项）
  // 由于没有指数运算，我们使用乘法
  let term1 = 2
  let term2 = @azimuth.multiply(2, 2)
  let term3 = @azimuth.multiply(2, 4)
  let term4 = @azimuth.multiply(2, 8)
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(30, geo_sum)
}

test "financial_calculations" {
  // 测试金融计算场景
  
  // 计算折扣价格：原价100元，折扣20%，最终价格
  let original_price = 100
  let discount_rate = 20
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(80, final_price)
  
  // 计算税费：价格200元，税率10%，总价格
  let price = 200
  let tax_rate = 10
  let tax_amount = @azimuth.multiply(price, tax_rate) / 100
  let total_price = @azimuth.add(price, tax_amount)
  @azimuth.assert_eq(220, total_price)
}

test "geometry_calculations" {
  // 测试几何计算
  
  // 计算矩形的面积和周长
  let length = 8
  let width = 5
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, area)
  @azimuth.assert_eq(26, perimeter)
  
  // 计算三角形的面积（使用海伦公式的简化版本）
  // 假设三角形三边为 3, 4, 5，这是一个直角三角形
  let a = 3
  let b = 4
  let c = 5
  // 直角三角形面积 = 底 × 高 / 2
  let triangle_area = @azimuth.multiply(a, b) / 2
  @azimuth.assert_eq(6, triangle_area)
}

test "temperature_conversion_calculations" {
  // 测试温度转换（简化版本）
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 由于只有整数运算，使用近似值：F = C × 2 + 30
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  // 由于只有整数运算，使用近似值：C = (F - 30) / 2
  let fahrenheit2 = 70
  let celsius2 = @azimuth.add(fahrenheit2, -30) / 2
  @azimuth.assert_eq(20, celsius2)
}

test "complex_business_logic" {
  // 测试复杂的业务逻辑
  
  // 计算订单总价：商品价格 × 数量，然后求和
  let item1_price = 50
  let item1_quantity = 3
  let item2_price = 30
  let item2_quantity = 2
  let item3_price = 20
  let item3_quantity = 4
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let total3 = @azimuth.multiply(item3_price, item3_quantity)
  
  let order_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  @azimuth.assert_eq(290, order_total)  // 50*3 + 30*2 + 20*4 = 150 + 60 + 80 = 290
  
  // 计算折扣后的价格：订单满200元减20元
  let discount_threshold = 200
  let discount_amount = 20
  let final_total = if order_total >= discount_threshold {
    @azimuth.add(order_total, -discount_amount)
  } else {
    order_total
  }
  @azimuth.assert_eq(270, final_total)
}

test "number_system_conversions" {
  // 测试数字系统转换（简化版本）
  
  // 二进制转十进制：1010 = 1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  // 由于没有指数运算，我们使用乘法模拟
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 0
  
  let decimal_value = @azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(bit3, 8),  // 1 × 8
    @azimuth.multiply(bit2, 4)   // 0 × 4
  ), @azimuth.multiply(bit1, 2)), // 1 × 2
    @azimuth.multiply(bit0, 1)    // 0 × 1
  )
  @azimuth.assert_eq(10, decimal_value)
  
  // 十进制转二进制验证：10 = 1010（通过位运算验证）
  let decimal = 10
  let binary_1010 = @azimuth.add(
    @azimuth.multiply(1, 8),  // 1 × 2³
    @azimuth.multiply(0, 4),  // 0 × 2²
    @azimuth.multiply(1, 2),  // 1 × 2¹
    @azimuth.multiply(0, 1)   // 0 × 2⁰
  )
  @azimuth.assert_eq(decimal, binary_1010)
}

test "statistical_calculations" {
  // 测试统计计算
  
  // 计算平均值
  let numbers = [10, 20, 30, 40, 50]
  // 由于没有数组，我们手动计算
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(30, average)
  
  // 计算方差（简化版本）
  // 方差 = (每个数与平均值的差的平方和) / 数量
  let mean = 30
  let diff1 = @azimuth.add(10, -mean)  // -20
  let diff2 = @azimuth.add(20, -mean)  // -10
  let diff3 = @azimuth.add(30, -mean)  // 0
  let diff4 = @azimuth.add(40, -mean)  // 10
  let diff5 = @azimuth.add(50, -mean)  // 20
  
  let square1 = @azimuth.multiply(diff1, diff1)
  let square2 = @azimuth.multiply(diff2, diff2)
  let square3 = @azimuth.multiply(diff3, diff3)
  let square4 = @azimuth.multiply(diff4, diff4)
  let square5 = @azimuth.multiply(diff5, diff5)
  
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(square1, square2), square3), square4), square5)
  let variance = sum_of_squares / count
  @azimuth.assert_eq(200, variance)  // (400 + 100 + 0 + 100 + 400) / 5 = 1000 / 5 = 200
}

test "time_calculations" {
  // 测试时间计算
  
  // 计算总秒数：2小时30分钟15秒 = 2×3600 + 30×60 + 15
  let hours = 2
  let minutes = 30
  let seconds = 15
  
  let total_seconds = @azimuth.add(@azimuth.add(
    @azimuth.multiply(hours, 3600),     // 2 × 3600 = 7200
    @azimuth.multiply(minutes, 60)      // 30 × 60 = 1800
  ), seconds)                           // + 15
  @azimuth.assert_eq(9015, total_seconds)
  
  // 计算天数：9015秒 = 9015 ÷ 86400 = 0天余9015秒
  let total_seconds2 = 9015
  let seconds_per_day = 86400
  let days = total_seconds2 / seconds_per_day
  let remaining_seconds = @azimuth.add(total_seconds2, -@azimuth.multiply(days, seconds_per_day))
  @azimuth.assert_eq(0, days)
  @azimuth.assert_eq(9015, remaining_seconds)
}