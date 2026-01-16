// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含高级测试场景

test "add_sequence_fibonacci_style" {
  // 测试类似斐波那契数列的加法序列
  let a = 1
  let b = 1
  let c = @azimuth.add(a, b)  // 2
  let d = @azimuth.add(b, c)  // 3
  let e = @azimuth.add(c, d)  // 5
  @azimuth.assert_eq(2, c)
  @azimuth.assert_eq(3, d)
  @azimuth.assert_eq(5, e)
}

test "multiply_power_of_two" {
  // 测试2的幂次方乘法
  @azimuth.assert_eq(@azimuth.multiply(2, 2), 4)
  @azimuth.assert_eq(@azimuth.multiply(4, 2), 8)
  @azimuth.assert_eq(@azimuth.multiply(8, 2), 16)
  @azimuth.assert_eq(@azimuth.multiply(16, 2), 32)
}

test "add_temperature_conversion" {
  // 测试温度转换相关的加法计算
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let celsius_0 = 0
  let celsius_100 = 100
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 9), 160)  // 0*9/5 + 32 = 32
  let fahrenheit_100 = @azimuth.add(@azimuth.multiply(celsius_100, 9), 160)  // 100*9/5 + 32 = 212
  @azimuth.assert_eq(32, fahrenheit_0)
  @azimuth.assert_eq(212, fahrenheit_100)
}

test "multiply_area_calculations" {
  // 测试不同形状的面积计算
  // 正方形面积 = 边长 × 边长
  let square_side = 6
  let square_area = @azimuth.multiply(square_side, square_side)
  @azimuth.assert_eq(36, square_area)
  
  // 长方形面积 = 长 × 宽
  let rectangle_length = 8
  let rectangle_width = 5
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  @azimuth.assert_eq(40, rectangle_area)
}

test "greet_multilingual_names" {
  // 测试多语言名称的问候
  @azimuth.assert_eq_string(@azimuth.greet("张三"), "Hello, 张三!")
  @azimuth.assert_eq_string(@azimuth.greet("李四"), "Hello, 李四!")
  @azimuth.assert_eq_string(@azimuth.greet("José"), "Hello, José!")
  @azimuth.assert_eq_string(@azimuth.greet("Müller"), "Hello, Müller!")
}

test "add_time_calculations" {
  // 测试时间相关的加法计算
  // 计算总秒数: 小时×3600 + 分钟×60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  let total_seconds = @azimuth.add(
    @azimuth.multiply(hours, 3600),
    @azimuth.add(@azimuth.multiply(minutes, 60), seconds)
  )
  @azimuth.assert_eq(9045, total_seconds)
}

test "multiply_price_calculations" {
  // 测试价格计算
  // 计算折扣价: 原价 × 折扣率
  let original_price = 100
  let discount_rate = 80  // 80%
  let discounted_price = @azimuth.multiply(original_price, discount_rate)
  @azimuth.assert_eq(8000, discounted_price)  // 实际价格为 80.00
  
  // 计算税费: 价格 × 税率
  let tax_rate = 8  // 8%
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate)
  @azimuth.assert_eq(64000, tax_amount)  // 实际税费为 640.00
}

test "add_coordinate_system" {
  // 测试坐标系中的加法运算
  // 计算两点之间的向量差
  let x1 = 10
  let y1 = 15
  let x2 = 4
  let y2 = 7
  let delta_x = @azimuth.add(x1, -x2)  // 10 - 4 = 6
  let delta_y = @azimuth.add(y1, -y2)  // 15 - 7 = 8
  @azimuth.assert_eq(6, delta_x)
  @azimuth.assert_eq(8, delta_y)
}

test "complex_business_logic" {
  // 测试复合业务逻辑
  // 计算订单总价: (商品单价 × 数量 + 运费) × 折扣率
  let unit_price = 50
  let quantity = 3
  let shipping = 10
  let discount = 90  // 90%
  
  let subtotal = @azimuth.add(@azimuth.multiply(unit_price, quantity), shipping)  // 50*3 + 10 = 160
  let total = @azimuth.multiply(subtotal, discount)  // 160 * 0.9 = 144
  @azimuth.assert_eq(14400, total)  // 实际总价为 144.00
}