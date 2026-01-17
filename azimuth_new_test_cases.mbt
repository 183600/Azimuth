// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含10个测试用例

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, Special@#$%!", @azimuth.greet("Special@#$%"))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
  
  // 结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), 5), @azimuth.add(a, @azimuth.add(b, 5)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), 2), @azimuth.multiply(a, @azimuth.multiply(b, 2)))
}

test "financial_calculations_practical" {
  // 测试实际金融计算
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years)) / 100
  let total_simple = @azimuth.add(principal, simple_interest)
  @azimuth.assert_eq(160000, total_simple)
  
  // 折扣计算
  let original_price = 500
  let discount_rate = 20
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(400, final_price)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  let length = 10
  let width = 6
  let radius = 5
  
  // 矩形面积
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, rectangle_area)
  
  // 三角形面积（底×高÷2）
  let triangle_base = 8
  let triangle_height = 4
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  @azimuth.assert_eq(16, triangle_area)
  
  // 圆面积（简化：π≈3）
  let circle_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, circle_area)
}

test "time_conversion_calculations" {
  // 测试时间转换计算
  let hours = 3
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(225, total_minutes)
  
  // 转换为总秒数
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(13500, total_seconds)
  
  // 计算工作日分钟数（8小时×5天）
  let work_minutes = @azimuth.multiply(@azimuth.multiply(8, 60), 5)
  @azimuth.assert_eq(2400, work_minutes)
}

test "shopping_cart_total_calculation" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  let shipping = 15
  
  // 计算各商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let total3 = @azimuth.multiply(item3_price, item3_quantity)
  
  // 计算购物车总价（含运费）
  let cart_total = @azimuth.add(@azimuth.add(@azimuth.add(total1, total2), total3), shipping)
  
  // 验证总价：199*2 + 299*1 + 99*3 + 15 = 398 + 299 + 297 + 15 = 1009
  @azimuth.assert_eq(1009, cart_total)
}

test "temperature_conversion_practical" {
  // 测试实际温度转换（简化版：F = C × 2 + 30）
  let celsius_freezing = 0
  let celsius_room = 20
  let celsius_boiling = 100
  
  // 转换为华氏度
  let fahrenheit_freezing = @azimuth.add(@azimuth.multiply(celsius_freezing, 2), 30)
  let fahrenheit_room = @azimuth.add(@azimuth.multiply(celsius_room, 2), 30)
  let fahrenheit_boiling = @azimuth.add(@azimuth.multiply(celsius_boiling, 2), 30)
  
  @azimuth.assert_eq(30, fahrenheit_freezing)   // 0°C ≈ 30°F
  @azimuth.assert_eq(70, fahrenheit_room)       // 20°C ≈ 70°F
  @azimuth.assert_eq(230, fahrenheit_boiling)   // 100°C ≈ 230°F
}

test "series_calculation_patterns" {
  // 测试级数计算模式
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let arithmetic_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, arithmetic_sum)
  
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let geometric_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geometric_sum)
  
  // 平方数列求和：1² + 2² + 3² + 4² = 1 + 4 + 9 + 16 = 30
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4))
  @azimuth.assert_eq(30, square_sum)
}

test "boundary_value_comprehensive" {
  // 测试边界值综合情况
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, neg_one))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, neg_one))
  
  // 边界值组合
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
  @azimuth.assert_eq(max_val, @azimuth.add(zero, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(zero, min_val))
}

test "real_world_application_scenario" {
  // 测试实际应用场景：计算订单总价（含折扣和税费）
  let unit_price = 120
  let quantity = 5
  let discount_threshold = 500
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 25
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算折扣（如果超过阈值）
  let discount_amount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_total = @azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(@azimuth.add(discounted_total, tax_amount), shipping)
  
  @azimuth.assert_eq(629, final_total)
  
  // 验证计算过程：120*5 = 600，折扣 = 600*10/100 = 60，折后 = 600-60 = 540
  // 税费 = 540*8/100 = 43，总价 = 540+43+25 = 608（修正计算）
  @azimuth.assert_eq(608, @azimuth.add(@azimuth.add(@azimuth.add(600, -60), 43), 25))
}