// Azimuth 项目实用测试用例集
// 使用标准 MoonBit 测试语法，专注于实际应用场景

test "financial_calculations" {
  // 金融计算测试：简单利息计算
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算简单利息：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 计算总金额：本金 + 利息
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(150000, total)  // 10000 + 10000*5*3 = 160000
}

test "temperature_conversion" {
  // 温度转换测试：摄氏度转华氏度（简化版）
  // 使用近似公式：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)  // 25 × 2 + 30 = 80
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
}

test "shopping_cart_total" {
  // 购物车总价计算测试
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 15
  
  // 计算商品小计
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal1, subtotal2), shipping)
  
  @azimuth.assert_eq(712, total)  // 199*2 + 299*1 + 15 = 398 + 299 + 15 = 712
}

test "area_perimeter_calculations" {
  // 面积和周长计算测试
  let length = 8
  let width = 5
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  // 计算矩形周长
  let perimeter = @azimuth.multiply(@azimuth.add(length, width), 2)
  
  @azimuth.assert_eq(40, area)      // 8 × 5 = 40
  @azimuth.assert_eq(26, perimeter) // (8 + 5) × 2 = 26
}

test "grade_average_calculation" {
  // 成绩平均分计算测试
  let grade1 = 85
  let grade2 = 92
  let grade3 = 78
  let grade4 = 90
  
  // 计算总分
  let total = @azimuth.add(@azimuth.add(@azimuth.add(grade1, grade2), grade3), grade4)
  // 计算平均分
  let average = total / 4
  
  @azimuth.assert_eq(345, total)    // 85 + 92 + 78 + 90 = 345
  @azimuth.assert_eq(86, average)   // 345 / 4 = 86.25 → 86（整数除法）
}

test "compound_interest_simple" {
  // 简化复利计算测试
  let principal = 5000
  let rate = 8
  let years = 2
  
  // 简化复利公式：最终金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let final_amount = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(85000, final_amount)  // 5000 × (1 + 8×2) = 5000 × 17 = 85000
}

test "speed_distance_time" {
  // 速度、距离、时间计算测试
  let speed = 60  // km/h
  let time = 2.5  // 小时（使用整数表示：25/10）
  
  // 计算距离：速度 × 时间
  // 使用整数计算：60 × 25 / 10 = 150
  let distance = @azimuth.multiply(speed, 25) / 10
  
  @azimuth.assert_eq(150, distance)  // 60 × 2.5 = 150 km
}

test "batch_processing" {
  // 批处理计算测试
  let batch_size = 24
  let items_per_batch = 5
  let leftover_items = 3
  
  // 计算总项目数
  let total_items = @azimuth.add(@azimuth.multiply(batch_size, items_per_batch), leftover_items)
  
  @azimuth.assert_eq(123, total_items)  // 24 × 5 + 3 = 120 + 3 = 123
}

test "greeting_function_tests" {
  // 问候函数测试
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "boundary_value_operations" {
  // 边界值运算测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}