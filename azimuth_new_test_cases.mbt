// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "integer_addition_edge_cases" {
  // 整数加法边界值测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
}

test "integer_multiplication_edge_cases" {
  // 整数乘法边界值测试
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(100, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
}

test "complex_arithmetic_expressions" {
  // 复杂算术表达式测试
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  @azimuth.assert_eq(16, @azimuth.multiply(@azimuth.add(a, b), c))
  
  // a * b + c
  @azimuth.assert_eq(17, @azimuth.add(@azimuth.multiply(a, b), c))
  
  // (a - b) * (a + b)
  @azimuth.assert_eq(16, @azimuth.multiply(@azimuth.add(a, -b), @azimuth.add(a, b)))
}

test "string_operations_advanced" {
  // 高级字符串操作测试
  let prefix = "Azimuth"
  let suffix = "Project"
  let space = " "
  
  @azimuth.assert_eq_string("Azimuth Project", @azimuth.add(@azimuth.add(prefix, space), suffix))
  @azimuth.assert_eq_string("AzimuthAzimuth", @azimuth.add(prefix, prefix))
  @azimuth.assert_eq_string("ProjectProject", @azimuth.add(suffix, suffix))
}

test "mathematical_series" {
  // 数学级数测试
  // 等差数列: 1, 3, 5, 7, 9 的和
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9)
  @azimuth.assert_eq(25, sum1)
  
  // 等比数列: 1, 2, 4, 8 的和
  let sum2 = @azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8)
  @azimuth.assert_eq(15, sum2)
  
  // 平方数: 1, 4, 9, 16 的和
  let sum3 = @azimuth.add(@azimuth.add(@azimuth.add(1, 4), 9), 16)
  @azimuth.assert_eq(30, sum3)
}

test "business_calculations_comprehensive" {
  // 综合商业计算测试
  let product_a_price = 25
  let product_b_price = 30
  let product_a_quantity = 3
  let product_b_quantity = 2
  let discount_rate = 10
  let tax_rate = 8
  
  // 计算小计
  let subtotal_a = @azimuth.multiply(product_a_price, product_a_quantity)
  let subtotal_b = @azimuth.multiply(product_b_price, product_b_quantity)
  let total_subtotal = @azimuth.add(subtotal_a, subtotal_b)
  
  // 计算折扣
  let discount = @azimuth.multiply(total_subtotal, discount_rate) / 100
  let discounted_total = @azimuth.add(total_subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_total, tax_rate) / 100
  let final_total = @azimuth.add(discounted_total, tax)
  
  @azimuth.assert_eq(75, subtotal_a)
  @azimuth.assert_eq(60, subtotal_b)
  @azimuth.assert_eq(135, total_subtotal)
  @azimuth.assert_eq(13, discount)
  @azimuth.assert_eq(122, discounted_total)
  @azimuth.assert_eq(9, tax)
  @azimuth.assert_eq(131, final_total)
}

test "time_calculations" {
  // 时间计算测试
  let days = 5
  let hours_per_day = 24
  let minutes_per_hour = 60
  
  // 计算总小时数
  let total_hours = @azimuth.multiply(days, hours_per_day)
  @azimuth.assert_eq(120, total_hours)
  
  // 计算总分钟数
  let total_minutes = @azimuth.multiply(total_hours, minutes_per_hour)
  @azimuth.assert_eq(7200, total_minutes)
  
  // 计算工作小时数 (假设每天8小时)
  let work_hours_per_day = 8
  let total_work_hours = @azimuth.multiply(days, work_hours_per_day)
  @azimuth.assert_eq(40, total_work_hours)
}

test "geometry_calculations_extended" {
  // 扩展几何计算测试
  let rectangle_length = 8
  let rectangle_width = 6
  let square_side = 5
  let triangle_base = 10
  let triangle_height = 6
  
  // 矩形面积和周长
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  let rectangle_perimeter = @azimuth.multiply(2, @azimuth.add(rectangle_length, rectangle_width))
  
  // 正方形面积和周长
  let square_area = @azimuth.multiply(square_side, square_side)
  let square_perimeter = @azimuth.multiply(4, square_side)
  
  // 三角形面积 (近似值: 底 × 高 ÷ 2)
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  
  @azimuth.assert_eq(48, rectangle_area)
  @azimuth.assert_eq(28, rectangle_perimeter)
  @azimuth.assert_eq(25, square_area)
  @azimuth.assert_eq(20, square_perimeter)
  @azimuth.assert_eq(30, triangle_area)
}

test "temperature_conversions" {
  // 温度转换测试 (近似值)
  let celsius = 25
  
  // 摄氏度转华氏度 (近似公式: F = C * 9/5 + 32)
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  
  // 华氏度转摄氏度 (近似公式: C = (F - 32) * 5/9)
  let celsius_back = @azimuth.multiply(@azimuth.add(fahrenheit, -32), 5) / 9
  
  @azimuth.assert_eq(77, fahrenheit)
  @azimuth.assert_eq(25, celsius_back)
}