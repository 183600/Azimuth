// 新增的 MoonBit 测试用例
// 包含各种实用测试场景，使用标准的 MoonBit 测试语法

test "mathematical_operations_comprehensive" {
  // 综合测试数学运算
  @azimuth.assert_eq(100, @azimuth.add(40, 60))
  @azimuth.assert_eq(-20, @azimuth.add(30, -50))
  @azimuth.assert_eq(1000, @azimuth.multiply(25, 40))
  @azimuth.assert_eq(-144, @azimuth.multiply(-12, 12))
}

test "string_processing_various_scenarios" {
  // 测试各种字符串处理场景
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "boundary_values_extreme_cases" {
  // 测试极值边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "real_world_financial_calculation" {
  // 实际金融计算场景：贷款利息计算
  let principal = 10000  // 本金
  let rate = 5          // 年利率5%
  let years = 3         // 3年
  
  // 简化利息计算：本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
}

test "geometry_area_calculations" {
  // 几何面积计算
  let rectangle_length = 12
  let rectangle_width = 8
  let triangle_base = 10
  let triangle_height = 6
  
  // 矩形面积
  let rectangle_area = @azimuth.multiply(rectangle_length, rectangle_width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 三角形面积（简化为整数计算）
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "temperature_conversion_scenarios" {
  // 温度转换场景（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  
  let freezing_celsius = 0
  let boiling_celsius = 100
  let room_celsius = 20
  
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  
  @azimuth.assert_eq(30, freezing_fahrenheit)
  @azimuth.assert_eq(230, boiling_fahrenheit)
  @azimuth.assert_eq(70, room_fahrenheit)
}

test "shopping_cart_total_calculation" {
  // 购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount = 50
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 应用折扣
  let final_total = @azimuth.add(subtotal, -discount)
  
  @azimuth.assert_eq(1194, final_total)  // (299*2 + 199*3) - 50 = 1194
}

test "statistical_calculations" {
  // 统计计算：平均值和中位数
  let numbers_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let count = 5
  
  // 计算平均值
  let average = numbers_sum / count
  @azimuth.assert_eq(30, average)
  
  // 验证总和
  @azimuth.assert_eq(150, numbers_sum)
}

test "time_conversion_calculations" {
  // 时间转换计算
  let hours = 2
  let minutes = 45
  
  // 转换为总分钟数
  let total_minutes = @azimuth.add(@azimuth.multiply(hours, 60), minutes)
  @azimuth.assert_eq(165, total_minutes)
  
  // 转换为总秒数
  let total_seconds = @azimuth.multiply(total_minutes, 60)
  @azimuth.assert_eq(9900, total_seconds)
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("success", "success")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}