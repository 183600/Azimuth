// 用户新增的标准 MoonBit 测试用例
// 包含10个测试用例，覆盖基本功能和实际应用场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(10, -10))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-30, @azimuth.multiply(5, -6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 25
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证实际数值
  @azimuth.assert_eq(42, @azimuth.add(25, 17))
  @azimuth.assert_eq(425, @azimuth.multiply(25, 17))
}

test "boundary_value_calculations" {
  // 测试边界值计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "real_world_shopping_cart" {
  // 测试实际应用：购物车计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  let discount = 50
  
  // 计算商品总价
  let subtotal = @azimuth.add(
    @azimuth.multiply(item1_price, item1_qty),
    @azimuth.multiply(item2_price, item2_qty)
  )
  
  // 计算最终价格
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(637, final_total)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 简化公式：F = C × 2 + 30
  
  // 常见温度点转换
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  let room_c = 22
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  @azimuth.assert_eq(74, room_f)
  
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：15, 25, 35, 45, 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(35, average)
  
  // 计算范围
  let range = @azimuth.add(55, -15)
  @azimuth.assert_eq(40, range)
}

test "compound_interest_calculation" {
  // 测试复利计算
  let principal = 5000
  let rate = 6  // 6%
  let years = 4
  
  // 简化复利计算：最终金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let final_amount = @azimuth.multiply(principal, growth_factor)
  
  @azimuth.assert_eq(125000, final_amount)
  
  // 计算利息收益
  let interest = @azimuth.add(final_amount, -principal)
  @azimuth.assert_eq(120000, interest)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  // 矩形面积
  let rect_length = 15
  let rect_width = 8
  let rect_area = @azimuth.multiply(rect_length, rect_width)
  @azimuth.assert_eq(120, rect_area)
  
  // 三角形面积（简化：底 × 高 ÷ 2）
  let triangle_base = 12
  let triangle_height = 10
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2
  @azimuth.assert_eq(60, triangle_area)
  
  // 梯形面积（简化：(上底 + 下底) × 高 ÷ 2）
  let top_base = 8
  let bottom_base = 16
  let height = 6
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(72, trapezoid_area)
}

test "time_duration_calculations" {
  // 测试时间持续时间计算
  let start_hour = 8
  let start_min = 45
  let end_hour = 15
  let end_min = 30
  
  // 转换为分钟数
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_min)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_min)
  
  // 计算持续时间
  let duration = @azimuth.add(end_total, -start_total)
  @azimuth.assert_eq(405, duration)
  
  // 转换为小时和分钟
  let hours = duration / 60
  let minutes = duration % 60
  
  @azimuth.assert_eq(6, hours)
  @azimuth.assert_eq(45, minutes)
}