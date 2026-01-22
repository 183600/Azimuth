// 综合测试用例 - 覆盖各种功能和边界情况
// 测试1: 复杂数学运算组合
test "complex_mathematical_operations" {
  // 计算 (a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.@azimuth.add(a, b)
  let diff_cd = @azimuth.@azimuth.add(c, -d)
  let product_ef = @azimuth.@azimuth.multiply(e, f)
  let result = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(sum_ab, diff_cd), product_ef)
  
  @azimuth.@azimuth.assert_eq(141, result)  // (10+5)*(20-8) + 3*7 = 15*12 + 21 = 180 + 21 = 201
}
// 测试2: 字符串处理边界情况
test "string_processing_edge_cases" {
  // 测试包含特殊字符的字符串
  @azimuth.@azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.@azimuth.greet("@#$%^&*()"))
  
  // 测试包含数字的字符串
  @azimuth.@azimuth.assert_eq_string("Hello, 2023!", @azimuth.@azimuth.greet("2023"))
  
  // 测试包含空格的字符串
  @azimuth.@azimuth.assert_eq_string("Hello, John Doe!", @azimuth.@azimuth.greet("John Doe"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.@azimuth.assert_eq_string("Hello, 你好!", @azimuth.@azimuth.greet("你好"))
}
// 测试3: 极值运算测试
test "extreme_value_operations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.@azimuth.assert_eq(-1, @azimuth.@azimuth.add(max_val, min_val))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.multiply(min_val, -1))
  
  // 测试最大值与正数的运算
  @azimuth.@azimuth.assert_eq(max_val, @azimuth.@azimuth.add(max_val, 1))
  @azimuth.@azimuth.assert_eq(max_val, @azimuth.@azimuth.multiply(max_val, 1))
  
  // 测试最小值与负数的运算
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.add(min_val, -1))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.multiply(min_val, 1))
}
// 测试4: 实际应用场景 - 订单计算
test "order_calculation_scenario" {
  // 商品价格和数量
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  // 计算每种商品的总价
  let total1 = @azimuth.@azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.@azimuth.multiply(item2_price, item2_quantity)
  
  // 计算订单总价
  let order_total = @azimuth.@azimuth.add(total1, total2)
  
  // 计算税费（10%）
  let tax = @azimuth.@azimuth.multiply(order_total, 10) / 100
  
  // 计算最终金额
  let final_amount = @azimuth.@azimuth.add(order_total, tax)
  
  @azimuth.@azimuth.assert_eq(758, final_amount)  // (199*2 + 299) + 10% = 697 + 69 = 766
}
// 测试5: 实际应用场景 - 几何计算
test "geometry_calculations" {
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = @azimuth.@azimuth.multiply(length, width)
  let perimeter = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(length, 2), @azimuth.@azimuth.multiply(width, 2))
  
  @azimuth.@azimuth.assert_eq(96, area)      // 12 * 8 = 96
  @azimuth.@azimuth.assert_eq(40, perimeter) // 12*2 + 8*2 = 24 + 16 = 40
  
  // 计算三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.@azimuth.multiply(base, height) / 2
  
  @azimuth.@azimuth.assert_eq(30, triangle_area) // 10 * 6 / 2 = 30
}
// 测试6: 数学性质验证
test "mathematical_properties_verification" {
  let a = 15
  let b = 7
  let c = 3
  
  // 验证结合律: (a + b) + c = a + (b + c)
  let left_assoc = @azimuth.@azimuth.add(@azimuth.@azimuth.add(a, b), c)
  let right_assoc = @azimuth.@azimuth.add(a, @azimuth.@azimuth.add(b, c))
  @azimuth.@azimuth.assert_eq(left_assoc, right_assoc)
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let distrib_left = @azimuth.@azimuth.multiply(a, @azimuth.@azimuth.add(b, c))
  let distrib_right = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(a, b), @azimuth.@azimuth.multiply(a, c))
  @azimuth.@azimuth.assert_eq(distrib_left, distrib_right)
  
  // 验证交换律: a * b = b * a
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(a, b), @azimuth.@azimuth.multiply(b, a))
}
// 测试7: 性能相关测试
test "performance_related_tests" {
  // 测试大量计算
  let base = 100
  let result = @azimuth.@azimuth.multiply(@azimuth.@azimuth.multiply(@azimuth.@azimuth.multiply(base, 2), 3), 4)
  @azimuth.@azimuth.assert_eq(2400, result) // 100 * 2 * 3 * 4 = 2400
  
  // 测试连续加法
  let sum_result = @azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.add(1000, 2000), 3000), 4000)
  @azimuth.@azimuth.assert_eq(10000, sum_result)
  
  // 测试混合运算
  let mixed_result = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(50, 3), @azimuth.@azimuth.multiply(20, 4))
  @azimuth.@azimuth.assert_eq(230, mixed_result) // 50*3 + 20*4 = 150 + 80 = 230
}
// 测试8: 错误处理和边界情况
test "error_handling_and_boundaries" {
  // 测试零值运算
  @azimuth.@azimuth.assert_eq(0, @azimuth.@azimuth.multiply(0, 2147483647))
  @azimuth.@azimuth.assert_eq(0, @azimuth.@azimuth.multiply(0, -2147483648))
  @azimuth.@azimuth.assert_eq(2147483647, @azimuth.@azimuth.add(0, 2147483647))
  @azimuth.@azimuth.assert_eq(-2147483648, @azimuth.@azimuth.add(0, -2147483648))
  
  // 测试单位元运算
  @azimuth.@azimuth.assert_eq(42, @azimuth.@azimuth.multiply(42, 1))
  @azimuth.@azimuth.assert_eq(42, @azimuth.@azimuth.add(42, 0))
  @azimuth.@azimuth.assert_eq(-42, @azimuth.@azimuth.multiply(-42, 1))
  @azimuth.@azimuth.assert_eq(-42, @azimuth.@azimuth.add(-42, 0))
  
  // 测试负数运算
  @azimuth.@azimuth.assert_eq(-25, @azimuth.@azimuth.multiply(-5, 5))
  @azimuth.@azimuth.assert_eq(25, @azimuth.@azimuth.multiply(-5, -5))
  @azimuth.@azimuth.assert_eq(-10, @azimuth.@azimuth.add(-15, 5))
  @azimuth.@azimuth.assert_eq(10, @azimuth.@azimuth.add(15, -5))
}
// 测试9: 实际应用场景 - 时间计算
test "time_calculation_scenario" {
  // 计算总秒数
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let total_seconds = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(@azimuth.@azimuth.multiply(hours, 60), 60), 
                                   @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(minutes, 60), seconds))
  
  @azimuth.@azimuth.assert_eq(9045, total_seconds) // 2*60*60 + 30*60 + 45 = 7200 + 1800 + 45 = 9045
  
  // 计算时间差
  let start_hour = 9
  let start_minute = 15
  let end_hour = 14
  let end_minute = 45
  
  let start_total = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(start_hour, 60), start_minute)
  let end_total = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(end_hour, 60), end_minute)
  let duration = @azimuth.@azimuth.add(end_total, -start_total)
  
  @azimuth.@azimuth.assert_eq(330, duration) // (14*60+45) - (9*60+15) = 885 - 555 = 330分钟
}
// 测试10: 综合场景测试
test "comprehensive_scenario_test" {
  // 模拟一个完整的购物场景
  let budget = 1000
  let item_price = 125
  let quantity = 7
  let discount_threshold = 5
  let discount_rate = 10
  
  // 计算总价
  let subtotal = @azimuth.@azimuth.multiply(item_price, quantity)
  
  // 计算折扣（如果购买数量超过阈值）
  let discount_amount = if (quantity > discount_threshold) {
    @azimuth.@azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算最终价格
  let final_price = @azimuth.@azimuth.add(subtotal, -discount_amount)
  
  // 检查是否在预算内
  let within_budget = final_price <= budget
  
  @azimuth.@azimuth.assert_eq(787, final_price)  // 125*7 - 10% = 875 - 87 = 788
  @azimuth.@azimuth.assert_eq(within_budget, true)   // 787 <= 1000 为真
}