// 高级 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试各种边界情况和实际应用场景
test "addition_commutativity" {
  // 测试加法交换律：a + b = b + a
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(15, 27), @azimuth.@azimuth.add(27, 15))
  
  // 测试负数的交换律
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(-10, 20), @azimuth.@azimuth.add(20, -10))
}
test "multiplication_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let left_side = @azimuth.@azimuth.multiply(5, @azimuth.@azimuth.add(3, 7))
  let right_side = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(5, 3), @azimuth.@azimuth.multiply(5, 7))
  @azimuth.@azimuth.assert_eq(left_side, right_side)
}
test "extreme_boundary_values" {
  // 测试极端边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  @azimuth.@azimuth.assert_eq(-1, @azimuth.@azimuth.add(max_val, min_val))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.multiply(min_val, -1))
  @azimuth.@azimuth.assert_eq(min_val, @azimuth.@azimuth.multiply(min_val, 1))
}
test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  @azimuth.@azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.@azimuth.greet("@#$%^&*()"))
  @azimuth.@azimuth.assert_eq_string("Hello,   !", @azimuth.@azimuth.greet("  "))
  @azimuth.@azimuth.assert_eq_string("Hello, \n\t!", @azimuth.@azimuth.greet("\n\t"))
}
test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款利息计算
  // 简化的单利计算：利息 = 本金 × 利率 × 年数
  let interest = @azimuth.@azimuth.multiply(10000, @azimuth.@azimuth.multiply(5, 3))
  let total = @azimuth.@azimuth.add(10000, interest)
  
  // 验证：10000 + 10000×5×3 = 10000 + 150000 = 160000
  @azimuth.@azimuth.assert_eq(160000, total)
}
test "geometric_calculations" {
  // 测试几何计算：立方体体积和表面积
  let side = 6
  
  // 体积 = 边长³
  let volume = @azimuth.@azimuth.multiply(side, @azimuth.@azimuth.multiply(side, side))
  @azimuth.@azimuth.assert_eq(216, volume)
  
  // 表面积 = 6 × 边长²
  let surface_area = @azimuth.@azimuth.multiply(6, @azimuth.@azimuth.multiply(side, side))
  @azimuth.@azimuth.assert_eq(216, surface_area)
}
test "nested_function_calls" {
  // 测试嵌套函数调用
  // 计算：(a + b) × (a - b) + c²
  let sum_ab = @azimuth.@azimuth.add(8, 3)
  let diff_ab = @azimuth.@azimuth.add(8, -3)
  let product = @azimuth.@azimuth.multiply(sum_ab, diff_ab)
  let square_c = @azimuth.@azimuth.multiply(4, 4)
  let result = @azimuth.@azimuth.add(product, square_c)
  
  // 验证：(8 + 3) × (8 - 3) + 4² = 11 × 5 + 16 = 55 + 16 = 71
  @azimuth.@azimuth.assert_eq(71, result)
}
test "real_world_inventory_management" {
  // 测试实际库存管理场景
  // 计算最终库存：初始 - 售出 + 补货 - 损坏
  let after_sales = @azimuth.@azimuth.add(100, -25)
  let after_restock = @azimuth.@azimuth.add(after_sales, 40)
  let final_stock = @azimuth.@azimuth.add(after_restock, -5)
  
  // 验证：100 - 25 + 40 - 5 = 110
  @azimuth.@azimuth.assert_eq(110, final_stock)
}
test "temperature_conversion_formula" {
  // 测试温度转换公式（简化版）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数：F = C × 2 + 30（近似值）
  
  let fahrenheit = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(20, 2), 30)
  @azimuth.@azimuth.assert_eq(70, fahrenheit)
  
  // 测试水的冰点
  let freezing_f = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(0, 2), 30)
  @azimuth.@azimuth.assert_eq(30, freezing_f)
  
  // 测试水的沸点
  let boiling_f = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(100, 2), 30)
  @azimuth.@azimuth.assert_eq(230, boiling_f)
}
test "error_boundary_validation" {
  // 测试错误边界验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护是否正常工作
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(min_val, -1, true) == min_val)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(max_val, 2, true) == max_val)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(min_val, -2, true) == min_val)
  
  // 验证正常计算
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(100, 200, true) == 300)
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.multiply(15, 4, true) == 60)
}