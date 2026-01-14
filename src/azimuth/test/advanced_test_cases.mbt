// 高级 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试各种边界情况和实际应用场景

test "addition_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 测试负数的交换律
  let c = -10
  let d = 20
  azimuth::assert_eq(azimuth::add(c, d), azimuth::add(d, c))
}

test "multiplication_distributivity" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 5
  let b = 3
  let c = 7
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  azimuth::assert_eq(left_side, right_side)
}

test "extreme_boundary_values" {
  // 测试极端边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  azimuth::assert_eq(-1, azimuth::add(max_val, min_val))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, -1))
  azimuth::assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含各种特殊字符的字符串
  azimuth::assert_eq_string("Hello, @#$%^&*()!", azimuth::greet("@#$%^&*()"))
  azimuth::assert_eq_string("Hello,   !", azimuth::greet("  "))
  azimuth::assert_eq_string("Hello, \n\t!", azimuth::greet("\n\t"))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款利息计算
  let principal = 10000  // 本金
  let rate = 5           // 年利率5%
  let years = 3          // 3年
  
  // 简化的单利计算：利息 = 本金 × 利率 × 年数
  let interest = azimuth::multiply(principal, azimuth::multiply(rate, years))
  let total = azimuth::add(principal, interest)
  
  // 验证：10000 + 10000×5×3 = 10000 + 150000 = 160000
  azimuth::assert_eq(160000, total)
}

test "geometric_calculations" {
  // 测试几何计算：立方体体积和表面积
  let side = 6
  
  // 体积 = 边长³
  let volume = azimuth::multiply(side, azimuth::multiply(side, side))
  azimuth::assert_eq(216, volume)
  
  // 表面积 = 6 × 边长²
  let surface_area = azimuth::multiply(6, azimuth::multiply(side, side))
  azimuth::assert_eq(216, surface_area)
}

test "nested_function_calls" {
  // 测试嵌套函数调用
  let a = 8
  let b = 3
  let c = 4
  
  // 计算：(a + b) × (a - b) + c²
  let sum_ab = azimuth::add(a, b)
  let diff_ab = azimuth::add(a, -b)
  let product = azimuth::multiply(sum_ab, diff_ab)
  let square_c = azimuth::multiply(c, c)
  let result = azimuth::add(product, square_c)
  
  // 验证：(8 + 3) × (8 - 3) + 4² = 11 × 5 + 16 = 55 + 16 = 71
  azimuth::assert_eq(71, result)
}

test "real_world_inventory_management" {
  // 测试实际库存管理场景
  let initial_stock = 100
  let sold = 25
  let restocked = 40
  let damaged = 5
  
  // 计算最终库存：初始 - 售出 + 补货 - 损坏
  let after_sales = azimuth::add(initial_stock, -sold)
  let after_restock = azimuth::add(after_sales, restocked)
  let final_stock = azimuth::add(after_restock, -damaged)
  
  // 验证：100 - 25 + 40 - 5 = 110
  azimuth::assert_eq(110, final_stock)
}

test "temperature_conversion_formula" {
  // 测试温度转换公式（简化版）
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数：F = C × 2 + 30（近似值）
  
  let celsius = 20
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  azimuth::assert_eq(70, fahrenheit)
  
  // 测试水的冰点
  let freezing_c = 0
  let freezing_f = azimuth::add(azimuth::multiply(freezing_c, 2), 30)
  azimuth::assert_eq(30, freezing_f)
  
  // 测试水的沸点
  let boiling_c = 100
  let boiling_f = azimuth::add(azimuth::multiply(boiling_c, 2), 30)
  azimuth::assert_eq(230, boiling_f)
}

test "error_boundary_validation" {
  // 测试错误边界验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护是否正常工作
  azimuth::assert_true(azimuth::add(max_val, 1) == max_val)
  azimuth::assert_true(azimuth::add(min_val, -1) == min_val)
  azimuth::assert_true(azimuth::multiply(max_val, 2) == max_val)
  azimuth::assert_true(azimuth::multiply(min_val, -2) == min_val)
  
  // 验证正常计算
  azimuth::assert_true(azimuth::add(100, 200) == 300)
  azimuth::assert_true(azimuth::multiply(15, 4) == 60)
}