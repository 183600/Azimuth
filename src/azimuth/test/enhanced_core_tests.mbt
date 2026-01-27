// Azimuth 项目增强核心测试用例
// 专注于边界条件、错误处理和复杂场景测试

test "extreme_boundary_values" {
  // 测试极值边界条件
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
}

test "zero_handling_edge_cases" {
  // 测试零值处理的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
}

test "division_comprehensive_scenarios" {
  // 全面的除法场景测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-8, -3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(8, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(1, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 0))
}

test "string_processing_edge_cases" {
  // 字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello,\n\n!", @azimuth.greet("\n\n"))
  @azimuth.assert_eq_string("Hello, \t\t!", @azimuth.greet("\t\t"))
}

test "mathematical_distributive_property" {
  // 测试数学分配律性质
  let a = 6
  let b = 4
  let c = 2
  
  // a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "complex_inventory_calculation" {
  // 复杂库存计算场景
  let items_per_box = 12
  let total_items_needed = 145
  let boxes_needed = @azimuth.divide_with_ceil(total_items_needed, items_per_box)
  
  let cost_per_box = 25
  let total_cost = @azimuth.multiply(boxes_needed, cost_per_box)
  
  @azimuth.assert_eq(13, boxes_needed)  // ceil(145/12) = 13
  @azimuth.assert_eq(325, total_cost)   // 13 * 25 = 325
}

test "scientific_temperature_conversion" {
  // 科学温度转换计算
  let celsius = 25
  // 转换为华氏度: F = (C * 9/5) + 32
  let fahrenheit = @azimuth.add(@azimuth.multiply(@azimuth.multiply(celsius, 9), 1) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)
}

test "financial_compound_interest" {
  // 金融复利计算简化版
  let principal = 1000
  let rate = 5
  let years = 3
  
  // 简化的年复利计算 (不使用幂运算)
  let year1 = @azimuth.add(principal, @azimuth.multiply(principal, rate) / 100)
  let year2 = @azimuth.add(year1, @azimuth.multiply(year1, rate) / 100)
  let year3 = @azimuth.add(year2, @azimuth.multiply(year2, rate) / 100)
  
  @azimuth.assert_eq(1050, year1)  // 1000 + 50
  @azimuth.assert_eq(1102, year2)  // 1050 + 52
  @azimuth.assert_eq(1157, year3)  // 1102 + 55
}

test "batch_processing_efficiency" {
  // 批处理效率计算
  let total_records = 987
  let batch_size = 50
  let batches_needed = @azimuth.divide_with_ceil(total_records, batch_size)
  
  let processing_time_per_batch = 12  // 分钟
  let total_time = @azimuth.multiply(batches_needed, processing_time_per_batch)
  
  @azimuth.assert_eq(20, batches_needed)  // ceil(987/50) = 20
  @azimuth.assert_eq(240, total_time)     // 20 * 12 = 240
}