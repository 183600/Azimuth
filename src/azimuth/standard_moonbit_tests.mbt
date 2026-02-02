// 标准 MoonBit 测试用例 - 为 azimuth 项目新增
// 包含核心功能的全面测试，覆盖边界情况和实际应用场景

// 导入必要的包
test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(30, multiply(5, 6))
  assert_eq(-3, subtract(10, 13))
  assert_eq(4, divide_with_ceil(11, 3))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 18
  
  // 交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(a, add(a, 0))
  assert_eq(0, multiply(a, 0))
}

test "negative_number_comprehensive_test" {
  // 负数综合测试
  assert_eq(-15, add(-7, -8))
  assert_eq(-30, multiply(5, -6))
  assert_eq(25, subtract(10, -15))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "business_calculation_scenario" {
  // 业务计算场景测试
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping_cost = 12
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_cost = add(add(subtotal, tax_amount), shipping_cost)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)
  assert_eq(304, total_cost)
}

test "packaging_and_logistics_calculation" {
  // 包装和物流计算测试
  let total_items = 58
  let items_per_box = 15
  let boxes_per_pallet = 4
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  let remaining_capacity = subtract(multiply(boxes_needed, items_per_box), total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(1, pallets_needed)
  assert_eq(2, remaining_capacity)
}

test "complex_nested_operations" {
  // 复杂嵌套运算测试
  let base_value = 10
  let multiplier = 3
  let offset = 5
  
  let result1 = add(multiply(base_value, multiplier), offset)
  let result2 = multiply(add(base_value, offset), multiplier)
  let result3 = divide_with_ceil(multiply(base_value, multiplier), offset)
  
  assert_eq(35, result1)  // 10*3 + 5 = 35
  assert_eq(45, result2)  // (10 + 5)*3 = 45
  assert_eq(6, result3)   // ceil(10*3/5) = ceil(6) = 6
}

test "performance_and_efficiency_calculation" {
  // 性能和效率计算测试
  let baseline_performance = 1000
  let improvement_factor = 25
  let overhead_percentage = 10
  
  let improved_performance = add(baseline_performance, divide_with_ceil(multiply(baseline_performance, improvement_factor), 100))
  let overhead = divide_with_ceil(multiply(improved_performance, overhead_percentage), 100)
  let net_performance = subtract(improved_performance, overhead)
  
  assert_eq(1250, improved_performance)
  assert_eq(125, overhead)
  assert_eq(1125, net_performance)
}

test "data_analysis_and_statistics" {
  // 数据分析和统计测试
  let data1 = 95
  let data2 = 87
  let data3 = 92
  let data4 = 88
  let data5 = 94
  let sum_manual = add(add(add(add(data1, data2), data3), data4), data5)
  let average = divide_with_ceil(sum_manual, 5)
  
  assert_eq(456, sum_manual)
  assert_eq(92, average)
}