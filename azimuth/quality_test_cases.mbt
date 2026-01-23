// 高质量 MoonBit 测试用例
// 覆盖边界情况、特殊场景和实际应用

test "add_function_precision" {
  // 测试加法精度和边界情况
  @azimuth.assert_eq(1000000000, @azimuth.add(500000000, 500000000))
  @azimuth.assert_eq(-1000000000, @azimuth.add(-500000000, -500000000))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(-1, @azimuth.add(-2147483648, 2147483647))
}

test "multiply_function_large_scale" {
  // 测试大规模乘法运算
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(-100000000, @azimuth.multiply(-10000, 10000))
  @azimuth.assert_eq(100000000, @azimuth.multiply(-10000, -10000))
  @azimuth.assert_eq(46340, @azimuth.multiply(46340, 1))
}

test "divide_with_ceil_business_logic" {
  // 测试业务逻辑场景：资源分配
  let employees = 23
  let desks_per_row = 6
  let rows_needed = @azimuth.divide_with_ceil(employees, desks_per_row)
  @azimuth.assert_eq(4, rows_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(rows_needed, desks_per_row)
  @azimuth.assert_eq(24, total_capacity)
}

test "greet_function_multilingual" {
  // 测试多语言问候功能
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目成本
  let team_size = 5
  let daily_rate = 200
  let project_days = 12
  let overhead_rate = 15
  
  // 计算基础成本
  let base_cost = @azimuth.multiply(team_size, @azimuth.multiply(daily_rate, project_days))
  
  // 计算管理费用
  let overhead_cost = @azimuth.multiply(base_cost, overhead_rate)
  
  // 计算总成本
  let total_cost = @azimuth.add(base_cost, overhead_cost)
  
  @azimuth.assert_eq(12000, base_cost)      // 5 * 200 * 12 = 12000
  @azimuth.assert_eq(180000, overhead_cost) // 12000 * 15 = 180000
  @azimuth.assert_eq(192000, total_cost)    // 12000 + 180000 = 192000
}

test "mathematical_properties_comprehensive" {
  // 测试数学性质的综合验证
  let a = 17
  let b = 23
  let c = 31
  
  // 加法交换律和结合律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法交换律和结合律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "edge_case_zero_handling" {
  // 测试零值的全面处理
  // 加法中的零值
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法中的零值
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -42))
  
  // 除法中的零值
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 42))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -42))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
}

test "negative_number_comprehensive" {
  // 测试负数的全面运算
  // 负数加法
  @azimuth.assert_eq(-15, @azimuth.add(-7, -8))
  @azimuth.assert_eq(-1, @azimuth.add(-7, 6))
  @azimuth.assert_eq(1, @azimuth.add(-6, 7))
  
  // 负数乘法
  @azimuth.assert_eq(56, @azimuth.multiply(-7, -8))
  @azimuth.assert_eq(-42, @azimuth.multiply(-7, 6))
  @azimuth.assert_eq(-42, @azimuth.multiply(7, -6))
  
  // 负数除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-15, 8))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(15, -8))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-15, -8))
}

test "real_world_inventory_management" {
  // 测试真实世界库存管理场景
  let total_products = 157
  let box_capacity = 24
  let warehouse_sections = 6
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  @azimuth.assert_eq(7, boxes_needed)
  
  // 计算每个仓库区域的箱子数量
  let boxes_per_section = @azimuth.divide_with_ceil(boxes_needed, warehouse_sections)
  @azimuth.assert_eq(2, boxes_per_section)
  
  // 计算总存储容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = @azimuth.add(total_capacity, -total_products)
  @azimuth.assert_eq(11, remaining_space)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串和特殊字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \t\n\r!", @azimuth.greet("\t\n\r"))
  
  // 特殊符号和表情符号
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, 🎉🚀🌟!", @azimuth.greet("🎉🚀🌟"))
  
  // 长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetingFunctionWithExtendedInput"
  let expected = "Hello, " + long_string + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_string))
}