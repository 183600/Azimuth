// Azimuth 综合测试套件 - 10个标准 MoonBit 测试用例
// 覆盖核心功能的各种场景和边界情况

test "add_function_boundary_values" {
  // 测试加法函数的边界值情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(1, @azimuth.add(-2147483648, 2147483647))
}

test "multiply_function_zero_properties" {
  // 测试乘法函数的零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -2147483648))
}

test "divide_with_ceil_comprehensive_coverage" {
  // 全面测试向上取整除法的各种情况
  // 正数情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(34, @azimuth.divide_with_ceil(100, 3))
  
  // 负数情况
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
  
  // 特殊情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "greet_function_unicode_and_special_chars" {
  // 测试问候函数的Unicode和特殊字符处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγδε!", @azimuth.greet("αβγδε"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet("!@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, 📱💻⌨️!", @azimuth.greet("📱💻⌨️"))
}

test "complex_financial_calculations" {
  // 复杂的金融计算场景测试
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 计算复利（简化版本，使用向上取整）
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate), 100))
  
  @azimuth.assert_eq(10500, year1)
  @azimuth.assert_eq(11025, year2)
  @azimuth.assert_eq(11576, year3)
}

test "mathematical_laws_verification" {
  // 验证数学定律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "real_world_inventory_management" {
  // 真实世界的库存管理场景
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 200
  let days_to_simulate = 7
  
  // 计算一周后的库存
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  let stock_before_restock = @azimuth.add(initial_stock, -total_sold)
  let final_stock = @azimuth.add(stock_before_restock, restock_amount)
  
  @azimuth.assert_eq(175, total_sold)
  @azimuth.assert_eq(325, stock_before_restock)
  @azimuth.assert_eq(525, final_stock)
}

test "engineering_calculations" {
  // 工程计算测试
  let length = 15
  let width = 8
  let height = 3
  let material_cost_per_unit = 45
  
  // 计算表面积
  let floor_area = @azimuth.multiply(length, width)
  let wall_area = @azimuth.multiply(2, @azimuth.multiply(@azimuth.add(length, width), height))
  let total_area = @azimuth.add(floor_area, wall_area)
  
  // 计算材料成本
  let total_cost = @azimuth.multiply(total_area, material_cost_per_unit)
  
  @azimuth.assert_eq(120, floor_area)
  @azimuth.assert_eq(138, wall_area)
  @azimuth.assert_eq(258, total_area)
  @azimuth.assert_eq(11610, total_cost)
}

test "data_processing_algorithms" {
  // 数据处理算法测试
  let data_points = 1000
  let batch_size = 37
  let processing_time_per_batch = 5
  
  // 计算需要的批次数
  let batches_needed = @azimuth.divide_with_ceil(data_points, batch_size)
  
  // 计算总处理时间
  let total_processing_time = @azimuth.multiply(batches_needed, processing_time_per_batch)
  
  @azimuth.assert_eq(28, batches_needed)
  @azimuth.assert_eq(140, total_processing_time)
}

test "comprehensive_error_handling" {
  // 全面的错误处理测试
  // 测试除零的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
  
  // 测试极端数值的稳定性
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
}