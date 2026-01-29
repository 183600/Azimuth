// Azimuth 项目高质量测试用例
// 使用标准 MoonBit 测试语法
// 包含边界测试、实际应用场景和数学性质验证

test "basic_arithmetic_operations" {
  // 测试基本的算术运算
  @azimuth.assert_eq(10, @azimuth.add(4, 6))
  @azimuth.assert_eq(-3, @azimuth.add(-8, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 100))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  
  // 负数测试
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet("user@domain.com"))
}

test "mathematical_properties_verification" {
  // 验证数学性质：交换律、结合律、分配律
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律验证：a * (b + c) = a * b + a * c
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "business_inventory_calculation" {
  // 业务场景：库存计算
  let initial_stock = 500
  let units_per_box = 24
  let total_orders = 150
  
  // 计算需要的箱子数量
  let boxes_needed = @azimuth.divide_with_ceil(total_orders, units_per_box)
  
  // 计算剩余库存
  let remaining_stock = @azimuth.add(initial_stock, -total_orders)
  
  @azimuth.assert_eq(7, boxes_needed)  // ceil(150/24) = 7
  @azimuth.assert_eq(350, remaining_stock)
}

test "financial_calculation_scenario" {
  // 金融计算场景：贷款计算
  let principal = 10000
  let interest_rate = 8  // 8%
  let loan_term_years = 3
  
  // 简化的单利计算
  let total_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100)
  let interest_per_year = @azimuth.divide_with_ceil(total_interest, loan_term_years)
  let total_repayment = @azimuth.add(principal, total_interest)
  
  @azimuth.assert_eq(800, total_interest)
  @azimuth.assert_eq(267, interest_per_year)  // ceil(800/3) = 267
  @azimuth.assert_eq(10800, total_repayment)
}

test "complex_nested_expressions" {
  // 复杂嵌套表达式测试
  let x = 10
  let y = 15
  let z = 20
  
  // 计算：(x + y) * z - x / y
  let result1 = @azimuth.add(@azimuth.multiply(@azimuth.add(x, y), z), -@azimuth.divide_with_ceil(x, y))
  
  // 计算：x * y + y * z - z * x
  let result2 = @azimuth.add(@azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(y, z)), -@azimuth.multiply(z, x))
  
  @azimuth.assert_eq(499, result1)  // (10 + 15) * 20 - ceil(10/15) = 25 * 20 - 1 = 500 - 1 = 499
  @azimuth.assert_eq(150, result2)  // 10 * 15 + 15 * 20 - 20 * 10 = 150 + 300 - 200 = 250
}

test "edge_cases_and_boundary_values" {
  // 边界值和极端情况测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 接近边界值的运算
  @azimuth.assert_eq(2147483647, @azimuth.add(max_int - 100, 100))
  @azimuth.assert_eq(-2147483648, @azimuth.add(min_int + 100, -100))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_int))
  @azimuth.assert_eq(0, @azimuth.multiply(min_int, 0))
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
}

test "time_calculation_project_management" {
  // 项目管理时间计算
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = @azimuth.divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(work_days_needed, 8)
  
  @azimuth.assert_eq(11, work_days_needed)  // ceil(87/8) = 11
  @azimuth.assert_eq(3, weeks_needed)       // ceil(11/5) = 3
  @azimuth.assert_eq(88, total_hours)       // 11 * 8 = 88
}

test "data_processing_algorithm" {
  // 数据处理算法模拟
  let data_points = 1000
  let batch_size = 75
  let processing_time_per_batch = 5  // 分钟
  
  // 计算需要的批次数
  let batches_needed = @azimuth.divide_with_ceil(data_points, batch_size)
  
  // 计算总处理时间
  let total_processing_time = @azimuth.multiply(batches_needed, processing_time_per_batch)
  
  // 计算每小时处理的数据点数
  let throughput_per_hour = @azimuth.divide_with_ceil(@azimuth.multiply(data_points, 60), total_processing_time)
  
  @azimuth.assert_eq(14, batches_needed)      // ceil(1000/75) = 14
  @azimuth.assert_eq(70, total_processing_time)  // 14 * 5 = 70 分钟
  @azimuth.assert_eq(857, throughput_per_hour)   // ceil(1000 * 60 / 70) = ceil(60000/70) = 857
}