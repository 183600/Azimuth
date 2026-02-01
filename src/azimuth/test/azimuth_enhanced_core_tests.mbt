// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "subtract_functionality" {
  // 测试减法功能
  @azimuth.assert_eq(5, @azimuth.subtract(10, 5))
  @azimuth.assert_eq(-5, @azimuth.subtract(5, 10))
  @azimuth.assert_eq(0, @azimuth.subtract(5, 5))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
}

test "complex_arithmetic_sequence" {
  // 测试复杂数学序列计算
  let a = 2
  let b = 3
  let c = 4
  
  // 计算 (a + b) * c - a
  let result = @azimuth.subtract(
    @azimuth.multiply(@azimuth.add(a, b), c),
    a
  )
  @azimuth.assert_eq(18, result)  // (2 + 3) * 4 - 2 = 5 * 4 - 2 = 20 - 2 = 18
}

test "financial_calculation" {
  // 金融计算场景：计算折扣价格
  let original_price = 120
  let discount_percentage = 15
  let discount_amount = @azimuth.divide_with_ceil(
    @azimuth.multiply(original_price, discount_percentage),
    100
  )
  let final_price = @azimuth.subtract(original_price, discount_amount)
  
  @azimuth.assert_eq(18, discount_amount)  // ceil(120 * 15 / 100) = ceil(18) = 18
  @azimuth.assert_eq(102, final_price)    // 120 - 18 = 102
}

test "temperature_conversion" {
  // 温度转换模拟：摄氏度到华氏度的简化计算
  let celsius = 25
  // 简化的华氏度计算：F = C * 2 + 30 (这不是真实公式，仅用于测试)
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 50 + 30 = 80
}

test "inventory_optimization" {
  // 库存优化计算
  let current_stock = 150
  let daily_sales = 12
  let days_to_reorder = 7
  let safety_stock = 20
  
  let sales_until_reorder = @azimuth.multiply(daily_sales, days_to_reorder)
  let reorder_point = @azimuth.add(sales_until_reorder, safety_stock)
  let buffer = @azimuth.subtract(current_stock, reorder_point)
  
  @azimuth.assert_eq(84, sales_until_reorder)   // 12 * 7 = 84
  @azimuth.assert_eq(104, reorder_point)        // 84 + 20 = 104
  @azimuth.assert_eq(46, buffer)                // 150 - 104 = 46
}

test "resource_allocation" {
  // 资源分配计算
  let total_budget = 1000
  let project_a_cost = 350
  let project_b_cost = 280
  let remaining_budget = @azimuth.subtract(
    total_budget,
    @azimuth.add(project_a_cost, project_b_cost)
  )
  
  // 计算可以支持的小项目数量
  let small_project_cost = 50
  let small_projects = @azimuth.divide_with_ceil(remaining_budget, small_project_cost)
  
  @azimuth.assert_eq(370, remaining_budget)  // 1000 - (350 + 280) = 1000 - 630 = 370
  @azimuth.assert_eq(8, small_projects)      // ceil(370 / 50) = ceil(7.4) = 8
}

test "time_management" {
  // 时间管理计算
  let total_tasks = 45
  let tasks_per_hour = 6
  let working_hours_per_day = 8
  
  let hours_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_hour)
  let days_needed = @azimuth.divide_with_ceil(hours_needed, working_hours_per_day)
  
  @azimuth.assert_eq(8, hours_needed)  // ceil(45 / 6) = ceil(7.5) = 8
  @azimuth.assert_eq(1, days_needed)   // ceil(8 / 8) = ceil(1) = 1
}

test "batch_processing" {
  // 批处理计算
  let total_records = 1250
  let batch_size = 100
  let processing_time_per_batch = 5
  
  let total_batches = @azimuth.divide_with_ceil(total_records, batch_size)
  let total_processing_time = @azimuth.multiply(total_batches, processing_time_per_batch)
  
  @azimuth.assert_eq(13, total_batches)        // ceil(1250 / 100) = ceil(12.5) = 13
  @azimuth.assert_eq(65, total_processing_time) // 13 * 5 = 65
}

test "greeting_with_special_formatting" {
  // 测试特殊格式的问候
  let special_names = ["", "A", "John Doe", "张三", "user@domain.com", "12345", "Hello World"]
  
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(special_names[0]))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet(special_names[1]))
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet(special_names[2]))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(special_names[3]))
  @azimuth.assert_eq_string("Hello, user@domain.com!", @azimuth.greet(special_names[4]))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet(special_names[5]))
  @azimuth.assert_eq_string("Hello, Hello World!", @azimuth.greet(special_names[6]))
}

test "edge_case_division" {
  // 边界情况除法测试
  // 测试各种边界值的除法
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))     // 相等数
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100))  // 接近但不等于
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(100, 1)) // 大数除以小数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))     // 零除以正数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))     // 零除以零（安全处理）
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))   // 负数相除
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))     // 简单分数
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(999999, 1000)) // 大数除法
}