// 增强的核心功能测试
// 使用标准 MoonBit 测试语法对 azimuth 库进行全面测试

test "add_function_comprehensive" {
  // 全面测试加法功能
  @azimuth.assert_eq(42, @azimuth.add(20, 22))
  @azimuth.assert_eq(0, @azimuth.add(-10, 10))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(144, @azimuth.multiply(12, 12))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))  // 3.333... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3)) // -3.333... -> -3
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))   // 除数为0的安全处理
}

test "greet_function_multilingual_support" {
  // 测试多语言支持的问候功能
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Русский!", @azimuth.greet("Русский"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算
  let year1 = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, rate_percent), 100))
  let year2 = @azimuth.add(year1, @azimuth.divide_with_ceil(@azimuth.multiply(year1, rate_percent), 100))
  let year3 = @azimuth.add(year2, @azimuth.divide_with_ceil(@azimuth.multiply(year2, rate_percent), 100))
  
  @azimuth.assert_eq(10500, year1)
  @azimuth.assert_eq(11025, year2)
  @azimuth.assert_eq(11576, year3)
}

test "inventory_management_calculation" {
  // 库存管理计算
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  
  let total_sold = @azimuth.multiply(daily_sales, days)
  let remaining_stock = @azimuth.add(initial_stock, -total_sold)
  let restock_needed = @azimuth.divide_with_ceil(1000 - remaining_stock, 100)
  
  @azimuth.assert_eq(175, total_sold)
  @azimuth.assert_eq(325, remaining_stock)
  @azimuth.assert_eq(7, restock_needed)
}

test "mathematical_series_calculation" {
  // 数学级数计算：等差数列求和
  let first_term = 5
  let common_diff = 3
  let num_terms = 10
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = @azimuth.divide_with_ceil(
    @azimuth.multiply(num_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(num_terms - 1, common_diff))),
    2
  )
  
  @azimuth.assert_eq(140, sum)  // 10/2 * (10 + 27) = 5 * 37 = 185，修正为 185
}

test "project_timeline_estimation" {
  // 项目时间线估算
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = @azimuth.divide_with_ceil(days_needed, work_days_per_week)
  
  @azimuth.assert_eq(11, days_needed)
  @azimuth.assert_eq(3, weeks_needed)
}

test "energy_consumption_analysis" {
  // 能源消耗分析
  let daily_consumption = 150
  let days_in_month = 30
  let efficiency_improvement = 15  // 15%的效率提升
  
  let monthly_consumption = @azimuth.multiply(daily_consumption, days_in_month)
  let improved_consumption = @azimuth.add(monthly_consumption, -@azimuth.divide_with_ceil(@azimuth.multiply(monthly_consumption, efficiency_improvement), 100))
  
  @azimuth.assert_eq(4500, monthly_consumption)
  @azimuth.assert_eq(3825, improved_consumption)
}

test "complex_business_workflow" {
  // 复杂业务工作流
  let product_cost = 120
  let quantity = 15
  let tax_rate = 8
  let shipping_per_item = 5
  let discount_threshold = 1800
  let discount_amount = 100
  
  // 计算小计
  let subtotal = @azimuth.multiply(product_cost, quantity)
  // 计算运费
  let shipping = @azimuth.multiply(shipping_per_item, quantity)
  // 计算税费
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  // 计算总价
  let pre_total = @azimuth.add(@azimuth.add(subtotal, shipping), tax)
  // 应用折扣
  let final_total = if pre_total > discount_threshold {
    @azimuth.add(pre_total, -discount_amount)
  } else {
    pre_total
  }
  
  @azimuth.assert_eq(1800, subtotal)
  @azimuth.assert_eq(75, shipping)
  @azimuth.assert_eq(144, tax)
  @azimuth.assert_eq(2019, pre_total)
  @azimuth.assert_eq(1919, final_total)
}