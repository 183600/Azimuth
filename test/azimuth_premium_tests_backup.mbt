// Azimuth 高级测试用例
// 包含复杂场景和边界条件的全面测试

test "advanced_financial_compound_interest" {
  // 复利计算：本金1000，年利率5%，3年
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(1158, year3_amount)  // 1000 -> 1050 -> 1103 -> 1158
}

test "advanced_resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 10000
  let project_a_cost = 3200
  let project_b_cost = 2800
  let project_c_cost = 1900
  
  let allocated_amount = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -allocated_amount)
  
  // 计算可以启动的小项目数量（每个800）
  let small_projects_count = divide_with_ceil(remaining_budget, 800)
  
  assert_eq(7900, allocated_amount)
  assert_eq(2100, remaining_budget)
  assert_eq(3, small_projects_count)
}

test "advanced_time_series_analysis" {
  // 时间序列分析：计算移动平均
  let day1_sales = 120
  let day2_sales = 150
  let day3_sales = 180
  let day4_sales = 165
  let day5_sales = 195
  
  // 计算3天移动平均
  let avg_days_1_3 = divide_with_ceil(add(add(day1_sales, day2_sales), day3_sales), 3)
  let avg_days_2_4 = divide_with_ceil(add(add(day2_sales, day3_sales), day4_sales), 3)
  let avg_days_3_5 = divide_with_ceil(add(add(day3_sales, day4_sales), day5_sales), 3)
  
  assert_eq(150, avg_days_1_3)
  assert_eq(165, avg_days_2_4)
  assert_eq(180, avg_days_3_5)
}

test "advanced_geometric_calculations" {
  // 几何计算：矩形面积和周长
  let length = 15
  let width = 8
  
  let area = multiply(length, width)
  let perimeter = multiply(2, add(length, width))
  
  // 计算需要多少块1x1的地砖（向上取整）
  let tiles_needed = area
  let tile_packages = divide_with_ceil(tiles_needed, 10)  // 每包10块
  
  assert_eq(120, area)
  assert_eq(46, perimeter)
  assert_eq(12, tile_packages)
}

test "advanced_error_boundary_conditions" {
  // 错误边界条件测试
  let max_safe_value = 1000000
  let min_safe_value = -1000000
  
  // 测试接近边界值的运算
  let near_max_result = add(max_safe_value, 1)
  let near_min_result = add(min_safe_value, -1)
  
  // 测试除法的各种边界情况
  let division_by_large = divide_with_ceil(100, max_safe_value)
  let division_of_large = divide_with_ceil(max_safe_value, 999999)
  let negative_division = divide_with_ceil(-max_safe_value, 3)
  
  assert_eq(1000001, near_max_result)
  assert_eq(-1000001, near_min_result)
  assert_eq(1, division_by_large)
  assert_eq(2, division_of_large)
  assert_eq(-333333, negative_division)
}

test "advanced_string_processing_scenarios" {
  // 高级字符串处理场景
  let prefix = "Dr."
  let first_name = "Jane"
  let last_name = "Smith"
  let suffix = "PhD"
  
  // 构建完整姓名
  let full_name = prefix + " " + first_name + " " + last_name + ", " + suffix
  let formal_greeting = greet(full_name)
  
  // 测试特殊字符组合
  let email = "user.name+tag@example.com"
  let email_greeting = greet(email)
  
  assert_eq_string("Hello, Dr. Jane Smith, PhD!", formal_greeting)
  assert_eq_string("Hello, user.name+tag@example.com!", email_greeting)
}

test "advanced_inventory_turnover_calculation" {
  // 库存周转率计算
  let initial_inventory = 500
  let purchases = 1200
  let final_inventory = 300
  let cost_of_goods_sold = add(initial_inventory, add(purchases, -final_inventory))
  
  let average_inventory = divide_with_ceil(add(initial_inventory, final_inventory), 2)
  let turnover_rate = divide_with_ceil(multiply(cost_of_goods_sold, 100), average_inventory)
  
  assert_eq(1400, cost_of_goods_sold)
  assert_eq(400, average_inventory)
  assert_eq(350, turnover_rate)
}

test "advanced_performance_metrics_calculation" {
  // 性能指标计算
  let baseline_operations = 1000
  let optimized_operations = 3500
  let system_overhead = 200
  
  let improvement_ratio = divide_with_ceil(optimized_operations, baseline_operations)
  let net_improvement = add(optimized_operations, -system_overhead)
  let efficiency_gain = divide_with_ceil(multiply(net_improvement, 100), baseline_operations)
  
  assert_eq(4, improvement_ratio)
  assert_eq(3300, net_improvement)
  assert_eq(330, efficiency_gain)
}

test "advanced_statistical_variance_simulation" {
  // 统计方差模拟（简化版本）
  let sum_values = add(add(add(add(10, 15), 20), 25), 30)
  let mean = divide_with_ceil(sum_values, 5)
  
  // 计算平方和（简化版本）
  let squared_diff_sum = add(add(add(
    multiply(10 - mean, 10 - mean),
    multiply(15 - mean, 15 - mean)),
    multiply(20 - mean, 20 - mean)),
    multiply(25 - mean, 25 - mean)),
    multiply(30 - mean, 30 - mean))
  
  let variance = divide_with_ceil(squared_diff_sum, 5)
  
  assert_eq(100, sum_values)
  assert_eq(20, mean)
  assert_eq(40, variance)
}

test "advanced_complex_business_workflow" {
  // 复杂业务工作流：订单处理全流程
  let product_price = 89
  let quantity = 12
  let customer_discount = 5  // 5%
  let tax_rate = 8  // 8%
  let shipping_base = 15
  let shipping_per_item = 2
  
  // 计算商品总价
  let subtotal = multiply(product_price, quantity)
  
  // 应用客户折扣
  let discount_amount = divide_with_ceil(multiply(subtotal, customer_discount), 100)
  let discounted_total = add(subtotal, -discount_amount)
  
  // 计算运费
  let total_shipping = add(shipping_base, multiply(shipping_per_item, quantity))
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(discounted_total, tax_rate), 100)
  
  // 计算最终总价
  let final_total = add(add(discounted_total, total_shipping), tax_amount)
  
  assert_eq(1068, subtotal)
  assert_eq(54, discount_amount)
  assert_eq(1014, discounted_total)
  assert_eq(39, total_shipping)
  assert_eq(82, tax_amount)
  assert_eq(1135, final_total)
}