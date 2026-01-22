// Azimuth 综合测试套件
// 包含10个高质量的 MoonBit 测试用例

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法单位元
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  
  // 乘法单位元
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  
  // 乘法零元
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(-17, 0))
}

test "complex_business_workflow" {
  // 模拟完整业务工作流程
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping_cost = 15
  let discount = 30
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_before_discount = add(add(subtotal, tax_amount), shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total_before_discount)
  assert_eq(201, final_total)
}

test "string_processing_internationalization" {
  // 测试字符串处理的国际化功能
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(7, divide_with_ceil(20, 3))
  assert_eq(1, divide_with_ceil(3, 5))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-6, divide_with_ceil(-17, 3))
  
  // 负数除负数
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(6, divide_with_ceil(-17, -3))
  
  // 零除数
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 150
  let days_to_simulate = 7
  
  // 计算总销售量
  let total_sold = multiply(daily_sales, days_to_simulate)
  
  // 计算补货后的库存
  let stock_after_sales = add(initial_stock, -total_sold)
  let final_stock = add(stock_after_sales, restock_amount)
  
  // 计算需要的补货次数
  let restock_cycles_needed = divide_with_ceil(total_sold, restock_amount)
  
  assert_eq(175, total_sold)
  assert_eq(325, stock_after_sales)
  assert_eq(475, final_stock)
  assert_eq(2, restock_cycles_needed)
}

test "mathematical_series_calculation" {
  // 数学级数计算
  // 等差数列求和：1 + 3 + 5 + ... + 19 (10项)
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证结果
  assert_eq(100, sum_formula)
}

test "complex_financial_calculation" {
  // 复杂金融计算
  let principal = 10000
  let interest_rate = 5
  let years = 3
  let compound_frequency = 12
  
  // 简化的复利计算（年复利）
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, interest_rate), 100))
  let amount_after_year3 = add(amount_after_year2, divide_with_ceil(multiply(amount_after_year2, interest_rate), 100))
  
  // 计算总利息
  let total_interest = add(amount_after_year3, -principal)
  
  assert_eq(10500, amount_after_year1)
  assert_eq(11025, amount_after_year2)
  assert_eq(11576, amount_after_year3)
  assert_eq(1576, total_interest)
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let project_c_cost = 2200
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个800）
  let small_projects_count = divide_with_ceil(remaining_budget, 800)
  
  assert_eq(8500, allocated_budget)
  assert_eq(1500, remaining_budget)
  assert_eq(2, small_projects_count)
}

test "data_analysis_statistics" {
  // 数据分析统计
  let data_points = [45, 67, 23, 89, 12, 56, 78, 34, 90, 41]
  let sum_first_half = add(add(add(add(45, 67), add(23, 89)), add(12, 56)))
  let sum_second_half = add(add(add(add(78, 34), add(90, 41)), 0))
  let total_sum = add(sum_first_half, sum_second_half)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(total_sum, 10)
  
  // 计算最大值和最小值的差
  let max_value = 90
  let min_value = 12
  let range = add(max_value, -min_value)
  
  assert_eq(292, sum_first_half)
  assert_eq(243, sum_second_half)
  assert_eq(535, total_sum)
  assert_eq(54, average)
  assert_eq(78, range)
}

test "engineering_calculation_scenario" {
  // 工程计算场景
  let beam_length = 12
  let beam_width = 8
  let beam_height = 6
  let material_density = 2400
  
  // 计算体积
  let volume = multiply(multiply(beam_length, beam_width), beam_height)
  
  // 计算重量
  let weight = multiply(volume, material_density)
  
  // 计算支撑点数量（每3米一个）
  let support_points_needed = divide_with_ceil(beam_length, 3)
  
  // 计算安全系数（重量除以1000，向上取整）
  let safety_factor = divide_with_ceil(weight, 1000)
  
  assert_eq(576, volume)
  assert_eq(1382400, weight)
  assert_eq(4, support_points_needed)
  assert_eq(1383, safety_factor)
}