// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，针对核心功能进行全面测试

test "string_operations_with_greet" {
  // 测试字符串操作与问候函数的组合
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  assert_eq_string("Hello, 张三!", greeting)
  
  // 测试包含数字的字符串
  let user_id = "user_" + "12345"
  let id_greeting = greet(user_id)
  assert_eq_string("Hello, user_12345!", id_greeting)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let interest_rate = 5
  let years = 3
  
  // 计算复利（简化版本）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(10500, year1_amount)
  assert_eq(11025, year2_amount)
  assert_eq(11576, year3_amount)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 5000
  let team_a_cost = 1200
  let team_b_cost = 1800
  let team_c_cost = 1500
  
  // 计算已分配预算
  let allocated_budget = add(add(team_a_cost, team_b_cost), team_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的额外小项目数量
  let small_project_cost = 200
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(4500, allocated_budget)
  assert_eq(500, remaining_budget)
  assert_eq(3, additional_projects)
}

test "time_management_calculations" {
  // 测试时间管理计算
  let daily_working_hours = 8
  let total_tasks = 75
  let average_task_time = 2  // 每个任务平均2小时
  
  // 计算完成任务需要的总小时数
  let total_hours_needed = multiply(total_tasks, average_task_time)
  
  // 计算需要的工作天数
  let workdays_needed = divide_with_ceil(total_hours_needed, daily_working_hours)
  
  // 计算剩余小时数（最后一个工作日）
  let remaining_hours = add(total_hours_needed, -multiply(workdays_needed - 1, daily_working_hours))
  
  assert_eq(150, total_hours_needed)
  assert_eq(19, workdays_needed)
  assert_eq(6, remaining_hours)
}

test "inventory_turnover_calculation" {
  // 测试库存周转率计算
  let initial_inventory = 500
  let monthly_sales = 120
  let months_to_simulate = 6
  
  // 计算总销售量
  let total_sales = multiply(monthly_sales, months_to_simulate)
  
  // 计算期末库存
  let final_inventory = add(initial_inventory, -total_sales)
  
  // 计算需要补货的时间点（当库存低于100时）
  let reorder_point = 100
  let months_until_reorder = divide_with_ceil(add(initial_inventory, -reorder_point), monthly_sales)
  
  assert_eq(720, total_sales)
  assert_eq(-220, final_inventory)
  assert_eq(4, months_until_reorder)
}

test "data_processing_batch_size" {
  // 测试数据处理批次大小计算
  let total_records = 10000
  let max_batch_size = 750
  let processing_overhead = 50  // 每批次的额外开销
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_records, max_batch_size)
  
  // 计算实际处理的记录数（包含开销）
  let total_processed = add(multiply(batches_needed, max_batch_size), multiply(batches_needed, processing_overhead))
  
  // 计算处理效率
  let efficiency = divide_with_ceil(multiply(total_records, 100), total_processed)
  
  assert_eq(14, batches_needed)
  assert_eq(11200, total_processed)
  assert_eq(89, efficiency)
}

test "geometric_progression_calculations" {
  // 测试等比数列计算
  let first_term = 3
  let common_ratio = 2
  let number_of_terms = 8
  
  // 计算第n项（简化版本，只计算前几项）
  let term1 = first_term
  let term2 = multiply(term1, common_ratio)
  let term3 = multiply(term2, common_ratio)
  let term4 = multiply(term3, common_ratio)
  
  // 计算前四项的和
  let sum_first_four = add(add(add(term1, term2), term3), term4)
  
  assert_eq(3, term1)
  assert_eq(6, term2)
  assert_eq(12, term3)
  assert_eq(24, term4)
  assert_eq(45, sum_first_four)
}

test "shipping_logistics_optimization" {
  // 测试物流配送优化
  let total_packages = 157
  let truck_capacity = 24
  let delivery_cost_per_truck = 150
  let handling_cost_per_package = 5
  
  // 计算需要的卡车数量
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  // 计算总配送成本
  let total_delivery_cost = multiply(trucks_needed, delivery_cost_per_truck)
  
  // 计算总处理成本
  let total_handling_cost = multiply(total_packages, handling_cost_per_package)
  
  // 计算总成本
  let total_cost = add(total_delivery_cost, total_handling_cost)
  
  assert_eq(7, trucks_needed)
  assert_eq(1050, total_delivery_cost)
  assert_eq(785, total_handling_cost)
  assert_eq(1835, total_cost)
}

test "temperature_conversion_calculations" {
  // 测试温度转换计算（简化版本）
  let celsius_temp = 25
  let fahrenheit_temp = 77
  
  // 模拟温度转换公式（简化为整数运算）
  // F = C * 9/5 + 32
  let converted_fahrenheit = add(divide_with_ceil(multiply(celsius_temp, 9), 5), 32)
  
  // C = (F - 32) * 5/9
  let converted_celsius = divide_with_ceil(multiply(add(fahrenheit_temp, -32), 5), 9)
  
  assert_eq(77, converted_fahrenheit)
  assert_eq(25, converted_celsius)
}

test "error_boundary_testing" {
  // 测试错误边界情况
  // 测试极大值运算
  let large_num1 = 1000000
  let large_num2 = 999999
  let large_sum = add(large_num1, large_num2)
  let large_product = multiply(1000, 1000)
  
  // 测试边界除法
  let boundary_div1 = divide_with_ceil(1, 1000000)
  let boundary_div2 = divide_with_ceil(999999, 1000000)
  let boundary_div3 = divide_with_ceil(1000000, 1)
  
  // 测试零值处理
  let zero_add = add(0, 0)
  let zero_multiply = multiply(0, 12345)
  let zero_divide = divide_with_ceil(0, 123)
  let zero_divisor = divide_with_ceil(123, 0)
  
  assert_eq(1999999, large_sum)
  assert_eq(1000000, large_product)
  assert_eq(1, boundary_div1)
  assert_eq(1, boundary_div2)
  assert_eq(1000000, boundary_div3)
  assert_eq(0, zero_add)
  assert_eq(0, zero_multiply)
  assert_eq(0, zero_divide)
  assert_eq(0, zero_divisor)
}