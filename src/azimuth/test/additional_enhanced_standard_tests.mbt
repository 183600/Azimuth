// 额外的增强标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能添加的高质量测试

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  @azimuth.assert_eq(5, @azimuth.subtract(8, 3))
  @azimuth.assert_eq(-5, @azimuth.subtract(3, 8))
  @azimuth.assert_eq(0, @azimuth.subtract(10, 10))
  @azimuth.assert_eq(10, @azimuth.subtract(0, -10))
  @azimuth.assert_eq(-10, @azimuth.subtract(-10, 0))
  @azimuth.assert_eq(20, @azimuth.subtract(-5, -25))
}

test "mathematical_distributive_law" {
  // 测试分配律：a * (b + c) = a * b + a * c
  let a = 4
  let b = 6
  let c = 9
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 4 * (6 + 9) = 4 * 6 + 4 * 9 = 60
}

test "complex_nested_calculations" {
  // 测试复杂的嵌套计算
  let x = 12
  let y = 8
  let z = 5
  
  // 计算：(x + y) * z - x / y
  let result = @azimuth.add(
    @azimuth.multiply(@azimuth.add(x, y), z), 
    -@azimuth.divide_with_ceil(x, y)
  )
  @azimuth.assert_eq(98, result)  // (12 + 8) * 5 - ceil(12/8) = 20*5 - 2 = 100 - 2 = 98
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  let allocated_budget = @azimuth.add(
    @azimuth.add(project_a_cost, project_b_cost), 
    project_c_cost
  )
  let remaining_budget = @azimuth.add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个300）
  let small_projects = @azimuth.divide_with_ceil(remaining_budget, 300)
  
  @azimuth.assert_eq(3900, allocated_budget)
  @azimuth.assert_eq(1100, remaining_budget)
  @azimuth.assert_eq(4, small_projects)
}

test "time_management_workflow" {
  // 时间管理工作流测试
  let daily_work_hours = 8
  let meeting_hours_per_day = 2
  let focused_work_hours = @azimuth.add(daily_work_hours, -meeting_hours_per_day)
  
  let tasks_per_hour = 3
  let daily_tasks = @azimuth.multiply(focused_work_hours, tasks_per_hour)
  
  let weekly_tasks = @azimuth.multiply(daily_tasks, 5)
  let days_needed_for_project = @azimuth.divide_with_ceil(100, daily_tasks)
  
  @azimuth.assert_eq(6, focused_work_hours)
  @azimuth.assert_eq(18, daily_tasks)
  @azimuth.assert_eq(90, weekly_tasks)
  @azimuth.assert_eq(6, days_needed_for_project)
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  let sum_manual = @azimuth.add(@azimuth.add(@azimuth.add(15, 22), @azimuth.add(18, 25)), @azimuth.add(@azimuth.add(20, 16), 24))
  let count = 7
  let average = @azimuth.divide_with_ceil(sum_manual, count)
  
  @azimuth.assert_eq(140, sum_manual)
  @azimuth.assert_eq(20, average)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算测试
  let beginning_inventory = 500
  let ending_inventory = 300
  let cost_of_goods_sold = 1200
  
  let average_inventory = @azimuth.divide_with_ceil(@azimuth.add(beginning_inventory, ending_inventory), 2)
  let inventory_turnover = @azimuth.divide_with_ceil(cost_of_goods_sold, average_inventory)
  
  @azimuth.assert_eq(400, average_inventory)
  @azimuth.assert_eq(3, inventory_turnover)
}

test "energy_efficiency_metrics" {
  // 能源效率指标测试
  let energy_input = 1000
  let useful_output = 750
  let efficiency_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(useful_output, 100), energy_input)
  
  let waste_energy = @azimuth.add(energy_input, -useful_output)
  let waste_percentage = @azimuth.divide_with_ceil(@azimuth.multiply(waste_energy, 100), energy_input)
  
  @azimuth.assert_eq(75, efficiency_percentage)
  @azimuth.assert_eq(250, waste_energy)
  @azimuth.assert_eq(25, waste_percentage)
}

test "complex_string_greeting_scenarios" {
  // 复杂字符串问候场景测试
  let company = "TechCorp"
  let department = "研发部"
  let project = "Azimuth"
  
  let full_greeting = @azimuth.greet(@azimuth.add(@azimuth.add(company, " "), @azimuth.add(department, @azimuth.add(" ", project))))
  @azimuth.assert_eq_string("Hello, TechCorp 研发部 Azimuth!", full_greeting)
  
  // 测试特殊字符和数字组合
  let numeric_id = "2024"
  let special_chars = "!@#$%"
  let mixed_greeting = @azimuth.greet(@azimuth.add(numeric_id, special_chars))
  @azimuth.assert_eq_string("Hello, 2024!@#$%!", mixed_greeting)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate = 8  // 8%
  
  // 第一年
  let year1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100)
  let year1_total = @azimuth.add(principal, year1_interest)
  
  // 第二年
  let year2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year1_total, annual_rate), 100)
  let year2_total = @azimuth.add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(year2_total, annual_rate), 100)
  let year3_total = @azimuth.add(year2_total, year3_interest)
  
  @azimuth.assert_eq(800, year1_interest)
  @azimuth.assert_eq(10800, year1_total)
  @azimuth.assert_eq(864, year2_interest)
  @azimuth.assert_eq(11664, year2_total)
  @azimuth.assert_eq(934, year3_interest)
  @azimuth.assert_eq(12598, year3_total)
}