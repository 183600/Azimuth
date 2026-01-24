// 标准 MoonBit 测试用例 - 10个高质量测试用例
// 使用正确的 @azimuth. 前缀语法

test "basic_functionality_validation" {
  // 基本功能验证测试
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(-4, @azimuth.add(-7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  @azimuth.assert_eq(21, @azimuth.multiply(7, 3))
  @azimuth.assert_eq(-21, @azimuth.multiply(-7, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律：a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 2
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(x, @azimuth.add(0, x))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(x, 0))
}

test "boundary_conditions_and_edge_cases" {
  // 边界条件和边缘情况测试
  // 极值测试
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  
  // 除零错误处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(100001, @azimuth.divide_with_ceil(1000001, 10))
}

test "unicode_and_internationalization" {
  // Unicode和国际化测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, ñoño!", @azimuth.greet("ñoño"))
  @azimuth.assert_eq_string("Hello, 🏳️‍🌈!", @azimuth.greet("🏳️‍🌈"))
  @azimuth.assert_eq_string("Hello, 🌙⭐!", @azimuth.greet("🌙⭐"))
  
  // 特殊字符和空白字符
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \"quotes\"!", @azimuth.greet("\"quotes\""))
  @azimuth.assert_eq_string("Hello, 'apostrophe'!", @azimuth.greet("'apostrophe'"))
  @azimuth.assert_eq_string("Hello, \\backslash\\!", @azimuth.greet("\\backslash\\"))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源规划计算
  
  // 基础参数
  let team_size = 8
  let tasks_per_person = 5
  let sprint_capacity = 15
  let complexity_factor = 2
  
  // 计算总任务数
  let total_tasks = @azimuth.multiply(team_size, tasks_per_person)
  @azimuth.assert_eq(40, total_tasks)
  
  // 应用复杂度因子
  let adjusted_tasks = @azimuth.multiply(total_tasks, complexity_factor)
  @azimuth.assert_eq(80, adjusted_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = @azimuth.divide_with_ceil(adjusted_tasks, sprint_capacity)
  @azimuth.assert_eq(6, sprints_needed)  // ceil(80/15) = 6
  
  // 计算总容量
  let total_capacity = @azimuth.multiply(sprints_needed, sprint_capacity)
  @azimuth.assert_eq(90, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, adjusted_tasks)
  @azimuth.assert_eq(10, remaining_capacity)
}

test "real_world_business_scenarios" {
  // 真实世界业务场景测试
  // 场景1：库存管理和装箱
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = @azimuth.divide_with_ceil(total_products, box_capacity)
  @azimuth.assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(boxes_needed, box_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  // 场景2：活动座位安排
  let participants = 87
  let chairs_per_table = 8
  let tables_needed = @azimuth.divide_with_ceil(participants, chairs_per_table)
  @azimuth.assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  let total_seats = @azimuth.multiply(tables_needed, chairs_per_table)
  @azimuth.assert_eq(88, total_seats)
  
  // 场景3：运输车辆计算
  let packages = 25
  let capacity_per_truck = 8
  let trucks_needed = @azimuth.divide_with_ceil(packages, capacity_per_truck)
  @azimuth.assert_eq(4, trucks_needed)  // ceil(25/8) = 4
}

test "error_handling_and_resilience" {
  // 错误处理和恢复能力测试
  // 除零错误的多种情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
  
  // 极小除数情况
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(999999, 1))
  @azimuth.assert_eq(-999999, @azimuth.divide_with_ceil(-999999, 1))
  
  // 边界算术运算
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483645, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483646, -1))
  
  // 字符串边界情况
  @azimuth.assert_eq_string("Hello, null!", @azimuth.greet("null"))
  @azimuth.assert_eq_string("Hello, undefined!", @azimuth.greet("undefined"))
  @azimuth.assert_eq_string("Hello, 0!", @azimuth.greet("0"))
}

test "performance_and_large_scale_operations" {
  // 性能和大规模运算测试
  let large_number = 100000
  
  // 大数加法
  let sum_result = @azimuth.add(large_number, large_number)
  @azimuth.assert_eq(200000, sum_result)
  
  // 大数乘法
  let product_result = @azimuth.multiply(100, large_number)
  @azimuth.assert_eq(10000000, product_result)
  
  // 大数除法
  let division_result = @azimuth.divide_with_ceil(product_result, 50000)
  @azimuth.assert_eq(200, division_result)
  
  // 连续运算性能测试
  let step1 = @azimuth.add(large_number, 50000)      // 150000
  let step2 = @azimuth.multiply(step1, 2)            // 300000
  let step3 = @azimuth.divide_with_ceil(step2, 1000) // 300
  let step4 = @azimuth.add(step3, 700)               // 1000
  @azimuth.assert_eq(1000, step4)
}

test "financial_calculation_scenarios" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 100000
  let monthly_payment = 3500
  let months_needed = @azimuth.divide_with_ceil(loan_amount, monthly_payment)
  @azimuth.assert_eq(29, months_needed)  // ceil(100000/3500) = 29
  
  // 验证总支付金额
  let total_payment = @azimuth.multiply(months_needed, monthly_payment)
  @azimuth.assert_eq(101500, total_payment)
  
  // 计算最后一个月的实际支付金额
  let regular_payments = @azimuth.multiply(months_needed - 1, monthly_payment)
  let final_payment = subtract(total_payment, regular_payments)
  @azimuth.assert_eq(1500, final_payment)
  
  // 场景：投资回报计算
  let principal = 10000
  let annual_rate = 5  // 5%
  
  // 简化的复利计算
  let year1_end = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100))
  let year2_end = @azimuth.add(year1_end, @azimuth.divide_with_ceil(@azimuth.multiply(year1_end, annual_rate), 100))
  let year3_end = @azimuth.add(year2_end, @azimuth.divide_with_ceil(@azimuth.multiply(year2_end, annual_rate), 100))
  
  @azimuth.assert_eq(10500, year1_end)  // 10000 + 5%
  @azimuth.assert_eq(11025, year2_end)  // 10500 + 5%
  @azimuth.assert_eq(11576, year3_end)  // 11025 + 5%
}

test "comprehensive_integration_test" {
  // 综合集成测试
  // 复杂的多步骤业务场景
  
  // 项目参数
  let project_duration = 12  // 月
  let team_members = 6
  let hourly_rate = 75
  let hours_per_day = 8
  let work_days_per_month = 22
  
  // 计算月工时
  let monthly_hours = @azimuth.multiply(hours_per_day, work_days_per_month)
  @azimuth.assert_eq(176, monthly_hours)
  
  // 计算团队月工时
  let team_monthly_hours = @azimuth.multiply(monthly_hours, team_members)
  @azimuth.assert_eq(1056, team_monthly_hours)
  
  // 计算项目总工时
  let total_hours = @azimuth.multiply(team_monthly_hours, project_duration)
  @azimuth.assert_eq(12672, total_hours)
  
  // 计算项目总成本
  let total_cost = @azimuth.multiply(total_hours, hourly_rate)
  @azimuth.assert_eq(950400, total_cost)
  
  // 计算季度预算
  let quarterly_budget = @azimuth.divide_with_ceil(total_cost, 4)
  @azimuth.assert_eq(237600, quarterly_budget)
  
  // 验证总预算
  let calculated_total = @azimuth.multiply(quarterly_budget, 4)
  @azimuth.assert_eq(950400, calculated_total)
  
  // 项目完成度计算
  let completed_tasks = 85
  let total_tasks = 100
  let completion_percentage = @azimuth.multiply(@azimuth.divide_with_ceil(@azimuth.multiply(completed_tasks, 100), total_tasks), 1)
  @azimuth.assert_eq(85, completion_percentage)
  
  // 项目完成问候
  let project_name = "Azimuth"
  let greeting_message = @azimuth.greet(project_name)
  @azimuth.assert_eq_string("Hello, Azimuth!", greeting_message)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}