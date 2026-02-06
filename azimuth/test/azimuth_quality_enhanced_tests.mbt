// Azimuth 项目增强测试用例
// 包含10个高质量的MoonBit标准测试用例
// 覆盖边界情况、实际应用场景和复杂计算逻辑

test "mathematical_distributive_law" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 5
  let c = 3
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  @azimuth.assert_eq(56, left_side)  // 7 * (5 + 3) = 56
}

test "resource_allocation_with_constraints" {
  // 测试带约束的资源分配问题
  let total_tasks = 89
  let team_members = 6
  let working_days = 5
  
  // 计算每人每天需要完成的任务数
  let tasks_per_person_total = @azimuth.divide_with_ceil(total_tasks, team_members)
  let tasks_per_person_per_day = @azimuth.divide_with_ceil(tasks_per_person_total, working_days)
  
  @azimuth.assert_eq(15, tasks_per_person_total)  // ceil(89/6) = 15
  @azimuth.assert_eq(3, tasks_per_person_per_day)  // ceil(15/5) = 3
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(@azimuth.multiply(tasks_per_person_per_day, team_members), working_days)
  @azimuth.assert_eq(90, total_capacity)  // 3 * 6 * 5 = 90
}

test "financial_calculation_with_interest" {
  // 测试带利息的金融计算
  let principal = 10000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 计算简单利息: principal * (1 + rate * years)
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(interest_rate, years))
  let interest_amount = @azimuth.divide_with_ceil(total_interest, 100)
  let total_amount = @azimuth.add(principal, interest_amount)
  
  @azimuth.assert_eq(1500, interest_amount)  // 10000 * 5 * 3 / 100 = 1500
  @azimuth.assert_eq(11500, total_amount)    // 10000 + 1500 = 11500
}

test "inventory_optimization_problem" {
  // 测试库存优化问题
  let demand_forecast = 247
  let package_size = 24
  let safety_stock_ratio = 20  // 20%
  
  // 计算基础包装数量
  let base_packages = @azimuth.divide_with_ceil(demand_forecast, package_size)
  
  // 计算安全库存
  let safety_stock = @azimuth.divide_with_ceil(@azimuth.multiply(demand_forecast, safety_stock_ratio), 100)
  let safety_packages = @azimuth.divide_with_ceil(safety_stock, package_size)
  
  // 总包装数量
  let total_packages = @azimuth.add(base_packages, safety_packages)
  
  @azimuth.assert_eq(11, base_packages)      // ceil(247/24) = 11
  @azimuth.assert_eq(50, safety_stock)       // ceil(247*20/100) = 50
  @azimuth.assert_eq(3, safety_packages)     // ceil(50/24) = 3
  @azimuth.assert_eq(14, total_packages)     // 11 + 3 = 14
}

test "time_calculation_with_overtime" {
  // 测试包含加班的时间计算
  let regular_hours_per_day = 8
  let days_worked = 6
  let overtime_hours = 12
  
  // 计算正常工作时间
  let total_regular_hours = @azimuth.multiply(regular_hours_per_day, days_worked)
  
  // 计算加班等效时间 (1.5倍 = 3/2)
  let overtime_equivalent = @azimuth.multiply(@azimuth.multiply(overtime_hours, 3), 1)
  let overtime_equivalent_final = @azimuth.divide_with_ceil(overtime_equivalent, 2)
  
  // 总等效工作时间
  let total_equivalent_hours = @azimuth.add(total_regular_hours, overtime_equivalent_final)
  
  @azimuth.assert_eq(48, total_regular_hours)           // 8 * 6 = 48
  @azimuth.assert_eq(18, overtime_equivalent_final)     // ceil(12*3/2) = 18
  @azimuth.assert_eq(66, total_equivalent_hours)        // 48 + 18 = 66
}

test "shipping_cost_calculation" {
  // 测试运费计算
  let package_weight = 17
  let base_rate = 5
  let weight_threshold = 10
  let additional_rate = 2
  
  // 计算基础运费
  let base_cost = base_rate
  
  // 计算超重费用
  let excess_weight = @azimuth.subtract(package_weight, weight_threshold)
  let excess_packages = @azimuth.divide_with_ceil(excess_weight, 5)  // 每5kg一个计费单位
  let excess_cost = @azimuth.multiply(excess_packages, additional_rate)
  
  // 总运费
  let total_cost = @azimuth.add(base_cost, excess_cost)
  
  @azimuth.assert_eq(7, excess_weight)        // 17 - 10 = 7
  @azimuth.assert_eq(2, excess_packages)      // ceil(7/5) = 2
  @azimuth.assert_eq(4, excess_cost)          // 2 * 2 = 4
  @azimuth.assert_eq(9, total_cost)           // 5 + 4 = 9
}

test "project_timeline_optimization" {
  // 测试项目时间线优化
  let total_tasks = 156
  let parallel_teams = 4
  let tasks_per_team = @azimuth.divide_with_ceil(total_tasks, parallel_teams)
  let days_per_task = 2
  let buffer_time_ratio = 10  // 10%
  
  // 计算基础时间
  let base_days = @azimuth.multiply(tasks_per_team, days_per_task)
  
  // 计算缓冲时间
  let buffer_days = @azimuth.divide_with_ceil(@azimuth.multiply(base_days, buffer_time_ratio), 100)
  
  // 总时间线
  let total_timeline = @azimuth.add(base_days, buffer_days)
  
  @azimuth.assert_eq(39, tasks_per_team)      // ceil(156/4) = 39
  @azimuth.assert_eq(78, base_days)           // 39 * 2 = 78
  @azimuth.assert_eq(8, buffer_days)          // ceil(78*10/100) = 8
  @azimuth.assert_eq(86, total_timeline)      // 78 + 8 = 86
}

test "energy_consumption_calculation" {
  // 测试能源消耗计算
  let devices = 12
  let power_per_device = 150  // 瓦特
  let hours_per_day = 8
  let days_per_month = 30
  let efficiency_factor = 85  // 85%效率
  
  // 计算总功率消耗
  let total_power = @azimuth.multiply(devices, power_per_device)
  
  // 计算每日能源消耗 (瓦特时)
  let daily_consumption = @azimuth.multiply(total_power, hours_per_day)
  
  // 计算月度实际消耗 (考虑效率)
  let monthly_raw_consumption = @azimuth.multiply(daily_consumption, days_per_month)
  let monthly_actual_consumption = @azimuth.multiply(monthly_raw_consumption, efficiency_factor)
  let final_consumption = @azimuth.divide_with_ceil(monthly_actual_consumption, 100)
  
  @azimuth.assert_eq(1800, total_power)                    // 12 * 150 = 1800
  @azimuth.assert_eq(14400, daily_consumption)             // 1800 * 8 = 14400
  @azimuth.assert_eq(432000, monthly_raw_consumption)      // 14400 * 30 = 432000
  @azimuth.assert_eq(367200, monthly_actual_consumption)   // 432000 * 85 = 367200
  @azimuth.assert_eq(3672, final_consumption)              // ceil(367200/100) = 3672
}

test "complex_greeting_scenarios" {
  // 测试复杂问候场景
  let user_name = "MoonBit Developer"
  
  // 基本问候
  let basic_greeting = @azimuth.greet(user_name)
  
  // 验证问候内容
  @azimuth.assert_eq_string("Hello, MoonBit Developer!", basic_greeting)
  @azimuth.assert_eq_string("Hello, " + user_name + "!", basic_greeting)
  
  // 测试特殊字符处理
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_greeting = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", special_greeting)
  
  // 测试Unicode字符
  let unicode_name = "开发者"
  let unicode_greeting = @azimuth.greet(unicode_name)
  @azimuth.assert_eq_string("Hello, 开发者!", unicode_greeting)
}

test "comprehensive_mathematical_validation" {
  // 综合数学验证测试
  let base_value = 23
  let multiplier = 7
  let divisor = 5
  let addition = 19
  
  // 复杂数学运算序列
  let step1 = @azimuth.multiply(base_value, multiplier)           // 23 * 7 = 161
  let step2 = @azimuth.divide_with_ceil(step1, divisor)           // ceil(161/5) = 33
  let step3 = @azimuth.add(step2, addition)                       // 33 + 19 = 52
  let step4 = @azimuth.multiply(step3, divisor)                   // 52 * 5 = 260
  let step5 = @azimuth.divide_with_ceil(step4, multiplier)        // ceil(260/7) = 38
  
  @azimuth.assert_eq(161, step1)     // 23 * 7 = 161
  @azimuth.assert_eq(33, step2)      // ceil(161/5) = 33
  @azimuth.assert_eq(52, step3)      // 33 + 19 = 52
  @azimuth.assert_eq(260, step4)     // 52 * 5 = 260
  @azimuth.assert_eq(38, step5)      // ceil(260/7) = 38
  
  // 验证数学恒等式
  let identity_test = @azimuth.multiply(step5, divisor)
  let identity_result = @azimuth.divide_with_ceil(identity_test, divisor)
  @azimuth.assert_eq(step5, identity_result)  // 验证除法和乘法的逆运算
}