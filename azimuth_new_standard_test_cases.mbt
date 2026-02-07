// Azimuth 项目新增标准测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  // 测试基本运算的准确性
  
  // 加法测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-15, 5))
  assert_eq(1000, add(500, 500))
  
  // 乘法测试
  assert_eq(42, multiply(6, 7))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-4, 6))
  assert_eq(25, multiply(-5, -5))
  
  // 减法测试
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(50, 50))
  assert_eq(20, subtract(-10, -30))
}

test "divide_with_ceil_comprehensive_testing" {
  // 向上取整除法综合测试
  // 测试各种边界情况和数值范围
  
  // 正常除法情况
  assert_eq(4, divide_with_ceil(13, 4))
  assert_eq(3, divide_with_ceil(12, 4))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 50))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  
  // 负数测试
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-3, divide_with_ceil(7, -4))
  assert_eq(2, divide_with_ceil(-7, -4))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_multilingual_support" {
  // 问候函数多语言支持测试
  // 测试不同语言和特殊字符的处理
  
  // 英文测试
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  
  // 中文测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 用户!", greet("用户"))
  
  // 数字和特殊字符
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  
  // 空字符串和空格
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_calculation_scenario" {
  // 复杂业务计算场景测试
  // 模拟真实世界的计算需求
  
  // 场景：订单总价计算
  let unit_price = 25
  let quantity = 8
  let discount = 15
  let tax_rate = 8
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  assert_eq(200, subtotal)
  
  // 计算折扣后价格
  let discounted_price = subtract(subtotal, discount)
  assert_eq(185, discounted_price)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(discounted_price, tax_rate), 100)
  assert_eq(15, tax_amount)
  
  // 计算总价
  let total_price = add(discounted_price, tax_amount)
  assert_eq(200, total_price)
}

test "mathematical_sequence_calculations" {
  // 数学序列计算测试
  // 测试等差数列和等比数列的计算
  
  // 等差数列：3, 7, 11, 15, 19 (首项3，公差4)
  let a1 = 3
  let d = 4
  let n = 5
  
  let a2 = add(a1, d)
  let a3 = add(a2, d)
  let a4 = add(a3, d)
  let a5 = add(a4, d)
  
  // 计算前5项和
  let sum_5 = add(add(add(add(a1, a2), a3), a4), a5)
  assert_eq(55, sum_5)
  
  // 等比数列：2, 6, 18, 54 (首项2，公比3)
  let g1 = 2
  let r = 3
  let g2 = multiply(g1, r)
  let g3 = multiply(g2, r)
  let g4 = multiply(g3, r)
  
  // 计算前4项和
  let sum_geo = add(add(add(g1, g2), g3), g4)
  assert_eq(80, sum_geo)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 模拟资源分配和优化算法
  
  let total_budget = 1000
  let project_a_cost = 350
  let project_b_cost = 280
  let project_c_cost = 190
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  assert_eq(820, allocated_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, allocated_budget)
  assert_eq(180, remaining_budget)
  
  // 计算可以支持的新项目数量（每个项目需要120预算）
  let new_projects_possible = divide_with_ceil(remaining_budget, 120)
  assert_eq(2, new_projects_possible)
  
  // 计算预算利用率
  let utilization_rate = divide_with_ceil(multiply(allocated_budget, 100), total_budget)
  assert_eq(82, utilization_rate)
}

test "time_based_calculations" {
  // 基于时间的计算测试
  // 模拟项目时间管理和调度
  
  let work_days_per_week = 5
  let weeks_per_month = 4
  let tasks_per_day = 3
  let project_duration_months = 3
  
  // 计算每月工作天数
  let work_days_per_month = multiply(work_days_per_week, weeks_per_month)
  assert_eq(20, work_days_per_month)
  
  // 计算项目总天数
  let total_project_days = multiply(work_days_per_month, project_duration_months)
  assert_eq(60, total_project_days)
  
  // 计算项目总任务数
  let total_tasks = multiply(total_project_days, tasks_per_day)
  assert_eq(180, total_tasks)
  
  // 计算需要的团队规模（假设每人每天完成2个任务）
  let tasks_per_person_per_day = 2
  let team_size_needed = divide_with_ceil(tasks_per_day, tasks_per_person_per_day)
  assert_eq(2, team_size_needed)
}

test "data_analysis_simulation" {
  // 数据分析模拟测试
  // 模拟统计分析和数据处理
  
  // 模拟数据集：[12, 15, 18, 21, 24, 27, 30]
  let data_point1 = 12
  let data_point2 = 15
  let data_point3 = 18
  let data_point4 = 21
  let data_point5 = 24
  let data_point6 = 27
  let data_point7 = 30
  
  // 计算总和
  let sum = add(add(add(add(add(add(data_point1, data_point2), data_point3), data_point4), data_point5), data_point6), data_point7)
  assert_eq(147, sum)
  
  // 计算平均值（向上取整）
  let count = 7
  let mean = divide_with_ceil(sum, count)
  assert_eq(21, mean)
  
  // 计算中位数近似值
  let median_approx = divide_with_ceil(add(data_point4, data_point5), 2)
  assert_eq(23, median_approx)
  
  // 计算范围
  let range = subtract(data_point7, data_point1)
  assert_eq(18, range)
}