// Azimuth 增强测试用例 - 新的高质量测试用例集
// 包含实际应用场景和边界条件测试

test "advanced_calculator_operations" {
  // 高级计算器操作测试
  let base = 15
  let multiplier = 4
  let divisor = 7
  
  // 复杂计算链
  let step1 = multiply(base, multiplier)  // 15 * 4 = 60
  let step2 = divide_with_ceil(step1, divisor)  // ceil(60/7) = 9
  let step3 = add(step2, base)  // 9 + 15 = 24
  let result = subtract(step3, multiplier)  // 24 - 4 = 20
  
  assert_eq(20, result)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let product_a = 45
  let product_b = 32
  let product_c = 67
  let box_capacity = 20
  
  // 计算每种产品需要的箱子数量
  let boxes_a = divide_with_ceil(product_a, box_capacity)  // ceil(45/20) = 3
  let boxes_b = divide_with_ceil(product_b, box_capacity)  // ceil(32/20) = 2
  let boxes_c = divide_with_ceil(product_c, box_capacity)  // ceil(67/20) = 4
  
  // 计算总箱子数和总产品数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_products = add(add(product_a, product_b), product_c)
  
  assert_eq(9, total_boxes)
  assert_eq(144, total_products)
  
  // 计算总存储容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(180, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(36, remaining_space)
}

test "event_planning_scenario" {
  // 活动规划场景测试
  let guests = 125
  let tables = 15
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(guests, chairs_per_table)  // ceil(125/8) = 16
  assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  let has_enough_tables = tables_needed <= tables
  assert_false(has_enough_tables)  // 需要16张，但只有15张
  
  // 计算实际可容纳的客人数量
  let actual_capacity = multiply(tables, chairs_per_table)
  assert_eq(120, actual_capacity)
  
  // 计算无法容纳的客人数量
  let guests_unseated = subtract(guests, actual_capacity)
  assert_eq(5, guests_unseated)
}

test "financial_budget_allocation" {
  // 财务预算分配测试
  let total_budget = 50000
  let departments = 4
  let reserve_percentage = 10
  
  // 计算储备金
  let reserve_amount = divide_with_ceil(multiply(total_budget, reserve_percentage), 100)
  let available_budget = subtract(total_budget, reserve_amount)
  
  assert_eq(5000, reserve_amount)
  assert_eq(45000, available_budget)
  
  // 计算每个部门的预算
  let budget_per_dept = divide_with_ceil(available_budget, departments)
  assert_eq(11250, budget_per_dept)
  
  // 计算总分配预算
  let total_allocated = multiply(budget_per_dept, departments)
  assert_eq(45000, total_allocated)
  
  // 验证分配正确性
  assert_eq(available_budget, total_allocated)
}

test "temperature_conversion_system" {
  // 温度转换系统测试（简化模拟）
  let celsius_temps = 0
  let conversion_factor = 9
  let divisor = 5
  let offset = 32
  
  // 模拟摄氏度转华氏度：F = (C * 9/5) + 32
  let step1 = multiply(celsius_temps, conversion_factor)
  let step2 = divide_with_ceil(step1, divisor)
  let fahrenheit = add(step2, offset)
  
  assert_eq(32, fahrenheit)  // 0°C = 32°F
  
  // 测试另一个温度点
  let celsius_temps_2 = 100
  let step1_2 = multiply(celsius_temps_2, conversion_factor)
  let step2_2 = divide_with_ceil(step1_2, divisor)
  let fahrenheit_2 = add(step2_2, offset)
  
  assert_eq(212, fahrenheit_2)  // 100°C = 212°F
}

test "shipping_cost_calculator" {
  // 运费计算器测试
  let base_weight = 5
  let additional_items = 3
  let weight_per_item = 2
  let cost_per_kg = 12
  let base_cost = 25
  
  // 计算总重量
  let additional_weight = multiply(additional_items, weight_per_item)
  let total_weight = add(base_weight, additional_weight)
  
  assert_eq(11, total_weight)
  
  // 计算运费
  let weight_cost = multiply(total_weight, cost_per_kg)
  let total_cost = add(base_cost, weight_cost)
  
  assert_eq(132, weight_cost)
  assert_eq(157, total_cost)
}

test "project_timeline_planning" {
  // 项目时间规划测试
  let total_tasks = 87
  let team_members = 5
  let tasks_per_person_per_day = 3
  let work_days_per_week = 5
  
  // 计算每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  assert_eq(15, daily_capacity)
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(6, work_days_needed)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  assert_eq(2, weeks_needed)
  
  // 计算实际处理能力
  let actual_capacity = multiply(work_days_needed, daily_capacity)
  assert_eq(90, actual_capacity)
  
  // 计算剩余处理能力
  let spare_capacity = subtract(actual_capacity, total_tasks)
  assert_eq(3, spare_capacity)
}

test "user_greeting_localization" {
  // 用户问候本地化测试
  let user_names = ["Alice", "Bob", "Charlie", "世界", "🚀"]
  
  // 测试各种用户名的问候
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, Charlie!", greet("Charlie"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  
  // 测试特殊字符和边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, CamelCase!", greet("CamelCase"))
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  let total_resources = 100
  let project_a_needs = 35
  let project_b_needs = 28
  let project_c_needs = 42
  
  // 计算总需求
  let total_needs = add(add(project_a_needs, project_b_needs), project_c_needs)
  assert_eq(105, total_needs)
  
  // 检查资源是否充足
  let is_sufficient = total_resources >= total_needs
  assert_false(is_sufficient)
  
  // 计算资源缺口
  let resource_gap = subtract(total_needs, total_resources)
  assert_eq(5, resource_gap)
  
  // 计算每个项目按比例分配的资源
  let resource_a = divide_with_ceil(multiply(total_resources, project_a_needs), total_needs)
  let resource_b = divide_with_ceil(multiply(total_resources, project_b_needs), total_needs)
  let resource_c = divide_with_ceil(multiply(total_resources, project_c_needs), total_needs)
  
  assert_eq(34, resource_a)
  assert_eq(27, resource_b)
  assert_eq(40, resource_c)
  
  // 验证总分配资源
  let total_allocated = add(add(resource_a, resource_b), resource_c)
  assert_eq(101, total_allocated)
}