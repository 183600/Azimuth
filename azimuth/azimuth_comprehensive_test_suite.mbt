// Azimuth 项目综合测试套件 - 10个高质量 MoonBit 测试用例
// 覆盖所有主要功能：add, multiply, subtract, divide_with_ceil, greet
// 包含边界情况、负数测试和实际业务场景

test "comprehensive_arithmetic_operations" {
  // 综合算术运算测试
  // 基本加法测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  
  // 基本乘法测试
  assert_eq(21, multiply(3, 7))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(0, multiply(3, 0))
  
  // 基本减法测试
  assert_eq(5, subtract(12, 7))
  assert_eq(-5, subtract(7, 12))
  assert_eq(0, subtract(8, 8))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  // 基本英文问候
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // Unicode字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  
  // 特殊情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "business_packaging_optimization" {
  // 业务包装优化场景测试
  // 场景：电商仓库需要将商品装入标准箱子
  let total_products = 127
  let box_capacity = 15
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(9, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(135, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(8, remaining_space)
  
  // 计算空间利用率
  let utilization_percent = multiply(divide_with_ceil(multiply(total_products, 100), total_capacity), 1)
  assert_eq(94, utilization_percent)  // floor(12700/135) = 94%
}

test "project_resource_management" {
  // 项目资源管理测试
  // 场景：计算项目所需的人力和时间资源
  let team_members = 7
  let tasks_per_member = 12
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2  // 周
  let tasks_per_sprint = 20
  let total_sprints = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  let project_duration_weeks = multiply(total_sprints, sprint_duration)
  
  assert_eq(84, total_tasks)  // 7 * 12 = 84
  assert_eq(5, total_sprints)  // ceil(84/20) = 5
  assert_eq(10, project_duration_weeks)  // 5 * 2 = 10
  
  // 验证总工作人日
  let total_person_days = multiply(total_tasks, 1)  // 假设每个任务需要1人日
  assert_eq(84, total_person_days)
}

test "financial_budget_calculation" {
  // 财务预算计算测试
  // 场景：计算项目预算和支付计划
  let hourly_rate = 85
  let hours_per_day = 8
  let team_size = 6
  let project_days = 15
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, project_days))
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  
  // 计算支付周期（每周期不超过20000）
  let payment_limit = 20000
  let payment_periods = divide_with_ceil(total_budget, payment_limit)
  
  assert_eq(720, total_hours)  // 6 * 8 * 15 = 720
  assert_eq(61200, total_budget)  // 720 * 85 = 61200
  assert_eq(4, payment_periods)  // ceil(61200/20000) = 4
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极小值测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(0, multiply(-1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000001, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
}

test "negative_number_comprehensive_operations" {
  // 负数综合运算测试
  // 负数基本运算
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-3, 7))
  assert_eq(-4, add(3, -7))
  
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(-21, multiply(3, -7))
  
  assert_eq(4, subtract(-3, -7))
  assert_eq(-10, subtract(-3, 7))
  assert_eq(10, subtract(3, -7))
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "complex_workflow_integration" {
  // 复杂工作流集成测试
  // 场景：完整的产品开发和发布流程
  
  // 1. 开发阶段计算
  let developers = 8
  let features_per_developer = 5
  let total_features = multiply(developers, features_per_developer)
  
  // 2. 测试阶段计算
  let testers = 4
  let features_per_tester = 10
  let testing_capacity = multiply(testers, features_per_tester)
  
  // 3. 发布批次计算
  let batch_size = 12
  let total_batches = divide_with_ceil(total_features, batch_size)
  
  // 4. 时间线计算
  let days_per_batch = 3
  let total_days = multiply(total_batches, days_per_batch)
  
  // 5. 资源验证
  let remaining_testing_capacity = subtract(testing_capacity, total_features)
  
  // 验证计算结果
  assert_eq(40, total_features)  // 8 * 5 = 40
  assert_eq(40, testing_capacity)  // 4 * 10 = 40
  assert_eq(4, total_batches)  // ceil(40/12) = 4
  assert_eq(12, total_days)  // 4 * 3 = 12
  assert_eq(0, remaining_testing_capacity)  // 40 - 40 = 0
  
  // 6. 最终验证问候功能
  let project_name = "Azimuth"
  let success_message = greet(project_name)
  assert_eq_string("Hello, Azimuth!", success_message)
}