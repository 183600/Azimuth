// 标准 MoonBit 测试套件
// 为 Azimuth 项目添加的 10 个高质量测试用例
// 覆盖核心功能模块和各种测试场景

test "core_arithmetic_operations" {
  // 核心算术运算测试
  // 测试加法运算的各种情况
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
  
  // 测试乘法运算的各种情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  
  // 测试减法运算的各种情况
  assert_eq(3, subtract(8, 5))
  assert_eq(-3, subtract(5, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(20, subtract(10, -10))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  // 基本正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "string_processing_functionality" {
  // 字符串处理功能测试
  // 基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,  !", greet(" "))
  
  // 特殊字符和Unicode
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, émoji!", greet("émoji"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  let c = 5
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  // 场景：计算装盒需要的数量
  let total_items = 47
  let box_capacity = 12
  let packages_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源管理和时间规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  let days_per_sprint = 5
  let total_project_days = multiply(sprints_needed, days_per_sprint)
  
  assert_eq(48, total_tasks)
  assert_eq(4, sprints_needed)
  assert_eq(20, total_project_days)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 100))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(1, add(1, 0))
  assert_eq(100, multiply(1, 100))
  assert_eq(1, subtract(1, 0))
  assert_eq(100, divide_with_ceil(100, 1))
  
  // 大数测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(100000, 100))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 10000
  let monthly_payment = 850
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(12, months_needed)
  
  // 计算实际支付总额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(10200, total_payment)
  
  // 计算额外利息
  let extra_interest = subtract(total_payment, loan_amount)
  assert_eq(200, extra_interest)
}

test "data_analysis_simulation" {
  // 数据分析模拟测试
  // 场景：统计分析计算
  let data_points = 125
  let samples_per_batch = 20
  let batches_needed = divide_with_ceil(data_points, samples_per_batch)
  
  assert_eq(7, batches_needed)
  
  // 计算总处理容量
  let total_capacity = multiply(batches_needed, samples_per_batch)
  assert_eq(140, total_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, data_points)
  assert_eq(15, remaining_capacity)
  
  // 计算利用率百分比
  let utilization_rate = divide_with_ceil(multiply(data_points, 100), total_capacity)
  assert_eq(89, utilization_rate)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  
  // 极值情况处理
  let max_positive = 1000000
  let max_negative = -1000000
  
  assert_eq(2000000, add(max_positive, max_positive))
  assert_eq(-2000000, add(max_negative, max_negative))
  assert_eq(0, add(max_positive, max_negative))
  
  assert_eq(1000000, multiply(max_positive, max_positive))
  assert_eq(1000000, multiply(max_negative, max_negative))
  assert_eq(-1000000, multiply(max_positive, max_negative))
}