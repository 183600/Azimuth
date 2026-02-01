// 标准 MoonBit 测试用例
// 为 Azimuth 项目添加的10个高质量测试用例
// 使用标准 MoonBit 测试语法

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(24, multiply(6, 4))
  assert_eq(0, multiply(0, 100))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(5, subtract(12, 7))
  assert_eq(-5, subtract(7, 12))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(15, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), 3), multiply(x, multiply(y, 3)))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "business_inventory_calculation" {
  // 库存管理业务计算测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  let principal = 10000
  let interest_rate = 8
  let years = 3
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(10800, year1_amount)
  assert_eq(11664, year2_amount)
  assert_eq(12597, year3_amount)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 25
  let multiplier = 4
  let divisor = 7
  let adjustment = 12
  
  // 计算流水线
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let step3 = add(step2, adjustment)
  let final_result = multiply(step3, 2)
  
  assert_eq(100, step1)
  assert_eq(15, step2)
  assert_eq(27, step3)
  assert_eq(54, final_result)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, multiply(2147483647, 0))
}

test "real_world_project_management" {
  // 真实世界项目管理测试
  let team_members = 8
  let tasks_per_member = 6
  let days_per_sprint = 10
  let total_tasks = 150
  
  // 计算团队总任务数
  let team_capacity = multiply(team_members, tasks_per_member)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, team_capacity)
  
  // 计算总工作天数
  let total_work_days = multiply(sprints_needed, days_per_sprint)
  
  assert_eq(48, team_capacity)
  assert_eq(4, sprints_needed)
  assert_eq(40, total_work_days)
}

test "data_processing_and_analysis" {
  // 数据处理和分析测试
  let data_points = 1000
  let sample_size = 75
  let processing_time_per_sample = 3
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(data_points, sample_size)
  
  // 计算总处理时间
  let total_processing_time = multiply(batches_needed, processing_time_per_sample)
  
  // 计算实际处理的样本数
  let actual_samples_processed = multiply(batches_needed - 1, sample_size)
  let final_batch_size = subtract(data_points, actual_samples_processed)
  
  assert_eq(14, batches_needed)
  assert_eq(42, total_processing_time)
  assert_eq(25, final_batch_size)
}