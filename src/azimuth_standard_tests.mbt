// 标准 MoonBit 测试用例 - 简化版
// 为 azimuth 项目添加的高质量测试用例

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(x, add(x, 0))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "string_greeting_international" {
  // 字符串问候国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, English!", greet("English"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "business_inventory_calculation" {
  // 库存管理业务计算测试
  let product_a_qty = 45
  let product_b_qty = 32
  let product_c_qty = 67
  let box_capacity = 24
  
  // 计算总数量
  let total_quantity = add(add(product_a_qty, product_b_qty), product_c_qty)
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_quantity, box_capacity)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  
  // 计算剩余空间
  let remaining_space = add(total_capacity, -total_quantity)
  
  assert_eq(144, total_quantity)
  assert_eq(6, boxes_needed)
  assert_eq(144, total_capacity)
  assert_eq(0, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 4
  let divisor = 7
  let adjustment = 12
  
  // 计算流水线：((base * multiplier) / divisor) + adjustment
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, adjustment)
  
  // 验证中间步骤
  assert_eq(60, step1)
  assert_eq(9, step2)
  assert_eq(21, result)
}

test "edge_cases_and_boundary_values" {
  // 边界条件和极值测试
  // 零值运算
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 单位运算
  assert_eq(10, add(10, 0))
  assert_eq(10, multiply(10, 1))
  assert_eq(1, divide_with_ceil(10, 10))
  
  // 极值测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "project_planning_calculation" {
  // 项目规划计算测试
  let team_members = 8
  let tasks_per_person = 5
  let sprint_duration = 2
  let total_tasks = 150
  let buffer_percentage = 20
  
  // 计算团队每期能力
  let sprint_capacity = multiply(team_members, tasks_per_person)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 计算缓冲时间
  let buffer_sprints = divide_with_ceil(multiply(sprints_needed, buffer_percentage), 100)
  
  // 计算总项目时间
  let total_sprints = add(sprints_needed, buffer_sprints)
  let total_weeks = multiply(total_sprints, sprint_duration)
  
  assert_eq(40, sprint_capacity)
  assert_eq(4, sprints_needed)
  assert_eq(1, buffer_sprints)
  assert_eq(5, total_sprints)
  assert_eq(10, total_weeks)
}

test "financial_loan_calculation" {
  // 金融贷款计算测试
  let loan_amount = 50000
  let annual_interest_rate = 6
  let loan_term_years = 5
  
  // 计算总利息（简化计算）
  let total_interest = divide_with_ceil(multiply(loan_amount, annual_interest_rate), 100)
  let total_interest_over_term = multiply(total_interest, loan_term_years)
  
  // 计算总还款金额
  let total_repayment = add(loan_amount, total_interest_over_term)
  
  // 计算月供
  let total_months = multiply(loan_term_years, 12)
  let monthly_payment = divide_with_ceil(total_repayment, total_months)
  
  assert_eq(3000, total_interest)
  assert_eq(15000, total_interest_over_term)
  assert_eq(65000, total_repayment)
  assert_eq(1083, monthly_payment)
}

test "algorithm_efficiency_comparison" {
  // 算法效率比较测试
  let input_size = 1000
  
  // 模拟O(n)算法
  let linear_operations = multiply(input_size, 3)
  
  // 模拟O(log n)算法（简化为除法）
  let logarithmic_operations = divide_with_ceil(input_size, 10)
  
  // 模拟O(n²)算法（简化为乘法）
  let quadratic_operations = divide_with_ceil(multiply(input_size, input_size), 1000)
  
  // 计算效率比率
  let linear_to_log_ratio = divide_with_ceil(linear_operations, logarithmic_operations)
  let quadratic_to_linear_ratio = divide_with_ceil(quadratic_operations, linear_operations)
  
  assert_eq(3000, linear_operations)
  assert_eq(100, logarithmic_operations)
  assert_eq(1000, quadratic_operations)
  assert_eq(30, linear_to_log_ratio)
  assert_eq(1, quadratic_to_linear_ratio)
}