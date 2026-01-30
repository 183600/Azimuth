// 高质量 MoonBit 测试用例 - Azimuth 项目
// 包含 8 个精心设计的测试用例，覆盖核心功能和实际应用场景

test "basic_arithmetic_comprehensive" {
  // 综合测试基本算术运算
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-5, 9))
  
  assert_eq(42, multiply(6, 7))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-6, 4))
  assert_eq(36, multiply(-6, -6))
  
  assert_eq(5, subtract(12, 7))
  assert_eq(-5, subtract(7, 12))
  assert_eq(0, subtract(10, 10))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 验证数学性质和恒等式
  let x = 17
  let y = 23
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法交换律和分配律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
  
  // 恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
}

test "real_world_inventory_management" {
  // 真实世界库存管理场景测试
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

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源规划
  let team_members = 8
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  assert_eq(960, total_hours)
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(1920, total_tasks)
  
  // 计算需要的冲刺数（每冲刺400个任务）
  let tasks_per_sprint = 400
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(5, sprints_needed)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数运算
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment_max = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment_max)
  assert_eq(23, months_needed)
  
  // 计算实际总支付
  let actual_monthly_payment = divide_with_ceil(loan_amount, months_needed)
  let total_payment = multiply(actual_monthly_payment, months_needed)
  
  assert_eq(2174, actual_monthly_payment)
  assert_eq(50002, total_payment)
  
  // 验证最后一个月的支付金额
  let regular_payments = months_needed - 1
  let regular_total = multiply(regular_payments, actual_monthly_payment)
  let final_payment = subtract(total_payment, regular_total)
  assert_eq(1456, final_payment)
}