// Azimuth 标准测试用例 - 高质量 MoonBit 测试集合
// 包含10个精心设计的测试用例，覆盖各种场景和边界情况

test "basic_arithmetic_comprehensive" {
  // 综合基本算术运算测试
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-10, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(0, multiply(0, 12345))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边缘情况测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(2, divide_with_ceil(-5, -3))
}

test "greet_function_international_support" {
  // 问候函数的国际字符支持测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
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
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
}

test "business_logistics_calculation" {
  // 物流业务计算测试
  // 场景：计算仓库存储需求
  let total_products = 157
  let boxes_per_pallet = 24
  let pallets_needed = divide_with_ceil(total_products, boxes_per_pallet)
  
  assert_eq(7, pallets_needed)
  
  // 验证总存储容量
  let total_capacity = multiply(pallets_needed, boxes_per_pallet)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源分配计算
  let team_size = 6
  let daily_hours = 8
  let project_days = 12
  let tasks_per_hour = 2
  
  // 计算总任务能力
  let daily_capacity = multiply(team_size, multiply(daily_hours, tasks_per_hour))
  let total_capacity = multiply(daily_capacity, project_days)
  
  assert_eq(1152, total_capacity)
  
  // 计算任务分配
  let total_tasks = 1000
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(1, days_needed)
}

test "error_handling_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 大数测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "string_formatting_edge_cases" {
  // 字符串格式化边缘情况测试
  assert_eq_string("Hello, null!", greet("null"))
  assert_eq_string("Hello, undefined!", greet("undefined"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, space between!", greet("space between"))
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 50000
  let monthly_payment = 2200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(23, months_needed)
  
  // 验证总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(50600, total_payment)
  
  // 计算最后一个月的实际支付
  let regular_payments = months_needed - 1
  let regular_total = multiply(regular_payments, monthly_payment)
  let final_payment = subtract(total_payment, regular_total)
  assert_eq(200, final_payment)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：服务器资源分配优化
  let total_memory = 8192  // GB
  let servers = 4
  
  // 初始分配
  let memory_per_server = divide_with_ceil(total_memory, servers)
  assert_eq(2048, memory_per_server)
  
  // 验证分配结果
  let total_allocated = multiply(memory_per_server, servers)
  assert_eq(8192, total_allocated)
  
  // 优化分配：增加服务器
  let optimized_servers = 5
  let optimized_memory = divide_with_ceil(total_memory, optimized_servers)
  assert_eq(1640, optimized_memory)
  
  // 计算优化后的浪费
  let optimized_total = multiply(optimized_memory, optimized_servers)
  let memory_waste = subtract(optimized_total, total_memory)
  assert_eq(108, memory_waste)
}