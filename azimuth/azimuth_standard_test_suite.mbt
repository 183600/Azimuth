// Azimuth 标准测试套件 - 10个高质量 MoonBit 测试用例
// 覆盖核心功能、边界情况和实际应用场景

test "basic_arithmetic_comprehensive" {
  // 基础算术综合测试
  assert_eq(15, add(8, 7))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(0, 0))
  assert_eq(24, multiply(6, 4))
  assert_eq(-18, multiply(-6, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(5, subtract(12, 7))
  assert_eq(-10, subtract(3, 13))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greeting_function_international" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 42
  let y = 17
  let z = 5
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：计算仓库存储需求
  let total_products = 157
  let boxes_per_pallet = 24
  let pallets_needed = divide_with_ceil(total_products, boxes_per_pallet)
  
  assert_eq(7, pallets_needed)
  
  // 计算总存储容量
  let total_capacity = multiply(pallets_needed, boxes_per_pallet)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "financial_calculation_scenario" {
  // 金融计算场景测试
  // 场景：贷款分期计算
  let loan_amount = 25000
  let monthly_payment = 1200
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  
  assert_eq(22, months_needed)
  
  // 计算总支付金额
  let total_payment = multiply(months_needed, monthly_payment)
  assert_eq(26400, total_payment)
  
  // 计算最后一个月的实际支付金额
  let final_payment = subtract(total_payment, multiply(months_needed - 1, monthly_payment))
  assert_eq(600, final_payment)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源规划
  let team_size = 8
  let tasks_per_person = 5
  let days_per_task = 2
  let working_hours_per_day = 8
  
  // 计算总任务数
  let total_tasks = multiply(team_size, tasks_per_person)
  assert_eq(40, total_tasks)
  
  // 计算总天数
  let total_days = multiply(total_tasks, days_per_task)
  assert_eq(80, total_days)
  
  // 计算总工时
  let total_hours = multiply(total_days, working_hours_per_day)
  assert_eq(640, total_hours)
  
  // 计算需要的周数（每周40小时）
  let weeks_needed = divide_with_ceil(total_hours, 40)
  assert_eq(16, weeks_needed)
}

test "resource_optimization_problem" {
  // 资源优化问题测试
  // 场景：服务器资源分配
  let total_memory_gb = 128
  let servers_count = 5
  let memory_per_server = divide_with_ceil(total_memory_gb, servers_count)
  
  assert_eq(26, memory_per_server)
  
  // 计算实际分配的总内存
  let total_allocated = multiply(memory_per_server, servers_count)
  assert_eq(130, total_allocated)
  
  // 计算内存浪费
  let memory_waste = subtract(total_allocated, total_memory_gb)
  assert_eq(2, memory_waste)
  
  // 优化方案：增加一台服务器
  let optimized_servers = add(servers_count, 1)
  let optimized_memory_per_server = divide_with_ceil(total_memory_gb, optimized_servers)
  assert_eq(19, optimized_memory_per_server)
  
  let optimized_total_allocated = multiply(optimized_memory_per_server, optimized_servers)
  let optimized_memory_waste = subtract(optimized_total_allocated, total_memory_gb)
  assert_eq(6, optimized_memory_waste)
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议活动安排
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 验证桌子数量是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
  
  // 计算每桌实际人数（平均分配）
  let avg_per_table = divide_with_ceil(participants, tables_needed)
  assert_eq(8, avg_per_table)
  
  // 计算未使用的桌子
  let unused_tables = subtract(tables_available, tables_needed)
  assert_eq(1, unused_tables)
}