// Azimuth 项目标准增强测试用例
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

// 导入 azimuth 库
use azimuth

test "enhanced_arithmetic_precision" {
  // 增强算术精度测试
  assert_eq(1000001, add(1000000, 1))
  assert_eq(-999999, add(-1000000, 1))
  assert_eq(250000, multiply(500, 500))
  assert_eq(-250000, multiply(-500, 500))
  assert_eq(250000, multiply(-500, -500))
}

test "divide_with_ceil_comprehensive_coverage" {
  // 向上取整除法全面覆盖测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(999, 10))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(50, 0))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello,  !", greet("  "))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, CAPS123!", greet("CAPS123"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, émoji🎉!", greet("émoji🎉"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 17
  let b = 23
  let c = 5
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "complex_logistics_calculation" {
  // 复杂物流计算测试
  let total_packages = 347
  let truck_capacity = 48
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(8, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(384, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_packages)
  assert_eq(37, remaining_space)
  
  // 计算空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_packages, 100), total_capacity)
  assert_eq(91, utilization_percent)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  
  // 极值运算
  let large_num = 100000
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, add(large_num, 0))
  assert_eq(0, subtract(large_num, large_num))
}

test "business_scenario_event_planning" {
  // 业务场景：活动策划测试
  let attendees = 256
  let tables_available = 30
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(32, tables_needed)
  
  // 检查桌子可用性
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, budget_per_chair)
  assert_eq(6400, total_cost)
  
  // 验证预算（这里会超出预算，测试验证逻辑）
  let budget_exceeded = total_cost > total_budget
  assert_true(budget_exceeded)
  
  // 计算预算差额
  let budget_shortage = subtract(total_cost, total_budget)
  assert_eq(400, budget_shortage)
}

test "performance_calculation_workflow" {
  // 性能计算工作流测试
  let cpu_cores = 8
  let threads_per_core = 2
  let tasks_per_thread = 25
  let task_duration_minutes = 5
  let total_time_limit_minutes = 480
  
  // 计算总线程数
  let total_threads = multiply(cpu_cores, threads_per_core)
  assert_eq(16, total_threads)
  
  // 计算总任务数
  let total_tasks = multiply(total_threads, tasks_per_thread)
  assert_eq(400, total_tasks)
  
  // 计算总执行时间
  let total_execution_time = multiply(total_tasks, task_duration_minutes)
  assert_eq(2000, total_execution_time)
  
  // 计算需要的执行周期（每个周期不超过时间限制）
  let execution_cycles = divide_with_ceil(total_execution_time, total_time_limit_minutes)
  assert_eq(5, execution_cycles)
  
  // 计算每个周期的平均任务数
  let tasks_per_cycle = divide_with_ceil(total_tasks, execution_cycles)
  assert_eq(80, tasks_per_cycle)
}