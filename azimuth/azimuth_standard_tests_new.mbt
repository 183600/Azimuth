// Azimuth 项目标准测试套件 - 10个高质量测试用例
// 使用标准 MoonBit 测试语法

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(7, 0))
  assert_eq(24, multiply(-3, -8))
  assert_eq(-21, multiply(3, -7))
  
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
  
  // 恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
  assert_eq(0, subtract(a, a))
}

test "boundary_values_and_extremes" {
  // 边界值和极值测试
  // 极小值测试
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(0, 1000))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 大数测试（安全范围内）
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
  assert_eq(10001, divide_with_ceil(10001, 1))
}

test "negative_numbers_comprehensive" {
  // 负数综合处理测试
  // 加法负数测试
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  assert_eq(-1, add(7, -8))
  
  // 乘法负数测试
  assert_eq(56, multiply(-7, -8))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(-56, multiply(7, -8))
  assert_eq(0, multiply(-7, 0))
  
  // 减法负数测试
  assert_eq(1, subtract(-7, -8))
  assert_eq(-15, subtract(-7, 8))
  assert_eq(15, subtract(7, -8))
  
  // 除法负数测试
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(-2, divide_with_ceil(7, -4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库存储优化
  let total_products = 157
  let box_capacity = 24
  let available_boxes = 10
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 验证箱子是否足够
  assert_true(boxes_needed <= available_boxes)
  
  // 计算总存储容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 计算空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(94, utilization_percent)  // floor(15700/168) = 93%
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 8
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  let tasks_per_sprint = 100
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算总任务数
  let total_tasks = multiply(total_hours, tasks_per_hour)
  assert_eq(1920, total_tasks)  // 960 * 2 = 1920
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(20, sprints_needed)  // ceil(1920/100) = 20
  
  // 计算实际处理能力
  let actual_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(2000, actual_capacity)
  
  // 计算剩余任务槽位
  let remaining_slots = subtract(actual_capacity, total_tasks)
  assert_eq(80, remaining_slots)
}

test "error_handling_resilience" {
  // 错误处理和恢复能力测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(9999, 0))
  assert_eq(0, divide_with_ceil(-9999, 0))
  
  // 极值运算测试
  let max_safe_value = 1000000
  let min_safe_value = -1000000
  
  assert_eq(0, multiply(max_safe_value, 0))
  assert_eq(0, multiply(min_safe_value, 0))
  assert_eq(max_safe_value, add(max_safe_value, 0))
  assert_eq(min_safe_value, add(min_safe_value, 0))
  
  // 边界减法测试
  assert_eq(0, subtract(max_safe_value, max_safe_value))
  assert_eq(0, subtract(min_safe_value, min_safe_value))
  assert_eq(2000000, subtract(max_safe_value, min_safe_value))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  // 场景：会议安排
  let attendees = 250
  let tables_available = 30
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(32, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, budget_per_chair)
  assert_eq(6400, total_cost)
  
  // 验证预算（这里会超出预算，用于测试错误处理）
  assert_true(total_cost > total_budget)
  
  // 计算预算缺口
  let budget_shortage = subtract(total_cost, total_budget)
  assert_eq(400, budget_shortage)
}

test "performance_resource_optimization" {
  // 性能和资源优化测试
  // 场景：计算资源分配
  let total_cpu_cores = 64
  let services = 7
  let min_cores_per_service = 4
  let max_cores_per_service = 16
  
  // 计算每个服务的最小核心数
  let min_cores_total = multiply(services, min_cores_per_service)
  assert_eq(28, min_cores_total)
  
  // 验证资源充足
  assert_true(total_cpu_cores >= min_cores_total)
  
  // 计算平均可用核心数
  let avg_cores_per_service = divide_with_ceil(total_cpu_cores, services)
  assert_eq(10, avg_cores_per_service)  // ceil(64/7) = 10
  
  // 验证平均分配合理
  assert_true(avg_cores_per_service >= min_cores_per_service)
  assert_true(avg_cores_per_service <= max_cores_per_service)
  
  // 计算最大可能服务数
  let max_possible_services = divide_with_ceil(total_cpu_cores, min_cores_per_service)
  assert_eq(16, max_possible_services)  // ceil(64/4) = 16
  
  // 计算资源利用率
  let utilization_percent = divide_with_ceil(multiply(min_cores_total, 100), total_cpu_cores)
  assert_eq(44, utilization_percent)  // floor(2800/64) = 43%
  
  // 计算剩余核心数
  let remaining_cores = subtract(total_cpu_cores, min_cores_total)
  assert_eq(36, remaining_cores)
}