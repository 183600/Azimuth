// 质量 MoonBit 标准测试用例
// 为 Azimuth 项目添加的高质量测试覆盖

test "mathematical_commutative_law" {
  // 验证数学交换律
  let a = 17
  let b = 29
  
  // 加法交换律: a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体计算结果
  assert_eq(46, add(17, 29))
  assert_eq(493, multiply(17, 29))
}

test "mathematical_associative_law" {
  // 验证数学结合律
  let a = 5
  let b = 8
  let c = 12
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 验证具体计算结果
  assert_eq(25, add(add(5, 8), 12))
  assert_eq(480, multiply(multiply(5, 8), 12))
}

test "mathematical_distributive_law" {
  // 验证数学分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 9
  let c = 4
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  assert_eq(91, left_side)  // 7 * (9 + 4) = 7 * 13 = 91
}

test "complex_business_inventory" {
  // 复杂业务库存管理场景
  let total_products = 245
  let box_capacity = 18
  let warehouse_sections = 6
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(14, boxes_needed)  // ceil(245/18) = 14
  
  // 计算每个仓库区域的箱子数量
  let boxes_per_section = divide_with_ceil(boxes_needed, warehouse_sections)
  assert_eq(3, boxes_per_section)  // ceil(14/6) = 3
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(252, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(7, remaining_space)
}

test "event_planning_scenario" {
  // 活动规划场景测试
  let attendees = 156
  let tables_available = 20
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(20, tables_needed)  // ceil(156/8) = 20
  
  // 验证桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(160, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(4, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, budget_per_chair)
  assert_eq(4000, total_cost)
  
  // 验证预算是否充足
  assert_true(total_cost <= total_budget)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 零值边界
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(123, 0))
  assert_eq(0, divide_with_ceil(0, 123))
  assert_eq(0, divide_with_ceil(123, 0))
  
  // 单位边界
  assert_eq(1, add(1, 0))
  assert_eq(1, add(0, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
  
  // 负数边界
  assert_eq(-1, add(0, -1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "performance_calculation_workflow" {
  // 性能计算工作流测试
  let cpu_cores = 8
  let threads_per_core = 2
  let tasks_per_thread = 25
  let time_per_task_ms = 50
  
  // 计算总线程数
  let total_threads = multiply(cpu_cores, threads_per_core)
  assert_eq(16, total_threads)
  
  // 计算总任务数
  let total_tasks = multiply(total_threads, tasks_per_thread)
  assert_eq(400, total_tasks)
  
  // 计算总时间（毫秒）
  let total_time_ms = multiply(total_tasks, time_per_task_ms)
  assert_eq(20000, total_time_ms)
  
  // 计算需要的秒数（向上取整）
  let total_seconds = divide_with_ceil(total_time_ms, 1000)
  assert_eq(20, total_seconds)
  
  // 计算每秒处理的任务数
  let tasks_per_second = divide_with_ceil(total_tasks, total_seconds)
  assert_eq(20, tasks_per_second)
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}