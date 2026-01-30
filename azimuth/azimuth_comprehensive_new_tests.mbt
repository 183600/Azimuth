// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  add(7, 8)
  add(-10, 4)
  add(123, -123)
  
  multiply(7, 8)
  multiply(-5, 8)
  multiply(123, 0)
  
  divide_with_ceil(15, 4)
  divide_with_ceil(12, 4)
  divide_with_ceil(10, 0)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 24
  let y = 18
  
  // 加法交换律和结合律
  assert_eq(add(x, y), add(y, x))
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法交换律和结合律
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(123, add(123, 0))
  assert_eq(0, multiply(123, 0))
  assert_eq(0, divide_with_ceil(0, 123))
  
  // 单位元素测试
  assert_eq(123, multiply(123, 1))
  assert_eq(123, add(123, 0))
  
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "string_processing_international" {
  // 字符串处理国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, 12345!", greet("12345"))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库库存管理
  let total_products = 347
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(15, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(360, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(13, remaining_space)
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(97, utilization_percent)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 负数除法测试
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(-2, divide_with_ceil(10, -5))
  assert_eq(3, divide_with_ceil(-10, -4))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_size = 8
  let sprints = 5
  let hours_per_sprint = 40
  let tasks_per_hour = 2
  let total_tasks = 320
  
  // 计算团队总工时
  let total_hours = multiply(team_size, multiply(sprints, hours_per_sprint))
  assert_eq(1600, total_hours)
  
  // 计算总任务容量
  let total_capacity = multiply(total_hours, tasks_per_hour)
  assert_eq(3200, total_capacity)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, multiply(team_size, multiply(hours_per_sprint, tasks_per_hour)))
  assert_eq(1, sprints_needed)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：计算资源分配
  let total_budget = 50000
  let departments = 7
  let min_budget_per_dept = divide_with_ceil(total_budget, departments)
  
  assert_eq(7143, min_budget_per_dept)
  
  // 验证总分配
  let total_allocated = multiply(min_budget_per_dept, departments)
  assert_eq(50001, total_allocated)
  
  // 计算额外需要的预算
  let additional_needed = subtract(total_allocated, total_budget)
  assert_eq(1, additional_needed)
}

test "performance_and_scalability" {
  // 性能和可扩展性测试
  // 大数运算测试
  let large_num1 = 100000
  let large_num2 = 200000
  
  assert_eq(300000, add(large_num1, large_num2))
  assert_eq(20000000000, multiply(large_num1, large_num2))
  assert_eq(1, divide_with_ceil(large_num1, large_num2))
  
  // 批量计算模拟
  let batch_size = 1000
  let batch_count = 50
  let total_items = multiply(batch_size, batch_count)
  let processed_per_second = 150
  let seconds_needed = divide_with_ceil(total_items, processed_per_second)
  
  assert_eq(50000, total_items)
  assert_eq(334, seconds_needed)
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试
  // 场景：活动策划和预算管理
  let attendees = 250
  let tables = 15
  let chairs_per_table = 8
  let cost_per_chair = 25
  let cost_per_table = 80
  let total_budget = 8000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(32, tables_needed)
  
  // 检查桌子可用性
  assert_true(tables_needed <= tables)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let chair_cost = multiply(total_seats, cost_per_chair)
  let table_cost = multiply(tables_needed, cost_per_table)
  let total_cost = add(chair_cost, table_cost)
  
  assert_eq(6400, chair_cost)
  assert_eq(2560, table_cost)
  assert_eq(8960, total_cost)
  
  // 验证预算充足性
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(-960, remaining_budget)
}