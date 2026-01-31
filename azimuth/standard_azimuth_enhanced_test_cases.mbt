// 标准 Azimuth 增强测试用例
// 包含10个高质量的 MoonBit 测试用例，覆盖各种场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(7, 5))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 4)),
    add(multiply(x, y), multiply(x, 4))
  )
  
  // 减法性质验证
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(add(x, y), y))
}

test "boundary_conditions_and_extreme_values" {
  // 边界条件和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(5, subtract(5, 0))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 大数安全测试
  assert_eq(2000, add(1000, 1000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "negative_number_comprehensive_handling" {
  // 负数综合处理测试
  // 加法负数测试
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(-8, add(-5, -3))
  
  // 乘法负数测试
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  
  // 减法负数测试
  assert_eq(-8, subtract(-5, 3))
  assert_eq(8, subtract(3, -5))
  
  // 除法负数测试
  assert_eq(-1, divide_with_ceil(-5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "string_processing_and_greeting_functionality" {
  // 字符串处理和问候功能测试
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  
  // 特殊字符测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  
  // Unicode和国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "business_logic_inventory_management" {
  // 业务逻辑：库存管理测试
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  // 验证需要的箱子数量
  assert_eq(7, boxes_needed)  // ceil(158/24) = 7
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(10, remaining_space)
  
  // 验证空间利用率
  let utilization_percentage = divide_with_ceil(
    multiply(total_products, 100), 
    total_capacity
  )
  assert_eq(94, utilization_percentage)  // floor(15800/168) = 94%
}

test "complex_calculation_pipeline_workflow" {
  // 复杂计算流水线工作流测试
  // 场景：项目资源规划
  let team_size = 6
  let hours_per_day = 8
  let working_days = 15
  let tasks_per_hour = 2
  let total_tasks = 150
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, working_days))
  assert_eq(720, total_hours)  // 6 * 8 * 15 = 720
  
  // 计算总任务处理能力
  let total_capacity = multiply(total_hours, tasks_per_hour)
  assert_eq(1440, total_capacity)  // 720 * 2 = 1440
  
  // 计算需要的项目周期（基于任务数量）
  let daily_task_capacity = multiply(team_size, multiply(hours_per_day, tasks_per_hour))
  let days_needed = divide_with_ceil(total_tasks, daily_task_capacity)
  assert_eq(2, days_needed)  // ceil(150/(6*8*2)) = 2
  
  // 验证任务完成能力
  let actual_tasks_completed = multiply(days_needed, daily_task_capacity)
  assert_eq(192, actual_tasks_completed)
}

test "resource_allocation_optimization_scenarios" {
  // 资源分配优化场景测试
  let total_budget = 25000
  let departments = 4
  let emergency_reserve = 3000
  
  // 计算可分配预算
  let available_budget = subtract(total_budget, emergency_reserve)
  assert_eq(22000, available_budget)
  
  // 计算每个部门的最大分配额
  let max_per_department = divide_with_ceil(available_budget, departments)
  assert_eq(5500, max_per_department)  // ceil(22000/4) = 5500
  
  // 验证总分配额
  let total_allocated = multiply(max_per_department, departments)
  assert_eq(22000, total_allocated)
  
  // 验证预算平衡
  let final_reserve = add(emergency_reserve, subtract(total_allocated, available_budget))
  assert_eq(3000, final_reserve)
}

test "error_handling_and_input_validation" {
  // 错误处理和输入验证测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(1000000, 0))
  
  // 极值运算验证
  let large_number = 100000
  assert_eq(200000, add(large_number, large_number))
  assert_eq(0, multiply(large_number, 0))
  assert_eq(large_number, multiply(large_number, 1))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(100000, divide_with_ceil(large_number, 1))
}

test "real_world_event_planning_comprehensive" {
  // 真实世界活动规划综合测试
  let attendees = 256
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let venue_capacity = 300
  let total_budget = 8000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(32, tables_needed)  // ceil(256/8) = 32
  
  // 验证桌子可用性
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(0, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6400, total_cost)
  
  // 验证预算充足性
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(1600, remaining_budget)
  
  // 验证场地容量限制
  assert_true(attendees <= venue_capacity)
  
  // 计算场地利用率
  let venue_utilization = divide_with_ceil(multiply(attendees, 100), venue_capacity)
  assert_eq(85, venue_utilization)  // floor(25600/300) = 85%
}