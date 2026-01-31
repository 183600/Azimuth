// 新增的高质量 MoonBit 测试用例
// 测试 azimuth 库的核心功能和边界情况

test "add_function_comprehensive_test" {
  // 加法函数综合测试
  azimuth::assert_eq(10, azimuth::add(3, 7))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-10, azimuth::add(-3, -7))
  azimuth::assert_eq(4, azimuth::add(-3, 7))
  azimuth::assert_eq(1000000, azimuth::add(500000, 500000))
  azimuth::assert_eq(1, azimuth::add(1, 0))
  azimuth::assert_eq(-1, azimuth::add(-1, 0))
}

test "multiply_function_extensive_test" {
  // 乘法函数扩展测试
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
}

test "divide_with_ceil_business_scenarios" {
  // 向上取整除法的业务场景测试
  // 场景1：计算需要的运输车辆
  assert_eq(5, divide_with_ceil(47, 10))
  
  // 场景2：计算需要的存储容器
  assert_eq(3, divide_with_ceil(25, 8))
  
  // 场景3：计算项目周数
  assert_eq(7, divide_with_ceil(30, 5))
  
  // 边界情况：除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_unicode_and_special_chars" {
  // 问候函数的Unicode和特殊字符测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculation_pipeline_test" {
  // 复杂计算流水线测试
  // 场景：计算项目成本和时间
  let team_size = 5
  let daily_rate = 100
  let project_days = 12
  let tasks_per_day = 8
  let total_tasks = 100
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(project_days, 8))
  assert_eq(480, total_hours)
  
  // 计算总成本
  let total_cost = multiply(total_hours, daily_rate)
  assert_eq(48000, total_cost)
  
  // 计算需要的实际天数
  let days_needed = divide_with_ceil(total_tasks, multiply(team_size, tasks_per_day))
  assert_eq(3, days_needed)
  
  // 计算剩余任务
  let capacity = multiply(days_needed, multiply(team_size, tasks_per_day))
  let remaining = subtract(capacity, total_tasks)
  assert_eq(20, remaining)
}

test "mathematical_properties_and_identities" {
  // 数学性质和恒等式测试
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法性质
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "edge_cases_and_boundary_conditions" {
  // 边界条件和极值测试
  // 极小值测试
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(999, 1000))
  assert_eq(1000, divide_with_ceil(1000, 1))
  assert_eq(1, divide_with_ceil(1000, 1000))
  assert_eq(2, divide_with_ceil(1001, 1000))
  
  // 负数边界测试
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
}

test "resource_allocation_and_optimization" {
  // 资源分配和优化测试
  // 场景：预算分配
  let total_budget = 10000
  let departments = 3
  
  // 计算最小分配
  let min_allocation = divide_with_ceil(total_budget, departments)
  assert_eq(3334, min_allocation)
  
  // 验证总分配
  let total_allocated = multiply(min_allocation, departments)
  assert_eq(10002, total_allocated)
  
  // 计算超额分配
  let over_allocation = subtract(total_allocated, total_budget)
  assert_eq(2, over_allocation)
  
  // 场景：库存管理
  let items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(items, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(11, remaining_space)
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算测试
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, add(large_num, 0))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 字符串边界测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, a!", greet("a"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, verylongstringwithoutspaces!", greet("verylongstringwithoutspaces"))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 场景1：活动安排
  let participants = 87
  let tables = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= participants)
  
  // 场景2：生产计划
  let daily_target = 100
  let workers = 8
  let hours_per_day = 8
  let efficiency = 75  // 75%
  
  // 计算实际日产量
  let actual_daily = multiply(workers, multiply(hours_per_day, divide_with_ceil(efficiency, 100)))
  assert_eq(48, actual_daily)
  
  // 计算需要的额外班次
  let additional_shifts = divide_with_ceil(subtract(daily_target, actual_daily), actual_daily)
  assert_eq(2, additional_shifts)
  
  // 计算总产量
  let total_production = multiply(add(1, additional_shifts), actual_daily)
  assert_eq(144, total_production)
  
  // 验证达到目标
  assert_true(total_production >= daily_target)
}