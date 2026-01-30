// 标准 MoonBit 测试套件 - 包含10个高质量测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  let result1 = add(7, 8)
  result1 = 15
  
  let result2 = add(-5, 3)
  result2 = -2
  
  let result3 = add(5, -5)
  result3 = 0
  
  let result4 = multiply(7, 8)
  result4 = 56
  
  let result5 = multiply(-5, 3)
  result5 = -15
  
  let result6 = multiply(5, 0)
  result6 = 0
  
  let result7 = subtract(10, 7)
  result7 = 3
  
  let result8 = subtract(5, 13)
  result8 = -8
  
  let result9 = subtract(7, 7)
  result9 = 0
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  let result1 = divide_with_ceil(10, 3)
  result1 = 4
  
  let result2 = divide_with_ceil(9, 3)
  result2 = 3
  
  let result3 = divide_with_ceil(1, 100)
  result3 = 1
  
  let result4 = divide_with_ceil(0, 5)
  result4 = 0
  
  let result5 = divide_with_ceil(10, 0)
  result5 = 0
  
  // 负数测试
  let result6 = divide_with_ceil(-10, 3)
  result6 = -3
  
  let result7 = divide_with_ceil(10, -3)
  result7 = -4
  
  let result8 = divide_with_ceil(-10, -3)
  result8 = 4
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  let result1 = greet("World")
  result1 = "Hello, World!"
  
  let result2 = greet("MoonBit")
  result2 = "Hello, MoonBit!"
  
  let result3 = greet("世界")
  result3 = "Hello, 世界!"
  
  let result4 = greet("🚀")
  result4 = "Hello, 🚀!"
  
  let result5 = greet("café")
  result5 = "Hello, café!"
  
  let result6 = greet("")
  result6 = "Hello, !"
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  let result1 = add(x, y)
  let result2 = add(y, x)
  result1 = result2
  
  // 乘法交换律
  let result3 = multiply(x, y)
  let result4 = multiply(y, x)
  result3 = result4
  
  // 分配律
  let result5 = multiply(x, add(y, 3))
  let result6 = add(multiply(x, y), multiply(x, 3))
  result5 = result6
  
  // 结合律
  let result7 = add(add(x, y), 3)
  let result8 = add(x, add(y, 3))
  result7 = result8
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库货物装箱
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  boxes_needed = 7
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  total_capacity = 168
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  remaining_space = 11
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 5
  let days_per_task = 2
  let buffer_days = 3
  
  // 计算总任务数
  let total_tasks = multiply(team_members, tasks_per_member)
  total_tasks = 40
  
  // 计算基础天数
  let base_days = multiply(total_tasks, days_per_task)
  base_days = 80
  
  // 计算总项目天数
  let total_days = add(base_days, buffer_days)
  total_days = 83
  
  // 计算需要的冲刺数（每冲刺15天）
  let sprints_needed = divide_with_ceil(total_days, 15)
  sprints_needed = 6
}

test "error_handling_and_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误处理
  let result1 = divide_with_ceil(10, 0)
  result1 = 0
  
  let result2 = divide_with_ceil(-10, 0)
  result2 = 0
  
  let result3 = divide_with_ceil(0, 0)
  result3 = 0
  
  // 极值测试
  let result4 = divide_with_ceil(1, 1)
  result4 = 1
  
  let result5 = divide_with_ceil(-1, -1)
  result5 = -1
  
  let result6 = divide_with_ceil(0, 1)
  result6 = 0
  
  let result7 = divide_with_ceil(0, -1)
  result7 = 0
  
  // 大数测试
  let result8 = multiply(1000, 1000)
  result8 = 1000000
  
  let result9 = add(1000000, 1000000)
  result9 = 2000000
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  let result1 = greet("Developer")
  result1 = "Hello, Developer!"
  
  let result2 = greet("123")
  result2 = "Hello, 123!"
  
  let result3 = greet("@#$%")
  result3 = "Hello, @#$%"
  
  let result4 = greet("mixedCASE")
  result4 = "Hello, mixedCASE!"
  
  let result5 = greet("space test")
  result5 = "Hello, space test!"
  
  let result6 = greet("  ")
  result6 = "Hello,   !"
  
  let result7 = greet("trailing")
  result7 = "Hello, trailing!"
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：预算分配
  let total_budget = 25000
  let departments = 4
  
  // 平均分配（向上取整确保覆盖）
  let base_allocation = divide_with_ceil(total_budget, departments)
  base_allocation = 6250
  
  // 验证总分配
  let total_allocated = multiply(base_allocation, departments)
  total_allocated = 25000
  
  // 特殊部门额外分配
  let special_dept_extra = 1500
  let adjusted_total = add(total_allocated, special_dept_extra)
  adjusted_total = 26500
  
  // 重新计算平均分配
  let new_average = divide_with_ceil(adjusted_total, departments)
  new_average = 6625
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议安排
  let attendees = 125
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  tables_needed = 16
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  total_seats = 128
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  remaining_seats = 3
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  total_cost = 3200
  
  // 计算预算差额
  let budget_difference = subtract(total_cost, total_budget)
  budget_difference = 200
  
  // 验证成本合理性（成本不应超过预算的20%）
  let acceptable_overhead = divide_with_ceil(multiply(total_budget, 20), 100)
  let is_reasonable = budget_difference <= acceptable_overhead
  is_reasonable = true
}