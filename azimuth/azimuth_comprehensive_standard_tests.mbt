// Azimuth 项目综合标准测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例
// 覆盖基本功能、边界情况、错误处理和实际应用场景

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  add(15, 25) = 40
  add(-10, 5) = -5
  add(0, 100) = 100
  
  multiply(6, 7) = 42
  multiply(-3, 8) = -24
  multiply(0, 999) = 0
  
  subtract(50, 20) = 30
  subtract(10, 30) = -20
  subtract(0, 0) = 0
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  divide_with_ceil(10, 3) = 4
  divide_with_ceil(15, 5) = 3
  divide_with_ceil(7, 2) = 4
  divide_with_ceil(0, 8) = 0
  divide_with_ceil(25, 0) = 0
  divide_with_ceil(-9, 4) = -2
  divide_with_ceil(9, -4) = -2
  divide_with_ceil(-9, -4) = 3
}

test "greet_function_multilingual" {
  // 问候函数多语言测试
  greet("MoonBit") = "Hello, MoonBit!"
  greet("世界") = "Hello, 世界!"
  greet("🚀") = "Hello, 🚀!"
  greet("") = "Hello, !"
  greet("café") = "Hello, café!"
  greet("Москва") = "Hello, Москва!"
}

test "mathematical_properties_and_identities" {
  // 数学性质和恒等式测试
  // 加法交换律
  add(12, 8) = add(8, 12)
  
  // 乘法交换律
  multiply(7, 9) = multiply(9, 7)
  
  // 加法恒等式
  add(25, 0) = 25
  add(0, 25) = 25
  
  // 乘法恒等式
  multiply(15, 1) = 15
  multiply(1, 15) = 15
  multiply(15, 0) = 0
  
  // 减法性质
  subtract(20, 20) = 0
  subtract(15, 0) = 15
}

test "business_logic_inventory_management" {
  // 库存管理业务逻辑测试
  let total_products = 87
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  boxes_needed = 8
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  total_capacity = 96
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  remaining_space = 9
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目预算计算
  let team_members = 6
  let hourly_rate = 45
  let hours_per_day = 8
  let project_days = 10
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  total_hours = 480
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  total_budget = 21600
  
  // 计算需要的支付周期（每周期不超过5000）
  let payment_cycles = divide_with_ceil(total_budget, 5000)
  payment_cycles = 5
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零错误处理
  divide_with_ceil(10, 0) = 0
  divide_with_ceil(-10, 0) = 0
  divide_with_ceil(0, 0) = 0
  
  // 极值测试
  divide_with_ceil(1, 1) = 1
  divide_with_ceil(-1, -1) = 1
  divide_with_ceil(0, 1) = 0
  divide_with_ceil(1000000, 1) = 1000000
  
  // 大数运算
  add(500000, 500000) = 1000000
  multiply(1000, 1000) = 1000000
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  greet("single") = "Hello, single!"
  greet("space test") = "Hello, space test!"
  greet("123numbers456") = "Hello, 123numbers456!"
  greet("!@#$%^&*()") = "Hello, !@#$%^&*()!"
  greet("MiXeD CaSe") = "Hello, MiXeD CaSe!"
  greet("   leading and trailing spaces   ") = "Hello,    leading and trailing spaces   !"
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  // 场景：计算会议室分配
  let attendees = 65
  let rooms_available = 8
  let capacity_per_room = 10
  
  // 计算需要的房间数量
  let rooms_needed = divide_with_ceil(attendees, capacity_per_room)
  rooms_needed = 7
  
  // 验证房间是否足够
  let total_capacity = multiply(rooms_needed, capacity_per_room)
  total_capacity = 70
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, attendees)
  remaining_capacity = 5
  
  // 验证可用房间充足
  rooms_needed <= rooms_available = true
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议安排计算
  let participants = 120
  let tables = 15
  let chairs_per_table = 8
  let cost_per_participant = 25
  let total_budget = 3500
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  tables_needed = 15
  
  // 验证桌子是否足够
  tables_needed <= tables = true
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  total_seats = 120
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  remaining_seats = 0
  
  // 计算总成本
  let total_cost = multiply(participants, cost_per_participant)
  total_cost = 3000
  
  // 验证预算充足
  total_cost <= total_budget = true
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  remaining_budget = 500
}