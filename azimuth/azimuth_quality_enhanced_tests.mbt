// Azimuth 项目增强测试用例
// 包含10个高质量的标准 MoonBit 测试用例

// 导入被测试的模块
use azimuth

test "enhanced_arithmetic_operations" {
  // 增强算术运算测试
  azimuth.assert_eq(100, azimuth.add(45, 55))
  azimuth.assert_eq(-100, azimuth.add(-45, -55))
  azimuth.assert_eq(10, azimuth.add(-15, 25))
  
  azimuth.assert_eq(1000, azimuth.multiply(10, 100))
  azimuth.assert_eq(-1000, azimuth.multiply(-10, 100))
  azimuth.assert_eq(1000, azimuth.multiply(-10, -100))
  
  azimuth.assert_eq(50, azimuth.subtract(100, 50))
  azimuth.assert_eq(-50, azimuth.subtract(50, 100))
  azimuth.assert_eq(0, azimuth.subtract(50, 50))
}

test "enhanced_divide_with_ceil_operations" {
  // 增强向上取整除法运算测试
  azimuth.assert_eq(7, azimuth.divide_with_ceil(20, 3))
  azimuth.assert_eq(6, azimuth.divide_with_ceil(18, 3))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 10))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(15, 0))
  
  // 负数测试
  azimuth.assert_eq(-6, azimuth.divide_with_ceil(-20, 3))
  azimuth.assert_eq(-7, azimuth.divide_with_ceil(20, -3))
  azimuth.assert_eq(6, azimuth.divide_with_ceil(-20, -3))
}

test "enhanced_greet_function_international" {
  // 增强问候函数国际化测试
  azimuth.assert_eq_string("Hello, Universe!", azimuth.greet("Universe"))
  azimuth.assert_eq_string("Hello, 北京!", azimuth.greet("北京"))
  azimuth.assert_eq_string("Hello, São Paulo!", azimuth.greet("São Paulo"))
  azimuth.assert_eq_string("Hello, 🌟!", azimuth.greet("🌟"))
  azimuth.assert_eq_string("Hello, empty!", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, space !", azimuth.greet("space "))
}

test "mathematical_identity_enhanced" {
  // 增强数学恒等式测试
  let x = 25
  let y = 15
  
  // 加法恒等式
  azimuth.assert_eq(x, azimuth.add(x, 0))
  azimuth.assert_eq(x, azimuth.add(0, x))
  
  // 乘法恒等式
  azimuth.assert_eq(x, azimuth.multiply(x, 1))
  azimuth.assert_eq(x, azimuth.multiply(1, x))
  azimuth.assert_eq(0, azimuth.multiply(x, 0))
  
  // 减法恒等式
  azimuth.assert_eq(0, azimuth.subtract(x, x))
  azimuth.assert_eq(x, azimuth.subtract(x, 0))
}

test "business_inventory_enhanced" {
  // 增强库存管理业务测试
  let total_products = 250
  let box_capacity = 30
  let boxes_needed = azimuth.divide_with_ceil(total_products, box_capacity)
  
  azimuth.assert_eq(9, boxes_needed)
  
  // 验证总容量
  let total_capacity = azimuth.multiply(boxes_needed, box_capacity)
  azimuth.assert_eq(270, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth.subtract(total_capacity, total_products)
  azimuth.assert_eq(20, remaining_space)
  
  // 验证空间利用率
  let utilization = azimuth.divide_with_ceil(azimuth.multiply(total_products, 100), total_capacity)
  azimuth.assert_eq(93, utilization)  // floor(25000/270) = 92%
}

test "complex_workflow_enhanced" {
  // 增强复杂工作流测试
  // 场景：项目管理
  let team_size = 12
  let weeks_per_sprint = 2
  let tasks_per_person_per_week = 3
  let total_tasks = 150
  
  // 计算每周处理能力
  let weekly_capacity = azimuth.multiply(team_size, tasks_per_person_per_week)
  azimuth.assert_eq(36, weekly_capacity)
  
  // 计算冲刺处理能力
  let sprint_capacity = azimuth.multiply(weekly_capacity, weeks_per_sprint)
  azimuth.assert_eq(72, sprint_capacity)
  
  // 计算需要的冲刺数
  let sprints_needed = azimuth.divide_with_ceil(total_tasks, sprint_capacity)
  azimuth.assert_eq(3, sprints_needed)
}

test "boundary_values_enhanced" {
  // 增强边界值测试
  // 极小值测试
  azimuth.assert_eq(1, azimuth.add(1, 0))
  azimuth.assert_eq(0, azimuth.multiply(1, 0))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(1, 1))
  
  // 极大值测试
  azimuth.assert_eq(20000, azimuth.add(10000, 10000))
  azimuth.assert_eq(100000000, azimuth.multiply(10000, 10000))
  azimuth.assert_eq(10001, azimuth.divide_with_ceil(10000, 1))
  
  // 边界除法测试
  azimuth.assert_eq(1, azimuth.divide_with_ceil(9999, 10000))
  azimuth.assert_eq(10000, azimuth.divide_with_ceil(10000, 1))
  azimuth.assert_eq(1, azimuth.divide_with_ceil(10000, 10000))
}

test "string_handling_enhanced" {
  // 增强字符串处理测试
  azimuth.assert_eq_string("Hello, Developer!", azimuth.greet("Developer"))
  azimuth.assert_eq_string("Hello, 2024!", azimuth.greet("2024"))
  azimuth.assert_eq_string("Hello, @user!", azimuth.greet("@user"))
  azimuth.assert_eq_string("Hello, $price!", azimuth.greet("$price"))
  azimuth.assert_eq_string("Hello, #hashtag!", azimuth.greet("#hashtag"))
  azimuth.assert_eq_string("Hello, mixedCASE123!", azimuth.greet("mixedCASE123"))
}

test "error_resilience_enhanced" {
  // 增强错误恢复能力测试
  // 除零错误的全面测试
  azimuth.assert_eq(0, azimuth.divide_with_ceil(1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-1, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(9999, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(-9999, 0))
  azimuth.assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 极值运算测试
  let max_value = 1000000
  let min_value = -1000000
  
  azimuth.assert_eq(0, azimuth.multiply(max_value, 0))
  azimuth.assert_eq(0, azimuth.multiply(min_value, 0))
  azimuth.assert_eq(max_value, azimuth.add(max_value, 0))
  azimuth.assert_eq(min_value, azimuth.add(min_value, 0))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let attendees = 250
  let tables_available = 25
  let chairs_per_table = 8
  let cost_per_chair = 15
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = azimuth.divide_with_ceil(attendees, chairs_per_table)
  azimuth.assert_eq(32, tables_needed)
  
  // 检查桌子是否足够
  azimuth.assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = azimuth.multiply(tables_needed, chairs_per_table)
  azimuth.assert_eq(256, total_seats)
  
  // 计算剩余座位
  let remaining_seats = azimuth.subtract(total_seats, attendees)
  azimuth.assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = azimuth.multiply(total_seats, cost_per_chair)
  azimuth.assert_eq(3840, total_cost)
  
  // 验证预算充足
  azimuth.assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = azimuth.subtract(total_budget, total_cost)
  azimuth.assert_eq(160, remaining_budget)
}