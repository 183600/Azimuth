// Azimuth 增强测试套件
// 包含针对 azimuth 库的额外测试用例

test "advanced_arithmetic_combinations" {
  // 测试高级算术组合
  let a = 12
  let b = 8
  let c = 5
  
  // 复杂表达式测试
  let result1 = add(multiply(a, b), subtract(c, 2))
  assert_eq(99, result1)  // 12*8 + (5-2) = 96 + 3 = 99
  
  let result2 = subtract(multiply(add(a, b), c), divide_with_ceil(a, b))
  assert_eq(98, result2)  // (12+8)*5 - ceil(12/8) = 20*5 - 2 = 100 - 2 = 98
}

test "financial_calculations" {
  // 金融计算测试
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 简单利息计算
  let annual_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let total_interest = multiply(annual_interest, years)
  let final_amount = add(principal, total_interest)
  
  assert_eq(800, annual_interest)
  assert_eq(2400, total_interest)
  assert_eq(12400, final_amount)
}

test "inventory_optimization" {
  // 库存优化测试
  let current_stock = 150
  let daily_demand = 12
  let lead_time_days = 5
  let safety_factor = 2
  
  // 计算安全库存
  let safety_stock = multiply(daily_demand, multiply(lead_time_days, safety_factor))
  
  // 计算再订购点
  let reorder_point = add(multiply(daily_demand, lead_time_days), safety_stock)
  
  // 计算需要订购的箱数（每箱25件）
  let boxes_to_order = divide_with_ceil(reorder_point, 25)
  
  assert_eq(120, safety_stock)
  assert_eq(180, reorder_point)
  assert_eq(8, boxes_to_order)
}

test "temperature_conversions" {
  // 温度转换测试
  let celsius = 25
  
  // 摄氏度转华氏度: F = C × 9/5 + 32
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  // 华氏度转摄氏度: C = (F - 32) × 5/9
  let celsius_back = divide_with_ceil(multiply(subtract(fahrenheit, 32), 5), 9)
  
  assert_eq(77, fahrenheit)
  assert_eq(25, celsius_back)
}

test "game_mechanics" {
  // 游戏机制测试
  let base_score = 100
  let level = 5
  let multiplier = 2
  let bonus = 50
  
  // 计算等级得分
  let level_score = multiply(base_score, level)
  
  // 应用倍数
  let multiplied_score = multiply(level_score, multiplier)
  
  // 添加奖励
  let final_score = add(multiplied_score, bonus)
  
  // 计算星级（每200分一星）
  let stars = divide_with_ceil(final_score, 200)
  
  assert_eq(500, level_score)
  assert_eq(1000, multiplied_score)
  assert_eq(1050, final_score)
  assert_eq(6, stars)
}

test "project_management" {
  // 项目管理测试
  let total_tasks = 45
  let team_members = 5
  let working_days = 10
  let buffer_days = 2
  
  // 计算每人任务数
  let tasks_per_person = divide_with_ceil(total_tasks, team_members)
  
  // 计算每人每天任务数
  let daily_tasks_per_person = divide_with_ceil(tasks_per_person, working_days)
  
  // 计算项目总天数（含缓冲）
  let total_days = add(working_days, buffer_days)
  
  // 计算项目效率
  let efficiency = divide_with_ceil(multiply(total_tasks, 100), multiply(total_days, team_members))
  
  assert_eq(9, tasks_per_person)
  assert_eq(1, daily_tasks_per_person)
  assert_eq(12, total_days)
  assert_eq(75, efficiency)
}

test "data_structure_operations" {
  // 数据结构操作模拟测试
  let array_size = 10
  let page_size = 3
  
  // 计算分页数量
  let total_pages = divide_with_ceil(array_size, page_size)
  
  // 计算最后一页的元素数量
  let last_page_size = subtract(array_size, multiply(subtract(total_pages, 1), page_size))
  
  // 计算完整页的数量
  let full_pages = subtract(total_pages, 1)
  
  assert_eq(4, total_pages)
  assert_eq(1, last_page_size)
  assert_eq(3, full_pages)
}

test "resource_allocation" {
  // 资源分配测试
  let total_resources = 100
  let priority_tasks = 3
  let normal_tasks = 5
  let priority_weight = 3
  let normal_weight = 1
  
  // 计算优先级任务权重
  let priority_total_weight = multiply(priority_tasks, priority_weight)
  
  // 计算普通任务权重
  let normal_total_weight = multiply(normal_tasks, normal_weight)
  
  // 计算总权重
  let total_weight = add(priority_total_weight, normal_total_weight)
  
  // 分配资源给优先级任务
  let priority_resources = divide_with_ceil(multiply(total_resources, priority_total_weight), total_weight)
  
  // 分配资源给普通任务
  let normal_resources = subtract(total_resources, priority_resources)
  
  assert_eq(9, priority_total_weight)
  assert_eq(5, normal_total_weight)
  assert_eq(14, total_weight)
  assert_eq(65, priority_resources)
  assert_eq(35, normal_resources)
}

test "error_handling_edge_cases" {
  // 错误处理边界情况测试
  let max_value = 1000
  let min_value = -1000
  
  // 测试零除法
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试边界值运算
  let boundary_sum = add(max_value, min_value)
  let boundary_product = multiply(max_value, 0)
  let boundary_diff = subtract(max_value, min_value)
  
  assert_eq(0, boundary_sum)
  assert_eq(0, boundary_product)
  assert_eq(2000, boundary_diff)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试特殊字符和边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, a!b@c#!", greet("a!b@c#"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
  
  // 测试长字符串
  let long_name = "VeryLongNameThatExceedsNormalExpectationsForTestingPurposes"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
}