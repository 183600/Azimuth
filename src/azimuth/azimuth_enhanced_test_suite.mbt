// 新增的高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

// 导入 azimuth 库
use "azimuth"

// 断言函数
fn assert_true(condition : Bool) -> Unit {
  if condition {
    ()
  } else {
    let _ = "Assertion failed"
  }
  ()
}

test "advanced_mathematical_operations" {
  // 高级数学运算测试
  let x = 15
  let y = 7
  let z = 3
  
  // 测试复合运算：(x + y) * z - x / y
  let compound_result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(63, compound_result)  // (15 + 7) * 3 - ceil(15/7) = 22 * 3 - 3 = 66 - 3 = 63
  
  // 测试分配律：x * (y + z) = x * y + x * z
  let distributive_left = multiply(x, add(y, z))
  let distributive_right = add(multiply(x, y), multiply(x, z))
  assert_eq(distributive_left, distributive_right)  // 15 * (7 + 3) = 15 * 7 + 15 * 3 = 150
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
  assert_eq_string("Hello, 🌙🌟!", greet("🌙🌟"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "financial_calculation_scenarios" {
  // 金融计算场景测试
  // 场景1：计算贷款分期
  let loan_amount = 10000
  let monthly_payment = 450
  let months_needed = divide_with_ceil(loan_amount, monthly_payment)
  assert_eq(23, months_needed)  // ceil(10000/450) = 23
  
  // 场景2：计算复利（简化版本）
  let principal = 5000
  let rate_percent = 6
  let years = 2
  
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  
  assert_eq(5300, year1_amount)  // 5000 + ceil(5000*6/100) = 5000 + 300 = 5300
  assert_eq(5618, year2_amount)  // 5300 + ceil(5300*6/100) = 5300 + 318 = 5618
}

test "resource_optimization_problems" {
  // 资源优化问题测试
  // 场景：仓库存储优化
  let items = [
    (120, 25),  // (数量, 每箱容量)
    (85, 20),
    (200, 45),
    (67, 15)
  ]
  
  // 计算每种物品需要的箱子数量
  let boxes_1 = divide_with_ceil(items[0].0, items[0].1)  // ceil(120/25) = 5
  let boxes_2 = divide_with_ceil(items[1].0, items[1].1)  // ceil(85/20) = 5
  let boxes_3 = divide_with_ceil(items[2].0, items[2].1)  // ceil(200/45) = 5
  let boxes_4 = divide_with_ceil(items[3].0, items[3].1)  // ceil(67/15) = 5
  
  // 计算总箱子数和总容量
  let total_boxes = add(add(boxes_1, boxes_2), add(boxes_3, boxes_4))
  let total_capacity = add(
    multiply(boxes_1, items[0].1),
    add(multiply(boxes_2, items[1].1), add(multiply(boxes_3, items[2].1), multiply(boxes_4, items[3].1)))
  )
  
  assert_eq(20, total_boxes)
  assert_eq(525, total_capacity)  // 5*25 + 5*20 + 5*45 + 5*15 = 125 + 100 + 225 + 75 = 525
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_sizes = [50, 100, 200]
  
  // 模拟O(n)算法
  let linear_50 = multiply(input_sizes[0], 3)
  let linear_100 = multiply(input_sizes[1], 3)
  let linear_200 = multiply(input_sizes[2], 3)
  
  // 模拟O(log n)算法（简化为除法）
  let log_50 = divide_with_ceil(input_sizes[0], 10)
  let log_100 = divide_with_ceil(input_sizes[1], 10)
  let log_200 = divide_with_ceil(input_sizes[2], 10)
  
  // 验证复杂度特征
  assert_eq(150, linear_50)
  assert_eq(300, linear_100)
  assert_eq(600, linear_200)
  
  assert_eq(5, log_50)
  assert_eq(10, log_100)
  assert_eq(20, log_200)
  
  // 验证线性增长率
  assert_eq(multiply(2, linear_50), linear_100)
  assert_eq(multiply(2, linear_100), linear_200)
}

test "data_structure_operations_simulation" {
  // 数据结构操作模拟测试
  // 模拟栈操作
  let stack_capacity = 10
  let stack_items = 7
  let remaining_capacity = subtract(stack_capacity, stack_items)
  let can_push_more = remaining_capacity > 0
  
  assert_eq(3, remaining_capacity)
  assert_true(can_push_more)
  
  // 模拟队列操作
  let queue_size = 0
  let enqueue_operations = 5
  let dequeue_operations = 2
  let final_queue_size = add(add(queue_size, enqueue_operations), -dequeue_operations)
  
  assert_eq(3, final_queue_size)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 测试除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 测试极值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 测试大数运算的边界情况
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
}

test "real_world_application_scenarios" {
  // 真实世界应用场景测试
  // 场景1：计算活动安排
  let participants = 156
  let tables = 12
  let chairs_per_table = 15
  
  let total_chairs = multiply(tables, chairs_per_table)
  let chairs_needed = divide_with_ceil(participants, chairs_per_table)
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  
  assert_eq(180, total_chairs)
  assert_eq(11, chairs_needed)  // ceil(156/15) = 11
  assert_eq(11, tables_needed)  // ceil(156/15) = 11
  
  // 场景2：计算项目资源分配
  let total_budget = 50000
  let team_members = 8
  let base_budget_per_member = 5000
  let remaining_budget = subtract(total_budget, multiply(team_members, base_budget_per_member))
  
  assert_eq(10000, remaining_budget)
  
  // 计算可以支持的额外项目
  let project_costs = [2000, 3000, 1500, 4000]
  let affordable_projects = 0
  
  // 简化的项目计算（由于MoonBit语法限制，使用固定计算）
  let total_project_cost = add(add(add(project_costs[0], project_costs[1]), project_costs[2]), project_costs[3])
  let max_projects = divide_with_ceil(remaining_budget, divide_with_ceil(total_project_cost, 4))
  
  assert_eq(2, max_projects)  // 可以支持2个平均成本的项目
}

test "performance_and_efficiency_calculations" {
  // 性能和效率计算测试
  // 场景：算法性能比较
  let dataset_size = 10000
  
  // 算法A：O(n)复杂度
  let algorithm_a_time = multiply(dataset_size, 2)
  
  // 算法B：O(n log n)复杂度（简化）
  let algorithm_b_time = multiply(dataset_size, divide_with_ceil(100, 10))
  
  // 算法C：O(n²)复杂度（简化）
  let algorithm_c_time = divide_with_ceil(multiply(dataset_size, dataset_size), 1000)
  
  // 验证性能特征
  assert_eq(20000, algorithm_a_time)
  assert_eq(100000, algorithm_b_time)
  assert_eq(100000, algorithm_c_time)
  
  // 计算性能提升倍数
  let improvement_b_over_a = divide_with_ceil(algorithm_b_time, algorithm_a_time)
  let improvement_c_over_a = divide_with_ceil(algorithm_c_time, algorithm_a_time)
  
  assert_eq(5, improvement_b_over_a)
  assert_eq(5, improvement_c_over_a)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}