// 高质量 MoonBit 测试套件
// 包含 8 个全面的测试用例，覆盖核心功能和边界情况

test "arithmetc_sequence_operations" {
  // 测试算术序列操作
  let start = 5
  let difference = 3
  let terms = 4
  
  // 计算等差数列的和
  let term1 = start
  let term2 = add(term1, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(32, sum)  // 5 + 8 + 11 + 14 = 38
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, terms)
  assert_eq(10, average)  // ceil(38/4) = 10
}

test "inventory_management_system" {
  // 库存管理系统测试
  let product_a_stock = 120
  let product_b_stock = 85
  let product_c_stock = 47
  
  let box_capacity = 24
  let total_products = add(add(product_a_stock, product_b_stock), product_c_stock)
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(11, boxes_needed)  // ceil(252/24) = 11
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, box_capacity)
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(12, remaining_space)
}

test "time_calculation_scenarios" {
  // 时间计算场景测试
  let hours_per_day = 24
  let days_per_week = 7
  let weeks_per_month = 4
  
  // 计算一个月的总小时数
  let hours_per_week = multiply(hours_per_day, days_per_week)
  let hours_per_month = multiply(hours_per_week, weeks_per_month)
  assert_eq(672, hours_per_month)  // 24 * 7 * 4 = 672
  
  // 项目时间分配
  let project_hours = 150
  let work_hours_per_day = 8
  let days_needed = divide_with_ceil(project_hours, work_hours_per_day)
  assert_eq(19, days_needed)  // ceil(150/8) = 19
}

test "string_processing_validation" {
  // 字符串处理验证测试
  assert_eq_string("Hello, Developer!", greet("Developer"))
  assert_eq_string("Hello, User123!", greet("User123"))
  assert_eq_string("Hello, space separated!", greet("space separated"))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
  assert_eq_string("Hello, empty string!", greet(""))
  assert_eq_string("Hello,   multiple spaces   !", greet("  multiple spaces   "))
}

test "mathematical_series_calculation" {
  // 数学级数计算测试
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square1 = multiply(1, 1)
  let square2 = multiply(2, 2)
  let square3 = multiply(3, 3)
  let square4 = multiply(4, 4)
  let square5 = multiply(5, 5)
  
  let sum_of_squares = add(add(add(add(square1, square2), square3), square4), square5)
  assert_eq(55, sum_of_squares)  // 1 + 4 + 9 + 16 + 25 = 55
  
  // 计算立方和：1³ + 2³ + 3³
  let cube1 = multiply(multiply(1, 1), 1)
  let cube2 = multiply(multiply(2, 2), 2)
  let cube3 = multiply(multiply(3, 3), 3)
  
  let sum_of_cubes = add(add(cube1, cube2), cube3)
  assert_eq(36, sum_of_cubes)  // 1 + 8 + 27 = 36
}

test "resource_planning_optimization" {
  // 资源规划优化测试
  let team_a_members = 8
  let team_b_members = 6
  let team_c_members = 10
  
  let total_members = add(add(team_a_members, team_b_members), team_c_members)
  let tasks_per_member = 5
  let total_tasks = multiply(total_members, tasks_per_member)
  
  // 任务分配到冲刺中
  let sprint_capacity = 40
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)  // ceil(120/40) = 3
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let extra_capacity = subtract(total_capacity, total_tasks)
  assert_eq(0, extra_capacity)  // 完美匹配
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(2147483647, multiply(1, 2147483647))
  assert_eq(-2147483648, multiply(-1, 2147483648))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：电商订单处理
  let order_items = 47
  let items_per_box = 8
  let boxes_per_pallet = 6
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  assert_eq(6, boxes_needed)  // ceil(47/8) = 6
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(boxes_needed, boxes_per_pallet)
  assert_eq(1, pallets_needed)  // ceil(6/6) = 1
  
  // 计算总容量和浪费空间
  let total_box_capacity = multiply(boxes_needed, items_per_box)
  let wasted_box_space = subtract(total_box_capacity, order_items)
  assert_eq(1, wasted_box_space)  // 48 - 47 = 1
  
  let total_pallet_capacity = multiply(pallets_needed, boxes_per_pallet)
  let wasted_pallet_space = subtract(total_pallet_capacity, boxes_needed)
  assert_eq(0, wasted_pallet_space)  // 6 - 6 = 0
}