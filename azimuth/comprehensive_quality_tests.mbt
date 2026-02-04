// 综合高质量测试用例 - 为 Azimuth 项目添加
// 包含8个标准 MoonBit 测试用例，覆盖各种场景和边界情况

test "mathematical_distributive_law" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 7
  let b = 5
  let c = 3
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 7 * (5 + 3) = 7 * 5 + 7 * 3 = 56
}

test "complex_resource_allocation" {
  // 复杂资源分配场景测试
  let total_tasks = 125
  let team_members = 8
  let working_days = 15
  
  // 计算每人每天需要完成的任务数
  let tasks_per_day = divide_with_ceil(total_tasks, multiply(team_members, working_days))
  
  assert_eq(2, tasks_per_day)  // ceil(125/(8*15)) = ceil(125/120) = 2
  
  // 验证总容量
  let total_capacity = multiply(tasks_per_day, multiply(team_members, working_days))
  assert_eq(240, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(115, remaining_capacity)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, Multiple Words!", greet("Multiple Words"))
  assert_eq_string("Hello, 123456789!", greet("123456789"))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
  assert_eq_string("Hello,   spaces   !", greet("   spaces   "))
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))
}

test "negative_number_comprehensive" {
  // 负数综合测试
  // 加法负数测试
  assert_eq(-10, add(-3, -7))
  assert_eq(0, add(-5, 5))
  assert_eq(3, add(-8, 11))
  
  // 乘法负数测试
  assert_eq(21, multiply(-3, -7))
  assert_eq(-21, multiply(-3, 7))
  assert_eq(0, multiply(-3, 0))
  
  // 减法负数测试
  assert_eq(-10, subtract(-3, 7))
  assert_eq(10, subtract(7, -3))
  assert_eq(0, subtract(-5, -5))
}

test "divide_with_ceil_precision" {
  // 向上取整除法精度测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  
  // 大数精度测试
  assert_eq(1001, divide_with_ceil(2001, 2))
  assert_eq(667, divide_with_ceil(2001, 3))
  assert_eq(334, divide_with_ceil(2001, 6))
  
  // 负数精度测试
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(2, divide_with_ceil(-3, -2))
}

test "business_inventory_optimization" {
  // 业务库存优化测试
  let product_a = 45
  let product_b = 67
  let product_c = 23
  let package_capacity = 20
  
  // 计算每种产品需要的包装数
  let packages_a = divide_with_ceil(product_a, package_capacity)
  let packages_b = divide_with_ceil(product_b, package_capacity)
  let packages_c = divide_with_ceil(product_c, package_capacity)
  
  assert_eq(3, packages_a)  // ceil(45/20) = 3
  assert_eq(4, packages_b)  // ceil(67/20) = 4
  assert_eq(2, packages_c)  // ceil(23/20) = 2
  
  // 计算总包装数和总容量
  let total_packages = add(add(packages_a, packages_b), packages_c)
  let total_capacity = multiply(total_packages, package_capacity)
  
  assert_eq(9, total_packages)
  assert_eq(180, total_capacity)
  
  // 计算总产品数
  let total_products = add(add(product_a, product_b), product_c)
  assert_eq(135, total_products)
  
  // 计算剩余容量
  let remaining_capacity = subtract(total_capacity, total_products)
  assert_eq(45, remaining_capacity)
}

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 单位值测试
  assert_eq(2, add(1, 1))
  assert_eq(1, multiply(1, 1))
  assert_eq(0, subtract(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 极值测试（安全范围内）
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let guests = 156
  let tables_available = 20
  let chairs_per_table = 8
  let budget_per_chair = 25
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(guests, chairs_per_table)
  assert_eq(20, tables_needed)  // ceil(156/8) = 20
  
  // 验证桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(160, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, guests)
  assert_eq(4, remaining_seats)
  
  // 计算椅子总成本
  let total_cost = multiply(total_seats, budget_per_chair)
  assert_eq(4000, total_cost)
  
  // 验证预算是否充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(0, remaining_budget)
  
  // 计算座位利用率
  let utilization_rate = divide_with_ceil(multiply(guests, 100), total_seats)
  assert_eq(98, utilization_rate)  // floor(15600/160) = 97.5%，向上取整为98%
}