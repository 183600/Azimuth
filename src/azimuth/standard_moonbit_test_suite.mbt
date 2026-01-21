// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "arithithmetic_addition_comprehensive" {
  // 全面的加法测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-8, add(-3, -5))
  assert_eq(1000, add(999, 1))
  assert_eq(-1000, add(-999, -1))
}

test "multiplication_various_cases" {
  // 各种乘法情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(-25, multiply(5, -5))
  assert_eq(0, multiply(0, 12345))
  assert_eq(144, multiply(12, 12))
  assert_eq(-144, multiply(-12, 12))
}

test "divide_with_ceil_practical_scenarios" {
  // 向上取整除法的实际场景测试
  assert_eq(3, divide_with_ceil(25, 10))  // 需要3个盒子装25个物品，每盒10个
  assert_eq(5, divide_with_ceil(41, 8))   // 需要5天完成41个任务，每天8个
  assert_eq(1, divide_with_ceil(1, 100))  // 至少需要1个容器
  assert_eq(0, divide_with_ceil(0, 7))    // 0个物品需要0个容器
  assert_eq(0, divide_with_ceil(10, 0))   // 除数为0时返回0
}

test "greet_function_multilingual_support" {
  // 多语言问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, Мир!", greet("Мир"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算订单总价（商品价格×数量+运费-折扣）
  let product_price = 45
  let quantity = 6
  let shipping = 15
  let discount = 25
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(280, final_total)  // 45*6 + 15 - 25 = 270 + 15 - 25 = 260
}

test "resource_planning_calculation" {
  // 资源规划计算测试
  // 场景：项目资源分配
  let team_members = 8
  let tasks_per_member = 5
  let additional_tasks = 12
  
  let regular_tasks = multiply(team_members, tasks_per_member)
  let total_tasks = add(regular_tasks, additional_tasks)
  let days_needed = divide_with_ceil(total_tasks, multiply(team_members, 2))
  
  assert_eq(3, days_needed)  // ceil((8*5+12)/(8*2)) = ceil(52/16) = 4
}

test "mathematical_operations_edge_cases" {
  // 数学运算边界情况测试
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(123, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(2, divide_with_ceil(9, 5))
}

test "string_handling_special_cases" {
  // 字符串处理特殊情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello, 12345!", greet("12345"))
}

test "business_logic_integration" {
  // 业务逻辑集成测试
  // 场景：计算库存补充需求
  let current_stock = 120
  let daily_sales = 15
  let days_to_reorder = 7
  let safety_stock = 20
  
  let expected_sales = multiply(daily_sales, days_to_reorder)
  let reorder_point = add(expected_sales, safety_stock)
  let needs_reorder = current_stock < reorder_point
  
  assert_eq(true, needs_reorder)  // 120 < (15*7 + 20) = 125
}

test "performance_calculation_optimization" {
  // 性能计算优化测试
  // 场景：计算任务处理时间
  let base_time = 5
  let complexity_factor = 3
  let optimization_bonus = 2
  let tasks = 8
  
  let individual_time = multiply(base_time, complexity_factor)
  let optimized_time = add(individual_time, -optimization_bonus)
  let total_time = multiply(optimized_time, tasks)
  
  assert_eq(104, total_time)  // (5*3 - 2) * 8 = 13 * 8 = 104
}