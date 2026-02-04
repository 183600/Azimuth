// Azimuth 增强测试套件 - 标准 MoonBit 测试用例
// 包含 8 个高质量测试用例，仅使用已定义的断言函数

test "enhanced_basic_arithmetic" {
  // 增强基本算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-5, add(-10, 5))
  assert_eq(0, add(10, -10))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-80, multiply(-10, 8))
  assert_eq(80, multiply(-10, -8))
  
  assert_eq(3, subtract(15, 12))
  assert_eq(-3, subtract(12, 15))
  assert_eq(0, subtract(20, 20))
}

test "enhanced_divide_with_ceil_operations" {
  // 增强向上取整除法运算测试
  assert_eq(5, divide_with_ceil(14, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(15, 0))
  
  // 负数测试
  assert_eq(-4, divide_with_ceil(-14, 3))
  assert_eq(-5, divide_with_ceil(14, -3))
  assert_eq(5, divide_with_ceil(-14, -3))
  assert_eq(-1, divide_with_ceil(-2, 3))
}

test "enhanced_greet_function_tests" {
  // 增强问候函数测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, Testing!", greet("Testing"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}

test "enhanced_mathematical_properties" {
  // 增强数学性质测试
  let x = 18
  let y = 12
  
  // 加法交换律验证
  assert_eq(add(x, y), add(y, x))
  assert_eq(30, add(18, 12))
  assert_eq(30, add(12, 18))
  
  // 乘法交换律验证
  assert_eq(multiply(x, y), multiply(y, x))
  assert_eq(216, multiply(18, 12))
  assert_eq(216, multiply(12, 18))
  
  // 加法恒等式验证
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式验证
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
}

test "enhanced_warehouse_management" {
  // 增强仓库管理场景测试
  let total_products = 280
  let pallet_capacity = 35
  let pallets_needed = divide_with_ceil(total_products, pallet_capacity)
  
  assert_eq(8, pallets_needed)
  
  // 验证总容量
  let total_capacity = multiply(pallets_needed, pallet_capacity)
  assert_eq(280, total_capacity)
  
  // 验证空间利用率
  let utilization = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(100, utilization)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(0, remaining_space)
}

test "enhanced_project_planning" {
  // 增强项目规划测试
  let developers = 12
  let stories_per_developer = 5
  let total_stories = multiply(developers, stories_per_developer)
  
  let sprint_capacity = 25
  let sprints_needed = divide_with_ceil(total_stories, sprint_capacity)
  
  assert_eq(60, total_stories)
  assert_eq(3, sprints_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(75, total_capacity)
  
  // 验证额外处理能力
  let extra_capacity = subtract(total_capacity, total_stories)
  assert_eq(15, extra_capacity)
  
  // 验证缓冲比例
  let buffer_ratio = divide_with_ceil(multiply(extra_capacity, 100), total_stories)
  assert_eq(25, buffer_ratio)
}

test "enhanced_boundary_conditions" {
  // 增强边界条件测试
  // 大数运算测试
  assert_eq(3000000, add(1500000, 1500000))
  assert_eq(225000000, multiply(15000, 15000))
  assert_eq(1500001, divide_with_ceil(1500000, 1))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(1, divide_with_ceil(999, 1000))
  assert_eq(1000, divide_with_ceil(1000, 1))
  
  // 特殊字符串处理测试
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, newline\n!", greet("newline\n"))
  assert_eq_string("Hello, tab\t!", greet("tab\t"))
}

test "enhanced_event_management" {
  // 增强活动管理测试
  let attendees = 350
  let round_tables_available = 25
  let seats_per_round_table = 10
  let rectangular_tables_available = 15
  let seats_per_rectangular_table = 8
  let cost_per_seat = 30
  let total_budget = 9000
  
  // 计算需要的圆桌数量
  let round_tables_needed = divide_with_ceil(attendees, seats_per_round_table)
  assert_eq(35, round_tables_needed)
  
  // 检查圆桌是否足够
  let round_tables_shortage = subtract(round_tables_needed, round_tables_available)
  assert_eq(10, round_tables_shortage)
  
  // 计算需要额外的方桌数量
  let remaining_attendees = subtract(attendees, multiply(round_tables_available, seats_per_round_table))
  let rectangular_tables_needed = divide_with_ceil(remaining_attendees, seats_per_rectangular_table)
  assert_eq(13, rectangular_tables_needed)
  
  // 检查方桌是否足够
  assert_eq(1, divide_with_ceil(rectangular_tables_available, rectangular_tables_needed))
  
  // 计算总座位数
  let total_seats = add(multiply(round_tables_available, seats_per_round_table), multiply(rectangular_tables_needed, seats_per_rectangular_table))
  assert_eq(354, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(4, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_seat)
  assert_eq(10620, total_cost)
  
  // 计算预算超支
  let budget_overrun = subtract(total_cost, total_budget)
  assert_eq(1620, budget_overrun)
  
  // 计算超支比例
  let overrun_percentage = divide_with_ceil(multiply(budget_overrun, 100), total_budget)
  assert_eq(18, overrun_percentage)
}