// Azimuth 增强测试套件
// 包含8个高质量的标准 MoonBit 测试用例

test "mathematical_identity_properties" {
  // 数学恒等式测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  // 正数场景
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数场景
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界场景
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, élan!", greet("élan"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "complex_business_calculation" {
  // 复杂业务计算测试：项目资源规划
  let team_size = 7
  let sprints_needed = 5
  let hours_per_sprint = 40
  let total_hours = multiply(team_size, multiply(sprints_needed, hours_per_sprint))
  
  let budget_per_hour = 75
  let total_budget = multiply(total_hours, budget_per_hour)
  
  let payment_limit = 10000
  let payments_needed = divide_with_ceil(total_budget, payment_limit)
  
  assert_eq(1400, total_hours)  // 7 * 5 * 40 = 1400
  assert_eq(105000, total_budget)  // 1400 * 75 = 105000
  assert_eq(11, payments_needed)  // ceil(105000/10000) = 11
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证具体数值
  assert_eq(42, left_side)  // 6 * (4 + 3) = 42
  assert_eq(42, right_side)  // 6 * 4 + 6 * 3 = 24 + 18 = 42
}

test "inventory_management_optimization" {
  // 库存管理优化测试
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(10, remaining_space)
  
  // 验证空间利用率
  let utilization_percentage = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(94, utilization_percentage)  // floor(15800/168) = 94%
}

test "negative_number_arithmetic_comprehensive" {
  // 负数算术综合测试
  // 加法负数测试
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(0, add(-10, 10))
  
  // 乘法负数测试
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
  
  // 减法负数测试
  assert_eq(-8, subtract(-5, 3))
  assert_eq(8, subtract(3, -5))
  assert_eq(0, subtract(-5, -5))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let attendees = 127
  let tables_available = 15
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 检查桌子是否足够
  let additional_tables_needed = subtract(tables_needed, tables_available)
  assert_eq(1, additional_tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(1, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3200, total_cost)
  
  // 计算预算差额
  let budget_shortfall = subtract(total_cost, total_budget)
  assert_eq(200, budget_shortfall)
}