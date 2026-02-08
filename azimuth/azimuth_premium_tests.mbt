// Azimuth 高级测试用例 - 使用标准 MoonBit 测试语法
// 包含8个高质量测试用例，覆盖核心功能和边界情况

test "premium_mathematical_identities" {
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
  assert_eq(-x, subtract(0, x))
}

test "premium_divide_with_ceil_business_logic" {
  // 业务逻辑：计算项目资源分配
  let total_tasks = 47
  let team_capacity = 8
  let sprints_needed = divide_with_ceil(total_tasks, team_capacity)
  
  assert_eq(6, sprints_needed)
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, team_capacity)
  let unused_capacity = subtract(total_capacity, total_tasks)
  
  assert_eq(48, total_capacity)
  assert_eq(1, unused_capacity)
}

test "premium_string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, mixed123!", greet("mixed123"))
  assert_eq_string("Hello, spaced out!", greet("spaced out"))
}

test "premium_negative_number_arithmetic" {
  // 负数算术运算测试
  let a = -15
  let b = 8
  
  assert_eq(-7, add(a, b))
  assert_eq(-23, add(a, -b))
  assert_eq(-120, multiply(a, b))
  assert_eq(120, multiply(a, -b))
  assert_eq(-23, subtract(a, b))
  assert_eq(-7, subtract(a, -b))
}

test "premium_complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算电商订单的包装需求
  
  let order_items = 23
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算总重量（假设每个物品重2单位）
  let weight_per_item = 2
  let total_weight = multiply(order_items, weight_per_item)
  
  // 计算每箱平均重量
  let avg_weight_per_box = divide_with_ceil(total_weight, boxes_needed)
  
  assert_eq(46, total_weight)
  assert_eq(12, avg_weight_per_box)
}

test "premium_boundary_value_analysis" {
  // 边界值分析测试
  // 测试除法的各种边界情况
  
  // 零边界
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 一边界
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(5, divide_with_ceil(5, 1))
  assert_eq(-5, divide_with_ceil(-5, 1))
  
  // 大数边界
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(1001, divide_with_ceil(1000001, 1000))
}

test "premium_mathematical_properties_comprehensive" {
  // 综合数学性质测试
  let a = 12
  let b = 8
  let c = 4
  
  // 分配律：a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 结合律： (a + b) + c = a + (b + c)
  let associative_add_left = add(add(a, b), c)
  let associative_add_right = add(a, add(b, c))
  assert_eq(associative_add_left, associative_add_right)
  
  // 交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "premium_real_world_scenarios" {
  // 真实世界场景测试
  // 场景：计算活动安排
  
  let participants = 85
  let tables_capacity = 10
  let tables_needed = divide_with_ceil(participants, tables_capacity)
  
  assert_eq(9, tables_needed)
  
  // 计算需要的椅子数量（每张桌子配10把椅子）
  let chairs_per_table = 10
  let total_chairs = multiply(tables_needed, chairs_per_table)
  
  // 计算剩余椅子
  let empty_chairs = subtract(total_chairs, participants)
  
  assert_eq(90, total_chairs)
  assert_eq(5, empty_chairs)
  
  // 验证问候功能在不同场景下的应用
  let event_name = "Annual Conference"
  let welcome_message = greet(event_name)
  assert_eq_string("Hello, Annual Conference!", welcome_message)
}