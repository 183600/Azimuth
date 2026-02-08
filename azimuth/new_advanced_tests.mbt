// 新的高级 MoonBit 测试用例 - 8个高质量测试
// 使用标准 MoonBit 测试语法

test "advanced_mathematical_identities" {
  // 数学恒等式验证
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "advanced_business_calculations" {
  // 业务计算：项目资源分配
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

test "advanced_string_processing" {
  // 字符串处理测试
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, mixed123!", greet("mixed123"))
}

test "advanced_negative_arithmetic" {
  // 负数算术运算
  let a = -15
  let b = 8
  
  assert_eq(-7, add(a, b))
  assert_eq(-120, multiply(a, b))
  assert_eq(-23, subtract(a, b))
  assert_eq(-2, divide_with_ceil(-15, 8))
}

test "advanced_complex_workflow" {
  // 复杂工作流：电商订单包装
  let order_items = 23
  let items_per_box = 6
  let boxes_needed = divide_with_ceil(order_items, items_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 计算总重量
  let weight_per_item = 2
  let total_weight = multiply(order_items, weight_per_item)
  let avg_weight_per_box = divide_with_ceil(total_weight, boxes_needed)
  
  assert_eq(46, total_weight)
  assert_eq(12, avg_weight_per_box)
}

test "advanced_boundary_analysis" {
  // 边界值分析
  // 零边界
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
  
  // 一边界
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(5, divide_with_ceil(5, 1))
  
  // 大数边界
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(1001, divide_with_ceil(1000001, 1000))
}

test "advanced_mathematical_properties" {
  // 综合数学性质
  let a = 12
  let b = 8
  let c = 4
  
  // 分配律：a * (b + c) = a * b + a * c
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)
  
  // 结合律验证
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "advanced_real_world_scenario" {
  // 真实世界场景：活动安排
  let participants = 85
  let tables_capacity = 10
  let tables_needed = divide_with_ceil(participants, tables_capacity)
  
  assert_eq(9, tables_needed)
  
  // 计算椅子配置
  let chairs_per_table = 10
  let total_chairs = multiply(tables_needed, chairs_per_table)
  let empty_chairs = subtract(total_chairs, participants)
  
  assert_eq(90, total_chairs)
  assert_eq(5, empty_chairs)
  
  // 验证问候功能
  let event_name = "Annual Conference"
  let welcome_message = greet(event_name)
  assert_eq_string("Hello, Annual Conference!", welcome_message)
}