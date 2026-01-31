// 标准 MoonBit 测试用例
// 为 azimuth 项目添加的高质量测试套件

test "arithmetic_identity_properties" {
  // 测试算术恒等性质
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

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：项目资源规划
  let team_size = 8
  let sprint_duration = 2  // 周
  let tasks_per_person_per_week = 3
  let total_tasks = 75
  
  // 计算团队每周处理能力
  let weekly_capacity = multiply(team_size, tasks_per_person_per_week)
  
  // 计算每个冲刺的处理能力
  let sprint_capacity = multiply(weekly_capacity, sprint_duration)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  // 验证结果
  assert_eq(24, weekly_capacity)  // 8 * 3
  assert_eq(48, sprint_capacity)  // 24 * 2
  assert_eq(2, sprints_needed)    // ceil(75/48)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(96, total_capacity)   // 2 * 48
}

test "mathematical_commutative_associative" {
  // 测试数学交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(a, add(b, c)), add(add(a, b), c))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(a, multiply(b, c)), multiply(multiply(a, b), c))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "error_handling_edge_cases" {
  // 错误处理和边缘情况测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  
  // 大数测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "string_formatting_special_cases" {
  // 字符串格式化特殊情况测试
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, trailing !", greet("trailing "))
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  // 场景：仓库物品装箱
  let item_a_count = 47
  let item_b_count = 63
  let item_c_count = 29
  let box_capacity = 12
  
  // 计算每种物品需要的箱子数
  let boxes_a = divide_with_ceil(item_a_count, box_capacity)
  let boxes_b = divide_with_ceil(item_b_count, box_capacity)
  let boxes_c = divide_with_ceil(item_c_count, box_capacity)
  
  // 验证箱子数量
  assert_eq(4, boxes_a)   // ceil(47/12)
  assert_eq(6, boxes_b)   // ceil(63/12)
  assert_eq(3, boxes_c)   // ceil(29/12)
  
  // 计算总箱子数
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  assert_eq(13, total_boxes)
  
  // 计算总存储容量
  let total_capacity = multiply(total_boxes, box_capacity)
  assert_eq(156, total_capacity)
  
  // 计算总物品数
  let total_items = add(add(item_a_count, item_b_count), item_c_count)
  assert_eq(139, total_items)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(17, remaining_space)
}