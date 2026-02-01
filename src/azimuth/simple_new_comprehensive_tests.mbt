// Azimuth 综合新测试用例 - 标准MoonBit测试语法
// 包含8个高质量测试用例，覆盖核心功能和边缘情况

test "mathematical_commutative_properties" {
  // 测试数学交换律性质
  let x = 42
  let y = 17
  
  // 加法交换律: a + b = b + a
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律: a * b = b * a
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 验证具体数值
  assert_eq(59, add(42, 17))
  assert_eq(714, multiply(42, 17))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-4, divide_with_ceil(10, -3))
  
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "business_logic_inventory_management" {
  // 业务逻辑：库存管理场景测试
  let total_products = 157
  let box_capacity = 24
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)  // 7 * 24 = 168
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)  // 168 - 157 = 11
}

test "string_processing_multilingual_support" {
  // 字符串处理：多语言支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_distributive_property" {
  // 测试数学分配律: a * (b + c) = a * b + a * c
  let a = 6
  let b = 4
  let c = 3
  
  let left_side = multiply(a, add(b, c))      // 6 * (4 + 3) = 6 * 7 = 42
  let right_side = add(multiply(a, b), multiply(a, c))  // 6*4 + 6*3 = 24 + 18 = 42
  
  assert_eq(left_side, right_side)
  assert_eq(42, left_side)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源分配计算
  
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)  // 8 * 5 = 40
  
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)  // ceil(40/15) = 3
  
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)  // 3 * 15 = 45
  let unused_capacity = subtract(total_capacity, total_tasks)  // 45 - 40 = 5
  
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
  assert_eq(45, total_capacity)
  assert_eq(5, unused_capacity)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))
  assert_eq(-5, subtract(0, 5))
  
  // 单位元测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, subtract(5, 0))
  
  // 极值测试
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界场景：活动规划测试
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)  // 11 * 8 = 88
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)  // 88 - 87 = 1
  
  // 验证所有参与者都有座位
  assert_true(total_seats >= participants)
}