// Azimuth 项目标准测试用例
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(10, add(3, 7))
  assert_eq(-4, add(-10, 6))
  assert_eq(0, add(5, -5))
  
  assert_eq(21, multiply(3, 7))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(8, 0))
  
  assert_eq(5, subtract(12, 7))
  assert_eq(-5, subtract(7, 12))
  assert_eq(0, subtract(8, 8))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(15, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 15
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(
    multiply(a, add(b, 2)),
    add(multiply(a, b), multiply(a, 2))
  )
}

test "business_inventory_management" {
  // 库存管理业务场景测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, add(5, 0))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(60, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, remaining_capacity)
}

test "negative_numbers_comprehensive" {
  // 负数综合测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(-2, add(3, -5))
  
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-15, multiply(3, -5))
  
  assert_eq(2, subtract(-3, -5))
  assert_eq(-8, subtract(-3, 5))
  assert_eq(8, subtract(3, -5))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
}

test "mathematical_precision_and_large_numbers" {
  // 数学精度和大数测试
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 精度测试
  assert_eq(1000, divide_with_ceil(9999, 10))
  assert_eq(1001, divide_with_ceil(10001, 10))
  assert_eq(333334, divide_with_ceil(1000000, 3))
  
  // 复合运算精度
  let result = divide_with_ceil(add(multiply(100, 100), multiply(50, 50)), 150)
  assert_eq(84, result)  // ceil((10000+2500)/150) = ceil(12500/150) = 84
}