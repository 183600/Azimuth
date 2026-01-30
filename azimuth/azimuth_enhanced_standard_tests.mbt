// Azimuth 增强标准测试套件
// 包含8个高质量的标准 MoonBit 测试用例

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-2, add(-5, 3))
  assert_eq(0, add(5, -5))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  
  assert_eq(2, subtract(10, 8))
  assert_eq(-2, subtract(8, 10))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边缘情况测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(-2, divide_with_ceil(3, -2))
  assert_eq(2, divide_with_ceil(-3, -2))
}

test "greet_function_international_support" {
  // 问候函数国际化支持测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, space !", greet("space "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 减法性质
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目管理计算
  let team_size = 6
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(30, total_tasks)
  assert_eq(2, sprints_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(30, total_capacity)
}

test "error_handling_and_boundary_values" {
  // 错误处理和边界值测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let participants = 87
  let chairs_per_table = 8
  let tables_available = 15
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 验证桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
}