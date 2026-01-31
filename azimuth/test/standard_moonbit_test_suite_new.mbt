// 标准 MoonBit 测试套件 - 新增测试用例
// 包含 8 个高质量的标准 MoonBit 测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(8, 7))
  assert_eq(56, multiply(8, 7))
  assert_eq(1, add(0, 1))
  assert_eq(0, multiply(5, 0))
  assert_eq(-3, subtract(2, 5))
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-7, 4))
}

test "greet_function_multilingual" {
  // 问候函数多语言测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 95
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(8, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(96, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源分配
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  let sprints = 3
  let tasks_per_sprint = divide_with_ceil(total_tasks, sprints)
  
  assert_eq(48, total_tasks)
  assert_eq(16, tasks_per_sprint)
  
  // 验证最终分配
  let final_check = multiply(tasks_per_sprint, sprints)
  assert_eq(48, final_check)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值边界测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}

test "real_world_scenario_event_planning" {
  // 真实世界场景：活动规划测试
  let participants = 156
  let tables_available = 20
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(20, tables_needed)
  
  // 验证桌子是否足够（使用条件表达式转换为布尔值）
  let enough_tables = if tables_needed <= tables_available { 1 } else { 0 }
  assert_eq(1, enough_tables)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(160, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(4, remaining_seats)
}