// 标准 Azimuth 测试套件 - 包含8个高质量的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "arithmetic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-5, 4))
  assert_eq(0, add(-3, 3))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-20, multiply(-4, 5))
  assert_eq(0, multiply(0, 100))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-3, subtract(7, 10))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法的边缘情况测试
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 100))
  
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(-5, 0))
  
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(-2, divide_with_ceil(3, -2))
}

test "greet_function_various_inputs" {
  // 问候函数的各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  assert_eq(add(add(x, y), 5), add(x, add(y, 5)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), 2), multiply(x, multiply(y, 2)))
  
  // 分配律
  assert_eq(multiply(x, add(y, 3)), add(multiply(x, y), multiply(x, 3)))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库存储优化
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总存储容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率
  assert_true(total_products <= total_capacity)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源规划
  let team_members = 6
  let work_days = 15
  let daily_hours = 8
  let task_complexity = 3
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(work_days, daily_hours))
  assert_eq(720, total_hours)
  
  // 计算复杂度调整后的工时
  let adjusted_hours = multiply(total_hours, task_complexity)
  assert_eq(2160, adjusted_hours)
  
  // 计算需要的冲刺次数（每冲刺200小时）
  let sprints_needed = divide_with_ceil(adjusted_hours, 200)
  assert_eq(11, sprints_needed)
  
  // 验证总冲刺容量
  let total_sprint_capacity = multiply(sprints_needed, 200)
  assert_eq(2200, total_sprint_capacity)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议安排
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 验证桌子数量是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let empty_seats = subtract(total_seats, participants)
  assert_eq(1, empty_seats)
  
  // 计算桌子利用率
  let tables_used = tables_needed
  let tables_unused = subtract(tables_available, tables_used)
  assert_eq(1, tables_unused)
}