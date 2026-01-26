// 新标准 MoonBit 测试用例 - 针对 azimuth 库的综合测试
// 包含不超过10个高质量测试用例，使用标准 MoonBit 测试语法

test "arithmetic_commutative_properties" {
  // 测试算术运算的交换律性质
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(42, add(15, 27))
  assert_eq(405, multiply(15, 27))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  // 场景1: 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 场景2: 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 场景3: 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "business_logic_resource_calculation" {
  // 业务逻辑：资源计算测试
  // 场景：项目资源分配
  let total_tasks = 47
  let team_members = 6
  let tasks_per_member = divide_with_ceil(total_tasks, team_members)
  
  assert_eq(8, tasks_per_member)  // ceil(47/6) = 8
  
  // 计算总处理能力
  let total_capacity = multiply(tasks_per_member, team_members)
  assert_eq(48, total_capacity)
  
  // 计算剩余能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(1, remaining_capacity)
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 5
  let b = 12
  let c = 7
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
  assert_eq(95, left_side)  // 5 * (12 + 7) = 5 * 19 = 95
}

test "error_handling_and_boundary_values" {
  // 错误处理和边界值测试
  // 极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
  
  // 边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：多步骤计算
  let initial_value = 20
  
  // 步骤1: 加法运算
  let step1 = add(initial_value, 15)  // 35
  
  // 步骤2: 乘法运算
  let step2 = multiply(step1, 3)  // 105
  
  // 步骤3: 除法运算
  let step3 = divide_with_ceil(step2, 25)  // 5
  
  // 步骤4: 最终验证
  assert_eq(5, step3)
  
  // 验证中间步骤
  assert_eq(35, step1)
  assert_eq(105, step2)
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  // 特殊字符和格式
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^&*()!", greet("!@#$%^&*()"))
  assert_eq_string("Hello,   spaces   !", greet("   spaces   "))
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'apostrophe'!", greet("'apostrophe'"))
}

test "performance_large_number_operations" {
  // 大数运算性能测试
  // 大数加法
  assert_eq(2000000, add(1000000, 1000000))
  
  // 大数乘法
  assert_eq(1000000, multiply(1000, 1000))
  
  // 大数除法
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(999, divide_with_ceil(1000000, 1001))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试
  // 场景：活动安排和座位分配
  let participants = 87
  let tables_available = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)  // ceil(87/10) = 9
  
  // 检查是否有足够的桌子
  let enough_tables = tables_needed <= tables_available
  assert_true(enough_tables)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(3, remaining_seats)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}