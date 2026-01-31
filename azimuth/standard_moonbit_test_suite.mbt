// 标准 MoonBit 测试套件 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "basic_arithmetic_addition" {
  // 基本加法运算测试
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
}

test "multiplication_comprehensive" {
  // 乘法运算综合测试
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(-12345, multiply(-1, 12345))
  assert_eq(12345, multiply(-1, -12345))
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "ceil_division_precision" {
  // 向上取整除法精度测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "subtraction_basic_operations" {
  // 基本减法运算测试
  assert_eq(4, subtract(7, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-4, subtract(3, 7))
  assert_eq(10, subtract(0, -10))
  assert_eq(1000000, subtract(1500000, 500000))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(3, sprints_needed)  // ceil(40/15) = 3
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
}

test "boundary_conditions_handling" {
  // 边界条件处理测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 单位值测试
  assert_eq(7, add(7, 0))
  assert_eq(7, multiply(7, 1))
  assert_eq(1, divide_with_ceil(7, 7))
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(9999, 0))
  assert_eq(0, divide_with_ceil(-9999, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let max_value = 1000000
  let min_value = -1000000
  
  assert_eq(0, multiply(max_value, 0))
  assert_eq(0, multiply(min_value, 0))
  assert_eq(max_value, add(max_value, 0))
  assert_eq(min_value, add(min_value, 0))
}

test "real_world_business_logic" {
  // 真实世界业务逻辑测试
  // 场景：活动座位安排
  let participants = 87
  let tables_available = 25
  let chairs_per_table = 8
  let cost_per_chair = 15
  let total_budget = 4000
  
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
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(1320, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(2680, remaining_budget)
}