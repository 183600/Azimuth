// Azimuth 项目新增的高质量 MoonBit 测试用例
// 包含10个标准测试用例，覆盖各种场景和边界情况

test "arithmetics_zero_element_properties" {
  // 测试算术运算的零元素性质
  assert_eq(5, add(5, 0))  // 加法零元素
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(5, 0))  // 乘法零元素
  assert_eq(0, multiply(0, 5))
  assert_eq(5, subtract(5, 0))  // 减法零元素
  assert_eq(0, divide_with_ceil(0, 5))  // 零除以非零数
  assert_eq(0, divide_with_ceil(5, 0))  // 除零处理
}

test "arithmetics_identity_elements" {
  // 测试算术运算的单位元素性质
  assert_eq(5, multiply(5, 1))  // 乘法单位元素
  assert_eq(5, multiply(1, 5))
  assert_eq(0, subtract(5, 5))  // 减法自身性质
  assert_eq(5, divide_with_ceil(5, 1))  // 除以1
  assert_eq(5, divide_with_ceil(5, 5))  // 除以自身
}

test "arithmetics_negative_numbers_comprehensive" {
  // 测试负数的综合算术运算
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(8, subtract(5, -3))
  assert_eq(-2, subtract(-5, -3))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 测试向上取整除法的综合场景
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(2, divide_with_ceil(7, 4))
  assert_eq(1, divide_with_ceil(3, 5))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 混合情况
  assert_eq(0, divide_with_ceil(-3, 10))
  assert_eq(-1, divide_with_ceil(-13, 10))
}

test "greet_function_special_inputs" {
  // 测试问候函数的特殊输入
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "mathematical_commutative_associative_properties" {
  // 测试数学的交换律和结合律性质
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "real_world_resource_allocation" {
  // 真实世界资源分配场景测试
  let total_tasks = 47
  let team_members = 6
  let working_days = 8
  let tasks_per_day = 5
  
  // 计算团队总任务容量
  let daily_capacity = multiply(team_members, tasks_per_day)
  let total_capacity = multiply(daily_capacity, working_days)
  
  assert_eq(30, daily_capacity)  // 6 * 5 = 30
  assert_eq(240, total_capacity)  // 30 * 8 = 240
  
  // 计算需要的额外天数
  let additional_days = divide_with_ceil(total_tasks, daily_capacity)
  assert_eq(2, additional_days)  // ceil(47/30) = 2
  
  // 计算剩余任务容量
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(193, remaining_capacity)  // 240 - 47 = 193
}

test "boundary_values_and_extreme_cases" {
  // 边界值和极值测试
  // 边界除法
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数运算（在安全范围内）
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  
  // 极值减法
  assert_eq(0, subtract(1000000, 1000000))
  assert_eq(2000000, subtract(1000000, -1000000))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目预算计算
  let team_size = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_duration_days = 15
  let budget_per_phase = 10000
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(hours_per_day, project_duration_days))
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  
  // 计算需要的项目阶段数
  let phases_needed = divide_with_ceil(total_budget, budget_per_phase)
  
  // 验证计算结果
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  assert_eq(72000, total_budget)  // 960 * 75 = 72000
  assert_eq(8, phases_needed)  // ceil(72000/10000) = 8
  
  // 计算最后一个阶段的预算
  let last_phase_budget = subtract(total_budget, multiply(phases_needed - 1, budget_per_phase))
  assert_eq(2000, last_phase_budget)  // 72000 - 7*10000 = 2000
}

test "string_and_numeric_integration" {
  // 字符串和数值集成测试
  // 测试各种字符串输入的问候函数
  let test_names = ["Alice", "Bob", "Charlie", "123", "测试", "🌟", ""]
  let expected_outputs = [
    "Hello, Alice!",
    "Hello, Bob!", 
    "Hello, Charlie!",
    "Hello, 123!",
    "Hello, 测试!",
    "Hello, 🌟!",
    "Hello, !"
  ]
  
  // 逐个测试
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, Charlie!", greet("Charlie"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
  
  // 结合数值计算的复杂场景
  let user_id = 12345
  let group_size = 8
  let group_number = divide_with_ceil(user_id, group_size)
  let group_name = "Group-" + group_number.to_string()
  
  assert_eq(1543, group_number)  // ceil(12345/8) = 1543
}