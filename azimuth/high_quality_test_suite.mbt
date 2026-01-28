// 高质量 MoonBit 测试套件
// 包含 10 个全面测试 azimuth 库功能的测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算：加法、乘法、减法
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(-2, subtract(3, 5))
  
  // 测试零值运算
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, subtract(5, 0))
  
  // 测试负数运算
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-8, subtract(-5, 3))
}

test "divide_with_ceil_comprehensive" {
  // 测试向上取整除法的各种情况
  assert_eq(4, divide_with_ceil(10, 3))  // 10/3 = 3.33 -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 测试负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.33 -> -3
  assert_eq(3, divide_with_ceil(-10, -3))  // -10/-3 = 3.33 -> 4
  assert_eq(-1, divide_with_ceil(10, -3))  // 10/-3 = -3.33 -> -3
  
  // 测试除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  
  // 测试特殊字符和数字
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
  assert_eq_string("Hello, space test!", greet("space test"))
  
  // 测试首尾空格
  assert_eq_string("Hello,  test !", greet(" test "))
}

test "mathematical_properties_verification" {
  // 验证数学基本性质
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

test "boundary_conditions_and_limits" {
  // 测试边界条件和极限值
  // 零值边界
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, subtract(0, 0))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 单位边界
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(0, subtract(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 大数运算（在合理范围内）
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
}

test "complex_business_calculation" {
  // 复杂业务计算场景：项目资源规划
  let team_members = 8
  let hourly_rate = 75
  let hours_per_day = 8
  let project_days = 15
  let budget_per_sprint = 10000
  
  // 计算总工时
  let total_hours = multiply(team_members, multiply(hours_per_day, project_days))
  assert_eq(960, total_hours)  // 8 * 8 * 15 = 960
  
  // 计算总预算
  let total_budget = multiply(total_hours, hourly_rate)
  assert_eq(72000, total_budget)  // 960 * 75 = 72000
  
  // 计算需要的冲刺次数
  let sprints_needed = divide_with_ceil(total_budget, budget_per_sprint)
  assert_eq(8, sprints_needed)  // ceil(72000/10000) = 8
  
  // 计算最终预算
  let final_budget = multiply(sprints_needed, budget_per_sprint)
  assert_eq(80000, final_budget)  // 8 * 10000 = 80000
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极值运算
  let max_safe = 1000000
  let min_safe = -1000000
  
  assert_eq(0, add(max_safe, -max_safe))
  assert_eq(2000000, add(max_safe, max_safe))
  assert_eq(-2000000, add(min_safe, min_safe))
  
  assert_eq(100000000, multiply(max_safe, max_safe))
  assert_eq(100000000, multiply(min_safe, min_safe))
  assert_eq(-100000000, multiply(max_safe, min_safe))
}

test "unicode_and_internationalization" {
  // Unicode 和国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, 🌙⭐!", greet("🌙⭐"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
}

test "sequential_calculation_pipeline" {
  // 连续计算流水线测试
  let initial_value = 10
  
  // 第一阶段：加法运算
  let stage1 = add(initial_value, 5)  // 15
  assert_eq(15, stage1)
  
  // 第二阶段：乘法运算
  let stage2 = multiply(stage1, 3)  // 45
  assert_eq(45, stage2)
  
  // 第三阶段：减法运算
  let stage3 = subtract(stage2, 20)  // 25
  assert_eq(25, stage3)
  
  // 第四阶段：向上取整除法
  let stage4 = divide_with_ceil(stage3, 8)  // ceil(25/8) = 4
  assert_eq(4, stage4)
  
  // 最终验证：完整流水线结果
  let final_result = divide_with_ceil(subtract(multiply(add(initial_value, 5), 3), 20), 8)
  assert_eq(4, final_result)
}

test "real_world_application_scenario" {
  // 真实世界应用场景：活动组织规划
  let participants = 127
  let tables_available = 15
  let chairs_per_table = 8
  let budget_per_table = 150
  let total_budget = 3000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(16, tables_needed)  // ceil(127/8) = 16
  
  // 验证桌子数量是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)  // 16 * 8 = 128
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)  // 128 - 127 = 1
  
  // 计算桌子总成本
  let total_cost = multiply(tables_needed, budget_per_table)
  assert_eq(2400, total_cost)  // 16 * 150 = 2400
  
  // 验证预算是否充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(600, remaining_budget)  // 3000 - 2400 = 600
  
  // 计算预算利用率
  let budget_utilization = divide_with_ceil(multiply(total_cost, 100), total_budget)
  assert_eq(80, budget_utilization)  // ceil(240000/3000) = 80%
}