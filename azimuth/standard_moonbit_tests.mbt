// 标准 MoonBit 测试用例
// 为 azimuth 项目添加的高质量测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-8, 13))
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 123))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(1000000, multiply(1000, 1000))
}

test "divide_with_ceil_business_scenarios" {
  // 测试向上取整除法的业务场景
  // 场景：计算需要的运输车辆数量
  let packages = 58
  let capacity_per_truck = 12
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(60, total_capacity)
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间
  let tasks = 35
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = multiply(people, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(5, days_needed)  // ceil(35/(4*2)) = 5
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(40, total_capacity)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let x = 24
  let y = 18
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 15000
  let team_members = 7
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(2143, max_share)  // ceil(15000/7) = 2143
  
  // 验证总分配
  let total_allocated = multiply(max_share, team_members)
  assert_eq(15001, total_allocated)
}

test "error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 除零情况
  assert_eq(0, divide_with_ceil(25, 0))
  assert_eq(0, divide_with_ceil(-25, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(10, 10))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "real_world_calculation_scenario" {
  // 真实世界计算场景
  // 场景：计算活动安排
  let participants = 125
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(13, tables_needed)  // ceil(125/10) = 13
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(130, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(5, remaining_seats)
}