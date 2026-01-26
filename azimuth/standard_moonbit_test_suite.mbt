// 标准 MoonBit 测试套件 - 包含 10 个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(8, add(5, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(15, multiply(5, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "divide_with_ceil_business_scenarios" {
  // 测试向上取整除法的业务场景
  // 场景：计算需要的运输车辆数量
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
}

test "subtract_function_validation" {
  // 测试减法函数的各种情况
  assert_eq(2, subtract(5, 3))
  assert_eq(8, subtract(5, -3))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-2, subtract(-5, -3))
  assert_eq(0, subtract(5, 5))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间和资源需求
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = multiply(people, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(32, total_capacity)
  
  // 计算剩余处理能力
  let remaining_capacity = subtract(total_capacity, tasks)
  assert_eq(7, remaining_capacity)
}

test "mathematical_properties_verification" {
  // 验证基本数学性质
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
  
  // 减法的逆运算性质
  assert_eq(x, add(subtract(x, y), y))
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
  
  // 计算溢出量
  let overflow = subtract(total_allocated, total_budget)
  assert_eq(2, overflow)
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let chairs_per_table = 8
  let tables_available = 12
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 验证桌子是否足够
  assert_true(tables_available >= tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= participants)
}