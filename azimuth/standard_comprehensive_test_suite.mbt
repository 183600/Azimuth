// 标准 MoonBit 测试套件 - 包含10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_comprehensive_coverage" {
  // 全面测试加法函数的各种情况
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-7, -3))
  assert_eq(4, add(-7, 11))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(-100, add(-150, 50))
  assert_eq(2147483647, add(2147483646, 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边缘情况和负数
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(0, multiply(-5, 0))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(-1, -1))
  assert_eq(-1, multiply(1, -1))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符和特殊符号支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
  assert_eq_string("Hello, !@#$%^&*()", greet("!@#$%^&*()"))
}

test "divide_with_ceil_business_logic" {
  // 测试向上取整除法的实际业务逻辑场景
  // 场景：计算需要的运输车辆数量
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
  
  // 验证剩余容量
  let unused_capacity = subtract(total_capacity, packages)
  assert_eq(3, unused_capacity)
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间和资源分配
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = multiply(people, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(tasks, daily_capacity)
  assert_eq(4, days_needed)  // ceil(25/(4*2)) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(days_needed, daily_capacity)
  assert_eq(32, total_capacity)
  
  // 验证效率
  let efficiency = divide_with_ceil(tasks, multiply(days_needed, people))
  assert_eq(2, efficiency)
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
  
  // 结合律
  assert_eq(
    multiply(multiply(x, y), 2),
    multiply(x, multiply(y, 2))
  )
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
  
  // 验证超支
  let over_budget = subtract(total_allocated, total_budget)
  assert_eq(2, over_budget)
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零情况
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  
  // 大数测试
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, space test!", greet("space test"))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello,   spaces!", greet("  spaces  "))
}

test "real_world_calculation_scenario" {
  // 真实世界计算场景
  // 场景：计算活动安排和座位分配
  let participants = 87
  let chairs_per_table = 8
  let tables_available = 12
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 验证是否有足够的桌子
  let enough_tables = tables_needed <= tables_available
  assert_true(enough_tables)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let empty_seats = subtract(total_seats, participants)
  assert_eq(1, empty_seats)
  
  // 计算座位利用率
  let utilization_percent = divide_with_ceil(multiply(participants, 100), total_seats)
  assert_eq(99, utilization_percent)  // ceil(87*100/88) = 99%
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}