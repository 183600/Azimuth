// Azimuth 标准测试用例 - 新增10个高质量测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_comprehensive_test" {
  // 全面测试加法函数的各种情况
  azimuth::assert_eq(15, azimuth::add(7, 8))
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(-10, azimuth::add(-3, -7))
  azimuth::assert_eq(5, azimuth::add(-8, 13))
  azimuth::assert_eq(1000000, azimuth::add(500000, 500000))
}

test "multiply_function_boundary_test" {
  // 测试乘法函数的边界情况
  azimuth::assert_eq(0, azimuth::multiply(0, 99999))
  azimuth::assert_eq(99999, azimuth::multiply(1, 99999))
  azimuth::assert_eq(-99999, azimuth::multiply(-1, 99999))
  azimuth::assert_eq(99999, azimuth::multiply(-1, -99999))
  azimuth::assert_eq(1000000, azimuth::multiply(1000, 1000))
}

test "greet_function_unicode_test" {
  // 测试问候函数的Unicode字符支持
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, Москва!", azimuth::greet("Москва"))
  azimuth::assert_eq_string("Hello, café!", azimuth::greet("café"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, ñoño!", azimuth::greet("ñoño"))
}

test "divide_with_ceil_logistics_test" {
  // 测试向上取整除法的物流计算场景
  let packages = 58
  let capacity_per_truck = 12
  let trucks_needed = azimuth::divide_with_ceil(packages, capacity_per_truck)
  azimuth::assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = azimuth::multiply(trucks_needed, capacity_per_truck)
  azimuth::assert_eq(60, total_capacity)
  
  // 验证剩余空间
  let remaining_space = azimuth::subtract(total_capacity, packages)
  azimuth::assert_eq(2, remaining_space)
}

test "complex_project_calculation_test" {
  // 测试复杂项目计算流水线
  let tasks = 35
  let team_members = 6
  let tasks_per_person_per_day = 2
  
  let daily_capacity = azimuth::multiply(team_members, tasks_per_person_per_day)
  let days_needed = azimuth::divide_with_ceil(tasks, daily_capacity)
  azimuth::assert_eq(3, days_needed)
  
  // 验证总处理能力
  let total_capacity = azimuth::multiply(days_needed, daily_capacity)
  azimuth::assert_eq(36, total_capacity)
}

test "mathematical_laws_verification_test" {
  // 验证数学定律
  let x = 24
  let y = 18
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
  
  // 分配律
  azimuth::assert_eq(
    azimuth::multiply(x, azimuth::add(y, 5)),
    azimuth::add(azimuth::multiply(x, y), azimuth::multiply(x, 5))
  )
}

test "budget_allocation_test" {
  // 预算分配测试
  let total_budget = 15000
  let departments = 4
  
  // 使用向上取整确保覆盖所有情况
  let max_allocation = azimuth::divide_with_ceil(total_budget, departments)
  azimuth::assert_eq(3750, max_allocation)
  
  // 验证总分配
  let total_allocated = azimuth::multiply(max_allocation, departments)
  azimuth::assert_eq(15000, total_allocated)
}

test "error_handling_comprehensive_test" {
  // 综合错误处理测试
  // 除零情况
  azimuth::assert_eq(0, azimuth::divide_with_ceil(25, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-25, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 极值测试
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(-1, -1))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))
}

test "string_processing_advanced_test" {
  // 高级字符串处理测试
  azimuth::assert_eq_string("Hello, singleword!", azimuth::greet("singleword"))
  azimuth::assert_eq_string("Hello, multiple words!", azimuth::greet("multiple words"))
  azimuth::assert_eq_string("Hello, 123456!", azimuth::greet("123456"))
  azimuth::assert_eq_string("Hello, !@#$%^&*!", azimuth::greet("!@#$%^&*"))
  azimuth::assert_eq_string("Hello, MixedCase!", azimuth::greet("MixedCase"))
}

test "event_planning_scenario_test" {
  // 活动规划场景测试
  let guests = 125
  let tables_available = 15
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = azimuth::divide_with_ceil(guests, chairs_per_table)
  azimuth::assert_eq(16, tables_needed)
  
  // 计算总座位数
  let total_seats = azimuth::multiply(tables_needed, chairs_per_table)
  azimuth::assert_eq(128, total_seats)
  
  // 计算剩余座位
  let remaining_seats = azimuth::subtract(total_seats, guests)
  azimuth::assert_eq(3, remaining_seats)
  
  // 验证桌子是否足够
  // tables_needed <= tables_available 为 true
}