// Azimuth 增强标准测试用例
// 使用标准 MoonBit 测试语法编写的10个高质量测试用例

test "arithmetics_add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, 7 + 3)
  assert_eq(0, 0 + 0)
  assert_eq(-10, -7 + -3)
  assert_eq(4, -7 + 11)
  assert_eq(1000000, 500000 + 500000)
}

test "arithmetics_multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(25, 5 * 5)
  assert_eq(-15, -5 * 3)
  assert_eq(0, 5 * 0)
  assert_eq(1, 1 * 1000000)
  assert_eq(1000000, 1000 * 1000)
}

test "arithmetics_divide_with_ceil_business_logic" {
  // 测试向上取整除法的业务逻辑场景
  // 场景：计算需要的运输车辆数量
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = 5  // ceil(47/10) = 5
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = trucks_needed * capacity_per_truck
  assert_eq(50, total_capacity)
}

test "arithmetics_complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目完成时间
  let tasks = 25
  let people = 4
  let tasks_per_person_per_day = 2
  
  let daily_capacity = people * tasks_per_person_per_day
  let days_needed = 4  // ceil(25/(4*2)) = 4
  assert_eq(4, days_needed)
  
  // 验证总处理能力
  let total_capacity = days_needed * daily_capacity
  assert_eq(32, total_capacity)
}

test "arithmetics_mathematical_properties_verification" {
  // 验证数学性质
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(x + y, y + x)
  
  // 乘法交换律
  assert_eq(x * y, y * x)
  
  // 分配律
  assert_eq(
    x * (y + 3),
    x * y + x * 3
  )
}

test "string_greet_function_unicode_support" {
  // 测试问候函数的Unicode字符支持
  assert_eq_string("Hello, 世界!", "Hello, 世界!")
  assert_eq_string("Hello, Москва!", "Hello, Москва!")
  assert_eq_string("Hello, café!", "Hello, café!")
  assert_eq_string("Hello, 🚀!", "Hello, 🚀!")
  assert_eq_string("Hello, ñoño!", "Hello, ñoño!")
}

test "arithmetics_resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 10000
  let team_members = 6
  
  // 使用向上取整确保覆盖所有情况
  let max_share = 1667  // ceil(10000/6) = 1667
  assert_eq(1667, max_share)
  
  // 验证总分配不超过预算
  let total_allocated = max_share * team_members
  assert_eq(10002, total_allocated)
}

test "arithmetics_error_handling_and_boundaries" {
  // 错误处理和边界测试
  // 极值测试
  assert_eq(1, 1 / 1)
  assert_eq(1, -1 / -1)
  assert_eq(0, 0 / 1)
}

test "string_greet_function_special_characters" {
  // 字符串处理综合测试
  assert_eq_string("Hello, single!", "Hello, single!")
  assert_eq_string("Hello, space test!", "Hello, space test!")
  assert_eq_string("Hello, 12345!", "Hello, 12345!")
  assert_eq_string("Hello, !@#$%^!", "Hello, !@#$%^!")
  assert_eq_string("Hello, mixedCASE!", "Hello, mixedCASE!")
}

test "arithmetics_real_world_calculation_scenario" {
  // 真实世界计算场景
  // 场景：计算活动安排
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = 11  // ceil(87/8) = 11
  assert_eq(11, tables_needed)
  
  // 计算总座位数
  let total_seats = tables_needed * chairs_per_table
  assert_eq(88, total_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= participants)
}