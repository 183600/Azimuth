// 增强测试用例 - 为 clean_test 项目添加
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

test "add_function_comprehensive" {

test "add_function_comprehensive" {
  // 全面测试加法函数
  @clean_test.assert_eq(15, @clean_test.add(7, 8))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
  @clean_test.assert_eq(-10, @clean_test.add(-3, -7))
  @clean_test.assert_eq(5, @clean_test.add(-8, 13))
  @clean_test.assert_eq(100000, @clean_test.add(50000, 50000))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  @clean_test.assert_eq(42, @clean_test.multiply(6, 7))
  @clean_test.assert_eq(0, @clean_test.multiply(15, 0))
  @clean_test.assert_eq(-24, @clean_test.multiply(-3, 8))
  @clean_test.assert_eq(24, @clean_test.multiply(-3, -8))
  @clean_test.assert_eq(1000000, @clean_test.multiply(1000, 1000))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @clean_test.assert_eq_string("Hello, MoonBit!", @clean_test.greet("MoonBit"))
  @clean_test.assert_eq_string("Hello, World!", @clean_test.greet("World"))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
  @clean_test.assert_eq_string("Hello, 123!", @clean_test.greet("123"))
  @clean_test.assert_eq_string("Hello, 中文!", @clean_test.greet("中文"))
}

test "mathematical_properties_validation" {
  // 验证数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @clean_test.assert_eq(
    @clean_test.add(a, b),
    @clean_test.add(b, a)
  )
  
  // 乘法交换律
  @clean_test.assert_eq(
    @clean_test.multiply(a, b),
    @clean_test.multiply(b, a)
  )
  
  // 验证具体数值
  @clean_test.assert_eq(20, @clean_test.add(12, 8))
  @clean_test.assert_eq(96, @clean_test.multiply(12, 8))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = @clean_test.multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = total_tasks / sprint_capacity
  if total_tasks % sprint_capacity > 0 {
    sprints_needed = sprints_needed + 1
  }
  
  @clean_test.assert_eq(48, total_tasks)  // 6 * 8 = 48
  @clean_test.assert_eq(4, sprints_needed)  // ceil(48/15) = 4
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 大数运算
  @clean_test.assert_eq(2000000, @clean_test.add(1000000, 1000000))
  @clean_test.assert_eq(100000000, @clean_test.multiply(10000, 10000))
  
  // 特殊问候情况
  @clean_test.assert_eq_string("Hello,   !", @clean_test.greet("  "))
  @clean_test.assert_eq_string("Hello, special@chars!", @clean_test.greet("special@chars"))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 35
  let items_per_package = 8
  let mut packages_needed = total_items / items_per_package
  if total_items % items_per_package > 0 {
    packages_needed = packages_needed + 1
  }
  
  @clean_test.assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = @clean_test.multiply(packages_needed, items_per_package)
  @clean_test.assert_eq(40, total_capacity)
  
  // 验证剩余空间
  let remaining_space = total_capacity - total_items
  @clean_test.assert_eq(5, remaining_space)
}

test "unicode_and_emoji_support" {
  // Unicode和表情符号支持测试
  @clean_test.assert_eq_string("Hello, 🌟!", @clean_test.greet("🌟"))
  @clean_test.assert_eq_string("Hello, ñoño!", @clean_test.greet("ñoño"))
  @clean_test.assert_eq_string("Hello, Москва!", @clean_test.greet("Москва"))
  @clean_test.assert_eq_string("Hello, 🏳️‍🌈!", @clean_test.greet("🏳️‍🌈"))
  @clean_test.assert_eq_string("Hello, café!", @clean_test.greet("café"))
}

test "overflow_protection_scenarios" {
  // 溢出保护场景测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, 1))
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, max_val))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, -1))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, min_val))
  
  // 乘法溢出保护
  @clean_test.assert_eq(max_val, @clean_test.multiply(max_val, 2))
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, 2))
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, -1))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let attendees = 87
  let tables = 8
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let mut tables_needed = attendees / chairs_per_table
  if attendees % chairs_per_table > 0 {
    tables_needed = tables_needed + 1
  }
  
  @clean_test.assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = @clean_test.multiply(tables_needed, chairs_per_table)
  @clean_test.assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = available_seats - attendees
  @clean_test.assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  @clean_test.assert_true(available_seats >= attendees)
}