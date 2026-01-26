// Azimuth 新增标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "arithmetics_associative_properties" {
  // 测试算术结合律性质
  let a = 2
  let b = 3
  let c = 4
  
  // 加法结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试负数场景的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-5, 10))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-15, 10))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, éspanol!", @azimuth.greet("éspanol"))
  @azimuth.assert_eq_string("Hello, 🎉!", @azimuth.greet("🎉"))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "resource_planning_calculation" {
  // 资源规划计算测试
  let total_items = 157
  let container_capacity = 24
  let containers_needed = @azimuth.divide_with_ceil(total_items, container_capacity)
  
  @azimuth.assert_eq(7, containers_needed)
  
  // 验证总容量
  let total_capacity = @azimuth.multiply(containers_needed, container_capacity)
  @azimuth.assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let unused_space = @azimuth.add(total_capacity, -total_items)
  @azimuth.assert_eq(11, unused_space)
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "large_number_precision" {
  // 大数精度测试
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1001, @azimuth.divide_with_ceil(1000000, 999))
  @azimuth.assert_eq(500000, @azimuth.add(1000000, -500000))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目时间估算
  let tasks = 47
  let team_members = 5
  let tasks_per_person_per_day = 2
  let working_days = 5
  
  // 计算每日处理能力
  let daily_capacity = @azimuth.multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的周数
  let weeks_needed = @azimuth.divide_with_ceil(tasks, @azimuth.multiply(daily_capacity, working_days))
  
  @azimuth.assert_eq(1, weeks_needed)
  
  // 验证总处理能力
  let weekly_capacity = @azimuth.multiply(daily_capacity, working_days)
  @azimuth.assert_eq(50, weekly_capacity)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试极值和边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试单位运算
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
}