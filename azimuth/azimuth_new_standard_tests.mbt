// Azimuth 新增标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "arithmetics_associative_properties" {
  // 测试算术结合律性质
  let a = 2
  let b = 3
  let c = 4
  
  // 加法结合律: (a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
}

test "divide_with_ceil_negative_scenarios" {
  // 测试负数场景的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(-5, 10))
  assert_eq(-1, divide_with_ceil(-15, 10))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, éspanol!", greet("éspanol"))
  assert_eq_string("Hello, 🎉!", greet("🎉"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "resource_planning_calculation" {
  // 资源规划计算测试
  let total_items = 157
  let container_capacity = 24
  let containers_needed = divide_with_ceil(total_items, container_capacity)
  
  assert_eq(7, containers_needed)
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, container_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let unused_space = subtract(total_capacity, total_items)
  assert_eq(11, unused_space)
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "large_number_precision" {
  // 大数精度测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1001, divide_with_ceil(1000000, 999))
  assert_eq(500000, subtract(1000000, 500000))
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目时间估算
  let tasks = 47
  let team_members = 5
  let tasks_per_person_per_day = 2
  let working_days = 5
  
  // 计算每日处理能力
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(tasks, multiply(daily_capacity, working_days))
  
  assert_eq(1, weeks_needed)
  
  // 验证总处理能力
  let weekly_capacity = multiply(daily_capacity, working_days)
  assert_eq(50, weekly_capacity)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试极值和边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试单位运算
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(-5, multiply(5, -1))
  assert_eq(5, subtract(5, 0))
}