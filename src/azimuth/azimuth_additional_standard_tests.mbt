// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "binary_arithmetic_operations" {
  // 测试二进制算术运算
  assert_eq(1, add(0, 1))
  assert_eq(2, add(1, 1))
  assert_eq(4, multiply(2, 2))
  assert_eq(8, multiply(2, 4))
}

test "negative_value_operations" {
  // 测试负值运算
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(5, -5))
}

test "decimal_simulation_operations" {
  // 模拟小数运算（使用整数表示）
  // 1.5 * 2 = 3 (用 15/10 * 2 = 30/10 = 3)
  let simulated_decimal = divide_with_ceil(multiply(15, 2), 10)
  assert_eq(3, simulated_decimal)
  
  // 2.7 + 1.3 = 4.0 (用 27/10 + 13/10 = 40/10 = 4)
  let decimal_sum = divide_with_ceil(add(27, 13), 10)
  assert_eq(4, decimal_sum)
}

test "greet_function_with_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, #hashtag!", greet("#hashtag"))
  assert_eq_string("Hello, $price!", greet("$price"))
  assert_eq_string("Hello, %percent!", greet("%percent"))
}

test "divide_with_ceil_edge_scenarios" {
  // 测试向上取整除法的边缘场景
  assert_eq(1, divide_with_ceil(2, 2))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(1, divide_with_ceil(-2, -2))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c / 10
  let result1 = divide_with_ceil(multiply(add(a, b), c), 10)
  assert_eq(90, result1)  // (10 + 20) * 30 / 10 = 30 * 30 / 10 = 90
  
  // a * b + c * 2
  let result2 = add(multiply(a, b), multiply(c, 2))
  assert_eq(260, result2)  // 10 * 20 + 30 * 2 = 200 + 60 = 260
}

test "string_greeting_numerical_input" {
  // 测试数字输入的字符串问候
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello, 0!", greet("0"))
  assert_eq_string("Hello, -100!", greet("-100"))
  assert_eq_string("Hello, 3.14!", greet("3.14"))
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_resources = 100
  let project1_needs = 30
  let project2_needs = 45
  let remaining = add(total_resources, -add(project1_needs, project2_needs))
  
  // 计算可以支持的小项目数量
  let small_project_size = 5
  let small_projects_count = divide_with_ceil(remaining, small_project_size)
  
  assert_eq(25, remaining)
  assert_eq(5, small_projects_count)
}

test "time_management_calculations" {
  // 测试时间管理计算
  let work_hours_per_day = 8
  let total_tasks = 50
  let tasks_per_hour = 2
  
  // 计算需要的工作天数
  let total_hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let work_days_needed = divide_with_ceil(total_hours_needed, work_hours_per_day)
  
  assert_eq(25, total_hours_needed)
  assert_eq(4, work_days_needed)
}

test "mathematical_identity_verification" {
  // 验证数学恒等式
  let x = 42
  
  // 验证 x + 0 = x
  assert_eq(x, add(x, 0))
  
  // 验证 x * 1 = x
  assert_eq(x, multiply(x, 1))
  
  // 验证 x / x = 1 (当 x != 0 时)
  assert_eq(1, divide_with_ceil(x, x))
  
  // 验证 0 / x = 0 (当 x != 0 时)
  assert_eq(0, divide_with_ceil(0, x))
}