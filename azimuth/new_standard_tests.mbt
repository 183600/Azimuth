// 新增的标准 MoonBit 测试用例
// 为 azimuth 库添加额外的测试覆盖

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
  assert_eq(0, subtract(0, 0))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果处理
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
  assert_eq(6, divide_with_ceil(11, 2))
}

test "complex_business_calculation" {
  // 复杂业务计算场景：项目资源规划
  let team_members = 12
  let hours_per_day = 8
  let working_days = 15
  let total_hours = multiply(team_members, multiply(hours_per_day, working_days))
  
  let tasks_per_hour = 2
  let total_tasks = multiply(total_hours, tasks_per_hour)
  
  let tasks_per_sprint = 100
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(1440, total_hours)  // 12 * 8 * 15
  assert_eq(2880, total_tasks)  // 1440 * 2
  assert_eq(29, sprints_needed)  // ceil(2880/100)
}

test "mathematical_identity_properties" {
  // 测试数学恒等式性质
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边缘情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
}

test "divide_with_ceil_extreme_values" {
  // 测试向上取整除法的极值情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 1000))
  assert_eq(1000, divide_with_ceil(1000, 1))
  assert_eq(-1000, divide_with_ceil(-1000, 1))
}

test "sequential_operations_chain" {
  // 测试连续操作链
  let initial = 5
  let step1 = add(initial, 3)        // 8
  let step2 = multiply(step1, 2)     // 16
  let step3 = divide_with_ceil(step2, 5)  // 4
  let step4 = subtract(step3, 1)     // 3
  let step5 = multiply(step4, 3)     // 9
  let final_result = divide_with_ceil(step5, 4)  // 3
  
  assert_eq(3, final_result)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 50000
  let departments = 7
  let min_budget_per_dept = divide_with_ceil(total_budget, departments)
  
  assert_eq(7143, min_budget_per_dept)  // ceil(50000/7)
  
  // 验证分配结果
  let total_allocated = multiply(min_budget_per_dept, departments)
  assert_eq(50001, total_allocated)
  
  // 计算预算溢出
  let budget_overflow = subtract(total_allocated, total_budget)
  assert_eq(1, budget_overflow)
}

test "string_processing_unicode_extended" {
  // 扩展Unicode字符串处理测试
  assert_eq_string("Hello, 🌙⭐🌟!", greet("🌙⭐🌟"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, ελληνικά!", greet("ελληνικά"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 6 * (4 + 3) = 6 * 4 + 6 * 3 = 42
}