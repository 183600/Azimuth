// 新增的 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-100, add(-60, -40))
  assert_eq(-100, add(-40, -60))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))  // 相等数
  assert_eq(1, divide_with_ceil(99, 100))  // 被除数小于除数
  assert_eq(100, divide_with_ceil(100, 1))  // 除数为1
  assert_eq(0, divide_with_ceil(0, 50))  // 被除数为0
  assert_eq(0, divide_with_ceil(50, 0))  // 除数为0
}

test "greet_function_special_characters" {
  // 测试问候函数处理特殊字符
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, test+123!", greet("test+123"))
  assert_eq_string("Hello, a_b_c!", greet("a_b_c"))
  assert_eq_string("Hello, 123-456!", greet("123-456"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let shipping_cost = 12
  let tax_rate = 8  // 8%
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping_cost)
  
  assert_eq(270, subtotal)  // 45 * 6
  assert_eq(22, tax)       // ceil(270 * 8 / 100) = ceil(21.6) = 22
  assert_eq(304, total)    // 270 + 22 + 12
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列前n项和
  let first_term = 3
  let common_diff = 2
  let n_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(n_terms, add(multiply(2, first_term), multiply(n_terms - 1, common_diff))),
    2
  )
  
  assert_eq(48, sum)  // 6/2 * (6 + 10) = 3 * 16 = 48
}

test "resource_allocation_scenario" {
  // 资源分配场景
  let total_budget = 1000
  let large_project_cost = 350
  let medium_project_cost = 200
  let small_project_cost = 80
  
  // 计算大项目数量
  let large_projects = divide_with_ceil(total_budget, large_project_cost)
  
  // 计算剩余预算
  let remaining_after_large = add(total_budget, -multiply(large_projects, large_project_cost))
  
  // 计算中等项目数量
  let medium_projects = divide_with_ceil(remaining_after_large, medium_project_cost)
  
  // 计算小项目数量
  let remaining_after_medium = add(remaining_after_large, -multiply(medium_projects, medium_project_cost))
  let small_projects = divide_with_ceil(remaining_after_medium, small_project_cost)
  
  assert_eq(3, large_projects)   // ceil(1000/350) = 3
  assert_eq(1, medium_projects)  // ceil(50/200) = 1
  assert_eq(1, small_projects)   // ceil(50/80) = 1
}

test "time_management_calculation" {
  // 时间管理计算
  let total_tasks = 75
  let tasks_per_hour = 8
  let working_hours_per_day = 8
  
  // 计算所需小时数
  let total_hours = divide_with_ceil(total_tasks, tasks_per_hour)
  
  // 计算所需天数
  let total_days = divide_with_ceil(total_hours, working_hours_per_day)
  
  assert_eq(10, total_hours)  // ceil(75/8) = 10
  assert_eq(2, total_days)    // ceil(10/8) = 2
}

test "string_greeting_multilingual_support" {
  // 多语言字符串问候测试
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 한국어!", greet("한국어"))
  assert_eq_string("Hello, русский!", greet("русский"))
  assert_eq_string("Hello, عربي!", greet("عربي"))
}

test "comprehensive_error_handling" {
  // 综合错误处理测试
  // 测试除零安全性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, add(2147483646, -2147483645))
  
  // 测试空字符串处理
  assert_eq_string("Hello, !", greet(""))
}