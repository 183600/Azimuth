// 标准 MoonBit 测试用例 - 为 azimuth 项目新增
// 包含对核心功能的全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(12, 4))  // 12/4 = 3 -> 3
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // 10/-3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为零
  assert_eq(1, divide_with_ceil(1, 1))     // 相等数
  assert_eq(1, divide_with_ceil(99, 100))  // 被除数小于除数
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 😊!", greet("😊"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 8
  let tax_rate = 10  // 10%
  let shipping = 25
  let discount = 30
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), add(shipping, -discount))
  
  assert_eq(360, subtotal)  // 45 * 8
  assert_eq(36, tax)        // ceil(360 * 10 / 100)
  assert_eq(391, total)     // 360 + 36 + 25 - 30
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_diff = 4
  let num_terms = 6
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(78, sum)  // 6/2 * (6 + 20) = 3 * 26 = 78
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算剩余预算
  let used_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -used_budget)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 350
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(3900, used_budget)
  assert_eq(1100, remaining_budget)
  assert_eq(4, additional_projects)  // ceil(1100/350) = 4
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_hour = 4
  let working_hours = 8
  let total_tasks = 35
  
  // 计算需要的天数
  let daily_capacity = multiply(tasks_per_hour, working_hours)
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  
  // 计算总工作小时数
  let total_hours = multiply(days_needed, working_hours)
  
  assert_eq(32, daily_capacity)  // 4 * 8
  assert_eq(2, days_needed)      // ceil(35/32) = 2
  assert_eq(16, total_hours)     // 2 * 8
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端数值
  assert_eq(0, multiply(0, -2147483648))
  assert_eq(2147483647, add(2147483646, 1))
  assert_eq(-2147483648, add(-2147483647, -1))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quote\"!", greet("\"quote\""))
  
  // 测试长字符串
  let long_name = "This is a very long name with spaces and numbers 123456789"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
}