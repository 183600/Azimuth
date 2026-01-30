// 标准 MoonBit 测试套件
// 包含针对 azimuth 库核心功能的高质量测试用例

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法功能
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为零返回0
  assert_eq(1, divide_with_ceil(1, 100)) // 小数向上取整
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算场景：订单总价计算
  let unit_price = 45
  let quantity = 6
  let shipping_cost = 18
  let tax_rate = 8  // 8%
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total_cost = add(add(subtotal, tax_amount), shipping_cost)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)
  assert_eq(310, total_cost)
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
  // 资源分配优化问题
  let total_budget = 1500
  let project_a_cost = 350
  let project_b_cost = 280
  let project_c_cost = 420
  
  // 计算剩余预算
  let used_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -used_budget)
  
  // 计算可以支持的小项目数量（每个80单位）
  let small_projects = divide_with_ceil(remaining_budget, 80)
  
  assert_eq(1050, used_budget)
  assert_eq(450, remaining_budget)
  assert_eq(6, small_projects)
}

test "time_management_calculation" {
  // 时间管理计算：任务完成时间估算
  let tasks_per_day = 7
  let total_tasks = 52
  let break_days = 3
  
  // 计算工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总天数（包含休息日）
  let total_days = add(work_days_needed, break_days)
  
  assert_eq(8, work_days_needed)  // ceil(52/7) = 8
  assert_eq(11, total_days)       // 8 + 3 = 11
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试各种边界情况
  assert_eq(0, divide_with_ceil(0, 0))      // 0/0
  assert_eq(0, divide_with_ceil(100, 0))    // 正数/0
  assert_eq(0, divide_with_ceil(-100, 0))   // 负数/0
  
  // 测试大数运算
  let large_num = 50000
  assert_eq(100000, add(large_num, large_num))
  assert_eq(2500000000, multiply(large_num, 50000))
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 测试各种特殊字符和组合
  assert_eq_string("Hello, a+b=c!", greet("a+b=c"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello,  !", greet("  "))  // 空格
  assert_eq_string("Hello, \t\n!", greet("\t\n"))  // 制表符和换行符
}