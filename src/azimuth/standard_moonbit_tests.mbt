// 标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能测试

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 6 * (4 + 3) = 6 * 4 + 6 * 3 = 42
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(3, divide_with_ceil(8, 3))    // 8/3 = 2.666... -> 3
  assert_eq(2, divide_with_ceil(6, 3))    // 6/3 = 2 -> 2
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数情况
  assert_eq(-2, divide_with_ceil(-8, 3))   // -8/3 = -2.666... -> -2
  assert_eq(-3, divide_with_ceil(-9, 3))   // -9/3 = -3 -> -3
  
  // 零除数情况
  assert_eq(0, divide_with_ceil(100, 0))   // 除数为0时返回0
  assert_eq(0, divide_with_ceil(-100, 0))  // 除数为0时返回0
}

test "greet_function_multilingual_support" {
  // 测试多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Мир!", greet("Мир"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8  // 8%
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)     // 25 * 8 = 200
  assert_eq(16, tax)           // ceil(200 * 8 / 100) = ceil(16) = 16
  assert_eq(231, total)        // 200 + 16 + 15 = 231
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 5
  let common_diff = 3
  let num_terms = 6
  
  // 手动计算前6项：5, 8, 11, 14, 17, 20
  let sum1 = first_term
  let sum2 = add(sum1, add(first_term, common_diff))
  let sum3 = add(sum2, add(first_term, multiply(2, common_diff)))
  let sum4 = add(sum3, add(first_term, multiply(3, common_diff)))
  let sum5 = add(sum4, add(first_term, multiply(4, common_diff)))
  let sum6 = add(sum5, add(first_term, multiply(5, common_diff)))
  
  assert_eq(75, sum6)  // 5 + 8 + 11 + 14 + 17 + 20 = 75
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 1000
  let project_a_cost = 350
  let project_b_cost = 280
  let project_c_cost = 420
  
  // 计算三个项目总成本
  let total_cost = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining = add(total_budget, -total_cost)
  
  // 计算可以支持的小项目数量（每个50单位）
  let small_projects = divide_with_ceil(remaining, 50)
  
  assert_eq(1050, total_cost)
  assert_eq(-50, remaining)
  assert_eq(-1, small_projects)  // 预算不足，返回-1表示无法支持
}

test "time_management_calculation" {
  // 时间管理计算
  let tasks_per_day = 7
  let total_tasks = 45
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  assert_eq(7, work_days_needed)  // ceil(45/7) = 7
  assert_eq(2, weeks_needed)      // ceil(7/5) = 2
}

test "string_processing_validation" {
  // 字符串处理验证
  let username = "Alice"
  let domain = "example.com"
  let email = username + "@" + domain
  
  // 使用greet函数验证邮箱格式
  let greeting = greet(email)
  
  assert_eq_string("Hello, Alice@example.com!", greeting)
  assert_eq_string("Hello, @!", greet("@"))
  assert_eq_string("Hello, a.b.c!", greet("a.b.c"))
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  // 测试大数值
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000, multiply(10000, 100000))
  
  // 测试零值运算
  assert_eq(0, multiply(0, 123456))
  assert_eq(123456, add(0, 123456))
  assert_eq(0, divide_with_ceil(0, 123456))
  
  // 测试负数边界
  assert_eq(-1, divide_with_ceil(-1, 100))
  assert_eq(-1000, add(-500, -500))
  assert_eq(250000, multiply(-500, -500))
}