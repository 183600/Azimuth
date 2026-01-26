// 新增的标准 MoonBit 测试用例 - 覆盖核心功能和边界情况
// 包含 10 个高质量的测试用例

// 导入 azimuth 包中的函数
test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法单位元
  assert_eq(7, add(7, 0))
  assert_eq(-3, add(-3, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法单位元
  assert_eq(5, multiply(5, 1))
  assert_eq(-8, multiply(-8, 1))
  assert_eq(0, multiply(0, 1))
}

test "division_ceil_fractional_cases" {
  // 测试向上取整除法的小数情况
  assert_eq(2, divide_with_ceil(3, 2))    // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))    // 2.666... -> 3
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  assert_eq(11, divide_with_ceil(100, 9)) // 11.111... -> 12
}

test "greeting_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", greet(""))           // 空字符串
  assert_eq_string("Hello, A!", greet("A"))         // 单字符
  assert_eq_string("Hello,   !", greet("  "))       // 空格
  assert_eq_string("Hello, 123!", greet("123"))     // 数字
}

test "complex_business_calculations" {
  // 测试复杂业务计算场景
  // 计算订单总价：商品单价 × 数量 + 运费 - 折扣
  let unit_price = 45
  let quantity = 8
  let shipping = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(360, subtotal)           // 45 * 8 = 360
  assert_eq(375, total_before_discount) // 360 + 15 = 375
  assert_eq(355, final_total)        // 375 - 20 = 355
}

test "negative_number_comprehensive" {
  // 全面测试负数运算
  assert_eq(-10, add(-3, -7))        // 负数加法
  assert_eq(2, add(-5, 7))           // 正负数加法
  assert_eq(15, multiply(-3, -5))    // 负数乘法（得正）
  assert_eq(-12, multiply(3, -4))    // 正负数乘法（得负）
  assert_eq(-3, divide_with_ceil(-10, 3))  // 负数除法
  assert_eq(4, divide_with_ceil(-10, -3))  // 负数除负数
}

test "resource_allocation_scenarios" {
  // 测试资源分配场景
  let total_budget = 1000
  let project_cost = 150
  let team_size = 8
  
  // 计算可以支持的项目数量
  let max_projects = divide_with_ceil(total_budget, project_cost)
  
  // 计算人均预算
  let budget_per_person = divide_with_ceil(total_budget, team_size)
  
  assert_eq(7, max_projects)         // ceil(1000/150) = 7
  assert_eq(125, budget_per_person)  // ceil(1000/8) = 125
}

test "time_and_scheduling_calculations" {
  // 测试时间和调度计算
  let tasks_per_day = 6
  let total_tasks = 35
  let work_days = 5
  
  // 计算需要的工作日
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(days_needed, 8)
  
  // 计算工作周数
  let weeks_needed = divide_with_ceil(days_needed, work_days)
  
  assert_eq(6, days_needed)   // ceil(35/6) = 6
  assert_eq(48, total_hours)  // 6 * 8 = 48
  assert_eq(2, weeks_needed)  // ceil(6/5) = 2
}

test "string_processing_multilingual" {
  // 测试多语言字符串处理
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Français!", greet("Français"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, 🌟🚀!", greet("🌟🚀"))
}

test "mathematical_series_and_patterns" {
  // 测试数学级数和模式
  // 计算等差数列：3, 7, 11, 15, 19 的和
  let term1 = 3
  let diff = 4
  let n = 5
  
  let sum = add(add(add(add(term1, add(term1, diff)), add(term1, multiply(2, diff))), add(term1, multiply(3, diff))), add(term1, multiply(4, diff)))
  
  // 使用公式验证：n/2 * (2a + (n-1)d)
  let formula_result = divide_with_ceil(multiply(n, add(multiply(2, term1), multiply(n - 1, diff))), 2)
  
  assert_eq(55, sum)            // 3 + 7 + 11 + 15 + 19 = 55
  assert_eq(55, formula_result) // 5/2 * (6 + 16) = 55
}

test "error_handling_and_boundaries" {
  // 测试错误处理和边界情况
  // 除数为零的安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1000000))  // 极小商
  assert_eq(1000000, divide_with_ceil(1000000, 1))  // 极大商
  
  // 零值运算
  assert_eq(0, multiply(0, -1000))
  assert_eq(0, multiply(1000, 0))
  assert_eq(0, divide_with_ceil(0, 100))
}