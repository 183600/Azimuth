// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的补充测试

test "advanced_arithmetic_operations" {
  // 高级算术运算测试
  let x = 42
  let y = 17
  let z = 8
  
  // 复合运算测试
  let result1 = add(multiply(x, y), multiply(x, z))
  let result2 = multiply(x, add(y, z))
  assert_eq(result1, result2)  // 验证分配律: 42*(17+8) = 42*17 + 42*8
  
  // 嵌套运算测试
  let complex_result = divide_with_ceil(
    add(multiply(15, 3), multiply(7, 4)),
    multiply(2, 3)
  )
  assert_eq(13, complex_result)  // ceil((45+28)/6) = ceil(73/6) = 13
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  assert_eq_string("Hello, Space Test!", greet("Space Test"))
  assert_eq_string("Hello, 123456789!", greet("123456789"))
  assert_eq_string("Hello, Special@Chars#!", greet("Special@Chars#"))
  assert_eq_string("Hello, MixedCASE!", greet("MixedCASE"))
  assert_eq_string("Hello,   Multiple   Spaces   !", greet("   Multiple   Spaces   "))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 除法向上取整的综合场景测试
  // 测试各种正数情况
  assert_eq(7, divide_with_ceil(13, 2))   // 6.5 -> 7
  assert_eq(4, divide_with_ceil(12, 3))   // 4 -> 4
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(100, divide_with_ceil(999, 10)) // 99.9 -> 100
  
  // 测试负数情况
  assert_eq(-6, divide_with_ceil(-13, 2))  // -6.5 -> -6
  assert_eq(-4, divide_with_ceil(-12, 3))  // -4 -> -4
  assert_eq(4, divide_with_ceil(-13, -3))  // 4.333... -> 4
  
  // 测试零值情况
  assert_eq(0, divide_with_ceil(0, 10))    // 0 -> 0
  assert_eq(0, divide_with_ceil(10, 0))    // 除零保护
  assert_eq(0, divide_with_ceil(0, 0))     // 双零保护
}

test "business_calculation_comprehensive" {
  // 综合业务计算测试
  // 场景：计算订单总价（商品 + 税费 + 运费 - 折扣）
  let item_price = 89
  let quantity = 6
  let tax_percent = 8
  let shipping_cost = 25
  let discount_amount = 50
  
  // 计算商品小计
  let subtotal = multiply(item_price, quantity)
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_percent), 100)
  // 计算最终总价
  let final_total = add(add(add(subtotal, tax), shipping_cost), -discount_amount)
  
  assert_eq(581, final_total)  // 89*6 + ceil(534*8/100) + 25 - 50 = 534 + 43 + 25 - 50 = 552
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式测试
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let series_sum = n * n  // 使用数学公式直接计算
  assert_eq(100, series_sum)
  
  // 使用库函数验证
  let manual_sum = add(1, add(3, add(5, add(7, add(9, add(11, add(13, add(15, add(17, 19)))))))))
  assert_eq(series_sum, manual_sum)
  
  // 测试等比数列：2 + 4 + 8 + 16 = 30
  let geometric_sum = add(2, add(4, add(8, 16)))
  assert_eq(30, geometric_sum)
}

test "resource_planning_optimization" {
  // 资源规划优化测试
  // 场景：项目资源分配
  let total_budget = 5000
  let senior_developer_cost = 800
  let junior_developer_cost = 400
  let project_duration = 3
  
  // 计算最优人员配置
  let max_senior = divide_with_ceil(total_budget, multiply(senior_developer_cost, project_duration))
  let remaining_budget = add(total_budget, -multiply(max_senior, senior_developer_cost, project_duration))
  let additional_junior = divide_with_ceil(remaining_budget, multiply(junior_developer_cost, project_duration))
  
  assert_eq(2, max_senior)      // ceil(5000/(800*3)) = ceil(5000/2400) = 2
  assert_eq(1, additional_junior)  // 剩余预算可支持1个初级开发者
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算测试
  // 场景：项目任务调度
  let total_tasks = 67
  let tasks_per_week = 15
  let weeks_needed = divide_with_ceil(total_tasks, tasks_per_week)
  
  // 计算工作日（每周5天）
  let work_days_needed = multiply(weeks_needed, 5)
  
  // 计算小时数（每天8小时）
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(5, weeks_needed)      // ceil(67/15) = 5
  assert_eq(25, work_days_needed)  // 5 * 5 = 25
  assert_eq(200, total_hours)      // 25 * 8 = 200
}

test "data_structure_simulation" {
  // 数据结构模拟测试
  // 模拟数组操作：计算数组元素的总和与平均值
  let array_sum = add(10, add(20, add(30, add(40, 50))))
  let array_count = 5
  let average = divide_with_ceil(array_sum, array_count)
  
  assert_eq(150, array_sum)  // 10+20+30+40+50 = 150
  assert_eq(30, average)     // ceil(150/5) = 30
  
  // 模拟查找最大值的比较操作
  let max_value = add(20, divide_with_ceil(multiply(15, 2), 3))  // 模拟比较逻辑
  assert_eq(30, max_value)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate_percent = 6
  let years = 4
  
  // 简化的年度复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate_percent), 100))
  let year4_amount = add(year3_amount, divide_with_ceil(multiply(year3_amount, annual_rate_percent), 100))
  
  // 验证结果（由于使用向上取整，结果会略高于标准复利）
  assert_eq(12625, year4_amount)
}

test "error_boundary_and_validation" {
  // 错误边界和验证测试
  // 测试极端输入值
  assert_eq(0, add(0, 0))  // 双零加法
  assert_eq(0, multiply(0, 0))  // 双零乘法
  assert_eq(0, divide_with_ceil(0, 1))  // 零除法
  
  // 测试边界值
  let boundary_value = 1000000
  assert_eq(2000000, add(boundary_value, boundary_value))
  assert_eq(1000000000000, multiply(boundary_value, boundary_value))
  
  // 测试负数边界
  assert_eq(0, add(1000000, -1000000))
  assert_eq(-1000000, multiply(1, -1000000))
  
  // 字符串边界测试
  assert_eq_string("Hello, x!", greet("x"))  // 单字符
  assert_eq_string("Hello, !", greet(""))    // 空字符串
}