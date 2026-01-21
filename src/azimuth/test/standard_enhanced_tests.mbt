// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的补充测试

test "advanced_arithmetic_operations" {
  // 高级算术运算测试
  let x = 42
  let y = 17
  let z = 8
  
  // 复合运算测试
  let result1 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  let result2 = @azimuth.multiply(x, @azimuth.add(y, z))
  @azimuth.assert_eq(result1, result2)  // 验证分配律: 42*(17+8) = 42*17 + 42*8
  
  // 嵌套运算测试
  let complex_result = @azimuth.divide_with_ceil(
    @azimuth.add(@azimuth.multiply(15, 3), @azimuth.multiply(7, 4)),
    @azimuth.multiply(2, 3)
  )
  @azimuth.assert_eq(13, complex_result)  // ceil((45+28)/6) = ceil(73/6) = 13
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  @azimuth.assert_eq_string("Hello, Space Test!", @azimuth.greet("Space Test"))
  @azimuth.assert_eq_string("Hello, 123456789!", @azimuth.greet("123456789"))
  @azimuth.assert_eq_string("Hello, Special@Chars#!", @azimuth.greet("Special@Chars#"))
  @azimuth.assert_eq_string("Hello, MixedCASE!", @azimuth.greet("MixedCASE"))
  @azimuth.assert_eq_string("Hello,   Multiple   Spaces   !", @azimuth.greet("   Multiple   Spaces   "))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 除法向上取整的综合场景测试
  // 测试各种正数情况
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(13, 2))   // 6.5 -> 7
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))   // 4 -> 4
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 0.01 -> 1
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(999, 10)) // 99.9 -> 100
  
  // 测试负数情况
  @azimuth.assert_eq(-6, @azimuth.divide_with_ceil(-13, 2))  // -6.5 -> -6
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-12, 3))  // -4 -> -4
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-13, -3))  // 4.333... -> 4
  
  // 测试零值情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))    // 0 -> 0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))    // 除零保护
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))     // 双零保护
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
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算税费（向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_percent), 100)
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(@azimuth.add(subtotal, tax), shipping_cost), -discount_amount)
  
  @azimuth.assert_eq(581, final_total)  // 89*6 + ceil(534*8/100) + 25 - 50 = 534 + 43 + 25 - 50 = 552
}

test "mathematical_series_and_patterns" {
  // 数学级数和模式测试
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let series_sum = n * n  // 使用数学公式直接计算
  @azimuth.assert_eq(100, series_sum)
  
  // 使用库函数验证
  let manual_sum = @azimuth.add(1, @azimuth.add(3, @azimuth.add(5, @azimuth.add(7, @azimuth.add(9, @azimuth.add(11, @azimuth.add(13, @azimuth.add(15, @azimuth.add(17, 19)))))))))
  @azimuth.assert_eq(series_sum, manual_sum)
  
  // 测试等比数列：2 + 4 + 8 + 16 = 30
  let geometric_sum = @azimuth.add(2, @azimuth.add(4, @azimuth.add(8, 16)))
  @azimuth.assert_eq(30, geometric_sum)
}

test "resource_planning_optimization" {
  // 资源规划优化测试
  // 场景：项目资源分配
  let total_budget = 5000
  let senior_developer_cost = 800
  let junior_developer_cost = 400
  let project_duration = 3
  
  // 计算最优人员配置
  let max_senior = @azimuth.divide_with_ceil(total_budget, @azimuth.multiply(senior_developer_cost, project_duration))
  let remaining_budget = @azimuth.add(total_budget, -@azimuth.multiply(@azimuth.multiply(max_senior, senior_developer_cost), project_duration))
  let additional_junior = @azimuth.divide_with_ceil(remaining_budget, @azimuth.multiply(junior_developer_cost, project_duration))
  
  @azimuth.assert_eq(2, max_senior)      // ceil(5000/(800*3)) = ceil(5000/2400) = 2
  @azimuth.assert_eq(1, additional_junior)  // 剩余预算可支持1个初级开发者
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算测试
  // 场景：项目任务调度
  let total_tasks = 67
  let tasks_per_week = 15
  let weeks_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_week)
  
  // 计算工作日（每周5天）
  let work_days_needed = @azimuth.multiply(weeks_needed, 5)
  
  // 计算小时数（每天8小时）
  let total_hours = @azimuth.multiply(work_days_needed, 8)
  
  @azimuth.assert_eq(5, weeks_needed)      // ceil(67/15) = 5
  @azimuth.assert_eq(25, work_days_needed)  // 5 * 5 = 25
  @azimuth.assert_eq(200, total_hours)      // 25 * 8 = 200
}

test "data_structure_simulation" {
  // 数据结构模拟测试
  // 模拟数组操作：计算数组元素的总和与平均值
  let array_sum = @azimuth.add(10, @azimuth.add(20, @azimuth.add(30, @azimuth.add(40, 50))))
  let array_count = 5
  let average = @azimuth.divide_with_ceil(array_sum, array_count)
  
  @azimuth.assert_eq(150, array_sum)  // 10+20+30+40+50 = 150
  @azimuth.assert_eq(30, average)     // ceil(150/5) = 30
  
  // 模拟查找最大值的比较操作
  let max_value = @azimuth.add(20, @azimuth.divide_with_ceil(@azimuth.multiply(15, 2), 3))  // 模拟比较逻辑
  @azimuth.assert_eq(30, max_value)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate_percent = 6
  
  // 简化的年度复利计算
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate_percent), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, annual_rate_percent), 100))
  let year3_amount = @azimuth.add(year2_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year2_amount, annual_rate_percent), 100))
  let year4_amount = @azimuth.add(year3_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year3_amount, annual_rate_percent), 100))
  
  // 验证结果（由于使用向上取整，结果会略高于标准复利）
  @azimuth.assert_eq(12625, year4_amount)
}

test "error_boundary_and_validation" {
  // 错误边界和验证测试
  // 测试极端输入值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))  // 双零加法
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))  // 双零乘法
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))  // 零除法
  
  // 测试边界值
  let boundary_value = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(boundary_value, boundary_value))
  @azimuth.assert_eq(1000000000, @azimuth.multiply(boundary_value, boundary_value))
  
  // 测试负数边界
  @azimuth.assert_eq(0, @azimuth.add(1000000, -1000000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1, -1000000))
  
  // 字符串边界测试
  @azimuth.assert_eq_string("Hello, x!", @azimuth.greet("x"))  // 单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))    // 空字符串
}