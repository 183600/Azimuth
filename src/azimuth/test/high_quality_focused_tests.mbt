// 高质量聚焦测试用例 - 针对 azimuth 库核心功能的全面测试
// 包含数学性质验证、边界条件和实际应用场景

test "mathematical_identity_properties" {
  // 验证数学单位元性质
  // 加法单位元：a + 0 = a
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法单位元：a * 1 = a
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-17, @azimuth.multiply(-17, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 1))
}

test "commutative_and_associative_laws" {
  // 验证交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律：a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律：a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
}

test "distributive_property_verification" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 9
  let c = 4
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)  // 7 * (9 + 4) = 7 * 9 + 7 * 4 = 91
}

test "divide_with_ceil_comprehensive_analysis" {
  // 全面分析向上取整除法的行为
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))   // 3.666... -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))    // 3.0 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))  // 0.01 -> 1
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -3.333... -> -3
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))  // -3.666... -> -4
  
  // 负负得正
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))  // 3.666... -> 4
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))     // 0/5 = 0 -> 0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))     // 除数为零返回0
}

test "string_greeting_multilingual_support" {
  // 测试多语言问候支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, Café!", @azimuth.greet("Café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
  
  // 特殊情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))           // 空字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))     // 纯数字
  @azimuth.assert_eq_string("Hello, a@b.com!", @azimuth.greet("a@b.com"))  // 邮箱格式
}

test "complex_business_workflow_simulation" {
  // 模拟复杂业务工作流程：电商订单处理
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_per_item = 3
  let discount_threshold = 300
  let discount_amount = 25
  
  // 计算商品小计
  let subtotal = @azimuth.multiply(unit_price, quantity)
  
  // 计算运费
  let total_shipping = @azimuth.multiply(shipping_per_item, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = @azimuth.add(@azimuth.add(subtotal, total_shipping), tax_amount)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    @azimuth.add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  @azimuth.assert_eq(356, subtotal)
  @azimuth.assert_eq(12, total_shipping)
  @azimuth.assert_eq(29, tax_amount)
  @azimuth.assert_eq(397, pre_discount_total)
  @azimuth.assert_eq(372, final_total)
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 1000
  let project_a_cost = 180
  let project_b_cost = 220
  let project_c_cost = 150
  
  // 计算已分配预算
  let allocated_budget = @azimuth.add(@azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = @azimuth.add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个80元）
  let small_projects_count = @azimuth.divide_with_ceil(remaining_budget, 80)
  
  @azimuth.assert_eq(550, allocated_budget)
  @azimuth.assert_eq(450, remaining_budget)
  @azimuth.assert_eq(6, small_projects_count)
}

test "time_and_scheduling_calculations" {
  // 时间和调度计算
  let tasks_per_day = 6
  let total_tasks = 47
  let working_days_per_week = 5
  
  // 计算需要的工作日
  let workdays_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数（向上取整）
  let weeks_needed = @azimuth.divide_with_ceil(workdays_needed, working_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_work_hours = @azimuth.multiply(workdays_needed, 8)
  
  @azimuth.assert_eq(8, workdays_needed)   // ceil(47/6) = 8
  @azimuth.assert_eq(2, weeks_needed)      // ceil(8/5) = 2
  @azimuth.assert_eq(64, total_work_hours) // 8 * 8 = 64
}

test "mathematical_sequence_calculations" {
  // 数学序列计算
  // 等差数列：首项a1=3，公差d=2，项数n=10
  let first_term = 3
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用等差数列求和公式：S = n/2 * (2a1 + (n-1)d)
  let sum_formula = @azimuth.divide_with_ceil(
    @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  // 验证：3 + 5 + 7 + 9 + 11 + 13 + 15 + 17 + 19 + 21 = 120
  @azimuth.assert_eq(120, sum_formula)
  
  // 等比数列验证：首项2，公比3，前4项和
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, @azimuth.multiply(2, 3)), @azimuth.multiply(2, @azimuth.multiply(3, 3))), @azimuth.multiply(2, @azimuth.multiply(3, @azimuth.multiply(3, 3))))
  @azimuth.assert_eq(80, geo_sum)  // 2 + 6 + 18 + 54 = 80
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 极值测试
  let large_positive = 1000000
  let large_negative = -1000000
  let zero = 0
  
  // 大数运算
  @azimuth.assert_eq(2000000, @azimuth.add(large_positive, large_positive))
  @azimuth.assert_eq(-2000000, @azimuth.add(large_negative, large_negative))
  @azimuth.assert_eq(0, @azimuth.add(large_positive, large_negative))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(zero, large_positive))
  @azimuth.assert_eq(0, @azimuth.multiply(large_positive, zero))
  @azimuth.assert_eq(large_positive, @azimuth.add(large_positive, zero))
  
  // 除法边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(zero, large_positive))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(large_positive, zero))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(zero, zero))
  
  // 特殊除法情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_positive, large_positive))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(large_negative, large_negative))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(large_negative, large_positive))
}