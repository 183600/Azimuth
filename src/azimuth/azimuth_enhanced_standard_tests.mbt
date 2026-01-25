// 新增的标准 MoonBit 测试用例 - 针对 azimuth 库的增强测试
// 包含边界情况、错误处理和实际应用场景

test "extreme_boundary_values_addition" {
  // 测试加法的极端边界值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与最小值+1相加
  assert_eq(-2147483647, add(-2147483648, 1))  // 最小值加1
  assert_eq(2147483646, add(2147483647, -1))   // 最大值减1
  assert_eq(1, add(-2147483647, 2147483648))    // 接近边界的负数加正数
}

test "extreme_boundary_values_multiplication" {
  // 测试乘法的极端边界值
  assert_eq(0, multiply(2147483647, 0))         // 最大值乘以0
  assert_eq(0, multiply(-2147483648, 0))        // 最小值乘以0
  assert_eq(2147483647, multiply(2147483647, 1)) // 最大值乘以1
  assert_eq(-2147483648, multiply(-2147483648, 1)) // 最小值乘以1
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 全面测试向上取整除法的边界情况
  assert_eq(0, divide_with_ceil(0, 1))          // 0除以任何数
  assert_eq(0, divide_with_ceil(0, -1))         // 0除以负数
  assert_eq(0, divide_with_ceil(0, 2147483647)) // 0除以最大值
  assert_eq(1, divide_with_ceil(1, 1))          // 1除以1
  assert_eq(-1, divide_with_ceil(-1, 1))        // -1除以1
  assert_eq(1, divide_with_ceil(-1, -1))        // -1除以-1
}

test "complex_financial_calculations" {
  // 复杂金融计算场景：复利计算
  let principal = 10000
  let rate1 = 5  // 第一年利率
  let rate2 = 6  // 第二年利率
  let rate3 = 7  // 第三年利率
  
  // 计算三年复利
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate1), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate2), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate3), 100))
  
  assert_eq(10500, year1)  // 10000 + 5% = 10500
  assert_eq(11130, year2)  // 10500 + 6% = 11130
  assert_eq(11909, year3)  // 11130 + 7% = 11909
}

test "advanced_string_processing_scenarios" {
  // 高级字符串处理场景
  assert_eq_string("Hello, 🌟🚀!", greet("🌟🚀"))           // 多个表情符号
  assert_eq_string("Hello, line1\nline2!", greet("line1\nline2")) // 换行符
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))   // 引号
  assert_eq_string("Hello, space separated!", greet("space separated")) // 空格分隔
}

test "mathematical_sequence_calculations" {
  // 数列计算场景
  // 计算等差数列前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let arithmetic_sum = multiply(n, n)
  assert_eq(100, arithmetic_sum)
  
  // 计算等比数列前n项和：2 + 4 + 8 + ... + 2^n = 2^(n+1) - 2
  let geometric_sum = add(multiply(2, 1024), -2)  // 2^11 - 2 = 2048 - 2 = 2046
  assert_eq(2046, geometric_sum)
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以支持的小项目数量（每个300）
  let small_projects = divide_with_ceil(remaining, 300)
  
  assert_eq(3500, allocated)
  assert_eq(1500, remaining)
  assert_eq(5, small_projects)
}

test "time_and_scheduling_calculations" {
  // 时间调度计算
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作日
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算需要的周数
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = multiply(work_days_needed, 8)
  
  assert_eq(11, work_days_needed)  // ceil(87/8) = 11
  assert_eq(3, weeks_needed)       // ceil(11/5) = 3
  assert_eq(88, total_hours)       // 11 * 8 = 88
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 测试各种边界和异常情况
  
  // 除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 极大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
}

test "internationalization_and_localization" {
  // 国际化和本地化测试
  // 测试各种语言的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Мир!", greet("Мир"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, naïve!", greet("naïve"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, Zürich!", greet("Zürich"))
}

test "comprehensive_business_workflow" {
  // 综合业务工作流测试
  // 模拟完整的电商订单处理流程
  
  let product_price = 299
  let quantity = 3
  let tax_rate = 8
  let shipping_per_item = 15
  let discount_threshold = 800
  let discount_amount = 50
  let loyalty_points = 120
  
  // 步骤1：计算商品总价
  let subtotal = multiply(product_price, quantity)
  
  // 步骤2：计算运费
  let total_shipping = multiply(shipping_per_item, quantity)
  
  // 步骤3：计算税费
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 步骤4：计算折扣前总价
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  
  // 步骤5：应用折扣（如果满足条件）
  let after_discount = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  // 步骤6：应用积分抵扣
  let points_discount = divide_with_ceil(loyalty_points, 10)  // 10积分=1元
  let final_total = add(after_discount, -points_discount)
  
  // 验证计算结果
  assert_eq(897, subtotal)           // 299 * 3 = 897
  assert_eq(45, total_shipping)      // 15 * 3 = 45
  assert_eq(72, tax_amount)          // ceil(897 * 8 / 100) = ceil(71.76) = 72
  assert_eq(1014, pre_discount_total) // 897 + 45 + 72 = 1014
  assert_eq(964, after_discount)     // 1014 - 50 = 964
  assert_eq(12, points_discount)     // ceil(120 / 10) = 12
  assert_eq(952, final_total)        // 964 - 12 = 952
}