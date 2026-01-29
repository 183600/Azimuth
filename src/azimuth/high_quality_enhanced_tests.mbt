// 高质量增强测试用例 - 针对 azimuth 库的全面测试
// 包含边界情况、错误处理和实际应用场景

test "enhanced_mathematical_identities" {
  // 测试数学恒等式和性质
  let a = 7
  let b = 13
  
  // 加法交换律和结合律
  assert_eq(add(a, b), add(b, a))
  assert_eq(add(add(a, b), 5), add(a, add(b, 5)))
  
  // 乘法交换律和分配律
  assert_eq(multiply(a, b), multiply(b, a))
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
  
  // 单位元性质
  assert_eq(a, add(a, 0))
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
}

test "enhanced_division_precision" {
  // 测试除法向上取整的精度和边界情况
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  assert_eq(4, divide_with_ceil(12, 3))  // 12/3 = 4 -> 4
  assert_eq(1, divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -11/3 = -3.666... -> -4
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "enhanced_string_processing" {
  // 测试字符串处理和特殊字符
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello,   !", greet("  "))  // 空格字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, 中文!", greet("中文"))  // 中文字符
  assert_eq_string("Hello, 🚀!", greet("🚀"))  // Emoji字符
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))  // 邮箱格式
}

test "enhanced_business_calculations" {
  // 复杂业务计算场景：电商订单计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 12
  let discount_threshold = 300
  let discount_amount = 20
  
  // 计算小计
  let subtotal = multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价（含运费）
  let total_before_discount = add(add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣（如果满足条件）
  let final_total = if total_before_discount > discount_threshold {
    add(total_before_discount, -discount_amount)
  } else {
    total_before_discount
  }
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(397, total_before_discount)
  assert_eq(377, final_total)
}

test "enhanced_resource_allocation" {
  // 资源分配和项目管理场景
  let total_budget = 10000
  let team_members = 8
  let project_duration_days = 15
  
  // 计算每人每日预算
  let daily_budget_per_person = divide_with_ceil(total_budget, multiply(team_members, project_duration_days))
  
  // 计算总项目成本（向上取整可能导致略微超出预算）
  let actual_total_cost = multiply(daily_budget_per_person, multiply(team_members, project_duration_days))
  
  // 计算预算差异
  let budget_variance = add(actual_total_cost, -total_budget)
  
  assert_eq(84, daily_budget_per_person)
  assert_eq(10080, actual_total_cost)
  assert_eq(80, budget_variance)
}

test "enhanced_scientific_calculations" {
  // 科学计算场景：物理实验数据处理
  let measurement1 = 15
  let measurement2 = 17
  let measurement3 = 16
  let measurement4 = 14
  let measurement5 = 18
  let measurement_count = 5
  
  // 计算总和
  let sum = add(add(add(add(measurement1, measurement2), measurement3), measurement4), measurement5)
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum, measurement_count)
  
  // 计算标准差的简化版本（范围）
  let max_value = 18
  let min_value = 14
  let range = add(max_value, -min_value)
  
  assert_eq(80, sum)
  assert_eq(16, average)
  assert_eq(4, range)
}

test "enhanced_performance_metrics" {
  // 性能指标计算场景
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  let concurrent_users = 50
  
  // 计算优化后的操作数
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  // 计算系统开销
  let system_overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_operations, -system_overhead)
  
  // 计算每用户性能分配
  let performance_per_user = divide_with_ceil(actual_performance, concurrent_users)
  
  assert_eq(3000, optimized_operations)
  assert_eq(450, system_overhead)
  assert_eq(2550, actual_performance)
  assert_eq(51, performance_per_user)
}

test "enhanced_edge_case_comprehensive" {
  // 综合边界情况和错误处理测试
  // 极值测试
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  
  // 零值运算
  assert_eq(0, multiply(0, 2147483647))
  assert_eq(0, multiply(-2147483648, 0))
  assert_eq(2147483647, add(2147483647, 0))
  
  // 除法边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
  assert_eq(0, divide_with_ceil(0, 2147483647))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 字符串边界测试
  let very_long_name = "This is a very long name that might cause issues in some systems but should work fine in MoonBit"
  let long_greeting = greet(very_long_name)
  assert_eq_string("Hello, " + very_long_name + "!", long_greeting)
}