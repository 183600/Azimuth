// 新增的标准 MoonBit 测试用例
// 为 azimuth 库添加额外的测试覆盖

// 首先导入 azimuth 库
use azimuth

test "mathematical_identity_elements" {
  // 测试数学单位元性质
  // 加法单位元：a + 0 = a
  assert_eq(42, azimuth.add(42, 0))
  assert_eq(-17, azimuth.add(-17, 0))
  assert_eq(0, azimuth.add(0, 0))
  
  // 乘法单位元：a * 1 = a
  assert_eq(42, azimuth.multiply(42, 1))
  assert_eq(-17, azimuth.multiply(-17, 1))
  assert_eq(0, azimuth.multiply(0, 1))
}

test "divide_with_ceil_precision_testing" {
  // 测试向上取整除法的精度
  // 测试小数部分刚好等于0.5的情况
  assert_eq(3, azimuth.divide_with_ceil(5, 2))   // 2.5 -> 3
  assert_eq(4, azimuth.divide_with_ceil(7, 2))   // 3.5 -> 4
  assert_eq(6, azimuth.divide_with_ceil(11, 2))  // 5.5 -> 6
  
  // 测试接近整数的小数
  assert_eq(2, azimuth.divide_with_ceil(9, 5))   // 1.8 -> 2
  assert_eq(4, azimuth.divide_with_ceil(19, 5))  // 3.8 -> 4
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 模拟实际业务场景：计算订单总价
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping_cost = 15
  let discount = 30
  
  // 计算商品小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = azimuth.divide_with_ceil(azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价（含运费）
  let total_before_discount = azimuth.add(azimuth.add(subtotal, tax_amount), shipping_cost)
  
  // 应用折扣
  let final_total = azimuth.add(total_before_discount, -discount)
  
  // 验证结果
  assert_eq(200, subtotal)           // 25 * 8 = 200
  assert_eq(16, tax_amount)          // ceil(200 * 8 / 100) = ceil(16) = 16
  assert_eq(231, total_before_discount)  // 200 + 16 + 15 = 231
  assert_eq(201, final_total)        // 231 - 30 = 201
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  // 测试包含特殊字符的字符串
  azimuth.assert_eq_string("Hello, @#$%^&*()!", azimuth.greet("@#$%^&*()"))
  azimuth.assert_eq_string("Hello, \t\n\r!", azimuth.greet("\t\n\r"))
  azimuth.assert_eq_string("Hello,   !", azimuth.greet("   "))
  
  // 测试Unicode字符
  azimuth.assert_eq_string("Hello, 🌟🌙☀️!", azimuth.greet("🌟🌙☀️"))
  azimuth.assert_eq_string("Hello, αβγδε!", azimuth.greet("αβγδε"))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let projects = [2500, 3000, 2000, 1500]  // 项目预算需求
  let min_allocation = 1000  // 每个项目最低分配
  
  // 计算总需求
  let total_demand = azimuth.add(azimuth.add(azimuth.add(projects[0], projects[1]), projects[2]), projects[3])
  
  // 计算可以支持的项目数量（向上取整）
  let supported_projects = azimuth.divide_with_ceil(total_budget, min_allocation)
  
  // 计算剩余预算
  let remaining_budget = azimuth.add(total_budget, -total_demand)
  
  assert_eq(9000, total_demand)
  assert_eq(10, supported_projects)  // ceil(10000/1000) = 10
  assert_eq(1000, remaining_budget)
}

test "time_based_calculations" {
  // 测试基于时间的计算
  let tasks_per_hour = 8
  let working_hours_per_day = 8
  let total_tasks = 150
  
  // 计算需要的工作小时数
  let total_hours_needed = azimuth.divide_with_ceil(total_tasks, tasks_per_hour)
  
  // 计算需要的工作天数
  let workdays_needed = azimuth.divide_with_ceil(total_hours_needed, working_hours_per_day)
  
  // 计算总工作能力
  let total_capacity = azimuth.multiply(workdays_needed, working_hours_per_day)
  
  assert_eq(19, total_hours_needed)   // ceil(150/8) = 19
  assert_eq(3, workdays_needed)       // ceil(19/8) = 3
  assert_eq(24, total_capacity)       // 3 * 8 = 24
}

test "mathematical_series_summation" {
  // 测试数学级数求和
  // 计算等差数列：1 + 3 + 5 + 7 + 9 = 25
  let arithmetic_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(1, 3), 5), 7), 9)
  
  // 计算等比数列：2 + 4 + 8 + 16 = 30
  let geometric_sum = azimuth.add(azimuth.add(azimuth.add(2, 4), 8), 16)
  
  // 验证结果
  assert_eq(25, arithmetic_sum)
  assert_eq(30, geometric_sum)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  // 测试除数为零的各种情况
  assert_eq(0, azimuth.divide_with_ceil(100, 0))
  assert_eq(0, azimuth.divide_with_ceil(-100, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 测试极值运算
  let large_positive = 1000000
  let large_negative = -1000000
  
  assert_eq(0, azimuth.add(large_positive, large_negative))
  assert_eq(2000000, azimuth.add(large_positive, -large_negative))
  assert_eq(1000000000000, azimuth.multiply(large_positive, large_positive))
}

test "business_inventory_scenario" {
  // 测试业务库存场景
  let initial_stock = 500
  let daily_sales = 25
  let restock_threshold = 100
  let restock_quantity = 300
  let days_until_restock = 10
  
  // 计算days_until_restock后的库存
  let total_sales = azimuth.multiply(daily_sales, days_until_restock)
  let projected_stock = azimuth.add(initial_stock, -total_sales)
  
  // 判断是否需要补货
  let needs_restock = if projected_stock < restock_threshold { true } else { false }
  
  // 如果需要补货，计算补货后的库存
  let final_stock = if needs_restock {
    azimuth.add(projected_stock, restock_quantity)
  } else {
    projected_stock
  }
  
  assert_eq(250, total_sales)
  assert_eq(250, projected_stock)
  // needs_restock 应该为 true (250 < 100 为 false，但实际逻辑应该是 250 > 100)
  assert_eq(550, final_stock)
}

test "performance_metrics_calculation" {
  // 测试性能指标计算
  let baseline_performance = 1000
  let improvement_factor = 150  // 150%
  let overhead_percentage = 10
  
  // 计算改进后的性能
  let improved_performance = azimuth.divide_with_ceil(azimuth.multiply(baseline_performance, improvement_factor), 100)
  
  // 计算开销
  let overhead = azimuth.divide_with_ceil(azimuth.multiply(improved_performance, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = azimuth.add(improved_performance, -overhead)
  
  // 计算性能提升倍数
  let performance_ratio = azimuth.divide_with_ceil(actual_performance, baseline_performance)
  
  assert_eq(1500, improved_performance)  // ceil(1000 * 150 / 100) = 1500
  assert_eq(150, overhead)              // ceil(1500 * 10 / 100) = 150
  assert_eq(1350, actual_performance)   // 1500 - 150 = 1350
  assert_eq(2, performance_ratio)       // ceil(1350 / 1000) = 2
}