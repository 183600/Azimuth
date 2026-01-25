// 核心增强测试用例 - 为 azimuth 库添加的10个高质量测试
// 使用标准 MoonBit 测试语法，全面覆盖核心功能

test "extreme_numerical_operations" {
  // 极端数值运算测试
  // 测试接近Int边界的运算
  let large_pos = 1000000
  let large_neg = -1000000
  let medium_val = 500
  
  // 测试大数加法
  assert_eq(2000000, add(large_pos, large_pos))
  assert_eq(0, add(large_pos, large_neg))
  assert_eq(-500000, add(large_neg, medium_val))
  
  // 测试大数乘法
  assert_eq(500000000, multiply(large_pos, medium_val))
  assert_eq(-500000000, multiply(large_neg, medium_val))
  
  // 测试大数除法向上取整
  assert_eq(2000, divide_with_ceil(large_pos, medium_val))
  assert_eq(-2000, divide_with_ceil(large_neg, medium_val))
}

test "mathematical_distributive_law" {
  // 数学分配律验证：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  // 计算左边：a * (b + c)
  let sum_bc = add(b, c)
  let left_side = multiply(a, sum_bc)
  
  // 计算右边：a * b + a * c
  let product_ab = multiply(a, b)
  let product_ac = multiply(a, c)
  let right_side = add(product_ab, product_ac)
  
  // 验证分配律
  assert_eq(left_side, right_side)
  
  // 额外验证：使用不同的数值
  let x = 15
  let y = 4
  let z = 6
  assert_eq(multiply(x, add(y, z)), add(multiply(x, y), multiply(x, z)))
}

test "complex_business_calculation" {
  // 复杂业务场景计算：电商订单总价计算
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8  // 8% 税率
  let shipping_per_item = 5
  let discount_threshold = 500  // 满500减30
  let discount_amount = 30
  
  // 计算商品小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算运费
  let total_shipping = multiply(shipping_per_item, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let pre_discount_total = add(add(subtotal, total_shipping), tax_amount)
  
  // 应用折扣（如果满足条件）
  let final_total = if pre_discount_total > discount_threshold {
    add(pre_discount_total, -discount_amount)
  } else {
    pre_discount_total
  }
  
  // 验证计算结果
  assert_eq(534, subtotal)
  assert_eq(30, total_shipping)
  assert_eq(43, tax_amount)
  assert_eq(607, pre_discount_total)
  assert_eq(577, final_total)
}

test "boundary_value_conditions" {
  // 边界条件处理测试
  // 测试零值运算
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(0, 12345))
  assert_eq(0, divide_with_ceil(0, 12345))
  assert_eq(0, divide_with_ceil(12345, 0))
  
  // 测试单位元运算
  assert_eq(12345, multiply(12345, 1))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(12346, add(12345, 1))
  
  // 测试相等数除法
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(1, divide_with_ceil(-50, -50))
  
  // 测试被除数小于除数
  assert_eq(1, divide_with_ceil(5, 10))
  assert_eq(1, divide_with_ceil(99, 100))
  assert_eq(1, divide_with_ceil(-5, -10))
}

test "comprehensive_negative_operations" {
  // 负数运算综合测试
  let neg_a = -15
  let neg_b = -8
  let pos_c = 12
  
  // 负数加法测试
  assert_eq(-23, add(neg_a, neg_b))
  assert_eq(-3, add(neg_a, pos_c))
  assert_eq(4, add(pos_c, neg_b))
  
  // 负数乘法测试
  assert_eq(120, multiply(neg_a, neg_b))
  assert_eq(-180, multiply(neg_a, pos_c))
  assert_eq(-96, multiply(pos_c, neg_b))
  
  // 负数除法向上取整测试
  assert_eq(2, divide_with_ceil(neg_a, neg_b))
  assert_eq(-1, divide_with_ceil(neg_a, pos_c))
  assert_eq(-1, divide_with_ceil(pos_c, neg_b))
  
  // 混合运算测试
  let mixed_result = add(multiply(neg_a, neg_b), divide_with_ceil(pos_c, neg_a))
  assert_eq(119, mixed_result)
}

test "enhanced_string_processing" {
  // 字符串处理增强测试
  // 测试各种特殊字符和组合
  assert_eq_string("Hello, 数字123!", greet("数字123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, C++编程!", greet("C++编程"))
  assert_eq_string("Hello, 数学公式α+β=γ!", greet("数学公式α+β=γ"))
  
  // 测试空格和特殊空白字符
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 	\n!", greet("\t\n"))
  
  // 测试长字符串和组合
  let long_name = "这是一个非常长的用户名称用来测试greet函数的处理能力"
  let expected = "Hello, " + long_name + "!"
  assert_eq_string(expected, greet(long_name))
}

test "real_world_application_scenario" {
  // 实际应用场景测试：库存管理系统
  let initial_stock = 500
  let daily_sales = 25
  let days_in_month = 30
  let restock_threshold = 200
  let restock_amount = 300
  
  // 计算月销售量
  let monthly_sales = multiply(daily_sales, days_in_month)
  
  // 计算月末库存
  let final_stock = add(initial_stock, -monthly_sales)
  
  // 判断是否需要补货
  let needs_restock = if final_stock < restock_threshold { 
    add(final_stock, restock_amount) 
  } else { 
    final_stock 
  }
  
  // 计算需要的货架空间（每箱50件）
  let shelves_needed = divide_with_ceil(needs_restock, 50)
  
  assert_eq(750, monthly_sales)
  assert_eq(-250, final_stock)
  assert_eq(50, needs_restock)
  assert_eq(1, shelves_needed)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除零错误处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极端数值组合
  let extreme_sum = add(2147483647, -2147483647)
  assert_eq(0, extreme_sum)
  
  // 测试连续运算的稳定性
  let chain_calculation = add(
    multiply(100, divide_with_ceil(50, 3)),
    multiply(50, divide_with_ceil(30, 7))
  )
  assert_eq(1750, chain_calculation)
  
  // 测试负数除零
  assert_eq(0, divide_with_ceil(-999, 0))
}

test "performance_calculation_simulation" {
  // 性能计算模拟测试
  let base_operations = 1000
  let optimization_factor = 4
  let overhead_percentage = 20
  let cache_hit_rate = 75
  
  // 计算优化后的操作数
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  // 计算系统开销
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算缓存优化效果
  let cache_bonus = divide_with_ceil(multiply(optimized_operations, cache_hit_rate), 100)
  
  // 计算实际性能
  let actual_performance = add(add(optimized_operations, cache_bonus), -overhead)
  
  // 计算性能提升倍数
  let performance_gain = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(4000, optimized_operations)
  assert_eq(800, overhead)
  assert_eq(3000, cache_bonus)
  assert_eq(6200, actual_performance)
  assert_eq(7, performance_gain)
}

test "comprehensive_functionality_test" {
  // 综合功能测试：多功能组合场景
  // 场景：计算一个项目的总成本和时间
  
  let hourly_rate = 85
  let hours_per_day = 8
  let project_days = 12
  let material_cost = 2500
  let contingency_percentage = 15
  
  // 计算人工成本
  let daily_labor_cost = multiply(hourly_rate, hours_per_day)
  let total_labor_cost = multiply(daily_labor_cost, project_days)
  
  // 计算应急费用
  let contingency_fund = divide_with_ceil(
    multiply(add(total_labor_cost, material_cost), contingency_percentage), 
    100
  )
  
  // 计算项目总成本
  let total_project_cost = add(add(total_labor_cost, material_cost), contingency_fund)
  
  // 计算项目总工时
  let total_hours = multiply(hours_per_day, project_days)
  
  // 计算需要的团队人数（假设要在6天内完成）
  let required_team_size = divide_with_ceil(total_hours, multiply(6, hours_per_day))
  
  // 生成项目报告
  let project_summary = greet("项目总成本：" + total_project_cost.to_string() + "元")
  
  assert_eq(680, daily_labor_cost)
  assert_eq(8160, total_labor_cost)
  assert_eq(1599, contingency_fund)
  assert_eq(12259, total_project_cost)
  assert_eq(96, total_hours)
  assert_eq(2, required_team_size)
  assert_eq_string("Hello, 项目总成本：12259元!", project_summary)
}