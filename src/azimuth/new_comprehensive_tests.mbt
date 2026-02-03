// Azimuth 项目新增综合测试用例
// 使用标准的 MoonBit 测试语法，针对核心功能进行全面测试

test "azimuth_comprehensive_addition_edge_cases" {
  // 加法功能的综合边界情况测试
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
  assert_eq(1000000, add(999999, 1))  // 大数相加
  assert_eq(-1000000, add(-999999, -1))  // 负大数相加
}

test "azimuth_comprehensive_multiplication_properties" {
  // 乘法功能的数学性质测试
  let x = 12
  let y = 15
  
  // 交换律验证
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, y))
  
  // 单位元性质
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(1, y))
  
  // 负数乘法
  assert_eq(-180, multiply(x, -y))
  assert_eq(180, multiply(-x, -y))
}

test "azimuth_comprehensive_divide_with_ceil_precision" {
  // 向上取整除法的精度和边界测试
  // 正数除法精度测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3))  // 3.666... -> 4
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  
  // 负数除法测试
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))  // -3.666... -> -4
  assert_eq(4, divide_with_ceil(-11, -3))  // 3.666... -> 4
  
  // 特殊情况
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "azimuth_comprehensive_greet_unicode_handling" {
  // 问候函数的Unicode和特殊字符处理测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, user@example.com!", greet("user@example.com"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "azimuth_comprehensive_complex_expressions" {
  // 复杂数学表达式测试
  let a = 10
  let b = 20
  let c = 5
  
  // 测试复杂表达式：(a + b) * c - a / b
  let result1 = add(multiply(add(a, b), c), -divide_with_ceil(a, b))
  assert_eq(149, result1)  // (10 + 20) * 5 - ceil(10/20) = 30*5 - 1 = 150 - 1 = 149
  
  // 测试嵌套表达式：ceil((a + b*c) * 2 / 3)
  let result2 = divide_with_ceil(multiply(add(a, multiply(b, c)), 2), 3)
  assert_eq(67, result2)  // ceil((10 + 20*5) * 2 / 3) = ceil(110 * 2 / 3) = ceil(220/3) = 74
  
  // 测试分配律验证
  let distributive_left = multiply(a, add(b, c))
  let distributive_right = add(multiply(a, b), multiply(a, c))
  assert_eq(distributive_left, distributive_right)  // 10*(20+5) = 10*20 + 10*5 = 250
}

test "azimuth_comprehensive_business_calculations" {
  // 综合业务计算场景测试
  // 场景1：电商订单计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let shipping_cost = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(add(subtotal, tax_amount), shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(397, total_before_discount)
  assert_eq(377, final_total)
  
  // 场景2：库存管理计算
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 100
  
  let total_sold = multiply(daily_sales, days)
  let current_stock = subtract(initial_stock, total_sold)
  let final_stock = add(current_stock, new_shipment)
  
  assert_eq(175, total_sold)
  assert_eq(325, current_stock)
  assert_eq(425, final_stock)
}

test "azimuth_comprehensive_performance_metrics" {
  // 性能指标计算测试
  // 场景1：算法性能基准
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  let optimized_operations = multiply(base_operations, optimization_factor)
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  let actual_performance = add(optimized_operations, -overhead)
  let performance_improvement = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(3000, optimized_operations)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
  assert_eq(3, performance_improvement)
  
  // 场景2：数据处理效率
  let data_size_mb = 850
  let processing_speed_mbps = 50
  let parallel_factor = 4
  
  let sequential_time = divide_with_ceil(data_size_mb, processing_speed_mbps)
  let parallel_time = divide_with_ceil(sequential_time, parallel_factor)
  let efficiency_gain = subtract(sequential_time, parallel_time)
  
  assert_eq(17, sequential_time)
  assert_eq(5, parallel_time)
  assert_eq(12, efficiency_gain)
}

test "azimuth_comprehensive_error_handling_robustness" {
  // 错误处理和健壮性测试
  // 测试除零情况的健壮性
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  let max_int = 2147483647
  let min_int = -2147483648
  
  let near_max_result = add(max_int - 1000, 500)
  let near_min_result = add(min_int + 1000, -500)
  
  assert_eq(2147483147, near_max_result)
  assert_eq(-2147483148, near_min_result)
  
  // 测试字符串边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, A!", greet("A"))
  
  // 测试计算结果稳定性
  let stable_result = add(multiply(10, 10), subtract(20, 5))
  assert_eq(115, stable_result)  // 10*10 + (20-5) = 100 + 15 = 115
}

test "azimuth_comprehensive_time_and_resource_calculations" {
  // 时间和资源计算综合测试
  // 场景1：项目时间规划
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  let buffer_percentage = 20
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  let buffer_days = divide_with_ceil(multiply(days_needed, buffer_percentage), 100)
  let total_days_with_buffer = add(days_needed, buffer_days)
  
  assert_eq(11, days_needed)
  assert_eq(3, weeks_needed)
  assert_eq(3, buffer_days)
  assert_eq(14, total_days_with_buffer)
  
  // 场景2：资源分配优化
  let total_budget = 10000
  let team_size = 6
  let project_duration_weeks = 8
  let contingency_percentage = 10
  
  let budget_per_person = divide_with_ceil(total_budget, team_size)
  let weekly_budget_per_person = divide_with_ceil(budget_per_person, project_duration_weeks)
  let contingency_fund = divide_with_ceil(multiply(total_budget, contingency_percentage), 100)
  let effective_budget = add(total_budget, -contingency_fund)
  
  assert_eq(1667, budget_per_person)
  assert_eq(209, weekly_budget_per_person)
  assert_eq(1000, contingency_fund)
  assert_eq(9000, effective_budget)
}