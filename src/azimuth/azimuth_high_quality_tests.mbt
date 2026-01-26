// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行深度测试

test "mathematical_identity_properties" {
  // 测试数学单位元性质
  // 加法单位元：a + 0 = a
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法单位元：a * 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(0, 1))
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数情况
  assert_eq(4, divide_with_ceil(10, 3))    // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))   // 0.01 -> 1
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))  // -3.333... -> -3
  assert_eq(3, divide_with_ceil(10, -3))   // -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))  // 3.333... -> 4
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))     // 0 -> 0
  assert_eq(0, divide_with_ceil(5, 0))     // 除零保护
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 89
  let quantity = 4
  let tax_rate = 8    // 8%
  let shipping = 12
  let discount = 20
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), add(shipping, -discount))
  
  assert_eq(356, subtotal)    // 89 * 4
  assert_eq(29, tax)          // ceil(356 * 8 / 100)
  assert_eq(377, total)       // 356 + 29 + 12 - 20
}

test "string_greeting_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, María!", greet("María"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, !", greet(""))
}

test "algorithm_efficiency_simulation" {
  // 模拟算法效率计算
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  // 优化后的操作数
  let optimized = multiply(base_operations, optimization_factor)
  // 开销
  let overhead = divide_with_ceil(multiply(optimized, overhead_percentage), 100)
  // 实际性能
  let actual = add(optimized, -overhead)
  
  assert_eq(3000, optimized)
  assert_eq(450, overhead)
  assert_eq(2550, actual)
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let reserve_percentage = 10
  
  // 已分配预算
  let allocated = add(project_a_cost, project_b_cost)
  // 储备金
  let reserve = divide_with_ceil(multiply(total_budget, reserve_percentage), 100)
  // 可用剩余预算
  let available = add(total_budget, -add(allocated, reserve))
  
  assert_eq(6300, allocated)
  assert_eq(1000, reserve)
  assert_eq(2700, available)
}

test "time_series_calculation" {
  // 时间序列计算
  let initial_value = 1000
  let growth_rates = [5, 8, 12, 6]  // 百分比增长
  
  // 第一年
  let year1 = add(initial_value, divide_with_ceil(multiply(initial_value, growth_rates[0]), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, growth_rates[1]), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, growth_rates[2]), 100))
  // 第四年
  let year4 = add(year3, divide_with_ceil(multiply(year3, growth_rates[3]), 100))
  
  assert_eq(1050, year1)
  assert_eq(1134, year2)
  assert_eq(1271, year3)
  assert_eq(1348, year4)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算
  let initial_inventory = 500
  let purchases = 1200
  let final_inventory = 300
  let cost_of_goods_sold = add(initial_inventory, purchases) - final_inventory
  let average_inventory = divide_with_ceil(add(initial_inventory, final_inventory), 2)
  let turnover_rate = divide_with_ceil(multiply(cost_of_goods_sold, 100), average_inventory)
  
  assert_eq(1400, cost_of_goods_sold)
  assert_eq(400, average_inventory)
  assert_eq(350, turnover_rate)
}

test "data_structure_performance_test" {
  // 数据结构性能测试
  let base_access_time = 10  // 微秒
  let data_size = 1000
  let algorithm_complexity_factor = 2  // O(log n) 近似
  
  // 预期访问时间
  let expected_time = multiply(base_access_time, divide_with_ceil(data_size, algorithm_complexity_factor))
  
  // 性能提升后的时间
  let optimization_factor = 3
  let optimized_time = divide_with_ceil(expected_time, optimization_factor)
  
  assert_eq(5000, expected_time)
  assert_eq(1667, optimized_time)
}

test "error_boundary_robustness" {
  // 错误边界健壮性测试
  // 测试极端值
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 测试大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  
  // 测试边界字符串
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
}