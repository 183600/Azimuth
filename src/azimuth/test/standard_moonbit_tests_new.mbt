// 新增的标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能和实际应用场景

test "basic_arithmetic_commutativity" {
  // 测试基本算术运算的交换律
  let a = 15
  let b = 27
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  // 测试小数部分不同的情况
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 3))  // 1.33 -> 2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))  // 2.66 -> 3
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(9, 2))  // 4.5 -> 5
  
  // 测试边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))  // 1.0 -> 1
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 0.01 -> 1
}

test "greeting_function_internationalization" {
  // 测试问候函数的国际化支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, München!", @azimuth.greet("München"))
  @azimuth.assert_eq_string("Hello, São Paulo!", @azimuth.greet("São Paulo"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let product_price = 125
  let quantity = 7
  let tax_rate = 8  // 8%
  let shipping_cost = 25
  let discount = 50
  
  // 计算商品小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 计算税费（向上取整）
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_cost), -discount)
  
  @azimuth.assert_eq(875, subtotal)
  @azimuth.assert_eq(70, tax_amount)
  @azimuth.assert_eq(920, final_total)
}

test "mathematical_sequence_fibonacci" {
  // 测试数学序列：斐波那契数列前几项
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)      // 2
  let fib4 = @azimuth.add(fib2, fib3)      // 3
  let fib5 = @azimuth.add(fib3, fib4)      // 5
  let fib6 = @azimuth.add(fib4, fib5)      // 8
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
}

test "resource_allocation_scenario" {
  // 测试资源分配场景
  let total_budget = 5000
  let project_costs = @azimuth.multiply(350, 8)  // 8个项目，每个350
  let remaining_budget = @azimuth.add(total_budget, -project_costs)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 200
  let small_projects = @azimuth.divide_with_ceil(remaining_budget, small_project_cost)
  
  @azimuth.assert_eq(2800, project_costs)
  @azimuth.assert_eq(2200, remaining_budget)
  @azimuth.assert_eq(11, small_projects)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let tasks_per_day = 6
  let total_tasks = 47
  let work_days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(work_days_needed, 8)
  
  @azimuth.assert_eq(8, work_days_needed)
  @azimuth.assert_eq(64, total_hours)
}

test "negative_number_complex_operations" {
  // 测试负数的复杂运算
  let base_value = -20
  let increment = 15
  let multiplier = -3
  
  // 复合运算
  let result1 = @azimuth.add(base_value, increment)  // -20 + 15 = -5
  let result2 = @azimuth.multiply(result1, multiplier)  // -5 * -3 = 15
  let result3 = @azimuth.divide_with_ceil(result2, 4)  // ceil(15/4) = 4
  
  @azimuth.assert_eq(-5, result1)
  @azimuth.assert_eq(15, result2)
  @azimuth.assert_eq(4, result3)
}

test "data_analysis_statistics" {
  // 测试数据分析：基本统计计算
  let data_points = [23, 45, 67, 89, 12, 34, 56, 78, 90, 41]
  
  // 计算总和（手动计算前5个数据点）
  let sum_first_half = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(23, 45), 67), 89), 12), 34)
  
  // 计算平均值（向上取整）
  let average_first_half = @azimuth.divide_with_ceil(sum_first_half, 5)
  
  @azimuth.assert_eq(270, sum_first_half)
  @azimuth.assert_eq(54, average_first_half)
}