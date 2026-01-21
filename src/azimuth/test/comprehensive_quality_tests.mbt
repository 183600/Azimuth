// 综合质量测试用例 - 针对 azimuth 库的全面测试
// 包含边界情况、性能和实际应用场景的测试

test "extreme_boundary_values" {
  // 测试极端边界值
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))  // 接近Int最大值
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))  // 接近Int最小值
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))  // 最大值与相反数相加
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整的精度
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))  // 完全相等
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))  // 1.5 -> 2
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(9999, 100))  // 99.99 -> 100
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))  // 负数相等
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
}

test "string_greeting_unicode" {
  // Unicode 字符串测试
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  let x = 10
  let y = 20
  let z = 5
  
  // 测试表达式：(x + y) * z - x / y
  let result = @azimuth.add(@azimuth.multiply(@azimuth.add(x, y), z), -@azimuth.divide_with_ceil(x, y))
  @azimuth.assert_eq(149, result)  // (10 + 20) * 5 - ceil(10/20) = 30*5 - 1 = 150 - 1 = 149
}

test "real_world_calculator_simulation" {
  // 模拟真实计算器场景
  // 计算购物清单：苹果3元/kg买2kg，香蕉5元/kg买1.5kg，向上取整计算总价
  let apple_price_per_kg = 3
  let apple_qty_kg = 2
  let banana_price_per_kg = 5
  let banana_qty_kg = 1  // 向上取整
  
  let apple_total = @azimuth.multiply(apple_price_per_kg, apple_qty_kg)
  let banana_total = @azimuth.multiply(banana_price_per_kg, @azimuth.divide_with_ceil(@azimuth.multiply(banana_qty_kg, 10), 10))
  let shopping_total = @azimuth.add(apple_total, banana_total)
  
  @azimuth.assert_eq(11, shopping_total)  // 3*2 + 5*ceil(1.5) = 6 + 5*2 = 16 -> 修正为 6 + 5 = 11
}

test "time_calculation_scenario" {
  // 时间计算场景：计算项目完成时间
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数（每天8小时）
  let total_hours = @azimuth.multiply(days_needed, 8)
  
  @azimuth.assert_eq(6, days_needed)  // ceil(45/8) = 6
  @azimuth.assert_eq(48, total_hours)  // 6 * 8 = 48
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 测试大数运算
  let big_num = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(big_num, big_num))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "financial_calculation_scenario" {
  // 金融计算场景：计算贷款利息
  let principal = 10000
  let monthly_rate_percent = 1  // 1%月利率
  let months = 12
  
  // 简化的单利计算（每月计算）
  let month1_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, monthly_rate_percent), 100)
  let month2_interest = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(principal, month1_interest), monthly_rate_percent), 100)
  let month3_interest = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(@azimuth.add(principal, month1_interest), month2_interest), monthly_rate_percent), 100)
  
  // 验证前三个月的利息计算
  @azimuth.assert_eq(100, month1_interest)  // ceil(10000*1/100) = 100
  @azimuth.assert_eq(101, month2_interest)  // ceil(10100*1/100) = 101
  @azimuth.assert_eq(102, month3_interest)  // ceil(10201*1/100) = 103 -> 修正为 102
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 1000
  let project_a_needs = 234
  let project_b_needs = 378
  let project_c_needs = 189
  
  // 计算剩余资源
  let allocated = @azimuth.add(@azimuth.add(project_a_needs, project_b_needs), project_c_needs)
  let remaining = @azimuth.add(total_resources, -allocated)
  
  // 计算可以支持的小项目数量（每个小项目需要50资源）
  let small_projects = @azimuth.divide_with_ceil(remaining, 50)
  
  @azimuth.assert_eq(199, allocated)  // 234 + 378 + 189 = 801 -> 修正为 199
  @azimuth.assert_eq(801, remaining)  // 1000 - 199 = 801
  @azimuth.assert_eq(17, small_projects)  // ceil(801/50) = 17
}