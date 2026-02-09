// 用户添加的 MoonBit 测试用例
// 包含10个标准测试用例，覆盖核心功能和边界情况

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(3, @azimuth.subtract(10, 7))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
}

test "negative_number_operations" {
  // 负数运算测试
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-5, 6))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
  @azimuth.assert_eq(-8, @azimuth.subtract(5, 13))
}

test "zero_handling_operations" {
  // 零值处理测试
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.subtract(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "greet_function_edge_cases" {
  // 问候函数边界情况测试
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 特殊字符@#$!", @azimuth.greet("特殊字符@#$"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 17, y = 23, z = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 模拟购物车计算
  let item1_price = 25, item1_qty = 3
  let item2_price = 15, item2_qty = 2
  let shipping = 10
  
  let subtotal1 = @azimuth.multiply(item1_price, item1_qty)
  let subtotal2 = @azimuth.multiply(item2_price, item2_qty)
  let items_total = @azimuth.add(subtotal1, subtotal2)
  let grand_total = @azimuth.add(items_total, shipping)
  
  @azimuth.assert_eq(75, subtotal1)
  @azimuth.assert_eq(30, subtotal2)
  @azimuth.assert_eq(105, items_total)
  @azimuth.assert_eq(115, grand_total)
}

test "resource_allocation_calculations" {
  // 资源分配计算测试
  let total_budget = 1000
  let project1_cost = 350
  let project2_cost = 275
  let project3_cost = 180
  
  let allocated = @azimuth.add(@azimuth.add(project1_cost, project2_cost), project3_cost)
  let remaining = @azimuth.subtract(total_budget, allocated)
  let emergency_reserve = @azimuth.divide_with_ceil(remaining, 2)
  
  @azimuth.assert_eq(805, allocated)
  @azimuth.assert_eq(195, remaining)
  @azimuth.assert_eq(98, emergency_reserve)
}

test "time_based_calculations" {
  // 基于时间的计算测试
  let hours_per_day = 8
  let days_per_week = 5
  let weeks_per_month = 4
  
  let monthly_hours = @azimuth.multiply(hours_per_day, @azimuth.multiply(days_per_week, weeks_per_month))
  let overtime_rate = 2
  let overtime_hours = 10
  let overtime_pay = @azimuth.multiply(hours_per_day, overtime_rate)
  
  @azimuth.assert_eq(160, monthly_hours)
  @azimuth.assert_eq(16, overtime_pay)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  let max_value = 100
  let min_value = 1
  
  // 测试边界附近的值
  @azimuth.assert_eq(101, @azimuth.add(max_value, 1))
  @azimuth.assert_eq(99, @azimuth.subtract(max_value, 1))
  @azimuth.assert_eq(0, @azimuth.subtract(min_value, 1))
  @azimuth.assert_eq(2, @azimuth.add(min_value, 1))
  
  // 测试边界除法
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(max_value, 1))
  @azimuth.assert_eq(50, @azimuth.divide_with_ceil(max_value, 2))
  @azimuth.assert_eq(34, @azimuth.divide_with_ceil(max_value, 3))
}

test "practical_business_metrics" {
  // 实际业务指标测试
  let revenue = 50000
  let costs = 35000
  let employees = 8
  
  // 计算利润
  let profit = @azimuth.subtract(revenue, costs)
  
  // 计算利润率
  let profit_rate = @azimuth.divide_with_ceil(@azimuth.multiply(profit, 100), revenue)
  
  // 计算人均产值
  let revenue_per_employee = @azimuth.divide_with_ceil(revenue, employees)
  
  // 计算人均利润
  let profit_per_employee = @azimuth.divide_with_ceil(profit, employees)
  
  @azimuth.assert_eq(15000, profit)
  @azimuth.assert_eq(30, profit_rate)
  @azimuth.assert_eq(6250, revenue_per_employee)
  @azimuth.assert_eq(1875, profit_per_employee)
}