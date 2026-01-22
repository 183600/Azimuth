// 新增的高质量 MoonBit 测试用例
// 包含数学性质、实际应用场景和边缘情况的测试

test "mathematical_associative_laws" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 15
  let b = 27
  let c = 42
  
  let left_assoc = @azimuth.add(@azimuth.add(a, b), c)
  let right_assoc = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_assoc, right_assoc)
  
  // 测试乘法结合律: (a * b) * c = a * (b * c)
  let m = 3
  let n = 7
  let p = 11
  
  let left_mult = @azimuth.multiply(@azimuth.multiply(m, n), p)
  let right_mult = @azimuth.multiply(m, @azimuth.multiply(n, p))
  @azimuth.assert_eq(left_mult, right_mult)
}

test "mathematical_distributive_law" {
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 12
  let c = 8
  
  let left_dist = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_dist = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_dist, right_dist)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：计算投资回报
  let principal = 10000  // 本金
  let rate1 = 5          // 第一年收益率5%
  let rate2 = 8          // 第二年收益率8%
  let rate3 = 6          // 第三年收益率6%
  
  // 计算三年后的总金额（简化计算，使用乘法模拟百分比）
  let year1 = @azimuth.multiply(principal, rate1)
  let year2 = @azimuth.multiply(year1, rate2)
  let year3 = @azimuth.multiply(year2, rate3)
  
  // 计算总收益（简化版）
  let total_return = @azimuth.add(year1, @azimuth.add(year2, year3))
  @azimuth.assert_true(total_return > 0)
}

test "inventory_management_scenario" {
  // 测试库存管理场景
  let initial_stock = 500
  let shipment1 = 150
  let shipment2 = 200
  let sold_items = 320
  
  // 计算最终库存
  let total_received = @azimuth.add(shipment1, shipment2)
  let current_stock = @azimuth.add(initial_stock, total_received)
  let final_stock = @azimuth.add(current_stock, -sold_items)
  
  @azimuth.assert_eq(530, final_stock)
  
  // 计算需要的箱子数量（每箱装25个）
  let boxes_needed = @azimuth.divide_with_ceil(final_stock, 25)
  @azimuth.assert_eq(22, boxes_needed)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景（简化版）
  let celsius_temp = 25
  let conversion_factor = 9
  let conversion_divisor = 5
  let offset = 32
  
  // 简化的华氏度计算：F = C * 9/5 + 32
  let multiplied = @azimuth.multiply(celsius_temp, conversion_factor)
  let divided = @azimuth.divide_with_ceil(multiplied, conversion_divisor)
  let fahrenheit_temp = @azimuth.add(divided, offset)
  
  @azimuth.assert_eq(77, fahrenheit_temp)
}

test "error_handling_boundary_conditions" {
  // 测试错误处理的边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "string_processing_complex_cases" {
  // 测试复杂的字符串处理情况
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let unicode_chars = "αβγδεζηθικλμνξοπρστυφχψω"
  
  let result1 = @azimuth.greet(special_chars)
  let result2 = @azimuth.greet(unicode_chars)
  
  // 验证字符串不为空，通过简单的断言检查
  @azimuth.assert_true(result1 != "")
  @azimuth.assert_true(result2 != "")
}

test "algorithmic_sequence_calculation" {
  // 测试算法序列计算
  // 计算斐波那契数列的前几项（简化版）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  let fib_sum = @azimuth.add(fib1, @azimuth.add(fib2, @azimuth.add(fib3, @azimuth.add(fib4, @azimuth.add(fib5, fib6)))))
  @azimuth.assert_eq(20, fib_sum)
}

test "resource_allocation_scenario" {
  // 测试资源分配场景
  let total_budget = 10000
  let team_size = 8
  let project_duration = 12  // 月
  
  // 计算每人每月预算
  let total_person_months = @azimuth.multiply(team_size, project_duration)
  let budget_per_person_month = @azimuth.divide_with_ceil(total_budget, total_person_months)
  
  // 计算总分配（可能由于向上取整而超过原预算）
  let allocated_budget = @azimuth.multiply(budget_per_person_month, total_person_months)
  
  @azimuth.assert_true(budget_per_person_month > 0)
  @azimuth.assert_true(allocated_budget >= total_budget)
}

test "geometric_calculation_scenario" {
  // 测试几何计算场景
  // 计算矩形的周长和面积（简化版）
  let length = 15
  let width = 8
  
  // 周长 = 2 * (length + width)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  // 面积 = length * width
  let area = @azimuth.multiply(length, width)
  
  @azimuth.assert_eq(46, perimeter)
  @azimuth.assert_eq(120, area)
  
  // 计算需要多少个1x1的瓷砖来覆盖面积
  let tiles_needed = @azimuth.divide_with_ceil(area, 1)
  @azimuth.assert_eq(120, tiles_needed)
}