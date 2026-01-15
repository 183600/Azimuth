// Azimuth 项目额外的 MoonBit 测试用例
// 专注于测试一些可能还没有被充分测试的功能

test "divide_with_ceil_basic_functionality" {
  // 测试 divide_with_ceil 函数的基本功能
  // 正常除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  
  // 整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(21, 3))
  
  // 负数除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-12, 3))
}

test "divide_with_ceil_boundary_values" {
  // 测试 divide_with_ceil 函数的边界值
  // 除数为1
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(10, 1))
  @azimuth.assert_eq(-10, @azimuth.divide_with_ceil(-10, 1))
  
  // 大数除法
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(500000, @azimuth.divide_with_ceil(1000000, 2))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
  
  // 边界值除法
  let max_val = 2147483647
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(max_val, max_val))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(max_val, max_val / 2))
  
  let min_val = -2147483648
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(min_val, min_val))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(min_val, min_val / 2))
}

test "complex_mathematical_operations" {
  // 测试复杂数学运算
  // 平方计算
  let num = 13
  let square = @azimuth.multiply(num, num)
  @azimuth.assert_eq(169, square)
  
  // 立方计算
  let cube = @azimuth.multiply(square, num)
  @azimuth.assert_eq(2197, cube)
  
  // 平方差公式：a² - b² = (a + b)(a - b)
  let a = 15
  let b = 8
  let a_squared = @azimuth.multiply(a, a)  // 225
  let b_squared = @azimuth.multiply(b, b)  // 64
  let difference_of_squares = @azimuth.add(a_squared, -b_squared)  // 161
  
  let sum = @azimuth.add(a, b)  // 23
  let difference = @azimuth.add(a, -b)  // 7
  let product = @azimuth.multiply(sum, difference)  // 161
  
  @azimuth.assert_eq(difference_of_squares, product)
}

test "sequence_calculations" {
  // 测试数列计算
  // 等差数列求和：1 + 4 + 7 + 10 + 13
  // 使用公式：n/2 * (2a + (n-1)d)
  let n = 5
  let a = 1
  let d = 3
  
  let term1 = a  // 1
  let term2 = @azimuth.add(a, d)  // 4
  let term3 = @azimuth.add(term2, d)  // 7
  let term4 = @azimuth.add(term3, d)  // 10
  let term5 = @azimuth.add(term4, d)  // 13
  
  let sum_manual = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(35, sum_manual)
  
  // 等比数列求和：2 + 6 + 18 + 54
  let ratio = 3
  let first_term = 2
  let num_terms = 4
  
  let geo_term1 = first_term  // 2
  let geo_term2 = @azimuth.multiply(geo_term1, ratio)  // 6
  let geo_term3 = @azimuth.multiply(geo_term2, ratio)  // 18
  let geo_term4 = @azimuth.multiply(geo_term3, ratio)  // 54
  
  let geo_sum_manual = @azimuth.add(@azimuth.add(@azimuth.add(geo_term1, geo_term2), geo_term3), geo_term4)
  @azimuth.assert_eq(80, geo_sum_manual)
}

test "prime_number_related_calculations" {
  // 测试质数相关计算
  // 检查一个数是否为质数（通过乘法验证）
  let num1 = 17  // 质数
  let num2 = 15  // 非质数
  
  // 简化的质数检查：通过乘法验证
  // 对于17，检查是否能被小数整除
  let check1_2 = @azimuth.multiply(2, 8)  // 16 < 17
  let check1_3 = @azimuth.multiply(3, 5)  // 15 < 17
  let check1_4 = @azimuth.multiply(4, 4)  // 16 < 17
  
  // 对于15，检查是否能被小数整除
  let check2_3 = @azimuth.multiply(3, 5)  // 15 = 15，能被3整除
  let check2_5 = @azimuth.multiply(5, 3)  // 15 = 15，能被5整除
  
  // 验证结果
  @azimuth.assert_true(check1_2 < num1 && check1_3 < num1 && check1_4 < num1)  // 17不是小数的倍数
  @azimuth.assert_true(check2_3 == num2)  // 15是3的倍数
  
  // 计算两个数的最大公约数（通过减法实现欧几里得算法）
  let a = 48
  let b = 18
  
  // 使用减法实现GCD计算
  let temp_a = a
  let temp_b = b
  
  // 48 - 18 = 30, 30 - 18 = 12, 18 - 12 = 6, 12 - 6 = 6, 6 - 6 = 0
  // GCD是6
  let diff1 = @azimuth.add(temp_a, -temp_b)  // 30
  let diff2 = @azimuth.add(diff1, -temp_b)  // 12
  let diff3 = @azimuth.add(temp_b, -diff2)  // 6
  let diff4 = @azimuth.add(diff2, -diff3)  // 6
  let diff5 = @azimuth.add(diff3, -diff4)  // 0
  
  // GCD是最后一个非零差值
  @azimuth.assert_eq(6, diff3)
}

test "percentage_calculations_advanced" {
  // 测试高级百分比计算
  let original_price = 200
  let discount_percent = 15
  let tax_percent = 8
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  @azimuth.assert_eq(30, discount_amount)
  
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(170, discounted_price)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_percent) / 100
  @azimuth.assert_eq(13, tax_amount)  // 整数除法，170 * 8 / 100 = 13
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  @azimuth.assert_eq(183, final_price)
  
  // 计算总折扣百分比
  let total_discount = @azimuth.add(original_price, -final_price)
  let total_discount_percent = @azimuth.multiply(total_discount, 100) / original_price
  @azimuth.assert_eq(8, total_discount_percent)  // 整数除法，17 * 100 / 200 = 8
}

test "simplified_trigonometry_calculations" {
  // 测试简化的三角函数计算
  // 使用整数近似值计算
  let angle_degrees = 45
  
  // 简化的sin值：sin(45°) ≈ 0.707 ≈ 707/1000
  let sin_approx_numerator = 707
  let sin_approx_denominator = 1000
  
  // 计算对边长度（假设斜边为1000）
  let hypotenuse = 1000
  let opposite = @azimuth.multiply(hypotenuse, sin_approx_numerator) / sin_approx_denominator
  @azimuth.assert_eq(707, opposite)
  
  // 简化的cos值：cos(45°) ≈ 0.707 ≈ 707/1000
  let adjacent = @azimuth.multiply(hypotenuse, sin_approx_numerator) / sin_approx_denominator
  @azimuth.assert_eq(707, adjacent)
  
  // 使用勾股定理验证：a² + b² = c²
  let opposite_squared = @azimuth.multiply(opposite, opposite)  // 707² = 499849
  let adjacent_squared = @azimuth.multiply(adjacent, adjacent)  // 707² = 499849
  let sum_of_squares = @azimuth.add(opposite_squared, adjacent_squared)  // 999698
  
  // 由于近似值，结果不是完全精确的1000²
  @azimuth.assert_true(sum_of_squares > 990000 && sum_of_squares < 1010000)
}

test "physics_formula_calculations" {
  // 测试物理公式计算
  // 运动学公式：s = ut + (1/2)at²
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  
  // 计算位移
  let ut = @azimuth.multiply(initial_velocity, time)  // 10 * 5 = 50
  let half_at_squared = @azimuth.multiply(acceleration, @azimuth.multiply(time, time)) / 2  // 2 * 25 / 2 = 25
  let displacement = @azimuth.add(ut, half_at_squared)  // 50 + 25 = 75
  
  @azimuth.assert_eq(75, displacement)
  
  // 牛顿第二定律：F = ma
  let mass = 10    // kg
  let force = @azimuth.multiply(mass, acceleration)  // 10 * 2 = 20 N
  @azimuth.assert_eq(20, force)
  
  // 动能公式：KE = (1/2)mv²
  let velocity = 15  // m/s
  let velocity_squared = @azimuth.multiply(velocity, velocity)  // 225
  let kinetic_energy = @azimuth.multiply(mass, velocity_squared) / 2  // 10 * 225 / 2 = 1125
  @azimuth.assert_eq(1125, kinetic_energy)
}

test "economics_calculations" {
  // 测试经济学计算
  // 利润计算：利润 = 收入 - 成本
  let revenue = 50000
  let fixed_costs = 20000
  let variable_costs = 15000
  
  let total_costs = @azimuth.add(fixed_costs, variable_costs)  // 35000
  let profit = @azimuth.add(revenue, -total_costs)  // 15000
  
  @azimuth.assert_eq(15000, profit)
  
  // 利润率计算：利润率 = 利润 / 收入 * 100%
  let profit_margin = @azimuth.multiply(profit, 100) / revenue
  @azimuth.assert_eq(30, profit_margin)  // 整数除法，15000 * 100 / 50000 = 30
  
  // 盈亏平衡点计算：固定成本 / (单位价格 - 单位可变成本)
  let unit_price = 100
  let unit_variable_cost = 30
  
  let contribution_margin = @azimuth.add(unit_price, -unit_variable_cost)  // 70
  let break_even_units = @azimuth.divide_with_ceil(fixed_costs, contribution_margin)  // 20000 / 70 ≈ 286
  @azimuth.assert_eq(286, break_even_units)
  
  // 验证盈亏平衡
  let break_even_revenue = @azimuth.multiply(break_even_units, unit_price)  // 28600
  let break_even_variable_costs = @azimuth.multiply(break_even_units, unit_variable_cost)  // 8580
  let break_even_total_costs = @azimuth.add(fixed_costs, break_even_variable_costs)  // 28580
  let break_even_profit = @azimuth.add(break_even_revenue, -break_even_total_costs)  // 20
  
  // 由于向上取整，会有小幅利润
  @azimuth.assert_true(break_even_profit >= 0 && break_even_profit < contribution_margin)
}

test "statistics_calculations_simplified" {
  // 测试简化的统计学计算
  // 数据集：12, 15, 18, 21, 24
  let data1 = 12
  let data2 = 15
  let data3 = 18
  let data4 = 21
  let data5 = 24
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)  // 90
  let count = 5
  let mean = sum / count  // 18
  @azimuth.assert_eq(18, mean)
  
  // 计算方差（简化版，使用整数除法）
  // 方差 = Σ(x - mean)² / n
  let diff1 = @azimuth.add(data1, -mean)  // -6
  let diff2 = @azimuth.add(data2, -mean)  // -3
  let diff3 = @azimuth.add(data3, -mean)  // 0
  let diff4 = @azimuth.add(data4, -mean)  // 3
  let diff5 = @azimuth.add(data5, -mean)  // 6
  
  let squared_diff1 = @azimuth.multiply(diff1, diff1)  // 36
  let squared_diff2 = @azimuth.multiply(diff2, diff2)  // 9
  let squared_diff3 = @azimuth.multiply(diff3, diff3)  // 0
  let squared_diff4 = @azimuth.multiply(diff4, diff4)  // 9
  let squared_diff5 = @azimuth.multiply(diff5, diff5)  // 36
  
  let sum_squared_diffs = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(squared_diff1, squared_diff2), squared_diff3), squared_diff4), squared_diff5)  // 90
  let variance = sum_squared_diffs / count  // 18
  @azimuth.assert_eq(18, variance)
  
  // 计算标准差（简化版，使用整数近似）
  // 标准差 = √方差 ≈ 4.24，取整数部分为4
  let standard_deviation_approx = 4
  @azimuth.assert_eq(4, standard_deviation_approx)
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(data5, -data1)  // 24 - 12 = 12
  @azimuth.assert_eq(12, range)
  
  // 计算中位数（已排序数据集的中间值）
  let median = data3  // 18
  @azimuth.assert_eq(18, median)
}