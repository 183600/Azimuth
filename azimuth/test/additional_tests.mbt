// Azimuth 项目附加测试用例
// 使用标准 MoonBit 测试语法，补充更多测试场景

test "fibonacci_sequence_calculation" {
  // 使用 add 函数计算斐波那契数列
  let n1 = 0
  let n2 = 1
  let n3 = @azimuth.add(n1, n2)  // 1
  let n4 = @azimuth.add(n2, n3)  // 2
  let n5 = @azimuth.add(n3, n4)  // 3
  let n6 = @azimuth.add(n4, n5)  // 5
  let n7 = @azimuth.add(n5, n6)  // 8
  let n8 = @azimuth.add(n6, n7)  // 13
  
  @azimuth.assert_eq(1, n3)
  @azimuth.assert_eq(2, n4)
  @azimuth.assert_eq(3, n5)
  @azimuth.assert_eq(5, n6)
  @azimuth.assert_eq(8, n7)
  @azimuth.assert_eq(13, n8)
}

test "factorial_calculation" {
  // 使用 multiply 函数计算阶乘
  let fact_1 = @azimuth.multiply(1, 1)  // 1! = 1
  let fact_2 = @azimuth.multiply(fact_1, 2)  // 2! = 2
  let fact_3 = @azimuth.multiply(fact_2, 3)  // 3! = 6
  let fact_4 = @azimuth.multiply(fact_3, 4)  // 4! = 24
  let fact_5 = @azimuth.multiply(fact_4, 5)  // 5! = 120
  
  @azimuth.assert_eq(1, fact_1)
  @azimuth.assert_eq(2, fact_2)
  @azimuth.assert_eq(6, fact_3)
  @azimuth.assert_eq(24, fact_4)
  @azimuth.assert_eq(120, fact_5)
}

test "geometric_calculations" {
  // 几何计算测试
  
  // 正方形面积：边长的平方
  let side_length = 6
  let square_area = @azimuth.multiply(side_length, side_length)
  @azimuth.assert_eq(36, square_area)
  
  // 长方形面积：长 × 宽
  let length = 8
  let width = 5
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "time_calculations" {
  // 时间计算测试
  
  // 计算总秒数：小时 × 3600 + 分钟 × 60 + 秒
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let hours_to_seconds = @azimuth.multiply(hours, 3600)
  let minutes_to_seconds = @azimuth.multiply(minutes, 60)
  let total_seconds = @azimuth.add(@azimuth.add(hours_to_seconds, minutes_to_seconds), seconds)
  @azimuth.assert_eq(9045, total_seconds)
  
  // 计算天数：小时 ÷ 24
  let total_hours = 72
  let days = total_hours / 24
  @azimuth.assert_eq(3, days)
}

test "financial_calculations" {
  // 金融计算测试
  
  // 简单利息：本金 × 利率 × 时间
  let principal = 1000
  let interest_rate = 5  // 5%
  let time_years = 3
  let simple_interest = @azimuth.multiply(@azimuth.multiply(principal, interest_rate), time_years) / 100
  @azimuth.assert_eq(150, simple_interest)
  
  // 总金额：本金 + 利息
  let total_amount = @azimuth.add(principal, simple_interest)
  @azimuth.assert_eq(1150, total_amount)
  
  // 复合利息（简化）：本金 × (1 + 利率)^时间
  let rate_decimal = 5
  let compound_factor = @azimuth.add(100, rate_decimal)  // 105%
  let amount_after_2_years = @azimuth.multiply(principal, compound_factor) / 100
  let final_amount = @azimuth.multiply(amount_after_2_years, compound_factor) / 100
  @azimuth.assert_eq(1102, final_amount)  // 向下取整
}

test "statistical_calculations" {
  // 统计计算测试
  
  // 计算平均值
  let numbers = [12, 15, 18, 21, 24]
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  let count = 5
  let average = sum / count
  @azimuth.assert_eq(18, average)
  
  // 计算方差（简化）
  let deviations = [
    @azimuth.add(numbers[0], -average),  // -6
    @azimuth.add(numbers[1], -average),  // -3
    @azimuth.add(numbers[2], -average),  // 0
    @azimuth.add(numbers[3], -average),  // 3
    @azimuth.add(numbers[4], -average)   // 6
  ]
  let squared_deviations = [
    @azimuth.multiply(deviations[0], deviations[0]),  // 36
    @azimuth.multiply(deviations[1], deviations[1]),  // 9
    @azimuth.multiply(deviations[2], deviations[2]),  // 0
    @azimuth.multiply(deviations[3], deviations[3]),  // 9
    @azimuth.multiply(deviations[4], deviations[4])   // 36
  ]
  let variance_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(squared_deviations[0], squared_deviations[1]), squared_deviations[2]), squared_deviations[3]), squared_deviations[4])
  let variance = variance_sum / count
  @azimuth.assert_eq(18, variance)
}

test "greet_function_with_special_characters" {
  // 测试 greet 函数处理特殊字符
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))  // 空格
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))  // 换行和制表符
  @azimuth.assert_eq_string("Hello, \"!", @azimuth.greet("\""))  // 引号
  @azimuth.assert_eq_string("Hello, \\", @azimuth.greet("\\"))  // 反斜杠
}

test "mixed_operations_stress_test" {
  // 混合运算压力测试
  let result = 0
  
  // 执行一系列混合运算
  result = @azimuth.add(result, @azimuth.multiply(10, 5))     // 50
  result = @azimuth.add(result, @azimuth.multiply(-3, 4))     // 38
  result = @azimuth.add(result, @azimuth.multiply(7, -2))     // 24
  result = @azimuth.add(result, @azimuth.multiply(0, 100))    // 24
  result = @azimuth.add(result, @azimuth.multiply(-5, -5))    // 49
  result = @azimuth.add(result, @azimuth.multiply(1, 51))     // 100
  
  @azimuth.assert_eq(100, result)
}

test "boundary_value_combinations" {
  // 边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的运算
  let sum_extremes = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, sum_extremes)
  
  // 最大值与-1的运算
  let max_minus_one = @azimuth.add(max_val, -1)
  @azimuth.assert_eq(2147483646, max_minus_one)
  
  // 最小值与1的运算
  let min_plus_one = @azimuth.add(min_val, 1)
  @azimuth.assert_eq(-2147483647, min_plus_one)
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}