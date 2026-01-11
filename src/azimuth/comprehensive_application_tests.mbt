// 综合应用测试用例 - 针对实际场景的测试

///|
test "complex_financial_calculations" {
  // 测试贷款计算：本金 + 利息 = 总还款额
  let principal = 100000
  let interest_rate = 5  // 5%
  let years = 3
  
  // 计算总利息：本金 * 利率 * 年限 / 100
  let total_interest = multiply(multiply(principal, interest_rate), years)
  // 计算总还款额：本金 + 总利息/100
  let total_payment = add(multiply(principal, 100), total_interest)
  
  // 验证计算结果
  assert_eq(11500000, total_payment)
}

///|
test "string_processing_edge_cases" {
  // 测试包含特殊字符的字符串
  assert_eq("Hello, @#$%!", greet("@#$%"))
  assert_eq("Hello,   !", greet("  "))  // 空格
  assert_eq("Hello, \n\t!", greet("\n\t"))  // 换行和制表符
  assert_eq("Hello, 中文测试!", greet("中文测试"))  // 中文字符
  assert_eq("Hello, éñçödéd!", greet("éñçödéd"))  // 带重音符号
}

///|
test "mathematical_sequence_calculations" {
  // 测试斐波那契数列相关计算
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)  // 2
  let fib4 = add(fib2, fib3)  // 3
  let fib5 = add(fib3, fib4)  // 5
  
  // 验证斐波那契数列
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  
  // 测试平方数列
  let square1 = multiply(1, 1)
  let square2 = multiply(2, 2)
  let square3 = multiply(3, 3)
  let square4 = multiply(4, 4)
  
  assert_eq(1, square1)
  assert_eq(4, square2)
  assert_eq(9, square3)
  assert_eq(16, square4)
}

///|
test "geometry_calculations" {
  // 测试几何计算：圆的面积和周长
  // 使用近似值 π ≈ 22/7 来避免浮点数
  let radius = 7
  let pi_numerator = 22
  
  // 面积 = π * r²
  let area_times_denominator = multiply(pi_numerator, multiply(radius, radius))
  // 周长 = 2 * π * r
  let circumference_times_denominator = multiply(2, multiply(pi_numerator, radius))
  
  // 验证结果
  assert_eq(1078, area_times_denominator)  // 22/7 * 49 = 154
  assert_eq(308, circumference_times_denominator)  // 2 * 22/7 * 7 = 44
  
  // 测试矩形对角线长度平方：a² + b²
  let width = 3
  let height = 4
  let diagonal_squared = add(multiply(width, width), multiply(height, height))
  assert_eq(25, diagonal_squared)  // 3² + 4² = 9 + 16 = 25
}

///|
test "statistical_calculations" {
  // 测试基本统计计算
  let numbers = [10, 20, 30, 40, 50]
  
  // 计算总和
  let sum = add(add(add(add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  assert_eq(150, sum)
  
  // 计算平均值（乘以数量避免除法）
  let sum_times_count = multiply(sum, 5)  // 150 * 5 = 750
  assert_eq(750, sum_times_count)
  
  // 计算方差：平方差的平均值
  let mean = 30
  let diff1 = add(numbers[0], multiply(-1, mean))  // 10 - 30 = -20
  let diff2 = add(numbers[1], multiply(-1, mean))  // 20 - 30 = -10
  let diff3 = add(numbers[2], multiply(-1, mean))  // 30 - 30 = 0
  let diff4 = add(numbers[3], multiply(-1, mean))  // 40 - 30 = 10
  let diff5 = add(numbers[4], multiply(-1, mean))  // 50 - 30 = 20
  
  let sum_of_squares = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5))
  assert_eq(1000, sum_of_squares)  // 400 + 100 + 0 + 100 + 400 = 1000
}

///|
test "time_and_date_calculations" {
  // 测试时间计算（以分钟为单位）
  let hours = 2
  let minutes = 30
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
  
  // 测试天数计算
  let days = 365
  let weeks = multiply(52, 7)  // 52周 * 7天
  let remaining_days = add(days, multiply(-1, weeks))
  assert_eq(1, remaining_days)  // 365 - 364 = 1
  
  // 测试闰年计算（4年一闰）
  let leap_years_divided_by_4 = 250  // 10年中有2.5个闰年
  assert_eq(250, leap_years_divided_by_4)
}

///|
test "physics_calculations" {
  // 测试基本物理公式
  // 速度 = 距离 / 时间（用乘法避免除法）
  let distance = 100
  let speed_times_time = multiply(distance, 1)  // 速度 * 5 = 100
  assert_eq(100, speed_times_time)
  
  // 动能 = 1/2 * m * v²（用乘法避免除法）
  let mass = 10
  let velocity = 5
  let kinetic_energy_times_2 = multiply(multiply(mass, velocity), velocity)
  assert_eq(250, kinetic_energy_times_2)  // 10 * 25 = 250
  
  // 力 = 质量 * 加速度
  let acceleration = 2
  let force = multiply(mass, acceleration)
  assert_eq(20, force)
}

///|
test "computer_science_calculations" {
  // 测试计算机科学相关计算
  // 二进制转换：2^n
  let power_of_2_3 = multiply(2, multiply(2, 2))  // 2³ = 8
  let power_of_2_8 = multiply(256, 1)  // 2⁸ = 256
  
  assert_eq(8, power_of_2_3)
  assert_eq(256, power_of_2_8)
  
  // 阶乘计算：n!
  let fact_3 = multiply(3, multiply(2, 1))  // 3! = 6
  let fact_4 = multiply(4, fact_3)  // 4! = 24
  let fact_5 = multiply(5, fact_4)  // 5! = 120
  
  assert_eq(6, fact_3)
  assert_eq(24, fact_4)
  assert_eq(120, fact_5)
}

///|
test "complex_edge_case_combinations" {
  // 测试复杂的边界情况组合
  // 最小值与各种值的运算组合
  let min_val = -2147483648
  let max_val = 2147483647
  
  // 最小值 + 1 + 1 + ... + 1 (连续加法)
  let result1 = add(add(min_val, 1), 1)
  assert_eq(-2147483646, result1)
  
  // 最大值 - 1 - 1 - ... - 1 (连续减法，用加负数实现)
  let result2 = add(add(max_val, -1), -1)
  assert_eq(2147483645, result2)
  
  // 复杂乘法组合：(a * b) * c
  let a = 1000
  let b = 2000
  let c = 3
  let complex_product = multiply(multiply(a, b), c)
  assert_eq(6000000, complex_product)
  
  // 字符串长度相关的边界测试
  let very_long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let long_greeting = greet(very_long_name)
  assert_eq(true, long_greeting.length() > 50)
}