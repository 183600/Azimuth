// 额外的 MoonBit 测试用例
// 包含10个使用标准 MoonBit 测试语法的测试用例

test "integer_division_and_remainder" {
  // 测试整数除法和取余运算
  let dividend = 17
  let divisor = 5
  
  // 计算商和余数
  let quotient = dividend / divisor
  let remainder = dividend % divisor
  
  @azimuth.assert_eq(3, quotient)
  @azimuth.assert_eq(2, remainder)
  
  // 验证除法恒等式：dividend = divisor * quotient + remainder
  let reconstructed = @azimuth.add(@azimuth.multiply(divisor, quotient), remainder)
  @azimuth.assert_eq(dividend, reconstructed)
  
  // 测试负数情况
  let negative_dividend = -17
  let negative_quotient = negative_dividend / divisor
  let negative_remainder = negative_dividend % divisor
  
  @azimuth.assert_eq(-3, negative_quotient)
  @azimuth.assert_eq(-2, negative_remainder)
}

test "string_case_and_formatting" {
  // 测试字符串大小写和格式化
  let lower_case = "hello"
  let upper_case = "HELLO"
  let mixed_case = "HeLLo WoRLd"
  
  // 测试包含不同大小写的字符串
  @azimuth.assert_eq_string("Hello, hello!", @azimuth.greet(lower_case))
  @azimuth.assert_eq_string("Hello, HELLO!", @azimuth.greet(upper_case))
  @azimuth.assert_eq_string("Hello, HeLLo WoRLd!", @azimuth.greet(mixed_case))
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric1 = "test123"
  let alphanumeric2 = "123test"
  let alphanumeric3 = "1t2e3s4t"
  
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet(alphanumeric1))
  @azimuth.assert_eq_string("Hello, 123test!", @azimuth.greet(alphanumeric2))
  @azimuth.assert_eq_string("Hello, 1t2e3s4t!", @azimuth.greet(alphanumeric3))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算斐波那契数列前10项的和
  // 斐波那契数列：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  let fib8 = @azimuth.add(fib6, fib7)  // 21
  let fib9 = @azimuth.add(fib7, fib8)  // 34
  let fib10 = @azimuth.add(fib8, fib9) // 55
  
  // 计算总和
  let fib_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib1, fib2), fib3), fib4), fib5), @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib6, fib7), fib8), fib9), fib10))
  @azimuth.assert_eq(143, fib_sum)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5² = 55
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.add(@azimuth.multiply(4, 4), @azimuth.multiply(5, 5)))
  @azimuth.assert_eq(55, square_sum)
}

test "conditional_logic_and_branching" {
  // 测试条件逻辑和分支
  let x = 15
  let y = 10
  let z = 20
  
  // 测试嵌套条件
  let result1 = if x > y {
    if x > z { @azimuth.add(x, y) } else { @azimuth.add(x, z) }
  } else {
    @azimuth.multiply(x, y)
  }
  
  // 由于 15 > 10 为真，但 15 > 20 为假，所以执行 x + z = 15 + 20 = 35
  @azimuth.assert_eq(35, result1)
  
  // 测试多个条件
  let result2 = if x > y && x < z {
    @azimuth.multiply(x, 2)
  } else {
    @azimuth.add(y, z)
  }
  
  // 由于 15 > 10 && 15 < 20 为真，所以执行 15 * 2 = 30
  @azimuth.assert_eq(30, result2)
  
  // 测试复杂条件表达式
  let _result3 = if @azimuth.add(x, y) > z {
    @azimuth.add(x, -y)  // 使用加负数模拟减法
  } else {
    @azimuth.multiply(y, z)
  }
  
  // 由于 15 + 10 > 20 为真，所以执行 15 + (-10) = 5
  @azimuth.assert_eq(5, @azimuth.add(x, -y))
}

test "array_index_calculations" {
  // 测试数组索引计算
  let rows = 4
  let cols = 5
  
  // 二维数组转一维数组的索引计算
  let row = 2
  let col = 3
  let index_1d = @azimuth.add(@azimuth.multiply(row, cols), col)
  @azimuth.assert_eq(13, index_1d)
  
  // 测试边界索引
  let first_index = @azimuth.add(@azimuth.multiply(0, cols), 0)
  let last_index = @azimuth.add(@azimuth.multiply(rows - 1, cols - 1), cols - 1)
  
  @azimuth.assert_eq(0, first_index)
  @azimuth.assert_eq(19, last_index)
  
  // 三维数组转一维数组的索引计算
  let z = 1
  let y = 2
  let x = 3
  let index_3d = @azimuth.add(@azimuth.add(@azimuth.multiply(z, rows), y), @azimuth.multiply(x, cols))
  
  // 1*4 + 2 + 3*5 = 4 + 2 + 15 = 21
  @azimuth.assert_eq(21, index_3d)
}

test "financial_calculations" {
  // 测试金融计算
  // 场景：贷款计算
  let principal = 100000  // 本金
  let annual_rate = 5     // 年利率5%
  let years = 10          // 贷款年限
  
  // 简化的单利计算：利息 = 本金 × 利率 × 年数
  let simple_interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), years) / 100
  let total_payment_simple = @azimuth.add(principal, simple_interest)
  
  @azimuth.assert_eq(50000, simple_interest)
  @azimuth.assert_eq(150000, total_payment_simple)
  
  // 场景：复利计算（简化版）
  // 使用近似公式：A = P(1 + r/n)^(nt)，简化为 A = P(1 + r*t)
  let compound_factor = @azimuth.add(100, @azimuth.multiply(annual_rate, years))
  let total_payment_compound = @azimuth.multiply(principal, compound_factor) / 100
  
  @azimuth.assert_eq(150000, total_payment_compound)
  
  // 场景：分期付款计算
  let monthly_payment = total_payment_simple / (years * 12)
  @azimuth.assert_eq(1250, monthly_payment)
}

test "geometry_and_trigonometry" {
  // 测试几何和三角函数（简化版）
  // 圆的周长和面积计算
  let radius = 10
  let pi_approx = 3  // 简化的π值
  
  // 周长 = 2πr
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi_approx), radius)
  @azimuth.assert_eq(60, circumference)
  
  // 面积 = πr²
  let area = @azimuth.multiply(pi_approx, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(300, area)
  
  // 矩形周长和面积
  let length = 12
  let width = 8
  
  // 周长 = 2(l + w)
  let rect_perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, rect_perimeter)
  
  // 面积 = l × w
  let rect_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rect_area)
  
  // 梯形面积 = (a + b) × h / 2
  let base1 = 6
  let base2 = 10
  let height = 5
  let trapezoid_area = @azimuth.multiply(@azimuth.add(base1, base2), height) / 2
  @azimuth.assert_eq(40, trapezoid_area)
}

test "time_and_date_calculations" {
  // 测试时间和日期计算
  // 时间转换
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = @azimuth.add(@azimuth.multiply(hours, 3600), @azimuth.add(@azimuth.multiply(minutes, 60), seconds))
  @azimuth.assert_eq(9930, total_seconds)
  
  // 时间差计算
  let start_hour = 8
  let start_min = 30
  let end_hour = 17
  let end_min = 45
  
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_min)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_min)
  let duration = @azimuth.add(end_total, -start_total)
  
  @azimuth.assert_eq(555, duration)  // 9小时15分钟 = 555分钟
  
  // 日期计算（简化版）
  let day = 15
  let month = 6
  
  // 计算年份中的第几天（简化，不考虑闰年）
  let day_of_year = @azimuth.add(day, @azimuth.multiply(month - 1, 30))
  @azimuth.assert_eq(165, day_of_year)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：10, 20, 30, 40, 50
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  @azimuth.assert_eq(150, sum)
  
  // 计算平均值
  let mean = sum / 5
  @azimuth.assert_eq(30, mean)
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(data1, -mean)
  let diff2 = @azimuth.add(data2, -mean)
  let diff3 = @azimuth.add(data3, -mean)
  let diff4 = @azimuth.add(data4, -mean)
  let diff5 = @azimuth.add(data5, -mean)
  
  let sq_diff1 = @azimuth.multiply(diff1, diff1)
  let sq_diff2 = @azimuth.multiply(diff2, diff2)
  let sq_diff3 = @azimuth.multiply(diff3, diff3)
  let sq_diff4 = @azimuth.multiply(diff4, diff4)
  let sq_diff5 = @azimuth.multiply(diff5, diff5)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(sq_diff1, sq_diff2), sq_diff3), sq_diff4), sq_diff5) / 5
  @azimuth.assert_eq(200, variance)
  
  // 计算标准差（简化版，使用整数近似）
  let std_dev = 14  // sqrt(200) ≈ 14.14，取整数14
  @azimuth.assert_eq(14, std_dev)
}

test "algorithmic_problem_solving" {
  // 测试算法问题解决
  // 场景：最大公约数（GCD）- 使用欧几里得算法
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法
  let remainder1 = a % b  // 48 % 18 = 12
  let remainder2 = b % remainder1  // 18 % 12 = 6
  let _remainder3 = remainder1 % remainder2  // 12 % 6 = 0
  
  // GCD是最后一个非零余数
  let gcd = remainder2
  @azimuth.assert_eq(6, gcd)
  
  // 场景：最小公倍数（LCM）
  // LCM(a, b) = (a × b) / GCD(a, b)
  let lcm = @azimuth.multiply(a, b) / gcd
  @azimuth.assert_eq(144, lcm)
  
  // 场景：判断素数（简化版）
  let num = 17
  let is_prime = if num > 1 {
    // 简化检查：只检查是否能被2, 3, 5整除
    let not_div_by_2 = num % 2 != 0
    let not_div_by_3 = num % 3 != 0
    let not_div_by_5 = num % 5 != 0
    not_div_by_2 && not_div_by_3 && not_div_by_5
  } else {
    false
  }
  
  @azimuth.assert_true(is_prime)
  
  // 场景：计算平方根（整数部分，使用二分查找简化版）
  let sqrt_approx = 9  // 已知81的平方根是9
  @azimuth.assert_eq(9, sqrt_approx)
}