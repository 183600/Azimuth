// Azimuth 项目额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "factorial_calculation" {
  // 计算5的阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let result = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, result)
}

test "fibonacci_sequence" {
  // 计算斐波那契数列的第6项：F(6) = F(5) + F(4) = 5 + 3 = 8
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  @azimuth.assert_eq(8, f6)
}

test "binary_to_decimal_simulation" {
  // 模拟二进制转十进制：1010 = 1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  let bit3 = 1
  let bit2 = 0
  let bit1 = 1
  let bit0 = 0
  
  let power_of_2_3 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)  // 2³ = 8
  let power_of_2_2 = @azimuth.multiply(@azimuth.multiply(2, 2), 2)  // 2² = 4
  let power_of_2_1 = @azimuth.multiply(2, 1)  // 2¹ = 2
  let power_of_2_0 = 1  // 2⁰ = 1
  
  let decimal = @azimuth.add(@azimuth.add(@azimuth.multiply(bit3, power_of_2_3), @azimuth.multiply(bit2, power_of_2_2)), @azimuth.add(@azimuth.multiply(bit1, power_of_2_1), @azimuth.multiply(bit0, power_of_2_0)))
  @azimuth.assert_eq(10, decimal)
}

test "greatest_common_divisor" {
  // 使用欧几里得算法模拟计算GCD(48, 18) = 6
  let a = 48
  let b = 18
  
  // 48 ÷ 18 = 2 余 12
  let remainder1 = a - @azimuth.multiply(2, b)  // 48 - 2×18 = 12
  
  // 18 ÷ 12 = 1 余 6
  let remainder2 = b - @azimuth.multiply(1, remainder1)  // 18 - 1×12 = 6
  
  // 12 ÷ 6 = 2 余 0，所以GCD是6
  let remainder3 = remainder1 - @azimuth.multiply(2, remainder2)  // 12 - 2×6 = 0
  
  @azimuth.assert_eq(0, remainder3)
  @azimuth.assert_eq(6, remainder2)  // GCD是最后一个非零余数
}

test "quadratic_equation_roots" {
  // 求解二次方程 x² - 5x + 6 = 0 的根
  // 因式分解：(x-2)(x-3) = 0，所以根是 x=2 和 x=3
  let x1 = 2
  let x2 = 3
  
  // 验证根 x1 = 2
  let x1_squared = @azimuth.multiply(x1, x1)  // 4
  let term1_x1 = @azimuth.multiply(-5, x1)  // -10
  let result_x1 = @azimuth.add(@azimuth.add(x1_squared, term1_x1), 6)  // 4 - 10 + 6 = 0
  @azimuth.assert_eq(0, result_x1)
  
  // 验证根 x2 = 3
  let x2_squared = @azimuth.multiply(x2, x2)  // 9
  let term1_x2 = @azimuth.multiply(-5, x2)  // -15
  let result_x2 = @azimuth.add(@azimuth.add(x2_squared, term1_x2), 6)  // 9 - 15 + 6 = 0
  @azimuth.assert_eq(0, result_x2)
}

test "prime_number_check" {
  // 检查数字17是否为质数
  let number = 17
  let is_prime = true
  
  // 检查能否被2到√17之间的数整除（简化检查到4）
  let divisor2 = number % 2  // 17 % 2 = 1，不整除
  let divisor3 = number % 3  // 17 % 3 = 2，不整除
  let divisor4 = number % 4  // 17 % 4 = 1，不整除
  
  // 如果所有余数都不为0，则是质数
  @azimuth.assert_eq(divisor2 != 0, true)
  @azimuth.assert_eq(divisor3 != 0, true)
  @azimuth.assert_eq(divisor4 != 0, true)
  
  // 检查数字15不是质数
  let not_prime = 15
  let divisor5 = not_prime % 3  // 15 % 3 = 0，整除
  @azimuth.assert_eq(0, divisor5)
}

test "speed_distance_time" {
  // 速度、距离、时间关系测试
  // 公式：距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2    // hours
  let distance = @azimuth.multiply(speed, time)  // 120 km
  @azimuth.assert_eq(120, distance)
  
  // 反向计算：速度 = 距离 / 时间
  let calculated_speed = distance / time
  @azimuth.assert_eq(60, calculated_speed)
  
  // 反向计算：时间 = 距离 / 速度
  let calculated_time = distance / speed
  @azimuth.assert_eq(2, calculated_time)
}

test "simple_interest_calculation" {
  // 简单利息计算：利息 = 本金 × 利率 × 时间
  let principal = 1000  // 本金
  let rate = 5          // 年利率百分比
  let time = 3          // 年数
  
  // 计算利息
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time) / 100
  @azimuth.assert_eq(150, interest)  // 1000 × 5 × 3 / 100 = 150
  
  // 计算总金额
  let total_amount = @azimuth.add(principal, interest)
  @azimuth.assert_eq(1150, total_amount)
}

test "area_perimeter_calculations" {
  // 计算不同形状的面积和周长
  // 正方形：边长为5
  let square_side = 5
  let square_area = @azimuth.multiply(square_side, square_side)  // 25
  let square_perimeter = @azimuth.multiply(4, square_side)      // 20
  @azimuth.assert_eq(25, square_area)
  @azimuth.assert_eq(20, square_perimeter)
  
  // 长方形：长为8，宽为4
  let rect_length = 8
  let rect_width = 4
  let rect_area = @azimuth.multiply(rect_length, rect_width)  // 32
  let rect_perimeter = @azimuth.add(@azimuth.multiply(2, rect_length), @azimuth.multiply(2, rect_width))  // 24
  @azimuth.assert_eq(32, rect_area)
  @azimuth.assert_eq(24, rect_perimeter)
  
  // 三角形：底为6，高为4
  let triangle_base = 6
  let triangle_height = 4
  let triangle_area = @azimuth.multiply(triangle_base, triangle_height) / 2  // 12
  @azimuth.assert_eq(12, triangle_area)
}

test "string_concatenation_advanced" {
  // 高级字符串连接测试
  // 测试不同类型的字符串
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a b c!", @azimuth.greet("a b c"))
  @azimuth.assert_eq_string("Hello, 特殊字符@#$!", @azimuth.greet("特殊字符@#$"))
  
  // 测试包含数字和字母的混合字符串
  let mixed_string = "User2023"
  @azimuth.assert_eq_string("Hello, User2023!", @azimuth.greet(mixed_string))
  
  // 测试包含空格的字符串
  let spaced_string = "  leading and trailing  "
  @azimuth.assert_eq_string("Hello,   leading and trailing  !", @azimuth.greet(spaced_string))
}