// 增强的 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能的高级应用场景

test "binary_number_conversion" {
  // 测试二进制数转换模拟
  // 将二进制数 1011 (11) 转换为十进制
  let bit3 = 1  // 2³ = 8
  let bit2 = 0  // 2² = 0
  let bit1 = 1  // 2¹ = 2
  let bit0 = 1  // 2⁰ = 1
  
  let decimal_value = @azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(bit3, 8), 
    @azimuth.multiply(bit2, 4)), 
    @azimuth.multiply(bit1, 2)), 
    @azimuth.multiply(bit0, 1))
  
  @azimuth.assert_eq(11, decimal_value)
  
  // 测试另一个二进制数 1100 (12)
  let decimal_value2 = @azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(1, 8), 
    @azimuth.multiply(1, 4)), 
    @azimuth.multiply(0, 2)), 
    @azimuth.multiply(0, 1))
  
  @azimuth.assert_eq(12, decimal_value2)
}

test "array_sum_calculation" {
  // 测试数组求和计算（模拟）
  // 模拟数组 [5, 10, 15, 20, 25] 的求和
  let element1 = 5
  let element2 = 10
  let element3 = 15
  let element4 = 20
  let element5 = 25
  
  let array_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    element1, element2), element3), element4), element5)
  
  @azimuth.assert_eq(75, array_sum)
  
  // 计算平均值
  let array_length = 5
  let average = array_sum / array_length
  @azimuth.assert_eq(15, average)
}

test "simple_interest_calculation" {
  // 测试单利计算
  let principal = 5000  // 本金
  let rate = 8  // 年利率百分比
  let time = 3  // 年数
  
  // 单利公式：利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), time)
  
  // 总金额 = 本金 + 利息
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(120000, interest)  // 5000 × 8 × 3 = 120000
  @azimuth.assert_eq(125000, total_amount)  // 5000 + 120000 = 125000
}

test "speed_distance_time_calculation" {
  // 测试速度、距离、时间计算
  let speed = 60  // km/h
  let time = 2.5  // 小时（用整数表示：2.5 = 5/2）
  
  // 距离 = 速度 × 时间
  // 使用整数计算：60 × 5 ÷ 2 = 150
  let distance = @azimuth.multiply(speed, 5) / 2
  
  @azimuth.assert_eq(150, distance)
  
  // 反向计算：时间 = 距离 ÷ 速度
  let calculated_time = @azimuth.multiply(distance, 2) / speed
  @azimuth.assert_eq(5, calculated_time)  // 应该得到 5（表示 5/2 = 2.5 小时）
}

test "string_length_processing" {
  // 测试字符串长度处理（通过字符计数模拟）
  let short_name = "Hi"  // 2个字符
  let medium_name = "MoonBit"  // 7个字符
  let long_name = "Azimuth Project"  // 16个字符
  
  // 测试不同长度字符串的greet函数处理
  @azimuth.assert_eq_string("Hello, Hi!", @azimuth.greet(short_name))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(medium_name))
  @azimuth.assert_eq_string("Hello, Azimuth Project!", @azimuth.greet(long_name))
  
  // 测试包含数字的字符串
  let numeric_name = "User123"
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(numeric_name))
}

test "percentage_discount_calculation" {
  // 测试百分比折扣计算
  let original_price = 200
  let discount_percentage = 15  // 15% 折扣
  
  // 折扣金额 = 原价 × 折扣百分比 ÷ 100
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  
  // 折后价格 = 原价 - 折扣金额
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(30, discount_amount)  // 200 × 15 ÷ 100 = 30
  @azimuth.assert_eq(170, discounted_price)  // 200 - 30 = 170
  
  // 测试多级折扣
  let additional_discount = 10  // 额外10%折扣
  let additional_amount = @azimuth.multiply(discounted_price, additional_discount) / 100
  let final_price = @azimuth.add(discounted_price, -additional_amount)
  
  @azimuth.assert_eq(17, additional_amount)  // 170 × 10 ÷ 100 = 17
  @azimuth.assert_eq(153, final_price)  // 170 - 17 = 153
}

test "coordinate_geometry_distance" {
  // 测试坐标几何距离计算（简化版，不使用平方根）
  // 点 A(3, 4) 和点 B(7, 1)
  let x1 = 3
  let y1 = 4
  let x2 = 7
  let y2 = 1
  
  // 计算x和y的差值
  let dx = @azimuth.add(x2, -x1)  // 7 - 3 = 4
  let dy = @azimuth.add(y2, -y1)  // 1 - 4 = -3
  
  // 计算距离的平方（避免平方根）
  let dx_squared = @azimuth.multiply(dx, dx)  // 4² = 16
  let dy_squared = @azimuth.multiply(dy, dy)  // (-3)² = 9
  let distance_squared = @azimuth.add(dx_squared, dy_squared)  // 16 + 9 = 25
  
  @azimuth.assert_eq(25, distance_squared)  // 距离的平方是25，实际距离是5
}

test "fibonacci_sequence_calculation" {
  // 测试斐波那契数列计算（前几项）
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)  // 1
  let f3 = @azimuth.add(f2, f1)  // 2
  let f4 = @azimuth.add(f3, f2)  // 3
  let f5 = @azimuth.add(f4, f3)  // 5
  let f6 = @azimuth.add(f5, f4)  // 8
  let f7 = @azimuth.add(f6, f5)  // 13
  
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  
  // 计算前8项的和
  let sum_fib = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f0, f1), f2), f3), f4), f5), f6), f7)
  @azimuth.assert_eq(33, sum_fib)  // 0 + 1 + 1 + 2 + 3 + 5 + 8 + 13 = 33
}

test "leap_year_calculation" {
  // 测试闰年计算逻辑
  // 闰年规则：能被4整除但不能被100整除，或者能被400整除
  
  let year1 = 2020  // 闰年（能被4整除）
  let year2 = 1900  // 非闰年（能被100整除但不能被400整除）
  let year3 = 2000  // 闰年（能被400整除）
  let year4 = 2023  // 非闰年（不能被4整除）
  
  // 使用模运算模拟（通过减法实现）
  let is_leap1 = (year1 / 4) * 4 == year1 && (year1 / 100) * 100 != year1
  let is_leap2 = (year2 / 4) * 4 == year2 && (year2 / 100) * 100 != year2
  let is_leap3 = (year3 / 400) * 400 == year3
  let is_leap4 = (year4 / 4) * 4 == year4 && (year4 / 100) * 100 != year4
  
  // 验证闰年判断
  @azimuth.assert_true(is_leap1)  // 2020是闰年
  @azimuth.assert_false(is_leap2)  // 1900不是闰年
  @azimuth.assert_true(is_leap3)  // 2000是闰年
  @azimuth.assert_false(is_leap4)  // 2023不是闰年
}

test "complex_number_operations_simulation" {
  // 测试复数运算模拟（简化版）
  // 复数 (3 + 2i) 和 (1 + 4i) 的运算
  
  let real1 = 3
  let imag1 = 2
  let real2 = 1
  let imag2 = 4
  
  // 复数加法：(a + bi) + (c + di) = (a + c) + (b + d)i
  let add_real = @azimuth.add(real1, real2)  // 3 + 1 = 4
  let add_imag = @azimuth.add(imag1, imag2)  // 2 + 4 = 6
  
  // 复数乘法：(a + bi) × (c + di) = (ac - bd) + (ad + bc)i
  let mul_real = @azimuth.add(@azimuth.multiply(real1, real2), @azimuth.multiply(-imag1, imag2))  // 3×1 - 2×4 = 3 - 8 = -5
  let mul_imag = @azimuth.add(@azimuth.multiply(real1, imag2), @azimuth.multiply(imag1, real2))  // 3×4 + 2×1 = 12 + 2 = 14
  
  @azimuth.assert_eq(4, add_real)
  @azimuth.assert_eq(6, add_imag)
  @azimuth.assert_eq(-5, mul_real)
  @azimuth.assert_eq(14, mul_imag)
}