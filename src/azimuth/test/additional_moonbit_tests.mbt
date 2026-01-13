// 额外的 MoonBit 测试用例
// 这些测试用例补充现有的测试，覆盖更多场景

test "add_function_commutativity" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -12345
  let d = 67890
  
  let result3 = @azimuth.add(c, d)
  let result4 = @azimuth.add(d, c)
  
  @azimuth.assert_eq(result3, result4)
}

test "multiply_function_distributivity" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 10
  let c = 15
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "arithmetic_sequence_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + n = n * (n + 1) / 2
  let n = 100
  
  // 使用公式计算：100 * (100 + 1) / 2 = 100 * 101 / 2 = 5050
  let formula_result = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  
  // 使用循环验证（通过展开的方式）
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
      1, 2), 3), 4), 5), 6), 7), 8), 9), 10)
  
  // 验证前10项的和：1+2+...+10 = 55
  @azimuth.assert_eq(55, sum)
  
  // 验证公式结果
  @azimuth.assert_eq(5050, formula_result)
}

test "percentage_calculation" {
  // 测试百分比计算
  let total = 500
  let percentage = 20  // 20%
  
  // 计算百分比值：total * percentage / 100
  let percentage_value = @azimuth.multiply(total, percentage) / 100
  
  @azimuth.assert_eq(100, percentage_value)
  
  // 测试另一个例子
  let total2 = 1200
  let percentage2 = 15  // 15%
  
  let percentage_value2 = @azimuth.multiply(total2, percentage2) / 100
  
  @azimuth.assert_eq(180, percentage_value2)
}

test "discount_calculation" {
  // 测试折扣计算
  let original_price = 1000
  let discount_percentage = 25  // 25% 折扣
  
  // 计算折扣金额：original_price * discount_percentage / 100
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  
  // 计算最终价格：original_price - discount_amount
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(250, discount_amount)
  @azimuth.assert_eq(750, final_price)
}

test "compound_interest_simple" {
  // 测试简单复利计算
  let principal = 1000
  let rate = 5  // 5% 利率
  let years = 3
  
  // 简化复利计算：最终金额 = 本金 * (1 + 年数 * 利率/100)
  let total_multiplier = @azimuth.add(1, @azimuth.multiply(years, rate) / 100)
  let final_amount = @azimuth.multiply(principal, total_multiplier)
  
  @azimuth.assert_eq(1150, final_amount)  // 1000 * (1 + 3*5/100) = 1000 * 1.15 = 1150
}

test "string_length_handling" {
  // 测试不同长度字符串的处理
  let empty = ""
  let short = "Hi"
  let medium = "MoonBit"
  let long = "This is a very long string that contains many words"
  
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty))
  @azimuth.assert_eq_string("Hello, Hi!", @azimuth.greet(short))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet(medium))
  @azimuth.assert_eq_string("Hello, This is a very long string that contains many words!", @azimuth.greet(long))
}

test "mathematical_identities" {
  // 测试数学恒等式
  
  // 加法单位元：a + 0 = a
  let a = 42
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法单位元：a * 1 = a
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  
  // 乘法零元：a * 0 = 0
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, a))
  
  // 乘法负一元：a * -1 = -a
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(42, @azimuth.multiply(-42, -1))
}

test "geometric_calculations" {
  // 测试几何计算
  
  // 正方形面积：边长 × 边长
  let side_length = 10
  let square_area = @azimuth.multiply(side_length, side_length)
  @azimuth.assert_eq(100, square_area)
  
  // 正方形周长：4 × 边长
  let square_perimeter = @azimuth.multiply(4, side_length)
  @azimuth.assert_eq(40, square_perimeter)
  
  // 长方形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 长方形周长：2 × (长 + 宽)
  let rectangle_perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, rectangle_perimeter)
}

test "time_calculations" {
  // 测试时间计算
  
  // 小时转分钟：小时 × 60
  let hours = 3
  let minutes = @azimuth.multiply(hours, 60)
  @azimuth.assert_eq(180, minutes)
  
  // 天转小时：天 × 24
  let days = 5
  let hours_in_days = @azimuth.multiply(days, 24)
  @azimuth.assert_eq(120, hours_in_days)
  
  // 计算总分钟数：（天 × 24 + 小时）× 60 + 分钟
  let total_days = 2
  let total_hours = 5
  let total_minutes = 30
  
  let total_hours_all = @azimuth.add(@azimuth.multiply(total_days, 24), total_hours)
  let total_minutes_all = @azimuth.add(@azimuth.multiply(total_hours_all, 60), total_minutes)
  
  @azimuth.assert_eq(2910, total_minutes_all)  // (2*24 + 5)*60 + 30 = 2910
}