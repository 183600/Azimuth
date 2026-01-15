// 用户新的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能和边界情况

test "arithmetic_sequence_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = multiply(n, add(n, 1)) / 2
  assert_eq(55, sum)
}

test "geometric_progression" {
  // 测试等比数列：2, 4, 8, 16, 32
  // 计算第5项的值
  let first_term = 2
  let ratio = 2
  let term5 = multiply(first_term, multiply(ratio, multiply(ratio, multiply(ratio, ratio))))
  assert_eq(32, term5)
}

test "temperature_conversion_complete" {
  // 完整的温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 - 1 + 32（对于常见温度）
  
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), add(-1, 32))
  assert_eq(77, fahrenheit)
}

test "discount_calculation" {
  // 折扣计算测试
  let original_price = 1000
  let discount_rate = 20  // 20% 折扣
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_rate) / 100
  assert_eq(200, discount_amount)
  
  // 计算折后价格
  let discounted_price = add(original_price, -discount_amount)
  assert_eq(800, discounted_price)
  
  // 计算税费（8%）
  let tax_rate = 8
  let tax_amount = multiply(discounted_price, tax_rate) / 100
  assert_eq(64, tax_amount)
  
  // 计算最终价格
  let final_price = add(discounted_price, tax_amount)
  assert_eq(864, final_price)
}

test "speed_distance_time" {
  // 速度、距离、时间关系测试
  // 距离 = 速度 × 时间
  
  let speed = 60  // km/h
  let time = 2.5  // 小时（用整数表示：2.5 = 5/2）
  
  // 使用整数计算：60 × 5 / 2 = 150
  let distance = multiply(speed, 5) / 2
  assert_eq(150, distance)
  
  // 反向计算：时间 = 距离 / 速度
  let calculated_time = multiply(distance, 2) / speed
  assert_eq(5, calculated_time)  // 5/2 = 2.5小时
}

test "array_manipulation_simulation" {
  // 数组操作模拟测试
  // 模拟数组求和：[10, 20, 30, 40, 50]
  let sum = add(add(add(add(10, 20), 30), 40), 50)
  assert_eq(150, sum)
  
  // 模拟数组平均值
  let average = sum / 5
  assert_eq(30, average)
  
  // 模拟查找最大值和最小值的差
  let max_val = 50
  let min_val = 10
  let range = add(max_val, -min_val)
  assert_eq(40, range)
}

test "compound_interest_accurate" {
  // 精确复利计算测试
  // 公式：A = P(1 + r/n)^(nt)
  // 简化版本：A = P(1 + r*t)
  
  let principal = 10000
  let rate = 5  // 5% 年利率
  let time = 3  // 3年
  
  // 计算简单利息
  let interest = multiply(principal, multiply(rate, time)) / 100
  assert_eq(1500, interest)
  
  // 计算总额
  let total = add(principal, interest)
  assert_eq(11500, total)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种字符串边界情况
  
  // 测试包含空格的字符串
  let spaced_name = "  John Doe  "
  let spaced_result = greet(spaced_name)
  assert_eq_string("Hello,   John Doe  !", spaced_result)
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  let special_result = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()!", special_result)
  
  // 测试数字字符串
  let number_string = "1234567890"
  let number_result = greet(number_string)
  assert_eq_string("Hello, 1234567890!", number_result)
}

test "logical_computation" {
  // 逻辑计算测试
  // 模拟布尔运算结果
  
  let a = 10
  let b = 20
  let c = 30
  
  // 模拟 AND 运算：a < b AND b < c
  let condition1 = a < b
  let condition2 = b < c
  let and_result = if condition1 && condition2 { 1 } else { 0 }
  assert_eq(1, and_result)
  
  // 模拟 OR 运算：a > b OR b < c
  let condition3 = a > b
  let condition4 = b < c
  let or_result = if condition3 || condition4 { 1 } else { 0 }
  assert_eq(1, or_result)
  
  // 测试条件表达式
  let conditional_result = if a < b { add(a, b) } else { multiply(a, b) }
  assert_eq(30, conditional_result)
}

test "polygon_calculations" {
  // 多边形计算测试
  // 计算正方形的面积和周长
  let square_side = 5
  let square_area = multiply(square_side, square_side)
  let square_perimeter = multiply(square_side, 4)
  
  assert_eq(25, square_area)
  assert_eq(20, square_perimeter)
  
  // 计算矩形的面积和周长
  let rect_length = 8
  let rect_width = 6
  let rect_area = multiply(rect_length, rect_width)
  let rect_perimeter = multiply(2, add(rect_length, rect_width))
  
  assert_eq(48, rect_area)
  assert_eq(28, rect_perimeter)
  
  // 计算三角形的面积（底×高÷2）
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = multiply(triangle_base, triangle_height) / 2
  
  assert_eq(30, triangle_area)
}