// 用户定义的综合测试用例
// 测试 azimuth 包的核心功能和边界情况

test "arithmetic_sequence_calculation" {
  // 计算等差数列的前n项和：1 + 3 + 5 + ... + (2n-1) = n²
  let n = 10
  let first_term = 1
  let common_diff = 2
  
  // 计算第n项
  let last_term = add(first_term, multiply(common_diff, n - 1))
  
  // 使用等差数列求和公式：n × (首项 + 末项) / 2
  let sum = multiply(n, add(first_term, last_term)) / 2
  
  // 验证结果：前10个奇数的和应该是100（10²）
  assert_eq(100, sum)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 + 30
  
  let celsius_freezing = 0      // 冰点
  let celsius_boiling = 100     // 沸点
  let celsius_room = 20         // 室温
  
  // 转换为华氏度
  let fahrenheit_freezing = add(multiply(celsius_freezing, 2), 30)
  let fahrenheit_boiling = add(multiply(celsius_boiling, 2), 30)
  let fahrenheit_room = add(multiply(celsius_room, 2), 30)
  
  assert_eq(30, fahrenheit_freezing)    // 0°C ≈ 32°F，近似为30
  assert_eq(230, fahrenheit_boiling)    // 100°C ≈ 212°F，近似为230
  assert_eq(70, fahrenheit_room)        // 20°C ≈ 68°F，近似为70
}

test "compound_interest_calculation" {
  // 复利计算：A = P × (1 + r × t)
  // 其中P是本金，r是年利率，t是年数
  
  let principal = 10000    // 本金
  let rate_percent = 5     // 5%年利率
  let years = 3            // 3年
  
  // 计算总利息：本金 × 利率 × 年数
  let total_interest = multiply(principal, multiply(rate_percent, years))
  
  // 计算本息和：本金 + 总利息
  let total_amount = add(principal, total_interest)
  
  assert_eq(160000, total_amount)  // 10000 + 10000×5×3 = 160000
}

test "string_processing_with_various_inputs" {
  // 测试各种字符串输入的处理
  
  // 空字符串
  assert_eq_string("Hello, !", greet(""))
  
  // 单字符
  assert_eq_string("Hello, A!", greet("A"))
  
  // 数字字符串
  assert_eq_string("Hello, 12345!", greet("12345"))
  
  // 混合字符
  assert_eq_string("Hello, User123!", greet("User123"))
  
  // 特殊字符
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  
  // 中文字符
  assert_eq_string("Hello, 用户!", greet("用户"))
}

test "geometric_area_calculations" {
  // 几何面积计算测试
  
  // 矩形面积：长 × 宽
  let length = 12
  let width = 8
  let rectangle_area = multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = multiply(base, height) / 2
  assert_eq(30, triangle_area)
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 5
  let bottom_base = 9
  let trapezoid_height = 4
  let trapezoid_area = multiply(add(top_base, bottom_base), trapezoid_height) / 2
  assert_eq(28, trapezoid_area)
}

test "boundary_value_comprehensive_test" {
  // 综合边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 加法边界测试
  assert_eq(max_val, add(max_val, zero))
  assert_eq(min_val, add(min_val, zero))
  assert_eq(max_val, add(max_val, one))
  assert_eq(min_val, add(min_val, neg_one))
  assert_eq(-1, add(max_val, min_val))
  
  // 乘法边界测试
  assert_eq(zero, multiply(max_val, zero))
  assert_eq(zero, multiply(min_val, zero))
  assert_eq(max_val, multiply(max_val, one))
  assert_eq(min_val, multiply(min_val, one))
  assert_eq(min_val, multiply(min_val, neg_one))
  
  // 复合边界运算
  let complex_result = add(multiply(add(max_val, min_val), one), max_val)
  assert_eq(max_val, complex_result)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let a = 15
  let b = 7
  let c = 3
  
  // 加法交换律：a + b = b + a
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律：a × b = b × a
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律：(a + b) + c = a + (b + c)
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律：(a × b) × c = a × (b × c)
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律：a × (b + c) = a × b + a × c
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "shopping_cart_with_discount_calculation" {
  // 带折扣的购物车计算
  
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let item3_price = 99
  let item3_quantity = 3
  
  // 计算各项总价
  let total1 = multiply(item1_price, item1_quantity)
  let total2 = multiply(item2_price, item2_quantity)
  let total3 = multiply(item3_price, item3_quantity)
  
  // 计算小计
  let subtotal = add(add(total1, total2), total3)
  
  // 计算折扣（满1000减100）
  let discount = if (subtotal >= 1000) { 100 } else { 0 }
  
  // 计算运费（满500免运费）
  let shipping = if (add(subtotal, -discount) >= 500) { 0 } else { 20 }
  
  // 计算最终总价
  let final_total = add(add(subtotal, -discount), shipping)
  
  assert_eq(1094, final_total)  // 598 + 199 + 297 - 100 + 0 = 1094
}

test "time_and_date_calculations" {
  // 时间和日期计算测试
  
  // 时间转换：2小时30分钟 = 总分钟数
  let hours = 2
  let minutes = 30
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
  
  // 时间差计算：从8:00到17:30
  let start_hour = 8
  let start_minute = 0
  let end_hour = 17
  let end_minute = 30
  
  let start_total = add(multiply(start_hour, 60), start_minute)
  let end_total = add(multiply(end_hour, 60), end_minute)
  let duration = add(end_total, -start_total)
  
  assert_eq(570, duration)  // 9小时30分钟 = 570分钟
  
  // 天数转换：3周2天 = 总天数
  let weeks = 3
  let extra_days = 2
  let total_days = add(multiply(weeks, 7), extra_days)
  assert_eq(23, total_days)
}

test "assertion_functions_comprehensive_test" {
  // 断言函数综合测试
  
  // 基本断言测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  // 数值比较断言
  assert_true(10 > 5)
  assert_true(10 >= 10)
  assert_false(5 > 10)
  assert_false(10 <= 5)
  
  // 字符串比较断言
  let str1 = "Hello"
  let str2 = "World"
  let combined = str1 + ", " + str2 + "!"
  assert_eq_string("Hello, World!", combined)
  
  // 函数结果断言
  let add_result = add(15, 25)
  let multiply_result = multiply(6, 7)
  let greet_result = greet("MoonBit")
  
  assert_eq(40, add_result)
  assert_eq(42, multiply_result)
  assert_eq_string("Hello, MoonBit!", greet_result)
  
  // 复合运算断言
  let complex_result = multiply(add(5, 3), add(10, 2))
  assert_eq(96, complex_result)  // (5+3)×(10+2) = 8×12 = 96
}