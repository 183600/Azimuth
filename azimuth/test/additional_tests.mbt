// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "string_length_operations" {
  // 测试字符串长度相关的操作
  let short_string = "Hi"
  let medium_string = "MoonBit"
  let long_string = "This is a very long string for testing purposes"
  
  // 通过 greet 函数测试不同长度字符串的处理
  assert_eq_string("Hello, Hi!", greet(short_string))
  assert_eq_string("Hello, MoonBit!", greet(medium_string))
  assert_eq_string("Hello, This is a very long string for testing purposes!", greet(long_string))
}

test "mathematical_progressions" {
  // 测试数学数列计算
  // 等差数列：2, 5, 8, 11, 14 (公差为3)
  let a1 = 2
  let d = 3
  let n = 5
  
  // 计算第n项：a1 + (n-1)*d
  let an = add(a1, multiply(n - 1, d))
  assert_eq(14, an)
  
  // 计算前n项和：n*(a1 + an)/2
  let sum = multiply(n, add(a1, an)) / 2
  assert_eq(40, sum)
}

test "business_calculations" {
  // 测试商业计算场景
  let unit_price = 150
  let quantity = 8
  let discount_rate = 5  // 5%折扣
  let tax_rate = 10      // 10%税率
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  assert_eq(1200, subtotal)
  
  // 计算折扣金额
  let discount_amount = multiply(subtotal, discount_rate) / 100
  assert_eq(60, discount_amount)
  
  // 计算折后金额
  let discounted_total = add(subtotal, -discount_amount)
  assert_eq(1140, discounted_total)
  
  // 计算税费
  let tax_amount = multiply(discounted_total, tax_rate) / 100
  assert_eq(114, tax_amount)
  
  // 计算最终总额
  let final_total = add(discounted_total, tax_amount)
  assert_eq(1254, final_total)
}

test "temperature_conversions" {
  // 测试温度转换计算
  // 摄氏度转华氏度：F = C * 9/5 + 32 (简化为 F = C * 2 + 30)
  let celsius_freezing = 0
  let celsius_boiling = 100
  let celsius_room = 20
  
  let fahrenheit_freezing = add(multiply(celsius_freezing, 2), 30)
  let fahrenheit_boiling = add(multiply(celsius_boiling, 2), 30)
  let fahrenheit_room = add(multiply(celsius_room, 2), 30)
  
  assert_eq(30, fahrenheit_freezing)   // 0°C ≈ 30°F (简化)
  assert_eq(230, fahrenheit_boiling)   // 100°C ≈ 230°F (简化)
  assert_eq(70, fahrenheit_room)       // 20°C ≈ 70°F (简化)
}

test "geometric_calculations" {
  // 测试几何计算
  // 计算矩形面积和周长
  let length = 12
  let width = 8
  
  let area = multiply(length, width)
  let perimeter = add(multiply(2, length), multiply(2, width))
  
  assert_eq(96, area)
  assert_eq(40, perimeter)
  
  // 计算三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = multiply(base, height) / 2
  
  assert_eq(30, triangle_area)
}

test "number_system_conversions" {
  // 测试数字系统转换（简化版）
  // 二进制转十进制：1010 = 1×2³ + 0×2² + 1×2¹ + 0×2⁰ = 8 + 0 + 2 + 0 = 10
  let binary_1010 = add(add(multiply(1, 8), multiply(0, 4)), add(multiply(1, 2), multiply(0, 1)))
  assert_eq(10, binary_1010)
  
  // 八进制转十进制：123 = 1×8² + 2×8¹ + 3×8⁰ = 64 + 16 + 3 = 83
  let octal_123 = add(add(multiply(1, 64), multiply(2, 8)), multiply(3, 1))
  assert_eq(83, octal_123)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：15, 25, 35, 45, 55
  let data1 = 15
  let data2 = 25
  let data3 = 35
  let data4 = 45
  let data5 = 55
  
  // 计算总和
  let total = add(add(add(add(data1, data2), data3), data4), data5)
  assert_eq(175, total)
  
  // 计算平均值
  let average = total / 5
  assert_eq(35, average)
  
  // 计算中位数（已排序数据的中间值）
  let median = data3
  assert_eq(35, median)
  
  // 计算范围（最大值-最小值）
  let range = add(data5, -data1)
  assert_eq(40, range)
}

test "time_calculations" {
  // 测试时间计算
  // 计算总秒数
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  let total_seconds = add(add(multiply(hours, 3600), multiply(minutes, 60)), seconds)
  assert_eq(9930, total_seconds)
  
  // 计算时间差
  let start_hour = 9
  let start_minute = 30
  let end_hour = 14
  let end_minute = 45
  
  let start_total = add(multiply(start_hour, 60), start_minute)
  let end_total = add(multiply(end_hour, 60), end_minute)
  let duration = add(end_total, -start_total)
  
  assert_eq(315, duration)  // 5小时15分钟 = 315分钟
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算 (a + b) * (c - d) + e * f
  let a = 12
  let b = 8
  let c = 20
  let d = 5
  let e = 3
  let f = 7
  
  let sum_ab = add(a, b)
  let diff_cd = add(c, -d)
  let product_ef = multiply(e, f)
  let result = add(multiply(sum_ab, diff_cd), product_ef)
  
  assert_eq(261, result)  // (12+8)*(20-5) + 3*7 = 20*15 + 21 = 300 + 21 = 321
}

test "string_processing_advanced" {
  // 测试高级字符串处理
  // 测试包含各种字符的字符串
  let alphanumeric = "User123"
  let email = "test@example.com"
  let unicode = "测试用户"
  let mixed = "Hello_世界123"
  
  assert_eq_string("Hello, User123!", greet(alphanumeric))
  assert_eq_string("Hello, test@example.com!", greet(email))
  assert_eq_string("Hello, 测试用户!", greet(unicode))
  assert_eq_string("Hello, Hello_世界123!", greet(mixed))
  
  // 测试特殊字符
  let with_quotes = "\"John\""
  let with_spaces = "  Mary Jane  "
  let with_symbols = "user@2023#1"
  
  assert_eq_string("Hello, \"John\"!", greet(with_quotes))
  assert_eq_string("Hello,   Mary Jane  !", greet(with_spaces))
  assert_eq_string("Hello, user@2023#1!", greet(with_symbols))
}