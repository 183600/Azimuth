// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含多样化测试场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-25, @azimuth.multiply(5, -5))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 8
  let b = 7
  let c = 3
  
  // 计算 (a + b) * c
  let sum_ab = @azimuth.add(a, b)  // 15
  let result = @azimuth.multiply(sum_ab, c)  // 45
  
  @azimuth.assert_eq(45, result)
  
  // 计算 a * b + a * c（分配律）
  let product_ab = @azimuth.multiply(a, b)  // 56
  let product_ac = @azimuth.multiply(a, c)  // 24
  let distributive_result = @azimuth.add(product_ab, product_ac)  // 80
  
  @azimuth.assert_eq(80, distributive_result)
}

test "sequence_calculations" {
  // 测试序列计算
  // 斐波那契数列: 1, 1, 2, 3, 5, 8
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
}

test "business_calculations" {
  // 测试商业计算
  let unit_price = 25
  let quantity = 8
  let discount_rate = 10
  let tax_rate = 5
  
  // 计算小计
  let subtotal = @azimuth.multiply(unit_price, quantity)  // 200
  
  // 计算折扣
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100  // 20
  let discounted_total = @azimuth.add(subtotal, -discount_amount)  // 180
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_total, tax_rate) / 100  // 9
  let final_total = @azimuth.add(discounted_total, tax_amount)  // 189
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(180, discounted_total)
  @azimuth.assert_eq(189, final_total)
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 10
  let width = 6
  let height = 4
  
  // 计算面积
  let area = @azimuth.multiply(length, width)  // 60
  
  // 计算周长
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))  // 32
  
  // 计算体积
  let volume = @azimuth.multiply(area, height)  // 240
  
  @azimuth.assert_eq(60, area)
  @azimuth.assert_eq(32, perimeter)
  @azimuth.assert_eq(240, volume)
}

test "temperature_conversion" {
  // 测试温度转换
  let celsius = 25
  
  // 摄氏度转华氏度: F = C * 9/5 + 32
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)  // 77
  
  // 摄氏度转开尔文: K = C + 273
  let kelvin = @azimuth.add(celsius, 273)  // 298
  
  @azimuth.assert_eq(77, fahrenheit)
  @azimuth.assert_eq(298, kelvin)
}

test "time_calculations" {
  // 测试时间计算
  let hours = 3
  let minutes = 45
  let seconds = 30
  
  // 小时转分钟
  let hours_to_minutes = @azimuth.multiply(hours, 60)  // 180
  
  // 分钟转秒
  let minutes_to_seconds = @azimuth.multiply(minutes, 60)  // 2700
  
  // 总秒数: (小时 * 60 + 分钟) * 60 + 秒数
  let total_minutes = @azimuth.add(hours_to_minutes, minutes)  // 225
  let total_seconds = @azimuth.multiply(total_minutes, 60)  // 13500
  let final_seconds = @azimuth.add(total_seconds, seconds)  // 13530
  
  @azimuth.assert_eq(180, hours_to_minutes)
  @azimuth.assert_eq(2700, minutes_to_seconds)
  @azimuth.assert_eq(13530, final_seconds)
}

test "financial_calculations" {
  // 测试金融计算
  let principal = 1000
  let annual_rate = 5
  let years = 3
  
  // 简单利息: 本金 * 利率 * 时间 / 100
  let simple_interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), years) / 100  // 150
  
  // 总金额
  let total_amount = @azimuth.add(principal, simple_interest)  // 1150
  
  @azimuth.assert_eq(150, simple_interest)
  @azimuth.assert_eq(1150, total_amount)
}

test "string_greeting_operations" {
  // 测试字符串问候操作
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 最大值加1（应限制在最大值）
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 1))
  
  // 最小值减1（应限制在最小值）
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, -1))
  
  // 最大值乘以0
  @azimuth.assert_eq(0, @azimuth.multiply(max_int, 0))
  
  // 最小值乘以1
  @azimuth.assert_eq(min_int, @azimuth.multiply(min_int, 1))
}