// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  let result1 = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result1)
  
  let result2 = @azimuth.add(-5, 5)
  @azimuth.assert_eq(0, result2)
  
  let result3 = @azimuth.add(0, 100)
  @azimuth.assert_eq(100, result3)
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  let result1 = @azimuth.multiply(6, 7)
  @azimuth.assert_eq(42, result1)
  
  let result2 = @azimuth.multiply(-8, 5)
  @azimuth.assert_eq(-40, result2)
  
  let result3 = @azimuth.multiply(0, 999)
  @azimuth.assert_eq(0, result3)
}

test "greet_functionality" {
  // 测试问候函数
  let result1 = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result1)
  
  let result2 = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result2)
  
  let result3 = @azimuth.greet("测试")
  @azimuth.assert_eq_string("Hello, 测试!", result3)
}

test "add_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  let result1 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result1)
  
  // 负数溢出
  let result2 = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result2)
  
  // 边界值相加
  let result3 = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, result3)
}

test "multiply_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 大数相乘溢出
  let result1 = @azimuth.multiply(46341, 46341)
  @azimuth.assert_eq(max_val, result1)
  
  // 最小值与-1相乘
  let result2 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, result2)
  
  // 边界值与1相乘
  let result3 = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, result3)
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算矩形周长：2 * (长 + 宽)
  let length = 15
  let width = 10
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(50, perimeter)
  
  // 计算简单利息：本金 * 利率 * 时间
  let principal = 1000
  let rate = 5
  let time = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  @azimuth.assert_eq(15000, interest)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  let c = 5
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "edge_cases" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与0运算
  @azimuth.assert_eq(@azimuth.add(max_val, 0), max_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 0), 0)
  
  // 最小值与0运算
  @azimuth.assert_eq(@azimuth.add(min_val, 0), min_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 0), 0)
  
  // 1和-1的特殊情况
  @azimuth.assert_eq(@azimuth.multiply(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 1), min_val)
}

test "string_operations" {
  // 测试字符串操作
  // 包含特殊字符的字符串
  let special_chars = @azimuth.greet("!@#$%^&*()")
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", special_chars)
  
  // 包含空格的字符串
  let spaced = @azimuth.greet("Hello World")
  @azimuth.assert_eq_string("Hello, Hello World!", spaced)
  
  // 包含数字的字符串
  let numeric = @azimuth.greet("12345")
  @azimuth.assert_eq_string("Hello, 12345!", numeric)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车总价计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
  
  // 温度转换（简化版）：摄氏度转华氏度 F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}