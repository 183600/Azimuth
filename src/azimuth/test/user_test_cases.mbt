// 用户自定义的MoonBit测试用例
// 测试azimuth库的核心功能

test "user_add_basic_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "user_add_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val))
}

test "user_multiply_basic_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "user_multiply_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
}

test "user_greet_functionality" {
  // 问候函数测试
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "user_complex_calculations" {
  // 复杂计算测试：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(26, perimeter)
}

test "user_financial_calculation" {
  // 金融计算测试：简单利息计算
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  // 利息 = 本金 × 利率 × 时间
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time))
  @azimuth.assert_eq(150000, interest)
  
  // 总金额 = 本金 + 利息
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)
}

test "user_temperature_conversion" {
  // 温度转换测试：摄氏度转华氏度（简化版）
  let celsius = 25
  
  // 华氏度 = 摄氏度 × 2 + 30（简化公式）
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "user_statistical_calculations" {
  // 统计计算测试：计算平均值
  let numbers = [85, 90, 78, 92, 88]
  
  // 总和 = 85 + 90 + 78 + 92 + 88
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(85, 90), 78), 92), 88)
  @azimuth.assert_eq(433, sum)
  
  // 平均值 = 总和 / 数量
  let average = sum / 5
  @azimuth.assert_eq(86, average)  // 整数除法
}

test "user_string_processing" {
  // 字符串处理测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "张三"
  
  // 测试不同类型的名字
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet(name1))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet(name2))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(name3))
  
  // 测试包含数字和符号的名字
  let complex_name = "User123@Test"
  @azimuth.assert_eq_string("Hello, User123@Test!", @azimuth.greet(complex_name))
}