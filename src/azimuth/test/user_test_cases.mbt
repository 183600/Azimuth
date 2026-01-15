// 用户自定义的MoonBit测试用例
// 测试azimuth库的核心功能

test "user_add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-3, 3))
  assert_eq(-5, add(-2, -3))
}

test "user_add_edge_cases" {
  // 边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  assert_eq(max_val, add(max_val, 1))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(0, add(max_val, min_val))
}

test "user_multiply_basic_functionality" {
  // 基本乘法功能测试
  assert_eq(15, multiply(3, 5))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(0, multiply(5, 0))
}

test "user_multiply_edge_cases" {
  // 乘法边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(1, -1))
}

test "user_greet_functionality" {
  // 问候函数测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 用户!", greet("用户"))
  assert_eq_string("Hello, !", greet(""))
}

test "user_complex_calculations" {
  // 复杂计算测试：计算矩形的面积和周长
  let length = 8
  let width = 5
  
  // 面积 = 长 × 宽
  let area = multiply(length, width)
  assert_eq(40, area)
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(26, perimeter)
}

test "user_financial_calculation" {
  // 金融计算测试：简单利息计算
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  // 利息 = 本金 × 利率 × 时间
  let interest = multiply(principal, multiply(rate, time))
  assert_eq(150000, interest)
  
  // 总金额 = 本金 + 利息
  let total = add(principal, interest)
  assert_eq(160000, total)
}

test "user_temperature_conversion" {
  // 温度转换测试：摄氏度转华氏度（简化版）
  let celsius = 25
  
  // 华氏度 = 摄氏度 × 2 + 30（简化公式）
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
}

test "user_statistical_calculations" {
  // 统计计算测试：计算平均值
  let numbers = [85, 90, 78, 92, 88]
  
  // 总和 = 85 + 90 + 78 + 92 + 88
  let sum = add(add(add(add(85, 90), 78), 92), 88)
  assert_eq(433, sum)
  
  // 平均值 = 总和 / 数量
  let average = sum / 5
  assert_eq(86, average)  // 整数除法
}

test "user_string_processing" {
  // 字符串处理测试
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "张三"
  
  // 测试不同类型的名字
  assert_eq_string("Hello, Alice!", greet(name1))
  assert_eq_string("Hello, Bob!", greet(name2))
  assert_eq_string("Hello, 张三!", greet(name3))
  
  // 测试包含数字和符号的名字
  let complex_name = "User123@Test"
  assert_eq_string("Hello, User123@Test!", greet(complex_name))
}