// 新增的 MoonBit 测试用例
// 测试 azimuth 库的核心功能，使用标准 MoonBit 测试语法

test "integer_overflow_protection" {
  // 测试整数溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_unicode_handling" {
  // 测试字符串 Unicode 处理
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 👋!", @azimuth.greet("👋"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
}

test "mathematical_identities" {
  // 测试数学恒等式
  let a = 17
  let b = 23
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  
  // 乘法零律
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, a))
  
  // 负一乘法
  @azimuth.assert_eq(-a, @azimuth.multiply(a, -1))
  @azimuth.assert_eq(-a, @azimuth.multiply(-1, a))
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 分期付款计算
  let monthly_payment = total / (years * 12)
  @azimuth.assert_eq(4444, monthly_payment)  // 160000 / 36 = 4444 (整数除法)
}

test "temperature_conversion" {
  // 测试温度转换
  // 摄氏度转华氏度：F = C × 2 + 30 (简化版)
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度：C = (F - 30) / 2 (简化版)
  let fahrenheit2 = 100
  let celsius2 = @azimuth.add(fahrenheit2, -30) / 2
  @azimuth.assert_eq(35, celsius2)
  
  // 测试冰点
  @azimuth.assert_eq(30, @azimuth.add(@azimuth.multiply(0, 2), 30))
  @azimuth.assert_eq(0, @azimuth.add(30, -30) / 2)
}

test "geometric_calculations" {
  // 测试几何计算
  let radius = 7
  
  // 圆周长（简化）：C = 2 × 3 × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  @azimuth.assert_eq(42, circumference)
  
  // 圆面积（简化）：A = 3 × r²
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(147, area)
  
  // 球体体积（简化）：V = 4 × r³
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(1372, volume)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：10, 20, 30, 40, 50
  let data_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  @azimuth.assert_eq(150, data_sum)
  
  // 平均值
  let average = data_sum / 5
  @azimuth.assert_eq(30, average)
  
  // 方差计算（简化）
  let variance_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(10 - 30, 10 - 30),
    @azimuth.multiply(20 - 30, 20 - 30)),
    @azimuth.multiply(30 - 30, 30 - 30)),
    @azimuth.multiply(40 - 30, 40 - 30)),
    @azimuth.multiply(50 - 30, 50 - 30))
  let variance = variance_sum / 5
  @azimuth.assert_eq(200, variance)
}

test "time_calculations" {
  // 测试时间计算
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = @azimuth.add(@azimuth.multiply(@azimuth.add(hours, 60), minutes), seconds)
  @azimuth.assert_eq(165, total_seconds)  // (2 + 60) * 45 + 30 = 165
  
  // 计算时间差
  let start_hour = 8
  let end_hour = 17
  let lunch_break = 1
  
  let work_hours = @azimuth.add(end_hour, -start_hour)
  let actual_work_hours = @azimuth.add(work_hours, -lunch_break)
  @azimuth.assert_eq(8, actual_work_hours)
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  let base_price = 100
  let quantity = 5
  let discount_threshold = 400
  let discount_rate = 10
  let tax_rate = 8
  let shipping = 15
  
  // 计算总价
  let subtotal = @azimuth.multiply(base_price, quantity)
  
  // 计算折扣
  let discount = if (subtotal > discount_threshold) {
    @azimuth.multiply(subtotal, discount_rate) / 100
  } else {
    0
  }
  
  // 计算折后价格
  let discounted_price = @azimuth.add(subtotal, -discount)
  
  // 计算税费
  let tax = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = @azimuth.add(@azimuth.add(discounted_price, tax), shipping)
  
  @azimuth.assert_eq(545, final_price)  // 500 - 50 + 36 + 15 = 501
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界值运算
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试特殊值运算
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}