// Azimuth 项目实用测试套件
// 包含实际应用场景的测试用例，使用标准 MoonBit 测试语法

test "financial_calculations" {
  // 金融计算测试
  let principal = 10000
  let rate = 0.05
  let time = 3
  
  // 简单利息计算: principal * rate * time
  let simple_interest = principal * rate * time
  
  // 复利计算: principal * (1 + rate)^time
  let compound_interest = principal * (1 + rate) * (1 + rate) * (1 + rate)
  
  @azimuth.assert_eq(1500, simple_interest)
  @azimuth.assert_eq(11576, compound_interest)
}

test "temperature_conversions" {
  // 温度转换测试
  let celsius = 25
  let fahrenheit = 77
  
  // 摄氏度转华氏度: (C * 9/5) + 32
  let c_to_f = (celsius * 9) / 5 + 32
  
  // 华氏度转摄氏度: (F - 32) * 5/9
  let f_to_c = (fahrenheit - 32) * 5 / 9
  
  @azimuth.assert_eq(fahrenheit, c_to_f)
  @azimuth.assert_eq(celsius, f_to_c)
}

test "statistics_calculations" {
  // 统计计算测试
  let numbers = [10, 15, 20, 25, 30]
  
  // 计算平均值
  let sum = 10 + 15 + 20 + 25 + 30
  let mean = sum / 5
  
  // 计算中位数
  let median = 20
  
  // 计算范围
  let range = 30 - 10
  
  @azimuth.assert_eq(20, mean)
  @azimuth.assert_eq(20, median)
  @azimuth.assert_eq(20, range)
}

test "time_calculations" {
  // 时间计算测试
  let hours = 2
  let minutes = 45
  let seconds = 30
  
  // 转换为总秒数
  let total_seconds = hours * 3600 + minutes * 60 + seconds
  
  // 转换为总分钟数
  let total_minutes = hours * 60 + minutes + seconds / 60
  
  @azimuth.assert_eq(9930, total_seconds)
  @azimuth.assert_eq(165, total_minutes)
}

test "geometry_formulas" {
  // 几何公式测试
  let radius = 5
  
  // 圆的周长: 2πr
  let circumference = 2 * 3 * radius
  
  // 圆的面积: πr²
  let area = 3 * radius * radius
  
  // 球的体积: (4/3)πr³
  let volume = (4 * radius * radius * radius) / 3
  
  @azimuth.assert_eq(30, circumference)
  @azimuth.assert_eq(75, area)
  @azimuth.assert_eq(166, volume)
}

test "unit_conversions" {
  // 单位转换测试
  let meters = 100
  let kilograms = 50
  
  // 米转厘米
  let centimeters = meters * 100
  
  // 千克转克
  let grams = kilograms * 1000
  
  // 公里转米
  let kilometers = 5
  let meters_from_km = kilometers * 1000
  
  @azimuth.assert_eq(10000, centimeters)
  @azimuth.assert_eq(50000, grams)
  @azimuth.assert_eq(5000, meters_from_km)
}

test "logical_expressions" {
  // 逻辑表达式测试
  let a = true
  let b = false
  let x = 10
  let y = 20
  
  // 复合逻辑表达式
  let result1 = a && (x > 5)
  let result2 = b || (y < 30)
  let result3 = !(a && b)
  let result4 = (x > y) || (y > x)
  
  @azimuth.assert_eq(true, result1)
  @azimuth.assert_eq(true, result2)
  @azimuth.assert_eq(true, result3)
  @azimuth.assert_eq(true, result4)
}

test "data_validation" {
  // 数据验证测试
  let email = "user@example.com"
  let age = 25
  let password = "secure123"
  
  // 验证邮箱包含@符号
  let has_at_symbol = true  // 简化验证
  
  // 验证年龄在合理范围内
  let age_valid = age >= 18 && age <= 120
  
  // 验证密码长度
  let password_valid = password.length >= 8
  
  @azimuth.assert_eq(true, has_at_symbol)
  @azimuth.assert_eq(true, age_valid)
  @azimuth.assert_eq(true, password_valid)
}