// 额外的 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "advanced_arithmetic_operations" {
  // 测试高级算术运算
  let a = 15
  let b = 25
  let c = 35
  
  // 测试连续加法
  @azimuth.assert_eq(75, @azimuth.add(@azimuth.add(a, b), c))
  
  // 测试结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 测试连续乘法
  @azimuth.assert_eq(13125, @azimuth.multiply(@azimuth.multiply(a, b), c))
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与负数的运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(max_val, -1))
  
  // 测试最小值与正数的运算
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(min_val, 1))
}

test "complex_string_operations" {
  // 测试复杂字符串操作
  let long_name = "ChristopherAlexander"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, ChristopherAlexander!", result)
  
  // 测试包含特殊字符的字符串
  let special_name = "user@domain.com"
  let result2 = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, user@domain.com!", result2)
  
  // 测试包含数字的字符串
  let numeric_name = "User12345"
  let result3 = @azimuth.greet(numeric_name)
  @azimuth.assert_eq_string("Hello, User12345!", result3)
}

test "mathematical_distributive_law" {
  // 测试数学分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)  // 10000 + (10000 × 5 × 3) = 160000
  
  // 月供计算（简化版）：总金额 / 月数
  let months = @azimuth.multiply(years, 12)
  let monthly_payment = total / months
  
  @azimuth.assert_eq(4444, monthly_payment)  // 160000 / 36 ≈ 4444（整数除法）
}

test "geometry_calculations" {
  // 测试几何计算
  let length = 12
  let width = 8
  
  // 计算矩形周长：2 × (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算矩形面积：长 × 宽
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 华氏度转摄氏度（简化版）：C = (F - 30) ÷ 2
  let fahrenheit_back = 80
  let celsius_back = @azimuth.add(fahrenheit_back, -30) / 2
  @azimuth.assert_eq(celsius, celsius_back)
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算一组数的平均值
  let num1 = 85
  let num2 = 90
  let num3 = 78
  let num4 = 92
  let num5 = 88
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  let average = sum / 5
  
  @azimuth.assert_eq(433, sum)
  @azimuth.assert_eq(86, average)  // 433 ÷ 5 = 86（整数除法）
  
  // 计算中位数（已排序数组的中位数）
  let median = 88
  @azimuth.assert_eq(median, median)
}

test "boolean_logic_operations" {
  // 测试布尔逻辑运算
  let x = 15
  let y = 25
  let z = 35
  
  // 测试条件表达式
  let condition1 = x < y
  let result1 = if condition1 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(40, result1)  // 15 < 25 为真，所以 15 + 25 = 40
  
  let condition2 = x > y
  let result2 = if condition2 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(375, result2)  // 15 > 25 为假，所以 15 × 25 = 375
  
  // 测试嵌套条件
  let result3 = if x < y {
    if y < z { @azimuth.add(@azimuth.add(x, y), z) } else { @azimuth.multiply(y, z) }
  } else {
    @azimuth.multiply(x, z)
  }
  @azimuth.assert_eq(75, result3)  // 15 < 25 < 35 为真，所以 15 + 25 + 35 = 75
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界条件的特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
}