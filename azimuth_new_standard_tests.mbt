// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.multiply(3, 4), 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "geometric_calculations" {
  // 测试几何计算
  // 正方形面积
  let side = 6
  let area = @azimuth.multiply(side, side)
  @azimuth.assert_eq(36, area)
  
  // 矩形周长
  let length = 8
  let width = 3
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(22, perimeter)
}

test "financial_simple_interest" {
  // 测试金融简单利息计算
  let principal = 5000
  let rate = 4  // 4%
  let years = 3
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), years) / 100
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(5600, total)
}

test "temperature_conversions" {
  // 测试温度转换
  // 摄氏度到华氏度
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)
  
  // 冰点测试
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 9) / 5, 32)
  @azimuth.assert_eq(32, freezing_fahrenheit)
}

test "shopping_cart_calculations" {
  // 测试购物车计算
  let item1_price = 150
  let item1_qty = 2
  let item2_price = 75
  let item2_qty = 3
  let tax_rate = 10  // 10%
  
  let subtotal = @azimuth.add(@azimuth.multiply(item1_price, item1_qty), @azimuth.multiply(item2_price, item2_qty))
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(subtotal, tax)
  
  @azimuth.assert_eq(660, total)
}

test "time_unit_conversions" {
  // 测试时间单位转换
  // 2小时 = 120分钟
  let hours_to_minutes = @azimuth.multiply(2, 60)
  @azimuth.assert_eq(120, hours_to_minutes)
  
  // 3天 = 72小时
  let days_to_hours = @azimuth.multiply(3, 24)
  @azimuth.assert_eq(72, days_to_hours)
  
  // 1周 = 10080分钟
  let week_to_minutes = @azimuth.multiply(@azimuth.multiply(7, 24), 60)
  @azimuth.assert_eq(10080, week_to_minutes)
}

test "sequence_calculations" {
  // 测试序列计算
  // 前5个自然数之和: 1+2+3+4+5 = 15
  let natural_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 3), 4), 5)
  @azimuth.assert_eq(15, natural_sum)
  
  // 前4个偶数之和: 2+4+6+8 = 20
  let even_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 6), 8)
  @azimuth.assert_eq(20, even_sum)
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 1000
  let min_val = -1000
  
  // 边界值加法
  @azimuth.assert_eq(1001, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(-1001, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(1000, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(-1000, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑
  // 员工薪资计算：基本工资 + 加班费 - 税收
  let base_salary = 3000
  let overtime_hours = 10
  let overtime_rate = 25  // 每小时25元
  let tax_rate = 15  // 15%
  
  let overtime_pay = @azimuth.multiply(overtime_hours, overtime_rate)
  let gross_salary = @azimuth.add(base_salary, overtime_pay)
  let tax_amount = @azimuth.multiply(gross_salary, tax_rate) / 100
  let net_salary = @azimuth.add(gross_salary, -tax_amount)
  
  @azimuth.assert_eq(2975, net_salary)
}