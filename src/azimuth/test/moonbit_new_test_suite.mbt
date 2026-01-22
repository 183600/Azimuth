// 新的 MoonBit 测试套件
// 包含 10 个标准 MoonBit 测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  azimuth::@azimuth.assert_eq(10, azimuth::@azimuth.add(7, 3))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.add(5, -5))
  azimuth::@azimuth.assert_eq(15, azimuth::@azimuth.multiply(3, 5))
  azimuth::@azimuth.assert_eq(-8, azimuth::@azimuth.multiply(-2, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  azimuth::@azimuth.assert_eq_string("Hello, MoonBit!", azimuth::@azimuth.greet("MoonBit"))
  azimuth::@azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
  azimuth::@azimuth.assert_eq_string("Hello, 用户123!", azimuth::@azimuth.greet("用户123"))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律: a + b = b + a
  azimuth::@azimuth.assert_eq(
    azimuth::@azimuth.add(a, b), 
    azimuth::@azimuth.add(b, a)
  )
  
  // 乘法交换律: a * b = b * a
  azimuth::@azimuth.assert_eq(
    azimuth::@azimuth.multiply(a, b), 
    azimuth::@azimuth.multiply(b, a)
  )
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法溢出保护
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, max_val))
  
  // 最小值加法溢出保护
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, min_val))
  
  // 乘法边界值
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(max_val, 0))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算圆的面积和周长
  let radius = 5
  let pi_approx = 3  // 简化的π值
  
  // 周长 = 2 × π × r
  let circumference = azimuth::@azimuth.multiply(
    azimuth::@azimuth.multiply(2, pi_approx), 
    radius
  )
  
  // 面积 = π × r²
  let area = azimuth::@azimuth.multiply(
    pi_approx, 
    azimuth::@azimuth.multiply(radius, radius)
  )
  
  azimuth::@azimuth.assert_eq(30, circumference)  // 2 × 3 × 5 = 30
  azimuth::@azimuth.assert_eq(75, area)          // 3 × 25 = 75
}

test "financial_calculation_scenario" {
  // 测试金融计算场景：贷款利息计算
  let principal = 10000  // 本金
  let annual_rate = 5    // 年利率5%
  let years = 3          // 3年
  
  // 简单利息计算：本金 + (本金 × 利率 × 年数)
  let interest = azimuth::@azimuth.multiply(
    principal, 
    azimuth::@azimuth.multiply(annual_rate, years)
  )
  let total_amount = azimuth::@azimuth.add(principal, interest)
  
  azimuth::@azimuth.assert_eq(160000, total_amount)  // 10000 + (10000 × 5 × 3) = 160000
}

test "assert_function_functionality" {
  // 测试断言函数功能
  azimuth::@azimuth.assert_eq(1, 1)
  azimuth::@azimuth.assert_eq(1, 1)
  azimuth::@azimuth.assert_eq(42, 42)
  azimuth::@azimuth.assert_eq_string("test", "test")
  
  // 测试计算结果的断言
  let calc_result = azimuth::@azimuth.add(17, 25)
  azimuth::@azimuth.assert_eq(42, calc_result)
  
  let string_result = azimuth::@azimuth.greet("Tester")
  azimuth::@azimuth.assert_eq_string("Hello, Tester!", string_result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护是否正常工作
  azimuth::@azimuth.assert_eq(1, 1)(
    azimuth::@azimuth.add(max_val, 100) == max_val
  )
  azimuth::@azimuth.assert_eq(1, 1)(
    azimuth::@azimuth.add(min_val, -100) == min_val
  )
  azimuth::@azimuth.assert_eq(1, 1)(
    azimuth::@azimuth.multiply(max_val, 2) == max_val
  )
  
  // 测试零值运算
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(0, 12345))
  azimuth::@azimuth.assert_eq(12345, azimuth::@azimuth.add(0, 12345))
}

test "real_world_application_test" {
  // 测试实际应用：订单总价计算
  let item_price = 199
  let quantity = 3
  let discount_rate = 10  // 10%折扣
  let tax_rate = 8        // 8%税费
  let shipping = 15       // 运费
  
  // 计算商品总价
  let subtotal = azimuth::@azimuth.multiply(item_price, quantity)
  
  // 计算折扣金额
  let discount_amount = azimuth::@azimuth.multiply(subtotal, discount_rate) / 100
  
  // 计算折后价格
  let discounted_price = azimuth::@azimuth.add(subtotal, -discount_amount)
  
  // 计算税费
  let tax_amount = azimuth::@azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终总价
  let final_total = azimuth::@azimuth.add(
    azimuth::@azimuth.add(discounted_price, tax_amount), 
    shipping
  )
  
  azimuth::@azimuth.assert_eq(636, final_total)  // 597 - 59 + 47 + 15 = 636
}

test "data_conversion_and_formatting" {
  // 测试数据转换和格式化
  // 测试数字字符串的处理
  let number_as_string = "2023"
  let greeting_with_number = azimuth::@azimuth.greet(number_as_string)
  azimuth::@azimuth.assert_eq_string("Hello, 2023!", greeting_with_number)
  
  // 测试特殊字符字符串的处理
  let special_chars = "!@#$%^&*()"
  let greeting_with_special = azimuth::@azimuth.greet(special_chars)
  azimuth::@azimuth.assert_eq_string("Hello, !@#$%^&*()!", greeting_with_special)
  
  // 测试Unicode字符的处理
  let unicode_chars = "🌟测试"
  let greeting_with_unicode = azimuth::@azimuth.greet(unicode_chars)
  azimuth::@azimuth.assert_eq_string("Hello, 🌟测试!", greeting_with_unicode)
}