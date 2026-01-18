// Azimuth 项目新增标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "mathematical_operations_precision" {
  // 数学运算精度测试
  let a = 123456
  let b = 654321
  
  // 测试大数加法
  let sum_result = azimuth.add(a, b)
  @assertion.assert_eq(777777, sum_result)?
  
  // 测试大数乘法
  let product_result = azimuth.multiply(123, 456)
  @assertion.assert_eq(56088, product_result)?
  
  // 测试负数运算
  let negative_result = azimuth.add(-500, 300)
  @assertion.assert_eq(-200, negative_result)?
}

test "boundary_value_comprehensive" {
  // 边界值综合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @assertion.assert_eq(max_val, azimuth.add(max_val, 0))?
  @assertion.assert_eq(max_val, azimuth.add(max_val, -100))?
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 1))?
  
  // 测试最小值的各种运算
  @assertion.assert_eq(min_val, azimuth.add(min_val, 0))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, 100))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 1))?
  
  // 测试极值相加
  @assertion.assert_eq(-1, azimuth.add(max_val, min_val))?
}

test "string_processing_variations" {
  // 字符串处理变化测试
  let normal_string = "MoonBit"
  let empty_string = ""
  let space_string = "   "
  let special_chars = "!@#$%^&*()"
  let unicode_string = "测试🚀"
  
  // 测试各种字符串类型
  @assertion.assert_eq_string("Hello, MoonBit!", azimuth.greet(normal_string))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(empty_string))?
  @assertion.assert_eq_string("Hello,   !", azimuth.greet(space_string))?
  @assertion.assert_eq_string("Hello, !@#$%^&*()!", azimuth.greet(special_chars))?
  @assertion.assert_eq_string("Hello, 测试🚀!", azimuth.greet(unicode_string))?
}

test "complex_calculation_scenarios" {
  // 复杂计算场景测试
  // 场景1：计算矩形周长和面积
  let length = 15
  let width = 8
  
  let perimeter = azimuth.add(azimuth.multiply(2, length), azimuth.multiply(2, width))
  let area = azimuth.multiply(length, width)
  
  @assertion.assert_eq(46, perimeter)?  // 2*15 + 2*8 = 46
  @assertion.assert_eq(120, area)?      // 15*8 = 120
  
  // 场景2：计算折扣价格
  let original_price = 1000
  let discount_rate = 20  // 20%折扣
  let discounted_price = azimuth.multiply(original_price, azimuth.add(100, -discount_rate)) / 100
  
  @assertion.assert_eq(800, discounted_price)?  // 1000 * (100-20) / 100 = 800
}

test "sequential_operations_chain" {
  // 连续操作链测试
  let initial_value = 10
  
  // 连续加法：10 + 5 + 15 + 20 = 50
  let chain_add = azimuth.add(azimuth.add(azimuth.add(initial_value, 5), 15), 20)
  @assertion.assert_eq(50, chain_add)?
  
  // 连续乘法：2 * 3 * 4 * 5 = 120
  let chain_multiply = azimuth.multiply(azimuth.multiply(azimuth.multiply(2, 3), 4), 5)
  @assertion.assert_eq(120, chain_multiply)?
  
  // 混合操作：(10 + 5) * (8 - 3) = 15 * 5 = 75
  let mixed_operation = azimuth.multiply(azimuth.add(10, 5), azimuth.add(8, -3))
  @assertion.assert_eq(75, mixed_operation)?
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证溢出保护
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1000))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1000))?
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 10))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 10))?
  
  // 验证特殊值处理
  @assertion.assert_eq(0, azimuth.multiply(0, max_val))?
  @assertion.assert_eq(0, azimuth.multiply(0, min_val))?
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 1))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 1))?
}

test "real_world_application_simulations" {
  // 实际应用模拟测试
  // 场景1：银行利息计算
  let principal = 10000
  let interest_rate = 5  // 5%
  let years = 3
  
  let simple_interest = azimuth.multiply(principal, azimuth.multiply(interest_rate, years))
  let total_amount = azimuth.add(principal, simple_interest)
  
  @assertion.assert_eq(160000, total_amount)?  // 10000 + 10000*5*3 = 160000
  
  // 场景2：温度转换（摄氏度转华氏度：F = C * 9/5 + 32，简化为 F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  @assertion.assert_eq(80, fahrenheit)?  // 25*2 + 30 = 80
}

test "performance_and_stress" {
  // 性能和压力测试
  // 大数运算测试
  let large_number1 = 1000000
  let large_number2 = 2000000
  
  let large_sum = azimuth.add(large_number1, large_number2)
  let large_product = azimuth.multiply(1000, 1000)
  
  @assertion.assert_eq(3000000, large_sum)?
  @assertion.assert_eq(1000000, large_product)?
  
  // 重复运算测试
  let base = 5
  let result1 = azimuth.add(base, azimuth.add(base, azimuth.add(base, azimuth.add(base, base))))
  let result2 = azimuth.multiply(base, azimuth.multiply(base, azimuth.multiply(base, base)))
  
  @assertion.assert_eq(25, result1)?  // 5+5+5+5+5 = 25
  @assertion.assert_eq(625, result2)? // 5*5*5*5 = 625
}

test "data_type_edge_cases" {
  // 数据类型边界情况测试
  // 零值运算测试
  @assertion.assert_eq(0, azimuth.multiply(0, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, 12345))?
  @assertion.assert_eq(0, azimuth.multiply(12345, 0))?
  @assertion.assert_eq(12345, azimuth.add(0, 12345))?
  @assertion.assert_eq(12345, azimuth.add(12345, 0))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
  
  // 单位运算测试
  @assertion.assert_eq(5, azimuth.multiply(5, 1))?
  @assertion.assert_eq(5, azimuth.multiply(1, 5))?
  @assertion.assert_eq(-5, azimuth.multiply(5, -1))?
  @assertion.assert_eq(-5, azimuth.multiply(-1, 5))?
  
  // 相等数运算测试
  @assertion.assert_eq(10, azimuth.add(5, 5))?
  @assertion.assert_eq(25, azimuth.multiply(5, 5))?
  @assertion.assert_eq(0, azimuth.add(5, -5))?
  @assertion.assert_eq(-25, azimuth.multiply(-5, 5))?
}