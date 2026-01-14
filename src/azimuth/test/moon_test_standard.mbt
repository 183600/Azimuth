// MoonBit 标准测试用例
// 测试 azimuth 模块的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法测试
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_processing_functions" {
  // 测试字符串处理功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), 
            @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算多项式值：2x² + 3x + 1，其中 x = 5
  let x = 5
  let x_squared = @azimuth.multiply(x, x)  // 25
  let term1 = @azimuth.multiply(2, x_squared)  // 50
  let term2 = @azimuth.multiply(3, x)  // 15
  let result = @azimuth.add(@azimuth.add(term1, term2), 1)  // 66
  
  @azimuth.assert_eq(66, result)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景：计算订单总价（商品价格 × 数量 + 运费）
  let unit_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(612, total)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试零值处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
}

test "performance_calculations" {
  // 测试性能相关计算
  // 计算等差数列求和：1 + 2 + ... + 100 = 100 × 101 / 2 = 5050
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  
  @azimuth.assert_eq(5050, sum)
  
  // 计算等比数列求和：2 + 4 + 8 + 16 = 30
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
}

test "data_conversion_scenarios" {
  // 测试数据转换场景
  // 模拟数字字符串的处理
  let number_str = "42"
  let greeting = @azimuth.greet(number_str)
  
  @azimuth.assert_eq_string("Hello, 42!", greeting)
  
  // 测试特殊字符处理
  let special_chars = "!@#$%^&*()"
  let special_greeting = @azimuth.greet(special_chars)
  
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", special_greeting)
}

test "comprehensive_application_test" {
  // 综合应用测试：计算贷款月供（简化版）
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率百分比
  let years = 30          // 贷款年限
  
  // 简化计算：月供 ≈ (本金 + 总利息) / (年数 × 12)
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years)) / 100
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = total_payment / @azimuth.multiply(years, 12)
  
  @azimuth.assert_eq(555, monthly_payment)  // 简化计算结果
}