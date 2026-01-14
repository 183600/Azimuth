// 标准 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，测试项目的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(8, -3))
  @azimuth.assert_eq(36, @azimuth.multiply(-6, -6))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试零值
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "string_processing_tests" {
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, a@b.com!", @azimuth.greet("a@b.com"))
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
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, c)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  )
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 圆面积计算（简化）：π × r² ≈ 3 × r²
  let radius = 5
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
  
  // 简单贷款计算：本金 × (1 + 利率 × 年数)
  let principal = 5000
  let rate = 6  // 6%
  let years = 2
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, years)))
  @azimuth.assert_eq(65000, total)
  
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 计算多项式值：ax³ + bx² + cx + d
  let x = 3
  let a = 2
  let b = 5
  let c = 7
  let d = 11
  
  let x_squared = @azimuth.multiply(x, x)  // 9
  let x_cubed = @azimuth.multiply(x_squared, x)  // 27
  
  let ax_cubed = @azimuth.multiply(a, x_cubed)  // 54
  let bx_squared = @azimuth.multiply(b, x_squared)  // 45
  let cx = @azimuth.multiply(c, x)  // 21
  
  let result = @azimuth.add(@azimuth.add(@azimuth.add(ax_cubed, bx_squared), cx), d)
  @azimuth.assert_eq(131, result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
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
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
}

test "performance_related_operations" {
  // 测试性能相关操作
  // 计算等差数列和：1 + 2 + ... + n = n × (n + 1) / 2
  let n = 100
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算等比数列和：2 + 4 + 8 + 16 = 30
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
  
  // 计算阶乘：5! = 120
  let factorial = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial)
}

test "business_application_scenarios" {
  // 测试业务应用场景
  // 订单计算
  let unit_price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  let shipping = 15
  
  let subtotal = @azimuth.multiply(unit_price, quantity)  // 597
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100  // 47
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)  // 659
  
  @azimuth.assert_eq(659, total)
  
  // 折扣计算
  let original_price = 1000
  let discount_percentage = 20  // 20%
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100  // 200
  let discounted_price = @azimuth.add(original_price, -discount_amount)  // 800
  
  @azimuth.assert_eq(800, discounted_price)
}