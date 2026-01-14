// MoonBit 测试用例示例
// 使用标准的 MoonBit 测试语法，测试 azimuth 包的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "overflow_protection" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
}

test "mathematical_formula_evaluation" {
  // 测试数学公式计算
  let a = 8
  let b = 12
  let c = 5
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(100, product)
  
  // 计算 a * b + c
  let mult = @azimuth.multiply(a, b)
  let result = @azimuth.add(mult, c)
  @azimuth.assert_eq(101, result)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 测试最小值与1的运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 测试特殊值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 圆的周长和面积计算（简化版）
  let radius = 7
  let pi_approx = 3
  
  // 周长 = 2 * π * r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi_approx), radius)
  @azimuth.assert_eq(42, circumference)
  
  // 面积 = π * r²
  let area = @azimuth.multiply(pi_approx, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(147, area)
}

test "real_world_application" {
  // 测试实际应用场景：订单总价计算
  let item_price = 199
  let quantity = 3
  let shipping_fee = 15
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping_fee)
  
  // 验证结果：(199*3) + (199*3*8/100) + 15 = 597 + 47 + 15 = 659
  @azimuth.assert_eq(659, total)
}

test "special_value_combinations" {
  // 测试特殊值组合
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 测试零的运算特性
  @azimuth.assert_eq(5, @azimuth.add(5, zero))
  @azimuth.assert_eq(0, @azimuth.multiply(5, zero))
  
  // 测试1的运算特性
  @azimuth.assert_eq(6, @azimuth.multiply(6, one))
  @azimuth.assert_eq(6, @azimuth.add(5, one))
  
  // 测试-1的运算特性
  @azimuth.assert_eq(-6, @azimuth.multiply(6, neg_one))
  @azimuth.assert_eq(4, @azimuth.add(5, neg_one))
}

test "assertion_function_verification" {
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试复合条件
  let x = 10
  let y = 20
  @azimuth.assert_true(@azimuth.add(x, y) == 30)
  @azimuth.assert_false(@azimuth.multiply(x, y) == 150)
}

test "performance_optimization_patterns" {
  // 测试性能优化模式
  let base = 100
  
  // 测试连续运算优化
  let result1 = @azimuth.add(
    @azimuth.add(base, base),
    @azimuth.add(base, base)
  )
  @azimuth.assert_eq(400, result1)
  
  // 测试嵌套运算
  let result2 = @azimuth.multiply(
    @azimuth.add(base, 50),
    @azimuth.add(base, -50)
  )
  @azimuth.assert_eq(7500, result2)
  
  // 测试复杂表达式：(a + b) * (c + d) - e * f
  let a = 10
  let b = 20
  let c = 5
  let d = 15
  let e = 8
  let f = 12
  
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let product1 = @azimuth.multiply(sum1, sum2)
  let product2 = @azimuth.multiply(e, f)
  let final_result = @azimuth.add(product1, -product2)
  
  @azimuth.assert_eq(504, final_result)
}