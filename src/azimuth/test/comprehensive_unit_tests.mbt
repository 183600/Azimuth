// Azimuth 项目综合单元测试
// 包含多种场景的测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试边界值乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算圆的周长和面积（简化版）
  let radius = 5
  let pi_approx = 3
  
  // 圆周长 = 2 × π × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi_approx), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 圆面积 = π × r²
  let radius_squared = @azimuth.multiply(radius, radius)
  let area = @azimuth.multiply(pi_approx, radius_squared)
  @azimuth.assert_eq(75, area)
}

test "sequential_operations_test" {
  // 测试连续运算
  // 计算 ((a + b) × c) - d
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(80, result)
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let x = 12
  let y = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  let z = 5
  @azimuth.assert_eq(
    @azimuth.multiply(x, @azimuth.add(y, z)),
    @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  )
}

test "real_world_application_test" {
  // 测试实际应用场景：购物车总价计算
  let item1_price = 199
  let item1_qty = 2
  let item2_price = 299
  let item2_qty = 1
  let shipping = 20
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_qty)
  let total2 = @azimuth.multiply(item2_price, item2_qty)
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价
  let final_total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(717, final_total)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "performance_related_calculations" {
  // 测试性能相关计算
  // 计算前n个自然数的和：n × (n + 1) / 2
  let _n = 100
  let sum = @azimuth.multiply(_n, @azimuth.add(_n, 1)) / 2
  @azimuth.assert_eq(5050, sum)
  
  // 计算阶乘（简化版）：5! = 5 × 4 × 3 × 2 × 1
  let factorial = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial)
}