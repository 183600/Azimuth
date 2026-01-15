// 新增的 MoonBit 测试用例
// 测试 azimuth 模块的各种功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(-3, 8))
}

test "string_concatenation_basic" {
  // 测试基本字符串连接
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "overflow_protection_tests" {
  // 测试溢出保护功能
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -5
  let d = 9
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算圆的面积和周长（简化版）
  let radius = 5
  let pi_approx = 3  // π的近似值
  
  // 计算周长：2 × π × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi_approx), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 计算面积：π × r²
  let radius_squared = @azimuth.multiply(radius, radius)
  let area = @azimuth.multiply(pi_approx, radius_squared)
  @azimuth.assert_eq(75, area)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与0的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 边界值与1的运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
}

test "real_world_application_sales_tax" {
  // 测试实际应用：销售税计算
  let item_price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = @azimuth.add(subtotal, tax_amount)
  
  @azimuth.assert_eq(597, subtotal)
  @azimuth.assert_eq(47, tax_amount)
  @azimuth.assert_eq(644, total)
}

test "string_processing_with_unicode" {
  // 测试Unicode字符串处理
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 5
  let c = 3
  
  // 计算左边：a × (b + c)
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  
  // 计算右边：a × b + a × c
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试其他数值组合
  let x = 12
  let y = 8
  let z = 4
  
  let left = @azimuth.multiply(x, @azimuth.add(y, z))
  let right = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left, right)
}

test "sequential_operations_test" {
  // 测试连续运算
  // 计算：(a + b) × (c + d) - e
  let a = 10
  let b = 15
  let c = 5
  let d = 8
  let e = 20
  
  // 步骤计算
  let sum1 = @azimuth.add(a, b)  // 25
  let sum2 = @azimuth.add(c, d)  // 13
  let product = @azimuth.multiply(sum1, sum2)  // 325
  let result = @azimuth.add(product, -e)  // 305
  
  @azimuth.assert_eq(305, result)
  
  // 测试另一个场景：计算平均值的总和
  let avg1 = @azimuth.add(@azimuth.add(10, 20), 30) / 3  // 20
  let avg2 = @azimuth.add(@azimuth.add(40, 50), 60) / 3  // 50
  let avg_sum = @azimuth.add(avg1, avg2)  // 70
  
  @azimuth.assert_eq(70, avg_sum)
}