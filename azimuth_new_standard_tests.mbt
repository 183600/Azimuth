// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖不同功能场景

test "add_function_edge_cases" {
  // 测试 add 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值加法
  assert_eq(100, azimuth.add(100, 0))
  assert_eq(100, azimuth.add(0, 100))
  assert_eq(0, azimuth.add(0, 0))
  
  // 测试最大值边界
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(max_val, azimuth.add(max_val, max_val))
  
  // 测试最小值边界
  assert_eq(min_val, azimuth.add(min_val, -1))
  assert_eq(min_val, azimuth.add(min_val, min_val))
  
  // 测试最大值与最小值相加
  assert_eq(-1, azimuth.add(max_val, min_val))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值乘法
  assert_eq(0, azimuth.multiply(100, 0))
  assert_eq(0, azimuth.multiply(0, 100))
  assert_eq(0, azimuth.multiply(0, 0))
  
  // 测试单位值乘法
  assert_eq(100, azimuth.multiply(100, 1))
  assert_eq(100, azimuth.multiply(1, 100))
  assert_eq(-100, azimuth.multiply(-100, 1))
  assert_eq(-100, azimuth.multiply(1, -100))
  
  // 测试负一乘法
  assert_eq(-100, azimuth.multiply(100, -1))
  assert_eq(-100, azimuth.multiply(-1, 100))
  assert_eq(100, azimuth.multiply(-100, -1))
  
  // 测试最小值与负一相乘的特殊情况
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  
  // 测试溢出情况
  assert_eq(max_val, azimuth.multiply(46341, 46341))
  assert_eq(min_val, azimuth.multiply(-46341, 46341))
}

test "greet_function_variations" {
  // 测试 greet 函数的各种情况
  // 测试普通字符串
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  
  // 测试空字符串
  assert_eq_string("Hello, !", azimuth.greet(""))
  
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", azimuth.greet("John Doe"))
  assert_eq_string("Hello,  Leading Space!", azimuth.greet(" Leading Space"))
  assert_eq_string("Hello, Trailing Space !", azimuth.greet("Trailing Space "))
  
  // 测试包含特殊字符的字符串
  assert_eq_string("Hello, test@example.com!", azimuth.greet("test@example.com"))
  assert_eq_string("Hello, user_123!", azimuth.greet("user_123"))
  
  // 测试包含Unicode字符的字符串
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算矩形周长
  let length = 10
  let width = 5
  let perimeter = azimuth.add(azimuth.multiply(2, length), azimuth.multiply(2, width))
  assert_eq(30, perimeter)
  
  // 场景2：计算矩形面积
  let area = azimuth.multiply(length, width)
  assert_eq(50, area)
  
  // 场景3：计算平均值
  let num1 = 80
  let num2 = 90
  let num3 = 100
  let sum = azimuth.add(azimuth.add(num1, num2), num3)
  let average = sum / 3
  assert_eq(90, average)
  
  // 场景4：计算折扣价格
  let original_price = 200
  let discount_rate = 15  // 15%
  let discount_amount = azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = azimuth.add(original_price, -discount_amount)
  assert_eq(170, discounted_price)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 测试加法交换律：a + b = b + a
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 测试加法结合律：(a + b) + c = a + (b + c)
  let left_assoc = azimuth.add(azimuth.add(a, b), c)
  let right_assoc = azimuth.add(a, azimuth.add(b, c))
  assert_eq(left_assoc, right_assoc)
  
  // 测试乘法交换律：a * b = b * a
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 测试乘法结合律：(a * b) * c = a * (b * c)
  let left_mult_assoc = azimuth.multiply(azimuth.multiply(a, b), c)
  let right_mult_assoc = azimuth.multiply(a, azimuth.multiply(b, c))
  assert_eq(left_mult_assoc, right_mult_assoc)
  
  // 测试分配律：a * (b + c) = a * b + a * c
  let distrib_left = azimuth.multiply(a, azimuth.add(b, c))
  let distrib_right = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  assert_eq(distrib_left, distrib_right)
}

test "real_world_application_calculations" {
  // 测试实际应用场景的计算
  // 场景1：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let item1_total = azimuth.multiply(item1_price, item1_quantity)
  let item2_total = azimuth.multiply(item2_price, item2_quantity)
  let subtotal = azimuth.add(item1_total, item2_total)
  
  // 计算税费
  let tax_amount = azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = azimuth.add(azimuth.add(subtotal, tax_amount), shipping_fee)
  
  // 验证结果：199*2 + 299*1 = 697，税费 = 697*8/100 = 55，总价 = 697 + 55 + 20 = 772
  assert_eq(772, total)
  
  // 场景2：温度转换（简化版：F = C × 2 + 30）
  let celsius_temp = 25
  let fahrenheit_temp = azimuth.add(azimuth.multiply(celsius_temp, 2), 30)
  assert_eq(80, fahrenheit_temp)
  
  // 场景3：贷款利息计算（简化版）
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  let simple_interest = azimuth.multiply(principal, azimuth.multiply(annual_rate, years))
  let total_repayment = azimuth.add(principal, simple_interest)
  assert_eq(160000, total_repayment)  // 10000 + 10000*5*3 = 160000
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  // 测试整除情况
  assert_eq(5, azimuth.divide_with_ceil(10, 2))
  assert_eq(3, azimuth.divide_with_ceil(9, 3))
  assert_eq(7, azimuth.divide_with_ceil(14, 2))
  
  // 测试有余数情况
  assert_eq(6, azimuth.divide_with_ceil(11, 2))  // 11/2 = 5.5 → 6
  assert_eq(4, azimuth.divide_with_ceil(10, 3))  // 10/3 ≈ 3.33 → 4
  assert_eq(3, azimuth.divide_with_ceil(8, 3))   // 8/3 ≈ 2.67 → 3
  
  // 测试负数情况
  assert_eq(-5, azimuth.divide_with_ceil(-10, 2))
  assert_eq(-5, azimuth.divide_with_ceil(-11, 2))  // -11/2 = -5.5 → -5（向上取整）
  
  // 测试边界情况
  assert_eq(0, azimuth.divide_with_ceil(0, 5))
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(1, azimuth.divide_with_ceil(2, 2))
}

test "assertion_functions" {
  // 测试断言函数
  // 测试 assert_true
  azimuth.assert_true(true)
  azimuth.assert_true(5 > 3)
  azimuth.assert_true(azimuth.add(2, 3) == 5)
  
  // 测试 assert_false
  azimuth.assert_false(false)
  azimuth.assert_false(5 < 3)
  azimuth.assert_false(azimuth.add(2, 3) == 6)
  
  // 测试 assert_eq
  azimuth.assert_eq(5, azimuth.add(2, 3))
  azimuth.assert_eq(6, azimuth.multiply(2, 3))
  azimuth.assert_eq(0, azimuth.multiply(5, 0))
  
  // 测试 assert_eq_string
  azimuth.assert_eq_string("Hello, World!", azimuth.greet("World"))
  azimuth.assert_eq_string("Hello, !", azimuth.greet(""))
  azimuth.assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
}

test "performance_and_optimization" {
  // 测试性能和优化相关的计算
  // 测试大量数据的累加
  let base = 100
  let count = 100
  
  // 使用公式：n × (n + 1) / 2 计算 1+2+...+n 的和
  let sum_formula = azimuth.multiply(count, azimuth.add(count, 1)) / 2
  let scaled_sum = azimuth.multiply(base, sum_formula)
  
  // 验证前几项的计算
  let sum_first_10 = azimuth.multiply(10, 11) / 2
  let scaled_sum_10 = azimuth.multiply(base, sum_first_10)
  assert_eq(5500, scaled_sum_10)  // 100 × (10 × 11 / 2) = 5500
  
  // 测试指数增长（2的幂）
  let power_of_2_8 = azimuth.multiply(
    azimuth.multiply(azimuth.multiply(2, 2), azimuth.multiply(2, 2)),
    azimuth.multiply(azimuth.multiply(2, 2), azimuth.multiply(2, 2))
  )
  assert_eq(256, power_of_2_8)
  
  // 测试阶乘计算（5!）
  let factorial_5 = azimuth.multiply(5, azimuth.multiply(4, azimuth.multiply(3, azimuth.multiply(2, 1))))
  assert_eq(120, factorial_5)
}

test "error_handling_and_boundary_conditions" {
  // 测试错误处理和边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = azimuth.add(max_val, max_val)
  assert_eq(max_val, overflow_add)
  
  let underflow_add = azimuth.add(min_val, min_val)
  assert_eq(min_val, underflow_add)
  
  let overflow_mul = azimuth.multiply(max_val, 2)
  assert_eq(max_val, overflow_mul)
  
  let underflow_mul = azimuth.multiply(min_val, 2)
  assert_eq(min_val, underflow_mul)
  
  // 测试特殊值的运算
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  assert_eq(min_val, azimuth.multiply(-1, min_val))
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
  
  // 测试除以零的保护（通过 divide_with_ceil 函数的调用）
  // 注意：这里我们不直接测试除以零，因为会导致运行时错误
  // 而是测试正常情况下的行为
  assert_eq(10, azimuth.divide_with_ceil(100, 10))
  assert_eq(11, azimuth.divide_with_ceil(101, 10))
}