// 用户新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法
// 测试 azimuth 模块的核心功能

test "add_function_comprehensive" {
  // 全面测试 add 函数
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 100))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -100))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
}

test "greet_function_unicode_support" {
  // 测试 greet 函数的 Unicode 支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 17
  let b = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -15
  let d = 8
  
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let base = 5
  
  // 计算 (base + 3) * 2 - 4
  let step1 = @azimuth.add(base, 3)
  let step2 = @azimuth.multiply(step1, 2)
  let result = @azimuth.add(step2, -4)
  
  @azimuth.assert_eq(12, result)
  
  // 计算 base * 3 + base * 2
  let term1 = @azimuth.multiply(base, 3)
  let term2 = @azimuth.multiply(base, 2)
  let sum = @azimuth.add(term1, term2)
  
  @azimuth.assert_eq(25, sum)
}

test "real_world_shopping_cart" {
  // 测试实际购物车计算
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

test "temperature_conversion_scenario" {
  // 测试温度转换场景
  // 摄氏度转华氏度：F = C * 2 + 30（简化版）
  let celsius_temps = [@azimuth.add(0, 0), @azimuth.add(10, 0), @azimuth.add(25, 0), @azimuth.add(100, 0)]
  
  // 转换每个温度
  let temp1_f = @azimuth.add(@azimuth.multiply(celsius_temps[0], 2), 30)
  let temp2_f = @azimuth.add(@azimuth.multiply(celsius_temps[1], 2), 30)
  let temp3_f = @azimuth.add(@azimuth.multiply(celsius_temps[2], 2), 30)
  let temp4_f = @azimuth.add(@azimuth.multiply(celsius_temps[3], 2), 30)
  
  @azimuth.assert_eq(30, temp1_f)   // 0°C -> 30°F
  @azimuth.assert_eq(50, temp2_f)   // 10°C -> 50°F
  @azimuth.assert_eq(80, temp3_f)   // 25°C -> 80°F
  @azimuth.assert_eq(230, temp4_f)  // 100°C -> 230°F
}

test "assertion_functions_validation" {
  // 测试断言函数的有效性
  let test_num = 42
  let test_str = "MoonBit测试"
  
  // 基本断言测试
  @azimuth.assert_eq(test_num, @azimuth.add(40, 2))
  @azimuth.assert_eq_string(test_str, "MoonBit测试")
  
  // 布尔断言测试
  @azimuth.assert_true(@azimuth.add(5, 3) > 7)
  @azimuth.assert_false(@azimuth.add(5, 3) < 7)
  
  // 复合条件测试
  @azimuth.assert_true(@azimuth.multiply(6, 7) == 42)
  @azimuth.assert_false(@azimuth.multiply(6, 7) != 42)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  
  // 最大值与其他值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 最小值与其他值的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
  
  // 零值组合
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, max_val))
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, min_val))
}

test "string_formatting_variations" {
  // 测试字符串格式变化
  // 测试不同类型的名称
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet("John"))
  @azimuth.assert_eq_string("Hello, Jane Doe!", @azimuth.greet("Jane Doe"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  
  // 测试包含特殊字符的名称
  @azimuth.assert_eq_string("Hello, O'Reilly!", @azimuth.greet("O'Reilly"))
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
  @azimuth.assert_eq_string("Hello, John & Jane!", @azimuth.greet("John & Jane"))
}