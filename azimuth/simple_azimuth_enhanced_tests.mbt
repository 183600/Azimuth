// 简单的 Azimuth 增强测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_enhanced_boundary" {
  // 测试加法函数的增强边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -100))
  
  // 测试中等数值的加法
  @azimuth.assert_eq(1000, @azimuth.add(500, 500))
  @azimuth.assert_eq(0, @azimuth.add(-500, 500))
}

test "multiply_function_enhanced_precision" {
  // 测试乘法函数的增强精度
  let sqrt_max = 46340
  
  // 测试接近溢出边界的乘法
  @azimuth.assert_eq(2147395600, @azimuth.multiply(sqrt_max, sqrt_max))
  @azimuth.assert_eq(-2147395600, @azimuth.multiply(sqrt_max, -sqrt_max))
  
  // 测试特殊值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "divide_with_ceil_enhanced_fractional" {
  // 测试向上取整除法的增强小数处理
  // 测试正数分数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(5, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 3))
  
  // 测试负数分数
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
}

test "greet_function_enhanced_unicode" {
  // 测试问候函数的增强Unicode处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
}

test "complex_mathematical_operations" {
  // 测试复杂的数学运算组合
  // 嵌套运算测试
  @azimuth.assert_eq(25, @azimuth.add(@azimuth.multiply(3, 5), @azimuth.multiply(2, 5)))
  @azimuth.assert_eq(20, @azimuth.multiply(@azimuth.add(3, 2), 4))
  @azimuth.assert_eq(8, @azimuth.divide_with_ceil(@azimuth.add(@azimuth.multiply(2, 3), 2), 2))
  
  // 多层嵌套测试
  @azimuth.assert_eq(15, @azimuth.add(@azimuth.multiply(@azimuth.divide_with_ceil(10, 2), 2), 5))
}

test "assert_functions_comprehensive" {
  // 断言函数综合测试
  @azimuth.assert_true(1 + 1 == 2)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_true(-1 == -1)
  
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 == 1)
  @azimuth.assert_false(-1 == 1)
  
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq(0, 0)
  
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}

test "real_world_application_scenarios" {
  // 实际应用场景测试
  // 场景1：计算购物车总价（含税）
  let item1 = 299
  let item2 = 399
  let item3 = 199
  let tax_rate = 3
  
  let subtotal = @azimuth.add(@azimuth.add(item1, item2), item3)
  let total_with_tax = @azimuth.multiply(subtotal, tax_rate)
  @azimuth.assert_eq(2691, total_with_tax)
  
  // 场景2：计算平均分（向上取整）
  let score1 = 87
  let score2 = 93
  let score3 = 79
  let score4 = 85
  
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4)
  let average_score = @azimuth.divide_with_ceil(total_score, 4)
  @azimuth.assert_eq(87, average_score)
  
  // 场景3：用户欢迎消息
  let username = "张三"
  let welcome_msg = @azimuth.greet(username)
  @azimuth.assert_eq_string("Hello, 张三!", welcome_msg)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(123, 456), @azimuth.add(456, 123))
  @azimuth.assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(12, 34), @azimuth.multiply(34, 12))
  @azimuth.assert_eq(@azimuth.multiply(-5, 7), @azimuth.multiply(7, -5))
  
  // 结合律验证（在安全范围内）
  let a = 100
  let b = 200
  let c = 300
  
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}