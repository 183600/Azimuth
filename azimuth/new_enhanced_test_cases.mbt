// 新增的 MoonBit 测试用例
// 覆盖 azimuth 库的主要功能，包含边界情况、特殊场景和实际应用

test "add_function_extreme_values" {
  // 测试加法函数的极值情况
  @azimuth.assert_eq(1000000, @azimuth.add(999999, 1))
  @azimuth.assert_eq(-1000000, @azimuth.add(-999999, -1))
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(1, @azimuth.add(-2147483648, 2147483649))
}

test "multiply_function_precision_cases" {
  // 测试乘法函数的精度和特殊情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
}

test "greet_function_unicode_extended" {
  // 测试问候函数的扩展Unicode字符支持
  @azimuth.assert_eq_string("Hello, 🌙⭐!", @azimuth.greet("🌙⭐"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, 🍕🍔🍟!", @azimuth.greet("🍕🍔🍟"))
  @azimuth.assert_eq_string("Hello, 𝓜𝓸𝓸𝓷𝓑𝓲𝓽!", @azimuth.greet("𝓜𝓸𝓸𝓷𝓑𝓲𝓽"))
}

test "divide_with_ceil_fractional_precision" {
  // 测试向上取整除法的分数精度
  @azimuth.assert_eq(100, @azimuth.divide_with_ceil(999, 10))
  @azimuth.assert_eq(50, @azimuth.divide_with_ceil(999, 20))
  @azimuth.assert_eq(34, @azimuth.divide_with_ceil(100, 3))
  @azimuth.assert_eq(67, @azimuth.divide_with_ceil(200, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(13, 4))
}

test "complex_calculation_pipeline" {
  // 测试复杂的计算管道场景
  let initial_value = 100
  let step1 = @azimuth.divide_with_ceil(initial_value, 3)  // 34
  let step2 = @azimuth.multiply(step1, 2)                  // 68
  let step3 = @azimuth.add(step2, 32)                      // 100
  let step4 = @azimuth.divide_with_ceil(step3, 25)         // 4
  let final_result = @azimuth.multiply(step4, 25)          // 100
  
  @azimuth.assert_eq(100, final_result)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "batch_processing_simulation" {
  // 测试批量处理模拟场景
  let batch_size = 47
  let items_per_batch = 8
  let total_batches = @azimuth.divide_with_ceil(batch_size, items_per_batch)
  
  // 验证批处理计算
  @azimuth.assert_eq(6, total_batches)
  
  // 计算最后一个批次的项目数
  let full_batches = @azimuth.divide_with_ceil(batch_size - 1, items_per_batch)
  let last_batch_size = @azimuth.add(batch_size, @azimuth.multiply(-full_batches, items_per_batch))
  @azimuth.assert_eq(7, last_batch_size)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  let product_price = 199
  let quantity = 3
  let tax_rate = 8  // 8%
  let subtotal = @azimuth.multiply(product_price, quantity)
  let tax_amount = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(subtotal, tax_amount)
  
  @azimuth.assert_eq(597, subtotal)
  @azimuth.assert_eq(48, tax_amount)  // ceil(597 * 8 / 100) = ceil(47.76) = 48
  @azimuth.assert_eq(645, total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边缘情况
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, 
	!", @azimuth.greet("
	"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, 'single'!", @azimuth.greet("'single'"))
  @azimuth.assert_eq_string("Hello, \\backslash!", @azimuth.greet("\\backslash"))
}