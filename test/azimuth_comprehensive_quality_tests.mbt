// Azimuth 库综合质量测试用例
// 包含多样化的测试场景，确保代码质量和功能正确性

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, azimuth.add(7, 3))
  assert_eq(-4, azimuth.add(-10, 6))
  assert_eq(0, azimuth.add(5, -5))
  
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-12, azimuth.multiply(-3, 4))
  assert_eq(0, azimuth.multiply(0, 100))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, !", azimuth.greet(""))
  assert_eq_string("Hello, 张三!", azimuth.greet("张三"))
  assert_eq_string("Hello, 123!", azimuth.greet("123"))
  assert_eq_string("Hello, 🌟!", azimuth.greet("🌟"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, azimuth.divide_with_ceil(10, 3))
  assert_eq(3, azimuth.divide_with_ceil(9, 3))
  assert_eq(1, azimuth.divide_with_ceil(1, 1))
  assert_eq(0, azimuth.divide_with_ceil(5, 0))
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))
  assert_eq(4, azimuth.divide_with_ceil(-11, -3))
}

test "mathematical_properties" {
  // 数学性质验证测试
  // 加法交换律
  let a = 7
  let b = 12
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法分配律
  let c = 5
  let distrib_left = azimuth.multiply(a, azimuth.add(b, c))
  let distrib_right = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  assert_eq(distrib_left, distrib_right)
  
  // 零元素性质
  assert_eq(0, azimuth.multiply(0, a))
  assert_eq(a, azimuth.add(a, 0))
}

test "boundary_conditions" {
  // 边界条件测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 大数运算
  assert_eq(max_int, azimuth.add(max_int - 1, 1))
  assert_eq(min_int, azimuth.add(min_int + 1, -1))
  
  // 极端除法
  assert_eq(1, azimuth.divide_with_ceil(1, max_int))
  assert_eq(1, azimuth.divide_with_ceil(max_int, max_int))
  assert_eq(0, azimuth.divide_with_ceil(0, max_int))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 购物车计算
  let apple_price = 8
  let apple_qty = 3
  let orange_price = 6
  let orange_qty = 4
  
  let subtotal = azimuth.add(
    azimuth.multiply(apple_price, apple_qty),
    azimuth.multiply(orange_price, orange_qty)
  )
  assert_eq(48, subtotal)  // 8*3 + 6*4 = 24 + 24 = 48
  
  // 包装需求计算
  let total_items = 37
  let items_per_box = 10
  let boxes_needed = azimuth.divide_with_ceil(total_items, items_per_box)
  assert_eq(4, boxes_needed)  // ceil(37/10) = 4
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零处理
  assert_eq(0, azimuth.divide_with_ceil(100, 0))
  assert_eq(0, azimuth.divide_with_ceil(-100, 0))
  assert_eq(0, azimuth.divide_with_ceil(0, 0))
  
  // 负数除法
  assert_eq(-2, azimuth.divide_with_ceil(-5, 2))
  assert_eq(-3, azimuth.divide_with_ceil(5, -2))
  assert_eq(2, azimuth.divide_with_ceil(-5, -2))
}

test "complex_calculations" {
  // 复杂计算测试
  // 多层嵌套计算
  let result1 = azimuth.add(
    azimuth.multiply(5, azimuth.divide_with_ceil(17, 4)),
    azimuth.multiply(3, azimuth.divide_with_ceil(25, 6))
  )
  assert_eq(31, result1)  // 5*5 + 3*5 = 25 + 15 = 40 -> 修正为 5*5 + 3*4 = 25 + 12 = 37
  
  // 数学级数计算
  let n = 5
  let series_sum = azimuth.multiply(n, azimuth.add(n, 1)) / 2
  assert_eq(15, series_sum)  // 5*6/2 = 15
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 特殊字符组合
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_eq_string("Hello, " + special_chars + "!", azimuth.greet(special_chars))
  
  // 长字符串
  let long_name = "This is a very long name that might be used in some special cases"
  let long_greeting = azimuth.greet(long_name)
  assert_eq_string("Hello, " + long_name + "!", long_greeting)
  
  // Unicode 字符
  assert_eq_string("Hello, αβγδε!", azimuth.greet("αβγδε"))
  assert_eq_string("Hello, 🌍🌎🌏!", azimuth.greet("🌍🌎🌏"))
}

test "performance_and_efficiency" {
  // 性能和效率测试
  // 重复计算验证一致性
  let base = 123
  let multiplier = 456
  
  let result1 = azimuth.multiply(base, multiplier)
  let result2 = azimuth.multiply(base, multiplier)
  let result3 = azimuth.multiply(base, multiplier)
  
  assert_eq(result1, result2)
  assert_eq(result2, result3)
  
  // 累加计算
  let accumulator = 0
  let values = [10, 20, 30, 40, 50]
  accumulator = azimuth.add(accumulator, values[0])
  accumulator = azimuth.add(accumulator, values[1])
  accumulator = azimuth.add(accumulator, values[2])
  accumulator = azimuth.add(accumulator, values[3])
  accumulator = azimuth.add(accumulator, values[4])
  
  assert_eq(150, accumulator)  // 10+20+30+40+50 = 150
}