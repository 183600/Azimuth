// 新的 MoonBit 测试示例
// 包含10个标准测试用例，展示不同的测试场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  // 加法测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-3, @azimuth.add(5, -8))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  
  // 乘法测试
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(8, -3))
  @azimuth.assert_eq(36, @azimuth.multiply(-6, -6))
  
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
}

test "string_functionality_test" {
  // 测试字符串功能
  // 基本问候
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 长字符串处理
  let long_name = "VeryLongNameForTestingPurposes"
  @azimuth.assert_eq_string("Hello, VeryLongNameForTestingPurposes!", @azimuth.greet(long_name))
}

test "mathematical_formulas" {
  // 测试数学公式
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(40, area)        // 8 * 5 = 40
  @azimuth.assert_eq(26, perimeter)   // 2*8 + 2*5 = 26
  
  // 计算三角形面积（简化）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)  // 10 * 6 / 2 = 30
}

test "boundary_value_testing" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  
  // 最小值测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 极值运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 99
  let item1_quantity = 2
  let item2_price = 149
  let item2_quantity = 1
  let shipping = 10
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let final_total = @azimuth.add(@azimuth.add(total1, total2), shipping)
  
  @azimuth.assert_eq(357, final_total)  // 99*2 + 149*1 + 10 = 357
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 基本整除
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(10, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  
  // 需要向上取整
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(7, 2))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(8, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(3, 2))
  
  // 负数处理
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  
  // 零值处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "assertion_function_tests" {
  // 测试断言函数
  // assert_true 和 assert_false
  @azimuth.assert_eq(true, 10 > 5)
  @azimuth.assert_eq(0 == 0, true)
  @azimuth.assert_eq(0, 1)(10 < 5)
  @azimuth.assert_eq(0, 1)(1 == 2)
  
  // assert_eq 测试
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  
  // assert_eq_string 测试
  @azimuth.assert_eq_string("Test", "Test")
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "complex_calculation_patterns" {
  // 测试复杂计算模式
  // 阶乘计算（5!）
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
  
  // 平方和计算（1² + 2² + 3² + 4² + 5²）
  let sum_of_squares = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 4), 9), 16), 25)
  @azimuth.assert_eq(55, sum_of_squares)
  
  // 等差数列求和（1 + 3 + 5 + 7 + 9）
  let arithmetic_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7), 9)
  @azimuth.assert_eq(25, arithmetic_sum)
}

test "performance_and_optimization" {
  // 测试性能和优化场景
  // 批量计算
  let base_value = 100
  let multiplier = 5
  let iterations = 10
  
  // 使用公式计算：base_value * multiplier * iterations
  let batch_result = @azimuth.multiply(@azimuth.multiply(base_value, multiplier), iterations)
  @azimuth.assert_eq(5000, batch_result)
  
  // 缓存模拟计算
  let cache_size = 1000
  let hit_rate = 85  // 85%
  let total_requests = 10000
  
  let cache_hits = @azimuth.multiply(total_requests, hit_rate) / 100
  @azimuth.assert_eq(8500, cache_hits)
  
  // 内存使用计算
  let object_size = 32
  let object_count = 500
  let total_memory = @azimuth.multiply(object_size, object_count)
  @azimuth.assert_eq(16000, total_memory)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护验证
  @azimuth.assert_eq(@azimuth.add(max_val, 1000, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1000, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1, true) == min_val)
  
  // 边界条件验证
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
  
  // 特殊值处理
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  @azimuth.assert_eq(0, @azimuth.add(1, -1))
}