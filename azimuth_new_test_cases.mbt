// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  let a = 15
  let b = 7
  
  // 测试加法
  let sum = azimuth.add(a, b)
  @assertion.assert_eq(22, sum)?
  
  // 测试乘法
  let product = azimuth.multiply(a, b)
  @assertion.assert_eq(105, product)?
  
  // 测试复合运算
  let result = azimuth.add(azimuth.multiply(a, 2), azimuth.multiply(b, 3))
  @assertion.assert_eq(51, result)?
}

test "negative_number_operations" {
  // 负数运算测试
  let positive = 10
  let negative = -5
  
  // 测试正负数相加
  let result1 = azimuth.add(positive, negative)
  @assertion.assert_eq(5, result1)?
  
  // 测试负数相加
  let result2 = azimuth.add(negative, negative)
  @assertion.assert_eq(-10, result2)?
  
  // 测试正负数相乘
  let result3 = azimuth.multiply(positive, negative)
  @assertion.assert_eq(-50, result3)?
  
  // 测试负数相乘
  let result4 = azimuth.multiply(negative, negative)
  @assertion.assert_eq(25, result4)?
}

test "zero_operations" {
  // 零值运算测试
  let number = 42
  
  // 测试与零相加
  let result1 = azimuth.add(number, 0)
  @assertion.assert_eq(42, result1)?
  
  // 测试与零相乘
  let result2 = azimuth.multiply(number, 0)
  @assertion.assert_eq(0, result2)?
  
  // 测试零自加
  let result3 = azimuth.add(0, 0)
  @assertion.assert_eq(0, result3)?
  
  // 测试零自乘
  let result4 = azimuth.multiply(0, 0)
  @assertion.assert_eq(0, result4)?
}

test "greeting_function_basic" {
  // 基本问候函数测试
  let name1 = "Alice"
  let result1 = azimuth.greet(name1)
  @assertion.assert_eq_string("Hello, Alice!", result1)?
  
  let name2 = "Bob"
  let result2 = azimuth.greet(name2)
  @assertion.assert_eq_string("Hello, Bob!", result2)?
  
  let name3 = "Charlie"
  let result3 = azimuth.greet(name3)
  @assertion.assert_eq_string("Hello, Charlie!", result3)?
}

test "greeting_function_edge_cases" {
  // 问候函数边界情况测试
  // 测试空字符串
  let empty_name = ""
  let result1 = azimuth.greet(empty_name)
  @assertion.assert_eq_string("Hello, !", result1)?
  
  // 测试单字符
  let single_char = "A"
  let result2 = azimuth.greet(single_char)
  @assertion.assert_eq_string("Hello, A!", result2)?
  
  // 测试包含空格的字符串
  let spaced_name = "John Doe"
  let result3 = azimuth.greet(spaced_name)
  @assertion.assert_eq_string("Hello, John Doe!", result3)?
}

test "calculation_sequences" {
  // 计算序列测试
  let base = 5
  
  // 测试递增序列
  let seq1 = azimuth.add(base, 1)
  let seq2 = azimuth.add(seq1, 1)
  let seq3 = azimuth.add(seq2, 1)
  @assertion.assert_eq(8, seq3)?
  
  // 测试递乘序列
  let mul1 = azimuth.multiply(base, 2)
  let mul2 = azimuth.multiply(mul1, 2)
  let mul3 = azimuth.multiply(mul2, 2)
  @assertion.assert_eq(40, mul3)?
  
  // 测试混合序列
  let mixed1 = azimuth.add(base, 3)
  let mixed2 = azimuth.multiply(mixed1, 2)
  let mixed3 = azimuth.add(mixed2, 10)
  @assertion.assert_eq(26, mixed3)?
}

test "real_world_scenarios" {
  // 实际场景测试
  // 模拟购物车计算
  let item_price = 25
  let quantity = 4
  let tax_rate = 8
  let shipping = 10
  
  // 计算商品总价
  let subtotal = azimuth.multiply(item_price, quantity)
  
  // 计算税费
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  // 验证结果：(25×4) + (25×4×8/100) + 10 = 100 + 8 + 10 = 118
  @assertion.assert_eq(118, total)?
}

test "mathematical_formulas" {
  // 数学公式测试
  // 计算矩形面积和周长
  let length = 8
  let width = 6
  
  // 面积 = 长 × 宽
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(48, area)?
  
  // 周长 = 2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(28, perimeter)?
  
  // 计算梯形面积（简化）：(上底 + 下底) × 高 / 2
  let top_base = 5
  let bottom_base = 9
  let height = 4
  let trapezoid_area = azimuth.multiply(azimuth.add(top_base, bottom_base), height) / 2
  @assertion.assert_eq(28, trapezoid_area)?
}

test "performance_benchmarks" {
  // 性能基准测试
  let base = 100
  
  // 测试连续加法性能
  let add_start = 0
  let add_result = azimuth.add(azimuth.add(azimuth.add(azimuth.add(add_start, base), base), base), base)
  @assertion.assert_eq(400, add_result)?
  
  // 测试连续乘法性能
  let mul_start = 1
  let mul_result = azimuth.multiply(azimuth.multiply(azimuth.multiply(azimuth.multiply(mul_start, 2), 2), 2), 2)
  @assertion.assert_eq(16, mul_result)?
  
  // 测试混合运算性能
  let mixed_result = azimuth.add(azimuth.multiply(base, 2), azimuth.multiply(base, 3))
  @assertion.assert_eq(500, mixed_result)?
}

test "boundary_value_testing" {
  // 边界值测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试接近最大值的运算
  let near_max = azimuth.add(max_int, -1000)
  @assertion.assert_eq(2147482647, near_max)?
  
  // 测试接近最小值的运算
  let near_min = azimuth.add(min_int, 1000)
  @assertion.assert_eq(-2147482648, near_min)?
  
  // 测试大数乘法
  let large_mul = azimuth.multiply(1000000, 1000)
  @assertion.assert_eq(1000000000, large_mul)?
  
  // 测试负数大数乘法
  let large_neg_mul = azimuth.multiply(-1000000, 1000)
  @assertion.assert_eq(-1000000000, large_neg_mul)?
}