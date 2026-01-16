// Azimuth 项目新功能测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "large_number_operations" {
  // 大数运算测试
  @azimuth.assert_eq(3000000, @azimuth.add(1000000, 2000000))
  @azimuth.assert_eq(1000000, @azimuth.add(999999, 1))
  @azimuth.assert_eq(10000000, @azimuth.multiply(10000, 1000))
  @azimuth.assert_eq(83810205, @azimuth.multiply(12345, 6789))
}

test "string_length_edge_cases" {
  // 字符串长度边界测试
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, AB!", @azimuth.greet("AB"))
  @azimuth.assert_eq_string("Hello, ABC!", @azimuth.greet("ABC"))
  @azimuth.assert_eq_string("Hello, VeryLongNameForTesting!", @azimuth.greet("VeryLongNameForTesting"))
}

test "mathematical_formulas" {
  // 数学公式测试
  
  // 二次方程求根公式的一部分：b²-4ac
  let a = 1
  let b = 5
  let c = 6
  let discriminant = @azimuth.add(@azimuth.multiply(b, b), @azimuth.multiply(-4, @azimuth.multiply(a, c)))
  @azimuth.assert_eq(1, discriminant)
  
  // 勾股定理：a² + b² = c²
  let side_a = 3
  let side_b = 4
  let side_c_squared = @azimuth.add(@azimuth.multiply(side_a, side_a), @azimuth.multiply(side_b, side_b))
  @azimuth.assert_eq(25, side_c_squared)
}

test "real_world_calculations" {
  // 实际应用场景测试
  
  // 购物车总价计算
  let item1_price = 25
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  let total = @azimuth.add(@azimuth.multiply(item1_price, item1_quantity), @azimuth.multiply(item2_price, item2_quantity))
  @azimuth.assert_eq(95, total)
  
  // 速度距离时间计算：距离 = 速度 × 时间
  let speed = 60  // km/h
  let distance = @azimuth.divide_with_ceil(@azimuth.multiply(speed, 25), 10)
  @azimuth.assert_eq(150, distance)
}

test "boundary_value_analysis" {
  // 边界值分析测试
  
  // 测试零值附近的运算
  @azimuth.assert_eq(0, @azimuth.add(-1, 1))
  @azimuth.assert_eq(0, @azimuth.add(1, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 99999))
  @azimuth.assert_eq(0, @azimuth.multiply(99999, 0))
  
  // 测试单位值附近的运算
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(1, @azimuth.add(0, 1))
}

test "calculation_precision" {
  // 计算精度测试
  
  // 累加计算
  let mut accumulator = 0
  accumulator = @azimuth.add(accumulator, 10)
  accumulator = @azimuth.add(accumulator, 20)
  accumulator = @azimuth.add(accumulator, 30)
  accumulator = @azimuth.add(accumulator, 40)
  @azimuth.assert_eq(100, accumulator)
  
  // 累乘计算
  let mut product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  @azimuth.assert_eq(24, product)
}

test "special_string_formats" {
  // 特殊字符串格式测试
  
  // 数字字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 456789!", @azimuth.greet("456789"))
  
  // 符号字符串
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
  @azimuth.assert_eq_string("Hello, ()[]{}!", @azimuth.greet("()[]{}"))
  
  // 混合字符串
  @azimuth.assert_eq_string("Hello, Test123!", @azimuth.greet("Test123!"))
}

test "complex_arithmetic_sequences" {
  // 复杂数学序列测试
  
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial_5 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(5, 4), 3), 2), 1)
  @azimuth.assert_eq(120, factorial_5)
  
  // 计算组合数：C(5,2) = 5! / (2! × 3!) = 10
  let n = 5
  let k = 2
  let numerator = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(n, n-1), n-2), n-3), n-4)
  let denominator = @azimuth.multiply(@azimuth.multiply(k, k-1), @azimuth.multiply(@azimuth.multiply(3, 2), 1))
  let combination = @azimuth.divide_with_ceil(numerator, denominator)
  @azimuth.assert_eq(10, combination)
}

test "statistical_calculations" {
  // 统计计算测试
  
  // 计算平均值：(10 + 20 + 30 + 40 + 50) / 5 = 30
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let count = 5
  let average = @azimuth.divide_with_ceil(sum, count)
  @azimuth.assert_eq(30, average)
  
  // 计算加权平均：10×0.2 + 20×0.3 + 30×0.5 = 23
  let value1 = 10
  let weight1 = 2
  let value2 = 20
  let weight2 = 3
  let value3 = 30
  let weight3 = 5
  let weighted_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(value1, weight1), @azimuth.multiply(value2, weight2)), @azimuth.multiply(value3, weight3))
  let total_weight = @azimuth.add(@azimuth.add(weight1, weight2), weight3)
  let weighted_average = @azimuth.divide_with_ceil(weighted_sum, total_weight)
  @azimuth.assert_eq(23, weighted_average)
}

test "error_prone_scenarios" {
  // 容易出错的场景测试
  
  // 连续减法（通过加负数实现）
  let mut result = 100
  result = @azimuth.add(result, -10)  // 90
  result = @azimuth.add(result, -20)  // 70
  result = @azimuth.add(result, -30)  // 40
  @azimuth.assert_eq(40, result)
  
  // 混合运算优先级
  let a = 2
  let b = 3
  let c = 4
  // 先乘后加：a + b × c = 2 + 3 × 4 = 14
  let mixed_result = @azimuth.add(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(14, mixed_result)
  
  // 先加后乘：(a + b) × c = (2 + 3) × 4 = 20
  let mixed_result2 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(20, mixed_result2)
}