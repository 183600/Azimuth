// 附加 MoonBit 测试用例
// 覆盖实际应用场景和边缘情况

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(3, -6))
}

test "string_concatenation_various_inputs" {
  // 测试各种字符串输入的连接
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, a@b.c!", @azimuth.greet("a@b.c"))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 3 + 5 + ... + 19
  let n = 10
  let first_term = 1
  let last_term = 19
  let sum = @azimuth.multiply(n, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(100, sum)
  
  // 计算等比数列前n项和：2 + 4 + 8 + 16
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
}

test "boundary_value_comprehensive" {
  // 测试边界值的综合情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(-1, min_val))
}

test "real_world_calculations" {
  // 测试实际应用计算
  // 圆的面积和周长（简化计算，π≈3）
  let radius = 5
  let circumference = @azimuth.multiply(@azimuth.multiply(2, 3), radius)
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  
  @azimuth.assert_eq(30, circumference)
  @azimuth.assert_eq(75, area)
  
  // 简单贷款计算：本金 × (1 + 利率 × 时间)
  let principal = 10000
  let rate = 5
  let time = 2
  let total = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(rate, time)))
  @azimuth.assert_eq(110000, total)
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值
  // 计算：(a + b) × (c - d) + e × f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)
  let diff_cd = @azimuth.add(c, -d)
  let product_ef = @azimuth.multiply(e, f)
  let result = @azimuth.add(@azimuth.multiply(sum_ab, diff_cd), product_ef)
  
  @azimuth.assert_eq(121, result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边缘情况
  // 零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  
  // 单位运算
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-1, 12345))
  
  // 大数运算
  let large_num = 1000000
  @azimuth.assert_eq(2000000, @azimuth.add(large_num, large_num))
  @azimuth.assert_eq(1000000, @azimuth.multiply(large_num, 1))
}

test "algorithmic_patterns" {
  // 测试算法模式
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1
  let factorial_5 = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_5)
  
  // 计算2的幂：2^6 = 64
  let power_of_2_6 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)
  @azimuth.assert_eq(64, power_of_2_6)
  
  // 计算斐波那契数列第7项（简化的递推计算）
  let fib_7 = @azimuth.add(@azimuth.multiply(5, 3), @azimuth.multiply(3, 2))
  @azimuth.assert_eq(21, fib_7)
}

test "data_processing_scenarios" {
  // 测试数据处理场景
  // 计算数组索引：二维数组转一维数组
  let row = 2
  let col = 3
  let width = 5
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(13, index)
  
  // 计算百分比和比例
  let total = 500
  let part = 125
  let percentage = @azimuth.multiply(part, 100) / total
  @azimuth.assert_eq(25, percentage)
  
  // 计算平均值
  let values_sum = @azimuth.add(@azimuth.add(@azimuth.add(80, 90), 85), 95)
  let count = 4
  let average = values_sum / count
  @azimuth.assert_eq(87, average)
}