// 新增的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_commutativity_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的交换律
  let c = -12345
  let d = 67890
  let result3 = @azimuth.add(c, d)
  let result4 = @azimuth.add(d, c)
  @azimuth.assert_eq(result3, result4)
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 15
  let b = 20
  let c = 25
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "calculation_pyramid" {
  // 测试金字塔计算：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 10
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(sum1, 3)
  let sum3 = @azimuth.add(sum2, 4)
  let sum4 = @azimuth.add(sum3, 5)
  let sum5 = @azimuth.add(sum4, 6)
  let sum6 = @azimuth.add(sum5, 7)
  let sum7 = @azimuth.add(sum6, 8)
  let sum8 = @azimuth.add(sum7, 9)
  let sum9 = @azimuth.add(sum8, 10)
  let formula_result = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(sum9, formula_result)
}

test "string_concatenation_chain" {
  // 测试字符串连接链
  let name = "MoonBit"
  
  // 通过greet函数进行连接
  let result = @azimuth.greet(name)
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
  
  // 测试包含空格的名字
  let spaced_name = "MoonBit Language"
  let spaced_result = @azimuth.greet(spaced_name)
  @azimuth.assert_eq_string("Hello, MoonBit Language!", spaced_result)
}

test "boundary_value_arithmetic" {
  // 测试边界值算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与中等值的运算
  @azimuth.assert_eq(2147483647, @azimuth.add(max_val, 1000))
  @azimuth.assert_eq(2147483647, @azimuth.add(max_val, max_val))
  
  // 测试最小值与中等值的运算
  @azimuth.assert_eq(-2147483648, @azimuth.add(min_val, -1000))
  @azimuth.assert_eq(-2147483648, @azimuth.add(min_val, min_val))
  
  // 测试最大值与负数的乘法
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(max_val, -1))
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值：(a + b) × (c - d) + e × f
  let a = 10
  let b = 20
  let c = 30
  let d = 15
  let e = 5
  let f = 8
  
  let sum_ab = @azimuth.add(a, b)  // 30
  let diff_cd = @azimuth.add(c, -d)  // 15
  let product_ef = @azimuth.multiply(e, f)  // 40
  let result = @azimuth.add(@azimuth.multiply(sum_ab, diff_cd), product_ef)  // 30 × 15 + 40 = 490
  
  @azimuth.assert_eq(490, result)
}

test "mathematical_series_validation" {
  // 测试数学级数验证：平方和公式 1² + 2² + ... + n² = n × (n + 1) × (2n + 1) / 6
  let n = 5
  
  // 手动计算前5个平方数
  let sq1 = @azimuth.multiply(1, 1)
  let sq2 = @azimuth.multiply(2, 2)
  let sq3 = @azimuth.multiply(3, 3)
  let sq4 = @azimuth.multiply(4, 4)
  let sq5 = @azimuth.multiply(5, 5)
  let sum1 = @azimuth.add(sq1, sq2)
  let sum2 = @azimuth.add(sum1, sq3)
  let sum3 = @azimuth.add(sum2, sq4)
  let squares_sum = @azimuth.add(sum3, sq5)
  
  // 使用公式计算
  let formula_result = @azimuth.multiply(@azimuth.multiply(n, @azimuth.add(n, 1)), @azimuth.add(@azimuth.multiply(2, n), 1)) / 6
  
  @azimuth.assert_eq(squares_sum, formula_result)
}

test "real_world_financial_calculation" {
  // 测试实际金融计算：贷款月供计算（简化版）
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率（百分比）
  let years = 30          // 贷款年限
  
  // 简化的月供计算：本金 × (1 + 年利率 × 年数) / (年数 × 12)
  let total_amount = @azimuth.multiply(principal, @azimuth.add(1, @azimuth.multiply(annual_rate, years)))
  let monthly_payment = total_amount / @azimuth.multiply(years, 12)
  
  // 验证结果：100000 × (1 + 6×30) / 360 = 100000 × 181 / 360 ≈ 50277
  @azimuth.assert_eq(50277, monthly_payment)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试各种边界条件的组合
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)  // 正溢出
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)  // 负溢出
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)  // 乘法正溢出
  @azimuth.assert_true(@azimuth.multiply(min_val, -2) == min_val)  // 乘法特殊情况
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "algorithmic_pattern_recognition" {
  // 测试算法模式识别：斐波那契数列计算（前10项）
  // F(0)=0, F(1)=1, F(n)=F(n-1)+F(n-2)
  
  // 手动计算前10项
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f1, f0)  // 1
  let f3 = @azimuth.add(f2, f1)  // 2
  let f4 = @azimuth.add(f3, f2)  // 3
  let f5 = @azimuth.add(f4, f3)  // 5
  let f6 = @azimuth.add(f5, f4)  // 8
  let f7 = @azimuth.add(f6, f5)  // 13
  let f8 = @azimuth.add(f7, f6)  // 21
  let f9 = @azimuth.add(f8, f7)  // 34
  
  // 验证结果
  @azimuth.assert_eq(0, f0)
  @azimuth.assert_eq(1, f1)
  @azimuth.assert_eq(1, f2)
  @azimuth.assert_eq(2, f3)
  @azimuth.assert_eq(3, f4)
  @azimuth.assert_eq(5, f5)
  @azimuth.assert_eq(8, f6)
  @azimuth.assert_eq(13, f7)
  @azimuth.assert_eq(21, f8)
  @azimuth.assert_eq(34, f9)
  
  // 验证前10项的和
  let sum1 = @azimuth.add(f0, f1)
  let sum2 = @azimuth.add(sum1, f2)
  let sum3 = @azimuth.add(sum2, f3)
  let sum4 = @azimuth.add(sum3, f4)
  let sum5 = @azimuth.add(sum4, f5)
  let sum6 = @azimuth.add(sum5, f6)
  let sum7 = @azimuth.add(sum6, f7)
  let sum8 = @azimuth.add(sum7, f8)
  let sum_first_10 = @azimuth.add(sum8, f9)
  @azimuth.assert_eq(88, sum_first_10)
}