// Azimuth 项目增强单元测试
// 使用标准 MoonBit 测试语法，专注于核心功能的深度测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试负数情况
  let c = -12345
  let d = 67890
  
  let result3 = @azimuth.add(c, d)
  let result4 = @azimuth.add(d, c)
  
  @azimuth.assert_eq(result3, result4)
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 15
  let b = 25
  let c = 35
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let x = -10
  let y = 20
  let z = 30
  
  let left_side2 = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_side2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  
  @azimuth.assert_eq(left_side2, right_side2)
}

test "greet_function_special_characters" {
  // 测试 greet 函数处理特殊字符
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result = @azimuth.greet(special_chars)
  
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result)
  
  // 测试包含空格和制表符的字符串
  let whitespace_chars = "  Hello \t World  "
  let result2 = @azimuth.greet(whitespace_chars)
  
  @azimuth.assert_eq_string("Hello,   Hello \t World  !", result2)
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算 ((a + b) × c - d) ÷ e
  let a = 100
  let b = 200
  let c = 3
  let d = 150
  let e = 5
  
  // 步骤计算
  let sum_ab = @azimuth.add(a, b)  // 300
  let product = @azimuth.multiply(sum_ab, c)  // 900
  let difference = @azimuth.add(product, -d)  // 750
  let result = difference / e  // 150
  
  @azimuth.assert_eq(150, result)
}

test "boundary_value_combinations" {
  // 边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  let sum_result = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, sum_result)
  
  // 测试最大值与0的运算
  let max_with_zero = @azimuth.add(max_val, 0)
  @azimuth.assert_eq(max_val, max_with_zero)
  
  // 测试最小值与0的运算
  let min_with_zero = @azimuth.add(min_val, 0)
  @azimuth.assert_eq(min_val, min_with_zero)
  
  // 测试边界值的乘法
  let max_times_one = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, max_times_one)
  
  let min_times_one = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, min_times_one)
}

test "real_world_application_calculator" {
  // 实际应用场景：计算贷款月供（简化版）
  let principal = 100000  // 贷款本金
  let annual_rate = 6     // 年利率（百分比）
  let years = 30          // 贷款年限
  
  // 简化计算：月供 = (本金 + 本金 × 年利率 × 年数) ÷ (年数 × 12)
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = total_payment / @azimuth.multiply(years, 12)
  
  // 验证计算结果：(100000 + 100000×6×30) ÷ 360 = 19000000 ÷ 360 = 52777
  @azimuth.assert_eq(52777, monthly_payment)
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况
  // 测试极长字符串
  let very_long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  let result1 = @azimuth.greet(very_long_name)
  
  @azimuth.assert_eq_string("Hello, " + very_long_name + "!", result1)
  
  // 测试包含各种Unicode字符的字符串
  let unicode_name = "Hello世界🌍123@test"
  let result2 = @azimuth.greet(unicode_name)
  
  @azimuth.assert_eq_string("Hello, Hello世界🌍123@test!", result2)
  
  // 测试空字符串和单字符
  let empty_string = ""
  let single_char = "A"
  
  let result3 = @azimuth.greet(empty_string)
  let result4 = @azimuth.greet(single_char)
  
  @azimuth.assert_eq_string("Hello, !", result3)
  @azimuth.assert_eq_string("Hello, A!", result4)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：斐波那契数列前10项的和
  // 斐波那契数列：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  let f1 = 1
  let f2 = 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9) // 55
  
  // 计算前10项的和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(f1, f2), f3), f4), f5), f6), f7), f8), f9), f10)
  
  @azimuth.assert_eq(143, sum)
}