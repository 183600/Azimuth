// 新增的 MoonBit 测试用例
// 这些测试用例覆盖了更多边界情况和实际应用场景

// 导入主模块
test "add_negative_positive_combinations" {
  // 测试负数和正数的各种组合相加
  assert_eq(0, add(-100, 100))
  assert_eq(-50, add(-100, 50))
  assert_eq(50, add(100, -50))
  assert_eq(-150, add(-200, 50))
  assert_eq(150, add(200, -50))
}

test "multiply_fractional_simulation" {
  // 模拟分数运算：通过整数运算模拟分数乘法
  // 计算 (3/4) * (8/9) = 24/36 = 2/3，模拟为 3*8/(4*9) = 24/36
  let numerator1 = 3
  let denominator1 = 4
  let numerator2 = 8
  let denominator2 = 9
  
  let result_numerator = multiply(numerator1, numerator2)
  let result_denominator = multiply(denominator1, denominator2)
  
  assert_eq(24, result_numerator)
  assert_eq(36, result_denominator)
}

test "string_length_impact" {
  // 测试不同长度字符串的处理
  let empty = ""
  let single = "A"
  let short = "Hello"
  let medium = "MoonBit Programming"
  let long = "This is a very long string that contains multiple words and should test the string handling capabilities"
  
  assert_eq_string("Hello, !", greet(empty))
  assert_eq_string("Hello, A!", greet(single))
  assert_eq_string("Hello, Hello!", greet(short))
  assert_eq_string("Hello, MoonBit Programming!", greet(medium))
  assert_eq_string("Hello, This is a very long string that contains multiple words and should test the string handling capabilities!", greet(long))
}

test "arithmetic_progression" {
  // 测试等差数列求和：1 + 3 + 5 + ... + 19 = 100
  let first_term = 1
  let common_diff = 2
  let num_terms = 10
  
  // 手动计算前10项
  let term1 = first_term
  let term2 = add(term1, common_diff)  // 3
  let term3 = add(term2, common_diff)  // 5
  let term4 = add(term3, common_diff)  // 7
  let term5 = add(term4, common_diff)  // 9
  let term6 = add(term5, common_diff)  // 11
  let term7 = add(term6, common_diff)  // 13
  let term8 = add(term7, common_diff)  // 15
  let term9 = add(term8, common_diff)  // 17
  let term10 = add(term9, common_diff) // 19
  
  let sum = add(add(add(add(add(add(add(add(add(term1, term2), term3), term4), term5), term6), term7), term8), term9), term10)
  assert_eq(100, sum)
}

test "geometric_progression" {
  // 测试等比数列求和：2 + 4 + 8 + 16 = 30
  let first_term = 2
  let common_ratio = 2
  let num_terms = 4
  
  // 手动计算前4项
  let term1 = first_term
  let term2 = multiply(term1, common_ratio)  // 4
  let term3 = multiply(term2, common_ratio)  // 8
  let term4 = multiply(term3, common_ratio)  // 16
  
  let sum = add(add(add(term1, term2), term3), term4)
  assert_eq(30, sum)
}

test "compound_interest_calculation" {
  // 测试复利计算：本金 × (1 + 利率)^年数
  // 简化版本，使用整数运算
  let principal = 1000  // 本金
  let rate_percent = 5   // 利率百分比
  let years = 3         // 年数
  
  // 第一年：1000 + 1000*5% = 1000 + 50 = 1050
  let year1_interest = multiply(principal, rate_percent) / 100
  let year1_total = add(principal, year1_interest)
  
  // 第二年：1050 + 1050*5% = 1050 + 52 = 1102（整数除法）
  let year2_interest = multiply(year1_total, rate_percent) / 100
  let year2_total = add(year1_total, year2_interest)
  
  // 第三年：1102 + 1102*5% = 1102 + 55 = 1157（整数除法）
  let year3_interest = multiply(year2_total, rate_percent) / 100
  let year3_total = add(year2_total, year3_interest)
  
  assert_eq(1157, year3_total)
}

test "factorial_calculation" {
  // 测试阶乘计算：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  
  let step1 = multiply(n, 4)  // 5 * 4 = 20
  let step2 = multiply(step1, 3)  // 20 * 3 = 60
  let step3 = multiply(step2, 2)  // 60 * 2 = 120
  let step4 = multiply(step3, 1)  // 120 * 1 = 120
  
  assert_eq(120, step4)
}

test "binary_conversion_simulation" {
  // 模拟二进制转换：将二进制数 1011 转换为十进制
  // 1011₂ = 1×2³ + 0×2² + 1×2¹ + 1×2⁰ = 8 + 0 + 2 + 1 = 11
  
  let bit3 = 1  // 最高位
  let bit2 = 0
  let bit1 = 1
  let bit0 = 1  // 最低位
  
  let value3 = multiply(bit3, multiply(2, multiply(2, 2)))  // 1 * 2³ = 8
  let value2 = multiply(bit2, multiply(2, 2))              // 0 * 2² = 0
  let value1 = multiply(bit1, 2)                           // 1 * 2¹ = 2
  let value0 = multiply(bit0, 1)                           // 1 * 2⁰ = 1
  
  let decimal_value = add(add(add(value3, value2), value1), value0)
  assert_eq(11, decimal_value)
}

test "volume_calculation" {
  // 测试体积计算：长方体体积 = 长 × 宽 × 高
  let length = 5
  let width = 3
  let height = 4
  
  let base_area = multiply(length, width)  // 5 * 3 = 15
  let volume = multiply(base_area, height) // 15 * 4 = 60
  
  assert_eq(60, volume)
  
  // 圆柱体体积（简化版）：底面积 × 高
  let radius = 3
  let cyl_height = 10
  
  // 圆面积 ≈ 半径² × 3（π的近似值）
  let circle_area = multiply(multiply(radius, radius), 3)  // 3² × 3 = 27
  let cylinder_volume = multiply(circle_area, cyl_height)  // 27 × 10 = 270
  
  assert_eq(270, cylinder_volume)
}

test "string_case_scenarios" {
  // 测试不同大小写场景的字符串处理
  let lowercase = "moonbit"
  let uppercase = "MOONBIT"
  let mixedcase = "MoonBit"
  let camelcase = "moonBit"
  
  assert_eq_string("Hello, moonbit!", greet(lowercase))
  assert_eq_string("Hello, MOONBIT!", greet(uppercase))
  assert_eq_string("Hello, MoonBit!", greet(mixedcase))
  assert_eq_string("Hello, moonBit!", greet(camelcase))
}