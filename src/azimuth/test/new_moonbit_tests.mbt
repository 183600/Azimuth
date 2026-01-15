// 新增的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能，使用标准 MoonBit 测试语法

test "arithmetic_sequence_sum" {
  // 测试等差数列求和：1 + 2 + 3 + ... + 10 = 55
  let n = 10
  let sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, sum)
}

test "geometric_progression" {
  // 测试等比数列：2, 4, 8, 16, 32
  // 计算前5项的和：2 + 4 + 8 + 16 + 32 = 62
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 2)
  let term3 = @azimuth.multiply(term2, 2)
  let term4 = @azimuth.multiply(term3, 2)
  let term5 = @azimuth.multiply(term4, 2)
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(62, sum)
}

test "discount_calculation" {
  // 测试折扣计算场景
  let original_price = 1000
  let discount_percentage = 15
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(850, final_price)
}

test "compound_interest_simplified" {
  // 测试简化的复利计算
  let principal = 5000
  let rate = 5
  let years = 3
  // 简化计算：本金 + 本金 × 利率 × 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(80000, total)
}

test "perimeter_calculation" {
  // 测试周长计算
  let length = 12
  let width = 8
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(40, perimeter)
}

test "string_with_special_characters" {
  // 测试包含特殊字符的字符串处理
  let special_name = "User@123#Test"
  let result = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, User@123#Test!", result)
}

test "negative_number_operations" {
  // 测试负数运算
  let a = -15
  let b = -7
  
  // 负数加法
  @azimuth.assert_eq(-22, @azimuth.add(a, b))
  
  // 负数乘法
  @azimuth.assert_eq(105, @azimuth.multiply(a, b))
  
  // 正负数混合运算
  @azimuth.assert_eq(-8, @azimuth.add(a, 7))
  @azimuth.assert_eq(21, @azimuth.multiply(-3, -7))
}

test "boundary_value_division_simulation" {
  // 使用现有函数模拟除法运算
  let dividend = 100
  let divisor = 8
  // 使用 divide_with_ceil 函数进行向上取整除法
  let quotient = @azimuth.divide_with_ceil(dividend, divisor)
  
  @azimuth.assert_eq(13, quotient)  // 100 / 8 = 12.5，向上取整为 13
}

test "mixed_type_calculations" {
  // 测试混合类型计算（通过字符串表示）
  let numeric_string = "42"
  let greeting_result = @azimuth.greet(numeric_string)
  @azimuth.assert_eq_string("Hello, 42!", greeting_result)
  
  // 测试计算结果与字符串的结合
  let calculation_result = @azimuth.add(10, 20)
  let result_string = "Result: " + calculation_result.to_string()
  @azimuth.assert_true(result_string.length() > 0)
}

test "error_boundary_handling" {
  // 测试错误边界处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护是否正常工作
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试正常运算不受影响
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}