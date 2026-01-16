// Azimuth 项目综合新增测试用例
// 使用标准 MoonBit 测试语法，提供多样化的测试场景

test "addition_with_large_numbers" {
  // 测试大数加法
  let large_num1 = 1000000
  let large_num2 = 2000000
  let result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(result, 3000000)
}

test "multiplication_table_pattern" {
  // 测试乘法表模式
  let base = 7
  let expected1 = @azimuth.multiply(base, 1)  // 7
  let expected2 = @azimuth.multiply(base, 2)  // 14
  let expected3 = @azimuth.multiply(base, 3)  // 21
  let expected4 = @azimuth.multiply(base, 4)  // 28
  let expected5 = @azimuth.multiply(base, 5)  // 35
  
  @azimuth.assert_eq(expected1, 7)
  @azimuth.assert_eq(expected2, 14)
  @azimuth.assert_eq(expected3, 21)
  @azimuth.assert_eq(expected4, 28)
  @azimuth.assert_eq(expected5, 35)
}

test "string_concatenation_through_greet" {
  // 测试通过 greet 函数进行字符串连接
  let name1 = "Alice"
  let expected1 = "Hello, Alice!"
  let result1 = @azimuth.greet(name1)
  @azimuth.assert_eq_string(result1, expected1)
  
  let name2 = "Bob"
  let expected2 = "Hello, Bob!"
  let result2 = @azimuth.greet(name2)
  @azimuth.assert_eq_string(result2, expected2)
  
  let name3 = "Charlie"
  let expected3 = "Hello, Charlie!"
  let result3 = @azimuth.greet(name3)
  @azimuth.assert_eq_string(result3, expected3)
}

test "nested_arithmetic_operations" {
  // 测试嵌套算术运算
  // 计算: ((5 + 3) * 2) - (10 / 2)
  let step1 = @azimuth.add(5, 3)  // 8
  let step2 = @azimuth.multiply(step1, 2)  // 16
  let step3 = 10 / 2  // 5
  let result = @azimuth.add(step2, -step3)  // 11
  
  @azimuth.assert_eq(result, 11)
}

test "alternating_addition_subtraction" {
  // 测试交替加减法
  let start = 100
  let op1 = @azimuth.add(start, 10)    // 110
  let op2 = @azimuth.add(op1, -5)      // 105
  let op3 = @azimuth.add(op2, 20)      // 125
  let op4 = @azimuth.add(op3, -15)     // 110
  let result = @azimuth.add(op4, 30)   // 140
  
  @azimuth.assert_eq(result, 140)
}

test "mathematical_series_summation" {
  // 测试数学级数求和
  // 计算 1 + 2 + 3 + ... + 10
  let sum1 = @azimuth.add(0, 1)   // 1
  let sum2 = @azimuth.add(sum1, 2)  // 3
  let sum3 = @azimuth.add(sum2, 3)  // 6
  let sum4 = @azimuth.add(sum3, 4)  // 10
  let sum5 = @azimuth.add(sum4, 5)  // 15
  let sum6 = @azimuth.add(sum5, 6)  // 21
  let sum7 = @azimuth.add(sum6, 7)  // 28
  let sum8 = @azimuth.add(sum7, 8)  // 36
  let sum9 = @azimuth.add(sum8, 9)  // 45
  let sum10 = @azimuth.add(sum9, 10) // 55
  
  @azimuth.assert_eq(sum10, 55)
}

test "exponential_growth_simulation" {
  // 测试指数增长模拟
  // 模拟细菌增长：每小时翻倍，共3小时
  let initial_count = 100
  let hour1 = @azimuth.multiply(initial_count, 2)  // 200
  let hour2 = @azimuth.multiply(hour1, 2)          // 400
  let hour3 = @azimuth.multiply(hour2, 2)          // 800
  
  @azimuth.assert_eq(hour3, 800)
}

test "string_greeting_with_numbers" {
  // 测试包含数字的字符串问候
  let input1 = "123"
  let expected1 = "Hello, 123!"
  let result1 = @azimuth.greet(input1)
  @azimuth.assert_eq_string(result1, expected1)
  
  let input2 = "user456"
  let expected2 = "Hello, user456!"
  let result2 = @azimuth.greet(input2)
  @azimuth.assert_eq_string(result2, expected2)
  
  let input3 = "test_789"
  let expected3 = "Hello, test_789!"
  let result3 = @azimuth.greet(input3)
  @azimuth.assert_eq_string(result3, expected3)
}

test "factorial_calculation_simulation" {
  // 测试阶乘计算模拟
  // 计算 5! = 5 * 4 * 3 * 2 * 1 = 120
  let step1 = @azimuth.multiply(1, 1)   // 1
  let step2 = @azimuth.multiply(step1, 2)  // 2
  let step3 = @azimuth.multiply(step2, 3)  // 6
  let step4 = @azimuth.multiply(step3, 4)  // 24
  let step5 = @azimuth.multiply(step4, 5)  // 120
  
  @azimuth.assert_eq(step5, 120)
}

test "compound_interest_calculation" {
  // 测试复利计算
  // 本金1000，年利率5%，3年复利
  let principal = 1000
  let rate = 5  // 5%
  
  // 第一年
  let interest1 = @azimuth.multiply(principal, rate) / 100  // 50
  let year1 = @azimuth.add(principal, interest1)           // 1050
  
  // 第二年
  let interest2 = @azimuth.multiply(year1, rate) / 100     // 52
  let year2 = @azimuth.add(year1, interest2)              // 1102
  
  // 第三年
  let interest3 = @azimuth.multiply(year2, rate) / 100     // 55
  let year3 = @azimuth.add(year2, interest3)              // 1157
  
  @azimuth.assert_eq(year3, 1157)
}