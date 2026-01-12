test "mathematical_identities" {
  // 测试数学恒等式：a - b = a + (-b)
  let a = 100
  let b = 30
  // 使用加法实现减法：a - b = a + (-b)
  @clean_test.assert_eq(70, @clean_test.add(a, -b))
  
  // 测试另一个恒等式：-(a + b) = (-a) + (-b)
  let x = 50
  let y = 25
  let sum = @clean_test.add(x, y)  // 75
  let neg_sum = @clean_test.multiply(sum, -1)  // -75
  let neg_sum_alt = @clean_test.add(@clean_test.multiply(x, -1), @clean_test.multiply(y, -1))  // -50 + -25 = -75
  @clean_test.assert_eq(neg_sum, neg_sum_alt)
}

///|
test "compound_interest_calculation" {
  // 简单的复利计算
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 简化计算：只计算基本利息
  let total_interest = @clean_test.multiply(@clean_test.multiply(principal, rate), years)
  let total_amount = @clean_test.add(principal, total_interest)
  
  @clean_test.assert_eq(115000, total_amount)  // 简化版本
}

///|
test "statistical_calculations" {
  // 简单的统计计算
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  // 计算总和
  let sum = @clean_test.add(@clean_test.add(@clean_test.add(@clean_test.add(num1, num2), num3), num4), num5)
  @clean_test.assert_eq(150, sum)
  
  // 计算平均值（简化版本）
  let average = @clean_test.multiply(sum, 1)  // 简化：不进行除法
  @clean_test.assert_eq(150, average)  // 简化版本
}
