// 数学基础性质测试

// 测试加法结合律：(a + b) + c = a + (b + c)
///|
test "addition_associative_property" {
  let a = 100
  let b = 200
  let c = 300
  
  // (a + b) + c
  let left = @azimuth.add(@azimuth.add(a, b), c)
  // a + (b + c)
  let right = @azimuth.add(a, @azimuth.add(b, c))
  
  assert_eq(left, right)
  
  // 测试负数情况
  let x = -50
  let y = -75
  let z = 125
  
  assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
}

// 测试乘法结合律：(a * b) * c = a * (b * c)
///|
test "multiplication_associative_property" {
  let a = 10
  let b = 20
  let c = 30
  
  // (a * b) * c
  let left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  // a * (b * c)
  let right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  
  assert_eq(left, right)
  
  // 测试包含负数的情况
  let x = -5
  let y = 8
  let z = -3
  
  assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
}

// 测试加法单位元：a + 0 = a
///|
test "addition_identity_element" {
  assert_eq(42, @azimuth.add(42, 0))
  assert_eq(-17, @azimuth.add(-17, 0))
  assert_eq(0, @azimuth.add(0, 0))
  assert_eq(2147483647, @azimuth.add(2147483647, 0))
  assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

// 测试乘法单位元：a * 1 = a
///|
test "multiplication_identity_element" {
  assert_eq(42, @azimuth.multiply(42, 1))
  assert_eq(-17, @azimuth.multiply(-17, 1))
  assert_eq(0, @azimuth.multiply(0, 1))
  assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
}

// 测试乘法零元：a * 0 = 0
///|
test "multiplication_zero_element" {
  assert_eq(0, @azimuth.multiply(42, 0))
  assert_eq(0, @azimuth.multiply(-17, 0))
  assert_eq(0, @azimuth.multiply(0, 0))
  assert_eq(0, @azimuth.multiply(2147483647, 0))
  assert_eq(0, @azimuth.multiply(-2147483648, 0))
}

// 实际应用：银行账户余额计算
///|
test "bank_account_balance_calculation" {
  // 初始余额
  let initial_balance = 1000
  
  // 存款和取款
  let deposit1 = 500
  let deposit2 = 300
  let withdrawal1 = -200
  let withdrawal2 = -150
  
  // 计算最终余额
  let final_balance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(initial_balance, deposit1), deposit2), withdrawal1), withdrawal2)
  
  assert_eq(1450, final_balance)
}

// 实际应用：温度转换（摄氏度到华氏度）
///|
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 由于整数除法，我们使用：F = C * 9 + 160，然后除以5（这里简化为整数运算）
  
  let celsius = 25
  // 先计算 C * 9
  let step1 = @azimuth.multiply(celsius, 9)
  // 再加 160
  let step2 = @azimuth.add(step1, 160)
  
  // 25°C = (25 * 9 + 160) / 5 = 385 / 5 = 77°F
  // 这里我们只检查中间步骤，因为整数除法不在我们的函数范围内
  assert_eq(385, step2)
  
  // 测试冰点：0°C
  let freezing = @azimuth.add(@azimuth.multiply(0, 9), 160)
  assert_eq(160, freezing) // 32°F * 5 = 160
}

// 实际应用：计算平均数
///|
test "average_calculation" {
  // 计算一组数的平均数：(a + b + c) / 3
  // 由于除法不在函数范围内，我们只测试求和部分
  
  let numbers = [80, 90, 85, 75, 95]
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(numbers[0], numbers[1]), numbers[2]), numbers[3]), numbers[4])
  
  assert_eq(425, sum) // 80 + 90 + 85 + 75 + 95 = 425
}

// 实际应用：计算复利（简化版）
///|
test "compound_interest_calculation" {
  // 本金
  let principal = 1000
  
  // 年利率（百分比）
  let rate_percent = 5
  
  // 第一年后的金额：principal + principal * rate_percent / 100
  // 简化为：principal + principal * rate / 100（这里简化计算）
  
  // 计算利息：principal * 5%
  let interest = @azimuth.multiply(principal, 5)
  // 简化处理：除以100不在函数范围内，我们检查乘法结果
  assert_eq(5000, interest) // 1000 * 5 = 5000（实际应该是50）
}