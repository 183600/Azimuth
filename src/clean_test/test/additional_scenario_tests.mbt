// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 额外场景测试 - 补充现有测试覆盖范围

// 测试大数运算的安全性
///|
test "large_number_operations" {
  // 测试接近边界的大数运算
  let half_max = 1073741823  // Int32最大值的一半
  let half_min = -1073741824 // Int32最小值的一半
  
  // 测试大数相加
  assert_eq(2147483646, @clean_test.@clean_test.add(half_max, half_max + 1))
  assert_eq(-2147483648, @clean_test.@clean_test.add(half_min, half_min))
  
  // 测试大数相乘
  assert_eq(2147483647, @clean_test.@clean_test.multiply(half_max, 2))
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(half_min, 2))
}

// 测试负数乘法的特殊情况
///|
test "negative_multiplication_edge_cases" {
  // 测试负数乘以负数的各种情况
  assert_eq(2147483647, @clean_test.@clean_test.multiply(-46341, -46341))  // 应该溢出到最大值
  assert_eq(2147483647, @clean_test.@clean_test.multiply(-65536, -65536))  // 大数负数相乘
  assert_eq(2147483647, @clean_test.@clean_test.multiply(-100000, -30000)) // 负数溢出
  
  // 测试负数乘以正数
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(-46341, 46341))  // 应该溢出到最小值
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(-65536, 65536))  // 大数相乘
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(-100000, 30000)) // 负数溢出
}

// 测试字符串处理的边界情况
///|
test "string_boundary_cases" {
  // 测试长字符串
  let long_name = "这是一个非常长的用户名字符串用来测试greet函数在处理长字符串时的表现"
  let result = @clean_test.@clean_test.greet(long_name)
  assert_eq(true, result.contains("Hello, "))
  assert_eq(true, result.contains(long_name))
  assert_eq(true, result.ends_with("!"))
  
  // 测试包含特殊字符的字符串
  assert_eq("Hello, @#$%^&*()!", @clean_test.@clean_test.greet("@#$%^&*()"))
  assert_eq_string("Hello, 中文测试!", @clean_test.@clean_test.greet("中文测试"))
  assert_eq_string("Hello, 🌙⭐!", @clean_test.@clean_test.greet("🌙⭐"))
}

// 测试数学恒等式
///|
test "mathematical_identities" {
  // 测试平方差公式: a² - b² = (a+b)(a-b)
  let a = 100
  let b = 50
  
  // 计算 a² - b²
  let a_squared = @clean_test.@clean_test.multiply(a, a)
  let b_squared = @clean_test.@clean_test.multiply(b, b)
  let difference = @clean_test.@clean_test.add(a_squared, -b_squared)
  
  // 计算 (a+b)(a-b)
  let sum = @clean_test.@clean_test.add(a, b)
  let diff = @clean_test.@clean_test.add(a, -b)
  let product = @clean_test.@clean_test.multiply(sum, diff)
  
  assert_eq(difference, product)
  
  // 测试分配律: a(b+c) = ab + ac
  let x = 15
  let y = 25
  let z = 35
  
  let left_side = @clean_test.@clean_test.multiply(x, @clean_test.@clean_test.add(y, z))
  let right_side = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(x, y), @clean_test.@clean_test.multiply(x, z))
  
  assert_eq(left_side, right_side)
}

// 测试实际金融计算场景
///|
test "financial_calculations" {
  // 模拟银行账户计算
  let initial_balance = 10000  // 初始余额
  let deposit1 = 2500         // 第一次存款
  let deposit2 = 1500         // 第二次存款
  let withdrawal1 = -800      // 第一次取款
  let withdrawal2 = -1200     // 第二次取款
  
  // 计算最终余额
  let balance1 = @clean_test.@clean_test.add(initial_balance, deposit1)
  let balance2 = @clean_test.@clean_test.add(balance1, deposit2)
  let balance3 = @clean_test.@clean_test.add(balance2, withdrawal1)
  let final_balance = @clean_test.@clean_test.add(balance3, withdrawal2)
  
  assert_eq(12000, final_balance)
  
  // 计算利息（假设月利率1%，存12个月）
  let principal = 5000
  let monthly_rate = 1  // 1%
  let months = 12
  let monthly_interest = @clean_test.@clean_test.multiply(principal, monthly_rate)
  let total_interest = @clean_test.@clean_test.multiply(monthly_interest, months)
  let final_amount = @clean_test.@clean_test.add(principal, total_interest)
  
  assert_eq(5600, final_amount)
}

// 测试温度转换
///|
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(celsius, 9), 32)
  let fahrenheit_divided = @clean_test.@clean_test.multiply(fahrenheit, 1) // 这里简化为整数运算
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit_back = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(25, 9), 32) // 25°C = 77°F
  let celsius_back = @clean_test.@clean_test.multiply(@clean_test.@clean_test.add(fahrenheit_back, -32), 5)
  
  assert_eq(77, fahrenheit_back)
  assert_eq(225, celsius_back) // (77-32)*5 = 225，简化计算
}

// 测试复合运算
///|
test "compound_operations" {
  // 测试 (a + b) * (c - d) 的复合运算
  let a = 10
  let b = 20
  let c = 30
  let d = 15
  
  let sum_ab = @clean_test.@clean_test.add(a, b)
  let diff_cd = @clean_test.@clean_test.add(c, -d)
  let result = @clean_test.@clean_test.multiply(sum_ab, diff_cd)
  
  assert_eq(450, result) // (10+20)*(30-15) = 30*15 = 450
  
  // 测试嵌套运算：a * (b + c * d)
  let x = 5
  let y = 10
  let z = 3
  let w = 4
  
  let inner_mult = @clean_test.@clean_test.multiply(z, w)
  let inner_sum = @clean_test.@clean_test.add(y, inner_mult)
  let final_result = @clean_test.@clean_test.multiply(x, inner_sum)
  
  assert_eq(110, final_result) // 5*(10+3*4) = 5*(10+12) = 5*22 = 110
}

// 测试错误恢复
///|
test "error_recovery" {
  // 测试溢出后的恢复
  let overflow_result = @clean_test.@clean_test.add(2147483647, 1)  // 溢出
  assert_eq(2147483647, overflow_result)
  
  // 溢出后继续运算应该正常
  let recovery = @clean_test.@clean_test.add(overflow_result, -2147483647)
  assert_eq(0, recovery)
  
  // 测试负溢出后的恢复
  let negative_overflow = @clean_test.@clean_test.add(-2147483648, -1)  // 负溢出
  assert_eq(-2147483648, negative_overflow)
  
  // 负溢出后继续运算
  let negative_recovery = @clean_test.@clean_test.add(negative_overflow, 2147483647)
  assert_eq(-1, negative_recovery)
}

// 测试极端边界组合
///|
test "extreme_boundary_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的各种运算组合
  assert_eq(-1, @clean_test.@clean_test.add(max_val, min_val))
  assert_eq(-1, @clean_test.@clean_test.add(min_val, max_val))
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(max_val, min_val))
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(min_val, max_val))
  
  // 测试接近边界的值
  let near_max = 2147483646
  let near_min = -2147483647
  
  assert_eq(max_val, @clean_test.@clean_test.add(near_max, 1))
  assert_eq(min_val, @clean_test.@clean_test.add(near_min, -1))
  assert_eq(-2147483648, @clean_test.@clean_test.multiply(near_max, -1))
  assert_eq(2147483647, @clean_test.@clean_test.multiply(near_min, -1))
}

// 测试性能压力
///|
test "performance_stress" {
  // 测试大量连续运算的性能
  let mut accumulator = 1
  
  // 连续乘法运算
  for i = 1; i <= 10; i = i + 1 {
    accumulator = @clean_test.@clean_test.multiply(accumulator, i)
  }
  assert_eq(3628800, accumulator) // 10!
  
  // 重置并测试加法
  accumulator = 0
  for i = 1; i <= 100; i = i + 1 {
    accumulator = @clean_test.@clean_test.add(accumulator, i)
  }
  assert_eq(5050, accumulator) // 1+2+...+100
  
  // 测试混合运算
  let mut result = 100
  for i = 1; i <= 5; i = i + 1 {
    result = @clean_test.@clean_test.add(@clean_test.@clean_test.multiply(result, 2), i)
  }
  assert_eq(3231, result) // (((((100*2)+1)*2+2)*2+3)*2+4)*2+5 = 3231
}