// 增强测试用例 - 使用标准 MoonBit 测试语法
// 包含8个测试用例，涵盖各种实际应用场景

test "arith_progression_sum" {
  // 计算等差数列求和：1 + 4 + 7 + 10 + 13
  // 使用公式：n/2 * (2a1 + (n-1)d)
  let n = 5
  let first_term = 1
  let difference = 3
  
  // 计算总和
  let sum = multiply(n, add(multiply(2, first_term), multiply(n - 1, difference))) / 2
  assert_eq(35, sum)
}

test "string_processing_advanced" {
  // 测试复杂的字符串处理场景
  let username = "john_doe"
  let domain = "example.com"
  let email = username + "@" + domain
  
  // 使用greet函数处理完整的邮箱地址
  let greeting = greet(email)
  assert_eq_string("Hello, john_doe@example.com!", greeting)
  
  // 测试包含数字和特殊字符的字符串
  let complex_id = "user_2024@project#123"
  let complex_greeting = greet(complex_id)
  assert_eq_string("Hello, user_2024@project#123!", complex_greeting)
}

test "business_calculations" {
  // 商业计算：利润计算
  let revenue = 15000
  let cost = 8500
  let tax_rate = 15  // 15%
  
  // 计算利润（收入 - 成本）
  let profit = add(revenue, -cost)
  assert_eq(6500, profit)
  
  // 计算税后利润
  let tax_amount = multiply(profit, tax_rate) / 100
  let net_profit = add(profit, -tax_amount)
  assert_eq(5525, net_profit)  // 6500 - 6500*15/100 = 5525
}

test "geometry_perimeter" {
  // 几何计算：计算梯形的周长
  let base1 = 12
  let base2 = 8
  let side1 = 5
  let side2 = 7
  
  // 计算周长：base1 + base2 + side1 + side2
  let perimeter = add(add(add(base1, base2), side1), side2)
  assert_eq(32, perimeter)
  
  // 计算梯形面积（简化版）：(base1 + base2) * height / 2
  let height = 6
  let area = multiply(add(base1, base2), height) / 2
  assert_eq(60, area)
}

test "time_zone_conversion" {
  // 时区转换计算
  let local_time = 14  // 下午2点（24小时制）
  let time_difference = -8  // 与UTC的时差（小时）
  
  // 计算UTC时间
  let utc_time_raw = add(local_time, -time_difference)
  // 处理跨日情况（使用模运算，简化为条件判断）
  let utc_time = if (utc_time_raw >= 24) { 
    add(utc_time_raw, -24) 
  } else if (utc_time_raw < 0) { 
    add(utc_time_raw, 24) 
  } else { 
    utc_time_raw 
  }
  
  assert_eq(22, utc_time)  // 14 - (-8) = 22，即晚上10点UTC
}

test "statistical_variance" {
  // 统计计算：计算方差（简化版）
  // 数据集：5, 8, 12, 15, 20
  let values = [5, 8, 12, 15, 20]
  
  // 计算平均值
  let sum = add(add(add(add(5, 8), 12), 15), 20)
  let mean = sum / 5
  assert_eq(12, mean)
  
  // 计算离差平方和（简化计算）
  let diff1 = add(5, -mean)
  let diff2 = add(8, -mean)
  let diff3 = add(12, -mean)
  let diff4 = add(15, -mean)
  let diff5 = add(20, -mean)
  
  let squared_diff_sum = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), 
                                   multiply(diff3, diff3)), multiply(diff4, diff4)), 
                                   multiply(diff5, diff5))
  
  // 计算方差
  let variance = squared_diff_sum / 5
  assert_eq(26, variance)  // 简化计算结果
}

test "physics_kinematics" {
  // 物理计算：运动学公式
  // 公式：s = ut + (1/2)at^2
  // 简化为整数计算：s = ut + at^2/2
  let initial_velocity = 10  // m/s
  let acceleration = 2      // m/s²
  let time = 5              // s
  
  // 计算位移
  let ut = multiply(initial_velocity, time)
  let half_at_squared = multiply(acceleration, multiply(time, time)) / 2
  let displacement = add(ut, half_at_squared)
  
  assert_eq(75, displacement)  // 10*5 + 2*25/2 = 50 + 25 = 75
}

test "encryption_simple" {
  // 简单的加密算法模拟（凯撒密码变体）
  let original_number = 1234
  let shift_key = 7
  let modulo_base = 10000  // 4位数字
  
  // 加密：(original + shift) mod 10000
  let encrypted = add(original_number, shift_key)
  if (encrypted >= modulo_base) {
    encrypted = add(encrypted, -modulo_base)
  }
  
  assert_eq(1241, encrypted)
  
  // 解密：(encrypted - shift + 10000) mod 10000
  let decrypted_raw = add(encrypted, -shift_key)
  let decrypted = if (decrypted_raw < 0) { 
    add(decrypted_raw, modulo_base) 
  } else { 
    decrypted_raw 
  }
  
  assert_eq(original_number, decrypted)
}