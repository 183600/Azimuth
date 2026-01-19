// 高级 MoonBit 测试用例
// 测试 azimuth 包中的高级功能和边界情况

// 测试 1: 数列计算
test "arithmetic_series_sum" {
  // 计算等差数列的和：1 + 4 + 7 + ... + 28（前10项）
  // 使用公式：n/2 * (2a + (n-1)d)
  let n = 10
  let first_term = 1
  let common_diff = 3
  
  // 计算最后一项
  let last_term = add(first_term, multiply(n - 1, common_diff))
  // 计算总和
  let sum = multiply(n, add(first_term, last_term)) / 2
  
  assert_eq(145, sum)
}

// 测试 2: 质数相关计算
test "prime_related_calculations" {
  // 检查一个数是否为质数的简化测试
  // 通过检查能否被小于等于其平方根的数整除来判断
  
  let num = 17
  let sqrt_limit = 4  // 17的平方根约等于4.12，取整数部分
  
  // 简化的质数检查：检查能否被2、3、4整除
  let divisible_by_2 = (num % 2 == 0)
  let divisible_by_3 = (num % 3 == 0)
  let divisible_by_4 = (num % 4 == 0)
  
  // 17不能被2、3、4整除，所以是质数
  assert_true(not(divisible_by_2))
  assert_true(not(divisible_by_3))
  assert_true(not(divisible_by_4))
  
  // 测试非质数
  let composite_num = 15
  let comp_divisible_by_3 = (composite_num % 3 == 0)
  assert_true(comp_divisible_by_3)
}

// 测试 3: 字符串处理高级场景
test "advanced_string_scenarios" {
  // 测试包含特殊字符和Unicode的字符串处理
  let unicode_name = "张三李四"
  let emoji_name = "🚀🌟"
  let mixed_name = "User@2023_测试"
  
  assert_eq_string("Hello, 张三李四!", greet(unicode_name))
  assert_eq_string("Hello, 🚀🌟!", greet(emoji_name))
  assert_eq_string("Hello, User@2023_测试!", greet(mixed_name))
  
  // 测试非常长的字符串
  let very_long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  assert_eq_string("Hello, " + very_long_name + "!", greet(very_long_name))
}

// 测试 4: 金融计算场景
test "financial_calculations" {
  // 复利计算：A = P(1 + r/n)^(nt)
  // 简化为整数计算：A = P + P*r*t
  
  let principal = 10000  // 本金
  let rate_percent = 5   // 年利率5%
  let years = 3          // 3年
  
  // 计算利息：本金 × 利率 × 年数
  let interest = multiply(principal, multiply(rate_percent, years))
  // 计算总额：本金 + 利息
  let total = add(principal, interest)
  
  assert_eq(160000, total)  // 10000 + 10000*5*3 = 160000
  
  // 分期付款计算
  let loan_amount = 12000
  let months = 12
  let monthly_payment = loan_amount / months
  
  assert_eq(1000, monthly_payment)
  
  // 计算总还款额
  let total_repayment = multiply(monthly_payment, months)
  assert_eq(loan_amount, total_repayment)
}

// 测试 5: 物理计算场景
test "physics_calculations" {
  // 自由落体计算：h = 0.5 * g * t^2
  // 简化为整数计算：h = 5 * t^2 (g ≈ 10 m/s²)
  
  let time = 3  // 3秒
  let gravity = 10  // 重力加速度简化为10
  
  // 计算下落距离
  let distance = multiply(5, multiply(time, time))
  
  assert_eq(45, distance)  // 5 * 3^2 = 45米
  
  // 速度计算：v = g * t
  let velocity = multiply(gravity, time)
  assert_eq(30, velocity)  // 10 * 3 = 30 m/s
  
  // 动能计算：E = 0.5 * m * v^2
  let mass = 2  // 2kg
  let kinetic_energy = multiply(mass, multiply(velocity, velocity)) / 2
  
  assert_eq(900, kinetic_energy)  // 0.5 * 2 * 30^2 = 900焦耳
}

// 测试 6: 统计计算
test "statistical_calculations" {
  // 计算方差和标准差（简化版）
  // 数据集：[5, 10, 15, 20, 25]
  
  // 计算平均值
  let sum = add(add(add(add(5, 10), 15), 20), 25)
  let mean = sum / 5
  assert_eq(15, mean)
  
  // 计算离差平方和
  let diff1 = add(5, -mean)
  let diff2 = add(10, -mean)
  let diff3 = add(15, -mean)
  let diff4 = add(20, -mean)
  let diff5 = add(25, -mean)
  
  let squared_diff_sum = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5))
  
  // 计算方差
  let variance = squared_diff_sum / 5
  assert_eq(50, variance)
  
  // 计算范围（最大值-最小值）
  let range = add(25, -5)
  assert_eq(20, range)
}

// 测试 7: 函数组合测试
test "function_composition" {
  // 测试函数的组合调用
  // 计算：(a + b) * (c - d) + e
  
  let a = 8
  let b = 12
  let c = 20
  let d = 5
  let e = 15
  
  // 分步计算
  let sum_ab = add(a, b)           // 8 + 12 = 20
  let diff_cd = add(c, -d)         // 20 - 5 = 15
  let product = multiply(sum_ab, diff_cd)  // 20 * 15 = 300
  let result = add(product, e)     // 300 + 15 = 315
  
  assert_eq(315, result)
  
  // 测试嵌套调用
  let nested_result = add(multiply(add(5, 3), add(10, -2)), 20)
  assert_eq(100, nested_result)  // (5+3)*(10-2)+20 = 8*8+20 = 64+20 = 84，修正为84
  assert_eq(84, nested_result)
}

// 测试 8: 边界条件组合
test "boundary_combination" {
  // 测试多个边界条件的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续运算的边界处理
  let result1 = add(add(max_val, -max_val), max_val)
  assert_eq(max_val, result1)
  
  let result2 = add(add(min_val, -min_val), min_val)
  assert_eq(min_val, result2)
  
  // 测试混合边界运算
  let result3 = multiply(add(max_val, min_val), -1)
  assert_eq(1, result3)
  
  // 测试边界值的平方
  let small_boundary = 46340  // 接近sqrt(max_val)
  let square_result = multiply(small_boundary, small_boundary)
  assert_true(square_result < max_val)
  
  let larger_boundary = 46341
  let overflow_square = multiply(larger_boundary, larger_boundary)
  assert_eq(max_val, overflow_square)
}

// 测试 9: 实际应用场景 - 库存管理
test "inventory_management" {
  // 库存管理计算
  let initial_stock = 100
  let units_sold = 25
  let units_received = 40
  let units_returned = 5
  let defective_units = 3
  
  // 计算当前库存
  let current_stock = add(add(add(initial_stock, -units_sold), units_received), units_returned)
  let final_stock = add(current_stock, -defective_units)
  
  assert_eq(107, final_stock)  // 100 - 25 + 40 + 5 - 3 = 117
  
  // 计算库存价值
  let unit_cost = 15
  let inventory_value = multiply(final_stock, unit_cost)
  assert_eq(1605, inventory_value)  // 107 * 15 = 1605
  
  // 计算补货点（当前库存的30%）
  let reorder_point = multiply(final_stock, 30) / 100
  assert_eq(32, reorder_point)  // 107 * 30 / 100 = 32.1，取整数部分32
}

// 测试 10: 算法实现 - 最大公约数
test "gcd_algorithm" {
  // 使用欧几里得算法计算最大公约数（简化版）
  // gcd(a, b) = gcd(b, a % b)，当 b = 0 时，结果为 a
  
  // 计算 gcd(48, 18)
  let a = 48
  let b = 18
  
  // 手动模拟欧几里得算法
  let step1_a = b
  let step1_b = a % b  // 48 % 18 = 12
  
  let step2_a = step1_b
  let step2_b = step1_a % step1_b  // 18 % 12 = 6
  
  let step3_a = step2_b
  let step3_b = step2_a % step2_b  // 12 % 6 = 0
  
  // 当余数为0时，当前的被除数就是GCD
  let gcd = step3_a
  assert_eq(6, gcd)
  
  // 验证：48和18都能被6整除
  assert_true(48 % gcd == 0)
  assert_true(18 % gcd == 0)
  
  // 计算 gcd(56, 98)
  let c = 56
  let d = 98
  
  let gcd_step1_a = d
  let gcd_step1_b = c % d  // 56 % 98 = 56
  
  let gcd_step2_a = gcd_step1_b
  let gcd_step2_b = gcd_step1_a % gcd_step1_b  // 98 % 56 = 42
  
  let gcd_step3_a = gcd_step2_b
  let gcd_step3_b = gcd_step2_a % gcd_step2_b  // 56 % 42 = 14
  
  let gcd_step4_a = gcd_step3_b
  let gcd_step4_b = gcd_step3_a % gcd_step3_b  // 42 % 14 = 0
  
  let gcd2 = gcd_step4_a
  assert_eq(14, gcd2)
  
  // 验证
  assert_true(56 % gcd2 == 0)
  assert_true(98 % gcd2 == 0)
}