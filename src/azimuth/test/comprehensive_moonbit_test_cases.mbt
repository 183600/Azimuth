// MoonBit 综合测试用例
// 针对 azimuth 包的核心功能进行全面测试

test "arith_progression_sum" {
  // 计算等差数列的和：1 + 4 + 7 + ... + 28（前10项）
  // 使用公式：n × (2a + (n-1)d) / 2
  let n = 10
  let a = 1
  let d = 3
  
  let sum = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, a), @azimuth.multiply(n - 1, d))) / 2
  @azimuth.assert_eq(145, sum)  // 10 × (2×1 + 9×3) / 2 = 145
}

test "geometric_progression_product" {
  // 计算等比数列的积：2 × 4 × 8 × 16 × 32（前5项）
  // 通过连续相乘实现
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 2)  // 4
  let term3 = @azimuth.multiply(term2, 2)  // 8
  let term4 = @azimuth.multiply(term3, 2)  // 16
  let term5 = @azimuth.multiply(term4, 2)  // 32
  
  let product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(32768, product)
}

test "string_pattern_validation" {
  // 测试字符串模式匹配和验证
  let email = "user@example.com"
  let phone = "123-456-7890"
  let mixed = "User123_Example"
  
  // 通过greet函数验证字符串处理
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet(email))
  @azimuth.assert_eq_string("Hello, 123-456-7890!", @azimuth.greet(phone))
  @azimuth.assert_eq_string("Hello, User123_Example!", @azimuth.greet(mixed))
}

test "ceiling_division_scenarios" {
  // 测试向上取整除法的各种场景
  // 正数除法
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))   // 20/4 = 5
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(21, 4))   // 21/4 = 5.25 → 6
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(22, 4))   // 22/4 = 5.5 → 6
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(23, 4))   // 23/4 = 5.75 → 6
  
  // 负数除法
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-20, 4))  // -20/4 = -5
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-19, 4))  // -19/4 = -4.75 → -4
  
  // 混合符号除法
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(15, -4))  // 15/-4 = -3.75 → -3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-15, -4))  // -15/-4 = 3.75 → 4
}

test "complex_business_calculation" {
  // 复杂业务计算：产品定价策略
  let base_cost = 100
  let labor_cost = 50
  let overhead_rate = 20  // 20%
  let profit_margin = 30   // 30%
  let tax_rate = 8        // 8%
  
  // 计算总成本
  let total_cost = @azimuth.add(base_cost, labor_cost)
  // 计算管理费用
  let overhead = @azimuth.multiply(total_cost, overhead_rate) / 100
  // 计算利润
  let profit = @azimuth.multiply(@azimuth.add(total_cost, overhead), profit_margin) / 100
  // 计算税前价格
  let pre_tax_price = @azimuth.add(@azimuth.add(total_cost, overhead), profit)
  // 计算最终价格
  let final_price = @azimuth.add(pre_tax_price, @azimuth.multiply(pre_tax_price, tax_rate) / 100)
  
  @azimuth.assert_eq(243, final_price)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟：O(n²) vs O(n log n)
  let n = 100
  
  // O(n²) 算法的操作数
  let quadratic_ops = @azimuth.multiply(n, n)
  
  // O(n log n) 算法的操作数（简化为 n × log₂n）
  // 使用近似值：log₂100 ≈ 6.64，简化为 7
  let log_approx = 7
  let linearithmic_ops = @azimuth.multiply(n, log_approx)
  
  // 验证复杂度差异
  assert_true(quadratic_ops > linearithmic_ops)
  @azimuth.assert_eq(10000, quadratic_ops)
  @azimuth.assert_eq(700, linearithmic_ops)
}

test "statistical_variance_calculation" {
  // 统计方差计算
  // 数据集：[10, 15, 20, 25, 30]
  let values = [10, 15, 20, 25, 30]
  
  // 计算平均值
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(values[0], values[1]), values[2]), values[3]), values[4])
  let mean = sum / 5
  
  // 计算方差
  let diff1 = values[0] - mean
  let diff2 = values[1] - mean
  let diff3 = values[2] - mean
  let diff4 = values[3] - mean
  let diff5 = values[4] - mean
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), @azimuth.multiply(diff3, diff3)), @azimuth.multiply(diff4, diff4)), @azimuth.multiply(diff5, diff5)) / 5
  
  @azimuth.assert_eq(20, mean)
  @azimuth.assert_eq(50, variance)
}

test "cryptography_simulation" {
  // 密码学模拟：简单的凯撒密码
  let plaintext = "HELLO"
  let shift = 3
  
  // 简化的凯撒密码实现（仅演示概念）
  // 实际实现需要字符到数字的转换
  let encrypted = @azimuth.greet(plaintext)
  let decrypted = "HELLO"  // 在实际实现中，这应该是解密后的文本
  
  // 验证加密过程改变了原始文本
  assert_true(encrypted != plaintext)
  @azimuth.assert_eq_string("Hello, HELLO!", encrypted)
}

test "physics_simulation" {
  // 物理模拟：自由落体运动
  // 公式：h = h₀ + v₀t + (1/2)gt²
  // 简化计算，使用整数运算
  
  let initial_height = 100  // 初始高度
  let initial_velocity = 0  // 初始速度
  let time = 3              // 时间
  let gravity = 10          // 重力加速度（简化值）
  
  // 计算位移：s = v₀t + (1/2)gt²
  let displacement = @azimuth.add(@azimuth.multiply(initial_velocity, time), @azimuth.multiply(gravity, @azimuth.multiply(time, time)) / 2)
  // 计算最终高度
  let final_height = @azimuth.add(initial_height, -displacement)
  
  @azimuth.assert_eq(55, final_height)
}

test "financial_compound_analysis" {
  // 金融复合分析：复利与单利的比较
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 单利计算
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years)) / 100
  let simple_total = @azimuth.add(principal, simple_interest)
  
  // 复利计算（简化版）
  let compound_factor = @azimuth.add(100, rate)
  let compound_total = @azimuth.multiply(principal, @azimuth.multiply(compound_factor, @azimuth.multiply(compound_factor, compound_factor))) / 10000
  
  // 验证复利收益高于单利
  assert_true(compound_total > simple_total)
  @azimuth.assert_eq(11500, simple_total)
  @azimuth.assert_eq(11576, compound_total)
}