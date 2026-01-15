// 新的 MoonBit 测试用例
// 覆盖 azimuth 项目的高级功能和边界情况

test "advanced_arithmetic_sequences" {
  // 测试等差数列求和：1 + 4 + 7 + ... + 28（前10项）
  let first_term = 1
  let common_diff = 3
  
  // 使用公式：S = n/2 × (2a₁ + (n-1)d)
  let sum = multiply(10, add(multiply(2, first_term), multiply(9, common_diff))) / 2
  assert_eq(145, sum)
}

test "geometric_progression_calculation" {
  // 测试等比数列求和：2 + 6 + 18 + 54（前4项）
  // 手动计算验证
  let term1 = 2
  let term2 = multiply(term1, 3)
  let term3 = multiply(term2, 3)
  let term4 = multiply(term3, 3)
  let sum = add(add(add(term1, term2), term3), term4)
  
  assert_eq(80, sum)
}

test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款分期付款
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 30年
  
  // 简化的月供计算：本金 / (年数 × 12) + 利息部分
  let total_months = multiply(years, 12)
  let monthly_principal = principal / total_months
  let monthly_interest = multiply(principal, annual_rate) / multiply(100, 12)
  let monthly_payment = add(monthly_principal, monthly_interest)
  
  assert_eq(777, monthly_payment)  // 简化计算结果
}

test "physics_motion_calculation" {
  // 测试物理运动计算：自由落体运动
  let initial_velocity = 0      // 初速度
  let gravity = 10              // 重力加速度（简化值）
  let time = 5                  // 时间
  
  // 位移公式：s = v₀t + ½gt²
  let distance = add(multiply(initial_velocity, time), multiply(multiply(gravity, time), time) / 2)
  assert_eq(125, distance)
  
  // 末速度公式：v = v₀ + gt
  let final_velocity = add(initial_velocity, multiply(gravity, time))
  assert_eq(50, final_velocity)
}

test "data_structure_operations" {
  // 测试数据结构操作：模拟栈操作
  let stack_size = 10
  let push_operations = 7
  let pop_operations = 3
  
  // 计算最终栈大小
  let final_size = add(stack_size, add(push_operations, -pop_operations))
  assert_eq(14, final_size)
  
  // 模拟栈顶元素访问
  let top_element_index = add(final_size, -1)
  assert_eq(13, top_element_index)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟：O(n²)算法的时间复杂度
  let n = 100
  
  // 模拟嵌套循环的操作次数：n × n
  let operations = multiply(n, n)
  assert_eq(10000, operations)
  
  // 模拟O(n log n)算法的操作次数（简化）
  let log_n_approx = 7  // log₂100 ≈ 6.64，取整为7
  let n_log_n_operations = multiply(n, log_n_approx)
  assert_eq(700, n_log_n_operations)
}

test "statistical_analysis" {
  // 测试统计分析：方差计算
  let n = 5
  
  // 计算平均值
  let sum = add(add(add(add(10, 15), 20), 25), 30)
  let mean = sum / n
  assert_eq(20, mean)
  
  // 计算方差（简化版）
  let diff1 = add(10, -mean)
  let diff2 = add(15, -mean)
  let diff3 = add(20, -mean)
  let diff4 = add(25, -mean)
  let diff5 = add(30, -mean)
  
  let squared_diff1 = multiply(diff1, diff1)
  let squared_diff2 = multiply(diff2, diff2)
  let squared_diff3 = multiply(diff3, diff3)
  let squared_diff4 = multiply(diff4, diff4)
  let squared_diff5 = multiply(diff5, diff5)
  
  let squared_diff_sum = add(add(add(add(squared_diff1, squared_diff2), squared_diff3), add(squared_diff4, squared_diff5))
  let variance = squared_diff_sum / n
  
  assert_eq(50, variance)
}

test "cryptography_basic_simulation" {
  // 测试密码学基础模拟：简单凯撒密码
  let plaintext_value = 65  // 'A'的ASCII码
  let shift = 3
  let alphabet_size = 26
  
  // 加密：(明文 + 位移) mod 字母表大小
  let encrypted = add(plaintext_value, shift)
  let threshold = add(65, alphabet_size)
  let normalized = if encrypted >= threshold { 
    add(encrypted, -alphabet_size) 
  } else { 
    encrypted 
  }
  
  assert_eq(68, normalized)  // 'D'的ASCII码
  
  // 解密：(密文 - 位移) mod 字母表大小
  let decrypted = add(normalized, -shift)
  assert_eq(65, decrypted)  // 回到'A'
}

test "network_protocol_simulation" {
  // 测试网络协议模拟：TCP窗口大小计算
  let segment_size = 1460  // 标准TCP段大小
  let window_scale_factor = 4
  let initial_window = 65535  // 64KB
  
  // 计算扩展窗口大小
  let scaled_window = multiply(initial_window, window_scale_factor)
  assert_eq(262140, scaled_window)
  
  // 计算可以发送的段数
  let segments_per_window = scaled_window / segment_size
  assert_eq(179, segments_per_window)
}

test "machine_learning_basic" {
  // 测试机器学习基础：线性回归预测
  let weight = 2        // 权重
  let bias = 10         // 偏置
  let input = 15        // 输入特征
  
  // 预测值：y = wx + b
  let prediction = add(multiply(weight, input), bias)
  assert_eq(40, prediction)
  
  // 计算损失（均方误差）
  let actual = 38
  let error = add(prediction, -actual)
  let squared_error = multiply(error, error)
  assert_eq(4, squared_error)
}