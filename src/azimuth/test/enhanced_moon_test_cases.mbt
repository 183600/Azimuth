// 增强的 MoonBit 测试用例
// 覆盖更多实际应用场景和边界情况

test "advanced_arithmetic_sequence" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13
  let first_term = 1
  let common_diff = 3
  let num_terms = 5
  
  // 使用公式：和 = n × (首项 + 末项) / 2
  let last_term = add(first_term, multiply(common_diff, num_terms - 1))  // 1 + 3×4 = 13
  let sum = multiply(num_terms, add(first_term, last_term)) / 2  // 5 × (1+13) / 2 = 35
  
  assert_eq(35, sum)
  
  // 手动验证
  let manual_sum = add(add(add(add(1, 4), 7), 10), 13)
  assert_eq(35, manual_sum)
}

test "geometric_progression_calculation" {
  // 测试等比数列：2, 6, 18, 54, 162
  let first_term = 2
  let ratio = 3
  let num_terms = 5
  
  // 手动计算前5项
  let term1 = first_term
  let term2 = multiply(term1, ratio)  // 6
  let term3 = multiply(term2, ratio)  // 18
  let term4 = multiply(term3, ratio)  // 54
  let term5 = multiply(term4, ratio)  // 162
  
  let sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(242, sum)
  
  // 验证最后一项
  assert_eq(162, term5)
}

test "complex_string_formatting" {
  // 测试复杂字符串格式化场景
  let user_name = "张三"
  let user_id = "10086"
  let department = "技术部"
  
  // 模拟用户信息格式化
  let greeting = greet(user_name)
  let user_info = greeting + " ID:" + user_id + " 部门:" + department
  
  assert_eq_string("Hello, 张三! ID:10086 部门:技术部", user_info)
  
  // 测试包含特殊字符的用户名
  let special_user = "user@company.com"
  let special_greeting = greet(special_user)
  assert_eq_string("Hello, user@company.com!", special_greeting)
}

test "boundary_stress_testing" {
  // 边界值压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续边界运算
  let result1 = add(add(max_val, -1), add(min_val, 1))  // (max-1) + (min+1) = -2 + 0 = -2
  assert_eq(-2, result1)
  
  // 测试边界值的乘法组合
  let result2 = multiply(multiply(max_val, 0), multiply(min_val, 0))  // 0 × 0 = 0
  assert_eq(0, result2)
  
  // 测试边界值与普通值的混合运算
  let result3 = add(multiply(100, max_val), multiply(-100, min_val))  // 溢出保护测试
  assert_eq(max_val, add(multiply(100, max_val), 0))  // 验证溢出保护
}

test "real_world_inventory_management" {
  // 实际库存管理场景测试
  let initial_stock = 1000
  let received = 500
  let sold = 300
  let returned = 50
  let damaged = 20
  
  // 计算最终库存
  let after_receiving = add(initial_stock, received)  // 1500
  let after_sales = add(after_receiving, -sold)      // 1200
  let after_returns = add(after_sales, returned)     // 1250
  let final_stock = add(after_returns, -damaged)     // 1230
  
  assert_eq(1230, final_stock)
  
  // 计算库存周转率（简化）
  let total_sold = add(sold, -returned)  // 250
  let turnover_rate = multiply(total_sold, 100) / initial_stock  // 25%
  assert_eq(25, turnover_rate)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试：O(n²) vs O(n log n)
  let n = 100
  
  // 模拟O(n²)算法的操作数
  let on2_operations = multiply(n, n)  // 10000
  
  // 模拟O(n log n)算法的操作数（简化log计算）
  let log_n_approx = 7  // log₂(100) ≈ 6.64，取整为7
  let onlogn_operations = multiply(n, log_n_approx)  // 700
  
  assert_eq(10000, on2_operations)
  assert_eq(700, onlogn_operations)
  
  // 计算性能提升倍数
  let improvement_factor = on2_operations / onlogn_operations
  assert_eq(14, improvement_factor)  // 约14倍提升
}

test "data_validation_scenarios" {
  // 数据验证场景测试
  let input_value = 42
  let min_allowed = 10
  let max_allowed = 100
  
  // 验证输入是否在允许范围内
  let is_valid = add(input_value, -min_allowed) >= 0 && add(max_allowed, -input_value) >= 0
  assert_true(is_valid)
  
  // 测试边界值
  let at_min = add(min_allowed, -min_allowed) >= 0 && add(max_allowed, -min_allowed) >= 0
  assert_true(at_min)
  
  let at_max = add(max_allowed, -min_allowed) >= 0 && add(max_allowed, -max_allowed) >= 0
  assert_true(at_max)
  
  // 测试超出范围的值
  let too_small = add(min_allowed - 1, -min_allowed) >= 0 && add(max_allowed, -(min_allowed - 1)) >= 0
  assert_false(too_small)
  
  let too_large = add(max_allowed + 1, -min_allowed) >= 0 && add(max_allowed, -(max_allowed + 1)) >= 0
  assert_false(too_large)
}

test "engineering_calculations" {
  // 工程计算测试：梁的弯曲应力（简化）
  let force = 1000  // 力 (N)
  let length = 10    // 长度 (m)
  let moment_of_inertia = 50  // 惯性矩 (m⁴)
  
  // 简化的弯曲应力计算：σ = M × y / I
  // 其中 M = F × L（弯矩），y = L/2（到中性轴的距离）
  let moment = multiply(force, length)  // 10000 N·m
  let distance = length / 2  // 5 m
  let stress = multiply(moment, distance) / moment_of_inertia  // 10000 × 5 / 50 = 1000
  
  assert_eq(1000, stress)
  
  // 测试不同参数的影响
  let double_force = multiply(force * 2, length) * (length / 2) / moment_of_inertia
  assert_eq(2000, double_force)
  
  let double_length = multiply(force, length * 2) * ((length * 2) / 2) / moment_of_inertia
  assert_eq(4000, double_length)
}

test "cryptography_simulation" {
  // 密码学模拟测试：简化的凯撒密码
  let message = "HELLO"
  let shift = 3
  
  // 由于我们只处理数字，这里模拟数字版本的凯撒密码
  let numeric_value = 12345
  let shifted_value = add(numeric_value, shift)  // 12348
  
  assert_eq(12348, shifted_value)
  
  // 解密过程
  let decrypted_value = add(shifted_value, -shift)  // 12345
  assert_eq(12345, decrypted_value)
  
  // 测试循环移位（模拟字母表循环）
  let alphabet_size = 26
  let letter_position = 25  // 'Z'
  let shifted_position = add(letter_position, shift)  // 28
  let wrapped_position = shifted_position % alphabet_size  // 2，对应'C'
  
  assert_eq(2, wrapped_position)
}

test "physics_simulation" {
  // 物理模拟测试：自由落体运动（简化）
  let initial_velocity = 0  // 初速度
  let gravity = 10  // 重力加速度 (m/s²，简化值)
  let time = 5  // 时间 (s)
  
  // 位移公式：s = v₀t + ½gt²
  // 简化计算：s = gt²/2（因为v₀=0）
  let time_squared = multiply(time, time)  // 25
  let distance = multiply(gravity, time_squared) / 2  // 10 × 25 / 2 = 125
  
  assert_eq(125, distance)
  
  // 计算末速度：v = v₀ + gt
  let final_velocity = add(initial_velocity, multiply(gravity, time))  // 0 + 10×5 = 50
  assert_eq(50, final_velocity)
  
  // 计算平均速度：v_avg = (v₀ + v) / 2
  let average_velocity = add(initial_velocity, final_velocity) / 2  // 25
  assert_eq(25, average_velocity)
  
  // 验证：距离 = 平均速度 × 时间
  let distance_check = multiply(average_velocity, time)  // 25 × 5 = 125
  assert_eq(distance, distance_check)
}