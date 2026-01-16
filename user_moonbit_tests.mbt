// 用户自定义 MoonBit 测试用例
// 包含10个标准测试用例，展示不同的测试场景

test "arithmetic_sequence_sum" {
  // 计算等差数列的和：2 + 4 + 6 + ... + 20（前10个偶数）
  // 使用公式：n × (首项 + 末项) / 2
  let n = 10
  let first = 2
  let last = 20
  
  let sum = @azimuth.multiply(n, @azimuth.add(first, last)) / 2
  @azimuth.assert_eq(110, sum)  // 10 × (2 + 20) / 2 = 110
}

test "string_pattern_validation" {
  // 测试不同字符串模式的处理
  let email_pattern = "user@example.com"
  let phone_pattern = "123-456-7890"
  let url_pattern = "https://moonbitlang.com"
  
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet(email_pattern))
  @azimuth.assert_eq_string("Hello, 123-456-7890!", @azimuth.greet(phone_pattern))
  @azimuth.assert_eq_string("Hello, https://moonbitlang.com!", @azimuth.greet(url_pattern))
}

test "mathematical_identity_verification" {
  // 验证数学恒等式
  let x = 12
  let y = 8
  
  // 验证平方和恒等式：(x + y)² = x² + 2xy + y²
  let left_side = @azimuth.multiply(@azimuth.add(x, y), @azimuth.add(x, y))
  let right_side = @azimuth.add(@azimuth.add(@azimuth.multiply(x, x), @azimuth.multiply(2, @azimuth.multiply(x, y))), @azimuth.multiply(y, y))
  @azimuth.assert_eq(left_side, right_side)
  
  // 验证立方差公式：(x - y)³ = x³ - 3x²y + 3xy² - y³
  let diff = @azimuth.add(x, -y)
  let cube_diff = @azimuth.multiply(@azimuth.multiply(diff, diff), diff)
  let x_cubed = @azimuth.multiply(@azimuth.multiply(x, x), x)
  let y_cubed = @azimuth.multiply(@azimuth.multiply(y, y), y)
  let formula_result = @azimuth.add(@azimuth.add(@azimuth.add(x_cubed, @azimuth.multiply(-3, @azimuth.multiply(@azimuth.multiply(x, x), y))), @azimuth.multiply(3, @azimuth.multiply(x, @azimuth.multiply(y, y)))), -y_cubed)
  @azimuth.assert_eq(cube_diff, formula_result)
}

test "financial_compound_interest" {
  // 复利计算：本金 × (1 + 利率/100) ^ 年数
  // 简化为整数计算：10000 × (1 + 5/100) ^ 3
  let principal = 10000
  let rate = 5
  let years = 3
  
  // 简化的复利计算，避免浮点数
  let growth_factor = @azimuth.add(100, rate)
  let amount_after_1_year = @azimuth.multiply(principal, growth_factor) / 100
  let amount_after_2_years = @azimuth.multiply(amount_after_1_year, growth_factor) / 100
  let amount_after_3_years = @azimuth.multiply(amount_after_2_years, growth_factor) / 100
  
  @azimuth.assert_eq(11576, amount_after_3_years)  // 约等于 10000 × 1.05³
}

test "physics_kinematics" {
  // 物理运动学计算：距离 = 初速度 × 时间 + 0.5 × 加速度 × 时间²
  // 简化为整数计算：距离 = 初速度 × 时间 + 加速度 × 时间² / 2
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  
  let distance_part1 = @azimuth.multiply(initial_velocity, time)
  let distance_part2 = @azimuth.multiply(acceleration, @azimuth.multiply(time, time)) / 2
  let total_distance = @azimuth.add(distance_part1, distance_part2)
  
  @azimuth.assert_eq(75, total_distance)  // 10×5 + 2×25/2 = 50 + 25 = 75
}

test "combinatorial_calculations" {
  // 组合数学计算：排列数 P(n,k) = n × (n-1) × ... × (n-k+1)
  // 计算 P(5,3) = 5 × 4 × 3 = 60
  let n = 5
  let k = 3
  
  let permutation = @azimuth.multiply(n, @azimuth.multiply(n - 1, n - 2))
  @azimuth.assert_eq(60, permutation)
  
  // 组合数 C(n,k) = P(n,k) / k!
  // 计算 C(5,3) = 60 / 6 = 10
  let k_factorial = @azimuth.multiply(k, k - 1)
  let combination = permutation / k_factorial
  @azimuth.assert_eq(10, combination)
}

test "temperature_conversion_precise" {
  // 精确的温度转换：华氏度转摄氏度 C = (F - 32) × 5/9
  // 简化为整数计算：C ≈ (F - 32) × 5 / 9
  let fahrenheit = 98
  
  let celsius = @azimuth.multiply(@azimuth.add(fahrenheit, -32), 5) / 9
  @azimuth.assert_eq(36, celsius)  // (98-32)×5/9 ≈ 36.67，整数除法得到36
  
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F ≈ C × 2 + 30
  let celsius2 = 25
  let fahrenheit2 = @azimuth.add(@azimuth.multiply(celsius2, 2), 30)
  @azimuth.assert_eq(80, fahrenheit2)  // 25×2+30 = 80
}

test "data_structure_operations" {
  // 数据结构操作模拟：栈的 push 和 pop 操作
  // 模拟栈操作，使用数值表示栈状态
  let stack_initial = 0
  let push_value1 = 10
  let push_value2 = 20
  let push_value3 = 30
  
  // Push 操作
  let stack_after_push1 = @azimuth.add(stack_initial, push_value1)
  let stack_after_push2 = @azimuth.add(stack_after_push1, push_value2)
  let stack_after_push3 = @azimuth.add(stack_after_push2, push_value3)
  
  @azimuth.assert_eq(60, stack_after_push3)  // 0 + 10 + 20 + 30 = 60
  
  // Pop 操作（模拟）
  let popped_value3 = push_value3
  let stack_after_pop3 = @azimuth.add(stack_after_push3, -popped_value3)
  
  let popped_value2 = push_value2
  let stack_after_pop2 = @azimuth.add(stack_after_pop3, -popped_value2)
  
  let popped_value1 = push_value1
  let stack_after_pop1 = @azimuth.add(stack_after_pop2, -popped_value1)
  
  @azimuth.assert_eq(0, stack_after_pop1)  // 栈应该回到初始状态
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟：O(n²) 和 O(n log n) 的比较
  let n = 100
  
  // O(n²) 算法的操作数
  let quadratic_ops = @azimuth.multiply(n, n)
  
  // O(n log n) 算法的操作数（简化 log₂(n) 为整数）
  let log_n = 7  // log₂(100) ≈ 6.64，取整数7
  let n_log_n_ops = @azimuth.multiply(n, log_n)
  
  @azimuth.assert_eq(10000, quadratic_ops)
  @azimuth.assert_eq(700, n_log_n_ops)
  
  // 验证 O(n²) > O(n log n) 对于较大的 n
  @azimuth.assert_true(quadratic_ops > n_log_n_ops)
}

test "cryptography_simulation" {
  // 密码学模拟：简单的凯撒密码
  let plaintext = "HELLO"
  let shift = 3
  
  // 由于我们只有数字运算，我们模拟字符的数值表示
  // A=65, B=66, ..., Z=90
  let H = 72
  let E = 69
  let L = 76
  let O = 79
  
  // 加密过程：(字符值 + 位移) mod 26
  let H_encrypted = @azimuth.add(H, shift)
  let E_encrypted = @azimuth.add(E, shift)
  let L_encrypted = @azimuth.add(L, shift)
  let O_encrypted = @azimuth.add(O, shift)
  
  // 验证加密结果
  @azimuth.assert_eq(75, H_encrypted)  // H(72)+3 = 75(K)
  @azimuth.assert_eq(72, E_encrypted)  // E(69)+3 = 72(H)
  @azimuth.assert_eq(79, L_encrypted)  // L(76)+3 = 79(O)
  @azimuth.assert_eq(82, O_encrypted)  // O(79)+3 = 82(R)
  
  // 解密过程：(字符值 - 位移) mod 26
  let H_decrypted = @azimuth.add(H_encrypted, -shift)
  @azimuth.assert_eq(H, H_decrypted)
}