// 用户新增的 MoonBit 测试用例

test "advanced_arithmetic_sequence" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13 = 35
  let a1 = 1
  let d = 3
  let n = 5
  
  // 使用等差数列求和公式：Sn = n/2 * (2a1 + (n-1)d)
  let sum = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, a1), @azimuth.multiply(n - 1, d))) / 2
  @azimuth.assert_eq(35, sum)
}

test "geometric_progression_calculation" {
  // 测试等比数列求和：2 + 6 + 18 + 54 = 80
  let a1 = 2
  let r = 3
  let n = 4
  
  // 手动计算等比数列前4项和
  let term1 = a1
  let term2 = @azimuth.multiply(term1, r)
  let term3 = @azimuth.multiply(term2, r)
  let term4 = @azimuth.multiply(term3, r)
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(80, sum)
}

test "string_pattern_processing" {
  // 测试字符串模式处理
  let pattern = "abc"
  let repeat = 3
  
  // 模拟字符串重复：abcabcabc
  let repeated = pattern + pattern + pattern
  let result = @azimuth.greet(repeated)
  @azimuth.assert_eq_string("Hello, abcabcabc!", result)
  
  // 测试包含重复模式的字符串
  let complex_pattern = "ab" + "cd" + "ef"
  let complex_result = @azimuth.greet(complex_pattern)
  @azimuth.assert_eq_string("Hello, abcdef!", complex_result)
}

test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款分期还款
  let principal = 120000  // 贷款本金
  let annual_rate = 6     // 年利率6%
  let years = 10          // 贷款年限
  
  // 简化的月供计算：本金/月数 + 利息
  let months = @azimuth.multiply(years, 12)
  let monthly_principal = principal / months
  let monthly_interest = @azimuth.multiply(principal, annual_rate) / 100 / 12
  let monthly_payment = @azimuth.add(monthly_principal, monthly_interest)
  
  // 计算总还款额
  let total_payment = @azimuth.multiply(monthly_payment, months)
  
  // 验证计算结果（简化计算）
  @azimuth.assert_true(monthly_payment > 1000)  // 月供应大于1000
  @azimuth.assert_true(total_payment > principal)  // 总还款额应大于本金
}

test "temperature_conversion_precise" {
  // 测试精确的温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 使用整数近似：F = C × 2 - C/5 + 32
  
  let celsius = 25
  let fahrenheit_approx = @azimuth.add(@azimuth.add(@azimuth.multiply(celsius, 2), @azimuth.multiply(-celsius / 5, 1)), 32)
  @azimuth.assert_eq(77, fahrenheit_approx)  // 25°C ≈ 77°F
  
  // 测试负温度
  let negative_celsius = -10
  let negative_fahrenheit = @azimuth.add(@azimuth.add(@azimuth.multiply(negative_celsius, 2), @azimuth.multiply(-negative_celsius / 5, 1)), 32)
  @azimuth.assert_eq(14, negative_fahrenheit)  // -10°C ≈ 14°F
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟：O(n²) 操作
  let n = 10
  
  // 模拟嵌套循环的总操作数：n × n
  let total_operations = @azimuth.multiply(n, n)
  @azimuth.assert_eq(100, total_operations)
  
  // 模拟 O(n log n) 操作（简化为 n × log₂n）
  // 使用近似值：log₂10 ≈ 3
  let log_n = 3
  let n_log_n_operations = @azimuth.multiply(n, log_n)
  @azimuth.assert_eq(30, n_log_n_operations)
}

test "combinatorial_calculations" {
  // 测试组合数学计算
  // 计算排列数 P(n,k) = n!/(n-k)!
  // 简化计算：P(5,3) = 5 × 4 × 3 = 60
  
  let n = 5
  let k = 3
  let permutation = @azimuth.multiply(n, @azimuth.multiply(n - 1, n - 2))
  @azimuth.assert_eq(60, permutation)
  
  // 计算组合数 C(n,k) = P(n,k)/k!
  // 简化计算：C(5,3) = 60/(3×2×1) = 10
  let combination = permutation / @azimuth.multiply(@azimuth.multiply(3, 2), 1)
  @azimuth.assert_eq(10, combination)
}

test "data_structure_operations" {
  // 测试数据结构操作模拟
  // 模拟栈操作：push, push, pop, push
  let mut stack_size = 0
  
  // push 操作
  stack_size = @azimuth.add(stack_size, 1)  // size = 1
  stack_size = @azimuth.add(stack_size, 1)  // size = 2
  
  // pop 操作
  stack_size = @azimuth.add(stack_size, -1)  // size = 1
  
  // push 操作
  stack_size = @azimuth.add(stack_size, 1)  // size = 2
  
  @azimuth.assert_eq(2, stack_size)
  
  // 模拟队列操作：enqueue, dequeue, enqueue
  let mut queue_size = 0
  
  // enqueue 操作
  queue_size = @azimuth.add(queue_size, 1)  // size = 1
  queue_size = @azimuth.add(queue_size, 1)  // size = 2
  
  // dequeue 操作
  queue_size = @azimuth.add(queue_size, -1)  // size = 1
  
  // enqueue 操作
  queue_size = @azimuth.add(queue_size, 1)  // size = 2
  
  @azimuth.assert_eq(2, queue_size)
}

test "number_system_conversion" {
  // 测试数字系统转换
  // 十进制转二进制（模拟）：13 = 1101₂
  let decimal = 13
  
  // 模拟二进制转换：通过位运算
  let bit0 = decimal % 2          // 1
  let bit1 = (decimal / 2) % 2    // 0
  let bit2 = (decimal / 4) % 2    // 1
  let bit3 = (decimal / 8) % 2    // 1
  
  // 将二进制位转换为十进制验证
  let binary_to_decimal = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(bit3, 8), @azimuth.multiply(bit2, 4)), @azimuth.multiply(bit1, 2)), bit0)
  @azimuth.assert_eq(decimal, binary_to_decimal)
  
  // 十进制转八进制（模拟）：100 = 144₈
  let octal_decimal = 100
  let octal_digit0 = octal_decimal % 8      // 4
  let octal_digit1 = (octal_decimal / 8) % 8  // 4
  let octal_digit2 = (octal_decimal / 64) % 8 // 1
  
  // 验证八进制转换
  let octal_to_decimal = @azimuth.add(@azimuth.add(@azimuth.multiply(octal_digit2, 64), @azimuth.multiply(octal_digit1, 8)), octal_digit0)
  @azimuth.assert_eq(octal_decimal, octal_to_decimal)
}

test "physics_simulation" {
  // 测试物理模拟计算
  // 自由落体运动：h = v₀t + ½gt²
  let initial_velocity = 10  // 初始速度 m/s
  let time = 5               // 时间 s
  let gravity = 10           // 重力加速度 m/s²（简化值）
  
  // 计算位移：h = 10×5 + ½×10×5² = 50 + 125 = 175
  let height = @azimuth.add(@azimuth.multiply(initial_velocity, time), @azimuth.multiply(@azimuth.multiply(gravity, time), time) / 2)
  @azimuth.assert_eq(175, height)
  
  // 动能计算：E = ½mv²
  let mass = 2        // 质量 kg
  let velocity = 15   // 速度 m/s
  
  // 计算动能：E = ½×2×15² = 225
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  @azimuth.assert_eq(225, kinetic_energy)
}