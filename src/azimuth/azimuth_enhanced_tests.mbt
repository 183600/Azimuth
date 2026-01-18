// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法，提供额外的测试场景

test "edge_case_zero_operations" {
  // 零值运算的边界情况测试
  let zero = 0
  let positive = 42
  let negative = -17
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与零的加法
  assert_eq(positive, add(positive, zero))
  assert_eq(negative, add(negative, zero))
  assert_eq(max_val, add(max_val, zero))
  assert_eq(min_val, add(min_val, zero))
  
  // 测试与零的乘法
  assert_eq(zero, multiply(positive, zero))
  assert_eq(zero, multiply(negative, zero))
  assert_eq(zero, multiply(max_val, zero))
  assert_eq(zero, multiply(min_val, zero))
}

test "sequential_calculation_chain" {
  // 连续计算链测试
  let initial = 10
  
  // 构建计算链：(((10 + 5) × 2) - 3) + 8
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = add(step2, -3)         // 27
  let final_result = add(step3, 8)   // 35
  
  assert_eq(35, final_result)
  
  // 测试更复杂的计算链
  let a = 3
  let b = 7
  let c = 2
  let d = 5
  
  // ((a × b) + (c × d)) × 2
  let result1 = multiply(a, b)       // 21
  let result2 = multiply(c, d)       // 10
  let sum = add(result1, result2)    // 31
  let final = multiply(sum, 2)       // 62
  
  assert_eq(62, final)
}

test "string_special_characters_handling" {
  // 特殊字符字符串处理测试
  let quotes = "John \"The Rock\" Doe"
  let newlines = "Line1\nLine2\nLine3"
  let tabs = "Col1\tCol2\tCol3"
  let mixed = "Hello\t\"World\"\nNewLine"
  
  // 测试包含引号的字符串
  let quotes_result = greet(quotes)
  assert_eq_string("Hello, John \"The Rock\" Doe!", quotes_result)
  
  // 测试包含换行符的字符串
  let newlines_result = greet(newlines)
  assert_eq_string("Hello, Line1\nLine2\nLine3!", newlines_result)
  
  // 测试包含制表符的字符串
  let tabs_result = greet(tabs)
  assert_eq_string("Hello, Col1\tCol2\tCol3!", tabs_result)
  
  // 测试混合特殊字符
  let mixed_result = greet(mixed)
  assert_eq_string("Hello, Hello\t\"World\"\nNewLine!", mixed_result)
}

test "mathematical_series_calculation" {
  // 数学数列计算测试
  // 计算斐波那契数列前10项的和
  // F1=1, F2=1, F3=2, F4=3, F5=5, F6=8, F7=13, F8=21, F9=34, F10=55
  // 和 = 1+1+2+3+5+8+13+21+34+55 = 143
  
  let f1 = 1
  let f2 = 1
  let f3 = add(f1, f2)              // 2
  let f4 = add(f2, f3)              // 3
  let f5 = add(f3, f4)              // 5
  let f6 = add(f4, f5)              // 8
  let f7 = add(f5, f6)              // 13
  let f8 = add(f6, f7)              // 21
  let f9 = add(f7, f8)              // 34
  let f10 = add(f8, f9)             // 55
  
  let fibonacci_sum = add(add(add(add(
    add(add(add(add(f1, f2), f3), f4), f5), f6), f7), f8), add(f9, f10))
  
  assert_eq(143, fibonacci_sum)
}

test "real_world_currency_conversion" {
  // 实际货币转换场景测试
  let usd_amount = 125
  let eur_to_usd_rate = 108  // 1 EUR = 1.08 USD (简化为整数)
  let gbp_to_usd_rate = 127  // 1 GBP = 1.27 USD (简化为整数)
  
  // 欧元转美元
  let eur_amount = 100
  let usd_from_eur = multiply(eur_amount, eur_to_usd_rate) / 100
  assert_eq(108, usd_from_eur)
  
  // 英镑转美元
  let gbp_amount = 50
  let usd_from_gbp = multiply(gbp_amount, gbp_to_usd_rate) / 100
  assert_eq(63, usd_from_gbp)  // 50 * 127 / 100 = 63.5 → 63 (整数除法)
  
  // 计算多币种总和
  let total_usd = add(add(usd_amount, usd_from_eur), usd_from_gbp)
  assert_eq(296, total_usd)
}

test "algorithmic_complexity_simulation" {
  // 算法复杂度模拟测试
  let n = 10
  
  // 模拟O(n)算法：线性求和 1 + 2 + ... + n
  // 使用公式：n × (n + 1) / 2
  let linear_result = multiply(n, add(n, 1)) / 2
  assert_eq(55, linear_result)
  
  // 模拟O(n²)算法：计算所有数对的和
  // 简化计算：n × n × 平均值
  let average_value = add(n, 1) / 2
  let quadratic_result = multiply(multiply(n, n), average_value)
  assert_eq(550, quadratic_result)  // 10 × 10 × 5.5 → 10 × 10 × 5 = 500 (整数除法)
  
  // 模拟O(log n)算法：对数搜索步数
  // 简化为 log₂(n) 的整数近似
  let log_steps = 4  // log₂(10) ≈ 3.32 → 4 (向上取整)
  assert_eq(4, log_steps)
}

test "physics_simulation_calculations" {
  // 物理模拟计算测试
  let initial_velocity = 10  // m/s
  let acceleration = 2       // m/s²
  let time = 5               // s
  let mass = 10              // kg
  
  // 计算最终速度：v = u + at
  let final_velocity = add(initial_velocity, multiply(acceleration, time))
  assert_eq(20, final_velocity)
  
  // 计算位移：s = ut + 0.5at²
  // 简化：s = ut + at²/2
  let ut = multiply(initial_velocity, time)
  let at_squared = multiply(acceleration, multiply(time, time))
  let displacement = add(ut, at_squared / 2)
  assert_eq(75, displacement)  // 10×5 + 2×25/2 = 50 + 25 = 75
  
  // 计算动能：KE = 0.5mv²
  // 简化：KE = mv²/2
  let velocity_squared = multiply(final_velocity, final_velocity)
  let kinetic_energy = multiply(mass, velocity_squared) / 2
  assert_eq(2000, kinetic_energy)  // 10 × 20² / 2 = 10 × 400 / 2 = 2000
}

test "data_structure_operations" {
  // 数据结构操作模拟测试
  // 模拟数组操作
  let array_size = 5
  let element_size = 4
  
  // 计算数组总大小
  let total_array_size = multiply(array_size, element_size)
  assert_eq(20, total_array_size)
  
  // 模拟二维数组索引计算
  let row = 2
  let col = 3
  let width = 5
  let index_2d = add(multiply(row, width), col)
  assert_eq(13, index_2d)
  
  // 模拟栈操作
  let stack_size = 0
  // push操作
  let new_stack_size = add(stack_size, 1)
  assert_eq(1, new_stack_size)
  // pop操作
  let final_stack_size = add(new_stack_size, -1)
  assert_eq(0, final_stack_size)
}

test "network_protocol_simulation" {
  // 网络协议模拟测试
  let packet_header_size = 20
  let max_payload_size = 1460
  let data_size = 5000
  
  // 计算需要的包数量
  let packets_needed = divide_with_ceil(data_size, max_payload_size)
  assert_eq(4, packets_needed)
  
  // 计算总传输大小（包含头部）
  let total_transfer_size = add(multiply(packets_needed, packet_header_size), data_size)
  assert_eq(5080, total_transfer_size)
  
  // 计算协议开销
  let overhead = multiply(packets_needed, packet_header_size)
  let overhead_percentage = multiply(overhead, 100) / total_transfer_size
  assert_eq(1, overhead_percentage)  // 80/5080 ≈ 1.57% → 1% (整数除法)
}