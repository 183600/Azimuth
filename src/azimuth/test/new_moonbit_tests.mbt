// Azimuth 项目额外的 MoonBit 测试用例
// 包含边界值、错误处理和实际应用场景的测试

test "extreme_boundary_addition" {
  // 测试极端边界值的加法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与接近最大值的数相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 最小值与接近最小值的数相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 最大值与最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "extreme_boundary_multiplication" {
  // 测试极端边界值的乘法运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值的乘法测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值的乘法测试
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 零与边界值的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "string_edge_case_processing" {
  // 测试字符串边缘情况处理
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, A B C!", @azimuth.greet("A B C"))
  
  // 包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, 	\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "complex_nested_calculations" {
  // 测试复杂嵌套计算
  // 计算：((a + b) * (c - d)) + (e * f) - g
  let a = 10
  let b = 20
  let c = 50
  let d = 30
  let e = 5
  let f = 8
  let g = 15
  
  let sum1 = @azimuth.add(a, b)  // 30
  let diff1 = @azimuth.add(c, -d)  // 20
  let product1 = @azimuth.multiply(sum1, diff1)  // 600
  let product2 = @azimuth.multiply(e, f)  // 40
  let result = @azimuth.add(@azimuth.add(product1, product2), -g)  // 625
  
  @azimuth.assert_eq(625, result)
}

test "real_world_currency_conversion" {
  // 测试实际货币转换场景
  let usd_amount = 150
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  let fee_rate = 2  // 2% 手续费
  
  // 转换为人民币
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)  // 1050
  
  // 计算手续费
  let fee = @azimuth.multiply(cny_amount, fee_rate) / 100  // 21
  
  // 计算最终金额
  let final_amount = @azimuth.add(cny_amount, fee)  // 1071
  
  @azimuth.assert_eq(1071, final_amount)
}

test "physics_simulation_simple" {
  // 测试简单物理模拟
  // 计算自由落体距离：d = 0.5 * g * t²
  // 简化为整数计算：d = 5 * t² (其中 g = 10 m/s²)
  let gravity = 10
  let time = 5
  
  let time_squared = @azimuth.multiply(time, time)  // 25
  let distance = @azimuth.multiply(gravity, time_squared) / 2  // 125
  
  @azimuth.assert_eq(125, distance)
  
  // 计算速度：v = g * t
  let velocity = @azimuth.multiply(gravity, time)  // 50
  @azimuth.assert_eq(50, velocity)
}

test "data_structure_simulation" {
  // 测试数据结构模拟
  // 模拟栈操作：push, push, pop, push
  let stack_size = 0
  let value1 = 10
  let value2 = 20
  let value3 = 30
  
  // push value1
  stack_size = @azimuth.add(stack_size, 1)
  // push value2
  stack_size = @azimuth.add(stack_size, 1)
  // pop
  stack_size = @azimuth.add(stack_size, -1)
  // push value3
  stack_size = @azimuth.add(stack_size, 1)
  
  @azimuth.assert_eq(2, stack_size)
  
  // 模拟队列操作：enqueue, dequeue, enqueue
  let queue_size = 0
  
  // enqueue
  queue_size = @azimuth.add(queue_size, 1)
  // dequeue
  queue_size = @azimuth.add(queue_size, -1)
  // enqueue
  queue_size = @azimuth.add(queue_size, 1)
  
  @azimuth.assert_eq(1, queue_size)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  // 模拟 O(n²) 算法的操作次数
  let n = 10
  
  // 计算 n * (n - 1) / 2
  let n_minus_1 = @azimuth.add(n, -1)  // 9
  let product = @azimuth.multiply(n, n_minus_1)  // 90
  let operations = product / 2  // 45
  
  @azimuth.assert_eq(45, operations)
  
  // 模拟 O(n log n) 算法（简化为 n * log₂n，其中 log₂10 ≈ 3）
  let log_n = 3  // 简化的 log₂10
  let n_log_n = @azimuth.multiply(n, log_n)  // 30
  
  @azimuth.assert_eq(30, n_log_n)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(max_val, max_val) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.add(min_val, min_val) == min_val)
  
  // 测试乘法溢出保护
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, 2) == min_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试特殊情况
  @azimuth.assert_true(@azimuth.multiply(0, max_val) == 0)
  @azimuth.assert_true(@azimuth.multiply(0, min_val) == 0)
  @azimuth.assert_true(@azimuth.multiply(1, max_val) == max_val)
  @azimuth.assert_true(@azimuth.multiply(1, min_val) == min_val)
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等比数列：2, 4, 8, 16, 32 的和
  let term1 = 2
  let term2 = @azimuth.multiply(term1, 2)  // 4
  let term3 = @azimuth.multiply(term2, 2)  // 8
  let term4 = @azimuth.multiply(term3, 2)  // 16
  let term5 = @azimuth.multiply(term4, 2)  // 32
  
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(62, sum)
  
  // 计算等差数列：3, 7, 11, 15, 19 的和
  let a1 = 3
  let diff = 4
  let a2 = @azimuth.add(a1, diff)  // 7
  let a3 = @azimuth.add(a2, diff)  // 11
  let a4 = @azimuth.add(a3, diff)  // 15
  let a5 = @azimuth.add(a4, diff)  // 19
  
  let arithmetic_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(a1, a2), a3), a4), a5)
  @azimuth.assert_eq(55, arithmetic_sum)
}