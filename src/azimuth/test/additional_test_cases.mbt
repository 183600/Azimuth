// 额外的 MoonBit 测试用例
// 测试 azimuth 库的各种功能和边界情况

test "boundary_value_arithmetic" {
  // 测试边界值的算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与边界值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  
  // 测试最小值与边界值的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  
  // 测试乘法的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "mathematical_properties_verification" {
  // 验证基本的数学性质
  let a = 12
  let b = 8
  let c = 5
  
  // 验证加法结合律: (a + b) + c = a + (b + c)
  let left_associative = @azimuth.add(@azimuth.add(a, b), c)
  let right_associative = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_associative, right_associative)
  
  // 验证乘法结合律: (a * b) * c = a * (b * c)
  let left_mult_associative = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mult_associative = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mult_associative, right_mult_associative)
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试空字符串和单字符
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  
  // 测试非常长的字符串
  let very_long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  @azimuth.assert_eq_string("Hello, " + very_long_name + "!", @azimuth.greet(very_long_name))
  
  // 测试包含数字和字母混合的字符串
  @azimuth.assert_eq_string("Hello, Test123ABC!", @azimuth.greet("Test123ABC"))
}

test "complex_financial_calculations" {
  // 测试复杂金融计算场景
  // 计算贷款月供：P * r * (1 + r)^n / ((1 + r)^n - 1)
  // 简化为整数计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率百分比
  let years = 30          // 贷款年限
  
  // 简化计算：月供 ≈ 本金 / (年限 * 12) + 利息
  let monthly_principal = principal / (years * 12)
  let monthly_interest = @azimuth.multiply(principal, annual_rate) / 1200
  let monthly_payment = @azimuth.add(monthly_principal, monthly_interest)
  
  // 验证计算结果
  @azimuth.assert_eq(777, monthly_payment)
  
  // 计算总还款额
  let total_payment = @azimuth.multiply(monthly_payment, years * 12)
  @azimuth.assert_eq(279720, total_payment)
}

test "scientific_calculations" {
  // 测试科学计算场景
  // 计算物体自由落体距离：s = 0.5 * g * t^2
  // 简化为整数计算：s ≈ 5 * t^2
  let gravity = 5  // 简化的重力常数
  let time1 = 2
  let time2 = 5
  
  let distance1 = @azimuth.multiply(gravity, @azimuth.multiply(time1, time1))
  let distance2 = @azimuth.multiply(gravity, @azimuth.multiply(time2, time2))
  
  @azimuth.assert_eq(20, distance1)   // 5 * 2^2 = 20
  @azimuth.assert_eq(125, distance2)  // 5 * 5^2 = 125
  
  // 计算速度差：v2 - v1 = g * (t2 - t1)
  let velocity_diff = @azimuth.multiply(gravity, @azimuth.add(time2, -time1))
  @azimuth.assert_eq(15, velocity_diff)  // 5 * (5 - 2) = 15
}

test "nested_conditional_logic" {
  // 测试嵌套条件逻辑
  let x = 15
  let y = 25
  let z = 10
  
  // 复杂条件：如果 x < y 且 y - x > z，则计算 x + y + z，否则计算 x * y * z
  let condition1 = x < y
  let condition2 = @azimuth.add(y, -x) > z
  let final_condition = condition1 && condition2
  
  let result = if final_condition {
    @azimuth.add(@azimuth.add(x, y), z)
  } else {
    @azimuth.multiply(@azimuth.multiply(x, y), z)
  }
  
  // 由于 15 < 25 且 25 - 15 > 10 (10 > 10 为假)，所以条件为假
  // 应该计算 15 * 25 * 10 = 3750
  @azimuth.assert_eq(3750, result)
  
  // 测试条件为真的情况
  let x2 = 10
  let y2 = 30
  let z2 = 5
  
  let condition3 = x2 < y2
  let condition4 = @azimuth.add(y2, -x2) > z2
  let final_condition2 = condition3 && condition4
  
  let result2 = if final_condition2 {
    @azimuth.add(@azimuth.add(x2, y2), z2)
  } else {
    @azimuth.multiply(@azimuth.multiply(x2, y2), z2)
  }
  
  // 由于 10 < 30 且 30 - 10 > 5 (20 > 5 为真)，所以条件为真
  // 应该计算 10 + 30 + 5 = 45
  @azimuth.assert_eq(45, result2)
}

test "data_structure_operations" {
  // 测试数据结构操作（模拟）
  // 模拟栈操作：push和pop
  let stack_size = 0
  let _element1 = 10
  let _element2 = 20
  let _element3 = 30
  
  // push操作
  let push1 = @azimuth.add(stack_size, 1)  // size = 1
  let push2 = @azimuth.add(push1, 1)       // size = 2
  let push3 = @azimuth.add(push2, 1)       // size = 3
  
  @azimuth.assert_eq(3, push3)
  
  // pop操作
  let pop1 = @azimuth.add(push3, -1)  // size = 2
  let pop2 = @azimuth.add(pop1, -1)   // size = 1
  let pop3 = @azimuth.add(pop2, -1)   // size = 0
  
  @azimuth.assert_eq(0, pop3)
  
  // 模拟队列操作：enqueue和dequeue
  let queue_size = 0
  let enqueue_count = 5
  let dequeue_count = 3
  
  // enqueue操作
  let final_size = @azimuth.add(queue_size, enqueue_count)
  @azimuth.assert_eq(5, final_size)
  
  // dequeue操作
  let remaining_size = @azimuth.add(final_size, -dequeue_count)
  @azimuth.assert_eq(2, remaining_size)
}

test "algorithmic_complexity_simulation" {
  // 测试算法复杂度模拟
  // 模拟O(n)复杂度：线性求和
  let n = 100
  // 使用公式：1 + 2 + ... + n = n * (n + 1) / 2
  let linear_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, linear_sum)
  
  // 模拟O(n²)复杂度：双重循环求和
  // 简化计算：n²
  let quadratic_sum = @azimuth.multiply(n, n)
  @azimuth.assert_eq(10000, quadratic_sum)
  
  // 模拟O(log n)复杂度：二分查找步数
  // 简化计算：log₂(n) ≈ log₂(128) = 7
  let log_steps = 7
  @azimuth.assert_true(log_steps < n)
  
  // 模拟O(2^n)复杂度：指数增长
  // 简化计算：2^10 = 1024
  let exponential_result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)))
  @azimuth.assert_eq(1024, exponential_result)
}

test "error_boundary_testing" {
  // 测试错误边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数的正确性
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护的正确性
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试边界条件的组合
  let edge_case1 = @azimuth.add(@azimuth.multiply(max_val, 0), min_val)
  @azimuth.assert_eq(min_val, edge_case1)
  
  let edge_case2 = @azimuth.multiply(@azimuth.add(max_val, min_val), 1)
  @azimuth.assert_eq(-1, edge_case2)
  
  // 测试字符串处理的边界情况
  @azimuth.assert_eq_string("Hello, " + "" + "!", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, " + " " + "!", @azimuth.greet(" "))
}