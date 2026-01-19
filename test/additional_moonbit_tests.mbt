// 额外的 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "arithmetic_sequence_calculations" {
  // 测试等差数列计算
  // 计算前n个自然数的和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 10
  let sum = multiply(n, add(n, 1)) / 2
  assert_eq(55, sum)
  
  // 计算前n个偶数的和：2 + 4 + 6 + ... + 2n = n × (n + 1)
  let even_sum = multiply(n, add(n, 1))
  assert_eq(110, even_sum)
}

test "geometric_sequence_calculations" {
  // 测试等比数列计算
  // 计算2^0 + 2^1 + 2^2 + 2^3 + 2^4 = 31
  let term1 = 1
  let term2 = multiply(2, 1)
  let term3 = multiply(2, term2)
  let term4 = multiply(2, term3)
  let term5 = multiply(2, term4)
  let geo_sum = add(add(add(add(term1, term2), term3), term4), term5)
  assert_eq(31, geo_sum)
}

test "temperature_conversion_scenarios" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius_temperatures = [0, 10, 20, 30, 40]
  let expected_fahrenheit = [30, 50, 70, 90, 110]
  
  assert_eq(30, add(multiply(0, 2), 30))
  assert_eq(50, add(multiply(10, 2), 30))
  assert_eq(70, add(multiply(20, 2), 30))
  assert_eq(90, add(multiply(30, 2), 30))
  assert_eq(110, add(multiply(40, 2), 30))
}

test "financial_calculations" {
  // 测试金融计算场景
  // 简单利息计算：本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  let simple_interest = multiply(principal, multiply(rate, time))
  assert_eq(150000, simple_interest)
  
  // 本息合计：本金 + 利息
  let total_amount = add(principal, simple_interest)
  assert_eq(160000, total_amount)
}

test "string_validation_tests" {
  // 测试字符串验证功能
  // 测试不同类型的输入
  assert_eq_string("Hello, user123!", greet("user123"))
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, User_Name_2023!", greet("User_Name_2023"))
  assert_eq_string("Hello,   spaced   name!", greet("  spaced   name"))
}

test "boundary_value_extremes" {
  // 测试极值边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值之间的运算
  assert_eq(0, add(max_val, add(min_val, 1)))
  assert_eq(-2, add(min_val, add(min_val, 2)))
  
  // 测试极值与普通值的运算
  assert_eq(2147483646, add(max_val, -1))
  assert_eq(-2147483647, add(min_val, 1))
  
  // 测试极值乘法
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "logical_expression_evaluation" {
  // 测试逻辑表达式评估
  let x = 10
  let y = 20
  let z = 30
  
  // 测试比较结果的运算
  let condition1 = x < y  // true
  let condition2 = y > z  // false
  
  // 基于条件的计算
  let result1 = if condition1 { add(x, y) } else { multiply(x, y) }
  let result2 = if condition2 { add(y, z) } else { multiply(y, z) }
  
  assert_eq(30, result1)  // 10 + 20 = 30
  assert_eq(600, result2)  // 20 * 30 = 600
}

test "algorithmic_complexity_simulation" {
  // 测试算法复杂度模拟
  // O(n) 复杂度：线性求和
  let n = 100
  let linear_sum = multiply(n, add(n, 1)) / 2
  assert_eq(5050, linear_sum)
  
  // O(n²) 复杂度：平方和（简化计算）
  let square_sum_approx = multiply(multiply(n, n), n) / 3
  assert_eq(333333, square_sum_approx)
  
  // O(log n) 复杂度：对数运算的整数近似
  let log_approx = n / 10  // 简化的对数近似
  assert_eq(10, log_approx)
}

test "data_structure_operations" {
  // 测试数据结构操作模拟
  // 栈操作模拟：使用加减法模拟栈的push和pop
  let stack_initial = 0
  let push1 = add(stack_initial, 10)
  let push2 = add(push1, 20)
  let push3 = add(push2, 30)
  let pop1 = add(push3, -30)
  let pop2 = add(pop1, -20)
  
  assert_eq(10, pop2)
  
  // 队列操作模拟：使用计数器模拟队列
  let enqueue_count = add(0, 5)  // 入队5个元素
  let dequeue_count = add(0, 2)  // 出队2个元素
  let queue_size = add(enqueue_count, -dequeue_count)
  
  assert_eq(3, queue_size)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续溢出操作
  let overflow1 = add(max_val, 1)
  let overflow2 = add(overflow1, 1)
  let overflow3 = add(overflow2, 1)
  
  // 所有溢出操作都应该返回最大值
  assert_eq(max_val, overflow1)
  assert_eq(max_val, overflow2)
  assert_eq(max_val, overflow3)
  
  // 测试连续下溢操作
  let underflow1 = add(min_val, -1)
  let underflow2 = add(underflow1, -1)
  let underflow3 = add(underflow2, -1)
  
  // 所有的下溢操作都应该返回最小值
  assert_eq(min_val, underflow1)
  assert_eq(min_val, underflow2)
  assert_eq(min_val, underflow3)
  
  // 测试乘法溢出的级联效应
  let mul_overflow1 = multiply(max_val, 2)
  let mul_overflow2 = multiply(mul_overflow1, 2)
  
  assert_eq(max_val, mul_overflow1)
  assert_eq(max_val, mul_overflow2)
}