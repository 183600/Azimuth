// 新增 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖各种测试场景

test "bitwise_operations_simulation" {
  // 通过算术运算模拟位运算
  // 模拟左移操作：x << n = x * 2^n
  @azimuth.assert_eq(8, @azimuth.multiply(1, 8))  // 1 << 3 = 8
  @azimuth.assert_eq(16, @azimuth.multiply(2, 8)) // 2 << 3 = 16
  @azimuth.assert_eq(40, @azimuth.multiply(5, 8)) // 5 << 3 = 40
  
  // 模拟右移操作：x >> n = x / 2^n (向上取整)
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(16, 8))  // 16 >> 3 = 2
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(10, 8))  // 10 >> 3 = 2 (向上取整)
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(9, 8))   // 9 >> 3 = 2 (向上取整)
}

test "string_encoding_and_length" {
  // 测试字符串编码和长度相关的边界情况
  @azimuth.assert_eq_string("Hello, ASCII!", @azimuth.greet("ASCII"))
  @azimuth.assert_eq_string("Hello, 中文123!", @azimuth.greet("中文123"))
  @azimuth.assert_eq_string("Hello, 🌟🚀!", @azimuth.greet("🌟🚀"))
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \"\\!", @azimuth.greet("\"\\"))
  
  // 测试极端长度的字符串（相对）
  let long_name = "TheQuickBrownFoxJumpsOverTheLazyDog"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
}

test "numerical_range_validation" {
  // 数值范围验证测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 验证加法不会超出范围
  let safe_add1 = @azimuth.add(1000000, 1000000)
  let safe_add2 = @azimuth.add(-1000000, -1000000)
  @azimuth.assert_eq(2000000, safe_add1)
  @azimuth.assert_eq(-2000000, safe_add2)
  
  // 验证乘法不会超出范围
  let safe_mul1 = @azimuth.multiply(1000, 1000)
  let safe_mul2 = @azimuth.multiply(-1000, 1000)
  @azimuth.assert_eq(1000000, safe_mul1)
  @azimuth.assert_eq(-1000000, safe_mul2)
  
  // 验证边界值处理
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "financial_calculations" {
  // 金融计算场景测试
  // 场景：计算复利
  let principal = 10000  // 本金
  let rate = 2           // 利率 50% (1/2)
  let years = 3          // 年数
  
  // 第一年：10000 * 1.5 = 15000
  let year1 = @azimuth.divide_with_ceil(@azimuth.multiply(principal, 3), 2)
  
  // 第二年：15000 * 1.5 = 22500
  let year2 = @azimuth.divide_with_ceil(@azimuth.multiply(year1, 3), 2)
  
  // 第三年：22500 * 1.5 = 33750
  let year3 = @azimuth.divide_with_ceil(@azimuth.multiply(year2, 3), 2)
  
  @azimuth.assert_eq(15000, year1)
  @azimuth.assert_eq(22500, year2)
  @azimuth.assert_eq(33750, year3)
  
  // 场景：贷款分期计算
  let loan_amount = 120000
  let months = 12
  let monthly_payment = @azimuth.divide_with_ceil(loan_amount, months)
  @azimuth.assert_eq(10000, monthly_payment)
}

test "mathematical_series" {
  // 数学序列测试
  // 斐波那契数列（简化版）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)      // 2
  let fib4 = @azimuth.add(fib2, fib3)      // 3
  let fib5 = @azimuth.add(fib3, fib4)      // 5
  let fib6 = @azimuth.add(fib4, fib5)      // 8
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  
  // 平方数序列
  let square1 = @azimuth.multiply(1, 1)      // 1
  let square2 = @azimuth.multiply(2, 2)      // 4
  let square3 = @azimuth.multiply(3, 3)      // 9
  let square4 = @azimuth.multiply(4, 4)      // 16
  
  @azimuth.assert_eq(1, square1)
  @azimuth.assert_eq(4, square2)
  @azimuth.assert_eq(9, square3)
  @azimuth.assert_eq(16, square4)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出后的函数状态
  let overflow_add = @azimuth.add(max_val, max_val)
  let overflow_mul = @azimuth.multiply(max_val, 2)
  
  // 验证正常操作仍然工作
  let normal_add = @azimuth.add(42, 58)
  let normal_mul = @azimuth.multiply(6, 7)
  let normal_greet = @azimuth.greet("Test")
  
  @azimuth.assert_eq(100, normal_add)
  @azimuth.assert_eq(42, normal_mul)
  @azimuth.assert_eq_string("Hello, Test!", normal_greet)
  
  // 验证溢出结果在边界内
  @azimuth.assert_true(overflow_add == max_val || overflow_add == min_val)
  @azimuth.assert_true(overflow_mul == max_val || overflow_mul == min_val)
}

test "performance_critical_operations" {
  // 性能关键操作测试
  let base = 100
  
  // 连续运算性能测试
  let result1 = @azimuth.add(@azimuth.add(@azimuth.add(base, 50), 25), 10)
  let result2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(base, 2), 3), 4)
  let result3 = @azimuth.divide_with_ceil(@azimuth.divide_with_ceil(@azimuth.divide_with_ceil(1000, 3), 2), 5)
  
  @azimuth.assert_eq(185, result1)  // 100 + 50 + 25 + 10
  @azimuth.assert_eq(2400, result2) // 100 * 2 * 3 * 4
  @azimuth.assert_eq(34, result3)   // ceil(ceil(ceil(1000/3)/2)/5)
  
  // 大数运算性能测试
  let large_num = 100000
  let large_result = @azimuth.add(@azimuth.multiply(large_num, 10), @azimuth.divide_with_ceil(large_num, 3))
  @azimuth.assert_eq(1033334, large_result)
}

test "internationalization_support" {
  // 国际化支持测试
  // 测试不同语言的问候
  @azimuth.assert_eq_string("Hello, English!", @azimuth.greet("English"))
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 日本語!", @azimuth.greet("日本語"))
  @azimuth.assert_eq_string("Hello, 한국어!", @azimuth.greet("한국어"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
  @azimuth.assert_eq_string("Hello, русский!", @azimuth.greet("русский"))
  @azimuth.assert_eq_string("Hello, español!", @azimuth.greet("español"))
  @azimuth.assert_eq_string("Hello, français!", @azimuth.greet("français"))
  @azimuth.assert_eq_string("Hello, português!", @azimuth.greet("português"))
  @azimuth.assert_eq_string("Hello, italiano!", @azimuth.greet("italiano"))
}

test "scientific_calculations" {
  // 科学计算测试
  // 场景：物理学计算（简化版）
  let mass = 1000      // 质量
  let acceleration = 9 // 加速度
  let force = @azimuth.multiply(mass, acceleration)  // F = ma
  @azimuth.assert_eq(9000, force)
  
  // 场景：化学反应计算（简化版）
  let concentration1 = 25
  let concentration2 = 75
  let total_concentration = @azimuth.add(concentration1, concentration2)
  let ratio1 = @azimuth.divide_with_ceil(@azimuth.multiply(concentration1, 100), total_concentration)
  let ratio2 = @azimuth.divide_with_ceil(@azimuth.multiply(concentration2, 100), total_concentration)
  
  @azimuth.assert_eq(100, total_concentration)
  @azimuth.assert_eq(25, ratio1)  // 25%
  @azimuth.assert_eq(75, ratio2)  // 75%
  
  // 场景：统计学计算
  let data1 = 85
  let data2 = 92
  let data3 = 78
  let data4 = 90
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4)
  let average = @azimuth.divide_with_ceil(sum, 4)
  
  @azimuth.assert_eq(345, sum)
  @azimuth.assert_eq(87, average)
}

test "algorithm_complexity_validation" {
  // 算法复杂度验证测试
  // 线性复杂度模拟：O(n)
  let n = 100
  let linear_sum = 0
  // 模拟线性累加（实际使用常数时间操作）
  let linear_result = @azimuth.multiply(n, 50)  // 假设每个元素平均值为50
  @azimuth.assert_eq(5000, linear_result)
  
  // 平方复杂度模拟：O(n²)
  let n_squared = @azimuth.multiply(n, n)
  let quadratic_result = @azimuth.divide_with_ceil(n_squared, 2)  // n*(n+1)/2 的简化版
  @azimuth.assert_eq(5000, quadratic_result)
  
  // 对数复杂度模拟：O(log n)
  // 模拟二分查找的步骤数
  let log_steps = @azimuth.divide_with_ceil(@azimuth.divide_with_ceil(n, 2), 2)
  @azimuth.assert_eq(25, log_steps)
}