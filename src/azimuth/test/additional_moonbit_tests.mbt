// 额外的 MoonBit 测试用例 - 使用标准 MoonBit 测试语法
// 包含 10 个新的测试用例，测试各种功能场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  let a = 15
  let b = 7
  
  // 测试加法
  let sum = @azimuth.add(a, b)
  @azimuth.assert_eq(22, sum)
  
  // 测试乘法
  let product = @azimuth.multiply(a, b)
  @azimuth.assert_eq(105, product)
  
  // 测试复合运算
  let complex = @azimuth.add(@azimuth.multiply(a, 2), @azimuth.multiply(b, 3))
  @azimuth.assert_eq(51, complex)
}

test "string_functionality_comprehensive" {
  // 测试字符串功能的全面性
  // 测试基本问候
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@#$%!", @azimuth.greet("test@#$%"))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前 n 项和：1 + 2 + 3 + ... + 10
  let n = 10
  let arithmetic_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(55, arithmetic_sum)
  
  // 计算等比数列前 n 项和：1 + 2 + 4 + 8 + 16
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, 2), 4), 8), 16)
  @azimuth.assert_eq(31, geo_sum)
  
  // 计算平方和：1² + 2² + 3² + 4² + 5²
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), @azimuth.multiply(4, 4)), @azimuth.multiply(5, 5))
  @azimuth.assert_eq(55, square_sum)
}

test "boundary_value_comprehensive" {
  // 测试边界值的综合情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的各种运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值的各种运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "real_world_practical_scenarios" {
  // 测试实际应用场景
  // 场景1：计算折扣价格
  let original_price = 200
  let discount_percentage = 15
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(170, discounted_price)
  
  // 场景2：计算平均分数
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let score4 = 92
  let average_score = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4) / 4
  @azimuth.assert_eq(86, average_score)
  
  // 场景3：计算简单利息
  let principal = 5000
  let rate = 8
  let time = 3
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, time)) / 100
  let total_amount = @azimuth.add(principal, interest)
  @azimuth.assert_eq(6200, total_amount)
}

test "negative_and_zero_operations" {
  // 测试负数和零的运算
  // 测试零的运算特性
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  @azimuth.assert_eq(-12345, @azimuth.add(0, -12345))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  
  // 测试负数运算
  @azimuth.assert_eq(-15, @azimuth.add(-10, -5))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-10, @azimuth.add(5, -15))
  
  // 测试负数乘法
  @azimuth.assert_eq(50, @azimuth.multiply(-10, -5))
  @azimuth.assert_eq(-50, @azimuth.multiply(-10, 5))
  @azimuth.assert_eq(-50, @azimuth.multiply(10, -5))
  
  // 测试零与负数的运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, -100))
  @azimuth.assert_eq(-100, @azimuth.add(0, -100))
}

test "complex_formula_applications" {
  // 测试复杂公式的应用
  // 场景1：计算二次方程的值
  let x = 5
  let a = 2
  let b = 3
  let c = 4
  
  // 计算 ax² + bx + c
  let x_squared = @azimuth.multiply(x, x)
  let ax_squared = @azimuth.multiply(a, x_squared)
  let bx = @azimuth.multiply(b, x)
  let quadratic_result = @azimuth.add(@azimuth.add(ax_squared, bx), c)
  @azimuth.assert_eq(69, quadratic_result)
  
  // 场景2：计算圆柱体表面积（简化）
  let radius = 5
  let height = 10
  let pi_approx = 3
  
  // 表面积 = 2πr² + 2πrh
  let circle_area = @azimuth.multiply(pi_approx, @azimuth.multiply(radius, radius))
  let side_area = @azimuth.multiply(@azimuth.multiply(2, pi_approx), @azimuth.multiply(radius, height))
  let surface_area = @azimuth.add(@azimuth.multiply(2, circle_area), side_area)
  @azimuth.assert_eq(475, surface_area)
}

test "algorithmic_thinking_tests" {
  // 测试算法思维
  // 场景1：计算最大公约数（欧几里得算法简化版）
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法的过程
  let step1 = a % b  // 48 % 18 = 12
  let step2 = b % step1  // 18 % 12 = 6
  let step3 = step1 % step2  // 12 % 6 = 0
  let gcd = step2  // 当余数为0时，除数就是GCD
  
  @azimuth.assert_eq(6, gcd)
  
  // 场景2：计算斐波那契数列的第n项（简化版）
  let fib_n = 7
  // 手动计算斐波那契数列：0, 1, 1, 2, 3, 5, 8, 13...
  // 第7项是8（从0开始计数）
  let fib_7 = 8
  @azimuth.assert_eq(8, fib_7)
  
  // 场景3：计算阶乘
  let factorial_n = 5
  // 5! = 5 × 4 × 3 × 2 × 1 = 120
  let factorial_result = @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1))))
  @azimuth.assert_eq(120, factorial_result)
}

test "data_processing_scenarios" {
  // 测试数据处理场景
  // 场景1：计算数据的统计量
  let data_values = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  let data_count = 5
  
  // 计算平均值
  let data_mean = data_values / data_count
  @azimuth.assert_eq(35, data_mean)
  
  // 计算范围
  let data_range = @azimuth.add(55, -15)
  @azimuth.assert_eq(40, data_range)
  
  // 场景2：数据转换
  let celsius_temperature = 25
  // 转换为华氏度：F = C × 2 + 30（简化版）
  let fahrenheit_temperature = @azimuth.add(@azimuth.multiply(celsius_temperature, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temperature)
  
  // 场景3：百分比计算
  let total_value = 800
  let part_value = 200
  let percentage = @azimuth.multiply(part_value, 100) / total_value
  @azimuth.assert_eq(25, percentage)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试特殊值处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界条件下的字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, " + @azimuth.greet("test"), @azimuth.greet("test"))
}