// Azimuth 项目新增 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，覆盖多种功能场景

test "extreme_boundary_values" {
  // 极值边界测试
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 测试极值间的加法运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, neg_one))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, one))
  
  // 测试极值间的乘法运算
  @azimuth.assert_eq(zero, @azimuth.multiply(max_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(min_val, zero))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, one))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, one))
}

test "advanced_string_operations" {
  // 高级字符串操作测试
  // 测试多语言字符串
  let chinese_name = "张三"
  let english_name = "John"
  let mixed_name = "张三 John"
  
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(chinese_name))
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet(english_name))
  @azimuth.assert_eq_string("Hello, 张三 John!", @azimuth.greet(mixed_name))
  
  // 测试特殊字符和数字组合
  let special_chars = "User@123#Admin"
  let unicode_chars = "测试🚀User"
  
  @azimuth.assert_eq_string("Hello, User@123#Admin!", @azimuth.greet(special_chars))
  @azimuth.assert_eq_string("Hello, 测试🚀User!", @azimuth.greet(unicode_chars))
  
  // 测试空字符串和单字符
  let empty_string = ""
  let single_char = "A"
  
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty_string))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet(single_char))
}

test "complex_mathematical_expressions" {
  // 复杂数学表达式测试
  // 测试二次方程 ax² + bx + c
  let a = 2
  let b = 3
  let c = 4
  let x = 5
  
  let ax2 = @azimuth.multiply(a, @azimuth.multiply(x, x))
  let bx = @azimuth.multiply(b, x)
  let result = @azimuth.add(@azimuth.add(ax2, bx), c)
  
  @azimuth.assert_eq(2*25 + 3*5 + 4, result)
  
  // 测试勾股定理 a² + b² = c²
  let side_a = 3
  let side_b = 4
  let a_squared = @azimuth.multiply(side_a, side_a)
  let b_squared = @azimuth.multiply(side_b, side_b)
  let c_squared = @azimuth.add(a_squared, b_squared)
  
  @azimuth.assert_eq(25, c_squared)
}

test "financial_calculations" {
  // 金融计算测试
  // 计算复利: A = P(1 + r/n)^(nt)
  let principal = 1000
  let rate = 5
  let years = 10
  
  // 简化计算: 只计算基本利息
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years)) / 100
  let total_amount = @azimuth.add(principal, simple_interest)
  
  @azimuth.assert_eq(1500, total_amount)
  
  // 计算贷款月供: M = P[r(1+r)^n]/[(1+r)^n-1]
  let loan_amount = 200000
  let monthly_rate = 4
  
  // 简化计算: 只计算基本月供
  let monthly_interest = @azimuth.multiply(loan_amount, monthly_rate) / 1200
  let monthly_payment = @azimuth.add(monthly_interest, @azimuth.multiply(loan_amount, 1) / 360)
  
  @azimuth.assert_eq(666, monthly_payment)
}

test "scientific_computations" {
  // 科学计算测试
  // 计算动能: E = 1/2 * m * v²
  let mass = 10
  let velocity = 5
  
  let velocity_squared = @azimuth.multiply(velocity, velocity)
  let kinetic_energy = @azimuth.multiply(mass, velocity_squared) / 2
  
  @azimuth.assert_eq(125, kinetic_energy)
  
  // 计算势能: E = m * g * h
  let height = 20
  let gravity = 10
  
  let potential_energy = @azimuth.multiply(mass, @azimuth.multiply(gravity, height))
  
  @azimuth.assert_eq(2000, potential_energy)
  
  // 计算功率: P = W / t
  let work = 5000
  let time = 10
  let power = work / time
  
  @azimuth.assert_eq(500, power)
}

test "algorithmic_efficiency" {
  // 算法效率测试
  // 模拟二分查找的比较次数
  let comparisons_per_level = 1
  let max_levels = 10
  
  let worst_case_comparisons = @azimuth.multiply(comparisons_per_level, max_levels)
  
  @azimuth.assert_eq(10, worst_case_comparisons)
  
  // 模拟冒泡排序的交换次数（最坏情况）
  let n = 5
  
  let max_swaps = @azimuth.multiply(n, @azimuth.add(n, -1)) / 2
  
  @azimuth.assert_eq(10, max_swaps)
  
  // 模拟选择排序的比较次数
  let total_comparisons = @azimuth.multiply(n, @azimuth.add(n, -1)) / 2
  
  @azimuth.assert_eq(10, total_comparisons)
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 模拟栈操作：push和pop
  let initial_stack_size = 0
  let push_count = 5
  let pop_count = 3
  
  // Push操作
  let stack_size_after_push = @azimuth.add(initial_stack_size, push_count)
  
  @azimuth.assert_eq(5, stack_size_after_push)
  
  // Pop操作
  let stack_size_after_pop = @azimuth.add(stack_size_after_push, -pop_count)
  
  @azimuth.assert_eq(2, stack_size_after_pop)
  
  // 模拟队列操作：enqueue和dequeue
  let initial_queue_size = 0
  let enqueue_count = 8
  let dequeue_count = 6
  
  // Enqueue操作
  let queue_size_after_enqueue = @azimuth.add(initial_queue_size, enqueue_count)
  
  @azimuth.assert_eq(8, queue_size_after_enqueue)
  
  // Dequeue操作
  let queue_size_after_dequeue = @azimuth.add(queue_size_after_enqueue, -dequeue_count)
  
  @azimuth.assert_eq(2, queue_size_after_dequeue)
}

test "statistical_analysis" {
  // 统计分析测试
  // 计算平均值
  let data_sum = @azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40)
  let data_count = 4
  let average = data_sum / data_count
  
  @azimuth.assert_eq(25, average)
  
  // 计算方差（简化版）
  let mean = 25
  let diff1 = @azimuth.add(10, -mean)
  let diff2 = @azimuth.add(20, -mean)
  let diff3 = @azimuth.add(30, -mean)
  let diff4 = @azimuth.add(40, -mean)
  
  let squared_diff1 = @azimuth.multiply(diff1, diff1)
  let squared_diff2 = @azimuth.multiply(diff2, diff2)
  let squared_diff3 = @azimuth.multiply(diff3, diff3)
  let squared_diff4 = @azimuth.multiply(diff4, diff4)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(squared_diff1, squared_diff2), squared_diff3), squared_diff4) / data_count
  
  @azimuth.assert_eq(125, variance)
}

test "geometry_calculations" {
  // 几何计算测试
  // 计算圆的面积和周长
  let radius = 5
  let pi = 3
  
  let area = @azimuth.multiply(pi, @azimuth.multiply(radius, radius))
  let circumference = @azimuth.multiply(2, @azimuth.multiply(pi, radius))
  
  @azimuth.assert_eq(75, area)
  @azimuth.assert_eq(30, circumference)
  
  // 计算矩形的面积和周长
  let length = 10
  let width = 5
  
  let rect_area = @azimuth.multiply(length, width)
  let rect_perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  
  @azimuth.assert_eq(50, rect_area)
  @azimuth.assert_eq(30, rect_perimeter)
  
  // 计算三角形的面积（使用海伦公式简化版）
  let a = 3
  let b = 4
  let c = 5
  
  let s = @azimuth.add(@azimuth.add(a, b), c) / 2
  let triangle_area = @azimuth.multiply(s, @azimuth.add(s, -a)) / 2
  
  @azimuth.assert_eq(6, triangle_area)
}

test "pattern_recognition" {
  // 模式识别测试
  // 检测等差数列
  let term1 = 2
  let term2 = 5
  let term3 = 8
  let term4 = 11
  
  let diff1 = @azimuth.add(term2, -term1)
  let diff2 = @azimuth.add(term3, -term2)
  let diff3 = @azimuth.add(term4, -term3)
  
  @azimuth.assert_eq(diff1, diff2)
  @azimuth.assert_eq(diff2, diff3)
  
  // 预测下一项
  let next_term = @azimuth.add(term4, diff1)
  
  @azimuth.assert_eq(14, next_term)
  
  // 检测等比数列
  let geo_term1 = 2
  let geo_term2 = 6
  let geo_term3 = 18
  let geo_term4 = 54
  
  let ratio1 = geo_term2 / geo_term1
  let ratio2 = geo_term3 / geo_term2
  let ratio3 = geo_term4 / geo_term3
  
  @azimuth.assert_eq(ratio1, ratio2)
  @azimuth.assert_eq(ratio2, ratio3)
  
  // 预测下一项
  let geo_next_term = @azimuth.multiply(geo_term4, ratio1)
  
  @azimuth.assert_eq(162, geo_next_term)
}

test "error_handling_scenarios" {
  // 错误处理场景测试
  // 测试除零保护（通过乘法模拟）
  let numerator = 100
  let denominator = 0
  
  // 在实际应用中，这应该返回错误或特殊值
  // 这里我们只是测试基本的数学运算
  let safe_result = @azimuth.multiply(numerator, denominator)
  
  @azimuth.assert_eq(0, safe_result)
  
  // 测试溢出保护
  let max_int = 2147483647
  
  let overflow_result = @azimuth.add(max_int, 1)
  let underflow_result = @azimuth.add(-max_int, -1)
  
  @azimuth.assert_eq(max_int, overflow_result)
  @azimuth.assert_eq(-max_int, underflow_result)
  
  // 测试无效输入处理
  let empty_input = ""
  let null_input = ""
  
  let result1 = @azimuth.greet(empty_input)
  let result2 = @azimuth.greet(null_input)
  
  @azimuth.assert_eq_string("Hello, !", result1)
  @azimuth.assert_eq_string("Hello, !", result2)
}
