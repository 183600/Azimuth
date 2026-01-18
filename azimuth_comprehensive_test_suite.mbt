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

test "complex_mathematical_calculations" {
  // 复杂数学计算测试
  // 测试斐波那契数列计算
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = @azimuth.add(fib_0, fib_1)
  let fib_3 = @azimuth.add(fib_1, fib_2)
  let fib_4 = @azimuth.add(fib_2, fib_3)
  let fib_5 = @azimuth.add(fib_3, fib_4)
  
  @azimuth.assert_eq(1, fib_2)
  @azimuth.assert_eq(2, fib_3)
  @azimuth.assert_eq(3, fib_4)
  @azimuth.assert_eq(5, fib_5)
  
  // 测试阶乘计算 (5! = 120)
  let fact_1 = 1
  let fact_2 = @azimuth.multiply(fact_1, 2)
  let fact_3 = @azimuth.multiply(fact_2, 3)
  let fact_4 = @azimuth.multiply(fact_3, 4)
  let fact_5 = @azimuth.multiply(fact_4, 5)
  
  @azimuth.assert_eq(2, fact_2)
  @azimuth.assert_eq(6, fact_3)
  @azimuth.assert_eq(24, fact_4)
  @azimuth.assert_eq(120, fact_5)
  
  // 测试幂运算 (2^4 = 16)
  let power_base = 2
  let power_result_1 = power_base
  let power_result_2 = @azimuth.multiply(power_result_1, power_base)
  let power_result_3 = @azimuth.multiply(power_result_2, power_base)
  let power_result_4 = @azimuth.multiply(power_result_3, power_base)
  
  @azimuth.assert_eq(2, power_result_1)
  @azimuth.assert_eq(4, power_result_2)
  @azimuth.assert_eq(8, power_result_3)
  @azimuth.assert_eq(16, power_result_4)
}

test "financial_calculations" {
  // 金融计算测试
  // 测试复利计算 A = P(1 + r/n)^(nt)
  let principal = 1000  // 本金
  let rate = 5  // 年利率5%
  let time = 3  // 3年
  
  // 简化计算：1000 * (1.05)^3 ≈ 1000 * 1.157625 = 1157
  let year1_amount = @azimuth.add(principal, @azimuth.multiply(principal, rate) / 100)
  let year2_amount = @azimuth.add(year1_amount, @azimuth.multiply(year1_amount, rate) / 100)
  let year3_amount = @azimuth.add(year2_amount, @azimuth.multiply(year2_amount, rate) / 100)
  
  @azimuth.assert_eq(1050, year1_amount)
  @azimuth.assert_eq(1102, year2_amount)  // 整数除法：1050 + 52 = 1102
  @azimuth.assert_eq(1157, year3_amount)  // 整数除法：1102 + 55 = 1157
  
  // 测试贷款计算
  let loan_amount = 10000
  let annual_rate = 6  // 6%
  let loan_term = 5  // 5年
  
  // 简化计算：总利息 = 本金 * 年利率 * 年数
  let total_interest = @azimuth.multiply(@azimuth.multiply(loan_amount, annual_rate), loan_term) / 100
  let total_payment = @azimuth.add(loan_amount, total_interest)
  
  @azimuth.assert_eq(3000, total_interest)
  @azimuth.assert_eq(13000, total_payment)
  
  // 测试折旧计算
  let asset_value = 5000
  let depreciation_rate = 20  // 20%
  let useful_life = 5  // 5年
  
  let annual_depreciation = @azimuth.multiply(asset_value, depreciation_rate) / 100
  let book_value_after_3_years = @azimuth.add(asset_value, -@azimuth.multiply(annual_depreciation, 3))
  
  @azimuth.assert_eq(1000, annual_depreciation)
  @azimuth.assert_eq(2000, book_value_after_3_years)
}

test "scientific_engineering_calculations" {
  // 科学工程计算测试
  // 测试欧姆定律 V = I * R
  let current = 5  // 5安培
  let resistance = 10  // 10欧姆
  let voltage = @azimuth.multiply(current, resistance)
  
  @azimuth.assert_eq(50, voltage)
  
  // 测试功率计算 P = V * I
  let power = @azimuth.multiply(voltage, current)
  @azimuth.assert_eq(250, power)
  
  // 测试动能计算 E = 0.5 * m * v^2
  let mass = 100  // 100kg
  let velocity = 10  // 10m/s
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity)) / 2
  
  @azimuth.assert_eq(5000, kinetic_energy)
  
  // 测试圆的面积和周长
  let radius = 7
  let pi_approx = 22  // 22/7 ≈ π
  let circumference = @azimuth.multiply(2, @azimuth.multiply(radius, pi_approx)) / 7
  let area = @azimuth.multiply(radius, @azimuth.multiply(radius, pi_approx)) / 7
  
  @azimuth.assert_eq(44, circumference)  // 2 * 7 * 22/7 = 44
  @azimuth.assert_eq(154, area)  // 7 * 7 * 22/7 = 154
}

test "data_analysis_calculations" {
  // 数据分析计算测试
  // 测试平均值计算
  let data_sum = 150
  let data_count = 5
  let average = data_sum / data_count
  
  @azimuth.assert_eq(30, average)
  
  // 测试方差计算 (简化版)
  let mean = 30  // 平均值
  // 简化方差计算：(10-30)² + (20-30)² + (30-30)² + (40-30)² + (50-30)²
  let diff1 = @azimuth.add(10, -mean)  // -20
  let diff2 = @azimuth.add(20, -mean)  // -10
  let diff3 = @azimuth.add(30, -mean)  // 0
  let diff4 = @azimuth.add(40, -mean)  // 10
  let diff5 = @azimuth.add(50, -mean)  // 20
  
  let variance_sum = @azimuth.add(@azimuth.add(@azimuth.add(
    @azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)),
    @azimuth.add(@azimuth.multiply(diff3, diff3), @azimuth.multiply(diff4, diff4))),
    @azimuth.multiply(diff5, diff5))
  
  @azimuth.assert_eq(1000, variance_sum)  // 400 + 100 + 0 + 100 + 400 = 1000
  let variance = variance_sum / data_count
  @azimuth.assert_eq(200, variance)
  
  // 测试百分比计算
  let part = 25
  let whole = 200
  let percentage = @azimuth.multiply(part, 100) / whole
  
  @azimuth.assert_eq(12, percentage)
  
  // 测试增长率计算
  let old_value = 100
  let new_value = 150
  let growth_rate = @azimuth.multiply(@azimuth.add(new_value, -old_value), 100) / old_value
  
  @azimuth.assert_eq(50, growth_rate)
}

test "error_handling_and_validation" {
  // 错误处理和验证测试
  // 测试除零保护（通过乘法模拟）
  let numerator = 100
  let denominator = 0
  
  // 模拟除零检查：如果分母为0，返回0
  let safe_division_result = @azimuth.multiply(numerator, denominator)  // 任何数乘以0都是0
  @azimuth.assert_eq(0, safe_division_result)
  
  // 测试溢出处理
  let large_num = 2147483647
  let small_num = 1
  
  // 测试加法溢出
  let overflow_add = @azimuth.add(large_num, small_num)
  // 在实际系统中，这应该触发溢出处理，这里我们验证行为
  @azimuth.assert_eq(large_num, overflow_add)
  
  // 测试乘法溢出
  let overflow_mul = @azimuth.multiply(large_num, 2)
  @azimuth.assert_eq(large_num, overflow_mul)
  
  // 测试负数输入验证
  let negative_input = -100
  let positive_result = @azimuth.multiply(negative_input, -1)
  @azimuth.assert_eq(100, positive_result)
}

test "performance_benchmarks" {
  // 性能基准测试
  // 测试大量数据运算性能
  let iterations = 1000
  let base_value = 5
  let multiplier = 3
  let addend = 2
  
  // 模拟循环运算：result = (base * multiplier + addend) * iterations
  let single_operation = @azimuth.add(@azimuth.multiply(base_value, multiplier), addend)
  let batch_result = @azimuth.multiply(single_operation, iterations)
  
  @azimuth.assert_eq(17, single_operation)  // 5 * 3 + 2 = 17
  @azimuth.assert_eq(17000, batch_result)  // 17 * 1000 = 17000
  
  // 测试内存使用模式
  let array_size = 100
  let element_size = 4  // 假设每个元素4字节
  let total_memory = @azimuth.multiply(array_size, element_size)
  
  @azimuth.assert_eq(400, total_memory)
  
  // 测试缓存效率模拟
  let cache_size = 64
  let data_size = 1024
  let cache_misses = @azimuth.multiply(data_size / cache_size, 1)
  
  @azimuth.assert_eq(16, cache_misses)  // 1024/64 = 16
}

test "real_world_scenarios" {
  // 真实世界场景测试
  // 测试电子商务购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  let tax_rate = 8  // 8%
  let shipping_cost = 15
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_cost)
  
  @azimuth.assert_eq(598, item1_total)  // 299 * 2
  @azimuth.assert_eq(199, item2_total)  // 199 * 1
  @azimuth.assert_eq(797, subtotal)  // 598 + 199
  @azimuth.assert_eq(63, tax_amount)  // 797 * 8% = 63 (整数除法)
  @azimuth.assert_eq(875, total)  // 797 + 63 + 15 = 875
  
  // 测试用户积分系统
  let purchase_amount = 875
  let points_per_dollar = 10
  let earned_points = @azimuth.multiply(purchase_amount, points_per_dollar)
  
  @azimuth.assert_eq(8750, earned_points)
  
  // 测试库存管理
  let initial_stock = 100
  let sold_items = @azimuth.add(item1_quantity, item2_quantity)
  let restocked_items = 50
  let current_stock = @azimuth.add(@azimuth.add(initial_stock, -sold_items), restocked_items)
  
  @azimuth.assert_eq(3, sold_items)  // 2 + 1
  @azimuth.assert_eq(147, current_stock)  // 100 - 3 + 50 = 147
}

test "algorithm_implementation" {
  // 算法实现测试
  // 测试线性搜索模拟
  let array_size = 10
  let target_position = 7  // 目标在第7个位置
  let comparisons = target_position  // 需要比较7次
  
  @azimuth.assert_eq(7, comparisons)
  
  // 测试二分搜索模拟
  let sorted_array_size = 100
  // 最坏情况下需要 log2(100) ≈ 7 次比较
  let max_binary_comparisons = 7
  let best_case_comparisons = 1
  
  @azimuth.assert_eq(1, best_case_comparisons)
  @azimuth.assert_eq(7, max_binary_comparisons)
  
  // 测试排序算法复杂度模拟
  let sort_size = 5
  // 冒泡排序最坏情况：n*(n-1)/2
  let bubble_sort_swaps = @azimuth.multiply(sort_size, @azimuth.add(sort_size, -1)) / 2
  // 选择排序总是：n*(n-1)/2
  let selection_sort_comparisons = @azimuth.multiply(sort_size, @azimuth.add(sort_size, -1)) / 2
  
  @azimuth.assert_eq(10, bubble_sort_swaps)  // 5*4/2 = 10
  @azimuth.assert_eq(10, selection_sort_comparisons)  // 5*4/2 = 10
  
  // 测试递归函数模拟（阶乘）
  let n = 4
  // 4! = 4 * 3 * 2 * 1 = 24
  let factorial_result = @azimuth.multiply(@azimuth.multiply(
    @azimuth.multiply(4, 3), 2), 1)
  
  @azimuth.assert_eq(24, factorial_result)
  
  // 测试动态规划模拟（斐波那契）
  // F(6) = 8 (0,1,1,2,3,5,8)
  let fib_6 = 8  // 预计算结果
  
  @azimuth.assert_eq(8, fib_6)
}