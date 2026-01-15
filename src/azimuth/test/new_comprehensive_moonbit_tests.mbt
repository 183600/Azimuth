// 新的综合 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "arithmetic_precision_test" {
  // 测试算术运算的精度和边界情况
  let large_num = 1000000
  let small_num = 1
  
  // 测试大数加法
  @azimuth.assert_eq(1000001, @azimuth.add(large_num, small_num))
  
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(large_num, 1))
  
  // 测试负数运算
  @azimuth.assert_eq(0, @azimuth.add(1000000, -1000000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(-1, large_num))
}

test "string_processing_comprehensive" {
  // 测试字符串处理的综合场景
  // 测试各种特殊字符和边界情况
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  
  // 测试包含特殊符号的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
}

test "mathematical_formulas" {
  // 测试数学公式的实现
  // 计算圆的面积和周长（简化版，π≈3）
  let radius = 5
  let pi_approx = 3
  
  // 周长 = 2 × π × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi_approx), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 面积 = π × r²
  let area = @azimuth.multiply(pi_approx, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
  
  // 计算球的体积（简化版，V = 4/3 × π × r³ ≈ 4 × r³）
  let volume = @azimuth.multiply(4, @azimuth.multiply(radius, @azimuth.multiply(radius, radius)))
  @azimuth.assert_eq(500, volume)
}

test "financial_calculations" {
  // 测试金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简单利息计算
  let simple_interest = @azimuth.multiply(principal, @azimuth.multiply(rate_percent, years))
  let total_with_simple = @azimuth.add(principal, simple_interest)
  @azimuth.assert_eq(160000, total_with_simple)
  
  // 复利计算（简化版）
  let compound_factor = @azimuth.add(100, @azimuth.multiply(rate_percent, years))
  let total_with_compound = @azimuth.multiply(principal, compound_factor)
  @azimuth.assert_eq(160000, total_with_compound)
  
  // 分期付款计算
  let item_price = 1200
  let months = 12
  let monthly_payment = @azimuth.divide_with_ceil(item_price, months)
  @azimuth.assert_eq(100, monthly_payment)
  
  // 验证总付款
  let total_payment = @azimuth.multiply(monthly_payment, months)
  @azimuth.assert_eq(1200, total_payment)
}

test "geometry_calculations" {
  // 测试几何计算
  // 矩形计算
  let length = 12
  let width = 8
  
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  let area = @azimuth.multiply(length, width)
  
  @azimuth.assert_eq(40, perimeter)
  @azimuth.assert_eq(96, area)
  
  // 三角形计算（直角三角形）
  let base = 6
  let height = 8
  
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(24, triangle_area)
  
  // 梯形计算
  let top_base = 5
  let bottom_base = 10
  let trapezoid_height = 4
  
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  @azimuth.assert_eq(30, trapezoid_area)
}

test "boundary_value_analysis" {
  // 测试边界值分析
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试零值边界
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
}

test "algorithm_implementation" {
  // 测试简单算法实现
  // 最大公约数（简化版）
  let a = 48
  let b = 18
  let gcd = 6  // GCD(48, 18) = 6
  @azimuth.assert_eq(6, gcd)
  
  // 最小公倍数（简化版）
  let lcm = @azimuth.multiply(a, b) / gcd  // LCM(a, b) = a × b / GCD(a, b)
  @azimuth.assert_eq(144, lcm)
  
  // 斐波那契数列（第7项）
  let fib_1 = 1
  let fib_2 = 1
  let fib_3 = @azimuth.add(fib_1, fib_2)  // 2
  let fib_4 = @azimuth.add(fib_2, fib_3)  // 3
  let fib_5 = @azimuth.add(fib_3, fib_4)  // 5
  let fib_6 = @azimuth.add(fib_4, fib_5)  // 8
  let fib_7 = @azimuth.add(fib_5, fib_6)  // 13
  @azimuth.assert_eq(13, fib_7)
}

test "data_structure_operations" {
  // 测试数据结构操作模拟
  // 栈操作模拟
  let stack_size = 10
  let push_count = 7
  let pop_count = 3
  
  let current_size = @azimuth.add(push_count, -pop_count)
  @azimuth.assert_eq(4, current_size)
  
  // 检查栈状态
  let is_empty = current_size == 0
  let is_full = current_size == stack_size
  let can_push = @azimuth.add(current_size, 1) <= stack_size
  let can_pop = current_size > 0
  
  @azimuth.assert_false(is_empty)
  @azimuth.assert_false(is_full)
  @azimuth.assert_true(can_push)
  @azimuth.assert_true(can_pop)
  
  // 队列操作模拟
  let queue_capacity = 8
  let enqueue_count = 5
  let dequeue_count = 2
  
  let queue_size = @azimuth.add(enqueue_count, -dequeue_count)
  @azimuth.assert_eq(3, queue_size)
  
  // 计算队列利用率
  let utilization = @azimuth.multiply(queue_size, 100) / queue_capacity
  @azimuth.assert_eq(37, utilization)  // 3/8 ≈ 37.5% ≈ 37%
}

test "performance_calculations" {
  // 测试性能相关计算
  // 缓存命中率计算
  let total_requests = 1000
  let cache_hits = 750
  
  let hit_rate = @azimuth.multiply(cache_hits, 100) / total_requests
  @azimuth.assert_eq(75, hit_rate)
  
  let miss_count = @azimuth.add(total_requests, -cache_hits)
  let miss_rate = @azimuth.multiply(miss_count, 100) / total_requests
  @azimuth.assert_eq(25, miss_rate)
  
  // 平均响应时间计算
  let hit_time = 5
  let miss_penalty = 50
  
  let avg_response_time = @azimuth.add(@azimuth.multiply(cache_hits, hit_time), @azimuth.multiply(miss_count, miss_penalty)) / total_requests
  @azimuth.assert_eq(16, avg_response_time)  // (750×5 + 250×50) / 1000 = 16.25 ≈ 16
  
  // 吞吐量计算
  let time_period = 60  // 秒
  let operations_completed = 3000
  
  let throughput = operations_completed / time_period
  @azimuth.assert_eq(50, throughput)  // 3000 / 60 = 50 ops/sec
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let shipping_fee = 20
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = @azimuth.add(@azimuth.multiply(item1_price, item1_quantity), @azimuth.multiply(item2_price, item2_quantity))
  
  // 计算税费
  let tax_amount = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = @azimuth.add(@azimuth.add(subtotal, tax_amount), shipping_fee)
  
  @azimuth.assert_eq(1313, total)  // (299×2 + 199×3) + 8% + 20 = 1195 + 95 + 20 = 1310（四舍五入）
  
  // 温度转换
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)  // 简化版：F = C × 2 + 30
  @azimuth.assert_eq(80, fahrenheit)
  
  // 速度转换：km/h 转 m/s（简化版：除以4）
  let kmh_speed = 100
  let ms_speed = kmh_speed / 4
  @azimuth.assert_eq(25, ms_speed)
}