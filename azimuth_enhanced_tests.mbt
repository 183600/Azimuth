// Azimuth 项目增强 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供全面的测试覆盖

// 导入必要的模块
use azimuth

test "boundary_value_multiplication" {
  // 边界值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与不同值的乘法
  @assertion.assert_eq(0, azimuth.multiply(max_val, 0))?
  @assertion.assert_eq(max_val, azimuth.multiply(max_val, 1))?
  @assertion.assert_eq(min_val, azimuth.multiply(max_val, -1))?
  
  // 测试最小值与不同值的乘法
  @assertion.assert_eq(0, azimuth.multiply(min_val, 0))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, 1))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
  
  // 测试边界值附近的乘法
  @assertion.assert_eq(2147483646, azimuth.multiply(max_val, 1))?
  @assertion.assert_eq(-2147483647, azimuth.multiply(min_val, 1))?
}

test "complex_string_scenarios" {
  // 复杂字符串场景测试
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result1 = azimuth.greet(special_chars)
  @assertion.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result1)?
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "User123Admin456"
  let result2 = azimuth.greet(alphanumeric)
  @assertion.assert_eq_string("Hello, User123Admin456!", result2)?
  
  // 测试包含空格的字符串
  let spaced_name = "  Leading and trailing spaces  "
  let result3 = azimuth.greet(spaced_name)
  @assertion.assert_eq_string("Hello,   Leading and trailing spaces  !", result3)?
}

test "mathematical_series_calculations" {
  // 数学级数计算测试
  // 计算等差数列前n项和：n/2 * (2a + (n-1)d)
  let n = 10
  let a = 3  // 首项
  let d = 2  // 公差
  
  // 使用简化公式：n * (首项 + 末项) / 2
  let last_term = azimuth.add(a, azimuth.multiply(n - 1, d))  // 末项 = 3 + 9*2 = 21
  let series_sum = azimuth.multiply(n, azimuth.add(a, last_term)) / 2
  @assertion.assert_eq(120, series_sum)?  // 10 * (3 + 21) / 2 = 120
  
  // 计算等比数列前n项和：a * (r^n - 1) / (r - 1)
  let first_term = 2
  let ratio = 3
  let terms = 4
  
  // 简化计算：2 + 6 + 18 + 54 = 80
  let term1 = first_term
  let term2 = azimuth.multiply(term1, ratio)
  let term3 = azimuth.multiply(term2, ratio)
  let term4 = azimuth.multiply(term3, ratio)
  let geo_sum = azimuth.add(azimuth.add(azimuth.add(term1, term2), term3), term4)
  @assertion.assert_eq(80, geo_sum)?
}

test "business_calculations" {
  // 商业计算测试
  // 计算折扣价格
  let original_price = 1000
  let discount_percentage = 15
  let discount_amount = azimuth.multiply(original_price, discount_percentage) / 100
  let discounted_price = azimuth.add(original_price, -discount_amount)
  @assertion.assert_eq(850, discounted_price)?
  
  // 计算利润率
  let cost_price = 500
  let selling_price = 750
  let profit = azimuth.add(selling_price, -cost_price)
  let profit_margin = azimuth.multiply(profit, 100) / selling_price
  @assertion.assert_eq(33, profit_margin)?  // 整数除法
  
  // 计算复合增长率
  let initial_value = 1000
  let final_value = 2000
  let years = 5
  // 简化计算：(最终值/初始值 - 1) * 100 / 年数
  let growth_rate = azimuth.multiply(azimuth.add(final_value / initial_value, -1) * 100, 1) / years
  @assertion.assert_eq(20, growth_rate)?
}

test "scientific_calculations" {
  // 科学计算测试
  // 计算动能：E = 1/2 * m * v^2
  let mass = 10
  let velocity = 5
  let kinetic_energy = azimuth.multiply(mass, azimuth.multiply(velocity, velocity)) / 2
  @assertion.assert_eq(125, kinetic_energy)?
  
  // 计算势能：E = m * g * h
  let height = 20
  let gravity = 10  // 简化重力加速度
  let potential_energy = azimuth.multiply(mass, azimuth.multiply(gravity, height))
  @assertion.assert_eq(2000, potential_energy)?
  
  // 计算功率：P = W / t
  let work = 5000
  let time = 10
  let power = work / time
  @assertion.assert_eq(500, power)?
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 模拟栈操作：push和pop
  let stack_size = 0
  let push_count = 5
  let pop_count = 3
  
  // Push操作
  stack_size = azimuth.add(stack_size, push_count)
  @assertion.assert_eq(5, stack_size)?
  
  // Pop操作
  stack_size = azimuth.add(stack_size, -pop_count)
  @assertion.assert_eq(2, stack_size)?
  
  // 模拟队列操作：enqueue和dequeue
  let queue_size = 0
  let enqueue_count = 8
  let dequeue_count = 6
  
  // Enqueue操作
  queue_size = azimuth.add(queue_size, enqueue_count)
  @assertion.assert_eq(8, queue_size)?
  
  // Dequeue操作
  queue_size = azimuth.add(queue_size, -dequeue_count)
  @assertion.assert_eq(2, queue_size)?
}

test "algorithm_simulation" {
  // 算法模拟测试
  // 模拟二分查找的比较次数
  let array_size = 1000
  let comparisons_per_level = 1
  let max_levels = 10  // log2(1000) ≈ 10
  
  // 最坏情况下的比较次数
  let worst_case_comparisons = azimuth.multiply(comparisons_per_level, max_levels)
  @assertion.assert_eq(10, worst_case_comparisons)?
  
  // 模拟冒泡排序的交换次数（最坏情况）
  let n = 5
  // 最坏情况下：n*(n-1)/2
  let max_swaps = azimuth.multiply(n, azimuth.add(n, -1)) / 2
  @assertion.assert_eq(10, max_swaps)?
  
  // 模拟选择排序的比较次数
  // 总比较次数：n*(n-1)/2
  let total_comparisons = azimuth.multiply(n, azimuth.add(n, -1)) / 2
  @assertion.assert_eq(10, total_comparisons)?
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续溢出操作
  let overflow1 = azimuth.add(max_val, 1)
  let overflow2 = azimuth.add(overflow1, 1)
  @assertion.assert_eq(max_val, overflow2)?
  
  // 测试连续下溢操作
  let underflow1 = azimuth.add(min_val, -1)
  let underflow2 = azimuth.add(underflow1, -1)
  @assertion.assert_eq(min_val, underflow2)?
  
  // 测试混合溢出操作
  let mixed1 = azimuth.multiply(max_val, 2)
  let mixed2 = azimuth.add(mixed1, 100)
  @assertion.assert_eq(max_val, mixed2)?
  
  // 测试极值间的运算
  let extreme1 = azimuth.add(max_val, min_val)
  let extreme2 = azimuth.multiply(extreme1, -1)
  @assertion.assert_eq(1, extreme2)?
}

test "performance_optimization_scenarios" {
  // 性能优化场景测试
  // 模拟缓存命中率计算
  let total_requests = 1000
  let cache_hits = 850
  let hit_rate = azimuth.multiply(cache_hits, 100) / total_requests
  @assertion.assert_eq(85, hit_rate)?
  
  // 模拟数据库查询优化
  let original_query_time = 1000  // 毫秒
  let optimization_factor = 5
  let optimized_time = original_query_time / optimization_factor
  @assertion.assert_eq(200, optimized_time)?
  
  // 模拟内存使用优化
  let original_memory = 10000  // KB
  let compression_ratio = 40  // 压缩到40%
  let compressed_memory = azimuth.multiply(original_memory, compression_ratio) / 100
  @assertion.assert_eq(4000, compressed_memory)?
  
  // 计算性能提升百分比
  let performance_improvement = azimuth.multiply(
    azimuth.add(original_query_time, -optimized_time), 100) / original_query_time
  @assertion.assert_eq(80, performance_improvement)?
}

test "real_world_application_integration" {
  // 实际应用集成测试
  // 模拟电子商务订单处理
  let product_price = 199
  let quantity = 3
  let customer_discount = 10  // 10%
  let loyalty_points = 500
  let point_value = 1  // 每点价值1分
  
  // 计算商品总价
  let subtotal = azimuth.multiply(product_price, quantity)
  
  // 应用客户折扣
  let discount_amount = azimuth.multiply(subtotal, customer_discount) / 100
  let discounted_total = azimuth.add(subtotal, -discount_amount)
  
  // 应用积分抵扣
  let points_value = azimuth.multiply(loyalty_points, point_value) / 100  // 转换为元
  let final_total = azimuth.add(discounted_total, -points_value)
  
  @assertion.assert_eq(496, final_total)?  // (199*3 - 10%) - 5元 = 597 - 60 - 5 = 532
  
  // 生成订单确认消息
  let order_id = 12345
  let customer_name = "张三"
  let greeting = azimuth.greet(customer_name)
  // 注意：这里我们只能测试greet函数，不能测试完整的字符串拼接
  @assertion.assert_eq_string("Hello, 张三!", greeting)?
}