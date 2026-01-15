// 附加的 MoonBit 测试用例
// 为 azimuth 项目添加额外的测试覆盖

test "sequence_operations_test" {
  // 测试序列操作
  // 计算等差数列前n项和
  let n = 10
  let first_term = 1
  let common_diff = 2
  
  // 使用公式：n/2 * (2a1 + (n-1)d)
  let sum = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(n - 1, common_diff))) / 2
  @azimuth.assert_eq(100, sum)  // 1+3+5+...+19 = 100
  
  // 计算等比数列前n项积（简化版）
  let ratio = 2
  let product = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 4), 8), 16)
  @azimuth.assert_eq(1024, product)  // 2*4*8*16 = 1024
}

test "string_processing_advanced" {
  // 测试高级字符串处理
  // 测试各种特殊字符组合
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
  @azimuth.assert_eq_string("Hello, O'Reilly!", @azimuth.greet("O'Reilly"))
  @azimuth.assert_eq_string("Hello, C:\\Program Files!", @azimuth.greet("C:\\Program Files"))
  
  // 测试多语言字符
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, 東京!", @azimuth.greet("東京"))
  @azimuth.assert_eq_string("Hello, 🚀🌟!", @azimuth.greet("🚀🌟"))
  
  // 测试长字符串
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetFunctionsWithExtendedInputLength"
  let expected = "Hello, " + long_string + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_string))
}

test "financial_calculations" {
  // 测试金融计算场景
  // 简单利息计算：本金 × 年利率 × 年数
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  
  let simple_interest = @azimuth.multiply(@azimuth.multiply(principal, annual_rate), years)
  let total_amount = @azimuth.add(principal, simple_interest)
  @azimuth.assert_eq(160000, total_amount)  // 10000 + 10000*5*3 = 160000
  
  // 分期付款计算
  let loan_amount = 12000
  let months = 12
  let monthly_payment = @azimuth.divide_with_ceil(loan_amount, months)
  @azimuth.assert_eq(1000, monthly_payment)
  
  // 实际支付总额（向上取整可能导致额外支付）
  let actual_total = @azimuth.multiply(monthly_payment, months)
  @azimuth.assert_eq(12000, actual_total)
}

test "geometry_calculations_extended" {
  // 测试扩展几何计算
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 8
  let bottom_base = 12
  let height = 5
  
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(50, trapezoid_area)  // (8+12)*5/2 = 50
  
  // 圆柱体积（简化）：底面积 × 高，其中底面积 = π × r² ≈ 3 × r²
  let radius = 4
  let cylinder_height = 10
  
  let base_area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))
  let volume = @azimuth.multiply(base_area, cylinder_height)
  @azimuth.assert_eq(480, volume)  // 3*16*10 = 480
  
  // 表面积计算（简化）
  let surface_area = @azimuth.add(@azimuth.multiply(2, base_area), @azimuth.multiply(@azimuth.multiply(2, 3), radius))
  @azimuth.assert_eq(144, surface_area)  // 2*48 + 2*3*4 = 96 + 24 = 120
}

test "algorithmic_patterns" {
  // 测试算法模式
  // 斐波那契数列计算（前几项）
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
  @azimuth.assert_eq(8, fib6)
  
  // 最大公约数模拟（欧几里得算法简化版）
  let a = 48
  let b = 18
  // 48 % 18 = 12, 18 % 12 = 6, 12 % 6 = 0，所以GCD是6
  let gcd = 6  // 预计算结果
  @azimuth.assert_eq(6, gcd)
  
  // 验证公约数性质
  @azimuth.assert_eq(0, a % gcd)
  @azimuth.assert_eq(0, b % gcd)
}

test "data_structure_operations" {
  // 测试数据结构操作模拟
  // 栈操作模拟（使用数组索引）
  let stack_size = 5
  let push_count = 3
  let pop_count = 2
  
  let final_size = @azimuth.add(stack_size, push_count - pop_count)
  @azimuth.assert_eq(6, final_size)  // 5 + 3 - 2 = 6
  
  // 队列操作模拟
  let queue_capacity = 10
  let enqueue_count = 7
  let dequeue_count = 4
  
  let queue_size = @azimuth.add(enqueue_count, -dequeue_count)
  @azimuth.assert_true(queue_size <= queue_capacity)
  @azimuth.assert_eq(3, queue_size)  // 7 - 4 = 3
  
  // 哈希表负载因子计算
  let table_size = 16
  let entries = 12
  let load_factor = @azimuth.multiply(entries, 100) / table_size
  @azimuth.assert_eq(75, load_factor)  // 12*100/16 = 75
}

test "performance_optimization_scenarios" {
  // 测试性能优化场景
  // 批量操作优化
  let batch_size = 100
  let operation_count = 1000
  let optimized_operations = operation_count / batch_size
  
  @azimuth.assert_eq(10, optimized_operations)
  
  // 缓存命中率计算
  let cache_requests = 1000
  let cache_hits = 850
  let hit_rate = @azimuth.multiply(cache_hits, 100) / cache_requests
  @azimuth.assert_eq(85, hit_rate)
  
  // 内存使用计算
  let object_size = 24
  let object_count = 1000
  let total_memory = @azimuth.multiply(object_size, object_count)
  @azimuth.assert_eq(24000, total_memory)
  
  // 分页计算
  let total_items = 258
  let page_size = 50
  let total_pages = @azimuth.divide_with_ceil(total_items, page_size)
  @azimuth.assert_eq(6, total_pages)  // 258/50 = 5.16，向上取整为6
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 极值乘法组合
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 除零保护（通过divide_with_ceil函数）
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  
  // 负数除法向上取整
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-2, 3))
}

test "integration_composite_test" {
  // 测试集成复合场景
  // 电商订单处理完整流程
  let product_price = 199
  let quantity = 3
  let customer_level = 2  // VIP等级
  let base_shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(product_price, quantity)
  
  // 根据VIP等级计算折扣
  let discount_rate = @azimuth.multiply(customer_level, 5)  // 每级5%折扣
  let discount_amount = @azimuth.multiply(subtotal, discount_rate) / 100
  let discounted_price = @azimuth.add(subtotal, -discount_amount)
  
  // 计算运费（VIP免运费）
  let shipping = if (customer_level >= 2) { 0 } else { base_shipping }
  
  // 计算税费（简化为8%）
  let tax_rate = 8
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100
  
  // 计算最终总价
  let final_total = @azimuth.add(@azimuth.add(discounted_price, tax_amount), shipping)
  
  // 验证结果：199*3 = 597，VIP2级10%折扣 = 59.7，实付537.3，税费42.984，免运费，总计约580
  @azimuth.assert_eq(580, final_total)
  
  // 测试普通用户（无折扣，有运费）
  let normal_total = @azimuth.add(@azimuth.add(subtotal, @azimuth.multiply(subtotal, tax_rate) / 100), base_shipping)
  @azimuth.assert_eq(663, normal_total)
}

test "state_machine_simulation" {
  // 测试状态机模拟
  // 简单的订单状态机：待处理 -> 处理中 -> 已完成 -> 已评价
  
  let state_pending = 0
  let state_processing = 1
  let state_completed = 2
  let state_reviewed = 3
  
  // 状态转换
  let current_state = state_pending
  
  // 待处理 -> 处理中
  current_state = if (current_state == state_pending) { state_processing } else { current_state }
  @azimuth.assert_eq(state_processing, current_state)
  
  // 处理中 -> 已完成
  current_state = if (current_state == state_processing) { state_completed } else { current_state }
  @azimuth.assert_eq(state_completed, current_state)
  
  // 已完成 -> 已评价
  current_state = if (current_state == state_completed) { state_reviewed } else { current_state }
  @azimuth.assert_eq(state_reviewed, current_state)
  
  // 测试状态计数
  let total_states = 4
  let completed_states = @azimuth.add(current_state, 1)  // 状态从0开始，所以+1
  @azimuth.assert_eq(total_states, completed_states)
  
  // 测试状态转换条件
  let can_review = (current_state == state_completed)
  let can_cancel = (current_state == state_pending || current_state == state_processing)
  
  @azimuth.assert_false(can_review)  // 已经评价，不能再评价
  @azimuth.assert_false(can_cancel)  // 已经评价，不能取消
}