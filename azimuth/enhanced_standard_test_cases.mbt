// 增强标准测试用例 - 10个高质量 MoonBit 测试用例
// 为 azimuth 库添加额外的测试覆盖

test "advanced_mathematical_sequences" {
  // 高级数学序列测试
  let fib_start = 5
  let fib_next = 8
  
  // 斐波那契序列验证
  let fib_sum = @azimuth.add(fib_start, fib_next)
  @azimuth.assert_eq(13, fib_sum)
  
  // 几何序列测试
  let geometric_base = 3
  let geometric_ratio = 4
  let geometric_result = @azimuth.multiply(geometric_base, geometric_ratio)
  @azimuth.assert_eq(12, geometric_result)
  
  // 算术序列测试
  let arithmetic_first = 10
  let arithmetic_diff = 7
  let arithmetic_second = @azimuth.add(arithmetic_first, arithmetic_diff)
  @azimuth.assert_eq(17, arithmetic_second)
}

test "statistical_calculations" {
  // 统计计算测试
  let values_count = 8
  let sum_values = 156
  
  // 计算平均值
  let average = @azimuth.divide_with_ceil(sum_values, values_count)
  @azimuth.assert_eq(20, average)
  
  // 计算总和验证
  let verified_sum = @azimuth.multiply(average, values_count)
  @azimuth.assert_eq(160, verified_sum)
  
  // 计算余数
  let remainder = @azimuth.subtract(verified_sum, sum_values)
  @azimuth.assert_eq(4, remainder)
}

test "inventory_optimization" {
  // 库存优化测试
  let product_a_qty = 45
  let product_b_qty = 67
  let product_c_qty = 23
  
  let package_capacity = 20
  
  // 计算每种产品需要的包装数
  let packages_a = @azimuth.divide_with_ceil(product_a_qty, package_capacity)
  let packages_b = @azimuth.divide_with_ceil(product_b_qty, package_capacity)
  let packages_c = @azimuth.divide_with_ceil(product_c_qty, package_capacity)
  
  @azimuth.assert_eq(3, packages_a)
  @azimuth.assert_eq(4, packages_b)
  @azimuth.assert_eq(2, packages_c)
  
  // 计算总包装数
  let total_packages = @azimuth.add(@azimuth.add(packages_a, packages_b), packages_c)
  @azimuth.assert_eq(9, total_packages)
  
  // 计算总容量
  let total_capacity = @azimuth.multiply(total_packages, package_capacity)
  @azimuth.assert_eq(180, total_capacity)
  
  // 计算总产品数
  let total_products = @azimuth.add(@azimuth.add(product_a_qty, product_b_qty), product_c_qty)
  @azimuth.assert_eq(135, total_products)
  
  // 计算剩余容量
  let remaining_capacity = @azimuth.subtract(total_capacity, total_products)
  @azimuth.assert_eq(45, remaining_capacity)
}

test "time_management_calculations" {
  // 时间管理计算测试
  let work_hours_per_day = 8
  let work_days_per_week = 5
  let tasks_per_hour = 3
  let total_tasks = 120
  
  // 计算每周工作小时数
  let weekly_hours = @azimuth.multiply(work_hours_per_day, work_days_per_week)
  @azimuth.assert_eq(40, weekly_hours)
  
  // 计算每周任务处理能力
  let weekly_capacity = @azimuth.multiply(weekly_hours, tasks_per_hour)
  @azimuth.assert_eq(120, weekly_capacity)
  
  // 计算需要的周数
  let weeks_needed = @azimuth.divide_with_ceil(total_tasks, weekly_capacity)
  @azimuth.assert_eq(1, weeks_needed)
  
  // 计算每天需要处理的任务数
  let daily_tasks = @azimuth.divide_with_ceil(total_tasks, @azimuth.multiply(weeks_needed, work_days_per_week))
  @azimuth.assert_eq(24, daily_tasks)
}

test "financial_planning_scenario" {
  // 财务规划场景测试
  let monthly_income = 5000
  let monthly_expenses = 3200
  let savings_goal = 18000
  let emergency_fund = 6000
  
  // 计算月度储蓄
  let monthly_savings = @azimuth.subtract(monthly_income, monthly_expenses)
  @azimuth.assert_eq(1800, monthly_savings)
  
  // 计算达到储蓄目标需要的月数
  let months_for_goal = @azimuth.divide_with_ceil(savings_goal, monthly_savings)
  @azimuth.assert_eq(10, months_for_goal)
  
  // 计算建立应急基金需要的月数
  let months_for_emergency = @azimuth.divide_with_ceil(emergency_fund, monthly_savings)
  @azimuth.assert_eq(4, months_for_emergency)
  
  // 计算总储蓄目标
  let total_savings_goal = @azimuth.add(savings_goal, emergency_fund)
  @azimuth.assert_eq(24000, total_savings_goal)
  
  // 计算达到总目标需要的月数
  let months_for_total = @azimuth.divide_with_ceil(total_savings_goal, monthly_savings)
  @azimuth.assert_eq(14, months_for_total)
}

test "algorithm_efficiency_comparison" {
  // 算法效率比较测试
  let dataset_size = 1000
  
  // O(n) 算法模拟
  let linear_operations = @azimuth.multiply(dataset_size, 5)
  @azimuth.assert_eq(5000, linear_operations)
  
  // O(log n) 算法模拟（简化）
  let log_operations = @azimuth.divide_with_ceil(dataset_size, 50)
  @azimuth.assert_eq(20, log_operations)
  
  // O(n²) 算法模拟（简化）
  let quadratic_operations = @azimuth.multiply(dataset_size, @azimuth.divide_with_ceil(dataset_size, 100))
  @azimuth.assert_eq(10000, quadratic_operations)
  
  // 计算效率比
  let linear_vs_log = @azimuth.divide_with_ceil(linear_operations, log_operations)
  @azimuth.assert_eq(250, linear_vs_log)
  
  let quadratic_vs_linear = @azimuth.divide_with_ceil(quadratic_operations, linear_operations)
  @azimuth.assert_eq(2, quadratic_vs_linear)
}

test "data_structure_operations" {
  // 数据结构操作测试
  // 模拟栈操作
  let stack_initial_size = 0
  let push_operations = 15
  let pop_operations = 7
  
  let stack_after_push = @azimuth.add(stack_initial_size, push_operations)
  @azimuth.assert_eq(15, stack_after_push)
  
  let stack_final_size = @azimuth.subtract(stack_after_push, pop_operations)
  @azimuth.assert_eq(8, stack_final_size)
  
  // 模拟队列操作
  let queue_capacity = 20
  let enqueue_operations = 25
  let dequeue_operations = 18
  
  let queue_final_size = @azimuth.subtract(enqueue_operations, dequeue_operations)
  @azimuth.assert_eq(7, queue_final_size)
  
  // 计算剩余队列容量
  let remaining_capacity = @azimuth.subtract(queue_capacity, queue_final_size)
  @azimuth.assert_eq(13, remaining_capacity)
}

test "network_bandwidth_calculations" {
  // 网络带宽计算测试
  let file_size_mb = 850
  let network_speed_mbps = 50
  let overhead_factor = 10  // 10% 开销
  
  // 计算原始传输时间（秒）
  let raw_time_seconds = @azimuth.divide_with_ceil(file_size_mb, network_speed_mbps)
  @azimuth.assert_eq(17, raw_time_seconds)
  
  // 计算包含开销的时间
  let overhead_time = @azimuth.divide_with_ceil(@azimuth.multiply(raw_time_seconds, overhead_factor), 100)
  @azimuth.assert_eq(2, overhead_time)
  
  let total_time = @azimuth.add(raw_time_seconds, overhead_time)
  @azimuth.assert_eq(19, total_time)
  
  // 计算实际传输速度
  let actual_speed = @azimuth.divide_with_ceil(file_size_mb, total_time)
  @azimuth.assert_eq(45, actual_speed)
  
  // 计算速度损失
  let speed_loss = @azimuth.subtract(network_speed_mbps, actual_speed)
  @azimuth.assert_eq(5, speed_loss)
}

test "caching_strategy_optimization" {
  // 缓存策略优化测试
  let cache_size = 100
  let total_requests = 500
  let cache_hit_rate = 75  // 75%
  
  // 计算缓存命中数
  let cache_hits = @azimuth.divide_with_ceil(@azimuth.multiply(total_requests, cache_hit_rate), 100)
  @azimuth.assert_eq(375, cache_hits)
  
  // 计算缓存未命中数
  let cache_misses = @azimuth.subtract(total_requests, cache_hits)
  @azimuth.assert_eq(125, cache_misses)
  
  // 计算缓存利用率
  let cache_utilization = @azimuth.divide_with_ceil(@azimuth.multiply(cache_hits, 100), total_requests)
  @azimuth.assert_eq(75, cache_utilization)
  
  // 计算需要的缓存大小（假设每个请求占用1个单位）
  let optimal_cache_size = @azimuth.divide_with_ceil(cache_hits, 2)
  @azimuth.assert_eq(188, optimal_cache_size)
  
  // 计算需要增加的缓存大小
  let additional_cache_needed = @azimuth.subtract(optimal_cache_size, cache_size)
  @azimuth.assert_eq(88, additional_cache_needed)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种字符串格式
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  @azimuth.assert_eq_string("Hello, camelCase!", @azimuth.greet("camelCase"))
  @azimuth.assert_eq_string("Hello, snake_case!", @azimuth.greet("snake_case"))
  @azimuth.assert_eq_string("Hello, kebab-case!", @azimuth.greet("kebab-case"))
  @azimuth.assert_eq_string("Hello, PascalCase!", @azimuth.greet("PascalCase"))
  
  // 测试特殊符号
  @azimuth.assert_eq_string("Hello, C++!", @azimuth.greet("C++"))
  @azimuth.assert_eq_string("Hello, C#!", @azimuth.greet("C#"))
  @azimuth.assert_eq_string("Hello, Node.js!", @azimuth.greet("Node.js"))
  @azimuth.assert_eq_string("Hello, TypeScript!", @azimuth.greet("TypeScript"))
  @azimuth.assert_eq_string("Hello, HTML5!", @azimuth.greet("HTML5"))
  
  // 测试数字和字母混合
  @azimuth.assert_eq_string("Hello, test2024!", @azimuth.greet("test2024"))
  @azimuth.assert_eq_string("Hello, v1.0.0!", @azimuth.greet("v1.0.0"))
  @azimuth.assert_eq_string("Hello, API_v2!", @azimuth.greet("API_v2"))
}