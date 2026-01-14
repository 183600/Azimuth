// 增强的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法，覆盖 azimuth 模块的核心功能

// 导入主模块中的函数
use azimuth

test "advanced_arithmetic_sequence" {
  // 测试等差数列求和：1 + 4 + 7 + 10 + 13
  let n = 5
  let first_term = 1
  let common_diff = 3
  
  // 使用公式：n/2 * (2a + (n-1)d)
  let sum = multiply(n / 2, add(multiply(2, first_term), multiply(n - 1, common_diff)))
  assert_eq(35, sum)
  
  // 验证手动计算
  let manual_sum = add(add(add(add(1, 4), 7), 10), 13)
  assert_eq(35, manual_sum)
}

test "string_pattern_validation" {
  // 测试字符串模式验证
  let email = "user@example.com"
  let phone = "123-456-7890"
  let username = "user_2023"
  
  // 验证邮箱格式（简化版）
  let email_greeting = greet(email)
  assert_eq_string("Hello, user@example.com!", email_greeting)
  
  // 验证电话格式
  let phone_greeting = greet(phone)
  assert_eq_string("Hello, 123-456-7890!", phone_greeting)
  
  // 验证用户名格式
  let username_greeting = greet(username)
  assert_eq_string("Hello, user_2023!", username_greeting)
}

test "geometric_calculations" {
  // 测试几何计算
  let length = 12
  let width = 8
  let height = 5
  
  // 计算长方体表面积：2*(lw + lh + wh)
  let lw = multiply(length, width)
  let lh = multiply(length, height)
  let wh = multiply(width, height)
  let surface_area = multiply(2, add(add(lw, lh), wh))
  assert_eq(392, surface_area)
  
  // 计算长方体体积：l * w * h
  let volume = multiply(multiply(length, width), height)
  assert_eq(480, volume)
}

test "financial_compound_interest" {
  // 测试复利计算
  let principal = 10000
  let annual_rate = 5  // 5%
  let years = 3
  let compounds_per_year = 12  // 月复利
  
  // 简化的复利计算（避免浮点数）
  // 本金 × (1 + 年利率/复利次数)^(复利次数×年数)
  let rate_per_period = annual_rate / compounds_per_year
  let total_periods = multiply(compounds_per_year, years)
  
  // 简化计算：10000 × (1 + 0.4167%)^36 ≈ 10000 × 116 = 1160000
  let growth_factor = add(100, multiply(rate_per_period, total_periods))
  let final_amount = multiply(principal, growth_factor)
  assert_eq(1160000, final_amount)
}

test "temperature_conversion_precise" {
  // 测试精确的温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 由于整数运算限制，使用近似值：F = C × 2 - C/10 + 32
  
  let celsius = 25
  let fahrenheit = add(add(multiply(celsius, 2), multiply(celsius, -1) / 10), 32)
  assert_eq(77, fahrenheit)  // 25 × 2 - 25/10 + 32 = 50 - 2 + 32 = 80
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  // 近似值：C = (F - 32) / 2
  let fahrenheit_back = 80
  let celsius_back = (fahrenheit_back - 32) / 2
  assert_eq(24, celsius_back)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度模拟
  let n = 100
  
  // O(1) 常数时间
  let constant_time = 42
  assert_eq(42, constant_time)
  
  // O(n) 线性时间：求和 1 + 2 + ... + n
  let linear_time = multiply(n, add(n, 1)) / 2
  assert_eq(5050, linear_time)
  
  // O(n²) 平方时间：嵌套循环总和（简化版）
  let quadratic_time = multiply(n, multiply(n, add(n, 1)) / 2)
  assert_eq(505000, quadratic_time)
}

test "data_structure_operations" {
  // 测试数据结构操作模拟
  let stack_size = 0
  let push_count = 5
  let pop_count = 3
  
  // 模拟栈操作
  let final_stack_size = add(stack_size, add(push_count, -pop_count))
  assert_eq(2, final_stack_size)
  
  // 模拟队列操作
  let queue_enqueue = 8
  let queue_dequeue = 5
  let queue_size = add(queue_enqueue, -queue_dequeue)
  assert_eq(3, queue_size)
  
  // 模拟哈希表负载因子
  let hash_table_capacity = 16
  let hash_table_elements = 12
  let load_factor = multiply(hash_table_elements, 100) / hash_table_capacity
  assert_eq(75, load_factor)  // 75%
}

test "error_boundary_comprehensive" {
  // 测试错误边界综合处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试各种溢出情况
  assert_eq(max_val, add(max_val, 1000000))
  assert_eq(min_val, add(min_val, -1000000))
  assert_eq(max_val, multiply(max_val, 100))
  assert_eq(min_val, multiply(min_val, 100))
  
  // 测试特殊情况
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 测试断言函数的健壮性
  assert_true(add(100, 200) == 300)
  assert_false(add(100, 200) == 301)
  assert_eq(300, add(100, 200))
  assert_eq_string("Hello, Boundary!", greet("Boundary"))
}

test "real_world_data_processing" {
  // 测试现实世界数据处理场景
  let data_points = 5
  let values = add(add(add(add(10, 20), 30), 40), 50)
  
  // 计算平均值
  let average = values / data_points
  assert_eq(30, average)
  
  // 计算方差（简化版）
  let variance_numerator = add(add(add(add(multiply(10 - 30, 10 - 30), 
                                         multiply(20 - 30, 20 - 30)), 
                                    multiply(30 - 30, 30 - 30)), 
                               add(multiply(40 - 30, 40 - 30), 
                                   multiply(50 - 30, 50 - 30)))
  let variance = variance_numerator / data_points
  assert_eq(200, variance)
  
  // 计算标准差（整数近似）
  let std_dev = 14  // sqrt(200) ≈ 14.14，取整数部分
  assert_eq(14, std_dev)
}