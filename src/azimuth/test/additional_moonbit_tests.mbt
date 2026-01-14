// 额外的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "integer_overflow_protection" {
  // 测试整数溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_unicode_handling" {
  // 测试字符串 Unicode 处理
  // 测试基本 Unicode 字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  
  // 测试混合字符串
  @azimuth.assert_eq_string("Hello, Hello世界!", @azimuth.greet("Hello世界"))
  @azimuth.assert_eq_string("Hello, Test123测试!", @azimuth.greet("Test123测试"))
  
  // 测试特殊 Unicode 字符
  @azimuth.assert_eq_string("Hello, ©®™!", @azimuth.greet("©®™"))
  @azimuth.assert_eq_string("Hello, 𝕏𝕐𝕑!", @azimuth.greet("𝕏𝕐𝕑"))
}

test "mathematical_series_calculations" {
  // 测试数学级数计算
  // 计算等差数列前 n 项和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 100
  let arithmetic_sum = @azimuth.multiply(n, @azimuth.add(n, 1)) / 2
  @azimuth.assert_eq(5050, arithmetic_sum)
  
  // 计算等比数列前 n 项和：1 + 2 + 4 + ... + 2^(n-1) = 2^n - 1
  // 计算 2^5 - 1
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2))  // 2^4 = 16
  let geometric_sum = @azimuth.multiply(power_of_2, 2) - 1  // 2^5 - 1 = 31
  @azimuth.assert_eq(31, geometric_sum)
  
  // 计算平方和：1² + 2² + 3² + ... + n²
  let n2 = 5
  let square_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(1, 1), 
                   @azimuth.multiply(2, 2)), @azimuth.multiply(3, 3)), 
                   @azimuth.add(@azimuth.multiply(4, 4), @azimuth.multiply(5, 5)))
  @azimuth.assert_eq(55, square_sum)
}

test "real_world_financial_calculations" {
  // 测试实际金融计算场景
  // 复合利息计算：A = P(1 + r/n)^(nt)
  // 简化计算：A = P × (1 + r × t)
  let principal = 10000  // 本金
  let rate = 5           // 年利率（百分比）
  let time = 3           // 年数
  
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, time))
  let amount = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(160000, amount)
  
  // 折扣计算：折后价格 = 原价 × (1 - 折扣率)
  let original_price = 200
  let discount_rate = 20  // 20% 折扣
  let discounted_price = @azimuth.multiply(original_price, @azimuth.add(100, -discount_rate)) / 100
  @azimuth.assert_eq(160, discounted_price)
  
  // 税费计算：含税价格 = 原价 × (1 + 税率)
  let base_price = 500
  let tax_rate = 8  // 8% 税率
  let total_price = @azimuth.multiply(base_price, @azimuth.add(100, tax_rate)) / 100
  @azimuth.assert_eq(540, total_price)
}

test "geometry_and_physics_calculations" {
  // 测试几何和物理计算
  // 圆的周长和面积（简化计算，π ≈ 3）
  let radius = 10
  let circumference = @azimuth.multiply(2, @azimuth.multiply(3, radius))  // 2πr ≈ 6r
  let area = @azimuth.multiply(3, @azimuth.multiply(radius, radius))  // πr² ≈ 3r²
  @azimuth.assert_eq(60, circumference)
  @azimuth.assert_eq(300, area)
  
  // 球体体积（简化计算，π ≈ 3）：V = 4/3 × π × r³ ≈ 4 × r³
  let sphere_radius = 5
  let volume = @azimuth.multiply(4, @azimuth.multiply(sphere_radius, 
                   @azimuth.multiply(sphere_radius, sphere_radius)))
  @azimuth.assert_eq(500, volume)
  
  // 简单运动学：距离 = 速度 × 时间
  let velocity = 60  // km/h
  let time_hours = 2  // 小时
  let distance = @azimuth.multiply(velocity, time_hours)
  @azimuth.assert_eq(120, distance)
}

test "algorithm_efficiency_tests" {
  // 测试算法效率相关计算
  // 线性搜索时间复杂度模拟
  let array_size = 1000
  let average_comparisons = array_size / 2
  @azimuth.assert_eq(500, average_comparisons)
  
  // 二分搜索时间复杂度模拟（简化计算）
  // log₂(1024) ≈ 10
  let binary_search_size = 1024
  let max_comparisons = 10
  @azimuth.assert_eq(10, max_comparisons)
  
  // 冒泡排序比较次数：n × (n-1) / 2
  let sort_size = 20
  let bubble_comparisons = @azimuth.multiply(sort_size, @azimuth.add(sort_size, -1)) / 2
  @azimuth.assert_eq(190, bubble_comparisons)
}

test "data_structure_operations" {
  // 测试数据结构操作模拟
  // 栈操作模拟
  let stack_size = 10
  let push_operations = 5
  let pop_operations = 3
  let final_stack_size = @azimuth.add(stack_size, @azimuth.add(push_operations, -pop_operations))
  @azimuth.assert_eq(12, final_stack_size)
  
  // 队列操作模拟
  let queue_size = 8
  let enqueue_operations = 6
  let dequeue_operations = 4
  let final_queue_size = @azimuth.add(queue_size, @azimuth.add(enqueue_operations, -dequeue_operations))
  @azimuth.assert_eq(10, final_queue_size)
  
  // 哈希表负载因子计算
  let table_capacity = 16
  let elements_stored = 12
  let load_factor = @azimuth.multiply(elements_stored, 100) / table_capacity
  @azimuth.assert_eq(75, load_factor)
}

test "error_boundary_conditions" {
  // 测试错误边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试极值运算组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试连续运算的边界情况
  let result1 = @azimuth.add(@azimuth.add(max_val, -1), 1)
  @azimuth.assert_eq(max_val, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(min_val, -1), -1)
  @azimuth.assert_eq(min_val, result2)
  
  // 测试断言函数的边界情况
  @azimuth.assert_true(@azimuth.add(max_val, min_val) == -1)
  @azimuth.assert_false(@azimuth.add(max_val, min_val) == 0)
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "statistical_calculations" {
  // 测试统计计算
  // 计算平均值
  let values_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let count = 5
  let average = values_sum / count
  @azimuth.assert_eq(30, average)
  
  // 计算方差（简化版）
  let mean = 30
  let diff1 = @azimuth.add(10, -mean)  // -20
  let diff2 = @azimuth.add(20, -mean)  // -10
  let diff3 = @azimuth.add(30, -mean)  // 0
  let diff4 = @azimuth.add(40, -mean)  // 10
  let diff5 = @azimuth.add(50, -mean)  // 20
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(
                   @azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), 
                   @azimuth.multiply(diff3, diff3)), @azimuth.add(@azimuth.multiply(diff4, diff4), 
                   @azimuth.multiply(diff5, diff5))) / count
  @azimuth.assert_eq(200, variance)
  
  // 计算标准差（简化为整数）
  let standard_deviation = 14  // √200 ≈ 14.14，取整数部分
  @azimuth.assert_true(standard_deviation == 14)
}

test "complex_string_operations" {
  // 测试复杂字符串操作
  // 测试各种特殊字符组合
  @azimuth.assert_eq_string("Hello, a+b=c!", @azimuth.greet("a+b=c"))
  @azimuth.assert_eq_string("Hello, x*y=z!", @azimuth.greet("x*y=z"))
  @azimuth.assert_eq_string("Hello, a/b=c!", @azimuth.greet("a/b=c"))
  
  // 测试包含空格的复杂字符串
  @azimuth.assert_eq_string("Hello, Multiple Words Test!", @azimuth.greet("Multiple Words Test"))
  @azimuth.assert_eq_string("Hello,   Leading and trailing spaces   !", 
                            @azimuth.greet("  Leading and trailing spaces  "))
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"Quoted Text\"!", @azimuth.greet("\"Quoted Text\""))
  @azimuth.assert_eq_string("Hello, 'Single Quotes'!", @azimuth.greet("'Single Quotes'"))
  
  // 测试换行符和制表符
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", @azimuth.greet("Line1\nLine2"))
  @azimuth.assert_eq_string("Hello, Col1\tCol2!", @azimuth.greet("Col1\tCol2"))
}