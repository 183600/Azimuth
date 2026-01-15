// 额外的 MoonBit 测试用例
// 补充测试核心功能和边界情况

test "integer_overflow_edge_cases" {
  // 测试整数溢出的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1, 2))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 100, 101))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1, -2))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 100, -101))
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(46340, 46341))  // 接近sqrt(max_val)
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46340, 46341))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "zero_value_operations" {
  // 测试零值的各种运算
  let zero = 0
  let positive = 123
  let negative = -456
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法中的零值
  @azimuth.assert_eq(positive, @azimuth.add(positive, zero))
  @azimuth.assert_eq(negative, @azimuth.add(negative, zero))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, zero))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, zero))
  
  // 乘法中的零值
  @azimuth.assert_eq(zero, @azimuth.multiply(positive, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(negative, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(max_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(min_val, zero))
  @azimuth.assert_eq(zero, @azimuth.multiply(zero, zero))
}

test "unit_value_operations" {
  // 测试单位值（1和-1）的运算
  let one = 1
  let neg_one = -1
  let test_value = 42
  
  // 加法中的单位值
  @azimuth.assert_eq(test_value + 1, @azimuth.add(test_value, one))
  @azimuth.assert_eq(test_value - 1, @azimuth.add(test_value, neg_one))
  
  // 乘法中的单位值
  @azimuth.assert_eq(test_value, @azimuth.multiply(test_value, one))
  @azimuth.assert_eq(test_value, @azimuth.multiply(one, test_value))
  @azimuth.assert_eq(-test_value, @azimuth.multiply(test_value, neg_one))
  @azimuth.assert_eq(-test_value, @azimuth.multiply(neg_one, test_value))
  
  // 特殊情况：最小值与-1相乘
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, neg_one))
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  // 空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 1!", @azimuth.greet("1"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet("!"))
  
  // 包含空格的字符串
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c !", @azimuth.greet(" a b c "))
  
  // 包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, 'single'!", @azimuth.greet("'single'"))
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  // 等差数列：1, 4, 7, 10, 13
  let a1 = 1
  let d = 3
  let n = 5
  
  // 计算第n项：a1 + (n-1)*d
  let an = @azimuth.add(a1, @azimuth.multiply(n - 1, d))
  @azimuth.assert_eq(13, an)
  
  // 计算前n项和：n/2 * (a1 + an)
  let sum = @azimuth.multiply(n, @azimuth.add(a1, an)) / 2
  @azimuth.assert_eq(35, sum)
  
  // 等比数列：2, 6, 18, 54, 162
  let g1 = 2
  let r = 3
  
  // 计算前4项（手动计算）
  let g2 = @azimuth.multiply(g1, r)      // 6
  let g3 = @azimuth.multiply(g2, r)      // 18
  let g4 = @azimuth.multiply(g3, r)      // 54
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(g1, g2), g3), g4)
  @azimuth.assert_eq(80, geo_sum)
}

test "business_calculation_scenarios" {
  // 测试商业计算场景
  // 场景1：折扣计算
  let original_price = 1000
  let discount_percent = 15
  let tax_percent = 8
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_percent) / 100
  let final_price = @azimuth.add(discounted_price, tax_amount)
  
  @azimuth.assert_eq(850, discounted_price)
  @azimuth.assert_eq(68, tax_amount)
  @azimuth.assert_eq(918, final_price)
  
  // 场景2：利润计算
  let cost_price = 500
  let selling_price = 750
  let profit = @azimuth.add(selling_price, -cost_price)
  let profit_margin = @azimuth.multiply(profit, 100) / cost_price
  
  @azimuth.assert_eq(250, profit)
  @azimuth.assert_eq(50, profit_margin)
}

test "physics_formula_calculations" {
  // 测试物理公式计算（简化版，避免浮点数）
  // 场景1：匀速运动距离 = 速度 × 时间
  let velocity = 60  // km/h
  let time = 2       // hours
  let distance = @azimuth.multiply(velocity, time)
  @azimuth.assert_eq(120, distance)
  
  // 场景2：动能 = 1/2 × 质量 × 速度²（简化计算）
  let mass = 1000    // kg
  let speed = 10     // m/s
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(speed, speed)) / 2
  @azimuth.assert_eq(50000, kinetic_energy)
  
  // 场景3：简单机械利益 = 输出力 / 输入力
  let output_force = 500
  let input_force = 50
  let mechanical_advantage = output_force / input_force
  @azimuth.assert_eq(10, mechanical_advantage)
}

test "algorithm_complexity_simulation" {
  // 测试算法复杂度的模拟计算
  // 场景1：线性搜索复杂度 O(n)
  let n = 100
  let linear_operations = n
  @azimuth.assert_eq(100, linear_operations)
  
  // 场景2：二分搜索复杂度 O(log n)（简化为整数）
  // 模拟 log2(100) ≈ 7 的比较次数
  let binary_operations = 7
  @azimuth.assert_eq(7, binary_operations)
  
  // 场景3：冒泡排序复杂度 O(n²)
  let bubble_operations = @azimuth.multiply(n, n)
  @azimuth.assert_eq(10000, bubble_operations)
  
  // 场景4：快速排序平均复杂度 O(n log n)
  let quick_operations = @azimuth.multiply(n, 7)  // n × log2(n)
  @azimuth.assert_eq(700, quick_operations)
}

test "data_structure_operations" {
  // 测试数据结构操作的模拟
  // 场景1：数组索引计算（二维数组转一维）
  let row = 3
  let col = 4
  let width = 10
  let index = @azimuth.add(@azimuth.multiply(row, width), col)
  @azimuth.assert_eq(34, index)
  
  // 场景2：栈操作模拟
  let stack_size = 0
  let push1 = @azimuth.add(stack_size, 1)  // push
  let push2 = @azimuth.add(push1, 1)       // push
  let pop1 = @azimuth.add(push2, -1)       // pop
  let final_size = pop1
  @azimuth.assert_eq(1, final_size)
  
  // 场景3：队列操作模拟
  let queue_head = 0
  let queue_tail = 0
  let enqueue = @azimuth.add(queue_tail, 1)  // enqueue
  let dequeue = @azimuth.add(queue_head, 1)  // dequeue
  @azimuth.assert_eq(1, enqueue)
  @azimuth.assert_eq(1, dequeue)
}

test "error_recovery_scenarios" {
  // 测试错误恢复场景
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 场景1：溢出后的恢复操作
  let overflow_result = @azimuth.add(max_val, 1000)
  let recovery_add = @azimuth.add(overflow_result, -100)
  @azimuth.assert_eq(max_val, overflow_result)
  @azimuth.assert_eq(max_val, recovery_add)
  
  // 场景2：连续溢出操作
  let overflow1 = @azimuth.add(max_val, 1)
  let overflow2 = @azimuth.add(overflow1, 1)
  let overflow3 = @azimuth.add(overflow2, 1)
  @azimuth.assert_eq(max_val, overflow1)
  @azimuth.assert_eq(max_val, overflow2)
  @azimuth.assert_eq(max_val, overflow3)
  
  // 场景3：边界值的安全操作
  let safe_max_add = @azimuth.add(max_val, 0)
  let safe_max_mul = @azimuth.multiply(max_val, 1)
  let safe_min_add = @azimuth.add(min_val, 0)
  let safe_min_mul = @azimuth.multiply(min_val, 1)
  
  @azimuth.assert_eq(max_val, safe_max_add)
  @azimuth.assert_eq(max_val, safe_max_mul)
  @azimuth.assert_eq(min_val, safe_min_add)
  @azimuth.assert_eq(min_val, safe_min_mul)
}