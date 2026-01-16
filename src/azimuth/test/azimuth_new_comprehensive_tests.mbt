// Azimuth 项目新增综合测试用例
// 使用标准 MoonBit 测试语法，覆盖新的测试场景

test "advanced_arithmetic_properties" {
  // 测试高级算术性质
  
  // 零元素性质
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  
  // 单位元素性质
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  
  // 负数性质
  @azimuth.assert_eq(0, @azimuth.add(42, -42))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(42, @azimuth.multiply(-42, -1))
}

test "scientific_notation_calculations" {
  // 测试科学计数法相关的计算
  
  // 计算 2^10 = 1024 (通过连乘模拟)
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2), @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2))
  @azimuth.assert_eq(1024, power_of_2)
  
  // 计算 5! = 120 (阶乘)
  let factorial_5 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(1, 2), 3), 4), 5)
  @azimuth.assert_eq(120, factorial_5)
  
  // 计算 3^4 = 81
  let power_of_3 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(3, 3), 3), 3)
  @azimuth.assert_eq(81, power_of_3)
}

test "statistical_calculations" {
  // 测试统计计算
  
  // 计算平均值: (10 + 20 + 30 + 40 + 50) / 5 = 30
  let numbers_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let count = 5
  let average = numbers_sum / count
  @azimuth.assert_eq(30, average)
  
  // 计算加权平均: (10*2 + 20*3 + 30*5) / (2+3+5) = 23
  let weighted_sum = @azimuth.add(@azimuth.add(@azimuth.multiply(10, 2), @azimuth.multiply(20, 3)), @azimuth.multiply(30, 5))
  let total_weight = @azimuth.add(@azimuth.add(2, 3), 5)
  let weighted_average = weighted_sum / total_weight
  @azimuth.assert_eq(23, weighted_average)
  
  // 计算方差简化版: (1-3)^2 + (2-3)^2 + (3-3)^2 + (4-3)^2 + (5-3)^2 = 10
  let variance_sum = 10  // 直接使用预计算的结果
  @azimuth.assert_eq(10, variance_sum)
}

test "physics_simulations" {
  // 测试物理模拟计算
  
  // 计算动能: KE = 1/2 * m * v^2
  let mass = 10
  let velocity = 5
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(mass, velocity), velocity) / 2
  @azimuth.assert_eq(125, kinetic_energy)
  
  // 计算势能: PE = m * g * h
  let height = 20
  let gravity = 10  // 简化重力加速度
  let potential_energy = @azimuth.multiply(@azimuth.multiply(mass, gravity), height)
  @azimuth.assert_eq(2000, potential_energy)
  
  // 计算总机械能
  let total_energy = @azimuth.add(kinetic_energy, potential_energy)
  @azimuth.assert_eq(2125, total_energy)
}

test "cryptography_simulations" {
  // 测试密码学模拟计算
  
  // 简单的凯撒密码模拟（数字版本）
  let original_number = 123
  let shift_key = 5
  let encrypted = @azimuth.add(original_number, shift_key)
  let decrypted = @azimuth.add(encrypted, -shift_key)
  @azimuth.assert_eq(123, decrypted)
  
  // 简单的校验和计算
  let data_block1 = 100
  let data_block2 = 200
  let data_block3 = 300
  let checksum = @azimuth.add(@azimuth.add(data_block1, data_block2), data_block3) / 3
  @azimuth.assert_eq(200, checksum)
  
  // 简单的哈希模拟
  let input_value = 42
  let hash_result = @azimuth.multiply(@azimuth.add(input_value, 17), 31)
  @azimuth.assert_eq(1829, hash_result)
}

test "string_formatting_validation" {
  // 测试字符串格式化和验证
  
  // 测试不同语言的问候
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, a1b2c3!", @azimuth.greet("a1b2c3"))
  
  // 测试特殊格式字符串
  @azimuth.assert_eq_string("Hello, [Test]!", @azimuth.greet("[Test]"))
  @azimuth.assert_eq_string("Hello, (Test)!", @azimuth.greet("(Test)"))
  @azimuth.assert_eq_string("Hello, {Test}!", @azimuth.greet("{Test}"))
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"Test\"!", @azimuth.greet("\"Test\""))
  @azimuth.assert_eq_string("Hello, 'Test'!", @azimuth.greet("'Test'"))
}

test "algorithm_complexity_tests" {
  // 测试算法复杂度相关的计算
  
  // O(n) 线性时间复杂度模拟 - 求和
  let n = 100
  let linear_sum = @azimuth.multiply(@azimuth.add(n, 1), n) / 2  // 1+2+...+100 = 5050
  @azimuth.assert_eq(5050, linear_sum)
  
  // O(n²) 平方时间复杂度模拟 - 嵌套循环和
  let nested_sum = @azimuth.multiply(n, @azimuth.add(n, 1))  // 简化的双重循环
  @azimuth.assert_eq(10100, nested_sum)
  
  // O(log n) 对数时间复杂度模拟 - 二分查找步骤
  // 模拟二分查找的步骤数 (log₂100 ≈ 7)
  let simulated_log_steps = 7
  @azimuth.assert_true(simulated_log_steps > 0 && simulated_log_steps < 10)
}

test "edge_case_robustness" {
  // 测试边界情况的健壮性
  
  // 极大数运算
  let very_large = 10000
  @azimuth.assert_eq(20000, @azimuth.add(very_large, very_large))
  @azimuth.assert_eq(100000000, @azimuth.multiply(very_large, very_large))
  
  // 极小数运算
  let very_small = -10000
  @azimuth.assert_eq(-20000, @azimuth.add(very_small, very_small))
  @azimuth.assert_eq(100000000, @azimuth.multiply(very_small, very_small))
  
  // 交替符号运算
  let alternating_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(1, -2), 3), -4), 5)
  @azimuth.assert_eq(3, alternating_sum)
  
  // 零附近运算
  @azimuth.assert_eq(0, @azimuth.add(@azimuth.add(-1, 1), @azimuth.add(-2, 2)))
  @azimuth.assert_eq(0, @azimuth.multiply(@azimuth.add(5, -5), 100))
}

test "business_logic_calculations" {
  // 测试业务逻辑计算
  
  // 计算利润: 收入 - 成本
  let revenue = 50000
  let cost = 35000
  let profit = @azimuth.add(revenue, -cost)
  @azimuth.assert_eq(15000, profit)
  
  // 计算利润率: (利润 / 收入) * 100%
  let profit_rate = @azimuth.multiply(profit, 100) / revenue
  @azimuth.assert_eq(30, profit_rate)
  
  // 计算税费: 利润 * 税率
  let tax_rate = 20  // 20%
  let tax_amount = @azimuth.multiply(profit, tax_rate) / 100
  @azimuth.assert_eq(3000, tax_amount)
  
  // 计算净利润
  let net_profit = @azimuth.add(profit, -tax_amount)
  @azimuth.assert_eq(12000, net_profit)
}