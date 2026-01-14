// 额外的 MoonBit 测试用例
// 测试 azimuth 包的各种功能

test "integer_overflow_protection" {
  // 测试整数溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试乘法溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 15
  let b = 7
  let c = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_name = "ThisIsAVeryLongNameThatTestsStringHandlingCapabilities"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 包含特殊字符
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // 包含Unicode字符
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "financial_calculations" {
  // 测试实际金融计算场景
  // 贷款计算：月供 = (本金 × 月利率 × (1 + 月利率)^月数) / ((1 + 月利率)^月数 - 1)
  // 简化版本：月供 = 本金 / 月数 + 利息
  let principal = 120000  // 本金
  let months = 360        // 30年 = 360个月
  let annual_rate = 5     // 年利率5%
  let monthly_rate = annual_rate / 12  // 月利率（简化）
  
  // 简化的月供计算
  let monthly_principal = principal / months
  let first_month_interest = principal * monthly_rate / 100
  let first_month_payment = @azimuth.add(monthly_principal, first_month_interest)
  
  @azimuth.assert_eq(233, first_month_payment)  // 120000/360 + 120000*5/1200 = 333 + 500 = 833, 取整数部分
  
  // 总还款额（简化）
  let total_payment = @azimuth.multiply(first_month_payment, months)
  @azimuth.assert_eq(83880, total_payment)
}

test "physics_simulation" {
  // 测试物理模拟计算
  // 自由落体运动：h = h0 - 0.5 * g * t^2
  // 简化版本：h = h0 - g * t^2（避免小数）
  let initial_height = 1000  // 初始高度
  let gravity = 10           // 重力加速度（简化）
  let time = 5               // 时间
  
  let time_squared = @azimuth.multiply(time, time)  // t^2 = 25
  let distance_fallen = @azimuth.multiply(gravity, time_squared)  // g * t^2 = 250
  let current_height = @azimuth.add(initial_height, -distance_fallen)  // h0 - g * t^2 = 750
  
  @azimuth.assert_eq(750, current_height)
  
  // 速度计算：v = g * t
  let velocity = @azimuth.multiply(gravity, time)
  @azimuth.assert_eq(50, velocity)
  
  // 动能计算：E = 0.5 * m * v^2
  // 简化版本：E = m * v^2
  let mass = 10
  let kinetic_energy = @azimuth.multiply(mass, @azimuth.multiply(velocity, velocity))
  @azimuth.assert_eq(25000, kinetic_energy)
}

test "data_structure_operations" {
  // 测试数据结构操作（模拟）
  // 栈操作：push, pop
  let mut stack_size = 0
  
  // push操作
  stack_size = @azimuth.add(stack_size, 1)  // push value1
  @azimuth.assert_eq(1, stack_size)
  
  stack_size = @azimuth.add(stack_size, 1)  // push value2
  @azimuth.assert_eq(2, stack_size)
  
  stack_size = @azimuth.add(stack_size, 1)  // push value3
  @azimuth.assert_eq(3, stack_size)
  
  // pop操作
  stack_size = @azimuth.add(stack_size, -1)  // pop value3
  @azimuth.assert_eq(2, stack_size)
  
  stack_size = @azimuth.add(stack_size, -1)  // pop value2
  @azimuth.assert_eq(1, stack_size)
  
  stack_size = @azimuth.add(stack_size, -1)  // pop value1
  @azimuth.assert_eq(0, stack_size)
}

test "statistical_analysis" {
  // 测试统计分析函数
  // 数据集：15, 25, 35, 45, 55, 65, 75, 85, 95
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55), 65), 75), 85), 95)
  @azimuth.assert_eq(495, sum)
  
  // 计算平均值
  let count = 9
  let mean = sum / count
  @azimuth.assert_eq(55, mean)
  
  // 计算中位数（奇数个数据点）
  let median = 55
  @azimuth.assert_eq(55, median)
  
  // 计算范围
  let min_val = 15
  let max_val = 95
  let range = @azimuth.add(max_val, -min_val)
  @azimuth.assert_eq(80, range)
}

test "game_mechanics" {
  // 测试游戏机制模拟
  // RPG游戏伤害计算
  let base_damage = 100
  let weapon_damage = 50
  let strength = 20
  let enemy_defense = 30
  
  // 基础伤害计算
  let total_damage = @azimuth.add(@azimuth.add(base_damage, weapon_damage), strength)
  @azimuth.assert_eq(170, total_damage)
  
  // 考虑防御的实际伤害
  let actual_damage = @azimuth.add(total_damage, -enemy_defense)
  @azimuth.assert_eq(140, actual_damage)
  
  // 经验值计算
  let base_exp = 200
  let level_multiplier = 3
  let exp_gained = @azimuth.multiply(base_exp, level_multiplier)
  @azimuth.assert_eq(600, exp_gained)
  
  // 升级所需经验
  let current_level = 5
  let exp_to_next_level = @azimuth.multiply(current_level, 1000)
  @azimuth.assert_eq(5000, exp_to_next_level)
}

test "encryption_simulation" {
  // 测试加密模拟（简化版）
  // Caesar密码
  let plaintext = "HELLO"
  let shift = 3
  
  // 简化的加密过程（仅模拟，不实际处理字符）
  let text_length = 5
  let encrypted_length = text_length
  @azimuth.assert_eq(5, encrypted_length)
  @azimuth.assert_eq_string("HELLO", plaintext)  // 使用plaintext避免未使用警告
  
  // 简化的哈希计算
  let hash_base = 31
  let hash_value = @azimuth.multiply(text_length, hash_base)
  @azimuth.assert_eq(155, hash_value)
  
  // 简化的校验和计算
  let checksum = @azimuth.add(hash_value, shift)
  @azimuth.assert_eq(158, checksum)
  
  // 解密验证（简化）
  let decrypted_checksum = @azimuth.add(checksum, -shift)
  @azimuth.assert_eq(155, decrypted_checksum)
  @azimuth.assert_eq(hash_value, decrypted_checksum)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30（避免小数）
  
  // 测试常见温度点
  let freezing_celsius = 0    // 冰点
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 20       // 室温
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(70, room_fahrenheit)
  
  let boiling_celsius = 100   // 沸点
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}