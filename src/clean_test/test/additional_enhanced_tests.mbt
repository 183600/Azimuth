test "advanced_string_scenarios" {
  // 测试包含特殊字符的字符串
  @clean_test.assert_eq_string("Hello, World@2023!", @clean_test.greet("World@2023"))
  @clean_test.assert_eq_string("Hello, 中文测试!", @clean_test.greet("中文测试"))
  @clean_test.assert_eq_string("Hello,   !", @clean_test.greet("   ")) // 空格字符串
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet("")) // 空字符串
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameThatMightBeUsedInRealWorldApplications"
  @clean_test.assert_eq_string("Hello, " + long_name + "!", @clean_test.greet(long_name))
}

// 测试加法和乘法的组合运算
///|
test "complex_arithmetic_operations" {
  // 测试 (a + b) * c 的场景
  let a = 100
  let b = 200
  let c = 3
  
  let sum_ab = @clean_test.add(a, b)
  let result = @clean_test.multiply(sum_ab, c)
  @clean_test.assert_eq(900, result) // (100 + 200) * 3 = 900
  
  // 测试 a * b + c 的场景
  let x = 50
  let y = 4
  let z = 100
  
  let product_xy = @clean_test.multiply(x, y)
  let result2 = @clean_test.add(product_xy, z)
  @clean_test.assert_eq(300, result2) // 50 * 4 + 100 = 300
  
  // 测试负数的组合运算
  let m = -10
  let n = 5
  let p = -2
  
  let sum_mn = @clean_test.add(m, n)
  let result3 = @clean_test.multiply(sum_mn, p)
  @clean_test.assert_eq(10, result3) // (-10 + 5) * -2 = 10
}

// 测试金融计算场景
///|
test "financial_calculation_scenarios" {
  // 计算投资回报：本金 * (1 + 利率) * 时间
  let principal = 10000
  let rate = 5 // 5%
  let years = 3
  
  let rate_plus_one = @clean_test.add(1, rate)
  let yearly_return = @clean_test.multiply(principal, rate_plus_one)
  let total_return = @clean_test.multiply(yearly_return, years)
  
  // 注意：这是简化的计算，实际金融计算会更复杂
  @clean_test.assert_eq(180000, total_return) // 10000 * 6 * 3 = 180000
  
  // 计算折扣价格：原价 * (1 - 折扣率)
  let original_price = 500
  let discount_rate = 20 // 20%折扣
  let discount_multiplier = @clean_test.add(1, -discount_rate)
  let discounted_price = @clean_test.multiply(original_price, discount_multiplier)
  
  @clean_test.assert_eq(-9500, discounted_price) // 500 * -19 = -9500 (负数表示折扣)
}

// 测试温度转换场景
///|
test "temperature_conversion_scenarios" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 100
  let multiply_factor = 9
  let add_offset = 32
  
  let multiplied = @clean_test.multiply(celsius, multiply_factor)
  let divided = @clean_test.multiply(multiplied, 1) // 简化：乘以1/5，这里简化为乘以1
  let fahrenheit = @clean_test.add(divided, add_offset)
  
  // 注意：这是简化的计算，实际需要除法
  @clean_test.assert_eq(932, fahrenheit) // 100 * 9 + 32 = 932 (简化计算)
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 212
  let subtract_offset = 32
  
  let subtracted = @clean_test.add(fahrenheit2, -subtract_offset)
  let celsius2 = @clean_test.multiply(subtracted, 1) // 简化：乘以5/9，这里简化为乘以1
  
  @clean_test.assert_eq(180, celsius2) // (212 - 32) * 1 = 180 (简化计算)
}

// 测试物理计算场景
///|
test "physics_calculation_scenarios" {
  // 计算动能：E = 1/2 * m * v^2
  let mass = 10
  let velocity = 5
  
  let velocity_squared = @clean_test.multiply(velocity, velocity)
  let energy = @clean_test.multiply(mass, velocity_squared)
  let half_energy = @clean_test.multiply(energy, 0) // 简化：乘以1/2，这里简化为乘以0
  
  // 注意：这是简化的计算，实际需要除法
  @clean_test.assert_eq(0, half_energy) // 10 * 25 * 0 = 0 (简化计算)
  
  // 计算距离：s = v * t
  let velocity2 = 60
  let time = 2
  let distance = @clean_test.multiply(velocity2, time)
  
  @clean_test.assert_eq(120, distance) // 60 * 2 = 120
}

// 测试数组索引计算场景
///|
test "array_index_calculation_scenarios" {
  // 计算二维数组的线性索引：index = row * width + column
  let row = 3
  let column = 4
  let width = 10
  
  let row_offset = @clean_test.multiply(row, width)
  let linear_index = @clean_test.add(row_offset, column)
  
  @clean_test.assert_eq(34, linear_index) // 3 * 10 + 4 = 34
  
  // 计算三维数组的线性索引：index = (depth * height + row) * width + column
  let depth = 2
  let height = 5
  let row2 = 3
  let column2 = 4
  let width2 = 10
  
  let depth_row_offset = @clean_test.add(@clean_test.multiply(depth, height), row2)
  let linear_index2 = @clean_test.multiply(depth_row_offset, width2)
  let final_index = @clean_test.add(linear_index2, column2)
  
  // 注意：这是简化的计算，实际需要更复杂的运算
  @clean_test.assert_eq(134, final_index) // ((2 * 5 + 3) * 10) + 4 = 134 (简化计算)
}

// 测试时间计算场景
///|
test "time_calculation_scenarios" {
  // 计算总秒数：hours * 3600 + minutes * 60 + seconds
  let hours = 2
  let minutes = 30
  let seconds = 45
  
  let hours_to_seconds = @clean_test.multiply(hours, 3600)
  let minutes_to_seconds = @clean_test.multiply(minutes, 60)
  let total_seconds = @clean_test.add(@clean_test.add(hours_to_seconds, minutes_to_seconds), seconds)
  
  @clean_test.assert_eq(9045, total_seconds) // 2 * 3600 + 30 * 60 + 45 = 9045
  
  // 计算天数差异：days1 - days2
  let days1 = 365
  let days2 = 100
  let days_difference = @clean_test.add(days1, -days2)
  
  @clean_test.assert_eq(265, days_difference) // 365 - 100 = 265
}

// 测试游戏计算场景
///|
test "game_calculation_scenarios" {
  // 计算经验值：base_exp * level_multiplier + bonus_exp
  let base_exp = 100
  let level = 5
  let level_multiplier = 2
  let bonus_exp = 50
  
  let level_bonus = @clean_test.multiply(level, level_multiplier)
  let total_exp = @clean_test.add(@clean_test.multiply(base_exp, level_bonus), bonus_exp)
  
  @clean_test.assert_eq(1050, total_exp) // 100 * (5 * 2) + 50 = 1050
  
  // 计算伤害输出：base_damage * attack_multiplier - defense_reduction
  let base_damage = 200
  let attack_multiplier = 3
  let defense_reduction = 100
  
  let attack_damage = @clean_test.multiply(base_damage, attack_multiplier)
  let final_damage = @clean_test.add(attack_damage, -defense_reduction)
  
  @clean_test.assert_eq(500, final_damage) // 200 * 3 - 100 = 500
}

// 测试边界值的组合运算
///|
test "boundary_value_combinations" {
  // 测试接近最大值的运算
  let near_max = 2147483640
  let small_increment = 5
  let multiplier = 2
  
  let sum_result = @clean_test.add(near_max, small_increment)
  let multiply_result = @clean_test.multiply(near_max, multiplier)
  
  @clean_test.assert_eq(2147483647, sum_result) // 应该溢出
  @clean_test.assert_eq(2147483647, multiply_result) // 应该溢出
  
  // 测试接近最小值的运算
  let near_min = -2147483640
  let small_decrement = -5
  let negative_multiplier = -2
  
  let sum_result2 = @clean_test.add(near_min, small_decrement)
  let multiply_result2 = @clean_test.multiply(near_min, negative_multiplier)
  
  @clean_test.assert_eq(-2147483648, sum_result2) // 应该溢出
  @clean_test.assert_eq(-2147483648, multiply_result2) // 应该溢出
}

// 测试字符串拼接的边界情况
///|
test "string_boundary_cases" {
  // 测试单字符字符串
  @clean_test.assert_eq_string("Hello, A!", @clean_test.greet("A"))
  @clean_test.assert_eq_string("Hello, 1!", @clean_test.greet("1"))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet("!"))
  
  // 测试包含空格的字符串
  @clean_test.assert_eq_string("Hello, Hello World!", @clean_test.greet("Hello World"))
  @clean_test.assert_eq_string("Hello, Leading Space!", @clean_test.greet(" Leading Space"))
  @clean_test.assert_eq_string("Hello, Trailing Space !", @clean_test.greet("Trailing Space "))
  
  // 测试特殊字符
  @clean_test.assert_eq_string("Hello, \n\t!", @clean_test.greet("\n\t"))
  @clean_test.assert_eq_string("Hello, \"Quotes\"!", @clean_test.greet("\"Quotes\""))
}
