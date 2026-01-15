// Azimuth 项目额外的 MoonBit 测试用例
// 包含10个标准测试用例，覆盖不同功能和场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(24, @azimuth.multiply(6, 4))
  @azimuth.assert_eq(-18, @azimuth.multiply(-3, 6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 13
  let b = 27
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证具体数值
  @azimuth.assert_eq(40, @azimuth.add(a, b))
  @azimuth.assert_eq(351, @azimuth.multiply(a, b))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  // 计算 (a + b) * c - d / e (简化为整数运算)
  let a = 10
  let b = 15
  let c = 4
  let d = 20
  let e = 5
  
  let sum_ab = @azimuth.add(a, b)  // 25
  let product = @azimuth.multiply(sum_ab, c)  // 100
  let division = d / e  // 4
  let result = @azimuth.add(product, -division)  // 96
  
  @azimuth.assert_eq(96, result)
}

test "real_world_shopping_scenario" {
  // 测试实际购物场景
  let item_price = 199
  let quantity = 3
  let discount_threshold = 500
  let discount_amount = 50
  let shipping_fee = 20
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)  // 597
  
  // 判断是否满足折扣条件
  let final_total = if (subtotal > discount_threshold) {
    @azimuth.add(@azimuth.add(subtotal, -discount_amount), shipping_fee)
  } else {
    @azimuth.add(subtotal, shipping_fee)
  }
  
  @azimuth.assert_eq(567, final_total)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  // 测试单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试长字符串
  let long_name = "VeryLongNameWithManyCharacters"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // 测试特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // 测试空格
  @azimuth.assert_eq_string("Hello, Space Test!", @azimuth.greet("Space Test"))
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列前n项和：1 + 3 + 5 + ... + 19 (前10个奇数)
  let n = 10
  let first_term = 1
  let last_term = 19
  
  // 使用公式：n × (首项 + 末项) / 2
  let sum = @azimuth.multiply(n, @azimuth.add(first_term, last_term)) / 2
  @azimuth.assert_eq(100, sum)
  
  // 手动验证前几项
  let partial_sum = @azimuth.add(@azimuth.add(@azimuth.add(1, 3), 5), 7)
  @azimuth.assert_eq(16, partial_sum)
}

test "temperature_conversion_scenario" {
  // 测试温度转换场景
  // 摄氏度转华氏度（简化公式：F = C × 2 + 30）
  
  // 测试常见温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 22
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(74, room_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "game_score_calculation" {
  // 测试游戏得分计算
  let base_score = 1000
  let level_multiplier = 4
  let time_bonus = 500
  let penalty = 200
  
  // 计算等级得分
  let level_score = @azimuth.multiply(base_score, level_multiplier)  // 4000
  
  // 加上时间奖励
  let with_bonus = @azimuth.add(level_score, time_bonus)  // 4500
  
  // 减去惩罚分数
  let final_score = @azimuth.add(with_bonus, -penalty)  // 4300
  
  @azimuth.assert_eq(4300, final_score)
  
  // 计算排名等级（每1000分一级）
  let rank_level = final_score / 1000
  @azimuth.assert_eq(4, rank_level)
}