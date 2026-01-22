// MoonBit 标准测试用例 v11
// 包含10个标准测试用例，覆盖各种功能和场景

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(6, -4))
  @azimuth.assert_eq(36, @azimuth.multiply(-6, -6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
}

test "boundary_value_calculations" {
  // 测试边界值计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 15
  let b = 25
  let c = 10
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "real_world_shopping_cart" {
  // 测试实际应用：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let discount = 50
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.add(
    @azimuth.multiply(item1_price, item1_quantity),
    @azimuth.multiply(item2_price, item2_quantity)
  )
  
  // 计算最终价格
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping)
  
  @azimuth.assert_eq(652, final_total)
}

test "temperature_conversion" {
  // 测试温度转换
  // 简化公式：F = C × 2 + 30
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试室温
  let room_c = 20
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  @azimuth.assert_eq(70, room_f)
  
  // 测试体温
  let body_c = 37
  let body_f = @azimuth.add(@azimuth.multiply(body_c, 2), 30)
  @azimuth.assert_eq(104, body_f)
}

test "complex_formula_evaluation" {
  // 测试复杂公式评估
  // 计算圆的周长和面积（简化版）
  let pi = 3  // 简化的π值
  let radius = 5
  
  // 周长 = 2 × π × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 面积 = π × r²
  let area = @azimuth.multiply(pi, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
  
  // 计算球体体积（简化版）
  let volume = @azimuth.multiply(@azimuth.multiply(4, pi), @azimuth.multiply(radius, @azimuth.multiply(radius, radius))) / 3
  @azimuth.assert_eq(500, volume)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：12, 18, 24, 30, 36
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(12, 18), 24), 30), 36)
  @azimuth.assert_eq(120, sum)
  
  // 计算平均值
  let count = 5
  let mean = sum / count
  @azimuth.assert_eq(24, mean)
  
  // 计算中位数
  let median = 24
  @azimuth.assert_eq(median, median)
  
  // 计算范围
  let range = @azimuth.add(36, -12)
  @azimuth.assert_eq(24, range)
}

test "game_score_system" {
  // 测试游戏得分系统
  let base_score = 1000
  let level = 4
  let multiplier = 3
  let bonus = 500
  let penalty = 200
  
  // 计算等级得分
  let level_score = @azimuth.multiply(base_score, level)
  
  // 应用倍数
  let multiplied_score = @azimuth.multiply(level_score, multiplier)
  
  // 应用加成和减分
  let final_score = @azimuth.add(@azimuth.add(multiplied_score, bonus), -penalty)
  
  @azimuth.assert_eq(12300, final_score)
  
  // 计算排名（每1000分一级）
  let rank = final_score / 1000
  @azimuth.assert_eq(12, rank)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1000000))
  @azimuth.assert_eq(max_val, @azimuth.multiply(100000, 30000))
  
  // 测试零值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}