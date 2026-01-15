// Azimuth 项目标准 MoonBit 测试用例集
// 包含10个实用的测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a × b = b × a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -7
  let d = 13
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "practical_calculator_scenario" {
  // 测试实际计算器场景：计算购物车总价
  let apple_price = 5
  let apple_quantity = 8
  let banana_price = 3
  let banana_quantity = 12
  let orange_price = 7
  let orange_quantity = 6
  
  // 计算每种水果总价
  let apple_total = @azimuth.multiply(apple_price, apple_quantity)
  let banana_total = @azimuth.multiply(banana_price, banana_quantity)
  let orange_total = @azimuth.multiply(orange_price, orange_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(@azimuth.add(apple_total, banana_total), orange_total)
  
  // 验证结果: 5×8 + 3×12 + 7×6 = 40 + 36 + 42 = 118
  @azimuth.assert_eq(118, cart_total)
}

test "temperature_conversion_simplified" {
  // 测试简化的温度转换：摄氏度转华氏度
  // 使用简化公式：F = C × 2 + 30（避免小数运算）
  
  let celsius_0 = 0      // 冰点
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_0)
  
  let celsius_25 = 25    // 室温
  let fahrenheit_25 = @azimuth.add(@azimuth.multiply(celsius_25, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_25)
  
  let celsius_100 = 100  // 沸点
  let fahrenheit_100 = @azimuth.add(@azimuth.multiply(celsius_100, 2), 30)
  @azimuth.assert_eq(230, fahrenheit_100)
}

test "geometry_rectangle_calculations" {
  // 测试几何计算：矩形面积和周长
  let length = 15
  let width = 9
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(135, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(48, perimeter)
  
  // 计算对角线平方（避免平方根计算）
  let diagonal_squared = @azimuth.add(@azimuth.multiply(length, length), @azimuth.multiply(width, width))
  @azimuth.assert_eq(306, diagonal_squared)
}

test "statistical_mean_calculation" {
  // 测试统计计算：平均值
  // 数据集: 15, 25, 35, 45, 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let mean = sum / count
  @azimuth.assert_eq(35, mean)
  
  // 验证中位数
  let median = 35
  @azimuth.assert_eq(mean, median)  // 对于对称数据集，平均值等于中位数
}

test "complex_expression_evaluation" {
  // 测试复杂表达式求值：((a + b) × c) - d
  let a = 8
  let b = 12
  let c = 5
  let d = 20
  
  // 分步计算
  let sum_ab = @azimuth.add(a, b)           // 8 + 12 = 20
  let product = @azimuth.multiply(sum_ab, c)  // 20 × 5 = 100
  let result = @azimuth.add(product, -d)      // 100 - 20 = 80
  
  @azimuth.assert_eq(80, result)
  
  // 验证另一种计算顺序
  let ac = @azimuth.multiply(a, c)           // 8 × 5 = 40
  let bc = @azimuth.multiply(b, c)           // 12 × 5 = 60
  let alternative = @azimuth.add(@azimuth.add(ac, bc), -d)  // 40 + 60 - 20 = 80
  @azimuth.assert_eq(result, alternative)
}

test "assertion_functions_validation" {
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试断言函数与实际功能结合
  let add_result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, add_result)
  @azimuth.assert_true(add_result > 25)
  @azimuth.assert_false(add_result < 25)
  
  let greet_result = @azimuth.greet("Tester")
  @azimuth.assert_eq_string("Hello, Tester!", greet_result)
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
}