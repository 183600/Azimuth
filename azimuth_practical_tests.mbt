// Azimuth 项目实用测试用例集
// 使用标准 MoonBit 测试语法，涵盖实际应用场景

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
  
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "greeting_functionality" {
  // 问候功能测试
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "calculation_chains" {
  // 计算链测试
  let step1 = @azimuth.add(10, 5)
  let step2 = @azimuth.multiply(step1, 2)
  let step3 = @azimuth.add(step2, 3)
  @azimuth.assert_eq(33, step3)
  
  // 另一个计算链
  let chain = @azimuth.multiply(@azimuth.add(4, 6), @azimuth.add(2, 3))
  @azimuth.assert_eq(50, chain)
}

test "edge_cases" {
  // 边界情况测试
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(1, @azimuth.multiply(1, -999))
}

test "real_world_scenarios" {
  // 实际应用场景测试
  
  // 购物车计算
  let item_price = 25
  let quantity = 4
  let tax = 8
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(108, total)
  
  // 平均分计算
  let score1 = 85
  let score2 = 90
  let score3 = 95
  let sum = @azimuth.add(@azimuth.add(score1, score2), score3)
  let average = sum / 3
  @azimuth.assert_eq(90, average)
}

test "pattern_calculations" {
  // 模式计算测试
  
  // 平方数计算
  let num = 7
  let square = @azimuth.multiply(num, num)
  @azimuth.assert_eq(49, square)
  
  // 立方数计算
  let cube = @azimuth.multiply(@azimuth.multiply(num, num), num)
  @azimuth.assert_eq(343, cube)
  
  // 阶乘简化计算 (5!)
  let factorial_5 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(1, 2), 3), 4), 5)
  @azimuth.assert_eq(120, factorial_5)
}

test "unit_conversions" {
  // 单位转换测试
  
  // 英寸转厘米 (1英寸 = 2.54厘米，简化为整数运算)
  let inches = 10
  let centimeters = @azimuth.multiply(inches, 254) / 100
  @azimuth.assert_eq(25, centimeters)
  
  // 磅转千克 (1磅 = 0.453千克，简化为整数运算)
  let pounds = 100
  let kilograms = @azimuth.multiply(pounds, 453) / 1000
  @azimuth.assert_eq(45, kilograms)
}

test "algorithm_simulation" {
  // 算法模拟测试
  
  // 简单的累加算法
  let accumulator = 0
  accumulator = @azimuth.add(accumulator, 1)
  accumulator = @azimuth.add(accumulator, 2)
  accumulator = @azimuth.add(accumulator, 3)
  accumulator = @azimuth.add(accumulator, 4)
  accumulator = @azimuth.add(accumulator, 5)
  @azimuth.assert_eq(15, accumulator)
  
  // 简单的累乘算法
  let product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  @azimuth.assert_eq(24, product)
}

test "data_processing" {
  // 数据处理测试
  
  // 数据验证：检查数字范围
  let value = 75
  let min_val = 0
  let max_val = 100
  let in_range = @azimuth.add(@azimuth.add(value, -min_val), -max_val) == 0
  @azimuth.assert_eq(true, in_range)
  
  // 简单的数据统计
  let data_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let data_count = 5
  let data_average = data_sum / data_count
  @azimuth.assert_eq(30, data_average)
}