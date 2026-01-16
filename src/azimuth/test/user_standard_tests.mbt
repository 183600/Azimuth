// 用户标准测试用例 - Azimuth 项目
// 包含10个标准 MoonBit 测试用例，覆盖主要功能和边界情况

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(-3, @azimuth.add(5, -8))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(6, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(123, 0))
}

test "greeting_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 17
  let b = 23
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数情况
  let c = -5
  let d = 12
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最小值边界测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 单字符测试
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串测试
  let long_name = "VeryLongNameWithManyCharacters"
  @azimuth.assert_eq_string("Hello, VeryLongNameWithManyCharacters!", @azimuth.greet(long_name))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  
  // Unicode字符测试
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景：计算 (a + b) * c - d
  let a = 10
  let b = 15
  let c = 3
  let d = 20
  
  let sum_ab = @azimuth.add(a, b)  // 25
  let product = @azimuth.multiply(sum_ab, c)  // 75
  let result = @azimuth.add(product, -d)  // 55
  
  @azimuth.assert_eq(55, result)
  
  // 场景：计算矩形面积和周长
  let length = 8
  let width = 6
  
  let area = @azimuth.multiply(length, width)  // 48
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))  // 28
  
  @azimuth.assert_eq(48, area)
  @azimuth.assert_eq(28, perimeter)
}

test "financial_calculation_scenario" {
  // 测试金融计算场景
  // 场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 15
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 398
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 299
  let subtotal = @azimuth.add(total1, total2)  // 697
  
  // 添加运费
  let final_total = @azimuth.add(subtotal, shipping)  // 712
  
  @azimuth.assert_eq(712, final_total)
}

test "temperature_conversion_calculation" {
  // 测试温度转换计算
  // 简化的摄氏度转华氏度：F = C * 2 + 30
  
  // 测试冰点
  let celsius_freezing = 0
  let fahrenheit_freezing = @azimuth.add(@azimuth.multiply(celsius_freezing, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_freezing)
  
  // 测试室温
  let celsius_room = 20
  let fahrenheit_room = @azimuth.add(@azimuth.multiply(celsius_room, 2), 30)
  @azimuth.assert_eq(70, fahrenheit_room)
  
  // 测试沸点
  let celsius_boiling = 100
  let fahrenheit_boiling = @azimuth.add(@azimuth.multiply(celsius_boiling, 2), 30)
  @azimuth.assert_eq(230, fahrenheit_boiling)
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试最小值特殊处理
  let min_val = -2147483648
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
}

test "statistical_calculations" {
  // 测试统计计算
  // 数据集：15, 25, 35, 45, 55
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(15, 25), 35), 45), 55)
  @azimuth.assert_eq(175, sum)
  
  // 计算平均值
  let count = 5
  let mean = sum / count
  @azimuth.assert_eq(35, mean)
  
  // 计算范围
  let min_value = 15
  let max_value = 55
  let range = @azimuth.add(max_value, -min_value)
  @azimuth.assert_eq(40, range)
  
  // 计算中位数（已排序数据集）
  let median = 35
  @azimuth.assert_eq(35, median)
}