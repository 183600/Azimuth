// Azimuth 项目综合 MoonBit 测试用例
// 包含10个标准测试用例，覆盖各种功能场景

// 测试1: 基本算术运算验证
test "basic_arithmetic_operations" {
  // 测试加法基本功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  
  // 测试乘法基本功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
  @azimuth.assert_eq(36, @azimuth.multiply(-6, -6))
}

// 测试2: 边界值处理
test "boundary_value_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值边界
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

// 测试3: 字符串处理功能
test "string_processing_functionality" {
  // 测试基本字符串拼接
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试特殊字符处理
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  
  // 测试长字符串处理
  let long_name = "VeryLongNameWithManyCharacters"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
}

// 测试4: 数学恒等式验证
test "mathematical_identities" {
  let a = 42
  let b = 17
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  
  // 交换律验证
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

// 测试5: 实际应用场景 - 购物车计算
test "shopping_cart_calculation" {
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(697, final_total)
}

// 测试6: 实际应用场景 - 温度转换
test "temperature_conversion" {
  // 简化的摄氏度转华氏度公式：F = C × 2 + 30
  
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

// 测试7: 复合计算场景
test "compound_calculation_scenarios" {
  // 计算 (a + b) × c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(30, result)
  
  // 计算矩形的面积和周长
  let length = 8
  let width = 6
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(48, area)
  @azimuth.assert_eq(28, perimeter)
}

// 测试8: 负数运算验证
test "negative_number_operations" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-20, @azimuth.add(-25, 5))
  
  // 测试负数乘法
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -100))
  
  // 测试混合运算
  @azimuth.assert_eq(-5, @azimuth.add(@azimuth.multiply(2, -3), 1))
}

// 测试9: 断言函数验证
test "assertion_functions_validation" {
  // 测试断言函数本身
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(100, 100)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试条件断言
  let condition = @azimuth.add(5, 3) > 7
  @azimuth.assert_eq(condition, true)
  
  let condition2 = @azimuth.multiply(4, 5) < 15
  @azimuth.assert_eq(0, 1)(condition2)
}

// 测试10: 性能和大数据运算
test "performance_large_data_operations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  let large_sum = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(3000000, large_sum)
  
  let large_product = @azimuth.multiply(1000, 2000)
  @azimuth.assert_eq(2000000, large_product)
  
  // 测试连续运算
  let chain_result = @azimuth.add(@azimuth.multiply(100, 2), @azimuth.multiply(50, 3))
  @azimuth.assert_eq(350, chain_result)
  
  // 测试累加计算
  let accumulator = 0
  let step1 = @azimuth.add(accumulator, 10)
  let step2 = @azimuth.add(step1, 20)
  let step3 = @azimuth.add(step2, 30)
  @azimuth.assert_eq(60, step3)
}