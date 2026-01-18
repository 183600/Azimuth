// Azimuth 项目简单新增测试
// 使用标准 MoonBit 测试语法，提供额外的测试覆盖

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "string_greeting_variations" {
  // 字符串问候变体测试
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "mathematical_identities" {
  // 数学恒等式测试
  let a = 8
  let b = 12
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "boundary_value_tests" {
  // 边界值测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "practical_calculation_scenario" {
  // 实际计算场景测试
  // 计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(697, cart_total)  // 199*2 + 299*1 = 398 + 299 = 697
}

test "temperature_conversion_scenario" {
  // 温度转换场景测试
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
}

test "geometric_calculations" {
  // 几何计算测试
  let length = 10
  let width = 6
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(60, area)
  
  // 计算矩形周长
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(32, perimeter)
}

test "statistical_operations" {
  // 统计运算测试
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let num4 = 80
  let num5 = 100
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  @azimuth.assert_eq(450, sum)
  
  // 计算平均值
  let average = sum / 5
  @azimuth.assert_eq(90, average)
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "complex_expression_evaluation" {
  // 复杂表达式求值测试
  // 计算 (a + b) * c - d / e
  let a = 10
  let b = 5
  let c = 3
  let d = 12
  let e = 4
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum_ab, c)  // 45
  let division = d / e  // 3
  let result = @azimuth.add(product, -division)  // 42
  
  @azimuth.assert_eq(42, result)
}