// 用户自定义 MoonBit 测试用例集
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

test "mathematical_properties" {
  // 测试数学性质
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

test "practical_shopping_scenario" {
  // 测试实际购物场景
  let item_price = 199
  let quantity = 3
  let discount = 50
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  // 计算折扣后价格
  let discounted = @azimuth.add(subtotal, -discount)
  // 计算最终总价
  let total = @azimuth.add(discounted, shipping)
  
  // 验证结果: (199×3) - 50 + 15 = 597 - 50 + 15 = 562
  @azimuth.assert_eq(562, total)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度
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

test "geometry_calculations" {
  // 测试几何计算
  let length = 12
  let width = 8
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算矩形周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算三角形面积（底×高÷2）
  let base = 10
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "statistical_calculations" {
  // 测试统计计算
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