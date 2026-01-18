// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值溢出保护
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试最大值和最小值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  // 测试最小值特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}

test "greet_functionality" {
  // 测试问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, user@123!", @azimuth.greet("user@123"))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let sum_ab = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum_ab, c)
  @azimuth.assert_eq(900, result)
  
  // 计算 a * b + c
  let product_ab = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product_ab, c)
  @azimuth.assert_eq(230, result2)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 场景：计算订单总价（商品价格 × 数量 + 运费）
  let price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(612, total)
  
  // 场景：温度转换（摄氏度转华氏度，简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}