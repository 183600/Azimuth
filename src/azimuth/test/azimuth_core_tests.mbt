// Azimuth 核心功能测试
// 使用标准 MoonBit 测试语法编写的核心测试用例

test "add_function_basic" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
}

test "add_function_overflow" {
  // 测试 add 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(1000000000, 1500000000))
  
  // 负数溢出测试
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(-1500000000, -1000000000))
  
  // 混合溢出测试
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
}

test "multiply_function_edge_cases" {
  // 测试 multiply 函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 乘以1和-1
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 溢出情况
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "greet_function_basic" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_function_special_chars" {
  // 测试 greet 函数处理特殊字符
  @azimuth.assert_eq_string("Hello, test@123!", @azimuth.greet("test@123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 12
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算周长
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(40, perimeter)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 7
  let y = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 3)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, 3)))
}

test "real_world_shopping_cart" {
  // 测试实际应用场景：购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  let shipping = 15
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping)
  
  // 验证总价：199*2 + 99*3 + 15 = 398 + 297 + 15 = 710
  @azimuth.assert_eq(710, total)
}

test "temperature_conversion" {
  // 测试实际应用场景：温度转换（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30（简化公式）
  
  // 测试常温
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}