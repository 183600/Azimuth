// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法

// 导入 azimuth 模块
use azimuth

test "addition_with_positive_numbers" {
  // 测试正数相加
  @assertion.assert_eq(15, azimuth.add(7, 8))?
  @assertion.assert_eq(100, azimuth.add(50, 50))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
}

test "addition_with_negative_numbers" {
  // 测试负数相加
  @assertion.assert_eq(-10, azimuth.add(-3, -7))?
  @assertion.assert_eq(-100, azimuth.add(-50, -50))?
  @assertion.assert_eq(5, azimuth.add(-10, 15))?
}

test "multiplication_basic_cases" {
  // 测试基本乘法
  @assertion.assert_eq(12, azimuth.multiply(3, 4))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
  @assertion.assert_eq(-15, azimuth.multiply(3, -5))?
}

test "multiplication_with_large_numbers" {
  // 测试大数相乘
  @assertion.assert_eq(1000000, azimuth.multiply(1000, 1000))?
  @assertion.assert_eq(-1000000, azimuth.multiply(-1000, 1000))?
  @assertion.assert_eq(2147483647, azimuth.multiply(46341, 46341))?  // 溢出测试
}

test "greet_function_with_various_inputs" {
  // 测试 greet 函数的各种输入
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  @assertion.assert_eq_string("Hello, 张三!", azimuth.greet("张三"))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
}

test "mathematical_properties_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 18
  
  // 加法交换律
  @assertion.assert_eq(azimuth.add(a, b), azimuth.add(b, a))?
  
  // 乘法交换律
  @assertion.assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))?
}

test "mathematical_properties_associativity" {
  // 测试数学结合律
  let a = 5
  let b = 10
  let c = 15
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let result1 = azimuth.add(azimuth.add(a, b), c)
  let result2 = azimuth.add(a, azimuth.add(b, c))
  @assertion.assert_eq(result1, result2)?
}

test "boundary_value_tests" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @assertion.assert_eq(max_val, azimuth.add(max_val, 1))?
  @assertion.assert_eq(min_val, azimuth.add(min_val, -1))?
  @assertion.assert_eq(-1, azimuth.add(max_val, min_val))?
  
  // 边界值乘法
  @assertion.assert_eq(0, azimuth.multiply(max_val, 0))?
  @assertion.assert_eq(min_val, azimuth.multiply(min_val, -1))?
}

test "real_world_shopping_cart" {
  // 实际应用场景：购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 20
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = azimuth.multiply(item_price, quantity)
  
  // 计算税费
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  // 验证总价：(199×3) + (199×3×8/100) + 20 = 597 + 47 + 20 = 664
  @assertion.assert_eq(664, total)?
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算矩形的面积和周长
  let length = 12
  let width = 8
  
  // 计算面积：长 × 宽
  let area = azimuth.multiply(length, width)
  @assertion.assert_eq(96, area)?
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  @assertion.assert_eq(40, perimeter)?
  
  // 测试温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = azimuth.add(azimuth.multiply(celsius, 2), 30)
  @assertion.assert_eq(80, fahrenheit)?
}