// 核心 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

// 导入被测试的模块
// use line removed

// 测试 add 函数的基本功能
test "test_basic_addition" {
  assert_eq!(5, azimuth::@azimuth.add(2, 3))
  assert_eq!(0, azimuth::@azimuth.add(0, 0))
  assert_eq!(-5, azimuth::@azimuth.add(-2, -3))
}

// 测试 multiply 函数的基本功能
test "test_basic_multiplication" {
  assert_eq!(6, azimuth::@azimuth.multiply(2, 3))
  assert_eq!(0, azimuth::@azimuth.multiply(5, 0))
  assert_eq!(-6, azimuth::@azimuth.multiply(2, -3))
  assert_eq!(6, azimuth::@azimuth.multiply(-2, -3))
}

// 测试 greet 函数
test "test_greet_function" {
  assert_eq!("Hello, World!", azimuth::@azimuth.greet("World"))
  assert_eq!("Hello, !", azimuth::@azimuth.greet(""))
  assert_eq!("Hello, 测试!", azimuth::@azimuth.greet("测试"))
}

// 测试 add 函数的边界情况
test "test_add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  assert_eq!(max_val, azimuth::@azimuth.add(max_val, 1))
  assert_eq!(min_val, azimuth::@azimuth.add(min_val, -1))
  assert_eq!(-1, azimuth::@azimuth.add(max_val, min_val))
}

// 测试 multiply 函数的边界情况
test "test_multiply_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出处理
  assert_eq!(max_val, azimuth::@azimuth.multiply(46341, 46341))
  assert_eq!(min_val, azimuth::@azimuth.multiply(-46341, 46341))
  assert_eq!(min_val, azimuth::@azimuth.multiply(min_val, -1))
}

// 测试复杂数学运算场景
test "test_complex_mathematical_operations" {
  // 测试 (a + b) * c 的计算
  let a = 5
  let b = 3
  let c = 4
  let sum = azimuth::@azimuth.add(a, b)
  let result = azimuth::@azimuth.multiply(sum, c)
  assert_eq!(32, result)
  
  // 测试 a * b + c * d 的计算
  let a2 = 10
  let b2 = 20
  let c2 = 30
  let d2 = 40
  let product1 = azimuth::@azimuth.multiply(a2, b2)
  let product2 = azimuth::@azimuth.multiply(c2, d2)
  let sum_result = azimuth::@azimuth.add(product1, product2)
  assert_eq!(1400, sum_result)
}

// 测试实际应用场景：购物车计算
test "test_shopping_cart_scenario" {
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  
  // 计算每种商品的总价
  let total1 = azimuth::@azimuth.multiply(item1_price, item1_quantity)
  let total2 = azimuth::@azimuth.multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = azimuth::@azimuth.add(total1, total2)
  
  // 验证总价：299*2 + 199*3 = 598 + 597 = 1195
  assert_eq!(1195, cart_total)
}

// 测试实际应用场景：温度转换（简化版）
test "test_temperature_conversion" {
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  let celsius_temp = 25
  let fahrenheit_temp = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius_temp, 2), 30)
  assert_eq!(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(freezing_celsius, 2), 30)
  assert_eq!(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
}

// 测试数学性质：交换律
test "test_mathematical_commutative_properties" {
  let a = 12
  let b = 15
  
  // 加法交换律
  assert_eq!(azimuth::@azimuth.add(a, b), azimuth::@azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq!(azimuth::@azimuth.multiply(a, b), azimuth::@azimuth.multiply(b, a))
  
  // 测试包含负数的情况
  let c = -20
  let d = 30
  
  assert_eq!(azimuth::@azimuth.multiply(c, d), azimuth::@azimuth.multiply(d, c))
}

// 测试数学性质：结合律
test "test_mathematical_associative_properties" {
  let a = 10
  let b = 20
  let c = 30
  
  // 加法结合律
  assert_eq!(azimuth::@azimuth.add(azimuth::@azimuth.add(a, b), c), azimuth::@azimuth.add(a, azimuth::@azimuth.add(b, c)))
  
  // 乘法结合律
  assert_eq!(azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(a, b), c), azimuth::@azimuth.multiply(a, azimuth::@azimuth.multiply(b, c)))
}