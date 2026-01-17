// Azimuth 综合单元测试
// 使用标准 MoonBit 测试语法

test "add_function_zero_cases" {
  // 测试加法函数的零值处理
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(0, -3))
  @azimuth.assert_eq(-3, @azimuth.add(-3, 0))
}

test "add_function_positive_numbers" {
  // 测试正数加法
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(1, @azimuth.add(1, 0))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(-1, @azimuth.add(-1, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "add_function_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(2147483640, 10))
  
  // 负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(min_val, @azimuth.add(-2147483640, -10))
}

test "multiply_function_identity_elements" {
  // 测试乘法单位元
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(1, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "multiply_function_negative_one" {
  // 测试乘以-1的情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(5, @azimuth.multiply(-5, -1))
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))  // 特殊情况：最小值乘以-1
}

test "multiply_function_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))  // sqrt(max_val) * sqrt(max_val)
  
  // 负数溢出
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
}

test "greet_function_various_inputs" {
  // 测试greet函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 太空!", @azimuth.greet("太空"))
  @azimuth.assert_eq_string("Hello, space!", @azimuth.greet("space"))
}

test "mathematical_commutative_properties" {
  // 测试数学交换律
  let a = 15
  let b = -7
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 混合测试
  let c = 3
  @azimuth.assert_eq(@azimuth.add(@azimuth.multiply(a, c), @azimuth.multiply(b, c)), @azimuth.multiply(@azimuth.add(a, b), c))
}

test "real_world_calculation_scenarios" {
  // 实际计算场景测试
  // 场景1：计算购物车总价（商品价格 × 数量）
  let price1 = 25
  let quantity1 = 3
  let price2 = 15
  let quantity2 = 2
  
  let total1 = @azimuth.multiply(price1, quantity1)
  let total2 = @azimuth.multiply(price2, quantity2)
  let cart_total = @azimuth.add(total1, total2)
  @azimuth.assert_eq(105, cart_total)
  
  // 场景2：计算温度转换（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9), 160)  // 简化的 C×9/5+32，避免除法
  @azimuth.assert_eq(385, fahrenheit)
}