test "mathematical_identity_properties" {
  // 测试数学恒等性质
  // 加法恒等元
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  
  // 乘法恒等元
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  
  // 乘法零元
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
}

test "divide_with_ceil_practical_cases" {
  // 测试向上取整除法的实际应用场景
  // 计算需要的容器数量
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))  // 11个物品，每个容器装3个
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))   // 9个物品，每个容器装3个
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 10))  // 1个物品，每个容器装10个
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  
  // 除数为0的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
}

test "string_greeting_multilingual" {
  // 测试多语言问候功能
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, Mundo!", @azimuth.greet("Mundo"))
  @azimuth.assert_eq_string("Hello, Monde!", @azimuth.greet("Monde"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 假设计算订单总价：商品价格 × 数量 + 运费
  let item_price = 25
  let quantity = 8
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(200, subtotal)
  @azimuth.assert_eq(215, total)
}

test "boundary_value_analysis" {
  // 测试边界值分析
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_int, @azimuth.add(max_int, 1))  // 溢出保护
  @azimuth.assert_eq(min_int, @azimuth.add(min_int, -1)) // 下溢保护
  
  // 边界值乘法
  @azimuth.assert_eq(max_int, @azimuth.multiply(46340, 46341))  // 接近sqrt(max_int)
  @azimuth.assert_eq(min_int, @azimuth.multiply(-46340, 46341))
}

test "number_sign_operations" {
  // 测试数字符号运算
  // 正数运算
  @azimuth.assert_eq(15, @azimuth.add(10, 5))
  @azimuth.assert_eq(50, @azimuth.multiply(10, 5))
  
  // 负数运算
  @azimuth.assert_eq(-15, @azimuth.add(-10, -5))
  @azimuth.assert_eq(50, @azimuth.multiply(-10, -5))
  
  // 混合符号运算
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-50, @azimuth.multiply(10, -5))
}

test "sequential_operations" {
  // 测试序列运算
  // 累加运算
  let mut sum = 0
  sum = @azimuth.add(sum, 10)
  sum = @azimuth.add(sum, 20)
  sum = @azimuth.add(sum, 30)
  @azimuth.assert_eq(60, sum)
  
  // 累乘运算
  let mut product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  @azimuth.assert_eq(24, product)
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 除数为0的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 空字符串问候
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
}

test "assertion_function_verification" {
  // 测试断言函数功能
  let test_value = 42
  let test_string = "MoonBit"
  
  // 测试各种断言
  @azimuth.assert_eq(test_value, test_value)
  @azimuth.assert_eq_string(test_string, test_string)
  
  // 使用 assert_eq 测试布尔条件
  @azimuth.assert_eq(1, if (test_value > 0) { 1 } else { 0 })
  @azimuth.assert_eq(1, if (test_string.length() > 0) { 1 } else { 0 })
  @azimuth.assert_eq(0, if (test_value < 0) { 1 } else { 0 })
  @azimuth.assert_eq(0, if (test_string.length() == 0) { 1 } else { 0 })
}