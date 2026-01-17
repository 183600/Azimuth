// 用户请求的 MoonBit 测试用例
// 包含不超过10个标准测试用例，使用 MoonBit 标准测试语法

// 测试基本加法运算
test "add_basic_functionality" {
  // 测试正数加法
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  
  // 测试包含零的加法
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
  
  // 测试负数加法
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
  @azimuth.assert_eq(1, @azimuth.add(-2, 3))
}

// 测试基本乘法运算
test "multiply_basic_functionality" {
  // 测试正数乘法
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  
  // 测试包含零的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  
  // 测试包含一的乘法
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  
  // 测试负数乘法
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

// 测试字符串处理功能
test "greet_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, MoonBit@2023!", @azimuth.greet("MoonBit@2023"))
}

// 测试边界值处理
test "boundary_value_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试最小值加法
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试最大值乘法
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值特殊乘法
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试数学运算律
test "mathematical_laws" {
  let a = 10
  let b = 15
  
  // 测试加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let c = 20
  @azimuth.assert_eq(
    @azimuth.add(@azimuth.add(a, b), c),
    @azimuth.add(a, @azimuth.add(b, c))
  )
}

// 测试实际应用场景：购物车计算
test "shopping_cart_calculation" {
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping)
  
  // 验证计算结果：199 * 3 + 15 = 612
  @azimuth.assert_eq(612, total)
}

// 测试实际应用场景：温度转换
test "temperature_conversion" {
  // 简化的摄氏度转华氏度公式：F = C × 2 + 30
  
  // 测试冰点温度（0°C）
  let freezing_c = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试室温（20°C）
  let room_c = 20
  let room_f = @azimuth.add(@azimuth.multiply(room_c, 2), 30)
  @azimuth.assert_eq(70, room_f)
  
  // 测试沸点温度（100°C）
  let boiling_c = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

// 测试复合计算场景
test "compound_calculation" {
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  // 步骤计算
  let sum_ab = @azimuth.add(a, b)  // 10 + 5 = 15
  let product = @azimuth.multiply(sum_ab, c)  // 15 * 3 = 45
  let result = @azimuth.add(product, -d)  // 45 - 7 = 38
  
  @azimuth.assert_eq(38, result)
}

// 测试断言函数
test "assertion_functions" {
  // 测试布尔值断言
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试整数断言
  @azimuth.assert_eq(42, 42)
  
  // 测试字符串断言
  @azimuth.assert_eq_string("test", "test")
}

// 测试错误处理场景
test "error_handling_scenarios" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -100) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试正常运算
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
}