// Azimuth 综合测试套件
// 使用标准的 MoonBit 测试语法，包含8个测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法函数
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  let a = 15
  let b = 25
  let c = -10
  
  // 计算 (a * b) + c
  let product = @azimuth.multiply(a, b)
  let result1 = @azimuth.add(product, c)
  @azimuth.assert_eq(365, result1)
  
  // 计算 a * (b + c)
  let sum = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum)
  @azimuth.assert_eq(225, result2)
}

test "mathematical_properties" {
  // 验证数学性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(100, 200), @azimuth.add(200, 100))
  @azimuth.assert_eq(@azimuth.add(-50, 75), @azimuth.add(75, -50))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(12, 8), @azimuth.multiply(8, 12))
  @azimuth.assert_eq(@azimuth.multiply(-3, 7), @azimuth.multiply(7, -3))
}

test "boolean_evaluations" {
  // 布尔表达式测试
  @azimuth.assert_true(10 > 5)
  @azimuth.assert_true(-3 < 0)
  @azimuth.assert_true(42 == 42)
  @azimuth.assert_true(@azimuth.add(2, 3) == 5)
  
  @azimuth.assert_false(3 > 10)
  @azimuth.assert_false(-5 > 0)
  @azimuth.assert_false(42 == 24)
  @azimuth.assert_false(@azimuth.multiply(2, 3) == 7)
}

test "real_world_scenarios" {
  // 实际应用场景测试
  // 场景1：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 29
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(626, total)
  
  // 场景2：计算页面数（向上取整）
  let total_items = 100
  let items_per_page = 30
  let pages_needed = @azimuth.divide_with_ceil(total_items, items_per_page)
  @azimuth.assert_eq(4, pages_needed)
  
  // 场景3：用户欢迎消息
  let user_name = "李明"
  let welcome = @azimuth.greet(user_name)
  @azimuth.assert_eq_string("Hello, 李明!", welcome)
}