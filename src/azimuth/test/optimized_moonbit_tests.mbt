// 优化的 MoonBit 测试用例 - 专注于核心功能和边界情况测试

test "add_function_basic_functionality" {
  // 测试 add 函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
}

test "add_function_overflow_protection" {
  // 测试 add 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试边界值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_functionality" {
  // 测试 multiply 函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "multiply_function_overflow_protection" {
  // 测试 multiply 函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))  // sqrt(max_val)的平方会溢出
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试边界值相乘
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "greet_function_string_handling" {
  // 测试 greet 函数的字符串处理
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
}

test "assertion_functions_functionality" {
  // 测试断言函数的功能
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试复合条件
  let result = @azimuth.add(3, 4)
  @azimuth.assert_eq(result == 7, true)
  @azimuth.assert_eq(0, 1)(result != 7)
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1: 计算矩形面积和周长
  let length = 5
  let width = 3
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(15, area)
  @azimuth.assert_eq(16, perimeter)
  
  // 场景2: 简单的利息计算
  let principal = 1000
  let rate = 5
  let years = 2
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(11000, total)  // 1000 + 1000*5*2 = 11000
}

test "edge_case_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 组合1: 最大值与最小值的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 组合2: 零与边界值的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  
  // 组合3: 1与边界值的运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 1))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 7
  let b = 11
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
}

test "real_world_application_scenarios" {
  // 测试实际应用场景
  // 场景1: 购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping = 20
  
  let subtotal = @azimuth.add(@azimuth.multiply(item1_price, item1_quantity), @azimuth.multiply(item2_price, item2_quantity))
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(717, total)  // 199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  
  // 场景2: 温度转换（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)  // 简化公式: F = C * 2 + 30
  
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}