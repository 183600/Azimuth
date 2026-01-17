// 用户综合测试用例
// 使用标准 MoonBit 测试语法测试 azimuth 包的核心功能

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(0, 100))
  @azimuth.assert_eq(100, @azimuth.add(100, 0))
}

test "add_function_overflow_protection" {
  // 测试加法函数的溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123))
  @azimuth.assert_eq(123, @azimuth.multiply(123, 1))
  @azimuth.assert_eq(-123, @azimuth.multiply(123, -1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最小值与-1相乘的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  
  // 测试极值与0和1的运算
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, user@example.com!", @azimuth.greet("user@example.com"))
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  // 正常除法
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-12, 3))
  
  // 整除情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(16, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(17, result2)
  
  // (a - b) * (c + 1)
  let result3 = @azimuth.multiply(@azimuth.add(a, -b), @azimuth.add(c, 1))
  @azimuth.assert_eq(6, result3)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  
  // 购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 1
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(797, cart_total)  // 299*2 + 199*1 = 598 + 199 = 797
  
  // 温度转换（简化版）
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)  // 20*2 + 30 = 70
}

test "mathematical_properties_validation" {
  // 测试数学性质的验证
  let a = 7
  let b = 11
  let c = 3
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 结合律
  let assoc1 = @azimuth.add(@azimuth.add(a, b), c)
  let assoc2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(assoc1, assoc2)
}

test "assert_functions_themselves" {
  // 测试断言函数本身
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试断言函数与实际计算结合
  let calc_result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, calc_result)
  
  let greet_result = @azimuth.greet("Test")
  @azimuth.assert_eq_string("Hello, Test!", greet_result)
  
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_false(@azimuth.add(5, 3) == 9)
}