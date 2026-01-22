// 用户新增的 MoonBit 测试用例
// 测试 azimuth 项目的核心功能

test "add_function_comprehensive" {
  // 测试 add 函数的综合情况
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
}

test "multiply_function_comprehensive" {
  // 测试 multiply 函数的综合情况
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-20, @azimuth.multiply(4, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46341, 46341))
}

test "greet_function_various_scenarios" {
  // 测试 greet 函数的各种场景
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户123!", @azimuth.greet("用户123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, Space Test!", @azimuth.greet("Space Test"))
}

test "divide_with_ceil_function" {
  // 测试 divide_with_ceil 函数
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "complex_arithmetic_operations" {
  // 测试复杂算术运算
  let a = 15
  let b = 4
  let c = 7
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(133, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(67, result2)
  
  // (a + b + c) / 2 向上取整
  let result3 = @azimuth.divide_with_ceil(@azimuth.add(@azimuth.add(a, b), c), 2)
  @azimuth.assert_eq(13, result3)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  @azimuth.assert_eq_string("Hello, " + long_string + "!", @azimuth.greet(long_string))
  
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // Unicode 字符
  @azimuth.assert_eq_string("Hello, 中文字符!", @azimuth.greet("中文字符"))
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 12
  let y = 8
  
  // 交换律
  @azimuth.assert_eq(@azimuth.add(x, y), @azimuth.add(y, x))
  @azimuth.assert_eq(@azimuth.multiply(x, y), @azimuth.multiply(y, x))
  
  // 结合律
  let z = 5
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(x, y), z), @azimuth.add(x, @azimuth.add(y, z)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(x, y), z), @azimuth.multiply(x, @azimuth.multiply(y, z)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, z)), @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(612, total)
  
  // 温度转换（简化版）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 页面布局计算
  let content_width = 800
  let margin = 20
  let total_width = @azimuth.add(@azimuth.multiply(margin, 2), content_width)
  @azimuth.assert_eq(840, total_width)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出保护
  @azimuth.assert_eq(@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -2, true) == min_val)
  
  // 断言函数测试
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}