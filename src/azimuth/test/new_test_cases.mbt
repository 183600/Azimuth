// 新增 MoonBit 测试用例
// 这些测试用例覆盖 azimuth 模块的核心功能，使用标准的 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试基本的加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_boundary_cases" {
  // 测试加法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试与0的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
}

test "multiply_function_basic_cases" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

test "multiply_function_boundary_cases" {
  // 测试乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试特殊值
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "complex_mathematical_operations" {
  // 测试复杂的数学运算组合
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(900, result1)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(230, result2)
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  // 计算矩形周长：2 * (长 + 宽)
  let length = 15
  let width = 10
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(50, perimeter)
  
  // 计算折扣价格：原价 - 折扣金额
  let original_price = 200
  let discount_rate = 20  // 20%
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(160, discounted_price)
}

test "mathematical_properties_verification" {
  // 测试数学性质
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  let c = 5
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", @azimuth.greet(special_chars))
  
  // 测试包含空格的字符串
  let spaced_string = "Hello World"
  @azimuth.assert_eq_string("Hello, Hello World!", @azimuth.greet(spaced_string))
  
  // 测试包含数字的字符串
  let numeric_string = "User2023"
  @azimuth.assert_eq_string("Hello, User2023!", @azimuth.greet(numeric_string))
}

test "error_handling_and_validation" {
  // 测试错误处理和验证
  // 测试断言函数
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试边界条件的正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_eq(@azimuth.add(max_val, 1, true) == max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1, true) == min_val)
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2, true) == max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, -1, true) == min_val)
}