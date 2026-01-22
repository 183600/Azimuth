// 标准 MoonBit 测试套件
// 包含8个高质量测试用例，覆盖 azimuth 库的核心功能

test "add_function_boundary_cases" {
  // 测试加法函数的边界情况
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(1, @azimuth.add(-2147483648, 2147483649))
}

test "multiply_function_zero_properties" {
  // 测试乘法函数的零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 2147483647))
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
  @azimuth.assert_eq_string("Hello, 'single'!", @azimuth.greet("'single'"))
  @azimuth.assert_eq_string("Hello, &amp;!", @azimuth.greet("&amp;"))
  @azimuth.assert_eq_string("Hello, emoji 😀!", @azimuth.greet("emoji 😀"))
}

test "divide_with_ceil_negative_dividend" {
  // 测试负数被除数的向上取整除法
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-3, 4))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 2))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -4))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(7, -4))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(3, -4))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(20, -4))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base_price = 100
  let tax_rate = 8
  let quantity = 3
  let discount = 15
  
  // 计算总价：(基础价格 * 数量) - 折扣
  let subtotal = @azimuth.multiply(base_price, quantity)
  let total = @azimuth.add(subtotal, -discount)
  
  // 计算需要的包装箱数量（每个箱子装25个）
  let boxes_needed = @azimuth.divide_with_ceil(quantity, 25)
  
  @azimuth.assert_eq(285, total)  // (100 * 3) - 15 = 285
  @azimuth.assert_eq(1, boxes_needed)  // ceil(3/25) = 1
}

test "mathematical_identities" {
  // 数学恒等式测试
  let x = 42
  let y = 17
  
  // 加法恒等式
  @azimuth.assert_eq(x, @azimuth.add(x, 0))
  @azimuth.assert_eq(y, @azimuth.add(0, y))
  
  // 乘法恒等式
  @azimuth.assert_eq(x, @azimuth.multiply(x, 1))
  @azimuth.assert_eq(y, @azimuth.multiply(1, y))
  
  // 分配律测试
  @azimuth.assert_eq(@azimuth.multiply(x, @azimuth.add(y, 1)), 
                     @azimuth.add(@azimuth.multiply(x, y), x))
}

test "string_and_number_combination" {
  // 字符串和数字组合测试
  let number_as_string = "123"
  let expected_result = "Hello, 123!"
  @azimuth.assert_eq_string(expected_result, @azimuth.greet(number_as_string))
  
  // 测试包含数字的字符串
  let mixed_string = "User123"
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet(mixed_string))
  
  // 测试纯数字字符串
  let pure_number = "456789"
  @azimuth.assert_eq_string("Hello, 456789!", @azimuth.greet(pure_number))
}