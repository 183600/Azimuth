// 新增的 MoonBit 测试用例
// 专注于测试项目的核心功能：add, multiply, greet 和辅助函数

test "add_function_negative_numbers" {
  // 测试负数相加
  let result1 = @azimuth.add(-10, -20)
  @azimuth.assert_eq(result1, -30)
  
  let result2 = @azimuth.add(-15, 25)
  @azimuth.assert_eq(result2, 10)
  
  let result3 = @azimuth.add(30, -45)
  @azimuth.assert_eq(result3, -15)
}

test "multiply_function_negative_numbers" {
  // 测试负数相乘
  let result1 = @azimuth.multiply(-5, -6)
  @azimuth.assert_eq(result1, 30)
  
  let result2 = @azimuth.multiply(-4, 7)
  @azimuth.assert_eq(result2, -28)
  
  let result3 = @azimuth.multiply(9, -3)
  @azimuth.assert_eq(result3, -27)
}

test "greet_function_with_various_inputs" {
  // 测试 greet 函数的各种输入
  @azimuth.assert_eq_string(@azimuth.greet("Alice"), "Hello, Alice!")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
  @azimuth.assert_eq_string(@azimuth.greet("!@#$"), "Hello, !@#$!")
  @azimuth.assert_eq_string(@azimuth.greet("   "), "Hello,    !")
}

test "divide_with_ceil_function" {
  // 测试向上取整除法函数
  @azimuth.assert_eq(@azimuth.divide_with_ceil(10, 3), 4)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(9, 3), 3)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(1, 2), 1)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(0, 5), 0)
  @azimuth.assert_eq(@azimuth.divide_with_ceil(-7, 3), -2)
}

test "combined_operations_complex" {
  // 测试复合运算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(result1, 16)
  
  // a * b + c
  let result2 = @azimuth.add(@azimuth.multiply(a, b), c)
  @azimuth.assert_eq(result2, 17)
  
  // a * (b + c)
  let result3 = @azimuth.multiply(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result3, 25)
}

test "assert_functions_verification" {
  // 测试断言函数本身
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_eq(10, 5 + 5)
  @azimuth.assert_eq_string("test", "test")
}

test "extreme_values_handling" {
  // 测试极值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保极值加法不会崩溃
  let result1 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(result1, max_val)
  
  let result2 = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(result2, min_val)
  
  // 确保极值乘法不会崩溃
  let result3 = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(result3, max_val)
}

test "real_world_calculations" {
  // 测试实际应用场景的计算
  // 计算矩形面积和周长
  let length = 10
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(area, 50)
  
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(perimeter, 30)
  
  // 计算折扣价格
  let original_price = 100
  let discount_percent = 20
  let discount_amount = @azimuth.multiply(original_price, discount_percent) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(final_price, 80)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetFunctionWithALengthyInput"
  let result1 = @azimuth.greet(long_string)
  @azimuth.assert_eq_string(result1, "Hello, " + long_string + "!")
  
  let empty_string = ""
  let result2 = @azimuth.greet(empty_string)
  @azimuth.assert_eq_string(result2, "Hello, !")
  
  let special_chars = "\n\t\r"
  let result3 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string(result3, "Hello, \n\t\r!")
}

test "mathematical_properties_verification" {
  // 验证数学性质
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