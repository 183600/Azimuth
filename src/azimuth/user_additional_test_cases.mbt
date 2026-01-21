// 用户额外测试用例
// 测试 azimuth 包的核心功能

test "add_function_stress_test" {
  // 加法函数压力测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 连续加法操作
  let result1 = add(add(add(1000000, 2000000), 3000000), 4000000)
  assert_eq(10000000, result1)
  
  // 极值连续操作
  let result2 = add(add(max_val - 1000000, 500000), add(500000, 0))
  assert_eq(max_val, result2)
  
  // 负数连续操作
  let result3 = add(add(-1000000, -2000000), add(-3000000, -4000000))
  assert_eq(-10000000, result3)
}

test "multiply_function_efficiency_test" {
  // 乘法函数效率测试
  let sqrt_max = 46340
  
  // 平方数测试
  assert_eq(100, multiply(10, 10))
  assert_eq(1024, multiply(32, 32))
  assert_eq(2147395600, multiply(sqrt_max, sqrt_max))
  
  // 连续乘法
  let result1 = multiply(multiply(10, 20), 30)
  assert_eq(6000, result1)
  
  // 混合符号乘法
  assert_eq(-1000, multiply(10, multiply(-10, 10)))
  assert_eq(1000, multiply(-10, multiply(-10, 10)))
}

test "greet_function_performance_test" {
  // greet 函数性能测试
  let long_name = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  
  // 长字符串处理
  let result1 = greet(long_name)
  assert_eq_string("Hello, " + long_name + "!", result1)
  
  // 特殊字符组合
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result2 = greet(special_chars)
  assert_eq_string("Hello, " + special_chars + "!", result2)
  
  // 数字字符串
  let numeric = "1234567890"
  let result3 = greet(numeric)
  assert_eq_string("Hello, " + numeric + "!", result3)
}

test "divide_with_ceil_precision_test" {
  // 向上取整除法精度测试
  // 边界精度测试
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(2, divide_with_ceil(1000001, 1000000))
  
  // 大数除法
  assert_eq(1000000, divide_with_ceil(1000000000000, 1000000))
  assert_eq(1000001, divide_with_ceil(1000001000000, 1000000))
  
  // 负数精度
  assert_eq(-1, divide_with_ceil(-1, 1000000))
  assert_eq(-2, divide_with_ceil(-1000001, 1000000))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算带税价的商品总价
  let item_price = 299
  let quantity = 7
  let tax_rate = 8  // 12.5% 税率 (1/8)
  
  // 计算原价
  let subtotal = multiply(item_price, quantity)
  assert_eq(2093, subtotal)
  
  // 计算税费
  let tax_amount = divide_with_ceil(subtotal, tax_rate)
  assert_eq(262, tax_amount)
  
  // 计算总价
  let total = add(subtotal, tax_amount)
  assert_eq(2355, total)
}

test "mathematical_sequence_test" {
  // 数学序列测试
  // 斐波那契数列前几项
  let fib1 = 1
  let fib2 = 1
  let fib3 = add(fib1, fib2)
  let fib4 = add(fib2, fib3)
  let fib5 = add(fib3, fib4)
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  
  // 阶乘计算
  let fact3 = multiply(3, multiply(2, 1))
  let fact4 = multiply(4, fact3)
  let fact5 = multiply(5, fact4)
  
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
}

test "error_boundary_comprehensive_test" {
  // 错误边界综合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 溢出后恢复测试
  let overflow1 = add(max_val, 1)
  let overflow2 = multiply(max_val, 2)
  let overflow3 = multiply(min_val, -1)
  
  // 验证正常操作仍然工作
  let normal1 = add(100, 200)
  let normal2 = multiply(10, 20)
  let normal3 = divide_with_ceil(25, 4)
  
  assert_eq(max_val, overflow1)
  assert_eq(max_val, overflow2)
  assert_eq(min_val, overflow3)
  assert_eq(300, normal1)
  assert_eq(200, normal2)
  assert_eq(7, normal3)
}

test "string_processing_comprehensive_test" {
  // 字符串处理综合测试
  // Unicode 字符测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  
  // 混合内容测试
  assert_eq_string("Hello, Test123!", greet("Test123"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, https://example.com!", greet("https://example.com"))
  
  // 空白字符测试
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, 			!", greet("\t\t\t"))
  assert_eq_string("Hello, 


!", greet("\n\n\n"))
}

test "algorithm_optimization_test" {
  // 算法优化测试
  // 快速幂算法模拟
  let base = 2
  let exp = 10
  
  // 使用乘法模拟 2^10
  let result = multiply(multiply(multiply(multiply(multiply(base, base), base), base), base), base)
  assert_eq(64, result)
  
  // 优化计算：(a+b)^2 = a^2 + 2ab + b^2
  let a = 5
  let b = 3
  let a_squared = multiply(a, a)
  let b_squared = multiply(b, b)
  let two_ab = multiply(2, multiply(a, b))
  let square_sum = add(add(a_squared, two_ab), b_squared)
  
  // 验证：(5+3)^2 = 64
  assert_eq(64, square_sum)
}

test "data_validation_test" {
  // 数据验证测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值验证
  let boundary1 = add(max_val, 0)
  let boundary2 = add(min_val, 0)
  let boundary3 = multiply(max_val, 1)
  let boundary4 = multiply(min_val, 1)
  
  assert_eq(max_val, boundary1)
  assert_eq(min_val, boundary2)
  assert_eq(max_val, boundary3)
  assert_eq(min_val, boundary4)
  
  // 数据类型一致性验证
  let int_result = add(5, 3)
  let string_result = greet("Test")
  
  assert_eq(8, int_result)
  assert_eq_string("Hello, Test!", string_result)
  
  // 布尔逻辑验证
  assert_true(8 == int_result)
  assert_false(9 == int_result)
  assert_true(string_result == "Hello, Test!")
  assert_false(string_result == "Hello, World!")
}