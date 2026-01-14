// 标准 MoonBit 测试用例
// 基于项目的 add, multiply, greet 函数创建的测试用例

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(0, add(0, 0))
  assert_eq(5, add(2, 3))
  assert_eq(-5, add(-2, -3))
  assert_eq(0, add(5, -5))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值边界
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, 100))
  
  // 测试最小值边界
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, -100))
  
  // 测试最大值和最小值相加
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_special_values" {
  // 测试特殊值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1相乘
  assert_eq(5, multiply(5, 1))
  assert_eq(5, multiply(1, 5))
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 测试与-1相乘
  assert_eq(-5, multiply(5, -1))
  assert_eq(-5, multiply(-1, 5))
  assert_eq(min_val, multiply(min_val, -1))  // 特殊情况：最小值乘以-1仍为最小值
  
  // 测试溢出情况
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_functionality" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 测试!", greet("测试"))
}

test "complex_calculations" {
  // 测试复合计算
  // 计算 (2 + 3) * (4 + 5) = 5 * 9 = 45
  let result1 = multiply(add(2, 3), add(4, 5))
  assert_eq(45, result1)
  
  // 计算 (10 * 2) + (20 * 3) = 20 + 60 = 80
  let result2 = add(multiply(10, 2), multiply(20, 3))
  assert_eq(80, result2)
  
  // 计算 ((5 + 5) * (3 + 2)) - 10 = (10 * 5) - 10 = 40
  let result3 = add(multiply(add(5, 5), add(3, 2)), -10)
  assert_eq(40, result3)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品1价格299，数量2；商品2价格199，数量3
  let item1_total = multiply(299, 2)  // 598
  let item2_total = multiply(199, 3)  // 597
  let cart_total = add(item1_total, item2_total)  // 1195
  assert_eq(1195, cart_total)
  
  // 温度转换：摄氏度转华氏度（简化版 F = C * 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 简单贷款计算：本金10000，利率5%，期限3年
  let principal = 10000
  let interest = multiply(principal, multiply(5, 3))
  let total = add(principal, interest)
  assert_eq(160000, total)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  let c = 5
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法结合律
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(a, b), c), multiply(a, multiply(b, c)))
  
  // 分配律
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试溢出保护
  assert_true(add(max_val, 1) == max_val)
  assert_true(add(min_val, -1) == min_val)
  assert_true(multiply(max_val, 2) == max_val)
  assert_true(multiply(min_val, 2) == min_val)
  
  // 测试零值特性
  assert_eq(0, multiply(0, max_val))
  assert_eq(0, multiply(0, min_val))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
}

test "performance_and_large_numbers" {
  // 测试性能和大数处理
  // 大数相加
  assert_eq(2000000000, add(1000000000, 1000000000))
  assert_eq(-2000000000, add(-1000000000, -1000000000))
  
  // 大数相乘
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
  
  // 连续运算
  let chain_result = add(multiply(100, 100), multiply(200, 200))
  assert_eq(50000, chain_result)
  
  // 复杂计算：计算前5个自然数的平方和
  let sum_of_squares = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, sum_of_squares)
}

test "string_and_unicode_handling" {
  // 测试字符串和Unicode处理
  // 基本字符串测试
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  
  // 包含空格的字符串
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,  !", greet(" "))
  
  // 特殊字符
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, user_123!", greet("user_123"))
  
  // Unicode字符
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, café!", greet("café"))
  
  // 混合内容
  assert_eq_string("Hello, Test123中文!", greet("Test123中文"))
  assert_eq_string("Hello, 🎉Party🎉!", greet("🎉Party🎉"))
}