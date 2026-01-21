// 标准 MoonBit 测试用例 - 增强版
// 测试 azimuth 库的核心功能和边界情况

test "divide_with_ceil_precision_tests" {
  // 测试 divide_with_ceil 函数的精度和边界情况
  
  // 测试完全整除的情况
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(15, 3))
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(21, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))
  @azimuth.assert_eq(-7, @azimuth.divide_with_ceil(-21, 3))
  
  // 测试接近整除的情况
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(16, 3))
  @azimuth.assert_eq(6, @azimuth.divide_with_ceil(17, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-16, 3))
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-17, 3))
  
  // 测试大数除法
  @azimuth.assert_eq(1000000, @azimuth.divide_with_ceil(1000000, 1))
  @azimuth.assert_eq(500000, @azimuth.divide_with_ceil(1000000, 2))
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(1000000, 3))
}

test "divide_with_ceil_edge_cases" {
  // 测试 divide_with_ceil 函数的边缘情况
  
  // 测试除数为1和-1的情况
  @azimuth.assert_eq(42, @azimuth.divide_with_ceil(42, 1))
  @azimuth.assert_eq(-42, @azimuth.divide_with_ceil(42, -1))
  @azimuth.assert_eq(-42, @azimuth.divide_with_ceil(-42, 1))
  @azimuth.assert_eq(42, @azimuth.divide_with_ceil(-42, -1))
  
  // 测试被除数为0的情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, -100))
  
  // 测试除数为0的情况（应该返回0）
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(42, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-42, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "complex_function_composition" {
  // 测试复杂的函数组合
  
  // 模拟计算表达式：(a + b) * (c - d) / e
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  
  let sum = @azimuth.add(a, b)  // 15
  let diff = @azimuth.add(c, -d)  // 12
  let product = @azimuth.multiply(sum, diff)  // 180
  let result = @azimuth.divide_with_ceil(product, e)  // 60
  
  @azimuth.assert_eq(60, result)
  
  // 测试嵌套函数调用
  let nested_result = @azimuth.divide_with_ceil(
    @azimuth.multiply(
      @azimuth.add(3, 4), 
      @azimuth.add(5, 6)
    ), 
    7
  )
  @azimuth.assert_eq(8, nested_result)
}

test "string_greet_format_validation" {
  // 测试字符串问候函数的格式验证
  
  // 测试各种格式的输入
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet("John"))
  @azimuth.assert_eq_string("Hello, john!", @azimuth.greet("john"))
  @azimuth.assert_eq_string("Hello, JOHN!", @azimuth.greet("JOHN"))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, User123!", @azimuth.greet("User123"))
  @azimuth.assert_eq_string("Hello, 123User!", @azimuth.greet("123User"))
  
  // 测试包含下划线和连字符的字符串
  @azimuth.assert_eq_string("Hello, user_name!", @azimuth.greet("user_name"))
  @azimuth.assert_eq_string("Hello, user-name!", @azimuth.greet("user-name"))
  
  // 测试单字符输入
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, Z!", @azimuth.greet("Z"))
}

test "mathematical_property_validation" {
  // 测试数学性质验证
  
  // 测试加法的结合律
  let a = 12
  let b = 34
  let c = 56
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试乘法的结合律
  let result3 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result4 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(result3, result4)
  
  // 测试分配律 (a * (b + c) = a * b + a * c)
  let result5 = @azimuth.multiply(a, @azimuth.add(b, c))
  let result6 = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(result5, result6)
}

test "performance_batch_operations" {
  // 测试批量操作的性能相关测试
  
  // 批量加法测试
  let mut sum = 0
  
  // 模拟循环加法
  sum = @azimuth.add(sum, 1)
  sum = @azimuth.add(sum, 2)
  sum = @azimuth.add(sum, 3)
  sum = @azimuth.add(sum, 4)
  sum = @azimuth.add(sum, 5)
  
  @azimuth.assert_eq(15, sum)
  
  // 批量乘法测试
  let mut product = 1
  product = @azimuth.multiply(product, 2)
  product = @azimuth.multiply(product, 3)
  product = @azimuth.multiply(product, 4)
  product = @azimuth.multiply(product, 5)
  
  @azimuth.assert_eq(120, product)
  
  // 混合批量操作
  let mut result = 10
  result = @azimuth.add(result, 5)  // 15
  result = @azimuth.multiply(result, 2)  // 30
  result = @azimuth.add(result, 10)  // 40
  result = @azimuth.divide_with_ceil(result, 8)  // 5
  
  @azimuth.assert_eq(5, result)
}

test "error_boundary_comprehensive" {
  // 综合错误边界测试
  
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法的各种边界组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1000000000))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1000000000))
  
  // 测试乘法的各种边界组合
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(1, max_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(1, min_val))
  
  // 测试除法的边界情况
  @azimuth.assert_eq(max_val, @azimuth.divide_with_ceil(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.divide_with_ceil(min_val, 1))
  @azimuth.assert_eq(-max_val, @azimuth.divide_with_ceil(max_val, -1))
  @azimuth.assert_eq(-min_val, @azimuth.divide_with_ceil(min_val, -1))
}

test "real_world_application_simulation" {
  // 模拟真实世界应用场景
  
  // 场景1：计算订单总价（含税）
  let item_price = 100
  let quantity = 5
  let tax_rate = 8  // 8%税，用8表示0.08（简化）
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)  // 500
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)  // 40
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)  // 555
  
  @azimuth.assert_eq(555, total)
  
  // 场景2：计算平均分（向上取整）
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let student_count = 4
  
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4)
  let average_score = @azimuth.divide_with_ceil(total_score, student_count)
  
  @azimuth.assert_eq(88, average_score)
  
  // 场景3：生成个性化欢迎消息
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let welcome_message = @azimuth.greet(full_name)
  
  @azimuth.assert_eq_string("Hello, 张三!", welcome_message)
}

test "assertion_function_comprehensive" {
  // 综合断言函数测试
  
  // 测试整数相等断言
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq(-42, -42)
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  
  // 测试字符串相等断言
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("Hello", "Hello")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
  @azimuth.assert_eq_string("测试", "测试")
  @azimuth.assert_eq_string("🌙", "🌙")
}