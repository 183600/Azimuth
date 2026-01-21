// 测试新创建的测试文件
// 使用标准的 MoonBit 测试语法

test "add_function_basic_test" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "multiply_function_basic_test" {
  // 测试乘法函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "greet_function_basic_test" {
  // 测试问候函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "divide_with_ceil_function_basic_test" {
  // 测试向上取整除法的基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "assert_functions_test" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}

test "complex_operations_test" {
  // 测试复合运算
  let a = 10
  let b = 20
  let c = -5
  
  // 计算 (a * b) + c
  let product = @azimuth.multiply(a, b)
  let result = @azimuth.add(product, c)
  @azimuth.assert_eq(195, result)
  
  // 计算 a * (b + c)
  let sum = @azimuth.add(b, c)
  let result2 = @azimuth.multiply(a, sum)
  @azimuth.assert_eq(150, result2)
}

test "edge_cases_test" {
  // 测试边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与0的运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  
  // 测试与1的运算
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "mathematical_properties_test" {
  // 测试数学性质
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(123, 456), @azimuth.add(456, 123))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(12, 34), @azimuth.multiply(34, 12))
  
  // 乘法分配律
  let a = 123
  let b = 456
  let c = 789
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "real_world_scenarios_test" {
  // 测试实际应用场景
  // 场景1：计算购物车总价
  let item1 = 299
  let item2 = 399
  let item3 = 199
  let subtotal = @azimuth.add(@azimuth.add(item1, item2), item3)
  @azimuth.assert_eq(897, subtotal)
  
  // 场景2：计算平均分（向上取整）
  let score1 = 87
  let score2 = 93
  let score3 = 79
  let score4 = 85
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4)
  let average_score = @azimuth.divide_with_ceil(total_score, 4)
  @azimuth.assert_eq(87, average_score)
  
  // 场景3：用户欢迎消息
  let username = "张三"
  let welcome_msg = @azimuth.greet(username)
  @azimuth.assert_eq_string("Hello, 张三!", welcome_msg)
}

test "unicode_and_special_chars_test" {
  // 测试Unicode字符和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
}