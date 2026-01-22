// Azimuth 综合测试套件
// 包含10个高质量的MoonBit测试用例，覆盖各种功能和边界情况

test "basic_arithmetic_operations" {
  // 测试基本的算术运算功能
  // 加法测试
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  
  // 乘法测试
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  
  // 向上取整除法测试
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "string_operations_edge_cases" {
  // 测试字符串操作的边界情况
  // 空字符串测试
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单字符测试
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  
  // 长字符串测试
  let long_name = "VeryLongName"
  @azimuth.assert_eq_string("Hello, " + long_name + "!", @azimuth.greet(long_name))
  
  // Unicode字符测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "division_by_zero_handling" {
  // 测试除零错误处理
  // 根据lib.mbt中的实现，除零应该返回0
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-10, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(2147483647, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-2147483648, 0))
}

test "overflow_boundary_tests" {
  // 测试溢出边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 乘法溢出测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(-46341, 46341))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 边界值运算测试
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "mathematical_properties" {
  // 测试数学性质（交换律、结合律等）
  let a = 123
  let b = 456
  let c = 789
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  @azimuth.assert_eq(@azimuth.multiply(-5, 7), @azimuth.multiply(7, -5))
  
  // 结合律验证（在安全范围内）
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律验证
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "negative_number_operations" {
  // 测试负数运算的各种情况
  let neg1 = -15
  let neg2 = -25
  let pos1 = 10
  let pos2 = 20
  
  // 负数加法
  @azimuth.assert_eq(-40, @azimuth.add(neg1, neg2))
  @azimuth.assert_eq(-5, @azimuth.add(neg1, pos1))
  @azimuth.assert_eq(5, @azimuth.add(pos1, neg1))
  
  // 负数乘法
  @azimuth.assert_eq(375, @azimuth.multiply(neg1, neg2))
  @azimuth.assert_eq(-150, @azimuth.multiply(neg1, pos1))
  @azimuth.assert_eq(-150, @azimuth.multiply(pos1, neg1))
  
  // 负数除法（向上取整）
  @azimuth.assert_eq(-5, @azimuth.divide_with_ceil(-15, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(15, -5))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-15, -5))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 嵌套运算：((a + b) * c) + d
  let a = 5
  let b = 3
  let c = 4
  let d = 7
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, d)
  @azimuth.assert_eq(35, result)
  
  // 复合运算：a * (b + c) + d * e
  let e = 2
  let sum_bc = @azimuth.add(b, c)
  let term1 = @azimuth.multiply(a, sum_bc)
  let term2 = @azimuth.multiply(d, e)
  let complex_result = @azimuth.add(term1, term2)
  @azimuth.assert_eq(54, complex_result)
  
  // 分数运算的向上取整
  @azimuth.assert_eq(8, @azimuth.divide_with_ceil(@azimuth.add(@azimuth.multiply(3, 5), 7), 3))
}

test "input_validation_tests" {
  // 测试输入验证
  // greet函数的输入验证
  @azimuth.assert_eq_string("Hello, null!", @azimuth.greet("null"))
  @azimuth.assert_eq_string("Hello, undefined!", @azimuth.greet("undefined"))
  @azimuth.assert_eq_string("Hello, 0!", @azimuth.greet("0"))
  @azimuth.assert_eq_string("Hello, false!", @azimuth.greet("false"))
  @azimuth.assert_eq_string("Hello, true!", @azimuth.greet("true"))
  
  // 数值函数的边界输入
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  
  // 极值输入测试
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "performance_critical_operations" {
  // 测试性能关键操作
  // 大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @azimuth.assert_eq(3000000, @azimuth.add(large_num1, large_num2))
  @azimuth.assert_eq(2000000000000, @azimuth.multiply(large_num1, large_num2))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(large_num2, large_num1))
  
  // 重复运算测试
  let base = 100
  let result1 = @azimuth.add(base, @azimuth.add(base, @azimuth.add(base, base)))
  let result2 = @azimuth.multiply(base, @azimuth.multiply(base, base))
  
  @azimuth.assert_eq(400, result1)
  @azimuth.assert_eq(1000000, result2)
  
  // 链式运算
  let chain_result = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, chain_result)
}

test "real_world_use_cases" {
  // 测试实际用例
  // 购物车计算：商品总价 + 税费
  let item1_price = 299
  let item2_price = 399
  let item3_price = 199
  let tax_multiplier = 3
  
  let subtotal = @azimuth.add(@azimuth.add(item1_price, item2_price), item3_price)
  let total_with_tax = @azimuth.multiply(subtotal, tax_multiplier)
  @azimuth.assert_eq(2691, total_with_tax)
  
  // 平均分计算（向上取整）
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 89
  let score5 = 94
  
  let total_score = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4), score5)
  let average_score = @azimuth.divide_with_ceil(total_score, 5)
  @azimuth.assert_eq(88, average_score)
  
  // 用户欢迎消息
  let user_name = "Alice"
  let admin_name = "Bob"
  let guest_name = "Guest"
  
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet(user_name))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet(admin_name))
  @azimuth.assert_eq_string("Hello, Guest!", @azimuth.greet(guest_name))
  
  // 库存管理：计算剩余库存空间
  let total_capacity = 10000
  let used_space = 3456
  let item_size = 123
  
  let remaining_space = @azimuth.add(total_capacity, -used_space)
  let items_can_add = @azimuth.divide_with_ceil(remaining_space, item_size)
  @azimuth.assert_eq(54, items_can_add)
}